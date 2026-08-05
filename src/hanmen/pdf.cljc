(ns hanmen.pdf
  "A PDF page's content stream, walked, into `hanmen.page` marks.

  ## Why this is not in `org-iso-pdf`

  `org-iso-pdf` is the COS object model: objects, xref, the page tree, stream
  filters. A content stream is not part of that model — it is a *program* in a
  small stack language, and running it needs a graphics state, a text state
  and a matrix stack that the object model has no reason to own. Keeping the
  interpreter out of the spec repo is the same boundary `pdf.core` already
  draws when it hands back opaque `DCTDecode` bytes rather than decoding them.

  What `org-iso-pdf` does offer and this uses: `parse`, `pages`,
  `page-content-str` and `resolve-ref`. Nothing here re-reads the file.

  ## What placement actually requires

  `pdf.core/extract-text` collects the strings between `BT` and `ET`. That is
  the right answer for search and no answer at all for a viewer: it has no
  `Tm`, so every run in the document is at the same place, which is nowhere.

  Placing a run means carrying two matrices — the CTM, which `q`/`Q`/`cm`
  move, and the text matrix, which `Tm`/`Td`/`T*` move and which every glyph
  advances. The run's origin is `Trm` applied to the origin, and `Trm` is
  `[Tfs·Th 0 0 Tfs 0 Trise] × Tm × CTM`. There is no shortcut: a producer that
  emits one `Tm` and then a hundred `TJ`s — which is most of them — puts every
  line after the first in the wrong place if the advance is not accumulated.

  ## Advance width is the honest part

  Where a run *starts* is exact: it comes out of the matrices. How wide it is
  depends on the font's glyph widths, and a document only sometimes ships
  them. When `/Widths` is there this computes the run's width in text space
  and `hanmen.page/text-item` carries it, so a drawer can make its own font
  occupy exactly that much. When it is not — a composite font, a standard-14
  font with no `/Widths` — the width is **absent rather than estimated**, and
  the drawer starts the run correctly and ends it wherever its font lands.

  An estimate would be indistinguishable from a measurement in the output and
  wrong by a different amount on every document. Absence is a fact a consumer
  can act on.

  ## CJK is decoded through `/ToUnicode` or not claimed

  A composite (`/Type0`) font's codes are not characters — with `Identity-H`
  they are glyph ids, and mapping them needs the font's `/ToUnicode` CMap.
  This reads `beginbfchar` / `beginbfrange` and uses it. A `/Type0` run with
  no usable `/ToUnicode` becomes a `:frame` marked `:font/no-tounicode`
  rather than a run of mojibake: text nobody can read is worse than a marked
  region, because it goes into search results and into anything that quotes
  the page."
  (:require [clojure.string :as str]
            [hanmen.page :as page]
            [pdf.core :as pdf]))

;; ── matrices ─────────────────────────────────────────────────────────────────
;; [a b c d e f], the PDF form: (x,y) -> (a·x + c·y + e, b·x + d·y + f).

(def identity-matrix [1.0 0.0 0.0 1.0 0.0 0.0])

(defn mul
  "`m1` then `m2`."
  [[a1 b1 c1 d1 e1 f1] [a2 b2 c2 d2 e2 f2]]
  [(+ (* a1 a2) (* b1 c2))
   (+ (* a1 b2) (* b1 d2))
   (+ (* c1 a2) (* d1 c2))
   (+ (* c1 b2) (* d1 d2))
   (+ (* e1 a2) (* f1 c2) e2)
   (+ (* e1 b2) (* f1 d2) f2)])

(defn apply-point [[a b c d e f] [x y]]
  [(+ (* a x) (* c y) e) (+ (* b x) (* d y) f)])

(defn- translation [tx ty] [1.0 0.0 0.0 1.0 (double tx) (double ty)])

(defn- y-scale
  "How much this matrix stretches a unit step in y.

  The font size a drawer needs is not `Tf`'s operand: a `cm` of `[0.5 0 0 0.5
  0 0]` halves 12pt text to 6pt on the page, and a viewer that drew 12 would
  be twice as big as the document on every scaled form ever produced."
  [[_ b _ d _ _]]
  (Math/sqrt (+ (* (double b) (double b)) (* (double d) (double d)))))

;; ── tokenizer ────────────────────────────────────────────────────────────────

(defn- ws? [c] (contains? #{\space \tab \newline \return \formfeed (char 0)} c))
(defn- delim? [c] (contains? #{\( \) \< \> \[ \] \{ \} \/ \%} c))

(defn- num-token? [^String s]
  (boolean (re-matches #"[-+]?(\d+\.?\d*|\.\d+)" s)))

(defn- read-literal-string
  "A `(…)` string, from the index just after the open paren. Returns
  `[chars next-index]`.

  Parens nest and may be escaped, which is the whole reason this cannot be a
  scan to the next `)`. Octal escapes are three digits at most and stop early
  on a non-digit."
  [^String s start]
  (let [n (count s)]
    (loop [i start depth 1 out (transient [])]
      (if (>= i n)
        [(persistent! out) i]
        (let [c (.charAt s i)]
          (cond
            (= c \\)
            (let [d (when (< (inc i) n) (.charAt s (inc i)))]
              (case d
                \n (recur (+ i 2) depth (conj! out \newline))
                \r (recur (+ i 2) depth (conj! out \return))
                \t (recur (+ i 2) depth (conj! out \tab))
                \b (recur (+ i 2) depth (conj! out \backspace))
                \f (recur (+ i 2) depth (conj! out \formfeed))
                \newline (recur (+ i 2) depth out)
                (if (and d (<= (int \0) (int d) (int \7)))
                  (let [end (min n (+ i 4))
                        octal (loop [j (inc i)]
                                (if (and (< j end)
                                         (<= (int \0) (int (.charAt s j)) (int \7)))
                                  (recur (inc j))
                                  j))]
                    (recur octal depth
                           (conj! out (char (mod #?(:clj (Integer/parseInt (subs s (inc i) octal) 8)
                                                    :cljs (js/parseInt (subs s (inc i) octal) 8))
                                                 256)))))
                  (recur (+ i 2) depth (if d (conj! out d) out)))))

            (= c \() (recur (inc i) (inc depth) (conj! out c))
            (= c \)) (if (= depth 1)
                       [(persistent! out) (inc i)]
                       (recur (inc i) (dec depth) (conj! out c)))
            :else (recur (inc i) depth (conj! out c))))))))

(defn- read-hex-string [^String s start]
  (let [n (count s)
        end (or (str/index-of s ">" start) n)
        hex (str/replace (subs s start end) #"[^0-9A-Fa-f]" "")
        hex (if (odd? (count hex)) (str hex "0") hex)]
    [(mapv (fn [pair] (char #?(:clj (Integer/parseInt pair 16)
                               :cljs (js/parseInt pair 16))))
           (map #(apply str %) (partition 2 hex)))
     (inc end)]))

(defn tokenize
  "A content stream into `[:num n]` / `[:str chars]` / `[:name k]` /
  `[:op \"Tj\"]` and the array/dict delimiters.

  Inline images (`BI … ID … EI`) are skipped whole: their binary payload is
  not tokens, and a tokenizer that tried would produce operators out of pixel
  data. What is lost is one `:frame`, which is a smaller lie than a stream of
  invented operators."
  [^String s]
  (let [n (count s)]
    (loop [i 0 out (transient [])]
      (if (>= i n)
        (persistent! out)
        (let [c (.charAt s i)]
          (cond
            (ws? c) (recur (inc i) out)

            (= c \%) (recur (let [e (str/index-of s "\n" i)] (if e (inc e) n)) out)

            (= c \()
            (let [[chars next-i] (read-literal-string s (inc i))]
              (recur next-i (conj! out [:str chars])))

            (and (= c \<) (not= (when (< (inc i) n) (.charAt s (inc i))) \<))
            (let [[chars next-i] (read-hex-string s (inc i))]
              (recur next-i (conj! out [:str chars])))

            (and (= c \<) (= (when (< (inc i) n) (.charAt s (inc i))) \<))
            (recur (+ i 2) (conj! out [:dict-open]))

            (and (= c \>) (= (when (< (inc i) n) (.charAt s (inc i))) \>))
            (recur (+ i 2) (conj! out [:dict-close]))

            (= c \[) (recur (inc i) (conj! out [:array-open]))
            (= c \]) (recur (inc i) (conj! out [:array-close]))

            (= c \/)
            (let [end (loop [j (inc i)]
                        (if (and (< j n) (not (ws? (.charAt s j))) (not (delim? (.charAt s j))))
                          (recur (inc j)) j))]
              (recur end (conj! out [:name (subs s (inc i) end)])))

            :else
            (let [end (loop [j i]
                        (if (and (< j n) (not (ws? (.charAt s j))) (not (delim? (.charAt s j))))
                          (recur (inc j)) j))
                  word (subs s i (max end (inc i)))]
              (cond
                (num-token? word)
                (recur end (conj! out [:num (double #?(:clj (Double/parseDouble word)
                                                       :cljs (js/parseFloat word)))]))

                ;; An inline image's payload starts after `ID` and one space.
                (= word "BI")
                (let [ei (loop [j (or (str/index-of s "ID" i) n)]
                           (let [k (str/index-of s "EI" j)]
                             (cond (nil? k) n
                                   ;; `EI` inside the payload is only a
                                   ;; terminator when it is delimited.
                                   (and (or (zero? k) (ws? (.charAt s (dec k))))
                                        (or (>= (+ k 2) n) (ws? (.charAt s (+ k 2)))))
                                   (+ k 2)
                                   :else (recur (+ k 2)))))]
                  (recur ei (conj! out [:op "BI-skipped"])))

                :else (recur (max end (inc i)) (conj! out [:op word]))))))))))

;; ── fonts ────────────────────────────────────────────────────────────────────

(defn parse-tounicode
  "`beginbfchar` / `beginbfrange` out of a `/ToUnicode` CMap, as code → string.

  Only the two forms real producers emit. A `bfrange` whose destination is an
  array is expanded elementwise; one whose destination is a single value is
  incremented, which is what the spec says and what breaks if you assume the
  array form is the only one."
  [^String cmap]
  (let [hex->long (fn [h] #?(:clj (Long/parseLong h 16) :cljs (js/parseInt h 16)))
        hex->str (fn [h] (apply str (map (fn [p] (char (hex->long (apply str p))))
                                         (partition 4 4 nil h))))
        chars (reduce
               (fn [acc block]
                 (reduce (fn [acc [_ src dst]]
                           (assoc acc (hex->long src) (hex->str dst)))
                         acc
                         (re-seq #"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>" block)))
               {}
               (map second (re-seq #"(?s)beginbfchar(.*?)endbfchar" cmap)))]
    (reduce
     (fn [acc block]
       (reduce
        (fn [acc entry]
          (let [[_ lo hi arr single] entry
                lo (hex->long lo) hi (hex->long hi)]
            (cond
              (seq arr)
              (reduce (fn [a [i [_ v]]] (assoc a (+ lo i) (hex->str v)))
                      acc
                      (map-indexed vector (re-seq #"<([0-9A-Fa-f]+)>" arr)))

              (seq single)
              (let [base (hex->long single)]
                (reduce (fn [a i] (assoc a (+ lo i) (str (char (+ base i)))))
                        acc
                        (range 0 (inc (max 0 (- hi lo))))))

              :else acc)))
        acc
        (re-seq #"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(?:\[([^\]]*)\]|<([0-9A-Fa-f]+)>)"
                block)))
     chars
     (map second (re-seq #"(?s)beginbfrange(.*?)endbfrange" cmap)))))

(defn font-table
  "The page's `/Resources /Font`, as name → what a run needs to know.

  `:widths` is code → glyph-space width; absent when the font did not ship
  one. `:composite?` and `:to-unicode` decide whether a `/Type0` run can be
  read at all."
  [objs page-dict]
  (let [res (pdf/resolve-ref objs (:Resources page-dict))
        fonts (pdf/resolve-ref objs (:Font res))]
    (when (map? fonts)
      (into {}
            (keep (fn [[k v]]
                    (let [fd (pdf/resolve-ref objs v)]
                      (when (map? fd)
                        (let [subtype (:Subtype fd)
                              first-char (pdf/resolve-ref objs (:FirstChar fd))
                              widths (pdf/resolve-ref objs (:Widths fd))
                              tu (pdf/resolve-ref objs (:ToUnicode fd))
                              tu-map (when (and (map? tu) (:pdf.core/stream tu))
                                       (parse-tounicode
                                        (apply str (map char (pdf/decode-stream objs tu)))))]
                          [(name k)
                           {:base-font (some-> (:BaseFont fd) name)
                            :composite? (= subtype :Type0)
                            :to-unicode tu-map
                            :widths (when (and (number? first-char) (vector? widths))
                                      (into {}
                                            (keep-indexed
                                             (fn [i w]
                                               (let [w (pdf/resolve-ref objs w)]
                                                 (when (number? w)
                                                   [(+ (long first-char) i) (double w)]))))
                                            widths))}]))))
                  fonts)))))

(def ^:private default-width
  "What a code is worth when the font did not say.

  500/1000 em is the width of a digit in most of the standard 14, and it is
  used **only to advance the pen** — never to report `:item/width`, which
  stays absent so that nothing downstream mistakes this number for a
  measurement."
  500.0)

(defn- codes-of
  "A string operand's bytes as codes: one byte per code for a simple font,
  two for a composite one.

  Composite fonts are two-byte here because `Identity-H` is what essentially
  every producer uses for CJK; a font with a different CMap will come out
  wrong, and it comes out as `:frame` rather than as text because the
  `/ToUnicode` lookup misses."
  [chars composite?]
  (if composite?
    (mapv (fn [[hi lo]] (+ (* 256 (int hi)) (int (or lo (char 0)))))
          (partition 2 2 nil chars))
    (mapv int chars)))

;; ── the walk ─────────────────────────────────────────────────────────────────

(def ^:private initial-text-state
  {:font nil :size 0.0 :char-spacing 0.0 :word-spacing 0.0
   :horizontal 1.0 :leading 0.0 :rise 0.0 :render-mode 0})

(defn- run-of
  "One string operand, placed, as either a text item or a frame."
  [{:keys [text-state ctm tm fonts]} chars]
  (let [{:keys [font size char-spacing word-spacing horizontal rise render-mode]} text-state
        fd (get fonts font)
        composite? (boolean (:composite? fd))
        codes (codes-of chars composite?)
        widths (:widths fd)
        tu (:to-unicode fd)
        text (if composite?
               (apply str (keep #(get tu %) codes))
               (apply str (map char codes)))
        ;; Advance in *text space*, before Tm/CTM. Word spacing applies to
        ;; single-byte code 32 only — applying it to a composite font's
        ;; two-byte code that happens to contain 32 is a classic drift.
        advance (reduce (fn [acc code]
                          (+ acc
                             (* (+ (* (/ (double (get widths code default-width)) 1000.0) size)
                                   char-spacing
                                   (if (and (not composite?) (= code 32)) word-spacing 0.0))
                                horizontal)))
                        0.0 codes)
        trm (mul (mul [(* size horizontal) 0.0 0.0 size 0.0 rise] tm) ctm)
        [x y] (apply-point trm [0.0 0.0])
        drawn-size (* size (y-scale (mul tm ctm)))]
    {:advance advance
     :item (cond
             ;; Mode 3 is "invisible", which is what a scanner writes under
             ;; the page image so the text is selectable. It IS the text
             ;; layer of a scanned page, so it is kept — dropping it would
             ;; make exactly the documents that most need search unsearchable.
             (and composite? (empty? text) (seq codes))
             (page/frame-item {:x x :y (- y drawn-size) :width (* advance (y-scale ctm))
                               :height drawn-size
                               :label (or (:base-font fd) "text")
                               :reason :font/no-tounicode})

             (str/blank? text) nil

             :else
             (page/text-item
              {:x x :y y :size drawn-size :text text
               :font (:base-font fd)
               :width (when widths (* advance (y-scale ctm)))
               :direction (when (= render-mode 3) :invisible)}))}))

(defn- flip
  "Reader space out of PDF user space, with the page's rotation applied.

  `[llx lly urx ury]` is the media box, which is not always at the origin —
  a cropped page can have a negative `lly`, and treating the box as
  `[0 0 w h]` shifts every mark on such a page by the offset. Measured on a
  real cropped file, where the whole page rendered off the top edge."
  [[llx lly urx ury] rotation]
  (let [w (- urx llx) h (- ury lly)
        to-origin (translation (- llx) (- lly))]
    ;; `/Rotate` is CLOCKWISE, and the y-flip is folded into the same matrix
    ;; rather than composed after it. Each of these therefore has determinant
    ;; −1 — a flip composed with a rotation is orientation-reversing, and a
    ;; matrix here with determinant +1 is a mirrored page. The first draft of
    ;; the 90° case turned the page anticlockwise and read as plausible in
    ;; every way except the one the test measured.
    (case (mod (or rotation 0) 360)
      90 {:matrix (mul to-origin [0.0 1.0 1.0 0.0 0.0 0.0]) :width h :height w}
      180 {:matrix (mul to-origin [-1.0 0.0 0.0 1.0 w 0.0]) :width w :height h}
      270 {:matrix (mul to-origin [0.0 -1.0 -1.0 0.0 h w]) :width h :height w}
      {:matrix (mul to-origin [1.0 0.0 0.0 -1.0 0.0 h]) :width w :height h})))

(defn- media-box [objs page-dict]
  (let [b (mapv #(pdf/resolve-ref objs %) (pdf/resolve-ref objs (:MediaBox page-dict)))]
    (if (and (= 4 (count b)) (every? number? b))
      (mapv double b)
      [0.0 0.0 612.0 792.0])))

(defn- number-operands [stack]
  (into [] (comp (filter #(= :num (first %))) (map second)) stack))

(defn walk
  "Run one page's content stream into placed marks.

  The operand stack is reset after every operator, which is what a content
  stream's grammar actually says and is why a malformed one degrades into
  missing marks rather than into marks in the wrong place."
  [objs page-dict]
  (let [content (pdf/page-content-str objs page-dict)
        rotation (or (pdf/resolve-ref objs (:Rotate page-dict)) 0)
        {base :matrix :keys [width height]} (flip (media-box objs page-dict) rotation)
        fonts (or (font-table objs page-dict) {})
        xobjects (let [res (pdf/resolve-ref objs (:Resources page-dict))]
                   (pdf/resolve-ref objs (:XObject res)))]
    (loop [tokens (tokenize content)
           stack []
           state {:ctm base :gs [] :text-state initial-text-state
                  :tm identity-matrix :tlm identity-matrix :gray nil
                  :fonts fonts}
           items (transient [])]
      (if-let [[kind value] (first tokens)]
        (let [rest-tokens (rest tokens)]
          (if (not= kind :op)
            (recur rest-tokens (conj stack [kind value]) state items)
            (let [nums (number-operands stack)
                  names (into [] (comp (filter #(= :name (first %))) (map second)) stack)
                  strings (into [] (comp (filter #(= :str (first %))) (map second)) stack)
                  ts (:text-state state)
                  place (fn [state chars]
                          (let [{:keys [advance item]} (run-of state chars)]
                            [(update state :tm #(mul (translation advance 0.0) %))
                             item]))]
              (case value
                "q" (recur rest-tokens [] (update state :gs conj (select-keys state [:ctm :gray])) items)
                "Q" (let [top (peek (:gs state))]
                      (recur rest-tokens []
                             (-> state (update :gs pop) (merge (or top {})))
                             items))
                "cm" (recur rest-tokens []
                            (cond-> state
                              (= 6 (count nums)) (update :ctm #(mul (mapv double nums) %)))
                            items)

                "BT" (recur rest-tokens [] (assoc state :tm identity-matrix
                                                  :tlm identity-matrix) items)
                "ET" (recur rest-tokens [] state items)

                "Tf" (recur rest-tokens []
                            (assoc state :text-state
                                   (assoc ts :font (last names)
                                          :size (double (or (last nums) 0.0))))
                            items)
                "Tc" (recur rest-tokens [] (assoc-in state [:text-state :char-spacing]
                                                     (double (or (last nums) 0.0))) items)
                "Tw" (recur rest-tokens [] (assoc-in state [:text-state :word-spacing]
                                                     (double (or (last nums) 0.0))) items)
                "Tz" (recur rest-tokens [] (assoc-in state [:text-state :horizontal]
                                                     (/ (double (or (last nums) 100.0)) 100.0)) items)
                "TL" (recur rest-tokens [] (assoc-in state [:text-state :leading]
                                                     (double (or (last nums) 0.0))) items)
                "Ts" (recur rest-tokens [] (assoc-in state [:text-state :rise]
                                                     (double (or (last nums) 0.0))) items)
                "Tr" (recur rest-tokens [] (assoc-in state [:text-state :render-mode]
                                                     (long (or (last nums) 0))) items)

                "Td" (let [[tx ty] (take-last 2 nums)
                           next-tlm (mul (translation (or tx 0.0) (or ty 0.0)) (:tlm state))]
                       (recur rest-tokens [] (assoc state :tm next-tlm :tlm next-tlm) items))
                "TD" (let [[tx ty] (take-last 2 nums)
                           next-tlm (mul (translation (or tx 0.0) (or ty 0.0)) (:tlm state))]
                       (recur rest-tokens []
                              (-> state (assoc :tm next-tlm :tlm next-tlm)
                                  (assoc-in [:text-state :leading] (- (double (or ty 0.0)))))
                              items))
                "Tm" (let [m (if (= 6 (count nums)) (mapv double nums) identity-matrix)]
                       (recur rest-tokens [] (assoc state :tm m :tlm m) items))
                "T*" (let [next-tlm (mul (translation 0.0 (- (:leading ts))) (:tlm state))]
                       (recur rest-tokens [] (assoc state :tm next-tlm :tlm next-tlm) items))

                ("Tj" "TJ" "'" "\"")
                (let [;; `'` and `"` move to the next line first. `"` also
                      ;; sets word and char spacing from its two numbers,
                      ;; which a reader that ignored them would render with
                      ;; the previous line's spacing.
                      state (if (contains? #{"'" "\""} value)
                              (let [next-tlm (mul (translation 0.0 (- (:leading ts))) (:tlm state))]
                                (assoc state :tm next-tlm :tlm next-tlm))
                              state)
                      state (if (and (= value "\"") (= 2 (count nums)))
                              (-> state
                                  (assoc-in [:text-state :word-spacing] (double (first nums)))
                                  (assoc-in [:text-state :char-spacing] (double (second nums))))
                              state)]
                  (if (= value "TJ")
                    ;; The array interleaves strings with kerning numbers in
                    ;; thousandths of text space, subtracted from the advance.
                    (let [[state' items']
                          (reduce (fn [[st acc] token]
                                    (case (first token)
                                      :str (let [[st' item] (place st (second token))]
                                             [st' (if item (conj acc item) acc)])
                                      :num (let [tsz (get-in st [:text-state :size])
                                                 th (get-in st [:text-state :horizontal])
                                                 tx (* (- (/ (double (second token)) 1000.0)) tsz th)]
                                             [(update st :tm #(mul (translation tx 0.0) %)) acc])
                                      [st acc]))
                                  [state []]
                                  stack)]
                      (recur rest-tokens [] state'
                             (reduce conj! items items')))
                    (let [[state' item] (place state (or (last strings) []))]
                      (recur rest-tokens [] state'
                             (if item (conj! items item) items)))))

                "re" (if (= 4 (count (take-last 4 nums)))
                       (let [[x y w h] (take-last 4 nums)]
                         (recur rest-tokens []
                                (assoc state :pending-rect [x y w h]) items))
                       (recur rest-tokens [] state items))

                ("f" "F" "f*" "b" "b*" "B" "B*")
                (if-let [[x y w h] (:pending-rect state)]
                  (let [[x0 y0] (apply-point (:ctm state) [x y])
                        [x1 y1] (apply-point (:ctm state) [(+ x w) (+ y h)])
                        item (page/rule-item {:x (min x0 x1) :y (min y0 y1)
                                              :width (Math/abs (- x1 x0))
                                              :height (Math/abs (- y1 y0))
                                              :ink (when (number? (:gray state))
                                                     (- 1.0 (:gray state)))})]
                    (recur rest-tokens [] (dissoc state :pending-rect) (conj! items item)))
                  (recur rest-tokens [] state items))

                ("g" "G") (recur rest-tokens []
                                 (assoc state :gray (double (or (last nums) 0.0))) items)
                ("rg" "RG") (recur rest-tokens []
                                   (assoc state :gray
                                          (if (= 3 (count (take-last 3 nums)))
                                            (let [[r g b] (take-last 3 nums)]
                                              (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))
                                            (:gray state)))
                                   items)

                "Do" (let [nm (last names)
                           xo (pdf/resolve-ref objs (get xobjects (keyword nm)))
                           [x0 y0] (apply-point (:ctm state) [0.0 0.0])
                           [x1 y1] (apply-point (:ctm state) [1.0 1.0])]
                       (recur rest-tokens [] state
                              (if (and nm (map? xo))
                                (conj! items
                                       (page/frame-item
                                        {:x (min x0 x1) :y (min y0 y1)
                                         :width (Math/abs (- x1 x0))
                                         :height (Math/abs (- y1 y0))
                                         :label nm
                                         :reason (if (= :Image (:Subtype (:dict xo)))
                                                   :image/not-decoded
                                                   :xobject/not-run)}))
                                items)))

                ;; Everything else — paths, clipping, colour spaces, marked
                ;; content — advances no state this cares about. Not an
                ;; error: a content stream is full of operators a viewer at
                ;; this level has no opinion about.
                (recur rest-tokens [] state items)))))
        {:items (persistent! items) :width width :height height :rotation rotation}))))

;; ── the public shape ─────────────────────────────────────────────────────────

(defn page-dicts
  "`pdf.core/pages`, minus the one it invents for a file that is not a PDF.

  `collect-pages` walks from `(:Pages (:root parsed))`, and when the root is
  nil — no catalog, no trailer, because the bytes were never a PDF — its
  `:else` branch returns `[{}]`. One empty map, which counts as one page.
  Measured: a text file uploaded as `application/pdf` reported a page and
  rendered blank, which is the answer that sends somebody looking for the
  missing content of a document that was never there.

  A page dict says it is one, or carries content, or carries a box. A blank
  page of a real document has the last two even with no content stream, so
  this drops the phantom without dropping a legitimately empty page."
  [parsed]
  (into [] (filter (fn [d]
                     (and (map? d)
                          (or (= :Page (:Type d))
                              (contains? d :Contents)
                              (contains? d :MediaBox)))))
        (pdf/pages parsed)))

(defn page-at
  "Page `index` of `parsed`, as a `hanmen.page` value."
  [parsed index]
  (let [objs (:objects parsed)
        dicts (page-dicts parsed)
        dict (nth dicts index nil)]
    (when dict
      (let [{:keys [items width height rotation]} (walk objs dict)]
        (page/page {:index index :width width :height height
                    :rotation rotation :items items})))))

(defn read-document
  "Every page of `bytes`, as a `hanmen.page/document`.

  `bytes` is whatever `pdf.core/parse` takes — a seq of unsigned byte
  values. Reading the file is the caller's capability to spend, not this
  library's: nothing here opens anything."
  [bytes]
  (let [parsed (pdf/parse bytes)
        n (count (page-dicts parsed))]
    (page/document (keep #(page-at parsed %) (range n)))))

(defn page-count [parsed] (count (page-dicts parsed)))
