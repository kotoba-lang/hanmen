(ns hanmen.pdf-test
  "Placement, against PDFs written here.

  `pdf.core/write-document` is the oracle: a content stream this test wrote
  says where a mark is in PDF user space, so where it comes out in reader
  space is arithmetic with one right answer rather than a rendering somebody
  looked at and thought seemed fine. Every assertion below is a coordinate,
  not a screenshot."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hanmen.page :as page]
            [hanmen.pdf :as hpdf]
            [pdf.core :as pdf]))

(defn- doc
  "A one-page PDF with `content` as its content stream."
  ([content] (doc content {}))
  ([content {:keys [width height] :or {width 595 height 842}}]
   (pdf/parse (pdf/write-document [{:width width :height height :content content}]))))

(defn- page-of [content & [opts]]
  (hpdf/page-at (doc content (or opts {})) 0))

(defn- texts [p] (filterv #(= :text (:item/kind %)) (:page/items p)))

;; ── the flip ─────────────────────────────────────────────────────────────────

(deftest a-baseline-lands-where-the-document-put-it
  ;; 72pt from the left and 720pt UP from the bottom of an 842pt page is
  ;; 122pt DOWN from the top. Getting this backwards renders every document
  ;; upside down, and it is the single most likely mistake in this file.
  ;; ASCII, because `pdf/write-document` writes a standard-14 Type1 font and
  ;; truncates every char to one byte — CJK through that path produces
  ;; mojibake in the FIXTURE, not in the reader. The composite-font test
  ;; below is where Japanese is actually exercised.
  (let [p (page-of (pdf/text-command {:x 72 :y 720 :text "Agreement" :size 12}))
        [t] (texts p)]
    (is (= 1 (count (texts p))))
    (is (= "Agreement" (:item/text t)))
    (is (= 72.0 (:item/x t)))
    (is (= 122.0 (:item/y t)) "842 − 720")
    (is (= 12.0 (:item/size t)))
    (testing "and the page reports its own size"
      (is (= 595.0 (:page/width p)))
      (is (= 842.0 (:page/height p))))))

(deftest a-media-box-that-does-not-start-at-the-origin-is-taken-into-account
  ;; A cropped page. Treating the box as [0 0 w h] puts every mark off the
  ;; edge by the offset, which looks like "the renderer draws nothing".
  (let [content (pdf/text-command {:x 110 :y 700 :text "cropped" :size 10})
        raw (pdf/write-document [{:width 595 :height 842 :content content}])
        ;; Rewrite the MediaBox in place — same length, so no offset moves.
        text (apply str (map char raw))
        cropped (str/replace text "/MediaBox [0 0 595 842]" "/MediaBox [10 20 595 842]")
        parsed (pdf/parse (mapv #(bit-and (int %) 0xff) (.getBytes ^String cropped "ISO-8859-1")))
        p (hpdf/page-at parsed 0)
        [t] (texts p)]
    (is (= 585.0 (:page/width p)) "595 − 10")
    (is (= 822.0 (:page/height p)) "842 − 20")
    (is (= 100.0 (:item/x t)) "110 − 10")
    (is (= 142.0 (:item/y t)) "822 − (700 − 20)")))

;; ── the matrices ─────────────────────────────────────────────────────────────

(deftest text-accumulates-its-own-advance
  ;; One BT, one Tm, three shows. A reader that does not advance the text
  ;; matrix stacks all three on one spot — and because the first one is
  ;; right, it looks like it works.
  (let [p (page-of (str "BT /F1 10 Tf 1 0 0 1 50 700 Tm "
                        "(one) Tj (two) Tj (three) Tj ET"))
        xs (mapv :item/x (texts p))]
    (is (= 3 (count xs)))
    (is (apply < xs) "each run starts to the right of the last")
    (is (= 50.0 (first xs)))))

(deftest a-line-break-uses-the-leading-not-the-last-position
  ;; `T*` moves relative to the line matrix, not the text matrix. Using the
  ;; text matrix makes every line after the first drift right by the width
  ;; of the line above it.
  (let [p (page-of (str "BT /F1 10 Tf 12 TL 1 0 0 1 40 700 Tm "
                        "(first line) Tj T* (second line) Tj ET"))
        [a b] (texts p)]
    (is (= 40.0 (:item/x a)))
    (is (= 40.0 (:item/x b)) "back to the left margin, not after `first line`")
    (is (= 12.0 (- (:item/y b) (:item/y a))) "one leading down the page")))

(deftest a-scaling-cm-scales-the-type
  ;; 12pt inside a 0.5 scale is 6pt on the page. A viewer that drew the `Tf`
  ;; operand would be twice the document's size on every scaled form.
  (let [p (page-of (str "q 0.5 0 0 0.5 0 0 cm "
                        "BT /F1 12 Tf 1 0 0 1 100 1400 Tm (small) Tj ET Q"))
        [t] (texts p)]
    (is (= 6.0 (:item/size t)))
    (is (= 50.0 (:item/x t)) "100 × 0.5")
    (is (= 142.0 (:item/y t)) "842 − (1400 × 0.5)")))

(deftest q-and-Q-restore-the-matrix
  (let [p (page-of (str "q 2 0 0 2 0 0 cm BT /F1 10 Tf 1 0 0 1 10 400 Tm (big) Tj ET Q "
                        "BT /F1 10 Tf 1 0 0 1 10 400 Tm (normal) Tj ET"))
        [big normal] (texts p)]
    (is (= 20.0 (:item/size big)))
    (is (= 10.0 (:item/size normal)) "Q put the CTM back")
    (is (= 20.0 (:item/x big)))
    (is (= 10.0 (:item/x normal)))))

(deftest tj-kerning-numbers-move-the-pen-backwards
  ;; The numbers in a TJ array are subtracted, in thousandths of text space.
  ;; A reader that added them would open gaps where the document closed them.
  (let [tight (page-of "BT /F1 10 Tf 1 0 0 1 0 700 Tm [(A) 1000 (B)] TJ ET")
        loose (page-of "BT /F1 10 Tf 1 0 0 1 0 700 Tm [(A) -1000 (B)] TJ ET")
        x-of (fn [p] (:item/x (second (texts p))))]
    (is (< (x-of tight) (x-of loose))
        "a positive number pulls the next run left")
    (is (= 20.0 (- (x-of loose) (x-of tight)))
        "one em of a 10pt font each way, so twenty points apart")))

;; ── rotation ─────────────────────────────────────────────────────────────────

(deftest rotation-is-applied-to-the-size-and-to-the-marks
  (let [content (pdf/text-command {:x 100 :y 700 :text "turned" :size 10})
        raw (pdf/write-document [{:width 595 :height 842 :content content}])
        text (apply str (map char raw))
        turned (str/replace text "/Type /Page /Parent" "/Rotate 90 /Type /Page /Parent")
        parsed (pdf/parse (mapv #(bit-and (int %) 0xff) (.getBytes ^String turned "ISO-8859-1")))
        p (hpdf/page-at parsed 0)]
    (is (= 90 (:page/rotation p)))
    (is (= 842.0 (:page/width p)) "the long edge is across now")
    (is (= 595.0 (:page/height p)))
    (testing "and the mark moved with the page rather than staying put"
      (let [[t] (texts p)]
        (is (= 700.0 (:item/x t)))
        (is (= 100.0 (:item/y t)))))))

;; ── rules ────────────────────────────────────────────────────────────────────

(deftest a-filled-rectangle-becomes-a-rule
  (let [p (page-of (str "0 g " (pdf/rect-command {:x 50 :y 100 :width 200 :height 2
                                                  :fill? true})))
        [r] (filterv #(= :rule (:item/kind %)) (:page/items p))]
    (is (some? r))
    (is (= 50.0 (:item/x r)))
    (is (= 740.0 (:item/y r)) "842 − (100 + 2), the TOP edge in reader space")
    (is (= 200.0 (:item/width r)))
    (is (= 2.0 (:item/height r)))
    (is (= 1.0 (:item/ink r)) "0 g is black, which is full ink in reader terms")))

(deftest a-stroked-rectangle-is-not-a-rule
  ;; `re … S` outlines; only `f` fills. Treating both as filled turns every
  ;; table border into a solid black block.
  (let [p (page-of (pdf/rect-command {:x 50 :y 100 :width 200 :height 50 :fill? false}))]
    (is (empty? (filterv #(= :rule (:item/kind %)) (:page/items p))))))

(deftest grey-becomes-ink-on-the-way-in
  ;; PDF's 0 is black and this model's 0 is nothing. Converting at the
  ;; producer keeps the inverted convention out of a format-neutral value —
  ;; a second producer that read `:item/ink` as PDF grey would draw every
  ;; rule in negative.
  (let [p (page-of (str "0.75 g " (pdf/rect-command {:x 0 :y 0 :width 10 :height 10
                                                     :fill? true})))
        [r] (filterv #(= :rule (:item/kind %)) (:page/items p))]
    (is (= 0.25 (:item/ink r)) "1 − 0.75")))

;; ── widths ───────────────────────────────────────────────────────────────────

(deftest a-run-width-is-absent-when-the-font-did-not-ship-widths
  ;; `write-document` emits Helvetica with no /Widths. An estimate here
  ;; would be indistinguishable from a measurement downstream.
  (let [p (page-of (pdf/text-command {:x 10 :y 700 :text "unmeasured" :size 10}))
        [t] (texts p)]
    (is (nil? (:item/width t)))))

;; ── composite fonts ──────────────────────────────────────────────────────────

(defn- composite-doc
  "A hand-written PDF with a `/Type0` font, because `write-document` only
  emits Helvetica. `pdf.core` finds objects by scanning for `N G obj` and
  stream ends by searching for `endstream`, so neither a correct xref nor a
  correct `/Length` is needed here — which is what makes this fixture short
  enough to read."
  [tounicode]
  (let [text (str "%PDF-1.4\n"
                  "1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n"
                  "2 0 obj\n<< /Type /Pages /Kids [3 0 R] /Count 1 >>\nendobj\n"
                  "3 0 obj\n<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] "
                  "/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>\nendobj\n"
                  "4 0 obj\n<< /Length 48 >>\nstream\n"
                  "BT /F1 12 Tf 1 0 0 1 10 50 Tm <00410042> Tj ET\n"
                  "endstream\nendobj\n"
                  "5 0 obj\n<< /Type /Font /Subtype /Type0 /BaseFont /KozMin "
                  "/Encoding /Identity-H"
                  (if tounicode " /ToUnicode 6 0 R" "")
                  " >>\nendobj\n"
                  (if tounicode
                    (str "6 0 obj\n<< /Length 120 >>\nstream\n" tounicode
                         "\nendstream\nendobj\n")
                    "")
                  "trailer\n<< /Size 7 /Root 1 0 R >>\n%%EOF\n")]
    (pdf/parse (mapv #(bit-and (int %) 0xff)
                     #?(:clj (.getBytes ^String text "ISO-8859-1")
                        :cljs (map #(.charCodeAt text %) (range (count text))))))))

(deftest a-two-byte-code-is-read-through-tounicode
  ;; Identity-H codes are glyph ids, not characters. This is the path every
  ;; Japanese PDF takes, and reading it as one byte per code produces
  ;; plausible-looking Latin garbage rather than an obvious failure.
  (let [p (hpdf/page-at (composite-doc (str "begincmap\n"
                                            "2 beginbfchar\n"
                                            "<0041> <5951>\n"
                                            "<0042> <7D04>\n"
                                            "endbfchar\n"
                                            "endcmap"))
                        0)
        [t] (texts p)]
    (is (= "契約" (:item/text t)) "0x0041 0x0042, two codes, not four")
    (is (= 10.0 (:item/x t)))
    (is (= 50.0 (:item/y t)) "100 − 50")))

(deftest a-bfrange-increments-its-destination
  ;; The single-value form. Assuming the array form is the only one drops
  ;; every range a producer wrote the compact way.
  ;; The destination is a CODE POINT that increments, not a character that
  ;; means anything: `<30A2>` is ア and the next two are ィ and イ, which is
  ;; how katakana is laid out and is why this form exists at all.
  (is (= {0x41 "ア" 0x42 "ィ" 0x43 "イ"}
         (select-keys (hpdf/parse-tounicode
                       "beginbfrange\n<0041> <0043> <30A2>\nendbfrange")
                      [0x41 0x42 0x43])))
  (testing "and the array form maps elementwise"
    (is (= {0x41 "A" 0x42 "Z"}
           (select-keys (hpdf/parse-tounicode
                         "beginbfrange\n<0041> <0042> [<0041> <005A>]\nendbfrange")
                        [0x41 0x42])))))

(deftest a-composite-run-with-no-tounicode-is-a-frame-not-mojibake
  ;; Text nobody can read is worse than a marked region: it goes into search
  ;; results and into anything that quotes the page.
  (let [p (hpdf/page-at (composite-doc nil) 0)
        [f] (:page/items p)]
    (is (empty? (texts p)))
    (is (= :frame (:item/kind f)))
    (is (= :font/no-tounicode (:item/reason f)))
    (is (= "KozMin" (:item/label f)))
    (is (page/scanned? p) "and the page says a search cannot see it")))

;; ── documents ────────────────────────────────────────────────────────────────

(deftest a-document-is-its-pages-in-order
  (let [bytes (pdf/write-document
               [{:width 200 :height 100
                 :content (pdf/text-command {:x 10 :y 50 :text "one" :size 8})}
                {:width 200 :height 100
                 :content (pdf/text-command {:x 10 :y 50 :text "two" :size 8})}
                {:width 200 :height 100 :content ""}])
        d (hpdf/read-document bytes)]
    (is (= 3 (:document/count d)))
    (is (= ["one" "two"] (mapv #(first (page/text-of %)) (take 2 (:document/pages d)))))
    (is (= [0 1 2] (mapv :page/index (:document/pages d))))
    (testing "a page with no marks at all is empty, not scanned"
      (let [blank (nth (:document/pages d) 2)]
        (is (page/empty-page? blank))
        (is (not (page/scanned? blank)))
        (is (= [] (:document/scanned-pages d)))))))

(deftest a-page-with-marks-but-no-text-is-scanned
  ;; The state a search has to be able to explain. `app-preview.model` says
  ;; the same thing about a listing; this answers it from the marks.
  (let [p (page-of (pdf/rect-command {:x 0 :y 0 :width 100 :height 100 :fill? true}))]
    (is (page/scanned? p))
    (is (zero? (page/text-chars p)))))

(deftest an-unreadable-stream-is-a-page-with-no-marks-not-a-throw
  ;; A viewer that threw here would take the whole document down over one
  ;; page it could not read.
  (let [p (page-of "BT /F1 !!! garbage ~~~ Tf ((( ET")]
    (is (map? p))
    (is (= 595.0 (:page/width p)))))
