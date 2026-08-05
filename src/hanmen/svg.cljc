(ns hanmen.svg
  "A `hanmen.page` drawn as SVG — safe to put inside a `default-src 'none'`
  page.

  ## Why SVG, and why that is not the contradiction it looks like

  The host this was built for serves uploaded bytes with
  `Content-Disposition: attachment` and keeps SVG out of its inline allowlist
  on purpose: an SVG is XML, it may contain `<script>`, and a browser runs it
  when it is a document rather than an `<img>` source. Emitting SVG to show a
  PDF therefore has to answer that objection rather than ignore it, and the
  answer is that these are two different things wearing one file extension.

  The refused one is **bytes a user uploaded**, whose element set is whatever
  they wrote. This one is **markup this namespace generates** from a value
  whose vocabulary is `hanmen.page/item-kinds` — three kinds, each emitting a
  fixed element with a fixed attribute list. There is no path from document
  content to an element name or an attribute name; content only ever becomes
  an escaped text node or an escaped attribute *value*.

  That is a property of the code rather than a promise about it, and
  `serialize` enforces it a second time at the boundary: an element not in
  `allowed-elements` or an attribute not in `allowed-attributes` throws
  instead of being written. A single check would be enough if the emitter
  never changed. Two are enough when it does.

  Inline in the page rather than a `data:` URI in an `<img>` for the same
  reason: a `data:` URI is a *load*, and a page whose CSP is `default-src
  'none'` has to be widened to permit it. Markup in the document is not a
  load and needs no widening. Deciding what a page may load is a decision on
  its own, not a side effect of adding a viewer.

  ## Nothing here picks a colour, a font or a size

  Everything paints in `currentColor` and carries a class. The host's
  stylesheet — its `--hig-*` tokens, its theme — decides what the ink looks
  like, which is the only way one rendering can be right in both light and
  dark. A document's own black is not the reader's black on a dark page, and
  a viewer that emitted `#000` would be unreadable in exactly the mode people
  read long documents in.

  Ink *density* is preserved: a 60% grey rule stays lighter than a black one,
  through `fill-opacity`, because that difference is the document's meaning
  and the absolute colour is not."
  (:require [clojure.string :as str]
            [hanmen.page :as page]))

(def allowed-elements
  "Every element this may emit. `foreignObject` is absent for the obvious
  reason and `style` for the less obvious one: a stylesheet in the fragment
  would be a second place styling is decided, and the host's is the only one."
  #{"svg" "g" "rect" "text" "title" "desc"})

(def allowed-attributes
  "Per element, so that `href` cannot appear anywhere at all — not on `text`
  by a typo and not on `svg` by a future edit. There is no element here that
  takes a URL, which is what makes the fragment inert."
  {"svg" #{"viewBox" "width" "height" "class" "role" "aria-label"
           "preserveAspectRatio" "xmlns"}
   "g" #{"class" "transform"}
   "rect" #{"x" "y" "width" "height" "class" "fill-opacity"}
   "text" #{"x" "y" "class" "font-size" "textLength" "lengthAdjust"
            "fill-opacity" "xml:space"}
   "title" #{}
   "desc" #{}})

;; ── serialization ────────────────────────────────────────────────────────────

(defn escape
  "XML text and attribute values.

  Both apostrophe forms, because the serializer below quotes with `\"` today
  and a future edit that switches to `'` should not open an injection. The
  cost of escaping one character nobody needs escaped is nothing."
  [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&apos;")))

(defn- number->str
  "`200` rather than `200.0`, and `1.235` rather than `1.2349999999`.

  Not cosmetic at this scale: a page of two thousand marks carries the two
  characters four times per mark, and every one of them is in a response
  body. Rounding first and dropping an integral tail is also what makes two
  runs byte-identical, which is what a digest over a rendering needs."
  [n]
  (let [r (page/round n 3)
        truncated #?(:clj (Math/floor r) :cljs (js/Math.floor r))]
    (if (== r truncated)
      (str #?(:clj (long r) :cljs (js/Math.trunc r)))
      (str r))))

(defn- attr-value [v]
  (cond
    (number? v) (number->str v)
    (keyword? v) (name v)
    :else (str v)))

(defn serialize
  "Hiccup to an SVG string, refusing anything outside the allowlists.

  The refusal is the point. This is called on markup this namespace just
  built, so a throw here means the emitter changed and the allowlist did
  not — which is exactly when a review would otherwise have to catch it."
  [node]
  (cond
    (string? node) (escape node)
    (number? node) (str node)
    (nil? node) ""
    (vector? node)
    (let [[tag & more] node
          tag (name tag)
          attrs (when (map? (first more)) (first more))
          children (if (map? (first more)) (rest more) more)]
      (when-not (contains? allowed-elements tag)
        (throw (ex-info (str "hanmen.svg would emit a disallowed element: " tag)
                        {:type :hanmen/disallowed-element :element tag})))
      (doseq [k (keys attrs)]
        (when-not (contains? (get allowed-attributes tag #{}) (name k))
          (throw (ex-info (str "hanmen.svg would emit a disallowed attribute: "
                               tag "/" (name k))
                          {:type :hanmen/disallowed-attribute
                           :element tag :attribute (name k)}))))
      (str "<" tag
           (apply str (for [[k v] (sort-by (comp name key) attrs)
                            :when (some? v)]
                        (str " " (name k) "=\"" (escape (attr-value v)) "\"")))
           (if (seq children)
             (str ">" (apply str (map serialize children)) "</" tag ">")
             "/>")))
    (seq? node) (apply str (map serialize node))
    :else (escape (str node))))

;; ── items ────────────────────────────────────────────────────────────────────

(defn emit-item
  "One mark as hiccup.

  Throws on an unknown kind rather than skipping it: a kind added to
  `hanmen.page/item-kinds` without a case here is a mark that vanishes from
  every rendering, and a page that is quietly missing something looks exactly
  like a page that is complete."
  [{:item/keys [kind x y width height text size ink label reason direction]}]
  (case kind
    :text
    (let [invisible? (= direction :invisible)]
      [:text (cond-> {:x x :y y :class (if invisible? "hanmen-text hanmen-text--invisible"
                                           "hanmen-text")
                      :font-size size
                      :xml:space "preserve"}
               ;; Only when the document shipped widths — see `hanmen.pdf`.
               ;; `spacingAndGlyphs` rather than `spacing`: with the reader's
               ;; font, adjusting gaps alone leaves a long run visibly short
               ;; or crowded, and the run's box is what has to match.
               (and width (pos? width)) (assoc :textLength width
                                               :lengthAdjust "spacingAndGlyphs")
               ;; The text layer under a scan. Present so it can be selected
               ;; and searched, invisible because the image above it is what
               ;; the reader is looking at — drawing both is double vision.
               invisible? (assoc :fill-opacity 0))
       (str text)])

    :rule
    [:rect (cond-> {:x x :y y :width width :height height :class "hanmen-rule"}
             ;; Density, not colour — see the ns docstring. A rule with no
             ;; ink recorded draws at full strength, which is what a page
             ;; that never issued a colour operator means.
             (number? ink) (assoc :fill-opacity ink))]

    :frame
    [:g {:class "hanmen-frame"}
     [:title (str label " — " (name (or reason :undecoded)))]
     [:rect {:x x :y y :width width :height height :class "hanmen-frame__box"}]]

    (throw (ex-info (str "hanmen.svg has no case for item kind: " kind)
                    {:type :hanmen/unhandled-item-kind :kind kind}))))

;; ── the page ─────────────────────────────────────────────────────────────────

(defn emit
  "A page as an SVG fragment, in hiccup.

  The `viewBox` is the page in its own units and `width`/`height` are what it
  occupies on screen, so scaling is the browser's job and the geometry never
  has to be recomputed for a second zoom level. A server that rendered at a
  fixed pixel size would be re-rendering on every resize, and a `re-render`
  is where a viewer stops feeling like paper."
  ([p] (emit p {}))
  ([{:page/keys [width height items label index] :as p}
    {:keys [fit-to class-name] :as opts}]
   (let [scale (or (when fit-to (:fit/scale (page/fit p fit-to :contain)))
                   (:scale opts)
                   1.0)]
     [:svg {:xmlns "http://www.w3.org/2000/svg"
            :viewBox (str "0 0 " (number->str width) " " (number->str height))
            :width (* width scale)
            :height (* height scale)
            :class (str "hanmen-page" (when class-name (str " " class-name)))
            :role "img"
            ;; Named rather than left to the reader's screen reader to
            ;; announce as "graphic": a document of forty pages read out as
            ;; forty graphics is a document nobody can navigate.
            :aria-label (str (or label (str "Page " (inc (or index 0))))
                             (when (page/scanned? p) " (scanned — no text)"))}
      (into [:g {:class "hanmen-page__marks"}] (map emit-item) items)])))

(defn ->svg
  "A page as an SVG string, allowlist-enforced."
  ([p] (->svg p {}))
  ([p opts] (serialize (emit p opts))))

(def stylesheet
  "What the host needs in ITS stylesheet for a rendering to look like a page.

  Shipped as a string rather than emitted into the fragment so that it lands
  in the host's cascade, under the host's tokens, and so a page containing
  forty renderings carries these rules once instead of forty times.

  No colour and no font family: `currentColor` takes the host's text colour
  and `font-family: inherit` takes its font, which is the only way this can
  be right in both themes. A host that wants the document's own serif sets
  `--hanmen-font` and nothing here has to know what it chose."
  (str ".hanmen-page{display:block;max-width:100%;height:auto;"
       "background:var(--hanmen-paper,transparent)}"
       ".hanmen-text{fill:currentColor;font-family:var(--hanmen-font,inherit);"
       "white-space:pre}"
       ".hanmen-rule{fill:currentColor}"
       ".hanmen-frame__box{fill:none;stroke:currentColor;stroke-opacity:.35;"
       "stroke-dasharray:4 3;stroke-width:1}"))
