(ns hanmen.page
  "版面 — one page, as placed marks in a coordinate space anybody can draw.

  ## What this is for

  `org-iso-pdf` answers what is *in* a document: the object graph, the page
  tree, the strings shown between `BT`/`ET`. It does not answer where any of
  it is, and a viewer is entirely a question of where. `pdf.core/extract-text`
  returns a vector of strings in content-stream order — enough to search,
  nothing to look at.

  So this namespace is the value in between: a page is a size, a rotation and
  a vector of **placed** items. Nothing here parses a format and nothing here
  draws; `hanmen.pdf` fills it and `hanmen.svg` draws it, and a second
  producer (an image, a `kasane` document, a Drive workbook) is a second
  producer rather than a second viewer.

  ## The coordinate space is the reader's, not the format's

  PDF user space has its origin at the bottom-left and y increasing upward.
  SVG, Canvas, CoreGraphics-in-UIKit and every raster buffer put it at the
  top-left with y increasing downward. One of those has to be the space this
  value is in, and it is the reader's: `[0 0]` is the top-left corner of the
  page as displayed, x grows right, y grows down, and the unit is the page's
  own (a PDF point, 1/72 inch).

  Doing the flip here rather than in each drawer is the whole point. A viewer
  that flips is a viewer that can flip twice, and a page drawn upside-down is
  a bug that only a human eye catches.

  ## Rotation is applied, not reported

  `/Rotate 90` on a page means the reader sees it turned. `app-preview.model`
  already learned that reporting the unrotated box lays out every thumbnail
  grid wrongly, so `:page/width` and `:page/height` here are the size **as
  seen**, and the marks inside are already turned to match. A consumer that
  wanted the unrotated box would have to un-rotate, which nothing wants.

  Everything here is pure and portable: no store, no clock, no host."
  (:require [clojure.string :as str]))

(def schema "kotoba-lang.hanmen.page.v1")

(def item-kinds
  "What a page can contain, and what each one means.

  A closed set. A drawer switches on this key exhaustively, so a kind added
  here without a case there is a mark that silently disappears — which is why
  `hanmen.svg/emit-item` throws on an unknown kind rather than skipping it.

  `:frame` is the honest one. An image, a form XObject or a shading is a
  region of the page whose *contents* this does not decode, and a viewer that
  drew nothing there would show a page that looks complete and is not. So the
  region is placed, named and marked as undrawn, and the drawer decides
  whether to outline it. Silence is the one thing it must not be."
  #{:text :rule :frame})

;; ── geometry ─────────────────────────────────────────────────────────────────

(defn- finite? [n]
  #?(:clj (and (number? n) (Double/isFinite (double n)))
     :cljs (and (number? n) (js/isFinite n))))

(defn round
  "To `places` decimals, as a double, so that two runs of the same input
  produce byte-identical output.

  Written out rather than taken from a formatter because the platforms
  disagree: `format \"%.2f\"` is JVM-only, and `.toFixed` returns a string
  with a locale-independent but *different* rounding rule at the halfway
  point. Multiplying by a power of ten and rounding is the same arithmetic
  everywhere, and determinism is what a digest over a rendering needs."
  ([n] (round n 3))
  ([n places]
   (if-not (finite? n)
     0.0
     (let [f (Math/pow 10 places)]
       (double (/ (Math/round (* (double n) f)) f))))))

(defn item
  "One placed mark.

  `x`/`y` is its top-left in reader space for a `:rule` and a `:frame`, and
  the **baseline start** for `:text` — which is not the same point and is the
  one every text renderer takes. Naming them the same would make a drawer
  that treats them alike look correct on a rectangle and be a font's ascent
  out on every line of prose."
  [kind attrs]
  (when-not (contains? item-kinds kind)
    (throw (ex-info (str "unknown item kind: " kind)
                    {:type :hanmen/unknown-item-kind :kind kind
                     :known (vec (sort item-kinds))})))
  (assoc attrs :item/kind kind))

(defn text-item
  "A run of text on one baseline.

  `width` is what the *document* says the run occupies, and it is optional
  because only a document that shipped its font's `/Widths` can say. When it
  is there a drawer can make the glyphs fill exactly that much space no
  matter which font it actually has; when it is not, the run starts in the
  right place and ends wherever the drawer's font puts it. Those are
  different qualities of answer and they are kept distinguishable rather than
  averaged into a guess."
  [{:keys [x y size text width font direction]}]
  (item :text (cond-> {:item/x (round x) :item/y (round y)
                       :item/size (round size)
                       :item/text (str text)}
                (finite? width) (assoc :item/width (round width))
                (seq font) (assoc :item/font (str font))
                direction (assoc :item/direction direction))))

(defn rule-item
  "A filled rectangle: a table border, an underline, a highlight.

  `ink` is **density, not colour**: 1.0 is as dark as this document goes and
  0.0 is nothing. It is deliberately not PDF's `/G`, where 0 is black — this
  value is in the reader's terms for the same reason the coordinates are, and
  a producer converts on the way in. Carrying the format's inverted
  convention into a format-neutral model is how a second producer ends up
  drawing every rule in negative."
  [{:keys [x y width height ink]}]
  (item :rule (cond-> {:item/x (round x) :item/y (round y)
                       :item/width (round width) :item/height (round height)}
                (finite? ink) (assoc :item/ink (round ink 3)))))

(defn frame-item
  "A region whose contents are not decoded — see `item-kinds`."
  [{:keys [x y width height label reason]}]
  (item :frame {:item/x (round x) :item/y (round y)
                :item/width (round width) :item/height (round height)
                :item/label (str label)
                :item/reason (or reason :undecoded)}))

;; ── the page ─────────────────────────────────────────────────────────────────

(defn page
  "A page value. `width`/`height` are as seen — rotation already applied."
  [{:keys [index width height rotation items label]}]
  {:page/schema schema
   :page/index (or index 0)
   :page/label (or label (str "Page " (inc (or index 0))))
   :page/width (round width)
   :page/height (round height)
   :page/rotation (or rotation 0)
   :page/items (vec items)})

(defn empty-page?
  "No marks at all. Distinct from a page whose marks are all `:frame`, which
  is a scanned page — one has nothing on it and the other has something this
  cannot draw, and telling a reader the wrong one sends them to fix the wrong
  thing."
  [p]
  (empty? (:page/items p)))

(defn text-of
  "Every text run, in the order the content stream placed them.

  Reading order is not claimed and the docstring says so rather than letting
  a caller find out from a two-column paper: content-stream order is the
  order the *producer* wrote marks in, which for most tools is close enough
  to reading order to search and nowhere near it to quote."
  [p]
  (into [] (comp (filter #(= :text (:item/kind %))) (map :item/text))
        (:page/items p)))

(defn text-chars [p] (reduce + 0 (map count (text-of p))))

(defn scanned?
  "Nothing to select and nothing to search: no text, but something there.

  The same predicate `app-preview.model/scanned?` applies to a page listing,
  answered here from the marks rather than from a reported count, so a viewer
  and a listing cannot disagree about which pages a search can see."
  [p]
  (and (zero? (text-chars p))
       (boolean (seq (:page/items p)))))

(defn summary [p]
  {:page/index (:page/index p)
   :page/label (:page/label p)
   :page/size [(:page/width p) (:page/height p)]
   :page/rotation (:page/rotation p)
   :page/items (count (:page/items p))
   :page/text-chars (text-chars p)
   :page/scanned? (scanned? p)})

(defn document
  "Pages plus what a listing needs to say about the whole of it."
  [pages]
  (let [pages (vec pages)]
    {:document/schema schema
     :document/pages pages
     :document/count (count pages)
     :document/text-chars (reduce + 0 (map text-chars pages))
     :document/scanned-pages (into [] (comp (filter scanned?) (map :page/index)) pages)}))

;; ── fitting ──────────────────────────────────────────────────────────────────

(defn fit
  "The scale that puts a page inside `[w h]`, and the size it comes out.

  `:contain` is the whole page visible, `:width` is fit-to-width — the two
  Preview actually offers. Zoom and pan are `kotoba-lang/canvaskit`'s
  subject and are deliberately not re-derived here; this is the one number a
  server-side render needs to choose an output size, which is a different
  question from what a gesture does to a viewport."
  ([p bounds] (fit p bounds :contain))
  ([{:page/keys [width height]} [w h] mode]
   (let [sx (if (pos? width) (/ (double w) width) 1.0)
         sy (if (pos? height) (/ (double h) height) 1.0)
         scale (case mode
                 :contain (min sx sy)
                 :width sx
                 :actual 1.0)]
     {:fit/scale (round scale 4)
      :fit/width (round (* width scale) 2)
      :fit/height (round (* height scale) 2)})))

;; ── text, for search ─────────────────────────────────────────────────────────

(defn- normalize [s] (str/lower-case (str/replace (str s) #"\s+" " ")))

(defn matches?
  "Whether `needle` appears in this page's text.

  Whitespace-collapsed and case-folded, because a content stream breaks a
  sentence wherever the producer moved the pen and a reader searching for
  what they saw on the page did not type those breaks."
  [p needle]
  (let [needle (normalize needle)]
    (or (str/blank? needle)
        (str/includes? (normalize (str/join " " (text-of p))) needle))))
