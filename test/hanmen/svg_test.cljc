(ns hanmen.svg-test
  "What the drawn fragment is allowed to be.

  Most of this file is about what does NOT come out, because the value of an
  allowlist is entirely in the cases it refuses."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hanmen.page :as page]
            [hanmen.svg :as svg]))

(defn- page-with [& items]
  (page/page {:index 0 :width 200 :height 100 :items items}))

;; ── injection ────────────────────────────────────────────────────────────────

(deftest document-text-cannot-become-markup
  ;; The whole reason this host refuses uploaded SVG. Content reaches the
  ;; output only as an escaped text node.
  (let [out (svg/->svg (page-with (page/text-item
                                   {:x 1 :y 1 :size 10
                                    :text "</text><script>fetch('/api/esign')</script>"})))]
    (is (not (str/includes? out "<script")))
    (is (str/includes? out "&lt;/text&gt;&lt;script&gt;"))
    (is (= 1 (count (re-seq #"<text " out))) "one text element, not two")))

(deftest a-frame-label-cannot-become-an-attribute
  ;; A `/XObject` name comes out of the file, so it is untrusted input that
  ;; reaches a `<title>`.
  (let [out (svg/->svg (page-with (page/frame-item
                                    {:x 0 :y 0 :width 10 :height 10
                                     :label "Im1\" onload=\"alert(1)"})))]
    ;; The word survives as text — it is in a `<title>` a reader can see.
    ;; What must not survive is the quote that would end the attribute it
    ;; sits in, so the test is on the syntax and not on the word.
    (is (not (str/includes? out "onload=\"")))
    (is (str/includes? out "&quot; onload=&quot;"))))

(deftest the-allowlist-refuses-rather-than-drops
  (testing "an element nobody may emit"
    (is (= :hanmen/disallowed-element
           (:type (try (svg/serialize [:script {} "x"])
                       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                         (ex-data e)))))))
  (testing "an attribute nobody may emit"
    (is (= :hanmen/disallowed-attribute
           (:type (try (svg/serialize [:rect {:href "https://example.test"}])
                       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                         (ex-data e))))))
    ;; Exactly one element may carry a URL, and only when the caller asked
    ;; for images. The default fragment loads nothing, which is what lets it
    ;; live inside `default-src 'none'` unchanged.
    (is (= #{"image"}
           (set (keep (fn [[el attrs]] (when (contains? attrs "href") el))
                      svg/allowed-attributes))))))

(deftest an-image-loads-nothing-until-the-host-says-where-from
  (let [p (page-with (page/image-item {:x 0 :y 0 :width 10 :height 10 :index 0
                                       :media-type "image/jpeg"}))]
    (testing "no href by default — the host has not decided its CSP"
      (let [out (svg/->svg p)]
        (is (not (str/includes? out "href")))
        (is (not (str/includes? out "<image")))
        ;; Outlined, like a frame: something is there and this cannot show it.
        (is (str/includes? out "hanmen-frame__box"))
        (is (str/includes? out "<title>image 0</title>"))))
    (testing "and the host's own path when it has"
      (let [out (svg/->svg p {:image-href (fn [{:keys [index]}]
                                            (str "/api/pages/0/images/" index))})]
        (is (str/includes? out "<image "))
        (is (str/includes? out "href=\"/api/pages/0/images/0\""))
        (is (str/includes? out "preserveAspectRatio=\"none\""))))))

(deftest an-image-href-that-leaves-this-origin-is-refused
  ;; The check that makes "no document content reaches a URL" a property
  ;; rather than a convention: even the HOST cannot point this off-origin.
  (let [p (page-with (page/image-item {:x 0 :y 0 :width 1 :height 1 :index 0}))]
    (doseq [bad ["https://example.test/x.png" "//example.test/x.png"
                 "javascript:alert(1)" "data:image/png;base64,AAA"
                 "\\\\example.test\\x.png" "x.png"]]
      (is (= :hanmen/foreign-image-href
             (:type (try (svg/->svg p {:image-href (constantly bad)})
                         (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                           (ex-data e)))))
          bad))))

(deftest an-unknown-item-kind-is-loud
  ;; A kind added to the model without a case here would otherwise vanish
  ;; from every rendering, and a page missing a mark looks complete.
  (is (= :hanmen/unhandled-item-kind
         (:type (try (svg/emit-item {:item/kind :annotation :item/x 0 :item/y 0})
                     (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                       (ex-data e)))))))

;; ── what it draws ────────────────────────────────────────────────────────────

(deftest the-viewbox-is-the-page-in-its-own-units
  ;; So the browser scales and the geometry is computed once. A server that
  ;; re-rendered per zoom level would be re-rendering on every resize.
  (let [out (svg/->svg (page-with) {:fit-to [400 400]})]
    (is (str/includes? out "viewBox=\"0 0 200 100\""))
    (is (str/includes? out "width=\"400\"") "contain: limited by width, ×2")
    (is (str/includes? out "height=\"200\""))))

(deftest a-measured-run-carries-its-width-and-an-unmeasured-one-does-not
  (let [measured (svg/->svg (page-with (page/text-item {:x 0 :y 10 :size 10
                                                        :text "ab" :width 12})))
        unmeasured (svg/->svg (page-with (page/text-item {:x 0 :y 10 :size 10
                                                          :text "ab"})))]
    (is (str/includes? measured "textLength=\"12\""))
    (is (str/includes? measured "lengthAdjust=\"spacingAndGlyphs\""))
    (is (not (str/includes? unmeasured "textLength")))
    (is (not (str/includes? unmeasured "lengthAdjust")))))

(deftest the-text-layer-of-a-scan-is-present-and-invisible
  ;; Selectable and searchable, not drawn on top of the image it describes.
  (let [out (svg/->svg (page-with (page/text-item {:x 0 :y 10 :size 10 :text "ocr"
                                                   :direction :invisible})))]
    (is (str/includes? out "ocr"))
    (is (str/includes? out "fill-opacity=\"0\""))
    (is (str/includes? out "hanmen-text--invisible"))))

(deftest nothing-here-picks-a-colour-or-a-font
  ;; The one way a rendering can be right in both light and dark.
  (let [out (str (svg/->svg (page-with
                             (page/text-item {:x 0 :y 1 :size 8 :text "x"})
                             (page/rule-item {:x 0 :y 0 :width 5 :height 1 :ink 0.5})
                             (page/frame-item {:x 0 :y 0 :width 5 :height 5 :label "Im1"})))
                svg/stylesheet)]
    (is (not (re-find #"#[0-9a-fA-F]{3,8}\b" out)) "no hex anywhere")
    (is (not (str/includes? out "font-family:\"")))
    (is (str/includes? svg/stylesheet "currentColor"))
    (is (str/includes? svg/stylesheet "font-family:var(--hanmen-font,inherit)"))))

(deftest ink-density-survives-and-colour-does-not
  (let [light (svg/->svg (page-with (page/rule-item {:x 0 :y 0 :width 5 :height 1
                                                     :ink 0.25})))
        full (svg/->svg (page-with (page/rule-item {:x 0 :y 0 :width 5 :height 1})))]
    (is (str/includes? light "fill-opacity=\"0.25\""))
    (is (not (str/includes? full "fill-opacity"))
        "no ink recorded means full strength, not transparent")))

(deftest a-page-names-itself-for-a-screen-reader
  ;; Forty pages announced as forty "graphic"s is a document nobody can
  ;; navigate.
  (let [plain (svg/->svg (page/page {:index 4 :width 10 :height 10
                                     :items [(page/text-item {:x 0 :y 1 :size 2
                                                              :text "hi"})]}))
        scan (svg/->svg (page-with (page/frame-item {:x 0 :y 0 :width 5 :height 5
                                                     :label "Im1"})))]
    (is (str/includes? plain "aria-label=\"Page 5\""))
    (is (str/includes? scan "(scanned — no text)"))))

(deftest the-same-page-renders-to-the-same-bytes
  ;; Attributes are emitted in a fixed order and numbers rounded the same
  ;; way on both platforms, so a digest over a rendering means something.
  (let [p (page-with (page/text-item {:x 1.23456 :y 2 :size 10 :text "a"})
                     (page/rule-item {:x 0 :y 0 :width 1 :height 1}))]
    (is (= (svg/->svg p) (svg/->svg p)))
    (is (str/includes? (svg/->svg p) "x=\"1.235\""))))
