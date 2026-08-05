(ns hanmen.page-test
  (:require [clojure.test :refer [deftest is testing]]
            [hanmen.page :as page]))

(deftest rounding-is-the-same-arithmetic-everywhere
  ;; A digest over a rendering is only meaningful if two platforms agree.
  (is (= 1.235 (page/round 1.23456)))
  (is (= 1.0 (page/round 0.9999)))
  (is (= 0.0 (page/round ##NaN)) "not a throw, and not a NaN in the output")
  (is (= 0.0 (page/round ##Inf))))

(deftest an-unknown-item-kind-is-refused-at-construction
  (is (= :hanmen/unknown-item-kind
         (:type (try (page/item :annotation {})
                     (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                       (ex-data e)))))))

(deftest a-run-width-is-carried-only-when-it-was-measured
  (is (contains? (page/text-item {:x 0 :y 0 :size 10 :text "a" :width 5}) :item/width))
  (is (not (contains? (page/text-item {:x 0 :y 0 :size 10 :text "a"}) :item/width)))
  (testing "and a non-finite one is not a measurement"
    (is (not (contains? (page/text-item {:x 0 :y 0 :size 10 :text "a" :width ##NaN})
                        :item/width)))))

(deftest empty-and-scanned-are-different-answers
  ;; One has nothing on it; the other has something this cannot read. Sending
  ;; a reader to fix the wrong one is the reason they are separate.
  (let [blank (page/page {:width 10 :height 10 :items []})
        scan (page/page {:width 10 :height 10
                         :items [(page/frame-item {:x 0 :y 0 :width 10 :height 10
                                                   :label "Im1"})]})
        read (page/page {:width 10 :height 10
                         :items [(page/text-item {:x 0 :y 0 :size 2 :text "hi"})]})]
    (is (page/empty-page? blank))
    (is (not (page/scanned? blank)))
    (is (page/scanned? scan))
    (is (not (page/empty-page? scan)))
    (is (not (page/scanned? read)))))

(deftest fit-answers-the-two-modes-a-viewer-offers
  (let [p (page/page {:width 200 :height 100 :items []})]
    (is (= 1.0 (:fit/scale (page/fit p [200 100] :contain))))
    (is (= 0.5 (:fit/scale (page/fit p [100 100] :contain))) "limited by width")
    (is (= 0.5 (:fit/scale (page/fit p [200 50] :contain))) "limited by height")
    (is (= 1.0 (:fit/scale (page/fit p [200 50] :width))) "fit-to-width ignores height")
    (is (= 1.0 (:fit/scale (page/fit p [17 3] :actual))))
    (is (= 100.0 (:fit/width (page/fit p [100 100] :contain))))))

(deftest search-collapses-what-the-producer-broke
  ;; A content stream breaks a sentence wherever the pen moved; a reader
  ;; searching for what they saw did not type those breaks.
  (let [p (page/page {:width 10 :height 10
                      :items [(page/text-item {:x 0 :y 0 :size 2 :text "Master"})
                              (page/text-item {:x 0 :y 2 :size 2 :text "Agreement"})]})]
    (is (page/matches? p "master agreement"))
    (is (page/matches? p "MASTER   AGREEMENT"))
    (is (page/matches? p ""))
    (is (not (page/matches? p "termination")))))

(deftest a-document-reports-which-pages-a-search-cannot-see
  (let [d (page/document
           [(page/page {:index 0 :width 10 :height 10
                        :items [(page/text-item {:x 0 :y 0 :size 2 :text "one"})]})
            (page/page {:index 1 :width 10 :height 10
                        :items [(page/frame-item {:x 0 :y 0 :width 10 :height 10
                                                  :label "Im1"})]})])]
    (is (= 2 (:document/count d)))
    (is (= 3 (:document/text-chars d)))
    (is (= [1] (:document/scanned-pages d)))))

(deftest a-summary-is-what-a-listing-needs
  ;; `app-preview` renders a catalog of pages; this is the row.
  (let [s (page/summary (page/page {:index 2 :width 595 :height 842 :rotation 90
                                    :items [(page/text-item {:x 0 :y 0 :size 2
                                                             :text "hi"})]}))]
    (is (= 2 (:page/index s)))
    (is (= "Page 3" (:page/label s)))
    (is (= [595.0 842.0] (:page/size s)))
    (is (= 90 (:page/rotation s)))
    (is (= 2 (:page/text-chars s)))
    (is (false? (:page/scanned? s)))))
