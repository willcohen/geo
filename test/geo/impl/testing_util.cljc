(ns geo.impl.testing-util)

;; approximately function was
;; taken from clojure-expectations/clojure-test
;; https://github.com/clojure-expectations/clojure-test
;;
;; Copyright © 2018-2020 Sean Corfield, all rights reserved.
;; Distributed under the Eclipse Public License version 1.0.

(defn approximately
  "Given a value and an optional delta (default 0.001), return a predicate
  that expects its argument to be within that delta of the given value."
  ([^double v] (approximately v 0.001))
  ([^double v ^double d]
      (fn [x] (<= (- v (Math/abs d)) x (+ v (Math/abs d))))))
