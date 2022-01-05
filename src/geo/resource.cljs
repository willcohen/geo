(ns geo.resource
  "Helper functions for identifying and manipulating
  Coordinate Reference Systems.")

(defmacro inline-resource [resource-path]
  (slurp (clojure.java.io/resource resource-path)))
