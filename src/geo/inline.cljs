(ns geo.inline
  "Helper functions for identifying and manipulating
  Coordinate Reference Systems."
  (:require-macros [geo.resource :as resource :refer [inline-resource]]))

(def projection (resource/inline-resource "proj4/nad/epsg"))
