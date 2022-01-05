(ns geo.crs.projections
  "Helper functions for identifying and manipulating
  Coordinate Reference Systems."
  (:require [clojure.string :as clojure.string])
  (:require-macros [geo.resource :as resource :refer [inline-resource]]))

(defn- projection?
  [prefix p]
  (let [match (re-matches (re-pattern "<([0-9]+)>\\s([.\\s\\w\\+\\_\\-=,<>]+)[<>\\s]*") p)]
    {(keyword (clojure.string/join [prefix (nth match 1)])) (nth match 2)}))

(defn prefix-projections
  [r prefix]
  (into {} (->> (clojure.string/split r "\n")
                (map #(projection? prefix %))
                (filter #(not= % {(keyword prefix) nil})))))

(def projections
  (merge
   (prefix-projections (resource/inline-resource "proj4/nad/epsg") "EPSG:")
   (prefix-projections (resource/inline-resource "proj4/nad/esri") "ESRI:")
   (prefix-projections (resource/inline-resource "proj4/nad/nad27") "NAD27:")
   (prefix-projections (resource/inline-resource "proj4/nad/nad83") "NAD83:")))
