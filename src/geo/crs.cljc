(ns geo.crs
  "Helper functions for identifying and manipulating
  Coordinate Reference Systems, and common JTS/JSTS builders."
  (:require [clojure.string :as clojure.string]
            #?(:clj [geo.impl.jts :as impl]
               :cljs [geo.impl.jsts :as impl])
            #?(:clj [clojure.java.io :as io]
               :cljs [cljs.reader :as cljs.reader :refer [read-string]])
            #?(:cljs [proj4])
            #?(:cljs [jsts]))
  #?(:clj
     (:import (org.locationtech.proj4j CoordinateReferenceSystem
                                       CoordinateTransform
                                       CoordinateTransformFactory
                                       CRSFactory
                                       ProjCoordinate)
              (org.locationtech.jts.geom Coordinate
                                         CoordinateSequence
                                         CoordinateSequenceFilter
                                         Geometry
                                         GeometryFactory
                                         PrecisionModel))
     :cljs (:require-macros [geo.crs :refer [inline-resource]])))



(def starts-with? ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of clojure.string/starts-with?."
  clojure.string/starts-with?)
(def includes? ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of clojure.string/includes?."
  clojure.string/includes?)

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

#?(:cljs
   (defn- projection?
     [prefix p]
     (let [match
           (re-matches
            (re-pattern "<([0-9]+)>\\s([.\\s\\w\\+\\_\\-=,]+)[<>\\s]*") p)]
       {(keyword (clojure.string/join [prefix (nth match 1)])) (nth match 2)})))

#?(:cljs
   (defn prefix-projections
     [r prefix]
     (into {} (->> (clojure.string/split r "\n")
                   (map #(projection? prefix %))
                   (filter #(not= % {(keyword prefix) nil}))))))

#?(:clj
   (defmacro inline-resource [resource-path]
     (slurp (clojure.java.io/resource resource-path)))
   :cljs
   (def projections
     (merge
      (prefix-projections (inline-resource "proj4/nad/epsg") "EPSG:")
      (prefix-projections (inline-resource "proj4/nad/esri") "ESRI:")
      (prefix-projections (inline-resource "proj4/nad/nad27") "NAD27:")
      (prefix-projections (inline-resource "proj4/nad/nad83") "NAD83:"))))

#?(:cljs
   (deftype CoordinateTransform [source target transform]))

#?(:cljs
   (deftype CoordinateReferenceSystem [name parameters]))

(def epsg-str? (partial re-matches #"EPSG:(\d+)"))
(def srid->epsg-str
  "Convert an SRID integer to EPSG string."
  (partial str "EPSG:"))

(defn epsg-str->srid
  "Converts EPSG string to SRID, if possible."
 [epsg]
  (let [match (epsg-str? epsg)]
    (assert match "Must be a valid EPSG string")
    (#?(:clj Integer/parseInt
        :cljs goog.string/parseInt) (last match))))


#?(:clj (def ^CoordinateTransformFactory ctf-factory
          (CoordinateTransformFactory.)))

#?(:clj (def ^CRSFactory crs-factory
          (CRSFactory.)))

(def valid-crs-prefixes ["EPSG" "ESRI" "NAD83" "NAD27" "WORLD"])

(defn crs-name?
  "Check if input is a valid CRS name accepted by proj4j.

  Accepted CRS names are in the forms:
  EPSG:xxxx, ESRI:xxxx, NAD83:xxxx, NAD27:xxx, or WORLD:xxxx."
  [crs-str]
  (some (fn [prefix] (clojure.string/starts-with? crs-str (str prefix ":")))
        valid-crs-prefixes))

(defn proj4-str?
  "Check if input appears to be a proj4 string"
  [crs-str]
  (clojure.string/includes? crs-str "+proj="))

(defn- create-crs-parameters
  #?(:clj [^String c]
     :cljs [^js/string c])
  #?(:clj (.createFromParameters crs-factory "" c)
     :cljs (->CoordinateReferenceSystem "" c)))

(defn- create-crs-name
  [c]
  #?(:clj (.createFromName crs-factory c)
     :cljs (->CoordinateReferenceSystem c ((keyword c) projections))))

(defn- create-crs-number
  [c]
  (let [valid? (not= c 0)]
    (assert valid? "Cannot create CRS with EPSG 0")
    (create-crs-name (srid->epsg-str c))))

(defn get-name
  "Get the name of a coordinate reference system."
  [^CoordinateReferenceSystem c]
  #?(:clj (.getName c)
     :cljs (.-name c)))

(defn get-parameter-string
  "Get the proj string from an existing coordinate reference system."
  [^CoordinateReferenceSystem c]
  (clojure.string/trim (#?(:clj .getParameterString
                           :cljs .-parameters) c)))

(defn get-parameters
  "Get the proj parameters from an existing coordinate reference system."
  [^CoordinateReferenceSystem c]
  #?(:clj (.getParameters c)
     :cljs (clojure.string/split (get-parameter-string c) #" +")))

(defn ^CoordinateReferenceSystem get-source-crs
  "Get the source coordinate reference system of a transform."
  [^CoordinateTransform t]
  #?(:clj (.getSourceCRS t)
     :cljs (.-source t)))

(defn ^CoordinateReferenceSystem get-target-crs
  "Get the source coordinate reference system of a transform."
  [^CoordinateTransform t]
  #?(:clj (.getTargetCRS t)
     :cljs (.-target t)))

(defprotocol Transformable
  (^CoordinateReferenceSystem create-crs [this]
   "Create a CRS system. If given an integer or long, assume it is an EPSG code.
    If given a valid CRS name or proj4 string, use that as the CRS identifier.
    If given a proj4j CoordinateReferenceSystem, return that.")
  #?(:clj (^GeometryFactory get-geometry-factory [this]
           "Creates a JTS GeometryFactory from a given CRS identifier.")
     :cljs (^js/jsts.geom.GeometryFactory get-geometry-factory [this]
            "Creates a JSTS GeometryFactory from a given CRS identifier."))
  #?(:clj (^Integer get-srid [this]
           "Attempt to get the SRID for a CRS identifier.
           If unable, return 0.")
     :cljs (get-srid [this]
            "Attempt to get the CRS for a CRS identifier.
            If unable, return 0."))
  (^CoordinateTransform create-transform [this] [this tgt])
  #?(:clj (^Geometry ^:private
           transform-helper [this g] [this g c] [this g c gf]
           "An internal helper to coordinate logic of crs/transform-geom.")
     :cljs (^js/jsts.geom.Geometry ^:private
            transform-helper [this g] [this g c] [this g c gf]
            "An internal helper to coordinate logic of crs/transform-geom.")))

(defn set-srid
  "Sets a geometry's SRID to a new value, and returns that geometry."
  #?(:clj [^Geometry geom srid]
     :cljs [^js/jsts.geom.Geometry geom srid])
  (if (= (get-srid geom) (get-srid srid))
      geom
      #?(:clj (.createGeometry (get-geometry-factory srid) geom)
         :cljs (case (.getGeometryType geom)
                 "Point" (impl/point (.getCoordinate geom)
                                     (get-geometry-factory srid))
                 "MultiPoint" (impl/multi-point (.-_geometries geom)
                                                (get-geometry-factory srid))
                 "LineString" (impl/linestring (.getCoordinates geom)

                                               (get-geometry-factory srid))
                 "LinearRing" (impl/linear-ring (.getCoordinates geom)
                                                (get-geometry-factory srid))
                 "MultiLineString" (impl/multi-linestring
                                    (.-_geometries geom)
                                    (get-geometry-factory srid))
                 "Polygon" (impl/polygon (.-_shell geom)
                                         (get-geometry-factory srid)
                                         (.-_holes geom))
                 "MultiPolygon" (impl/multi-polygon (.-_geometries geom)
                                                    (get-geometry-factory srid))
                 "GeometryCollection" (impl/geometry-collection
                                       (.-_geometries geom)
                                       (get-geometry-factory srid))))))

#?(:clj
   (defn- ^Coordinate transform-coord
     "Transforms a coordinate using a proj4j transform."
     [^Coordinate coord ^CoordinateTransform transform]
     (-> (.transform transform
                     (ProjCoordinate. (.x coord) (.y coord) (.z coord))
                     (ProjCoordinate.))
         (#(Coordinate. (.x ^ProjCoordinate %)
                        (.y ^ProjCoordinate %)
                        (.z ^ProjCoordinate %)))))
   :cljs
   (defn- ^js/jsts.geom.Coordinate transform-coord
     "Transforms a coordinate using a proj4js transform."
     [^js/jsts.geom.Coordinate coord ^CoordinateTransform transform]
     ((.-transform transform) coord)))


(defn- transform-coord-seq-item
  "Transforms one item in a CoordinateSequence using a proj4j transform."
  #?(:clj [^CoordinateSequence cseq ^Integer i ^CoordinateTransform transform]
     :cljs [^js/jsts.geom.CoordinateSequence cseq
            ^number i ^CoordinateTransform transform])
  (let [coordinate (.getCoordinate cseq i)
        transformed (transform-coord coordinate transform)]
    #?(:clj (do (.setOrdinate cseq i 0 (.x transformed))
                (.setOrdinate cseq i 1 (.y transformed)))
       :cljs (do (.setOrdinate cseq i 0 (.-x transformed))
                 (.setOrdinate cseq i 1 (.-y transformed))
                 (.setOrdinate cseq i 2 (.-z transformed))
                 cseq))))


#?(:clj
   (defn- ^CoordinateSequenceFilter transform-coord-seq-filter
  "Implement JTS's CoordinateSequenceFilter, to be applied to a
  Geometry using tf and transform-geom."
     [transform]
     (reify CoordinateSequenceFilter
       (filter [_ seq i]
         (transform-coord-seq-item seq i transform))
       (isDone [_]
         false)
       (isGeometryChanged [_]
         true)))
   :cljs
   (defn- transform-coord-seq-filter
     [transform]
     (js-obj "filter" (fn [seq i] (transform-coord-seq-item seq i transform))
             "isDone" (fn [] false)
             "isGeometryChanged" (fn [] true)
             "interfaces_" (array jsts.geom.CoordinateSequenceFilter)
      )))

(defn- geom-srid?
  "Check if a Geometry has a valid SRID."
  #?(:clj [^Geometry g]
     :cljs [^js/jsts.geom.Geometry g])
  (let [geom-srid (get-srid g)]
    (and (not= 0 geom-srid)
         (not (nil? geom-srid)))))

(defn- tf
  "Transform a Geometry by applying CoordinateTransform to the Geometry.
  When the target CRS has an SRID, set the geometry's SRID to that."
  #?(:clj [^Geometry g ^CoordinateTransform transform ^GeometryFactory gf]
     :cljs [^js/jsts.geom.Geometry g ^CoordinateTransform transform
            ^js/jsts.geom.GeometryFactory gf])
  (let #?(:clj [^Geometry g (.copy g)]
          :cljs [^js/jsts.geom.Geometry g (.copy g)])
    (.apply g (transform-coord-seq-filter transform))
    (set-srid g gf)))

#?(:clj
(extend-protocol Transformable
  Long
  (create-crs [this] (create-crs-number this))
  (get-geometry-factory [this] (get-geometry-factory (get-srid this)))
  (get-srid [this] (int this))
  (create-transform [this tgt]
    (create-transform (create-crs this) (create-crs tgt)))
  (transform-helper
    ([this g] (transform-helper (get-srid this) g))
    ([this g c] (transform-helper (get-srid this) g c))
    ([this g c1 f] (transform-helper (get-srid this) g c1 f)))

  Integer
  (create-crs [this] (create-crs-number this))
  (get-geometry-factory [this] (GeometryFactory. impl/pm this))
  (get-srid [this] this)
  (create-transform [this tgt]
    (create-transform (create-crs this) (create-crs tgt)))
  (transform-helper
    ([this g]
     (transform-helper (get-geometry-factory this) g (create-transform g this)))
    ([this g c]
     (if (geom-srid? g)
       (transform-helper (get-geometry-factory c) g (create-transform g c))
       (transform-helper (get-geometry-factory c) g (create-transform this c))))
    ([this g c2 f]
     (transform-helper (get-geometry-factory f) g (create-transform this c2))))

  String
  (create-crs [this] (cond (crs-name? this)
                           (create-crs-name this)
                           (proj4-str? this)
                           (create-crs-parameters this)))
  (get-geometry-factory [this] (get-geometry-factory (get-srid this)))
  (get-srid [this] (let [epsg? (epsg-str? this)]
                     (if epsg?
                       (read-string (last epsg?))
                       0)))
  (create-transform [this tgt]
    (create-transform (create-crs this) (create-crs tgt)))
  (transform-helper
    ([this g] (transform-helper (create-crs this) g))
    ([this g c] (transform-helper (create-crs this) g c))
    ([this g c1 f] (transform-helper (create-crs this) g c1 f)))

  CoordinateReferenceSystem
  (create-crs [this] this)
  (get-geometry-factory [this] (get-geometry-factory (get-srid this)))
  (get-srid [this] (get-srid (get-name this)))
  (create-transform [this tgt]
    (.createTransform ctf-factory this (create-crs tgt)))
  (transform-helper
    ([this g]
     (transform-helper
      (get-geometry-factory this) g (create-transform (create-crs g) this)))
    ([this g c2]
     (transform-helper
      (get-geometry-factory c2) g (create-transform this (create-crs c2))))
    ([this g c2 f]
     (transform-helper (get-geometry-factory f) g (create-transform this c2))))

  Geometry
  (create-crs [this] (create-crs (get-srid this)))
  (get-geometry-factory [this] (.getFactory this))
  (get-srid [this] (.getSRID this))
  (create-transform [this tgt]
    (create-transform (create-crs this) (create-crs tgt)))
  (transform-helper
    ([this t]
     (do (assert (geom-srid? this)
                 "Geometry must have a valid SRID to generate a transform.")
         (transform-helper (get-geometry-factory t) this t)))
    ([this c1 c2 gf]
     (transform-helper
      (get-geometry-factory gf) this (create-transform c1 c2))))

  CoordinateTransform
  (create-transform [this] this)
  (get-geometry-factory [this] (get-geometry-factory (get-target-crs this)))
  (transform-helper
    ([this g]
     (transform-helper (get-geometry-factory this) g this))
    ([this g factory]
     (transform-helper (get-geometry-factory factory) g this)))

  GeometryFactory
  (create-crs [this] (create-crs (get-srid this)))
  (get-geometry-factory [this] this)
  (get-srid [this] (.getSRID this))
  (create-transform [this tgt]
    (create-transform (create-crs this) (create-crs tgt)))
  (transform-helper
    ([this g]
     (transform-helper this g (create-transform g this)))
    ([this g t]
     (if (instance? CoordinateTransform t)
       ;; Base case: GeometryFactory, Geometry, CoordinateTransform
       (if (.equals (get-source-crs t)
                    (get-target-crs t))
         (set-srid g this)
         (tf g t this))
       (transform-helper t g this)))
    ([this g c1 c2]
     (transform-helper this g (create-transform c1 c2)))))

   :cljs
   (extend-protocol Transformable
     number
     (create-crs [this] (create-crs-number this))
     (get-geometry-factory [this]
       (impl/geometry-factory ^js/jsts.geom.PrecisionModel impl/pm this))
     (get-srid [this] this)
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this g]
        (transform-helper (get-geometry-factory this) g
                          (create-transform g this)))
       ([this g c]
        (if (geom-srid? g)
          (transform-helper (get-geometry-factory c) g (create-transform g c))
          (transform-helper (get-geometry-factory c) g
                            (create-transform this c))))
       ([this g c2 f]
        (transform-helper (get-geometry-factory f) g
                          (create-transform this c2))))

     string
     (create-crs [this] (cond (crs-name? this)
                              (create-crs-name this)
                              (proj4-str? this)
                              (create-crs-parameters this)))
     (get-geometry-factory [this] (get-geometry-factory (get-srid this)))
     (get-srid [this] (let [epsg? (epsg-str? this)]
                        (if epsg?
                          (read-string (last epsg?))
                          0)))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this g] (transform-helper (create-crs this) g))
       ([this g c] (transform-helper (create-crs this) g c))
       ([this g c1 f] (transform-helper (create-crs this) g c1 f)))

     CoordinateReferenceSystem
     (create-crs [this] this)
     (get-geometry-factory [this] (get-geometry-factory (get-srid this)))
     (get-srid [this] (get-srid (get-name this)))
     (create-transform [this tgt]
       (->CoordinateTransform
        this (create-crs tgt)
        (fn [coord] (.forward
                     ^js/proj4 (proj4.
                      (get-parameter-string this)
                      (get-parameter-string (create-crs tgt))) coord))))
     (transform-helper
       ([this g]
        (transform-helper
         (get-geometry-factory this) g (create-transform (create-crs g) this)))
       ([this g c2]
        (transform-helper
         (get-geometry-factory c2) g (create-transform this (create-crs c2))))
       ([this g c2 f]
        (transform-helper (get-geometry-factory f) g
                          (create-transform this c2))))

     jsts.geom.Point
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.MultiPoint
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.LineString
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.LinearRing
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.MultiLineString
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.Polygon
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.MultiPolygon
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.GeometryCollection
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     jsts.geom.Geometry
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] (.getFactory this))
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this t]
        (do (assert (geom-srid? this)
                    "Geometry must have a valid SRID to generate a transform.")
            (transform-helper (get-geometry-factory t) this t)))
       ([this c1 c2 gf]
        (transform-helper
         (get-geometry-factory gf) this (create-transform c1 c2))))

     CoordinateTransform
     (create-transform [this] this)
     (get-geometry-factory [this] (get-geometry-factory (get-target-crs this)))
     (transform-helper
       ([this g]
        (transform-helper (get-geometry-factory this) g this))
       ([this g factory]
        (transform-helper (get-geometry-factory factory) g this)))

     jsts.geom.GeometryFactory
     (create-crs [this] (create-crs (get-srid this)))
     (get-geometry-factory [this] this)
     (get-srid [this] (.getSRID this))
     (create-transform [this tgt]
       (create-transform (create-crs this) (create-crs tgt)))
     (transform-helper
       ([this g]
        (transform-helper this g (create-transform g this)))
       ([this g t]
        (if (instance? CoordinateTransform t)
          ;; Base case: GeometryFactory, Geometry, CoordinateTransform
          (if (goog.string.caseInsensitiveEquals
               (.-parameters (get-source-crs t))
               (.-parameters (get-target-crs t)))
            (set-srid g this)
            (tf g t this))
          (transform-helper t g this)))
       ([this g c1 c2]
        (transform-helper this g (create-transform c1 c2))))))


(def default-srid #?(:clj (int 4326)
                     :cljs 4326))

#?(:clj (def ^GeometryFactory gf-wgs84 (get-geometry-factory default-srid))
   :cljs (def ^js/jsts.geom.GeometryFactory gf-wgs84 (get-geometry-factory default-srid)))

#?(:cljs (def ^js/jsts.geom.CoordinateSequenceFactory csf-wgs84
           (impl/get-coordinate-sequence-factory gf-wgs84)))

(defn transform-geom
  "Transforms a Geometry to a different Coordinate Reference System.
   Takes a Geometry as a first argument, and either one, two, or three
   Transformables as additional arguments. When the second argument is
   a GeometryFactory, use that factory to construct the new Geometry.
   When only a target CRS is provided, use the Geometry's internal
   SRID as the source CRS.

   The most efficient way to call this is with its base case of
   (transform-geom Geometry GeometryFactory CoordinateTransform).

   All other argument calls to this function ultimately reduce
   down to that base case."
  ([g t]
   (transform-helper t g))
  ([g c1 c2]
   (transform-helper c1 g c2))
  ([g c1 c2 geometry-factory]
   (transform-helper c1 g c2 geometry-factory)))

