(ns geo.h3
  "Working with H3."
  (:require [geo.crs :as crs]
            [geo.spatial :as spatial]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as geo.jsts])
            [clojure.string :as string]
            [clojure.walk :as walk]
            #?(:clj [clojure.math.numeric-tower :as numeric-tower])
            #?(:cljs [h3-js])
            #?(:cljs [jsts]))
  #?(:clj
     (:import (ch.hsr.geohash GeoHash)
              (com.uber.h3core AreaUnit H3Core LengthUnit)
              (com.uber.h3core.util GeoCoord)
              (geo.spatial Point Shapelike)
              (org.locationtech.jts.geom Geometry LinearRing MultiPolygon Polygon)
              (org.locationtech.spatial4j.shape.impl RectangleImpl))))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

#?(:clj (def ^H3Core h3-inst (H3Core/newInstance)))

(def ^:internal safe-polyfill-hexagon-maximum
  "Lowering this will reduce the likelihood of H3's polyfill crashing, but
  will cause this library's polyfill to run more slowly due to more frequent subdivision"
  60000)

(def ^:internal safe-uncompact-hexagon-maximum
  "Lowering this will reduce the likelihood of this library's internal safe-uncompact
   throwing a heap error, but will make this library's polyfill more likely
   to return an uncompacted set"
  5000000)

#?(:clj
   (defn- long->string
     "Convert a long representation of an H3 cell to a string."
     [^Long h]
     (.h3ToString h3-inst h)))

#?(:clj
   (defn- string->long
     "Convert a string representation of an H3 cell to a long."
     [^String h]
     (.stringToH3 h3-inst h)))

#?(:clj
   (defn- h3->pt-long
     "Long helper to return a GeoCoord of the center point of a cell."
     [^Long h]
     (.h3ToGeo h3-inst h)))

#?(:clj
   (defn- h3->pt-string
     "String helper to return a GeoCoord of the center point of a cell."
     [^String h]
     (.h3ToGeo h3-inst h))
   :cljs
   (defn h3->pt
     [^js/string h]
     (h3-js/h3ToGeo h)))

#?(:clj
   (defn- get-resolution-string
     "String helper to return the resolution of a cell."
     [^String h]
     (.h3GetResolution h3-inst h))
   :cljs
   (defn get-resolution
     [^js/String h]
     (h3-js/h3GetResolution h)))

#?(:clj
    (defn- get-resolution-long
      "Long helper to return the resolution of a cell."
      [^Long h]
      (.h3GetResolution h3-inst h)))

#?(:clj
   (defn- get-faces-string
     "String helper to return the icosahedron faces intersected by a cell, represented
  by integers 0-19."
     [^String h]
     (into [] (.h3GetFaces h3-inst h)))
   :cljs
   (defn get-faces
     [^js/String h]
     (h3-js/h3GetFaces h)))

#?(:clj
   (defn- get-faces-long
     "Long helper to return the icosahedron faces intersected by a cell, represented
  by integers 0-19."
     [^Long h]
     (into [] (.h3GetFaces h3-inst h))))

#?(:clj
   (defn- k-ring-string
     "String helper to return a list of neighboring indices in all directions for 'k' rings."
     [^String h ^Integer k]
     (.kRing h3-inst h k))
   :cljs
   (defn k-ring
     [^js/String h ^js/numeric k]
     (h3-js/kRing h k)))

#?(:clj
   (defn- k-ring-long
     "Long helper to return a list of neighboring indices in all directions for 'k' rings."
     [^Long h ^Integer k]
     (.kRing h3-inst h k)))

#?(:clj
   (defn- k-ring-distances-string
     "String helper to return a list of neighboring indices in all directions for 'k' rings,
  ordered by distance from the origin index."
     [^String h ^Integer k]
     (.kRingDistances h3-inst h k))
   :cljs
   (defn k-ring-distances
     [^js/String h ^js/numeric k]
     (h3-js/kRingDistances h k)))

#?(:clj
   (defn- k-ring-distances-long
     "String helper to return a list of neighboring indices in all directions for 'k' rings,
  ordered by distance from the origin index."
     [^Long h ^Integer k]
     (.kRingDistances h3-inst h k)))

#?(:clj
   (defn- to-jts-common
     "Convert a geo boundary to JTS Polygon."
     [g]
     (as-> g v
       (into [] v)
       (conj v (first v))
       (map #(jts/coordinate (spatial/longitude %) (spatial/latitude %)) v)
       (jts/linear-ring v)
       (jts/polygon v)))
   :cljs
   (defn- to-jsts-common
     [g]
     (as-> g v
       (into [] v)
       (conj v (first v))
       (map #(geo.jsts/coordinate (last %) (first %)) v)
       (geo.jsts/linear-ring v)
       (geo.jsts/polygon v))))

#?(:clj
   (defn- to-jts-string
     "String helper for: given an H3 identifier, return a Polygon of that cell."
     [^String h]
     (to-jts-common (.h3ToGeoBoundary h3-inst h)))
   :cljs
   (defn to-jsts
     [^js/String h]
     (to-jsts-common (h3-js/h3ToGeoBoundary h))))

#?(:clj
   (defn- to-jts-long
     "Long helper for: given an H3 identifier, return a Polygon of that cell."
     [^Long h]
     (to-jts-common (.h3ToGeoBoundary h3-inst h))))

#?(:clj
   (defn- edge-string
     "String helper for: given both 'from' and 'to' cells, get a unidirectional edge index."
     [^String from ^String to]
     (.getH3UnidirectionalEdge h3-inst from to))
   :cljs
   (defn edge
     [^js/String from ^js/String to]
     (h3-js/getH3UnidirectionalEdge from to)))

#?(:clj
   (defn- edge-long
     "Long helper for: given both 'from' and 'to' cells, get a unidirectional edge index."
     [^Long from ^Long to]
     (.getH3UnidirectionalEdge h3-inst from to)))

#?(:clj
   (defn- edge-origin-string
     "String helper for: given a unidirectional edge, get its origin."
     [^String edge]
     (.getOriginH3IndexFromUnidirectionalEdge h3-inst edge))
   :cljs
   (defn edge-origin
     [^js/String edge]
     (h3-js/getOriginH3IndexFromUnidirectionalEdge edge)))

#?(:clj
   (defn- edge-origin-long
     "Long helper for: given a unidirectional edge, get its origin."
     [^Long edge]
     (.getOriginH3IndexFromUnidirectionalEdge h3-inst edge)))

#?(:clj
   (defn- edge-destination-string
     "String helper for: given a unidirectional edge, get its destination."
     [^String edge]
     (.getDestinationH3IndexFromUnidirectionalEdge h3-inst edge))
   :cljs
   (defn edge-destination
     [^js/String edge]
     (h3-js/getDestinationH3IndexFromUnidirectionalEdge edge)))

#?(:clj
   (defn- edge-destination-long
     "Long helper for: given a unidirectional edge, get its destination."
     [^Long edge]
     (.getDestinationH3IndexFromUnidirectionalEdge h3-inst edge)))

#?(:clj
   (defn- edges-string
     "String helper to get all edges originating from an index."
     [^String cell]
     (into [] (.getH3UnidirectionalEdgesFromHexagon h3-inst cell)))
   :cljs
   (defn edges
     [^js/String cell]
     (into [] (h3-js/getH3UnidirectionalEdgesFromHexagon cell))))

#?(:clj
   (defn- edges-long
     "Long helper to get all edges originating from an index."
     [^Long cell]
     (into [] (.getH3UnidirectionalEdgesFromHexagon h3-inst cell))))

#?(:clj
   (defn- edge-boundary-string
     "String helper to get coordinates representing the edge."
     [^String edge]
     (into [] (.getH3UnidirectionalEdgeBoundary h3-inst edge)))
   :cljs
   (defn edge-boundary
     [^js/String edge]
     (into [] (h3-js/getH3UnidirectionalEdgeBoundary edge))))

#?(:clj
   (defn- edge-boundary-long
     "Long helper to get coordinates representing the edge."
     [^Long edge]
     (into [] (.getH3UnidirectionalEdgeBoundary h3-inst edge))))

#?(:clj
   (defn- pentagon?-string
     "String helper to check if an index is a pentagon"
     [^String cell]
     (.h3IsPentagon h3-inst cell))
   :cljs
   (defn pentagon?
     [^js/String cell]
     (h3-js/h3IsPentagon cell)))

#?(:clj
   (defn- pentagon?-long
     "Long helper to check if an index is a pentagon"
     [^Long cell]
     (.h3IsPentagon h3-inst cell)))

#?(:clj
   (defn- is-valid?-string
     "String helper to check if an index is valid"
     [^String cell]
     (.h3IsValid h3-inst cell))
   :cljs
   (defn is-valid?
     [^js/String cell]
     (h3-js/h3IsValid cell)))

#?(:clj
   (defn- is-valid?-long
     "Long helper to check if an index is valid"
     [^Long cell]
     (.h3IsValid h3-inst cell)))

#?(:clj
   (defn- neighbors?-string
     "String helper to check if cells are neighbors"
     [^String c1 ^String c2]
     (.h3IndexesAreNeighbors h3-inst c1 c2))
   :cljs
   (defn neighbors?
     [^js/String c1 ^js/String c2]
     (h3-js/h3IndexesAreNeighbors c1 c2)))

#?(:clj
   (defn- neighbors?-long
     "String helper to check if cells are neighbors"
     [^Long c1 ^Long c2]
     (.h3IndexesAreNeighbors h3-inst c1 c2)))

#?(:clj
   (defn- h3-distance-string
     "String helper to return the distance in number of h3 cells"
     [^String c1 ^String c2]
     (.h3Distance h3-inst c1 c2))
   :cljs
   (defn h3-distance
     [^js/String c1 ^js/String c2]
     (h3-js/h3Distance c1 c2)))

#?(:clj
   (defn- h3-distance-long
     "Long helper to return the distance in number of h3 cells"
     [^Long c1 ^Long c2]
     (.h3Distance h3-inst c1 c2)))

#?(:clj
   (defn- h3-line-string
     "String helper to return the line of indexes between cells"
     [^String c1 ^String c2]
     (.h3Line h3-inst c1 c2))
   :cljs
   (defn h3-line
     [^js/String c1 ^js/String c2]
     (h3-js/h3Line c1 c2)))

#?(:clj
   (defn- h3-line-long
     "Long helper to return the line of indexes between cells"
     [^Long c1 ^Long c2]
     (.h3Line h3-inst c1 c2)))

#?(:clj
   (defn- h3-to-center-child-string
     "String helper to return the center child at the given resolution"
     [^String cell ^Integer child-res]
     (.h3ToCenterChild h3-inst cell child-res))
   :cljs
   (defn h3-to-center-child
     [^js/String cell ^js/numeric child-res]
     (h3-js/h3ToCenterChild cell child-res)))

#?(:clj
   (defn- h3-to-center-child-long
     "String helper to return the center child at the given resolution"
     [^Long cell ^Integer child-res]
     (.h3ToCenterChild h3-inst cell child-res)))

#?(:clj (defprotocol H3Index
          (to-string [this] "Return index as a string.")
          (to-long [this] "Return index as a long.")
          (h3->pt [this] "Return a GeoCoord of the center point of a cell.")
          (get-resolution [this] "Return the resolution of a cell.")
          (get-faces [this] "Return the icosahedron faces intersected by a cell, represented by integers 0-19.")
          (k-ring [this k] "Return a list of neighboring indices in all directions for 'k' rings.")
          (k-ring-distances [this k] "Return a list of neighboring indices in all directions for 'k' rings, ordered by distance from the origin index.")
          (to-jts [this] "Given an H3 identifier, return a Polygon of that cell.")
          (edge [from to] "Given both 'from' and 'to' cells, get a unidirectional edge index.")
          (edge-origin [this] "Given a unidirectional edge, get its origin.")
          (edge-destination [this] "Given a unidirectional edge, get its destination.")
          (edges [this] "Get all edges originating from an index.")
          (edge-boundary [this] "Get coordinates representing the edge.")
          (pentagon? [this] "Check if an index is a pentagon.")
          (is-valid? [this] "Check if an index is valid.")
          (neighbors? [this cell] "Check if two indexes are neighbors.")
          (h3-distance [this cell] "Return the grid distance, which is the distance expressed in number of cells.")
          (h3-line [this cell] "Return the line of indexes between two cells")
          (h3-to-center-child [this child-res] "Returns the center child at the given resolution.")))

#?(:clj
   (extend-protocol H3Index
     String
     (to-string [this] this)
     (to-long [this] (string->long this))
     (h3->pt [this] (h3->pt-string this))
     (get-resolution [this] (get-resolution-string this))
     (get-faces [this] (get-faces-string this))
     (k-ring [this k] (k-ring-string this k))
     (k-ring-distances [this k] (k-ring-distances-string this k))
     (to-jts [this] (to-jts-string this))
     (edge [from to] (edge-string from to))
     (edge-origin [this] (edge-origin-string this))
     (edge-destination [this] (edge-destination-string this))
     (edges [this] (edges-string this))
     (edge-boundary [this] (edge-boundary-string this))
     (pentagon? [this] (pentagon?-string this))
     (is-valid? [this] (is-valid?-string this))
     (neighbors? [this cell] (neighbors?-string this cell))
     (h3-distance [this cell] (h3-distance-string this cell))
     (h3-line [this cell] (h3-line-string this cell))
     (h3-to-center-child [this child-res]
       (h3-to-center-child-string this child-res))

     Long
     (to-string [this] (long->string this))
     (to-long [this] this)
     (h3->pt [this] (h3->pt-long this))
     (get-resolution [this] (get-resolution-long this))
     (get-faces [this] (get-faces-long this))
     (k-ring [this k] (k-ring-long this k))
     (k-ring-distances [this k] (k-ring-distances-long this k))
     (to-jts [this] (to-jts-long this))
     (edge [from to] (edge-long from to))
     (edge-origin [this] (edge-origin-long this))
     (edge-destination [this] (edge-destination-long this))
     (edges [this] (edges-long this))
     (edge-boundary [this] (edge-boundary-long this))
     (pentagon? [this] (pentagon?-long this))
     (is-valid? [this] (is-valid?-long this))
     (neighbors? [this cell] (neighbors?-long this cell))
     (h3-distance [this cell] (h3-distance-long this cell))
     (h3-line [this cell] (h3-line-long this cell))
     (h3-to-center-child [this child-res]
       (h3-to-center-child-long this child-res))))

(defprotocol Polygonal
  (to-polygon [this] [this srid] "Ensure that an object is 2D, with lineal boundaries.")
  #?(:clj (polyfill [this res] "Return all resolution 'res' cells in Long form that cover a given Shapelike,
                        excluding internal holes. If the set of cells is especially large, the set
                        may return partially compacted.")
     :cljs (polyfill [this res] "Return all resolution 'res' cells that cover a given Shapelike,
                        excluding internal holes. If the set of cells is especially large, the set
                        may return partially compacted."))
  #?(:clj (polyfill-address [this res] "Return all resolution 'res' cells in String form that cover a given
                                Shapelike, excluding internal holes. If the set of cells is especially large,
                                the set may return partially compacted.")
     :cljs (is-empty? [this] "Wrap JSTS's isEmpty function, using appropriate hints for optimization."))
  #?(:cljs (get-coordinates [this] "Wrap JSTS's getCoordinates function, using appropriate hints for optimization.")))

#?(:clj (declare polyfill-address-p))
#?(:clj (declare polyfill-address-mp))
#?(:clj (declare polyfill-address-check))
(declare polyfill-p)
(declare polyfill-mp)
(declare polyfill-check)

#?(:clj
   (extend-protocol Polygonal
     GeoHash
     (to-polygon ([this] (spatial/to-jts this))
       ([this srid] (spatial/to-jts this srid)))
     (polyfill [this res] (polyfill-check [this] res))
     (polyfill-address [this res] (polyfill-address-check [this] res))

     RectangleImpl
     (to-polygon ([this] (spatial/to-jts this))
       ([this srid] (spatial/to-jts this srid)))
     (polyfill [this res] (polyfill-check [this] res))
     (polyfill-address [this res] (polyfill-address-check [this] res))

     Polygon
     (to-polygon ([this] this)
       ([this srid] (spatial/to-jts this srid)))
     (polyfill [this res] (polyfill-check [this] res))
     (polyfill-address [this res] (polyfill-address-check [this] res))

     LinearRing
     (to-polygon ([this] (jts/polygon this))
       ([this srid] (jts/polygon (crs/transform-geom this srid))))
     (polyfill [this res] (polyfill-check [this] res))
     (polyfill-address [this res] (polyfill-address-check [this] res))

     MultiPolygon
     (to-polygon ([this] this)
       ([this srid] (spatial/to-jts this srid)))
     (polyfill [this res] (polyfill-mp this res))
     (polyfill-address [this res] (polyfill-address-mp this res))))

#?(:cljs
   (extend-protocol Polygonal
     jsts.geom.Polygon
     (to-polygon ([this] this)
       ([this srid] (spatial/to-jsts this srid)))
     (polyfill [this res] (polyfill-check [this] res))
     (is-empty? [this] (.isEmpty ^js/jsts.geom.Polygon this))
     (get-coordinates [this] (.getCoordinates ^js/jsts.geom.Polyon this))

     jsts.geom.LinearRing
     (to-polygon ([this] (geo.jsts/polygon this))
       ([this srid] (geo.jsts/polygon (crs/transform-geom this srid))))
     (polyfill [this res] (polyfill-check [this] res))
     (is-empty? [this] (.isEmpty ^js/jsts.geom.LinearRing this))
     (get-coordinates [this] (.getCoordinates ^js/jsts.geom.LinearRing this))

     jsts.geom.MultiPolygon
     (to-polygon ([this] this)
       ([this srid] (spatial/to-jsts this srid)))
     (polyfill [this res] (polyfill-mp this res))
     (is-empty? [this] (.isEmpty ^js/jsts.geom.MultiPolygon this))
     (get-coordinates [this] (.getCoordinates ^js/jsts.geom.MultiPolygon this))))


#?(:clj
   (defn pt->h3
     "Return the Long index of the resolution 'res' cell that a point or lat/lng pair is contained within."
     ([^Point pt ^Integer res]
      (pt->h3 (spatial/latitude pt) (spatial/longitude pt) res))
     ([^Double lat ^Double lng ^Integer res]
      (try (.geoToH3 h3-inst lat lng res)
           (catch Exception e
             (throw (Exception. (string/join ["Failed to complete pt->h3 for lat "
                                              lat ", long " lng, ", res " res
                                              ". H3 exception message: " e])))))))
   :cljs
   (defn pt->h3
     "Return the index of the resolution 'res' cell that a point or lat/lng pair is contained within."
     ([^spatial/Point  pt ^js/numeric res]
      (pt->h3 (spatial/latitude pt) (spatial/longitude pt) res))
     ([^js/numeric lat ^js/numeric lng ^js/numeric res]
      (h3-js/geoToH3 lat lng res))))

#?(:clj
   (defn pt->h3-address
     "Return the String index of the resolution 'res' cell that a point or lat/lng pair is contained within."
     ([^Point pt ^Integer res]
      (pt->h3-address (spatial/latitude pt) (spatial/longitude pt) res))
     ([^Double lat ^Double lng ^Integer res]
      (try (.geoToH3Address h3-inst lat lng res)
           (catch Exception e
             (throw (Exception. (string/join ["Failed to complete pt->h3-address for lat "
                                              lat ", long " lng, ", res " res
                                              ". H3 exception message: " e]))))))))
#?(:clj
   (defn geo-coords
     "Return all coordinates for a given Shapelike as GeoCoords"
     [^Shapelike s]
     (map spatial/h3-point (jts/coordinates (spatial/to-jts s))))
   :cljs
   (defn geo-coords
     "Return all coordinates for a given Shapelike as lat/lng pairs"
     [^spatial/Shapelike s]
     (map #(array (.-y %) (.-x %)) (geo.jsts/coordinates (spatial/to-jsts s)))))

(defn hex-area
  "Average area for indices at resolution 'res.' Optional second argument allows
  returning area in :m2 or :km2. Defaults to m2."
  ([res]
   (hex-area res :m2))
  ([res unit]
   (case unit
     :m2
     #?(:clj (.hexArea h3-inst res AreaUnit/m2)
        :cljs (h3-js/hexArea res (.-m2 h3-js/UNITS)))
     :km2
     #?(:clj (.hexArea h3-inst res AreaUnit/km2)
        :cljs (h3-js/hexArea res (.-km2 h3-js/UNITS))))))

(defn edge-length
  "Average edge length for indices at resolution 'res.' Optional second
  argument allows returning length in :m or :km. Defaults to m."
  ([res]
   (edge-length res :m))
  ([res unit]
   (case unit
     :m
     #?(:clj (.edgeLength h3-inst res LengthUnit/m)
        :cljs (h3-js/edgeLength res (.-m h3-js/UNITS)))
     :km
     #?(:clj (.edgeLength h3-inst res LengthUnit/km)
        :cljs (h3-js/edgeLength res (.-km h3-js/UNITS))))))

(defn- max-uncompact-size-helper
  "See h3's h3Index.c."
  [cell res]
  (let [cell-res (get-resolution cell)]
    (cond (= res cell-res)
          1
          (> res cell-res)
          #?(:clj (numeric-tower/expt 7 (- res cell-res))
             :cljs (Math/pow 7 (- res cell-res)))
          (< res cell-res)
          0)))

(defn- max-uncompact-size
  "The maximum number of cells that an uncompacted sequence would need."
  [cells res]
  (->> (map #(max-uncompact-size-helper % res) cells)
       (reduce +)))

(defn compact
  "Given a set of H3 cells, return a compacted set of cells, at possibly coarser resolutions."
  [cells]
  #?(:clj (cond (number? (first cells))
                (.compact h3-inst cells)
                (string? (first cells))
                (.compactAddress h3-inst cells))
     :cljs (h3-js/compact cells)))

(defn uncompact
  "Given a set of H3 cells, return an uncompacted set of cells to a certain resolution."
  [cells res]
  #?(:clj (cond (number? (first cells))
                (.uncompact h3-inst cells res)
                (string? (first cells))
                (.uncompactAddress h3-inst cells res))
     :cljs (h3-js/uncompact cells)))

(defn- safe-uncompact
  "Given a set of H3 cells, if the maximum size of the uncompacted set is below a
  safe limit, uncompact the set. Otherwise, return the original set."
  [cells res]
  (let [m (max-uncompact-size cells res)]
    (if (< m safe-uncompact-hexagon-maximum)
      (uncompact cells res)
      cells)))

(defn- polyfill-p-common
  "Common logic used to polyfill a shapelike made of a single polygon.
  Used for both the string and long methods."
  [s]
  (let [s (to-polygon s crs/gf-wgs84)
        num-interior-rings #?(:clj (.getNumInteriorRing ^Polygon s)
                              :cljs (.getNumInteriorRing ^js/jsts.geom.Polygon s))
        ext-ring #?(:clj (.getExteriorRing ^Polygon s)
                    :cljs (.-getNumInteriorRing ^js/jsts.geom.Polygon s))
        int-rings (map #?(:clj #(.getInteriorRingN ^Polygon s %)
                          :cljs #(.getInteriorRingN ^js/jsts.geom.Polygon s %))
                       (range num-interior-rings))]
    {:e ext-ring :i int-rings}))

#?(:clj
   (defn- polyfill-address-p
     "Helper to polyfill a single polygon, returning indexes in string form."
     [s ^Integer res]
     (let [h (polyfill-p-common s)]
       (if (.isEmpty (geo.spatial/to-jts s))
         []
         (compact (.polyfillAddress h3-inst (geo-coords (:e h)) (map geo-coords (:i h)) res))))))

#?(:clj
   (defn- polyfill-p
     "Helper to polyfill a single polygon, returning indexes in long form."
     [s ^Integer res]
     (let [h (polyfill-p-common s)]
       (if (.isEmpty (geo.spatial/to-jts s))
         []
         (compact (.polyfill h3-inst (geo-coords (:e h)) (map geo-coords (:i h)) res)))))
   :cljs
   (defn- polyfill-p
     "Helper to polyfill a single polygon, returning indexes."
     [s ^js/numeric res]
     (let [h (polyfill-p-common s)]
       (if (is-empty? (geo.spatial/to-jsts s))
         []
         (compact (h3-js/polyfill (geo-coords (:e h)) (map geo-coords (:i h)) res))))))

(defn- hex-radius-in-meters
  "See h3's bbox.c."
  [pt res]
  (let [h (pt->h3 pt res)
        h-jts (h3->pt h)
        h-centroid (spatial/point h-jts)
        h-boundary #?(:clj (to-jts h)
                      :cljs (to-jsts h))
        c1 (first #?(:clj (.getCoordinates ^Geometry h-boundary)
                     :cljs (get-coordinates h-boundary)))]
    (spatial/distance h-centroid c1)))

(defn- max-kring-size
  "Maximum indices that result from k-ring with a given k. See h3's algos.c."
  [k]
  (->> (for [i (range k)
             :let [r (* 6 (+ i 1))]]
         r)
       (reduce +)
       inc))

(defn- max-polyfill-size
  "Maximum number of indices used by h3's circular/k-ring polyfill method.
  See h3's algos.c. If h3's polyfill method is made more efficient, this should
  also change accordingly."
  #?(:clj [shape ^Integer res]
     :cljs [shape ^js/numeric res])
  (let [bbox #?(:clj (spatial/bounding-box shape)
                :cljs (geo.jsts/get-envelope-internal shape))
        min-x #?(:clj (.getMinX bbox)
                 :cljs (.getMinX ^js/jsts.geom.Envelope bbox))
        min-y #?(:clj (.getMinY bbox)
                 :cljs (.getMinY ^js/jsts.geom.Envelope bbox))
        max-y #?(:clj (.getMaxY bbox)
                 :cljs (.getMaxY ^js/jsts.geom.Envelope bbox))
        closest-side-vertex (if (< (Math/abs min-y)
                                   (Math/abs max-y))
                                (spatial/point min-y min-x)
                                (spatial/point max-y min-x))

        centroid #?(:clj (spatial/center bbox)
                    :cljs (geo.jsts/centroid bbox))
        bbox-radius-m (spatial/distance closest-side-vertex centroid)
        center-hex-radius-m (hex-radius-in-meters (h3->pt (pt->h3 centroid res)) res)
        bbox-hex-radius (Math/ceil (/ bbox-radius-m (* 1.5 center-hex-radius-m)))]
    (max-kring-size bbox-hex-radius)))

(defn- polyfill-check-common
  "Common logic needed to see if a all shapes in a set are single polygons that
  are sufficiently small to be polyfilled. If h3's polyfill method is made more efficient,
  modifying max-polyfill-size accordingly should make this run faster."
  #?(:clj [shapes ^Integer res]
     :cljs [shapes ^js/numeric res])
  (loop [good-geoms []
         remaining-geoms shapes]
    (if (not (empty? remaining-geoms))
      (let [current-poly (first remaining-geoms)
            max-hexagons (if #?(:clj (.isEmpty (spatial/to-jts current-poly))
                                :cljs (is-empty? (spatial/to-jsts current-poly)))
                           0
                           (max-polyfill-size current-poly res))]
        (cond (zero? max-hexagons)
              ; If max-hexagons yields zero,
              ; the shape is either too small or invalid, so skip.
              (recur good-geoms (rest remaining-geoms))
              #?(:clj (instance? MultiPolygon current-poly)
                 :cljs (instance? jsts.geom.MultiPolygon current-poly))
              ; If a subdivided quadrant became a multipolygon,
              ; split that into its polygons and recur
              (recur good-geoms (concat #?(:clj (jts/geometries current-poly)
                                           :cljs (geo.jsts/geometries current-poly))
                                        (rest remaining-geoms)))
              (< max-hexagons safe-polyfill-hexagon-maximum)

              ; If less than the safe maximum, the geometry is good to be polyfilled
              (recur (conj good-geoms current-poly) (rest remaining-geoms))
              :else
              ; Otherwise, if it's a polygon but larger than the safe maximum,
              ; divide into quadrants and recur
              (recur good-geoms (concat #?(:clj (jts/subdivide (spatial/to-jts current-poly))
                                           :cljs (geo.jsts/subdivide (spatial/to-jsts current-poly)))
                                        (rest remaining-geoms)))))
      good-geoms)))

#?(:clj
   (defn- polyfill-address-check
    "Apply polyfill-check-common to polyfill-address."
     [shapes ^Integer res]
     (let [h (polyfill-check-common shapes res)]
       (concat [] (safe-uncompact
                   (flatten (mapcat #(polyfill-address-p % res) h)) res)))))

(defn- polyfill-check
  "Apply polyfill-check-common to polyfill."
  #?(:clj [shapes ^Integer res]
     :cljs [shapes ^js/numeric res])
  (let [h (polyfill-check-common shapes res)]
    (concat [] (safe-uncompact (flatten (mapcat #(polyfill-p % res) h)) res))))

#?(:clj
   (defn- polyfill-address-mp
  "Multipolygon helper to return all resolution 'res' cells that cover a given shape,
   excluding internal holes."
     [mp ^Integer res]
     (let [pf-polys (fn [p] (mapcat #(polyfill-address-check [%] res) p))]
       (into [] (-> mp
                    jts/geometries
                    pf-polys
                    flatten)))))

(defn- polyfill-mp
  "Multipolygon helper to return all resolution 'res' cells that cover a given shape,
   excluding internal holes."
  #?(:clj [mp ^Integer res]
     :cljs [mp ^js/numeric res])
  (let [pf-polys (fn [p] (mapcat #(polyfill-check [%] res) p))]
    (into [] (-> mp
                 #?(:clj jts/geometries
                    :cljs geo.jsts/geometries)
                 pf-polys
                 flatten))))

(defn- geocoord-array-wkt
  "Create a wkt-style data structure from a collection of GeoCoords."
  [coords]
  (->> coords
       #?(:clj (map (fn [coord] [(spatial/longitude coord) (spatial/latitude coord)]))
          :cljs (map (fn [coord] (reverse coord))))
       flatten
       vec))

(defn- geocoord-multi-helper
  "Helper function to pass to postwalk for multi-polygon generators."
  [v]
  (if #?(:clj (instance? GeoCoord (first v))
         :cljs (seq? (first v)))
    (geocoord-array-wkt v)
    v))

#?(:clj
   (defn- multi-polygon-n
  "Multi-polygon generator for numbers"
     [cells]
     (as-> cells v
       (.h3SetToMultiPolygon h3-inst v true)
       (mapv #(into [] %) v)
       (walk/postwalk geocoord-multi-helper v)
       (jts/multi-polygon-wkt v))))

(defn- multi-polygon-s
  "Multi-polygon generator for strings"
  [cells]
  (as-> cells v
    #?(:clj (.h3AddressSetToMultiPolygon h3-inst v true)
       :cljs (h3-js/h3SetToMultiPolygon v true))

    (mapv #(into [] %) v)
    (walk/postwalk geocoord-multi-helper v)
    #?(:clj (jts/multi-polygon-wkt v)
       :cljs (geo.jsts/multi-polygon-wkt v))))

(defn multi-polygon
  "Given a contiguous set of H3 cells, return a JTS MultiPolygon."
  [cells]
  #?(:clj (cond (number? (first cells))
                (multi-polygon-n cells)
                (string? (first cells))
                (multi-polygon-s cells))
     :cljs (multi-polygon-s cells)))

(defn get-res-0-indexes
  "Return a collection of all base cells"
  []
  #?(:clj (.getRes0Indexes h3-inst)
     :cljs (h3-js/getRes0Indexes)))

(defn get-pentagon-indexes
  "Return a collection of all topologically pentagonal
  cells at the given resolution"
  [res]
  #?(:clj (.getPentagonIndexes h3-inst res)
     :cljs (h3-js/getPentagonIndexes res)))
