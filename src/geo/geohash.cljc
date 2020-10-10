(ns geo.geohash
  "Working with geohashes."
  (:require [geo.crs :as crs]
            [geo.spatial :as spatial]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as geo.jsts])
            #?(:cljs [ngeohash])
            #?(:cljs [jsts])
            #?(:cljs [goog.object :as gobj]))
  #?(:clj (:import (ch.hsr.geohash WGS84Point GeoHash)
                   (org.locationtech.spatial4j.shape Shape)
                   (org.locationtech.spatial4j.shape.impl RectangleImpl)
                   (org.locationtech.spatial4j.context.jts JtsSpatialContext))))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

#?(:cljs
   (deftype Geohash [id precision]))

(defn geohash
  "Creates a geohash from a string, or at the given point with the given bit
  precision."
  ([string]
   #?(:clj (GeoHash/fromGeohashString string)
      :cljs (->Geohash string (* 5 (.-length ^js/string string)))))
  ([point precision]
    (geohash (spatial/latitude point)
             (spatial/longitude point)
             precision))
  ([lat long precision]
   #?(:clj (GeoHash/withBitPrecision lat long precision)
      :cljs (->Geohash (ngeohash/encode_int lat long precision) precision))))

#?(:cljs
   (defprotocol GeohashIdentifier
     (-decode [this] [this precision])
     (-decode-bbox [this] [this precision])
     (-neighbor [this direction] [this direction precision])
     (-neighbors [this] [this precision])))

#?(:cljs
   (extend-protocol GeohashIdentifier
     string
     (-decode [this] (ngeohash/decode this))
     (-decode-bbox [this] (ngeohash/decode_bbox this))
     (-neighbor [this direction] (ngeohash/neighbor this direction))
     (-neighbors [this] (ngeohash/neighbors this))

     number
     (-decode [this precision] (ngeohash/decode_int this precision))
     (-decode-bbox [this precision] (ngeohash/decode_bbox_int this precision))
     (-neighbor [this direction precision]
       (ngeohash/neighbor_int this direction precision))
     (-neighbors [this precision] (ngeohash/neighbors_int this precision))

     Geohash
     (-decode [this] (if (string? (.-id this))
                       (-decode (.-id this))
                       (-decode (.-id this) (.-precision this))))
     (-decode-bbox [this] (if (string? (.-id this))
                       (-decode-bbox (.-id this))
                       (-decode-bbox (.-id this) (.-precision this))))
     (-neighbor [this direction] (if (string? (.-id this))
                                   (geohash (-neighbor (.-id this) direction))
                                   (geohash (-neighbor (.-id this) direction
                                                       (.-precision this))
                                            (.-precision this))))
     (-neighbors [this] (if (string? (.-id this))
                          (map geohash (-neighbors (.-id this)))
                          (map #(geohash % (.-precision this))
                               (-neighbors (.-id this) (.-precision this)))))))




#?(:clj
   (defn bbox ^Shape [^GeoHash geohash]
     (let [box (.getBoundingBox geohash)]
       (RectangleImpl. (.getWestLongitude box)
                       (.getEastLongitude box)
                       (.getSouthLatitude box)
                       (.getNorthLatitude box)
                       spatial/earth)))
   :cljs
   (defn bbox ^jsts.geom.Envelope [^Geohash geohash]
     (let [coords (-decode-bbox geohash)

           coord-0 (geo.jsts/coordinate (aget coords 1)
                                        (aget coords 0))
           coord-1 (geo.jsts/coordinate (aget coords 3)
                                        (aget coords 2))]
       (geo.jsts/envelope coord-0 coord-1))))

#?(:clj
   (defn bbox-geom ^org.locationtech.jts.geom.Polygon [^GeoHash geohash]
     (jts/set-srid (.getGeometryFrom JtsSpatialContext/GEO (bbox geohash))
                   crs/gf-wgs84))
   :cljs
   (defn bbox-geom [^Geohash geohash]
     (let [coords (-decode-bbox geohash)
           lat1 (aget coords 0)
           lat2 (aget coords 2)
           lon1 (aget coords 1)
           lon2 (aget coords 3)
           coord-0 (geo.jsts/coordinate lon1 lat1)
           coord-1 (geo.jsts/coordinate lon2 lat1)
           coord-2 (geo.jsts/coordinate lon2 lat2)
           coord-3 (geo.jsts/coordinate lon1 lat2)]
       (geo.jsts/linear-ring
        (array coord-0 coord-1 coord-2 coord-3 coord-0)))))

#?(:clj
   (extend-protocol spatial/Shapelike
     GeoHash
     (to-shape [^GeoHash geohash] (bbox geohash))
     (to-jts
       ([^GeoHash geohash] (bbox-geom geohash))
       ([^GeoHash geohash srid]
        (spatial/to-jts (bbox-geom geohash) srid))
       ([^GeoHash geohash c1 c2]
        (spatial/to-jts (bbox-geom geohash) c1 c2))
       ([^GeoHash geohash c1 c2 geometry-factory]
        (spatial/to-jts (bbox-geom geohash) c1 c2 geometry-factory)))

     WGS84Point
     (to-shape [this] (spatial/spatial4j-point this))
     (to-jts
       ([this] (spatial/jts-point this))
       ([this srid] (spatial/to-jts (spatial/jts-point this) srid))
       ([this c1 c2] (spatial/to-jts (spatial/jts-point this) c1 c2))
       ([this c1 c2 geometry-factory]
        (spatial/to-jts (spatial/jts-point this) c1 c2 geometry-factory))))
   :cljs
   (extend-protocol spatial/Shapelike
     Geohash
     (to-jsts
       ([^Geohash geohash] (bbox-geom geohash))
       ([^Geohash geohash srid]
        (spatial/to-jsts (bbox-geom geohash) srid))
       ([^Geohash geohash c1 c2]
        (spatial/to-jsts (bbox-geom geohash) c1 c2))
       ([^Geohash geohash c1 c2 geometry-factory]
        (spatial/to-jsts (bbox-geom geohash) c1 c2 geometry-factory)))))


#?(:clj
   (defn northern-neighbor [^GeoHash h] (.getNorthernNeighbour h))
   :cljs
   (defn northern-neighbor [^Geohash h] (-neighbor h (array 1 0))))

#?(:clj
   (defn eastern-neighbor [^GeoHash h] (.getEasternNeighbour h))
   :cljs
   (defn eastern-neighbor [^Geohash h] (-neighbor h (array 0 1))))

#?(:clj
   (defn western-neighbor [^GeoHash h] (.getWesternNeighbour h))
   :cljs
   (defn western-neighbor [^Geohash h] (-neighbor h (array 0 -1))))

#?(:clj
   (defn southern-neighbor [^GeoHash h] (.getSouthernNeighbour h))
   :cljs
   (defn southern-neighbor [^Geohash h] (-neighbor h (array -1 0))))

#?(:clj
   (defn neighbors [^GeoHash h] (vec (.getAdjacent h)))
   :cljs
   (defn neighbors [^Geohash h] (vec (-neighbors h))))

#?(:clj (defn significant-bits [^GeoHash geohash]
          (.significantBits geohash))
   :cljs (defn significant-bits [^Geohash geohash]
           (.-precision geohash)))

#?(:clj (defn character-precision [^GeoHash geohash]
          (.getCharacterPrecision geohash))
   :cljs (defn character-precision [^Geohash geohash]
           (if (string? (.-id geohash))
             (.-length ^js/string (.-id geohash)))))


(defn subdivide
  "Given a geohash, returns all geohashes inside it, of a given precision."
  #?(:clj ([^GeoHash geohash]
           (subdivide geohash (inc (significant-bits geohash))))
     :cljs ([^Geohash geohash]
            (subdivide geohash (inc (significant-bits geohash)))))
  #?(:clj
     ([^GeoHash geohash precision]
      (let [number (Math/pow 2 (- precision (significant-bits geohash)))]
        (->> (GeoHash/fromLongValue (.longValue geohash) precision)
             (iterate #(.next ^GeoHash %))
             (take number))))
     :cljs
     ([^Geohash geohash precision]
      (let [box ^js/jsts.geom.Envelope (bbox geohash)]
        (-> (ngeohash/bboxes_int (+ (.getMinY box) 1e-7)
                                 (+ (.getMinX box) 1e-7)
                                 (- (.getMaxY box) 1e-7)
                                 (- (.getMaxX box) 1e-7)
                                 precision))))))

(defn subdivide-levels
  "Given a geohash and a range of levels, return all geohashes inside the
   given geohash at all the levels in the range"
  #?(:clj [^GeoHash geohash min-precision max-precision]
     :cljs [^Geohash geohash min-precision max-precision])
  (let [start-precision (max (significant-bits geohash) min-precision)
        end-precision (max start-precision max-precision)]
    (->> (range start-precision (inc end-precision))
         (mapcat #(subdivide geohash %)))))

(defn square-ring
  "Given a geohash at the northeast corner of a square (n-2) geohashes on a
  side, returns a list of geohashes in a path around the square, such that the
  first entry in the list is the northeast corner of a square (n) geohashes on
  a side.

  O is the origin argument
  F is the first hash in the returned sequence
  L is the last hash in the returned sequence
  E represents the last *and* first in the sequence

  n=1   3      5        7
                     +----LF
             +--LF   |    O|
       +LF   |  O|   |     |
    E  |O|   |   |   |     |
       +-+   |   |   |     |
             +---+   |     |
                     +-----+

  If n is one, returns [origin].

  This algorithm is undefined at the poles."
  [origin n]
  (assert (odd? n))
  (assert (pos? n))
  (if (= n 1)
    ; Special case: if we're asked to return a 1x1 square, we'll return the
    ; origin argument.
    [origin]
    ; We build the list backwards by recurring from last to first,
    ; counterclockwise.
    ; Total sequence length is determined by (* 4 (dec n))
    ; Start (at L), with i = 0, going west
    (let [south (- n 2)            ; Turn south at i = n - 2
          east  (+ south (- n 1))  ; Turn east
          north (+ east  (- n 1))  ; Turn north
          end   (+ north n)]       ; Then stop (at F)
      (loop [ring (list)                       ; Accrued list
             i    0                            ; List length
             cell (northern-neighbor origin)]  ; Current hash
        (let [next-cell (condp <= i
                          end   nil
                          north (northern-neighbor cell)
                          east  (eastern-neighbor  cell)
                          south (southern-neighbor cell)
                          (western-neighbor cell))]
          (if (nil? next-cell)
            ring
            (recur (conj ring cell)
                   (inc i)
                   next-cell)))))))

(defn concentric-square-rings
  "Given a single geohash, returns a lazy sequence of concentric square rings
  of geohashes around it.  The first element is [hash], the second element is
  [northern-neighbor, northwest-neighbor, west-neighbor, ...], the third
  element is the list of all geohashes around *those*, and so on."
  ([origin]
   (concentric-square-rings origin 1))
  ([origin n]
   (let [ring (square-ring origin n)
         next-origin  (first ring)]
     (cons ring
           (lazy-seq (concentric-square-rings next-origin (+ n 2)))))))

(defn geohash-center
  "Returns the center point of a geohash."
  #?(:clj [^GeoHash geohash]
     :cljs [^Geohash geohash])
  #?(:clj (.getBoundingBoxCenter geohash)
     :cljs (let [g (-decode geohash)]
             (geo.jsts/point (gobj/get g "latitude")
                             (gobj/get g "longitude")))))

(defn geohash-midline-dimensions
  "Returns a vector of [lat-extent long-extent], where lat-extent is the length
  of the geohash through the midpoint of top and bottom, and long-extent is the
  length of the geohash through the midpoint of left and right sides. All
  figures in meters."
  #?(:clj [^GeoHash geohash]
     :cljs [^Geohash geohash])
  (let [box     #?(:clj (.getBoundingBox geohash)
                   :cljs (bbox geohash))
        min-lat #?(:clj (.getSouthLatitude box)
                   :cljs (.getMinY box))
        max-lat #?(:clj (.getNorthLatitude box)
                   :cljs (.getMaxY box))
        min-long #?(:clj (.getWestLongitude box)
                    :cljs (.getMinX box))
        max-long #?(:clj (.getEastLongitude box)
                    :cljs (.getMaxX box))
        mean-lat (/ (+ min-lat max-lat) 2)
        mean-long (/ (+ min-long max-long) 2)]
    [(spatial/distance (spatial/point min-lat mean-long)
               (spatial/point max-lat mean-long))
     (spatial/distance (spatial/point mean-lat min-long)
               (spatial/point mean-lat max-long))]))

(defn geohash-midline-area
  "An estimate of a geohash's area, in square meters, based on its midline
  dimensions."
  [geohash]
  (apply * (geohash-midline-dimensions geohash)))

(defn geohash-error
  "Returns the error (i.e. the distance in meters between opposite corners) of
  the given geohash."
  #?(:clj [^GeoHash geohash]
     :cljs [^Geohash geohash])
  (let [box #?(:clj (.getBoundingBox geohash)
               :cljs (bbox geohash))]
    (spatial/distance #?(:clj (.getSouthEastCorner box)
                         :cljs (geo.jsts/point (.getMinY box)
                                               (.getMaxX box)))
                      #?(:clj (.getNorthWestCorner box)
                         :cljs (geo.jsts/point (.getMaxY box)
                                               (.getMinX box))))))

(defn geohash-max-error
  "Returns the maximum error (i.e. the distance between opposite corners of the
  geohash bounding box) for a given number of geohash bits. Geohashes are least
  precise at the equator."
  [bits]
  (geohash-error (geohash 0 0 bits)))

(defn string
  "Returns the base32 encoded string value of a geohash."
  #?(:clj [^GeoHash geohash]
     :cljs [^Geohash geohash])
  #?(:clj (.toBase32 geohash)
     :cljs (.-id geohash)))


(def degrees-precision-long-cache
  (map (comp spatial/width (partial geohash 45 45)) (range 0 64)))
(def degrees-precision-lat-cache
  (map (comp spatial/height (partial geohash 45 45)) (range 0 64)))

(defn least-upper-bound-index
  "Given a sequence of numbers in descending order, finds the index of the
  largest number which is just greater than the target."
  [numbers target]
  (dec (count (take-while #(< target %) numbers))))

(defn shape->precision
  "Estimates the precision which generates geohash regions on the scale of the
  given shape."
  [shape]
  (min (least-upper-bound-index degrees-precision-lat-cache
                                (spatial/height shape))
       (least-upper-bound-index degrees-precision-long-cache
                                (spatial/height shape))))

#?(:clj (defn- queue [] clojure.lang.PersistentQueue/EMPTY))

(defn- geohashes-intersecting-matches!
  "Given geohash and shape-relation, modify the matches list as necessary
   :disjoint    no matches
   :intersects  part of geohash is in shape, but can't extend assertion to all geohash subdivisions
   :within      same logic as :intersects
   :contains    all of geohash in shape, all subdivisions at all valid levels match"
  [min-level max-level gh shape-relation matches]
  (case shape-relation
    ;; gh fully in shape. Add subdivisions of all valid
    ;; levels into matches
    :contains
    (->> (subdivide-levels gh min-level max-level)
         (reduce conj! matches))
    ;; disjoint, no new geohash matches
    :disjoint matches
    ;; within or intersects: shape partially covers geohash. Current
    ;; geohash match if level is appropriate
    (if (<= min-level (significant-bits gh) max-level)
      (conj! matches gh)
      matches)))

#?(:clj
(defn geohashes-intersecting
  ([shape desired-level] (geohashes-intersecting shape desired-level desired-level))
  ([shape min-level max-level]
     (let [shape (spatial/to-shape shape)
           update-matches! (partial geohashes-intersecting-matches! min-level max-level)]
       (loop [current (geohash "")
              matches (transient [])
              gh-q (queue)]
         (let [relation (spatial/relate shape current)
               matches (update-matches! current relation matches)
               gh-q (if (or (= :intersects relation) (= :within relation))
                      (reduce conj (pop gh-q) (subdivide current))
                      (pop gh-q))
               next (peek gh-q)]
           (if (and next (<= (significant-bits next) max-level))
             (recur next matches gh-q)
             (persistent! matches))))))))
#?(:clj
   (defn geohashes-near
     "Returns a list of geohashes of the given precision within radius meters of
  the given point."
     [point radius precision]
     (geohashes-intersecting (spatial/circle point radius) precision)))
