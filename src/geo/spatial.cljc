(ns geo.spatial
  "Provides common interfaces for working with spatial objects and geodesics.
  All units are in meters/radians/steradians unless otherwise specified.
  Provides static spatial contexts for the earth, and some constants like the
  earth's radii and circumferences, along with points like the poles.

  Defines protocols for unified access to Points and Shapes, to allow different
  geometry libraries to interoperate.

  Basic utility functions for unit conversion; e.g. degrees<->radians.

  Functions for computing earth radii, surface distances, and surface areas.

  Constructors for shapes like bounding-boxes and circles, and utility
  functions over shapes for their heights, centers, areas, etc. Can also
  compute relationships between shapes: their intersections, contains, disjoint
  statuses, etc."
  #?(:clj (:use [clojure.math.numeric-tower :only [abs]]))
  (:require [geo.crs :as crs]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as geo.jsts])
            #?(:cljs [geo.impl.jsts :as impl])
            #?(:cljs [jsts])
            #?(:cljs [goog.math]))
  #?(:clj (:import (ch.hsr.geohash WGS84Point)
                   (ch.hsr.geohash.util VincentyGeodesy)
                   (com.uber.h3core.util GeoCoord)
                   (org.locationtech.spatial4j.shape SpatialRelation
                                                     Shape
                                                     Rectangle)
                   (org.locationtech.jts.geom Coordinate
                                              Envelope
                                              Geometry)
                  ; (org.locationtech.spatial4j.shape Circle
                  ;                                   Rectangle
                  ;                                   Point)
                   (org.locationtech.spatial4j.shape.impl GeoCircle
                                                          PointImpl
                                                          RectangleImpl)
                   (org.locationtech.spatial4j.shape.jts JtsGeometry
                                                         JtsPoint)
                   (org.locationtech.spatial4j.distance DistanceCalculator
                                                        DistanceUtils)
                   (org.locationtech.spatial4j.context SpatialContextFactory
                                                       SpatialContext)
                   (org.locationtech.spatial4j.context.jts
                    JtsSpatialContext
                    JtsSpatialContextFactory))))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))


#?(:clj (declare spatial4j-point))
#?(:clj (declare geohash-point))
#?(:clj (declare jts-point)
   :cljs (declare jsts-point))
#?(:clj (declare h3-point))

(defn square [x]
  (Math/pow x 2))

#?(:clj
   (def ^SpatialContext earth
  "The SpatialContext of the earth, as according to spatial4j."
     (SpatialContextFactory/makeSpatialContext
      {"geo" "true"
       "datelineRule" "width180"
       "spatialContextFactory"
       "org.locationtech.spatial4j.context.jts.JtsSpatialContextFactory"
       "distCalculator" "vincentySphere"}
      (.getClassLoader JtsSpatialContext))))

;; to deprecate in favor of cross-platform distance
#?(:clj (def ^DistanceCalculator vincenty-distance-calculator
          (.getDistCalc earth)))

(def earth-mean-radius
  "Earth's mean radius, in meters."
  #?(:clj (* 1000 DistanceUtils/EARTH_MEAN_RADIUS_KM)
     :cljs 6371008.7714))

(def earth-equatorial-radius
  "Earth's radius at the equator, in meters."
  6378137.0)

(def earth-polar-radius
  "Earth's radius at the poles, in meters."
  6356752.3142)

(def earth-flattening
  "Earth's flattening of the ellipsoid."
  (/ 1 298.257223563))

(def earth-equatorial-radius-squared
  (square earth-equatorial-radius))

(def earth-polar-radius-squared
  (square earth-polar-radius))

(def earth-mean-circumference
  "Earth's mean circumference, in meters."
  (* 1000 40041))

(def earth-equatorial-circumference
  "Earth's equatorial circumference, in meters"
  (* 1000 40075))

(def earth-meridional-circumference
  "Earth's circumference around a meridian, in meters."
  (* 1000 40008))

(defprotocol Shapelike
  (^Shape to-shape [this] "Convert anything to a Shape.")
  (^Geometry to-jts [this] [this srid] [this c1 c2]
   [this c1 c2 geometry-factory]
   "Convert anything to a projected JTS Geometry. See geo.crs/transform-geom
    for argument information.")
  (to-jsts [this] [this srid] [this c1 c2] [this c1 c2 geometry-factory]
   "Convert anything to a projected JSTS Geometry. See geo.crs/transform-geom
    for argument information."))

(defn crosses-dateline?
  #?(:clj [jts-geom]
     :cljs [jsts-geom])
  (<= 180
      #?(:clj (.getWidth (.getEnvelopeInternal (to-jts jts-geom crs/gf-wgs84)))
         :cljs (.getWidth (impl/get-envelope-internal (to-jsts jsts-geom crs/gf-wgs84))))))



#?(:clj
   (extend-protocol Shapelike
     GeoCircle
     (to-shape [this] this)
     (to-jts ([_] (throw (Exception. "Cannot cast GeoCircle to JTS.")))
       ([_ _] (throw (Exception. "Cannot cast GeoCircle to JTS.")))
       ([_ _ _] (throw (Exception. "Cannot cast GeoCircle to JTS.")))
       ([_ _ _ _] (throw (Exception. "Cannot cast GeoCircle to JTS."))))

     RectangleImpl
     (to-shape [this] this)
     (to-jts
       ([this] (crs/set-srid (.getGeom ^JtsGeometry this) crs/gf-wgs84))
       ([this srid] (to-jts (to-jts this) srid))
       ([this c1 c2] (to-jts (to-jts this) c1 c2))
       ([this c1 c2 geometry-factory]
        (to-jts (to-jts this) c1 c2 geometry-factory)))

     PointImpl
     (to-shape [this] this)
     (to-jts
       ([this] (crs/set-srid (jts-point (.getY this) (.getX this))
                             crs/gf-wgs84))
       ([this srid] (to-jts (to-jts this) srid))
       ([this c1 c2] (to-jts (to-jts this) c1 c2))
       ([this c1 c2 geometry-factory] (to-jts (to-jts this)
                                              c1 c2 geometry-factory)))

     JtsGeometry
     (to-shape [this] this)
     (to-jts
       ([this] (crs/set-srid (.getGeom this) crs/gf-wgs84))
       ([this srid] (to-jts (to-jts this) srid))
       ([this c1 c2] (to-jts (to-jts this) c1 c2))
       ([this c1 c2 geometry-factory] (to-jts (to-jts this)
                                              c1 c2 geometry-factory)))

     JtsPoint
     (to-shape [this] this)
     (to-jts
       ([this] (crs/set-srid (jts-point (.getY this) (.getX this))
                             crs/gf-wgs84))
       ([this srid] (to-jts (to-jts this) srid))
       ([this c1 c2] (to-jts (to-jts this) c1 c2))
       ([this c1 c2 geometry-factory] (to-jts (to-jts this)
                                              c1 c2 geometry-factory)))

     Geometry
     (to-shape [this] (JtsGeometry. (crs/transform-geom this crs/gf-wgs84)
                                    earth true true))
     (to-jts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     GeoCoord
     (to-shape [this] (spatial4j-point this))
     (to-jts
       ([this] (jts-point this))
       ([this srid] (crs/transform-geom (to-jts this) srid))
       ([this c1 c2] (to-jts (to-jts this) c1 c2))
       ([this c1 c2 geometry-factory] (to-jts (to-jts this)
                                              c1 c2 geometry-factory))))
   
   :cljs
   (extend-protocol Shapelike
     jsts.geom.Geometry
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.Point
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.MultiPoint
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.LineString
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.LinearRing
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.MultiLineString
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.Polygon
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.MultiPolygon
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))

     jsts.geom.GeometryCollection
     (to-jsts
       ([this] this)
       ([this srid] (crs/transform-geom this srid))
       ([this c1 c2] (crs/transform-geom this c1 c2))
       ([this c1 c2 geometry-factory]
        (crs/transform-geom this c1 c2 geometry-factory)))))

(defprotocol Point
  (latitude [this])
  (longitude [this])
  (to-spatial4j-point [this])
  (to-geohash-point [this])
  (to-h3-point [this]))

#?(:clj
   (extend-protocol Point
     WGS84Point
     (latitude [this] (.getLatitude this))
     (longitude [this] (.getLongitude this))
     (to-spatial4j-point [this] (spatial4j-point this))
     (to-geohash-point [this] this)
     (to-h3-point [this] (h3-point this))

     org.locationtech.jts.geom.Point
     (latitude [this] (.getY ^org.locationtech.jts.geom.Point
                             (crs/transform-geom this crs/gf-wgs84)))
     (longitude [this] (.getX ^org.locationtech.jts.geom.Point
                              (crs/transform-geom this crs/gf-wgs84)))
     (to-spatial4j-point [this] (spatial4j-point this))
     (to-geohash-point [this] (geohash-point this))
     (to-h3-point [this] (h3-point this))

     org.locationtech.jts.geom.Coordinate
     (latitude [this] (.y this))
     (longitude [this] (.x this))
     (to-spatial4j-point [this] (spatial4j-point this))
     (to-geohash-point [this] (geohash-point this))
     (to-h3-point [this] (h3-point this))

     org.locationtech.spatial4j.shape.Point
     (latitude [this] (.getY this))
     (longitude [this] (.getX this))
     (to-spatial4j-point [this] this)
     (to-geohash-point [this] (geohash-point this))
     (to-h3-point [this] (h3-point this))

     com.uber.h3core.util.GeoCoord
     (latitude [this] (.lat this))
     (longitude [this] (.lng this))
     (to-spatial4j-point [this] (spatial4j-point this))
     (to-geohash-point [this] (geohash-point this))
     (to-h3-point [this] this))

   :cljs
   (extend-protocol Point
     jsts.geom.Point
     (latitude [this] (.getY ^js/jsts.geom.Point
                             (crs/transform-geom this crs/gf-wgs84)))
     (longitude [this] (.getX ^js/jsts.geom.Point
                              (crs/transform-geom this crs/gf-wgs84)))

     jsts.geom.Coordinate
     (latitude [this] (.-y this))
     (longitude [this] (.-x this))))


(defn degrees->radians
  [degrees]
  #?(:clj (DistanceUtils/toRadians degrees)
     :cljs (goog.math/toRadians degrees)))

(defn radians->degrees
  [radians]
  #?(:clj (DistanceUtils/toDegrees radians)
     :cljs (goog.math/toDegrees radians)))

(defn earth-radius
  "Returns an approximate radius for the earth, at some point. Based on the
  geodetic model for an oblate spheroid."
  [point]
  (let [l   (degrees->radians (latitude point))
        a   earth-equatorial-radius
        a2  earth-equatorial-radius-squared
        b   earth-polar-radius
        b2  earth-polar-radius-squared
        cos (Math/cos l)
        sin (Math/sin l)]
    (Math/sqrt
      (/ (+ (square (* a2 cos))
            (square (* b2 sin)))
         (+ (square (* a cos))
            (square (* b sin)))))))

(defn distance->radians
  "Converts distance, in meters on the surface of the earth, to radians.
  Assumes earth mean radius."
  ([meters]
   (distance->radians meters earth-mean-radius))
  ([meters radius]
   #?(:clj (DistanceUtils/dist2Radians meters radius)
      :cljs (/ meters radius))))

(defn distance-at-point->radians
  "Converts a distance near a point on the earth into radians; using a more
  accurate model of the radius of the earth near that point."
  [meters point]
  (distance->radians meters (earth-radius point)))

(defn radians->distance
  "Converts radians to meter distance on the surface of the earth. Assumes
  earth mean radius."
  ([radians]
   (radians->distance radians earth-mean-radius))
  ([radians radius]
   #?(:clj (DistanceUtils/radians2Dist radians radius)
      :cljs (* radians radius))))

(def square-degree-in-steradians
  (/ (* 180 180) (* Math/PI Math/PI)))

(defn square-degrees->steradians
  [square-degrees]
  (/ square-degrees square-degree-in-steradians))

(defn steradians->square-degrees
  [steradians]
  (* steradians square-degree-in-steradians))

#?(:clj
   (defn jts-point
    "Returns a Point used by JTS."
     ([point]
      (jts/point (latitude point) (longitude point)))
     ([lat long]
      (jts/point lat long)))
   :cljs
   (defn ^jsts.geom.Point jsts-point
     "Returns a Point used by JSTS."
     ([point]
      (geo.jsts/point (latitude point) (longitude point)))
     ([lat long]
      (geo.jsts/point lat long))))

(defn steradians->area
  "Converts steradians to square meters on the surface of the earth. Assumes
  earth mean radius."
  ([steradians]
   (steradians->area steradians earth-mean-radius))
  ([steradians radius]
   (* steradians (square radius))))

#?(:clj
   (defn spatial4j-point
  "A spatial4j point on the earth."
     ([point]
      (PointImpl. (longitude point) (latitude point) earth))
     ([lat long]
      (PointImpl. long lat earth))))

#?(:clj
   (defn geohash-point
  "Returns a WGS84Point used by the geohash library."
     ([point]
      (WGS84Point. (latitude point) (longitude point)))
     ([lat long]
      (WGS84Point. lat long))))

#?(:clj
   (defn h3-point
  "Returns a GeoCoord used by the H3 library."
     ([point]
      (h3-point (latitude point) (longitude point)))
     ([lat long]
      (GeoCoord. lat long))))

(def point #?(:clj spatial4j-point
              :cljs jsts-point))

#?(:clj (def north-pole (spatial4j-point 90 0)))
#?(:clj (def south-pole (spatial4j-point -90 0)))

(defn circle
  "A spatial4j circle around the given point or lat,long, with radius in
  meters."
  (#?(:clj [point meters]
      :cljs [^js/jsts.geom.Point point meters])
   ; GeoCircle takes its radius in degrees, so we need to figure out how many
   ; degrees to use. For anything under 100 kilometers we use a local
   ; approximation; for bigger stuff we use the mean radius.
   (let [point   #?(:clj (to-spatial4j-point point)
                    :cljs point)
         radians (if (< 1e6 meters)
                   (distance->radians meters)
                   (distance-at-point->radians meters point))
         degrees (radians->degrees radians)]
        #?(:clj (GeoCircle. point degrees earth)
           :cljs (.buffer point degrees)))))

#?(:clj
   (defn distance-in-degrees
  "Distance between two points, in degrees."
     [a b]
     (-> vincenty-distance-calculator
         (.distance (to-spatial4j-point a)
                    (to-spatial4j-point b)))))

(defn distance
  "Distance between two points, in meters."
  [a b]
  (let [U1 (Math/atan (* (- 1 earth-flattening)
                         (Math/tan (degrees->radians (latitude a)))))
        U2 (Math/atan (* (- 1 earth-flattening)
                         (Math/tan (degrees->radians (latitude b)))))
        sin-U1 (Math/sin U1)
        cos-U1 (Math/cos U1)
        sin-U2 (Math/sin U2)
        cos-U2 (Math/cos U2)
        L (degrees->radians (- (longitude b) (longitude a)))]
    (loop [lambda L
           iteration-limit 100]
      (let [sin-lambda (Math/sin lambda)
            cos-lambda (Math/cos lambda)
            sin-sigma (Math/sqrt (+ (square (* cos-U2 sin-lambda))
                                    (square (- (* cos-U1 sin-U2)
                                               (* sin-U1 cos-U2
                                                  cos-lambda)))))]
        (if (= 0.0 sin-sigma)
          0
          (let [cos-sigma (+ (* sin-U1 sin-U2) (* cos-U1 cos-U2 cos-lambda))
                sigma (Math/atan2 sin-sigma cos-sigma)
                sin-alpha (/ (* cos-U1 cos-U2 sin-lambda) sin-sigma)
                cos-sq-alpha (- 1 (square sin-alpha))
                cos-2-sigma-m (- cos-sigma (/ (* 2 sin-U1 sin-U2)
                                              cos-sq-alpha))
                cos-2-sigma-m (if #?(:clj (Double/isNaN cos-2-sigma-m)
                                     :cljs (not (js/isFinite cos-2-sigma-m)))
                                0
                                cos-2-sigma-m)
                C (* (/ earth-flattening 16)
                     cos-sq-alpha
                     (+ 4 (* earth-flattening
                             (- 4 (* 3 cos-sq-alpha)))))
                l-prev lambda
                l (+ L
                     (* (- 1 C)
                        earth-flattening
                        sin-alpha
                        (+ sigma
                           (* C sin-sigma
                              (+ cos-2-sigma-m
                                 (* C cos-sigma
                                    (+ -1
                                       (* 2 (square cos-2-sigma-m)))))))))]
            (cond (and (< (#?(:clj abs
                              :cljs Math/abs) (- l l-prev))
                    1e-12)
                       (> iteration-limit 0))
                  (let [u-sq (/ (* cos-sq-alpha
                             (- earth-equatorial-radius-squared
                                earth-polar-radius-squared))
                          earth-polar-radius-squared)
                  A (+ 1
                       (* (/ u-sq 16384)
                          (+ 4096
                             (* u-sq
                                (+ -768
                                   (* u-sq
                                      (- 320
                                         (* 175 u-sq))))))))
                  B (* (/ u-sq 1024)
                       (+ 256
                          (* u-sq
                             (+ -128
                                (* u-sq
                                   (- 74
                                      (* 47 u-sq)))))))
                  delta-sigma (* B
                                 sin-sigma
                                 (+ cos-2-sigma-m
                                    (* (/ B 4)
                                       (- (* cos-sigma
                                             (+ -1
                                                (* 2 (square cos-2-sigma-m))))
                                          (* (/ B 6)
                                             cos-2-sigma-m
                                             (+ -3 (* 4 (square sin-sigma)))
                                             (+ -3 (* 4 (square
                                                         cos-2-sigma-m))))))))
                  s (* earth-polar-radius A
                       (- sigma delta-sigma))]
                    s)
                  (= iteration-limit 0)
                  #?(:clj Double/NaN
                     :cljs js/NaN)
                  :else
                  (recur l (dec iteration-limit)))))))))

(defn bounding-box
  "Returns the bounding box of any shape."
  #?(:clj (^org.locationtech.spatial4j.shape.Rectangle [shape]
           (.getBoundingBox (to-shape shape)))
     :cljs ([geom]
            (let [e (impl/get-envelope-internal geom)
                  x1 (.getMinX e)
                  x2 (.getMaxX e)
                  y1 (.getMinY e)
                  y2 (.getMaxY e)]
              (geo.jsts/polygon-wkt
               [[x1 y1 x1 y2 x2 y2 x2 y1 x1 y1]])))))


#?(:clj
   (defn center
  "Returns the centroid of a spatial4j shape. Note that .getCenter does bad
  things for JTS shapes that cross the international dateline, so we use use
  (center (bounding-box x)) for JTS stuff."
     [shape]
     (let [shape (to-shape shape)]
       (if (instance? JtsGeometry shape)
         (.getCenter (bounding-box shape))
         (.getCenter (to-shape shape)))))
   :cljs
   (defn center
     "Returns the centroid of a JSTS shape."
     [shape]
     (let [shape (to-jsts shape)]
       (.getCentroid ^js/jsts.geom.Geometry shape))))

(defn height
  "Returns the height of a shape, in degrees."
  #?(:clj [shape]
     :cljs [geom])
  #?(:clj (-> shape bounding-box .getHeight)
     :cljs (-> geom ;; need to still convert to wgs-84
               impl/get-envelope-internal
               .getHeight
               )))

(defn split-across-dateline
  #?(:clj [e]
     :cljs [^js/jsts.geom.Envelope e])
  (let [min-x (.getMinX e)
        min-y (.getMinY e)
        max-x (.getMaxX e)
        max-y (.getMaxY e)]
    #?(:clj (if (> min-x max-x)
               [(jts/envelope (jts/coordinate max-x min-y)
                              (jts/coordinate 180 max-y))
                (jts/envelope (jts/coordinate -180 min-y)
                              (jts/coordinate min-x max-y))]
               e)
       :cljs (if (> min-x max-x)
               [(geo.jsts/envelope
                 (geo.jsts/coordinate max-x min-y)
                 (geo.jsts/coordinate 180 max-y))
                (geo.jsts/envelope
                 (geo.jsts/coordinate -180 min-y)
                 (geo.jsts/coordinate min-x max-y))]
               e))))

(defn width
  "Returns the height of a shape, in degrees."
  #?(:clj [shape]
     :cljs [geom])
  #?(:clj (-> shape bounding-box .getWidth)
     :cljs
     (let [width-fn (fn [geom] (-> geom impl/get-envelope-internal .getWidth))
           crosses? (crosses-dateline? geom)]
       (if (not crosses?)
         (-> geom impl/get-envelope-internal .getWidth)
         (reduce + (map impl/get-width (-> geom impl/get-envelope-internal
                                           split-across-dateline)))))))

(defn envelope-area-in-steradians
  #?(:clj [^Envelope rect]
     :cljs [rect])
  (let [a #?(:clj abs
             :cljs Math/abs)]
    (* (a (- (degrees->radians (.getMinX rect))
             (degrees->radians (.getMaxX rect))))
       (a
          (- (Math/sin (degrees->radians (.getMinY rect)))
             (Math/sin (degrees->radians (.getMaxY rect))))))))

(defn area-in-steradians
  "The solid angle area of a rectangle in steradians."
  #?(:clj [rect]
     :cljs [^js/jsts.geom.Rectangle rect])
  (let [crosses? #?(:clj (.getCrossesDateLine ^RectangleImpl rect)
                    :cljs (crosses-dateline? rect))]
      (if (not crosses?)
        (envelope-area-in-steradians rect)
        (reduce + (map envelope-area-in-steradians (-> rect split-across-dateline))))))


(defn area-in-square-degrees
  "The solid angle area of a rectangle in square degrees."
  [rect]
  #?(:clj (steradians->square-degrees (area-in-steradians rect))
                ;(.area vincenty-distance-calculator
               ;  ^Rectangle (to-shape rect))
     :cljs (steradians->square-degrees (area-in-steradians rect))))

(defn area
  "The area of a rectangle in square meters."
  [rect]
  (-> rect
      area-in-square-degrees
      square-degrees->steradians
      steradians->area))

#?(:clj
   (defn relate
  "The relationship between two shapes. Returns a keyword:

  :contains    a contains b
  :within      a falls within b
  :intersects  a and b have at least one point in common
  :disjoint    a and b have no points in common"
     [a b]
     (condp = (.relate (to-shape a) (to-shape b))
       SpatialRelation/DISJOINT    :disjoint
       SpatialRelation/INTERSECTS  :intersects
       SpatialRelation/WITHIN      :within
       SpatialRelation/CONTAINS    :contains)))

#?(:clj
   (defn intersects?
  "Do two shapes intersect in any way? Note that spatial4j's relate() considers
  intersection *different* from containment, e.g. if A completely surrounds B,
  their relation is not INTERSECTS. Spatial4j has a intersects() function on
  relations (the one used here) which considers two shapes intersecting if
  their intersection is non-empty; i.e. they are not disjoint."
     [a b]
     (.intersects (.relate (to-shape a) (to-shape b)))))

(defn dist-at-idx
  "Distance between the linestring's point at the given index and the subsequent
  point."
  [linestring idx]
  (distance (#?(:clj jts/point-n
                :cljs geo.jsts/point-n) linestring idx)
            (#?(:clj jts/point-n
                :cljs geo.jsts/point-n) linestring (inc idx))))

(defn length
  "Get geodesic length of a (jts) linestring by summing lengths of successive
  points"
  #?(:clj [^org.locationtech.jts.geom.LineString linestring]
     :cljs [^js/jsts.geom.Linestring linestring])
  (let [num-points (.getNumPoints linestring)]
    (if (= 0 num-points)
      0
      (loop [length 0.0
             idx 0]
        (if (= idx (dec num-points))
          length
          (recur (+ length (dist-at-idx linestring idx))
                 (inc idx)))))))

(defn within-dist? [p1 p2 dist]
  (<= (distance p1 p2) dist))

(defn point-between
  #?(:clj [^Coordinate c1 ^Coordinate c2 dist]
     :cljs [^js/jsts.geom.Coordinate c1 ^js/jsts.geom.Coordinate c2 dist])
  (let [ratio (/ dist (distance c1 c2))
        segment #?(:clj (org.locationtech.jts.geom.LineSegment. c1 c2)
                   :cljs (impl/line-segment c1 c2))]
    (.pointAlongOffset segment ratio 0)))

(defn- coord-list-length [coords]
  (if (< (count coords) 2)
    0
    (length (#?(:clj jts/linestring
                :cljs geo.jsts/linestring) coords))))

(defn- cut-point [current-segment next-coord segment-length]
  (let [current-length (coord-list-length current-segment)
        shortfall (- segment-length current-length)]
    (point-between (last current-segment) next-coord shortfall)))

(defn- under-cap-with-next-point? [coords next-coord dist]
  (< (length (#?(:clj jts/linestring
                 :cljs geo.jsts/linestring)
              (conj coords next-coord))) dist))

(defn- resegment-wgs84
  "Performs the resegment operation used in (resegment),
   with the assumption that a linestring is in WGS84 projection"
  [linestring segment-length]
  (loop [coords (#?(:clj jts/coords
                    :cljs geo.jsts/coords) linestring)
         segments []
         current []]
    (let [[next & remaining] coords]
      (cond
        (empty? coords) (map #?(:clj jts/linestring
                                :cljs geo.jsts/linestring)
                             (conj segments current))
        (empty? current) (recur remaining segments (conj current next))
        (under-cap-with-next-point? current next segment-length)
        (recur remaining segments (conj current next))
        :else (let [cut-point (cut-point current next segment-length)]
                (recur coords
                       (conj segments (conj current cut-point))
                       [cut-point]))))))

(defn resegment
  "Repartitions a JTS LineString into multiple contiguous linestrings, each up
   to the provided length (in meters). Final segment may be less than the
   requested length. Length of individual segments may vary a bit but total
   length should remain the same."
  [linestring segment-length]
  (let [srid (crs/get-srid linestring)]
    (map #(crs/transform-geom % srid)
         (resegment-wgs84 (crs/transform-geom linestring 4326)
                          segment-length))))

#?(:clj
   (def ^DistanceCalculator vincenty-distance-calculator
     (org.locationtech.spatial4j.distance.GeodesicSphereDistCalc$Vincenty.)))

#?(:clj
   (defn rand-point-in-radius
     "Get a random point around the given latitude and longitude within
     the given radius.

     (rand-point-in-radius 34.05656 -118.41881 100)
     (rand-point-in-radius 34.05656 -118.41881 100 :clustered)
     (rand-point-in-radius 34.05656 -118.41881 100 (fn [] 1))

     Returns org.locationtech.spatial4j.shape.jts.JtsPoint; Use
     geo.spatial/latitude and geo.spatial/longitude
     to retrieve raw coords.

     Accepts an optional 4th argument for customizing the distribution. Can be
     either :uniform or :clustered for built-in distributions, or a custom fn.

     Distribution fn should return a float between 0.0 and 1.0 when invoked.

     The built-in :clustered distribution uses a linear distribution of radius,
     which results in points clustered more heavily toward the center of the
     radius.

     :uniform uses an exponential distribution of radius which results in
     points being spread evenly across the circle.

     Default distribution is :uniform."
     ([center-lat center-lon radius-meters]
      (rand-point-in-radius center-lat center-lon radius-meters :uniform))
     ([center-lat center-lon radius-meters distribution]
      (assert (or (= :uniform distribution)
                  (= :clustered distribution)
                  (fn? distribution))
              "distribution must be :uniform, :clustered, or fn")
      (let [center (point center-lat center-lon)
            offset-fn (case distribution
                        :uniform (fn [] (Math/sqrt (rand)))
                        :clustered rand
                        distribution)
            radius (* (offset-fn) radius-meters)
            offset-degrees (-> radius
                               (distance-at-point->radians center)
                               (radians->degrees))
            angle-degrees (rand 360)]
        (.pointOnBearing vincenty-distance-calculator
                         center
                         offset-degrees
                         angle-degrees
                         earth
                         nil)))))
