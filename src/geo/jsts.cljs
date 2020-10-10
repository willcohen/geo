(ns geo.jsts
  "Wrapper for the JSTS spatial library. Constructors for points,
  coordinate sequences, rings, polygons, multipolygons, and so on."
  (:require [geo.crs :as crs :refer [Transformable]]
            [geo.impl.jsts :as impl]
            [jsts]))

(set! *warn-on-infer* true)

(defn coordinate
  "Creates a Coordinate."
  ([x y]
   (impl/coordinate x y))
  ([x y z]
   (impl/coordinate x y z))
  ([x y z m]
   (impl/coordinate-xyzm x y z m)))

(defn point
  "Creates a Point from a Coordinate, a lat/long, or an x,y pair with an SRID."
  ([coordinate]
   (impl/point coordinate crs/gf-wgs84))
  ([lat long]
   (point (coordinate long lat)))
  ([x y srid]
   (impl/point (coordinate x y) (crs/get-geometry-factory srid))))

(defn coord-array
  [coordinates]
  (into-array jsts.geom.Coordinate coordinates))

(defn point-array
  [points]
  (into-array jsts.geom.Point points))

(defn geom-array
  [geoms]
  (into-array jsts.geom.Geometry geoms))

(defn linear-ring-array
  [rings]
  (into-array jsts.geom.LinearRing rings))

(defn linestring-array
  [linestrings]
  (into-array jsts.geom.LineString linestrings))

(defn polygon-array
  [polygons]
  (into-array jsts.geom.Polygon polygons))

(defn multi-point
  "Given a list of points, generates a MultiPoint."
  [points]
  (.createMultiPoint (crs/get-geometry-factory (first points))
                     (point-array points)))

(defn ^js/jsts.geom.CoordinateSequence coordinate-sequence
  "Given a list of Coordinates, generates a CoordinateSequence."
  [coordinates]
  (.create
   ;(jsts.geom.impl.CoordinateArraySequenceFactory.)
   ;^js/jsts.geom.CoordinateSequenceFactory
   ;(.getCoordinateSequenceFactory ^jsts.geom.GeometryFactory crs/gf-wgs84)
   crs/csf-wgs84
   ^js/Array (coord-array coordinates)))

(defn ^js/jsts.geom.GeometryCollection geometry-collection
  "Given a list of Geometries, generates a GeometryCollection."
  [geometries]
  (.createGeometryCollection (crs/get-geometry-factory (first geometries))
                             (geom-array geometries)))

(defn geometries
  "Given a GeometryCollection, generate a sequence of Geometries"
  [^js/jsts.geom.GeometryCollection c]
  (mapv (fn [n] (.getGeometryN c n)) (range (.getNumGeometries c))))

(defn wkt->coords-array
  [flat-coord-list]
  (->> flat-coord-list
       (partition 2)
       (map (partial apply coordinate))))

(defn linestring
  "Given a list of Coordinates, creates a LineString.
  Allows an optional SRID argument at end."
  ([coordinates]
   (impl/linestring (coordinate-sequence coordinates) crs/gf-wgs84))
  ([coordinates srid]
   (impl/linestring (coordinate-sequence coordinates)
                    (impl/geometry-factory srid))))

(defn ^js/jsts.geom.MultiLineString multi-linestring
  "Given a list of LineStrings, generates a MultiLineString."
  [linestrings]
  (.createMultiLineString (crs/get-geometry-factory (first linestrings))
                          (linestring-array linestrings)))

(defn linestring-wkt
  "Makes a LineString from a WKT-style data structure: a flat sequence of
  coordinate pairs, e.g. [0 0, 1 0, 0 2, 0 0].
  Allows an optional SRID argument at end."

  ([coordinates]
   (-> coordinates wkt->coords-array linestring))
  ([coordinates srid]
   (-> coordinates wkt->coords-array (linestring srid))))

(defn multi-linestring-wkt
  "Creates a MultiLineString from a WKT-style data structure,
  e.g. [[0 0, 1 0, 0 2, 0 0] [0 -1 1 2]].
  Allows an optional SRID argument at end."
  ([wkt]
   (multi-linestring (map linestring-wkt wkt)))
  ([wkt srid]
   (multi-linestring (map #(linestring-wkt % srid) wkt))))

(defn coords
  [^js/jsts.geom.LineString linestring]
  (-> linestring impl/get-coordinate-sequence .toCoordinateArray))

(defn coord
  [^js/jsts.geom.Point point]
  (.getCoordinate point))

(defn point-n
  "Get the point for a linestring at the specified index."
  [^js/jsts.geom.LineString linestring idx]
  (.getPointN linestring idx))

(def line-segment impl/line-segment)

(defn segment-at-idx
  "LineSegment from a LineString's point at index to index + 1."
  [^js/jsts.geom.LineString linestring idx]
  (line-segment (coord (point-n linestring idx))
                (coord (point-n linestring (inc idx)))))

(defn ^js/jsts.geom.LinearRing linear-ring
  "Given a list of Coordinates, creates a LineString.
  Allows an optional SRID argument at end."
  ([coordinates]
   (impl/linear-ring (coordinate-sequence coordinates) crs/gf-wgs84))
  ([coordinates srid]
   (impl/linear-ring (coordinate-sequence coordinates)
                     (impl/geometry-factory srid))))

(defn linear-ring-wkt
  "Makes a LinearRing from a WKT-style data structure: a flat sequence of
  coordinate pairs, e.g. [0 0, 1 0, 0 2, 0 0].
  Allows an optional SRID argument at end."
  ([coordinates]
   (-> coordinates wkt->coords-array linear-ring))
  ([coordinates srid]
   (-> coordinates wkt->coords-array (linear-ring srid))))

(defn ^js/jsts.geom.Polygon polygon
  "Given a LinearRing shell, and a list of LinearRing holes, generates a
  polygon."
  ([^js/jsts.geom.LinearRing shell]
   (polygon shell nil))
  ([^js/jsts.geom.LinearRing shell holes]
   (.createPolygon (crs/get-geometry-factory shell) shell
                   (linear-ring-array holes))))

(defn polygon-wkt
  "Generates a polygon from a WKT-style data structure: a sequence of
  [outer-ring hole1 hole2 ...], where outer-ring and each hole is a flat list
  of coordinate pairs, e.g.

  [[0 0 10 0 10 10 0 0]
   [1 1  9 1  9  9 1 1]].

   Allows an optional SRID argument at end."
  ([rings]
   (polygon-wkt rings crs/gf-wgs84))
  ([rings srid]
   (let [rings (map #(linear-ring-wkt % srid) rings)]
     (polygon (first rings) (linear-ring-array (rest rings))))))

(defn ^js/jsts.geom.MultiPolygon multi-polygon
  "Given a list of polygons, generates a MultiPolygon."
  [polygons]
  (.createMultiPolygon (crs/get-geometry-factory (first polygons))
                       (polygon-array polygons)))

(defn multi-polygon-wkt
  "Creates a MultiPolygon from a WKT-style data structure, e.g. [[[0 0 1 0 2 2
  0 0]] [5 5 10 10 6 2]]. Allows an optional SRID argument at end."
  ([wkt]
   (multi-polygon (map polygon-wkt wkt)))
  ([wkt srid]
   (multi-polygon (map #(polygon-wkt % srid) wkt))))

(defn coordinates
  "Get a sequence of Coordinates from a Geometry"
  [^js/jsts.geom.Geometry geom]
  (into [] (.getCoordinates geom)))

(defn same-srid?
  "Check if two Geometries have the same SRID. If both geometries have SRIDs
  of 0, will also return true."
  [^js/jsts.geom.Geometry g1 ^js/jsts.geom.Geometry g2]
  (= (crs/get-srid g1) (crs/get-srid g2)))

(defn same-coords?
  "Check if two Coordinates have the same number of dimensions
  and equal ordinates."
  [^js/jsts.geom.Coordinate c1 ^js/jsts.geom.Coordinate c2]
  (.equals2D c1 c2))

(defn same-geom?
  "Check if two geometries are topologically equal, with the same SRID.
  Two SRIDs of 0 are considered equal to each other."
  [^js/jsts.geom.Geometry g1 ^js/jsts.geom.Geometry g2]
  (and (same-srid? g1 g2)
       (.equalsTopo g1 g2)))

(defn ^js/jsts.geom.Point centroid
  "Get the centroid of a JSTS object."
  [^js/jsts.geom.Geometry g]
  (.getCentroid g))

(defn intersection
  "Get the intersection of two geometries."
  [^js/jsts.geom.Geometry g1 ^js/jsts.geom.Geometry g2]
  (.intersection g1 g2))

(defn ^js/jsts.geom.Envelope get-envelope-internal
  "Get a JSTS envelope from a geometry."
  [^js/jsts.geom.Geometry g]
  (.getEnvelopeInternal g))

(defn envelope
  "Create a JSTS envelope from two coordinates."
  [c1 c2]
  (jsts.geom.Envelope. c1 c2))

(defn subdivide
  "Subdivide a Geometry into quadrants around its centroid."
  [^js/jsts.geom.Geometry g]
  (let [e (get-envelope-internal g)
        c (centroid g)
        c-x (.getX c)
        c-y (.getY c)
        min-x (.getMinX e)
        min-y (.getMinY e)
        max-x (.getMaxX e)
        max-y (.getMaxY e)
        gf (crs/get-geometry-factory g)
        make-quadrant (fn [c1 c2] (.toGeometry gf (envelope c1 c2)))
        q1 (make-quadrant (coord c) (coordinate max-x max-y))
        q2 (make-quadrant (coordinate min-x c-y) (coordinate c-x max-y))
        q3 (make-quadrant (coordinate min-x min-y) (coord c))
        q4 (make-quadrant (coordinate c-x min-y) (coordinate max-x c-y))]
    (map #(intersection g %) [q1 q2 q3 q4])))
