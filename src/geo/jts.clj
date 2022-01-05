(ns geo.jts
  "Wrapper for the locationtech JTS spatial library. Constructors for points,
  coordinate sequences, rings, polygons, multipolygons, and so on."
  (:require [geo.crs :as crs :refer [Transformable]]
            [geo.impl.jts :as impl])
  (:import (org.locationtech.jts.geom Coordinate
                                      CoordinateSequence
                                      CoordinateXYZM
                                      Envelope
                                      Geometry
                                      GeometryCollection
                                      GeometryFactory
                                      Point
                                      LinearRing
                                      LineSegment
                                      LineString
                                      MultiPoint
                                      MultiLineString
                                      MultiPolygon
                                      Polygon
                                      PrecisionModel)))

(set! *warn-on-reflection* true)

(def ^PrecisionModel pm ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.impl.jts/pm." impl/pm)
(def ^GeometryFactory gf ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/get-geometry-factory."
  crs/get-geometry-factory)
(def default-srid ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/default-srid."
  crs/default-srid)
(def ^GeometryFactory gf-wgs84 ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/gf-wgs84."
  crs/gf-wgs84)
(def ^GeometryFactory get-factory ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/get-geometry-factory."
  crs/get-geometry-factory)
(def get-srid ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/get-srid."
  crs/get-srid)
(def set-srid ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/set-srid."
  crs/set-srid)
(def transform-geom ; Deprecated as of 3.1.0
  "Deprecated as of 3.1.0, in favor of geo.crs/transform-geom."
  crs/transform-geom)

(defn coordinate
  "Creates a Coordinate."
  ([x y]
   (impl/coordinate x y))
  ([x y z]
   (impl/coordinate x y z))
  ([x y z m]
   (impl/coordinate-xyzm x y z m)))

(defn ^Point point
  "Creates a Point from a Coordinate, a lat/long, or an x,y pair with an SRID."
  ([^Coordinate coordinate]
   (impl/point coordinate crs/gf-wgs84))
  ([lat long]
   (point (coordinate long lat)))
  ([x y srid]
   (impl/point (coordinate x y) (crs/get-geometry-factory srid))))

(defn ^"[Lorg.locationtech.jts.geom.Coordinate;" coord-array
  [coordinates]
  (into-array Coordinate coordinates))

(defn ^"[Lorg.locationtech.jts.geom.Point;" point-array
  [points]
  (into-array Point points))

(defn ^"[Lorg.locationtech.jts.geom.Geometry;" geom-array
  [geoms]
  (into-array Geometry geoms))

(defn ^"[Lorg.locationtech.jts.geom.LinearRing;" linear-ring-array
  [rings]
  (into-array LinearRing rings))

(defn ^"[Lorg.locationtech.jts.geom.LineString;" linestring-array
  [linestrings]
  (into-array LineString linestrings))

(defn ^"[Lorg.locationtech.jts.geom.Polygon;" polygon-array
  [polygons]
  (into-array Polygon polygons))

(defn ^CoordinateSequence coordinate-sequence
  "Given a list of Coordinates, generates a CoordinateSequence."
  [coordinates]
  (-> (.getCoordinateSequenceFactory crs/gf-wgs84)
      (.create (coord-array coordinates))))

(defn coord
  [^Point point]
  (.getCoordinate point))

(defn ^MultiPoint multi-point
  "Given a list of points, generates a MultiPoint."
  [points]
  (.createMultiPoint (crs/get-geometry-factory (first points))
                     (coordinate-sequence (map coord points))))


(defn ^GeometryCollection geometry-collection
  "Given a list of Geometries, generates a GeometryCollection."
  [geometries]
  (.createGeometryCollection (crs/get-geometry-factory (first geometries))
                             (geom-array geometries)))

(defn geometries
  "Given a GeometryCollection, generate a sequence of Geometries"
  [^GeometryCollection c]
  (mapv (fn [n] (.getGeometryN c n)) (range (.getNumGeometries c))))

(defn wkt->coords-array
  [flat-coord-list]
  (->> flat-coord-list
       (partition 2)
       (map (partial apply coordinate))))

(defn ^LineString linestring
  "Given a list of Coordinates, creates a LineString. Allows an optional SRID argument at end."
  ([coordinates]
   (.createLineString crs/gf-wgs84 (coordinate-sequence coordinates)))
  ([coordinates srid]
   (.createLineString (crs/get-geometry-factory srid) (coordinate-sequence coordinates))))

(defn ^MultiLineString multi-linestring
  "Given a list of LineStrings, generates a MultiLineString."
  [linestrings]
  (.createMultiLineString (crs/get-geometry-factory (first linestrings))
                          (linestring-array linestrings)))

(defn linestring-wkt
  "Makes a LineString from a WKT-style data structure: a flat sequence of
  coordinate pairs, e.g. [0 0, 1 0, 0 2, 0 0]. Allows an optional SRID argument at end."
  ([coordinates]
   (-> coordinates wkt->coords-array linestring))
  ([coordinates srid]
   (-> coordinates wkt->coords-array (linestring srid))))

(defn multi-linestring-wkt
  "Creates a MultiLineString from a WKT-style data structure, e.g. [[0 0, 1 0, 0 2, 0 0] [0 -1 1 2]].
  Allows an optional SRID argument at end."
  ([wkt]
   (multi-linestring (map linestring-wkt wkt)))
  ([wkt srid]
   (multi-linestring (map #(linestring-wkt % srid) wkt))))

(defn coords
  [^LineString linestring]
  (-> linestring .getCoordinateSequence .toCoordinateArray))

(defn point-n
  "Get the point for a linestring at the specified index."
  [^LineString linestring idx]
  (.getPointN linestring idx))

(defn line-segment
  "Given two Coordinates, creates a LineSegment."
  [^Coordinate c1 ^Coordinate c2]
  (LineSegment. c1 c2))

(defn segment-at-idx
  "LineSegment from a LineString's point at index to index + 1."
  [^LineString linestring idx]
  (line-segment (coord (point-n linestring idx))
                (coord (point-n linestring (inc idx)))))

(defn ^LinearRing linear-ring
  "Given a list of Coordinates, creates a LinearRing. Allows an optional SRID argument at end."
  ([coordinates]
   (.createLinearRing crs/gf-wgs84 (coordinate-sequence coordinates)))
  ([coordinates srid]
   (.createLinearRing (crs/get-geometry-factory srid) (coordinate-sequence coordinates))))

(defn linear-ring-wkt
  "Makes a LinearRing from a WKT-style data structure: a flat sequence of
  coordinate pairs, e.g. [0 0, 1 0, 0 2, 0 0]. Allows an optional SRID argument at end."
  ([coordinates]
   (-> coordinates wkt->coords-array linear-ring))
  ([coordinates srid]
   (-> coordinates wkt->coords-array (linear-ring srid))))

(defn ^Polygon polygon
  "Given a LinearRing shell, and a list of LinearRing holes, generates a
  polygon."
  ([shell]
   (polygon shell nil))
  ([^LinearRing shell holes]
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

(defn ^MultiPolygon multi-polygon
  "Given a list of polygons, generates a MultiPolygon."
  [polygons]
  (.createMultiPolygon (crs/get-geometry-factory (first polygons))
                       (polygon-array polygons)))

; Deprecated since 3.1.0.
(defn polygons
  "Given a MultiPolygon, generate a sequence of Polygons.
  Deprecated since 3.1.0, in favor of geo.jts/geometries."
  [^MultiPolygon m]
  (geometries m))

(defn multi-polygon-wkt
  "Creates a MultiPolygon from a WKT-style data structure, e.g. [[[0 0 1 0 2 2
  0 0]] [5 5 10 10 6 2]]. Allows an optional SRID argument at end."
  ([wkt]
   (multi-polygon (map polygon-wkt wkt)))
  ([wkt srid]
   (multi-polygon (map #(polygon-wkt % srid) wkt))))

(defn coordinates
  "Get a sequence of Coordinates from a Geometry"
  [^Geometry geom]
  (into [] (.getCoordinates geom)))

(defn same-srid?
  "Check if two Geometries have the same SRID. If both geometries have SRIDs of 0, will also return true."
  [^Geometry g1 ^Geometry g2]
  (= (crs/get-srid g1) (crs/get-srid g2)))

(defn same-coords?
  "Check if two Coordinates have the same number of dimensions and equal ordinates."
  [^Coordinate c1 ^Coordinate c2]
  (.equals2D c1 c2))

(defn same-geom?
  "Check if two geometries are topologically equal, with the same SRID.
  Two SRIDs of 0 are considered equal to each other."
  [^Geometry g1 ^Geometry g2]
  (and (same-srid? g1 g2)
       (.equalsTopo g1 g2)))

(defn ^Point centroid
  "Get the centroid of a JTS object."
  [^Geometry g]
  (.getCentroid g))

(defn intersection
  "Get the intersection of two geometries."
  [^Geometry g1 ^Geometry g2]
  (.intersection g1 g2))

(defn ^Envelope get-envelope-internal
  "Get a JTS envelope from a geometry."
  [^Geometry g]
  (.getEnvelopeInternal g))

(defn envelope
  "Create a JTS envelope from two coordinates."
  [c1 c2]
  (Envelope. c1 c2))

(defn subdivide
  "Subdivide a Geometry into quadrants around its centroid."
  [^Geometry g]
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
