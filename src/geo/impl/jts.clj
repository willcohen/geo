(ns geo.impl.jts
  (:import (org.locationtech.jts.geom Coordinate
                                      CoordinateSequence
                                      CoordinateXYZM
                                      Geometry
                                      GeometryFactory
                                      PrecisionModel
                                      Point
                                      MultiPoint
                                      LineString
                                      LinearRing
                                      MultiLineString
                                      Polygon
                                      MultiPolygon
                                      GeometryCollection)))

(set! *warn-on-reflection* true)

(def ^PrecisionModel pm (PrecisionModel. PrecisionModel/FLOATING))

(defn coordinate
  "Creates a Coordinate."
    ([x y]
     (Coordinate. x y))
    ([x y z]
     (Coordinate. x y z)))

(defn coordinate-xyzm
  "Creates a CoordinateXYZM."
  [x y z m]
  (CoordinateXYZM. x y z m))

(defn ^Point point
  "Creates a Point from a Coordinate, a lat/long, or an x,y pair with an SRID."
  [^Coordinate coordinate ^GeometryFactory gf]
  (.createPoint gf coordinate))

(defn ^MultiPoint multi-point
  "Given a list of points, generates a MultiPoint."
  [^CoordinateSequence points ^GeometryFactory gf]
  (.createMultiPoint gf points))

(defn ^LineString linestring
  "Given a list of Coordinates, creates a LineString.
  Allows an optional SRID argument at end."
  [^CoordinateSequence coordinates ^GeometryFactory gf]
  (.createLineString gf coordinates))

(defn ^LinearRing linear-ring
  "Given a list of Coordinates, creates a LinearRing.
  Allows an optional SRID argument at end."
  [^CoordinateSequence coordinates ^GeometryFactory gf]
  (.createLinearRing gf coordinates))

(defn ^MultiLineString multi-linestring
  "Given a list of LineStrings, generates a MultiLineString."
  [linestrings ^GeometryFactory gf]
  (.createMultiLineString gf linestrings))

(defn ^Polygon polygon
  "Given a LinearRing shell, and a list of LinearRing holes, generates a
  polygon."
  [^LinearRing shell ^GeometryFactory gf holes]
   (.createPolygon gf shell holes))

(defn ^MultiPolygon multi-polygon
  "Given a list of polygons, generates a MultiPolygon."
  [polygons ^GeometryFactory gf]
  (.createMultiPolygon gf polygons))

(defn ^GeometryCollection geometry-collection
  "Given a list of Geometries, generates a GeometryCollection."
  [geometries ^GeometryFactory gf]
  (.createGeometryCollection gf geometries))
