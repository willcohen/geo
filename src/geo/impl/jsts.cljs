(ns geo.impl.jsts
  (:require [jsts]))

(set! *warn-on-infer* true)

(def pm ^js/jsts.geom.PrecisionModel
  (jsts.geom.PrecisionModel.))

(defn ^js/jsts.geom.Coordinate coordinate
  "Creates a Coordinate."
  ([x y]
   (jsts.geom.Coordinate. x y))
  ([x y z]
   (jsts.geom.Coordinate. x y z)))

(defn ^js/jsts.geom.CoordinateXYZM coordinate-xyzm
  "Creates a CoordinateXYZM."
  [x y z m]
  (jsts.geom.CoordinateXYZM. x y z m))

(defn ^js/jsts.geom.GeometryFactory geometry-factory
  ([srid]
   (geometry-factory pm srid))
  ([pm srid]
   (jsts.geom.GeometryFactory. pm srid)))

(defn ^js/jsts.geom.Point point
  "Creates a Point from a Coordinate, a lat/long, or an x,y pair with an SRID."
  [^js/jsts.geom.Coordinate coordinate ^js/jsts.geom.GeometryFactory gf]
  (.createPoint gf coordinate))

(defn ^js/jsts.geom.MultiPoint multi-point
  "Given a list of points, generates a MultiPoint."
  [points ^js/jsts.geom.GeometryFactory gf]
  (.createMultiPoint gf points))

(defn ^js/jsts.geom.LineString linestring
  "Given a list of Coordinates, creates a LineString. Allows an optional SRID argument at end."
  [coordinates ^js/jsts.geom.GeometryFactory gf]
  (.createLineString gf coordinates))

(defn ^js/jsts.geom.LineSegment line-segment
  "Given two Coordinates, creates a LineSegment."
  [^js/jsts.geom.Coordinate c1 ^js/jsts.geom.Coordinate c2]
  (jsts.geom.LineSegment. c1 c2))

(defn ^js/jsts.geom.LinearRing linear-ring
  "Given a list of Coordinates, creates a LinearRing.
  Allows an optional SRID argument at end."
  [coordinates ^js/jsts.geom.GeometryFactory gf]
  (.createLinearRing gf coordinates))

(defn ^js/jsts.geom.MultiLineString multi-linestring
  "Given a list of LineStrings, generates a MultiLineString."
  [linestrings ^js/jsts.geom.GeometryFactory gf]
  (.createMultiLineString gf linestrings))

;; polygon
(defn ^js/jsts.geom.Polygon polygon
  "Given a LinearRing shell, and a list of LinearRing holes, generates a
  polygon."
  [^js/jsts.geom.LinearRing shell ^js/jsts.geom.GeometryFactory gf holes]
  (.createPolygon gf shell holes))

(defn ^js/jsts.geom.MultiPolygon multi-polygon
  "Given a list of polygons, generates a MultiPolygon."
  [polygons ^js/jsts.geom.GeometryFactory gf]
  (.createMultiPolygon gf polygons))

(defn ^js/jsts.geom.GeometryCollection geometry-collection
  "Given a list of Geometries, generates a GeometryCollection."
  [geometries ^js/jsts.geom.GeometryFactory gf]
  (.createGeometryCollection gf geometries))

(defn ^js/jsts.geom.CoordinateSequenceFactory
  get-coordinate-sequence-factory
  [^js/jsts.geom.GeometryFactory gf]
  (.getCoordinateSequenceFactory gf))

(defn ^js/jsts.geom.CoordinateSequence
  get-coordinate-sequence
  [^js/jsts.geom.Geometry g]
  (.getCoordinateSequence g))

(defn ^js/jsts.geom.Envelope
  get-envelope-internal
  [^js/jsts.geom.Geometry g]
  (.getEnvelopeInternal g))

(defn get-width
  [^js/jsts.geom.Envelope e]
  (.getWidth e))

(defn get-height
  [^js/jsts.geom.Envelope e]
  (.getHeight e))

(defn get-geometry-type
  [^js/jsts.geom.Geometry g]
  (.getGeometryType g))

;; (defn copy
;;   [g]
;;   (case (get-geometry-type g)
;;     "Point" ^js/jsts.geom.Point (.copy ^js/jsts.geom.Point g)
;;     "MultiPoint" ^js/jsts.geom.MultiPoint (.copy ^js/jsts.geom.MultiPoint g)
;;     "LineString" ^js/jsts.geom.LineString (.copy ^js/jsts.geom.LineString g)
;;     "LinearRing" ^js/jsts.geom.LinearRing (.copy ^js/jsts.geom.LinearRing g)
;;     "MultiLineString" ^js/jsts.geom.MultiLineString (.copy ^js/jsts.geom.MultiLineString g)
;;     "Polygon" ^js/jsts.geom.Polygon (.copy ^js/jsts.geom.Polygon g)
;;     "MultiPolygon" ^js/jsts.geom.MultiPolygon (.copy ^js/jsts.geom.MultiPolygon g)
;;     "GeometryCollection" ^js/jsts.geom.GeometryCollection (.copy ^js/jsts.geom.GeometryCollection g)))
