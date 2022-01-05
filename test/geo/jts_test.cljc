(ns geo.jts-test
  (:require [geo.impl.testing-util :refer [approximately]]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as jts])
            [geo.crs :as crs]
            [geo.geohash :as geohash]
            [geo.io :as io]
            [geo.spatial :as spatial]
            [clojure.test :refer [deftest testing is]]
            #?(:cljs [jsts]))
  #?(:clj (:import (org.locationtech.jts.geom Coordinate CoordinateXYZM))))

(def to-jts
  #?(:clj spatial/to-jts
     :cljs spatial/to-jsts))

(deftest coordinate
  (testing "coordinate"
    (testing "XY coordinate"
      (is (= #?(:clj (Coordinate. 1 2)
                :cljs (jsts.geom.Coordinate. 1 2)))
          (jts/coordinate 1 2))
      (is (= 1.0 (.getX (jts/coordinate 1 2))))
      (is (= 2.0 (.getY (jts/coordinate 1 2))))
      (is (true? (#?(:clj Double/isNaN
                     :cljs js/Number.isNaN)
                  (.getZ (jts/coordinate 1 2))))))
    (testing "XYZ coordinate"
      (is (= #?(:clj (Coordinate. 1 2 3)
                :cljs (jsts.geom.Coordinate. 1 2 3)))
          (jts/coordinate 1 2 3))
      (is (= 1.0 (.getX (jts/coordinate 1 2 3))))
      (is (= 2.0 (.getY (jts/coordinate 1 2 3))))
      (is (= 3.0 (.getZ (jts/coordinate 1 2 3)))))
    (testing "XYZ coordinates can still be pulled out from geometries that don't support higher dimensions"
      (is (= 3.0 (.getZ (.getCoordinate (jts/point (jts/coordinate 1 2 3))))))
      (is (= 3.0
             (.getZ (.getCoordinateN (jts/linestring [(jts/coordinate 1 2 3)
                                                      (jts/coordinate 4 5 6)])
                                     0)))))
    (testing "XYZM coordinate"
      (is (= #?(:clj (CoordinateXYZM. 1 2 3 4)
                :cljs (jsts.geom.CoordinateXYZM. 1 2 3 4)))
          (jts/coordinate 1 2 3 4))
      (is (= 1.0 (.getX (jts/coordinate 1 2 3 4))))
      (is (= 2.0 (.getY (jts/coordinate 1 2 3 4))))
      (is (= 3.0 (.getZ (jts/coordinate 1 2 3 4))))
      (is (= 4.0 (.getM (jts/coordinate 1 2 3 4)))))
    (testing "XYZM coordinates can still be pulled out from geometries that don't support higher dimensions"
      (is (= 4.0 (.getM (.getCoordinate (jts/point (jts/coordinate 1 2 3 4))))))
      (is (= 4.0
             (.getM (.getCoordinateN (jts/linestring [(jts/coordinate 1 2 3 4)
                                                      (jts/coordinate 4 5 6 7)])
                                     0)))))))

(deftest multi-point
  (testing "multi-point"
    (is (jts/same-geom?
         (io/read-wkt "MULTIPOINT ((0 0), (1 1))")
         (jts/multi-point [(jts/point 0 0) (jts/point 1 1)])))))

(deftest coordinate-sequences
  (testing "coordinate sequences"
    (testing "XY/XYZ coordinate sequence"
      (is (= 3 (.getDimension
                (jts/coordinate-sequence [(jts/coordinate 1 1)
                                          (jts/coordinate 2 2)]))))
      (is (= 3 (.getDimension
                (jts/coordinate-sequence [(jts/coordinate 1 1 1)
                                          (jts/coordinate 2 2 2)])))))
    (testing "XYZM coordinate sequence"
      (is (= 4 (.getDimension
                (jts/coordinate-sequence [(jts/coordinate 1 1 1 1)
                                          (jts/coordinate 2 2 2 2)])))))))

(deftest polygon
  (testing "polygon"
    (is (jts/same-geom?
         (io/read-wkt "POLYGON ((0 0, 10 0, 10 10, 0 0))")
         (->> [0 0 10 0 10 10 0 0]
               (partition 2)
               (map (partial apply jts/coordinate))
               jts/linear-ring
               jts/polygon)))))

(deftest multipolygon-wkt
  (testing "multipolygon-wkt"
    (is (= (io/to-wkt (io/read-wkt "MULTIPOLYGON (((10 10, 110 10, 110 110, 10 110, 10 10), (20 20, 20 30, 30 30, 30 20, 20 20), (40 20, 40 30, 50 30, 50 20, 40 20)))"))
           (io/to-wkt (jts/multi-polygon-wkt
                       [[[10 10, 110 10, 110 110, 10 110, 10 10],
                         [20 20, 20 30, 30 30, 30 20, 20 20],
                         [40 20, 40 30, 50 30, 50 20, 40 20]]]))))))

(deftest polygons-multipolygon
  (testing "multipolygon to polygons"
    (is (= (io/to-wkt (io/read-wkt "POLYGON ((-1 -1, 11 -1, 11 11, -1 -1))"))
         (io/to-wkt (first (jts/geometries
                            (jts/multi-polygon-wkt
                             [[[-1 -1 11 -1 11 11 -1 -1]],
                              [[0 0 10 0 10 10 0 0]]])))))))
  (testing "polygons to multipolygon"
    (is (= (io/to-wkt (io/read-wkt "MULTIPOLYGON (((-1 -1, 11 -1, 11 11, -1 -1)), ((0 0, 10 0, 10 10, 0 0)))"))
           (io/to-wkt (jts/multi-polygon
                       [(jts/polygon-wkt [[-1 -1 11 -1 11 11 -1 -1]])
                        (jts/polygon-wkt [[0 0 10 0 10 10 0 0]])])))))
  (testing "multipolygon SRIDs"
    (is (= 4326
           (-> (jts/multi-polygon
                [(to-jts (geohash/geohash "u4pruy"))
                 (to-jts (geohash/geohash "u4pruu"))])
               crs/get-srid)))
    (is (= 4326
           (-> (jts/multi-polygon
                [(to-jts (geohash/geohash "u4pruy"))
                 (to-jts (geohash/geohash "u4pruu"))])
                  jts/geometries
                  first
                  crs/get-srid)))))

(deftest geometries-geometrycollection
  (testing "point geometries <> geometrycollection"
    (is (jts/same-geom?
         (io/read-wkt "POINT (0 0)")
         (-> [(jts/point 0 0)
              (jts/point 1 1)]
             jts/geometry-collection
             jts/geometries
             first))))
  (testing "geometries <> geometrycollection srids"
    (is (= 2229
           (-> [(jts/point 0 0 2229)
                (jts/point 0 0 2229)]
               jts/geometry-collection
               jts/geometries
               first
               crs/get-srid)))))

(deftest line-segments
  (testing "line segments"
    (let [c1 (jts/coordinate 0 0)
          c2 (jts/coordinate 1 1)
          ls (jts/line-segment c1 c2)]
      (is (= (type ls)
             #?(:clj org.locationtech.jts.geom.LineSegment
                :cljs jsts.geom.LineSegment)))
      (is (jts/same-coords? c1 (.getCoordinate ls 0)))
      (is (jts/same-coords? c2 (.getCoordinate ls 1))))))

(deftest linestrings
  (testing "linestrings"
    (is (= 3 (.getNumPoints (jts/linestring-wkt [0 0 0 1 0 2]))))
    (is (= #?(:clj org.locationtech.jts.geom.Coordinate
              :cljs jsts.geom.Coordinate)
           (-> [0 0 0 1 0 2]
               jts/linestring-wkt
               jts/coords
               first
               type)))
    (is (= 3 (count (jts/coords (jts/linestring-wkt [0 0 0 1 0 2])))))
    (is (= 3 (-> [0 0 0 1 0 2]
                 jts/linestring-wkt
                 jts/coords
                 jts/linestring
                 .getNumPoints)))
    (is (= 0.0 (-> [0 0 0 1 0 2]
                   jts/linestring-wkt
                   (jts/point-n 1)
                   .getX)))
    (is (= 1.0 (-> [0 0 0 1 0 2]
                   jts/linestring-wkt
                   (jts/point-n 1)
                   .getY)))
    (let [s (jts/segment-at-idx (jts/linestring-wkt [0 -1 1 2]) 0)]
      (is (= #?(:clj org.locationtech.jts.geom.LineSegment
                :cljs jsts.geom.LineSegment)
             (type s)))
      (is (= 0.0 (-> s (.getCoordinate 0) .getX)))
      (is (= -1.0 (-> s (.getCoordinate 0) .getY)))
      (is (= 1.0 (-> s (.getCoordinate 1) .getX)))
      (is (= 2.0 (-> s (.getCoordinate 1) .getY))))))

(deftest multi-linestrings
  (testing "multi-linestrings"
    (is (= "MULTILINESTRING ((0 0, 1 1), (2 2, 3 3))"
           (-> [(jts/linestring [(jts/coordinate 0 0) (jts/coordinate 1 1)])
                (jts/linestring [(jts/coordinate 2 2) (jts/coordinate 3 3)])]
               jts/multi-linestring
               geo.io/to-wkt))))
  (testing "multi-linestring-wkt"
    (is (= "MULTILINESTRING ((0 0, 1 0, 0 2, 0 0), (0 -1, 1 2))")
        (-> [[0 0, 1 0, 0 2, 0 0] [0 -1 1 2]]
            jts/multi-linestring-wkt
            geo.io/to-wkt))))

(deftest geometry-srids
  (testing "Comparing geometry SRIDs"
    (let [g1 (jts/linestring-wkt [0 0 0 1 0 2])
          g2 (jts/linestring-wkt [1 1 2 2 3 3])
          g3 (jts/linestring-wkt [1 1 2 2 3 3] 1234)]
      (is (= 4326 (crs/get-srid g1)))
      (is (true? (jts/same-srid? g1 g2)))
      (is (true? (jts/same-srid? (crs/set-srid g1 0)
                                 (crs/set-srid g2 0))))
      (is (false? (jts/same-srid? g1 g3))))))

(deftest setting-srid
  (testing "setting SRID for a geom"
    (is (= 4326 (crs/get-srid (jts/point 0 0))))
    (is (= 23031 (crs/get-srid (crs/set-srid (jts/point 0 0) 23031))))))

(deftest proj
  (testing "point: 3 param transform"
    (is (true? (jts/same-geom?
                (crs/transform-geom
                 (jts/point 3.8142776 51.285914 4326) 23031)
                #?(:clj (jts/point 556878.9016076007
                                   5682145.166264554
                                   23031)
                   :cljs (jts/point 556878.9016075983
                                    5682145.166262922
                                    23031)))))

    (is (true? (jts/same-geom?
                (-> (jts/point 3.8142776 51.285914 4326)
                    (crs/transform-geom "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs")
                    (crs/set-srid 23031))
                #?(:clj (jts/point 556878.9016076007
                                   5682145.166264554
                                   23031)
                   :cljs (jts/point 556878.9016075983
                                    5682145.166262922
                                    23031)))))

    (is ((approximately 51.285914 0.000001)
         (spatial/latitude (jts/point 556878.9016075983
                                      5682145.166264554 23031))))

    (is ((approximately 3.8142776 0.000001)
         (spatial/longitude (jts/point 556878.9016075983
                                       5682145.166264554 23031)))))

  (testing "geometry: stereographic azimuthal, using a linestring"
    (is (true? (jts/same-geom?
                (-> [0 -75 -57.65625 -79.21875]
                    jts/linestring-wkt
                    (crs/transform-geom 3031))
                (-> [0 1638783.2384072358
                     #?(:clj -992481.6337864351
                        :cljs -992481.6337864349)
                     #?(:clj 628482.0632797639
                        :cljs 628482.0632797638)]
                    (jts/linestring-wkt 3031))))))

  (testing "geometry: stereographic azimuthal, using linestring, w/ proj string"
    (is (true? (jts/same-geom?
                (-> [0 -75 -57.65625 -79.21875]
                    jts/linestring-wkt
                    (crs/transform-geom
                     "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
                (-> [0 1638783.2384072358
                     #?(:clj -992481.6337864351
                        :cljs -992481.6337864349)
                     #?(:clj 628482.0632797639
                        :cljs 628482.0632797638)]
                    (jts/linestring-wkt 0))))))
  (testing "geometry: projecting from a geometry with 0 SRID using only target CRS throws exception"
    (is (thrown? #?(:clj java.lang.AssertionError
                    :cljs js/Error)
                 (crs/transform-geom (jts/point 10 10 0) 4326))))

  (testing "geometry: projecting from a geometry with a 0 SRID using both a source and target CRS"
    (is (true? (jts/same-geom?
                (crs/transform-geom (jts/point 10 10 0) 4326 4326)
                (jts/point 10 10 4326)))))

  (testing "geometry: projection can happen using an external transform object, though SRID may be set to 0 if it cannot be determined."
    (is (true? (jts/same-geom?
                (crs/transform-geom
                 (jts/point 3.8142776 51.285914 4326)
                 (crs/create-transform 4326 23031))
                (jts/point
                 #?(:clj 556878.9016076007
                    :cljs 556878.9016075983)
                 #?(:clj 5682145.166264554
                    :cljs 5682145.166262922) 23031)))))

  (testing "An EPSG can be specified as a number, an 'EPSG:XXXX' string, as an equivalent proj4 string, or a proj4j CRS object."
    (let [p1 (jts/point 3.8142776 51.285914 4326)
          p2 (jts/point
              #?(:clj 556878.9016076007
                 :cljs 556878.9016075983)
              #?(:clj 5682145.166264554
                 :cljs 5682145.166262922) 23031)]
      (is (true? (jts/same-geom?
                  (crs/transform-geom p1 23031) p2)))
      (is (true? (jts/same-geom?
                  (crs/transform-geom p1 "EPSG:23031") p2)))
      (is (= 23031 (crs/get-srid (crs/transform-geom p1 23031))))
      (is (= 23031 (crs/get-srid (crs/transform-geom p1 "EPSG:23031"))))
      (is (= 23031 (crs/get-srid (crs/transform-geom
                                  p1 (crs/create-crs 23031)))))))

  (testing "If using a different CRS name or proj4 string, SRID is not automatically set"
    (let [p1 (jts/point 3.8142776 51.285914 4326)
          p2 (jts/point
              #?(:clj 556878.9016076007
                 :cljs 556878.9016075983)
              #?(:clj 5682145.166264554
                 :cljs 5682145.166262922) 23031)]
      (is (true? (jts/same-geom?
                  (-> (crs/transform-geom p1 "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs")
                      (crs/set-srid 23031))
                  p2)))))

  (testing "crs/set-srid can take any Transformable"
    (let [p1 (jts/point 10 10 0)]
      (is (= 23031 (crs/get-srid (crs/set-srid
                                  p1 (crs/create-crs 23031)))))
      (is (= 23031 (crs/get-srid (crs/set-srid
                                  p1 (crs/create-crs "EPSG:23031")))))))

  (testing "CRS systems with different names"
    (let [p1 (jts/point 42.3601 -71.0589)
          p2 (crs/transform-geom p1 26986)]
      (is (= (.getX p2) (.getX (crs/transform-geom p1 "EPSG:26986"))))
      (is ((approximately (.getX p2) 0.001)
           (.getX (crs/transform-geom p1 "ESRI:26986"))))
      #?(:clj (is (= (.getX p2) (.getX (crs/transform-geom p1 "NAD83:2001")))))
      (is (= (.getX p2)
             (.getX (crs/transform-geom p1 "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m +no_defs"))))))

  (testing "An external GeometryFactory can be passed"
    (let [p1 (jts/point 42.3601 -71.0589)
          p2 (crs/transform-geom p1 3586)
          p3 (crs/transform-geom p1 3586 (crs/get-geometry-factory 3586))
          p4 (crs/transform-geom p1 3586 (crs/get-geometry-factory p2))]
      (is (= 3586 (crs/get-srid p2)))
      (is (= 3586 (crs/get-srid p3)))
      (is (= 3586 (crs/get-srid p4)))
      (is (true? (jts/same-geom? p2 p3)))
      (is (true? (jts/same-geom? p2 p4)))
      (is (true? (jts/same-geom? p3 p4)))))

  (testing "When passing two CRSs and a GeometryFactory, the GeometryFactory's SRID will be used."
    (let [c1 4326
          c2 3586
          s1 (crs/create-crs c1)
          s2 (crs/create-crs c2)
          f1 (crs/get-geometry-factory 3586)
          f_false (crs/get-geometry-factory 9999)
          t1 (crs/create-transform 4326 3586)
          p1 (jts/point 42.3601 -71.0589)
          p2 (crs/transform-geom p1 c1 c2 f1)
          p3 (crs/transform-geom p1 s1 s2 f1)
          p4 (crs/transform-geom p1 t1 f1)
          p2_false (crs/transform-geom p1 c1 c2 f_false)
          p3_false (crs/transform-geom p1 s1 s2 f_false)
          p4_false (crs/transform-geom p1 t1 f_false)]
      (is (true? (jts/same-geom? p2 p3)))
      (is (true? (jts/same-geom? p2 p4)))
      (is (true? (jts/same-geom? p3 p4)))
      (is (= 9999 (crs/get-srid p2_false)))
      (is (= 9999 (crs/get-srid p3_false)))
      (is (= 9999 (crs/get-srid p4_false)))
      (is (false? (jts/same-geom? p2 p2_false)))
      (is (false? (jts/same-geom? p3 p3_false)))
      (is (false? (jts/same-geom? p4 p4_false)))
      (is (true? (jts/same-geom? p2_false p3_false)))
      (is (true? (jts/same-geom? p2_false p4_false)))
      (is (true? (jts/same-geom? p3_false p4_false)))))

  (testing "When passing one Transformable and one GeometryFactory, determine whether
            Transformable is target or source depending on if Geometry has SRID."
    (let [g1 (jts/point 42.3601 -71.0589)
          g2 (jts/point -71.0589 42.3601 0)
          g3 (crs/transform-geom g1 3586)
          f1 (crs/get-geometry-factory 4326)
          f2 (crs/get-geometry-factory 3586)]
      (is (true? (jts/same-geom? (crs/transform-geom g1 3586 f2) g3)))
      (is (false? (jts/same-geom? (crs/transform-geom g2 3586 f2) g3)))
      (is (true? (jts/same-geom?
                  (crs/transform-geom (crs/set-srid g2 4326) 3586 f2) g3)))
      (is (false? (jts/same-geom? (crs/transform-geom g2 3586 f2) g1)))
      (is (true? (jts/same-geom?
                  (crs/set-srid (crs/transform-geom g2 3586 f2) 4326) g1)))))

  (testing "When passing a CoordinateTransform and a GeometryFactory, the GeometryFactory's SRID will be used."
    (let [c1 4326
          c2 3586
          f1 (crs/get-geometry-factory 3586)
          f2 (crs/get-geometry-factory 4326)
          t1 (crs/create-transform 4326 3586)
          p1 (jts/point 42.3601 -71.0589)
          p2 (crs/transform-geom p1 t1)
          p3 (crs/transform-geom p1 t1 f1)
          p4 (crs/transform-geom p1 t1 f2)]
      (is (= 4326 (crs/get-srid p1)))
      (is (= 3586 (crs/get-srid p2)))
      (is (= 3586 (crs/get-srid p3)))
      (is (= 4326 (crs/get-srid p4))))))
