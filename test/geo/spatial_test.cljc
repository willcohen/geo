(ns geo.spatial-test
  (:require [geo.impl.testing-util :refer [approximately]]
            [geo.crs :as crs]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as jts])
            [geo.spatial :as s]
            [geo.geohash :as geohash]
            [clojure.test :refer [deftest testing is]])
  #?(:clj (:import (org.locationtech.spatial4j.context SpatialContext)
                   (org.locationtech.spatial4j.shape.jts JtsGeometry))))

#?(:clj
   (deftest earth
     (testing "earth"
       (is (partial instance? SpatialContext)
           s/earth))))

(deftest earth-radius
  (testing "accurate at equator"
    (is ((approximately s/earth-equatorial-radius 0.0001)
         (s/earth-radius (#?(:clj s/geohash-point
                             :cljs s/jsts-point) 0 0)))))
  (testing "accurate at pole"
    (is ((approximately s/earth-polar-radius 0.0001)
         (s/earth-radius (#?(:clj s/geohash-point
                             :cljs s/jsts-point) 90 0)))))
  ; A particularly special point in France which we know the radius for
  (testing "accurate in France"
    (is ((approximately 6366197 10) ; 10
         (s/earth-radius (#?(:clj s/geohash-point
                             :cljs s/jsts-point) 48.46791 0))))))

(deftest degrees->radians
  (is (= (s/degrees->radians 0) 0.0))
  (is (= (s/degrees->radians 180) Math/PI))
  (is (= (s/degrees->radians 360) (* 2 Math/PI))))

(deftest radians->degrees
  (is (= (s/radians->degrees 0) 0.0))
  (is (= (s/radians->degrees Math/PI) 180.0))
  (is (= (s/radians->degrees (* 2 Math/PI)) 360.0)))

(deftest square-degrees->steradians
  (is ((approximately 1)
       (s/square-degrees->steradians
        (s/square (/ 180 Math/PI))))))

(deftest distance->radians
  (is (= (s/distance->radians 0) 0.0))
  (is ((approximately (* 2 Math/PI) 0.002)
       (s/distance->radians
        s/earth-mean-circumference))))

(deftest radians->distance
  (is (= (s/radians->distance 0) 0.0))
  (is ((approximately s/earth-mean-circumference 12000)
       (s/radians->distance (* 2 Math/PI)))))

#?(:clj
   (deftest interchangeable-points
          (let [g->s (comp s/to-spatial4j-point s/to-geohash-point)
                s->g (comp s/to-geohash-point s/to-spatial4j-point)
                j->s (comp s/to-spatial4j-point s/to-jts)
                s->j (comp s/to-jts s/to-spatial4j-point)
                j->g (comp s/to-geohash-point s/to-jts)
                g->j (comp s/to-jts s/to-geohash-point)
                g->t (comp #(s/to-jts % 3586) s/to-geohash-point)
                t->g (comp s/to-geohash-point #(s/to-jts % 3586))
                s->t (comp #(s/to-jts % 3586) s/to-spatial4j-point)
                t->s (comp s/to-spatial4j-point #(s/to-jts % 3586))
                j->t (comp #(s/to-jts % 3586) #(s/to-jts % 4326))
                t->j (comp #(s/to-jts % 4326) #(s/to-jts % 3586))
                g1 (s/geohash-point 0 0)
                g2 (s/geohash-point 12.456 -98.765)
                s1 (s/spatial4j-point 0 0)
                s2 (s/spatial4j-point 12.456 -98.765)
                j1 (s/jts-point 0 0)
                j2 (s/jts-point 12.456 -98.765)
                t1 (jts/transform-geom j1 3586)
                t2 (jts/transform-geom j2 3586)]
            ; Identity conversions
            (is (= g2 (s/to-geohash-point g2)))
            (is (= s2 (s/to-spatial4j-point g2)))
            (is (= j2 (s/to-jts j2)))
            (is ((approximately (s/latitude t2))
                 (s/latitude (s/to-jts t2 3586))))

            ; Direct conversions
            (is (= s2 (s/to-spatial4j-point g2)))
            (is (= s2 (s/to-spatial4j-point j2)))
            (is ((approximately (s/latitude s2))
                 (s/latitude (s/to-spatial4j-point t2))))
            (is (= g2 (s/to-geohash-point s2)))
            (is (= (s/latitude g2)
                   (s/latitude (s/to-geohash-point j2))))
            (is (= j2 (s/to-jts s2)))
            (is (= j2 (s/to-jts g2)))
            (is (= t2 (s/to-jts t2)))
            (is (= t2 (s/to-jts g2 3586)))
            (is (= t2 (s/to-jts s2 3586)))
            (is (= t2 (s/to-jts j2 3586)))

            ; Two-way conversions
            (is (= g1 (s->g g1)))
            (is (= g2 (s->g g2)))
            (is (= g1 (j->g g1)))
            (is (= g2 (j->g g2)))
            (is ((approximately (s/latitude g1))
                 (s/latitude (t->g g1))))
            (is ((approximately (s/latitude g2))
                 (s/latitude (t->g g2))))
            (is (= s1 (g->s s1)))
            (is (= s2 (g->s s2)))
            (is (= s1 (j->s s1)))
            (is (= s2 (j->s s2)))
            (is ((approximately (s/latitude s1))
                 (s/latitude (t->s s1))))
            (is ((approximately (s/latitude s2))
                 (s/latitude (t->s s2))))
            (is (= j1 (s->j j1)))
            (is (= j2 (s->j j2)))
            (is (= j1 (g->j j1)))
            (is (= j2 (g->j j2)))
            (is ((approximately (s/longitude j1) 0.0001)
                 (s/longitude (t->j j1))))
            (is ((approximately (s/longitude j2))
                 (s/longitude (t->j j2)))))))

(deftest interchangeable-polygons
  (let [s->j #?(:clj (comp s/to-jts s/to-shape)
                :cljs identity)
        j1 (-> (->> [0 0 10 0 10 10 0 0]
                    (partition 2)
                    (map (partial apply jts/coordinate)))
               jts/linear-ring
               jts/polygon)]

    ; Identity conversion
    (is (= j1 (#?(:clj s/to-jts
                  :cljs s/to-jsts) j1)))

    ; Direct conversion
    #?(:clj (is (= JtsGeometry
                   (type (s/to-shape j1)))))

    ; Two-way conversion
    (is (= j1 (s->j j1)))))

#?(:clj
   (deftest spatial4j-circles-cannot-be-converted-to-JTS
     (let [cir1 (s/circle (s/point 0 0) 100)]

       ; Attempt to convert GeoCircle to JTS.
       (is (thrown? Exception (s/to-jts cir1)))

       ; Attempt to convert GeoCircle to projected JTS.
       (is (thrown? Exception (s/to-jts cir1 crs/gf-wgs84))))))

; Have some airports
(let [spatial4j-point #?(:clj s/spatial4j-point
                         :cljs s/jsts-point)
      geohash-point #?(:clj s/geohash-point
                       :cljs s/jsts-point)
      lhr (spatial4j-point 51.477500 -0.461388)
      syd (spatial4j-point -33.946110 151.177222)
      lax (spatial4j-point 33.942495 -118.408067)
      sfo (spatial4j-point 37.619105 -122.375236)
      oak (spatial4j-point 37.721306 -122.220721)
      ; and some distances between them, in meters
      lhr-syd 17015628
      syd-lax 12050690
      lax-lhr 8780169
      sfo-oak 17734]
  ; See http://www.gcmap.com/dist?P=LHR-SYD%2CSYD-LAX%2CLHR-LAX%0D%0A&DU=m&DM=&SG=&SU=mph
  (deftest distance-in-meters
    (is ((approximately 1.4E-9 2E-9)
         (s/distance (geohash-point 89.999999999999990 0)
                     (geohash-point 89.999999999999978 0))))
    (is ((approximately 1.4E-9 1E-10)
         (s/distance (geohash-point 89.9999999999999999 0)
                     (geohash-point 89.9999999999999920 0))))
    (is ((approximately 1.4E-9 1E-10)
         (s/distance (geohash-point 89.99999999999999999 0)
                     (geohash-point 89.99999999999999289 0))))
    (is ((approximately lhr-syd 1)
         (s/distance lhr syd)))
    (is ((approximately syd-lax 1)
         (s/distance syd lax)))
    (is ((approximately lax-lhr 1)
         (s/distance lax lhr)))
    (is ((approximately sfo-oak 1)
         (s/distance (geohash-point sfo)
                     (geohash-point oak)))))

  #?(:clj
     (deftest intersections
       (testing "A circle around a point intersects that point."
         (is (s/intersects? (s/circle lhr 10) lhr)))
       (testing "A circle around Sydney to London has an error of ~ 5 KM"
        ; This is a pretty darn big circle;
        ; covers more than half the globe.
         (is
          (not (s/intersects?
                syd (s/circle lhr (+ 4500 (s/distance lhr syd))))))
         (is (s/intersects? syd (s/circle lhr (+ 5000 (s/distance lhr syd))))))
       (testing "A circle around SFO to OAK has an error of roughly 13 meters."
         (is (s/intersects?
              sfo (s/circle oak (- (s/distance sfo oak) 12))))
         (is (not
              (s/intersects?
               sfo (s/circle oak (- (s/distance sfo oak) 14))))))))

  #?(:clj
     (deftest relationships
       (testing "relate returns keywords"
         (is (= :contains
                (s/relate (s/circle oak 10) (s/circle oak 10)))))))

  #?(:clj
     (deftest jts-multi-polygons
       (let [a (jts/multi-polygon-wkt [[[0 0, 2 0, 2 2, 0 0]]])
             b (jts/multi-polygon-wkt [[[0 0, 1 0, 0 1, 0 0]]])
             c (jts/multi-polygon-wkt [[[-1 -1, -2 -2, -1 -2, -1 -1]]])]
         (is (s/intersects? a b))
         (is (not (s/intersects? a c)))
         (is (not (s/intersects? b c))))))

  (deftest dateline-polygons
    (let [poly-wkt [[179 0 179 1 -179 1 -179 0 179 0]]]
      (is (= 1.0 (-> poly-wkt jts/polygon-wkt s/height)))
      (is (= 2.0 (-> poly-wkt jts/polygon-wkt s/width)))
      (is (= 2.0 (-> poly-wkt jts/polygon-wkt s/bounding-box s/width)))
      (is (= 2.0 (-> [poly-wkt] jts/multi-polygon-wkt s/width)))
      (is ((approximately 2.4727E10 1E6)
           (-> poly-wkt jts/polygon-wkt s/bounding-box s/area)))))


  (deftest non-dateline-polygons
    (let [poly-wkt [[0 0 0 1 2 1 2 0 0 0]]]
      (is (= 1.0 (-> poly-wkt jts/polygon-wkt s/height)))
      (is (= 2.0 (-> poly-wkt jts/polygon-wkt s/width)))
      (is (= 2.0 (-> poly-wkt jts/polygon-wkt s/bounding-box s/width)))
      (is (= 2.0 (-> [poly-wkt] jts/multi-polygon-wkt s/width)))
      (is ((approximately 2.4727E10 1E6)
           (-> poly-wkt jts/polygon-wkt s/bounding-box s/area))))))

  (deftest dateline-crossing
    (testing "Dateline-crossing geom handled properly with multiple to-shape calls"
       ;; Protecting against a previous bug
       ;; https://github.com/locationtech/spatial4j/issues/150
       (let [polygon (jts/polygon-wkt [[179 0 179 1 -179 1 -179 0 179 0]])]
         (is (= 2.0 (s/width polygon))))))

  #?(:clj
     (deftest centroid
       (is (= (s/spatial4j-point 5 5)
              (-> [[0 0, 10 0, 10 10, 0 10, 0 0]]
                  ; A little weird: centroids ignore holes in polygons
                  ; Maybe someday, try holes?
                  ; [1 1, 5 1,  5 9,   1 9,  1 1]]
                  jts/polygon-wkt
                  s/center)))))

(deftest linestring-length
  (is ((approximately 110574.38 0.01)
       (s/length (jts/linestring-wkt [0 0 0 1]))))
  (is ((approximately 221149.45 0.01)
       (s/length (jts/linestring-wkt [0 0 0 2]))))
  (is ((approximately 221149.45 0.01)
       (s/length (jts/linestring-wkt [0 0 0 1 0 2])))))

(def long-sample [-54.4482421875 23.946096014998382 -53.9208984375 24.467150664739002 -52.27294921875 24.926294766395593 -50.60302734375 24.487148563173424 -50.42724609375 23.704894502324912 -50.20751953125 22.63429269379353 -51.17431640625 22.51255695405145 -51.943359375 22.755920681486405 -51.85546874999999 23.443088931121785 -52.55859375 23.865745352647956 -53.23974609375 23.301901124188877 -53.3935546875 22.51255695405145 -54.07470703125 22.471954507739227 -54.29443359375 23.160563309048314])

(deftest length
  (testing "Resegmenting linestrings at max length"
    (testing "Splitting simple linestring over max length"
      (let [ls (jts/linestring-wkt [0 0 0 2])
            resegmented (s/resegment ls 10000)]
        (is ((approximately 221149.4533708848)
             (s/length ls)))
        (is (= 23 (count resegmented)))
        (is ((approximately 221149 1)
             (reduce + (map s/length resegmented))))))
    (testing "Splitting linestring under max gives single segment"
      (let [ls (jts/linestring-wkt [0 0 0.0001 0.0001 0.0002 0.0002])]
        (is ((approximately 31.38)
             (s/length ls)))
        (is (= 1 (count (s/resegment ls 1000))))
        (is ((approximately 31.38)
             (s/length (first (s/resegment ls 1000)))))))
    (testing "Splitting complex linestring over max length"
      (let [ls (jts/linestring-wkt long-sample)
            rs (s/resegment ls 100000)]
        (is ((approximately 1316265.356651721)
             (s/length ls)))
        (is ((approximately 1316265 5)
             (->> rs (map s/length) (reduce +))))
        (is (= 13 (count (filter
                          true?
                          (->> rs
                               (drop-last 1)
                               ;; last segment just has remaining distance
                               (map s/length)
                               (map (approximately 100000 100)))))))
        (is ((approximately 16265 50)
             (-> rs last s/length)))
        (is (= 14 (-> rs count)))))
    ))

;; ;; overriding core.rand with a custom generator to specify seed
#?(:clj
   (let [gen (java.util.Random. 1)]
     (with-redefs [clojure.core/rand (fn [& args] (if-let [i (first args)]
                                                    (* i (.nextDouble gen))
                                                    (.nextDouble gen)))]
       (deftest random
         (testing "Getting a random point in a radius"
           (let [points (take 20 (repeatedly
                                  (partial s/rand-point-in-radius
                                           0 0 500)))]
             (is (= 20 (count (filter
                               true?
                               (->> points
                                    (map (partial s/distance (s/point 0 0)))
                                    (map #(>= 500 % 0))))))))
           (testing "Accepts custom distribution function"
             (let [distrib (fn [] 1)
                   points (take 20 )]
               (is (= 20 (count (filter
                                 true?
                                 (->> (partial s/rand-point-in-radius
                                               0 0 100 distrib)
                                           (repeatedly)
                                           (take 20)
                                           (map (partial s/distance
                                                         (s/point 0 0)))
                                           (map (approximately
                                                 100.0 1.0)))))))))

             (testing "Uniform vs Clustered distributions"
               (let [c-points (take 1000 (repeatedly
                                          (partial s/rand-point-in-radius
                                                   0 0 100 :clustered)))
                     u-points (take 1000 (repeatedly
                                          (partial s/rand-point-in-radius
                                                   0 0 100 :uniform)))
                     dist (partial s/distance (s/point 0 0))]
                     ;; Clustered distribution places ~50% of points within 50m,
                     ;; i.e. radius midpoint
                 (is ((approximately 500 50)
                      (->> c-points
                           (map dist)
                           (filter (partial > 50))
                           count)))

                     ;; Uniform distribution places ~25% of points within 50m,
                     ;; i.e. radius midpoint
                 (is ((approximately 250 25)
                      (->> u-points
                           (map dist)
                           (filter (partial > 50))
                           count))))))))))
