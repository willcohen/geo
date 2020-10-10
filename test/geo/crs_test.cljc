(ns geo.crs-test
  (:require [geo.crs :as sut]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as jts])
            [clojure.test :refer [deftest testing is]]))

(deftest identifiers
  (testing "Identifying and converting EPSG and SRID"
    (is (sut/epsg-str? "EPSG:2000"))
    (is (not (sut/epsg-str? "pizza")))
    (is (not (sut/epsg-str? "EPSG:2000abc")))
    (is (not (sut/epsg-str? " EPSG:2000")))
    (is (not (sut/epsg-str? "pizza EPSG:2000")))
    (is (not (sut/epsg-str? "pizza EPSG:20000")))
    (is (= "EPSG:2000" (sut/srid->epsg-str 2000)))
    (is (->> sut/valid-crs-prefixes
             (map #(str % ":2000"))
             (every? sut/crs-name?)))
    (is (sut/proj4-str? "+proj=merc-+lat_ts=565-+ellps=GRS80"))
    (is (not (sut/proj4-str? "pizza")))
    (is (= 4326 (sut/epsg-str->srid "EPSG:4326")))
    (is (thrown? #?(:clj java.lang.AssertionError
                    :cljs js/Error)
                 (sut/epsg-str->srid "pizza")))))

(deftest transforms
  (testing "Creating transform from SRID ints"
    (let [transform (sut/create-transform 4326 2000)
          src (sut/get-source-crs transform)
          target (sut/get-target-crs transform)]
      (is (= "EPSG:4326" (sut/get-name src)))
      (is (= 4326 (sut/get-srid (sut/get-name src))))
      (is (= "EPSG:2000" (sut/get-name target)))
      (is (= 2000 (sut/get-srid (sut/get-name target))))))
  (testing "Creating transform from CRS strings"
    (let [transform (sut/create-transform "ESRI:37211" "ESRI:37220")
          src (sut/get-source-crs transform)
          target (sut/get-target-crs transform)]
      (is (= "ESRI:37211" (sut/get-name src)))
      (is (zero? (sut/get-srid (sut/get-name src))))
      (is (= "ESRI:37220" (sut/get-name target)))
      (is (zero? (sut/get-srid (sut/get-name target))))))

(testing "Creating transform from proj4 parameter strings"
  (let [transform (sut/create-transform
                    "+proj=longlat +a=6378270 +b=6356794.343434343 +no_defs"
                    "+proj=longlat +a=6376896 +b=6355834.846687363 +no_defs")
        src (sut/get-source-crs transform)
        target (sut/get-target-crs transform)]
    (is
      (=
        ["+proj=longlat"
         "+a=6378270"
         "+b=6356794.343434343"
         "+no_defs"]
        (into [] (sut/get-parameters src))))
    (is
      (=
        ["+proj=longlat"
         "+a=6376896"
         "+b=6355834.846687363"
         "+no_defs"]
        (into [] (sut/get-parameters target))))
    (is
      (=
        "+proj=longlat +a=6378270 +b=6356794.343434343 +no_defs"
        (sut/get-parameter-string src)))
    (is
      (=
        "+proj=longlat +a=6376896 +b=6355834.846687363 +no_defs"
        (sut/get-parameter-string target)))
    (is (= "" (sut/get-name src)))
    (is (= "" (sut/get-name target))))))

(deftest geometry-factories
  (testing "Geometry factories"
    (let [transform (sut/create-transform 4326 2000)
          src (sut/get-source-crs transform)
          target (sut/get-target-crs transform)
          gf1 (sut/get-geometry-factory src)
          gf2 (sut/get-geometry-factory target)
          g1 (jts/point 0 0 gf1)
          g2 (jts/point 0 0 gf2)]
      (is (= 4326 (sut/get-srid gf1)))
      (is (= 2000 (sut/get-srid gf2)))
      (is (= 4326 (sut/get-srid g1)))
      (is (= 2000 (sut/get-srid g2)))
      (is (= 4326 (sut/get-srid (sut/get-geometry-factory g1))))
      (is (= 2000 (sut/get-srid (sut/get-geometry-factory g2)))))))
