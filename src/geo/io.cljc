(ns geo.io
  "Helper functions for converting JTS geometries to and from various
   geospatial IO formats (geojson, wkt, wkb)."
  (:require [geo.crs :as crs]
            #?(:clj [geo.jts :as jts]
               :cljs [geo.jsts :as geo.jsts])
            #?(:clj [geo.spatial :refer [Shapelike to-jts]]
               :cljs [geo.spatial :refer [Shapelike to-jsts]])
            [clojure.data]
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            #?(:cljs [jsts]))
  #?(:clj (:import
           (java.util Arrays Arrays$ArrayList)
           (org.locationtech.jts.io WKTReader WKTWriter WKBReader WKBWriter)
           (org.locationtech.jts.geom Geometry GeometryCollection)
           (org.wololo.geojson Feature FeatureCollection GeoJSONFactory)
           (org.wololo.jts2geojson GeoJSONReader GeoJSONWriter))))

#?(:clj
   (defn ^Arrays$ArrayList feature-list
     [features]
     (Arrays/asList (into-array Feature features))))

(defn read-wkt
  "Read a WKT string and convert to a Geometry.
   Can optionally pass in SRID. Defaults to WGS84"
  (#?(:clj [^String wkt]
      :cljs [^js/String wkt])
   (.read (#?(:clj WKTReader.
              :cljs jsts.io.WKTReader.) crs/gf-wgs84) wkt))
  (#?(:clj [^String wkt srid]
      :cljs [^js/String wkt srid])
   (.read (#?(:clj WKTReader.
              :cljs jsts.io.WKTReader.)
           (crs/get-geometry-factory srid)) wkt)))

(defn to-wkt [shapelike]
  (.write (#?(:clj WKTWriter.
              :cljs jsts.io.WKTWriter.))
          (#?(:clj to-jts
              :cljs to-jsts) shapelike)))

#?(:clj
   (defn read-wkb
     "Read a WKB byte array and convert to a Geometry.
   Can optionally pass in SRID. Defaults to WGS84"
     ([^bytes bytes] (.read (WKBReader. crs/gf-wgs84) bytes))
     ([^bytes bytes srid]
      (.read (WKBReader.
              (crs/get-geometry-factory srid)) bytes))))

#?(:clj
   (defn read-wkb-hex
     "Read a WKB hex string and convert to a Geometry"
     ([^String s]
      (read-wkb (WKBReader/hexToBytes s)))
     ([^String s srid]
      (read-wkb (WKBReader/hexToBytes s) srid))))

#?(:clj
   (defn to-wkb
     "Write a WKB, excluding any SRID"
     [shapelike]
     (.write (WKBWriter.) (to-jts shapelike))))

#?(:clj
   (defn to-ewkb [shapelike]
     "Write an EWKB, including the SRID"
     (.write (WKBWriter. 2 true) (to-jts shapelike))))

#?(:clj
   (defn to-wkb-hex
     "Write a WKB as a hex string, excluding any SRID"
     [shapelike]
     (WKBWriter/toHex (to-wkb (to-jts shapelike)))))

#?(:clj
   (defn to-ewkb-hex
     "Write an EWKB as a hex string, including any SRID"
     [shapelike]
     (WKBWriter/toHex (to-ewkb (to-jts shapelike)))))

#?(:clj
   (defn parse-geojson
     "Parse a geojson using GeoJSONFactory's create"
     [^String geojson]
     (GeoJSONFactory/create geojson)))

#?(:clj
   (defn properties [^Feature feature]
     (keywordize-keys (into {} (.getProperties feature)))))

#?(:clj
   (defprotocol GeoJSONGeometry
     (read-geometry [this])))

#?(:clj
   (extend-protocol GeoJSONGeometry
     org.wololo.geojson.Geometry
     (read-geometry [this] (.read (GeoJSONReader.) this))
     Feature
     (read-geometry [this] (read-geometry (.getGeometry this)))))

#?(:clj
   (defprotocol GeoJSONFeatures
     (to-features [this])))

#?(:clj
   (extend-protocol GeoJSONFeatures
     org.wololo.geojson.Geometry
     (to-features [this] [{:properties {} :geometry (read-geometry this)}])
     org.wololo.geojson.GeometryCollection
     (to-features [this] (mapcat to-features (jts/geometries (read-geometry this))))
     Geometry
     (to-features [this] [{:properties {} :geometry this}])
     GeometryCollection
     (to-features [this] (mapcat to-features (jts/geometries this)))
     Feature
     (to-features [this] [{:properties (properties this) :geometry (read-geometry this)}])
     FeatureCollection
     (to-features [this] (mapcat to-features (.getFeatures this)))))

#?(:clj
   (defn read-geojson
     "Parse a GeoJSON string into a sequence of maps representing GeoJSON \"Features\".

  These will contain a :geometry key containing a JTS geometry and a :features key
  containing a (possibly empty) map of features.

  (read-geojson \"{\\\"type\\\":\\\"Polygon\\\",\\\"coordinates\\\":[[[-70.0024,30.0019],[-70.0024,30.0016],[-70.0017,30.0016],[-70.0017,30.0019],[-70.0024,30.0019]]]}\")
  => [{:properties {}, :geometry #object[org.locationtech.jts.geom.Polygon(...)]}]

  (read-geojson \"{\\\"type\\\":\\\"Feature\\\",\\\"geometry\\\":{\\\"type\\\":\\\"Point\\\",\\\"coordinates\\\":[0.0,0.0]},\\\"properties\\\":{\\\"name\\\":\\\"null island\\\"}}\")
  => [{:properties {:name \"null island\"}
       :geometry #object[org.locationtech.jts.geom.Point(...)]}]


  (read-geojson \"{\\\"type\\\":\\\"FeatureCollection\\\",\\\"features\\\":[{\\\"type\\\":\\\"Feature\\\",\\\"geometry\\\":{\\\"type\\\":\\\"Point\\\",\\\"coordinates\\\":[0.0,0.0]},\\\"properties\\\":{\\\"name\\\":\\\"null island\\\"}}]}\")
  => [{:properties {:name \"null island\"},
       :geometry #object[org.locationtech.jts.geom.Point(...)]}]
  "
     ([^String geojson]
      (read-geojson geojson crs/gf-wgs84))
     ([^String geojson srid]
      (->> geojson
           parse-geojson
           to-features
           (map (fn [f] (update f :geometry (fn [g] (jts/set-srid g srid))))))))

   :cljs
   (defn read-geojson
     ([^js/String geojson]
      (read-geojson geojson crs/gf-wgs84))
     ([^js/String geojson srid]
      (.read (jsts.io.GeoJSONReader.) geojson srid))))

#?(:clj
   (defn read-geojson-geometry
     "Parse a GeoJSON string representing a single Geometry into a JTS Geometry."
     ([^String geojson]
      (read-geojson-geometry geojson crs/gf-wgs84))
     ([^String geojson srid]
      (-> geojson
          parse-geojson
          read-geometry
          (jts/set-srid srid)))))


(defn to-geojson
  [shapelike]
  #?(:clj
     (.toString (.write (GeoJSONWriter.) (to-jts shapelike crs/gf-wgs84)))
     :cljs
     (.write (jsts.io.GeoJSONWriter.) (to-jsts shapelike crs/gf-wgs84))))

#?(:clj
   (defn- ^Feature gj-feature
     [{shapelike :geometry properties :properties}]
     (let [gj-geom (.write (GeoJSONWriter.) (to-jts shapelike crs/gf-wgs84))]
       (Feature. gj-geom (stringify-keys properties)))))

#?(:cljs
   (defn to-feature-maps
     [feature-maps]
     (cond ; Feature
           (= (.-type feature-maps) "Feature")
           {:geometry (.-geometry feature-maps)
            :properties (keywordize-keys (js->clj (.-properties feature-maps)))}
           ; FeatureCollection
           (not (nil? (.-features feature-maps)))
           (vec (map to-feature-maps (.-features feature-maps)))
           ; Geometry
           (some (partial = (type feature-maps))
                 [jsts.geom.Point jsts.geom.MultiPoint
                  jsts.geom.LineString jsts.geom.LinearRing
                  jsts.geom.MultiLineString jsts.geom.Polygon
                  jsts.geom.MultiPolygon jsts.geom.GeometryCollection])
           {:geometry feature-maps
            :properties []})))

(defn to-geojson-feature
  [feature-map]
  #?(:clj (.toString (gj-feature feature-map))
     :cljs (js-obj "type" "Feature"
                   "geometry" (to-geojson (:geometry feature-map))
                   "properties" (clj->js (:properties feature-map)))))


(defn to-geojson-feature-collection
  [feature-maps]
  #?(:clj (let [features (feature-list (map gj-feature feature-maps))]
            (.toString (.write (GeoJSONWriter.) features)))
     :cljs (js-obj "type" "FeatureCollection"
                   "features" (to-array (map to-geojson-feature feature-maps)))))
