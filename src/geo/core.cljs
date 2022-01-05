;(ns geo.core)

(ns geo.core
 (:require [geo.crs :as crs]
           [jsts]
           [proj4]))


(set! *warn-on-infer* true)

;;; All work!!!
;; (.log js/console crs/pjs)

;; (.log js/console (. (.-default ^js/proj4 proj4) -WGS84))

(.log js/console (.Proj (.-default ^js/proj4 proj4) "WGS84"))

(.log js/console (crs/proj "WGS84"))

(.log js/console (crs/create-transform 4326 3586))

(defn ^js/jsts.geom.Point point
  [^js/jsts.geom.Coordinate c ^js/jsts.geom.GeometryFactory gf]
  (.createPoint gf c))

(.log js/console (point (jsts.geom.Coordinate. 0 0 0) (crs/get-geometry-factory 4326)))

(.log js/console
      (crs/transform-geom
       (point (jsts.geom.Coordinate. 0 0) (crs/get-geometry-factory 4326))
       (crs/create-transform 4326 3586)))

;;
;;

;(.log js/console ^js/proj4.Proj.Projection (.WGS84 crs/pjs "WGS84" nil))

;; Works!!!
;(.log js/console (.-default ^js/proj4 proj4))

;(.log js/console ^js/proj4.Proj (.Proj. ^js/proj4 (.-default proj4) "WGS84"))


;;; Ehh
;; (.log js/console
;;       (.parse js/JSON
;;               (.stringify js/JSON
;;                           (crs/transform-geom ^js/jsts.geom.Point
;;                                               (.createPoint
;;                                                ^js/jsts.geom.GeometryFactory (crs/get-geometry-factory 4326)
;;                                                ^js/jsts.geom.Coordinate
;;                                                (jsts.geom.Coordinate. 0 0))
;;                                               4326 3586))))


;(.createPoint (crs/get-geometry-factory 4326) (jsts.geom.Coordinate. (array 0 0 0)))

;(jsts.geom.Coordinate. 0 0 0)
