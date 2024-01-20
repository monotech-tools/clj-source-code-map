
(ns source-code-map.map.ns-defs
    (:require [fruits.vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-def-name?
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count parent-tag]}]
  (and (-> (parent-tag)         (= :def))
       (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))))

(defn read-def-name
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result _ {:keys [tag-body]}]
  (let [left-symbol (tag-body :symbol)]
       (vector/update-last-item result assoc :name left-symbol)))

(defn read-def-meta?
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count parent-tag]}]
  (and (-> (parent-tag)         (= :def))
       (or (-> (ending-tag)     (= :meta-keyword))
           (-> (ending-tag)     (= :meta-map))
           (-> (ending-tag)     (= :meta-string))
           (-> (ending-tag)     (= :meta-symbol)))
       (-> (left-sibling-count) (= 1))))

(defn read-def-meta
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result {:keys [cursor]} {:keys [ending-tag tag-started-at]}]
  (let [left-type       (ending-tag)
        meta-started-at (tag-started-at left-type)]
       (-> result (vector/update-last-item assoc-in [:meta :type]   left-type)
                  (vector/update-last-item assoc-in [:meta :bounds] [meta-started-at cursor]))))

(defn read-def-value?
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count parent-tag]}]
  (and (-> (parent-tag)         (= :def))
       (-> (left-sibling-count) (> 0))
       (-> (ending-tag))))

(defn read-def-value
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result {:keys [cursor]} {:keys [ending-tag tag-started-at]}]
  (let [left-type        (ending-tag)
        value-started-at (tag-started-at left-type)]
       (-> result (vector/update-last-item assoc-in [:value :type]   left-type)
                  (vector/update-last-item assoc-in [:value :bounds] [value-started-at cursor]))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-def?
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :def)))

(defn add-def
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result _ _]
  (vector/conj-item result {}))

(defn close-def?
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :def)))

(defn close-def
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result {:keys [cursor]} {:keys [tag-started-at]}]
  (let [started-at (tag-started-at :def)]
       (vector/update-last-item result assoc :bounds [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn map-ns-defs
  ; @ignore
  ;
  ; @param (vector) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (vector)
  [result state metafunctions]
  (cond (add-def?        result state metafunctions) (add-def        result state metafunctions)
        (read-def-name?  result state metafunctions) (read-def-name  result state metafunctions)
        (read-def-meta?  result state metafunctions) (read-def-meta  result state metafunctions)
        (read-def-value? result state metafunctions) (read-def-value result state metafunctions)
        (close-def?      result state metafunctions) (close-def      result state metafunctions)
        :return result))
