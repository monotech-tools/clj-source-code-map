
(ns source-code-map.map.ns-defs
    (:require [map.api     :refer [assoc-by]]
              [seqable.api :refer [last-dex]]
              [vector.api  :as vector]))

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
  [_ _ {:keys [ending-tag left-sibling-count tag-parent?]}]
  (and (tag-parent? :def)
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
       (assoc-by result [last-dex :name] left-symbol)))

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
       (assoc-by result [last-dex :bounds] [started-at cursor])))

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
  (cond (add-def?       result state metafunctions) (add-def       result state metafunctions)
        (read-def-name? result state metafunctions) (read-def-name result state metafunctions)
        (close-def?     result state metafunctions) (close-def     result state metafunctions)
        :return result))
