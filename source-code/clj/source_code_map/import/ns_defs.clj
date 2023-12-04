
(ns source-code-map.import.ns-defs
    (:require [map.api     :refer [assoc-by]]
              [seqable.api :refer [last-dex]]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-def-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count tag-opened?]}]
  (and (tag-opened? :def)
       (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))))

(defn read-def-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result _ {:keys [tag-body]}]
  (let [left-symbol (tag-body :symbol)]
       (assoc-by result [last-dex :name] left-symbol)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-def?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :def)))

(defn add-def
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result _ _]
  (vector/conj-item result {}))

(defn close-def?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :def)))

(defn close-def
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [tag-started-at]}]
  (let [started-at (tag-started-at :def)]
       (assoc-by result [last-dex :bounds] [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn stop-reading?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-met-count]}]
  (= 2 (tag-met-count :ns)))

(defn stop-reading
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [stop]}]
  (stop result))

(defn read-ns-defs
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (cond (add-def?       result state metafunctions) (add-def       result state metafunctions)
        (read-def-name? result state metafunctions) (read-def-name result state metafunctions)
        (close-def?     result state metafunctions) (close-def     result state metafunctions)
        (stop-reading?  result state metafunctions) (stop-reading  result state metafunctions)
        :return result))
