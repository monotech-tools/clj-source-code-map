
(ns source-code-map.import.ns-declaration
    (:require [source-code-map.import.ns-deps :as import.ns-deps]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth ending-tag left-sibling-count tag-opened?]}]
  (and (tag-opened? :ns)
       (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))
       (-> (depth)              (= 1))))

(defn read-ns-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (assoc result :name left-symbol)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn close-ns?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :ns)))

(defn close-ns
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [started-at (tag-started-at :ns)]
       (assoc result :bounds [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-declaration
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (cond (read-ns-name? result state metafunctions) (read-ns-name                result state metafunctions)
        (close-ns?     result state metafunctions) (close-ns                    result state metafunctions)
        :read-ns-deps                              (import.ns-deps/read-ns-deps result state metafunctions)))
