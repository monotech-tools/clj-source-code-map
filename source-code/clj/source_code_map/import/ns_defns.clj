
(ns source-code-map.import.ns-defns
    (:require [map.api     :refer [assoc-by update-by]]
              [seqable.api :refer [last-dex]]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-defn-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count tag-opened?]}]
  (and (tag-opened? :defn)
       (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 1))))

(defn read-defn-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (tag-body :symbol)]
       (assoc-by result [:defn last-dex :name] left-symbol)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-defn?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :defn)))

(defn add-defn
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (update result :defn vector/conj-item {}))

(defn close-defn?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :defn)))

(defn close-defn
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [started-at (tag-started-at :defn)]
       (assoc-by result [:defn last-dex :bounds] [started-at cursor])))

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

(defn read-ns-defns
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (cond (add-defn?       result state metafunctions) (add-defn       result state metafunctions)
        (read-defn-name? result state metafunctions) (read-defn-name result state metafunctions)
        (close-defn?     result state metafunctions) (close-defn     result state metafunctions)
        (stop-reading?   result state metafunctions) (stop-reading   result state metafunctions)
        :return result))
