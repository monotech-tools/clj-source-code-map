
(ns source-code-map.import.defn
    (:require [map.api     :refer [assoc-by update-by]]
              [seqable.api :refer [last-dex]]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-defn-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count]}]
  (and (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 1))))

(defn read-defn-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (tag-body :symbol)]
       (assoc-by file-data [:defn last-dex :name] left-symbol)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-defn?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :defn)))

(defn add-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (update file-data :defn vector/conj-item {}))

(defn read-defn?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (tag-opened? :defn))

(defn read-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-defn-name? file-data state metafunctions) (read-defn-name file-data state metafunctions)
        :return file-data))

(defn close-defn?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :defn)))

(defn close-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [started-at (tag-started-at :defn)]
       (assoc-by file-data [:defn last-dex :bounds] [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-file
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (add-defn?   file-data state metafunctions) (add-defn   file-data state metafunctions)
        (read-defn?  file-data state metafunctions) (read-defn  file-data state metafunctions)
        (close-defn? file-data state metafunctions) (close-defn file-data state metafunctions)
        :return file-data))
