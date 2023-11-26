
(ns source-code-map.import.def
    (:require [source-code-map.import.utils :as import.utils]
              [vector.api                   :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-def-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag left-sibling-count]}]
  (and (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))))

(defn read-def-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [def-name (tag-body :symbol)]
       (import.utils/update-last-block-data file-data [:def] assoc :name def-name)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-def?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :def)))

(defn add-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (import.utils/conj-block-data file-data [:def] {}))

(defn read-def?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (tag-opened? :def))

(defn read-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-def-name? file-data state metafunctions) (read-def-name file-data state metafunctions)
        :return file-data))

(defn close-def?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :def)))

(defn close-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [ns-started-at (tag-started-at :def)]
       (update-def file-data state metafunctions merge {:ended-at cursor :started-at ns-started-at})))

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
  (cond (add-def?   file-data state metafunctions) (add-def   file-data state metafunctions)
        (read-def?  file-data state metafunctions) (read-def  file-data state metafunctions)
        (close-def? file-data state metafunctions) (close-def file-data state metafunctions)
        :return file-data))
