
(ns source-code-map.import.def
    (:require [vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn get-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data _ _]
  (-> file-data :def last))

(defn conj-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) initial
  ;
  ; @return (map)
  [file-data _ _ initial]
  (update file-data :def vector/conj-item initial))

(defn update-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (function) f
  ; @param (list of *) params
  ;
  ; @return (map)
  [file-data state metafunctions f & params]
  (apply update file-data :def vector/update-last-item f params))

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
  [_ _ {:keys [left-sibling-count tag-ends?]}]
  (and (-> :symbol tag-ends?)
       (= 1 (left-sibling-count))))

(defn read-def-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [def-name (-> :symbol tag-body)]
       (update-def file-data state metafunctions assoc :name def-name)))

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
  [_ _ {:keys [tag-starts?]}]
  (-> :def tag-starts?))

(defn add-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (conj-def file-data state metafunctions {}))

(defn read-def?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (-> :def tag-opened?))

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
  [_ _ {:keys [tag-ends?]}]
  (-> :def tag-ends?))

(defn close-def
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [ns-started-at (-> :def tag-started-at)]
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
