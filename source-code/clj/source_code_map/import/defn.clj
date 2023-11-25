
(ns source-code-map.import.defn
    (:require [vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn get-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data _ _]
  (-> file-data :defn last))

(defn conj-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) initial
  ;
  ; @return (map)
  [file-data _ _ initial]
  (update file-data :defn vector/conj-item initial))

(defn update-defn
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
  (apply update file-data :defn vector/update-last-item f params))

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
  [_ _ {:keys [left-sibling-count tag-ends?]}]
  (and (-> :symbol tag-ends?)
       (= 1 (left-sibling-count))))

(defn read-defn-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [defn-name (-> :symbol tag-body)]
       (update-defn file-data state metafunctions assoc :name defn-name)))

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
  [_ _ {:keys [tag-starts?]}]
  (-> :defn tag-starts?))

(defn add-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (conj-defn file-data state metafunctions {}))

(defn read-defn?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (-> :defn tag-opened?))

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
  [_ _ {:keys [tag-ends?]}]
  (-> :defn tag-ends?))

(defn close-defn
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [ns-started-at (-> :defn tag-started-at)]
       (update-defn file-data state metafunctions merge {:ended-at cursor :started-at ns-started-at})))

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
