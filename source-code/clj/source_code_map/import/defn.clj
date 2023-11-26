
(ns source-code-map.import.defn
    (:require [source-code-map.import.utils :as import.utils]
              [vector.api                   :as vector]))

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
  (let [defn-name (tag-body :symbol)]
       (import.utils/update-last-block-data file-data [:defn] assoc defn-name)))

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
  (import.utils/conj-block-data file-data [:defn] {}))

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
  (let [ns-started-at (tag-started-at :defn)]
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
