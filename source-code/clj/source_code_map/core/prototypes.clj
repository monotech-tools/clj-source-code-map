
(ns source-code-map.core.prototypes
    (:require [source-code-map.core.utils :as core.utils]
              [vector.api                 :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filepath-prototype
  ; @ignore
  ;
  ; @param (strings in vector) filepath
  ;
  ; @return (strings in vector)
  [filepath]
  (-> filepath core.utils/valid-source-path))

(defn source-paths-prototype
  ; @ignore
  ;
  ; @param (strings in vector) source-paths
  ;
  ; @return (strings in vector)
  [source-paths]
  (vector/->items source-paths core.utils/valid-source-path))

(defn options-prototype
  ; @ignore
  ;
  ; @param (map) options
  ;
  ; @return (map)
  ; {:filename-pattern (regex pattern)
  ;  :reader-options (keywords in vector)}
  [options]
  (merge {:filename-pattern #"[a-z\_\d]{1,}\.clj[cs]{0,1}"
          :reader-options   [:def :defn :ns]}
         (-> options)))
