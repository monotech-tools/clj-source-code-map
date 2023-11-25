
(ns source-code-map.core.prototypes
    (:require [source-code-map.core.utils :as core.utils]
              [vector.api                 :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn options-prototype
  ; @ignore
  ;
  ; @param (map) options
  ;
  ; @return (map)
  ; {:filename-pattern (regex pattern)
  ;  :reader-options (keywords in vector)
  ;  :source-paths (strings in vector)}
  [options]
  (merge {:filename-pattern #"[a-z\_\d]{1,}\.clj[cs]{0,1}"
          :reader-options   [:def :defn :ns]}
         (-> options (update :source-paths #(vector/->items % core.utils/valid-directory-path)))))
