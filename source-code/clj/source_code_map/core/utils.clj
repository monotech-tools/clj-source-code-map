
(ns source-code-map.core.utils
    (:require [string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn valid-source-path
  ; @ignore
  ;
  ; @param (string) source-path
  ;
  ; @return (string)
  [source-path]
  (-> source-path (string/not-starts-with! "/")
                  (string/not-ends-with!   "/")))
