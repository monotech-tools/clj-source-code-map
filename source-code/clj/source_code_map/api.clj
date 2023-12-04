
(ns source-code-map.api
    (:require [source-code-map.core.engine :as core.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; source-code-map.core.engine
(def ns-declaration-map      core.engine/ns-declaration-map)
(def read-ns-declaration-map core.engine/read-ns-declaration-map)
(def ns-defs-map             core.engine/ns-defs-map)
(def read-ns-defs-map        core.engine/read-ns-defs-map)
(def ns-defns-map            core.engine/ns-defns-map)
(def read-ns-defns-map       core.engine/read-ns-defns-map)
(def ns-map                  core.engine/ns-map)
(def read-ns-map             core.engine/read-ns-map)
