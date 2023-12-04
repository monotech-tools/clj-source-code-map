
(ns source-code-map.api
    (:require [source-code-map.core.engine :as core.engine]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; source-code-map.core.engine
(def read-ns-deps  core.engine/read-ns-deps)
(def read-ns-defs  core.engine/read-ns-defs)
(def read-ns-defns core.engine/read-ns-defns)
(def read-ns       core.engine/read-ns)
