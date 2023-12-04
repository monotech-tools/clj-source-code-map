
(ns source-code-map.core.engine
    (:require [io.api                          :as io]
              [source-code-map.core.config     :as core.config]
              [source-code-map.import.ns-defns :as import.ns-defns]
              [source-code-map.import.ns-defs  :as import.ns-defs]
              [source-code-map.import.ns-deps  :as import.ns-deps]
              [syntax-interpreter.api               :as syntax-interpreter]
              [vector.api                      :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-deps
  ; @description
  ; Reads the dependencies of the first namespace declaration in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (syntax-interpreter/interpreter file-content import.ns-deps/read-ns-deps {} core.config/NS-DEPS-PATTERNS)))

(defn read-ns-defs
  ; @description
  ; Reads the def macros of the first namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (syntax-interpreter/interpreter file-content import.ns-defs/read-ns-defs {} core.config/NS-DEFS-PATTERNS)))

(defn read-ns-defns
  ; @description
  ; Reads the defn macros of the first namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (syntax-interpreter/interpreter file-content import.ns-defns/read-ns-defns {} core.config/NS-DEFNS-PATTERNS)))

(defn read-ns
  ; @description
  ; Reads the dependencies, def macros and defn macros of the first namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  ; {:ns (map)
  ;   {:bounds (integers in vector)
  ;     [(integer) started-at
  ;      (integer) ended-at]
  ;    :import (map)
  ;     {:bounds (integers in vector)
  ;       [(integer) started-at
  ;        (integer) ended-at]
  ;      :deps (maps in vector)
  ;       [{:alias (string)
  ;         :bounds (integers in vector)
  ;          [(integer) started-at
  ;           (integer) ended-at]
  ;         :name (string)
  ;         :only (strings in vector)
  ;         :refer (keyword or strings in vector)
  ;         :rename (map)}]}
  ;    :name (string)
  ;    :require (map)
  ;    :use (map)}}
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (letfn [(f0 [result state metafunctions]
                      (-> result (update :ns-defs  (import.ns-defs/read-ns-defs   result state metafunctions))
                                 (update :ns-defns (import.ns-defns/read-ns-defns result state metafunctions))
                                 (update :ns-deps  (import.ns-deps/read-ns-deps   result state metafunctions))))]
                 (syntax-interpreter/interpreter file-content f0 {} (merge core.config/NS-DEFS-PATTERNS
                                                                           core.config/NS-DEFNS-PATTERNS
                                                                           core.config/NS-DEPS-PATTERNS)))))
