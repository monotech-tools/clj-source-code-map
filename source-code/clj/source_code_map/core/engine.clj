
(ns source-code-map.core.engine
    (:require [io.api                          :as io]
              [source-code-map.core.config     :as core.config]
              [source-code-map.import.ns-declaration :as import.ns-declaration]
              [source-code-map.import.ns-defns :as import.ns-defns]
              [source-code-map.import.ns-defs  :as import.ns-defs]
              [source-code-map.import.ns-deps  :as import.ns-deps]
              [syntax-interpreter.api               :as syntax-interpreter]
              [vector.api                      :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-declaration
  ; @description
  ; Reads the (first) namespace declaration in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  ; {:bounds (integers in vector)
  ;  :import (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}
  ;  :name (string)
  ;  :require (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}
  ;  :use (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}}
  [filepath]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :keyword :list :map :regex :string :symbol :vector :ns :import :require :use])]
       (if-let [file-content (io/read-file filepath {:warn? true})]
               (letfn [(f0 [result state {:keys [stop tag-left-count] :as metafunctions}]
                           (if (-> :ns tag-left-count (= 1))
                               (-> result (stop)) ; <- Stops when the first ns declaration is over
                               (-> result (import.ns-declaration/read-ns-declaration state metafunctions))))]
                      (syntax-interpreter/interpreter file-content f0 {} tag-patterns)))))

(defn read-ns-deps
  ; @description
  ; Reads the dependencies of the (first) namespace declaration in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  ; {:import (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}
  ;  :require (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}
  ;  :use (map)
  ;   {:bounds (integers in vector)
  ;    :deps (maps in vector)}}
  [filepath]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :keyword :list :map :regex :string :symbol :vector :ns :import :require :use])]
       (if-let [file-content (io/read-file filepath {:warn? true})]
               (letfn [(f0 [result state {:keys [stop tag-left-count] :as metafunctions}]
                           (if (-> :ns tag-left-count (= 1))
                               (-> result (stop)) ; <- Stops when the first ns declaration is over
                               (-> result (import.ns-deps/read-ns-deps state metafunctions))))]
                      (syntax-interpreter/interpreter file-content f0 {} tag-patterns)))))

(defn read-ns-defs
  ; @description
  ; Reads the def macros of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (vector)
  [filepath]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :list :regex :string :symbol :ns :def])]
       (if-let [file-content (io/read-file filepath {:warn? true})]
               (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                           (if (-> :ns tag-met-count (= 2))
                               (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                               (-> result (import.ns-defs/read-ns-defs state metafunctions))))]
                      (syntax-interpreter/interpreter file-content f0 [] tag-patterns)))))

(defn read-ns-defns
  ; @description
  ; Reads the defn macros of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (vector)
  [filepath]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :list :regex :string :symbol :ns :defn])]
       (if-let [file-content (io/read-file filepath {:warn? true})]
               (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                           (if (-> :ns tag-met-count (= 2))
                               (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                               (-> result (import.ns-defns/read-ns-defns state metafunctions))))]
                      (syntax-interpreter/interpreter file-content f0 [] tag-patterns)))))

(defn read-ns
  ; @description
  ; Reads the declaration with dependencies, def macros and defn macros of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  ; {:defs (maps in vector)
  ;  :defns (maps in vector)
  ;  :ns (map)
  ;   {:bounds (integers in vector)
  ;    :import (map)
  ;     {:bounds (integers in vector)
  ;      :deps (maps in vector)}
  ;    :name (string)
  ;    :require (map)
  ;     {:bounds (integers in vector)
  ;      :deps (maps in vector)}
  ;    :use (map)
  ;     {:bounds (integers in vector)
  ;      :deps (maps in vector)}}}
  [filepath]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :keyword :list :map :regex :string :symbol :vector :ns :import :require :use :def :defn])]
       (if-let [file-content (io/read-file filepath {:warn? true})]
               (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                           (if (-> :ns tag-met-count (= 2))
                               (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                               (-> result (update :ns    import.ns-declaration/read-ns-declaration state metafunctions)
                                          (update :defs  import.ns-defs/read-ns-defs               state metafunctions)
                                          (update :defns import.ns-defns/read-ns-defns             state metafunctions))))]
                      (syntax-interpreter/interpreter file-content f0 {} tag-patterns)))))
