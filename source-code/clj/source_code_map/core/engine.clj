
(ns source-code-map.core.engine
    (:require [io.api                             :as io]
              [source-code-map.core.config        :as core.config]
              [source-code-map.map.ns-declaration :as map.ns-declaration]
              [source-code-map.map.ns-defns       :as map.ns-defns]
              [source-code-map.map.ns-defs        :as map.ns-defs]
              [syntax-interpreter.api             :as syntax-interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-declaration-map
  ; @description
  ; Returns the namespace declaration map of the (first) namespace in the given 'file-content'.
  ;
  ; @param (string) file-content
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
  [file-content]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :keyword :list :map :regex :string :symbol :vector :ns :import :require :use])]
       (letfn [(f0 [result state {:keys [stop tag-left-count] :as metafunctions}]
                   (if (-> :ns tag-left-count (= 1))
                       (-> result (stop)) ; <- Stops when the first ns declaration is over
                       (-> result (map.ns-declaration/map-ns-declaration state metafunctions))))]
              (syntax-interpreter/interpreter file-content f0 {} tag-patterns))))

(defn read-ns-declaration-map
  ; @description
  ; Returns the namespace declaration map of the (first) namespace in the file found on the given filepath.
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
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (ns-declaration-map file-content)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-defs-map
  ; @description
  ; Returns the def macros map of the (first) namespace in the given 'file-content'.
  ;
  ; @param (string) file-content
  ;
  ; @return (vector)
  [file-content]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :list :regex :string :symbol :ns :def])]
       (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                   (if (-> :ns tag-met-count (= 2))
                       (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                       (-> result (map.ns-defs/map-ns-defs state metafunctions))))]
              (syntax-interpreter/interpreter file-content f0 [] tag-patterns))))

(defn read-ns-defs-map
  ; @description
  ; Returns the def macros map of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (vector)
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (ns-defs-map file-content)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-defns-map
  ; @description
  ; Returns the defn macros map of the (first) namespace in the given 'file-content'.
  ;
  ; @param (string) file-content
  ;
  ; @return (vector)
  [file-content]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :list :regex :string :symbol :ns :defn])]
       (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                   (if (-> :ns tag-met-count (= 2))
                       (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                       (-> result (map.ns-defns/map-ns-defns state metafunctions))))]
              (syntax-interpreter/interpreter file-content f0 [] tag-patterns))))

(defn read-ns-defns-map
  ; @description
  ; Returns the defn macros map of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (vector)
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (ns-defns-map file-content)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-map
  ; @description
  ; Returns the namespace declaration, def macros, and defn macros map of the (first) namespace in the given 'file-content'.
  ;
  ; @param (string) file-content
  ;
  ; @return (map)
  ; {:declaration (map)
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
  ;      :deps (maps in vector)}}
  ;  :defs (maps in vector)
  ;  :defns (maps in vector)}
  [file-content]
  (let [tag-patterns (select-keys core.config/TAG-PATTERNS [:comment :keyword :list :map :regex :string :symbol :vector :ns :import :require :use :def :defn])]
       (letfn [(f0 [result state {:keys [stop tag-met-count] :as metafunctions}]
                   (if (-> :ns tag-met-count (= 2))
                       (-> result (stop)) ; <- Stops when / if it reaches a second ns declaration
                       (-> result (update :declaration map.ns-declaration/map-ns-declaration state metafunctions)
                                  (update :defs        map.ns-defs/map-ns-defs               state metafunctions)
                                  (update :defns       map.ns-defns/map-ns-defns             state metafunctions))))]
              (syntax-interpreter/interpreter file-content f0 {} tag-patterns))))

(defn read-ns-map
  ; @description
  ; Returns the namespace declaration, def macros, and defn macros map of the (first) namespace in the file found on the given filepath.
  ;
  ; @param (string) filepath
  ;
  ; @return (map)
  ; {:declaration (map)
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
  ;      :deps (maps in vector)}}
  ;  :defs (maps in vector)
  ;  :defns (maps in vector)}
  [filepath]
  (if-let [file-content (io/read-file filepath {:warn? true})]
          (ns-map file-content)))
