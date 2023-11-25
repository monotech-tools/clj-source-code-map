
(ns source-code-map.core.env
    (:require [io.api                          :as io]
              [map.api                         :as map]
              [source-code-map.core.config     :as core.config]
              [source-code-map.core.prototypes :as core.prototypes]
              [source-code-map.import.def      :as import.def]
              [source-code-map.import.defn     :as import.defn]
              [source-code-map.import.ns       :as import.ns]
              [syntax-reader.api               :as syntax-reader]
              [vector.api                      :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn import-files
  ; @ignore
  ;
  ; @param (map) options
  ; {:filename-pattern (regex pattern)
  ;  :reader-options (keywords in vector)
  ;  :source-paths (strings in vector)}
  ;
  ; @return (map)
  [{:keys [filename-pattern reader-options source-paths]}]
  (letfn [; ...
          (f0 [result state metafunctions]
              (cond-> result (vector/contains-item? reader-options :def)  (import.def/read-file  state metafunctions)
                             (vector/contains-item? reader-options :defn) (import.defn/read-file state metafunctions)
                             (vector/contains-item? reader-options :ns)   (import.ns/read-file   state metafunctions)))]

         ; 1. Iterates over the 'source-paths' vector and replaces each item with a vector of files that are match with the given ':filename-pattern' option.
         ; 2. Flattens the result vector while replacing each item with a new vector that contains a filepath and its file content.
         ; 3. Replaces the file content in each vector with its imported source code map.
         ;
         ; 0. ["source-code"]
         ; 1. [["source-code/namespace1.clj" "source-code/namespace2.clj"]]
         ; 2. [["source-code/namespace1.clj" "..."] ["source-code/namespace2.clj" "..."]]
         ; 3. [["source-code/namespace1.clj" {...}] ["source-code/namespace2.clj" {...}]]
         (-> source-paths (vector/->items    (fn [source-path]                       (io/search-files source-path filename-pattern)))
                          (vector/flat-items (fn [filepath]                [filepath (io/read-file filepath {:warn? true})]))
                          (vector/->items    (fn [[filepath file-content]] [filepath (syntax-reader/interpreter file-content f0 {} core.config/PATTERNS)])))))

(defn read-source-code
  ; @param (map) options
  ; {:filename-pattern (regex pattern)(opt)
  ;   Default: #"[a-z\_\d]{1,}\.clj[cs]{0,1}"
  ;  :reader-options (keywords in vector)(opt)
  ;   [:def, :defn, :ns]
  ;   Default: [:def :defn :ns]
  ;  :source-paths (strings in vector)}
  ;
  ; @usage
  ; (read-source-code {...})
  ;
  ; @usage
  ; (read-source-code {:source-paths ["source-code"]})
  ; =>
  ; [["source-code/clj/my_namespace.clj" {...}]]
  ;
  ; @return (vectors in vector)
  ; [[(string) filepath
  ;   (map) source-code-map]]
  [options]
  (let [options (core.prototypes/options-prototype options)]
       (try (import-files options)
            (catch Exception e (println e)))))
