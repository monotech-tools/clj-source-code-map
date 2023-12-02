
(ns source-code-map.core.engine
    (:require [io.api                          :as io]
              [source-code-map.core.config     :as core.config]
              [source-code-map.core.prototypes :as core.prototypes]
              [source-code-map.import.def      :as import.def]
              [source-code-map.import.defn     :as import.defn]
              [source-code-map.import.ns       :as import.ns]
              [syntax-reader.api               :as syntax-reader]
              [vector.api                      :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn import-source-file
  ; @ignore
  ;
  ; @param (string) filepath
  ; @param (map) options
  ; {:reader-options (keywords in vector)}
  ;
  ; @return (map)
  [filepath {:keys [reader-options]}]
  (letfn [; ...
          (f0 [result state metafunctions]
              (cond-> result (vector/contains-item? reader-options :def)  (import.def/read-file  state metafunctions)
                             (vector/contains-item? reader-options :defn) (import.defn/read-file state metafunctions)
                             (vector/contains-item? reader-options :ns)   (import.ns/read-file   state metafunctions)))]
         ; ...
         (-> filepath (io/read-file {:warn? true})
                      (syntax-reader/interpreter (fn [a b c] (println (:cursor b)) (f0 a b c)) {} core.config/PATTERNS))))

(defn read-source-file
  ; @param (string) filepath
  ; @param (map)(opt) options
  ; {:reader-options (keywords in vector)(opt)
  ;   [:def, :defn, :ns]
  ;   Default: [:def :defn :ns]}
  ;
  ; @usage
  ; (read-source-file "source-code/clj/my_namespace.clj")
  ;
  ; @usage
  ; (read-source-file "source-code/clj/my_namespace.clj")
  ; =>
  ; {:def  [{...}]
  ;  :defn [{...}]
  ;  :ns   [{...}]}
  ;
  ; @return (map)
  ; {:def (maps in vector)
  ;  :def (maps in vector)
  ;  :ns (maps in vector)}
  ([filepath]
   (read-source-file filepath {}))

  ([filepath options]
   (let [filepath (core.prototypes/filepath-prototype filepath)
         options  (core.prototypes/options-prototype  options)]
        (try (import-source-file filepath options)
             (catch Exception e (println e))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn import-source-files
  ; @ignore
  ;
  ; @param (strings in vector) source-paths
  ; @param (map) options
  ; {:filename-pattern (regex pattern)}
  ;
  ; @return (map)
  [source-paths {:keys [filename-pattern] :as options}]
  (-> source-paths (vector/->items    (fn [source-path] (io/search-files source-path filename-pattern)))
                   (vector/flat-items (fn [filepath]    [filepath (import-source-file filepath options)]))))

(defn read-source-files
  ; @param (strings in vector) source-paths
  ; @param (map)(opt) options
  ; {:filename-pattern (regex pattern)(opt)
  ;   Default: #"[a-z\_\d]{1,}\.clj[cs]{0,1}"
  ;  :reader-options (keywords in vector)(opt)
  ;   [:def, :defn, :ns]
  ;   Default: [:def :defn :ns]}
  ;
  ; @usage
  ; (read-source-files ["source-code"])
  ;
  ; @usage
  ; (read-source-files ["source-code"])
  ; =>
  ; [["source-code/clj/my_namespace.clj"
  ;   {:def  [{...}]
  ;    :defn [{...}]
  ;    :ns   [{...}]}]
  ;  [...]]
  ;
  ; @return (vectors in vector)
  ; [[(string) filepath
  ;   (map) source-code-map
  ;    {:def (maps in vector)
  ;     :def (maps in vector)
  ;     :ns (maps in vector)}]]
  ([source-paths]
   (read-source-files source-paths {}))

  ([source-paths options]
   (let [source-paths (core.prototypes/source-paths-prototype source-paths)
         options      (core.prototypes/options-prototype      options)]
        (try (import-source-files source-paths options)
             (catch Exception e (println e))))))
