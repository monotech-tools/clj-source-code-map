
(ns source-code-map.import.ns
    (:require [reader.api :as reader]
              [string.api :as string]
              [vector.api :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-deps-directive
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (namespaced keyword)
  [_ _ {:keys [tag-opened?]}]
  (vector/first-match [:ns/import :ns/require :ns/use] tag-opened?))

(defn get-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data _ _]
  (-> file-data :ns last))

(defn conj-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) initial
  ;
  ; @return (map)
  [file-data _ _ initial]
  (update file-data :ns vector/conj-item initial))

(defn update-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (function) f
  ; @param (list of *) params
  ;
  ; @return (map)
  [file-data state metafunctions f & params]
  (apply update file-data :ns vector/update-last-item f params))

(defn get-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (-> file-data :ns last deps-directive last)))

(defn conj-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) initial
  ;
  ; @return (map)
  [file-data state metafunctions initial]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (update-ns file-data state metafunctions update deps-directive vector/conj-item initial)))

(defn update-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (function) f
  ; @param (list of *) params
  ;
  ; @return (map)
  [file-data state metafunctions f & params]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (apply update-ns file-data state metafunctions update deps-directive vector/update-last-item f params)))

(defn get-ns-prefixed-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (-> file-data :ns last deps-directive last :prefixed last)))

(defn conj-ns-prefixed-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (map) initial
  ;
  ; @return (map)
  [file-data state metafunctions initial]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (update-ns file-data state metafunctions update deps-directive vector/update-last-item update :prefixed vector/conj-item initial)))

(defn update-ns-prefixed-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ; @param (function) f
  ; @param (list of *) params
  ;
  ; @return (map)
  [file-data state metafunctions f & params]
  (apply update-ns-libspec file-data state metafunctions update :prefixed vector/update-last-item f params))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-ends?]}]
  (-> :keyword tag-ends?))

(defn read-ns-directive-operator
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-keyword (-> :keyword tag-body reader/read-edn)]
       (update-ns file-data state metafunctions assoc :left-operator left-keyword)))

(defn disarm-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-ends?]}]
  (or (-> :map    tag-ends?)
      (-> :vector tag-ends?)))

(defn disarm-ns-directive-operator
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (update-ns file-data state metafunctions dissoc :left-operator))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [left-sibling-count tag-ends?]}]
  (and (-> :symbol tag-ends?)
       (= 0 (left-sibling-count))))

(defn read-ns-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns file-data state metafunctions assoc :name left-symbol)))

(defn read-ns-body
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-ns-name? file-data state metafunctions) (read-ns-name file-data state metafunctions)
        :return file-data))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-libspec-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth left-sibling-count tag-ancestor? tag-ends?]}]
  (and (or (-> :string tag-ends?)
           (-> :symbol tag-ends?))
       (= 3 (depth))
       (= 0 (left-sibling-count))))

(defn read-ns-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (or (-> :string tag-body)
                        (-> :symbol tag-body))]
       (update-ns-libspec file-data state metafunctions assoc :name left-symbol)))

(defn read-ns-libspec-alias?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 3 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :as))))

(defn read-ns-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-libspec file-data state metafunctions assoc :alias left-symbol)))

(defn read-ns-libspec-only?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 4 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :only))))

(defn read-ns-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-libspec file-data state metafunctions update :only vector/conj-item left-symbol)))

(defn read-ns-libspec-refer?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 4 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :refer))))

(defn read-ns-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-libspec file-data state metafunctions update :refer vector/conj-item left-symbol)))

(defn read-ns-libspec-rename?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 4 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :rename))))

(defn read-ns-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (if (-> (left-sibling-count) even?)
           (update-ns-libspec file-data state metafunctions update :rename vector/conj-item [left-symbol])
           (update-ns-libspec file-data state metafunctions update :rename vector/update-last-item vector/conj-item left-symbol))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-prefixed-raw-libspec-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 3 (depth))
       (-> (get-ns-libspec file-data state metafunctions) :name string/nonempty?)
       (-> (get-ns         file-data state metafunctions) :left-operator not)))

(defn read-ns-prefixed-raw-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (conj-ns-prefixed-libspec file-data state metafunctions {:name left-symbol})))

(defn read-ns-prefixed-libspec-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 4 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator not)))

(defn read-ns-prefixed-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (conj-ns-prefixed-libspec file-data state metafunctions {:name left-symbol})))

(defn read-ns-prefixed-libspec-alias?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 4 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :as))))

(defn read-ns-prefixed-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-prefixed-libspec file-data state metafunctions assoc :alias left-symbol)))

(defn read-ns-prefixed-libspec-only?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 5 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :only))))

(defn read-ns-prefixed-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-prefixed-libspec file-data state metafunctions update :only vector/conj-item left-symbol)))

(defn read-ns-prefixed-libspec-refer?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 5 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :refer))))

(defn read-ns-prefixed-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (update-ns-prefixed-libspec file-data state metafunctions update :refer vector/conj-item left-symbol)))

(defn read-ns-prefixed-libspec-rename?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth tag-ends?] :as metafunctions}]
  (and (-> :symbol tag-ends?)
       (= 5 (depth))
       (-> (get-ns file-data state metafunctions) :left-operator (= :rename))))

(defn read-ns-prefixed-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (if (-> (left-sibling-count) even?)
           (update-ns-prefixed-libspec file-data state metafunctions update :rename vector/conj-item [left-symbol])
           (update-ns-prefixed-libspec file-data state metafunctions update :rename vector/update-last-item vector/conj-item left-symbol))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-ns-libspec?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth tag-starts?]}]
  (and (or (-> :list   tag-starts?)
           (-> :vector tag-starts?))
       (= 2 (depth))))

(defn add-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-ancestor?] :as metafunctions}]
  (let [deps-directive (vector/first-match [:ns/import :ns/require :ns/use] tag-ancestor?)]
       (conj-ns-libspec file-data state metafunctions {})))

(defn read-ns-libspec?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (or (-> :list   tag-opened?)
      (-> :vector tag-opened?)))

(defn read-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-ns-libspec-name?              file-data state metafunctions) (read-ns-libspec-name              file-data state metafunctions)
        (read-ns-prefixed-raw-libspec-name? file-data state metafunctions) (read-ns-prefixed-raw-libspec-name file-data state metafunctions)
        (read-ns-prefixed-libspec-name?     file-data state metafunctions) (read-ns-prefixed-libspec-name     file-data state metafunctions)
        (read-ns-libspec-alias?             file-data state metafunctions) (read-ns-libspec-alias             file-data state metafunctions)
        (read-ns-prefixed-libspec-alias?    file-data state metafunctions) (read-ns-prefixed-libspec-alias    file-data state metafunctions)
        (read-ns-libspec-only?              file-data state metafunctions) (read-ns-libspec-only              file-data state metafunctions)
        (read-ns-prefixed-libspec-only?     file-data state metafunctions) (read-ns-prefixed-libspec-only     file-data state metafunctions)
        (read-ns-libspec-refer?             file-data state metafunctions) (read-ns-libspec-refer             file-data state metafunctions)
        (read-ns-prefixed-libspec-refer?    file-data state metafunctions) (read-ns-prefixed-libspec-refer    file-data state metafunctions)
        (read-ns-libspec-rename?            file-data state metafunctions) (read-ns-libspec-rename            file-data state metafunctions)
        (read-ns-prefixed-libspec-rename?   file-data state metafunctions) (read-ns-prefixed-libspec-rename   file-data state metafunctions)
        :return file-data))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-ns-raw-libspec?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth tag-ends?]}]
  (and (-> :symbol tag-ends?)
       (= 2 (depth))))

(defn add-ns-raw-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [left-symbol (-> :symbol tag-body)]
       (conj-ns-libspec file-data state metafunctions {:name left-symbol})))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-deps?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (or (-> :ns/import  tag-opened?)
      (-> :ns/require tag-opened?)
      (-> :ns/use     tag-opened?)))

(defn read-ns-deps
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (add-ns-raw-libspec?           file-data state metafunctions) (add-ns-raw-libspec           file-data state metafunctions)
        (add-ns-libspec?               file-data state metafunctions) (add-ns-libspec               file-data state metafunctions)
        (read-ns-directive-operator?   file-data state metafunctions) (read-ns-directive-operator   file-data state metafunctions)
        (disarm-ns-directive-operator? file-data state metafunctions) (disarm-ns-directive-operator file-data state metafunctions)
        (read-ns-libspec?              file-data state metafunctions) (read-ns-libspec              file-data state metafunctions)
        :return file-data))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-ns?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-starts?]}]
  (-> :ns tag-starts?))

(defn add-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (conj-ns file-data state metafunctions {}))

(defn read-ns?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (-> :ns tag-opened?))

(defn read-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-ns-deps? file-data state metafunctions) (read-ns-deps file-data state metafunctions)
        :read-ns-body                                 (read-ns-body file-data state metafunctions)))

(defn close-ns?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-ends?]}]
  (-> :ns tag-ends?))

(defn close-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [ns-started-at (-> :ns tag-started-at)]
       (update-ns file-data state metafunctions merge {:ended-at cursor :started-at ns-started-at})))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-file
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (add-ns?   file-data state metafunctions) (add-ns   file-data state metafunctions)
        (read-ns?  file-data state metafunctions) (read-ns  file-data state metafunctions)
        (close-ns? file-data state metafunctions) (close-ns file-data state metafunctions)
        :return file-data))
