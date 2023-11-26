
(ns source-code-map.import.ns
    (:require [reader.api                   :as reader]
              [source-code-map.import.utils :as import.utils]
              [string.api                   :as string]
              [vector.api                   :as vector]))

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
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :keyword)))

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
       (import.utils/update-last-block-data file-data [:ns] assoc :left-operator left-keyword)))

(defn disarm-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (or (-> (ending-tag) (= :map))
      (-> (ending-tag) (= :vector))))

(defn disarm-ns-directive-operator
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (import.utils/update-last-block-data file-data [:ns] dissoc :left-operator))

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
  [_ _ {:keys [ending-tag left-sibling-count]}]
  (and (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))))

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
       (import.utils/update-last-block-data file-data [:ns] assoc :name left-symbol)))

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
  [_ _ {:keys [depth ending-tag left-sibling-count tag-ancestor?]}]
  (and (or (-> (ending-tag) (= :string))
           (-> (ending-tag) (= :symbol)))
       (-> (depth)              (= 3))
       (-> (left-sibling-count) (= 0))))

(defn read-ns-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (or (tag-body :string)
                           (tag-body :symbol))]
       (import.utils/update-last-block-data file-data [:ns deps-directive] assoc :name left-symbol)))

(defn read-ns-libspec-alias?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 3))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :as))))

(defn read-ns-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/update-last-block-data file-data [:ns deps-directive] assoc :alias left-symbol)))

(defn read-ns-libspec-only?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :only))))

(defn read-ns-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :only] left-symbol)))

(defn read-ns-libspec-refer?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :refer))))

(defn read-ns-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :refer] left-symbol)))

(defn read-ns-libspec-rename?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :rename))))

(defn read-ns-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (import.utils/conj-block-data        file-data [:ns deps-directive :rename] [left-symbol])
           (import.utils/update-last-block-data file-data [:ns deps-directive :rename] vector/conj-item left-symbol))))

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
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (and (-> (ending-tag) (= :symbol))
            (-> (depth)      (= 3))
            (-> (import.utils/get-last-block-data file-data [:ns deps-directive]) :name string/nonempty?)
            (-> (import.utils/get-last-block-data file-data [:ns])                :left-operator not))))

(defn read-ns-prefixed-raw-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :prefixed] {:name left-symbol})))

(defn read-ns-prefixed-libspec-name?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator not)))

(defn read-ns-prefixed-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :prefixed] {:name left-symbol})))

(defn read-ns-prefixed-libspec-alias?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :as))))

(defn read-ns-prefixed-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/update-last-block-data file-data [:ns deps-directive :prefixed] assoc :alias left-symbol)))

(defn read-ns-prefixed-libspec-only?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 5))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :only))))

(defn read-ns-prefixed-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :prefixed :only] left-symbol)))

(defn read-ns-prefixed-libspec-refer?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 5))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :refer))))

(defn read-ns-prefixed-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive :prefixed :refer] left-symbol)))

(defn read-ns-prefixed-libspec-rename?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [file-data state {:keys [depth ending-tag] :as metafunctions}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 5))
       (-> (import.utils/get-last-block-data file-data [:ns]) :left-operator (= :rename))))

(defn read-ns-prefixed-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (import.utils/conj-block-data        file-data [:ns deps-directive :prefixed :rename] [left-symbol])
           (import.utils/update-last-block-data file-data [:ns deps-directive :prefixed :rename] vector/conj-item left-symbol))))

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
  [_ _ {:keys [depth starting-tag]}]
  (and (or (-> (starting-tag) (= :list))
           (-> (starting-tag) (= :vector)))
       (-> (depth) (= 2))))

(defn add-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)]
       (import.utils/conj-block-data file-data [:ns deps-directive] {})))

(defn read-ns-libspec?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (or (tag-opened? :list)
      (tag-opened? :vector)))

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
  [_ _ {:keys [depth ending-tag]}]
  (and (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 2))))

(defn add-ns-raw-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [deps-directive (ns-deps-directive file-data state metafunctions)
        left-symbol    (tag-body :symbol)]
       (import.utils/conj-block-data file-data [:ns deps-directive] {:name left-symbol})))

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
  (or (tag-opened? :ns/import)
      (tag-opened? :ns/require)
      (tag-opened? :ns/use)))

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
  [_ _ {:keys [starting-tag]}]
  (-> (starting-tag) (= :ns)))

(defn add-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (import.utils/conj-block-data file-data [:ns] {}))

(defn read-ns?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-opened?]}]
  (tag-opened? :ns))

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
  [_ _ {:keys [ending-tag]}]
  (-> (ending-tag) (= :ns)))

(defn close-ns
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [tag-started-at] :as metafunctions}]
  (let [ns-started-at (tag-started-at :ns)]
       (import.utils/update-last-block-data file-data [:ns] merge {:ended-at cursor :started-at ns-started-at})))

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
