
(ns source-code-map.import.ns
    (:require [map.api     :refer [assoc-by dissoc-by get-by update-by]]
              [reader.api  :as reader]
              [seqable.api :refer [last-dex]]
              [string.api  :as string]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-directive
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
       (assoc-by file-data [:ns last-dex :left-operator] left-keyword)))

(defn disarm-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag]}]
  (or (-> (ending-tag) (= :list))
      (-> (ending-tag) (= :map))
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
  (dissoc-by file-data [:ns last-dex :left-operator]))

(defn handle-ns-directive-operators
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (cond (read-ns-directive-operator?   file-data state metafunctions) (read-ns-directive-operator   file-data state metafunctions)
        (disarm-ns-directive-operator? file-data state metafunctions) (disarm-ns-directive-operator file-data state metafunctions)
        :return file-data))

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
  [_ _ {:keys [depth ending-tag left-sibling-count]}]
  (and (-> (ending-tag)         (= :symbol))
       (-> (left-sibling-count) (= 0))
       (-> (depth)              (= 1))))

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
       (assoc-by file-data [:ns last-dex :name] left-symbol)))

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
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (or (tag-body :string)
                         (tag-body :symbol))]
       (assoc-by file-data [:ns last-dex ns-directive :deps last-dex :name] left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :as))))

(defn read-ns-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (assoc-by file-data [:ns last-dex ns-directive :deps last-dex :alias] left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :only))))

(defn read-ns-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :only] vector/conj-item left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :refer))))

(defn read-ns-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :refer] vector/conj-item left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :rename))))

(defn read-ns-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (update-by file-data [:ns last-dex ns-directive :deps last-dex :rename]          vector/conj-item [left-symbol])
           (update-by file-data [:ns last-dex ns-directive :deps last-dex :rename last-dex] vector/conj-item left-symbol))))

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
  (let [ns-directive (ns-directive file-data state metafunctions)]
       (and (-> (ending-tag) (= :symbol))
            (-> (depth)      (= 3))
            (-> (get-by file-data [:ns last-dex ns-directive :deps last-dex :name]) string/nonempty?)
            (-> (get-by file-data [:ns last-dex :left-operator]) not))))

(defn read-ns-prefixed-raw-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed] vector/conj-item {:name left-symbol})))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) not)))

(defn read-ns-prefixed-libspec-name
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed] vector/conj-item {:name left-symbol})))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :as))))

(defn read-ns-prefixed-libspec-alias
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (assoc-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed last-dex :alias] left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :only))))

(defn read-ns-prefixed-libspec-only
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed last-dex :only] vector/conj-item left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :refer))))

(defn read-ns-prefixed-libspec-refer
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed last-dex :refer] vector/conj-item left-symbol)))

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
       (-> (get-by file-data [:ns last-dex :left-operator]) (= :rename))))

(defn read-ns-prefixed-libspec-rename
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed last-dex :rename]          vector/conj-item [left-symbol])
           (update-by file-data [:ns last-dex ns-directive :deps last-dex :prefixed last-dex :rename last-dex] vector/conj-item left-symbol))))

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
  (let [ns-directive (ns-directive file-data state metafunctions)]
       (update-by file-data [:ns last-dex ns-directive :deps] vector/conj-item {})))

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

(defn close-ns-libspec?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth tag-ends?]}]
  (and (or (tag-ends? :list)
           (tag-ends? :vector))
       (-> (depth) (= 2))))

(defn close-ns-libspec
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor] :as state} {:keys [ending-tag tag-started-at] :as metafunctions}]
  (let [ns-directive (ns-directive file-data state metafunctions)
        libspec-type (ending-tag)
        started-at   (tag-started-at libspec-type)]
       (assoc-by file-data [:ns last-dex ns-directive :deps last-dex :bounds] [started-at cursor])))

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
  (let [ns-directive (ns-directive file-data state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by file-data [:ns last-dex ns-directive :deps] vector/conj-item {:name left-symbol})))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-directive?
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

(defn read-ns-directive
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data state metafunctions]
  (let [file-data (handle-ns-directive-operators file-data state metafunctions)]
       (cond (add-ns-raw-libspec? file-data state metafunctions) (add-ns-raw-libspec file-data state metafunctions)
             (add-ns-libspec?     file-data state metafunctions) (add-ns-libspec     file-data state metafunctions)
             (read-ns-libspec?    file-data state metafunctions) (read-ns-libspec    file-data state metafunctions)
             (close-ns-libspec?   file-data state metafunctions) (close-ns-libspec   file-data state metafunctions)
             :return file-data)))

(defn close-ns-directive?
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-ends?]}]
  (or (tag-ends? :ns/import)
      (tag-ends? :ns/require)
      (tag-ends? :ns/use)))

(defn close-ns-directive
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [file-data {:keys [cursor]} {:keys [ending-tag tag-started-at]}]
  (let [ns-directive (ending-tag)
        started-at   (tag-started-at ns-directive)]
       (assoc-by file-data [:ns last-dex ns-directive :bounds] [started-at cursor])))

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
  (update file-data :ns vector/conj-item {}))

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
  (cond (read-ns-name?       file-data state metafunctions) (read-ns-name       file-data state metafunctions)
        (read-ns-directive?  file-data state metafunctions) (read-ns-directive  file-data state metafunctions)
        (close-ns-directive? file-data state metafunctions) (close-ns-directive file-data state metafunctions)
        :return file-data))

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
  (let [started-at (tag-started-at :ns)]
       (assoc-by file-data [:ns last-dex :bounds] [started-at cursor])))

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
