
(ns source-code-map.map.ns-deps
    (:require [map.api     :refer [assoc-by get-by update-by]]
              [reader.api  :as reader]
              [seqable.api :refer [last-dex]]
              [string.api  :as string]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ns-directive
  ; @ignore
  ;
  ; @description
  ; Returns the actual namespace directive opened (if any).
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (keyword)
  ; :import, :require, :use
  [_ _ {:keys [tag-opened?]}]
  (vector/first-match [:import :require :use] tag-opened?))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag) (= :keyword))))

(defn read-ns-directive-operator
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result _ {:keys [tag-body]}]
  (let [left-keyword (-> :keyword tag-body reader/read-edn)]
       (assoc result :left-operator left-keyword)))

(defn disarm-ns-directive-operator?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (or (-> (ending-tag) (= :list))
           (-> (ending-tag) (= :map))
           (-> (ending-tag) (= :vector)))))

(defn disarm-ns-directive-operator
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result _ _]
  (dissoc result :left-operator))

(defn handle-ns-directive-operators
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (cond (read-ns-directive-operator?   result state metafunctions) (read-ns-directive-operator   result state metafunctions)
        (disarm-ns-directive-operator? result state metafunctions) (disarm-ns-directive-operator result state metafunctions)
        :return result))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-libspec-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth ending-tag left-sibling-count tag-ancestor? tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (or (-> (ending-tag) (= :string))
           (-> (ending-tag) (= :symbol)))
       (-> (depth)              (= 3))
       (-> (left-sibling-count) (= 0))))

(defn read-ns-libspec-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (or (tag-body :string)
                         (tag-body :symbol))]
       (assoc-by result [ns-directive :deps last-dex :name] left-symbol)))

(defn read-ns-libspec-alias?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 3))
       (-> result :left-operator (= :as))))

(defn read-ns-libspec-alias
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (assoc-by result [ns-directive :deps last-dex :alias] left-symbol)))

(defn read-ns-libspec-only?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 4))
       (-> result :left-operator (= :only))))

(defn read-ns-libspec-only
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :only] vector/conj-item left-symbol)))

(defn read-ns-libspec-refer-all?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (depth)               (= 3))
       (-> result :left-operator (= :all))))

(defn read-ns-libspec-refer-all
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (let [ns-directive (ns-directive result state metafunctions)]
       (assoc-by result [ns-directive :deps last-dex :refer] :all)))

(defn read-ns-libspec-refer?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 4))
       (-> result :left-operator (= :refer))))

(defn read-ns-libspec-refer
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :refer] vector/conj-item left-symbol)))

(defn read-ns-libspec-rename?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 4))
       (-> result :left-operator (= :rename))))

(defn read-ns-libspec-rename
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (update-by result [ns-directive :deps last-dex :rename]          vector/conj-item [left-symbol])
           (update-by result [ns-directive :deps last-dex :rename last-dex] vector/conj-item left-symbol))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn read-ns-prefixed-raw-libspec-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result state {:keys [depth ending-tag tag-opened?] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)]
       (and (tag-opened? :ns)
            (or (tag-opened? :import)
                (tag-opened? :require)
                (tag-opened? :use))
            (-> (ending-tag) (= :symbol))
            (-> (depth)      (= 3))
            (-> (get-by result [ns-directive :deps last-dex :name]) string/nonempty?)
            (-> result :left-operator not))))

(defn read-ns-prefixed-raw-libspec-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :prefixed] vector/conj-item {:name left-symbol})))

(defn read-ns-prefixed-libspec-name?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 4))
       (-> result :left-operator not)))

(defn read-ns-prefixed-libspec-name
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :prefixed] vector/conj-item {:name left-symbol})))

(defn read-ns-prefixed-libspec-alias?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 4))
       (-> result :left-operator (= :as))))

(defn read-ns-prefixed-libspec-alias
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (assoc-by result [ns-directive :deps last-dex :prefixed last-dex :alias] left-symbol)))

(defn read-ns-prefixed-libspec-only?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 5))
       (-> result :left-operator (= :only))))

(defn read-ns-prefixed-libspec-only
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :prefixed last-dex :only] vector/conj-item left-symbol)))

(defn read-ns-prefixed-libspec-refer-all?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (depth)               (= 4))
       (-> result :left-operator (= :all))))

(defn read-ns-prefixed-libspec-refer-all
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (let [ns-directive (ns-directive result state metafunctions)]
       (assoc-by result [ns-directive :deps last-dex :prefixed last-dex :refer] :all)))

(defn read-ns-prefixed-libspec-refer?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 5))
       (-> result :left-operator (= :refer))))

(defn read-ns-prefixed-libspec-refer
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-by result [ns-directive :deps last-dex :prefixed last-dex :refer] vector/conj-item left-symbol)))

(defn read-ns-prefixed-libspec-rename?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [result _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag)          (= :symbol))
       (-> (depth)               (= 5))
       (-> result :left-operator (= :rename))))

(defn read-ns-prefixed-libspec-rename
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [left-sibling-count tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (if (-> (left-sibling-count) even?)
           (update-by result [ns-directive :deps last-dex :prefixed last-dex :rename]          vector/conj-item [left-symbol])
           (update-by result [ns-directive :deps last-dex :prefixed last-dex :rename last-dex] vector/conj-item left-symbol))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-ns-libspec?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth starting-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (or (-> (starting-tag) (= :list))
           (-> (starting-tag) (= :vector)))
       (-> (depth) (= 2))))

(defn add-ns-libspec
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (let [ns-directive (ns-directive result state metafunctions)]
       (update-in result [ns-directive :deps] vector/conj-item {})))

(defn close-ns-libspec?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth tag-ends? tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (or (tag-ends? :list)
           (tag-ends? :vector))
       (-> (depth) (= 2))))

(defn close-ns-libspec
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result {:keys [cursor] :as state} {:keys [ending-tag tag-started-at] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        libspec-type (ending-tag)
        started-at   (tag-started-at libspec-type)]
       (assoc-by result [ns-directive :deps last-dex :bounds] [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-ns-raw-libspec?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [depth ending-tag tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-opened? :import)
           (tag-opened? :require)
           (tag-opened? :use))
       (-> (ending-tag) (= :symbol))
       (-> (depth)      (= 2))))

(defn add-ns-raw-libspec
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state {:keys [tag-body] :as metafunctions}]
  (let [ns-directive (ns-directive result state metafunctions)
        left-symbol  (tag-body :symbol)]
       (update-in result [ns-directive :deps] vector/conj-item {:name left-symbol})))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn close-ns-directive?
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (boolean)
  [_ _ {:keys [tag-ends? tag-opened?]}]
  (and (tag-opened? :ns)
       (or (tag-ends? :import)
           (tag-ends? :require)
           (tag-ends? :use))))

(defn close-ns-directive
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result {:keys [cursor]} {:keys [ending-tag tag-started-at]}]
  (let [ns-directive (ending-tag)
        started-at   (tag-started-at ns-directive)]
       (assoc-in result [ns-directive :bounds] [started-at cursor])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn map-ns-deps
  ; @ignore
  ;
  ; @param (map) result
  ; @param (map) state
  ; @param (map) metafunctions
  ;
  ; @return (map)
  [result state metafunctions]
  (let [result (handle-ns-directive-operators result state metafunctions)]
       (cond (add-ns-raw-libspec?                 result state metafunctions) (add-ns-raw-libspec                 result state metafunctions)
             (add-ns-libspec?                     result state metafunctions) (add-ns-libspec                     result state metafunctions)
             (read-ns-libspec-name?               result state metafunctions) (read-ns-libspec-name               result state metafunctions)
             (read-ns-prefixed-raw-libspec-name?  result state metafunctions) (read-ns-prefixed-raw-libspec-name  result state metafunctions)
             (read-ns-prefixed-libspec-name?      result state metafunctions) (read-ns-prefixed-libspec-name      result state metafunctions)
             (read-ns-libspec-alias?              result state metafunctions) (read-ns-libspec-alias              result state metafunctions)
             (read-ns-prefixed-libspec-alias?     result state metafunctions) (read-ns-prefixed-libspec-alias     result state metafunctions)
             (read-ns-libspec-only?               result state metafunctions) (read-ns-libspec-only               result state metafunctions)
             (read-ns-prefixed-libspec-only?      result state metafunctions) (read-ns-prefixed-libspec-only      result state metafunctions)
             (read-ns-libspec-refer?              result state metafunctions) (read-ns-libspec-refer              result state metafunctions)
             (read-ns-libspec-refer-all?          result state metafunctions) (read-ns-libspec-refer-all          result state metafunctions)
             (read-ns-prefixed-libspec-refer?     result state metafunctions) (read-ns-prefixed-libspec-refer     result state metafunctions)
             (read-ns-prefixed-libspec-refer-all? result state metafunctions) (read-ns-prefixed-libspec-refer-all result state metafunctions)
             (read-ns-libspec-rename?             result state metafunctions) (read-ns-libspec-rename             result state metafunctions)
             (read-ns-prefixed-libspec-rename?    result state metafunctions) (read-ns-prefixed-libspec-rename    result state metafunctions)
             (close-ns-libspec?                   result state metafunctions) (close-ns-libspec                   result state metafunctions)
             (close-ns-directive?                 result state metafunctions) (close-ns-directive                 result state metafunctions)
             :return result)))
