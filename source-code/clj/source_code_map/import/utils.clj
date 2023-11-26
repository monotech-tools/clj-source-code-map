
(ns source-code-map.import.utils
    (:require [map.api     :refer [get-by update-by]]
              [seqable.api :as seqable]
              [vector.api  :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn get-last-block-data
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (vector) path
  ;
  ; @usage
  ; (get-last-block-data {...} [:ns])
  ;
  ; @usage
  ; (get-last-block-data {...} [:ns :ns/require])
  ;
  ; @usage
  ; (get-last-block-data {...} [:ns :ns/require :prefixed])
  ;
  ; @example
  ; (get-last-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                            {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                      [:ns])
  ; =>
  ; {:name "second-ns" :ns/require [{:name "second-namespace"}]}
  ;
  ; @example
  ; (get-last-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                            {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                      [:ns :ns/require])
  ; =>
  ; {:name "second-namespace"}
  ;
  ; @return (map)
  [file-data path]
  (let [dynamic-path (vector/suffix-items path seqable/last-dex)]
       (get-by file-data dynamic-path)))

(defn conj-block-data
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (vector) path
  ; @param (*) initial
  ;
  ; @usage
  ; (conj-block-data {...} [:ns])
  ;
  ; @usage
  ; (conj-block-data {...} [:ns :ns/require])
  ;
  ; @usage
  ; (conj-block-data {...} [:ns :ns/require :prefixed])
  ;
  ; @example
  ; (conj-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                        {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                  [:ns]
  ;                  {:name "third-ns"})
  ; =>
  ; {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;       {:name "second-ns" :ns/require [{:name "second-namespace"}]}
  ;       {:name "third-ns"}]}
  ;
  ; @example
  ; (conj-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                        {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                  [:ns :ns/require]
  ;                  {:name "third-namespace"})
  ; =>
  ; {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;       {:name "second-ns" :ns/require [{:name "second-namespace"}
  ;                                       {:name "third-namespace"}]}]}
  ;
  ; @return (map)
  [file-data path initial]
  (let [dynamic-path (vector/gap-items path seqable/last-dex)]
       (update-by file-data dynamic-path vector/conj-item initial)))

(defn update-last-block-data
  ; @ignore
  ;
  ; @param (map) file-data
  ; @param (vector) path
  ; @param (function) f
  ; @param (list of *) params
  ;
  ; @usage
  ; (update-last-block-data {...} [:ns] assoc :name "NS name")
  ;
  ; @usage
  ; (update-last-block-data {...} [:ns :ns/require] ...)
  ;
  ; @usage
  ; (update-last-block-data {...} [:ns :ns/require :prefixed] ...)
  ;
  ; @example
  ; (update-last-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                               {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                  [:ns]
  ;                  assoc :started-at 42)
  ; =>
  ; {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;       {:name "second-ns" :ns/require [{:name "second-namespace"}] :started-at 42}]}
  ;
  ; @example
  ; (update-last-block-data {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;                               {:name "second-ns" :ns/require [{:name "second-namespace"}]}]}
  ;                         [:ns :ns/require]
  ;                         assoc :started-at 42)
  ; =>
  ; {:ns [{:name "first-ns"  :ns/require [{:name "first-namespace"}]}
  ;       {:name "second-ns" :ns/require [{:name "second-namespace" :started-at 42}]}]}
  ;
  ; @return (map)
  [file-data path f & params]
  (let [dynamic-path (vector/suffix-items path seqable/last-dex)]
       (apply update-by file-data dynamic-path f params)))
