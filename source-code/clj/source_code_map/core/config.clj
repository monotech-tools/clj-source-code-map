
(ns source-code-map.core.config
    (:require [syntax-reader.api :as syntax-reader]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @ignore
;
; @constant (map)
(def PATTERNS
     {; The ':list', ':map' and ':vector' patterns ensure that the brace, bracket and parenthesis openings and closings are processed accuratelly.
      :comment    (:comment    syntax-reader/CLJ-PATTERNS)
      :keyword    (:keyword    syntax-reader/CLJ-PATTERNS)
      :list       (:list       syntax-reader/CLJ-PATTERNS)
      :map        (:map        syntax-reader/CLJ-PATTERNS)
      :regex      (:regex      syntax-reader/CLJ-PATTERNS)
      :string     (:string     syntax-reader/CLJ-PATTERNS)
      :symbol     (:symbol     syntax-reader/CLJ-PATTERNS)
      :unresolved (:unresolved syntax-reader/CLJ-PATTERNS)
      :var        (:var        syntax-reader/CLJ-PATTERNS)
      :vector     (:vector     syntax-reader/CLJ-PATTERNS)

      ; The '{:priority :high}' setting ensures that the functions, macros and directives have higher priority
      ; in the interpreter than the ':list' pattern.
      :def              [#"\(def(?=[\n\r\s\t])"                    #"\)" {:priority :high}]
      :defn             [#"\(defn(?=[\n\r\s\t\-])"                 #"\)" {:priority :high}]
      :ns               [#"\(ns(?=[\n\r\s\t])"                     #"\)" {:priority :high}]
      :fn               [#"\(fn(?=[\n\r\s\t])"                     #"\)" {:priority :high}] ; Any function could be named as 'fn'!
      :fn-s             [#"\#\("                                   #"\)" {:priority :high}]
      :ns/info          [#"\(\:author|doc|license(?=[\n\r\s\t\)])" #"\)" {:accepted-parents [:ns] :priority :high}]
      :ns/gen-class     [#"\(\:gen-class(?=[\n\r\s\t\)])"          #"\)" {:accepted-parents [:ns] :priority :high}]
      :ns/import        [#"\(\:import(?=[\n\r\s\t])"               #"\)" {:accepted-parents [:ns] :priority :high}]
      :ns/refer-clojure [#"\(\:refer-clojure(?=[\n\r\s\t])"        #"\)" {:accepted-parents [:ns] :priority :high}]
      :ns/require       [#"\(\:require(?=[\n\r\s\t])"              #"\)" {:accepted-parents [:ns] :priority :high}]
      :ns/use           [#"\(\:use(?=[\n\r\s\t])"                  #"\)" {:accepted-parents [:ns] :priority :high}]})
