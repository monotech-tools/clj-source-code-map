
(ns source-code-map.core.config
    (:require [syntax-interpreter.api :as syntax-interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @ignore
;
; @constant (map)
(def TAG-PATTERNS
     ; - The ':list', ':map' and ':vector' patterns ensure that the brace, bracket and parenthesis openings and closings are processed accuratelly.
     ; - The '{:priority :high}' setting ensures that the patterns of functions, macros and namespace directives have higher priority than the ':list' and ':vector' patterns.
     ; - The ':pattern-limits' settings help decrease the interpreter processing time.
     ; - The ':comment', ':regex', and ':string' patterns ensures that the commented / quoted parts are not processed.
     {:comment       (:comment syntax-interpreter/CLJ-PATTERNS)
      :keyword       (:keyword syntax-interpreter/CLJ-PATTERNS)
      :list          (:list    syntax-interpreter/CLJ-PATTERNS)
      :map           (:map     syntax-interpreter/CLJ-PATTERNS)
      :regex         (:regex   syntax-interpreter/CLJ-PATTERNS)
      :string        (:string  syntax-interpreter/CLJ-PATTERNS)
      :symbol        (:symbol  syntax-interpreter/CLJ-PATTERNS)
      :vector        (:vector  syntax-interpreter/CLJ-PATTERNS)
      :ns            [#"\(ns(?=[\n\r\s\t])"                             #"\)" {:accepted-parents []    :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  3 :closing/lookahead 0 :opening/lookahead 1}}]
      :info          [#"\(\:author|\(\:doc|\(\:license(?=[\n\r\s\t\)])" #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  9 :closing/lookahead 0 :opening/lookahead 1}}]
      :gen-class     [#"\(\:gen-class(?=[\n\r\s\t\)])"                  #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match 11 :closing/lookahead 0 :opening/lookahead 1}}]
      :import        [#"\(\:import(?=[\n\r\s\t])"                       #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  8 :closing/lookahead 0 :opening/lookahead 1}}]
      :refer-clojure [#"\(\:refer-clojure(?=[\n\r\s\t])"                #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match 15 :closing/lookahead 0 :opening/lookahead 1}}]
      :require       [#"\(\:require(?=[\n\r\s\t])"                      #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  9 :closing/lookahead 0 :opening/lookahead 1}}]
      :use           [#"\(\:use(?=[\n\r\s\t])"                          #"\)" {:accepted-parents [:ns] :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  5 :closing/lookahead 0 :opening/lookahead 1}}]
      :def           [#"\(def(?=[\n\r\s\t])"                            #"\)" {:accepted-parents []    :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  4 :closing/lookahead 0 :opening/lookahead 1}}]
      :defn          [#"\(defn[\-]{0,}(?=[\n\r\s\t])"                   #"\)" {:accepted-parents []    :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  6 :closing/lookahead 0 :opening/lookahead 1}}]
      :anfn          [#"\(fn(?=[\n\r\s\t])"                             #"\)" {                        :priority :high :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  3 :closing/lookahead 0 :opening/lookahead 1}}]
      :anfn-s        [#"\#\("                                           #"\)" {                        :priority :high :pattern-limits {:lookbehind 0 :lookahead 0 :closing/match 1 :opening/match  2}}]})
