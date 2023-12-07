
(ns source-code-map.core.config
    (:require [syntax-interpreter.api :as syntax-interpreter]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @ignore
;
; @constant (map)
(def TAG-PATTERNS
     ; - The ':list', ':map' and ':vector' patterns ensure that the brace, bracket and parenthesis openings and closings are processed accuratelly.
     ; - The ':pattern-limits' settings help decrease the interpreter processing time.
     ; - The ':comment', ':regex', and ':string' patterns ensures that the commented / quoted parts are not processed.
     ; - The ':conditional-form' pattern ensures that the mapping process can skip the conditional forms in the source code,
     ;   because its too complicated to process them, maybe in later version it will be solved.
     [[:with-meta               #"\(with-meta(?=[\n\r\s\t])"                      #"\)"                         {:pattern-limits {:lookbehind 0              :closing/match 1 :opening/match 10 :closing/lookahead 0 :opening/lookahead 1}}]
      [:ns                      #"\(ns(?=[\n\r\s\t])"                             #"\)" {:accepted-parents []    :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  3 :closing/lookahead 0 :opening/lookahead 1}}]
      [:gen-class-directive     #"\(\:gen-class(?=[\n\r\s\t\)])"                  #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match 11 :closing/lookahead 0 :opening/lookahead 1}}]
      [:info-directive          #"\(\:author|\(\:doc|\(\:license(?=[\n\r\s\t\)])" #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  9 :closing/lookahead 0 :opening/lookahead 1}}]
      [:refer-clojure-directive #"\(\:refer-clojure(?=[\n\r\s\t])"                #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match 15 :closing/lookahead 0 :opening/lookahead 1}}]
      [:import-directive        #"\(\:import(?=[\n\r\s\t])"                       #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  8 :closing/lookahead 0 :opening/lookahead 1}}]
      [:require-directive       #"\(\:require(?=[\n\r\s\t])"                      #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  9 :closing/lookahead 0 :opening/lookahead 1}}]
      [:use-directive           #"\(\:use(?=[\n\r\s\t])"                          #"\)" {:accepted-parents [:ns] :pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  5 :closing/lookahead 0 :opening/lookahead 1}}]
      [:def                     #"\(def(?=[\n\r\s\t])"                            #"\)"                         {:pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  4 :closing/lookahead 0 :opening/lookahead 1}}]
      [:defn                    #"\(defn[\-]{0,}(?=[\n\r\s\t])"                   #"\)"                         {:pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  6 :closing/lookahead 0 :opening/lookahead 1}}]
      [:anfn                    #"\(fn(?=[\n\r\s\t])"                             #"\)"                         {:pattern-limits {:lookbehind 0              :closing/match 1 :opening/match  3 :closing/lookahead 0 :opening/lookahead 1}}]
      [:anfn-s                  #"\#\("                                           #"\)"                         {:pattern-limits {:lookbehind 0 :lookahead 0 :closing/match 1 :opening/match  2}}]
      (:boolean          syntax-interpreter/CLJ-PATTERNS)
      (:comment          syntax-interpreter/CLJ-PATTERNS)
      (:conditional-form syntax-interpreter/CLJ-PATTERNS)
      (:keyword          syntax-interpreter/CLJ-PATTERNS)
      (:meta-keyword     syntax-interpreter/CLJ-PATTERNS)
      (:meta-map         syntax-interpreter/CLJ-PATTERNS)
      (:meta-string      syntax-interpreter/CLJ-PATTERNS)
      (:meta-symbol      syntax-interpreter/CLJ-PATTERNS)
      (:list             syntax-interpreter/CLJ-PATTERNS)
      (:map              syntax-interpreter/CLJ-PATTERNS)
      (:regex-pattern    syntax-interpreter/CLJ-PATTERNS)
      (:string           syntax-interpreter/CLJ-PATTERNS)
      (:symbol           syntax-interpreter/CLJ-PATTERNS)
      (:vector           syntax-interpreter/CLJ-PATTERNS)])
