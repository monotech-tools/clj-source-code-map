
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
     ; - The ':comment', ':regex-pattern', ':meta-string', and ':string' patterns ensure that the commented / quoted parts are not processed.
     ; - The ':conditional-form' pattern ensures that the mapping process can skip the conditional forms in the source code,
     ;   because its too complicated to process them, maybe in later version it will be solved.
     [[:with-meta               #"\(with-meta(?=\s)"                        #"\)"                         {:pattern-limits {:closing/match 1 :opening/match 10 :opening/lookahead 1}}]
      [:ns                      #"\(ns(?=\s)"                               #"\)" {:accepted-parents []    :pattern-limits {:closing/match 1 :opening/match  3 :opening/lookahead 1}}]
      [:gen-class-directive     #"\(\:gen-class(?=[\s\)])"                  #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match 11 :opening/lookahead 1}}]
      [:info-directive          #"\(\:author|\(\:doc|\(\:license(?=[\s\)])" #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match  9 :opening/lookahead 1}}]
      [:refer-clojure-directive #"\(\:refer-clojure(?=\s)"                  #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match 15 :opening/lookahead 1}}]
      [:import-directive        #"\(\:import(?=\s)"                         #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match  8 :opening/lookahead 1}}]
      [:require-directive       #"\(\:require(?=\s)"                        #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match  9 :opening/lookahead 1}}]
      [:use-directive           #"\(\:use(?=\s)"                            #"\)" {:accepted-parents [:ns] :pattern-limits {:closing/match 1 :opening/match  5 :opening/lookahead 1}}]
      [:def                     #"\(def(?=\s)"                              #"\)"                         {:pattern-limits {:closing/match 1 :opening/match  4 :opening/lookahead 1}}]
      [:defn                    #"\(defn[\-]{0,}(?=\s)"                     #"\)"                         {:pattern-limits {:closing/match 1 :opening/match  6 :opening/lookahead 1}}]
      [:anfn                    #"\(fn(?=\s)"                               #"\)"                         {:pattern-limits {:closing/match 1 :opening/match  3 :opening/lookahead 1}}]
      [:anfn-s                  #"\#\("                                     #"\)"                         {:pattern-limits {:closing/match 1 :opening/match  2}}]
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
