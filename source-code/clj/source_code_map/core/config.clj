
(ns source-code-map.core.config)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @ignore
;
; @constant (map)
(def PATTERNS
     {; The interpreter is disabled during reading any commented or quoted part of the content.
      :comment       [#";"    #"\n" {:disable-interpreter? true}]
      :regex-pattern [#"\#\"" #"\"" {:disable-interpreter? true}]
      :string        [#"\""   #"\"" {:disable-interpreter? true}]
      ; Lists and vectors are required in order to ensure that all the bracket / parenthesis openings and closings are processed accuratelly.
      ; With the ':low' priority setting, raw lists and vectors are only processed on case of no other known tag starts / closes at the same cursor position.
      :list   [#"\(" #"\)" {:priority :low}]
      :map    [#"\{" #"\}" {:priority :low}]
      :vector [#"\[" #"\]" {:priority :low}]
      ; ...
      :def  [#"\(def(?=[\n\r\s\t])"    #"\)"]
      :defn [#"\(defn(?=[\n\r\s\t\-])" #"\)"]
      :ns   [#"\(ns(?=[\n\r\s\t])"     #"\)"]
      :fn   [#"\(fn(?=[\n\r\s\t])"     #"\)"] ; you could name a function as fn!
      :fn-s [#"\#\("                   #"\)"]
      ; ...
      :ns/info          [#"\(\:author|doc|license(?=[\n\r\s\t\)])" #"\)" {:accepted-parents [:ns]}]
      :ns/gen-class     [#"\(\:gen-class(?=[\n\r\s\t\)])"          #"\)" {:accepted-parents [:ns]}]
      :ns/import        [#"\(\:import(?=[\n\r\s\t])"               #"\)" {:accepted-parents [:ns]}]
      :ns/refer-clojure [#"\(\:refer-clojure(?=[\n\r\s\t])"        #"\)" {:accepted-parents [:ns]}]
      :ns/require       [#"\(\:require(?=[\n\r\s\t])"              #"\)" {:accepted-parents [:ns]}]
      :ns/use           [#"\(\:use(?=[\n\r\s\t])"                  #"\)" {:accepted-parents [:ns]}]
      ; The only difference between keywords and symbols is that keywords start with a colon character (":")
      ; Because of some browsers and also the Clojure RegEx (?) don't support negative lookbehind assertions,
      ; the symbol pattern contains a first character assertion (instead of a negative lookbehind) with all special characters except the colon character.
      ; + symbol first character couldn't be hashtag! #
      ; + symbol first character couldn't be : '
      ;
      ; (def a :a)
      ;
      ; (type #'a)   => var                        (mutable var associated with the symbol)
      ; (type  'a)   => symbol (unresolved symbol) (immutable reference)
      ; (type   a)   => keyword
      :keyword    [#"(?<=[\n\r\s\t\[\(\{])\:[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\#\'\.\:\~\^]{1,}(?=[\n\r\s\t\]\)\}])"]
      :symbol     [#"(?<=[\n\r\s\t\[\(\{])[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\#\'\.\:\~\^]{0,}(?=[\n\r\s\t\]\)\}])"]
      :unresolved [#"(?<=[\n\r\s\t\[\(\{])\'[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\#\'\.\:\~\^]{0,}(?=[\n\r\s\t\]\)\}])"]
      :var        [#"(?<=[\n\r\s\t\[\(\{])\#\'[a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\.\~\^][a-zA-Z\d\+\-\*\/\=\<\>\!\?\_\%\&\#\'\.\:\~\^]{0,}(?=[\n\r\s\t\]\)\}])"]})
