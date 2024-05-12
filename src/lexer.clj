(ns lexer
  (:require [clojure.set :refer [map-invert]] [iter :refer [to-iter]]))

(def single-tokens {; Operators
                    :ASSIGN  \=
                    :PLUS  \+
                    :MINUS  \-
                    :BANG \!
                    :ASTERISK \*
                    :LT \<
                    :GT \>
             ; Delimiters
                    :SLASH  \/,
                    :COMMA  \,
                    :SEMICOLON  \;
                    :LPAREN  \(
                    :RPAREN  \)
                    :LBRACE  \{
                    :RBRACE  \}})

(def double-tokens {; combined
                    :EQ [\= \=]
                    :NEQ [\! \=]})

(def keywords {:FUNCTION (seq "fn")
               :LET      (seq "let")
               :TRUE     (seq "true")
               :FALSE    (seq "false")
               :IF       (seq "if")
               :ELSE     (seq "else")
               :RETURN   (seq "return")})

(def double-char-to-token (map-invert double-tokens))
(def char-to-token (map-invert single-tokens))
(def chars-to-keyword (map-invert keywords))

(defn is-letter? [char] (Character/isLetter char))
(defn is-digit? [char] (Character/isDigit char))
(defn is-whitespace? [char] (Character/isWhitespace char))


(defn consume-x [iter predicate? type]
  (if (nil? (.peek-char iter)) [iter {:type :ILLEGAL}]
      (loop [token {:type type :ch []}]
        (cond
          (nil? (.peek-char iter)) token
          (predicate? (.peek-char iter)) (recur (assoc token :ch (conj (get token :ch) (.read-char iter))))
          :else token))))

(defn maybe-to-keyword [token] (if-some [keyword (get chars-to-keyword (get token :ch))] {:type keyword} token))

(defn consume-letters [string] (let [token  (consume-x string is-letter? :IDENT)]  (maybe-to-keyword token)))

(defn consume-digits [string] (consume-x string is-digit? :INT))

(defn consume-whitespaces [string] (consume-x string is-whitespace? :IGNORE))

(defn tokenize-iter [characters]
  (loop [result []]
    (cond
      (nil? (.peek-char characters)) (conj result {:type :EOF})

      (contains? double-char-to-token (take 2 (.get-coll characters))) (recur  (conj result {:type (get double-char-to-token (.read-n-char  characters 2))}))

      (contains? char-to-token (.peek-char characters)) (recur  (conj result {:type (get char-to-token (.read-char characters))}))

      (is-whitespace? (.peek-char characters))  (recur  (let [_ (consume-whitespaces characters)] result))

      (is-letter? (.peek-char characters)) (recur (conj result (consume-letters characters)))

      (is-digit? (.peek-char characters)) (recur (conj result (consume-digits characters)))

      :else (throw (Exception. (str "Unknown char: '" (.peek-char characters) "'"))))))

(defn tokenize [string] (tokenize-iter (to-iter (vec string))))


