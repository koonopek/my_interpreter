(ns lexer
  (:require [clojure.set :refer [map-invert]]))

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

(defn consume-x [input predicate? type]
  (if (empty? input) [input {:type :ILLEGAL}]
      (loop [string input token {:type type :ch []}]
        (let [char (first string)]
          (cond
            (nil? char) [string token]
            (predicate? char) (recur (rest string) (assoc token :ch (conj (get token :ch) char)))
            :else [string token])))))

(defn maybe-to-keyword [token] (if-some [keyword (get chars-to-keyword (get token :ch))] {:type keyword} token))

(defn consume-letters [string] (let [[left token]  (consume-x string is-letter? :IDENT)] [left (maybe-to-keyword token)]))
(defn consume-digits [string] (consume-x string is-digit? :INT))
(defn consume-whitespaces [string] (consume-x string is-whitespace? :IGNORE))

(defn tokenize [string]
  (loop [characters (seq string)
         result []]
    (cond
      (empty? characters) (conj result {:type :EOF})
      (contains? double-char-to-token (take 2 characters)) (recur (nthrest  characters 2) (conj result {:type (get double-char-to-token (take 2 characters))}))
      (contains? char-to-token (first characters)) (recur (rest characters) (conj result {:type (get char-to-token (first characters))}))
      (is-whitespace? (first characters)) (recur (first (consume-whitespaces characters))
                                                 result)
      (is-letter? (first characters)) (recur (first (consume-letters characters))
                                             (conj result (second (consume-letters characters))))
      (is-digit? (first characters)) (recur (first (consume-digits characters))
                                            (conj result (second (consume-digits characters))))
      :else (throw (Exception. (str "Unknown char: '" (first characters) "'"))))))

