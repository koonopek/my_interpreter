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

(defn parse-token [iterator]
  (cond
    (nil? (.peek-char iterator)) {:type :EOF}

    (contains? double-char-to-token (take 2 (.get-coll iterator))) {:type (get double-char-to-token (.read-n-char  iterator 2))}

    (contains? char-to-token (.peek-char iterator)) {:type (get char-to-token (.read-char iterator))}

    (is-whitespace? (.peek-char iterator))  (do (consume-whitespaces iterator) (parse-token iterator))

    (is-letter? (.peek-char iterator)) (consume-letters iterator)

    (is-digit? (.peek-char iterator)) (consume-digits iterator)

    :else (throw (Exception. (str "Unknown char: '" (.peek-char iterator) "'")))))


(defn tokenize-iter [iterator]
  (loop [result []]
    (let [next-token (parse-token iterator)]
      (if (= (:type next-token) :EOF)
        (conj result next-token)
        (recur (conj result next-token))))))

(defn tokenize [string] (tokenize-iter (to-iter (vec string))))


