(ns parser
  (:require
   [lexer :refer [parse-token]]))



(defn parse-and-assert [iter expected-type]
  (let [token (parse-token iter)]
    (if (= (:type token) expected-type)
      token
      nil)))


(defn parse-let-statment [iter]
  (let [name (parse-and-assert iter :IDENT)
        _assign (parse-and-assert iter :ASSIGN)]
    (while (not (= (parse-token iter) {:type :SEMICOLON})))
    {:name name}))


(defn parse-statment [iter]
  (let [token (parse-token iter)
        token-type (:type token)]
    (cond
      (= token-type :LET) (parse-let-statment iter)
      (= token-type :EOF) :EOF
      :else (throw (Exception. (str "uknown token: " token " iter: " (.get-coll iter)))))))

(defn parse-program [iter]
  (loop [statments []]
    (let [statement (parse-statment iter)]
      (if (= statement :EOF)
        statments
        (recur (conj statments statement))))))

