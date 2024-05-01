(ns repl
  (:require [lexer :refer [tokenize]]))


(defn start-repl []
  (print "Let's fucking go!")
  (flush)
  (loop []
    (print \newline ">> ")
    (flush)
    (let [input (read-line)
          result (tokenize input)]
      (print result))
    (recur)))