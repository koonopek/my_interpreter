(ns parser-test
  (:require
   [clojure.test :refer [deftest is]]
   [parser :as parser]
   [iter :as iter]))


(defn str-to-iter [str] (iter/to-iter (vec str)))

(deftest parse-let-statment-test
  (is (= (parser/parse-statment (str-to-iter "let a = 5 * 5 ;"))
         {:name {:type :IDENT :ch [\a]}}))

  (is (= (parser/parse-statment (str-to-iter "let a = 5; let xyz = 312;"))
         {:name {:type :IDENT :ch [\a]}})))

(deftest parse-program
  (is (= (parser/parse-program (str-to-iter "let a = 5; let xyz = 312;"))
         [{:name {:type :IDENT :ch [\a]}}
          {:name {:type :IDENT :ch [\x \y \z]}}])))