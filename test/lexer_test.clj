(ns lexer-test
  (:require
   [clojure.test :refer :all]
   [clojure.string]
   [lexer :refer :all]
   [iter :refer :all]))

(defn compare-arrays [arr1 arr2]
  (let [diffs (filter #(not= (arr1 %) (arr2 %))
                      (range (min (count arr1) (count arr2))))]
    (if (empty? diffs)
      (if (= (count arr1) (count arr2))
        "Arrays match completely."
        (str "Arrays differ in length: " (count arr1) " vs " (count arr2)))
      (str "Differences at indices: " (clojure.string/join ", " diffs) "."))))

(deftest test-tokenize
  (testing "tokenize"
    (is (= (tokenize "abc = 123") [{:type :IDENT :ch [\a \b \c]} {:type :ASSIGN} {:type :INT :ch [\1 \2 \3]} {:type :EOF}]))
    (is (= (tokenize "12") [{:type :INT :ch [\1 \2]} {:type :EOF}]))
    (is (= (tokenize "") [{:type :EOF}]))
    (is (= (tokenize "==") [{:type :EQ} {:type :EOF}]))
    (is (= (tokenize " !=") [{:type :NEQ} {:type :EOF}]))
    (is (= (tokenize "let") [{:type :LET} {:type :EOF}]))
    (is (= (tokenize " 98") [{:type :INT :ch [\9 \8]} {:type :EOF}]))
    (is (= (tokenize "let five = 5;
                      let ten = 10;") [{:type :LET}
                                       {:type :IDENT :ch [\f \i \v \e]}
                                       {:type :ASSIGN}
                                       {:type :INT :ch [\5]}
                                       {:type :SEMICOLON}
                                       {:type :LET}
                                       {:type :IDENT :ch [\t \e \n]}
                                       {:type :ASSIGN}
                                       {:type :INT :ch [\1 \0]}
                                       {:type :SEMICOLON}
                                       {:type :EOF}])))
  (testing "whole program"
    (is (= (tokenize "let five = 5;
                     let ten = 10;

                     let add = fn (x, y) {x + y;
                     };

                     let result = add (five, ten);
                     !-/*5;
                     5 < 10 > 5;

                     if (5 < 10) {return true;
                                  }else {return false;
                                         }

                     10 == 10;
                     10 != 9;") [{:type :LET}
                                 {:type :IDENT :ch [\f \i \v \e]}
                                 {:type :ASSIGN}
                                 {:type :INT :ch [\5]}
                                 {:type :SEMICOLON}
                                 {:type :LET}
                                 {:type :IDENT :ch [\t \e \n]}
                                 {:type :ASSIGN}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :SEMICOLON}
                                 {:type :LET}
                                 {:type :IDENT :ch [\a \d \d]}
                                 {:type :ASSIGN}
                                 {:type :FUNCTION}
                                 {:type :LPAREN}
                                 {:type :IDENT :ch [\x]}
                                 {:type :COMMA}
                                 {:type :IDENT :ch [\y]}
                                 {:type :RPAREN}
                                 {:type :LBRACE}
                                 {:type :IDENT :ch [\x]}
                                 {:type :PLUS}
                                 {:type :IDENT :ch [\y]}
                                 {:type :SEMICOLON}
                                 {:type :RBRACE}
                                 {:type :SEMICOLON}
                                 {:type :LET}
                                 {:type :IDENT :ch [\r \e \s \u \l \t]}
                                 {:type :ASSIGN}
                                 {:type :IDENT :ch [\a \d \d]}
                                 {:type :LPAREN}
                                 {:type :IDENT :ch [\f \i \v \e]}
                                 {:type :COMMA}
                                 {:type :IDENT :ch [\t \e \n]}
                                 {:type :RPAREN}
                                 {:type :SEMICOLON}
                                 {:type :BANG}
                                 {:type :MINUS}
                                 {:type :SLASH}
                                 {:type :ASTERISK}
                                 {:type :INT :ch [\5]}
                                 {:type :SEMICOLON}
                                 {:type :INT :ch [\5]}
                                 {:type :LT}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :GT}
                                 {:type :INT :ch [\5]}
                                 {:type :SEMICOLON}
                                 {:type :IF}
                                 {:type :LPAREN}
                                 {:type :INT :ch [\5]}
                                 {:type :LT}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :RPAREN}
                                 {:type :LBRACE}
                                 {:type :RETURN}
                                 {:type :TRUE}
                                 {:type :SEMICOLON}
                                 {:type :RBRACE}
                                 {:type :ELSE}
                                 {:type :LBRACE}
                                 {:type :RETURN}
                                 {:type :FALSE}
                                 {:type :SEMICOLON}
                                 {:type :RBRACE}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :EQ}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :SEMICOLON}
                                 {:type :INT :ch [\1 \0]}
                                 {:type :NEQ}
                                 {:type :INT :ch [\9]}
                                 {:type :SEMICOLON}
                                 {:type :EOF}]))))



;; (print (time (tokenize "let five = 5;
;;                      let ten = 10;

;;                      let add = fn (x, y) {x + y;
;;                      };

;;                      let result = add (five, ten);
;;                      !-/*5;
;;                      5 < 10 > 5;

;;                      if (5 < 10) {return true;
;;                                   }else {return false;
;;                                          }

;;                      10 == 10;
;;                      10 != 9;")))


;; (let [iterator (.iterator (tokenize infinite-program))]
;;   (print (.next iterator))
;;   (print  (.peek iterator))
;;   (print (.next iterator)))


;; ((deftest test-parse-let-statment
;;    (testing "Parsing let statment"
;;      (is (= (parse (tokenize "let x = 5;")) [])))))