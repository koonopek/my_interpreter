(ns parser)


(defn parse [tokens]
  (loop [left-tokens tokens
         statements []]
    (let [current-token (first left-tokens)]
      (cond
        (= (:type current-token) :EOF) statements
        (= (:type current-token) :LET) ()
        :else (recur (rest left-tokens) statements)))))

           ;; (defn parse-let-statment [tokens-ahead]
           ;;   (let [next-token (second tokens-ahead)]
           ;;     (cond (= :IDENT (:type next-token))
           ;;           (let [[right left-tokens] (loop
           ;;                                      [itera])]
           ;;             {:ast_t :LET_STATMENT :ident next-token  :value right})
           ;;           :else nil)))
