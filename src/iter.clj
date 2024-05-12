(ns iter)

(defprotocol Reader (read-char [reader]
                      "Returns the next char from the Reader, nil if the end of stream has been reached")
             (peek-char [reader]
               "Returns the next char from the Reader without removing it from the reader stream")
             (get-coll [reader]
               "Returns the next char from the Reader without removing it from the reader stream")
             (read-n-char [reader n]
               "Returns the next char from the Reader without removing it from the reader stream"))

(deftype Iter
         [coll ^long s-len ^:unsynchronized-mutable ^long s-pos]
  Reader

  (read-char [reader]
    (when (> s-len s-pos)
      (let [r (nth coll s-pos)]
        (set! (. reader s-pos) (inc s-pos))
        r)))

  (read-n-char [reader n]
    (loop [result []]
      (cond
        (nil? (.peek-char reader)) result
        (= (count result) n) result
        :else (recur (conj result (.read-char reader))))))

  (peek-char [reader]
    (when (> s-len s-pos)
      (nth coll s-pos)))


  (get-coll [reader]
    (subvec coll s-pos)))

(defn to-iter [seq] (->Iter seq (count seq) 0))
