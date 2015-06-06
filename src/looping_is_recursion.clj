(ns looping-is-recursion)

(defn power [base exp]
  (let [ph (fn [acc e]
             (if (zero? e)
               acc
               (recur (* acc base) (dec e))))]
    (ph 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (not= (first seq1) (first seq2))
            (empty? seq1)
            (empty? seq2)) false
        :true (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond (empty? s) nil
          (pred (first s)) i
          :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         len 0
         seq a-seq]
    (if (empty? seq)
      (/ acc len)
      (recur (+ acc (first seq)) (inc len) (rest seq)))))

(defn parity [a-seq]
  (set (map #(first %) (filter #(odd? (second %)) (frequencies a-seq)))))

(defn fast-fibo [n]
  (loop [x 0
         y 1
         i n]
    (cond (zero? i) 0
          (= i 1) 1
          (= i 2) (+ x y)
          :else (recur y (+ x y) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         s a-seq
         r []]
    (if (or (empty? s) (contains? seen (first s)))
      r
      (recur (conj seen (first s)) (rest s) (conj r (first s))))))

