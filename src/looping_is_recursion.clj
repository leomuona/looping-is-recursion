(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [loop-seq a-seq
         n 0]
    (cond
     (empty? loop-seq) nil
     (pred (first loop-seq)) n
     :else (recur (rest loop-seq) (inc n)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [loop-seq a-seq
           acc 0]
      (if (empty? loop-seq)
        (/ acc (count a-seq))
        (recur (rest loop-seq) (+ acc (first loop-seq)))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [loop-seq a-seq
         res-set #{}]
    (if (empty? loop-seq)
      res-set
      (recur (rest loop-seq) (toggle res-set (first loop-seq))))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [counter 1
           f-n 1
           f-n-1 0]
      (if (== counter n)
        f-n
        (recur (inc counter) (+ f-n f-n-1) f-n)))))

(defn cut-at-repetition [a-seq]
  (loop [loop-seq a-seq
         res-seq []]
    (if (or (empty? loop-seq) (some #(= (first loop-seq) %) res-seq))
      res-seq
      (recur (rest loop-seq) (conj res-seq (first loop-seq))))))

; ^____^
