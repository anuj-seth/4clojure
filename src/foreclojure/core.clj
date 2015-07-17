(ns foreclojure.core
  (require [clojure.test :refer :all]))

(defn my-last [s]
  (if (nil? (next s))
    (first s)
    (recur (next s))))

(defn my-2nd-last [s]
  (if (nil? (nnext s))
    (first s)
    (recur (next s))))

(defn my-nth [coll index]
  (if (= 0 index)
    (first coll)
    (recur (rest coll) (dec index))))

(defn my-inner-cnt [coll cnt]
  (if (nil? (seq coll))
    cnt
    (recur (rest coll) (inc cnt))))
(defn my-cnt [coll]
  (my-inner-cnt coll 0))


(defn my-reverse
  ([coll]
     (my-reverse coll '()))
  ([coll ret-coll]
     (if (nil? (seq coll))
       ret-coll
       (let [new-coll (conj ret-coll (first coll))]
         (my-reverse (rest coll) new-coll)))))
(fn fib
  ([nums]
     (fib '(1 1) (- nums 2)))
  ([fib-list nums]
     (if (= nums 0)
       (reverse fib-list)
       (recur (conj fib-list (+ (first fib-list) (nth fib-list 1)))
            (dec nums)))))


(fn fltn
  ([coll]
     (fltn '() coll))
  ([out-seq coll]
     (println out-seq)
     (if-not (coll? coll)
       (cons coll out-seq)
       (let [in-seq (seq coll)]
         (if-not (seq in-seq)
           out-seq
           (reverse (fltn (reverse (fltn out-seq (first in-seq))) (rest in-seq))))))))
         
  
(fn fltn-2 [coll]
  (if-not (coll? coll)
    coll
    (if (seq coll)
      (cons (fltn-2 (first coll)) (fltn-2 (rest coll))))))

((comp reverse
       (fn fltn
         ([coll]
            (fltn '() coll))
         ([out-seq coll]
            (if-not (coll? coll)
              (cons coll out-seq)
              (let [in-seq (seq coll)]
                (if-not (seq in-seq)
                  out-seq
                  (fltn (fltn out-seq (first in-seq)) (rest in-seq)))))))) '((1 2) 3 [4 [5 6]]))

((comp str #(filter (set (map char (range (int \A) (inc (int \Z))))) %1)) "HeLlO, WoRlD!")

(fn rng [start end]
  (if (< start end)
    (cons start (rng (inc start) end))))

(apply concat ((fn [sep coll] (for [elt coll] [elt sep])) 0 [1 2 3]))

((comp #(drop 1 %) (fn [sep coll] (mapcat (fn [elt] [sep elt]) coll))) 0 [1 2 3])


(= ((fn longest-sub-seq [in-coll]
      (let [inc-sub-seq (filter #(not (nil? %)) (apply concat (for [coll (take-while seq (iterate #(drop 1 %) in-coll))]
                                                                (for [c (take-while seq (iterate drop-last coll))]
                                                                  (if (and (> (count c) 1) (apply < c))
                                                                    c)))))]
        (if (seq inc-sub-seq)
          (reduce #(if (> (count %2) (count %1)) %2 %1) inc-sub-seq)
          inc-sub-seq))) [7 6 5 4]) [0 1 2 3])

((fn part
   ([n coll]
      (part n coll '()))
   ([n coll accum]
      (if-not (< (count coll) n)
        (part n (drop n coll) (cons (take n coll) accum))
        (reverse accum)))) 3 (range 9))

(reduce (fn [acc elt] (let [elt-cnt (get acc elt 0)]
                       (assoc acc elt (inc elt-cnt)))) {} [1 1 2 3 2 1 1])

(fn dxtnt [coll]
  (let [final-accum (reduce (fn [acc elt]
                               (if ((acc :seen) elt)
                                 acc
                                 {:seen (conj (acc :seen) elt) :out (conj (acc :out) elt)}))
                             {:seen #{} :out []}
                             coll)]
    (final-accum :out)))

((fn foo [x]
  (when (> x 0)
    (conj (foo (dec x)) x))) 5)


(((fn [& fn-list] (partial
                  (fn my-comp [funcs & args]
                    (println args)
                    (if (seq funcs)
                      (my-comp (drop 1 funcs) (apply (first funcs) args))
                      (first args))) (reverse fn-list)))
  rest reverse) [1 2 3 4])

(((fn [& fn-list]  (fn [& args] (reduce #(conj %1 (apply %2 args)) [] fn-list)))
   + max min)  2 3 5 1 6 4)

(fn my-iter [fun init]
    (cons init (lazy-seq (my-iter fun (fun init)))))

((fn my-group [fun coll]
   (reduce (fn [acc elt]
             (let [k (fun elt)
                   prev-val (get acc k [])]
               (assoc acc k (conj prev-val elt)))) {} coll)) #(> % 5) [1 3 6 8])

(map (fn my-type [arg]
    (let [k (gensym)
          v (gensym)
          test-elt [k v]
          modif (conj arg test-elt test-elt)
          modif-seq (seq modif)]
      (if (= (count (filter #(= test-elt %) modif-seq)) 1)
        (if (= (modif k) v) :map :set)
        (let [m-elt (gensym)
              nd-modif (conj modif m-elt)]
          (cond
           (= (first nd-modif) m-elt) :list
           (= (last nd-modif) m-elt) :vector))))) [{} #{} [] ()])


(fn [n] (take n ((fn primes [start]
                  (let [divisors (range 2 (Math/floor (Math/sqrt start)))]
                    (if (some #(zero? (rem start %)) divisors)
                      (lazy-seq (primes (inc start)))
                      (cons start (lazy-seq (primes (inc start))))))) 1)))

(fn [& args]
  (flatten (map seq args)))

(= 3
  (let [[fun coll] [+ (range 3)]] (apply fun coll))
  (let [[[fun coll] b] [[+ 1] 2]] (fun coll b))
  (let [[fun coll] [inc 2]] (fun coll)))

( (fn gcd [a b]
    (let [lower (min a b)]
      (some #(if (= 0 (rem a %) (rem b %))
               %
               false) (range lower 0 -1)))) 2 4)

( (fn eulers-totient [num]
    (if (= num 1)
      1
      (let [nums-below (range 0 num)
            gcd (fn gcd [a b]
                  (let [lower (min a b)]
                    (some #(if (= 0 (rem a %) (rem b %))
                             %
                             false) (range lower 0 -1))))
            totient (for [t nums-below]
                      (if (= 1 (gcd num t)) t nil))]
        (count (filter identity totient))))) 10)

( (fn anagram [coll]
    (let [anagram-map (group-by sort coll)
          anagram-set (set (map set (filter #(> (count %) 1) (vals anagram-map))))]
      anagram-set))
  ["meat" "mat" "team" "mate" "eat"])

(letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
  ((fn tramp
     ([a b] (tramp (a b)))
     ([a] 
        (if (fn? a)
          (tramp (a))
          a))) triple 2))

;; our tree node will have the following structure
;; Each node will be a vector
;; tree-node => [node-value {:left-adj tree-node :right-adj tree-node}]
;; the function takes a seq of vectors where each vector gives the
;; values in that level of the tree
( (fn [triangle]
    (letfn [(make-tree [lov indx]
              (if (seq lov)
                [(nth (first lov) indx) {:left-adj (make-tree (rest lov) indx) :right-adj (make-tree (rest lov) (inc indx))}]
                nil))
            (visit-nodes [tree acc]
              (if (apply :left-adj (rest tree))
                [(visit-nodes (apply :left-adj (rest tree)) (conj acc (first tree)))
                 (visit-nodes (apply :right-adj (rest tree)) (conj acc (first tree)))]
                (conj  acc (first tree))))
            (almost-flatten [x]
              (filter #(and (sequential? %) (not-any? sequential? %))
                      (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))]
      (apply min  (map #(apply + %) (almost-flatten (visit-nodes  (make-tree triangle 0) [])))))) '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))

(fn [rows]
  (apply min (flatten (reduce
                       (fn [t s]
                         (map-indexed
                          (fn [i v]
                            (mapcat (fn [vs] (map #(+ % v) vs)) (take (min (inc i) 2) (drop (max (dec i) 0) t))))
                          s))
                       [[0]] rows))))

( (fn [n]
    (let [divisors (filter #(zero? (rem n %)) (range 1 n))]
      (= (apply + divisors) n))) 6)

( #(set (filter  %1 %2)) #{:a :b :c :d} #{:c :e :a :f :d}  )

(letfn [(permute [words]
          (if (seq words)
            (for [w words]
              [w (permute (clojure.set/difference words (set [w])))])
            nil))]
  (let [in-words #{"cot" "hot" "bat" "fat"}]
     (permute in-words)))

(let [l #{:a :b}]
  (for [elt l]
    [elt (for [e2 (clojure.set/difference l (set [elt]))] e2)]))


( (fn permutations [xs]
    (if-not (seq xs)
      (list ())
      (for [x xs
            ys (permutations (for [z xs :when (not= z x)] z))]
        (conj ys x)))) ["to" (map char (range 97 123))])

(let [xs [1 2 3]] (for [x xs
                        ys (for [z xs :when (not= z x)] z)] [ ys x]))

(time (map (fn word-chain? [coll] 
             (letfn [(permutations [xs]
                       (if-not (seq xs)
                         (list ())
                         (for [x xs
                               ys (permutations (for [z xs :when (not= z x)] z))]
                           (conj ys x))))]
               (let [word-chains (permutations coll)
                     res (some identity 
                               (map (fn [w-chain]
                                      (every? identity 
                                              (map (fn [w1 w2]
                                                     (let [cnt-w1 (count w1)
                                                           cnt-w2 (count w2)]
                                                       (condp = (- cnt-w1 cnt-w2)
                                                         ;; check for addition of character
                                                         -1 (let [extra-char (some identity (map-indexed (fn [indx itm] (if (> indx (dec (count w1))) itm (if (= itm (nth w1 indx)) nil itm))) w2))
                                                                  s w1
                                                                  r  extra-char
                                                                  indx (range 0 (inc (count s)))]
                                                              (some #(= w2 %) (map (fn [i]
                                                                                     (apply str  (concat (take i s) [r] (drop i s)))) indx)))
                                                         ;; check for deletion of character
                                                         1 (some #(= w2 %) (map (fn [i] (apply str (concat (take (dec  i) w1) (drop  i w1)))) (range 1 (inc (count w1)))))
                                                         ;; check for substitution of character
                                                         0 (let [sub-char (clojure.set/difference (set w2) (set w1))]
                                                             (if (= 1 (count sub-char))
                                                               (let [s w1
                                                                     r  sub-char
                                                                     indx (range 1 (inc (count s)))]
                                                                 (some #(= w2 %) (map (fn [i]
                                                                                        (apply str  (concat (take (dec i) s) r (drop i s))))  indx)))
                                                               false))
                                                         ;; default value
                                                         false))) 
                                                   w-chain (rest w-chain)))) word-chains))]
                 (if (nil? res) false true)))) [#{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}
                                                #{"cot" "hot" "bat" "fat"}
                                                #{"to" "top" "stop" "tops" "toss"}
                                                #{"spout" "do" "pot" "pout" "spot" "dot"}
                                                #{"share" "hares" "shares" "hare" "are"}
                                                #{"share" "hares" "hare" "are"}
                                                #{"cat" "cot" "coat" "oat" "hat" "hot" "hog" "dog"}]))



( (fn [w1 w2]
    (letfn [
            (dist [d w1 w2]
              (cond
               (empty? w1) (count w2)
               (empty? w2) (count w1)
               :else
               (min
                (inc (d d (rest w1) w2))
                (inc (d d w1 (rest w2)))
                (+ (if (= (first w1) (first w2)) 0 1)
                   (d d (rest w1) (rest w2))))))]
      (dist (memoize dist) w1 w2))) "share" "shares")

(map  (let [tourable? (fn [g]
                        (<= (* 2 (count g))
                            (reduce +
                                    (map #(count (val %)) g))))
            dist (fn [w1 w2]
                   (letfn [(dist [d w1 w2]
                             (cond
                              (empty? w1) (count w2)
                              (empty? w2) (count w1)
                              :else
                              (min
                               (inc (d d (rest w1) w2))
                               (inc (d d w1 (rest w2)))
                               (+ (if (= (first w1) (first w2)) 0 1)
                                  (d d (rest w1) (rest w2))))))]
                     (dist (memoize dist) w1 w2)))]
        (fn [words]
          (tourable?
           (reduce (fn [m w] (assoc m w (filter #(= 1 (dist w %)) words))) {} words)))) [#{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}
                                                #{"cot" "hot" "bat" "fat"}
                                                #{"to" "top" "stop" "tops" "toss"}
                                                #{"spout" "do" "pot" "pout" "spot" "dot"}
                                                #{"share" "hares" "shares" "hare" "are"}
                                                #{"share" "hares" "hare" "are"}
                                                #{"cat" "cot" "coat" "oat" "hat" "hot" "hog" "dog"}])


(map  (let [dist (fn [w1 w2]
                   (letfn [(dist [d w1 w2]
                             (cond
                              (empty? w1) (count w2)
                              (empty? w2) (count w1)
                              :else
                              (min
                               (inc (d d (rest w1) w2))
                               (inc (d d w1 (rest w2)))
                               (+ (if (= (first w1) (first w2)) 0 1)
                                  (d d (rest w1) (rest w2))))))]
                     (dist (memoize dist) w1 w2)))]
        (fn [words]
          (reduce (fn [m w] (assoc m w (filter #(= 1 (dist w %)) words))) {} words))) [#{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}])


( (fn [& args] (= 2 (count (distinct args)))) false false)

(let [ws  ["share" "shares" "hares"  "hare" "are"]]
  (map  (fn [w1 w2]
          (let [cnt-w1 (count w1)
                cnt-w2 (count w2)]
            (condp = (- cnt-w1 cnt-w2)
              ;; check for addition of character
              -1 (let [extra-char (some identity (map-indexed (fn [indx itm] (if (> indx (dec (count w1))) itm (if (= itm (nth w1 indx)) nil itm))) w2))
                       s w1
                       r  extra-char
                       indx (range 0 (inc (count s)))]
                   (some #(= w2 %) (for [i  indx]
                                     (apply str  (concat (take i s) [r] (drop i s))))))
              ;; check for deletion of character
              1 (some #(= w2 %) (for [i (range 1 (inc (count w1)))] (apply str (concat (take (dec  i) w1) (drop  i w1)))))
              ;; check for substitution of character
              0 (let [sub-char (clojure.set/difference (set w2) (set w1))]
                  (if (= 1 (count sub-char))
                    (let [s w1
                          r  sub-char
                          indx (range 1 (inc (count s)))]
                      (some #(= w2 %) (for [i  indx]
                                        (apply str  (concat (take (dec i) s) r (drop i s))))))
                    false))
              ;; default value
              false))) ws (rest ws)))

(let [w1 "cot"
      w2 "cat"
      sub-char (clojure.set/difference (set w2) (set w1))]
  (println sub-char)
  (if (= 1 (count sub-char))
    (let [s w1
          r  sub-char
          indx (range 1 (inc (count s)))]
      (some #(= w2 %) (for [i  indx]
                        (apply str  (concat (take i s) r (drop i s))))))
    false))

(let [s "to"
      r (clojure.set/difference (set "top") (set "to"))
      indx (range 0 (inc (count s)))]
  (println r)
  (for [i  indx]
    (apply str  (concat (take i s) r (drop i s)))))

(map #(let [cnt-1 (count %1)
            cnt-2 (count %2)
            d (- cnt-1 cnt-2)
            abs-diff (max d (- d))] 
        (if (or (= 1 abs-diff) (zero? abs-diff))
          (if (> cnt-1 cnt-2) 
            (clojure.set/difference (set  %1) (set %2))
            (clojure.set/difference (set %2) (set %1)))))  [ "share" "shares" "hares"  "hare" "are"] ["shares" "hares"  "hare" "are"] 
            
)

;; (let [directory (clojure.java.io/file "c:/temp")
;;       dir? #(.isDirectory %)]
;;   ;;we want only files, therefore filter items that are not directories.
;;   (filter (comp not dir?) 
;;           (tree-seq dir? #(.listFiles %) directory)))


