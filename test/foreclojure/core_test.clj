(ns foreclojure.core-test
  (:require [clojure.test :refer :all]))

(deftest problem-84-transitive-closure 
  (let [fun  (fn transitive [relations]
               (let [to-add (filter identity (for [[i j] relations
                                                   [k l] relations :when (= j k)]
                                               [i l]))
                     rslt (clojure.set/union relations (set to-add))]
                 (if (= rslt relations)
                   rslt
                   (recur rslt))))]
    (is (= #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]} (fun #{[8 4] [9 3] [4 2] [27 9]})))
    (is (= #{["cat" "man"] ["cat" "snake"] ["man" "snake"] ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}
           (fun #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})))
    (is (= #{["father" "son"] ["father" "grandson"] ["uncle" "cousin"] ["son" "grandson"]}
           (fun #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]})))))

(deftest problem-85-power-set
  (let [power-set (fn [s]
                    (let [set-to-seq (seq s)
                          power-set-size (Math/pow 2 (count s))
                          eligibility (map #(Integer/toBinaryString %) (range power-set-size))]
                      (set (for [e eligibility]
                             (set (map first (filter (fn [[a b]] (= b \1)) (partition 2 (interleave set-to-seq (reverse e))))))))))]
    (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
    (is (= (power-set #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
    (is (= (count (power-set (into #{} (range 10)))) 1024))))

(deftest problem-86-happy-numbers
  (let [sum-of-squares (fn fun [n]
                         (let [digits (map second 
                                           (take-while #(not= [0 0] %) 
                                                       (iterate (fn [[a b]] [(quot a 10) (rem a 10)]) 
                                                                [(quot n 10) (rem n 10)])))
                               new-num (int (apply + (map #(Math/pow % 2) digits)))]
                           (cons new-num (lazy-seq (fun new-num)))))
        happy-number? (fn happy-number? [n] 
                        (if (= 1 (first (drop-while #(not (#{1 4} %)) (sum-of-squares n))))
                          true
                          false))]
    (is (= (happy-number? 7) true))
    (is (= (happy-number? 986543210) true))
    (is (= (happy-number? 2) false))
    (is (= (happy-number? 3) false))))

(deftest problem-89-graph-tour
  (let [graph-tour (fn graph-tour
                     ([g] (graph-tour g (first (first g))))
                     ([g next-vertex]
                      (if (nil? (seq g))
                        true
                        (let [next-nodes (filter #(or (= next-vertex (first %))
                                                      (= next-vertex (second %))) g)]
                          (if (some true? (map (fn [next-node] 
                                                 (graph-tour (remove #(= next-node %) g) (first (remove #(= next-vertex %) next-node)))) 
                                               next-nodes))
                            true
                            false)))))]
    (is (= true (graph-tour [[:a :b]])))
    (is (= false (graph-tour [[:a :a] [:b :b]])))
    (is (= true (graph-tour [[1 2] [2 3] [3 4] [4 1]])))
    (is (= true (graph-tour [[:a :b] [:a :c] [:c :b] [:a :e]
                             [:b :e] [:a :d] [:b :d] [:c :e]
                             [:d :e] [:c :f] [:d :f]])))
    (is (= false (graph-tour [[1 2] [2 3] [2 4] [2 5]])))))

(deftest problem-90-cartesian-product
  (let [cartesian-product (fn [s1 s2]
                            (into #{} (for [e1 s1 e2 s2]
                                        [e1 e2])))]
    (is  (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
            #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
              ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
              ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
    (is  (= (cartesian-product #{1 2 3} #{4 5})
            #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    (is (= 300 (count (cartesian-product (into #{} (range 10))
                                         (into #{} (range 30))))))))

(deftest problem-93-partially-flatten-a-sequence
  (let [fun (fn partially-flatten [l]
              (filter #(and (sequential? %) (not (sequential? (first %)))) 
                      (tree-seq sequential? seq l)))]
    (is  (= (fun [["Do"] ["Nothing"]])
            [["Do"] ["Nothing"]]))
    (is (= (fun [[[[:a :b]]] [[:c :d]] [:e :f]])
           [[:a :b] [:c :d] [:e :f]]))
    (is (= (fun '((1 2)((3 4)((((5 6)))))))
           '((1 2)(3 4)(5 6))))))

(deftest problem-95-to-tree-or-not-to-tree
  (let [binary-tree? (fn binary-tree? [t]
                       (cond
                        (and (nil? t) (not (coll? t))) true
                        (and (not (nil? t)) (not (coll? t)))  false
                        :else (if (= 3 (count t))
                                (and  (binary-tree? (nth t 1)) (binary-tree? (nth t 2)))
                                false)))]
    (is (= (binary-tree? '(:a (:b nil nil) nil))
            true))
    (is (= (binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
           true))
    (is (= (binary-tree? '(:a (:b nil nil)))
           false))
    (is (= (binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
           false))
    (is (= (binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
           true))
    (is (= (binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
           false))
    (is (= (binary-tree? '(:a nil ()))
           false))))

(deftest problem-134-nil-key
  (let [fun (fn nil-key [k m]
              (= nil (get m k :not-found)))]
    (is  (true?  (fun :a {:a nil :b 2})))
    (is (false? (fun :b {:a nil :b 2})))
    (is (false? (fun :c {:a nil :b 2})))))

(deftest problem-114-global-take-while
  (let [fun (fn fun [n pred s]
              (cond 
               (= 0 n) ()
               (and (pred (first s)) (not= 0 (dec n))) (cons (first s) (lazy-seq (fun (dec n) pred (rest s))))
               (and (pred (first s)) (zero? (dec n))) (lazy-seq (fun (dec n) pred (rest s)))
               :else (cons (first s) (lazy-seq (fun n pred (rest s))))))]
    (is (= [2 3 5 7 11 13] (fun 4 
                                #(= 2 (mod % 3))
                                [2 3 5 7 11 13 17 19 23])))
    (is  (= ["this" "is" "a" "sentence"]
            (fun 3 #(some #{\i} %)
                ["this" "is" "a" "sentence" "i" "wrote"])))
    (is  (= ["this" "is"]
            (fun 1 #{"a"}
                ["this" "is" "a" "sentence" "i" "wrote"])))))

(deftest problem-146-trees-into-tables
  (let [tree->table (fn tree->table [tr]
                      (into {} (for [[k v] tr [k2 v2] v]
                                 [[k k2] v2])))]
    (is (= (tree->table '{a {p 1, q 2}
                          b {m 3, n 4}})
           '{[a p] 1, [a q] 2
             [b m] 3, [b n] 4}))
    (is (= (tree->table '{[1] {a b c d}
                          [2] {q r s t u v w x}})
           '{[[1] a] b, [[1] c] d,
             [[2] q] r, [[2] s] t,
             [[2] u] v, [[2] w] x}))
    (is (= (tree->table '{m {1 [a b c] 3 nil}})
           '{[m 1] [a b c], [m 3] nil}))))


;; Below this line lie problems not yet solved.
;;

(deftest problem-111-crossword-puzzle
  (let [legal? (fn legal? [word board]
                 true)]
    (is (= true  (legal? "the" ["_ # _ _ e"])))
    (is (= false (legal? "the" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))
    (is (= true  (legal? "joy" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))
    (is (= false (legal? "joy" ["c o n j"
                                "_ _ y _"
                                "r _ _ #"])))
    (is (= true  (legal? "clojure" ["_ _ _ # j o y"
                                    "_ _ o _ _ _ _"
                                    "_ _ f _ # _ _"])))
    ))

(deftest problem-91-graph-connectivity
  (let [graph-connectivity (fn graph-connectivity [g]
                             g)]
    (= true (graph-connectivity #{[:a :a]}))
    (is (= true (graph-connectivity #{[:a :b]})))
    (is (= false (graph-connectivity #{[1 2] [2 3] [3 1]
                       [4 5] [5 6] [6 4]})))
    (is (= true (graph-connectivity #{[1 2] [2 3] [3 1]
                      [4 5] [5 6] [6 4] [3 4]})))
    (is (= false (graph-connectivity #{[:a :b] [:b :c] [:c :d]
                       [:x :y] [:d :a] [:b :e]})))
    (is (= true (graph-connectivity #{[:a :b] [:b :c] [:c :d]
                      [:x :y] [:d :a] [:b :e] [:x :a]})))
    )

  )
                               





(def world [[  1   1   1   1    1]
            [999 999 999 999    1]
            [  1   1   1   1    1]
            [  1 999 999 999  999]
            [  1   1   1   1    1]])

(defn neighbours
  ([size yx]
   (neighbours [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx)) 
           (map #(vec (map + yx %)) deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est (- 
                    (+ size size) 
                    y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost 
     (or (:cost cheapest-nbr) 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (first (sort-by f coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])
(total-cost 0 900 5 0 0)
(neighbours 5 [0 0])

(let [digits (fn [n] 
               (reverse (map second 
                             (take-while #(not= [0 0] %) 
                                         (iterate (fn [[a b]] [(quot a 10) (rem a 10)]) 
                                                  [(quot n 10) (rem n 10)])))))]
  (->>  (partition 13 1 (digits 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450))
        ;; (map (fn [s] [s (map #(Character/getNumericValue %) s)]))
        (map (fn [s] [s (apply * s)]))
        (sort-by second >)
        (take 2)))

(let [digits (fn [n] 
               (reverse (map second 
                             (take-while #(not= [0 0] %) 
                                         (iterate (fn [[a b]] [(quot a 10) (rem a 10)]) 
                                                  [(quot n 10) (rem n 10)])))))]
  (->>  (partition 13 1 (digits 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450))
        count))

(->>  (partition 13 1 "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
      (map (fn [s] [s (map #(Character/getNumericValue %) s)]))
      (map (fn [[s n]] [s (apply * n)]))
      (sort-by second >)
      (take 2))
