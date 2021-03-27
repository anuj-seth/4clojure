(ns foreclojure.core-test
  (:require [clojure.test :refer :all]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

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
                                                 (graph-tour (remove #(= next-node %) g) 
                                                             (first (remove #(= next-vertex %) 
                                                                            next-node)))) 
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

(deftest problem-92-read-roman-numerals                               
  (let [roman->decimal (fn roman->decimal [roman]
                         (let [Symbol->Value {\I 1, 
                                              \V 5, 
                                              \X 10, 
                                              \L 50, 
                                              \C 100, 
                                              \D 500, 
                                              \M 1000}]
                           (reduce (fn [acc [a b]] 
                                     (if (< a b) 
                                       (- acc a)
                                       (+ acc a))) 
                                   0
                                   (partition 2 1 [0] (map Symbol->Value roman)))))]
    (is (= 14 (roman->decimal "XIV")))
    (is (= 827 (roman->decimal "DCCCXXVII")))
    (is (= 3999 (roman->decimal "MMMCMXCIX")))
    (is (= 48 (roman->decimal "XLVIII")))))

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

(deftest problem-96-beauty-is-symmetry
  (let [mirror-image? (fn [t]
                        (let [rotate (fn rotate [t]
                                       (if (nil? t)
                                         nil
                                         (cons (first t) (cons (rotate (nth t 2)) (cons (rotate (nth t 1)) ())))))]
                          (= (nth t 1) (rotate (nth t 2)))))]
    (is (= (mirror-image? '(:a (:b nil nil) (:b nil nil))) true))
    (is (= (mirror-image? '(:a (:b nil nil) nil)) false))
    (is (= (mirror-image? '(:a (:b nil nil) (:c nil nil))) false))
    (is (= (mirror-image? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
           true))
    (is (= (mirror-image? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                           [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
           false))
    (is (= (mirror-image? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                           [2 [3 nil [4 [6 nil nil] nil]] nil]])
           false))))

(deftest problem-97-pascal-triangle
  (let [pascal-nth (fn [n]
                     (let [pascal-triangle (fn pascal-triangle [last-row]
                                             (lazy-seq
                                              (let [next-row (concat [1] 
                                                                     (map (fn [[a b]] (+ a b)) 
                                                                          (partition 2 1 last-row)) 
                                                                     [1])]
                                                (cons next-row (pascal-triangle next-row)))))]
                       (nth (cons [1] (pascal-triangle [1])) (dec n))))]
    (is  (= (pascal-nth 1) [1]))
    (is (= (map pascal-nth (range 1 6))
           [     [1]
                 [1 1]
                 [1 2 1]
                 [1 3 3 1]
                 [1 4 6 4 1]]))
    (is (= (pascal-nth 11)
           [1 10 45 120 210 252 210 120 45 10 1]))))

(deftest problem-98-equivalence-classes
  (let [equivalence-class (fn [f D]
                            (into #{} (vals (apply merge-with clojure.set/union (for [a D b D]
                                                                                  (if (= (f a) (f b))
                                                                                    {a #{b}}))))))]
    (is  (= (equivalence-class #(* % %) #{-2 -1 0 1 2})
            #{#{0} #{1 -1} #{2 -2}}))
    (is (= (equivalence-class #(rem % 3) #{0 1 2 3 4 5 })
           #{#{0 3} #{1 4} #{2 5}}))
    (is (= (equivalence-class identity #{0 1 2 3 4})
           #{#{0} #{1} #{2} #{3} #{4}}))
    (is (= (equivalence-class (constantly true) #{0 1 2 3 4})
           #{#{0 1 2 3 4}}))))

(deftest problem-99-product-digits
  (let [p-digits (fn [a b]
                   (vec (map (comp read-string str)  (str (* a b)))))]
    (= (p-digits 1 1) [1])
    (= (p-digits 99 9) [8 9 1])
    (= (p-digits 999 99) [9 8 9 0 1])))

(deftest problem-100-least-common-multiple
  (let [lcm (fn lcm [& args]
              (let [multiplier (apply min args)]
                (first (first (filter #(every? zero? (second %)) 
                                      (map (fn [numerator] 
                                             [numerator (map #(rem numerator %) args)]) 
                                           (iterate #(+ multiplier %) multiplier)))))))]
    (is (== (lcm 2 3) 6))
    (is (== (lcm 5 3 7) 105))
    (is (== (lcm 1/3 2/5) 2))
    (is (== (lcm 3/4 1/6) 3/2))
    (is (== (lcm 7 5/7 2 3/5) 210))))

(deftest problem-102-into-camel-case
  (let [camel-case (fn [s]
                     (let [splits (clojure.string/split s #"-")
                           camel-cased (cons (first splits) (map clojure.string/capitalize (rest splits)))]
                       (clojure.string/join camel-cased)))]
    (is (= (camel-case "something") "something"))
    (is (= (camel-case "multi-word-key") "multiWordKey"))
    (is (= (camel-case "leaveMeAlone") "leaveMeAlone"))))

(deftest problem-103-generating-k-combinations 
  (let [create-combinations (fn [k s]
                              (let [digits (fn [num base] 
                                             ;; this function will only work for bases from 2 to 10
                                             ;; please don't use base 1 :)
                                             (if (zero? num)
                                               '(0)
                                               (reverse (map second 
                                                             (take-while #(not= [0 0] %) 
                                                                         (iterate (fn [[a _]] 
                                                                                    [(quot a base) (rem a base)]) 
                                                                                  [(quot num base) (rem num base)]))))))
                                    n (count s)
                                    binary-nums (map #(digits % 2) (range (Math/pow 2 n)))
                                    pad-seq (repeat n 0)
                                    padded-binary-nums (map #(take-last n (concat pad-seq %)) binary-nums )
                                    k-ones (filter #(= k (apply + %)) padded-binary-nums)
                                    combinations (map (fn [ones-zeros] 
                                                        (remove nil? (map (fn [one-or-zero item]
                                                                            (if (zero? one-or-zero) nil item))
                                                                          ones-zeros
                                                                          (seq s)))) 
                                                      k-ones)]
                                (into #{} (map set combinations))))]
    (is (= (create-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
    (is (= (create-combinations 10 #{4 5 6}) #{}))
    (is (= (create-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
    (is (= (create-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                                  #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
    (is (= (create-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
    (is (= (create-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                               #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))))

(deftest problem-104-write-roman-numerals
  (let [roman (fn [decimal]
                (let [Value->Symbol (into {} (vec
                                              (map-indexed  
                                               (fn [idx itm] 
                                                 [(str idx) (zipmap [3 2 1 0] itm)])
                                               [["" "" "" ""] 	 	 	 
                                                ["M" "C" "X" "I"]
                                                ["MM" "CC" "XX" "II"]
                                                ["MMM" "CCC" "XXX" "III"]
                                                ["MMMM"	"CD" "XL" "IV"]
                                                ["MMMMM" "D" "L" "V"]
                                                ["MMMMMM" "DC" "LX" "VI"]
                                                ["MMMMMMM" "DCC" "LXX" "VII"]
                                                ["MMMMMMMM" "DCCC" "LXXX" "VIII"]
                                                ["MMMMMMMMM" "CM" "XC" "IX"]])))
                      str-decimal (str decimal)
                      roman (map (fn [digit place] 
                                   ((Value->Symbol (str digit)) place)) 
                                 str-decimal 
                                 (range (dec (count str-decimal)) -1 -1))]
                  (clojure.string/join roman)))]
    (is (= "I" (roman 1)))
    (is (= "XXX" (roman 30)))
    (is (= "IV" (roman 4)))
    (is (= "CXL" (roman 140)))
    (is (= "DCCCXXVII" (roman 827)))
    (is (= "MMMCMXCIX" (roman 3999)))
    (is (= "XLVIII" (roman 48)))))

(deftest problem-105-identify-keys-and-values
  (let [merge-map (fn merge-map 
                    ([args] (merge-map [] args))
                    ([acc args]
                     (if (nil? (seq args))
                       (into {} acc)
                       (let [new-acc (conj acc [(first args) (vec (take-while number? (rest args)))])] 
                         (recur new-acc (drop-while number? (rest args)))))))]
    (is (= {} (merge-map [])))
    (is (= {:a [1]} (merge-map [:a 1])))
    (is (= {:a [1], :b [2]} (merge-map [:a 1, :b 2])))
    (is (= {:a [1 2 3], :b [], :c [4]} (merge-map [:a 1 2 3 :b :c 4])))))

(deftest problem-108-lazy-searching
  ;; In Paris
  (let [lz-min (fn lz-min [driver & sorted-lists]
                 (let [num-of-lists (inc (count sorted-lists))]
                   (first (first (drop-while #(not= num-of-lists (count %)) 
                                             (for [x driver]
                                               (flatten (cons x (for [other-list sorted-lists]
                                                                  (filter #(= x %) (take-while #(<= % x) other-list)))))))))))]
    (is (= 3 (lz-min [3 4 5])))
    (is (= 4 (lz-min [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
    (is (= 7 (lz-min (range) (range 0 100 7/6) [2 3 5 7 11 13])))
    (is (= 64 (lz-min 
               ;; perfect cubes
               (map #(* % % %) (range)) 
               ;; powers of 2
               (filter #(zero? (bit-and % (dec %))) (range)) 
               ;; at least as large as 20
               (iterate inc 20))))))

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

(deftest problem-115-balance-of-N
  (let [balanced? (fn [n]
                    (let [digits (map second 
                                      (take-while #(not= [0 0] %) 
                                                  (iterate (fn [[a b]] [(quot a 10) (rem a 10)]) 
                                                           [(quot n 10) (rem n 10)])))
                          n-len (count digits)
                          len-parts (quot n-len 2)
                          parts (map #(% len-parts digits) [take take-last])]
                      (apply = (map #(apply + %) parts))))]
    (is (= true (balanced? 11)))
    (is (= true (balanced? 121)))
    (is (= false (balanced? 123)))
    (is (= true (balanced? 0)))
    (is (= false (balanced? 88099)))
    (is (= true (balanced? 89098)))
    (is (= true (balanced? 89089)))
    (is (= (take 20 (filter balanced? (range)))
           [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))))

(deftest problem-118-reimplement-map
  (let [re-map (fn re-map [f s]
                 (if (seq s)
                   (cons (f (first s)) (lazy-seq
                                        (re-map f (rest s))))
                   ()))]
    (is (= [3 4 5 6 7]
           (re-map inc [2 3 4 5 6])))
    (is (= (repeat 10 nil)
           (re-map (fn [_] nil) (range 10))))
    (is (= [1000000 1000001]
           (->> (re-map inc (range))
                (drop (dec 1000000))
                (take 2))))))

(deftest problem-120-sum-of-sqr-digits
  (let [smaller-than-squared-sum (fn [s]
                                   (let [digits (fn [n] (map second 
                                                             (take-while #(not= [0 0] %) 
                                                                         (iterate (fn [[a b]] [(quot a 10) (rem a 10)]) 
                                                                                  [(quot n 10) (rem n 10)]))))]
                                     (reduce (fn [acc elt]
                                               (if (< elt (apply + (map #(* % %) (digits elt))))
                                                 (inc acc)
                                                 acc))
                                             0
                                             s)))]
    (is (= 8 (smaller-than-squared-sum (range 10))))
    (is (= 19 (smaller-than-squared-sum (range 30))))
    (is (= 50 (smaller-than-squared-sum (range 100))))
    (is (= 50 (smaller-than-squared-sum (range 1000))))))

(deftest problem-121-universal-computation-engine
  (let [uce (fn uce [form]
              (let  [ff (fn ff [f env]
                          (let [f-elt (first f)]
                            (cond
                             (nil? f-elt) ()
                             (= '/ f-elt) (apply / (ff (rest f) env))
                             (= '+ f-elt) (apply + (ff (rest f) env))
                             (= '- f-elt) (apply - (ff (rest f) env))
                             (= '* f-elt) (apply * (ff (rest f) env))
                             (number? f-elt) (cons f-elt (ff (rest f) env))
                             (contains? env f-elt) (cons (env f-elt) (ff (rest f) env))
                             :else (cons (ff f-elt env) (ff (rest f) env)))))]
                (partial ff form)))]
    (is (= 2 ((uce '(/ a b))
              '{b 8 a 16})))
    (is (= 8 ((uce '(+ a b 2))
              '{a 2 b 4})))
    (is (= [6 0 -4]
           (map (uce '(* (+ 2 a)
                         (- 10 b)))
                '[{a 1 b 8}
                  {b 5 a -2}
                  {a 2 b 11}])))
    (is (= 1 ((uce '(/ (+ x 2)
                       (* 3 (+ y 1))))
              '{x 4 y 1})))))

(deftest problem-122-read-a-binary-number
  (let [binary->decimal (fn [binary]
                          (let [digits (map (comp read-string str) (seq binary))]
                            (int (apply + (map (fn [a b] (* a (Math/pow 2 b))) (reverse digits) (range))))))]
    (is (= 0     (binary->decimal "0")))
    (is (= 7     (binary->decimal "111")))
    (is (= 8     (binary->decimal "1000")))
    (is (= 9     (binary->decimal "1001")))
    (is (= 255   (binary->decimal "11111111")))
    (is (= 1365  (binary->decimal "10101010101")))
    (is (= 65535 (binary->decimal "1111111111111111")))))

(deftest problem-128-playing-cards
  (let [playing-cards (fn [s]
                        (let [suits {\S :spade \H :heart \D :diamond \C :club}
                              ranks (into {} (map (fn [a b] [a b]) (concat (apply str (range 2 10)) "TJQKA") (range 13)))]
                          {:suit (suits (first s)) :rank (ranks (second s))}))]
    (is (= {:suit :diamond :rank 10} (playing-cards "DQ")))
    (is (= {:suit :heart :rank 3} (playing-cards "H5")))
    (is (= {:suit :club :rank 12} (playing-cards "CA")))
    (is (= (range 13) (map (comp :rank playing-cards str)
                           '[S2 S3 S4 S5 S6 S7
                             S8 S9 ST SJ SQ SK SA])))))

(deftest problem-132-insert-between-two-items
  (let [insert-between (fn insert-between [pred-fn itm start-seq]
                         (let [a (first start-seq)
                               b (second start-seq)]
                           (cond
                            (nil? a) ()
                            (nil? b) (cons a ())
                            (pred-fn a b) (cons a (cons itm (lazy-seq (insert-between pred-fn itm (rest start-seq)))))
                            :else (cons a (lazy-seq (insert-between pred-fn itm (rest start-seq)))))))]
    (is (= '(1 :less 6 :less 7 4 3) (insert-between < :less [1 6 7 4 3])))
    (is (= '(2) (insert-between > :more [2])))
    (is (= [0 1 :x 2 :x 3 :x 4]  (insert-between #(and (pos? %) (< % %2)) :x (range 5))))
    (is (empty? (insert-between > :more ())))
    (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
           (take 12 (->> [0 1]
                         (iterate (fn [[a b]] [b (+ a b)]))
                         (map first)            ; fibonacci numbers
                         (insert-between (fn [a b] ; both even or both odd
                                           (= (mod a 2) (mod b 2)))
                                         :same)))))))

(deftest problem-134-nil-key
  (let [fun (fn nil-key [k m]
              (= nil (get m k :not-found)))]
    (is  (= true  (fun :a {:a nil :b 2})))
    (is (= false (fun :b {:a nil :b 2})))
    (is (= false (fun :c {:a nil :b 2})))))

(deftest problem-135-infix-calculator
  (let [infix-calc (fn [& infix-expr]
                     (if (= 1 (count infix-expr))
                       (first infix-expr)
                       (let [[operand-1 operator operand-2 & rest-expr] infix-expr]
                         (recur (cons (operator operand-1 operand-2) rest-expr)))))]
    (is  (= 7  (infix-calc 2 + 5)))
    (is (= 42 (infix-calc 38 + 48 - 2 / 2)))
    (is (= 8  (infix-calc 10 / 2 - 1 * 2)))
    (is (= 72 (infix-calc 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))))

(deftest problem-137-digits-and-bases
  (let [digits (fn digits [num base] 
                 (if (zero? num)
                   [0]
                   (reverse (map second 
                                 (take-while #(not= [0 0] %) 
                                             (iterate (fn [[a b]] [(quot a base) (rem a base)]) 
                                                      [(quot num base) (rem num base)]))))))]
    (is (= [1 2 3 4 5 0 1] (digits 1234501 10)))
    (is (= [0] (digits 0 11)))
    (= [1 0 0 1] (digits 9 2))
    (= [1 0] (let [n (rand-int 100000)](digits n n)))
    (= [16 18 5 24 15 1] (digits Integer/MAX_VALUE 42))))

(deftest problem-141-tricky-card-games
  ;; In Paris
  (let [tricky-card-games (fn tricky-card-games
                            ([trump] (partial tricky-card-games trump))
                            ([trump play]
                             (let [the-trump (if (nil? trump)
                                               (:suit (first play))
                                               trump)
                                   of-trumps (filter #(= the-trump (:suit %)) play)
                                   max-rank-of-trumps (apply max (map :rank of-trumps))]
                               {:suit the-trump :rank max-rank-of-trumps})))]
    (let [notrump (tricky-card-games nil)]
      (is (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                                   {:suit :club :rank 9}]))
               (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                   {:suit :club :rank 10}])))))
    (is (= {:suit :club :rank 10} ((tricky-card-games :club) [{:suit :spade :rank 2}
                                                              {:suit :club :rank 10}])))
    (is (= {:suit :heart :rank 8}
           ((tricky-card-games :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                                        {:suit :diamond :rank 10} {:suit :heart :rank 4}])))))

(deftest problem-144-oscilrate
  (let [oscilrate (fn oscilrate [start & fn-list]
                    (cons start (lazy-seq (apply oscilrate ((first fn-list) start) (concat (rest fn-list) [(first fn-list)])))))]
    (is (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
    (is (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
    (is (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))))

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

(deftest problem-148-the-big-divide
  ;; In Paris
  ;; The sum of the multiples of any number `a` are given by a * 1 +  a * 2 + a * 3 .... and so on.
  ;; This can be simplified to a(1 + 2 + 3.....).
  ;; The second part of the equation is the sum of the first n natural numbers and hence is given by n(n+1)/2.
  ;; We use the above facts to calculate the sum of multiples a number below a given value.
  (let [big-divide (fn big-divide [n a b]
                     (let [sum-of-multiples (fn [n a]
                                              (let [quot-a (quot (dec n) a)]
                                                (*' a (/ (*' quot-a (inc quot-a)) 2))))]
                       (- (+ (sum-of-multiples n a) 
                             (sum-of-multiples n b))
                          (sum-of-multiples n (* a b)))))]
        (is (= 0 (big-divide 3 17 11)))
        (is (= 23 (big-divide 10 3 5)))
        (is (= 233168 (big-divide 1000 3 5)))
        (is (= "2333333316666668" (str (big-divide 100000000 3 5))))
        (is (= "110389610389889610389610"
               (str (big-divide (* 10000 10000 10000) 7 11))))
        (is (= "1277732511922987429116"
               (str (big-divide (* 10000 10000 10000) 757 809))))
        (is (= "4530161696788274281"
               (str (big-divide (* 10000 10000 1000) 1597 3571))))))

(deftest problem-153-pairwise-disjoint-sets
  (let [pairwise-disjoint? (fn [s-o-s]
                             (every? true? (for [s1 s-o-s s2 s-o-s :when (not= s1 s2)]
                                             (nil? (seq (clojure.set/intersection s1 s2))))))]
    (is (= (pairwise-disjoint? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
              true))
    (is (= (pairwise-disjoint? #{#{:a :b :c :d :e}
                                 #{:a :b :c :d}
                                 #{:a :b :c}
                                 #{:a :b}
                                 #{:a}})
           false))
    (is (= (pairwise-disjoint? #{#{[1 2 3] [4 5]}
                                 #{[1 2] [3 4 5]}
                                 #{[1] [2] 3 4 5}
                                 #{1 2 [3 4] [5]}})
           true))
    (is (= (pairwise-disjoint? #{#{'a 'b}
                                 #{'c 'd 'e}
                                 #{'f 'g 'h 'i}
                                 #{''a ''c ''f}})
           true))
    (is (= (pairwise-disjoint? #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                                 #{#{:x :y :z} #{:x :y} #{:z} #{}}
                                 #{'[:x :y :z] [:x :y] [:z] [] {}}})
           false))
    (is (= (pairwise-disjoint? #{#{(= "true") false}
                                 #{:yes :no}
                                 #{(class 1) 0}
                                 #{(symbol "true") 'false}
                                 #{(keyword "yes") ::no}
                                 #{(class '1) (int \0)}})
           false))
    (is (= (pairwise-disjoint? #{#{distinct?}
                                 #{#(-> %) #(-> %)}
                                 #{#(-> %) #(-> %) #(-> %)}
                                 #{#(-> %) #(-> %) #(-> %)}})
           true))
    (is (= (pairwise-disjoint? #{#{(#(-> *)) + (quote mapcat) #_ nil}
                                 #{'+ '* mapcat (comment mapcat)}
                                 #{(do) set contains? nil?}
                                 #{, , , #_, , empty?}})
           false))))

(deftest problem-156-map-defaults
  (let [m-defs (fn [default keys]
                 (into {} (map vector keys (repeat default))))]
    (is (= (m-defs 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (m-defs "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (m-defs [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

(deftest problem-157-indexing-sequences
  (let [i-seq (fn [s]
                (map-indexed (fn [idx item] [item idx]) s))]
    (is (= (i-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
    (is (= (i-seq [0 1 3]) '((0 0) (1 1) (3 2))))
    (is (= (i-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))))

(deftest problem-158-decurry
  (let [decurry (fn [fns]
                  (fn [& args] (reduce #(%1 %2) fns args)))]
    (is (= 10 ((decurry (fn [a]
                          (fn [b]
                            (fn [c]
                              (fn [d]
                                (+ a b c d))))))
               1 2 3 4)))
    (is (= 24 ((decurry (fn [a]
                          (fn [b]
                            (fn [c]
                              (fn [d]
                                (* a b c d))))))
               1 2 3 4)))
    (is (= 25 ((decurry (fn [a]
                          (fn [b]
                            (* a b))))
               5 5)))))

(deftest problem-166-comparisons
  (let [comparisons (fn [lt-op operand-1 operand-2] 
                      (let [comparison-results [(lt-op operand-1 operand-2)
                                                (lt-op operand-2 operand-1)]]
                        ({[true false] :lt,
                          [false true] :gt,
                          [false false] :eq} comparison-results)))]
    (is (= :gt (comparisons < 5 1)))
    (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
    (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
    (is (= :gt (comparisons > 0 2)))))

(deftest problem-171-intervals
  ;; In Paris
  (let [intervals (fn intervals 
                    [the-list]
                    (map (fn [x] [(apply min x) (apply max x)]) 
                         (reverse (reduce (fn 
                                            [acc itm]
                                            (if (or (= (first (first acc)) itm)
                                                    (= (first (first acc)) (dec itm)))
                                              (cons (cons itm (first acc)) (rest acc) )
                                              (cons (cons itm ()) acc ))) 
                                          ()
                                          (sort the-list)))))]
    (is (= (intervals [1 2 3]) [[1 3]]))
    (is (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))
    (is (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))
    (is (= (intervals []) []))
    (is (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
           [[1 4] [6 6] [9 11] [13 17] [19 19]]))))

(deftest problem-177-balancing-brackets
  ;; In Paris
  (let [balanced? (fn [s]
                    (let [open-bracket #{\( \{ \[}
                          close-bracket #{\) \} \]}
                          close-open-map {\) \( \} \{ \] \[}
                          brackets-left (reduce (fn [acc itm]
                                                  (cond 
                                                   (open-bracket itm) (cons itm acc)
                                                   (close-bracket itm) (if (= (first acc) (close-open-map itm))
                                                                         (rest acc)
                                                                         (cons itm acc))
                                                   :else acc)) 
                                                () 
                                                s)]
                      (empty? brackets-left)))]
    (is (balanced? "This string has no brackets."))
    (is (balanced? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))
    (is (not (balanced? "(start, end]")))
    (is (not (balanced? "())")))
    (is (not (balanced? "[ { ] } ")))
    (is (balanced? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
    (is (not (balanced? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
    (is (not (balanced? "[")))))

(deftest problem-195-parentheses-again
  (let [balanced-parens (fn balanced-parens
                          ([n] (into #{} (map #(apply str %) (balanced-parens [] [] n 0))))
                          ([out-out out open-parens close-parens]
                           (if (and (zero? open-parens) (zero? close-parens))  
                             (conj out-out out)
                             (let [p1 (if (> open-parens 0) 
                                        (balanced-parens out-out (conj out \() (dec open-parens) (inc close-parens)))
                                   p2 (if (> close-parens 0) 
                                        (balanced-parens out-out (conj out \)) open-parens (dec close-parens)))]
                               (concat p1 p2)))))]
    (is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (balanced-parens n)) [0 1 2])))
    (is (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (balanced-parens 3)))
    (is (= 16796 (count (balanced-parens 10))))
    (is (= (nth (sort (filter #(.contains ^String % "(()()()())") (balanced-parens 9))) 6) "(((()()()())(())))"))
    (is (= (nth (sort (balanced-parens 12)) 5000) "(((((()()()()()))))(()))"))))



;; Below this line lie problems not yet solved.
;;

(deftest problem-112-sequs-horribilis
  (let [sequs-horribilis (fn sequs-horribilis
                           ([n s] (sequs-horribilis n s 0))
                           ([n [head & tail] current-sum]
                            (cond
                              (nil? head) ()
                              (sequential? head) [(sequs-horribilis n
                                                                    head
                                                                    current-sum)]
                              (> (+ current-sum head) n) ()
                              :else (cons head
                                          (sequs-horribilis n
                                                            tail
                                                            (+ current-sum head))))))]
    (is (= (sequs-horribilis 10 [1 2 [3 [4 5] 6] 7])
           '(1 2 (3 (4)))))

    (is (= (sequs-horribilis 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
           '(1 2 (3 (4 (5 (6 (7))))))))
    (is (= (sequs-horribilis 9 (range))
           '(0 1 2 3)))
    (is (= (sequs-horribilis 1 [[[[[1]]]]])
           '(((((1)))))))
    (is (= (sequs-horribilis 0 [1 2 [3 [4 5] 6] 7])
           '()))
    (is (= (sequs-horribilis 0 [0 0 [0 [0]]])
           '(0 0 (0 (0)))))
    (is (= (sequs-horribilis 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
           '(-10 (1 (2 3 (4))))))))

(deftest problem-113-making-data-dance
  (let [dance (fn [s] s)]
    (is (= "1, 2, 3" (str (dance 2 1 3))))
    (is (= '(2 1 3) (seq (dance 2 1 3))))
    (is (= '(2 1 3) (seq (dance 2 1 3 3 1 2))))
    (is (= '(1) (seq (apply dance (repeat 5 1)))))
    (is (= "1, 1, 1, 1, 1" (str (apply dance (repeat 5 1)))))
    (is (and (= nil (seq (dance)))
             (=  "" (str (dance)))))
    ))

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
  (let [connected? (fn connected? [g]
                     (let [adjacent-vertices (apply merge-with (comp vec concat) (mapcat (fn [[a b]] [{a [b]} {b [a]}]) g))]
                       adjacent-vertices))]
    ;; (= true (connected? #{[:a :a]}))
    ;; (is (= true (connected? #{[:a :b]})))
    ;; (is (= false (connected? #{[1 2] [2 3] [3 1]
    ;;                            [4 5] [5 6] [6 4]})))
    ;; (is (= true (connected? #{[1 2] [2 3] [3 1]
    ;;                           [4 5] [5 6] [6 4] [3 4]})))
    ;; (is (= false (connected? #{[:a :b] [:b :c] [:c :d]
    ;;                            [:x :y] [:d :a] [:b :e]})))
    ;; (is (= true (connected? #{[:a :b] [:b :c] [:c :d]
    ;;                           [:x :y] [:d :a] [:b :e] [:x :a]})))
    (println (connected? #{[:a :b] [:b :c] [:c :d]
                           [:x :y] [:d :a] [:b :e] [:x :a]}))
    ))

(let [x java.lang.Class]
  (and (= (class x) x) x))



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

(defn min-by [f coll]
  (first (sort-by f coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])
(total-cost 0 900 5 0 0)
(neighbours 5 [0 0])

(deftest problem-140-veitch-please
  (let [veitch-please (fn [])]
    (is (= (veitch-please #{#{'a 'B 'C 'd}
                            #{'A 'b 'c 'd}
                            #{'A 'b 'c 'D}
                            #{'A 'b 'C 'd}
                            #{'A 'b 'C 'D}
                            #{'A 'B 'c 'd}
                            #{'A 'B 'c 'D}
                            #{'A 'B 'C 'd}})
           #{#{'A 'c} 
             #{'A 'b}
             #{'B 'C 'd}}))
    (is (= (veitch-please #{#{'A 'B 'C 'D}
                            #{'A 'B 'C 'd}})
           #{#{'A 'B 'C}}))
    (is (= (veitch-please #{#{'a 'b 'c 'd}
                            #{'a 'B 'c 'd}
                            #{'a 'b 'c 'D}
                            #{'a 'B 'c 'D}
                            #{'A 'B 'C 'd}
                            #{'A 'B 'C 'D}
                            #{'A 'b 'C 'd}
                            #{'A 'b 'C 'D}})
           #{#{'a 'c}
             #{'A 'C}}))
    (is (= (veitch-please #{#{'a 'b 'c} 
                            #{'a 'B 'c}
                            #{'a 'b 'C}
                            #{'a 'B 'C}})
           #{#{'a}}))
    (is (= (veitch-please #{#{'a 'B 'c 'd}
                            #{'A 'B 'c 'D}
                            #{'A 'b 'C 'D}
                            #{'a 'b 'c 'D}
                            #{'a 'B 'C 'D}
                            #{'A 'B 'C 'd}})
           #{#{'a 'B 'c 'd}
             #{'A 'B 'c 'D}
             #{'A 'b 'C 'D}
             #{'a 'b 'c 'D}
             #{'a 'B 'C 'D}
             #{'A 'B 'C 'd}}))
    (is (= (veitch-please #{#{'a 'b 'c 'd}
                            #{'a 'B 'c 'd}
                            #{'A 'B 'c 'd}
                            #{'a 'b 'c 'D}
                            #{'a 'B 'c 'D}
                            #{'A 'B 'c 'D}})
           #{#{'a 'c}
             #{'B 'c}}))
    (is (= (veitch-please #{#{'a 'B 'c 'd}
                            #{'A 'B 'c 'd}
                            #{'a 'b 'c 'D}
                            #{'a 'b 'C 'D}
                            #{'A 'b 'c 'D}
                            #{'A 'b 'C 'D}
                            #{'a 'B 'C 'd}
                            #{'A 'B 'C 'd}})
           #{#{'B 'd}
             #{'b 'D}}))
    (is (= (veitch-please #{#{'a 'b 'c 'd}
                            #{'A 'b 'c 'd}
                            #{'a 'B 'c 'D}
                            #{'A 'B 'c 'D}
                            #{'a 'B 'C 'D}
                            #{'A 'B 'C 'D}
                            #{'a 'b 'C 'd}
                            #{'A 'b 'C 'd}})
           #{#{'B 'D}
             #{'b 'd}}))

    ))

(deftest problem-101-levenshtein-distance
  (let [lev-dist (fn lev-dist
                   [x y]
                   (cond
                    (= x y) 0
                    (nil? (seq x)) (count y)
                    (nil? (seq y)) (count x)
                    :else (loop [i 0
                                 v0 (range 0 (inc (count y)))]
                            (if (= i (count x))
                              (butlast v0)
                              (let [new-v0 (loop [j 0
                                                  v3 []]
                                             (if (= j (count y))
                                               v3
                                               (if (zero? j)
                                                 (recur (inc j) (conj v3 0))
                                                 (let [cost (if (= (nth x i) (nth y j)) 0 1)
                                                       m (min (inc (nth v3 j))
                                                              (inc (nth v0 (inc j)))
                                                              (+ (nth v0 j) cost))]
                                                   (recur (inc j) (conj v3 m)))
                                                 )))]
                                (recur (inc i) (dbg new-v0))))
                            )))]
    ;; (is (= (lev-dist "kitten" "sitting") 3))
    ;; (is (= (lev-dist "closure" "clojure") (lev-dist "clojure" "closure") 1))
    ;; (is (= (lev-dist "xyx" "xyyyx") 2))
    ;; (is (= (lev-dist "" "123456") 6))
    ;; (is (= (lev-dist "Clojure" "Clojure") (lev-dist "" "") (lev-dist [] []) 0))
    ;; (is (= (lev-dist [1 2 3 4] [0 2 3 4 5]) 2))
    ;; (is (= (lev-dist '(:a :b :c :d) '(:a :d)) 2))
    (is (= (lev-dist "ttttattttctg" "tcaaccctaccat") 10))
    ;; (is (= (lev-dist "gaattctaatctc" "caaacaaaaaattt") 9))

    )
  )

(deftest problem-150-palindromic-numbers
  (let [palindromic (fn palindromic [n]
                      (for [num (iterate inc n) 
                            :let [str-num (str num)
                                  rev-str-num (clojure.string/reverse str-num)] 
                            :when (= str-num  rev-str-num)]
                        num))]
    (is (= (take 26 (palindromic 0))
           [0 1 2 3 4 5 6 7 8 9 
            11 22 33 44 55 66 77 88 99 
            101 111 121 131 141 151 161]))
    (is (= (take 16 (palindromic 162))
           [171 181 191 202 
            212 222 232 242 
            252 262 272 282 
            292 303 313 323]))
    (is (= (take 6 (palindromic 1234550000))
           [1234554321 1234664321 1234774321 
            1234884321 1234994321 1235005321]))
    (is (= (first (palindromic (* 111111111 111111111)))
           (* 111111111 111111111)))
    (is (= (set (take 199 (palindromic 0)))
           (set (map #(first (palindromic %)) (range 0 10000)))))
    (is (= true 
           (apply < (take 6666 (palindromic 9999999)))))
    (is (= (nth (palindromic 0) 10101)
           9102019))

    )
  )

(deftest problem-150-palindromic-numbers-take-2
  (let [palindromic (fn palindromic [n]
                      (let [digits (fn [num] 
                                     (if (zero? num)
                                       '(0)
                                       (map second 
                                            (take-while #(not= [0 0] %) 
                                                        (iterate (fn [[a _]] 
                                                                   [(quot a 10) (rem a 10)]) 
                                                                 [(quot num 10) (rem num 10)])))))]
                        (for [num (iterate inc n) 
                              :let [digits-num (digits num)
                                    rev-digits-num (reverse digits-num)] 
                              :when (= digits-num  rev-digits-num)]
                          num)))]
    (is (= (take 26 (palindromic 0))
           [0 1 2 3 4 5 6 7 8 9 
            11 22 33 44 55 66 77 88 99 
            101 111 121 131 141 151 161]))
    (is (= (take 16 (palindromic 162))
           [171 181 191 202 
            212 222 232 242 
            252 262 272 282 
            292 303 313 323]))
    (is (= (take 6 (palindromic 1234550000))
           [1234554321 1234664321 1234774321 
            1234884321 1234994321 1235005321]))
    ;; (is (= (first (palindromic (* 111111111 111111111)))
    ;;        (* 111111111 111111111)))
    ;; (is (= (set (take 199 (palindromic 0)))
    ;;        (set (map #(first (palindromic %)) (range 0 10000)))))
    ;; (is (= true 
    ;;        (apply < (take 6666 (palindromic 9999999)))))
    ;; (is (= (nth (palindromic 0) 10101)
    ;;        9102019))

    )
  )

(deftest problem-150-palindromic-numbers-take-3
  (let [palindromic (fn palindromic [n]
                      (let [digits (fn [num] 
                                     (if (zero? num)
                                       '(0)
                                       (map second 
                                            (take-while #(not= [0 0] %) 
                                                        (iterate (fn [[a _]] 
                                                                   [(quot a 10) (rem a 10)]) 
                                                                 [(quot num 10) (rem num 10)])))))]
                        (map first (filter second (map (fn [num]
                                                     (let [digits-num (digits num)
                                                           rev-digits-num (reverse digits-num)] 
                                                       [num (= digits-num  rev-digits-num)])) 
                                                   (iterate inc n))))))]
    (is (= (take 26 (palindromic 0))
           [0 1 2 3 4 5 6 7 8 9 
            11 22 33 44 55 66 77 88 99 
            101 111 121 131 141 151 161]))
    (is (= (take 16 (palindromic 162))
           [171 181 191 202 
            212 222 232 242 
            252 262 272 282 
            292 303 313 323]))
    (is (= (take 6 (palindromic 1234550000))
           [1234554321 1234664321 1234774321 
            1234884321 1234994321 1235005321]))
    ;; (is (= (first (palindromic (* 111111111 111111111)))
    ;;        (* 111111111 111111111)))
    ;; (is (= (set (take 199 (palindromic 0)))
    ;;        (set (map #(first (palindromic %)) (range 0 10000)))))
    ;; (is (= true 
    ;;        (apply < (take 6666 (palindromic 9999999)))))
    ;; (is (= (nth (palindromic 0) 10101)
    ;;        9102019))

    )
  )



(time (take 26 ((fn palindromic [n]
                  (let [digits (fn [num] 
                                 (if (zero? num)
                                   '(0)
                                   (map second 
                                        (take-while #(not= [0 0] %) 
                                                    (iterate (fn [[a _]] 
                                                               [(quot a 10) (rem a 10)]) 
                                                             [(quot num 10) (rem num 10)])))))]
                    (for [num (iterate inc n) 
                          :let [digits-num (digits num)
                                rev-digits-num (reverse digits-num)] 
                          :when (= digits-num  rev-digits-num)]
                      num))) 0)))

(time (take 26 ((fn palindromic [n]
                  (let [digits (fn [num] 
                                 (if (zero? num)
                                   '(0)
                                   (map second 
                                        (take-while #(not= [0 0] %) 
                                                    (iterate (fn [[a _]] 
                                                               [(quot a 10) (rem a 10)]) 
                                                             [(quot num 10) (rem num 10)])))))]
                    (map first (filter second (map (fn [num]
                                                     (let [digits-num (digits num)
                                                           rev-digits-num (reverse digits-num)] 
                                                       [num (= digits-num  rev-digits-num)])) 
                                                   (iterate inc n))))
                    )) 0)))


(let [num 100]
  (take 10 (iterate (fn [[a _]] 
                      [(quot a 10) (rem a 10)]) 
                    [(quot num 10) (rem num 10)])))

(deftest problem-131-sum-some-set-subsets
  (let [equivalent-subsets? (fn [& in-sets]
                              (let [power-set (fn [s]
                                                (let [set-to-seq (seq s)
                                                      power-set-size (Math/pow 2 (count s))
                                                      eligibility 
                                                      (map #(Integer/toBinaryString %) (range power-set-size))]
                                                  (set (for [e eligibility]
                                                         (set (map first 
                                                                   (filter (fn [[a b]] (= b \1)) 
                                                                           (partition 2 (interleave set-to-seq 
                                                                                                    (reverse e))))))))))]
                                (not (empty? (apply clojure.set/intersection 
                                                    (map  (fn [in] 
                                                            (set (map #(apply + %) 
                                                                      (remove empty? 
                                                                              (power-set in))))) 
                                                          in-sets))))))]
    (is (= true  (equivalent-subsets? #{-1 1 99} 
                     #{-2 2 888}
                     #{-3 3 7777}))) ; ex. all sets have a subset which sums to zero
    (is (= false (equivalent-subsets? #{1}
                     #{2}
                     #{3}
                     #{4})))
    (is (= true  (equivalent-subsets? #{1})))
    (is (= false (equivalent-subsets? #{1 -3 51 9} 
                     #{0} 
                     #{9 2 81 33})))
    (is (= true  (equivalent-subsets? #{1 3 5}
                     #{9 11 4}
                     #{-3 12 3}
                     #{-3 4 -2 10})))
    (is (= false (equivalent-subsets? #{-1 -2 -3 -4 -5 -6}
                     #{1 2 3 4 5 6 7 8 9})))
    (is (= true  (equivalent-subsets? #{1 3 5 7}
                     #{2 4 6 8})))
    (is (= true  (equivalent-subsets? #{-1 3 -5 7 -9 11 -13 15}
                     #{1 -3 5 -7 9 -11 13 -15}
                     #{1 -1 2 -2 4 -4 8 -8})))
    (is (= true  (equivalent-subsets? #{-10 9 -8 7 -6 5 -4 3 -2 1}
                     #{10 -9 8 -7 6 -5 4 -3 2 -1})))
    
    )
)

(deftest problem-85-power-set-take-2
  (let [power-set-1 (fn [s]
                      (let [set-to-seq (seq s)
                            power-set-size (Math/pow 2 (count s))
                            eligibility (map #(Integer/toBinaryString %) (range power-set-size))]
                        (set (for [e eligibility]
                               (set (map first 
                                         (filter #(= \1 (second %)) 
                                                 (map (fn [elt include?] [elt include?])
                                                      set-to-seq 
                                                      (reverse e)))))))))
        power-set (fn [s]
                    (let [set-to-seq (seq s)
                          power-set-size (Math/pow 2 (count s))
                          eligibility (map  (fn [b] (map #(bit-test b %) (range (count s)))) (range power-set-size))]
                      (set (for [e eligibility]
                             (set (map first (filter second
                                                     (map (fn [elt include?] [elt include?]) 
                                                          set-to-seq e))))))))]
    (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
    (is (= (power-set #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
    (time (is (= (count (power-set-1 (into #{} (range 10)))) 1024)))))
