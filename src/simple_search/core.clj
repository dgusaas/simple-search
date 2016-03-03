(ns simple-search.core
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))

;;; An answer will be a map with (at least) four entries:
;;;   * :instance
;;;   * :choices - a vector of 0's and 1's indicating whether
;;;        the corresponding item should be included
;;;   * :total-weight - the weight of the chosen items
;;;   * :total-value - the value of the chosen items

(defrecord Answer
  [instance choices total-weight total-value])

(defn included-items
  "Takes a sequences of items and a sequence of choices and
  returns the subsequence of items corresponding to the 1's
  in the choices sequence."
  [items choices]
  (map first
       (filter #(= 1 (second %))
               (map vector items choices))))

(defn make-answer
  [instance choices]
  (let [included (included-items (:items instance) choices)]
    (->Answer instance choices
              (reduce + (map :weight included))
              (reduce + (map :value included)))))

(defn random-answer
  "Construct a random answer for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (repeatedly (count (:items instance))
                            #(rand-int 2))]
    (make-answer instance choices)))

; (random-answer knapPI_13_20_1000_7)

;;; It might be cool to write a function that
;;; generates weighted proportions of 0's and 1's.

(defn score
  "Takes the total-weight of the given answer unless it's over capacity,
   in which case we return 0."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    0
    (:total-value answer)))

(defn penalized-score
  "Takes the total-weight of the given answer unless it's over capacity,
   in which case we return the negative of the total weight."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    (- (:total-weight answer))
    (:total-value answer)))

(defn lexi-score
  [answer]
  (let [shuffled-items (shuffle (included-items (:items (:instance answer))
                                                (:choices answer)))
        capacity (:capacity (:instance answer))]
    (loop [value 0
           weight 0
           items shuffled-items]
      (if (empty? items)
        value
        (let [item (first items)
              w (:weight item)
              v (:value item)]
          (if (> (+ weight w) capacity)
            (recur value weight (rest items))
            (recur (+ value v)
                   (+ weight w)
                   (rest items))))))))

; (lexi-score (random-answer knapPI_16_200_1000_1))

(defn add-score
  "Computes the score of an answer and inserts a new :score field
   to the given answer, returning the augmented answer."
  [scorer answer]
  (assoc answer :score (scorer answer)))

(defn random-search
  [scorer instance max-tries]
  (apply max-key :score
         (map (partial add-score scorer)
              (repeatedly max-tries #(random-answer instance)))))

; (random-search penalized-score knapPI_16_200_1000_1 10000)

(defn mutate-choices
  [choices]
  (let [mutation-rate (/ 1 (count choices))]
    (map #(if (< (rand) mutation-rate) (- 1 %) %) choices)))

(defn mutate-answer
  [answer]
  (make-answer (:instance answer)
               (mutate-choices (:choices answer))))

; (def ra (random-answer knapPI_11_20_1000_1))
; (mutate-answer ra)

(defn hill-climber
  [mutator scorer instance max-tries size]
  (loop [current-best (add-score scorer (random-answer instance))
         num-tries 1]
    (let [new-answer (add-score scorer (mutator current-best))]
      (if (>= num-tries max-tries)
        current-best
        (if (> (:score new-answer)
               (:score current-best))
          (recur new-answer (inc num-tries))
          (recur current-best (inc num-tries)))))))

;; (time (random-search score knapPI_16_200_1000_1 100000
;; ))

;; (time (hill-climber mutate-answer score knapPI_16_200_1000_1 100000
;; ))

;; (time (hill-climber mutate-answer penalized-score knapPI_16_200_1000_1 100000
;; ))
(defn find-best
  "Sorts the population by fitness."
  [population]
  (let [return (sort-by :score > population)]
  return))

(defn tournament-selection
  "Take the best of a randomly chosen group of individuals."
  [size population]
  (take size (repeatedly (fn blah[] (first (find-best (take 4 (repeatedly #(rand-nth population)))))))))

(defn two-point-crossover
  "Picks two points and switches all the values of the parents between those points "
  [parent1 parent2]
  (let [point1 (rand-int (count parent1))
        point2 (rand-int (count parent1))
        left (if (<= point1 point2) point1 point2)
        right (if (>= point1 point2) point1 point2)
        ]
    {:child1 (mutate-choices (concat (subvec (vec parent1) 0 left) (subvec (vec parent2) left right) (subvec (vec parent1) right (count (vec parent1)))))
     :child2 (mutate-choices (concat (subvec (vec parent2) 0 left) (subvec (vec parent1) left right) (subvec (vec parent2) right (count (vec parent2)))))}))

(defn uniform-crossover
  "Takes two parents and returns two children that are the crossovers of the parents."
  [parent1 parent2]
  (let [flips (take (count parent1) (repeatedly (count parent1) #(rand-int 5)))]
  {:child1 (mutate-choices (map #(if (= %3 0) %2 %1) parent1 parent2 flips)) :child2 (mutate-choices (map #(if (= %3 0) %1 %2) parent1 parent2 flips))}))

(defn crossover
  "Takes in a population and returns a new population by randomly crossover parents"
  [population size mutator]
  (take size (apply concat (take size (repeatedly #(vals (mutator (rand-nth population) (rand-nth population))))))))

(defn spawn-generation
  "Creates a mew generation based of the best of the last generation and returns the choices of the best"
  [instance size mutator population]
  (let [all (concat (map #(add-score penalized-score %) (map (partial make-answer instance) (crossover population size mutator))) (map #(add-score penalized-score %) (map (partial make-answer instance) population)))

        best (tournament-selection 4 all)]

    (map :choices best)))

(defn crossover-search
  ""
  [mutator instance max-tries size]
  (let [population (map :choices (take size (repeatedly #(random-answer instance))))]
  (add-score penalized-score (make-answer instance (first (find-best (map #(add-score penalized-score %) (map (partial make-answer instance) (last (take max-tries (iterate (partial spawn-generation instance size mutator) population)))))))))))


(crossover-search uniform-crossover knapPI_16_20_1000_1 100 200)
