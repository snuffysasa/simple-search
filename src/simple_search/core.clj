(ns simple-search.core
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_11_200_1000
        simple-search.knapsack-examples.knapPI_11_1000_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_13_200_1000
        simple-search.knapsack-examples.knapPI_13_1000_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_16_200_1000
        simple-search.knapsack-examples.knapPI_16_1000_1000))

(first knapPI_11_20_1000_2)


;; Used to sort the items by ratio first and then by weight. Compares two elements at a time.
(defn comp-ratios
  [el1 el2]
  (if (or  (> (:ratio el1) (:ratio el2))
	  (and (= (:ratio el1) (:ratio el2))(> (:weight el1) (:weight el2))))
    true
    false))

;; Gets the ratios of each item and appends it to the item in the list. Returns the list!
(defn getRatios[items newItems]

  (conj newItems {:value (:value (first items)) :weight (:weight (first items)) :ratio (/ (:value (first items)) (:weight (first items)))})

  (if (= 1 (count items))
    (conj newItems {:value (:value (first items)) :weight (:weight (first items)) :ratio (/ (:value (first items)) (:weight (first items)))})
    (getRatios (rest items) (conj newItems {:value (:value (first items)) :weight (:weight (first items)) :ratio (/ (:value (first items)) (:weight (first items)))})))
  )


;; Grabs items from items and put it in usedItems if it fits in the bag with the other items in usedItems. Returns usedItems once nothing else fits.
(defn grab-items[items usedItems mcap ccap]
    (if (= (count items) 0)
      usedItems
      (if (= mcap ccap)
        usedItems
        (if (> (+ (:weight (first items)) ccap) mcap)
          (grab-items (rest items) usedItems mcap ccap)
          (grab-items (rest items) (conj usedItems (first items)) mcap (+ ccap (:weight (first items))))
        )
      )
    )
  )
(map :value (:items knapPI_11_20_1000_2))


;; Used to find totals of usedItems

;; Get the total value of all items
(defn get-value[items]
  (reduce + (map :value items))
  )
;; Get the total weight of all items
(defn get-weight[items]
  (reduce + (map :weight items))
  )


;; Double checking that the algorithm did not create or duplicate items

;; Remove cur from items if it exists
(defn remove-item [items cur]
  (and (= (:weight cur) (:weight (first items))) (= (:value cur) (:value (first items))))
  (if (and (= (:weight cur) (:weight (first items))) (= (:value cur) (:value (first items))))
    (rest items)
    (conj (remove-item (rest items) cur) (first items))
    )
  )
;; Check elements in usedItems to see if they are in items. If they are then return true
(defn check-used-items [items usedItems choices]
  (let [cur (first usedItems)]
    (if (= () usedItems)
      true
      (let [list-removed (remove-item items cur)]
      (if (= (count list-removed) (count items))
        false
        (check-used-items list-removed (rest usedItems))
      )
      )
    )
  )
  )

;; Checks an item to see if it is in the list of used items.
;; if it is it removes it from usedItems
(defn checkItem [item usedItems times]

  (if (= times 0)
    usedItems

    (if (and (= (:weight item) (:weight (nth usedItems (- times 1)))) (= (:value item) (:value (nth usedItems (- times 1)))))

      (let []
        (vec (concat (subvec usedItems 0 (- times 1)) (subvec usedItems times))))
      (checkItem item usedItems (- times 1))
      )
    )
  )




;  (checkItem {:weight 1 :value 5} [{:weight 1 :value 5}{:weight 1 :value 5}] 1)


;; return list of choices ([0 1 0]) that maps 0s and 1s to the values in items.
;; it will be a 1 if the item is in usedItems and 0 if not.
(defn makeChoices[items usedItems choices]
  (if (= (count items) 0)
    choices
  (let [newUsedItems (checkItem (first items) usedItems (count usedItems))]
    (if (= (count newUsedItems) (count usedItems))
      (makeChoices (rest items) usedItems (conj choices 0))
      (makeChoices (rest items) newUsedItems (conj choices 1))
    )

   )
  )
 )

;(makeChoices [{:weight 1 :value 5}{:weight 2 :value 5}] [{:weight 1 :value 5} {:weight 2 :value 5}] [])

;; Does the knapsack problem greedily
;; 1) generates ratios with getRatios
;; 2) Get all items that fit by hightest ratios and highest weight
;; 3) Ensure all used items exist in the original list
;; 4) Get the total value and total weight and return a map with those and the items used to make it
(defn knapsack[problem num-tries]
  (let [
        ratios (sort comp-ratios
          (getRatios (:items problem ) []))
        usedItems (grab-items ratios
        [] (:capacity problem ) 0)
        ]
      {
       :instance {:items (:items problem) :capacity (:capacity problem )}
       :choices (makeChoices (:items problem) usedItems [])
       :value (get-value usedItems)
       :weight (get-weight usedItems)
       :maxWeight (:capacity problem)
       :usedItems usedItems}
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;; Hillclimbing code
;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; An answer will be a map with (at least) four entries:
;;;   * :instance
;;;   * :choices - a vector of 0's and 1's indicating whether
;;;        the corresponding item should be included
;;;   * :total-weight - the weight of the chosen items
;;;   * :total-value - the value of the chosen items

(defn included-items
  "Takes a sequences of items and a sequence of choices and
  returns the subsequence of items corresponding to the 1's
  in the choices sequence."
  [items choices]
  (map first
       (filter #(= 1 (second %))
               (map vector items choices))))

(defn greedy-answer
  "Construct a greedy answer for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (:choices (knapsack instance 1))
        included (included-items (:items instance) choices)]
    {:instance instance
     :choices choices
     :total-weight (reduce + (map :weight included))
     :total-value (reduce + (map :value included))}))

(defn greedy-answerer
  "Construct a greedy answer for the given instance of the
  knapsack problem."
  [scorer scorr instance tries]
  (let [choices (:choices (knapsack instance 1))
        included (included-items (:items instance) choices)]
    (scorer {:instance instance
     :choices choices
     :total-weight (reduce + (map :weight included))
     :total-value (reduce + (map :value included))} scorr)))

(defn random-answer
  "Construct a random answer for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (repeatedly (count (:items instance)) #(rand-int 2))
        included (included-items (:items instance) choices)]
    {:instance instance
     :choices choices
     :total-weight (reduce + (map :weight included))
     :total-value (reduce + (map :value included))}))

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
   in which case we return over capacity * -1"
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    (- (:capacity (:instance answer)) (:total-weight answer))

    (:total-value answer)))

(defn add-score
  "Computes the score of an answer and inserts a new :score field
   to the given answer, returning the augmented answer."
  [answer score-function]
  (assoc answer :score (score-function answer)))

;; does normal random search and takes the best answer found
(defn random-search
  [score-function instance max-tries]
  (apply max-key :score
         (map #(add-score % score-function)
              (repeatedly max-tries #(random-answer instance)))))


;; Runs random-flip-check to climb the hill of either a random answer or a greedy answer depending on the argument greedy.


;; This will flip a random bit from a 1 to a 0 or a 0 to a 1 n number of times.
(defn random-flip [answer times]
  (if (> times -1)
  (let [randIndex (rand-int (count answer))
        vecInstance (vec answer)
        ]
    (if (= 0 (nth vecInstance randIndex))
      (random-flip (assoc vecInstance randIndex 1) (- times 1))
      (random-flip (assoc vecInstance randIndex 0) (- times 1))
      )
    )
    answer
  )
)

;; random-flip-check takes the current best and remainingTries, flips 0 to 3 bits and compares the resulting score to the current best's score
;; whichever is higher is taken as the current best for the next loop of recursion.


;; random-flip-jump takes the current best, the remaining tries that are to be done, and the instance which is used to jump to a new answer.
;; It runs random-flip-search 100 times, and then jumps to a new random answer and runs that 100 times. It then compares the two, keeps the best, and recurses with remaining tries - 100

;; Does the same as flip-search but uses iterate instead of recursing by hand. if greedy is true then start with greedy, otherwise start with random
(defn hill-climber [search-strategy mutate-answer score-function make-answer instance max-tries]
    (nth (take max-tries (iterate (partial search-strategy instance max-tries score-function mutate-answer) (make-answer instance))) (dec max-tries))
  )

;; Does the same as random-flip-check but setup to be ran by an iterate function
(defn random-flip-check-iterate [instance max-tries score-function mutate-answer currentBest]
    (let [finalAnswer (assoc currentBest :choices (mutate-answer (:choices currentBest) (rand-int 4)))
          finalFinalAnswer (assoc finalAnswer :total-weight (reduce + (map :weight (included-items (:items (:instance currentBest)) (:choices finalAnswer)))))
          finalFinalFinalAnswer (assoc finalFinalAnswer :total-value (reduce + (map :value (included-items (:items (:instance currentBest)) (:choices finalAnswer)))))]

    (if (> (:score (add-score finalFinalFinalAnswer score-function)) (:score (add-score currentBest score-function)))
      (add-score finalFinalFinalAnswer score-function)
      (add-score currentBest score-function)
      )
    )
  )

;; Does the same as random-flip-check but setup to be ran by an iterate function
(defn random-flip-check-jump [instance max-tries score-function mutate-answer currentBest]
  (if (= (rand-int (/ max-tries 10)) 0)
    (let [finalFinalFinalAnswer (hill-climber random-flip-check-iterate mutate-answer score-function random-answer instance (/ max-tries 10))]

      (if (> (:score (add-score finalFinalFinalAnswer score-function)) (:score (add-score currentBest score-function)))
        (add-score finalFinalFinalAnswer score-function)
        (add-score currentBest score-function)
        )
      )
    (let [finalAnswer (assoc currentBest :choices (mutate-answer (:choices currentBest) (rand-int 4)))
          finalFinalAnswer (assoc finalAnswer :total-weight (reduce + (map :weight (included-items (:items (:instance currentBest)) (:choices finalAnswer)))))
          finalFinalFinalAnswer (assoc finalFinalAnswer :total-value (reduce + (map :value (included-items (:items (:instance currentBest)) (:choices finalAnswer)))))]

      (if (> (:score (add-score finalFinalFinalAnswer score-function)) (:score (add-score currentBest score-function)))
        (add-score finalFinalFinalAnswer score-function)
        (add-score currentBest score-function)
        )
      )
    )
  )



;(:score (hill-climber random-flip-check-jump random-flip penalized-score random-answer knapPI_11_200_1000_1 10000))




;; This is Idea 4, the random jumping function
;(:score (random-search knapPI_16_20_1000_1 1000 penalized-score))
;(:score (random-flip-jump (flip-search knapPI_16_20_1000_1 100 false) 1000 knapPI_16_20_1000_1))


;; random flip search is made with a random starting answer. This is Idea 2
;(:score (flip-search knapPI_16_20_1000_1 1000 false))

;; greedy flip search is made with a greedy starting answer. This is Idea 3
;(:score (flip-search knapPI_16_20_1000_1 1000 true))

;; Both result in pretty much the same consistency of final answer.
;(:value (knapsack knapPI_16_20_1000_1))

;(knapsack knapPI_16_1000_1000_79)




