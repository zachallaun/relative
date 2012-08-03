(ns relative.rating-test
  (:use clojure.test
        relative.rating)
  (:require [relative.elo :as elo]))

(def names ["Zach" "Nick" "Bill" "Ben" "Dennis" "Sonali" "Dave"])

(deftest elo-test
  (testing "Expected scores"
    (with-test (defn within [n x y]
                 (> n (Math/abs (- x y))))
      ;; Expected scores are complements of each other
      (is (within 0.001 1 (+ (elo/e 1500 1500) (elo/e 1500 1500))))
      (is (within 0.001 1 (+ (elo/e 1400 1600) (elo/e 1600 1400))))))

  (testing "Calculated ratings"
    (is (< 1500 (elo/r 1500 1 0.5 32))) ;; Rating increases on win
    (is (> 1500 (elo/r 1500 0 0.5 32))) ;; Rating decreases on loss

    (testing "k-factor rating boundaries"
      (is (> 32 (Math/abs (- 1500 (elo/r 1500 1 0.01 32)))))
      (is (> 32 (Math/abs (- 1500 (elo/r 1500 0 0.99 32)))))))

  (testing "EloPlayer"
    (let [p1 (elo/player "Z")
          p2 (elo/player "A")]
      (is (= 1500 (rating p1)))
      (is (= (rating p1) (rating p2)))
      ;; p1 wins against p2
      (elo/-match! p1 p2 false 32)
      (is (> (rating p1) (rating p2)))))

  (testing "Elo serialization"
    (let [engine (elo/elo-engine)
          players (map elo/player names)]

      ;; Play various matches
      (dotimes [_ 10]
        (match! engine (rand-nth players) (rand-nth players)))

      ;; Serialize, resurrect, and confirm that rating are preserved
      (let [serialized (serialize engine players)]
        (is (every? true? (map #(= (rating %1) (rating %2))
                               players
                               (resurrect engine serialized))))))))
