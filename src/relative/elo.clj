(ns relative.elo
  (:use relative.rating))

;; Elo Rating (based on 1500 average)
;;
;; Given Player A with true strength Ra, and Player B with true
;; strength Rb, the expected score of Player A would be:
;;
;;    Ea = 1 / (1 + 10^((Rb - Ra) / 400))
;;       or
;;    Ea = Qa / (Qa + Qb)
;;       where
;;    Qa = 10^(Ra/400)
;;       and
;;    Qb = 10^(Rb/400)
;;
;; 0 < (Ea and Eb) < 1, where 0 would indicate loss, 1 would indicate
;; victory, and 0.5 would indicate draw (half a win and half a loss).
;;
;; Player A's new rating given a game with an actual score Sa would be:
;;
;;    Ra' = Ra + K(Sa - Ea)
;;        where K is the maximum possible adjustment per game
;;

(defn q
  "Q score based on average 1500 rating."
  [rating]
  (Math/pow 10 (/ rating 400)))

(defn expected
  "Returns the expected score of player with rating1 against player
  with rating2."
  [rating1 rating2]
  (let [q1 (q rating1)
        q2 (q rating2)]
    (/ q1 (+ q1 q2))))

(defn rating-given
  "Returns a new rating given an old rating, an actual score,
  an expected score, and a k-factor."
  [rating actual expected k-factor]
  (+ rating (* k-factor (- actual expected))))

(extend-type clojure.lang.PersistentHashMap
  IRelativeRatedPlayer
  (rating [m] (:rating m)))

(defn -match
  "Accepts two maps representing Elo players, and returns a pair of
  similar maps with updated :rating keys based on the outcome.

  `draw?` is a boolean indicating whether or not the match was a tie.
  `k` is the k-factor representing the maximum possible change in rating."
  [winner loser draw? k]
  (let [est-winner (expected (rating winner) (rating loser))
        est-loser (expected (rating loser) (rating winner))
        update (fn [player actual estimate]
                 (merge player
                        {:rating (rating-given (rating player) actual estimate k)}))]
    (if draw?
      [(update winner 0.5 est-winner)
       (update loser 0.5 est-loser)]
      [(update winner 1 est-winner)
       (update loser 0 est-loser)])))

(deftype EloEngine [k-factor]
  IRelativeRatingEngine
  ;; map should contain an :id and optional seed :rating
  (player [_ map]
    (merge (hash-map :rating 1500) map))

  (match [_ winner loser]
    (-match winner loser false k-factor))

  (match [_ winner loser draw?]
    (-match winner loser draw? k-factor))

  (serialize [_ entities]
    (prn-str (vec entities)))

  (resurrect [this serialized]
    (map (partial player this) (read-string serialized))))

(defn elo-engine
  ([] (elo-engine 32))
  ([k-factor] (->EloEngine k-factor)))
