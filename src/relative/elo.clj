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

(defn- q
  "Q score based on average 1500 rating."
  [rating]
  (Math/pow 10 (/ rating 400)))

(defn- expected
  "Returns the expected score of player with rating1 against player
  with rating2."
  [rating1 rating2]
  (let [q1 (q rating1)
        q2 (q rating2)]
    (/ q1 (+ q1 q2))))

(defn- rating-given
  "Returns a new rating given an old rating, an actual score,
  an expected score, and a k-factor."
  [rating actual expected k-factor]
  (+ rating (* k-factor (- actual expected))))

(extend-type clojure.lang.PersistentHashMap
  IRelativeRatedPlayer
  (rating [m] (:rating m)))

(deftype EloEngine [k-factor]
  IRelativeRatingEngine
  ;; map should contain an :id and optional seed :rating
  (player [_ map]
    (merge (hash-map :rating 1500) map))

  (match [this winner loser]
    (match this winner loser false))

  (match [_ winner loser draw?]
    (let [exp-winner (expected (rating winner) (rating loser))
          exp-loser (expected (rating loser) (rating winner))
          update (fn [player actual estimate]
                   (merge player
                          {:rating (rating-given (rating player)
                                                 actual
                                                 estimate
                                                 k-factor)}))]
      (if draw?
        [(update winner 0.5 exp-winner)
         (update loser 0.5 exp-loser)]
        [(update winner 1 exp-winner)
         (update loser 0 exp-loser)])))

  (match-quality [_ p1 p2]
    (let [exp (expected (rating p1) (rating p2))]
      (- 1 (* (Math/abs (- exp 0.5)) 2))))

  (serialize [_ entities]
    (prn-str (vec entities)))

  (resurrect [this serialized]
    (map (partial player this) (read-string serialized))))

(defn elo-engine
  "Returns an EloEngine. Optional argument `k-factor` defaults to 32, and is
  the maximum possible change in rating from a match."
  ([] (elo-engine 32))
  ([k-factor] (->EloEngine k-factor)))
