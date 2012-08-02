(ns relative.elo
  (:require relative.rating))

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

(defprotocol IEloPlayer
  (rating [_])
  (update! [_ rating]))

(defn q
  "Q score based on average 1500 rating."
  [rating]
  (Math/pow 10 (/ rating 400)))

(defn e
  "Returns the expected score of player with rating1 against player
  with rating2."
  [rating1 rating2]
  (let [q1 (q rating1)
        q2 (q rating2)]
    (/ q1 (+ q1 q2))))

(defn r
  "Returns a new rating given an old rating, an actual score,
  an expected score, and a k-factor."
  [rating actual expected k-factor]
  (+ rating (* k-factor (- actual expected))))

(defrecord EloPlayer [id rating-atom]
  IEloPlayer
  (rating [_] @rating-atom)
  (update! [_ rating] (reset! rating-atom rating)))

(defn player
  "Returns a new EloPlayer with a default rating of 1500."
  ([name] (player name 1500))
  ([name default] (->EloPlayer name (atom default))))

(defn -map->player
  "Accepts a map and returns an EloPlayer. Accepted keys:

  id:   (required) The unique identifier of the player.
  seed: (optional) The seed rating of the player. Defaults to 1500."
  [{:keys [id seed]}]
  (player id (or seed 1500)))

(defn -match
  "Accepts two EloPlayers and updates their Elo ratings based
  on the result of the match.

  winner and loser must be EloPlayers.
  draw? is a boolean indicating whether or not the match was a tie.
  k is the k-factor representing the maximum possible change in rating."
  [winner loser draw? k]
  (let [r-winner (rating winner)
        r-loser (rating loser)
        e-winner (e r-winner r-loser)
        e-loser (e r-loser r-winner)]
    (if draw?
      (do (update! winner (r r-winner 0.5 e-winner k))
          (update! loser (r r-loser 0.5 e-loser k)))
      (do (update! winner (r r-winner 1 e-winner k))
          (update! loser (r r-loser 0 e-loser k))))
    [(rating winner) (rating loser)]))

(deftype EloEngine [k-factor]
  relative.rating.IRelativeRatingEngine
  (map->player [_ player-map] (-map->player player-map))
  (match [_ winner loser] (-match winner loser false k-factor))
  (match [_ winner loser draw?] (-match winner loser draw? k-factor)))

(defn elo-engine
  ([] (elo-engine 32))
  ([k-factor] (->EloEngine k-factor)))
