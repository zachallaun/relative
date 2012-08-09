(ns relative.trueskill
  (:refer-clojure :exclude [with-precision])
  (:use [relative.rating]
        [relative.trueskill.distribution]
        [relative.trueskill.normal]))

;; TrueSkill Ranking System
;;
;; Described at:
;; https://research.microsoft.com/en-us/projects/trueskill/details.aspx
;;
;; Algorithms described in more detail at:
;; https://dl.dropbox.com/u/1083108/Moserware/Skill/The%20Math%20Behind%20TrueSkill.pdf
;;

(defn gauss-mean-mult
  "Mean Additive Truncated Gaussian Function."
  [t e]
  (/ (pdf (normal-distribution) (- t e))
     (cdf (normal-distribution) (- t e))))

(defn gauss-std-dev-mult
  "Gaussian Variance Multiplicative Function. Note that this function operates
  on a std dev not a variance as described in the Trueskill math paper."
  [t e]
  (let [multiplier (gauss-mean-mult t e)]
    (* multiplier (+ multiplier t (- e)))))

;; A TrueSkill player is a normal distribution
(extend-type relative.trueskill.normal.NormalDistribution
  IRelativeRatedPlayer
  (rating [this] (- (mean this) (* 3 (std-dev this)))))

(defprotocol ITrueSkillEngine
  (beta-sq [_]
    "Variance of performance around the skill of each player. Determined
    by the initial std-dev spread.")

  (tau-sq [_]
    "Additive dynamics factor. Determined by the initial std-dev spread.")

  (c-value [_ s1 s2]
    "Normalizing value. Determined by the performance variance of the
    system (beta-sq), and the std-dev spread of both players going into a match.")

  (mean-additive-factors [_ winner loser]
    "Returns a pair of mean additive factors corresponding to the winner and loser.")

  (draw-margin [this prob]
    "Returns a draw margin from an input draw probability.")

  (variance-mult-factors [_ winner loser]))

(deftype TrueSkillEngine [init-std-dev init-mean prob]
  ITrueSkillEngine
  (beta-sq [_]
    (Math/pow (/ init-std-dev 2) 2))

  (tau-sq [_]
    (Math/pow (/ init-std-dev 100) 2))

  (c-value [this p1 p2]
    (Math/sqrt (+ (* 2 (beta-sq this))
                  (variance p1)
                  (variance p2))))

  (draw-margin [this prob]
    (* (Math/sqrt 2) ;; assuming a one-on-one game, otherwise this is (+ (count team1) (count team2))
       (Math/sqrt (beta-sq this))
       (icdf (normal-distribution) (/ (inc prob) 2))))

  (mean-additive-factors [this winner loser]
    (let [c (c-value this winner loser)
          t (/ (- (mean winner) (mean loser)) c)
          e (/ (draw-margin this prob) c)
          multiplier (gauss-mean-mult t e)
          normalized-variance #(/ (variance %) c)]
      [(* (normalized-variance winner) multiplier)
       (* (normalized-variance loser) multiplier)]))

  (variance-mult-factors [this winner loser]
    (let [c (c-value this winner loser)
          t (/ (- (mean winner) (mean loser)) c)
          e (/ (draw-margin this prob) c)
          multiplier (gauss-std-dev-mult t e)
          normalized-variance #(/ (variance %) (Math/pow c 2))]
      [(- 1 (* (normalized-variance winner) multiplier))
       (- 1 (* (normalized-variance loser) multiplier))]))

  IRelativeRatingEngine
  ;; map should contain :id and optional :mean and :std-dev
  (player [_ {:keys [id mean std-dev]}]
    (-> (normal-distribution (or mean 25) (or std-dev 25/3))
        (assoc :id id)))

  (match [this winner loser]
    (match this winner loser false))

  (match [this winner loser draw?]
    (let [[wmean lmean] (mean-additive-factors this winner loser)
          [wvar lvar] (variance-mult-factors this winner loser)
          update (fn [player mdiff vdiff]
                   (-> (normal-distribution)
                       (with-mean (+ (mean winner) mdiff))
                       (with-variance (* (variance winner) vdiff))))]
      [(update winner wmean wvar)
       (update loser (- lmean) lvar)])))

(defn trueskill-engine
  "Accepts optional keyword arguments to specify an initial std-dev,
  mean skill value, and draw margin.

  Defaults to (trueskill-engine :std-dev 25/3 :mean 25 :prob 0)"
  [& {:keys [std-dev mean prob]}]
  (->TrueSkillEngine (or std-dev 25/3) (or mean 25) (or prob 0)))
