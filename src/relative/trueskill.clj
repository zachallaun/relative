(ns relative.trueskill
  (:use relative.rating))

;; TrueSkill Ranking System
;;
;; Described at:
;; https://research.microsoft.com/en-us/projects/trueskill/details.aspx
;;
;; Algorithms described in more detail at:
;; https://dl.dropbox.com/u/1083108/Moserware/Skill/The%20Math%20Behind%20TrueSkill.pdf
;;

(defn variance
  "The variance of a standard deviation sigma."
  [sigma] (Math/pow sigma 2))

(defn precision
  "The precision of a standard deviation, which is the inverse of the variance."
  [sigma]
  (/ 1 (variance sigma)))

(defn pdf
  "Normal probability density function:

  The value of a normal distribution at a given point x, with the distribution
  described by mean mu and standard deviation sigma."
  [x mu sigma]
  (let [exp (- (* (/ 1 (* 2 (variance sigma)))
                  (Math/pow (- x mu) 2)))]
    (* (/ 1 (* sigma (Math/sqrt (* 2 Math/PI))))
       (Math/pow Math/E exp))))

(defn error-fn
  "Approximation of the error function."
  [x]
  (let [p 0.3275911
        a [0.254829592
           -0.284496736
           1.421413741
           -1.453152027
           1.061405429]
        t (/ 1 (+ 1 (* p x)))]
    (- 1 (* (reduce + (map-indexed #(* %2 (Math/pow t (inc %1))) a))
            (Math/pow Math/E (- (Math/pow x 2)))))))

(defn cdf
  [x mu sigma]
  (let [xp (/ (- x mu) (* sigma (Math/sqrt 2)))]
    (* 1/2 (+ 1 (if (> x 0)
                  (error-fn xp)
                  (- (error-fn (- xp))))))))

;; (defn cumulative-dist
;;   "Approximation of the cumulative distribution function when -1 < t < 1
;;   given a normal distribution with the given precision."
;;   [t]
;;   (+ 1/2 (/ t (Math/sqrt (* 2 (Math/PI))))))

(defprotocol ITrueSkillPlayer
  (mu [_]
    "Average expected skill/rating of a player.")

  (sigma [_]
    "Standard deviation in skill of a player")

  (v [_ t e]
    "Mean Additive Truncated Gaussian Function."))

(defrecord TrueSkillPlayer [id mu-atom sigma-atom]
  ITrueSkillPlayer
  (mu [_] @mu-atom)

  (sigma [_] @sigma-atom)

  (v [_ t e]
    (/ (pdf (- t e) @mu-atom @sigma-atom)
       (cdf (- t e) @mu-atom @sigma-atom)))

  IRelativeRatedPlayer
  (rating [_] (- @mu-atom (* 3 @sigma-atom))))

(defprotocol ITrueSkillEngine
  (beta-sq [_]
    "Variance of performance around the skill of each player. Determined
    by the initial sigma spread.")

  (tau-sq [_]
    "Additive dynamics factor. Determined by the initial sigma spread.")

  (c-value [_ s1 s2]
    "Normalizing value. Determined by the performance variance of the
    system (beta-sq), and the sigma spread of both players going into a match.")

  (mu-additive-factors [_ winner loser]
    "Returns a pair of mu additive factors corresponding to the winner and loser."))

(deftype TrueSkillEngine [init-sigma init-mu draw]
  ITrueSkillEngine
  (beta-sq [_]
    (Math/pow (/ init-sigma 2) 2))

  (tau-sq [_]
    (Math/pow (/ init-sigma 100) 2))

  (c-value [this s1 s2]
    (Math/sqrt (+ (* 2 (beta-sq this))
                  (variance s1)
                  (variance s2))))

  ;; Clearly broken
  (mu-additive-factors [this winner loser]
    (let [c (c-value this (sigma winner) (sigma loser))
          t (/ (- (mu winner) (mu loser)) c)
          e (/ draw c)
          m-winner (v winner t e)
          m-loser (v loser t e)
          normalized-variance (fn [s]
                             (/ (variance s) c))]
      [(* (normalized-variance (sigma winner)) m-winner)
       (* (normalized-variance (sigma loser)) m-loser)]))

  IRelativeRatingEngine
  (map->player [_ {:keys [id seed opts]}]
    (->TrueSkillPlayer id (atom (or seed init-mu)) (atom (or (:sigma opts) init-sigma)))))

(defn trueskill-engine
  "Accepts optional keyword arguments to specify an initial sigma spread,
  mu skill value, and draw margin.

  Defaults to (trueskill-engine :sigma 25/3 :mu 25 :draw 0)"
  [& {:keys [sigma mu draw]}]
  (->TrueSkillEngine (or sigma 25/3) (or mu 25) (or draw 4)))
