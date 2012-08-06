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
  "The variance of a standard deviation stdd."
  [stdd] (Math/pow stdd 2))

(defn precision
  "The precision of a standard deviation, which is the inverse of the variance."
  [stdd]
  (/ 1 (variance stdd)))

(defn pdf
  "Normal probability density function:

  The value of a normal distribution at a given point x, with the distribution
  described by mean mean and standard deviation stdd."
  [x mean stdd]
  (let [exp (- (* (/ 1 (* 2 (variance stdd)))
                  (Math/pow (- x mean) 2)))]
    (* (/ 1 (* stdd (Math/sqrt (* 2 Math/PI))))
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
  [x mean stdd]
  (let [xp (/ (- x mean) (* stdd (Math/sqrt 2)))]
    (* 1/2 (+ 1 (if (> x 0)
                  (error-fn xp)
                  (- (error-fn (- xp))))))))

;; (defn cumeanlative-dist
;;   "Approximation of the cumeanlative distribution function when -1 < t < 1
;;   given a normal distribution with the given precision."
;;   [t]
;;   (+ 1/2 (/ t (Math/sqrt (* 2 (Math/PI))))))

(defprotocol ITrueSkillPlayer
  (mean [_]
    "Average expected skill/rating of a player.")

  (stdd [_]
    "Standard deviation in skill of a player")

  (v [_ t e]
    "Mean Additive Truncated Gaussian Function."))

(defrecord TrueSkillPlayer [id mean-atom stdd-atom]
  ITrueSkillPlayer
  (mean [_] @mean-atom)

  (stdd [_] @stdd-atom)

  (v [_ t e]
    (/ (pdf (- t e) @mean-atom @stdd-atom)
       (cdf (- t e) @mean-atom @stdd-atom)))

  IRelativeRatedPlayer
  (rating [_] (- @mean-atom (* 3 @stdd-atom))))

(defprotocol ITrueSkillEngine
  (beta-sq [_]
    "Variance of performance around the skill of each player. Determined
    by the initial stdd spread.")

  (tau-sq [_]
    "Additive dynamics factor. Determined by the initial stdd spread.")

  (c-value [_ s1 s2]
    "Normalizing value. Determined by the performance variance of the
    system (beta-sq), and the stdd spread of both players going into a match.")

  (mean-additive-factors [_ winner loser]
    "Returns a pair of mean additive factors corresponding to the winner and loser."))

(deftype TrueSkillEngine [init-stdd init-mean draw]
  ITrueSkillEngine
  (beta-sq [_]
    (Math/pow (/ init-stdd 2) 2))

  (tau-sq [_]
    (Math/pow (/ init-stdd 100) 2))

  (c-value [this s1 s2]
    (Math/sqrt (+ (* 2 (beta-sq this))
                  (variance s1)
                  (variance s2))))

  ;; Clearly broken
  (mean-additive-factors [this winner loser]
    (let [c (c-value this (stdd winner) (stdd loser))
          t (/ (- (mean winner) (mean loser)) c)
          e (/ draw c)
          m-winner (v winner t e)
          m-loser (v loser t e)
          normalized-variance (fn [s]
                             (/ (variance s) c))]
      [(* (normalized-variance (stdd winner)) m-winner)
       (* (normalized-variance (stdd loser)) m-loser)]))

  IRelativeRatingEngine
  (map->player [_ {:keys [id seed opts]}]
    (->TrueSkillPlayer id
                       (atom (or seed init-mean))
                       (atom (or (:stdd opts) init-stdd)))))

(defn trueskill-engine
  "Accepts optional keyword arguments to specify an initial stdd,
  mean skill value, and draw margin.

  Defaults to (trueskill-engine :stdd 25/3 :mean 25 :draw 0)"
  [& {:keys [stdd mean draw]}]
  (->TrueSkillEngine (or stdd 25/3) (or meant 25) (or draw 4)))
