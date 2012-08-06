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

(defn inverf
  "Approximation of the inverse error function"
  [x]
  (let [ numerators [1,1,7,127,4369,34807,20036983,2280356863,
                     49020204823,65967241200001,15773461423793767,
                     655889589032992201,94020690191035873697,
                     655782249799531714375489,
                     44737200694996264619809969]
        denominators [1,1,6,90,2520,16200,7484400,681080400,
                      11675664000,12504636144000,2375880867360000,
                      78404068622880000,8910391798788480000,
                      49229914688306352000000,2658415393168543008000000,
                      476169110129306674080000000,]
        cs (map / numerators denominators)]

    (reduce + (map-indexed (fn [i c]
                        (let [k (+ 1 (* 2 i))]
                          (* (/ c  k)
                             (Math/pow (* (/ ( Math/sqrt Math/PI) 2) x) k))))
                      cs))))
(defn cdf
  [x mean stdd]
  (let [xp (/ (- x mean) (* stdd (Math/sqrt 2)))]
    (* 1/2 (+ 1 (if (> xp 0)
                  (error-fn xp)
                  (- (error-fn (- xp))))))))
(defn invcdf
  [x mean stdd]
  (+ mean (* (* stdd  (Math/sqrt 2))
             (inverf (- (* 2 x) 1)))))

(defn gauss-mean-mult
  "Mean Additive Truncated Gaussian Function."
  [t e]
  (/ (pdf (- t e) 0 1)
     (cdf (- t e) 0 1)))

(defn gauss-stdd-mult
  "Gaussian Variance Multiplicative Function. Note that this
  function operates on a std dev not a variance as described
  in the Trueskill math paper."
  [t e]
  (let [multiplier (gauss-mean-mult t e)]
    (* multiplier (+ multiplier t (- e)))))

(defprotocol ITrueSkillPlayer
  (mean [_]
    "Average expected skill/rating of a player.")

  (stdd [_]
    "Standard deviation in skill of a player"))

(extend-type clojure.lang.PersistentHashMap
  ITrueSkillPlayer
  (mean [m] (:mean m))
  (stdd [m] (:stdd m))

  IRelativeRatedPlayer
  (rating [m] (- (:mean m) (* 3 (:stdd m)))))

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
    "Returns a pair of mean additive factors corresponding to the winner and loser.")

  (draw-margin [this prob]
    "Returns a draw margin from an input draw probability.")

  (variance-mult-factors [_ winner loser]))

(deftype TrueSkillEngine [init-stdd init-mean prob]
  ITrueSkillEngine
  (beta-sq [_]
    (Math/pow (/ init-stdd 2) 2))

  (tau-sq [_]
    (Math/pow (/ init-stdd 100) 2))

  (c-value [this s1 s2]
    (Math/sqrt (+ (* 2 (beta-sq this))
                  (variance s1)
                  (variance s2))))


  (mean-additive-factors [this winner loser]
    (let [c (c-value this (stdd winner) (stdd loser))
          t (/ (- (mean winner) (mean loser)) c)
          e (/ (draw-margin this prob) c)
          multiplier (gauss-mean-mult t e)
          normalized-variance (fn [s]
                             (/ (variance s) c))]
      [(* (normalized-variance (stdd winner)) multiplier)
       (* (normalized-variance (stdd loser)) multiplier)]))

  (draw-margin [this prob]
    (* (Math/sqrt 2) ;; assuming a one-on-one game, otherwise this is (+ (count team1) (count team2))
       (Math/sqrt (beta-sq this))
       (invcdf (/ (inc prob) 2) 0 1)))

  (variance-mult-factors [this winner loser]
    (let [c (c-value this (stdd winner) (stdd loser))
          t (/ (- (mean winner) (mean loser)) c)
          e (/ (draw-margin this prob) c)
          multiplier (gauss-stdd-mult t e)
          normalized-variance #(/ (variance %) (Math/pow c 2))]
      [(- 1 (* (normalized-variance (stdd winner)) multiplier))
       (- 1 (* (normalized-variance (stdd loser)) multiplier))]))

  IRelativeRatingEngine
  ;; map should contain :id and optional :mean and :stdd
  (player [_ map]
    (merge (hash-map :mean 25 :stdd 25/3) map))

  (match [this winner loser]
    (match this winner loser false))

  (match [this winner loser draw?]
    (let [[wmean lmean] (mean-additive-factors this winner loser)
          [wvar lvar] (variance-mult-factors this winner loser)
          update (fn [player mdiff vdiff]
                   (merge player {:mean (+ (mean winner) mdiff)
                                  :stdd (Math/sqrt (* (variance (stdd winner))
                                                      vdiff))}))]
      [(update winner wmean wvar)
       (update loser (- lmean) lvar)])))

(defn trueskill-engine
  "Accepts optional keyword arguments to specify an initial stdd,
  mean skill value, and draw margin.

  Defaults to (trueskill-engine :stdd 25/3 :mean 25 :prob 0)"
  [& {:keys [stdd mean prob]}]
  (->TrueSkillEngine (or stdd 25/3) (or mean 25) (or prob 0)))
