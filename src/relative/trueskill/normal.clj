(ns relative.trueskill.normal
  (:require [relative.trueskill.distribution :as dist]))

(defn- variance
  "The variance given a standard deviation."
  [stdd]
  (Math/pow stdd 2))

(defn- precision
  "The precision given a standard deviation."
  [stdd]
  (/ 1 (variance stdd)))

(defn- normal-pdf
  "Normal probability density function:

  The value of a normal distribution at a given point x, with the distribution
  described by a mean and standard deviation."
  [mean stdd x]
  (let [exp (- (* (/ 1 (* 2 (variance stdd)))
                  (Math/pow (- x mean) 2)))]
    (* (/ 1 (* stdd (Math/sqrt (* 2 Math/PI))))
       (Math/pow Math/E exp))))

(defn- normal-cdf
  "Normal cumulative density function."
  [mean stdd x]
  (let [xp (/ (- x mean) (* stdd (Math/sqrt 2)))]
    (* 1/2 (+ 1 (if (> xp 0)
                  (dist/erf xp)
                  (- (dist/erf (- xp))))))))

(defn- normal-icdf
  "Normal inverse cumulative density function."
  [mean stdd x]
  (+ mean (* (* stdd (Math/sqrt 2))
             (dist/ierf (- (* 2 x) 1)))))

(defrecord NormalDistribution [mean std-dev]
  relative.trueskill.distribution.IDistribution
  (mean [this]
    mean)
  (std-dev [this]
    std-dev)
  (variance [this]
    (variance std-dev))
  (precision [this]
    (precision std-dev))
  (precision-mean [this]
    (* (mean this) (precision this)))
  (pdf [this x]
    (normal-pdf mean std-dev x))
  (cdf [this x]
    (normal-cdf mean std-dev x))
  (icdf [this x]
    (normal-icdf mean std-dev x))

  relative.trueskill.distribution.IUpdatableDistribution
  (with-mean [this new-mean]
    (->NormalDistribution new-mean std-dev))
  (with-std-dev [this new-std-dev]
    (->NormalDistribution mean new-std-dev))
  (with-variance [this new-variance]
    (->NormalDistribution mean (Math/sqrt new-variance)))
  (with-precision [this new-precision]
    (->NormalDistribution mean (Math/sqrt (/ 1 new-precision)))))

(defn normal-distribution
  "Returns a NormalDistribution. Takes two optional arguments, a mean and a
  standard deviation, that will characterize the distribution. If no arguments
  are given, will return a distribution with mean 0 and standard deviation 1."
  ([] (->NormalDistribution 0 1))
  ([mean std-dev] (->NormalDistribution mean std-dev)))
