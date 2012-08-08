(ns relative.trueskill.distribution
  (:refer-clojure :exclude [with-precision]))

(defprotocol IDistribution
  "Functions that return values given a distribution."
  (mean [this])
  (std-dev [this])
  (variance [this])
  (precision [this])
  (precision-mean [this])
  (pdf [this x])
  (cdf [this x])
  (icdf [this x]))

(defprotocol IUpdatableDistribution
  "Functions that return an updated distribution given a characterizing value."
  (with-mean [this mean])
  (with-std-dev [this std-dev])
  (with-variance [this variance])
  (with-precision [this precision]))

(defn erf
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

(defn ierf
  "Approximation of the inverse error function."
  [x]
  (let [numerators   [1,1,7,127,4369,34807,20036983,2280356863,
                      49020204823,65967241200001,15773461423793767,
                      655889589032992201,94020690191035873697,
                      655782249799531714375489,
                      44737200694996264619809969]
        denominators [1,1,6,90,2520,16200,7484400,681080400,
                      11675664000,12504636144000,2375880867360000,
                      78404068622880000,8910391798788480000,
                      49229914688306352000000,2658415393168543008000000,
                      476169110129306674080000000]
        cs (map / numerators denominators)]
    (reduce + (map-indexed (fn [i c]
                             (let [k (+ 1 (* 2 i))]
                               (* (/ c  k)
                                  (Math/pow (* (/ ( Math/sqrt Math/PI) 2) x) k))))
                           cs))))
