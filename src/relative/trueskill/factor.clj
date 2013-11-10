(ns relative.trueskill.factor)

(defprotocol IFactor
  (up [factor] [factor idx])
  (down [factor] [factor idx]))

(defrecord PriorFactor [below]
  IFactor
  (down [factor] (down factor 0))
  (down [factor idx] (swap! (below idx) inc)))

(defrecord LikelihoodFactor [above below]
  IFactor
  (up [factor] (up factor 0))
  (up [factor idx] (swap! (above idx) dec))

  (down [factor] (down factor 0))
  (down [factor idx] (swap! (below idx) inc)))

(defrecord SumFactor [above below]
  IFactor
  (up [factor] (up factor 0))
  (up [factor idx] (swap! (above idx) dec))

  (down [factor] (down factor 0))
  (down [factor idx] (swap! (below idx) inc)))

(defrecord TruncateFactor [above]
  IFactor
  (up [factor] (up factor 0))
  (up [factor idx] (swap! (above idx) dec)))

(defn allup
  ([factors n]
     (doseq [f factors] (up f n)))
  ([factors]
     (doseq [f factors] (up f))))

(defn alldown [factors]
  (doseq [f factors] (down f)))
