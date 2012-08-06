# Relative

Relative is a Clojure library that supports relative rating systems such as Elo or Glicko. (Currently only the Elo rating system is implemented.)

### Basic Use

```clj
(ns elo-example
  (:require [relative.elo :as elo])
  (:use relative.rating))

;; We'll use the Elo engine
(def elo-engine (elo/elo-engine))

;; Create two players with default ratings of 1500.
(def player1 (player elo-engine {:id "Zach"}))
(def player2 (player elo-engine {:id "Nick"}))

(rating player1) ;; => 1500

;; player1 wins the first match.
(match elo-engine player1 player2) ;; => [{:id "Zach" :rating 1516.0}
                                   ;;     {:id "Nick" :rating 1484.0}]

(rating player1) ;; => 1516.0

;; player2 wins the second match.
(match elo-engine player2 player1) ;; => [{:id "Nick" :rating 1501.4695}
                                    ;;     {:id "Zach" :rating 1498.5305}]
```
