# Relative

*Relative is a Clojure library supporting relative rating systems such as Elo or TrueSkill.*

```clj
[relative "0.1.2"]
```

### Use

Relative currently supports two rating engines: Elo (`relative.elo/elo-engine`) and TrueSkill (`relative.trueskill/trueskill-engine`).

Both engines implement the same protocol, `IRelativeRatingEngine`, and support the following functions:

#### `(player [engine map])`

Creates a player data structure that implements `IRelativelyRatedPlayer`. A call to `rating` should return that player's current rating.

#### `(match [engine winner loser])`

Represents a match played against two players, and returns a vector pair of updated players given the outcome of the match.

#### `(match-quality [engine p1 p2])`

This returns a match quality score given a hypothetical match between two players. A high quality match is considered to be a match where there is a high likelihood of a draw.

#### `(serialize [engine entities])`

Serializes a sequence of player entities into a string representation that could be stored.

#### `(resurrect [engine serialized])`

Returns a sequence of player entities based on the serialized form.

### Example

```clj
(ns elo-example
  (:require [relative.elo :as elo])
  (:use relative.rating))

;; We'll use the Elo engine
(def elo-engine (elo/elo-engine))

;; Create two players with default ratings of 1500.
(def player1 (player elo-engine {:id "Zach"}))
(def player2 (player elo-engine {:id "James"}))

(rating player1) ;; => 1500

;; player1 wins the first match.
(match elo-engine player1 player2) ;; => [{:id "Zach" :rating 1516.0}
                                   ;;     {:id "James" :rating 1484.0}]

(rating player1) ;; => 1516.0

;; player2 wins the second match.
(match elo-engine player2 player1) ;; => [{:id "James" :rating 1501.4695}
                                   ;;     {:id "Zach" :rating 1498.5305}]
```
