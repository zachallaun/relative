(ns relative.rating)

(defprotocol IRelativeRatingEngine
  (player [engine map]
    "Should return some kind data object representing a player for
    this particular engine.")

  (match [engine winner loser] [engine winner loser draw?]
    "Should return a pair of Player data objects, representing the
    updated ratings for the winner and loser.")

  (match-quality [engine p1 p2]
    "Returns a match quality score given two players.")

  (serialize [engine entities]
    "Should return a serialized string representation of Player
    entities that could be persisted.")

  (resurrect [engine serialized]
    "Should return a collection of Player entities generated from
    the serialized representation."))

(defprotocol IRelativeRatedPlayer
  (rating [player] "Should return the current rating of the player."))
