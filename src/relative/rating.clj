(ns relative.rating)

(defprotocol IRelativeRatingEngine
  (player [_ map]
    "Should return some kind data object representing a player for
    this particular engine.")

  (match [_ winner loser] [_ winner loser draw?]
    "Should return a pair of Player data objects, representing the
    updated ratings for the winner and loser.")

  (serialize [_ entities]
    "Should return a serialized string representation of Player
    entities that could be persisted.")

  (resurrect [_ serialized]
    "Should return a collection of Player entities generated from
    the serialized representation."))

(defprotocol IRelativeRatedPlayer
  (rating [_] "Should return the current rating of the player."))
