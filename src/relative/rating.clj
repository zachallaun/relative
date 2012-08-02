(ns relative.rating)

(defprotocol IRelativeRatingEngine
  (map->player [_ player-map]
    "Should return some kind data object representing a Player.
    player-map keys:

    id:   (required) A unique identifier for the player.
    seed: (optional) A seed rating for the player.")

  (match! [_ winner loser] [_ winner loser draw?]
    "winner and loser should be Player data objects, and match!
    should update some sort of rating state to reflect the outcome."))

(defprotocol IRelativeRatedPlayer
  (rating [_] "Should return the current rating of the player."))
