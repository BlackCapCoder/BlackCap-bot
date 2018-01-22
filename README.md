This is my bot for the [Vindinium](http://vindinium.org) game- a game where you compete against other AI's! It is based on [vindinium-starter-haskell](https://github.com/Herzult/vindinium-starter-haskell).

My bot is built from strategies that might fail `Strategy a = State -> Maybe a`, that are concatinated with `tryAll :: [Strategy a] -> Strategy a`. This turned out to be a pretty neat and tidy way of representing it.

I was also toying with the idea of representing the game map as a 2-dimensional zipper focusing on the player, where the location of each opponent would simply be a turtle path `[Dir]` from the player, but I unfortunately decided against it because 1, pathfinding was kinda a pain and 2, I figured that I might eventually want to find the shortest path between mines using algorithms for the traveling salesman problem, and that would definitly be a pain with a zipper. Oh well.
