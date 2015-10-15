# Mars Rover TDD Code Kata

### The Scenario

Develop an api that moves a rover around on a grid.

Rules:

* You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.
* The rover receives a character array of commands.
* Implement commands that move the rover forward/backward (f,b).
* Implement commands that turn the rover left/right (l,r).
* Implement wrapping from one edge of the grid to another. (planets are spheres after all)
* Implement obstacle detection before each move to a new square. If a given sequence of commands encounters an obstacle, the rover moves up to the last possible point and reports the obstacle.

### Current Status

* rover.core is my hack on the train on the way in.
* rover.group is the product of a 60 minute breakfast code kata session. Live coding with a group of 10 ish mainly Java devs well versed in TDD and interested in Clojure and RDD.

The kata was centred on TDD "as if you really mean it" / "by the book".
