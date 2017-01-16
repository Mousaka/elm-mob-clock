# elm-mob-clock
A timer that counts down to zero and makes a ring tone and then rotates a queue of given participants.
Great for mob programming!

Try it out here http://mousaka.github.io/elm-mob-clock/

Clock animation based on this example program: http://elm-lang.org/examples/time

Keybord shortcuts:
* `S` -> Start/Stop/Reset After finnish
* `0-9` -> Numbers goes in as a new time to the timer if timer not running 
To run:

* open index.html in browser

To build:

* `npm install elm`
* `elm-package install`
* `elm make src/Main.elm --output=count-down-clock.js`

last build step can be replaced with the following if you `chmod` build file first (unix)
* `./build`
