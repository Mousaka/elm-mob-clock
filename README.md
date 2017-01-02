# elm-mob-clock
A timer that counts down to zero and makes a ring tone and then rotates a queue of given participants.
Great for mob programming!

Try it out here http://mousaka.github.io/elm-mob-clock/

Clock animation based on this example program: http://elm-lang.org/examples/time

To run:

* open index.html in browser

To build:

* `npm install elm`
* `elm-package install`
* `elm make src/Main.elm --output=count-down-clock.js`

last build step can be replaced with the following if you `chmod` build file first (unix)
* `./build`
