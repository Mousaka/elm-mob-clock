# elm-mob-clock
A timer that counts down to zero and makes a ring tone and then rotates a queue of given participants.
Great for mob programming!

Try it out here http://mousaka.github.io/elm-mob-clock/

Clock animation based on this example program: http://elm-lang.org/examples/time

For best user experience with keyboard shortcut use `enter` button instead of space for pause/resume/start/start next/add participant.

The clock automatically starts next persons turn after 30 seconds. Currently you cannot turn this off.

To run:

* open index.html in browser

To build:

* `npm install elm`
* `elm-package install`
* `elm make src/Main.elm --output=count-down-clock.js`

last build step can be replaced with the following if you `chmod` build file first (unix)
* `./build`
