SRC=$(shell find src/ -name '*.elm')
BIN=node_modules/.bin

.PHONY : all build setup

all: setup build

build: docs/elm-mob-clock.js docs/notifications.js

docs/elm-mob-clock.js: $(SRC)
	$(BIN)/elm make src/Main.elm --output=docs/elm-mob-clock.js

docs/notifications.js: src/notifications.js
	cp src/notifications.js docs/notifications.js

setup: node_modules

node_modules: package.json
        npm --loglevel=warn install