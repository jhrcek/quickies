.PHONY: live
live:
	elm-live src/Main.elm --open --start-page=index.html -- --output=elm.js --debug

.PHONY: check
check:
	elm make src/Main.elm --output=/dev/null

.PHONY: format
format:
	elm-format src/ --yes

