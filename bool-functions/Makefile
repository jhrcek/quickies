.PHONY: live
live:
	elm-live src/Main.elm --open -- --debug --output=elm.min.js

.PHONY: check
check:
	elm make src/Main.elm --output=/dev/null

.PHONY: format
format:
	elm-format . --yes

