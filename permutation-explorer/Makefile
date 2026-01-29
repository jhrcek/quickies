.PHONY: live
live:
	elm-live src/Main.elm --open -- --debug --output=elm.min.js

.PHONY: check
check:
	elm make src/Main.elm --output=/dev/null

.PHONY: test
test:
	elm-test

.PHONY: format
format:
	elm-format src/ tests/ --yes

.PHONY: review
review:
	elm-review
