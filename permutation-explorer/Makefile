.PHONY: live
live:
	elm-live src/Main.elm --open -- --output=elm.min.js

.PHONY: test
test:
	elm-test

.PHONY: format
format:
	elm-format src/ tests/ --yes

.PHONY: review
review:
	elm-review
