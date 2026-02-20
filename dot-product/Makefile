.PHONY: live
live:
	elm-live src/Main.elm --open --debug

.PHONY: check
check:
	elm make src/Main.elm --output=/dev/null

.PHONY: format
format:
	elm-format src/ --yes

.PHONY: review
review:
	elm-review
