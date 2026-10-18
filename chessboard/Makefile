.PHONY: live
live: assets
	elm-live src/Main.elm --start-page=index.html --open -- --debug --output=elm.min.js

.PHONY: assets
assets:
	./fetch-assets.sh

.PHONY: check
check:
	elm make src/Main.elm --output=/dev/null

.PHONY: format
format:
	elm-format . --yes
