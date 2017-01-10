ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')
APP = src/js/main.js

$(APP): $(ELM_FILES)
	elm-make --yes src/elm/*.elm --output $(APP) --warn

clean-deps:
	rm -rf elm-stuff

clean:
	rm -f $(APP)
	rm -rf elm-stuff/build-artifacts
