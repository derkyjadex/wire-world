SRCS = $(wildcard *.elm)

elm.js: $(SRCS)
	elm make Main.elm --yes --warn --output=elm.js
