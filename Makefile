ts:
	tsc -p .

hs:
	stack install

all:
	stack install
	tsc -p .

release:
	stack install
	tsc -p .
	for f in public/js/*.js; do uglifyjs $$f -o $$f -m --config-file minify.json --ecma 8 --verbose --warn; done

.PHONY: ts hs all release
