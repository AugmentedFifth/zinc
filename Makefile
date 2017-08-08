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
	for jsfile in public/js/*.js; do uglifyjs "$jsfile" -o "$jsfile" --compress --mangle --config-file minify.json --ecma 8 --verbose --warn; done

.PHONY: ts hs all release
