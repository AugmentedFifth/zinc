ts:
	tsc --outDir public/js public/ts/*.ts

hs:
	stack install

all:
	stack install
	tsc --outDir public/js public/ts/*.ts

.PHONY: ts hs all
