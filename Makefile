OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all: doc psilo

preprocessor:
	ghc $(OPTS) --make preprocessor.hs -o preprocessor

psilo:
	cabal build psilo
	cp dist/build/psilo/psilo .

typed:
	ghc $(OPTS) --make src/*.{hs, lhs} -o psilo

clean: doc
	rm psilo
	cabal clean
