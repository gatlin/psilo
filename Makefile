OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all: psilo

preprocessor:
	ghc $(OPTS) --make preprocessor.hs -o preprocessor

psilo:
	ghc $(OPTS) --make src/{Syntax,Lexer,Parser,Main}.hs -o psilo

clean:
	rm psilo
	rm src/*.{hi,o}
