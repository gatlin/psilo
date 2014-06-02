OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all: doc psilo

preprocessor:
	ghc $(OPTS) --make preprocessor.hs -o preprocessor

psilo:
	ghc $(OPTS) --make src/{Evaluator,Syntax,Lexer,Parser,Main}.lhs -o psilo

clean: doc
	rm psilo
	rm src/*.{hi,o}
