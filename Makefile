
SRC=$(shell find src -name '*.hs')

CABAL=stack
FLAGS=--optimizations --pedantic --ghc-options '-with-rtsopts="-N -s"'
# FLAGS=--executable-profiling --library-profiling --ghc-options '-with-rtsopts="-N -p -s -h -i0.1"'

# INPUT=barth.weights
INPUT=barth.10000
# INPUT=barth.1000

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

test: build
	stack test

run: build
	stack exec -- barth-par -i$(INPUT) -onetwork.json

barth-par.ps: src/Main.hs
	make run
	hp2ps barth-par.hp
	open barth-par.ps

# docs:
# generate api documentation
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

package: test configure
	cabal check
	cabal sdist

upload: package
	cabal upload --check `ls dist/*.tar.gz | sort | tail -1`
	cabal upload `ls dist/*.tar.gz | sort | tail -1`

configure:
	cabal configure --package-db=clear --package-db=global --package-db=`stack path --snapshot-pkg-db` --package-db=`stack path --local-pkg-db`

install:
	stack install

tags: ${SRC}
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean

build:
	stack build $(FLAGS)

watch:
	ghcid "--command=stack ghci"

restart: distclean init build

rebuild: clean build

.PHONY: all init configure test run clean distclean build rebuild hlint watch tags
