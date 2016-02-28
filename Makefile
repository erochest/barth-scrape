SRC=$(shell find src -name '*.hs')

CABAL=stack
FLAGS=--pedantic
# FLAGS=--optimizations --executable-profiling --library-profiling --ghc-options '-threaded -with-rtsopts="-N -s -p"'
# FLAGS=--executable-profiling --library-profiling --ghc-options '-threaded -with-rtsopts="-N -p -s -h -i0.1"'

# INPUT=barth.weights
# INPUT=barth.10000
# INPUT=barth.5000
INPUT=barth.1000
# INPUT=barth.100

# CHUNK_SIZE=0
# CHUNK_SIZE=128
# CHUNK_SIZE=1024
# CHUNK_SIZE=1048576
CHUNK_SIZE=33554432

# URL=http://solomon.dkbl.alexanderstreet.com/cgi-bin/asp/philo/dkbl/volumes_toc.pl?&church=ON
URL=$(HOME)/p/barth/solomon.dkbl.alexanderstreet.com/cgi-bin/asp/philo/dkbl/volumes_toc.pl?&church=ON.html

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init

test:
	stack test barth-par:test:barth-par-specs $(FLAG)

run: build
	rm -f dump/*
	stack exec -- barth-par scrape --clean --output output/ \
		--root-file "$(URL)"

bench: build
	for n in 0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096; do echo $$n; \
		stack exec -- barth-par -i$(INPUT) -o/dev/null -c$$n 2>&1 | grep Total; \
		echo; \
		done

barth-par.ps: $(SRC) app/Main.hs
	make run
	hp2ps barth-par.hp
	open barth-par.ps

profile:
	stack bench $(FLAGS)

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
	stack install $(FLAGS)

tags: ${SRC}
	codex update

hlint:
	hlint *.hs app benchmark src specs

clean:
	stack clean
	-rm -rf *.hp *.prof *.ps *.aux
	codex cache clean

distclean: clean

build:
	stack build $(FLAGS)

watch:
	ghcid "--command=stack ghci --main-is barth-par:exe:barth-par"

reload:
	stack build --pedantic --file-watch \
		--exec "barth-par scrape --root-file $(URL) --output ./output --clean"

restart: distclean build

rebuild: clean build

.PHONY: all init configure test bench package upload configure install tags hlint clean distclean build watch restart rebuild
