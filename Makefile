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

CHUNK=500
METADATA=yaml

# URL=http://solomon.dkbl.alexanderstreet.com/cgi-bin/asp/philo/dkbl/volumes_toc.pl?&church=ON
URL=$(HOME)/'c/solomon.dkbl.alexanderstreet.com/cgi-bin/asp/philo/dkbl/volumes_toc.pl?&church=ON.html'

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init

test:
	stack test barth-par:test:barth-par-specs $(FLAG)

open:
	open $(URL)

run: build
	rm -f dump/*
	stack exec -- barth-par scrape --clean --output output/ \
		--debug \
		--chunking paragraph \
		--root-file $(URL) --metadata $(METADATA)

chunks: build chunk-volume chunk-part chunk-chapter chunk-paragraph chunk-block chunk-chunk

chunk-volume: build
	stack exec -- barth-par scrape --output output/volume \
		--chunking volume --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-volume.log

chunk-part: build
	stack exec -- barth-par scrape --output output/part \
		--chunking part --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-part.log

chunk-chapter: build
	stack exec -- barth-par scrape --output output/chapter \
		--chunking chapter --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-chapter.log

chunk-paragraph: build
	stack exec -- barth-par scrape --output output/paragraph \
		--chunking paragraph --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-paragraph.log

chunk-block: build
	stack exec -- barth-par scrape --output output/block \
		--chunking block --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-block.log

chunk-chunk: build
	stack exec -- barth-par scrape --output output/chunk \
		--chunking $(CHUNK) --root-file $(URL) --metadata $(METADATA)
		# \ --debug &> dump/chunk-chunk.log

single/run.out: build
	stack exec -- barth-par page \
		--input    /Users/err8n/c/solomon.dkbl.alexanderstreet.com/cgi-bin/asp/philo/dkbl/getobject.pl?c.830:1.barth.html \
		--metadata $(METADATA) \
		--output   single/ \
		--chunking paragraph \
		&> single/run.out

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
	-rm -rf *.out
	codex cache clean

distclean: clean
	-rm -rf output
	-rm -rf single
	-rm -rf dump

build:
	stack build $(FLAGS)

watch:
	ghcid "--command=stack ghci barth-par"

reload:
	stack build --pedantic --file-watch \
		--exec "barth-par scrape --root-file $(URL) --output ./output --clean"

restart: distclean build

rebuild: clean build

.PHONY: all init configure test bench package upload configure install tags
.PHONY: hlint clean distclean build watch restart rebuild
.PHONY: chunk-volume chunk-part chunk-chapter chunk-paragraph chunk-block chunk-chunk chunks
