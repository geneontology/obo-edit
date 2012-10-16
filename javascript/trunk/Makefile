####
#### Testing, benchmarking, and release procedures for serverable/CLI JS.
####
#### A report-mistakes-only testing run can be done as:
####   make test | grep -i fail
####

TESTS = $(wildcard lib/*.js.tests) \
 $(wildcard lib/bbop/*.js.tests) \
 $(wildcard lib/bbop/contrib/*.js.tests) \
 $(wildcard lib/bbop/golr/*.js.tests) \
 $(wildcard lib/bbop/golr/manager/*.js.tests) \
 $(wildcard lib/bbop/parse/*.js.tests) \
 $(wildcard lib/bbop/model/*.js.tests) \
 $(wildcard lib/bbop/widget/*.js.tests)
BENCHMARKS = $(wildcard _benchmark/*.js)
JS = smjs # or smjs, rhino
JSFLAGS = # Some require things like "-opt -1" in some cases (big GO tests)
JSENGINES = node smjs rhino

all:
	@echo "Default JS engine: $(JS)"
	@echo "All JS engines: $(JSENGINES)"
	@echo "Tests defined: $(TESTS)"
	@echo "Benchmarks defined: $(BENCHMARKS)"
	@echo "Try make: 'test', 'benchmark', 'docs', 'bundle', or 'release'"

###
### Tests.
###

.PHONY: test $(TESTS)

test: $(TESTS)

$(TESTS):
	echo "trying: $@"
	cd $(@D) && $(JS) $(JSFLAGS) -f $(@F)

###
### Benchmarks.
###

.PHONY: benchmark $(BENCHMARKS)

benchmark: $(BENCHMARKS)

$(BENCHMARKS):
	for e in $(JSENGINES); do \
           echo "Trying engine: $$e"; \
           $$e -f $@; \
        done

###
### Documentation.
###

.PHONY: docs

docs: bundle
	naturaldocs --rebuild-output --input lib/bbop/ --project docs/.naturaldocs_project/ --output html docs/
#	naturaldocs --rebuild-output --input lib/bbop/ --input bin/ --project docs/.naturaldocs_project/ --output html docs/

###
### Create exportable JS bundle.
###

.PHONY: bundle

bundle:
	./scripts/release-js.pl -v -i scripts/release-file-map.txt -o staging/bbop.js -n bbop -d lib/bbop -r 0.9

###
### Release: docs and bundle; then to an upload.
###

.PHONY: release

release: bundle docs
	s3cmd -P put staging/bbop*.js s3://bbop/jsapi/

###
### Refresh test data from AmiGO.
### TODO/BUG: Sorry that it's hard-coded; it's secret for now...
###

.PHONY: refresh

refresh:
	/bin/cp ../../AmiGO/trunk/javascript/lib/amigo/data/*.js ./_data/
