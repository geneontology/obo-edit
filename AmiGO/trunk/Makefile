####
#### Testing and release procedures for AmiGO JS.
####
#### A report-mistakes-only testing run can be done as:
####   make test | grep -i fail
####

TESTS = $(wildcard javascript/lib/amigo/*.js.tests) \
 $(wildcard javascript/lib/amigo/data/*.js.tests) \
 $(wildcard javascript/lib/amigo/ui/*.js.tests)
#BENCHMARKS = $(wildcard _benchmark/*.js)
JS = rhino # or smjs
JSFLAGS = -opt -1
#JSENGINES = node smjs rhino
BBOP_JS = ../../../../git/bbop-js/

all:
	@echo "Default JS engine: $(JS)"
	@echo "See: http://wiki.geneontology.org/index.php/AmiGO_Manual:_Installation_2.0"
	@echo "for more details."
#	@echo "All JS engines: $(JSENGINES)"
#	@echo "Try make: 'test', 'docs', 'install', 'bundle', 'data', or 'release'"

###
### Tests.
###

.PHONY: test $(TESTS)

test: $(TESTS)

$(TESTS): bundle
	echo "trying: $@"
	cd $(@D) && $(JS) $(JSFLAGS) -f $(@F)

###
### Just the exit code results of the tests.
###

.PHONY: pass

pass:
	make test | grep -i fail; test $$? -ne 0

###
### Documentation for JavaScript and Perl modules.
###

.PHONY: docs

docs:
	naturaldocs --rebuild-output --input ./javascript/lib/amigo --project javascript/docs/.naturaldocs_project/ --output html javascript/docs/
	naturaldocs --rebuild-output --input ./perl/lib/ --project perl/docs/.naturaldocs_project/ --output html perl/docs

###
### Create exportable JS bundle. Only captures the statistics data if
### it has been generated separately.
###

.PHONY: bundle

bundle:
	./install -b

###
### Produce static statistics data files for landing page.
###

.PHONY: data

data:
	cd ./javascript/bin/; rhino -opt -1 ./generate_static_data.js --ann-source >../../staging/ann-source.dat; rhino -opt -1 ./generate_static_data.js --ann-evidence >../../staging/ann-evidence.dat; rhino -opt -1 ./generate_static_data.js --ann-overview >../../staging/ann-overview.dat

###
### Installation.
###

.PHONY: install

install: test docs
	./install -v -e -g

###
### Release: docs and bundle; then to an upload.
###

.PHONY: release

release: bundle docs
	s3cmd -P put javascript/staging/amigo*.js s3://bbop/jsapi/

###
### Ctags file for development.
###

.PHONY: tags

tags:
	@echo "Using BBOP-JS at: $(BBOP_JS)"
	rm -f TAGS
	find ./perl/lib ./javascript/lib/amigo $(BBOP_JS)/lib/bbop | grep ".*\.\(js\|pm\)$$" | xargs ctags -e -a

###
### Refresh the bundle in BBOP JS and install.
### Copy the bundle over for easy use by our tests.
###

.PHONY: refresh

refresh: tags bundle
	@echo "Using BBOP-JS at: $(BBOP_JS)"
	cd $(BBOP_JS); make bundle
	cp $(BBOP_JS)/staging/bbop.js ./_data
	./install -v -e -g

###
### Copy in some dummy values for use with testing.
###

.PHONY: dummy

dummy:
	cp conf/.dummy_values.yaml conf/amigo.yaml
