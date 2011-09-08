
TESTS = $(wildcard *.js.tests)
BENCHMARKS = $(wildcard benchmark/*.js)
#TESTS = bbop.core.js.tests bbop.test.js.tests
JS = smjs # or smjs, rhino
JSFLAGS = # Some require things like "-opt -1" in some cases (big GO tests)
JSENGINES = js smjs rhino

all:
	@echo "Default JS engine: $(JS)"
	@echo "All JS engines: $(JSENGINES)"
	@echo "Tests defined: $(TESTS)"
	@echo "Benchmarks defined: $(BENCHMARKS)"
	@echo "Try 'make test' or 'make benchmark'"

###
### Tests.
###

.PHONY: test $(TESTS)

test: $(TESTS)

$(TESTS):
	echo "trying: $@"
	$(JS) $(JSFLAGS) -f $@

###
### Benchmarks.
###

.PHONY: benchmark $(BENCHMARKS)

benchmark: $(BENCHMARKS)

$(BENCHMARKS):
	for e in $(JSENGINES); do \
           echo "Trying $$e..."; \
           $$e -f $@; \
        done
