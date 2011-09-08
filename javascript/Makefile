####
#### Testing and benchmarking for serverable/CLI JS.
####

TESTS = $(wildcard *.js.tests) \
 $(wildcard bbop/*.js.tests) \
 $(wildcard bbop/model/*.js.tests)
BENCHMARKS = $(wildcard _benchmark/*.js)
JS = smjs # or smjs, rhino
JSFLAGS = # Some require things like "-opt -1" in some cases (big GO tests)
JSENGINES = node smjs rhino

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
