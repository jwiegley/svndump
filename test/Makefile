# -*- Makefile -*-

TESTRAW    = ../dist/build/test-raw/test-raw
DATA	   = cunit.dump
SUBCONVERT = subconvert
TIME       = average -n 3

all: $(TESTRAW) $(DATA)				\
	egrep-scan				\
	subconvert-scan				\
	subconvert-verify-scan			\
	haskell-scan

egrep-scan:
	@echo "=== Timing for a straight egrep of the file ==="
	@$(TIME) egrep -a '^(Revision-number|Node-path):' $(DATA) > /dev/null
	@echo

subconvert-scan:
	@echo "=== C++ quick raw scan (fseeking past file contents) ==="
	@$(TIME) $(SUBCONVERT) scan $(DATA)
	@echo

subconvert-verify-scan:
	@echo "=== C++ straight raw scan (all contents read) ==="
	@$(TIME) $(SUBCONVERT) --verify scan $(DATA)
	@echo

haskell-scan:
	@echo "=== Haskell straight raw scan (all contents read) ==="
	@$(TIME) $(TESTRAW) $(DATA) > /dev/null
	@echo

$(TESTRAW): test-raw.hs
	(cd ..; cabal configure --enable-tests)
	(cd ..; cabal build)

### Makefile ends here
