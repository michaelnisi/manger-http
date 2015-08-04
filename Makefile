CLEAN_FILES += node_modules
NODE_VERSION = v0.12.7
NPM ?= npm
RMTREE ?= rm -rf
TAP := ./node_modules/.bin/tap

all: check test

.PHONY: check
check:
ifneq ($(shell node -v), $(NODE_VERSION))
$(error Expected Node $(NODE_VERSION))
endif

.PHONY: test
test: node_modules
	NODE_TEST=1 $(TAP) -b -C ./test/*.js

node_modules:
	$(NPM) install

.PHONY: clean
clean:
	$(RMTREE) $(CLEAN_FILES)
