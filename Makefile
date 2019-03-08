#
# Makefile - build a manger-http release
#

_AWK := $(shell (which gawk >/dev/null && echo gawk) \
	|| (which nawk >/dev/null && echo nawk) \
	|| echo awk)
BRANCH := $(shell git symbolic-ref HEAD | $(_AWK) -F/ '{print $$3}')
ifeq ($(TIMESTAMP),)
	TIMESTAMP := $(shell date -u "+%Y%m%dT%H%M%SZ")
endif
_GITDESCRIBE := g$(shell git describe --all --long --dirty | $(_AWK) -F'-g' '{print $$NF}')
STAMP := $(BRANCH)-$(TIMESTAMP)-$(_GITDESCRIBE)

# node-gyp will print build info useful for debugging with V=1
export V=1

ifeq ($(shell uname -s),SunOS)
	TAR	?= gtar
else
	TAR	?= tar
endif

NAME = manger-http

RELEASE_TARBALL := $(NAME)-pkg-$(STAMP).tar.bz2
MANGER_HTTP_BUILD := build/$(NAME)

all: build

node_modules:
	- NPM_CONFIG_LOGLEVEL=info npm install

.PHONY: test
test: node_modules
	npm test

build: test
	mkdir -p $(MANGER_HTTP_BUILD)
	cp -r \
		conf.js \
		index.js \
		lib \
		node_modules \
		package.json \
		start.js \
		$(MANGER_HTTP_BUILD)
	(cd build && $(TAR) -jcf $(RELEASE_TARBALL) $(NAME))

.PHONY: clean
clean:
	rm -rf build
