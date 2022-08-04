REBAR3?=rebar3


.PHONY: all
# target: all - Makes everything
all: build


.PHONY: build
# target: build - Builds the project
build:
	$(REBAR3) compile


.PHONY: check
# target: check - Checks if project builds and passes all the tests
check: build eunit


.PHONY: clean
# target: clean - Removes build artifacts
clean:
	$(REBAR3) clean
	rm -rf _build priv
	@[ -f *.lock ] && rm *.lock || true


.PHONY: distclean
# target: distclean - Removes all unversioned files
distclean: clean
	git clean -fxd


.PHONY: eunit
# target: eunit - Runs eunit test suite
eunit:
	$(REBAR3) eunit


.PHONY: help
# target: help - Prints this help
help:
	@egrep "^# target:" Makefile | sed -e 's/^# target: //g' | sort
