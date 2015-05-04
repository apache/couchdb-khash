REBAR?=./rebar


all: build


clean:
	$(REBAR) clean
	rm -rf logs
	rm -f test/*.beam


distclean: clean
	git clean -fxd


build:
	$(REBAR) compile


eunit:
	$(REBAR) eunit


check: build eunit


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean build eunit check
