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


etap: test/etap.beam test/util.beam test/gen_term.beam
	prove test/*.t


check: build etap


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean build etap check
