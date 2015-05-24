.PHONY: all deps update-deps get-deps compile test

REBAR_CONFIG = rebar.config

all: clean deps test

deps: get-deps update-deps
	@./rebar -C $(REBAR_CONFIG) compile

update-deps:
	@./rebar -C $(REBAR_CONFIG) update-deps

get-deps:
	@./rebar -C $(REBAR_CONFIG) get-deps

compile:
	@./rebar -C $(REBAR_CONFIG) compile skip_deps=true
	@./rebar -C $(REBAR_CONFIG) xref skip_deps=true

test: compile
	rm -rf .eunit
	./rebar -C $(REBAR_CONFIG) eunit skip_deps=true
