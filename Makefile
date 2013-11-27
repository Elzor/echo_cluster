#!/usr/bin/env make -rRf

ERL      	?= erl +A 4 +K true
APP      	:= echo_cluster
CUR_VERS    := 0.0.1
REL_APP  	:= echo_cluster
REBAR    	?= rebar
DIALYZER 	?= dialyzer
MKDIR 		?= mkdir
RM    		?= rm
MV    		?= mv
CP    		?= cp
CD    		?= cd
SHELL 		?= sh
INCLUDE_DIR := include
SRC_DIR     := src
ERL_LIBS	:= apps:deps:plugins

APPS   := $(a)
SUITES := $(s)
SDIR := `pwd`

.SILENT: init_rel

.PHONY: deps

all: deps compile

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

# Hot patching
sync:
	exec erl -name sync@127.0.0.1 -pa $(SDIR)/apps/ $(SDIR)/deps/ $(SDIR)/apps/*/ebin $(SDIR)/deps/*/ebin $(SDIR)/deps/*/deps/*/ebin -boot start_sasl -s inets -s ssl  -eval "[code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || P <- code:get_path(), F <- filelib:wildcard(P ++ \"/*.beam\")], net_adm:ping('echo_cluster@127.0.0.1'), sync:patch()." -s sync -config sync.config -setcookie echo_cluster -- ok

# Test
#   usage examples:
#   make test                      :: Test release
#	make test s=simple_test        :: Test module in all applications
#	make test a=edht s=simple_test :: Test module in one application
#	make test a=edht               :: Test one application
test: compile
	$(REBAR) eunit apps=$(a) suites=$(s) tests=$(t) skip_deps=true

# Run
#   usage example
#   make run master || make run slave
run: rel
	@$(SHELL) ./rel/common/$(REL_APP)/bin/$(REL_APP) console $(filter-out $@, $(MAKECMDGOALS))


# Releases
rel:  all common_rel

# Common release for local run
common_rel:
	@$(RM) -rf rel/common/$(REL_APP)
	@$(REBAR) generate
