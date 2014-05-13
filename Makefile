# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild

all: deps compile dialyzer test frontend

# =============================================================================
# Rules to build the system
# =============================================================================

frontend:
	`rm -rf deps/xray_web_frontend/.git`
	`cp -r deps/xray_web_frontend .`

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc:
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit 
ct: compile 
	$(REBAR) skip_deps=true ct

test: compile eunit dialyzer

start: compile 
	erl -name erlog  -pa ebin  -pa deps/*/ebin   #-s reloader 

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) 

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions  -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile escript dialyzer test
