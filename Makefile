# Copyright (c) 2019 Robert Virding
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
#  limitations under the License.

# Makefile for Erlog
# Building from .xrl, .yrl and .erl

EBINDIR = ./ebin
SRCDIR = ./src
INCDIR = ./include
DOC_DIR = ./doc

ERLCFLAGS = -W1
ERLC = erlc

## The .erl, .xrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(COMP_OPTS) $(ERLCFLAGS) $<

.SECONDARY: $(XSRCS:.xrl=.erl)

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

.PHONY: all compile doc clean distclean pdf

all: compile

compile: comp_opts.mk $(addprefix $(EBINDIR)/, $(EBINS))

comp_opts.mk:
	escript get_comp_opts.escript

-include comp_opts.mk

doc:

start: compile
	erl -pa ebin

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -f test/*.beam
	- rm -f $(EBINDIR)/*.beam
	- rm -f *.beam
	- rm -f erl_crash.dump
	- rm -f comp_opts.mk

distclean: clean

rebuild: distclean compile
