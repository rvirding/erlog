# Makefile for Erlog
# This simple Makefile uses rebar to compile/clean if it
# exists, else does it explicitly.

EBINDIR = ebin
SRCDIR = src
INCDIR = include
DOC_DIR = doc

ERLCFLAGS = -W1
ERLC = erlc

## The .erl, .xrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

all: compile docs

.PHONY: compile erlc_compile docs clean

## Compile using rebar if it exists else using make
compile:
	if which rebar > /dev/null; \
	then rebar compile; \
	else $(MAKE) $(MFLAGS) erlc_compile; \
	fi
	cp deps/jsx/ebin/*.* ebin

## Compile using erlc
erlc_compile: $(addprefix $(EBINDIR)/, $(EBINS))

rel: compile
	rebar generate

docs:

clean:
	rm -rf erl_crash.dump 
	rm -rf $(EBINDIR)/*.beam
	rm -rf $(DOC_DIR)/*.html
