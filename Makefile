EBIN_DIR=ebin
SOURCE_DIR=src
INCLUDE_DIR=include
DOC_DIR=doc

ERLC_FLAGS=-W0 -Ddebug +debug_info
ERLC=erlc -I $(INCLUDE_DIR) -o $(EBIN_DIR) $(ERLC_FLAGS) $(SOURCE_DIR)
ERL=erl -I -pa ebin -noshell -eval

all: compile

compile:
	@rebar clean compile

docs:
	@rebar doc

clean:
	@rebar clean
	rm -rf erl_crash.dump 
	rm -rf $(DOC_DIR)/*.html
