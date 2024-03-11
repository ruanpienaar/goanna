.PHONY: compile get-deps test clean deep-clean rebar3

compile: rebar3 get-deps
	@./rebar3 compile
	@./rebar3 escriptize

get-deps:
	@./rebar3 get-deps

clean:
	@./rebar3 clean

deep-clean: clean
	@./rebar3 delete-deps

setup_dialyzer:
	dialyzer --build_plt --apps deps/*/ebin ./ebin
	dialyzer --add_to_plt ebin

dialyzer: compile
	@./rebar3 dialyzer

analyze: checkplt
	@./rebar3 skip_deps=true dialyze

buildplt: setup_dialyzer
	@./rebar3 skip_deps=true build-plt

checkplt: buildplt
	@./rebar3 skip_deps=true check-plt

rebar3:
	@ls rebar3 || wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
	
xref:
	@./rebar3 xref
        
check: compile xref dialyzer test