.PHONY: test

compile:
	@rebar3 compile

test:
	#@DEBUG=1 rebar3 as test clean,compile
	@rebar3 as test clean,compile
	@erl -pa _build/test/lib/**/ebin -cwd "`pwd`" -eval \
		"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
		-noshell
clean:
	@rebar3 clean
	@find . -name '*~' -printf " '%30P' deleted\n" -delete
	@rm -f rebar3.crashdump

