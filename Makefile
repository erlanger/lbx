.PHONY: test

compile:
	@rebar3 compile

clean:
	@rebar3 clean
	@find . -name '*~' -printf " '%30P' deleted\n" -delete
	@rm -f rebar3.crashdump

test:
	#@DEBUG=1 rebar3 as test clean,compile
	@rebar3 as test clean,compile
	#Build the file myapp.app because myapp is generated dynamically
	DEPS_DIR=_build/test/lib/lbx/ebin; \
	cp "$$DEPS_DIR/lbx.app" "$$DEPS_DIR/myapp.app"; \
	sed -i -e 's/lbx/myapp/g' -e '4 a\ {mod,{myapp,[]}},' "$$DEPS_DIR/myapp.app"
	@erl -pa _build/test/lib/**/ebin -cwd "`pwd`" -eval \
		"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
		-noshell

eunit:
	#We have do do clean b/c lfe-compile plugin doesn't recognize include file change
	@rebar3 as test do clean,eunit
	@#rebar3 as test eunit --cover

doc:
	@echo lodox documentation generation is not working at the moment
	@rebar3 as doc  do compile,lfe lodox

