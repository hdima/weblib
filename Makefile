all: compile

compile: $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

ebin/%.beam: src/%.erl
	erlc -Wall -pa ebin -o ebin/ $<

test: compile
	erl -noshell -pa ebin -s test_httplib test -s init stop

doc: compile
	erl -noshell -pa ebin -s test_httplib generate_docs -s init stop

clean:
	rm -f ebin/*.beam erl_crash.dump \
		src/doc/*.html src/doc/*.css src/doc/*.png src/doc/edoc-info
