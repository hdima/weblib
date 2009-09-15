all: compile

compile: $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

ebin/%.beam: src/%.erl
	erlc -Wall -I include -pa ebin -o ebin/ $<

test: compile
	erl -noshell -pa ebin -s test_newslib test -s init stop

docs: compile
	erl -noshell -pa ebin -s test_newslib generate_docs -s init stop

clean:
	rm -f ebin/*.beam erl_crash.dump \
		src/doc/*.html src/doc/*.css src/doc/*.png src/doc/edoc-info
