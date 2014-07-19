all: 
	rebar clean get-deps compile

build:
	rebar clean compile

run:
	@erl -noshell -pa './ebin' -s media_uploader start

clean:
	rebar clean

test: build
	@mkdir ebin/tests
	@erlc -pa './ebin' -o 'ebin/tests' tests/*.erl
	@erl -noshell -pa './ebin' -pa './ebin/tests' -pa './deps/erlog/ebin' -s erlp3tags_tests tests -s init stop
