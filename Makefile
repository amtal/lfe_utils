all: src/*.lfe include/*.lfe
	touch src/*.lfe;rebar compile && erl -pa ebin -s lfe_utils_app -noshell

