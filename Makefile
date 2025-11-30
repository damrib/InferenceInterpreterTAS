build:
	erlc -o src\grammer_erl src\grammer_erl\token_lexer.xrl
	erlc -o src\grammer_erl src\grammer_erl\token_lexer.erl
	erlc -o src\grammer_erl src\grammer_erl\grammar_parser.yrl
	erlc -o src\grammer_erl src\grammer_erl\grammar_parser.erl
	gleam build

test:
	gleam test

clean:
	gleam clean
	rm src\grammer_erl\grammar_parser.erl
	rm src\grammer_erl\grammar_parser.beam
	rm src\grammer_erl\token_lexer.erl
	rm src\grammer_erl\token_lexer.beam

all:
	make build
	gleam run