-module(dreambook_web).

-export([
		 start_link/1,
		 stop/0,
		 loop/2
		]).
-define(DEFAULT_OPTS, [
					   {port, 8000},
					   {ip, "0.0.0.0"}
					   ]).
-define(LETTER_INDEX, ["А", "Б", "В", "Г", "Д", "E", "Ё", "Ж", "З", "И", "К", "Л", "М", "Н", "О", "П", "Р", "С", "Т", "У", "Ф", "Х", "Ц", "Ч", "Ш", "Щ", "Э", "Ю", "Я"]).

start_link(Options) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, "./priv/www")
           end,
	compile_dtls(),

	Options1 = mochilists:set_defaults(?DEFAULT_OPTS, Options),
	_DBserverName = proplists:get_value(db_server, Options1),
	Options2 = proplists:delete(db_server, Options1),

    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}] ++ Options2).

stop() ->
    mochiweb_http:stop(?MODULE).


loop(Req, DocRoot) ->
	case application:get_env(debug) of
		true ->
			compile_dtls();
		false ->
			ok;
		undefined ->
			ok
	end,
	try
		"/" ++ Path = Req:get(path),
		get_page(Path, Req, DocRoot)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, ""},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

get_page(Path, Req, DocRoot) ->
	try
		{ok, Uid} = auth(Req, false),
		io:format("[~p] Get: ~p~n", [Uid, Req:get(raw_path)]),
		{ok, Cont} = render_page(Path, Req),
		Req:respond({200, [{"Content-type", "text/html; charset=utf-8"}], [ Cont ]})
	catch
		error:function_clause ->
			Req:serve_file(Path, DocRoot);
	    Type:What ->
			Report = ["web request failed",
                      {path, ""},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})

	end.


render_page("", Req) ->
	render_page("index.html", Req);
render_page("iframe.html", Req) ->
	render_page("index.html", Req);
render_page("index.html", Req) ->
	base_tpl:render([
					 {raw_path, Req:get(raw_path)},
					 {footer, "footer"}
					]);
render_page("get_account", _) ->
	Accout = get_account(),
	account_tpl:render([
						{value, integer_to_list(Accout) ++ " " ++ get_correct_word_form(Accout, {"сон", "сна", "снов"}) }
					   ]);
render_page("get_main_window", _) ->
	main_tpl:render([
					 {index, ?LETTER_INDEX}
					]);
render_page("find_by_phrase", Req) ->
	Data = Req:parse_post(),
	Phrase = proplists:get_value("phrase", Data),
	Results = find_by_phrase(Phrase),
	io:format("~p ~p ~n", [Phrase, Results]),
	find_results_tpl:render([
							{results, Results},
							{index, ?LETTER_INDEX}
						   ]);
render_page("find_by_letter", Req) ->
	Data = Req:parse_post(),
	"letter_" ++ Letter = proplists:get_value("letter", Data),
	Results = find_by_letter(Letter),
	find_results_tpl:render([
							{results, Results},
							{index, ?LETTER_INDEX}
						   ]);
render_page("get_interpretation", Req) ->
	Data = Req:parse_post(),
	Word = proplists:get_value("word", Data),
	"dict_" ++ Dictionary = proplists:get_value("dictionary", Data),
	Interpretation = find_interpretation(Dictionary, Word),
	interpretation_tpl:render([
							{word, Word},
							{dictionary, Dictionary},
							{interpretation, Interpretation}
						   ]);
render_page("get_dictionaries", Req) ->
	Data = Req:parse_post(),
	"word_" ++ Word = proplists:get_value("word", Data),
	Dictionaries = find_dictionaries(Word),
	dictionaries_tpl:render([
							{word, Word},
							{dictionaries, Dictionaries}
						   ]).

compile_dtls() ->
	ok = erlydtl:compile("priv/templates/account.html", account_tpl),
	ok = erlydtl:compile("priv/templates/main.html", main_tpl),
	ok = erlydtl:compile("priv/templates/base.html", base_tpl),
	ok = erlydtl:compile("priv/templates/find_results.html", find_results_tpl),
	ok = erlydtl:compile("priv/templates/interpretation.html", interpretation_tpl),
	ok = erlydtl:compile("priv/templates/dictionaries.html", dictionaries_tpl),
	ok.

get_correct_word_form(Count, {_, _, Form}) when is_integer(Count), (Count rem 100) < 20 , (Count rem 100) > 10 ->
	Form;
get_correct_word_form(Count, {_, _, Form}) when is_integer(Count), (Count rem 10) =:= 0; (Count rem 10) > 4 ->
	Form;
get_correct_word_form(Count, {Form, _, _}) when is_integer(Count), (Count rem 10) =:= 1  ->
	Form;
get_correct_word_form(Count, {_, Form, _}) when is_integer(Count) ->
	Form.

find_by_phrase(_Phrase) ->
	todo,
	["word1", "word2"].

find_by_letter(_L) ->
	todo,
	["word3", "word4"].

find_interpretation(_Dictionary, _Word) ->
	todo,
	"test interpretation".

find_dictionaries(_Word) ->
	todo,
	["test dict1", "test dict2"].

get_account() ->
	todo,
	100511.

auth(Req, true) ->
	Params = mochiweb_util:parse_qs(Req:get(raw_path)),
	Uid = proplists:get_value(uid, Params),
	Hash = proplists:get_value(hash, Params),
	io:format("Hello: ~p ~p ~~n", [Uid, Hash]),
	todo,
	{ok, Uid};
auth(_Req, false) ->
	{ok, "42"}.
