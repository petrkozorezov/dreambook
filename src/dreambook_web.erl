-module(dreambook_web).

-export([
		 start_link/1,
		 stop/0,
		 loop/3
		]).

-define(DEFAULT_OPTS, [
					   {port, 8000},
					   {host, "0.0.0.0"},
					   {www, "www"},
					   {templates, "templates"}
					   ]).

-define(LETTER_INDEX, [
					   "А", "Б", "В", "Г", "Д", "Е", "Ж", "З", "И",
					   "К", "Л", "М", "Н", "О", "П", "Р", "С", "Т", "У",
					   "Ф", "Х", "Ц", "Ч", "Ш", "Щ", "Э", "Ю", "Я"
					  ]).

-define(TEMPLATES, [
					account,
					main,
					base,
					find_results,
					interpretation,
					dictionaries
				   ]).


start_link(Options) ->
	Options1 = mochilists:set_defaults(?DEFAULT_OPTS, Options),
	
	_DBserverName = proplists:get_value(db_server, Options1),
	WWW           = proplists:get_value(www,       Options1),
	Templates     = proplists:get_value(templates, Options1),
	Host          = proplists:get_value(host,      Options1),
	Port          = proplists:get_value(port,      Options1),

	WWWPath = code:priv_dir(dreambook) ++ "/" ++ WWW ++ "/",
	TemplatesPath = code:priv_dir(dreambook) ++ "/" ++ Templates ++ "/",
    Loop = {
	  ?MODULE,
	  loop,
	  [WWWPath,
	  TemplatesPath]
	},

	compile_dtls(TemplatesPath),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}, {ip, Host}, {port, Port}]).

stop() ->
    mochiweb_http:stop(?MODULE).


loop(Req, WWWPath, TemplatesPath) ->
	case application:get_env(debug) of
		{ok, true} ->
			compile_dtls(TemplatesPath);
		_ ->
			ok
	end,
	try
		"/" ++ Path = Req:get(path),
		get_page(Path, Req, WWWPath)
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
		{ok, UID} = auth(Req),
		io:format("[~p] Get: ~p~n", [UID, Req:get(raw_path)]),
		{ok, Cont} = render_page(Path, Req, UID),
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


%% authorized=1
%% application_key=CBAPNKABABABABABA
%% auth_sig=27aba74e1fa0c39e189c94efcc1ab3b2
%% api_server=http%3A%2F%2Fapi-sandbox.odnoklassniki.ru%3A8088%2F
%% apiconnection=CBAPNKABABABABABA_1309617616876
%% session_key=IGHGLHNJNQNWNVFDIGFFIFFLIFJOKLKKKIKNGLIFFGIGJJKPGEEBDAA
%% logged_user_id=7735397786349063815
%% sig=b9f3d4d2310820a4797fa9330a90539d
%% session_secret_key=d41d8cd98f00b204e9800998ecf8427e

render_page("", Req, UID) ->
	render_page("index.html", Req, UID);
render_page("iframe.html", Req, UID) ->
	render_page("index.html", Req, UID);
render_page("index.html", Req, UID) ->
	Account = get_account(UID),
	CommonParams = [
					{account_value, Account},
					{account_postfix, get_correct_word_form(Account, {"сон", "сна", "снов"})}
				   ],
	Params = case Req:get(raw_path) of
				 "/?" ++ ParamsString ->
					 mochiweb_util:parse_qs(ParamsString);
				 _ ->
					 []
			 end,
	base_tpl:render(CommonParams ++ Params);
render_page("get_main_window", _, _UID) ->
	main_tpl:render([
					 {index, ?LETTER_INDEX}
					]);
render_page("find_by_phrase", Req, UID) ->
	Data = Req:parse_post(),
	Phrase = proplists:get_value("phrase", Data),
	Results = find_by_phrase(UID, Phrase),
	io:format("~p ~p ~n", [Phrase, Results]),
	find_results_tpl:render([
							 {results, Results},
							 {index, ?LETTER_INDEX}
							]);
render_page("find_by_letter", Req, UID) ->
	Data = Req:parse_post(),
	"letter_" ++ Letter = proplists:get_value("letter", Data),
	Results = find_by_letter(UID, Letter),
	find_results_tpl:render([
							 {results, Results},
							 {index, ?LETTER_INDEX}
							]);
render_page("get_interpretation", Req, UID) ->
	Data = Req:parse_post(),
	Word = proplists:get_value("word", Data),
	"dict_" ++ Dictionary = proplists:get_value("dictionary", Data),
	Interpretation = find_interpretation(UID, Dictionary, Word),
	interpretation_tpl:render([
							   {word, Word},
							   {dictionary, Dictionary},
							   {interpretation, Interpretation}
							  ]);
render_page("get_dictionaries", Req, UID) ->
	Data = Req:parse_post(),
	"word_" ++ Word = proplists:get_value("word", Data),
	Dictionaries = find_dictionaries(UID, Word),
	dictionaries_tpl:render([
							 {word, Word},
							 {dictionaries, Dictionaries},
							 {paid_flag, is_in_history(UID, Word)}
							]).
%% render_page("get_account_change", Req) ->
%% 	%% long poll
%% 	start_polling(Req).


%% start_polling(Req) ->
%% 	TimerRef = erlang:send_after(?LONG_POLL_TIMEOUT, self(), ping),
%% 	proc_lib:hibernate(?MODULE, poll_next, [StaticManager, TypesMap, Response, TimerRef, PeerAddrStr]),
%% 	ok.

%% poll_next() ->
%% 	ok.

compile_dtls(TemplatesPath) ->
	lists:foreach(fun(E) ->
						  ok = erlydtl:compile(
								 TemplatesPath ++ atom_to_list(E) ++ ".html",
						         list_to_atom(atom_to_list(E) ++ "_tpl")
								)
				  end, ?TEMPLATES),
	ok.

get_correct_word_form(Count, {_, _, Form}) when is_integer(Count), (Count rem 100) < 20 , (Count rem 100) > 10 ->
	Form;
get_correct_word_form(Count, {_, _, Form}) when is_integer(Count), (Count rem 10) =:= 0; (Count rem 10) > 4 ->
	Form;
get_correct_word_form(Count, {Form, _, _}) when is_integer(Count), (Count rem 10) =:= 1  ->
	Form;
get_correct_word_form(Count, {_, Form, _}) when is_integer(Count) ->
	Form.

%%
%% core api callers
%%
is_fake() ->
	case application:get_env(fake) of
		{ok, true} ->
			true;
		_ ->
			false
	end.

find_by_phrase(UID, Phrase) ->
	find_by_phrase(UID, Phrase, is_fake()).
find_by_phrase(_, _, true) ->
	["word1", "word2"];
find_by_phrase(UID, Phrase, _) ->
	dreambook_server:find_keywords(UID, "%" ++ Phrase ++ "%").

find_by_letter(UID, L) ->
	find_by_letter(UID, L, is_fake()).
find_by_letter(_, _, true) ->
	[
	 "word3", "word4", "word4","word4","word4","word4","word4","word4","word4","word4","word4","word4",
	 "word3", "word4", "word4","word4","word4","word4","word4","word4","word4","word4","word4","word4"
	];
find_by_letter(UID, L, _) ->
	dreambook_server:find_keywords(UID, "" ++ L ++ "%").


find_interpretation(UID, Dictionary, Word) ->
	find_interpretation(UID, Dictionary, Word, is_fake()).
find_interpretation(_, _, _, true) ->
	"test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation test interpretation ";
find_interpretation(UID, Dictionary, Word, false) ->
	dreambook_server:find_meaning(UID, Dictionary, Word).

find_dictionaries(UID, Word) ->
	find_dictionaries(UID, Word, is_fake()).
find_dictionaries(_, _, true) ->
	["test dict1", "test dict2", "test dict1", "test dict2", "test dict1", "test dict2", "test dict1", "test dict2", "test dict1", "test dict2", "test dict1", "test dict2", "test dict1", "test dict2"];
find_dictionaries(UID, Word, _) ->
	dreambook_server:find_books(UID, Word).

get_account(UID) ->
	get_account(UID, is_fake()).
get_account(_, true) ->
	10;
get_account(UID, _) ->
	dreambook_server:get_balance(UID).

is_in_history(UID, Word) ->
	is_in_history(UID, Word, is_fake()).
is_in_history(_, _, true) ->
	false;
is_in_history(UID, Word, _) ->
	dreambook_server:is_in_history(UID, Word).


auth(Req) ->
	{ok, Auth} = application:get_env(auth),
	auth(Req, Auth).
auth(_Req, false) ->
	{ok, "42"};
auth(Req, _) ->
	Params = mochiweb_util:parse_qs(Req:get(raw_path)),
	UID = proplists:get_value(uid, Params),
	Hash = proplists:get_value(hash, Params),
	io:format("Hello: ~p ~p ~~n", [UID, Hash]),
	case social_net_api:validate_hash({UID, nil, Hash}) of
		ok ->
			{ok, UID};
		{error, Reason} ->
			{error, Reason}
	end.
