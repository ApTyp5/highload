-module(simple_http).
-author("aptyp potatoch").

%% Includes
-include("./types.hrl").
-include_lib("kernel/include/logger.hrl").

%% public API
-export([
	parse/1,
	addHeader/2,
	generate/3,
	generate/5,
	addBody/2,
	responseToString/1,
	responseToBinary/1
]).


-spec generate(
	ResponseVersion :: #response_version{},
	StatusCode :: integer(),
	ReasonPhrase :: string()
) ->
	#http_response{}.
generate(ResponseVersion, StatuseCode, ReasonPhrase) ->
	Response = #http_response{
		httpVersion = ResponseVersion,
		statusCode = StatuseCode,
		reasonPhrase = ReasonPhrase
	},
	addHeader(Response, #header{
		key = <<"Server">>,
		value = "Erlang server"
	}
	).

-spec generate(#response_version{},
	StatusCode :: integer(),
	ReasonPhrase :: string(),
	ContentType :: string(),
	ContentLength :: pos_integer()
) ->
	#http_response{}.
generate(ResponseVersion, StatusCode, ReasonPhrase, ContentType, ContentLength) ->
	addHeader(
		addHeader(
			generate(ResponseVersion, StatusCode, ReasonPhrase),
			#header{key = <<"Content-Length">>, value = atos(ContentLength)}
		),
		#header{key = <<"Content-Type">>, value = atos(ContentType)}
	).



-spec parse(Text :: string()) -> {ok, #http_request{}} | {error, Reason :: string()}.
parse(Text)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Text]),
	Lines = string:split(Text, "\r\n", all),
	treatHttpLines(Lines).


responseToString(Response) when is_record(Response, http_response)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Response]),
	lists:concat([
		"HTTP/",
		Response#http_response.httpVersion#response_version.major,
		".",
		Response#http_response.httpVersion#response_version.minor,
		" ",
		Response#http_response.statusCode,
		" ",
		Response#http_response.reasonPhrase,
		"\r\n",
		lists:concat(
			lists:map(
				fun(#header{key = Key, value = Value}) -> atos(Key) ++ ":" ++ atos(Value) ++ "\r\n" end,
				Response#http_response.headers
			)
		),
		"\r\n"
	]).


responseToBinary(Response)
		when is_record(Response, http_response)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Response]),
	list_to_binary(responseToString(Response));

responseToBinary(Any) -> ?LOG_ERROR("unexpected ~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Any]).


	-spec addHeader(#http_response{}, #header{}) -> #http_response{}.
addHeader(Response, HeaderTuple)
		when is_list(Response#http_response.headers)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Response, HeaderTuple]]),
	#http_response{
		httpVersion = Response#http_response.httpVersion,
		statusCode = Response#http_response.statusCode,
		reasonPhrase = Response#http_response.reasonPhrase,
		body = Response#http_response.body,
		headers = [HeaderTuple | Response#http_response.headers]
	};

addHeader(Response, HeaderTuple)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Response, HeaderTuple]]),
	#http_response{
		httpVersion = Response#http_response.httpVersion,
		statusCode = Response#http_response.statusCode,
		reasonPhrase = Response#http_response.reasonPhrase,
		body = Response#http_response.body,
		headers = [HeaderTuple]
	}.


-spec addBody(#http_response{}, string()) -> #http_response{}.
addBody(Response, Body)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Response, Body]]),
	#http_response{
		httpVersion = Response#http_response.httpVersion,
		statusCode = Response#http_response.statusCode,
		reasonPhrase = Response#http_response.reasonPhrase,
		headers = Response#http_response.headers,
		body = Body
	}.


%% private API

-spec treatHttpLines(Lines :: [string()]) -> ok | {error, Reason :: string()}.
treatHttpLines(Lines)
		when length(Lines) < 3
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, ["len < 3", Lines]]),
	{error, not_enough_lines};

treatHttpLines(Lines)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Lines]),
	[Method, RequestURI, HttpVersion] = parseReqestFLine(Lines),
	[Headers, Body] = parseRequestOtherLines(Lines),
	[{DecodedURI, _}] = uri_string:dissect_query(cutArgs(RequestURI)),
	Req = #http_request{
		method = Method,
		requestURI = DecodedURI,
		httpVersion = HttpVersion,
		headers = Headers,
		body = Body
	},
	treatHttpReqMethod(Req).


treatHttpReqMethod(Req)
		when Req#http_request.method == <<"POST">>
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, "post"]),
	{error, bad_request};

treatHttpReqMethod(Req)
		when Req#http_request.method == <<"HEAD">>
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, "head"]),
	treatHttpReqURI(Req);

treatHttpReqMethod(Req)
		when Req#http_request.method == <<"GET">>
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, "get"]),
	treatHttpReqURI(Req).


treatHttpReqURI(Req)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Req]),
	case string:find(Req#http_request.requestURI, "../") of
		nomatch ->
			URI = Req#http_request.requestURI,
			[CuttedURI | _] = string:split(URI, "?"),
			RestoredURI = restoreURI(CuttedURI),
			Headers = addContentTypeHeader(RestoredURI, Req#http_request.headers),
			HR = #http_request{
				requestURI = RestoredURI,
				method = Req#http_request.method,
				headers = Headers,
				body = Req#http_request.body,
				httpVersion = Req#http_request.httpVersion
			},
			{ok, HR};
		_ -> {error, bad_request}
	end.

addContentTypeHeader(URI, Headers)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [URI, Headers]]),
	Ext = lists:last(string:split(URI, ".", trailing)),
	case byte_size(Ext) of
		0 -> Headers;
		_ -> [#header{key = "Content-Type", value = getExtensionName(Ext)} | Headers]
	end.

getExtensionName(<<"html">>) -> "text/html";
getExtensionName(<<"css">>) -> "text/css";
getExtensionName(<<"jpeg">>) -> "image/jpeg";
getExtensionName(<<"jpg">>) -> "image/jpeg";
getExtensionName(<<"JPG">>) -> "image/jpeg";
getExtensionName(<<"png">>) -> "image/png";
getExtensionName(<<"PNG">>) -> "image/png";
getExtensionName(<<"svg">>) -> "image/svg";
getExtensionName(<<"gif">>) -> "image/gif";
getExtensionName(<<"js">>) -> "text/javascript";
getExtensionName(<<"txt">>) -> "text";
getExtensionName(<<"swf">>) -> "application/x-shockwave-flash".

restoreURI(URI)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, URI]),
	case [binary:last(URI)] of
		"/" -> <<URI/binary,<<"index.html">>/binary>>;
		_ -> URI
	end.

parseReqestFLine([FLine | _]) -> string:lexemes(FLine, " \t\n").
parseRequestOtherLines([_ | Other]) ->
	{HeaderLines, BodyLines} = lists:splitwith(fun(El) -> byte_size(El) > 0 end, Other),
	CleanedBodyLines = lists:filter(fun(El) -> byte_size(El) > 0 end, BodyLines),
	[parseHeaderLines(HeaderLines), parseBodyLines(CleanedBodyLines)].

parseHeaderLines(HeaderLines) ->
	lists:map(
		fun(Line) ->
			[Key, Val | _] = string:split(Line, ":"),
			#header{key = Key, value = Val} end,
		HeaderLines
	).

parseBodyLines(BodyLines) when length(BodyLines) > 0 -> lists:concat(BodyLines);
parseBodyLines(_) -> "".


atos(Smth) when is_list(Smth) -> Smth;
atos(Smth) when is_integer(Smth) -> integer_to_list(Smth);
atos(Smth) when is_atom(Smth) -> atom_to_list(Smth);
atos(Smth) when is_binary(Smth) -> binary_to_list(Smth).


cutArgs(Uri) -> lists:nth(1, (string:split(Uri, "?"))).