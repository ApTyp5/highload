%%%-------------------------------------------------------------------
%%% @author arthur
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2020 22:29
%%%-------------------------------------------------------------------
-author("arthur").

-record(header, {
	key :: string(),
	value :: string()
}).
-record(http_request, {
	method :: string(),
	requestURI :: string(),
	httpVersion :: string(),
	headers = [] :: [#header{}],
	body :: string()
}).
-record(http_response, {
	httpVersion :: string(),
	statusCode :: integer(),
	reasonPhrase :: string(),
	headers = [] :: [#header{}],
	body
}).
-record(response_version, {
	major :: integer(),
	minor :: integer()
}).
