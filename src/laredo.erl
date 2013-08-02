%%% @author        Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc           the laredo templating system
%%%
%%% @end
%%% Created : 28 Jul 2013 by gordon@vixo.com

-module(laredo).

-include("laredo.hrl").

-define(NOJAVASCRIPT, []).
-define(NOCSS,        []).

%% export the behaviour
-export([
         behaviour_info/1
        ]).

%% export for getting the default page structure
-export([
         get_page/0
        ]).

%% exports for making the page defaults
-export([
         title/0,
         language/0,
         meta/0,
         javascript_head/0,
         javascript_foot/0,
         css/0
        ]).

%% exports for making the panel defaults
-export([
         header/0,
         navigation/0,
         mainbody/0,
         search/0,
         sidebar1/0,
         sidebar2/0,
         sidebar3/0,
         adverts1/0,
         adverts2/0,
         adverts3/0,
         footer/0
        ]).


%%%-----------------------------------------------------------------------------
%%%
%%% Behaviour definition
%%%
%%% If you want to define your own template then create a new module
%%% that implements the laredo behaviour
%%%
%%%-----------------------------------------------------------------------------

behaviour_info(callbacks) ->
    [
     {get_page,        0},
     {title,           0},
     {language,        0},
     {meta,            0},
     {javascript_head, 0},
     {javascript_foot, 0},
     {css,             0},
     {header,          0},
     {navigation,      0},
     {mainbody,        0},
     {search,          0},
     {sidebar1,        0},
     {sidebar2,        0},
     {sidebar3,        0},
     {adverts1,        0},
     {adverts2,        0},
     {adverts3,        0},
     {footer,          0}
    ];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------------------
%%%
%%% Default Page
%%%
%%%-----------------------------------------------------------------------------
get_page() ->
    [
     #dv{contents = header},
     #dv{contents = mainbody},
     #dv{contents = footer}
    ].

%%%-----------------------------------------------------------------------------
%%%
%%% Page defaults
%%%
%%%-----------------------------------------------------------------------------
title() -> "<title>Laredo</title>".

language() -> "en".

meta() ->
    "<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />".

javascript_head() -> ?NOJAVASCRIPT.

javascript_foot() -> ?NOJAVASCRIPT.

css() -> ?NOCSS.
%%%-----------------------------------------------------------------------------
%%%
%%% Panel defaults
%%%
%%%-----------------------------------------------------------------------------
header() -> #webpanel{content_type = html,
                      content      = "Laredo"}.

navigation() -> none.

mainbody() -> #webpanel{content_type = html,
                        content      = "<h1>Hey!</h1><p>How you doing?</p>"}.

search() -> none.

sidebar1() -> none.

sidebar2() -> none.

sidebar3() -> none.

adverts1() -> none.

adverts2() -> none.

adverts3() -> none.

footer() -> #webpanel{content_type = html,
                      content      = "<div>This is my footer</div>"}.
