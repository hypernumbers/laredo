%%% @author        Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc           an example laredo template
%%%
%%% @end
%%% Created : 28 Jul 2013 by gordon@vixo.com

-module(example_template).
-behaviour(laredo).

-include("laredo.hrl").

-define(NOJAVASCRIPT, []).
-define(NOCSS,        []).

%% export for getting the default page structure
-export([
         get_page/0
        ]).

%% exports for making the page defaults
-export([
         title/0,
         language/0,
         meta/0,
         viewport/0,
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

get_page() ->
    [
     #pagediv{contents = header},
     #pagediv{contents = mainbody},
     #pagediv{contents = footer}
    ].

%%%-----------------------------------------------------------------------------
%%%
%%% Page defaults
%%%
%%%-----------------------------------------------------------------------------
title() -> "<title>Laredo</title>".

language() -> "de".

meta() -> none.

viewport() -> none.

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
