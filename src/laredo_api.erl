%%% @author        Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc           the laredo api
%%%
%%% @end
%%% Created : 28 Jul 2013 by gordon@vixo.com
-module(laredo_api).

-include_lib("eunit/include/eunit.hrl").
-include("laredo.hrl").

%% debuggin
-export([
         testing/0
        ]).

-export([
         render_page/1,
         html/1,
         rst/1,
         markdown/1
        ]).

html(X) ->
    X.

rst(_X) ->
    exit("Not implemented yet").

markdown(_X) ->
    exit("Not implemented yet").

render_page(#webpage{} = WP) ->
    WP2 = add_defaults(WP),
    _HTML = render2(WP2).

render2(#webpage{title           = Title,
                 template        = Tmpl,
                 language        = Lang,
                 meta            = Meta,
                 viewport        = VP,
                 javascript_head = JH,
                 javascript_foot = JF,
                 css             = CSS,
                 webbody         = WebBody}) ->

    Page = Tmpl:get_page(),

    {JH2, JF2, CSS2, JSReload, JSRemoting} = consolidate(WebBody, JH, JF, CSS),
    io:format("JsH is ~p~nJsf is ~p~nCss is ~p~n", [JH2, JF2, CSS2]),

    Title2    = set_defaults(Tmpl, title, Title),
    Lang2     = set_defaults(Tmpl, langauge, Lang),
    Meta2     = set_defaults(Tmpl, meta, Meta),
    Viewport2 = set_defaults(Tmpl, viewport, VP),

    io:format("Title2 is ~p Meta2 is ~p Viewport2 is ~p~n",
              [Title2, Meta2, Viewport2]),

    JSReload2 = make_reload(JSReload),
    JSRem2    = make_remoting(JSRemoting),

    Head = make_head(Title2, Meta2, Viewport2, JH2, JSReload2, JSRem2 , CSS2),

    Body = render_body(Page, WebBody, []),

    Body2 = make_body(Body, JF2),

    io:format("Head is ~p~nBody2 is ~p~n", [Head, Body2]),

    _HTML = make_html(Head, Lang2, Body2).

render_body([], _WebBody, Acc) ->
    lists:flatten(lists:reverse(Acc));
render_body([#pagediv{id = ID, class = Class, contents = C} | T], Body, Acc)
  when is_record(C, pagediv) orelse
       is_record(C, pagespan) ->
    NewAcc = make_div(ID, Class, render_body(C, Body, [])),
    render_body(T, Body, [NewAcc | Acc]);
render_body([#pagespan{id = ID, class = Class, contents = C} | T], Body, Acc)
  when is_record(C, pagediv) orelse
       is_record(C, pagespan) ->
    NewAcc = make_span(ID, Class, render_body(C, Body, [])),
    render_body(T, Body, [NewAcc | Acc]);
render_body([#pagediv{id = ID, class = Class, contents = C} | T], Body, Acc) ->
    NewAcc = make_div(ID, Class, get_panel(C, Body)),
    render_body(T, Body, [NewAcc | Acc]);
render_body([#pagespan{id = ID, class = Class, contents = C} | T], Body, Acc) ->
    NewAcc = make_span(ID, Class, get_panel(C, Body)),
    render_body(T, Body, [NewAcc | Acc]).

make_span(none, none,  C) -> "<span>" ++ C ++ "</span>";
make_span(ID,   none,  C) -> "<span id='" ++ ID ++ "'>" ++ C ++ "</span>";
make_span(none, Class, C) -> "<span class='" ++ Class ++ "'>" ++ C ++ "</span>";
make_span(ID,   Class, C) -> "<span id-'" ++ ID ++ "' class='" ++
                               Class ++ "'>" ++ C ++ "</span>".

make_div(none, none,  C) -> "<div>" ++ C ++ "</div>";
make_div(ID,   none,  C) -> "<div id='" ++ ID ++ "'>" ++ C ++ "</div>";
make_div(none, Class, C) -> "<div class='" ++ Class ++ "'>" ++ C ++ "</div>";
make_div(ID,   Class, C) -> "<div id-'" ++ ID ++ "' class='" ++
                                Class ++ "'>" ++ C ++ "</div>".

get_panel(header,     #webbody{header     = R}) -> render_panel(R);
get_panel(navigation, #webbody{navigation = R}) -> render_panel(R);
get_panel(mainbody,   #webbody{mainbody   = R}) -> render_panel(R);
get_panel(search,     #webbody{search     = R}) -> render_panel(R);
get_panel(sidebar1,   #webbody{sidebar1   = R}) -> render_panel(R);
get_panel(sidebar2,   #webbody{sidebar2   = R}) -> render_panel(R);
get_panel(sidebar3,   #webbody{sidebar3   = R}) -> render_panel(R);
get_panel(adverts1,   #webbody{adverts1   = R}) -> render_panel(R);
get_panel(adverts2,   #webbody{adverts2   = R}) -> render_panel(R);
get_panel(adverts3,   #webbody{adverts3   = R}) -> render_panel(R);
get_panel(footer,     #webbody{footer     = R}) -> render_panel(R).

render_panel(#webpanel{content_type = Type, content = C}) ->
    laredo_api:Type(C).

make_head(Title, Meta, Viewport, JH2, JSReload, JSRemoting, CSS2) ->
    "<head>"       ++ "\n" ++
        Meta       ++ "\n" ++
        Title      ++ "\n" ++
        Viewport   ++ "\n" ++
        JH2        ++ "\n" ++
        JSReload   ++ "\n" ++
        JSRemoting ++ "\n" ++
        CSS2       ++ "\n" ++
        "</head>".

make_reload(List) ->
    string:join(List, "\n").

make_remoting(List) ->
    string:join(List, "\n").

make_body(Body, JF) ->
    "<body>" ++ "\n" ++
        Body ++ "\n" ++
        JF   ++ "\n" ++
        "</body>".

make_html(Head, Lang, Body) ->
    "<!DOCTYPE html>"
    "<html lang='" ++ Lang ++ "'>" ++ "\n" ++
        Head ++ "\n" ++
        Body.

consolidate(#webbody{
               header     = Hdr,
               navigation = Nav,
               mainbody   = Main,
               search     = Search,
               sidebar1   = Side1,
               sidebar2   = Side2,
               sidebar3   = Side3,
               adverts1   = Ad1,
               adverts2   = Ad2,
               adverts3   = Ad3,
               footer     = Footer
              }, JH, JF, CSS) ->
    Panels = [
              Hdr,
              Nav,
              Main,
              Search,
              Side1,
              Side2,
              Side3,
              Ad1,
              Ad2,
              Ad3,
              Footer
             ],
    JH2  = [JH  | [X || #webpanel{javascript_head = X} <- Panels]],
    JF2  = [JF  | [X || #webpanel{javascript_foot = X} <- Panels]],
    CSS2 = [CSS | [X || #webpanel{css             = X} <- Panels]],

    JSReload   = [X || #webpanel{javascript_reload   = X} <- Panels],
    JSRemoting = [X || #webpanel{javascript_remoting = X} <- Panels],

    io:format("JH2 is ~p JF2 is ~p CSS2 is ~p~n", [JH2, JF2, CSS]),
    {
      dedup_preserve_order(JH2, []),
      dedup_preserve_order(JF2, []),
      dedup_preserve_order(CSS2, []),
      JSReload,
      JSRemoting
    }.

dedup_preserve_order([], Acc) ->
    lists:reverse(Acc);
dedup_preserve_order([[] | T], Acc ) ->
    dedup_preserve_order(T, Acc);
dedup_preserve_order([H | T], Acc) ->
    case lists:keymember(H, Acc) of
        true  -> dedup_preserve_order(T, Acc);
        false -> dedup_preserve_order(T, [H | Acc])
    end.

add_defaults(#webpage{
                title           = Title,
                template        = Tmpl,
                meta            = Meta,
                viewport        = Viewport,
                javascript_head = JSHead,
                javascript_foot = JSFoot,
                css             = CSS,
                webbody         = Body
               } = WP) ->
    WP#webpage{
      title            = set_defaults(Tmpl, title,           Title),
      meta             = set_defaults(Tmpl, meta,            Meta),
      viewport         = set_defaults(Tmpl, viewport,        Viewport),
      javascript_head  = set_defaults(Tmpl, javascript_head, JSHead),
      javascript_foot  = set_defaults(Tmpl, javascript_foot, JSFoot),
      css              = set_defaults(Tmpl, css,             CSS),
      webbody          = add_panel_defaults(Body, Tmpl)
     }.

add_panel_defaults(#webbody{
                      header     = Hdr,
                      navigation = Nav,
                      mainbody   = Main,
                      search     = Search,
                      sidebar1   = Side1,
                      sidebar2   = Side2,
                      sidebar3   = Side3,
                      adverts1   = Ad1,
                      adverts2   = Ad2,
                      adverts3   = Ad3,
                      footer     = Footer
                     } = WP, Tmpl) ->
    WP#webbody{
      header     = set_defaults(Tmpl, header,     Hdr),
      navigation = set_defaults(Tmpl, navigation, Nav),
      mainbody   = set_defaults(Tmpl, mainbody,   Main),
      search     = set_defaults(Tmpl, search ,    Search),
      sidebar1   = set_defaults(Tmpl, sidebar1,   Side1),
      sidebar2   = set_defaults(Tmpl, sidebar2,   Side2),
      sidebar3   = set_defaults(Tmpl, sidebar3,   Side3),
      adverts1   = set_defaults(Tmpl, adverts1,   Ad1),
      adverts2   = set_defaults(Tmpl, adverts2,   Ad2),
      adverts3   = set_defaults(Tmpl, adverts3,   Ad3),
      footer     = set_defaults(Tmpl, footer,     Footer)
     }.

set_defaults(Mod, Fn, default)            -> erlang:apply(Mod, Fn, []);
set_defaults(_,   _,  none)               -> [];
set_defaults(_,   _,  Other)              -> Other.

testing() ->
    WP = #webpage{},
    Got = render_page(WP),
    io:format("Got is ~p~n", [Got]),
    ok.

%%%-----------------------------------------------------------------------------
%%%
%%% Unit Tests
%%%
%%%-----------------------------------------------------------------------------
basic_test_() ->
    WP = #webpage{},
    Got = render_page(WP),
    Expected = #webpage{},
    [?assertEqual(Expected, Got)].
