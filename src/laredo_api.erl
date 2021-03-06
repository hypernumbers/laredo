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
                 javascript_head = JH,
                 javascript_foot = JF,
                 css             = CSS,
                 webbody         = WebBody}) ->

    Page = Tmpl:get_page(),

    {JH2, JF2, CSS2, JSReload, JSRemoting} = consolidate(WebBody, JH, JF, CSS),

    Title2    = set_defaults(Tmpl, title, Title),
    Lang2     = set_defaults(Tmpl, langauge, Lang),
    Meta2     = set_defaults(Tmpl, meta, Meta),

    JH3  = make_js(JH2),
    CSS3 = make_css(CSS2),
    Head = make_head(Title2, Meta2, JH3, CSS3),

    Body      = render_body(Page, WebBody),
    JF3       = make_js(JF2),
    JSReload2 = make_reload(JSReload),
    JSRem2    = make_remoting(JSRemoting),
    Body2     = make_body(Body, JF3, JSReload2, JSRem2),

    _HTML = list_to_binary(make_html(Head, Lang2, Body2)).


render_body(Page, WebBody) when is_record(WebBody, webbody) ->
    lists:flatten(render_b2(Page, WebBody, [])).

render_b2([], _WebBody, Acc) ->
    lists:flatten(lists:reverse(Acc));
render_b2([#dv{contents = C} = H | T], Body, Acc)
  when is_record(C, dv) orelse
       is_record(C, span) ->
    NewAcc = make_dv(H#dv{contents = render_b2(C, Body, [])}),
    render_b2(T, Body, [{seg, NewAcc} | Acc]);
render_b2([#span{contents = C} = H | T], Body, Acc)
  when is_record(C, dv) orelse
       is_record(C, span) ->
    NewAcc = make_span(H#span{contents = render_b2(C, Body, [])}),
    render_b2(T, Body, [NewAcc | Acc]);
render_b2([#dv{contents = L} = H | T], Body, Acc) when is_list(L) ->
    NewAcc = make_dv(H#dv{contents = render_b2(L, Body, [])}),
    render_b2(T, Body, [NewAcc | Acc]);
render_b2([#span{contents = L} = H | T], Body, Acc)
  when is_list(L) ->
    NewAcc = make_span(H#span{contents = render_b2(L, Body, [])}),
    render_b2(T, Body, [NewAcc | Acc]);
render_b2([#dv{contents = A} = H | T], Body, Acc) when is_atom(A) ->
    NewAcc = make_dv(H#dv{contents = get_panel(A, Body)}),
    render_b2(T, Body, [NewAcc | Acc]);
render_b2([#span{contents = A} = H | T], Body, Acc) when is_atom(A) ->
    NewAcc = make_span(H#span{contents = get_panel(A, Body)}),
    render_b2(T, Body, [NewAcc | Acc]).

make_span(#span{id      = Id,
                class   = Class,
                role    = Role,
                contents = C}) -> make_("span", Id, Class, Role, C).

make_dv(#dv{id      = Id,
            class   = Class,
            role    = Role,
            contents = C}) -> make_("div", Id, Class, Role, C).

make_(Tag, Id, Class, Role, Content) ->
    Inner = remove_empties([
                            make_2("id", Id),
                            make_2("class", Class),
                            make_2("role", Role)
                           ], []),
    case string:join(Inner, " ") of
        []   ->  "<" ++ Tag ++
                   ">" ++ Content ++ "</" ++ Tag ++ ">";
        List -> "<" ++ Tag ++ " " ++
                    List ++
                    ">" ++ Content ++ "</" ++ Tag ++ ">"
    end.

remove_empties([],       Acc) -> lists:reverse(Acc);
remove_empties([[] | T], Acc) -> remove_empties(T, Acc);
remove_empties([H  | T], Acc) -> remove_empties(T, [H | Acc]).

make_2(_, [])                         -> [];
make_2(K, V) when is_list(K) andalso
                  is_list(V)          -> K ++ "='" ++ V ++ "'".

get_panel(header,     #webbody{header     = R}) -> render_panel(header, R);
get_panel(navigation, #webbody{navigation = R}) -> render_panel(navigation, R);
get_panel(mainbody,   #webbody{mainbody   = R}) -> render_panel(mainbody, R);
get_panel(search,     #webbody{search     = R}) -> render_panel(search, R);
get_panel(sidebar1,   #webbody{sidebar1   = R}) -> render_panel(sidebar1, R);
get_panel(sidebar2,   #webbody{sidebar2   = R}) -> render_panel(sidebar2, R);
get_panel(sidebar3,   #webbody{sidebar3   = R}) -> render_panel(sidebar3, R);
get_panel(adverts1,   #webbody{adverts1   = R}) -> render_panel(adverts1, R);
get_panel(adverts2,   #webbody{adverts2   = R}) -> render_panel(adverts2, R);
get_panel(adverts3,   #webbody{adverts3   = R}) -> render_panel(adverts3, R);
get_panel(footer,     #webbody{footer     = R}) -> render_panel(footer, R).

render_panel(Panel, #webpanel{content_type = Type, content = C}) ->
    Rendered = laredo_api:Type(C),
    html:span(Rendered, [], "laredo-wrap-" ++ atom_to_list(Panel)).

make_head(Title, Meta, JH2, CSS2) ->
    List = [
            "<head>",
            Meta,
            Title,
            JH2,
            CSS2,
            "</head>"
           ],
    string:join(List, "\n").

make_js(List) -> make_j2(List, []).

make_j2([],      Acc) -> string:join(lists:reverse(Acc), "\n");
make_j2([H | T], Acc) -> NewAcc = "<script src='" ++ H ++ "'></script>",
                         make_j2(T, [NewAcc | Acc]).

make_css(List) -> make_c2(List, []).



make_c2([],      Acc) -> string:join(lists:reverse(Acc), "\n");
make_c2([H | T], Acc) -> NewA = "<link rel='stylesheet' href='" ++ H ++ "' />",
                         make_c2(T, [NewA | Acc]).

make_reload(List) ->
    List2 = [X ++ "();" || X <- List],
    List3 = [
             "<script>",
             "window.LAREDO.reload = function () {",
             string:join(List2, "\n"),
             "};"
             "</script>"
            ],
    string:join(List3, "\n").

make_remoting(List) ->
    List2 = make_r2(List, []),
    List3 = [
             "<script>",
             "window.LAREDO.handle_remoting = function (Msg) {",
             "switch (Msg.panel) {",
             List2,
             "};",
             "};",
             "</script>"
            ],
    string:join(List3, "\n").

make_r2([], Acc) ->
    string:join(lists:reverse(Acc), "\n");
make_r2([{Panel, Fun} | T], Acc) ->
    List = [
            "case '" ++ atom_to_list(Panel) ++ "':",
            Fun ++ "(Msg.body);",
            "break;"
           ],
    NewAcc = string:join(List, "\n"),
    make_r2(T, [NewAcc | Acc]).


make_body(Body, JF, JSReload, JSRemoting) ->
    List = [
            "<body>",
            Body,
            JF,
            JSReload,
            JSRemoting,
            "</body>"
           ],
    string:join(List, "\n").

make_html(Head, Lang, Body) ->
    Lang2 = "<html lang='" ++ Lang ++ "'>",
    List = [
            "<!DOCTYPE html>",
            Lang2,
            Head,
            Body
           ],
    string:join(List, "\n").

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
              {header,     Hdr},
              {navigation, Nav},
              {mainbody,   Main},
              {search,     Search},
              {sidebar1,   Side1},
              {sidebar2,   Side2},
              {sidebar3,   Side3},
              {adverts1,   Ad1},
              {adverts2,   Ad2},
              {adverts3,   Ad3},
              {footer,     Footer}
             ],
    JH2  = [JH  | [X || {_, #webpanel{javascript_head = X}} <- Panels]],
    JF2  = [JF  | [X || {_, #webpanel{javascript_foot = X}} <- Panels]],
    CSS2 = [CSS | [X || {_, #webpanel{css             = X}} <- Panels]],

    JSReload  = [X || {_, #webpanel{javascript_reload = X}} <- Panels],

    JSRemoting = [{P, X} || {P, #webpanel{javascript_remoting = X}} <- Panels],

    JH3  = dedup_preserve_order(preserve_flatten(JH2), []),
    JF3  = dedup_preserve_order(preserve_flatten(JF2), []),
    JF4  = remove_head_js_from_foot(JH3, JF3),
    CSS3 = dedup_preserve_order(preserve_flatten(CSS2), []),

    JSReload2  = preserve_flatten(JSReload),
    {JH3, JF4, CSS3, JSReload2, JSRemoting}.

preserve_flatten(List) ->
    preserve_flatten2(List, []).

preserve_flatten2([], Acc) ->
    Acc;
preserve_flatten2([H | T], Acc) ->
    NewAcc = preserve_2(H, Acc),
    preserve_flatten2(T, NewAcc).

%% don't reverse
preserve_2(List, Acc) ->
    preserve_3(lists:reverse(List), Acc).

%% don't reverse list
preserve_3([], Acc) ->
    Acc;
preserve_3([H | T], Acc) ->
    preserve_3(T, [H | Acc]).

remove_head_js_from_foot([], List) ->
    List;
remove_head_js_from_foot([H | T], List) ->
    NewList = case lists:member(H, List) of
                  true  -> recursive_delete(H, List);
                  false -> List
              end,
    remove_head_js_from_foot(T, NewList).

recursive_delete(H, List) ->
    case lists:member(H, List) of
        true  -> recursive_delete(H, lists:delete(H, List));
        false -> List
    end.

dedup_preserve_order([], Acc) ->
    lists:reverse(Acc);
dedup_preserve_order([[] | T], Acc ) ->
    dedup_preserve_order(T, Acc);
dedup_preserve_order([H | T], Acc) ->
    case lists:member(H, Acc) of
        true  -> dedup_preserve_order(T, Acc);
        false -> dedup_preserve_order(T, [H | Acc])
    end.

add_defaults(#webpage{
                title           = Title,
                template        = Tmpl,
                meta            = Meta,
                javascript_head = JSHead,
                javascript_foot = JSFoot,
                css             = CSS,
                webbody         = Body
               } = WP) ->
    WP#webpage{
      title            = set_defaults(Tmpl, title,           Title),
      meta             = set_defaults(Tmpl, meta,            Meta),
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
dedup_test_() ->
    List = [a, b, c, a, b, a, a, d, b, e, f],
    Dedup = dedup_preserve_order(List, []),
    [?_assertEqual([a, b, c, d, e, f], Dedup)].

remove_head_test_() ->
    Head = [a, b],
    Foot = [1, 2, b, 3, a, a, 4],
    Got = remove_head_js_from_foot(Head, Foot),
    [?_assertEqual([1, 2, 3, 4], Got)].

make_js_test_() ->
     List = [
             "/js/some.js",
              "http://example.com/js/other.js"
             ],
    Got = make_js(List),
    Expected = "<script src='/js/some.js'></script>\n" ++
        "<script src='http://example.com/js/other.js'></script>",
    [?_assertEqual(Expected, Got)].

make_css_test_() ->
     List = [
             "/css/some.css",
              "http://example.com/css/other.css"
             ],
    Got = make_css(List),
    Expected = "<link rel='stylesheet' href='/css/some.css' />\n" ++
        "<link rel='stylesheet' href='http://example.com/css/other.css' />",
    [?_assertEqual(Expected, Got)].

make_reload_test_() ->
     List = [
             "LAREDO.header.reload",
             "LAREDO.footer.reload"
             ],
    Got = make_reload(List),
    Expected = "<script>\n" ++
        "LAREDO.reload = function () {\n" ++
        "LAREDO.header.reload();\n" ++
        "LAREDO.footer.reload();\n" ++
        "};" ++
        "</script>",
    [?_assertEqual(Expected, Got)].

make_remoting_test_() ->
     List = [
             {header, "LAREDO.header.handle_msg"},
             {footer, "LAREDO.footer.handle_msg"}
             ],
    Got = make_remoting(List),
    Expected = "<script>\n" ++
        "LAREDO.handle_remoting = function (Msg) {\n" ++
        "switch (Msg.panel) {\n" ++
        "case 'header':\n" ++
        "LAREDO.header.handle_msg(Msg.body);\n" ++
        "break;\n" ++
        "case 'footer':\n" ++
        "LAREDO.footer.handle_msg(Msg.body);\n" ++
        "break;\n" ++
        "};\n" ++
        "</script>",
    [?_assertEqual(Expected, Got)].

