%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       A library for making html
%%%
%%% @end
%%% Created :  2 Aug 2013 by gordonguthrie@backawinner.gg

-module(html).

-export([
         header/1,
         header/2,
         header/3,

         dv/1,
         dv/2,
         dv/3,

         span/1,
         span/2,
         span/3,

         p/1,
         p/2,
         p/3,

         h1/1,
         h1/2,
         h1/3,

         h2/1,
         h2/2,
         h2/3,

         h3/1,
         h3/2,
         h3/3,

         h4/1,
         h4/2,
         h4/3,

         h5/1,
         h5/2,
         h5/3,

         h6/1,
         h6/2,
         h6/3,

         a/2,
         a/3,
         a/4,
         a/5,

         table/1,
         table/2,
         table/3,

         th/1,
         th/2,
         th/3,

         tr/1,
         tr/2,
         tr/3,

         td/1,
         td/2,
         td/3

        ]).

header([])      -> [];
header(Content) -> header(Content, []).

header(Content, Id) -> header(Content, Id, []).

header(Content, Id, Class)  -> make("header", Id, Class, Content).

dv([])      -> [];
dv(Content) -> dv(Content, []).

dv(Content, Id) -> dv(Content, Id, []).

dv(Content, Id, Class)  -> make("div", Id, Class, Content).

span([])      -> [];
span(Content) -> span(Content, []).

span(Content, Id) -> span(Content, Id, []).

span(Content, Id, Class)  -> make("span", Id, Class, Content).

p([])      -> [];
p(Content) -> p(Content, []).

p(Content, Id) -> p(Content, Id, []).

p(Content, Id, Class)  -> make("p", Id, Class, Content).

h1([])      -> [];
h1(Content) -> h1(Content, []).

h1(Content, Id) -> h1(Content, Id, []).

h1(Content, Id, Class)  -> make("h1", Id, Class, Content).

h2([])      -> [];
h2(Content) -> h2(Content, []).

h2(Content, Id) -> h2(Content, Id, []).

h2(Content, Id, Class)  -> make("h2", Id, Class, Content).

h3([])      -> [];
h3(Content) -> h3(Content, []).

h3(Content, Id) -> h3(Content, Id, []).

h3(Content, Id, Class)  -> make("h3", Id, Class, Content).

h4([])      -> [];
h4(Content) -> h4(Content, []).

h4(Content, Id) -> h4(Content, Id, []).

h4(Content, Id, Class)  -> make("h4", Id, Class, Content).

h5([])      -> [];
h5(Content) -> h5(Content, []).

h5(Content, Id) -> h5(Content, Id, []).

h5(Content, Id, Class)  -> make("h5", Id, Class, Content).

h6([])      -> [];
h6(Content) -> h6(Content, []).

h6(Content, Id) -> h6(Content, Id, []).

h6(Content, Id, Class)  -> make("h6", Id, Class, Content).

a(URL, Text) -> a(URL, Text, []).

a(URL, Text, Alt) -> a(URL, Text, Alt, []).

a(URL, Text, Alt, Id) -> a(URL, Text, Alt, Id, []).

a(URL, Text, Alt, Id, Class) when is_list(URL)  andalso
                                  is_list(Text) andalso
                                  is_list(Alt)  andalso
                                  is_list(Id )  andalso
                                  is_list(Class) ->
    "<a href='" ++ URL ++ "' " ++ id(Id) ++ " " ++ class(Class) ++ " " ++
        alt(Alt) ++ " >" ++ Text ++ "</a>".

table([])      -> [];
table(Content) -> table(Content, []).

table(Content, Id) -> table(Content, Id, []).

table(Content, Id, Class) -> make("table", Id, Class, Content).

th([])      -> [];
th(Content) -> th(Content, []).

th(Content, Id) -> th(Content, Id, []).

th(Content, Id, Class) -> make("th", Id, Class, Content).

tr([])      -> [];
tr(Content) -> tr(Content, []).

tr(Content, Id) -> tr(Content, Id, []).

tr(Content, Id, Class) -> make("tr", Id, Class, Content).

td([])      -> [];
td(Content) -> td(Content, []).

td(Content, Id) -> td(Content, Id, []).

td(Content, Id, Class) -> make("td", Id, Class, Content).


%%
%% Internal fns
%%

make(Tag, Id, Class, Content) when is_list(Tag)   andalso
                                   is_list(Id)    andalso
                                   is_list(Class) andalso
                                   is_list(Content) ->

    Inner = remove_empties([
                            id(Id),
                            class(Class)
                           ], []),
    case string:join(Inner, " ") of
        []   ->  "<" ++ Tag ++
                   ">" ++ Content ++ "</" ++ Tag ++ ">";
        List -> "<" ++ Tag ++ " " ++
                    List ++
                    ">" ++ Content ++ "</" ++ Tag ++ ">"
    end.

id(X)    -> make2("id", X).
class(X) -> make2("class", X).
alt(X)   -> make2("alt", X).

make2(_, [])                         -> [];
make2(Identifier, X) when is_list(X) -> Identifier ++ "='" ++ X ++ "'".

remove_empties([],       Acc) -> lists:reverse(Acc);
remove_empties([[] | T], Acc) -> remove_empties(T, Acc);
remove_empties([H  | T], Acc) -> remove_empties(T, [H | Acc]).
