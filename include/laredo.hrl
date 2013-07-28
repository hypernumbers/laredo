%%% @author        Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc           core record definitions for laredo
%%%
%%% @end
%%% Created : 28 Jul 2013 by gordon@vixo.com

-type content_type() :: html | markdown.
-type javascript()   :: list().
-type css()          :: list().
-type jsfiles()      :: [javascript()].
-type cssfiles()     :: [css()].
-type module_name()  :: atom(). %% the name of a module that implements the laredo behaviour

-type panels()       :: header | navigation | mainbody | search | sidebar1 | sidebar2 | sidebar3 | advers1 | adverts2 | adverts3 | footer | none.

-record(webpanel,
        {
          content_type         = html :: content_type(),
          content              = [],
          javascript_head      = []   :: jsfiles(),
          javascript_foot      = []   :: jsfiles(),
          css                  = []   :: cssfiles(),
          javascript_reload    = []   :: javascript(),
          javascript_remoting  = []
         }).

-type webpanel()     :: #webpanel{}.

-record(webbody,
        {
          header     = default :: webpanel() | default | none,
          navigation = default :: webpanel() | default | none,
          mainbody   = default :: webpanel() | default | none,
          search     = default :: webpanel() | default | none,
          sidebar1   = default :: webpanel() | default | none,
          sidebar2   = default :: webpanel() | default | none,
          sidebar3   = default :: webpanel() | default | none,
          adverts1   = default :: webpanel() | default | none,
          adverts2   = default :: webpanel() | default | none,
          adverts3   = default :: webpanel() | default | none,
          footer     = default :: webpanel() | default | none
        }).

-type webbody()      :: #webbody{}.

-record(webpage,
        {
          title           = default,
          template        = laredo     :: module_name(),
          language        = "en",
          meta            = default,
          viewport        = default,
          javascript_head = default    :: jsfiles(),
          javascript_foot = default    :: jsfiles(),
          css             = default    :: cssfiles(),
          webbody         = #webbody{} :: webbody()
        }).


-record(pagespan,
        {
          id       = none :: list() | none,
          class    = none :: list() | none,
          contents = []   :: list()
        }).

%% can't be called div 'cos that is a reserved atom in Erlang
-record(pagediv,
        {
          id       = none :: list() | none,
          class    = none :: list() | none,
          contents = []   :: panels()
        }).
