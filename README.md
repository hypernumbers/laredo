Status
======

This is not even alpha software.

Laredo
======

A remoting/templating system for use with cowboy to create dynamic html pages.

There are three records that define a webpage:
* ``#webpage{}``
* ``#webbody{}``
* ``#webpanel{}``

A page defines a webpage. It has javascript and css files defined and a body.

The body is defined by a series of panel records.

Individual panels might require certain javascript or css files to work. They may also register to receive update or remoting messages,

The contents of a panel can be in html, rst, text, markdown or other formats as they will be post-processed into the page.

The process is this:
* you register a laredo cowboy handler
* the handler passes the request through to your code
* your code returns a #webpage{} record
* the #webpage{} record is post-processed into a webpag which is returned to the user

The advantage of using Laredo is that in the background an erlang process for each url is maintained and the web page is connected to that erlang process by a remoting long-poll process. You can then push messages from within Erlang to a particular panel on a page for both all users and a specific user.

Usage
=====

When you are knocking up web apps you are mostly concerned with rendering the body, navigation and sidebars of the app, things like footers and headers you are not so bothered with.

With Laredo you simply build the panels you care about in the web handlers and the templating system fills in the ones you haven't with the site defaults.

The defaults are provided by a behaviour called laredo. The default laredo behaviour is in the module laredo.erl - but you can implement your own laredo behaviour in another module (see examples/ for examples) and then change the value of the template record to change the template

For Your Listening Pleasure
---------------------------

http://open.spotify.com/user/gordonguthrie/playlist/2OrBMd7TyuefFfU8CVvaCP