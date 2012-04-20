README
======

mensa_server is a Common Lisp server for the Socialunch app (mensa_app).
For the interface definition see api.txt.

Usage:
------

You can load mensa_server as an ASDF system called `m-server`.
Alternatively there is a system called `m-server-tpd2` which uses teepeedee2 as a frontend.
The package `m-server` currently exports two functions:

* `start-http-server`
  Starts the server loop with http.
* `stop-server`
  Stops the server loop.