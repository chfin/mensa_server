# README

mensa_server is a Common Lisp server for the Socialunch app (mensa_app).
For the interface definition see api.txt.

## Installation:

This server is written in Common Lisp, so you need a CL implementation.
It currently has only been tested on SBCL, but it _should_ run on any conforming implementation, like CLISP or ClozureCL.

mensa_server defines some asdf systems which you can load easily with quicklisp.
The one you most probably want to use is `m-server` which uses hunchentoot as the http frontend.
To use teepeedee2 as a frontend, load `m-server-tpd2`.
Finally there is `m-server-core` which is the part shared by the two, you probably don't want to load it.

## Usage:

Both asdf systems define the package `m-server` which currently exports two functions:

* `start-http-server`
  Starts the server loop with http.
* `stop-server`
  Stops the server loop.

There will be a `start-https-server` function for https support, since all data is sent in plain text.

## Backends:

You can choose a backend by creating an instance of a backend class (probably a subclass of `base-backend`) and assigning it to `*backend*`.
Note, that there is only `plain-backend` currently which keeps all data in simple lists.
You can write your own backend by and defining methods on a certain backend type for the generics listed in `backend-api.lisp`.
You probably want to use your own class (which may be a subclass of `base-backend`) for the backend type, since you need to hold some meaningful state in the backend object, but since you can define methods on everithing in CL, you also can define a string-backend.

## Frontends:

You can choose between hunchentoot and teepeedee2 by loading the corresponding asdf system, but you can also write your own frontend:

* Create an asdf system for your frontend.
* Add `m-server-core` as a dependency.
* Define the functions `start-http-server` and `stop-server` to start and stop the server loop (`start-http-server` should not block).
* Define the macros `defget` and `defpost` which are used by `server.lisp` to define handlers for GET and POST requests.
* Make sure `server.lisp` is loaded after that (in the asdf system definition).

You can look at the `*-frontend.lisp` files, as well es the corresponding .asd files.