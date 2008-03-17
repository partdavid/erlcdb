%% @author partdavid at gmail.com
%% @copyright 2008 partdavid at gmail.com
%% @doc The cdb application implements an interface to Dan J. Bernstein's
%% cdb (constant database) files, using the 
%% [http://www.corpit.ru/mjt/tinycdb.html TinyCDB shared library].
%%
%% This file is part of erlcdb.
%%
%% erlcdb is free software: you can redistribute it and/or modify it under the
%% terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% erlcdb is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%% FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with erlcdb.  If not, see <http://www.gnu.org/licenses/>.
%%

-module(cdb).

-behaviour(application).

-export([
         start/0,
         start/2,
         shutdown/0,
         stop/1
        ]).

%% API
-export([
         open_file/1,
         open_file/2,
         lookup/2,
         close/1
        ]).

%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @doc Start the CDB application. The application must be started before
%% using the CDB file lookup functions.
start() ->
   application:start(cdb).

start(Type, _StartArgs) ->
   %% TODO find appdir
   case cdb_sup:start_link(".") of
      {ok, Pid} -> 
         {ok, Pid};
      Error ->
         Error
   end.

%% API functions

%% @spec open_file(filename()) -> {ok, TableId} + {error, Reason}
%%    Reason = term()
%%    TableId = term()
%% @doc Opens a CDB file and returns a table identifier. The table
%% identifier should be considered an opaque data structure (but
%% in this version is in fact a file descriptor).
open_file(File) ->
   cdb_server:open_file(File).

%% @spec open_file(filename(), Options) -> {ok, TableId} + {error, Reason}
%%    Reason = term()
%%    TableId = term()
%%    Options = term()
%% @doc The same as {@link open_file/1}, the options argument is ignored.
open_file(File, Opts) ->
   cdb_server:open_file(File, Opts).

%% @spec lookup(TableId::term(), Key::string()) -> [string()]
%% @doc Look up the provided string in the opened table id. A list of
%% strings is returned (or an empty list, if no string is found).
lookup(Tab, Key) ->
   cdb_server:lookup(Tab, Key).

%% @spec close(TableId::term())
close(Tab) ->
   cdb_server:close(Tab).

%% @doc Called to shutdown the cdb application.
%% @spec shutdown() -> ok 
%% @end
shutdown() ->
   application:stop(cdb).

stop(State) ->
    ok.
