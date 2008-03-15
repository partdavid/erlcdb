%%
%% Copyright 2008 partdavid at gmail.com
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
-module(cdb_sup).

-behaviour(supervisor).

-export([
         start_link/1,
         start_shell/1
        ]).

-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

start_shell(StartArgs) ->
   {ok, Pid} = start_link(StartArgs),
   unlink(Pid),
   {ok, Pid}.

init(Appdir) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
    
    ChildSpecs =
	[
	 {cdb_server,
	  {cdb_server, start_link, [Appdir]},
	  permanent,
	  1000,
	  worker,
	  [cdb_server]}
	 ],
    {ok,{SupFlags, ChildSpecs}}.
