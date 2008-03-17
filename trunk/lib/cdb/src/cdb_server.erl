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

-module(cdb_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_shell/0, start_shell/1,
         open_file/1, open_file/2, lookup/2, close/1, info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
   start_link(".").

start_link(Appdir) ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, Appdir, []).

start_shell() ->
   {ok, Pid} = start_link(),
   unlink(Pid),
   {ok, Pid}.
   
start_shell(Appdir) ->
   {ok, Pid} = start_link(Appdir),
   unlink(Pid),
   {ok, Pid}.

open_file(File) when is_atom(File) ->
   open_file(atom_to_list(File));
open_file(File) ->
   gen_server:call(?SERVER, {open, File}).

open_file(File, _Opts) ->
   open_file(File).
   
lookup(Tab, Key) ->
   gen_server:call(?SERVER, {lookup, Tab, Key}).

close(Tab) ->
   gen_server:cast(?SERVER, {close, Tab}).

info() ->
   gen_server:call(?SERVER, info).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Appdir) ->
   Libloc = filename:join([Appdir, "lib"]),
   Loaded = case erl_ddll:load_driver(Libloc, "cdb_drv") of
               ok -> ok;
               {error, already_loaded} -> ok;
               X -> {error, cant_load, Libloc, "cdb_drv", X}
            end,
   case Loaded of
      ok ->
         Port = open_port({spawn, "cdb_drv"}, [binary]),
         {ok, #state{port = Port}};
      Err -> Err
   end.
   

handle_call(info, _From, #state{port = Port}) ->
   {reply, [{port, Port}], #state{port = Port}};
handle_call({lookup, Tab, Key}, _From, #state{port = Port}) ->
   Port ! {self(), {command, term_to_binary({lookup, Tab, Key})}},
   receive
      {Port, {data, Data}} ->
         case binary_to_term(Data) of
            {error, I} ->
               {reply, {error, I}, #state{port = Port}};
            Answer when is_list(Answer) ->
               {reply, Answer, #state{port = Port}}
         end
   end;
handle_call({open, File}, _From, #state{port = Port}) ->
   Port ! {self(), {command, term_to_binary({open, File})}},
   receive
      {Port, {data, Data}} ->
         case binary_to_term(Data) of
            {ok, Fd} ->
               {reply, {ok, Fd}, #state{port = Port}};
            Err ->
               {reply, Err, #state{port = Port}}
         end
   end;
handle_call(Request, _From, State) ->
   Reply = {error, {undefined_request, Request}},
   {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({close, Tab}, #state{port = Port}) ->
   Port ! {self(), {command, term_to_binary({close, Tab})}},
   receive
      {Port, {data, Data}} ->
         case binary_to_term(Data) of
            ok -> {noreply, #state{port = Port}};
            _Err ->
               {noreply, #state{port = Port}}
         end
   end;
handle_cast(stop, #state{port = Port}) ->
   {stop, shutdown, #state{port = Port}};
handle_cast(_Msg, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, #state{port = Port}) ->
   {stop, {port_died, Port, Reason}, #state{port = Port}};
handle_info(_Info, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{port = _Port}) ->
   erl_ddll:unload_driver("cdb_drv"),
   ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
