%%% Universal tcp driver for inet and inet6 address families
%%%
%%% (C) YANDEX LLC, 2015
%%%
%%% The Source Code called "inet64_tcp" available at https://github.com/yandex/inet64_tcp
%%% is subject to the terms of the MIT License (MIT) (hereinafter referred to as the "License").
%%% The text of the License is the following:
%%%
%%% The MIT License (MIT)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
-module(inet64_tcp).

-author("Danil Zagoskin <stolen@yandex-team.ru>").

%% Load/unload the driver by starting/stopping the application
-export([start/0, stop/0]).

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

-define(INET(Address), {_,_,_,_} = Address).
-define(INET6(Address), {_,_,_,_,_,_,_,_} = Address).

-define(INET_MOD, inet_tcp).
-define(INET6_MOD, inet6_tcp).

%% Helper: determine a module by IP address tuple
mod_by_addr(?INET(_)) -> ?INET_MOD;
mod_by_addr(?INET6(_)) -> ?INET6_MOD.

%% Helper: determine a module by socket options (may return undefined)
get_specified_mod([]) ->
    undefined;
get_specified_mod([{AddrTag, Address}|_]) when AddrTag == ip; AddrTag == ifaddr ->
    mod_by_addr(Address);
get_specified_mod([inet|_]) ->
    ?INET_MOD;
get_specified_mod([{ipv6_v6only, true}|_]) ->
    ?INET6_MOD;
get_specified_mod([_|Opts]) ->
    get_specified_mod(Opts).


%% When getting addresses, gather responses from both inet and inet6 modules,
%% prefer inet6 over inet4
getaddr(Address) ->
    case ?INET6_MOD:getaddr(Address) of
        {ok, _} = GoodResult ->
            GoodResult;
        _ ->
            ?INET_MOD:getaddr(Address)
    end.

getaddr(Address, Timer) ->
    case ?INET6_MOD:getaddr(Address, Timer) of
        {ok, _} = GoodResult ->
            GoodResult;
        _ ->
            ?INET_MOD:getaddr(Address, Timer)
    end.

getaddrs(Address) ->
    Result6 = ?INET6_MOD:getaddrs(Address),
    Result4 = ?INET_MOD:getaddrs(Address),
    merge_getaddrs_results(Result6, Result4).

getaddrs(Address, Timer) ->
    Result6 = ?INET6_MOD:getaddrs(Address, Timer),
    Result4 = ?INET_MOD:getaddrs(Address, Timer),
    merge_getaddrs_results(Result6, Result4).

merge_getaddrs_results({ok, Addrs1}, {ok, Addrs2}) ->
    {ok, Addrs1 ++ Addrs2};
merge_getaddrs_results({ok, Addrs1}, _Error2) ->
    {ok, Addrs1};
merge_getaddrs_results(_Error1, {ok, Addrs2}) ->
    {ok, Addrs2};
merge_getaddrs_results(Error1, _Error2) ->
    Error1.


%% Select module by given ip address tuple
connect(Address, Port, Opts) ->
    connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, Timeout) ->
    Mod = mod_by_addr(Address),
    Mod:connect(Address, Port, Opts, Timeout).


%% Guess module by given options
listen(Port, Opts) ->
    case get_specified_mod(Opts) of
        undefined -> % By default listen IPv6, it will accept IPv4 connections too
            ?INET6_MOD:listen(Port, Opts);
        Mod ->
            Mod:listen(Port, Opts)
    end.

%% Guess module by given options, by default try inet6 first and on error try inet
%% It's not good, but fdopen/2 seems to be unused by OTP, and third-party apps may specify the inet|inet6 option
fdopen(FD, Opts) ->
    case get_specified_mod(Opts) of
        undefined ->
            try_fdopen([?INET6_MOD, ?INET_MOD], FD, Opts, {error, einval});
        Mod ->
            Mod:fdopen(FD, Opts)
    end.

try_fdopen([], _, _, Error) ->
    Error;
try_fdopen([Mod|Mods], FD, Opts, _Error) ->
    case Mod:fdopen(FD, Opts) of
        {ok, S} ->
            {ok, S};
        {error, _} = Error ->
            try_fdopen(Mods, FD, Opts, Error)
    end.

%%%
%%% Following functions have identical implementations in inet_tcp and inet6_tcp
%%%
accept(L) ->
    ?INET_MOD:accept(L).

accept(L, Timeout) ->
    ?INET_MOD:accept(L, Timeout).


close(Socket) -> ?INET_MOD:close(Socket).

send(Socket, Packet, Opts) -> ?INET_MOD:send(Socket, Packet, Opts).
send(Socket, Packet) -> ?INET_MOD:send(Socket, Packet).

recv(Socket, Length) -> ?INET_MOD:recv(Socket, Length).
recv(Socket, Length, Timeout) -> ?INET_MOD:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> ?INET_MOD:unrecv(Socket, Data).

shutdown(Socket, How) -> ?INET_MOD:shutdown(Socket, How).

controlling_process(Socket, NewOwner) -> ?INET_MOD:controlling_process(Socket, NewOwner).

getserv(PortOrName) -> ?INET_MOD:getserv(PortOrName).
