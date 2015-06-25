Magic IPv4/IPv6 TCP driver for Erlang
==========

Usage
-----
  * Interactively: ```inet64_tcp:start()```
  * Permanently: add ```inet64_tcp``` to your application's run-time dependencies
  
Demo
------
Assume you have some IPv4-only and IPv6-only hosts running some old Erlang code
which is unaware of inet6 address family.

We will simulate this with couple of entries in ```/etc/hosts```:
```
# Testing v4-only and v6-only loopback
127.0.0.1   local4
::1         local6
```

Let's start some dummy server and try to connect to it:
``` erlang
1> Acceptor = fun Acceptor(LSock) -> {ok, S} = gen_tcp:accept(LSock), io:format("Accepted ~p~n", [inet:peername(S)]), gen_tcp:close(S), Acceptor(LSock) end.
#Fun<erl_eval.30.90072148>
2> spawn(fun() -> {ok, L} = gen_tcp:listen(7890, []), Acceptor(L) end).
<0.35.0>
4> gen_tcp:connect("local4", 7890, []).
{ok,#Port<0.719>}
Accepted {ok,{{127,0,0,1},56356}}
6> gen_tcp:connect("local6", 7890, []).
{error,nxdomain}
7> gen_tcp:connect("local6", 7890, [inet6]).
{error,econnrefused}
```

As you see, using IPv6 requires options on both server and client side.

Let's start a new VM, load ```inet64_tcp``` driver and try all the stuff above:
``` erlang
1> inet64_tcp:start().
ok
2> Acceptor = fun Acceptor(LSock) -> {ok, S} = gen_tcp:accept(LSock), io:format("Accepted ~p~n", [inet:peername(S)]), gen_tcp:close(S), Acceptor(LSock) end.
#Fun<erl_eval.30.90072148>
3> spawn(fun() -> {ok, L} = gen_tcp:listen(7890, []), Acceptor(L) end).
<0.40.0>
4> gen_tcp:connect("local4", 7890, []).
{ok,#Port<0.750>}
Accepted {ok,{{0,0,0,0,0,65535,32512,1},56491}}
5> gen_tcp:connect("local6", 7890, []).
{ok,#Port<0.752>}
Accepted {ok,{{0,0,0,0,0,0,0,1},56492}}
```
As you see, the code started to work with IPv6-only addresses without any modifications.

If you stop the ```inet64_tcp``` app, things break again:
``` erlang
25> inet64_tcp:stop().

=INFO REPORT==== 25-Jun-2015::15:40:32 ===
    application: inet64_tcp
    exited: stopped
    type: temporary
ok
26> gen_tcp:connect("local6", 7890, []).
{error,nxdomain}
```
