-module(inet64_tcp_app).

%% Define application callbacks
-behavior(application).
-export([start/2, stop/1]).

%% Also define a dummy supervisor callback to avoid creating a new module
-behavior(supervisor).
-export([init/1]).

%% Start: save the old tcp_module and replace it with ours
start(_, _) ->
    DefaultTcpMod = inet_db:tcp_module(),
    application:set_env(inet64_tcp, default_tcp_module, DefaultTcpMod),
    inet_db:set_tcp_module(inet64_tcp),
    supervisor:start_link({local, inet64_tcp}, ?MODULE, root).

%% Stop: restore the old tcp module
stop(_) ->
    {ok, DefaultTcpMod} = application:get_env(inet64_tcp, default_tcp_module),
    inet_db:set_tcp_module(DefaultTcpMod),
    ok.


%% Dummy application root supervisor
init(root) ->
    {ok, {{one_for_one, 1, 1}, []}}.
