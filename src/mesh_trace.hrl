%%
%% Compile-time mesh tracing, shared by meshtastic_server and
%% meshtastic_server_core. Disabled by default (expands to `ok`), so it is a
%% no-op in production builds and in the test suite. Enable with:
%%
%%   MESH_TRACE_ENABLED=1 ERL_COMPILER_OPTIONS='[{d,MESH_TRACE_ENABLED}]' \
%%       mix compile --force
%%
-ifdef(MESH_TRACE_ENABLED).
-define(MESH_TRACE(Format, Args), io:format(Format, Args)).
-else.
-define(MESH_TRACE(Format, Args), ok).
-endif.
