-module(rebar3_boss_db_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, boss_db).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {namespace, ?NAMESPACE},
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 boss_db compile"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "Compile boss_db models."},
        {desc, "Compile boss_db models."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running boss_db...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
        Opts = rebar_app_info:opts(AppInfo),
        OutDir = rebar_app_info:ebin_dir(AppInfo),

        filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),

        BossDbOpts = proplists:unfold(rebar_opts:get(Opts, boss_db_opts, [])),

        SourceDir = option(model_dir, BossDbOpts),
        SourceExt = option(source_ext, BossDbOpts),
        TargetExt = ".beam",
        rebar_base_compiler:run(Opts, [],
            SourceDir,
            SourceExt,
            OutDir,
            TargetExt,
            fun(S, T, _C) ->
                compile_model(S, T, BossDbOpts, Opts)
            end,
            [{check_last_mod, true}, {recursive, option(recursive, BossDbOpts)}])
     end || AppInfo <- Apps],
    {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================


option(Opt, BossDbOpts) ->
    proplists:get_value(Opt, BossDbOpts, option_default(Opt)).


option_default(model_dir) -> "src/model";
option_default(out_dir)  -> "ebin";
option_default(source_ext) -> ".erl";
option_default(recursive) -> false;
option_default(compiler_options) -> [verbose, return_errors].


compiler_options(ErlOpts, BossDbOpts) ->
    set_debug_info_option(proplists:get_value(debug_info, ErlOpts), option(compiler_options, BossDbOpts)).


set_debug_info_option(true, BossCompilerOptions) ->
    [debug_info | BossCompilerOptions];
set_debug_info_option(undefined, BossCompilerOptions) ->
    BossCompilerOptions.


compile_model(Source, Target, BossDbOpts, RebarConfig) ->
    ErlOpts = proplists:unfold(rebar_opts:get(RebarConfig, erl_opts, [])),

    RecordCompilerOpts = [
        {out_dir, filename:dirname(Target)},
        {compiler_options, compiler_options(ErlOpts, BossDbOpts)}
    ],

    rebar_api:debug("Compiling boss_db model \"~s\" -> \"~s\" with options:~n    ~s",
                    [Source, Target, io_lib:format("~p", [BossDbOpts])]),

    case boss_record_compiler:compile(Source, RecordCompilerOpts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, RecordCompilerOpts)
    end.
