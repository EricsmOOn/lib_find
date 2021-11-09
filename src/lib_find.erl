%%----------------------------------------------------
%% @doc
%% Programming Erlang, Second Edition
%% @author Eric Wong
%% @end
%% Created : 2021-07-26 19:20 Monday
%%----------------------------------------------------
-module(lib_find).
-export([
        files/2
        ,files/3
        ,files/5
        ,find_erl/1
        ,find_hrl/1
    ]).

-include_lib("kernel/include/file.hrl").

-define(ERL_FILE_REG, "*.erl").
-define(HRL_FILE_REG, "*.hrl").
-define(EXCLUDED_DIRS, []).

-ifdef(debug).
-define(D(__A__, __B__), io:format(__A__, __B__)).
-else.
-define(D(__A__, __B__), ok).
-endif.
-define(I(__A__, __B__), io:format(__A__, __B__)).

%%----------------------------------------------------
%% 外部接口
%%----------------------------------------------------
%% @doc 默认递归搜索文件夹/全路径二进制输出
-spec files(string(), string()) -> [binary()].
files(Dir, Re) -> 
    files(Dir, Re, true).

-spec files(string(), string(), boolean()) -> [binary()].
files(Dir, Re, Flag) -> 
    files(Dir, Re, Flag, true, binary).

-spec files(string(), string(), boolean(), boolean(), binary | string) -> [binary()] | [string()].
files(Dir, Re, Recursive, FullPath, Type) ->
    Reg = xmerl_regexp:sh_to_awk(Re),
    files(Dir, Reg, #{recursive => Recursive, full_path => FullPath, type => Type}, []),
    receive
        R -> R
    end.

%% @doc 查到对应文件夹(们)下的所有hrl文件
-spec find_hrl([string()] | string()) -> [binary()].
find_hrl(Dirs = [Dir | _]) when is_list(Dir) ->
    find_hrl(Dirs, []);
find_hrl(Dir) ->
    ?D("[LIB_FIND] Searching ~ts...~n",[Dir]),
    Hrls = files(Dir, ?HRL_FILE_REG, true),
    ?D("[LIB_FIND] Finish Scan! Total ~w hrl Files~n", [erlang:length(Hrls)]),
    Hrls.

%% @doc 查到对应文件夹(们)下的所有erl文件
-spec find_erl([string()] | string()) -> [binary()].
find_erl(Dirs = [Dir | _]) when is_list(Dir) ->
    find_erl(Dirs, []);
find_erl(Dir) ->
    ?D("[LIB_FIND] Searching ~ts...~n",[Dir]),
    Erls = files(Dir, ?ERL_FILE_REG, true),
    ?D("[LIB_FIND] Finish Scan! Total ~w erl Files~n", [erlang:length(Erls)]),
    Erls.

%%----------------------------------------------------
%% 内部私有
%%----------------------------------------------------
files(Dir, Reg, Args, Acc) ->
    Me = self(),
    case file:list_dir(Dir) of
        {ok, Files} ->
            case get(child_num) of
                N when is_integer(N) ->
                    put(child_num, N + 1);
                _ ->
                    put(child_num, 1)
            end,
            erlang:spawn(fun() -> find_files(Files, Reg, Acc, Args#{dir => Dir, master => Me}) end);
        {error, E} ->
            ?I("[LIB_FIND] Error Dir: ~ts Reason: ~w~n",[Dir, E]),
            Acc
    end.

find_files([File | T], Reg, Acc0, Args = #{dir := Dir, full_path := FullPath, recursive := Recursive, type := Type}) ->
    FullName = filename:join([Dir, File]),
    Name = case FullPath of
        true ->
            FullName;
        _ ->
            File
    end,
    %% 兼容中文
    TransName = unicode:characters_to_binary(Name),
    case file_type(FullName) of
        regular ->
            case catch re:run(TransName, Reg, [{capture, none}]) of
                match ->
                    ?D("[~p:~p] Find ~ts~n",[?FILE, ?LINE, TransName]),
                    case Type of
                        list ->
                            find_files(T, Reg, [binary_to_list(TransName) | Acc0], Args);
                        _ ->
                            find_files(T, Reg, [TransName | Acc0], Args)
                    end;
                nomatch ->
                    find_files(T, Reg, Acc0, Args);
                _ ->
                    ?I("[LIB_FIND] Error FileName: ~ts~n",[TransName]),
                    find_files(T, Reg, Acc0, Args)
            end;
        directory -> 
            ?D("~n[~p:~p] Searching ~ts...~n",[?FILE, ?LINE, FullName]),
            case Recursive of
                true ->
                    files(FullName, Reg, Args, Acc0),
                    find_files(T, Reg, Acc0, Args);
                _ ->
                    find_files(T, Reg, Acc0, Args)
            end;
        error -> 
            find_files(T, Reg, Acc0, Args)
    end;
find_files([], _, A, #{master := Master}) ->
    case get(child_num) of
        N when is_integer(N) ->
            receive_wait(N, A, Master);
        _ ->
            receive_wait(0, A, Master)
    end.

receive_wait(0, Result, Master) -> 
    ?D("~w进程及子进程收集结束 结果返回到父进程~w 大小~w~n", [self(), Master, length(Result)]),
    Master ! Result;
receive_wait(N, Result, Master) ->
    receive
        R ->
            receive_wait(N - 1, R ++ Result, Master)
    end.

file_type(File) ->
    case excluded(File) andalso file:read_file_info(File) of
        {ok, Facts} ->
            case Facts#file_info.type of
                regular -> regular;
                directory ->
                    directory;
                _ -> error
            end;
        _ ->
            error
    end.

excluded(File) ->
    DName = filename:dirname(File),
    case lists:member(DName, ?EXCLUDED_DIRS) of
        false -> true;
        _ -> false
    end.

find_hrl([], Acc) -> Acc;
find_hrl([Dir | T], Acc) ->
    Hrls = find_hrl(Dir),
    find_hrl(T, Hrls ++ Acc).

find_erl([], Acc) -> Acc;
find_erl([Dir | T], Acc) ->
    Erls = find_erl(Dir),
    find_erl(T, Erls ++ Acc).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-endif.
