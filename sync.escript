%% Put this file into the "work" directory (who contains your projects)

-mode(compile).

git_pull(Dir, Repo) ->
    Ret = os:cmd( "cd " ++ Dir ++ " && git pull " ++ Repo ++ " master --tags && cd .." ),
    io:format("~s  sync ~s (from ~s)~n~s~n", [lists:duplicate(60, "*"), Dir, Repo, Ret]).

main([]) ->
    PubDirs = ["erlplayground", "ecompiler", "eddy", "notes", "unixfiles"],
    PrvDirs = ["isocube", "isocube-client"],
    lists:foreach(fun (Dir) -> git_pull(Dir, "origin") end, PrvDirs),
    lists:foreach(fun (Dir) -> git_pull(Dir, "origin") end, PubDirs),
    lists:foreach(fun (Dir) -> git_pull(Dir, "github") end, PubDirs),
    io:get_line("synchonise finished, press ENTER to exit..."),
    ok.

