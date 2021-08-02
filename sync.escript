%% Put this file into the "work" directory (who contains your projects)

-mode(compile).

main([]) ->
    PublicDirectoriesToSync =   ["ecompiler", "erlplayground", "notes", "unixfiles"],
    PrivateDirectoriesToSync =  ["isocube", "isocube-client", "isocube-client-elm"],
    lists:foreach( fun (Directory) -> gitPull(Directory, "origin") end,  PrivateDirectoriesToSync ),
    lists:foreach( fun (Directory) -> gitPull(Directory, "origin") end,  PublicDirectoriesToSync ),
    lists:foreach( fun (Directory) -> gitPull(Directory, "github") end,  PublicDirectoriesToSync ),
    io:get_line("synchonise finished, press ENTER to exit..."),
    ok.

gitPull(Directory, Respository) ->
    Return = os:cmd( "cd " ++ Directory ++ " && git pull " ++ Respository ++ " master --tags && cd .." ),
    io:format("~s  synchonise ~s (from ~s)~n~s~n", [lists:duplicate(60, "*"), Directory, Respository, Return]).