-define(CHILD(Name, Params, Restart, Shutdown, Type), {
    Name,
    {Name, start_link, Params},
    Restart, Shutdown, Type,
    [Name]
}).

-define(GENERIC_WORKER(Name), ?CHILD(Name, [], permanent, 5000, worker)).

-define(GENERIC_SUPERVISOR(Name), ?CHILD(Name, [], permanent, 5000, supervisor)).