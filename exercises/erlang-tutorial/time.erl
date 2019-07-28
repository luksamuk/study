-module(time).
-export([swedish_date/0]).

date2str({Year, Month, Day}) ->
    TheYear = Year - (trunc(Year / 100) * 100),
    lists:flatten(
        io_lib:format("~2..0w~2..0w~2..0w",
            [TheYear, Month, Day])).

swedish_date() -> 
    date2str(date()).
