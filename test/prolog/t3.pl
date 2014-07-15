add_some_facts(Now):-
    add_some_facts(Now, 32, 10, [5, 15, 25, 30, 35, 40, 45, 50]).

add_some_facts(LastTime, HoursFrom, MinutesDiap, SomeValues):-
    date_diff(FirstDate, LastTime, hour, HoursFrom),
    generate_facts(FirstDate, LastTime,  MinutesDiap, SomeValues, SomeValues).

generate_facts(FirstDate, LastTime,  MinutesDiap, [Val|L], SomeValues):-
    date_diff( FirstDate, NextTime, minute, MinutesDiap),
    NextTime =< LastTime,
    assert(some_fact("some name", Val, NextTime)),
    generate_facts(NextTime, LastTime,  MinutesDiap, L, SomeValues).
generate_facts(FirstDate, LastTime,  MinutesDiap, [], SomeValues):-
    generate_facts(NextTime, LastTime,  MinutesDiap, SomeValues, SomeValues).
generate_facts(_, _,  _, _, _).

get_sum(Sum, Now):-
    Name = "some name",
    findall(Val, (some_fact( Name, Val, Time), date_diff(Time, Now, hour, Acum), Acum =< 24), Vals),
    sum(Vals, Sum).

sum( Vals, Sum):-
    sum( Vals, 0, Sum).
sum( [Val|Vals], Ac, Sum):-
    AcNext is Ac + Val,
    !,
    sum(Vals, AcNext, Sum).
sum([], Sum, Sum).

get_sum1(Sum, Now):-
    Name = "some name",
    get_sum1(Sum, Now, Name),
    acum_fact(Name, Sum).
get_sum1(Sum, Now, Name):-
    some_fact( Name, Val, Time),
    date_diff( Time, Now, hour, Acum),
    Acum =< 24,
    acum_val(Name, Acum),
    fail.
get_sum1(_Sum, _Now, _Name).

acum_val(Name, Acum):-
    retract(acum_fact(Name, PrevVal)),
    NextVal is PrevVal + Acum,
    assert(acum_fact(Name, NextVal)),
    !.
acum_val(Name, Val):-
    assert(acum_fact(Name, Val)).

test_all:-
    localtime(Now),
    add_some_facts(Now),
    get_sum(Sum, Now),
    get_sum1(Sum, Now).
