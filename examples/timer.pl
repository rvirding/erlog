%%% -*- mode: prolog -*-

cpu_time(Time) :-
	ecall(erlog_demo:efunc(erlang:statistics(runtime)), R),
	R =.. [T|_],
	Time is T * 0.001.

cpu_time(Goal, Duration) :-
	cpu_time(Before),
	( Goal -> true ; true ),
	cpu_time(After),
	Duration is After - Before.

wall_clock(Time) :-
	ecall(erlog_demo:efunc(erlang:statistics(wall_clock)), R),
	R =.. [T|_],
	Time is T * 0.001.

wall_clock(Goal, Duration) :-
	wall_clock(Before),
	( Goal -> true ; true ),
	wall_clock(After),
	Duration is After - Before.
