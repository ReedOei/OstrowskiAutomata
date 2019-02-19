:- use_module(library(clpfd)).

take_while(_, [], []).
take_while(Pred, [H|T], [H|Rest]) :- call(Pred, H), take_while(Pred, T, Rest).
take_while(Pred, [H|_], []) :- not(call(Pred, H)).

count(_, [], 0).
count(X, [X|Xs], N) :-
    N #= N1 + 1,
    count(X, Xs, N1).
count(X, [Y|Xs], N) :-
    not(X = Y),
    count(X, Xs, N).

even(X) :- X #= 2*_.
odd(X) :- X #= 2*_ + 1.

same_length(Xs, Ys) :-
    length(Xs, L),
    length(Ys, L).

end_even_zeroes(Word) :-
    reverse(Word, Rev),
    take_while(=(0), Rev, Temp),
    length(Temp, L),
    even(L).

end_odd_zeroes(Word) :-
    reverse(Word, Rev),
    take_while(=(0), Rev, Temp),
    length(Temp, L),
    odd(L).

generate_base(_, _, N, [], A, _) :-
    A #> N.
generate_base(Frac, [], N, Places, A, B) :-
    A #=< N,
    generate_base(Frac, Frac, N, Places, A, B).
generate_base(Frac, [F|Fs], N, [A|Places], A, B) :-
    A #=< N,
    NewB #= B*F + A,
    generate_base(Frac, Fs, N, Places, B, NewB).

greater_than(N, A) :- A #> N.

generate_base(Frac, N, [1|PlaceVals]) :-
    generate_base(Frac, Frac, N, Temp, 0, 1),
    include(greater_than(1), Temp, PlaceVals).

generate_rep([], _, []).
generate_rep([D|Ds], N, [M|Ms]) :-
    M #= N div D,
    NewN #= N - D*M,
    generate_rep(Ds, NewN, Ms).

ostrowski(N, Frac, Rep) :-
    generate_base(Frac, N, PlaceVals),
    reverse(PlaceVals, RevPlaceVals),
    generate_rep(RevPlaceVals, N, Rep).

sturmian_gen(Frac, N, D) :-
    ostrowski(N, Frac, Rep),
    (
        end_even_zeroes(Rep) -> D = 0;
        end_odd_zeroes(Rep) -> D = 1
    ).

get_rep(Letter, Str, Rep, Char) :-
    count(Letter, Str, N),
    length(Rep, M),
    Mod #= N mod M,
    nth0(Mod, Rep, Char).

replace_sturmian(Frac, ZRep, ORep, N, D) :-
    N1 #= N - 1,
    findall(D, (between(1, N1, X), sturmian_gen(Frac, X, D)), Ds),
    (
        sturmian_gen(Frac, N, 0) -> get_rep(0, Ds, ZRep, D);
        sturmian_gen(Frac, N, 1) -> get_rep(1, Ds, ORep, D)
    ).

rep_word(Frac, ZRep, ORep, Limit, Word) :-
    findall(D, (between(1, Limit, N), replace_sturmian(Frac, ZRep, ORep, N, D)), Word).

gen_ostrowki_examples(Frac, ZRep, ORep, Limit, Examples) :-
    findall(Rep -> D,
    (
        between(1, Limit, N),
        ostrowski(N, Frac, Rep),
        replace_sturmian(Frac, ZRep, ORep, N, D)
    ), Examples).

