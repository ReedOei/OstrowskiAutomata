#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(clpfd)).
:- use_module(library(achelois)).

main(Argv) :-
    Argv = [File, AlphabetStr, OutputFile] ->
        term_to_atom(Alphabet, AlphabetStr),
        examples_from_file(File, Examples),
        examples(Alphabet, Examples, States),
        write_walnut_automata(OutputFile, Alphabet, States);

    writeln('Usage: ./automata.pl EXAMPLE_FILE ALPHABET OUTPUT_FILE'),
    writeln('EXAMPLE_FILE - A file containing lines "Input -> Output", where Input is a list of letters in the alphabet'),
    writeln('ALPHABET - The list of symbols (must be some range like [0..n], but written like [0,1,2,3], when n = 3)'),
    writeln('OUTPUT_FILE - The file to write the output to (formatted so Walnut can use it)').

increasing(N) :- increasing(1, N).
increasing(N, N).
increasing(N, Next) :-
    N1 #= N + 1,
    increasing(N1, Next).

list(N, L) :-
    increasing(N, LN),
    length(L, LN).

list(L) :-
    increasing(N),
    length(L, N).

atom_to_example(Atom, In -> Out) :-
    term_to_atom(In -> Out, Atom).

examples_from_file(File, Examples) :-
    read_file_lines(File, Lines),
    maplist(atom_to_example, Lines, Examples).

word(_, []).
word(Alphabet, [X|Word]) :-
    word(Alphabet, Word),
    member(X, Alphabet).

word_len(_, 0, []).
word_len(Alphabet, N, [X|Xs]) :-
    N #> 0,
    N1 #= N - 1,
    (
        member(X, Alphabet),
        word_len(Alphabet, N1, Xs);
        word_len(Alphabet, N1, [X|Xs])
    ).

words_with_len(Alphabet, N, Words) :-
    findall(Word, word_len(Alphabet, N, Word), Words).

transitions(state(_, _, Transitions), Transitions).

letter(L -> _, L).
dest(_ -> Dest, Dest).

transition_letters([], _).
transition_letters([N -> _|Transitions], N) :-
    N1 #= N + 1,
    transition_letters(Transitions, N1).

constraint_transitions(AlphabetSize, Transitions) :-
    A1 #= AlphabetSize + 1,
    length(Transitions, A1),

    transition_letters(Transitions, 0).

make_transitions(_, []).
make_transitions(Alphabet, [state(_, _, Transitions)|States]) :-
    length(Alphabet, M),
    AlphabetSize #= M - 1,
    constraint_transitions(AlphabetSize, Transitions),
    make_transitions(Alphabet, States).

examples(Alphabet, Examples, States) :-
    list(States),
    length(States, N),
    N #>= 16,
    label_states(States, 0),
    make_transitions(Alphabet, States),
    writeln(States),
    examples_run(Alphabet, Examples, States).

atom_to_transition(Atom, In -> Out) :-
    (var(Out) -> Out = 0; true),
    term_to_atom(In -> Out, Atom).

atom_to_state(Atom, state(Label, Output, Transitions)) :-
    maplist(atom_to_transition, Atoms, Transitions),

    term_to_atom(LabelAtom, Label),
    (var(Output) -> Output = -1; true),
    term_to_atom(OutputAtom, Output),
    atomic_list_concat([LabelAtom, OutputAtom], ' ', StateLine),

    atomic_list_concat([StateLine|Atoms], '\n', Atom).

atom_to_automata(Atom, States) :-
    maplist(atom_to_state, Atoms, States),
    atomic_list_concat(Atoms, '\n', Atom).

write_walnut_automata(File, Alphabet, States) :-
    atomic_list_concat(Alphabet, ',', Temp),
    atomic_list_concat(['{', Temp, '}'], '', AlphabetAtom),
    atom_to_automata(MainAutomata, States),
    atomic_list_concat([AlphabetAtom, MainAutomata], '\n', FullStr),

    setup_call_cleanup(
        open(File, write, Stream, [encoding(unicode_be)]),
        writeln(Stream, FullStr),
        close(Stream)).

examples_run(_, [], _).
examples_run(Alphabet, [Word -> Output|Examples], States) :-
    Automata = automata(Alphabet, States),

    run(Automata, Word, Output),
    length(Examples, L),
    examples_run(Alphabet, Examples, States).

all_output(Automata, Words, Output) :-
    foreach(member(Word, Words), run(Automata, Word, Output)).

label(state(Label, _, _), Label).
labels(States, Labels) :- maplist(label, States, Labels).

label_states([], _).
label_states([state(N, _, _)|States], N) :-
    N1 #= N + 1,
    label_states(States, N1).

run(Automata, Input, Output) :-
    Automata = automata(_, [StartState|_]),
    run_from(StartState, Automata, Input, Output).

run_from(state(_, Output, _), _, [], Output).
run_from(state(_, _, Transitions), Automata, [X|Xs], Output) :-
    Automata = automata(_, States),

    nth0(X, Transitions, _ -> St),
    nth0(St, States, State),

    run_from(State, Automata, Xs, Output).

