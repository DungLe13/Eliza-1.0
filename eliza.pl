/*
 * Dung Le
 * HW 10 - Programming Languages
 * Problem 9 (written in Prolog)
 */

/* Start the program by calling ?- eliza. */
/* Please enter input in the format of string, and end with period. E.g. > "i am bored".
   Inputs are case-sensitive. Please use lowercased letters at all time.
   To escape the prompt, use period in the new line. E.g. > |: .
*/
eliza :-
  writeln('Hello, I am Eliza. How can I help you today?'),
  repeat,
  write('> '),
  read(X),
  call(input(X)),
  fail.

% simple conversation pattern
pattern(["i", "am", _, X], Template) :- pattern(["i", "am", X], Template).
pattern(["i", "am", X], Template) :-
  R is random(8),
  (R =< 1 ->
      Template = ["How long have you been", X, "?"];
   between(2, 3, R) ->
      Template = ["What makes you", X, "?"];
   between(4, 5, R) ->
      Template = ["Do you believe it is normal to be", X, "?"];
   between(6, 7, R) ->
      Template = ["You can always tell me why."]
  ).

pattern(["i", _, "like", "to", _, "about", X], Template) :- pattern(["i", "like", X], Template).
pattern(["i", "like", "to", _, "about", X], Template) :- pattern(["i", "like", X], Template).
pattern(["i", "like", X], Template) :-
  R is random(9),
  (R =< 3 ->
     Template = ["What is it about", X, "that you would like to discuss?"];
   between(4, 7, R) ->
     Template = ["Can you elaborate on", X, "?"];
   R = 8 ->
     Template = ["I have no knowledge about", X, ".", "Anything else you want to talk about?"]
  ).

pattern(["yes", _, _], Template) :- pattern(["yes"], Template).
pattern(["yes"], Template) :-
  R is random(6),
  (R =< 2 ->
     Template = ["Why so?"];
   between(3, 5, R) ->
     Template = ["Can you elaborate on that?"]
  ).
pattern(["no", _, _], Template) :- pattern(["no"], Template).
pattern(["no"], ["Why not?"]).

pattern([_, "bye", _], Template) :- pattern(["bye"], Template).
pattern(["bye", _], Template) :- pattern(["bye"], Template).
pattern(["bye"], ["It was nice talking to you. Good bye."]).

pattern(L, Template) :-
  lists:member("i", L),
  replace("i", "you", L, NewList),

  % check if "my" and "because" are in the list
  (lists:member("my", NewList) ->
    replace("my", "your", NewList, NewList1),
    (lists:member("because", NewList1) ->
        lists:delete(NewList1, "because", NewerList);
        NewerList = NewList1);
    (lists:member("because", NewList) ->
        lists:delete(NewList, "because", NewerList);
        NewerList = NewList)),

  % add "?" to the end of the list
  add_tail(NewerList, "?", FinalList),

  % deal with cases like "i don't", "i am", "i can't", etc. and return response
  (lists:member("don't", FinalList) ->
      lists:delete(FinalList, "don't", FinalList1),
      Template = ["why don't" | FinalList1];
   lists:member("didn't", FinalList) ->
      lists:delete(FinalList, "didn't", FinalList1),
      Template = ["why didn't" | FinalList1];
   lists:member("can't", FinalList) ->
      lists:delete(FinalList, "can't", FinalList1),
      Template = ["why can't" | FinalList1];
   lists:member("can", FinalList) ->
      lists:delete(FinalList, "can", FinalList1),
      Template = ["why can" | FinalList1];
   lists:member("am", FinalList) ->
      lists:delete(FinalList, "am", FinalList1),
      Template = ["why are" | FinalList1];
   lists:member("was", FinalList) ->
      lists:delete(FinalList, "was", FinalList1),
      Template = ["why were" | FinalList1];
      Template = ["why do" | FinalList]).

pattern(_, Template) :-
    R is random(6),
    (R =< 3 ->
       Template = ["Okk... please continue."];
     between(4, 5, R) ->
       Template = ["Sorry, but I don't understand what you said."]
    ).

% replace function: replace an original string in the list of input
% with new string, and return the new list
replace(_, _, [], []).
replace(O, R, [O|T1], [R|T2]) :- replace(O, R, T1, T2).
replace(O, R, [H|T1], [H|T2]) :- H \= O, replace(O, R, T1, T2).

% add tail function: used to add "?" to end of list
add_tail([], X, [X]).
add_tail([H|T], X, [H|L]) :- add_tail(T, X, L).

% convert list of words to string
words_to_string(L, Res) :-
  atomic_list_concat(L, ' ', Atom),
  atom_string(Atom, Res).

% convert input string to list of words,
% return list of response words -> convert back to string
% return response as string
input(S) :-
  split_string(S, " ", " ,.", Input),
  pattern(Input, Res), !,
  words_to_string(Res, Response),
  writeln(Response).
