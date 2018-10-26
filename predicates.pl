
%%%%%%TASK1

% This predicate finds all the teams by looking all the team predicates,
% then puts them in a list called M by means of the findall predicate.
% After that it finds the length of the that list using the length predicate,
% then it finds all the permutations of that list using permutation predicate and 
% puts them into the variable L.
allTeams(L,N) :- 
  findall(X,team(X,Y),M),
  length(M,N),
  permutation(M,L).

%TASK2

% This predicate finds all the matches which the team T wins when it is hometeam.
% This matches are played before the week W including the week W.
winsHelper(T,W,O) :- 
  match(X,T,TS,O,OS),
  TS > OS,
  X =< W.

% This predicate finds all the matches which the team T wins when it is away team.
% This matches are played before the week W including the week W.
winsHelper(T,W,O) :-
  match(X,O,OS,T,TS),
  TS > OS,
  X =< W.

% this predicate finds all the matches which the team T wins by winsHelper predicate,
% and then it puts the opponent teams played in these matches to a list called L. After that
% it finds the length of that list.
wins(T,W,L,N) :-
  findall(O,winsHelper(T,W,O),L),
  length(L,N).

% This predicate finds all the matches which the team T loses when it is hometeam.
% This matches are played before the week W including the week W.
lossesHelper(T,W,O) :-
  match(X,T,TS,O,OS),
  TS < OS,
  X =< W.

% This predicate finds all the matches which the team T loses when it is awayteam.
% This matches are played before the week W including the week W.
lossesHelper(T,W,O) :-
  match(X,O,OS,T,TS),
  TS < OS,
  X =< W.

% this predicate finds all the matches which the team T loses by lossesHelper predicate,
% and then it puts the opponent teams played in these matches to a list called L. After that
% it finds the length of that list.
losses(T,W,L,N) :-
  findall(O,lossesHelper(T,W,O),L),
  length(L,N).

% This predicate finds all the matches which the team T doesnt win or lose when it is hometeam.
% This matches are played before the week W including the week W.
drawsHelper(T,W,O) :-
  match(X,T,TS,O,OS),
  TS == OS,
  X =< W.

% This predicate finds all the matches which the team T doesnt win or lose when it is awayteam.
% This matches are played before the week W including the week W.
drawsHelper(T,W,O) :-
  match(X,O,OS,T,TS),
  TS == OS,
  X =< W.

% this predicate finds all the matches which the team T doesnt win or lose by drawsHelper predicate,
% and then it puts the opponent teams played in these matches to a list called L. After that
% it finds the length of that list
draws(T,W,L,N) :-
  findall(O,drawsHelper(T,W,O),L),
  length(L,N).

%TASK3

%The function of the following three predicates are to sum up the elements of a given list consisting of numbers.

%This predicate starts the summing process by initializing the accumulator with the head of the list and calls
%the helper predicate with tail of the list.
listSoum([H|T],Sum) :-
  listSumHelper(T,H,Sum).

%Base step of the recursive listSumHelper predicate which tells that stop the recursion when the list is empty and
%make Sum equal to accumulator.
listSumHelper([],Acc,Acc). 

%This is a recursive predicate in which an accumulator sums up the elements of the list. It always takes the head of the list
%and adds it to the accumulator and calls the predicate with the tail of the list until the tail becomes an empty list.
listSumHelper([H|T],Acc,Sum) :-
  NewAcc is H + Acc,
  listSumHelper(T,NewAcc,Sum).

% This is a helper predicate which finds all the scores scored by the given team before the given week including the given week
%when the team is hometeam. 
scoredHelper(T,W,S) :-
  match(X,T,S,O,C),
  X =< W.

% This is a helper predicate which finds all the scores scored by the given team before the given week including the given week
%when the team is awayteam. 
scoredHelper(T,W,S) :-
  match(X,O,C,T,S),
  X =< W.

%This predicate generates a list which contains the scores scored by the given team before the given week including the given week.
scoredListGenerator(T,W,L) :-
  findall(S,scoredHelper(T,W,S),L),
  length(L,N).

% This is a helper predicate which finds all the scores conceded by the given team before the given week including the given week
%when the team is hometeam. 
concededHelper(T,W,C) :-
  match(X,T,S,O,C),
  X =< W.

% This is a helper predicate which finds all the scores scored by the given team before the given week including the given week
%when the team is awayteam. 
concededHelper(T,W,C) :-
  match(X,O,C,T,S),
  X =< W.

%This predicate generates a list which contains the scores conceded by the given team before the given week including the given week.
concededListGenerator(T,W,L) :-
  findall(C,concededHelper(T,W,C),L),
  length(L,N).

%This predicate finds the total summation of the scores scored by the given team before the given week including the given week.
scored(T,W,S) :-
  scoredListGenerator(T,W,L),
  listSum(L,S).

%This predicate finds the total summation of the scores conceded by the given team before the given week including the given week.
conceded(T,W,C) :-
  concededListGenerator(T,W,L), 
  listSum(L,C).

%This predicate finds the average score of a team by substracting the scored points from the conceded points.
average(T,W,A) :-
  scored(T,W,S),
  conceded(T,W,C), 
  A is S - C.

%TASK4

%This predicate finds list of the teams without calculating the permutation.
allTeamsHelper(L,N) :- 
  findall(X,team(X,Y),M),
  length(M,N).

% This helper predicate takes 4 arguments.
% 1st argument = week
% 2nd argument = list of the average-Team pairs which is calculated within the predicate
% 3rd argument = list of the teams
% 4th argument = Final list of the average-Team pairs.
%%% This is a recursive predicate. In each step it finds a average-Team pair and adds it to a list by append predicate.
%%% It stops when the total number of teams added to the list becomes equal to the original number of teams in the knowledge base.
orderHelper(W,L2,[Head|Tail],Final) :- 
  findall(A-Head,average(Head,W,A),X),
  append(X,L2,MyList),
  length(MyList,N),
  allTeamsHelper(L3,M),
  N =< M,
  orderHelper(W,MyList,Tail,Final).

%%% This predicate is the one where the final form of the list is constituted.
orderHelper(W,L2,L4,Final) :- 
  Final = L2,
  length(Final,N),
  allTeamsHelper(L,M),
  N == M.

% In this predicate, first the team list is generated and assigned to L2 variable. Then orderHelper predicate is called and
% the list which contains the average-Team pairs are generated and assigned to L3 variable. After that this list is sorted 
% using the keysort predicate according to the averages in a ascending order. Then the average part of the pairs are removed
% by the pairs_values predicate and new list is assigned to L4. Finally the list is reversed by the reverse predicate and the
% desending order team list is generated and assigned to L.
order(L,W) :-
  findall(X,team(X,Y),L2),
  orderHelper(W,[],L2,L3),
  keysort(L3,Pairs),
  pairs_values(Pairs, L4),
  reverse(L4,L).

%In this predicate top three teams of the ordered list is fetched and assigned to the first argument of the top three predicate.
topThree([T1,T2,T3|T], W) :-
  order([X,Y,Z|Tail],W),
  T1 = X,
  T2 = Y,
  T3 = Z,
  T = [].