% student(Name, Group)
student(alisa, 2).
student(bob, 1).
student(chloe, 2).
student(denise, 1).
student(edward, 2).
% friend(Name, Name)
friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).
% parent(Parent, Child)
parent(marjorie, bart).
parent(marjorie, lisa).
parent(marjorie, maggie).
parent(homer, bart).
parent(homer, lisa).
parent(homer, maggie).
parent(abraham, homer).
parent(mona, homer).
parent(jacqueline, marjorie).
parent(jacqueline, patty).
parent(jacqueline, selma).
parent(clancy, marjorie).
parent(clancy, patty).
parent(clancy, selma).
parent(selma, ling).
% unary(Number)
unary(z).
unary(s(X)) :- unary(X).
% Exercise 1

% 1.a
% _____________________________________________________________________________________________________________________________________________________________
% friend(alisa, Y), friend(Y, Z). -> [friend(alisa, bob), Y = bob] friend(bob, Z) |-> [friend(bob, chloe), Z = chloe] -> Y = bob, Z = chloe                  //
%                                                                                 |-> [friend(bob, edward), Z = edward] -> Y = bob, Z = edward               //
%                                ______________________________________________________________________________________________________________________________
%                                -> [friend(alisa, denise), Y = denise] friend(denise, Z) -> [friend(denise, edward), Z = edward] -> Y = denise, Z = edward  //
%______________________________________________________________________________________________________________________________________________________________

% 1.b
% ____________________________________________________________________________
% friend(X, Y). -> [friend(alisa,Y), X = alisa] |-> X = alisa, Y = bob      //
% 												|-> X = alisa, Y = denise   // 
%			   _______________________________________________________________
%			   -> [friend(bob,Y), X = bob] |-> X = bob, Y = chloe           //
%										   |-> X = bob, Y = edward          //
%			   _______________________________________________________________
%			   -> [friend(chloe,Y), X = chloe] -> X = chloe, Y = denise     //
%			   _______________________________________________________________
%			   -> [friend(denise,Y), X = denise] -> X = denise, Y = edward  //
%_____________________________________________________________________________

% 1.c
% _________________________________________________________________________________________________________________________
% parent(jacqueline, Y), parent(Y, ling). |-> [parent(jacqueline, marjorie),parent(marjorie,ling) Y = marjorie] -> false //
% 										  |-> [parent(jacqueline, patty),parent(patty,ling) Y = patty] -> false 		 //
%										  |-> [parent(jacqueline, selma),parent(selma,ling) Y = selma] -> Y = selma      //
%__________________________________________________________________________________________________________________________



% Exercise 2
groupmates(X,Y) :- student(X,Z), student(Y,Z).

% Exercise 3
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), parent(Z,Y).
relative(X, Y) :- ancestor(Z,Y),ancestor(Z,X). 


% Exercise 4

% 4.a
double(z,z).
double(s(X),s(s(Y))) :-double(X,Y).

% 4.b
leq(z,s(_)).
leq(z,z).
leq(s(X),s(Y)) :- leq(X,Y).

% 4.c

add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(z, _, z).
mult(s(X), Y, Z) :- add(Y, S, Z), mult(X, Y, S).

% 4.d

powerOf2(z,s(z)).
powerOf2(s(X),Y) :-leq(X,Y), mult(s(s(z)),M, Y), powerOf2(X,M).


