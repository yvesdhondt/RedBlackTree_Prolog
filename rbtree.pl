/*
Copyright (c) 2019 Yves D'hondt
This software was released under the MIT license.
For more details see the file 'LICENSE' that should have been included
with this software.
*/

% A node looks as follows: node(Parent,Left,Right,Colour,Key,Value)
% There are two colours: black & red
% Keys MUST be integers/They must pass the integer/1 check
% --- Operations & Helpers --- %

% elem/2: Check whether the key is in the tree
elem(Key,Tree) :-
    elem(Key,Tree,_).

% elem/3: Check whether the key is in the tree and return the associated
% value
elem(Key,node(_,_,_,_,Key,Value),Value) :- !.
elem(Key,node(_,L,_,_,_,_),Value) :-
    elem(Key,L,Value),
    !.
elem(Key,node(_,R,_,_,_,_),Value) :-
    elem(Key,R,Value).

add(Key,Value,nil,node(nil,nil,nil,black,Key,Value)) :-
    integer(Key),
    !.
add(Key,Value,node(P,L,R,C,Key,_),node(P,L,R,C,Key,Value)).


% --- DATA STRUCTURE --- %
% --- BASIC PROPERTIES --- %

is_red(node(_,_,_,red,_,_)).

size(X,S) :-
    size(X,0,S).
size(nil,Acc,Acc).
size(node(_,L,R,_,_,_),Acc,Size) :-
    AccTemp is Acc + 1,
    size(L,AccTemp,SizeL),
    size(R,SizeL,Size).

is_empty(nil).

% --- FAMILY RELATIONS --- %

% The root node has no parent
parent(node(P,_,_,_,_,_),P) :-
    P \= nil.

% The children of the root node have no parent
grandparent(X,Gp) :-
    parent(X,P),
    parent(P,Gp).

% The sibling of X is Y if X and Y have the same parent and they are
% different
sibling(X,R) :-
    parent(X,node(_,X,R,_,_,_)).
sibling(X,L) :-
    parent(X,node(_,L,X,_,_,_)).

% An aunt or uncle is the sibling of a node's parent
uncle(X,U) :-
    parent(X,P),
    sibling(P,U).
aunt(X,U) :-
    uncle(X,U).

% --- ELEMENTARY OPERATIONS --- %

rotate_left(H,XNew) :-
    H = node(P,L1,X,C1,K1,V1),
    X = node(_,L2,R2,red,K2,V2),
    XNew = node(P,HNew,R2,C1,K2,V2),
    HNew = node(XNew,L1,L2,red,K1,V1).
rotate_right(H,XNew) :-
    H = node(P,X,R1,C1,K1,V1),
    X = node(_,L2,R2,red,K2,V2),
    XNew = node(P,L2,HNew,C1,K2,V2),
    HNew = node(XNew,R2,R1,red,K1,V1).

flip_colors(H,node(P,LNew,RNew,red,K,V)) :-
    H = node(P,
             node(H,L1,R1,red,K1,V1),
             node(H,L2,R2,red,K2,V2),
             black,K,V),
    LNew = node(H,L1,R1,black,K1,V1),
    RNew = node(H,L2,R2,black,K2,V2).













