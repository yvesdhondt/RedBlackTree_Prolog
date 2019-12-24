/*
Copyright (c) 2019 Yves D'hondt
This software was released under the MIT license.
For more details see the 'LICENSE' file that should have been included
with this software.
*/

%%%%% BACKGROUND INFORMATION %%%%%

% A node looks as follows: node(Parent,Left,Right,Colour,Key,Value)
% There are two colours: black & red
% Keys MUST be integers/They must pass the integer/1 check

%%%%% Operations %%%%%

% element(Key,Tree)
% element/2: Check whether the key is in the tree
element(Key,Tree) :-
    element(Key,_,Tree).

% element(Key,Tree,Value)
% element/3: Check whether the key is in the tree and return the
% associated value
element(Key,Value,node(_,_,_,_,Key,Value)) :- !.
element(Key,Value,node(_,L,_,_,K,_)) :-
    Key < K,
    element(Key,Value,L),
    !.
element(Key,Value,node(_,_,R,_,K,_)) :-
    Key > K,
    element(Key,Value,R).

% add(Key,Value,OldTree,NewTree)
% add/4: Check whether the new tree is equal to the old tree, with the
% new key-value pair in it
add(Key,Value,nil,node(nil,nil,nil,black,Key,Value)) :-
    integer(Key),
    !.
add(Key,Value,node(P,L,R,C,Key,_),node(P,L,R,C,Key,Value)) :- !.
add(Key,Value,Tree,NewTree) :-
    Tree = node(P,L,R,C,K,V),
    Key < K,
    !,
    add(Key,Value,L,NewL),
    fix_up(node(P,NewL,R,C,K,V),NewTree).
add(Key,Value,Tree,NewTree) :-
    Tree = node(P,L,R,C,K,V),
    Key > K,
    add(Key,Value,R,NewR),
    fix_up(node(P,L,NewR,C,K,V),NewTree).

% fix_up(Tree,NewTree)
% fix_up/4: Make sure that the colors of the chains in the tree are
% correct
fix_up(Tree,NewTree) :-
    left_child(L,Tree),
    \+ is_red(L),
    right_child(R,Tree),
    is_red(R),
    !,
    rotate_left(Tree,NewTree).
fix_up(Tree,NewTree) :-
    left_child(L,Tree),
    is_red(L),
    left_child(LL,L),
    is_red(LL),
    !,
    rotate_right(Tree,NewTree).
fix_up(Tree,NewTree) :-
    left_child(L,Tree),
    is_red(L),
    right_child(R,Tree),
    is_red(R),
    !,
    flip_colors(Tree,NewTree).
fix_up(Tree,Tree).
% WATCH OUT! This implementation uses a red-cut, removing this cut will
% lead to an incorrect program execution


%%%%% DATA STRUCTURE %%%%%
% --- BASIC PROPERTIES --- %

% is_red(Tree)
% is_red/1: Check whether the node is red
is_red(node(_,_,_,red,_,_)).

% size(Tree,Size)
% size/2: Check whether the tree has the given size
size(X,S) :-
    size(X,0,S).

% size(Tree,Accumulator,Size)
% size/3: Check whether the size of the tree is equal to the given size
% + the accumulator
size(nil,Acc,Acc).
size(node(_,L,R,_,_,_),Acc,Size) :-
    AccTemp is Acc + 1,
    size(L,AccTemp,SizeL),
    size(R,SizeL,Size).

% is_empty(Tree)
% is_empty/1: Check whether the given tree is empty
is_empty(nil).

% --- FAMILY RELATIONS --- %

% parent(Node,Parent)
% parent/2: Check whether the given parent is the parent of the given
% node
% The root node has no parent
parent(node(P,_,_,_,_,_),P) :-
    P \= nil.

% left_child(Node,Parent)
% left_child/2: Check whether the given node is the left-child of the
% given parent
left_child(L,node(_,L,_,_,_,_)).

% right_child(Node,Parent)
% right_child/2: Check whether the given node is the right-child of the
% given parent
right_child(R,node(_,_,R,_,_,_)).

% grandparent(Node,GrandParent)
% grandparent/2: Check whether the given grandparent is the grandparent
% of the given node
% The children of the root node have no grandparent
grandparent(X,Gp) :-
    parent(X,P),
    parent(P,Gp).

% sibling(Sibling1,Sibling2)
% sibling/2: Check whether the two nodes are siblings
sibling(X,R) :-
    parent(X,node(_,X,R,_,_,_)).
sibling(X,L) :-
    parent(X,node(_,L,X,_,_,_)).

% uncle(Node,Uncle)
% uncle/2: Check whether the given uncle is the uncle of the given node
uncle(X,U) :-
    parent(X,P),
    sibling(P,U).

% aunt(Node,Uncle)
% aunt/2: Synonym for uncle/2
aunt(X,U) :-
    uncle(X,U).

% --- ELEMENTARY HELPERS --- %

% rotate_left(Tree,NewTree)
% rotate_left/2: Rotate a right-leaning red chain and store the result
% in a new tree
rotate_left(H,XNew) :-
    H = node(P,L1,X,C1,K1,V1),
    X = node(_,L2,R2,red,K2,V2),
    XNew = node(P,HNew,R2,C1,K2,V2),
    HNew = node(XNew,L1,L2,red,K1,V1).

% rotate_right(Tree,NewTree)
% rotate_right/2: Rotate a left-leaning red chain and store the result
% in a new tree
rotate_right(H,XNew) :-
    H = node(P,X,R1,C1,K1,V1),
    X = node(_,L2,R2,red,K2,V2),
    XNew = node(P,L2,HNew,C1,K2,V2),
    HNew = node(XNew,R2,R1,red,K1,V1).

% flip_colors(Tree,NewTree)
% flip_colors/2: Flip the colors of a 'black' node with 2 'red' children
% to a 'red' node with 2 'black' children and store the result in a new
% tree
flip_colors(H,node(P,LNew,RNew,red,K,V)) :-
    H = node(P,
             node(H,L1,R1,red,K1,V1),
             node(H,L2,R2,red,K2,V2),
             black,K,V),
    LNew = node(H,L1,R1,black,K1,V1),
    RNew = node(H,L2,R2,black,K2,V2).













