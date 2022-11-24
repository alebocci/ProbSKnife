:- use_module(library(lists)).
:- consult('smallModel.pl').
:- consult('utils.pl').
:- consult('labellings.pl').
:- consult('probabilisticModel.pl').

combinations(NLabels,Res):-
    findall(DC,tag(DC,_),DCs),
    lattice(Lattice),
    taggedCombinations(DCs,Lattice,Labs),
    allCombinations(NLabels,Labs,Res).


lattice(Lattice):-
    findall(Label, (g_lattice_higherThan(Label,_);g_lattice_higherThan(_,Label)),LatticeDup),
    list_to_set(LatticeDup,Lattice).

taggedCombinations([DC|DCs],Lattice,Res):-
    tagDC(DC,Lattice,TDC),
    taggedCombinations(DCs,Lattice,TComb),
    append(TDC,TComb,Res).
taggedCombinations([],_,[]).

tagDC(DC,[L|Lattice],[(DC,L)|TDC]):-
    \+ tag(DC,L),
    tagDC(DC,Lattice,TDC).
tagDC(DC,[L|Lattice],TDC):-
    tag(DC,L),
    tagDC(DC,Lattice,TDC).
tagDC(_,[],[]).

allCombinations(NLabels,Labs,Res):-
    NLabels > 0,
    nCombinations(NLabels,Labs,[],Nres),
    NewN is NLabels - 1,
    allCombinations(NewN,Labs,NewRes),
    append(Nres,NewRes,Res).
allCombinations(0,_,[]).

nCombinations(NLabels,[(DC,L)|Ls],Acc,Res):-
    \+ member((DC,_),Acc),
    length(Acc,Lacc),
    NLabels > Lacc,
    nCombinations(NLabels, Ls,[(DC,L)|Acc],Res1),
    nCombinations(NLabels,Ls,Acc,Res2),
    append(Res1,Res2,Res).
nCombinations(NLabels,[(DC,_)|Ls],Acc,Res):-
    member((DC,_),Acc),
    length(Acc,Lacc),
    NLabels > Lacc,
    nCombinations(NLabels,Ls,Acc,Res).
nCombinations(NLabels,_,Acc,[Acc]):-
    length(Acc,NLabels).
nCombinations(NLabels,[],Acc,[]):-
    length(Acc,Lacc),
    NLabels > Lacc.


startingLabelling(SL):-
    findall((DC,L),tag(DC,L),SL).

combinationsProbability(Combinations,K,CombinationsProb):-
    expandCombinations(Combinations,ExpandedComb),
    startingLabelling(SL),
    disjunctionLabellings(ExpandedComb,Pconj),
    combinationsProbability(ExpandedComb,Combinations,K,SL,Pconj,CombinationsProb).
    
expandCombinations([C|Cs],[Cepx|CsExp]):-
    findall((DC,L),(tag(DC,L),\+member((DC,_),C)),Labelling),
    expandCombinations(Cs,CsExp),
    append(C,Labelling,Cepx).
expandCombinations([],[]).

combinationsProbability([Labl|Labls],[C|Cs],K,SL,Pconj,[(C,P)|CombinationsProb]):-
    labellingChange(SL,Labl,K,Pconj,P),
    combinationsProbability(Labls,Cs,K,SL,Pconj,CombinationsProb).
combinationsProbability([],[],_,_,_,[]).


test(C,K,P):-
    combinationsProbability(C,K,P).

%query(test([[(charact1, low)], [(charact2, low)], [(charact3, low)], [(charact4, low)], [(data1, low)], [(data2, low)], [(data3, low)], [(data4, low)], [(data5, low)], [(data6, low)]],1,P)).
%query(disjunctionLabellings([[(data1, low), (charact1, top), (charact2, top), (charact3, top), (charact4, top), (data2, top), (data3, top), (data4, top), (data5, top), (data6, top)]],P)).