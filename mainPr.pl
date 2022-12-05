:- use_module(library(lists)).
:- consult('smallModel.pl').
:- consult('utils.pl').
:- consult('sknife.pl').
:- consult('probabilisticModel.pl').

%'dream' predicate to give all the partioning with the cost
%skplace(AppId, StartingLabelling, DLimit,(P,C)):-
%    sKnife(AppId,StartingLabelling,DLimit,P),
%    partitioningCost(P,AppId,Labelling,DLimit,C).


%Cost of the partitioning P0 = (Domains number, Partitioning to reach, min cost)
partitioningCost(P0,AppId,DLimit,(Dn,Pmin,Cmin)):-
    domainNumber(P0,Dn),
    futureCost(P0,AppId,DLimit,(Pmin,Cmin)).

domainNumber([_|Ps],N):-
    domainNumber(Ps,Ns),
    N is Ns + 1.
domainNumber([],0).

%Cost C from P0 to Pi with Labelling
futureCost(P0,AppId,DLimit,(Labelling,Pi,C)):-
    labelling0L(Labelling),
    sKnife(AppId,Labelling,DLimit,Pi),
    cost(P0,Pi,C).

%C is cost of going from P1 to P2
cost(P1,P2,C):-
    links(P1,P1Links),
    links(P2,P2Links),
    linksCost(P1Links,P2Links,C).

%L is list of links of P, link = (sw1,sw2,C,status)
links(P,L):- links(P,[],L).

%from two lists of links sum cost of different ones
linksCost([(S1,S2,C,St1)|Slinks],NewLinks,Cost):-
    \+ member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost),
    Cost is C + SubCost.
linksCost([(S1,S2,C,St1)|Slinks],NewLinks,SubCost):-
    member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost).
linksCost([],_,0).