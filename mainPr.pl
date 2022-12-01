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

%Future cost C from P0 to Pi
futureCost(P0,AppId,DLimit,(Labelling,Pi,C)):-
    dataCharList(DataCharList),
    labelling(DataCharList,Labelling),
    sKnife(AppId,Labelling,DLimit,Pi),
    cost(P0,Pi,C).
/*
futureCost(P0,AppId,DataCharList,DLimit,(PMin,CMin)):-
    labelling(DataCharList,Labelling),
    findall(Pi,sKnife(AppId,Labelling,DLimit,Pi),Ps),
    costPList(P0,Ps,Cs),
    minCost(Cs,(PMin,CMin)).
*/
domainNumber([_|Ps],N):-
    domainNumber(Ps,Ns),
    N is Ns + 1.
domainNumber([],0).

cost(P1,P2,C):-
    links(P1,P1Links),
    links(P2,P2Links),
    linksCost(P1Links,P2Links,C).

costPList(P0,[P|Ps],[(P,C)|Cs]):-
    dif(P0,P),
    links(P0,P0Links),
    links(P,PLinks),
    linksCost(P0Links,PLinks,C),
    costPList(P0,Ps,Cs).
costPList(P0,[P0|_],[(P0,0)]).
costPList(_,[],[]).

minCost([(P,C)|Cs],(P,C)):-
    minCost(Cs,(_,Cmin)),
    C < Cmin.
minCost([(_,C)|Cs],(Pmin,Cmin)):-
    minCost(Cs,(Pmin,Cmin)),
    C >= Cmin.
minCost([],([],inf)).


links(P,L):- links(P,[],L).

linksCost([(S1,S2,C,St1)|Slinks],NewLinks,Cost):-
    \+ member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost),
    Cost is C + SubCost.
linksCost([(S1,S2,C,St1)|Slinks],NewLinks,SubCost):-
    member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost).
linksCost([],_,0).

%%%%%%%%%%%%%%%%%
%TEST
%%%%%%%%%%%%%%%%%%
%test(L,P,C):- %L = 3 or 4
%    startingLabelling(S),
%    skplace(smallExample,S,L,(P,C)).
/*
query(partitioningCost([((top, safe), [south, west, east, north])],
            smallExample,
            [charact1,charact2,charact3,charact4,data1,data2,data4,data5],
            3,
            RES)).

query(futureCost([((top, safe), [south, west]), ((top, safe), [east, north])],
            smallExample,
            [charact1,charact2,charact3,charact4,data2,data4,data5],
            3,
            RES)).

test(P0,AppId,DataCharList,DLimit,(Pmin,Cmin)):- (
    futureCost(P0,AppId,DataCharList,DLimit,(Labelling,Pi,C))
) => Labelling / minCost((Pi,C),(Pmin,Cmin)).

collect_minCost(CodeBlock, GroupBy, AggVar, AggRes):-
    aggregate(minCost, AggVar, GroupBy,CodeBlock, (GroupBy, AggRes)).

query(test([((top, safe), [south, west]), ((top, safe), [east, north])],
            smallExample,
            [charact1,charact2,charact3,charact4,data2,data4,data5],
            3,
            RES)).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
