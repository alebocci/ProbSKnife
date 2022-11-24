:- consult('smallModel.pl').
:- consult('utils.pl').
:- use_module(library(lists)).
%:- consult('probabilisticModel.pl').

test(L,P,C):- %L = 3 or 4
    startingLabelling(S),
    skplace(smallExample,S,L,(P,C)).



%query(test([((top, safe), [south, west]),  ((top, safe), [east]),  ((top, safe), [north])],smallExample,3,[(charact1, top),  (charact2, top),  (charact3, top),  (charact4, top),  (data1, top),  (data2, top),  (data3, low),  (data4, top),  (data5, top),  (data6, top)],C)).
test(_,AppId,DLimit,L,Ps):-
    findall(Pi,sKnife(AppId,L,DLimit,Pi),Ps).
    %cost(P0,Ps,Cs),
    %minList(Cs,LabCosts),
    %sumCosts(LabCosts,Fc).
%query(test2(L)).
%query(test2([((top, safe), [south, west]),  ((top, safe), [east]),  ((top, safe), [north])],[((top, safe), [south]),  ((top, safe), [west, north]),  ((top, safe), [east])],C)).
test2(AllLabellings):-
    startingLabelling(StartingLabelling),
    findall(Labelling,newLabelling(StartingLabelling,Labelling),AllLabellings).

startingLabelling(SL):-
    findall((DC,L),tag(DC,L),SL).

skplace(AppId, Labelling, DLimit,(P,C)):-
    sKnife(AppId,Labelling,DLimit,P),
    partitioningCost(P,AppId,Labelling,DLimit,C).

partitioningCost(P0,AppId,StartLabelling,DLimit,(Dn,Fc)):-
    domainNumber(P0,Dn),
    futureCost(P0,AppId,StartLabelling,DLimit,Fc).

futureCost(P0,AppId,StartingLabelling,DLimit,Fc):-
    findall(Labelling,newLabelling(StartingLabelling,Labelling),AllLabellings),
    findall(LabCost,
        (member(L,AllLabellings),
        findall(Pi,sKnife(AppId,L,DLimit,Pi),Ps),
        cost(P0,Ps,Cs),
        minList(Cs,LabCost)
        ),LabCosts),
    sumCosts(LabCosts,Fc).

/*
futureCost(P0,AppId,StartLabelling,DLimit,Fc):-
    findall(LabCost,
        (newLabelling(StartLabelling,L),
        findall(Pi,sKnife(AppId,L,DLimit,Pi),Ps),
        cost(P0,Ps,Cs),
        minList(Cs,LabCost)
        ),LabCosts),
    sumCosts(LabCosts,Fc).
*/
minList([C|Cs],C):-
    minList(Cs,MinCs),
    C < MinCs.
minList([C|Cs],MinCs):-
    minList(Cs,MinCs),
    C >= MinCs.
minList([],inf).

sKnife(AppId,Labelling, Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Labelling,Hardware),
    softwareLabel(Labelling,Software, LabelledSoftware),
    softwareOk(Labelling,LabelledSoftware),
    partitioning(LabelledSoftware,0, inf,[], Partitions).

sKnife(AppId, Labelling,PLimit,Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Labelling,Hardware),
    softwareLabel(Labelling,Software, LabelledSoftware),
    softwareOk(Labelling,LabelledSoftware),
    partitioning(LabelledSoftware, 0,PLimit,[], Partitions).


%check the labelling of hardware components
hardwareOK(Labelling,[H|Hs]) :- 
    hardware(H, Data, Characteristics,_),
    labelC(Labelling,Data, Characteristics, (TData,TChar)),
    gte(TChar,TData), %check if an hardware component is trusted for the level of its data 
    hardwareOK(Labelling,Hs).
hardwareOK(_,[]).

%labels software components with Type of Data and  Type of Characteristics
softwareLabel(Labelling,[Sw|Sws],[(Sw,TData,TChar)|LabelledSws]):-
    software(Sw, Data,Characteristics,_,_),
    labelC(Labelling,Data, Characteristics,(TData,TChar)),
    softwareLabel(Labelling,Sws,LabelledSws).
softwareLabel(_,[],[]).

softwareOk(Labelling,LabelledSoftware):-
    \+ (
        member((Sw,TData,TChar), LabelledSoftware),
        lt(TChar,TData),
        externalLeak(Labelling,[Sw], [] ,TData, LabelledSoftware)
      ).

externalLeak(Labelling,LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,_)),
    \+ trustedHW(Labelling,LinkedHW, TData).

externalLeak(Labelling,LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,VisitLinkedSW)),
    trustedHW(Labelling,LinkedHW, TData),
    externalLeak(Labelling,VisitLinkedSW, [Sw|Visited], TData, LabelledSoftware).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(Labelling,LinkedHW, TData):-
    \+ (
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(Labelling,Characteristics, CLabel), lowestType(CLabel, MinCType),
        lt(MinCType, TData)
        ).

partitioning([(S,TData,TChar)|Ss], Npar,PLimit,Partitions, NewPartitions) :-
    software(S,_,_,_,_),
    partitionCharLabel(TChar,TData,TCP),
    select( ((TData,TCP), P), Partitions, TmpPartitions),
    PNew = ( (TData,TCP), [S|P]),
    partitioning(Ss, Npar, PLimit,[PNew|TmpPartitions], NewPartitions).
partitioning([(S,TData,TChar)|Ss], Npar,PLimit,Partitions, NewPartitions) :-
    software(S,_,_,_,_),
    partitionCharLabel(TChar,TData,TCP),
    %\+ member( ((TData,TCP), _), Partitions), % comment this to find all solutions combinatorially
    P = ( (TData,TCP), [S]),
    NewNpar is Npar +  1,
    NewNpar =< PLimit,
    partitioning(Ss, NewNpar,PLimit,[P|Partitions], NewPartitions).
partitioning([],_,_,P,P).

labelC(Labelling,Data, Characteristics, (MaxType,CharactSecType)):-
    dataLabel(Labelling,Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(Labelling,Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).

domainNumber([_|Ps],N):-
    domainNumber(Ps,Ns),
    N is Ns + 1.
domainNumber([],0).

newLabelling(StartLabelling,L):-
    labelling(L),
    dif(StartLabelling,L).

labelling(L):-
    dataChar(DataChar),
    lattice(Lattice),
    combine(DataChar,Lattice,L).

lattice(Lattice):-
    findall(Label, (g_lattice_higherThan(Label,_);g_lattice_higherThan(_,Label)),LatticeDup),
    list_to_set(LatticeDup,Lattice).
dataChar(DataChar):-
    findall(DC,tag(DC,_),DataChar).

combine([DC|DCs],Lattice,[(DC,L)|Labbelling]):-
    member(L,Lattice),
    %tagChange(DC,L),
    combine(DCs,Lattice,Labbelling).
combine([],_,[]).

cost(P0,[P|Ps],[C|Cs]):-
    dif(P0,P),
    links(P0,P0Links),
    links(P,PLinks),
    linksCost(P0Links,PLinks,C),
    cost(P0,Ps,Cs).
cost(P0,[P0|_],[0]).
cost(_,[],[]).


links(P,L):- links(P,[],L).

linksCost([(S1,S2,C,St1)|Slinks],NewLinks,Cost):-
    \+ member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost),
    Cost is C + SubCost.
linksCost([(S1,S2,C,St1)|Slinks],NewLinks,SubCost):-
    member((S1,S2,C,St1),NewLinks),
    linksCost(Slinks,NewLinks,SubCost).
linksCost([],_,0).