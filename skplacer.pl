:- use_module(library(lists)).
:- consult('smallModel.pl').
:- consult('utils.pl').
:- consult('labellings.pl').
:- consult('probabilisticModel.pl').
%:- consult('skLinkCostInt').

query(test(smallExample,3,1,P)).

test(AppId,Limit,NLabelsChanging,P,CombinationsProb):-
    combinations(NLabelsChanging,Combinations),
    combinationsProbability(Combinations,NLabelsChanging,CombinationsProb),
    sKnife(AppId,Limit,P).
    %partitioningCost(AppId,P,Combinations,(Np,C)).

partitioningByCost(AppId,Limit,NLabelsChanging, (Np,C),Ps):-
    combinations(NLabelsChanging,Combinations),
    combinationsProbability(Combinations,NLabelsChanging,CombinationsProb),
    findall(((Np,C,P)),(sKnife(AppId,Limit,P),partitioningCost(AppId,P,CombinationsProb,(Np,C))),Ps).

allPartitioningCost(AppId,Limit,NLabelsChanging,Ps):-
    combinations(NLabelsChanging,Combinations),
    combinationsProbability(Combinations,NLabelsChanging,CombinationsProb),
    findall(((Np,C)),(sKnife(AppId,Limit,P),partitioningCost(AppId,P,CombinationsProb,(Np,C))),Ps).

sKnife(AppId, Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Hardware),
    softwareLabel(Software, LabelledSoftware),
    softwareOk(LabelledSoftware),
    partitioning(LabelledSoftware,0, inf,[], Partitions).

sKnife(AppId, PLimit,Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Hardware),
    softwareLabel(Software, LabelledSoftware),
    softwareOk(LabelledSoftware),
    partitioning(LabelledSoftware, 0,PLimit,[], Partitions).


%check the labelling of hardware components
hardwareOK([H|Hs]) :- 
    hardware(H, Data, Characteristics,_),
    labelC(Data, Characteristics, (TData,TChar)),
    gte(TChar,TData), %check if an hardware component is trusted for the level of its data 
    hardwareOK(Hs).
hardwareOK([]).

%labels software components with Type of Data and  Type of Characteristics
softwareLabel([Sw|Sws],[(Sw,TData,TChar)|LabelledSws]):-
    software(Sw, Data,Characteristics,_,_),
    labelC(Data, Characteristics,(TData,TChar)),
    softwareLabel(Sws,LabelledSws).
softwareLabel([],[]).

softwareOk(LabelledSoftware):-
    \+ (
        member((Sw,TData,TChar), LabelledSoftware),
        lt(TChar,TData),
        externalLeak([Sw], [] ,TData, LabelledSoftware)
      ).

externalLeak(LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,_)),
    \+ trustedHW(LinkedHW, TData).

externalLeak(LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,VisitLinkedSW)),
    trustedHW(LinkedHW, TData),
    externalLeak(VisitLinkedSW, [Sw|Visited], TData, LabelledSoftware).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(LinkedHW, TData):-
    \+ (
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(Characteristics, CLabel), lowestType(CLabel, MinCType),
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

labelC(Data, Characteristics, (MaxType,CharactSecType)):-
    dataLabel(Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).

%tagData(Data,Label)
dataLabel([Data|Ds],[Type|Label]):-
    tag(Data,Type),
    dataLabel(Ds, Label).
dataLabel([],[]).
%tagCharacteristics(ListOfCharacteristics,ListOfCharacteristicsTypes)
characteristicsLabel([],[HighestType]):-
    highestType(HighestType).
characteristicsLabel([Charact|Characteristics],[Type|ListOfCharactTypes]):-
    tag(Charact, Type),
    characteristicsLabel(Characteristics, ListOfCharactTypes).
characteristicsLabel([Charact|Characteristics],[HighestType|ListOfCharactTypes]):-
    \+ tag(Charact, _),
    highestType(HighestType),
    characteristicsLabel(Characteristics, ListOfCharactTypes).


