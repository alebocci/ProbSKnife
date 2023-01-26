%Finds Partitioning for appliction AppId given the labelling Labelling
%Every query returns a new different partitioning (not only minimal)
sKnife(AppId,Labelling, Partitioning) :-
    application(AppId, Hardware, Software),
    hardwareOK(Labelling,Hardware),
    softwareLabel(Labelling,Software, LabelledSoftware),
    softwareOk(Labelling,LabelledSoftware),
    partitioning(LabelledSoftware,0, inf,[], Partitioning).

%Finds Partitioning with at most PLimit partitions for appliction AppId given the labelling Labelling
%Every query returns a new different partitioning (not only minimal)
sKnife(AppId, Labelling,PLimit,Partitioning) :-
    application(AppId, Hardware, Software),
    hardwareOK(Labelling,Hardware),
    softwareLabel(Labelling,Software, LabelledSoftware),
    softwareOk(Labelling,LabelledSoftware),
    partitioning(LabelledSoftware, 0,PLimit,[], Partitioning).


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
    dataLabel(Labelling,Data,Labels),
    highestType(Labels,MaxType),
    characteristicsLabel(Labelling,Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).
