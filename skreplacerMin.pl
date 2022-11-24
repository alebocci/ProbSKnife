%skReplace(iotApp1,[((top, safe), [diskController, lockController, cameraController, network2Controller, authentication, businessLogic, userConfig]),  ((medium, safe), [lightsController, thermostatController]),  ((low, safe), [network1Controller]),  ((top, low), [aiLearning])], [(wantedTemp,low)],N). 
%skReplace(iotApp1,[((top, safe), [diskController, lockController, cameraController]),((top, safe), [network2Controller, authentication, businessLogic, userConfig]),  ((medium, safe), [lightsController, thermostatController]),  ((low, safe), [network1Controller]),  ((top, low), [aiLearning])], [(wantedTemp,low)],N).
skReplace(AppId, OldPartitions, OldLinks, NewLabels, Partitions) :-
    application(AppId, Hardware, Software),
    impactedHW(NewLabels,Hardware),
    softwareLabel(NewLabels,Software,NewTypedSw),
    softwareOk(NewLabels,NewTypedSw),
    once(calculatePartitions(OldPartitions,OldLinks,NewLabels,Software,NewTypedSw,Partitions)).

calculatePartitions(OldPartitions,_,_,Software,NewTypedSw,OldPartitions):-
    softwareLabel([],Software,OldTypedSw),
    tcTosafe(OldTypedSw,OldT),
    tcTosafe(NewTypedSw,NewT),
    findall(SW,(member(SW,OldT),\+ member(SW,NewT)),[]).

calculatePartitions(_,Links,NewLabels,_,NewTypedSw,Partitions):-
    newConnected(NewLabels,Links,InP,OutP),%pairs of components to leave together(InP) or not (OutP)
    clusterP(NewLabels,InP,StartingP),%generate a starting partitioning with InP
    removeClusteredP(NewTypedSw,InP,SwToPartition),%software not present in starting partitioning
    once(newPartitioning(SwToPartition,OutP,StartingP,Partitions)).

tcTosafe([(Sw,TData,TChar)|SWs],[(Sw,TData,TP)|TempSWs]):- %to avoid this put directly safe in sw labelling
    once(partitionCharLabel(TChar,TData,TP)),
    tcTosafe(SWs,TempSWs).
tcTosafe([],[]).
%check the labelling of hardware components using new labels
impactedHW(NewLabels,Hardware):-
    findall(Hw,
        (member(Hw, Hardware),
        hardware(Hw, Data,Characteristics,_),
        member((Label,NewType), NewLabels),
        tag(Label,OldType),
        NewType \= OldType,
        (member(Label, Data);member(Label, Characteristics))
        ),
        ImpactedHW
    ),
    hardwareOK(NewLabels, ImpactedHW).

hardwareOK(NewLabels,[H|Hs]) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(NewLabels,Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(NewLabels,Characteristics, CLabel), lowestType(CLabel, MinCType),
    gte(MinCType, MaxDType), %check if an hardware component is trusted for the level of its data
    hardwareOK(NewLabels,Hs).
hardwareOK(_,[]).

impactedSW(NewLabels,Software,TypeChangedSW):-
    findall(Sw,
        (member(Sw, Software),
        software(Sw, Data,Characteristics,_,_),
        member((Label,NewType), NewLabels),
        tag(Label,OldType),
        NewType \= OldType,
        (member(Label, Data);member(Label, Characteristics))
        ),
        LabelSW
    ),
    list_to_set(LabelSW,ImpactedSW),
    softwareLabel([],Software,TypedSW),
    softwareLabel(NewLabels,ImpactedSW,TypedImpactedSW),
    findall(TSw,(member(TSw,TypedImpactedSW),\+ member(TSw,TypedSW)),TypeChangedSW),
    softwareOk(NewLabels,TypeChangedSW).


%labels list of software components with Type of Data and  Type of Characteristics
softwareLabel(NewTags,[Sw|Sws],[(Sw,TData,TChar)|LabelledSws]):-
    software(Sw, Data,Characteristics,_,_),
    labelSw(NewTags, Data, Characteristics,(TData,TChar)),
    softwareLabel(NewTags,Sws,LabelledSws).
softwareLabel(_,[],[]).
%single software labelling
softwareLabel(NewTags,Sw,(Sw,TData,TChar)):-
    software(Sw, Data,Characteristics,_,_),
    labelSw(NewTags, Data, Characteristics,(TData,TChar)).

softwareOk(NewLabels,LabelledSoftware):-
    \+ (
        member((Sw,TData,TChar), LabelledSoftware),
        lt(TChar,TData),
        externalLeak(NewLabels, [Sw], [] ,TData, LabelledSoftware)
      ).

externalLeak(NewLabels, LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,_)),
    \+  trustedHW(NewLabels, LinkedHW, TData).

externalLeak(NewLabels, LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,_,(LinkedHW,VisitLinkedSW)),
    trustedHW(NewLabels, LinkedHW, TData),
    externalLeak(NewLabels,VisitLinkedSW, [Sw|Visited], TData, LabelledSoftware).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(NewTags, LinkedHW, TData):-
    \+ (
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(NewTags,Characteristics, CLabels), lowestType(CLabels, MinCType),
        lt(MinCType, TData)
        ).

newConnected(NewLabels, [(Sw1,Sw2,_,in)|OldLinks],[P|InPairs],OutPairs):-
    softwareLabel(NewLabels,Sw1, (Sw1,TD1,TC1)),
    softwareLabel(NewLabels,Sw2, (Sw2,TD2,TC2)),
    compatibleTypedSw((Sw1,TD1,TC1),(Sw2,TD2,TC2)),
    P = (Sw1,Sw2),
    newConnected(NewLabels,OldLinks,InPairs,OutPairs).
newConnected(NewLabels, [(Sw1,Sw2,_,in)|OldLinks],InPairs,[P|OutPairs]):-
    softwareLabel(NewLabels,Sw1, (Sw1,TD1,TC1)),
    softwareLabel(NewLabels,Sw2, (Sw2,TD2,TC2)),
    \+ compatibleTypedSw((Sw1,TD1,TC1),(Sw2,TD2,TC2)),
    P = (Sw1,Sw2),
    newConnected(NewLabels,OldLinks,InPairs,OutPairs).
newConnected(NewLabels, [(Sw1,Sw2,_,out)|OldLinks],InPairs,[P|OutPairs]):-
    P = (Sw1,Sw2),
    newConnected(NewLabels,OldLinks,InPairs,OutPairs).
newConnected(_,[],[],[]).

compatibleTypedSw((_,TD,TC1),(_,TD,TC2)):-
   partitionCharLabel(TC1,TD,TCP),
   partitionCharLabel(TC2,TD,TCP).

clusterP(NewLabels,InP,InPartitions):-
    once(findClusters(InP,ClusteredP)),
    labelCluster(NewLabels,ClusteredP,InPartitions).

findClusters([(S1,S2)|InP],SubCluster):-
    findClusters(InP,SubCluster),
    member(L,SubCluster),
    member(S1,L),
    member(S2,L).
findClusters([(S1,S2)|InP],[[S2|L]|TmpCluster]):-
    findClusters(InP,SubCluster),
    member(L,SubCluster),
    member(S1,L),
    \+ member(S2,L),
    select(L,SubCluster,TmpCluster).
findClusters([(S1,S2)|InP],[[S1|L]|TmpCluster]):-
    findClusters(InP,SubCluster),
    member(L,SubCluster),
    \+ member(S1,L),
    member(S2,L),
    select(L,SubCluster,TmpCluster).
findClusters([(S1,S2)|InP],[[S1,S2]]):-
    findClusters(InP,SubCluster),
    \+ member(_,SubCluster).
findClusters([(S1,S2)|InP],[[S1,S2]|SubCluster]):-
    findClusters(InP,SubCluster),
    member(L,SubCluster),
    \+ member(S1,L),
    \+ member(S2,L).
findClusters([],[]).

labelCluster(NewLabels,[[S|Ss]|Cs],[((TD,TP),[S|Ss])|ClusteredP]):-
    softwareLabel(NewLabels,S, (S,TD,TC)),
    partitionCharLabel(TD,TC,TP),
    labelCluster(NewLabels,Cs,ClusteredP).
labelCluster(_,[],[]).

removeClusteredP(NewTypedSw,InP,SwToPartition):-
    findall(S,(member((S,_),InP);member((_,S),InP)),Sws),
    list_to_set(Sws,ToRemoveSws),
    findall((S2,TD,TC),(member((S2,TD,TC),NewTypedSw),\+(member(S2,ToRemoveSws))),SwToPartition).

newPartitioning([(S,TData,TChar)|Ss], OutP,Partitions,NewPartitions) :-
    software(S,_,_,_,_), %for debug
    once(partitionCharLabel(TChar,TData,TCP)),
    \+ member( ((TData,TCP), _), Partitions),
    P = ( (TData,TCP), [S]),
    newPartitioning(Ss, OutP,[P|Partitions],NewPartitions).
newPartitioning([(S,TData,TChar)|Ss], OutP,Partitions,NewPartitions) :-
    software(S,_,_,_,_),
    once(partitionCharLabel(TChar,TData,TCP)),
    member( ((TData,TCP), P), Partitions),
    \+ linked(S,P,OutP),
    select( ((TData,TCP), P), Partitions, TmpPartitions),
    PNew = ( (TData,TCP), [S|P]),
    newPartitioning(Ss, OutP,[PNew|TmpPartitions],NewPartitions).
newPartitioning([(S,TData,TChar)|Ss], OutP,Partitions,NewPartitions) :-
    software(S,_,_,_,_),
    once(partitionCharLabel(TChar,TData,TCP)),
    member( ((TData,TCP), P), Partitions),
    linked(S,P,OutP),
    PNew = ( (TData,TCP), [S]),
    newPartitioning(Ss, OutP,[PNew|Partitions],NewPartitions).
newPartitioning([],_,P,P).



linked(S1,P,Link):-
    findall(S2, (member(S2,P),(member((S1,S2),Link);member((S2,S1),Link))),S2s),
    dif(S2s,[]). % use cut to avoid findall



updatePartitions([(S,TData,TChar)|Ss], Partitions,NewPartitions):-
    partitionCharLabel(TChar,TData,TCP),
    member( ((TData,TCP), P), Partitions),
    member(S,P),
    updatePartitions(Ss, Partitions, NewPartitions).
updatePartitions([(S,TData,TChar)|Ss], Partitions,NewPartitions):-
    software(S,_,_,_,_),
    partitionCharLabel(TChar,TData,TCP),
    select( ((TData,TCP), P), Partitions, TmpPartitions),
    \+ member(S,P),
    PNew = ( (TData,TCP), [S|P]),
    removeSw(S,TmpPartitions,UpdatedPartitions),
    updatePartitions(Ss, [PNew|UpdatedPartitions], NewPartitions).
updatePartitions([(S,TData,TChar)|Ss], Partitions,NewPartitions):-
    software(S,_,_,_,_),
    partitionCharLabel(TChar,TData,TCP),
    \+ member( ((TData,TCP), _), Partitions),
    P = ( (TData,TCP), [S]),
    removeSw(S,Partitions,UpdatedPartitions),
    updatePartitions(Ss, [P|UpdatedPartitions], NewPartitions).
updatePartitions([],P,P).

removeSw(S,[((TD,TC), Sws)|Ps],[((TD,TC), Sws)|UpdatedPartitions]):-
    \+ (member(S,Sws)),
    removeSw(S,Ps, UpdatedPartitions).

removeSw(S,[((TD,TC), Sws)|Ps],[((TD,TC), NewSws)|Ps]):-
    (member(S,Sws)),
    select(S,Sws,NewSws),
    software(S,_,_,_,_).