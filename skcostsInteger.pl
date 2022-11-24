%Partition: (DataLabel, CharLabel, [Sw1,Sw2,Sw3...])
:- consult('utils').
:- consult('changeProbInteger').

partitioningCost(Partitions, Costs):-
    allLabellings(Partitions,Labellings),
    partitioningCost(Labellings, Partitions, Costs).

partitioningCost(Labellings, [P|Ps], (TotNum,TotCost)):-
    partitionCost(Labellings, P,CostP),
    partitioningCost(Labellings, Ps,(Num,Cost)),
    TotNum is Num + 1,
    TotCost is CostP + Cost.
partitioningCost(_,[],(0,0)).

partitionCost(Labellings,((TData,TChar), [S|Sws]),TotCost):-
    softwareCost(Labellings, S, (TData,TChar), SCost),
    partitionCost(Labellings, ((TData,TChar), Sws),Costs),
    TotCost is SCost + Costs.
partitionCost(_,(_,[]),0).

softwareCost((Labels, Labellings), S, (TData,TChar), TotCost):-
    software(S, Data, Charact, Cost, _),
    probCalc(0,Labels, Data, DataP),
    probCalc(0,Labels, Charact, CharactP),
    removeLabellings(Labellings, (TData,TChar), CharactP,UseLabellings), %TODO calcola probabilità della TData,TChar poi 1-
    labellingsCost(Labels,UseLabellings, DataP, CharactP, LabCost),
    TotCost is LabCost * Cost //100.

removeLabellings(Labellings, (TData,TChar), CharactP, Res):-
    dif(CharactP,[(safe,100)]),
    once(select((TData,TChar), Labellings, Res)).
removeLabellings(Labellings, (TData,TChar), [(safe,100)], Res):-
    once(select((TData,TChar), Labellings, TmpLabs)),
    findall((Data,safe), member((Data,safe), TmpLabs), Res).

%Tchar = safe non la trova
labellingsCost(Labels,[(TData,safe)|Ls], DataP, CharactP, TotCost):-
    member((TData,DP), DataP),
    safeCost(Labels,TData, CharactP, CP),
    LCost is DP * CP //100,
    labellingsCost(Labels,Ls, DataP, CharactP, SubCost),
    TotCost is LCost + SubCost.
labellingsCost(Labels,[(TData,TChar)|Ls], DataP, CharactP, TotCost):-
    dif(TChar,safe),
    member((TData,DP), DataP),
    member((TChar,CP), CharactP),
    LCost is DP * CP //100,
    labellingsCost(Labels,Ls, DataP, CharactP, SubCost),
    TotCost is LCost + SubCost.
labellingsCost(_,[],_,_,0).

safeCost([L|Ls],TData, CharactP, Res):-
    dif(L,TData),
    safeCost(Ls,TData, CharactP, Res).
safeCost([TData|Ls],TData, CharactP, Res):-
    dif(Ls,[]),
    findall(P,(member(L,Ls),member((L,P), CharactP)),Probs),
    sumList(Probs, Res).
safeCost([TData], TData, _,100).


probCalc(Prec,[L|Ls], Data, [(L,Res)|LsRes]):-
    dif(Data,[]),
    lowerL(L, Ls, Data, Lower),
    %Upper is 1.0 - Prec,
    Res is 100 - (Prec + Lower),
    %Res is (1.0 - Lower) * Upper,
    Next is Prec + Res,
    probCalc(Next, Ls, Data, LsRes).
probCalc(_,[],Data,[]):- dif(Data,[]).
probCalc(_,_,[],[(safe,100)]).

lowerL(L, Ls, [D|Ds], Res):-
    findall(P, (tagChange(D,Label,P),member(Label,Ls)),Ps),
    sumList(Ps,SumProb),
    lowerL(L, Ls, Ds, LowerRes),
    Res is LowerRes * SumProb //100.
lowerL(_,_,[],100).
    
sumList([N|L],Sum):-
    sumList(L,S),
    Sum is N + S.
sumList([],0).


allLabellings(Partitions,(AllLabels,[(Lowest,safe)|Labellings])):-
	findall(T, g_lattice_higherThan(T,_),Labels),
	list_to_set(Labels, LabelsSet),
    buildClist(LabelsSet,CList),
	findall((TData,TChar),member(((TData,TChar),_),Partitions), Existing),
    subtractList(Existing, CList, Difference),
    append(CList,Difference, Labellings),
    lowestType(Lowest),
    append(LabelsSet, [Lowest], AllLabels).

buildClist([L|Ls],[(L,safe)|CList]):-
	findall((L,B), lattice_higherThan(L,B),Res),
	buildClist(Ls,SubRes),
	append(Res,SubRes,CList).
buildClist([],[]).    

checkChangeProb():-
    findall(D, tag(D,_),Data),
    allLabellings((Labels,_)),
    checkProbTags(Data,Labels).
checkProbTags([D|Ds],Labels):-
    checkLabelProb(D,Labels,P),
    P = 100,
    checkProbTags(Ds,Labels).
checkProbTags([],_).
checkLabelProb(D,[L|Ls],Sum):-
    tagChange(D,L,P),
    checkLabelProb(D,Ls,LSum),
    Sum is P + LSum.
checkLabelProb(_,[],0).

subtractList(L1,[E2|L2s],Lres):-
    dif(L1,[]),
    once(select(E2, L1, L1s)),
    subtractList(L1s,L2s,Lres).
subtractList(L1,[E2|L2s],Lres):-
    dif(L1,[]),
    \+ member(E2,L1),
    subtractList(L1,L2s,Lres).
subtractList([],A,[]):- dif(A,[]).
subtractList(A,[],A):- dif(A,[]).
subtractList([],[],[]).