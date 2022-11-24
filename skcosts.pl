%Partition: (DataLabel, CharLabel, [Sw1,Sw2,Sw3...])
:- consult('utils').
:- consult('changeProb').

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
    probCalc(0.0,Labels, Data, DataP),
    probCalc(0.0,Labels, Charact, CharactP),
    removeLabellings(Labellings, (TData,TChar), CharactP,UseLabellings), %TODO calcola probabilità della TData,TChar poi 1-
    labellingsCost(UseLabellings, DataP, CharactP, LabCost),
    TotCost is LabCost * Cost.

removeLabellings(Labellings, (TData,TChar), CharactP, Res):-
    dif(CharactP,[(safe,1.0)]),
    select((TData,TChar), Labellings, Res).
removeLabellings(Labellings, (TData,TChar), [(safe,1.0)], Res):-
    select((TData,TChar), Labellings, TmpLabs),
    findall((Data,safe), member((Data,safe), TmpLabs), Res).

%Tchar = safe non la trova
labellingsCost([(TData,TChar)|Ls], DataP, CharactP, TotCost):-
    member((TData,DP), DataP),
    member((TChar,CP), CharactP),
    LCost is DP * CP,
    labellingsCost(Ls, DataP, CharactP, SubCost),
    TotCost is LCost + SubCost.
labellingsCost([],_,_,0.0).

probCalc(Prec,[L|Ls], Data, [(L,Res)|LsRes]):-
    dif(Data,[]),
    lowerL(L, Ls, Data, Lower),
    %Upper is 1.0 - Prec,
    Res is 1 - (Prec + Lower),
    %Res is (1.0 - Lower) * Upper,
    Next is Prec + Res,
    probCalc(Next, Ls, Data, LsRes).
probCalc(_,[],Data,[]):- dif(Data,[]).
probCalc(_,_,[],[(safe,1.0)]).

lowerL(L, Ls, [D|Ds], Res):-
    findall(P, (tagChange(D,Label,P),member(Label,Ls)),Ps),
    sumList(Ps,SumProb),
    lowerL(L, Ls, Ds, LowerRes),
    Res is LowerRes * SumProb.
lowerL(_,_,[],1.0).
    
sumList([N|L],Sum):-
    sumList(L,S),
    Sum is N + S.
sumList([],0.0).


allLabellings(Partitions,(AllLabels,[(Lowest,safe)|CList])):-
	findall(T, g_lattice_higherThan(T,_),Labels),
	list_to_set(Labels, LabelsSet),
	findall((TData,TChar),member((TData,TChar,_),Patitions, CList),
%    buildClist(LabelsSet,CList),
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
    (P = 1.0;P=0.9999999999999999),
    checkProbTags(Ds,Labels).
checkProbTags([],_).
checkLabelProb(D,[L|Ls],Sum):-
    tagChange(D,L,P),
    checkLabelProb(D,Ls,LSum),
    Sum is P + LSum.
checkLabelProb(_,[],0).