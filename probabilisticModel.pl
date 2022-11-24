%%%% ProbLog does not support dif/2 %%%%
dif(A,B):- \+(A==B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%p_i::tagChange(DataCharacteristic, SecLabel)
%probability that the data or characterstic DC assume the label SecLabel
0.5::tagChange(charact1,top);0.5::tagChange(charact1,low).
0.5::tagChange(charact2,top);0.5::tagChange(charact2,low).
0.5::tagChange(charact3,top);0.5::tagChange(charact3,low).
0.5::tagChange(charact4,top);0.5::tagChange(charact4,low).

0.5::tagChange(data1,top);0.5::tagChange(data1,low).
0.5::tagChange(data2,top);0.5::tagChange(data2,low).
0.5::tagChange(data3,top);0.5::tagChange(data3,low).
0.5::tagChange(data4,top);0.5::tagChange(data4,low).
0.5::tagChange(data5,top);0.5::tagChange(data5,low).
0.5::tagChange(data6,top);0.5::tagChange(data6,low).

%probability of a labelling is conjunction of al data\characterstic probability
%labelling = list (DataCharacterstic, Label)
labelling(Labl, Plabl) :- subquery(labelling(Labl),Plabl).
labelling([(DC,L)|Dcs]):-
    tagChange(DC,L),
    labelling(Dcs).
labelling([]).

%probability of changing the labelling from starting to labelling given K changes
%labellingChange(StartingLabelling,EndingLabelling, K)
%                   {0                               if St - End > K  (different labels between St and End are more than K)
%Prob(St ->_k End) ={labelling(End)                  if St = End
%                   {labelling(St)*(1-labelling(End))/(Sum_i labelling(I)) where I is labelling with St - I <= K
labellingChange(StartingLabelling,StartingLabelling,_,_,Pstarting):-
    subquery(labelling(SL),Pstarting).
labellingChange(StartingLabelling,EndingLabelling,K,SumL,Pchange):-
    dif(EndingLabelling,StartingLabelling),
    difLabels(StartingLabelling,EndingLabelling,Diff),
    length(Diff,L), L =< K,
    labelling(EndingLabelling,Pending), 
    labelling(StartingLabelling,Pstarting),
    Pchange is Pending * (1-Pstarting) / SumL.

%calculate the disjunction probability of a list of labellings
disjunctionLabellings([Labl|Labls],Pres):-
    subquery(labelling(Labl),Plabl),
    disjunctionLabellings(Labls,Plabls),
    Pres is Plabl + Plabls.
disjunctionLabellings([],0).


%Diff is list containing labels different in Labelling1 and Labelling2
difLabels(Labelling1,Labelling2,Diff):-
    findall((DC,L),(member((DC,L),Labelling1),\+ member((DC,L),Labelling2)),Diff).

/*
%%%%%%%%%%%%%%%%%%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%

%example starting labelling
startingLabelling([(data1,top),(data2,top),(charact1,top)]).



testFinal(Lb,Combinations,K,P):-
    startingLabelling(S),
    disjunctionLabellings(Combinations,Pcomb),
    labellingChange(S,Lb,K,Pcomb,P).

%query(testFinal([(data1,low),(data2,top),(charact1,top)], [[(data1,top),(data2,low),(charact1,top)],[(data1,top),(data2,top),(charact1,low)],[(data1,low),(data2,top),(charact1,top)]],1,P)).
*/