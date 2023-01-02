%%%% ProbLog does not support dif/2 %%%%
dif(A,B):- A\=B.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%p_i::tagChange(DataCharacteristic, SecLabel)
%probability that the data or characterstic DC assume the label SecLabel

%probability of a labelling is conjunction of al data\characterstic probability
%Labelling = list (DataCharacterstic, Label) with unique DataCharacteristic
%labelling(ListOfDataCharacterstic,Labelling)
labelling([DC|DCs],[(DC,L)|Labelling]):-
    tagChange(DC,L),
    labelling(DCs,Labelling).
labelling([],[]).

%probability of a labelling given K changes from the starting labelling, D is number of different labels (D always <= K)
labellingK(K,L,D):-
    dataCharList(Datas),
    labellingK(Datas,K,L,D).

labellingK([DC|DCs],K,[(DC,L)|Labelling],Diff):-
    labellingK(DCs,K,Labelling,Diff),
    tagChange(DC,L),
    tag(DC,L).
labellingK([DC|DCs],K,[(DC,L)|Labelling],NewDiff):-
    labellingK(DCs,K,Labelling,Diff),
    tagChange(DC,L),
    \+tag(DC,L),
    NewDiff is Diff + 1,
    NewDiff =< K.
labellingK([],_,[],0).