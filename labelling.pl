:- consult('mainPr').

allLabelling([DC|DCs],[(DC,L)|Labelling]):-
    tagChange(DC,L),
    allLabelling(DCs,Labelling).
allLabelling([],[]).

tagChange(charact1,top).
tagChange(charact1,low).
tagChange(charact2,top).
tagChange(charact2,low).
tagChange(charact3,top).
tagChange(charact3,low).
tagChange(charact4,top).
tagChange(charact4,low).
tagChange(data1,top).
tagChange(data1,low).
tagChange(data2,top).
tagChange(data2,low).
tagChange(data3,top).
tagChange(data3,low).
tagChange(data4,top).
tagChange(data4,low).
tagChange(data5,top).
tagChange(data5,low).
tagChange(data6,top).
tagChange(data6,low).