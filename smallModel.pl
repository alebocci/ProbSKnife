application(smallExample,[],[north, east, west, south]).
%application(smallExample2,[hw1],[north, east, west, south]).


software(north,
        [data1,data2,data3],
        [charact1],
        10,
        ([],[east,west,south])
        ).

software(east,
        [data2, data3,data6],
        [charact2],
        10,
        ([],[north,west,south])
        ).

software(west,
        [data1,data4,data5],
        [charact3],
        10,
        ([],[north,east,south])
        ).

software(south,
        [data3, data5,data6],
        [charact4],
        10,
        ([],[north,east,west])
        ).

hardware(hw1,[],[],([],north)).

link(north,east).
link(north,west).
link(north,south).
link(east,west).
link(east,south).
link(west,south).

g_lattice_higherThan(top, low).


tag(charact1,top).
tag(charact2,top).
tag(charact3,top).
tag(charact4,top).

tag(data1, top).
tag(data2, top).
tag(data3, top).
tag(data4, top).
tag(data5, top).
tag(data6, top).

dataCharList([charact1,charact2,charact3,charact4,data1,data2,data3,data4,data5,data6]).
startingLabelling([(charact1, top), (charact2, top), (charact3, top), (charact4, top), (data1, top), (data2, top), (data3, top), (data4, top), (data5, top),(data6, top)]).