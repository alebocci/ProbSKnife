application(smallExample,[],[north, east, west, south]).
%application(smallExample2,[hw1],[north, east, west, south]).


software(north,
        [data4],
        [charact1],
        10,
        ([],[east,west,south])
        ).

software(east,
        [data4, data5],
        [charact2],
        10,
        ([],[north,west,south])
        ).

software(west,
        [data2, data5],
        [charact3],
        10,
        ([],[north,east,south])
        ).

software(south,
        [data2, data5],
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

%tag(data1, top).
tag(data2, top).
%tag(data3, top).
tag(data4, top).
tag(data5, top).
%tag(data6, top).