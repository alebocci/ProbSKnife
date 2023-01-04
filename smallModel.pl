application(smallModel,[],[north, east, west, south]).
%application(smallExample2,[hw1],[north, east, west, south]).


software(north,
        [data1,data2,data3],
        [charact1],
        10,
        ([],[east,west,south])
        ).

software(east,
        [data2, data4,data6],
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

tagChange(charact1,top,0.5).
tagChange(charact1,low,0.5).
tagChange(charact2,top,0.5).
tagChange(charact2,low,0.5).
tagChange(charact3,top,0.5).
tagChange(charact3,low,0.5).
tagChange(charact4,top,0.5).
tagChange(charact4,low,0.5).

tagChange(data1,top,0.5).
tagChange(data1,low,0.5).
tagChange(data2,top,0.5).
tagChange(data2,low,0.5).
tagChange(data3,top,0.5).
tagChange(data3,low,0.5).
tagChange(data4,top,0.5).
tagChange(data4,low,0.5).
tagChange(data5,top,0.5).
tagChange(data5,low,0.5).
tagChange(data6,top,0.5).
tagChange(data6,low,0.5).