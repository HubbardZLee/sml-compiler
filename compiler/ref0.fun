[
        (p,(id"main3",id"x",Inttp,Inttp,Id(id"x"))),
        (p,(id"main",id"x",Inttp,Tupletp [Inttp, Inttp],Tuple([Int 1, Int 9, Int 8]))),
        (p,(id"dmain",id"x",Inttp,Tupletp [Inttp, Inttp],Tuple([Int 1, Int 8]))),
        (p,(id"mfain",id"x",Inttp, Inttp,Proj(1, Tuple([Int 1, Id(id"x")])))),
        (p,(id"2tupout",id"x",Inttp, Tupletp([Inttp, Inttp]),If (Proj(0, Proj(1, Tuple([Id(id"x"), Tuple([Id(id"x")]), Int 9]))), Tuple([Int 9, Id(id"x"), Int 2]), Tuple([Int 8, Int 9, Id(id"x"), Int 8])))),
        (p,(id"2tupin",id"x",Tupletp([Inttp, Inttp]), Inttp, Proj(1, Id(id"x")))),
        (p,(id"calltest",id"x",Inttp, Inttp, Call(Id(id"2tupin"), Call(Id(id"2tupout"), Id(id"x"))))),
        (p,(id"optest",id"x",Inttp, Inttp, Op(Sub, [Int 3, Op(Sub, [Id(id"x"), Op(Sub, [Int 9, Proj(0,Tuple([Call(Id(id"main3"), Int 4)]))])])]))),
        (p,(id"whiletest",id"x",Inttp,Tupletp([]),While(Int 3, Int 4))),
        (p,(id"lettest",id"x",Inttp,Inttp,Let(id"y", Call(Id(id"2tupout"), Id(id"x")), Call(Id(id"2tupin"), Id(id"y"))))),
        (p,(id"reftest",id"x",Inttp,Inttp,Let(id"y", Op(Ref, [Tuple([Int 2, Int 3, Id(id"x")])]), Int 1))),
        (p,(id"refwhiletest",id"x",Inttp,Tupletp([]),Let(id"y", Op(Ref, [Int 10]), While(Op(Get, [Id(id"y")]), Op(Set, [Id(id"y"), Op(Sub, [Op(Get, [Id(id"y")]), Int 1])])))))            
                ]
