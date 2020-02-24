structure Test = struct
open Absyn

  val p : ErrorMsg.pos*ErrorMsg.pos = (0,0)
  val id = Symbol.symbol

  val test1 = [
        (p,(id"main",id"x",Inttp,Inttp,Id(id"x"))),
        (p,(id"main3",id"x",Inttp,Tupletp([Inttp, Inttp, Inttp]),Tuple([Int 1, Int 9, Int 8]))),
        (p,(id"dmain",id"x",Inttp,Tupletp [Inttp, Inttp],Tuple([Int 1, Int 8]))),
        (p,(id"mfain",id"x",Inttp, Inttp,Proj(1, Tuple([Int 1, Id(id"x")])))),
        (p,(id"2tupout",id"x",Inttp, Tupletp([Inttp, Inttp, Inttp]),If (Proj(0, Proj(1, Tuple([Id(id"x"), Tuple([Id(id"x")]), Int 9]))), Tuple([Int 9, Id(id"x"), Int 2]), Tuple([Int 8, Int 9, Id(id"x"), Int 8])))),
        (p,(id"2tupin",id"x",Tupletp([Inttp, Inttp]), Inttp, Proj(1, Id(id"x")))),
        (p,(id"calltest",id"x",Inttp, Inttp, Call(Id(id"2tupin"), Call(Id(id"2tupout"), Id(id"x"))))),
        (p,(id"optest",id"x",Inttp, Inttp, Op(Sub, [Int 3, Op(Sub, [Id(id"x"),
        Op(Sub, [Int 9, Proj(0,Tuple([Call(Id(id"main"), Int 4)]))])])]))),
        (p,(id"whiletest",id"x",Inttp,Tupletp([]),While(Int 3, Int 4))),
        (p,(id"lettest",id"x",Inttp,Inttp,Let(id"y", Call(Id(id"2tupout"), Id(id"x")), Call(Id(id"2tupin"), Id(id"y"))))),
        (p,(id"reftest",id"x",Inttp,Inttp,Let(id"y", Op(Ref, [Tuple([Int 2,
        Int 3, Id(id"x")])]), Int 1)))
  (*      (p,(id"refwhiletest",id"x",Inttp,Tupletp([]),Let(id"y", Op(Ref, [Int
        10]), While(Op(Get, [Id(id"y")]), Op(Set, [Id(id"y"), Op(Sub, [Op(Get,
        [Id(id"y")]), Int 1])])))))           *)
                        ]
  
  val error1 = [
    (p,(id"y",id"arg",Tupletp([Inttp, Inttp]),Inttp,Proj (1, Id(id"arg")))),  
    (p,(id"x",id"arg",Inttp,Tupletp([Inttp, Inttp, Inttp]),Tuple([Id(id"arg"), Int 3, Id(id"arg")]))),
    (p,(id"main",id"arg",Inttp,Inttp,Call(Id(id"y"), Call(Id(id"x"), Id(id"arg")))))
                ]
  
  fun error() = Codegen.codegen(error1);
  fun test() = Codegen.codegen(test1);










  val errortest = [
       (p,(id"y",id"arg",Tupletp([Inttp, Inttp]),Inttp,Let (id"d", If (Int 3, 
       Tuple([Int 3, Id(id"arg"), Int 8, Int 9]), 
       Tuple([Int 9, Tuple([Int 9, Int 9, Tuple ([Int 9])]), Tuple([Int 9]), Int 0, Int 3])), 
        Id(id"d")))),
       (p,(id"main",id"p",Inttp,Inttp,Int 0))
                    ]
  
  fun etest() = TypeCheck.tc(errortest);





















end
