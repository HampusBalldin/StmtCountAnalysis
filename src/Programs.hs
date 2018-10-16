module Programs where
import Stmt
import CFG

program1 = Compound [ If (NatVal 1 :<: NatVal 2) Skip Skip ]
program2 = Compound [ Skip, Skip,  ret0, Skip, Skip ]
program22 = Compound [ Skip, Skip,  program2]
program3 = Compound []
program4 = Compound [ Skip
                      ,I "Hampus" :<-: ValExpr (NatVal 1337)
                      ,Skip
                      ,Return (ValExpr (NatVal 0))
                      ,Skip
                    ]

ret0 = Return (ValExpr (NatVal 0))
program5 = Compound [
             Skip
            ,I "Hampus" :<-: ValExpr (NatVal 1337)
            ,Skip
            ,Skip
            ,Twice $ If (NatVal 0 :==: NatVal 0) program2 ret0
            ,ret0
        ]

program6 = Compound [
             Skip
            -- ,I "Hampus" :<-: ValExpr (NatVal 1337)
            ,Skip
            ,While (NatVal 0 :==: NatVal 0) program5
            ,Twice program4
            ,Twice $ If (NatVal 0 :==: NatVal 0) program2 Skip
        ]

program7 = Twice $ Twice $ Twice Skip
program8 = Twice $ If (NatVal 0 :==: NatVal 0) program2 Skip


program9 = Compound [
        Skip
        ,If (NatVal 0 :==: NatVal 0) Skip Skip
        ,Skip
            ,If (NatVal 0 :==: NatVal 0) Skip Skip
        ,Skip
    ]

program10 = Compound [ Twice $ While (NatVal 0 :==: NatVal 0) $ Compound [
               While (NatVal 0 :==: NatVal 0) $ 
                  While (NatVal 0 :==: NatVal 0) Skip
               , Skip], ret0 ]


programif = If (NatVal 0 :==: NatVal 0) Skip Skip
programwhile = While (NatVal 0 :==: NatVal 0) Skip
programtwice = Twice $ While (NatVal 0 :==: NatVal 0) Skip

valexpr = ValExpr . NatVal
programexample = Compound [ I "x" :<-: valexpr 0, nwhile, ret0]
    where
        nwhile = While (IdVal (I "x") :<: NatVal 10) $
                        While (IdVal (I "x") :<: NatVal 20) $
                            If (IdVal (I "x") :==: NatVal 5) (
                                Compound [
                                            I "x" :<-: (IdVal (I "x") :+: NatVal 1)
                                        ,Twice . Twice $ Skip
                                        ]
                            ) Skip

forwhile = Compound [ I "x" :<-: valexpr 0, nwhile, ret0]
    where
      nwhile =  While (IdVal (I "x") :<: NatVal 10) $
                Compound [
                            Skip
                            ,I "x" :<-: (IdVal (I "x") :+: NatVal 1)
                         ]

                
forwhileif = Compound [ I "y" :<-: valexpr 0, I "x" :<-: valexpr 0, nwhile, ret0]
    where
      nwhile =  While (IdVal (I "x") :<: NatVal 10) $
                Compound [
                            Skip
                            ,If (IdVal (I "x") :<: NatVal 5) Skip Skip
                            ,If (IdVal (I "y") :==: NatVal 0) Skip Skip
                            ,I "x" :<-: (IdVal (I "x") :+: NatVal 1)
                         ]

nestedwhile = Compound [ I "x" :<-: valexpr 0, while1, ret0]
    where
      while1 = While (IdVal (I "x") :<: NatVal 10) $
        Compound [
        I "y" :<-: valexpr 0
        ,While (IdVal (I "y") :<: IdVal (I "x")) $
         I "y" :<-: (IdVal (I "y") :+: NatVal 1)
        ,I "x" :<-: (IdVal (I "x") :+: NatVal 1)
                 ]
