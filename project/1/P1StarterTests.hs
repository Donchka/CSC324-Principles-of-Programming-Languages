{-|
Module: P1StarterTests
Description: Tests for Project 1
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}

module P1StarterTest where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import P1 (computeSpreadsheet, evalDeer)
import P1Types (Spreadsheet(..), Definition(..), Column(..), Expr(..), 
                Value(..), Env, exampleSpreadsheet)

import Data.Map
--Some simple tests to get you started--

prop_testValCols :: Property
prop_testValCols = label "a spreadsheet with only value columns" $
  let spreadsheet = Spreadsheet [] [ValCol "id" [VNum 1, VNum 2, VNum 3]]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1, VNum 2, VNum 3]]


prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "a spreadsheet with a basic identifier" $
  let spreadsheet = Spreadsheet [Def "basic-var" (Literal $ VStr "test")]
                                [ValCol "id" [VNum 1.0],
                                 ComputedCol "output-column" (Id "basic-var")]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1.0],
                ValCol "output-column" [VStr "test"]]


prop_testBuiltinIdentifier :: Property
prop_testBuiltinIdentifier = label "computedCol using a builtin identifier" $
  let spreadsheet = Spreadsheet [] 
                                [ValCol "id" [VNum 1, VNum 2, VNum 3],
                                 ComputedCol "times-10" 
                                   (Builtin "*" 
                                    [Literal $ VNum 10, Id "id"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "id" [VNum 1, VNum 2, VNum 3],
                ValCol "times-10" [VNum 10, VNum 20, VNum 30]]


prop_testBasicError :: Property
prop_testBasicError = label "a spreadsheet with a divide by 0 error" $
  let spreadsheet = Spreadsheet [Def "divide" (Lambda ["a", "b"]
                                                (Builtin "/" 
                                                  [Id "a", Id "b"]))]
                                [ValCol "x" [VNum 1, VNum 2, VNum 3],
                                 ValCol "y" [VNum 1, VNum 0, VNum 0],
                                 ComputedCol "result" (Apply 
                                                        (Id "divide") 
                                                        [Id "x", Id "y"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "x" [VNum 1.0, VNum 2.0, VNum 3.0],
                ValCol "y" [VNum 1.0, VNum 0.0, VNum 0.0],
                ValCol "result" [VNum 1.0, Error, Error]]


prop_fullExampleSpreadsheet :: Property
prop_fullExampleSpreadsheet = label "example spreadsheet in the project handout" $
  let result = computeSpreadsheet exampleSpreadsheet
  in result == [ValCol "id" [VNum 1.0, VNum 2.0, VNum 3.0, VNum 4.0, VNum 5.0],
               ValCol "name" [VStr "adam", VStr "betty", VStr "clare", 
                              VStr "eric", VStr "sam"],
               ValCol "age" [VNum 12.0, VNum 15.0, VNum 18.0, 
                             VNum 49.0, VNum 17.0],
               ValCol "voter" [VBool False, VBool False, VBool True, 
                               VBool True, VBool False],
               ValCol "name2" [VStr "adamadam", VStr "bettybetty", 
                               VStr "clareclare", VStr "ericeric", VStr "samsam"],
               ValCol "year-until-100" [VNum 88.0, VNum 85.0, VNum 82.0, 
                                        VNum 51.0, VNum 83.0]]


prop_caculateAddWith1yz :: Float -> Float -> Float -> Property
prop_caculateAddWith1yz x1 y1 z1 =
  label "Lexical Scoping with multiple variables defined (Named Functions)" $
    let expr = (Apply (Id "add1with1") [(Id "y"), (Id "z")])
        env =
          ( Data.Map.insert
              "add1with1"
              ( VClosure
                  ["y", "z"]
                  ( Builtin
                      "+"
                      [ (Id "x"),
                        (Builtin "+" [(Id "y"), (Id "z")])
                      ]
                  )
                  (Data.Map.insert "x" (VNum 1) Data.Map.empty)
              )
              ( Data.Map.insert
                  "x"
                  (VNum x1)
                  ( Data.Map.insert
                      "y"
                      (VNum y1)
                      ( Data.Map.insert
                          "z"
                          (VNum z1)
                          (Data.Map.empty)
                      )
                  )
              )
          )
        result = evalDeer expr env
     in {-
          Haskell Testing Hack

          This rounds the return values of the expected and actual
          to avoid in rounding errors, ex 0.0001 != 0.0
         -}
        case (result) of
          (VNum returnVal) ->
            ( let x = 1
                  add1with1 y z = x + y + z
                  roundedRet = (round (add1with1 y1 z1))
                  rounded = round (returnVal)
               in rounded == roundedRet
            )
          --(Error) -> True

prop_ApplyInvalidFirst :: String -> Float -> Bool -> Property
prop_ApplyInvalidFirst val1 val2 val3 =
  label "First argument of Apply does not evaluate to a closure" $
    let result =
          ( Apply
              (Builtin "+" [Literal $ VNum val2, Literal $ VStr val1])
              [(Literal $ VBool val3)]
          )
     in evalDeer result Data.Map.empty == Error


prop_ApplyInvalidVariableValue :: Float -> String -> String -> Property
prop_ApplyInvalidVariableValue arg1 id idval =
  label "Invalid Id value type" $
    let result =
          (Builtin ">=" [Literal $ VNum arg1, (Id id)])
     in evalDeer
          result
          ( Data.Map.insert
              id
              (VStr idval)
              Data.Map.empty
          )
          == Error

prop_zeroDivision :: Float -> Float -> Property
prop_zeroDivision val1 val2 = 
  label "zero division" $
    let result =      
          (Builtin "/" [Literal $ VNum val1, Literal $ VNum 0.0])        
      in evalDeer result Data.Map.empty == Error

prop_NormalDivision :: Float -> Float -> Property
prop_NormalDivision val1 val2 = 
  label "normal division" $
    let result =      
          (Builtin "/" [Literal $ VNum val1, Literal $ VNum val2])        
      in evalDeer result Data.Map.empty == VNum (val1 / val2)

prop_NormalAddition :: Float -> Float -> Property
prop_NormalAddition val1 val2 = 
  label "normal addition" $
    let result =      
          (Builtin "+" [Literal $ VNum val1, Literal $ VNum val2])        
      in evalDeer result Data.Map.empty == VNum (val1 + val2)

prop_NormalSub :: Float -> Float -> Property
prop_NormalSub val1 val2 = 
  label "normal subtraction" $
    let result =      
          (Builtin "-" [Literal $ VNum val1, Literal $ VNum val2])        
      in evalDeer result Data.Map.empty == VNum (val1 - val2)

prop_NormalMul :: Float -> Float -> Property
prop_NormalMul val1 val2 = 
  label "normal multiplication" $
    let result =      
          (Builtin "*" [Literal $ VNum val1, Literal $ VNum val2])        
      in evalDeer result Data.Map.empty == VNum (val1 * val2)

prop_NormalMulfew :: Float -> Float -> Property
prop_NormalMulfew val1 val2 = 
  label "normal multiplication too few argument" $
    let result =      
          (Builtin "*" [Literal $ VNum val1])        
      in evalDeer result Data.Map.empty == Error

prop_NormalMulmany :: Float -> Float -> Property
prop_NormalMulmany val1 val2 = 
  label "normal multiplication too many argument" $
    let result =      
          (Builtin "*" [Literal $ VNum val1, Literal $ VNum val2, Literal $ VNum 9.0])        
      in evalDeer result Data.Map.empty == Error

prop_BuildinWrongInputType :: Float -> Float -> Property
prop_BuildinWrongInputType val1 val2 = 
  label "Builtin function wrong input type with error" $
    let result =      
          (Builtin "*" [Literal $ VNum val1, Literal $ VStr "Hello"])        
      in evalDeer result Data.Map.empty == Error

prop_BuildinWrongInputTypeNoError :: Float -> Float -> Property
prop_BuildinWrongInputTypeNoError val1 val2 = 
  label "Builtin function wrong input type without error" $
    let result =      
          (Lambda ["x"] (Builtin "*" [(Id "x"), Literal $ VStr "Hello"]))        
      in evalDeer result Data.Map.empty == VClosure ["x"] (Builtin "*" [(Id "x"), Literal $ VStr "Hello"]) Data.Map.empty

prop_apply :: Float -> Float -> Property
prop_apply val1 val2 = 
  label "apply function" $
    let result =      
          (Apply (Lambda ["x", "y"] (Builtin "+" [(Id "x"), (Id "y")])) [Literal $ VNum 3.0, Literal $ VNum 5.0])         
      in evalDeer result Data.Map.empty == VNum 8.0

prop_BuildinBool :: Bool -> Property
prop_BuildinBool val1 = 
  label "Builtin function boolean" $
    let result =      
          (Builtin "!" [Literal $ VBool val1])        
      in evalDeer result Data.Map.empty == VBool (not val1)
--prop_buildDataRow :: Float -> Float -> Float -> Property
--prop_buildDataRow x y z = 
--  label "test buid data row" $
--    let result = 
--          [ValCol "id" [VNum x, VNum y, VNum z], ValCol "csc" [VStr "kk", VStr "op", VStr "lp"]]
--      in buildDataEnvs result Data.Map.empty == [fromList [("id", VNum x), ("csc", VStr "kk")], fromList [("id", VNum y), ("csc", VStr "op")], fromList [("id", VNum z), ("csc", VStr "lp")]]
          
prop_testCSPostSpreadsheet :: [String] -> [Float] -> Property
prop_testCSPostSpreadsheet names cgpas =
  (length cgpas > 3 && length names > 3)
    ==> label
      "A spreadsheet indicating if the student made it to CS POSt"
    $ let spreadsheet =
            Spreadsheet
              [ Def "cutoff" (Literal $ VNum 3),
                Def
                  "admitted"
                  ( Lambda
                      ["x"]
                      (Builtin ">=" [Id "x", Id "cutoff"])
                  )
              ]
              [ ValCol "Student Names" [VStr (names !! 0), VStr (names !! 1), VStr (names !! 2)],
                ValCol "CGPA" [VNum (cgpas !! 0), VNum (cgpas !! 1), VNum (cgpas !! 2)],
                ComputedCol "admitted" (Apply (Id "admitted") [Id "CGPA"])
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "Student Names" [VStr (names !! 0), VStr (names !! 1), VStr (names !! 2)],
                 ValCol "CGPA" [VNum (cgpas !! 0), VNum (cgpas !! 1), VNum (cgpas !! 2)],
                 ValCol "admitted" [VBool ((cgpas !! 0) >= 3), VBool ((cgpas !! 1) >= 3), VBool ((cgpas !! 2) >= 3)]
               ]

prop_testMixedDefs :: Float -> [Float] -> [Float] -> Int -> Property
prop_testMixedDefs y val1 val2 n =
  (n >= 3 && ((length val1 >= n) && (length val2 >= n)))
    ==> label "A spreadsheet with functions and `constant` definitions "
    $ let spreadsheet =
            Spreadsheet
              [ Def "x" (Literal $ VNum (val1 !! 0)),
                Def "y" (Literal $ VNum y),
                Def "f" (Lambda ["x"] (Id "y")),
                Def "g" (Lambda ["y"] (Id "f"))
              ]
              [ ValCol "id" [VNum (val2 !! 1), VNum (val1 !! 1), VNum (val1 !! 2)],
                ComputedCol "fevaled" (Apply (Id "f") [Literal $ VNum (val1 !! 3)]),
                ComputedCol "gevaled" (Apply (Id "f") [Literal $ VNum (val1 !! 4)])
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "id" [VNum (val2 !! 1), VNum (val1 !! 1), VNum (val1 !! 2)],
                 ValCol "fevaled" [VNum y, VNum y, VNum y],
                 ValCol "gevaled" [VNum y, VNum y, VNum y]
               ]


prop_testNotAllFilled :: [Float] -> Property
prop_testNotAllFilled values =
  (length values >= 3)
    ==> label "A spreadsheet with not all the columns filled, ie not (mxn)"
    $ let spreadsheet =
            Spreadsheet
              []
              [ ValCol "d" [VNum (values !! 0), VNum (values !! 1)],
                ValCol "c" [VNum (values !! 2)]
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "d" [VNum (values !! 0), VNum (values !! 1)],
                 ValCol "c" [VNum (values !! 2)]
               ]


-- Empty function parameter
prop_testBasicemptyparam :: Property
prop_testBasicemptyparam = label "a spreadsheet-function call with empty param" $
  let spreadsheet = Spreadsheet [Def "a" (Literal $ VBool False),
                                Def "func1" (Lambda []
                                                (Builtin "!" 
                                                  [Id "a"]))]
                                [ValCol "x" [VNum 1, VBool False, VBool True, VStr "r"],
                                  ComputedCol "result" (Apply 
                                                        (Id "func1") 
                                                        [Id "x"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "x" [VNum 1, VBool False, VBool True, VStr "r"],
                ValCol "result" [Error, Error, Error, Error]]


-- wrong input type
prop_testBasicErrorwrnginput :: Property
prop_testBasicErrorwrnginput = label "a spreadsheet-function call with wrong input" $
  let spreadsheet = Spreadsheet [Def "divide" (Lambda ["a", "b"]
                                                (Builtin "/" 
                                                  [Id "a", Id "b"]))]
                                [ValCol "x" [VNum 1, VNum 2, VBool False],
                                ValCol "y" [VStr "1", VNum 2, VNum 3],
                                  ComputedCol "result" (Apply 
                                                        (Id "divide") 
                                                        [Id "x", Id "y"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "x" [VNum 1, VNum 2, VBool False],
                ValCol "y" [VStr "1", VNum 2, VNum 3],
                ValCol "result" [Error, VNum 1.0, Error]]


prop_testBasicError33 :: Property
prop_testBasicError33 = label "a spreadsheet-function call with too many arguments- error" $
  let spreadsheet = Spreadsheet [Def "add" (Lambda ["a", "b"]
                                                (Builtin "+" 
                                                  [Id "a", Id "b", Id "c", Id "d"]))]
                                [ValCol "x" [VNum 1, VNum 2, VNum 3],
                                 ValCol "y" [VNum 1, VNum 0, VNum 0],
                                 ValCol "z" [VNum 5, VNum 5, VNum 5],
                                 ComputedCol "result" (Apply 
                                                        (Id "add") 
                                                        [Id "x", Id "y", Id "z"])]
      result = computeSpreadsheet spreadsheet
  in result == [ValCol "x" [VNum 1.0, VNum 2.0, VNum 3.0],
                ValCol "y" [VNum 1.0, VNum 0.0, VNum 0.0],
                ValCol "z" [VNum 5.0, VNum 5.0, VNum 5.0],
                ValCol "result" [Error, Error, Error]]


prop_testComputedColsCombined :: Integer -> Property
prop_testComputedColsCombined arg =
  label "A spreadsheet that uses a ComputedCol in another one" $
    let argFloat = fromInteger arg :: Float
        spreadsheet =
          Spreadsheet
            [ Def
                "add1"
                ( Lambda
                    ["x"]
                    (Builtin "+" [Id "x", Literal $ VNum 1.0])
                )
            ]
            [ ValCol "id" [VNum (argFloat)],
              ComputedCol "add1'" (Apply (Id "add1") [Id "id"]),
              ComputedCol "add2" (Apply (Id "add1") [Id "add1'"])
            ]
        result = computeSpreadsheet spreadsheet
     in result
          == [ ValCol "id" [VNum (argFloat)],
               ValCol "add1'" [VNum (argFloat + 1)],
               ValCol "add2" [VNum (argFloat + 2)]
             ]
-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testValCols 
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testBuiltinIdentifier 
    quickCheck prop_testBasicError 
    quickCheck prop_fullExampleSpreadsheet 
    quickCheck prop_caculateAddWith1yz
    quickCheck prop_ApplyInvalidFirst
    quickCheck prop_ApplyInvalidVariableValue
    quickCheck prop_zeroDivision
    --quickCheck prop_NormalDivision
    quickCheck prop_NormalAddition
    quickCheck prop_NormalSub
    quickCheck prop_NormalMul
    quickCheck prop_NormalMulfew
    quickCheck prop_apply
    quickCheck prop_BuildinBool
    quickCheck prop_NormalMulmany
    quickCheck prop_BuildinWrongInputType
    quickCheck prop_BuildinWrongInputTypeNoError

    quickCheck prop_testCSPostSpreadsheet
    quickCheck prop_testMixedDefs
    quickCheck prop_testNotAllFilled
    quickCheck prop_testBasicemptyparam
    quickCheck prop_testBasicErrorwrnginput
    quickCheck prop_testBasicError33
    quickCheck prop_testComputedColsCombined
