{-# LANGUAGE OverloadedStrings #-}
module Language.Solidity.Test.AbiSpec where


import Test.Hspec
import Data.Either (isLeft)
import Language.Solidity.Abi


spec :: Spec
spec = do
  describe "parseSolidityType" $
    describe "tuple type" $ do
        it "can parses a FunctionArg with tuple type" $ do
          let maa = FunctionArg  "makerAssetAmount" "uint256" Nothing
              ma = FunctionArg "makeAddress" "address" Nothing
              tupleFA = FunctionArg "order" "tuple" (Just [maa, ma])
              eRes = parseSolidityFunctionArgType tupleFA
          eRes `shouldBe` Right (SolidityTuple 2 [SolidityUint 256, SolidityAddress])
        it "fails to parse a FunctionArg with invalid tuple" $ do
          let tupleFA = FunctionArg "order" "tuple" Nothing
              eRes = parseSolidityFunctionArgType tupleFA
          isLeft eRes `shouldBe` True
  describe "signature" $ do
    it "can generate signature for fillOrder" $ do
      let fillOrderDec = buildFillOrderDec
          expected = "fillOrder((address,address,address,address,uint256,uint256,uint256,uint256,uint256,uint256,bytes,bytes),uint256,bytes)"
          sig = signature fillOrderDec
      sig `shouldBe` expected
    it "can generate signature for arrays of tuples" $ do
      let dec = DFunction "testArraysOfTuples" SMPure [arrayOfTuple] Nothing
          arrayOfTuple = FunctionArg "arrayOfTuple" "tuple[]" $ Just [FunctionArg "tuple" "uint64" Nothing]
      signature dec `shouldBe` "testArraysOfTuples((uint64)[])"
{-
    it "can generate signature for vector of tuples" $ do
      let dec = DFunction "testVectorsOfTuples" SMPure [vectorOfTuple] Nothing
          vectorOfTuple = FunctionArg "vectorOfTuple" "uint256[1][3]" $ Nothing --Just [FunctionArg "tuple" "uint64" Nothing]
      signature dec `shouldBe` "testVectorsOfTuples(string[1][3])"
-}
  describe "methodId" $
    it "can generate methodId for fillOrder" $ do
      let fillOrderDec = buildFillOrderDec
          expected = "0xb4be83d5"
          mId = methodId fillOrderDec
      mId `shouldBe` expected

buildFillOrderDec :: Declaration
buildFillOrderDec = DFunction "fillOrder" SMNonPayable funInputs' funOutputs'
  where
    funInputs' =
      [ makeTupleFuncArg ("order", "tuple") tupleComponents
      , makeBasicFuncArg ("takerAssetFillAmount", "uint256")
      , makeBasicFuncArg ("signature", "bytes")
      ]
    tupleComponents =
      [ ("makerAddress", "address")
      , ("takerAddress", "address")
      , ("feeRecipientAddress", "address")
      , ("senderAddress", "address")
      , ("makerAssetAmount", "uint256")
      , ("takerAssetAmount", "uint256")
      , ("makerFee", "uint256")
      , ("takerFee", "uint256")
      , ("expirationTimeSeconds", "uint256")
      , ("salt", "uint256")
      , ("makerAssetData",   "bytes")
      , ("takerAssetData",   "bytes")
      ]
    funOutputs' = Nothing
    makeBasicFuncArg (n,t) =
      FunctionArg n t Nothing
    makeTupleFuncArg (n,t) cmps =
      FunctionArg n t (Just $ map makeBasicFuncArg cmps)
