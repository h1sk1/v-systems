package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractAssetSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(createSwapFunc, finishSwapFunc, expireWithdrawFunc),
    Seq(),
    Seq(tokenBalanceMap.arr, tokenAAddressMap.arr, tokenAIdMap.arr, tokenAAmountMap.arr, swapExpiredTimeMap.arr,
        tokenBAddressMap.arr, tokenBIdMap.arr, tokenBAmountMap.arr, swapStatusMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).explicitGet()

  // StateVar
  val stateVarName = List()
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapTokenBalance = List("mapTokenBalance", "tokenIdWithAddressKey", "balanceValue")
  val stateMapSwapCreator = List("mapTokenAAdress", "swapId", "tokenAAdressValue")
  val stateMapSwapTokenId = List("mapTokenAId", "swapId", "tokenAIdValue")
  val stateMapSwapAmount = List("mapTokenAAmount", "swapId", "tokenAAmountValue")
  val stateMapSwapExpiredTime = List("mapSwapExpiredTime", "swapId", "swapExpiredTimeValue")
  val stateMapSwapTargetAddress = List("mapTokenBAddress", "swapId", "tokenBAddressValue")
  val stateMapSwapTargetTokenId = List("mapTokenBId", "swapId", "tokenBIdValue")
  val stateMapSwapTargetAmount = List("mapTokenBAmount", "swapId", "tokenBAmountValue")
  val stateMapSwapStatus = List("mapSwapStatus", "swapId", "swapStatusValue")
  
  val tokenBalanceMap: StateMap = StateMap(0.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val tokenAAddressMap: StateMap = StateMap(1.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val tokenAIdMap: StateMap = StateMap(2.toByte, DataType.ShortBytes.id.toByte, DataType.TokenId.id.toByte)
  val tokenAAmountMap: StateMap = StateMap(3.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val swapExpiredTimeMap: StateMap = StateMap(4.toByte, DataType.ShortBytes.id.toByte, DataType.Timestamp.id.toByte)
  val tokenBAddressMap: StateMap = StateMap(5.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val tokenBIdMap: StateMap = StateMap(6.toByte, DataType.ShortBytes.id.toByte, DataType.TokenId.id.toByte)
  val tokenBAmountMap: StateMap = StateMap(7.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val swapStatusMap: StateMap = StateMap(8.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapTokenBalance, stateMapSwapCreator, 
    stateMapSwapTokenId, stateMapSwapAmount, stateMapSwapExpiredTime, stateMapSwapTargetAddress,
    stateMapSwapTargetTokenId, stateMapSwapTargetAmount, stateMapSwapStatus))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq()
  val initDataType: Array[Byte] = Array()
  val initOpcs: Seq[Array[Byte]] = Seq()
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq(
    "depositor", // 0
    "amount", // 1
    "tokenId", // 2
    "tokenIdWithAddress" // 3
  )
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    basicConcat ++ Array(2.toByte, 0.toByte, 3.toByte),
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 3.toByte, 1.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // Withdraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId", "tokenIdWithAddress")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    basicConcat ++ Array(2.toByte, 0.toByte, 3.toByte),
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 3.toByte, 1.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Create Swap Function
  val createSwapId: Short = 0
  val createSwapPara: Seq[String] = Seq(
    "tokenAAddress", // 0
    "tokenAId", // 1
    "tokenAAmount", // 2
    "tokenBAddress", // 3
    "tokenBId", // 4
    "tokenBAmount", // 5
    "expiredTime", // 6
    "tokenIdWithAddress", // 7
    "txId", // 8
    "valueTrue" // 9
  )
  val createSwapDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte,
    DataType.Address.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte, DataType.Timestamp.id.toByte)
  val createSwapFunctionOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    basicConcat ++ Array(1.toByte, 0.toByte, 7.toByte),
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 7.toByte, 2.toByte),
    loadTransactionId ++ Array(8.toByte),
    cdbvMapSet ++ Array(tokenAAddressMap.index, 8.toByte, 0.toByte),
    cdbvMapSet ++ Array(tokenAIdMap.index, 8.toByte, 1.toByte),
    cdbvMapSet ++ Array(tokenAAmountMap.index, 8.toByte, 2.toByte),
    cdbvMapSet ++ Array(swapExpiredTimeMap.index, 8.toByte, 6.toByte),
    cdbvMapSet ++ Array(tokenBAddressMap.index, 8.toByte, 3.toByte),
    cdbvMapSet ++ Array(tokenBIdMap.index, 8.toByte, 4.toByte),
    cdbvMapSet ++ Array(tokenBAmountMap.index, 8.toByte, 5.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(9.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 8.toByte, 9.toByte)
  )
  lazy val createSwapFunc: Array[Byte] = getFunctionBytes(createSwapId, publicFuncType, nonReturnType, createSwapDataType, createSwapFunctionOpcs)
  val createSwapTextualBytes: Array[Byte] = textualFunc("createSwap", Seq(), createSwapPara)

  // Finish Swap Function
  val finishSwapId: Short = 1
  val finishSwapPara: Seq[String] = Seq(
    "txId", // 0
    "tokenBAddress", // 1
    "swapStatus", // 2
    "currentTime", // 3
    "swapExpiredTime", // 4
    "compareResult", // 5
    "tokenBId", // 6
    "tokenBAmount", // 7
    "destroyTokenBIdWithAddressB", // 8
    "tokenAAdress", // 9
    "tokenAId", // 10
    "tokenAAmount", // 11
    "receiveTokenBIdWithAddressA", // 12
    "receiveTokenAIdWithAddressB", // 13
    "valueFalse" // 14
  )
  
  val finishSwapDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)
  val finishSwapFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(tokenBAddressMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(swapStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    loadTimestamp ++ Array(3.toByte),
    cdbvrMapGet ++ Array(swapExpiredTimeMap.index, 0.toByte, 4.toByte),
    compareGreater ++ Array(4.toByte, 3.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvrMapGet ++ Array(tokenBIdMap.index, 0.toByte, 6.toByte), // token B id
    cdbvrMapGet ++ Array(tokenBAmountMap.index, 0.toByte, 7.toByte), // token B amount
    basicConcat ++ Array(6.toByte, 1.toByte, 8.toByte), // destroyTokenBIdWithAddressB
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 8.toByte, 7.toByte),
    cdbvrMapGet ++ Array(tokenAAddressMap.index, 0.toByte, 9.toByte), // token A address
    cdbvrMapGet ++ Array(tokenAIdMap.index, 0.toByte, 10.toByte), // token A id
    cdbvrMapGet ++ Array(tokenAAmountMap.index, 0.toByte, 11.toByte), // token A amount
    basicConcat ++ Array(6.toByte, 9.toByte, 12.toByte), // receiveTokenBIdWithAddressA
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 12.toByte, 7.toByte),
    basicConcat ++ Array(10.toByte, 1.toByte, 13.toByte), // receiveTokenAIdWithAddressB
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 13.toByte, 11.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(14.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 0.toByte, 14.toByte)
  )
  lazy val finishSwapFunc: Array[Byte] = getFunctionBytes(finishSwapId, publicFuncType, nonReturnType, finishSwapDataType, finishSwapFunctionOpcs)
  val finishSwapTextualBytes: Array[Byte] = textualFunc("finishSwap", Seq(), finishSwapPara)

  // Expire Withdraw Function
  val expireWithdrawId: Short = 2
  val expireWithdrawPara: Seq[String] = Seq(
    "txId", // 0
    "tokenAAddress", // 1
    "swapStatus", // 2
    "currentTime", // 3
    "swapExpiredTime", // 4
    "compareResult", // 5
    "tokenAId", // 6
    "tokenAAmount", // 7
    "tokenIdWithAddress", // 8
    "valueFalse" // 9
  )
  val expireWithdrawDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)
  val expireWithdrawFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(tokenAAddressMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(swapStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    loadTimestamp ++ Array(3.toByte),
    cdbvrMapGet ++ Array(swapExpiredTimeMap.index, 0.toByte, 4.toByte),
    compareGreater ++ Array(3.toByte, 4.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvrMapGet ++ Array(tokenAIdMap.index, 0.toByte, 6.toByte),
    cdbvrMapGet ++ Array(tokenAAmountMap.index, 0.toByte, 7.toByte),
    basicConcat ++ Array(6.toByte, 1.toByte, 8.toByte),
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 8.toByte, 7.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(9.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 0.toByte, 9.toByte)
  )
  lazy val expireWithdrawFunc: Array[Byte] = getFunctionBytes(expireWithdrawId, publicFuncType, nonReturnType, expireWithdrawDataType, expireWithdrawFunctionOpcs)
  val expireWithdrawTextualBytes: Array[Byte] = textualFunc("expireWithdraw", Seq(), expireWithdrawPara)

  // Generate Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(createSwapTextualBytes, finishSwapTextualBytes, expireWithdrawTextualBytes))
}