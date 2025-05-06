package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractCrossChain {
  lazy val contractSingleChain: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(supersedeFunc, lockTokenFunc, unlockTokenFunc, updateWitnessFunc, balanceOfFunc),
    Seq(makerStateVar.arr, witnessPublicKeyStateVar.arr, chainIdStateVar.arr, regulatorStateVar.arr),
    Seq(tokenBalanceMap.arr, lockBalanceMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).explicitGet()

  lazy val contractSingleChainWithFreeze: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initSingleChainWithFreezeTrigger, depositTrigger, withdrawTrigger),
    Seq(supersedeFunc, lockTokenSingleChainWithFreezeFunc, unlockTokenSingleChainWithFreezeFunc, updateWitnessSingleChainWithFreezeFunc, balanceOfFunc, freezeFunc, unfreezeFunc),
    Seq(makerStateVar.arr, witnessPublicKeyStateVar.arr, chainIdStateVar.arr, regulatorStateVar.arr, isActiveStateVar.arr),
    Seq(tokenBalanceMap.arr, lockBalanceMap.arr),
    Seq(singleChainWithFreezeTriggerTextual, singleChainWithFreezeDescriptorTextual, stateVarTextual, stateMapTextual)
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "witnessPublicKey", "chainId", "regulator")
  val makerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val witnessPublicKeyStateVar: StateVar = StateVar(1.toByte, DataType.PublicKey.id.toByte)
  val chainIdStateVar: StateVar = StateVar(2.toByte, DataType.ShortBytes.id.toByte)
  val regulatorStateVar: StateVar = StateVar(3.toByte, DataType.Address.id.toByte)
  val isActiveStateVar: StateVar = StateVar(4.toByte, DataType.Boolean.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapTokenBalance = List("mapTokenBalance", "tokenIdWithAddressKey", "balance")
  val stateMapLockBalance = List("mapLockBalance", "tokenIdKey", "balance")
  val tokenBalanceMap: StateMap = StateMap(0.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val lockBalanceMap: StateMap = StateMap(1.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapTokenBalance, stateMapLockBalance))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq(
    "witnessPublicKey", // 0
    "chainId", // 1
    "regulator", // 2
    "singer" // 3
  )
  val initDataType: Array[Byte] = Array(DataType.PublicKey.id.toByte, DataType.ShortBytes.id.toByte, DataType.Account.id.toByte)
  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(3.toByte),
    cdbvSet ++ Array(makerStateVar.index, 3.toByte),
    cdbvSet ++ Array(witnessPublicKeyStateVar.index, 0.toByte),
    cdbvSet ++ Array(chainIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(regulatorStateVar.index, 2.toByte),
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  val initSingleChainWithFreezePara: Seq[String] = Seq(
    "witnessPublicKey", // 0
    "chainId", // 1
    "regulator", // 2
    "singer", // 3
    "valueTrue" // 4
  )
  val initSingleChainWithFreezeTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(3.toByte),
    cdbvSet ++ Array(makerStateVar.index, 3.toByte),
    cdbvSet ++ Array(witnessPublicKeyStateVar.index, 0.toByte),
    cdbvSet ++ Array(chainIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(regulatorStateVar.index, 2.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(4.toByte),
    cdbvSet ++ Array(isActiveStateVar.index, 4.toByte)
  )
  lazy val initSingleChainWithFreezeTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initSingleChainWithFreezeTriggerOpcs)
  val initSingleChainWithFreezeTextualBytes: Array[Byte] = textualFunc("init", Seq(), initSingleChainWithFreezePara)

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

  // WithDraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq(
    "withdrawer", // 0
    "amount", // 1
    "tokenId", // 2
    "tokenIdWithAddress" // 3
  )
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    basicConcat ++ Array(2.toByte, 0.toByte, 3.toByte),
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 3.toByte, 1.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Supersede Function
  val supersedeId: Short = 0
  val supersedePara: Seq[String] = Seq(
    "newOwner", // 0
    "newRegulator", // 1
    "maker" // 2
  )
  val supersedeDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte)
  val supersedeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(makerStateVar.index, 2.toByte),
    assertSigner ++ Array(2.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte),
    cdbvSet ++ Array(regulatorStateVar.index, 1.toByte)
  )
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeFunctionOpcs)
  val supersedeTextualBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Lock Token Function
  val lockTokenId: Short = 1
  val lockTokenPara: Seq[String] = Seq(
    "userAddress", // 0
    "amount", // 1
    "tokenId", // 2
    "destinationChainId", // 3
    "destinationAddress", // 4
    "originalChainId", // 5
    "tokenIdWithAddress" // 6
  )
  val lockTokenDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte, 
                                             DataType.ShortBytes.id.toByte, DataType.ShortBytes.id.toByte)
  val lockTokenFunctionOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(chainIdStateVar.index, 5.toByte),
    assertEqual ++ Array(5.toByte, 3.toByte),
    basicConcat ++ Array(2.toByte, 0.toByte, 6.toByte),
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 6.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(lockBalanceMap.index, 2.toByte, 1.toByte)
  )
  lazy val lockTokenFunc: Array[Byte] = getFunctionBytes(lockTokenId, publicFuncType, nonReturnType, lockTokenDataType, lockTokenFunctionOpcs)
  val lockTokenTextualBytes: Array[Byte] = textualFunc("lockToken", Seq(), lockTokenPara)

  val lockTokenSingleChainWithFreezePara: Seq[String] = Seq(
    "userAddress", // 0
    "amount", // 1
    "tokenId", // 2
    "destinationChainId", // 3
    "destinationAddress", // 4
    "isActive", // 5
    "originalChainId", // 6
    "tokenIdWithAddress" // 7
  )
  val lockTokenSingleChainWithFreezeFunctionOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(isActiveStateVar.index, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvrGet ++ Array(chainIdStateVar.index, 6.toByte),
    assertEqual ++ Array(6.toByte, 3.toByte),
    basicConcat ++ Array(2.toByte, 0.toByte, 7.toByte),
    cdbvMapValMinus ++ Array(tokenBalanceMap.index, 7.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(lockBalanceMap.index, 2.toByte, 1.toByte)
  )
  lazy val lockTokenSingleChainWithFreezeFunc: Array[Byte] = getFunctionBytes(lockTokenId, publicFuncType, nonReturnType, lockTokenDataType, lockTokenSingleChainWithFreezeFunctionOpcs)
  val lockTokenSingleChainWithFreezeTextualBytes: Array[Byte] = textualFunc("lockToken", Seq(), lockTokenSingleChainWithFreezePara)

  // Unlock Token Function
  val unlockTokenId: Short = 2
  val unlockTokenPara: Seq[String] = Seq(
    "blockNumber", // 0
    "burner", // 1
    "sidechainTokenAddress", // 2
    "vsysTokenId", // 3
    "vsysRecipientAddress", // 4
    "amount", // 5
    "txHash", // 6
    "chainId", // 7
    "signature", // 8
    "originalChainId", // 9
    "valueZero", // 10
    "blockNumberValid", // 11
    "tokenIdWithRecipientAddress", // 12
    "msgConcat", // 13
    "witnessPublicKey", // 14
  )
  val unlockTokenDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.ShortBytes.id.toByte, DataType.ShortBytes.id.toByte,
                                               DataType.TokenId.id.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte,
                                               DataType.ShortBytes.id.toByte, DataType.ShortBytes.id.toByte, DataType.ShortBytes.id.toByte)
  val unlockTokenFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(chainIdStateVar.index, 9.toByte),
    assertEqual ++ Array(7.toByte, 9.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(0), DataType.Amount).bytes ++ Array(10.toByte),
    compareGreater ++ Array(0.toByte, 10.toByte, 11.toByte),
    assertTrue ++ Array(11.toByte),
    basicConcat ++ Array(3.toByte, 4.toByte, 12.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 13.toByte),
    basicConcat ++ Array(13.toByte, 2.toByte, 13.toByte),
    basicConcat ++ Array(13.toByte, 12.toByte, 13.toByte),
    basicConcat ++ Array(13.toByte, 5.toByte, 13.toByte),
    basicConcat ++ Array(13.toByte, 6.toByte, 13.toByte),
    cdbvrGet ++ Array(witnessPublicKeyStateVar.index, 14.toByte),
    assertSig ++ Array(13.toByte, 8.toByte, 14.toByte),
    cdbvMapValMinus ++ Array(lockBalanceMap.index, 3.toByte, 5.toByte),
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 12.toByte, 5.toByte)
  )
  lazy val unlockTokenFunc: Array[Byte] = getFunctionBytes(unlockTokenId, publicFuncType, nonReturnType, unlockTokenDataType, unlockTokenFunctionOpcs)
  val unlockTokenTextualBytes: Array[Byte] = textualFunc("unlockToken", Seq(), unlockTokenPara)

  val unlockTokenSingleChainWithFreezePara: Seq[String] = Seq(
    "blockNumber", // 0
    "burner", // 1
    "sidechainTokenAddress", // 2
    "vsysTokenId", // 3
    "vsysRecipientAddress", // 4
    "amount", // 5
    "txHash", // 6
    "chainId", // 7
    "signature", // 8
    "isActive", // 9
    "originalChainId", // 10
    "valueZero", // 11
    "blockNumberValid", // 12
    "tokenIdWithRecipientAddress", // 13
    "msgConcat", // 14
    "witnessPublicKey", // 15
  )
  val unlockTokenSingleChainWithFreezeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(isActiveStateVar.index, 9.toByte),
    assertTrue ++ Array(9.toByte),
    cdbvrGet ++ Array(chainIdStateVar.index, 10.toByte),
    assertEqual ++ Array(7.toByte, 10.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(0), DataType.Amount).bytes ++ Array(11.toByte),
    compareGreater ++ Array(0.toByte, 11.toByte, 12.toByte),
    assertTrue ++ Array(12.toByte),
    basicConcat ++ Array(3.toByte, 4.toByte, 13.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 14.toByte),
    basicConcat ++ Array(14.toByte, 2.toByte, 14.toByte),
    basicConcat ++ Array(14.toByte, 13.toByte, 14.toByte),
    basicConcat ++ Array(14.toByte, 5.toByte, 14.toByte),
    basicConcat ++ Array(14.toByte, 6.toByte, 14.toByte),
    cdbvrGet ++ Array(witnessPublicKeyStateVar.index, 15.toByte),
    assertSig ++ Array(14.toByte, 8.toByte, 15.toByte),
    cdbvMapValMinus ++ Array(lockBalanceMap.index, 3.toByte, 5.toByte),
    cdbvMapValAdd ++ Array(tokenBalanceMap.index, 13.toByte, 5.toByte)
  )
  lazy val unlockTokenSingleChainWithFreezeFunc: Array[Byte] = getFunctionBytes(unlockTokenId, publicFuncType, nonReturnType, unlockTokenDataType, unlockTokenSingleChainWithFreezeFunctionOpcs)
  val unlockTokenSingleChainWithFreezeTextualBytes: Array[Byte] = textualFunc("unlockToken", Seq(), unlockTokenSingleChainWithFreezePara)

  // Update Witness Function
  val updateWitnessId: Short = 3
  val updateWitnessPara: Seq[String] = Seq(
    "newWitnessPublicKey", // 0
    "randomNumber", // 1
    "signature", // 2
    "regulator", // 3
    "msgConcat", // 4
    "oldWitnessPublicKey" // 5
  )
  val updateWitnessDataType: Array[Byte] = Array(DataType.PublicKey.id.toByte, DataType.Amount.id.toByte, DataType.ShortBytes.id.toByte)
  val updateWitnessFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(regulatorStateVar.index, 3.toByte),
    assertSigner ++ Array(3.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 4.toByte),
    cdbvrGet ++ Array(witnessPublicKeyStateVar.index, 5.toByte),
    assertSig ++ Array(4.toByte, 2.toByte, 5.toByte),
    cdbvSet ++ Array(witnessPublicKeyStateVar.index, 0.toByte)
  )
  lazy val updateWitnessFunc: Array[Byte] = getFunctionBytes(updateWitnessId, publicFuncType, nonReturnType, updateWitnessDataType, updateWitnessFunctionOpcs)
  val updateWitnessTextualBytes: Array[Byte] = textualFunc("updateWitness", Seq(), updateWitnessPara)

  val updateWitnessSingleChainWithFreezePara: Seq[String] = Seq(
    "newWitnessPublicKey", // 0
    "randomNumber", // 1
    "signature", // 2
    "isActive", // 3
    "regulator", // 4
    "msgConcat", // 5
    "oldWitnessPublicKey" // 6
  )
  val updateWitnessSingleChainWithFreezeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(isActiveStateVar.index, 3.toByte),
    assertTrue ++ Array(3.toByte),
    cdbvrGet ++ Array(regulatorStateVar.index, 4.toByte),
    assertSigner ++ Array(4.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 5.toByte),
    cdbvrGet ++ Array(witnessPublicKeyStateVar.index, 6.toByte),
    assertSig ++ Array(5.toByte, 2.toByte, 6.toByte),
    cdbvSet ++ Array(witnessPublicKeyStateVar.index, 0.toByte)
  )
  lazy val updateWitnessSingleChainWithFreezeFunc: Array[Byte] = getFunctionBytes(updateWitnessId, publicFuncType, nonReturnType, updateWitnessDataType, updateWitnessSingleChainWithFreezeFunctionOpcs)
  val updateWitnessSingleChainWithFreezeTextualBytes: Array[Byte] = textualFunc("updateWitness", Seq(), updateWitnessSingleChainWithFreezePara)

  // Balance Of Function
  val balanceOfId: Short = 4
  val balanceOfPara: Seq[String] = Seq("account", "tokenId",
                                      "tokenIdWithAddress", "balance")
  val balanceOfDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.TokenId.id.toByte)
  val balanceOfReturnType: Array[Byte] = Array(DataType.Amount.id.toByte)
  val balanceOfFunctionOpcs: Seq[Array[Byte]] = Seq(
    basicConcat ++ Array(1.toByte, 0.toByte, 2.toByte),
    cdbvrMapGetOrDefault ++ Array(tokenBalanceMap.index, 2.toByte, 3.toByte),
    returnValue ++ Array(3.toByte)
  )
  lazy val balanceOfFunc: Array[Byte] = getFunctionBytes(balanceOfId, publicFuncType, balanceOfReturnType, balanceOfDataType, balanceOfFunctionOpcs)
  val balanceOfTextualBytes: Array[Byte] = textualFunc("balanceOf", Seq("balance"), balanceOfPara)

  // Freeze Function
  val freezeId: Short = 5
  val freezePara: Seq[String] = Seq(
    "maker", // 0
    "valueFalse" // 1
  )
  val freezeDataType: Array[Byte] = Array()
  val freezeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(makerStateVar.index, 0.toByte),
    assertSigner ++ Array(0.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(1.toByte),
    cdbvSet ++ Array(isActiveStateVar.index, 1.toByte)
  )
  lazy val freezeFunc: Array[Byte] = getFunctionBytes(freezeId, publicFuncType, nonReturnType, freezeDataType, freezeFunctionOpcs)
  val freezeTextualBytes: Array[Byte] = textualFunc("freeze", Seq(), freezePara)

  // Unfreeze Function
  val unfreezeId: Short = 6
  val unfreezePara: Seq[String] = Seq(
    "maker", // 0
    "valueTrue" // 1
  )
  val unfreezeDataType: Array[Byte] = Array()
  val unfreezeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(makerStateVar.index, 0.toByte),
    assertSigner ++ Array(0.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(1.toByte),
    cdbvSet ++ Array(isActiveStateVar.index, 1.toByte)
  )
  lazy val unfreezeFunc: Array[Byte] = getFunctionBytes(unfreezeId, publicFuncType, nonReturnType, unfreezeDataType, unfreezeFunctionOpcs)
  val unfreezeTextualBytes: Array[Byte] = textualFunc("unfreeze", Seq(), unfreezePara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeTextualBytes, lockTokenTextualBytes, unlockTokenTextualBytes, 
                                                                      updateWitnessTextualBytes, balanceOfTextualBytes))

  lazy val singleChainWithFreezeTriggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initSingleChainWithFreezeTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val singleChainWithFreezeDescriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeTextualBytes, lockTokenSingleChainWithFreezeTextualBytes, unlockTokenSingleChainWithFreezeTextualBytes,
                                                                      updateWitnessSingleChainWithFreezeTextualBytes, balanceOfTextualBytes, freezeTextualBytes, unfreezeTextualBytes))                                                
}