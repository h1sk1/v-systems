package vsys.blockchain.contract.crosschain

import org.scalacheck.Gen
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractCrossChain}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait CrossChainContractGen {

  val supersedeIndex: Short = 0
  val lockTokenIndex: Short = 1
  val unlockTokenIndex: Short = 2
  val updateWitnessIndex: Short = 3
  
  def crossChainSingleChainContractGen(): Gen[Contract] = ContractCrossChain.contractSingleChain

// TODO: Uncomment when multiChainContract is implemented
//   def crossChainMultiChainContractGen(): Gen[Contract] = ContractCrossChain.multiChainContract

  def genesisCrossChainContractGen(
    rep: PrivateKeyAccount,
    ts: Long): Gen[GenesisTransaction] =
      GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()
  
  def registerCrossChainContractGen(
    signer: PrivateKeyAccount,
    contract: Contract,
    dataStack: Seq[DataEntry],
    description: String,
    fee: Long,
    ts: Long): Gen[RegisterContractTransaction] =
      RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def initCrossChainContractSingleChainDataStackGen(
    witnessPublicKey: Array[Byte],
    chainId: Array[Byte]
  ): Gen[Seq[DataEntry]] = for {
    witnessPublicKey <- Gen.const(DataEntry.create(witnessPublicKey, DataType.ShortBytes).right.get)
    chainId <- Gen.const(DataEntry.create(chainId, DataType.ShortBytes).right.get)
  } yield Seq(witnessPublicKey, chainId)

  def supersedeCrossChainContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = supersedeIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def lockTokenCrossChainContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = lockTokenIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def unlockTokenCrossChainContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = unlockTokenIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def updateWitnessCrossChainContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = updateWitnessIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }
}