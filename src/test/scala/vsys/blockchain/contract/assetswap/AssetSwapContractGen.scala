package vsys.blockchain.contract.assetswap

import org.scalacheck.Gen
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractAssetSwap}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait AssetSwapContractGen {

  val createSwapIndex: Short = 0
  val finishSwapIndex: Short = 1
  val expireWithdrawIndex: Short = 2
  
  def assetSwapContractGen(): Gen[Contract] = ContractAssetSwap.contract

  def genesisAssetSwapContractGen(
    rep: PrivateKeyAccount,
    ts: Long): Gen[GenesisTransaction] =
      GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()
  
  def registerAssetSwapContractGen(
    signer: PrivateKeyAccount,
    contract: Contract,
    description: String,
    fee: Long,
    ts: Long): Gen[RegisterContractTransaction] =
      RegisterContractTransaction.create(signer, contract, Seq(), description, fee, feeScale, ts).explicitGet()

  def initAssetSwapContractDataStackGen(): Gen[Seq[DataEntry]] = Seq()

  def createSwapAssetSwapContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = createSwapIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def finishSwapAssetSwapContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = finishSwapIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def expireWithdrawAssetSwapContractDataStackGen(
    signer: PrivateKeyAccount,
    contractId: ContractAccount,
    data: Seq[Array[Byte]],
    dataType: Seq[DataType.DataTypeVal[_]],
    attachment: Array[Byte],
    fee: Long,
    ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = expireWithdrawIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }
}