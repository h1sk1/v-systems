package vsys.blockchain.contract.crosschain

import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
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
    chainId: Array[Byte],
    regulator: Address
  ): Gen[Seq[DataEntry]] = for {
    witnessPublicKey <- Gen.const(DataEntry.create(witnessPublicKey, DataType.PublicKey).right.get)
    chainId <- Gen.const(DataEntry.create(chainId, DataType.ShortBytes).right.get)
    regulator <- Gen.const(DataEntry.create(regulator.bytes.arr, DataType.Address).right.get)
  } yield Seq(witnessPublicKey, chainId, regulator)

  def supersedeDataStackGen(address1: Address, address2: Address): Gen[Seq[DataEntry]] = for {
    addr1 <- Gen.const(DataEntry(address1.bytes.arr, DataType.Address))
    addr2 <- Gen.const(DataEntry(address2.bytes.arr, DataType.Address))
  } yield Seq(addr1, addr2)

  def supersedeCrossChainContractDataStackGen(signer: PrivateKeyAccount, contractId: ContractAccount, newOwner: Address, newRegulator: Address,
                              attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- supersedeDataStackGen(newOwner, newRegulator)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, supersedeIndex, data, attachment, fee, feeScale, ts).explicitGet()

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