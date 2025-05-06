package vsys.blockchain.contract.crosschain

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.Contract
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.utils.crypto.EllipticCurveImpl

trait CrossChainFunctionHelperGen extends CrossChainContractGen with TokenContractGen {
  
  override val supersedeIndex: Short = 0

  override def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(addr)
  
  def registerToken(
    user: PrivateKeyAccount,
    totalSupply: Long,
    unity: Long,
    desc: String,
    fee: Long,
    timestamp: Long): Gen[RegisterContractTransaction] = for {
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, desc)
    description <- validDescStringGen
    tokenContract <- tokenContractGen(false)
    regTokenContract <- registerTokenGen(user, tokenContract, initTokenDataStack, description, fee, timestamp)
  } yield regTokenContract

  def issueToken(
    user: PrivateKeyAccount,
    contractId: ContractAccount,
    issueAmount: Long,
    fee: Long,
    timestamp: Long): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(user, contractId, issueAmount, attach, fee, timestamp)
  } yield issueToken

  def depositToken(
    user: PrivateKeyAccount,
    contractId: ContractAccount,
    sender: Array[Byte],
    contract: Array[Byte],
    amount: Long,
    fee: Long,
    timestamp: Long): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositTokenData = Seq(sender, contract, Longs.toByteArray(amount))
    depositTokenDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(user, contractId, false, depositTokenData, depositTokenDataType, attach, fee, timestamp)
  } yield depositToken

  def withdrawToken(
    user: PrivateKeyAccount,
    contractId: ContractAccount,
    contract: Array[Byte],
    sender: Array[Byte],
    amount: Long,
    fee: Long,
    timestamp: Long): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    withdrawTokenData = Seq(contract, sender, Longs.toByteArray(amount))
    withdrawTokenDataType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawToken <- withdrawTokenGen(user, contractId, false, withdrawTokenData, withdrawTokenDataType, attach, fee, timestamp)
  } yield withdrawToken

  val crossChainSingleChainContract: Gen[Contract] = crossChainSingleChainContractGen()
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  def createTokenAndInitCrossChainSingleChain(
    totalSupply: Long, unity: Long, issueAmount: Long, tokenDepositAmount: Long): Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long, Long, String, Array[Byte], Array[Byte], Array[Byte], Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisCrossChainContractGen(master, ts)
    user <- accountGen
    genesis2 <- genesisCrossChainContractGen(user, ts)
    regulator <- accountGen
    genesis3 <- genesisCrossChainContractGen(regulator, ts)
    tokenContractTemplate <- tokenContract
    crossChainContractTemplate <- crossChainSingleChainContract
    description <- validDescStringGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    // Register cross chain contract
    seedBytes: Array[Byte] = Ints.toByteArray(1000)
    pair = EllipticCurveImpl.createKeyPair(seedBytes)
    privateKey = pair._1
    publicKey = pair._2
    chainId = Longs.toByteArray(1L)
    initCorssChainDataStack: Seq[DataEntry] <- initCrossChainContractSingleChainDataStackGen(publicKey, chainId, regulator.toAddress)
    registeredCrossChainContract <- registerCrossChainContractGen(master, crossChainContractTemplate, initCorssChainDataStack, description, fee + 10000000000L, ts)
    crossChainContractId = registeredCrossChainContract.contractId
    // Register and deposit token
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    registeredTokenContract <- registerTokenGen(master, tokenContractTemplate, initTokenDataStack, description, fee + 10000000000L, ts + 1)
    tokenContractId = registeredTokenContract.contractId
    issueToken <- issueTokenGen(master, tokenContractId, issueAmount, attach, fee, ts + 2)
    depositToken <- depositToken(master, tokenContractId, master.toAddress.bytes.arr, crossChainContractId.bytes.arr, tokenDepositAmount, fee, ts + 3)
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
  ts, fee, description, attach, privateKey, publicKey, chainId)

  val crossChainSingleChainContractWithFreeze: Gen[Contract] = crossChainSingleChainContractWithFreezeGen()

  def createTokenAndInitCrossChainSingleChainWithFreeze(
    totalSupply: Long, unity: Long, issueAmount: Long, tokenDepositAmount: Long): Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long, Long, String, Array[Byte], Array[Byte], Array[Byte], Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisCrossChainContractGen(master, ts)
    user <- accountGen
    genesis2 <- genesisCrossChainContractGen(user, ts)
    regulator <- accountGen
    genesis3 <- genesisCrossChainContractGen(regulator, ts)
    tokenContractTemplate <- tokenContract
    crossChainContractTemplate <- crossChainSingleChainContractWithFreeze
    description <- validDescStringGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    // Register cross chain contract
    seedBytes: Array[Byte] = Ints.toByteArray(1000)
    pair = EllipticCurveImpl.createKeyPair(seedBytes)
    privateKey = pair._1
    publicKey = pair._2
    chainId = Longs.toByteArray(1L)
    initCorssChainDataStack: Seq[DataEntry] <- initCrossChainContractSingleChainDataStackGen(publicKey, chainId, regulator.toAddress)
    registeredCrossChainContract <- registerCrossChainContractGen(master, crossChainContractTemplate, initCorssChainDataStack, description, fee + 10000000000L, ts)
    crossChainContractId = registeredCrossChainContract.contractId
    // Register and deposit token
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    registeredTokenContract <- registerTokenGen(master, tokenContractTemplate, initTokenDataStack, description, fee + 10000000000L, ts + 1)
    tokenContractId = registeredTokenContract.contractId
    issueToken <- issueTokenGen(master, tokenContractId, issueAmount, attach, fee, ts + 2)
    depositToken <- depositToken(master, tokenContractId, master.toAddress.bytes.arr, crossChainContractId.bytes.arr, tokenDepositAmount, fee, ts + 3)
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
  ts, fee, description, attach, privateKey, publicKey, chainId)
}