package vsys.blockchain.contract.assetswap


import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{ContractAccount, PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.contract.Contract
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait AssetSwapFunctionHelperGen extends AssetSwapContractGen with TokenContractGen {
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

  val assetSwapContract: Gen[Contract] = assetSwapContractGen()
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  def createABTokenAndInitAssetSwap(
    totalSupplyA: Long, unityA: Long, issueAmountA: Long,
    totalSupplyB: Long, unityB: Long, issueAmountB: Long,
    tokenADepositAmount: Long, tokenBDepositAmount: Long): Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long, Long, String, Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisAssetSwapContractGen(master, ts)
    user <- accountGen
    genesis2 <- genesisAssetSwapContractGen(user, ts)
    tokenContractTemplate <- tokenContract
    assetSwapContractTemplate <- assetSwapContract
    description <- validDescStringGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    // Register asset swap contract
    registeredAssetSwapContract <- registerAssetSwapContractGen(master, assetSwapContractTemplate, description, fee + 10000000000L, ts)
    assetSwapContractId = registeredAssetSwapContract.contractId
    // Register and deposit first token
    initTokenADataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyA, unityA, "init")
    registeredTokenAContract <- registerTokenGen(master, tokenContractTemplate, initTokenADataStack, description, fee + 10000000000L, ts + 1)
    tokenAContractId = registeredTokenAContract.contractId
    issueTokenA <- issueTokenGen(master, tokenAContractId, issueAmountA, attach, fee, ts + 2)
    depositAToken <- depositToken(master, tokenAContractId, master.toAddress.bytes.arr, assetSwapContractId.bytes.arr, tokenADepositAmount, fee, ts + 3)
    // Register and deposit second token
    initTokenBDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyB, unityB, "init")
    registeredTokenBContract <- registerTokenGen(user, tokenContractTemplate, initTokenBDataStack, description, fee + 10000000000L, ts + 4)
    tokenBContractId = registeredTokenBContract.contractId
    issueTokenB <- issueTokenGen(user, tokenBContractId, issueAmountB, attach, fee, ts + 5)
    depositBToken <- depositToken(user, tokenBContractId, user.toAddress.bytes.arr, assetSwapContractId.bytes.arr, tokenBDepositAmount, fee, ts + 6)
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
      issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach)

  
  def getContractTokenBalanceKeys(tokenAContractId: Array[Byte], tokenBContractId: Array[Byte], assetSwapContractId: Array[Byte], master: PublicKeyAccount, user: PublicKeyAccount): (ByteStr, ByteStr) = {
    val tokenAId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    val tokenBId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()

    val contractTokenABalanceKey = ByteStr(Bytes.concat(tokenAId.arr, assetSwapContractId))
    val contractTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, assetSwapContractId))

    (contractTokenABalanceKey, contractTokenBBalanceKey)
  }

  def getUserTokenBalanceKeys(tokenAContractId: Array[Byte], tokenBContractId: Array[Byte], master: PublicKeyAccount, user: PublicKeyAccount): (ByteStr, ByteStr) = {
    val tokenAId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    val tokenBId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()

    val masterTokenABalanceKey = ByteStr(Bytes.concat(tokenAId.arr, master.toAddress.bytes.arr))
    val userTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, user.toAddress.bytes.arr))

    (masterTokenABalanceKey, userTokenBBalanceKey)
  }
}