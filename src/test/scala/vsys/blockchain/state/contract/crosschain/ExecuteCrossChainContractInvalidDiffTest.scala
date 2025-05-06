package vsys.blockchain.state.contract.crosschain

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.crosschain.{CrossChainContractGen, CrossChainFunctionHelperGen}
import vsys.blockchain.contract._
import vsys.blockchain.state.diffs._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.utils.crypto.EllipticCurveImpl

class ExecuteCrossChainContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with CrossChainContractGen
  with CrossChainFunctionHelperGen {
  
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsCrossChainSingleChainContractInsufficientDepositAmountToLockTokenInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000, // deposit amount of token
      )

    tokenContractId = registeredTokenContract.contractId.bytes.arr
    tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

    // A example of ethereum address
    destinationAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    // lock token
    lockTokenData = Seq(
      master.toAddress.bytes.arr,
      Longs.toByteArray(1001L), // lock amount of token
      tokenId.arr,
      chainId,
      destinationAddress
    )
    lockTokenDataType = Seq(
      DataType.Address,
      DataType.Amount,
      DataType.TokenId,
      DataType.ShortBytes,
      DataType.ShortBytes
    )
    lockToken <- lockTokenCrossChainContractDataStackGen(
      master,
      registeredCrossChainContract.contractId,
      lockTokenData,
      lockTokenDataType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken)

  property("unable to lock token for cross chain single chain contract due to insufficient token deposit amount") {
    forAll(preconditionsCrossChainSingleChainContractInsufficientDepositAmountToLockTokenInvalidDiffTest) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount, regulator: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2, genesis3)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken))),
          TestBlock.createWithTxStatus(
            lockToken.timestamp,
            Seq(lockToken),
            TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsCrossChainSingleChainContractWrongChainIdToLockTokenInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000, // deposit amount of token
      )

    tokenContractId = registeredTokenContract.contractId.bytes.arr
    tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

    // A example of ethereum address
    destinationAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    // lock token
    lockTokenData = Seq(
      master.toAddress.bytes.arr,
      Longs.toByteArray(10L), // lock amount of token
      tokenId.arr,
      Longs.toByteArray(2L), // wrong chain id
      destinationAddress
    )
    lockTokenDataType = Seq(
      DataType.Address,
      DataType.Amount,
      DataType.TokenId,
      DataType.ShortBytes,
      DataType.ShortBytes
    )
    lockToken <- lockTokenCrossChainContractDataStackGen(
      master,
      registeredCrossChainContract.contractId,
      lockTokenData,
      lockTokenDataType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken)

  property("unable to lock token for cross chain single chain contract due to wrong chain id") {
    forAll(preconditionsCrossChainSingleChainContractWrongChainIdToLockTokenInvalidDiffTest) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount, regulator: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2, genesis3)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken))),
          TestBlock.createWithTxStatus(
            lockToken.timestamp,
            Seq(lockToken),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}