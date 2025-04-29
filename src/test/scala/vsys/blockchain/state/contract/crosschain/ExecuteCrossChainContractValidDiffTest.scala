package vsys.blockchain.state.contract.crosschain

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract._
import vsys.blockchain.contract.crosschain.{CrossChainContractGen, CrossChainFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.utils.crypto.EllipticCurveImpl


class ExecuteCrossChainContractValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with CrossChainContractGen
  with CrossChainFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsCrossChainSingleChainContractAndWithdrawToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000, // deposit amount of token
      )
    // withdraw token
    withdrawToken <- withdrawToken(
      master,
      registeredTokenContract.contractId,
      registeredCrossChainContract.contractId.bytes.arr,
      master.toAddress.bytes.arr,
      10L, // withdraw amount of token
      fee,
      ts + 7
    )
  } yield (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, withdrawToken)

  property("cross chain single chain able to withdraw token") {
    forAll(preconditionsCrossChainSingleChainContractAndWithdrawToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      withdrawToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken))),
          TestBlock.createWithTxStatus(
            withdrawToken.timestamp,
            Seq(withdrawToken),
            TransactionStatus.Success)) { (blockDiff, newState) =>
          
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val crossChainContractId = registeredCrossChainContract.contractId.bytes.arr
        val tokenContractId = registeredTokenContract.contractId.bytes.arr
        val tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

        val masterTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, masterBytes))
        val contractTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, crossChainContractId))

        newState.tokenAccountBalance(masterTokenBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenBalanceKey) shouldBe 990L
      }
    }
  }

  val preconditionsCrossChainSingleChainContractAndLockToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000, // deposit amount of token
      )

    tokenContractId = registeredTokenContract.contractId.bytes.arr
    tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

    // A example of etherium address
    destinationAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    // lock token
    lockTokenData = Seq(
      master.toAddress.bytes.arr,
      Longs.toByteArray(10L), // lock amount of token
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
  } yield (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken)

  property("cross chain single chain able to lock token") {
    forAll(preconditionsCrossChainSingleChainContractAndLockToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken))),
          TestBlock.createWithTxStatus(
            lockToken.timestamp,
            Seq(lockToken),
            TransactionStatus.Success)) { (blockDiff, newState) =>
          
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val crossChainContractId = registeredCrossChainContract.contractId.bytes.arr
        val tokenContractId = registeredTokenContract.contractId.bytes.arr
        val tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet().arr

        val masterTokenBalanceKey = ByteStr(
          Bytes.concat(
            crossChainContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val contractLockTokenBalanceKey = ByteStr(
          Bytes.concat(
            crossChainContractId,
            Array(1.toByte), // state map index
            DataEntry.create(
              tokenId,
              DataType.TokenId
            ).right.get.bytes
          )
        )

        newState.contractNumInfo(masterTokenBalanceKey) shouldBe 1000L - 10L // original balance - lock token amount
        newState.contractNumInfo(contractLockTokenBalanceKey) shouldBe 10L // lock token amount
      }
    }
  }

  val preconditionsCrossChainSingleChainContractAndSupersedeAndLock: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000, // deposit amount of token
      )

    tokenContractId = registeredTokenContract.contractId.bytes.arr
    tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

    supersede <- supersedeCrossChainContractDataStackGen(
      master,
      registeredCrossChainContract.contractId,
      user.toAddress,
      attach,
      fee,
      ts
    )

    // A example of etherium address
    destinationAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    // lock token
    lockTokenData = Seq(
      master.toAddress.bytes.arr,
      Longs.toByteArray(10L), // lock amount of token
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
  } yield (genesis, genesis2, master, user, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, supersede, lockToken)

  property("cross chain single chain able to lock token after supersede") {
    forAll(preconditionsCrossChainSingleChainContractAndSupersedeAndLock) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      supersede: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken))),
          TestBlock.createWithTxStatus(
            lockToken.timestamp,
            Seq(supersede, lockToken),
            TransactionStatus.Success)) { (blockDiff, newState) =>
          
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val crossChainContractId = registeredCrossChainContract.contractId.bytes.arr
        val tokenContractId = registeredTokenContract.contractId.bytes.arr
        val tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet().arr

        val masterTokenBalanceKey = ByteStr(
          Bytes.concat(
            crossChainContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val contractLockTokenBalanceKey = ByteStr(
          Bytes.concat(
            crossChainContractId,
            Array(1.toByte), // state map index
            DataEntry.create(
              tokenId,
              DataType.TokenId
            ).right.get.bytes
          )
        )

        newState.contractNumInfo(masterTokenBalanceKey) shouldBe 1000L - 10L // original balance - lock token amount
        newState.contractNumInfo(contractLockTokenBalanceKey) shouldBe 10L // lock token amount
      }
    }
  }
}