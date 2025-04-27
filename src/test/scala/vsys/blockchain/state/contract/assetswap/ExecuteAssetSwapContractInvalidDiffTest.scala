package vsys.blockchain.state.contract.assetswap

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.assetswap.{AssetSwapContractGen, AssetSwapFunctionHelperGen}
import vsys.blockchain.contract._
import vsys.blockchain.state.diffs._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}

class ExecuteAssetSwapContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with AssetSwapContractGen
  with AssetSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  
  val preconditionsAssetSwapInsufficientDepositAmountToCreateSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwap(
        1000, // total supply of token A
        1, // unity of token A
        1000, // issue amount of token A
        1000, // total supply of token B
        1, // unity of token B
        1000, // issue amount of token B
        1000, // deposit amount of token A
        1000 // deposit amount of token B
      )

    tokenAContractId = registeredTokenAContract.contractId.bytes.arr
    tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    tokenBContractId = registeredTokenBContract.contractId.bytes.arr
    tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()

    // create swap with insufficient deposit amount for user to create swap
    // user deposit amount is 1000, but swap amount is 1001
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(1001L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user,registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap)

  property("unable to create swap due to insufficient deposit amount") {
    forAll(preconditionsAssetSwapInsufficientDepositAmountToCreateSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            createSwap.timestamp,
            Seq(createSwap),
            TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAssetSwapInsufficientDepositAmountToFinishSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwap(
        1000, // total supply of token A
        1, // unity of token A
        1000, // issue amount of token A
        1000, // total supply of token B
        1, // unity of token B
        1000, // issue amount of token B
        1000, // deposit amount of token A
        1000 // deposit amount of token B
      )

    tokenAContractId = registeredTokenAContract.contractId.bytes.arr
    tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    tokenBContractId = registeredTokenBContract.contractId.bytes.arr
    tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()

    // create swap
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(1001L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )

    // finish swap with wrong tx id
    finishSwapData = Seq(
      createSwap.id.arr, // wrong tx id
    )
    finishSwapType = Seq(
      DataType.ShortBytes
    )
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap)

  property("unable to finish swap due to insufficient deposit amount") {
    forAll(preconditionsAssetSwapInsufficientDepositAmountToFinishSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken,
                createSwap))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(finishSwap),
            TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAssetSwapWrongTxIdToFinishSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwap(
        1000, // total supply of token A
        1, // unity of token A
        1000, // issue amount of token A
        1000, // total supply of token B
        1, // unity of token B
        1000, // issue amount of token B
        1000, // deposit amount of token A
        1000 // deposit amount of token B
      )

    tokenAContractId = registeredTokenAContract.contractId.bytes.arr
    tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    tokenBContractId = registeredTokenBContract.contractId.bytes.arr
    tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()

    // create swap
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )

    // finish swap with wrong tx id
    finishSwapData = Seq(
      genesis.id.arr, // wrong tx id
    )
    finishSwapType = Seq(
      DataType.ShortBytes
    )
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap)

  property("unable to finish swap due to wrong tx id") {
    forAll(preconditionsAssetSwapWrongTxIdToFinishSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken,
                createSwap))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(finishSwap),
            TransactionStatus.ContractStateMapNotDefined)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
    }
  }
}