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
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user,registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap)

  property("unable to create swap due to insufficient Token A deposit amount") {
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
      Longs.toByteArray(10L), // swap amount for token A
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
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
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

  property("unable to finish swap due to insufficient Token B deposit amount") {
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
      Longs.toByteArray(10L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
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

  val preconditionsAssetSwapAlreadyFinishedSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
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

    // create swap
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    // finish swap
    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )

    // finish swap again
    finishSwapAgain <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts + 1
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, finishSwapAgain)

  property("unable to finish swap again due to already finished swap") {
    forAll(preconditionsAssetSwapAlreadyFinishedSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction, finishSwapAgain: ExecuteContractFunctionTransaction) =>
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
                createSwap, finishSwap))),
          TestBlock.createWithTxStatus(
            finishSwapAgain.timestamp,
            Seq(finishSwapAgain),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAssetSwapAlreadyExpiredSwapInvalidDiffTest: Gen[(
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

    // create swap with expiration time in the past
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time in the past
    )
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
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

  property("unable to finish swap due to already expired swap") {
    forAll(preconditionsAssetSwapAlreadyExpiredSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction, registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction, issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction, depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction, finishSwap: ExecuteContractFunctionTransaction) =>
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
            finishSwap.timestamp + 110,
            Seq(finishSwap),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAssetSwapNotYetExpiredWithdrawInvalidDiffTest: Gen[(
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

    // create swap with expiration time in the future
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time in the future
    )
    createSwap <- assetSwapCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    expireWithdrawData = Seq(createSwap.id.arr)
    expireWithdrawType = Seq(DataType.ShortBytes)
    expireWithdraw <- expireWithdrawAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      expireWithdrawData,
      expireWithdrawType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, expireWithdraw)

  property("unable to expire withdraw due to not yet expired") {
    forAll(preconditionsAssetSwapNotYetExpiredWithdrawInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction, registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction, issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction, depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction, expireWithdraw: ExecuteContractFunctionTransaction) =>
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
            expireWithdraw.timestamp + 50,
            Seq(expireWithdraw),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // Asset Swap Without Receiver Contract
  val preconditionsAssetSwapWithoutReceiverInsufficientDepositAmountToCreateSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

  } yield (genesis, genesis2, master, user,registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap)

  property("unable to create swap for without receiver contract due to insufficient Token A deposit amount") {
    forAll(preconditionsAssetSwapWithoutReceiverInsufficientDepositAmountToCreateSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
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

  val preconditionsAssetSwapWithoutReceiverInsufficientDepositAmountToFinishSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(1001L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    // finish swap
    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
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

  property("unable to finish swap for without receiver contract due to insufficient Token B deposit amount") {
    forAll(preconditionsAssetSwapWithoutReceiverInsufficientDepositAmountToFinishSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
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

  val preconditionsAssetSwapWithoutReceiverWrongTxIdToFinishSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
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

  property("unable to finish swap for without receiver contract due to wrong tx id") {
    forAll(preconditionsAssetSwapWithoutReceiverWrongTxIdToFinishSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
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

  val preconditionsAssetSwapWithoutReceiverAlreadyFinishedSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    // finish swap
    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )

    // finish swap again
    finishSwapAgain <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts + 1
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, finishSwapAgain)

  property("unable to finish swap again for without receiver contract due to already finished swap") {
    forAll(preconditionsAssetSwapWithoutReceiverAlreadyFinishedSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction, finishSwapAgain: ExecuteContractFunctionTransaction) =>
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
                createSwap, finishSwap))),
          TestBlock.createWithTxStatus(
            finishSwapAgain.timestamp,
            Seq(finishSwapAgain),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverAlreadyExpiredSwapInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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

    // create swap with expiration time in the past
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time in the past
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
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

  property("unable to finish swap for without receiver contract due to already expired swap") {
    forAll(preconditionsAssetSwapWithoutReceiverAlreadyExpiredSwapInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
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
            finishSwap.timestamp + 110,
            Seq(finishSwap),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverNotYetExpiredWithdrawInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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

    // create swap with expiration time in the future
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time in the future
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    expireWithdrawData = Seq(createSwap.id.arr)
    expireWithdrawType = Seq(DataType.ShortBytes)
    expireWithdraw <- expireWithdrawAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      expireWithdrawData,
      expireWithdrawType,
      attach,
      fee,
      ts
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, expireWithdraw)

  property("unable to expire withdraw for without receiver contract due to not yet expired") {
    forAll(preconditionsAssetSwapWithoutReceiverNotYetExpiredWithdrawInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      expireWithdraw: ExecuteContractFunctionTransaction) =>
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
            expireWithdraw.timestamp + 50,
            Seq(expireWithdraw),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverRaiseTokenBAmountInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenAndInitAssetSwapWithoutReceiver(
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

    // create swap with expiration time in the future
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(10L), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(20L), // swap amount for token B
      Longs.toByteArray(ts + 100) // expiration time in the future
    )
    createSwap <- assetSwapWithoutReceiverCreateSwap(
      master,
      registeredAssetSwapContract.contractId,
      createSwapData,
      attach,
      fee,
      ts
    )

    // call reduce token B amount function, but raise token B amount
    reduceTokenBAmountData = Seq(createSwap.id.arr, Longs.toByteArray(30L))
    reduceTokenBAmountType = Seq(DataType.ShortBytes, DataType.Amount)
    reduceTokenBAmount <- reduceTokenBAmountAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      reduceTokenBAmountData,
      reduceTokenBAmountType,
      attach,
      fee,
      ts
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, reduceTokenBAmount)

  property("unable to reduce token B amount for without receiver contract due to raise token B amount") {
    forAll(preconditionsAssetSwapWithoutReceiverRaiseTokenBAmountInvalidDiffTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction, registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction, depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction, createSwap: ExecuteContractFunctionTransaction,
      reduceTokenBAmount: ExecuteContractFunctionTransaction) =>
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
            reduceTokenBAmount.timestamp,
            Seq(reduceTokenBAmount),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}