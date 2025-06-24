package vsys.blockchain.state.contract.assetswap

import com.google.common.primitives.{Bytes, Ints, Longs}
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


class ExecuteAssetSwapContractValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with AssetSwapContractGen
  with AssetSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAssetSwapContractAndWithdrawToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
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

    // Random withdraw token amount of token A
    withdrawTokenAAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    // withdraw token A
    withdrawTokenA <- withdrawToken(
      master,
      registeredTokenAContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      master.toAddress.bytes.arr,
      withdrawTokenAAmount, // withdraw amount of token A
      fee,
      ts + 7
    )
    // withdraw token B
    withdrawTokenB <- withdrawToken(
      user,
      registeredTokenBContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      user.toAddress.bytes.arr,
      1000L, // withdraw amount of token B
      fee,
      ts + 8
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, withdrawTokenA, withdrawTokenB, withdrawTokenAAmount)

  property("asset swap able to withdraw token A and token B") {
    forAll(preconditionsAssetSwapContractAndWithdrawToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      withdrawTokenA: ExecuteContractFunctionTransaction,
      withdrawTokenB: ExecuteContractFunctionTransaction,
      withdrawTokenAAmount: Long) =>
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
            withdrawTokenB.timestamp,
            Seq(withdrawTokenA, withdrawTokenB),
            TransactionStatus.Success)) { (blockDiff, newState) =>
          
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = withdrawTokenA.proofs.firstCurveProof.explicitGet().publicKey
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr

        val (userTokenABalanceKey, userTokenBBalanceKey) = getUserTokenBalanceKeys(
          tokenAContractId,
          tokenBContractId,
          master,
          user
        )
        val (contractTokenABalanceKey, contractTokenBBalanceKey) = getContractTokenBalanceKeys(
          tokenAContractId,
          tokenBContractId,
          assetSwapContractId,
          master,
          user
        )

        newState.tokenAccountBalance(userTokenABalanceKey) shouldBe withdrawTokenAAmount
        newState.tokenAccountBalance(contractTokenABalanceKey) shouldBe (1000L - withdrawTokenAAmount)

        newState.tokenAccountBalance(userTokenBBalanceKey) shouldBe 1000L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 0L
      }
    }
  }

  val preconditionsAssetSwapContractAndSwap: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
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
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for token A
      user.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token B
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
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap able to swap token A and token B") {
    forAll(preconditionsAssetSwapContractAndSwap) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
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
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            userBytes
          )
        
        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L - tokenASwapAmount // original balance - swap token A amount
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 1000L // swap token B amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe tokenASwapAmount // swap token A amount
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 0L // original balance - swap token B amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapContractAndSwapSameToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract,
    issueTokenA, depositAToken, sendToken, depositBToken, ts, fee, description, attach) <-
      createSameTokenAndInitAssetSwap(
        2000, // total supply of token A
        1, // unity of token A
        2000, // issue amount of token A
        1000, // send token A amount to user
        1000, // deposit amount for master
        1000 // deposit amount for user
      )

    tokenAContractId = registeredTokenAContract.contractId.bytes.arr
    tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()

    // create swap
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for master
      user.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(1000L), // swap amount for user
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
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract,
  issueTokenA, depositAToken, sendToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap able to swap same token") {
    forAll(preconditionsAssetSwapContractAndSwapSameToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      sendToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract, registeredTokenAContract,
                issueTokenA, depositAToken, sendToken, depositBToken))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenATokenId,
            masterBytes,
            userBytes
          )
        
        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 2000L - tokenASwapAmount // original balance - swap token A amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe tokenASwapAmount // swap token A amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapContractAndSwapWithSameAccount: Gen[(
    GenesisTransaction, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
    (genesis, master, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenWithSameAccountAndInitAssetSwap(
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
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for token A
      master.toAddress.bytes.arr,
      tokenBTokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token B
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
    
    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )
  } yield (genesis, master, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap able to swap token A and token B with same account") {
    forAll(preconditionsAssetSwapContractAndSwapWithSameAccount) { case (
      genesis: GenesisTransaction, master: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            masterBytes
          )
        
        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L // original balance, swap back to master
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 1000L // original balance, swap back to master

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token B address
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapContractAndExpireWithdraw: Gen[(
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

  property("asset swap able to expire withdraw token A") {
    forAll(preconditionsAssetSwapContractAndExpireWithdraw) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
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
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            expireWithdraw.timestamp + 110,
            Seq(createSwap, expireWithdraw),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr

        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            userBytes
          )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L 
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenABalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 1000L
      }
    }
  }

  // Asset Swap Without Receiver Contract
  
  val preconditionsAssetSwapWithoutReceiverContractAndWithdrawToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
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

    // Random withdraw token amount of token A
    withdrawTokenAAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    // withdraw token A
    withdrawTokenA <- withdrawToken(
      master,
      registeredTokenAContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      master.toAddress.bytes.arr,
      withdrawTokenAAmount, // withdraw amount of token A
      fee,
      ts + 7
    )
    // withdraw token B
    withdrawTokenB <- withdrawToken(
      user,
      registeredTokenBContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      user.toAddress.bytes.arr,
      1000L, // withdraw amount of token B
      fee,
      ts + 8
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, withdrawTokenA, withdrawTokenB, withdrawTokenAAmount)

  property("asset swap without receiver able to withdraw token A and token B") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndWithdrawToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      withdrawTokenA: ExecuteContractFunctionTransaction,
      withdrawTokenB: ExecuteContractFunctionTransaction,
      withdrawTokenAAmount: Long) =>
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
            withdrawTokenB.timestamp,
            Seq(withdrawTokenA, withdrawTokenB),
            TransactionStatus.Success)) { (blockDiff, newState) =>
          
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = withdrawTokenA.proofs.firstCurveProof.explicitGet().publicKey
        val assetSwapWithoutReceiverContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr

        val (userTokenABalanceKey, userTokenBBalanceKey) = getUserTokenBalanceKeys(
          tokenAContractId,
          tokenBContractId,
          master,
          user
        )
        val (contractTokenABalanceKey, contractTokenBBalanceKey) = getContractTokenBalanceKeys(
          tokenAContractId,
          tokenBContractId,
          assetSwapWithoutReceiverContractId,
          master,
          user
        )

        newState.tokenAccountBalance(userTokenABalanceKey) shouldBe withdrawTokenAAmount
        newState.tokenAccountBalance(contractTokenABalanceKey) shouldBe (1000L - withdrawTokenAAmount)

        newState.tokenAccountBalance(userTokenBBalanceKey) shouldBe 1000L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 0L
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndSwap: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
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
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token B
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
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap without receiver able to swap token A and token B") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndSwap) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
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
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapWithoutReceiverContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
       val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapWithoutReceiverContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            userBytes
          )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L - tokenASwapAmount // original balance - swap token A amount
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 1000L // swap token B amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe tokenASwapAmount // swap token A amount
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 0L // original balance - swap token B amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapWithoutReceiverContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        // newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address (Asset Swap without receiver contract does not have token B address)
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndSwapSameToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract,
    issueTokenA, depositAToken, sendToken, depositBToken, ts, fee, description, attach) <-
      createSameTokenAndInitAssetSwapWithoutReceiver(
        2000, // total supply of token A
        1, // unity of token A
        2000, // issue amount of token A
        1000, // send token A amount to user
        1000, // deposit amount for master
        1000 // deposit amount for user
      )

    tokenAContractId = registeredTokenAContract.contractId.bytes.arr
    tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()

    // create swap
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for master
      tokenATokenId.arr,
      Longs.toByteArray(1000L), // swap amount for user
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
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract,
  issueTokenA, depositAToken, sendToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap without receiver able to swap same token") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndSwapSameToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      sendToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract, registeredTokenAContract,
                issueTokenA, depositAToken, sendToken, depositBToken))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenATokenId,
            masterBytes,
            userBytes
          )
        
        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 2000L - tokenASwapAmount // original balance - swap token A amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe tokenASwapAmount // swap token A amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        // newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address  (Asset Swap without receiver contract does not have token B address)
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndSwapWithSameAccount: Gen[(
    GenesisTransaction, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
    (genesis, master, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
    issueTokenA, issueTokenB, depositAToken, depositBToken, ts, fee, description, attach) <-
      createABTokenWithSameAccountAndInitAssetSwapWithoutReceiver(
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
    // Random swap token amount of token A
    tokenASwapAmount = Math.abs(scala.util.Random.nextLong() % 1000) + 1
    createSwapData = Seq(
      master.toAddress.bytes.arr,
      tokenATokenId.arr,
      Longs.toByteArray(tokenASwapAmount), // swap amount for token A
      tokenBTokenId.arr,
      Longs.toByteArray(1000L), // swap amount for token B
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
    
    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )
  } yield (genesis, master, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap, tokenASwapAmount)

  property("asset swap without receiver able to swap token A and token B with same account") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndSwapWithSameAccount) { case (
      genesis: GenesisTransaction, master: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction,
      tokenASwapAmount: Long) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis)),
              TestBlock.create(registeredAssetSwapContract.timestamp, Seq(
                registeredAssetSwapContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            finishSwap.timestamp,
            Seq(createSwap, finishSwap),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            masterBytes
          )
        
        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L // original balance, swap back to master
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 1000L // original balance, swap back to master

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(tokenASwapAmount), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        // newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token B address
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // token B amount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // swap status
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndExpireWithdraw: Gen[(
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

  property("asset swap without receiver able to expire withdraw token A") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndExpireWithdraw) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
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
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            expireWithdraw.timestamp + 110,
            Seq(createSwap, expireWithdraw),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val assetSwapWithoutReceiverContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr

        // StateMap Keys
        val (masterTokenABalanceKey, masterTokenBBalanceKey, userTokenABalanceKey, userTokenBBalanceKey) = getTokenBalanceStateMapKeys(
            assetSwapWithoutReceiverContractId,
            tokenATokenId,
            tokenBTokenId,
            masterBytes,
            userBytes
          )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenABalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 1000L
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndReduceTokenBAmount: Gen[(
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

    reduceTokenBAmountData = Seq(createSwap.id.arr, Longs.toByteArray(5L))
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

  property("asset swap without receiver able to reduce token B amount") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndReduceTokenBAmount) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
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
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            reduceTokenBAmount.timestamp,
            Seq(createSwap, reduceTokenBAmount),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val assetSwapContractId = registeredAssetSwapContract.contractId.bytes.arr
        val tokenBAmountKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(7.toByte), // state map index (tokenBAmountMap)
            DataEntry.create(
              createSwap.id.arr,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        newState.contractInfo(tokenBAmountKey) shouldEqual Some(DataEntry.create(Longs.toByteArray(5L), DataType.Amount).right.get) // reduced token B amount
      }
    }
  }
}