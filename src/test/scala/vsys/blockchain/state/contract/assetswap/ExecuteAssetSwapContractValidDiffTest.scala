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
    // withdraw token A
    withdrawTokenA <- withdrawToken(
      master,
      registeredTokenAContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      master.toAddress.bytes.arr,
      10L, // withdraw amount of token A
      fee,
      ts + 7
    )
    // withdraw token B
    withdrawTokenB <- withdrawToken(
      user,
      registeredTokenBContract.contractId,
      registeredAssetSwapContract.contractId.bytes.arr,
      user.toAddress.bytes.arr,
      10L, // withdraw amount of token B
      fee,
      ts + 8
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, withdrawTokenA, withdrawTokenB)

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
      withdrawTokenB: ExecuteContractFunctionTransaction) =>
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

        newState.tokenAccountBalance(userTokenABalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenABalanceKey) shouldBe 990L

        newState.tokenAccountBalance(userTokenBBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 990L
      }
    }
  }

  val preconditionsAssetSwapContractAndSwap: Gen[(
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
        val masterTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val masterTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L - 10L // original balance - swap token A amount
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 20L // swap token B amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe 10L // swap token A amount
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 1000L - 20L // original balance - swap token B amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(20L), DataType.Amount).right.get) // token B amount
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
        val masterTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val masterTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L 
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenABalanceKey) shouldBe 0L
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 1000L
      }
    }
  }

  
  val preconditionsAssetSwapWithoutReceiverContractAndWithdrawToken: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
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
    // withdraw token A
    withdrawTokenA <- withdrawToken(
      master,
      registeredTokenAContract.contractId,
      registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr,
      master.toAddress.bytes.arr,
      10L, // withdraw amount of token A
      fee,
      ts + 7
    )
    // withdraw token B
    withdrawTokenB <- withdrawToken(
      user,
      registeredTokenBContract.contractId,
      registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr,
      user.toAddress.bytes.arr,
      10L, // withdraw amount of token B
      fee,
      ts + 8
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, withdrawTokenA, withdrawTokenB)

  property("asset swap without receiver able to withdraw token A and token B") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndWithdrawToken) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapWithoutReceiverContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      withdrawTokenA: ExecuteContractFunctionTransaction,
      withdrawTokenB: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapWithoutReceiverContract.timestamp, Seq(
                registeredAssetSwapWithoutReceiverContract,
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
        val assetSwapWithoutReceiverContractId = registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr
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

        newState.tokenAccountBalance(userTokenABalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenABalanceKey) shouldBe 990L

        newState.tokenAccountBalance(userTokenBBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 990L
      }
    }
  }

  val preconditionsAssetSwapWithoutReceiverContractAndSwap: Gen[(
    GenesisTransaction, GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
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
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapWithoutReceiverContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )

    finishSwapData = Seq(createSwap.id.arr)
    finishSwapType = Seq(DataType.ShortBytes)
    finishSwap <- finishSwapAssetSwapContractDataStackGen(
      user,
      registeredAssetSwapWithoutReceiverContract.contractId,
      finishSwapData,
      finishSwapType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, finishSwap)

  property("asset swap without receiver able to swap token A and token B") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndSwap) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapWithoutReceiverContract: RegisterContractTransaction,
      registeredTokenAContract: RegisterContractTransaction,
      registeredTokenBContract: RegisterContractTransaction,
      issueTokenA: ExecuteContractFunctionTransaction,
      issueTokenB: ExecuteContractFunctionTransaction,
      depositAToken: ExecuteContractFunctionTransaction,
      depositBToken: ExecuteContractFunctionTransaction,
      createSwap: ExecuteContractFunctionTransaction,
      finishSwap: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2)),
              TestBlock.create(registeredAssetSwapWithoutReceiverContract.timestamp, Seq(
                registeredAssetSwapWithoutReceiverContract,
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
        val assetSwapWithoutReceiverContractId = registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr
        
        // StateMap Keys
        val masterTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val masterTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        newState.contractNumInfo(masterTokenABalanceKey) shouldBe 1000L - 10L // original balance - swap token A amount
        newState.contractNumInfo(masterTokenBBalanceKey) shouldBe 20L // swap token B amount
        newState.contractNumInfo(userTokenABalanceKey) shouldBe 10L // swap token A amount
        newState.contractNumInfo(userTokenBBalanceKey) shouldBe 1000L - 20L // original balance - swap token B amount

        val contractStateMapKeys = getAssetSwapContractStateMapKeys(assetSwapWithoutReceiverContractId, createSwap.id.arr)

        newState.contractInfo(contractStateMapKeys(0)) shouldEqual Some(DataEntry.create(masterBytes, DataType.Address).right.get) // token A address
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(tokenATokenId, DataType.TokenId).right.get) // token A id
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // token A amount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // expiration time
        // newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(userBytes, DataType.Address).right.get) // token B address (Asset Swap without receiver contract does not have token B address)
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(tokenBTokenId, DataType.TokenId).right.get) // token B id
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(20L), DataType.Amount).right.get) // token B amount
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
    (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
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
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapWithoutReceiverContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )

    expireWithdrawData = Seq(createSwap.id.arr)
    expireWithdrawType = Seq(DataType.ShortBytes)
    expireWithdraw <- expireWithdrawAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapWithoutReceiverContract.contractId,
      expireWithdrawData,
      expireWithdrawType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, expireWithdraw)

  property("asset swap without receiver able to expire withdraw token A") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndExpireWithdraw) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapWithoutReceiverContract: RegisterContractTransaction,
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
              TestBlock.create(registeredAssetSwapWithoutReceiverContract.timestamp, Seq(
                registeredAssetSwapWithoutReceiverContract,
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
        val assetSwapWithoutReceiverContractId = registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr
        val tokenAContractId = registeredTokenAContract.contractId.bytes.arr
        val tokenATokenId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet().arr
        val tokenBContractId = registeredTokenBContract.contractId.bytes.arr
        val tokenBTokenId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet().arr

        // StateMap Keys
        val masterTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val masterTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                masterBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenABalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenATokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
        )

        val userTokenBBalanceKey = ByteStr(
          Bytes.concat(
            assetSwapWithoutReceiverContractId,
            Array(0.toByte), // state map index
            DataEntry.create(
              DataEntry(
                tokenBTokenId,
                DataType.TokenId
              ).data ++ DataEntry(
                userBytes,
                DataType.Address
              ).data,
              DataType.ShortBytes
            ).right.get.bytes
          )
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
    (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
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
    createSwapType = Seq(
      DataType.Address,
      DataType.TokenId,
      DataType.Amount,
      DataType.TokenId,
      DataType.Amount,
      DataType.Timestamp
    )
    createSwap <- createSwapAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapWithoutReceiverContract.contractId,
      createSwapData,
      createSwapType,
      attach,
      fee,
      ts
    )

    reduceTokenBAmountData = Seq(createSwap.id.arr, Longs.toByteArray(5L))
    reduceTokenBAmountType = Seq(DataType.ShortBytes, DataType.Amount)
    reduceTokenBAmount <- reduceTokenBAmountAssetSwapContractDataStackGen(
      master,
      registeredAssetSwapWithoutReceiverContract.contractId,
      reduceTokenBAmountData,
      reduceTokenBAmountType,
      attach,
      fee,
      ts
    )

  } yield (genesis, genesis2, master, user, registeredAssetSwapWithoutReceiverContract, registeredTokenAContract, registeredTokenBContract,
  issueTokenA, issueTokenB, depositAToken, depositBToken, createSwap, reduceTokenBAmount)

  property("asset swap without receiver able to reduce token B amount") {
    forAll(preconditionsAssetSwapWithoutReceiverContractAndReduceTokenBAmount) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount,
      registeredAssetSwapWithoutReceiverContract: RegisterContractTransaction,
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
              TestBlock.create(registeredAssetSwapWithoutReceiverContract.timestamp, Seq(
                registeredAssetSwapWithoutReceiverContract,
                registeredTokenAContract,
                registeredTokenBContract,
                issueTokenA, issueTokenB,
                depositAToken, depositBToken))),
          TestBlock.createWithTxStatus(
            reduceTokenBAmount.timestamp,
            Seq(createSwap, reduceTokenBAmount),
            TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val assetSwapContractId = registeredAssetSwapWithoutReceiverContract.contractId.bytes.arr
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