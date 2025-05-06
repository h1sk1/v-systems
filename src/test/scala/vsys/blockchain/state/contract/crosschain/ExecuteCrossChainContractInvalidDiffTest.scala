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

  val preconditionsCrossChainSingleChainContractWrongSignatureToUnlockTokenInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000 // deposit amount of token
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

    blockNumber = Longs.toByteArray(1000L) // block number
    burner = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes // burner address
    sidechainTokenAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    vsysTokenId = tokenId.arr
    vsysRecipientAddress = user.toAddress.bytes.arr
    amount = Longs.toByteArray(10L) // unlock amount of token
    txHash = "0xabcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz12".getBytes

    // unlock token
    unlockTokenData = Seq(
      blockNumber,
      burner,
      sidechainTokenAddress,
      vsysTokenId,
      vsysRecipientAddress,
      amount,
      txHash,
      chainId,
      "abcdefghyjklmnopqrstuvwxyz".getBytes // wrong signature
    )
    unlockTokenDataType = Seq(
      DataType.Amount,
      DataType.ShortBytes,
      DataType.ShortBytes,
      DataType.TokenId,
      DataType.Address,
      DataType.Amount,
      DataType.ShortBytes,
      DataType.ShortBytes,
      DataType.ShortBytes
    )
    unlockToken <- unlockTokenCrossChainContractDataStackGen(
      master,
      registeredCrossChainContract.contractId,
      unlockTokenData,
      unlockTokenDataType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken, unlockToken)

  property("unable to unlock token for cross chain single chain contract due to wrong signature") {
    forAll(preconditionsCrossChainSingleChainContractWrongSignatureToUnlockTokenInvalidDiffTest) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount, regulator: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction,
      unlockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2, genesis3)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken,
                lockToken))),
          TestBlock.createWithTxStatus(
            unlockToken.timestamp,
            Seq(unlockToken),
            TransactionStatus.ContractInvalidSignature)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSignature
      }
    }
  }

  val preconditionsCrossChainSingleChainContractWrongChainIdToUnlockTokenInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000 // deposit amount of token
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

    blockNumber = Longs.toByteArray(1000L) // block number
    burner = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes // burner address
    sidechainTokenAddress = "0xabcdefghijklmnopqrstuvwxyz0123456789abcd".getBytes
    vsysTokenId = tokenId.arr
    vsysRecipientAddress = user.toAddress.bytes.arr
    amount = Longs.toByteArray(10L) // unlock amount of token
    txHash = "0xabcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz12".getBytes
    wrongChainId = Longs.toByteArray(2L) // wrong chain id

    blockNumberDataEntry = DataEntry.create(blockNumber, DataType.Amount).explicitGet().data
    burnerDataEntry = DataEntry.create(burner, DataType.ShortBytes).explicitGet().data
    sidechainTokenAddressDataEntry = DataEntry.create(sidechainTokenAddress, DataType.ShortBytes).explicitGet().data
    tokenIdWithRecipientAddress = vsysTokenId ++ vsysRecipientAddress
    tokenIdWithRecipientAddressDataEntry = DataEntry.create(tokenIdWithRecipientAddress, DataType.ShortBytes).explicitGet().data
    amountDataEntry = DataEntry.create(amount, DataType.Amount).explicitGet().data
    txHashDataEntry = DataEntry.create(txHash, DataType.ShortBytes).explicitGet().data
    messageConcat1 = blockNumberDataEntry ++ burnerDataEntry
    messageConcat1DataEntry = DataEntry.create(messageConcat1, DataType.ShortBytes).explicitGet().data
    
    messageConcat2 = messageConcat1DataEntry ++ sidechainTokenAddressDataEntry
    messageConcat2DataEntry = DataEntry.create(messageConcat2, DataType.ShortBytes).explicitGet().data
    
    messageConcat3 = messageConcat2DataEntry ++ tokenIdWithRecipientAddressDataEntry
    messageConcat3DataEntry = DataEntry.create(messageConcat3, DataType.ShortBytes).explicitGet().data

    messageConcat4 = messageConcat3DataEntry ++ amountDataEntry
    messageConcat4DataEntry = DataEntry.create(messageConcat4, DataType.ShortBytes).explicitGet().data

    messageToSign = messageConcat4DataEntry ++ txHashDataEntry
    
    signature = EllipticCurveImpl.sign(privateKey, messageToSign)

    // unlock token
    unlockTokenData = Seq(
      blockNumber,
      burner,
      sidechainTokenAddress,
      vsysTokenId,
      vsysRecipientAddress,
      amount,
      txHash,
      wrongChainId,
      signature
    )
    unlockTokenDataType = Seq(
      DataType.Amount,
      DataType.ShortBytes,
      DataType.ShortBytes,
      DataType.TokenId,
      DataType.Address,
      DataType.Amount,
      DataType.ShortBytes,
      DataType.ShortBytes,
      DataType.ShortBytes
    )
    unlockToken <- unlockTokenCrossChainContractDataStackGen(
      master,
      registeredCrossChainContract.contractId,
      unlockTokenData,
      unlockTokenDataType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken, unlockToken)

  property("unable to unlock token for cross chain single chain contract due to wrong chain id") {
    forAll(preconditionsCrossChainSingleChainContractWrongChainIdToUnlockTokenInvalidDiffTest) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount, regulator: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction,
      unlockToken: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2, genesis3)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken,
                lockToken))),
          TestBlock.createWithTxStatus(
            unlockToken.timestamp,
            Seq(unlockToken),
            TransactionStatus.Failed)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsCrossChainSingleChainContractWrongRandomNumberToUpdateWitnessInvalidDiffTest: Gen[(
    GenesisTransaction, GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount,
    RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken,
    ts, fee, description, attach, privateKey, publicKey, chainId) <-
      createTokenAndInitCrossChainSingleChain(
        1000, // total supply of token
        1, // unity of token
        1000, // issue amount of token
        1000 // deposit amount of token
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
      ts + 1
    )

    seedBytes: Array[Byte] = Ints.toByteArray(2000)
    newPair = EllipticCurveImpl.createKeyPair(seedBytes)
    newPrivateKey = newPair._1
    newPublicKey = newPair._2


    newPublicKeyDataEntry = DataEntry.create(newPublicKey, DataType.PublicKey).explicitGet().data
    randomNumber = Longs.toByteArray(1000L) // random number
    randomNumberDataEntry = DataEntry.create(randomNumber, DataType.Amount).explicitGet().data
    messageToSign = newPublicKeyDataEntry ++ randomNumberDataEntry
    updateWitnesssignature = EllipticCurveImpl.sign(privateKey, messageToSign)

    updateWitnessData = Seq(
      newPublicKey,
      Longs.toByteArray(1001L), // random number
      updateWitnesssignature
    )
    updateWitnessDataType = Seq(
      DataType.PublicKey,
      DataType.Amount,
      DataType.ShortBytes
    )
    updateWitness <- updateWitnessCrossChainContractDataStackGen(
      regulator,
      registeredCrossChainContract.contractId,
      updateWitnessData,
      updateWitnessDataType,
      attach,
      fee,
      ts
    )
  } yield (genesis, genesis2, genesis3, master, user, regulator, registeredCrossChainContract, registeredTokenContract, issueToken, depositToken, lockToken, updateWitness)

  property("unable to update witness for cross chain single chain contract due to wrong random number") {
    forAll(preconditionsCrossChainSingleChainContractWrongRandomNumberToUpdateWitnessInvalidDiffTest) { case (
      genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
      master: PrivateKeyAccount, user: PrivateKeyAccount, regulator: PrivateKeyAccount,
      registeredCrossChainContract: RegisterContractTransaction,
      registeredTokenContract: RegisterContractTransaction,
      issueToken: ExecuteContractFunctionTransaction,
      depositToken: ExecuteContractFunctionTransaction,
      lockToken: ExecuteContractFunctionTransaction,
      updateWitness: ExecuteContractFunctionTransaction) =>
        assertDiffAndStateCorrectBlockTime(
          Seq(
            TestBlock.create(
              genesis.timestamp, Seq(genesis, genesis2, genesis3)),
              TestBlock.create(registeredCrossChainContract.timestamp, Seq(
                registeredCrossChainContract,
                registeredTokenContract,
                issueToken, depositToken,
                lockToken))),
          TestBlock.createWithTxStatus(
            updateWitness.timestamp,
            Seq(updateWitness),
            TransactionStatus.ContractInvalidSignature)) { (blockDiff, newState) =>
            blockDiff.txsDiff.contractDB.isEmpty shouldBe true
            blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
            blockDiff.txsDiff.portfolios.isEmpty shouldBe false
            blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSignature
      }
    }
  }
}