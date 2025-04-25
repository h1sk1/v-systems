package vsys.blockchain.state.contract.assetswap

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.assetswap.{AssetSwapContractGen, AssetSwapFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}


class ExecuteAssetSwapContractValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with AssetSwapContractGen
  with AssetSwapFunctionHelperGen {
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
}