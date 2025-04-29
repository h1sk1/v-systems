package vsys.blockchain.state.contract.crosschain

import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.crosschain.{CrossChainContractGen, CrossChainFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}


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

        val master = withdrawToken.proofs.firstCurveProof.explicitGet().publicKey
        val crossChainContractId = registeredCrossChainContract.contractId.bytes.arr
        val tokenContractId = registeredTokenContract.contractId.bytes.arr
        val tokenId = tokenIdFromBytes(tokenContractId, Ints.toByteArray(0)).explicitGet()

        val masterTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, crossChainContractId))

        newState.tokenAccountBalance(masterTokenBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenBalanceKey) shouldBe 990L
      }
    }
  }
}