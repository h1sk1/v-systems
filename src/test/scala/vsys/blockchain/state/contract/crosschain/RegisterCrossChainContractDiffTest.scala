package vsys.blockchain.state.contract.crosschain

import cats.Monoid
import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.DataEntry
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractCrossChain}
import vsys.blockchain.contract.crosschain.CrossChainContractGen
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.utils.crypto.EllipticCurveImpl

class RegisterCrossChainContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with CrossChainContractGen {

  private implicit def noShrink[T]: Shrink[T] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 2

  val preconditionAndBuildCrossChainContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]],
    Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractCrossChain.contractSingleChain.trigger)
    descriptor <- Gen.const(ContractCrossChain.contractSingleChain.descriptor)
    stateVar <- Gen.const(ContractCrossChain.contractSingleChain.stateVar)
    stateMap <- Gen.const(ContractCrossChain.contractSingleChain.stateMap)
    textual <- Gen.const(ContractCrossChain.contractSingleChain.textual)
  } yield (langCode, langVer, init, descriptor, stateVar, stateMap, textual)

  property("register cross-chain single chain contract build doesn't break invariant"){
    forAll(preconditionAndBuildCrossChainContract) { case (langCode, langVer, init, descriptor, stateVar, stateMap, textual) =>
    Contract.buildContract(langCode, langVer, init, descriptor, stateVar, stateMap, textual) shouldBe an[Right[_, _]]
    }
  }
  
  val validContractSingleChain: Gen[Contract] = crossChainSingleChainContractGen()
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisCrossChainContractGen(master, ts)
    contract <- validContractSingleChain
    description <- validDescStringGen
    seedBytes: Array[Byte] = Ints.toByteArray(1000)
    pair = EllipticCurveImpl.createKeyPair(seedBytes)
    privateKey = pair._1
    publicKey = pair._2
    chainId = Longs.toByteArray(1L)
    initCorssChainDataStack: Seq[DataEntry] <- initCrossChainContractSingleChainDataStackGen(publicKey, chainId)
    create <- registerCrossChainContractGen(master, contract, initCorssChainDataStack, description, fee + 10000000000L, ts + 1)
  } yield (genesis, create)

  property("register cross-chain single chain contract transaction doesn't break invariant") {
    forAll(preconditionsAndRegContractTest) { case (genesis, reg: RegisterContractTransaction) =>
    assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -reg.transactionFee
        totalPortfolioDiff.effectiveBalance shouldBe -reg.transactionFee
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val contractId = reg.contractId.bytes

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractTokens(contractId) shouldBe 0
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractCrossChain.contractSingleChain))
      }
    }
  }
}