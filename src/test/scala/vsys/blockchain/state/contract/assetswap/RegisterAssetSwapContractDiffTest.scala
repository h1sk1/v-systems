package vsys.blockchain.state.contract.assetswap

import cats.Monoid
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractAssetSwap}
import vsys.blockchain.contract.assetswap.AssetSwapContractGen
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract.RegisterContractTransaction

class RegisterAssetSwapContractDiffTest extends PropSpec
    with PropertyChecks
    with GeneratorDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with AssetSwapContractGen {

    private implicit def noShrink[T]: Shrink[T] = Shrink(_ => Stream.empty)

    val languageCode: String = "vdds"
    val languageVersion: Int = 2

    val preconditionAndBuildAssetSwapContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]],
        Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
        langCode <- ContractGenHelper.languageCodeGen(languageCode)
        langVer <- ContractGenHelper.languageVersionGen(languageVersion)
        init <- Gen.const(ContractAssetSwap.contract.trigger)
        descriptor <- Gen.const(ContractAssetSwap.contract.descriptor)
        stateVar <- Gen.const(ContractAssetSwap.contract.stateVar)
        stateMap <- Gen.const(ContractAssetSwap.contract.stateMap)
        textual <- Gen.const(ContractAssetSwap.contract.textual)
    } yield (langCode, langVer, init, descriptor, stateVar, stateMap, textual)


    property("register asset-swap contract build doesn't break invariant"){
        forAll(preconditionAndBuildAssetSwapContract) { case (langCode, langVer, init, descriptor, stateVar, stateMap, textual) =>
        Contract.buildContract(langCode, langVer, init, descriptor, stateVar, stateMap, textual) shouldBe an[Right[_, _]]
        }
    }

    val validContract: Gen[Contract] = assetSwapContractGen()
    val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
        (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
        genesis <- genesisAssetSwapContractGen(master, ts)
        contract <- validContract
        //data: Seq[DataEntry] <- initAssetSwapDataStackGen()
        description <- validDescStringGen
        create <- registerAssetSwapContractGen(master, contract, description, fee + 10000000000L, ts + 1)
    } yield (genesis, create)

    property("register asset-swap contract transaction doesn't break invariant") {
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
            newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractAssetSwap.contract))
        }
        }
    }
}