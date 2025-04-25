package vsys.blockchain.contract.assetswap

import org.scalacheck.Gen
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{Contract, ContractAssetSwap}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{RegisterContractTransaction}

trait AssetSwapContractGen {
  
  def assetSwapContractGen(): Gen[Contract] = ContractAssetSwap.contract

  def genesisAssetSwapContractGen(rep: PrivateKeyAccount,
                                  ts: Long): Gen[GenesisTransaction] =
      GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()
  
  def registerAssetSwapContractGen(signer: PrivateKeyAccount,
                      contract: Contract,
                      description: String,
                      fee: Long,
                      ts: Long): Gen[RegisterContractTransaction] =
      RegisterContractTransaction.create(signer, contract, Seq(), description, fee, feeScale, ts).explicitGet()

  def initAssetSwapContractDataStackGen(): Gen[Seq[DataEntry]] = Seq()
}