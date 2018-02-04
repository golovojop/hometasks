package mining

import scorex.crypto.hash.CryptographicHash32

import scala.math.BigInt

class PoWMiner[HF <: CryptographicHash32](hashFunction: HF) {

  private val MaxTarget: BigInt = BigInt(1, Array.fill(32)((-1).toByte))
   
  def doWork(data: Array[Byte], difficulty: BigInt): ProvedData = {
   // println("data length = " + data.length)

	
	var range : Int = BigInt(2).pow(4).toInt
    var pd = ProvedData(data, 0);
	
	for(i <- -range to range) {
		if(validateWork(pd, difficulty)) return pd
		pd = ProvedData(data, i)
	}
		
	pd
	
	
	/*
	var range : Int = BigInt(2).pow(10).toInt
    var pd = ProvedData(data, range);
	for(i <- -range to range; if(validateWork(ProvedData(data, i), difficulty))) yield pd = ProvedData(data, i)
	pd
	*/
	
	
	
	
	  
  }

  def validateWork(data: ProvedData, difficulty: BigInt): Boolean = realDifficulty(data) >= difficulty

  private def realDifficulty(noncedData: ProvedData): BigInt =
    MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}
