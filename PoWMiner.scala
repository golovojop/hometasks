package mining

import scorex.crypto.hash.CryptographicHash32

import scala.math.BigInt

class PoWMiner[HF <: CryptographicHash32](hashFunction: HF) {

  private val MaxTarget: BigInt = BigInt(1, Array.fill(32)((-1).toByte))
   
  def doWork(data: Array[Byte], difficulty: BigInt): ProvedData = {
	
	/**
	* Set power value to 5 and upper (ie 2**5, 2**6, ...)
	*/
	var range : Int = 1 << 5
    var pd = ProvedData(data, range)
	
	for(i <- -range to range-1) {
		if(validateWork(pd, difficulty)){
			println ("nonce: " + i)
			return pd
		}
		pd = ProvedData(data, i)
	}
		
	pd
	
	
	/*
	var range : Int = BigInt(2).pow(5).toInt
    var pd = ProvedData(data, range);
	for(i <- -range to range; if(validateWork(ProvedData(data, i), difficulty))) yield pd = ProvedData(data, i)
	pd
	*/
	  
  }

  def validateWork(data: ProvedData, difficulty: BigInt): Boolean = realDifficulty(data) >= difficulty

  private def realDifficulty(noncedData: ProvedData): BigInt =
    MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}
