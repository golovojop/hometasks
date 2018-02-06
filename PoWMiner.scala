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
			println ("data: " 			+ HexBytesUtil.bytes2hex (data/*, Option(" ")*/))
			println ("hash: " 			+ HexBytesUtil.bytes2hex (hashFunction.hash(pd.bytes)/*, Option(" ")*/))
			println ("difficulty: "		+ HexBytesUtil.bytes2hex (difficulty.toByteArray))
			println ("realDifficulty: "	+ HexBytesUtil.bytes2hex (realDifficulty(pd).toByteArray))
			println ("MaxTarget: "		+ HexBytesUtil.bytes2hex (MaxTarget.toByteArray))
			println ("\n")

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

  private def realDifficulty(noncedData: ProvedData): BigInt = MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}

object HexBytesUtil {

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
    // bytes.foreach(println)
  }

  def example {
    val data = "48 65 6C 6C 6F 20 57 6F 72 6C 64 21 21"
    val bytes = hex2bytes(data)
    println(bytes2hex(bytes, Option(" ")))

    val data2 = "48-65-6C-6C-6F-20-57-6F-72-6C-64-21-21"
    val bytes2 = hex2bytes(data2)
    println(bytes2hex(bytes2, Option("-")))

    val data3 = "48656C6C6F20576F726C642121"
    val bytes3 = hex2bytes(data3)
    println(bytes2hex(bytes3))
  }

}


object HexStringUtil {

	// convert normal string to hex bytes string
	def string2hex(str: String): String = {
		str.toList.map(_.toInt.toHexString).mkString
	}

	// convert hex bytes string to normal string
    def hex2string(hex: String): String = {

        hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toChar).mkString

    }


    def demo {
        val appkey = "9GLV//lv/kYFW2o3/bihxwnMcmo="

        // string to hex
        val appkey_hex = string2hex(appkey)

        // 39474c562f2f6c762f6b594657326f332f62696878776e4d636d6f3d
        println(appkey_hex)
		
		// hex to string
		val appkey_string_again = hex2string(appkey_hex)
		
		// 9GLV//lv/kYFW2o3/bihxwnMcmo=
        println(appkey_string_again)

    }

}

