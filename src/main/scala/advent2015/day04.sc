import java.math.BigInteger
import java.security.MessageDigest

val input = "yzbqklnj"

val md5 = MessageDigest.getInstance("MD5")

def md5Hash(str: String): String = {
  val digest = md5.digest(str.getBytes)
  val bigInt = new BigInteger(1, digest)
  bigInt.toString(16).reverse.padTo(32, "0").reverse.mkString
}

(0 to Int.MaxValue).find(digits => md5Hash(s"$input$digits").startsWith("000000"))
