import com.myatsumoto.sabi.Sabi
import org.scalatest.FunSuite
import scala.collection.mutable._

class TestSuite extends FunSuite {
  test("search") {
    val str  = "abccbbabcax"
    val sabi = new Sabi(str)
    val keys = List("a", "b", "c", "x")
    keys.foreach(key =>
      sabi.search(key).foreach(i =>
        assert(key == str(i).toString)
      )
    )
  }
}
