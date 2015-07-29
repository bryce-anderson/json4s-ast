import org.scalameter.PerformanceTest.Microbenchmark
import org.scalameter.api._


//case class JBoolean(boolean: Boolean)

object JBooleanVariants extends Microbenchmark {

  import org.json4s.basic.ast.JBoolean

  val x = JBoolean.True

  val JBoolean(y) = true

  x match {
    case JBoolean(true)  => println("Its true!")
    case JBoolean(false) => println("Its false!")
  }

  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)

  val bools: Gen[Boolean] = for {
    size <- sizes
  } yield {
    if (size % 2 == 0)
      true
    else
      false
  }

  performance of "CaseObject" in {
    measure method "construct" in {
      using(bools) in {
        b => org.json4s.ast.JBoolean(b)
      }
    }
  }

  performance of "Constructor" in {
    measure method "construct" in {
      using(bools) in {
        b => JBoolean(b)
      }
    }
  }
}
