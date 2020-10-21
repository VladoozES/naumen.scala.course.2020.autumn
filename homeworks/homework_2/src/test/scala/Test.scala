import utest._
import Exercises.Vector2D

object Test extends TestSuite{

  val tests = Tests{
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(0, 10) == 33L)
      assert(Exercises.sumOfDivBy3Or5(0, 0) == 0L)
      assert(Exercises.sumOfDivBy3Or5(13, 15) == 15L)
      assert(Exercises.sumOfDivBy3Or5(6, 9) == 15L)
      assert(Exercises.sumOfDivBy3Or5(4, 5) == 5L)
    }
    'primeFactor - {
      assert(Exercises.primeFactor(80).toSet == Set(2, 5))
      assert(Exercises.primeFactor(0).toSet == Set())
      assert(Exercises.primeFactor(1).toSet == Set())
      assert(Exercises.primeFactor(3).toSet == Set(3))
      assert(Exercises.primeFactor(235).toSet == Set(5, 47))
    }
    'sumByFunc - {
      assert(Exercises.sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0)) == 0)
      assert(Exercises.sumScalars(Vector2D(2, 5), Vector2D(6, 15), Vector2D(1, 10), Vector2D(9, 16)) == 256)
      assert(Exercises.sumCosines(Vector2D(2, 2), Vector2D(2, 2), Vector2D(3, 2), Vector2D(3, 2)) == 2)
    }
    'sortByHeavyweight - {
      val balls = Map("Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35))
      assert(Exercises.sortByHeavyweight(balls) == List("Aluminum", "Tungsten"))
      val balls1 = Map("Aluminum" -> (3, 2.6889), "Tin" -> (1, 7.29), "Silver" -> (4, 4.505), "Cobalt" -> (4, 8.90))
      assert(Exercises.sortByHeavyweight(balls1) == List("Tin", "Aluminum", "Silver", "Cobalt"))
    }
  }
}
