import zio.{Duration, Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, Exit}
import zio.Duration._

object ZioFibers extends ZIOAppDefault {


  // Computation is value + effect (side effect) in the world.
  // It makes it hard to accomplish substitution model

  private val aValue = {
    println("Hello, scala")
    30
  }

  private def incrementValue(x: Int) = x + 1

  incrementValue(30) == incrementValue(aValue)

  // IO monad
  // ZIO[R, E, A]
  val zioValue: UIO[Int] = ZIO.succeed(30) // ZIO[Any, Nothing, Int]

  // concurrency
  private val takeShower = ZIO.succeed("Taking a shower")
  private val boilWater = ZIO.succeed("Boiling a bottle of water")
  private val prepareCoffee = ZIO.succeed("Preparing a cup of coffee")

  private def threadName = s"[${Thread.currentThread().getName}]"

  private def synchronousRoutine() = for {
    _ <- takeShower.debug(threadName)
    _ <- boilWater.debug(threadName)
    _ <- prepareCoffee.debug(threadName)
  } yield ()

  // fiber = schedulable computation
  // Fiber[E, A]

  private def concurrentRoutine() = for {
    showerFiber <- takeShower.debug(threadName).fork
    waterFiber <- boilWater.debug(threadName).fork
    zippedFiber = showerFiber.zip(waterFiber)
    result <- zippedFiber.join.debug(threadName) *> prepareCoffee.debug(threadName)
  } yield ()

  private val callFromAlice = ZIO.succeed("Call from Alice")
  private val boilWaterWithTime = boilWater.debug(threadName) *> ZIO.sleep(Duration.fromSeconds(5)) *> ZIO.succeed("Boiled water ready")

  private def concurrentRoutineWithAliceCall() = for {
    _ <- takeShower.debug(threadName)
    waterFiber <- boilWaterWithTime.fork
    _ <- callFromAlice.debug(threadName).fork *> waterFiber.interrupt.debug(threadName)
    _ <- ZIO.succeed("Screw my coffee, going with Alice").debug(threadName)
  } yield ()

  private val prepareCoffeeWithTime = prepareCoffee.debug(threadName) *> ZIO.sleep(Duration.fromSeconds(5)) *> ZIO.succeed("Coffee ready")

  private def concurrentRoutineWithCoffeeAtHone () = for {
    _ <- takeShower.debug(threadName)
    _ <- boilWater.debug(threadName)
    coffeeFiber <- prepareCoffeeWithTime.fork.uninterruptible
    result <- callFromAlice.debug(threadName).fork *> coffeeFiber.interrupt.debug(threadName)
    _ <- result match {
      case Exit.Success(_) => ZIO.succeed("Sorry Alice, having a coffee at home").debug(threadName)
      case _ => ZIO.succeed("Going to a cafe to have a coffee with Alice").debug(threadName)
    }
  } yield ()

  override def run: ZIO[ZioFibers.Environment with ZIOAppArgs with Scope, Any, Any] = {
    concurrentRoutineWithCoffeeAtHone().exitCode
  }



}
