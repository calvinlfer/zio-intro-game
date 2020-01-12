package net.degoes.zio

import zio._
import java.text.NumberFormat
import java.util.concurrent.TimeUnit

import zio.duration.Duration
import zio.random.Random

object ZIOTypes {
  type ??? = Nothing

  /**
    * EXERCISE 1
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends App {
  import zio.console._

  /**
    * EXERCISE 2
    *
    * Implement a simple "Hello World!" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!").as(0)
}

object PrintSequence extends App {
  import zio.console._

  /**
    * EXERCISE 3
    *
    * Using `*>` (`zipRight`), compose a sequence of `putStrLn` effects to
    * produce an effect that prints three lines of text to the console.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello!") *> putStrLn("How are you") *> putStrLn("Today").as(0)
}

object ErrorRecovery extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 4
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    failed.orElse(putStrLn("recovering!")).as(1)
}

object Looping extends App {
  import zio.console._

  /**
    * EXERCISE 5
    *
    * Implement a `repeat` combinator using `flatMap` and recursion.
    */
  def repeat[R, E, A](n: Int)(task: ZIO[R, E, A]): ZIO[R, E, A] =
    if (n == 1) task
    else task *> repeat(n - 1)(task)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    repeat(100)(putStrLn("All work and no play makes Jack a dull boy")) as 0
}

object EffectConversion extends App {

  /**
    * EXERCISE 6
    *
    * Using ZIO.effect, convert the side-effecting of `println` into a pure
    * functional effect.
    */
  def myPrintLn(line: String): Task[Unit] = ZIO.effect(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (myPrintLn("Hello Again!") as 0) orElse ZIO.succeed(1)
}

object ErrorNarrowing extends App {
  import java.io.IOException
  import scala.io.StdIn.readLine
  implicit class Unimplemented[A](v: A) {
    def ? = ???
  }

  /**
    * EXERCISE 7
    *
    * Using `ZIO#refineToOrDie`, narrow the error type of the following
    * effect to IOException.
    */
  val myReadLine: IO[IOException, String] =
    ZIO.effect(readLine()).refineToOrDie[IOException]

  def myPrintLn(line: String): UIO[Unit] = UIO(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- myPrintLn("What is your name?")
      name <- myReadLine
      _ <- myPrintLn(s"Good to meet you, ${name}")
    } yield 0) orElse ZIO.succeed(1)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 8
    *
    * Using `ZIO#flatMap`, implement a simple program that asks the user for
    * their name (using `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("What is your name?")
      .flatMap(_ => getStrLn)
      .flatMap(name => putStrLn(s"Hello $name"))
      .as(0)
      .fold(_ => StdInputFailed, identity)
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE 9
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Choose a random number between 0 and 10")
      guess <- getStrLn
      random <- nextInt(10)
      _ <- analyzeAnswer(random, guess)
    } yield 0).catchAll(_ => ZIO.succeed(1))
}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  /**
    * EXERCISE 10
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds. Use `refineOrDie` to
    * narrow the error type as necessary.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .effect(input.trim.toInt)
        .refineOrDie { case t: NumberFormatException => t }
        .map(Duration(_, TimeUnit.SECONDS))

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      putStrLn(s"$input was not a valid number of seconds, please try again") *>
        getAlarmDuration

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /**
    * EXERCISE 11
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      duration <- getAlarmDuration
      _ <- putStrLn(s"Sleeping for ${duration.asScala.toSeconds} seconds")
      _ <- ZIO.sleep(duration)
      _ <- putStrLn("Time to wakeup!!!")
    } yield 0).catchAll(e => putStrLn(s"failed with $e") *> ZIO.succeed(1))
}

object Cat extends App {
  import zio.console._
  import zio.blocking._
  import java.io.IOException

  /**
    * EXERCISE 12
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    ZIO.accessM { b =>
      // use a resource to ensure the file is closed
      val fileResource = ZManaged.make(
        ZIO.effect(scala.io.Source.fromFile(file))
      )(source => ZIO.effect(source.close()).catchAll(UIO.die))
      // can also do ZIO.effect(...).refineToOrDie[Nothing]
      // note that if you want to recover from dying (not a failure) since
      // dying removes the failure from the error channel, you need to recover using catch(All/Some)Cause

      // run the computation on Blocking executor
      b.blocking
        .blocking(fileResource.use(source => ZIO.effect(source.mkString)))
        .refineOrDie { case t: IOException => t }
    }

  /**
    * EXERCISE 13
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil =>
        readFile(file)
          .flatMap(putStrLn)
          .as(0)
          .catchAll(e => putStrLn(s"failed with $e") *> ZIO.succeed(1))

      case _ =>
        putStrLn("Usage: cat <file>") as 2
    }
}

object CatIncremental extends App {
  import zio.console._
  import zio.blocking._
  import java.io.{IOException, InputStream, FileInputStream}

  /**
    * BONUS EXERCISE
    *
    * Implement a `blockingIO` combinator to use in subsequent exercises.
    */
  def blockingIO[A](a: => A): ZIO[Blocking, IOException, A] =
    ZIO.accessM(_.blocking.effectBlocking(a).refineToOrDie[IOException])

  /**
    * EXERCISE 14
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] = blockingIO(is.close())

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      blockingIO {
        val r = is.readNBytes(32)
        if (r.isEmpty) None else Some(Chunk.fromArray(r))
      }
  }
  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blockingIO(FileHandle(new FileInputStream(file)))
  }

  /**
    * EXERCISE 15
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil =>
        FileHandle
          .open(file)
          .bracket(release = _.close.orDie, use = readUntilFinished)
          .as(0)
          .orElse(ZIO.succeed(1))

      case _ =>
        putStrLn("Usage: cat <file>") as 2
    }

  def readUntilFinished(
    f: FileHandle
  ): ZIO[Console with Blocking, IOException, Unit] = f.read.flatMap {
    case Some(chunk) =>
      putStr(new String(chunk.toArray)) *> readUntilFinished(f)

    case None =>
      ZIO.unit
  }
}

object AlarmAppImproved extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .effect(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
    * EXERCISE 16
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
    * prints a dot every second that the alarm is sleeping for, and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val dotEverySecondForever =
      (putStrLn(".") *> ZIO.sleep(Duration(1, TimeUnit.SECONDS))).forever
    val program = for {
      _ <- putStrLn("How long would you like to sleep for (in seconds)?")
      duration <- getAlarmDuration
      _ <- putStrLn(s"Preparing to sleep for $duration")
      _ <- ZIO.sleep(duration) race dotEverySecondForever
      _ <- putStrLn("Time to wakeup!!!")
    } yield 0

    program orElse ZIO.succeed(1)
  }
}

object ComputePi extends App {
  import zio.random._
  import zio.console._
  import zio.clock._
  import zio.duration._
  import zio.stm._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(inside: Ref[Long], total: Ref[Long])

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
    * EXERCISE 17
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    def updatePiState(
      piState: PiState
    ): ZIO[Random with Console, Nothing, Unit] = {
      randomPoint.flatMap {
        case (x, y) =>
          val inside = insideCircle(x, y)
          val updateTotal = piState.total.update(_ + 1)
          val updateInside = piState.inside.update(_ + 1)

          if (inside) updateInside zipPar updateTotal
          else updateTotal
      }.unit
    }

    def spawnWorkers(
      nrOfWorkers: Int
    )(piState: PiState): ZIO[Random with Console, Nothing, Unit] =
      ZIO.foreachPar_(0 until nrOfWorkers)(_ => updatePiState(piState))

    def printStats(piState: PiState): URIO[Console, Double] =
      (piState.inside.get zip piState.total.get)
        .flatMap {
          case (inside, outside) =>
            val res = estimatePi(inside, outside)
            putStrLn(res.toString) *> ZIO.succeed(res)
        }

    def printUntilAccuracyAchieved(
      piState: PiState,
      precision: Double = 0.0001
    ): URIO[Console, Double] =
      printStats(piState).doUntil(res => Math.abs(Math.PI - res) < precision)

    val createPiState: UIO[PiState] = for {
      in <- Ref.make(0L)
      total <- Ref.make(0L)
    } yield PiState(in, total)

//    createPiState
//      .flatMap { piState =>
//        (updatePiState(piState).forever zipPar updatePiState(piState).forever) race printStats(
//          piState
//        ).doUntil(res => res.toString.startsWith("3.1415"))
//      }
//      .as(0)

    createPiState
      .flatMap { piState =>
        spawnWorkers(10)(piState).forever race printUntilAccuracyAchieved(
          piState
        )
      }
      .as(0)
  }
}

object StmSwap extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 18
    *
    * Demonstrate the following code does not reliably swap two values in the
    * presence of concurrency.
    */
  def exampleRef = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      } yield ()

    for {
      ref1 <- Ref.make(100)
      ref2 <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get)(_ + _)
      // Output must add up to 100 but instead value ends up being 200
    } yield value
  }

  /**
    * EXERCISE 19
    *
    * Using `STM`, implement a safe version of the swap function.
    */
  def exampleStm = {
    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] = {
      val atomicSwap = for {
        one <- ref1.get
        two <- ref2.get
        _ <- ref1.set(two)
        _ <- ref2.set(one)
      } yield ()

      atomicSwap.commit
    }

    for {
      ref1 <- TRef.make(100).commit
      ref2 <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get)(_ + _).commit
    } yield value
  }

  def printResult(fa: UIO[Int]): URIO[Console, Unit] =
    fa.map(_.toString).flatMap(putStrLn)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    printResult(exampleRef) *> printResult(exampleStm) as 0
}

// See ATOMICALLY { DELETE YOUR ACTORS } - John A De Goes & Wiem Zine Elabadine | Scalar 2019 (15 mins in)
// Simon Peyton Jones has an excellent talk on STM ()
object StmLock extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 20
    *
    * Using STM, implement a simple binary lock by implementing the creation,
    * acquisition, and release methods.
    */
  class Lock private (tref: TRef[Boolean]) {
    def acquire: UIO[Unit] = acquireUsingCheck

    private def acquireUsingCheck: UIO[Unit] =
      (for {
        locked <- tref.get
        _ <- STM.check(!locked) // will suspend and retry when the variable changes
        _ <- tref.update(locked => !locked)
      } yield ()).commit

    private def acquireUsingRetry: UIO[Unit] =
      (for {
        locked <- tref.get
        _ <- (if (locked)
                STM.retry // abort the transaction and try it again from the start when the variables change
              else tref.set(true))
      } yield ()).commit

    def release: UIO[Unit] =
      (for {
        locked <- tref.get
        _ <- STM.check(locked)
        _ <- tref.update(locked => !locked)
      } yield ()).commit
  }

  object Lock {
    def make: UIO[Lock] = TRef.make(false).map(new Lock(_)).commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      lock <- Lock.make
      fiber1 <- lock.acquire
        .bracket_(lock.release)(putStrLn("Bob  : I have the lock!"))
        .repeat(Schedule.recurs(10))
        .fork
      fiber2 <- lock.acquire
        .bracket_(lock.release)(putStrLn("Sarah: I have the lock!"))
        .repeat(Schedule.recurs(10))
        .fork
      _ <- (fiber1 zip fiber2).join
    } yield 0) as 1
}

object StmLunchTime extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 21
    *
    * Using STM, implement the missing methods of Attendee.
    */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def isStarving: STM[Nothing, Boolean] = state.get.map {
      case Starving => true
      case Full     => false
    }

    def feed: STM[Nothing, Unit] =
      state.set(Full)
  }

  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full extends State
    }
  }

  /**
    * EXERCISE 22
    *
    * Using STM, implement the missing methods of Table.
    */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] =
      for {
        seatTaken <- seats.array(index).get
        _ <- STM.check(!seatTaken)
        _ <- seats.array(index).set(true)
      } yield ()

    def vacateSeat(index: Int): STM[Nothing, Unit] =
      for {
        seatTaken <- seats.array(index).get
        _ <- STM.check(seatTaken)
        _ <- seats.array(index).set(false)
      } yield ()
  }

  /**
    * EXERCISE 23
    *
    * Using STM, implement a method that feeds a single attendee.
    */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    for {
      index <- t.findEmptySeat.collect { case Some(index) => index }
      _ <- t.takeSeat(index) *> a.feed *> t.vacateSeat(index)
    } yield ()

  /**
    * EXERCISE 24
    *
    * Using STM, implement a method that feeds only the starving attendees.
    */
  def feedStarving(table: Table, list: List[Attendee]): UIO[Unit] =
    UIO.foreachPar_(list) { attendee =>
      (for {
        starving <- attendee.isStarving
        _ <- (if (starving) feedAttendee(table, attendee)
              else STM.succeed(()))
      } yield ()).commit
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(
        i =>
          TRef
            .make[Attendee.State](Attendee.State.Starving)
            .map(Attendee(_))
            .commit
      )
      table <- TArray
        .fromIterable(List.fill(TableSize)(false))
        .map(Table(_))
        .commit
      _ <- feedStarving(table, attendees)
    } yield 0
  }
}

object StmPriorityQueue extends App {
  import zio.console._
  import zio.stm._
  import zio.duration._

  /**
    * EXERCISE 25
    *
    * Using STM, design a priority queue, where lower integers are assumed
    * to have higher priority than higher integers.
    */
  class PriorityQueue[A] private (minLevel: TRef[Int],
                                  map: TMap[Int, TQueue[A]]) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      for {
        min <- minLevel.get
        _ <- (if (priority < min) minLevel.set(priority)
              else STM.unit)
        _ <- createOrInsertExisting(priority, a, map)
      } yield ()

    private def createOrInsertExisting(
      incomingPriority: Int,
      value: A,
      map: TMap[Int, TQueue[A]]
    ): STM[Nothing, Unit] =
      map.get(incomingPriority).flatMap {
        case Some(queue) =>
          queue.offer(value)

        case None =>
          for {
            queue <- TQueue.make[A](Int.MaxValue)
            _ <- queue.offer(value)
            _ <- map.put(incomingPriority, queue)
          } yield ()
      }

    def take: STM[Nothing, A] = takeHighestPriority(minLevel, map)

    private def takeHighestPriority(
      minLevel: TRef[Int],
      map: TMap[Int, TQueue[A]]
    ): STM[Nothing, A] =
      minLevel.get.flatMap { min =>
        map.get(min).flatMap {
          case None =>
            STM.retry

          case Some(queue) =>
            for {
              a <- queue.take
              size <- queue.size
              _ <- (if (size > 0) STM.succeed(())
                    else map.delete(min) *> pickNewMin(minLevel, map))
            } yield a
        }
      }

    private def pickNewMin(minLevel: TRef[Int],
                           map: TMap[Int, TQueue[A]]): STM[Nothing, Unit] =
      map.keys
        .map {
          case Nil  => Int.MaxValue
          case list => list.min
        }
        .flatMap(minLevel.set)
  }
  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      (TRef.make[Int](Int.MaxValue) zip TMap.empty[Int, TQueue[A]])
        .map { case (ref, map) => new PriorityQueue(ref, map) }
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(1.millis) *> queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(2.millis) *> queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
        .flatMap(putStrLn(_))
        .forever
        .fork *>
        getStrLn
    } yield 0).fold(_ => 1, _ => 0)
}

object StmReentrantLock extends App {
  import zio.console._
  import zio.stm._

  private final case class WriteLock(writeCount: Int,
                                     readCount: Int,
                                     fiberId: FiberId)
  private final class ReadLock private (readers: Map[Fiber.Id, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: Fiber.Id, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
    * EXERCISE 26
    *
    * Using STM, implement a reentrant read/write lock.
    */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = data.get.map(_.fold(_ => 0, _.writeCount)).commit

    def writeLocked: UIO[Boolean] = writeLocks.map(_ > 0)

    def readLocks: UIO[Int] = data.get.map(_.fold(_.total, _.readCount)).commit

    def readLocked: UIO[Boolean] = readLocks.map(_ > 0)

    val read: Managed[Nothing, Int] = ???

    val write: Managed[Nothing, Int] = ???
  }
  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] =
      TRef
        .make[Either[ReadLock, WriteLock]](Left(ReadLock.empty))
        .map(tref => new ReentrantReadWriteLock(tref))
        .commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = ???
}

object Sharding extends App {

  /**
    * EXERCISE 27
    *
    * Create N workers reading from a Queue, if one of them fails, then wait
    * for the other ones to process their current item, but terminate all the
    * workers.
    */
  def shard[R, E, A](queue: Queue[A],
                     n: Int,
                     worker: A => ZIO[R, E, Unit]): ZIO[R, E, Nothing] =
    ???

  def run(args: List[String]) = ???
}

object Hangman extends App {
  import zio.console._
  import zio.random._
  import java.io.IOException

  /**
    * EXERCISE 28
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] = ???

  /**
    * EXERCISE 29
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] = ???

  /**
    * EXERCISE 30
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] = ???

  /**
    * EXERCISE 31
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] = ???

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won extends GuessResult
    case object Lost extends GuessResult
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 32
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ???
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {
  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(row0: Int,
                            col0: Int,
                            rowInc: Int,
                            colInc: Int,
                            mark: Mark): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(row0: Int,
                                  col0: Int,
                                  rowInc: Int,
                                  colInc: Int): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(first: Iterable[Char],
                  second: Iterable[Char],
                  third: Iterable[Char]): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(List(' ', 'O', 'X'), List('O', 'X', 'O'), List('X', ' ', ' '))
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
