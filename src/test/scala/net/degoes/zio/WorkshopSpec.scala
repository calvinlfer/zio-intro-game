package net.degoes.zio

import zio.ZIO
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment._

object WorkshopSpec
    extends DefaultRunnableSpec({
      import BoardHelpers._
      import TicTacToe._

      suite("Workshop tests")(
        testM("HelloWorld") {
          for {
            value  <- HelloWorld.run(Nil)
            output <- TestConsole.output
          } yield assert(value, equalTo(0)) &&
            assert(output, equalTo(Vector("Hello World!\n")))
        },
        testM("ErrorRecovery") {
          assertM(ErrorRecovery.run(Nil), equalTo(1))
        },
        testM("Looping") {
          for {
            _      <- Looping.run(Nil)
            output <- TestConsole.output
          } yield assert(
            output.toList,
            forall(equalTo("All work and no play makes Jack a dull boy\n"))
          ) &&
            assert(output.length, equalTo(100)) &&
            assertCompletes
        },
        testM("PromptName") {
          ZIO(assertCompletes)
        } @@ ignore,
        testM("AlarmApp") {
          ZIO(assertCompletes)
        } @@ ignore,
        suite("StmLunchTime")(
          testM(
            "Finding an empty seat and taking it keeps the seat occupied then vacating it should free it"
          ) {
            import StmLunchTime._
            for {
              t        <- makeEmptyTable(2).commit
              seatOne  <- t.findEmptySeat.collect { case Some(s) => s }.commit
              _        <- t.takeSeat(seatOne).commit
              seatTwo  <- t.findEmptySeat.collect { case Some(s) => s }.commit
              _        <- t.vacateSeat(seatOne).commit
              freeSeat <- t.findEmptySeat.collect { case Some(s) => s }.commit
            } yield assert(seatOne, not(equalTo(seatTwo))) && assert(freeSeat, equalTo(seatOne))
          }
        ),
        suite("StmPriorityQueue")(
          testM("Enqueueing an element and removing it should succeed") {
            import StmPriorityQueue._
            for {
              q         <- PriorityQueue.make[String].commit
              element   = "First"
              _         <- q.offer(element, priority = 0).commit
              retrieved <- q.take.commit
            } yield assert(retrieved, equalTo(element))
          },
          testM("Inserting with different priorities and then pulling should pull the lowest priority first") {
            import StmPriorityQueue._
            for {
              q     <- PriorityQueue.make[String].commit
              world = "World"
              hello = "Hello"
              _     <- q.offer(world, 1).zip(q.offer(hello, 0)).commit
              h     <- q.take.commit
              w     <- q.take.commit
            } yield assert(h, equalTo(hello)) && assert(w, equalTo(world))
          },
          testM("Removing an element from an empty queue should wait forever") {
            import StmPriorityQueue._
            // NOTE: we rely on timeouts to ensure that this won't terminate
            for {
              q <- PriorityQueue.make[String].commit
              _ <- q.take.commit
            } yield assertCompletes
          } @@ timeout(500.millis) @@ failure
        ),
        suite("Board")(
          test("won horizontal first") {
            horizontalFirst(Mark.X) && horizontalFirst(Mark.O)
          },
          test("won horizontal second") {
            horizontalSecond(Mark.X) && horizontalSecond(Mark.O)
          },
          test("won horizontal third") {
            horizontalThird(Mark.X) && horizontalThird(Mark.O)
          },
          test("won vertical first") {
            verticalFirst(Mark.X) && verticalFirst(Mark.O)
          },
          test("won vertical second") {
            verticalSecond(Mark.X) && verticalSecond(Mark.O)
          },
          test("won vertical third") {
            verticalThird(Mark.X) && verticalThird(Mark.O)
          },
          test("won diagonal first") {
            diagonalFirst(Mark.X) && diagonalFirst(Mark.O)
          },
          test("won diagonal second") {
            diagonalSecond(Mark.X) && diagonalSecond(Mark.O)
          }
        )
      )
    })

object BoardHelpers {
  import TicTacToe._

  def horizontalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, chr, chr),
          List(' ', ' ', ' '),
          List(' ', ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def horizontalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', ' '),
          List(chr, chr, chr),
          List(' ', ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def horizontalThird(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', ' '),
          List(' ', ' ', ' '),
          List(chr, chr, chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, ' ', ' '),
          List(chr, ' ', ' '),
          List(chr, ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', chr, ' '),
          List(' ', chr, ' '),
          List(' ', chr, ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalThird(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', chr),
          List(' ', ' ', chr),
          List(' ', ' ', chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def diagonalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', chr),
          List(' ', chr, ' '),
          List(chr, ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def diagonalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, ' ', ' '),
          List(' ', chr, ' '),
          List(' ', ' ', chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }
}
