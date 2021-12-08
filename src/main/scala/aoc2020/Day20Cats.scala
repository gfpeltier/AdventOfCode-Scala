package aoc2020

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Stream, Pipe, text}
import fs2.io.file.{Files, Path}

object Day20Cats extends IOApp {

  case class Tile(id: Int, matrix: Seq[Seq[Char]])

  object Tile {
    def apply(strs: Seq[String]): Tile =
      Tile(strs.head.split(' ').last.toInt, strs.tail.map(_.toList))
  }

  def split[A](seq: Seq[A], f: A => Boolean): Seq[Seq[A]] =
    seq.foldLeft((List.empty[Seq[A]], List.empty[A]))((aggCurr: (List[Seq[A]], List[A]), a: A) => if (f(a)) {
    (aggCurr._1 :+ aggCurr._2, List[A]())
  } else {
    (aggCurr._1, aggCurr._2 :+ a)
  })
  ._1

  //def ssplit[F, A](f: A => Boolean): Pipe[F, Seq[A], Seq[Seq[A]]] = (strm) => strm.map(split(_, f))

//  def split[A](arr: Seq[A], pred: A => Boolean): Seq[Seq[A]] =
//    arr
//      .foldLeft(List(List.empty[A]))((out, next) => {
//        if(pred(next)) {
//          out :+ List()
//        } else {
//          out.updated(out.length - 1, out.last :+ next)
//        }
//      })
//      .filter(_.nonEmpty)

  override def run(args: List[String]): IO[ExitCode] = ???
//    Files[IO]
//      .readAll(Path("2020/d20test.txt"))
//      .through(text.utf8.decode)
//      .map(_.split('\n'))
//      .through(ssplit((s: String) => s.isEmpty))
//      .through(strs => Stream.emit(split(strs, (s: String) => s.isEmpty)))
//      .map((tstrs: Seq[String]) =>  Tile(tstrs))

}
