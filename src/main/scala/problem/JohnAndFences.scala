package problem

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable

/* Divide and Conquer can't solve this problem completely, still timeout inspite of O(nlogn) */
object JohnAndFences {
  case class Range(from: Int, to: Int)
  val MIN_SIZE = 5

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def solve(heights: Seq[Int]): Int = {
    def solve1(height: Int, heights: Seq[(Int,Int)]): Seq[Range] = {
      def go(hs: Seq[(Int, Int)], acc: List[Range]): Seq[Range] = hs.dropWhile{case (h, _) => h < height } match {
        case Nil => acc
        case xs => {
          val ys = xs.takeWhile { case(h, _) => h >= height }.map { case(_, idx) => idx }
          go(xs.drop(ys.length), Range(ys.min, ys.max) :: acc)
        }
      }

      go(heights, List())
    }

    def split(hs: Seq[(Int, Int)]): Seq[(Int, Range)] = {
      val len = hs.length
      if (hs.length <= MIN_SIZE) {
        hs.flatMap{ case (h, _) => solve1(h, hs).map(range => (h, range)) }
      }
      else {
        val (l, r) = hs.splitAt(len / 2)
        val (m1, m2) = parallel(split(l), split(r))
        merge1(m1, m2)
      }
    }

    def merge1(left: Seq[(Int, Range)], right: Seq[(Int, Range)]): Seq[(Int, Range)] = {
      val lR = for{
        l <- left
      } yield {
        val (h, rng) = l
        val a = right.filter{case (rh, Range(fr, _)) => rh >= h && fr == rng.to + 1}
        if (a.isEmpty) {
          l
        } else {
          val b = a.maxBy(_._2.to)
          (h, Range(rng.from, b._2.to))
        }
      }

      val rR = for {
        r <- right
      } yield {
        val (h, rng) = r
        val a = left.filter{case (lh, Range(_, to)) => lh >= h && to == rng.from - 1}
        if (a.isEmpty) {
          r
        } else {
          val b = a.minBy(_._2.from)
          (h, Range(b._2.from, rng.to))
        }
      }

      (lR ++ rR).distinct
    }

    split(heights.zipWithIndex).map{ case (h, ws) => h * (ws.to - ws.from + 1) }.max
  }

  def main(args: Array[String]): Unit = {
    readInt()

    val hs = readLine().split(' ').map(_.toInt)

    println(solve(hs.toList))
  }
}
