package com.joepritzel.problem

import scala.Option.option2Iterable

/**
 * The first 11 problems.
 *
 * @author Joe Prtizel
 */
object OneThroughEleven extends App {
  new One
  new Two
  new Three
  new Four
  new Five
  new Six
  new Seven
  new Eight
  new Nine
  new Ten
  new Eleven
}

class One extends Problem {
  override def execute {
    def last[T](list: List[T]) = list.last
    println(last(List(1, 1, 2, 3, 5, 8)))
  }
}

class Two extends Problem {
  override def execute {
    def penultimate[T](list: List[T]) = list.init.last
    println(penultimate(List(1, 1, 2, 3, 5, 8)))
  }
}

class Three extends Problem {
  override def execute {
    def nth[T](n: Int, list: List[T]) = list(n)
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
  }
}

class Four extends Problem {
  override def execute {
    def length[T](list: List[T]) = list.size
    println(length(List(1, 1, 2, 3, 5, 8)))
  }
}

class Five extends Problem {
  override def execute {
    def reverse[T](list: List[T]) = list.reverse
    println(reverse(List(1, 1, 2, 3, 5, 8)))
  }
}

class Six extends Problem {
  override def execute {
    def isPalindrome[T](list: List[T]) = list.reverse == list
    println(isPalindrome(List(1, 2, 3, 2, 1)))
  }
}

class Seven extends Problem {
  override def execute {
    def any2Itr[T](a: T): Iterable[T] = Some(a)
    def flatten(list: List[Any]): List[Any] = list.flatMap {
      case list: List[Any] => flatten(list)
      case element: Any => List(element)
    }
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }
}

class Eight extends Problem {
  override def execute {
    def compress[T](list: List[T]) = list.foldLeft(List[T]())((e, x) => if (!e.isEmpty && e.last == x) e else e :+ x)
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

class Nine extends Problem {
  override def execute {
    def pack[T](list: List[T]) = list.foldLeft(List[List[T]]())((e, x) => if (!e.isEmpty && e.last.head == x) e.init :+ (e.last :+ x) else e :+ List(x))
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

class Ten extends Problem {
  override def execute {
    def pack[T](list: List[T]) = list.foldLeft(List[(Int, T)]())((e, x) => if (!e.isEmpty && e.last._2 == x) e.init :+ e.last._1 + 1 -> x else e :+ 1 -> x)
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}

class Eleven extends Problem {
  override def execute {
    def pack[T](list: List[T]) = list.foldLeft(List[(Int, T)]())((e, x) => if (!e.isEmpty && e.last._2 == x) e.init :+ e.last._1 + 1 -> x else e :+ 1 -> x).map(e => if (e._1 == 1) e._2 else e)
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
