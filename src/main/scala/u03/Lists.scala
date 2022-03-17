package u03

import u02.Modules.{Person, isStudent}
import u02.Optionals.Option

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    /*def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()*/

    def map[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    /*
    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()*/

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(v => pred(v) match
        case true => Cons(v, Nil())
        case false => Nil()
      )

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) => n match
        case 1 => t
        case _ => drop(t, n-1)
      case Nil() => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    import u02.Optionals

    def max(l: List[Int]): Option[Int] =
      def _max(l: List[Int], currentMax: Option[Int]): Option[Int] = l match
        case Cons(h,t) =>
          currentMax match
            case Option.Some(v) if h >= v  => _max(t, Option.Some(h))
            case Option.None() => _max(t, Option.Some(h))
            case _ => _max(t, currentMax)
        case Nil() => currentMax
      _max(l, Option.None())

    def getTeacherCourses(l: List[Person]): List[String] =
      flatMap(l)(v => v match
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )

    def foldLeft[A,B](l: List[A])(acc: B)(f: (B, A) => B): B = l match
      case Cons(h,t) => foldLeft(t)(f(acc, h))(f)
      case _ => acc

    def reverse[A](l: List[A]): List[A] = l match
      case Cons(h,t) => append(reverse(t), Cons(h, Nil()))
      case Nil() => Nil()

    def foldRight[A,B](l: List[A])(acc: B)(f: (A, B) => B): B =
      foldLeft(reverse(l))(acc)((a,b) => f(b,a))



  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
