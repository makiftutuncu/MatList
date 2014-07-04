case class Cons(head: Int, tail: MatList) extends MatList
case object Nil extends MatList

trait MatList {
  def +(item: Int): MatList = this match {
    case Nil => Cons(item, Nil)
    case Cons(head, tail) => Cons(item, this)
  }

  def ++(list: MatList): MatList = this match {
    case Nil => list
    case Cons(head, tail) => Cons(head, tail ++ list)
  }

  def get(item: Int): Option[Int] = this match {
    case Nil => None
    case Cons(head, tail) if head == item => Some(head)
    case Cons(head, tail) => tail get item
  }

  def -(item: Int): MatList = this match {
    case Nil => Nil
    case Cons(head, tail) if head == item => tail
    case Cons(head, tail) => Cons(head, tail - item)
  }

  def contains(item: Int): Boolean = this match {
    case Nil => false
    case Cons(head, tail) if head == item => true
    case Cons(head, tail) => tail contains item
  }

  def map(function: Int => Int): MatList = this match {
    case Nil => Nil
    case Cons(head, tail) => Cons(function(head), tail map function)
  }

  def flatMap(function: Int => MatList): MatList = this match {
    case Nil => Nil
    case Cons(head, tail) => function(head) ++ (tail flatMap function)
  }

  def filter(function: Int => Boolean): MatList = this match {
    case Nil => Nil
    case Cons(head, tail) if function(head) => Cons(head, tail filter function)
    case Cons(head, tail) => tail filter function
  }

  def sum: Int = this match {
    case Nil => 0
    case Cons(head, tail) => head + tail.sum
  }

  def foldLeft[Z](base: Z)(function: (Z, Int) => Z): Z = this match {
    case Nil => base
    case Cons(head, tail) => function(tail.foldLeft(base)(function), head)
  }

  override def toString: String = this match {
    case Nil => "Nil"
    case Cons(head, tail) => head + ", " + tail
  }
}

object MatList {
  def apply(items: Int*): MatList = {
    if(items.isEmpty) Nil else Cons(items.head, apply(items.tail:_*))
  }
}
