package me.chuwy.otusbats

import me.chuwy.otusbats.Show.mkString_


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  implicit val intShow: Show[Int] = new Show[Int] {
    override def show(a: Int): String = a.toString
  }

  implicit val booleanShow: Show[Boolean] = new Show[Boolean]{
    override def show(a: Boolean): String = a.toString
  }

  implicit val doubleShow: Show[Double] = new Show[Double]{
    override def show(a: Double): String = a.toString
  }

  implicit val stringShow: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = new Show[List[A]] {
    override def show(a: List[A]): String = a.map(element => ev.show(element)).toString()
  }


  // 2. Summoner (apply)
  object ShowSummoner {
    def apply[A](value: A)(implicit s: Show[A]): String = s.show(value)
  }
  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String =
    list match {
      case::(head, Nil) => s"$begin$head$end"
      case ::(head, tail) =>{
        val sb = new StringBuilder()
        sb.append(begin)
        sb.append(head)
        tail.foreach(t => sb.append(s"$separator${t.show}"))
        sb.append(end)
        sb.toString()
      }
      case Nil => s"$begin$end"
    }


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = valueA => valueA.toString
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = valueA => f(valueA)

}

object Main{
  def main(args: Array[String]): Unit = {
    println(mkString_(List(1,2,3,4,5), "[", "]", ";"))
  }
}
