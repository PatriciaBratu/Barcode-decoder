import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable
import scala.math._

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit = {
    if (s == '0') Zero
    else One
  }
  def toBit(s: Int): Bit = {
    if (s == 0) Zero
    else One
  }

  // TODO 1.2
  def complement(c: Bit): Bit ={
    if (c == Zero) One
    else Zero
  }

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = {
    LStrings.map(_.map(toBit).toList)
  }// codificări L
  val rightList: List[List[Bit]] = {
    leftOddList.map(_.map(complement))
  }// codificări R
  val leftEvenList: List[List[Bit]] = {
    rightList.map(_.reverse)
  }// codificări  G
  
  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    def my_op(c: A, acc : List[List[A]]): List[List[A]] =
      acc match {
        case Nil => List(List(c))
        case line::xs=>
          if(c == line.last)
            (c :: line) :: xs
          else
            List(c) :: acc
      }
     l.foldRight(Nil)(my_op)

  }


  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] = {
    group(l).map(x => (x.length, x.head))
  }
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt =
      if(this.b == other.b) RatioInt(this.a - other.a, this.b)
      else RatioInt(this.a * other.b - other.a * this.b, this.b * other.b)
    def +(other: RatioInt): RatioInt =
      if(this.b == other.b) RatioInt(this.a + other.a, this.b)
      else RatioInt(this.a * other.b + other.a * this.b, this.b * other.b)
    def *(other: RatioInt): RatioInt =
      RatioInt(this.a * other.a, this.b * other.b)
    def /(other: RatioInt): RatioInt =
      RatioInt(this.a * other.b, this.b * other.a)
    def my_abs: RatioInt =
      if(this.a < 0) RatioInt(-this.a, this.b)
      else if(this.b <0) RatioInt(this.a, -this.b)
      else this

    // TODO 2.2
    def compare(other: RatioInt): Int =
      this.a * other.b - this.b * other.a
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    val nr_elem: Int = l.foldLeft(0)((acc, x) => acc + x._1)
    l.map(pair => (RatioInt(pair._1, nr_elem), pair._2))
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    val first: Bit = l.head._2
    val total_len: Int = l.foldLeft(0)((acc, x) => acc + x._1)
    val list:List[RatioInt] = l.map(pair => RatioInt(pair._1, total_len))
    (first, list)
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    def parity(c:Char):Parity =
      if(c == 'G') Even
      else  Odd
    s.map(parity).toList
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(_.toList).map(toParities)

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = leftOddList.map(runLength).map(scaledRunLength)
  val leftEvenSRL:  List[SRL] = leftEvenList.map(runLength).map(scaledRunLength)
  val rightSRL:  List[SRL] = rightList.map(runLength).map(scaledRunLength)

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = {
    def sum(acc: RatioInt, l3: List[RatioInt], l4: List[RatioInt]): RatioInt =
      (l3, l4) match {
        case (Nil, Nil) => acc
        case (_, _) => sum(acc + l3.head.-(l4.head).my_abs, l3.tail, l4.tail)

      }

     if( complement(l1._1) == l2._1)
       RatioInt(100, 1)
     else
       sum(RatioInt(0, 1), l1._2, l2._2)
  }
  def toDigit(nr: Int):Digit = {
    val digit:Digit = nr
    digit
  }
  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
    val listRat: List[RatioInt] = SRL_Codes.map { x => distance(digitCode, x) }
    val min:RatioInt = listRat.min
    val el: RatioInt = RatioInt(0,1)
    val index:Digit = toDigit(0)

    (min, listRat.indexOf(min))
  }
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = {
    val even: (RatioInt, Digit) = bestMatch(leftEvenSRL, digitCode)
    val odd:(RatioInt, Digit) = bestMatch(leftOddSRL, digitCode)
    if(even._1 <= odd._1)
      (Even, even._2)
    else
      (Odd, odd._2)
  }
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = {
    val (ratio, bestDigit:Digit) = bestMatch(rightSRL, digitCode)
    (NoParity, bestDigit)
  }

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
    require(rle.length == 59)
      val list: List[(Int, Bit)] = rle.drop(3).dropRight(3)
      val left:List[(Int, Bit)] = list.take(24)
      val right: List[(Int, Bit)]  = list.takeRight(24)

      val listsLeft : List[List[(Int, Bit)]] = chunksOf(4)(left)
      val listsRight : List[List[(Int, Bit)]] = chunksOf(4)(right)

    val leftDigits = listsLeft.map(schunk => scaledRunLength(schunk)).map(chunk => bestLeft(chunk))
    val rightDigits = listsRight.map(schunk => scaledRunLength(schunk)).map(chunk => bestRight(chunk))
    val combinedResults = leftDigits ++ rightDigits
    combinedResults
  }

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
    val newList:List[Parity] = l.take(6).map(_._1)
    val matchingParity = leftParityList.indexOf(newList) match {
      case -1 => None
      case index => Some(index)
    }
    matchingParity.map(index => toDigit(index))
  }

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = {
    def SumProd(lst: List[Digit], index: Int): Digit = lst match {
      case Nil => 0
      case head :: tail =>
      val weight = if ((index + 1) % 2 == 0) toDigit(3) else toDigit(1)
      head * weight + SumProd(tail, index + 1)
}

    (10 - (SumProd(l, 0) % 10)) % 10
  }
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
    if (code.length == 13 && code(0)._2.equals(firstDigit(code.drop(1).take(6)).getOrElse(0))
      && code(12)._2 == checkDigit(code.take(12).map(_._2))) {

      val s: Option[String] = Some(code.map(_._2).foldLeft("")(_ + _))
      s}
    else None
  }

  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = {
    val last12Digits = findLast12Digits(rle)
    val firstParity = firstDigit(last12Digits)
    val fullCode = (NoParity, firstParity.getOrElse(0)) :: last12Digits
    val valid = verifyCode(fullCode)
    valid match {
      case Some(validCode) => Some(validCode)
      case None => None
    }
  }
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}


