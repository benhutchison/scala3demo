package scala3demo

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

import scala.annotation.alpha

//for using Scala 2.12 Cats
import scala.language.implicitConversions

val TopLevel = """
  Top level definitions of vals, defs, implicits, types are allowed
  in Scala 3. Replaces Package Objects."""

//one I reached for is a combined Either[String, ?] and IO[?] monad
type IOErr[A] = EitherT[IO, String, A]



sealed abstract class OldColor(rbg: (Int, Int, Int)) 

case object VintageRed extends OldColor(122, 41, 24) 
case object AgedWood extends OldColor(150, 116, 77) 
case object OldbottleGreen extends OldColor(25, 66, 35) 

//lets look at modelling a typesafe color with Opaque types


object RGBColor {
  //an opaque type has the exact same underlying representation, but forgets all the underlying methods
  //but instead gains the methods defined in its scope (!Not Compainion object!)
  opaque type RGBColor = Int

  private val MaskLowByte = 255

  private def inclusiveRange(n: Int, label: String, max: Int, min: Int = 0) = 
    Validated.cond(n <= max && n >= min, n, s"$label outside valid range [$min, $max]: $n. ")

  /** Inputs assumed to be valid */
  private[scala3demo] def applyValid(r: Int, g: Int, b: Int): RGBColor = (r << 16) | (g << 8) | b

  def apply(r: Int, g: Int, b: Int): Either[String, RGBColor] = 
    //https://dotty.epfl.ch/docs/reference/other-new-features/parameter-untupling.html
    (
      inclusiveRange(r, "Red", 255),
      inclusiveRange(g, "Green", 255),
      inclusiveRange(b, "Blue", 255),
    ).mapN(applyValid).toEither

  def (c: RGBColor) toString = s"RGBColor($red, $green, $blue)"

  def (c: RGBColor) red: Int = c >> 16 & MaskLowByte
  def (c: RGBColor) green: Int = c >> 8 & MaskLowByte
  def (c: RGBColor) blue: Int = c & MaskLowByte

  def (c: RGBColor) redF: Double = c.red.toDouble / 255
  def (c: RGBColor) greenF: Double = c.green.toDouble / 255
  def (c: RGBColor) blueF: Double = c.blue.toDouble / 255

  given as Ordering[RGBColor] given (a: Ordering[Int]) = a

  @alpha("scale") def * (n: Double): RGBColor = ???

  def (c: RGBColor) toHSVColor: HSVColor.HSVColor = {
    val r: Double = c.redF
    val g = c.greenF
    val b = c.blueF

    val min = math.min(math.min(r, g), b)
    val max = math.max(math.max(r, g), b)
    val v = max
    val delta = max - min
    val s = if ( max != 0 )
      delta / max
    else 0.0
    val h1 = (if (max == 0.0 || delta == 0.0) {
      //if the color is black or any grey, it doesnt have a meaningful hue
      //so we use 0. Its saturation should be zero.
      require(s == 0.0, "saturation zero for a grey color")
      0
    } else {
      if(r == max)
        ( g - b ) / delta		// between yellow & magenta
      else if (g == max)
        2.0 + ( b - r ) / delta	// between cyan & yellow
      else
        4.0 + ( r - g ) / delta	// between magenta & cyan
    })/6.0
    val h = if(h1 < 0) h1 + 1 else h1

    HSVColor.applyValid(h, s, v)
  }

}

object HSVColor {

  opaque type HSVColor = IArray[Double]

  /** Inputs assumed to be valid */
  private[scala3demo] def applyValid(h: Double, s: Double, v: Double): HSVColor = IArray(h, s, v)

  def (c: HSVColor) hue: Double = c(0)
  def (c: HSVColor) saturation: Double = c(1)
  def (c: HSVColor) value: Double = c(2)

  def (c: HSVColor) asString = {
    f"HSVColor(${c.hue}%.4f, ${c.saturation}%.4f, ${c.value}%.4f)"
  }

}

import RGBColor._
import HSVColor._

val x = List(RGBColor(5, 5, 5).right.get, RGBColor(4, 4, 4).right.get ).sorted.map(_.toHSVColor).map(_.asString)


//extends java.lang.Enum 

enum NewColor(rbg: (Int, Int, Int)) {
  case NewDawnRed extends NewColor((122, 41, 24))
  case NewGrowth extends NewColor((150, 116, 77))
  case FreshLime extends NewColor((25, 66, 35))
}

//ADT
enum NewOption[+T] {
  case Some(x: T)
  case None
}

//GADT (generalized ADT)
//characteristic: cases extend type params (ie T below) with different types

enum Tree[T] {
  case True extends Tree[Boolean]
  case False extends Tree[Boolean]
  case IsZero(n: Tree[Int]) extends Tree[Boolean]
  case Zero  extends Tree[Int]
  case Succ(n: Tree[Int]) extends Tree[Int]
  case If(cond: Tree[Boolean], thenp: Tree[T], elsep: Tree[T]) extends Tree[T]
}

//immutable arrays based on Opaque Types
//performance overhead claimed to be zero due to 100% inlining 
case class SHA256Hash private (bytes: IArray[Byte])

object SHA256Hash {

  def apply(password: String): SHA256Hash = {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    //digest provides fresh, private array, cast immutable before handing out
    SHA256Hash(md.digest(password.getBytes("UTF-8")).asInstanceOf[IArray[Byte]])
  }

}
import scala.util.matching._

//Extension Methods
//https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html
object Extensions {

  def (regex: Regex) matchesIn(s: String) = regex.pattern.matcher(s).find

  def (regex: Regex) matchesAll(s: String) = regex.pattern.matcher(s).matches


  def (b: Boolean) validated[L, R] (l: =>L, r: =>R): Validated[L, R] = 
    if (b) Validated.Invalid(l) else Validated.Valid(r)

}
import Extensions._

case class Username private (name: String)
object Username {

  val ValidUsernameRegex = "[a-zA-Z0-9].{1,40}".r

  def validate(s: String): Validated[String, Username] = {
    ValidUsernameRegex.matchesAll(s).validated(
      s"Username '$s' doesn't confirm to regex $ValidUsernameRegex. ",
      Username(s))
  }
}

case class Email private (emailString: String)

object Email {
  val MaxEmail = 254
  val SimpleEmailRegex = """[^@\s]+@[^@\s]+\.[^@\s]+""".r

  //NOTE demonstrate error that results from using `|+|` instead of `combine`
  def validate(s: String): Validated[String, Email] = 
    Validated.cond(s.length < MaxEmail, s, s"Email '$s' exceeds $MaxEmail characters. ").combine(
      Validated.cond(SimpleEmailRegex.matchesAll(s), s, s"Email '$s' doesn't confirm to regex $SimpleEmailRegex. ")
    ).map(Email(_))
}

case class User(name: Username, email: Email)

def forgotPassword(id: Username | Email): Either[String, User] = id match {
  case username: Username => lookupUser(username)
  case email: Email => lookupUserByEmail(email)
}

def lookupUser(u: Username): Either[String, User] = ???

def lookupUserByEmail(email: Email): Either[String, User] = ???
