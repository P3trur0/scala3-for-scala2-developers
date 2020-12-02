/**
 * ENUMS
 * 
 * Scala 3 adds support for "enums", which are to sealed traits like case classes 
 * were to classes. That is, enums cut down on the boilerplate required to use 
 * the "sealed trait" pattern for modeling so-called sum types, in a fashion very 
 * similar to how case classes cut down on the boilerplate required to use 
 * classes to model so-called product types.
 * 
 * Strictly speaking, Scala 3 enums are not the same as Java enums: while the 
 * constructors of enums are finite, and defined statically at compile-time in the 
 * same file, these constructors may have parameters, and therefore, the total 
 * number of values of any enum type could be large or infinite.
 * 
 * Enums and case classes provide first-class support for "algebraic data types" 
 * in Scala 3.
 */
package enums:

  enum FavoriteIDE:
    case VSCode(version: Int)
    case Vim
    case IDEA(majorVersion: Int, minorVersion: Int)

  val vsCode = FavoriteIDE.VSCode(1)
  val idea = FavoriteIDE.IDEA(1, 2)

  def example =
    vsCode match
      case FavoriteIDE.VSCode(v) => println(s"code $v")
      case FavoriteIDE.IDEA(v, _) => println(s"code $v")
      case FavoriteIDE.Vim => println(s"code")
    
  /**
   * EXERCISE 1
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Enums do for sum types what case classes do for product types
   */
  enum DayOfWeek:  // this is the enum type
    case Sunday   //each of this is a constructor that allows to create a specific enum type
    case Monday
    case Tuesday
    case Wednesday 
    case Thursday 
    case Friday
    case Saturday

  /**
   * EXERCISE 2
   * 
   * Explore interop with Java enums by finding all values of `DayOfWeek`, and by 
   * finding the value corresponding to the string "Sunday".
   */
  def daysOfWeek: Array[DayOfWeek] = DayOfWeek.values
  def sunday: DayOfWeek = DayOfWeek.valueOf("Sunday")

  /**
   * EXERCISE 3
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type of any of the case constructors!
   */
  enum Color:
    case Red
    case Green
    case Blue
    case Custom(red: Int, green: Int, blue: Int)

  val custom: Color = Color.Custom(1, 2, 3)

  /**
   * EXERCISE 4
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Result[+Error, +Value]:
    case Succeed(value: Value)
    case Fail(error: Error)

  /**
   * EXERCISE 5
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Workflow[-Input, +Output]:
    case End(value: Output)

  /**
   * EXERCISE 6
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * This is a generalized ADT
   */
  enum Conversion[-From, +To]:
    case AnyToString extends Conversion[Any, String]
    case StringToInt extends Conversion[String, Option[Int]]

/**
 * CASE CLASSES
 * 
 * Scala 3 makes a number of improvements to case classes.
 */
package case_classes:
  /**
   * EXERCISE 1
   * 
   * By making the public constructor private, make a smart constructor for `Email` so that only 
   * valid emails may be created.
   */
  final case class Email private (value: String)
  object Email:
    def fromString(v: String): Option[Email] = if isValidEmail(v) then Some(Email(v)) else None

    def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")



  /**
   * EXERCISE 2
   * 
   * Try to make a copy of an existing `Email` using `Email#copy` and note what happens.
   * 
   */
  def changeEmail(email: Email): Email = ??? // email.copy(value = "pippo")

  /**
   * EXERCISE 3
   * 
   * Try to create an Email directly by using the generated constructor in the companion object.
   * 
   */
  def caseClassApply(value: String): Email = ??? // Email(value)

/**
 * PATTERN MATCHING
 * 
 * Scala 3 provides upgrades to the power and flexibility of pattern matching.
 */  
object pattern_matching:
  /**
   */
  def foo: Int = 2