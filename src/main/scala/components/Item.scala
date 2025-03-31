package components
import upickle.default.*
import os.*


trait Item {
  val name: String
  val description: String
  def describe = name +": "+this.description
  def shouldRemove = false
}

trait Consumable(val effectToStats: Map[String, Int], var uses: Int, val limit: Int) extends Item:
  //Item effect is handled differently based on item type.
  def effects = effectToStats
  def utilize(unit: Units) = use()
  def use() =
    uses += 1
  def isEmpty: Boolean =
    uses >= limit
  override def shouldRemove = isEmpty
  override def toString: String =
    s"$name (${limit-uses}/$limit)"
end Consumable

//!!! fix class types so no name = "" or description = ""

//Provides HP up to max
class Healing(e: Map[String, Int], u: Int, l: Int, var amount: Int) extends Consumable(e,u,l):
  val name = ""
  val description = ""
  def heal = amount
  override def utilize(unit: Units) =
    unit.healDamage(heal)
    use()

//Gives a temporary boost on stat(s)
class Booster(e: Map[String, Int], u: Int, l: Int) extends Consumable(e,u,l):
  val name = ""
  val description = ""
  override def utilize(unit: Units) =
    effects.foreach(n => unit.addTemporaryStat(n(0), n(1)))
    use()

//Gives a permanent increase to a stat
class Brand(e: Map[String, Int], u: Int, l: Int) extends Consumable(e,u,l):
  val name = ""
  val description = ""
  override def utilize(unit: Units) =
    effects.foreach(n => unit.addPermanent(n(0), n(1)))
    use()


trait Equipment extends Item:
  var equipped = false

  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int]

  def bonusGiven = bonusToStats

  //Method fo all equipment that returns wheter they are broken or not.
  def intact: Boolean
  override def shouldRemove = !intact

  //Handle being equipped
  def isEquipped: Boolean = equipped
  def equip() =
    equipped = true
  def unequip() =
    equipped = false
  def toggleEquip() =
    equipped = !equipped

end Equipment


trait Weapon extends Equipment:
//trait Weapon(filename: String) extends Equipment:

  //Info on weapon.
  val durability: Option[Int]
  var spent: Int
  val quick: Boolean
  val dmgType: String

  //Direct combat stats.
  val givenPower: Int
  val givenHit: Int
  val givenCrit: Int
  val givenRange: (Int, Int)
  val givenWeight: Int

  //Effective against these types
  val effectiveAgainst: Map[String, Int]

  //True if the weapon is not broken.
  override def intact: Boolean =
    durability match
      case Some(maxDurability) => maxDurability > spent
      case None => true

  //If true, the weapon always doubles (2x or 4x attacks)
  def isQuick =
    quick

  //These return weapon parameters
  def power: Int =  givenPower
  def hit: Int =    givenHit
  def crit: Int =   givenCrit
  def range: (Int, Int) =  givenRange
  def weight: Int = givenWeight
  def bonus: Map[String, Int] =     bonusToStats
  def effective: Map[String, Int] = effectiveAgainst
  def typing: String = dmgType

  //Cause the weapon to lose durability by increasing the amount spent.
  def spend(durabilityLoss: Int) =
    spent += durabilityLoss

  //Gives the item description with
  override def describe: String =
    durability match
      case Some(maxDurability) =>
        s"$name (${maxDurability-spent}/$maxDurability): $description"
      case None =>
        s"$name: $description"

  override def toString: String =
    val equipState = if equipped then "* " else ""
    durability match
      case Some(maxDurability) =>
        equipState+s"$name (${maxDurability-spent}/$maxDurability)"
      case None =>
        s"$name"

end Weapon

trait Sharp extends Weapon
trait Blunt extends Weapon
trait Long extends Weapon
trait Ranged extends Weapon
trait Spell extends Weapon
trait Medkit(amount: Int) extends Equipment:
  def heal = amount
  def intact = true
  val range = (0, 1)



trait Armor(part: Part) extends Equipment:

  //case Helmet(""), Body, Arms, Legs
  def partName = part.toString
  def bodyPart = part
  
  private var broken = false

  override def intact =
    broken

  def break() =
    broken = true
    equipped = false

  override def toString =
    val equipState = if equipped then "*" else ""
    s"$equipState $name"

end Armor


case class BluntFile(filename: String) extends Blunt derives ReadWriter:
  val wdata = ItemWeapon.getItem(filename)
  val name = wdata("name").str
  val description = wdata("description").str
  val durability: Option[Int] = wdata("durability").str.toIntOption
  val rank: String = wdata("rank").str
  var spent: Int = wdata("spent").str.toInt
  val quick: Boolean = wdata("quick").str == "true"
  val dmgType: String = wdata("type").str
  val givenPower: Int = wdata("power").str.toInt
  val givenHit: Int = wdata("hit").str.toInt
  val givenCrit: Int = wdata("crit").str.toInt
  val givenRange: (Int, Int) = (1, 1)
  val givenWeight: Int = wdata("weight").str.toInt
  val effectiveAgainst: Map[String, Int] = Map() //wdata("effective")
  val bonusToStats: Map[String, Int] = Map()
end BluntFile

/**/

object ItemWeapon:
  def getData(weaponType: String) =
    ujson.read(os.read(os.pwd / RelPath(s"src/main/scala/resources/data/blunts.json")))
  def getItem(filename: String) =
    val data = getData("")
    data(filename)