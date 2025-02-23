package components

trait Item {
  val name: String
  val description: String
  def describe = name +": "+this.description
}

trait Consumable(val effectToStats: Map[String, Int], var uses: Int, val limit: Int) extends Item:
  //Item effect is handled differently based on item type.
  def effects = effectToStats
  def use() =
    uses += 1
  def isEmpty: Boolean =
    uses >= limit

end Consumable


//Provides HP up to max
trait Healing(e: Map[String, Int], u: Int, l: Int, var amount: Int) extends Consumable:
  //override def use() =
    //uses += 1
  def heal = amount

//Gives a temporary boost on stat(s)
trait Booster(e: Map[String, Int], u: Int, l: Int) extends Consumable

//Gives a permanent increase to a stat
trait Brand(e: Map[String, Int], u: Int, l: Int) extends Consumable


trait Equipment extends Item:
  var equipped = false

  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int]

  def bonusGiven = bonusToStats

  //Method fo all equipment that returns wheter they are broken or not.
  def intact: Boolean

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

  //True if the weapon is broken.
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

end Weapon

trait Sharp extends Weapon
trait Blunt extends Weapon
trait Long extends Weapon
trait Ranged extends Weapon
trait Spell extends Weapon



trait Armor extends Equipment:

  private var broken = false

  override def intact =
    broken

  def break() =
    broken = true
    equipped = false

end Armor

trait Helmet extends Weapon
trait Body extends Weapon
trait Arms extends Weapon
trait Legs extends Weapon