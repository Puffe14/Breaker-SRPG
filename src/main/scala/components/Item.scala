package components
import game.DataLibrary
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or
import ujson.Value.Value
import upickle.core.LinkedHashMap


trait Item {
  val name: String
  val description: String
  def describe = name +": "+this.description
  def shouldRemove = false
  def copyMe: Item = this
}

trait Consumable(val effectToStats: Map[String, Int], var uses: Int, val limit: Int) extends Item:
  //Item effect is handled differently based on item type.
  def effects = effectToStats
  def utilize(unit: Units) = use()
  def use() =
    uses += 1
  def setUses(u: Int) =
    uses = u
  def isEmpty: Boolean =
    uses >= limit
  override def shouldRemove = isEmpty
  override def toString: String =
    s"$name (${limit-uses}/$limit)"
end Consumable

//!!! fix class types so no name = "" or description = ""

//Provides HP up to max
case class Healing(itemName: String, itemDescription: String, e: Map[String, Int], u: Int, l: Int, var amount: Int) extends Consumable(e,u,l):
  val name = itemName
  val description = itemDescription
  def heal = amount
  override def utilize(unit: Units) =
    unit.healDamage(heal)
    use()
  override def copyMe: Item = this.copy(itemName = itemName)

//Gives a temporary boost on stat(s)
case class Booster(itemName: String, itemDescription: String, e: Map[String, Int], u: Int, l: Int) extends Consumable(e,u,l):
  val name = itemName
  val description = itemDescription
  override def utilize(unit: Units) =
    effects.foreach(n => unit.addTemporaryStat(n(0), n(1)))
    use()
  override def copyMe: Item = this.copy(itemName = itemName)

//Gives a permanent increase to a stat
case class Brand(itemName: String, itemDescription: String, e: Map[String, Int], u: Int, l: Int) extends Consumable(e,u,l):
  val name = itemName
  val description = itemDescription
  override def utilize(unit: Units) =
    effects.foreach(n => unit.addPermanent(n(0), n(1)))
    use()
  override def copyMe: Item = this.copy(itemName = itemName)


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
  val wpnType: String
  val dmgType: String
  val rankLetter: String

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
  def dmgtyping: String = dmgType
  def wpntyping: String = wpnType
  def rank: Int =
    rankLetter match
      case "E" => 1
      case "D" => 2
      case "C" => 3
      case "B" => 4
      case "A" => 5

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

trait Sharp extends Weapon:
  override val wpnType: String = "Sharp"
trait Blunt extends Weapon:
  override val wpnType: String = "Blunt"
trait Long extends Weapon:
  override val wpnType: String = "Long"
trait Ranged extends Weapon:
  override val wpnType: String = "Ranged"
trait Spell extends Weapon:
  override val wpnType: String = "Spell"
trait Medkit(amount: Int) extends Equipment:
  def heal = amount
  def intact = true
  val range = (0, 1)
  var spent: Int
  val durability: Option[Int]

  //Cause the medkit to lose durability by increasing the amount spent.
  def spend(durabilityLoss: Int) =
    spent += durabilityLoss
  override def toString: String =
    val equipState = if equipped then "* " else ""
    durability match
      case Some(maxDurability) =>
        equipState+s"$name (${maxDurability-spent}/$maxDurability)"
      case None =>
        s"$name"
        
case class MedkitFile(map: LinkedHashMap[String, Value]) extends Medkit(read[Int](map("heal"))):
  val bonusToStats = read[Map[String, Int]](map("bonus"))
  val description = read[String](map("description"))
  val name = read[String](map("name"))
  val durability = read[Option[Int]](map.get("durability"))
  var spent = read[Int](map("spent"))
  override val range: (Int, Int) = read[(Int,Int)](map("range"))
  override def copyMe: Item = this.copy(map = map)
end MedkitFile



trait Armor extends Equipment:
  val part: Part
  //case Helmet(""), Body, Arms, Legs
  def partName = part.toString
  def bodyPart = part

  private var broken = false

  override def intact =
    !broken

  def break() =
    broken = true
    equipped = false

  override def toString =
    val equipState = if equipped then "* " else ""
    s"$equipState$name"

end Armor


case class ArmorFile(map: LinkedHashMap[String, Value]) extends Armor:
  val givenPart = read[String](map("part"))
  
  val name = read[String](map("name"))
  val part = rules.allParts.find(_.toString==givenPart).getOrElse(Part.AnyPart)
  val description = read[String](map("description"))
  val bonusToStats = read[Map[String, Int]](map("bonus"))
  override def copyMe: Item = this.copy(map = map)


case class WeaponFile(filename: String, used: Int = 0) extends Weapon:
  val wdata = ItemHandler.getWeaponsData(filename)
  val name = read[String](wdata("name"))
  val description = read[String](wdata("description"))
  val durability: Option[Int] = Some(read[Int](wdata("durability")))
  val rankLetter: String = read[String](wdata("rank"))
  var spent: Int = if used == 0 then read[Int](wdata("spent")) else used
  val quick: Boolean = read[Boolean](wdata("quick"))
  val dmgType: String = wdata("dmgtype").str
  val wpnType: String = wdata("wpntype").str
  val givenPower: Int = read[Int](wdata("power"))
  val givenHit: Int = read[Int](wdata("hit"))
  val givenCrit: Int = read[Int](wdata("crit"))
  val givenRange: (Int, Int) = read[(Int,Int)](wdata("range"))
  val givenWeight: Int = read[Int](wdata("weight"))
  val effectiveAgainst: Map[String, Int] = read[Map[String, Int]](wdata("effective"))
  val bonusToStats: Map[String, Int] = read[Map[String, Int]](wdata("bonus"))
  override def copyMe: Item = this.copy(used = spent)
end WeaponFile

/**/

object ItemHandler:
  def getData(weaponType: String) =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/weapons.json")))
  def getItem(filename: String) =
    val data = getData("")
    data(filename)

  def getConsumables =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/consumables.json")))
  var lastData = getConsumables
  var dataType = "healings"
  def getConsumablesData =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/consumables.json")))
  def getWeaponsData =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/weapons.json")))
  def getMedkitsData =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/medkits.json")))
  def getArmorsData =
    ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/armors.json")))
  def getInventoryData =
     ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/inventories.json")))

  // return all the classes to be created
  def create: Map[String, Item] =
    // cnsm
    lastData = getConsumables
    var itemFilenames = lastData.obj.keys
    val nameToItem = for name <- itemFilenames yield
     name -> consumableRead(name)
    // wpns
    lastData = getWeaponsData
    itemFilenames = lastData.obj.keys
    val nameToWeapon = for name <- itemFilenames yield
     name -> weaponRead(name)
    (nameToItem++nameToWeapon).toMap
    // medkits
    lastData = getMedkitsData
    itemFilenames = lastData.obj.keys
    val nameToMedkit = for (name, info) <- lastData.obj.toSeq yield
     name -> MedkitFile(info.obj)
    // armors
    lastData = getArmorsData
    itemFilenames = lastData.obj.keys
    val nameToArmor = for (name, info) <- lastData.obj.toSeq yield
     name -> ArmorFile(info.obj)
    (nameToItem++nameToWeapon++nameToMedkit++nameToArmor).toMap


  def weaponRead(filename: String): Equipment =
    WeaponFile(filename)

  // return all the classes to be created
  def createInventory: Map[String, Inventory] =
    lastData = getInventoryData
    val invFilenames = lastData.obj.keys
    val nameToItem = for name <- invFilenames yield
     name -> inventoryRead(name)
    nameToItem.toMap

  // From the last getData, read the particular map and create an item based on it.
  def consumableRead(filename: String): Item =
    val dt = lastData(filename)
    // Create new instance of the class
    read[String](dt("typing")) match
      case "Healing" =>
        new Healing(read[String](dt("name")),
                read[String](dt("description")),
                read[Map[String,Int]](dt("effect")),
                read[Int](dt("uses")),
                read[Int](dt("limit")),
                read[Int](dt("amount"))
        )
      case "Brand" =>
        new Brand(read[String](dt("name")),
                  read[String](dt("description")),
                  read[Map[String,Int]](dt("effect")),
                  read[Int](dt("uses")),
                  read[Int](dt("limit"))
            )
      case "Booster" =>
        new Booster(read[String](dt("name")),
                  read[String](dt("description")),
                  read[Map[String,Int]](dt("effect")),
                  read[Int](dt("uses")),
                  read[Int](dt("limit"))
            )

  def inventoryRead(inventoryName: String): Inventory =
    val dt = lastData
    //Put it in the slot based on name and set spent as previous uses
    val inventoryMap = dt.obj
    val allItems = DataLibrary.items
    val itemsAndUses = read[Seq[(String, Int)]](inventoryMap(s"$inventoryName")) //"inventory_$unitName"
    val slotsNumber = if itemsAndUses.size < 6 then 6 else itemsAndUses.size
    val inventory = Inventory(slotsNumber)
    // Set the items from the inventory to the spent state and add them to it
    itemsAndUses.map((item,uses)=>
      val newItem = allItems(item).copyMe
      newItem match
        case w: Weapon => w.spend(uses)
        case m: Medkit => m.spend(uses)
        case c: Consumable => c.setUses(uses)
        case _ =>
      inventory.add(Some(newItem))
    )
    inventory