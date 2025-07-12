package components

import scala.collection.mutable

class Inventory(slotCount: Int):
  /** Creates the slots for the inventory, filled with None. */
  private var slots: Vector[Option[Item]] = Vector.fill(slotCount)(None)

  /** Moves all non-empty slots to the top of the inventory slots. */
  private def definedToTop() =
    val itemsDefined = slots.filter(_.isDefined)
    val unDefined = slots.filter(_.isEmpty)
    slots = itemsDefined ++ unDefined


  def clean() =
    slots.foreach(i =>
      if i.forall(_.shouldRemove) then
        remove(i)
    )

  /** Removes an item from the inventory. */
  def remove(item: Option[Item]): Option[Item] =
    item match
      case Some(foundItem) if slots.contains(item) =>
        slots = slots.updated(slots.indexOf(Some(foundItem)), None)
        definedToTop()
        Some(foundItem)
      case _ => None

  def removeAll(): Vector[Option[Item]] =
    val removed: mutable.Buffer[Option[Item]] = mutable.Buffer() 
    slots.foreach(removed += remove(_))
    removed.toVector
  
  /** Adds an item to the inventory. Returns false if there are no slots to fill. */
  def add(item: Option[Item]): Boolean =
    if slots.contains(None) then
      slots = slots.updated(slots.indexOf(None), item)
      definedToTop()
      true
    else
      false

  /** Swaps two items in their slots between inventories. */
  def swap(other: Inventory, index: Int, otherIndex: Int) =
    if !(other == this && index == otherIndex) then
      val otherThing = other.remove(other.items(otherIndex))
      val thisThing = this.remove(this.items(index))

      if otherIndex > index then
        this.add(otherThing)
        other.add(thisThing)
      else
        other.add(thisThing)
        this.add(otherThing)

  //Return items in inventory wrapped in Some

  def items: Vector[Option[Item]] =
    slots

  def equippedWeapon: Option[Weapon] =
    items.foreach {
      //if an equipped weapon is found
      case Some(w: Weapon) if w.equipped =>
        return Some(w)
      case _ =>
    }
    None
    
  def equippedMedkit: Option[Medkit] =
    items.foreach {
      //if an equipped weapon is found
      case Some(m: Medkit) if m.equipped =>
        return Some(m)
      case _ =>
    }
    None

  def equippedArmors: Vector[Armor] =
    armors.filter(_.isEquipped)


  // List all items of a type in inventory

  def armors: Vector[Armor] =
    val slotted = items.flatten
    var armorsInSlots: Vector[Armor] = Vector()

    slotted.foreach {
      case armor: Armor =>
        armorsInSlots = armorsInSlots.appended(armor)
      case _ =>
    }
    armorsInSlots

  def weapons: Vector[Weapon] =
    val slotted = items.flatten
    var weaponInSlots: Vector[Weapon] = Vector()

    slotted.foreach {
      case weapon: Weapon =>
        weaponInSlots = weaponInSlots.appended(weapon)
      case _ =>
    }
    weaponInSlots

  def medkits: Vector[Medkit] =
    val slotted = items.flatten
    var medInSlots: Vector[Medkit] = Vector()

    slotted.foreach {
      case med: Medkit =>
        medInSlots = medInSlots.appended(med)
      case _ =>
    }
    medInSlots
    
  def consumables: Vector[Consumable] =
    val slotted = items.flatten
    var medInSlots: Vector[Consumable] = Vector()

    slotted.foreach {
      case med: Consumable =>
        medInSlots = medInSlots.appended(med)
      case _ =>
    }
    medInSlots

  // When someone is killed, their inventory is lootified
  def toLoot: Vector[Item] =
    weapons.foreach(w=>
      w.spend(w.durability
      .getOrElse(0)/rules.lootDurabilityCost)
      w.unequip())
    weapons
  def empty: Boolean =
    !items.exists(_.nonEmpty)

  // Equip methods

  def equipWeapon(weapon: Weapon, toggle: Boolean) =
    equippedWeapon.foreach(w=> if w!=weapon then w.unequip())
    if toggle then weapon.toggleEquip()
    else weapon.equip()

  def equipMedkit(medkit: Medkit, toggle: Boolean) =
    equippedMedkit.foreach(w=> if w!=medkit then w.unequip())
    if toggle then medkit.toggleEquip()
    else medkit.equip()

  def equipArmor(armor: Armor, toggle: Boolean) =
    //unequips any armor piece that fits on the same part of the body
    equippedArmors.filter(a => a.bodyPart == armor.bodyPart && a!=armor).foreach(_.unequip())
    if toggle then armor.toggleEquip()
    else armor.equip()

  //Returns information that are useful in other classes

  def listItems: Vector[String] =
    items.map {
      case Some(i) => i.toString
      case None => "empty"
    }

  override def toString =
    items.map {
      case Some(i) => i.describe
      case None => "empty"
    }.mkString(", ")

  def copyMe: Inventory =
    val newInv = Inventory(this.slotCount)
    slots.foreach {
      case Some(i)=> newInv.add(Some(i.copyMe))
      case _ =>
    }
    newInv

end Inventory