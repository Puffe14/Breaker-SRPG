package components

class Inventory(slotCount: Int):
  //Creates the slots for the inventory, filled with None.
  private var slots: Vector[Option[Item]] = Vector.fill(slotCount)(None)

  //Moves all non-empty slots to the top of the inventory slots.
  private def definedToTop() =
    val itemsDefined = slots.filter(_.isDefined)
    val unDefined = slots.filter(_.isEmpty)
    slots = itemsDefined ++ unDefined

  //Removes an item from the inventory.
  def remove(item: Option[Item]): Option[Item] =
    item match
      case Some(foundItem) if slots.contains(item) =>
        slots = slots.updated(slots.indexOf(Some(foundItem)), None)
        Some(foundItem)
      case _ => None

  //Adds an item to the inventory. Returns false if there are no slots to fill.
  def add(item: Option[Item]): Boolean =
    if slots.contains(None) then
      slots = slots.updated(slots.indexOf(None), item)
      definedToTop()
      true
    else
      false

  //Swaps two items in their slots between inventories.
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

end Inventory