package components

trait Tile(val photoFile: String, val name: String):
  def giveName: String = name
end Tile

class Occupiable(file: String,
                 name: String,
                 var occupant: Option[Units],
                 val atk: Int,
                 val dodge:Int,
                 val protection: Int,
                 val hpEffect: Int,
                 val reduction: Map[String, Int]
                ) extends Tile(file, name):
  def moveReduction(classMovementType: Class) = 0
  def effects: Map[String, Int] = Map("atk" -> 1)
  def occupied: Boolean = occupant.nonEmpty
  def addOccupant(newUnit: Units) = occupant = Some(newUnit)
  def removeOccupant() = occupant = None
  def occupantOnTile = occupant
end Occupiable

class Unoccupiable(file: String, name: String, val canFlyOver: Boolean) extends Tile(file, name):
end Unoccupiable

class Shop(file: String, name: String) extends Unoccupiable(file, name, true):
  val selection = Vector[(Item, String, Double)]()
  def sell(product: Item) = selection.head.head
end Shop