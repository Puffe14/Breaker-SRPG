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
  def moveReduction(classMovementType: Class) = ""
  def effects: Map[String, Int] = Map("atk" -> 1)
  def occupied: Boolean = occupant.nonEmpty
  def addOccupant(newUnit: Units) = occupant = Some(newUnit)
  def removeOccupant() = occupant = None
  def occupantOnTile = occupant
end Occupiable