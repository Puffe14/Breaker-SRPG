package components

trait Tile(val photoFile: String, val name: String):
  var position: (Int, Int, Int) = (0, 0, 0)
  def giveName: String = name
  def pos = position
  def moveReduction(classMovementType: Vector[String]): Double = 1
  def setPos(x: Int, y: Int, z: Int) =
    position = (x, y, z)
end Tile

class Occupiable(file: String,
                 name: String
                ) extends Tile(file, name):
  var occupant: Option[Units] = None
  val atk: Int = 0
  val dodge:Int = 0
  val protection: Int = 0
  val hpEffect: Int = 0
  var reduction: Map[String, Int] = Map()

  //On tiles that one can't fly over, move reduction is determined by class type
  override def moveReduction(classMovementType: Vector[String]): Double =
    //looks for class type based penalty
    classMovementType.foreach(n=>
      return reduction.getOrElse(n, 1).toDouble
    )
    //if no penalty is found
    1

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