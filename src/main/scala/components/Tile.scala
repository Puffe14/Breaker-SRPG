package components
import game.DataLibrary
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or

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
  val avoid:Int = 0
  val physical: Int = 0
  val magical: Int = 0
  val hpEffect: Int = 0
  val reduction: Map[String, Int] = Map()

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
  def removeOccupant(): Option[Units] =
    val tempO = occupant
    occupant = None
    occupant
  def occupantOnTile = occupant
  def statsVector =
    Vector(
      ("AT",atk),
      ("AV",avoid),
      ("PD",physical),
      ("MD",magical),
      ("Heal",hpEffect)
    )
  def statsMap =
    Map(
      ("AT"->atk),
      ("AV"->avoid),
      ("PD"->physical),
      ("MD"->magical)
    )
end Occupiable

class Unoccupiable(file: String, name: String, val canFlyOver: Boolean) extends Tile(file, name):
end Unoccupiable

class OccupiableFile(file: String, name: String, effect: Map[String,Int], reduce: Map[String,Int]) extends Occupiable(file, name):
  override val reduction = reduce
  override val atk = effect.getOrElse("atk",0)
  override val avoid = effect.getOrElse("avoid",0)
  override val physical = effect.getOrElse("physical",0)
  override val magical = effect.getOrElse("magical",0)
  override val hpEffect = effect.getOrElse("hpEffect",0)
end OccupiableFile

class Shop(file: String, name: String) extends Unoccupiable(file, name, true):
  val selection = Vector[(Item, String, Double)]()
  def sell(product: Item) = selection.head.head
end Shop


object TileHandler:
  var tileData = getData
  def getData = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/tiles.json")))

  def create: Map[String, Tile] =
    tileData = getData
    val dt = tileData.obj
    val dtu = dt("unoccupiables").obj
    var tileFilenames = dtu.keys
    val nameToUn = for file <- tileFilenames yield
      val tile = dtu(file)
      file -> Unoccupiable(read[String](tile("photo")),
                           read[String](tile("name")),
                           read[Boolean](tile("flyOver"))
              )
    val dto = dt("occupiables").obj
    tileFilenames = dto.keys
    val nameToOc = for file <- tileFilenames yield
      val tile = dto(file)
      file -> OccupiableFile(read[String](tile("photo")),
                             read[String](tile("name")),
                             read[Map[String,Int]](tile("effect")),
                             read[Map[String,Int]](tile("reduction"))
              )
    (nameToUn++nameToOc).toMap