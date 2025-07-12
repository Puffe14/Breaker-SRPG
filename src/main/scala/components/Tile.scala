package components
import game.{DataLibrary, ReadingHandler}
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or
import scalafx.scene.image.Image
import ujson.Value.Value
import upickle.core.LinkedHashMap

import java.io.FileInputStream

trait Tile(val photoFile: String, val name: String):
  var position: (Int, Int, Int) = (0, 0, 0)
  var interactible: Option[Interactible] = None
  def giveName: String = name
  def pos = position
  def moveReduction(classMovementType: Vector[String]): Double = 1
  def setPos(x: Int, y: Int, z: Int) =
    position = (x, y, z)
  def containsLoot: Boolean = interactible.nonEmpty && interactible.forall(_.loot.nonEmpty)
  def containsSoul: Boolean = interactible.nonEmpty && interactible.forall(_.soulLeft)
  def spendSoul() = interactible.foreach(_.consumeSoul())
  def copy: Tile
  val photo: Image =
    new Image(new FileInputStream("src/main/scala/resources/images/" + photoFile + ".png"))
  val bottom: Image =
    new Image(new FileInputStream("src/main/scala/resources/images/" + "field_base" + ".png"))
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

  def occupied: Boolean = occupant.nonEmpty
  def addOccupant(newUnit: Units) =
    if occupied then //fail if already occupied
      throw Exception(s"Adding Occupant $newUnit failed. Tile already has $occupant")
    else
      occupant = Some(newUnit)
  def removeOccupant(): Option[Units] =
    val tempO = occupant
    occupant = None
    occupant
  def addCorpse(loot: Option[Inventory], hasSoul: Boolean = true) =
    interactible = Some(Interactible(loot, hasSoul))
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
  def copy: Tile = Occupiable(photoFile, name)
end Occupiable

class Unoccupiable(file: String, name: String, val canFlyOver: Boolean) extends Tile(file, name):
  override def copy: Tile = Unoccupiable(file, name, canFlyOver)
end Unoccupiable

class OccupiableFile(file: String, name: String, effect: Map[String,Int], reduce: Map[String,Int]) extends Occupiable(file, name):
  override val reduction = reduce
  override val atk = effect.getOrElse("atk",0)
  override val avoid = effect.getOrElse("avoid",0)
  override val physical = effect.getOrElse("physical",0)
  override val magical = effect.getOrElse("magical",0)
  override val hpEffect = effect.getOrElse("hpEffect",0)
  override def copy: Tile = OccupiableFile(file, name, effect, reduce)
end OccupiableFile

class Shop(file: String, name: String) extends Unoccupiable(file, name, true):
  val selection = Vector[(Item, String, Double)]()
  def sell(product: Item) = selection.head.head
end Shop


class Interactible(var loot: Option[Inventory], hasSoul: Boolean = true):
  private var soul = hasSoul
  def noLoot: Boolean =
    loot.nonEmpty || loot.forall(_.empty)
  def soulLeft = soul
  def setLoot(newLoot: Option[Inventory]) =
    loot = newLoot
  /** A mystic consumes the soul to recover spell uses. */
  def consumeSoul(): Unit =
    soul = false


object TileHandler extends ReadingHandler:
  val fileName = "tiles.json"

  def create: Map[String, Tile] =
    val tileData = getData
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