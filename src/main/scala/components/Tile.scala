package components
import game.DataLibrary
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or
import scalafx.scene.image.Image
import ujson.Value.Value
import upickle.core.LinkedHashMap

import java.io.FileInputStream

trait Tile(val photoFile: String, val name: String):
  var position: (Int, Int, Int) = (0, 0, 0)
  def giveName: String = name
  def pos = position
  def moveReduction(classMovementType: Vector[String]): Double = 1
  def setPos(x: Int, y: Int, z: Int) =
    position = (x, y, z)
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



object MapHandler:
  var lastData = getData
  def getData = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/maps.json")))
  def create: Map[String, FieldMap] =
    lastData = getData
    val maps = lastData.obj
    val mapnames = maps.keys
    val nameToMap = for name <- mapnames yield
      val fmap = maps(name)

      //Create grid
      val gridMap = fmap("grid").obj
       // Get the tiles
      val tiles = read[Vector[String]](gridMap("tiles")).map(DataLibrary.tiles(_).copy)
      val grid = Grid(tiles,
                      read[Int](gridMap("row")),
                      read[Int](gridMap("column")),
                      read[Vector[Int]](gridMap("elevation"))
                 )
      grid.givePositionToTiles()

      val unitsInfo = read[Vector[(String,Int,(Int,Int))]](fmap("units"))

      def makeGroup(memberNames: Vector[String], team: Team): Group =
        Group(memberNames.map(makeUnit(_)), Behaviour.Agressive, team, false)
      def makeUnit(unitName: String): Units =
        val unit = Units(DataLibrary.characters(unitName).copyMe,         // Find the character
                         DataLibrary.inventories("inventory_"+unitName))  // Find the inventory
                                    .copyMe
        unitsInfo.find((a,b,c)=> a == unitName) // find out if they have a set place on the map
                 .foreach((a,b,c) =>
                    grid.addUnitAt(unit, c) // Place the character on the map
                    unit.takeDamage(b)      // Harm them enough
                    unit.equipFirst()       // Equip the weapon on their first slot
        )
        unit

      //Create enemy units
      val enemyList = read[Vector[Vector[String]]](fmap("enemies"))
      val enemies = enemyList.map(makeGroup(_, Team.Enemy))
      //Create ally units
      val allyList = read[Vector[Vector[String]]](fmap("allies"))
      val allies = allyList.map(makeGroup(_, Team.Ally))
      //Create joining player characters
      val joiningList = read[Vector[String]](fmap("joining"))
      val joining = joiningList.map(makeUnit(_))
      // Conditions
      val winCondition = readCondition(fmap("clear").obj)
      val loseConditions = Vector()
      // Events
      val mapEvents = fmap("events").obj
      val events = mapEvents.flatMap(n=>
        val currentMap = n._2.obj
        readEvent(currentMap)
      ).toVector
      // dummy player
      val dummyplayer = Organization(Vector(),Vector(),Inventory(0),Team.Player)
      val rotation = read[Int](fmap("rotation"))
      val turn = read[Int](fmap("turnNumber"))
      val deployment = read[Vector[(Int,Int)]](fmap("deploy"))
      // Finally create the FieldMap itself
      name -> FieldMap(enemies,
                       allies,
                       grid,
                       dummyplayer,
                       winCondition,
                       loseConditions,
                       rotation,
                       turn,
                       deployment,
                       joining,
                       events
              )
    nameToMap.toMap


  def readCondition(map: LinkedHashMap[String, Value]): Condition =
    read[String](map("title")) match
      case "survive" => Survive(read[Int](map("limit")))
      case "kill" => Kill(read[Vector[String]](map("target")))
      case _ => Route(Team.Enemy)

  def readEvent(map: LinkedHashMap[String, Value]): Option[Event] =
    read[String](map("title")) match
      case "reinforcement" =>
        val team = read[String](map("team")) match
          case "Player" => Team.Player
          case "Enemy" => Team.Enemy
          case "Ally" => Team.Ally
        val unitsCoords: Vector[(String,(Int,Int))] = read[Vector[(String,(Int,Int))]](map("units"))
        val bunch = unitsCoords.map((u,c) =>(
          Units(DataLibrary.characters(u),                // Find the character
                DataLibrary.inventories("inventory_"+u))  // Find the inventory
                .copyMe,
          c)
        )
        val turns = read[Vector[Int]](map("turns"))
        Some(Reinforcement(bunch, team, turns))
      case _ => None