package game
import components.*
import os.{RelPath, read as or}
import ujson.Value

import java.io.FileNotFoundException

object IOHandler:
  //Read the classes (game term) from files and
  def buildClasses() =
    try
      val classData = ClassHandler.getData
      DataLibrary.setClasses(ClassHandler.create)
    catch
      case e: FileNotFoundException => println("no class file found")
      case _ => println("failed to build classes")

  def buildItems() =
    try
      val itemData = ItemHandler.getData
      DataLibrary.setItems(ItemHandler.create)
    catch
      case e: FileNotFoundException => println("an item file not found")
      case _ => println("failed to build items")

  def buildInventory() =
    try
      val itemData = ItemHandler.getInventoryData
      DataLibrary.setInventories(ItemHandler.createInventory)
    catch
      case e: FileNotFoundException => println("an inventory file not found")
      case _ => println("failed to build inventories")

  def buildTiles() =
    try
      val tileData = TileHandler.getData
      DataLibrary.setTiles(TileHandler.create)
    catch
      case e: FileNotFoundException => println("a tile file not found")
      case _ => println("failed to build tiles")


  // POST CLASS BUILDING

  //Read the classes (game term) from files and
  def buildCharacters() =
    try
      val characterData = CharacterHandler.getData
      DataLibrary.setCharacters(CharacterHandler.create)
    catch
      case e: FileNotFoundException => println("a tile file not found")
      case _ => println("failed to build tiles")

  // POST CHARACTER BUILDING

  // Reads classes like Group, Condition, Grid, Units to make FieldMap
  def buildFieldMaps() =
    try
      val mapData = MapHandler.getData
      DataLibrary.setMaps(MapHandler.create)
    catch
      case e: FileNotFoundException => println("a maps file not found")
      case _ => println("failed to build fieldmaps")


end IOHandler

object DataLibrary:
  //Combines ALL classes (game term) read from files into a massive MAP(name -> Class)
  var classes: Map[String, Class] = Map()
  def setClasses(classMap: Map[String, Class]) =
    classes = classMap

  var characters: Map[String, Character] = Map()
  def setCharacters(characterMap: Map[String, Character]) =
    characters = characterMap

  var items: Map[String, Item] = Map()
  def setItems(itemMap: Map[String, Item]) =
    items = itemMap

  var inventories: Map[String, Inventory] = Map()
  def setInventories(inventoryMap: Map[String, Inventory]) =
    inventories = inventoryMap

  var tiles: Map[String, Tile] = Map()
  def setTiles(tileMap: Map[String, Tile]) =
    tiles = tileMap

  var maps: Map[String, FieldMap] = Map()
  def setMaps(fmapMap: Map[String, FieldMap]) =
    maps = fmapMap

end DataLibrary

trait ReadingHandler:
  var lastData: Value.Value = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/tiles.json")))
  val fileName: String

  def getData =
    val result = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/$fileName")))
    lastData = result
    result
