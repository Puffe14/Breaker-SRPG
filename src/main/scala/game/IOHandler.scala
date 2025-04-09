package game
import components.*

object IOHandler:
  //Read the classes (game term) from files and
  def buildClasses() =
    val classData = ClassHandler.getData
    DataLibrary.setClasses(ClassHandler.create)

  def buildItems() =
    val itemData = ItemHandler.getData
    DataLibrary.setItems(ItemHandler.create)

  def buildInventory() =
    val itemData = ItemHandler.getInventoryData
    DataLibrary.setInventories(ItemHandler.createInventory)

  def buildTiles() =
    val tileData = TileHandler.getData
    DataLibrary.setTiles(TileHandler.create)

  // POST CLASS BUILDING
  //Read the classes (game term) from files and
  def buildCharacters() =
    val characterData = CharacterHandler.getData
    DataLibrary.setCharacters(CharacterHandler.create)

  // Needs Group, Condition, Grid, Units
  def buildFieldMaps() =
    val mapData = MapHandler.getData
    DataLibrary.setMaps(MapHandler.create)

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