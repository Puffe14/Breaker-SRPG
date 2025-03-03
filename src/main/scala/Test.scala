import components.*
import game.*

class TestMace extends Blunt:
  val name = "test mace"
  val description = "it's the test mace"
  val dmgType = "force"
  //Info on weapon.
  val durability: Option[Int] = Some(12)
  var spent: Int = 0
  val quick: Boolean = false

  //Direct combat stats.
  val givenPower: Int = 1
  val givenHit: Int = 65
  val givenCrit: Int = 50
  val givenRange: (Int, Int) = (1, 1)
  val givenWeight: Int = 1

  //Effective against these types
  val effectiveAgainst: Map[String, Int] = Map()
  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int] = Map()
end TestMace

class TestSpell extends Spell:
  val name = "test spell"
  val description = "it's the test magic"
  val dmgType = "magic"
  //Info on weapon.
  val durability: Option[Int] = Some(10)
  var spent: Int = 0
  val quick: Boolean = false

  //Direct combat stats.
  val givenPower: Int = 1
  val givenHit: Int = 65
  val givenCrit: Int = 1
  val givenRange: (Int, Int) = (1, 2)
  val givenWeight: Int = 1

  //Effective against these types
  val effectiveAgainst: Map[String, Int] = Map("test" -> 2)
  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int] = Map()
end TestSpell

class TestSword extends Spell:
  val name = "test sword"
  val description = "it's the test sword"
  val dmgType = "force"
  //Info on weapon.
  val durability: Option[Int] = Some(10)
  var spent: Int = 0
  val quick: Boolean = true

  //Direct combat stats.
  val givenPower: Int = 1
  val givenHit: Int = 75
  val givenCrit: Int = 1
  val givenRange: (Int, Int) = (1, 1)
  val givenWeight: Int = 1

  //Effective against these types
  val effectiveAgainst: Map[String, Int] = Map()
  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int] = Map()
end TestSword


val testStatMap = Map("hitpoints" -> 5, "strength" -> 1, "magic" -> 1, "skill" -> 1, "speed" -> 1, "defence" -> 1, "resistance" -> 1, "movement" -> 3)
val testFastMap = Map("hitpoints" -> 24, "strength" -> 1, "magic" -> 1, "skill" ->10, "speed" ->12, "defence" -> 1, "resistance" -> 1, "movement" -> 1)
val testStrongMap = Map("hitpoints" -> 5, "strength" -> 5, "magic" -> 1, "skill" ->0, "speed" ->0, "defence" -> 5, "resistance" -> 1, "movement" -> 1)
val testSpiritMap = Map("hitpoints" -> 8, "strength" -> 1, "magic" -> 5, "skill" ->0, "speed" ->0, "defence" -> 0, "resistance" -> 5, "movement" -> 1)
val testZeroMap = Map("hitpoints" -> 0, "strength" -> 0, "magic" -> 0, "skill" -> 0, "speed" -> 0, "defence" -> 0, "resistance" -> 0, "movement" -> 0)
val testClass = new Class("test", 1, Vector("test"),testStatMap,testStatMap,testZeroMap,testZeroMap)

val cylna =     new Character("cylna", testClass, Vector(),testStrongMap,testStatMap)
val bonk =      new Character("bonk", testClass, Vector(),testStatMap,testStatMap)
val gonzales =  new Character("gonzales", testClass, Vector(),testStrongMap,testStrongMap)
val wrys   =    new Character("wrys", testClass, Vector(),testFastMap,testFastMap)
val ghost =     new Character("ghost", testClass, Vector(),testSpiritMap,testSpiritMap)

class testGrass(file: String, name: String) extends Occupiable(file, name):
end testGrass

class testSand(file: String, name: String) extends Occupiable(file, name):
  reduction = Map("test" -> 2)
end testSand

class testWall(file: String, name: String) extends Unoccupiable(file, name, false):
end testWall



class LogTest:
  var fight = Combat(Units(bonk), Units(bonk), 1)
  var field = FieldMap(Vector(), Vector(), new Grid(Vector(),0,0,Vector()),
                       new Organization(Vector(), Vector(), new Inventory(50)), "win")
  val unit1 = Units(cylna)
  val unit2 = Units(wrys)
  unit1.setTeam("player")
  unit2.setTeam("enemy")

  def setFight() =
    val itemi = TestMace()
    val itemi2 = TestSword()
    val invi = Inventory(6)
    val invi2 = Inventory(6)
    invi.add(Some(itemi))
    invi2.add(Some(itemi2))
    itemi.equip()
    itemi2.equip()
    unit1.inventory.swap(invi, 0, 0)
    unit2.inventory.swap(invi2, 0, 0)
    val fight12 = Combat(unit1, unit2, 1)
    fight = fight12
  end setFight

  def setGrid() =
    def gTile = testGrass("pöö","g")
    def wTile = testWall("wöö","w")
    def sTile = testSand("söö","s")

    val storage = Inventory(50)
    val grid =  Grid(
      Vector(gTile, gTile, wTile, gTile, sTile, gTile,
        gTile, gTile, gTile, gTile, gTile, gTile,
      gTile, gTile, gTile, gTile, gTile, gTile,
      gTile, gTile, gTile, gTile, gTile, gTile), 4, 6,
      Vector(0,0,1,1,0,2,0,0,1,1,0,2,0,0,1,1,0,2,0,1,1,0,2,1))

    grid.givePostitionToTiles()
    grid.occupiables.head.addOccupant(unit1)
    grid.occupiables(5).addOccupant(unit2)

    field = FieldMap(Vector(), Vector(), grid,
                     new Organization(Vector(), Vector(), storage), "win")
  end setGrid

  def log: Vector[String] =
    fight.play()

  def theField: FieldMap =
    field

  def moveAreaTiles =
    field.movementRangeTiles(unit1)


@main
def test =

  val itemi = TestMace()
  val itemi2 = TestMace()
  val itemi3 = TestSword()
  val itemi4 = TestSword()
  val itemi5 = TestMace()

  val invi = Inventory(6)
  val invi2 = Inventory(6)
  val invi3 = Inventory(6)
  val invi4 = Inventory(6)
  val invi5 = Inventory(6)

  val unit1 = Units(cylna)
  val unit2 = Units(gonzales)
  val unit3 = Units(wrys)
  val unit4 = Units(ghost)
  val unit5 = Units(bonk)

  def combatTest() =

    invi.add(Some(itemi))
    invi2.add(Some(itemi2))
    invi3.add(Some(itemi3))
    invi4.add(Some(itemi4))
    invi5.add(Some(itemi5))

    itemi.equip()
    itemi2.equip()
    itemi3.equip()
    itemi4.equip()
    itemi5.equip()

    unit1.inventory.swap(invi, 0, 0)
    unit2.inventory.swap(invi2, 0, 0)
    unit3.inventory.swap(invi3, 0, 0)
    unit4.inventory.swap(invi4, 0, 0)
    unit5.inventory.swap(invi5, 0, 0)

    val fight12 = Combat(unit1, unit2, 1)
    val fight13 = Combat(unit1, unit3, 1)
    val fight34 = Combat(unit3, unit4, 1)
    val fight43 = Combat(unit4, unit3, 2)
    val fight15 = Combat(unit1, unit5, 1)

    def testFight(fight: Combat) =
      println("")
      println("")
      println(fight.select)
      println("")
      println(fight.target)
      println("")
      fight.play()
      println("")
      println(fight.select.shortInfo)
      println("")
      println(fight.target.shortInfo)
      /*fight.select.weapon.foreach(n=>println(n.describe))
      fight.target.weapon.foreach(n=>println(n.describe))*/

    def testWound(fight: Combat) =
      println("")
      println("")
      println(fight.select)
      println("")
      println(fight.target)
      println("")
      fight.playWound("head")
      println("")
      println(fight.select.wounds)
      println("")
      println(fight.target.wounds)
    
    testWound(fight12)
    testFight(fight12)
    testFight(fight13)
    testFight(fight34)
    testFight(fight43)
    testFight(fight15)
  end combatTest


  def inventoryTest() =
    val itemi = TestMace()
    val itemi2 = TestMace()
    val itemi3 = TestMace()
    val itemi4 = TestMace()
    val invi = Inventory(6)
    val invi2 = Inventory(6)
    println(invi.items)
    invi.add(Some(itemi))
    invi.add(Some(itemi2))
    invi2.add(Some(itemi3))
    invi2.add(Some(itemi4))
    println(invi.items)
    invi.swap(invi,0,1)
    println(invi.items)
    println(invi2.items)
    invi2.swap(invi,0,1)
    invi2.swap(invi,0,2)
    println("1")
    println(invi.items)
    println("2")
    println(invi2.items)

  def gridTest() =
    val tile1 = testGrass("pöö","möö")
    val tile2 = testGrass("pöö2","möö2")
    val tile3 = testGrass("pöö3","möö")
    val tile4 = testGrass("pöö4","möö2")
    val tile5 = testGrass("pöö5","möö")
    val tile6 = testGrass("pöö6","möö2")
    val tile7 = testGrass("pöö","möö")
    val tile8 = testGrass("pöö2","möö2")
    val tile9 = testGrass("pöö3","möö")
    val tile10= testGrass("pöö4","möö2")
    val tile11= testGrass("pöö5","möö")
    val tile12= testGrass("pöö6","möö2")

    val grid =  Grid(Vector(tile1, tile2, tile3, tile4, tile5, tile6, tile7, tile8, tile9, tile10, tile11, tile12), 4, 3, Vector(0,0,1,1,0,2,0,0,1,1,0,2))
    val grid2 = Grid(Vector(), 3, 2, Vector(0,1,3,2,1,1))
    grid.givePostitionToTiles()
    grid2.givePostitionToTiles()
    println(grid.tileAt(0,1))
    println("tile positions 3,2")
    grid2.visibleTiles.foreach(n=>println(n.pos))
    println("tile positions 2,3")
    grid.visibleTiles.foreach(n=>println(n.pos))
    println("grid1")
    println("tile 0,0")
    grid.tileAt(0,0).foreach(n => grid.neighbors(n).foreach(n=>println(n.pos)))
    println("tile 1,1")
    grid.tileAt(1,1).foreach(n => grid.neighbors(n).foreach(n=>println(n.pos)))
    println("tile 1,0")
    grid.tileAt(1,0).foreach(n => grid.neighbors(n).foreach(n=>println(n.pos)))
    println("tile 1,2")
    grid.tileAt(2,1).foreach(n => grid.neighbors(n).foreach(n=>println(n.pos)))
    println("grid2")
    println("tile 1,0")
    grid2.tileAt(1,0).foreach(n => grid2.neighbors(n).foreach(n=>println(n.pos)))
    println("tile 1,2")
    grid2.tileAt(1,2).foreach(n => grid2.neighbors(n).foreach(n=>println(n.pos)))

    grid.allTiles.collect { case a: Occupiable => a }.head.addOccupant(unit1)
    val field = FieldMap(Vector(), Vector(), grid,
                         new Organization(Vector(), Vector(), invi5),
                         "win")
    field.movementRangeTiles(unit1)


  gridTest()
  //inventoryTest()
  //combatTest()