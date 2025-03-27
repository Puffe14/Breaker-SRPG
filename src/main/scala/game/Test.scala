package game

import components.*
import components.Behaviour.Agressive
import components.Part.*
import components.Team.*
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

class TestSword extends Sharp:
  val name = "test sword"
  val description = "it's the test sword"
  val dmgType = "force"
  //Info on weapon.
  val durability: Option[Int] = Some(10)
  var spent: Int = 0
  val quick: Boolean = true

  //Direct combat stats.
  val givenPower: Int = 5
  val givenHit: Int = 75
  val givenCrit: Int = 1
  val givenRange: (Int, Int) = (1, 1)
  val givenWeight: Int = 1

  //Effective against these types
  val effectiveAgainst: Map[String, Int] = Map()
  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int] = Map()
end TestSword

class TestMedkit extends Medkit(10):
  val bonusToStats = Map[String, Int]()
  val description = "test medkit"
  val name = "testmeds"

class TestSoup extends Healing(Map[String, Int](), 1, 3, 10):
  override val name: String = "Soup"
  override val description: String = "Tasty soup."

class TestHelmet extends Armor(Head):
  val name = "helmet"
  val description = "a basic helmet"
  val bonusToStats = Map("defence" -> 20)


val club = BluntFile("w_club")
val mace = BluntFile("d_mace")

val testStatMap = Map("hitpoints" -> 55, "strength" -> 1, "magic" -> 1, "skill" -> 1, "speed" -> 1, "defence" -> 1, "resistance" -> 1, "movement" -> 3, "jump" -> 2)
val testFastMap = Map("hitpoints" -> 24, "strength" -> 1, "magic" -> 1, "skill" ->10, "speed" ->12, "defence" -> 1, "resistance" -> 1, "movement" -> 1)
val testStrongMap = Map("hitpoints" -> 25, "strength" -> 5, "magic" -> 1, "skill" ->0, "speed" ->0, "defence" -> 5, "resistance" -> 1, "movement" -> 1)
val testSpiritMap = Map("hitpoints" -> 8, "strength" -> 1, "magic" -> 5, "skill" ->0, "speed" ->0, "defence" -> 0, "resistance" -> 5, "movement" -> 1)
val testZeroMap = Map("hitpoints" -> 0, "strength" -> 0, "magic" -> 0, "skill" -> 0, "speed" -> 0, "defence" -> 0, "resistance" -> 0, "movement" -> 0)
val test100Map = Map("hitpoints" -> 100, "strength" -> 100, "magic" -> 100, "skill" -> 100, "speed" -> 100, "defence" -> 100, "resistance" -> 100)
val testClass = new Class("test", 1, Vector("test"),testStatMap,testStatMap,testZeroMap,testZeroMap)

val cylna =     new Character("cylna", testClass, Vector(),1,0,test100Map,testStrongMap)
val bonk =      new Character("bonk", testClass, Vector(),1,0,testStatMap,testStatMap)
val gonzales =  new Character("gonzales", testClass, Vector(),1,0,testStrongMap,testStrongMap)
val wrys   =    new Character("wrys", testClass, Vector(),1,0,testFastMap,testFastMap)
val ghost =     new Character("ghost", testClass, Vector(),1,0,testSpiritMap,testSpiritMap)

class testGrass(file: String, name: String) extends Occupiable(file, name):
end testGrass

class testSand(file: String, name: String) extends Occupiable(file, name):
  reduction = Map("test" -> 2)
end testSand

class testWall(file: String, name: String) extends Unoccupiable(file, name, false):
end testWall



class LogTest:
  val game = Game()
  game.battleStart()
  val unit1 = Units(cylna)
  val unit2 = Units(wrys)
  val unit3 = Units(bonk)
  val unit4 = Units(ghost)
  val unit5 = Units(gonzales)
  val units = Vector(unit1, unit2, unit3, unit4)
  val group = Group(Vector(unit2, unit4, unit3),Agressive,Enemy)
  var ai = false
  var fight = Combat(Units(bonk), Units(bonk), 1)
  var field = FieldMap(Vector(), Vector(), new Grid(Vector(),0,0,Vector()),
                       new Organization(Vector(unit1), Vector(unit1), new Inventory(50), Player), Survive(0), Vector(), 0, 0)

  unit1.setTeam(Player)
  unit3.setTeam(Enemy)
  unit2.setTeam(Enemy)
  unit4.setTeam(Enemy)
  unit5.setTeam(Player)
  def dmace = BluntFile("d_mace")
  def wclub = BluntFile("w_club")
  def testspell = TestSpell()
  def testmed = TestMedkit()
  def testsoup = TestSoup()

  def resetFighters() =
    units.foreach(_.healDamage(100))
    units.foreach(_.inventory.removeAll())

    val itemi = TestMace()
    val itemi2 = TestSword()
    val itemi3 = TestHelmet()
    val itemi4 = testmed
    val itemi5 = testspell
    val itemi6 = dmace
    val invi = Inventory(6)
    val invi2 = Inventory(6)

    //invi2.add(Some(itemi2))
    itemi.equip()
    itemi2.equip()
    itemi3.equip()
    //itemi4.equip()
    itemi5.equip()
    itemi6.equip()
    //unit1.inventory.swap(invi, 0, 0)
    unit1.inventory.add(Some(itemi))
    unit1.inventory.add(Some(dmace))
    unit1.inventory.add(Some(wclub))
    unit1.inventory.add(Some(testspell))
    unit2.inventory.add(Some(itemi2))
    unit2.inventory.add(Some(itemi3))
    unit3.inventory.add(Some(itemi4))
    unit3.inventory.add(Some(testsoup))
    unit4.inventory.add(Some(itemi5))
    unit5.inventory.add(Some(itemi6))

    //unit2.inventory.swap(invi2, 0, 0)
  
  def setF12() =
    val fight12 = Combat(unit1, unit2, 1)
    fight = fight12
  end setF12

  def setF1C() =
    val target = unit1RangeUnits.head
    var range = 1
    field.tileOf(unit1).foreach(t1=>field.tileOf(target)
      .foreach(t2=> range = field.theGrid.tileDistance(t1,t2)))
    val fight1C = Combat(unit1, target, range)
    fight = fight1C
  end setF1C

  def setF32() =
    val heal32 = Combat(unit3, unit2, 1)
    fight = heal32

  def setF31() =
    val heal31 = Combat(unit3, unit1, 1)
    fight = heal31

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
      Vector(1,1,0,0,1,0,0,0,0,0,0,0,2,2,3,0,0,0,0,0,0,0,0,0)) //Vector(1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0)

    grid.givePostitionToTiles()
    grid.occupiables.head.addOccupant(unit1)
    grid.occupiables(5).addOccupant(unit2)
    grid.occupiables(1).addOccupant(unit3)
    grid.occupiables(17).addOccupant(unit4)
    grid.occupiables(20).addOccupant(unit5)

    field = FieldMap(Vector(group), Vector(), grid,
                     new Organization(Vector(unit1,unit5), Vector(unit1,unit5), storage, Player), Route(Enemy), Vector(Route(Player)), 0, 0)
  end setGrid

  def log(which: String): Vector[String] =
    which match
      case "break" => Break(unit1,unit2,1,Head).playS()
      case "wound" => Wound(unit1,unit2,1,Head).playS()
      case "heal" =>  Heal(unit3,unit1,1,testmed).playS()
      case "treat" => Treat(unit3,unit2,1,testmed,Head).playS()
      case "ai" => AIup()
      case "pass" =>
        if game.stack.isEmpty && !game.enemyTurnOver then
          AIup()
        else
          game.continueS()
      case _ =>      fight.playS()


  def theField: FieldMap =
    field

  def moveUnit1(x: Int, y: Int) =
    field.theGrid.tileAt(x, y).foreach(targetTile =>
      if moveAreaTiles.contains(targetTile) then
        field.moveTo(unit1, targetTile)
      else
        println(s"tile ${targetTile.pos} not available")
        println("pls give one of these")
        println(moveAreaPosString)
    )

  def inventoryUnit1() =
    unit1.inventory.toString
    levelUp(unit1)

  def levelUp(unit: Units) =
    unit.character.expTrack(300).foreach(println)

  def weaponsUnit1 =
    unit1.inventory.weapons

  def equipUnit1(slot: Int) =
    val choosables = unit1.inventory.weapons
    if choosables.size > slot then
      unit1.equip(choosables(slot))
    else println("unchoosable")

  def weaponsUnit1String =
    weaponsUnit1.map(_.toString).mkString(", ")

  def moveAreaTiles =
    field.movementRangeTiles(unit1)

  def info =
    game.currentMap = Some(field)
    units.foreach(println(_))
    println(game.availableActions(unit1).map(_.toString).mkString("\n"))

  def moveAreaPosString =
    moveAreaTiles.map(_.pos).mkString(", ")

  def unit1RangeUnits =
    field.attackRangeUnits(unit1)
  def unit1CanAtk =
    field.attackRangeUnits(unit1).nonEmpty

  def unit1CanAtkString =
    "can attack: "+field.attackRangeUnits(unit1).map(_.name).mkString(", ") + "\n" +
      fight.forecastString

  def AIup(): Vector[String] =
    game.currentMap = Some(field)
    AI.game = game
    AI.continue(group)
    ai = true
    //println("Ai up "+game.stack.mkString(", "))
    Vector()

  def AIcontrol: Boolean =
    if game.enemyTurnOver then ai = false
    ai

@main
def test() =

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

    unit1.unitsInventory.swap(invi, 0, 0)
    unit2.unitsInventory.swap(invi2, 0, 0)
    unit3.unitsInventory.swap(invi3, 0, 0)
    unit4.unitsInventory.swap(invi4, 0, 0)
    unit5.unitsInventory.swap(invi5, 0, 0)

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
      fight.playS()
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
      //fight.playWound(Head)
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
    grid2.visibleTiles(0).foreach(n=>println(n.pos))
    println("tile positions 2,3")
    grid.visibleTiles(0).foreach(n=>println(n.pos))
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
                         new Organization(Vector(), Vector(), invi5, Player),
                         Route(Enemy), Vector(Route(Player)), 1, 0)
    field.movementRangeTiles(unit1)

  def fileTest() =
    println(club)
    println(mace)

  fileTest()
  //gridTest()
  //inventoryTest()
  //combatTest()