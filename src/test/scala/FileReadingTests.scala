import components.*
import game.DataLibrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class FileReadingTests extends AnyFlatSpec with Matchers:

  val game = Game()
  val dt = DataLibrary
  val testPlayer = Organization(Vector(), Vector(), new Inventory(1), Team.Player)
  game.initialize()
  game.player = Some(testPlayer)

  "The initialize method" should
  "return the correct Item" in {
    val item = dt.items("super_club")
    (item.name) should equal ("Super Club")
    (item.description) should equal ("A test weapon.")
  }
  "The initialize method" should
  "return the correct Class" in {
    val testclass = dt.classes("tester")
    (testclass.name) should equal ("tester")
    (testclass.abilities) should equal (Vector("infantry"))
    (testclass.stats) should equal (testclass.stats.map((a,b)=>(a->1)))
  }

  game.currentMap = dt.maps.get("test")
  game.handleTurn()
  val testmap = dt.maps("test")
  val anEnemy = testmap.allCharacters.filter(_.side==Team.Enemy).head
  val aPlayer = testmap.allCharacters.filter(_.side==Team.Player).head
  val grid = testmap.theGrid

  "The initialize method FieldMaps" should
  "return the battle field with a correct grid" in {
    (grid.allTiles.map(_.pos)) should equal (Vector((0,0,0), (1,0,1)))
    (grid.allTiles.map(_.name)) should equal (Vector("grass","grass"))
  }
  "the first character on the list of all characters" should "be an enemy" in {
    (testmap.groups.head.side) should equal (Team.Enemy)
  }
  "the clear condition for the map" should "the given one" in {
    (testmap.clear) should equal ("Survive 3 turns")
  }
  "the joining player character" should "be deployed in player organization" in {
    (aPlayer) should equal (testmap.player.deployed.head)
  }
  "the player" should "have taken 1 damage on creation" in {
    (aPlayer.damageTaken) should equal (1)
  }
  "the tester inventory Super Club" should
  "be missing a point of durability" in {
    (aPlayer.inventory.weapons.head.spent) should equal (2)
  }
  "the units, and their inventories" should
  "be separate instances" in {
    (aPlayer) should not equal (anEnemy)
    (aPlayer.inventory.hashCode()) should not be (anEnemy.inventory.hashCode())
  }
  "the items in given inventories" should
  "be separate instances" in {
    aPlayer.inventory.weapons.head.spend(1)
    (aPlayer.inventory.weapons.head.toString) should not be (anEnemy.inventory.weapons.head.toString)
  }