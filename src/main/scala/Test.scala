import components.*

class TestMace extends Blunt:
  val name = "test mace"
  val description = "it's the test mace"
  //Info on weapon.
  val durability: Option[Int] = Some(1)
  var spent: Int = 1
  val quick: Boolean = true

  //Direct combat stats.
  val givenPower: Int = 1
  val givenHit: Int = 1
  val givenCrit: Int = 1
  val givenRange: Int = 1
  val givenWeight: Int = 1

  //Effective against these types
  val effectiveAgainst: Map[String, Int] = Map()
  //Possible stat bonuses from holding weapon.
  val bonusToStats: Map[String, Int] = Map()
end TestMace

class testGrass(file: String, name: String) extends Occupiable(file, name):

end testGrass


@main
def test =

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

    val grid =  Grid(Vector(tile1, tile2, tile3, tile4, tile5, tile6), 2, 3, Vector(0,0,1,1,0,2))
    val grid2 = Grid(Vector(tile7, tile8, tile9, tile10, tile11, tile12), 3, 2, Vector(0,1,3,2,1,1))
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


  gridTest()