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

  itemTest()