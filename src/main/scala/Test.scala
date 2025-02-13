import components.*

@main
def test =
  def itemTest() =
    val itemi = Item()
    val itemi2 = Item()
    val itemi3 = Item()
    val itemi4 = Item()
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