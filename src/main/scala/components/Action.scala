package components


trait Action:
  var location: Option[Tile] = None
  def play(): Vector[String]
end Action


class Move(unit: Units, fieldMap: FieldMap) extends Action:
  def play(): Vector[String] =
    var msg = Vector("")
    location.foreach(tile =>
      fieldMap.moveTo(unit,tile)
      msg = Vector(unit.name+" to "+tile.pos)
    )
    unit.moves()
    msg
end Move

class EmptyAction extends Action:
  def play(): Vector[String] = Vector("Empty Action")

class GameOver extends Action:
  def play() = Vector("Game Over")

class MapWon extends Action:
  def play() = Vector("Victory")

class Wait(unit: Units) extends Action:
  def play(): Vector[String] =
    unit.endTurn()
    Vector(unit.name + " waited")

class Trade(unit: Units, inventory: Inventory, slot1: Int, slot2: Int) extends Action:
  def play(): Vector[String] =
    val slots1 = unit.inventory.items
    val slots2 = inventory.items
    unit.inventory.swap(inventory,slot1,slot2)
    Vector("swapped "+slots1(slot1)+" and "+slots2(slot2))

class Use(unit: Units, item: Consumable) extends Action:
  def play(): Vector[String] =
    unit.useItem(item)
    unit.endTurn()
    Vector(unit.name + " used " + item.name)