package components

import components.Animation.*


trait Action:
  var location: Option[Tile] = None
  var explain: Explain = Explain("")
  def playS(): Vector[String] = Vector(play().name)
  def play(): Explain
end Action


class EmptyAction extends Action:
  def play(): Explain =  Explain("Empty Action")

class GameOver extends Action:
  def play(): Explain = Explain("Game Over")

class MapWon extends Action:
  def play(): Explain = Explain("Victory")


class Move(unit: Units, fieldMap: FieldMap) extends Action:
  def play(): Explain =
    var msg = Vector("")
    location.foreach(tile =>
      fieldMap.moveTo(unit,tile)
      msg = Vector(unit.name+" to "+tile.pos)
    )
    unit.moves()
    Explain(msg.mkString)
end Move

class Wait(unit: Units) extends Action:
  def play(): Explain =
    explain = Explain(unit.name + " waited")
    explain.addAnimation(unit,Evade,1)
    unit.endTurn()
    explain
end Wait

class Trade(unit: Units, inventory: Inventory, slot1: Int, slot2: Int) extends Action:
  def play(): Explain =
    val slots1 = unit.inventory.items
    val slots2 = inventory.items
    explain = Explain("swapped "+slots1(slot1)+" and "+slots2(slot2))
    explain.addAnimation(unit,Hurt,1)
    unit.inventory.swap(inventory,slot1,slot2)
    explain
end Trade

class Use(unit: Units, item: Consumable) extends Action:
  def play(): Explain =
    explain = Explain(unit.name + " used " + item.name)
    explain.addAnimation(unit,Hurt,1)
    unit.useItem(item)
    unit.endTurn()
    explain
end Use