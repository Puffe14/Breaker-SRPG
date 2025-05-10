package components

import components.Animation.*


trait Action:
  var location: Option[Tile] = None
  var weapon: Option[Weapon] = None
  var explain: Explain = Explain("")
  var actLength: Int = 30
  def playS(): Vector[String] = Vector(explain.name)
  def play(): Explain
end Action


class EmptyAction extends Action:
  explain = Explain("Empty Action")
  def play(): Explain = explain

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
    fieldMap.giveBonuses(false)
    Explain(msg.mkString)
end Move

class Wait(unit: Units) extends Action:
  def play(): Explain =
    explain = Explain(unit.name + " waited")
    explain.addAnimation(unit,Evade,actLength)
    unit.endTurn()
    explain
end Wait

class Trade(unit: Units, inventory: Inventory, slot1: Int, slot2: Int) extends Action:
  def play(): Explain =
    val slots1 = unit.inventory.items
    val slots2 = inventory.items
    explain = Explain("swapped "+slots1(slot1)+" and "+slots2(slot2))
    explain.addAnimation(unit,Hurt,actLength)
    unit.inventory.swap(inventory,slot1,slot2)
    explain
end Trade

class Use(unit: Units, item: Consumable) extends Action:
  def play(): Explain =
    explain = Explain(unit.name + " used " + item.name)
    explain.addAnimation(unit,Hurt,actLength,explain.name)
    unit.useItem(item)
    unit.endTurn()
    explain
end Use