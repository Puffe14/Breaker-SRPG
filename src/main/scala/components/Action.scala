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
    msg
end Move

class EmptyAction extends Action:
  def play(): Vector[String] = Vector("Empty Action")
  
class Wait(unit: Units) extends Action:
  def play(): Vector[String] =
    unit.endTurn()
    Vector(unit.name + " waited")