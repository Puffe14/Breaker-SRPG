package components

trait Event:
  //triggers when the conditions are met
  def trigger(): Boolean

class Reach(location: Vector[Tile], side: Team):
  def trigger(): Boolean =
    true




//Often used conditions
trait Condition:
  var field: Option[FieldMap] = None
  def met(fieldMap: FieldMap): Boolean

//Met when these characters are dead
class Kill(targets: Vector[Units]) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    targets.forall(!fieldMap.allCharacters.contains(_))

//Survive until a particular turn
class Route(targetTeam: Team) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.unitsOnTeam(targetTeam).isEmpty

//Survive until a particular turn
class Survive(turnLimit: Int) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.currentTurn == turnLimit