package components

trait Event:
  //triggers when the conditions are met
  def trigger(): Boolean



//Often used conditions
trait Condition:
  var field: Option[FieldMap] = None
  def met(fieldMap: FieldMap): Boolean

//Met when these characters are dead
class Kill(targets: Vector[Units]) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    targets.forall(!fieldMap.allCharacters.contains(_))

//Remove ALL characters on a particular team on map.
class Route(targetTeam: Team) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.unitsOnTeam(targetTeam).isEmpty

//Survive until a particular turn
class Survive(turnLimit: Int) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.currentTurn == turnLimit

//Reach a particular set of tiles with a specific team. //class Reach(locations: Vector[Occupiable], side: Team) extends Condition:
class Reach(locations: Vector[Occupiable], side: Team) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    locations.exists(_ //If for any of the locations
      .occupantOnTile  //There is an occupant
      .forall(_.team == side)) //On given team