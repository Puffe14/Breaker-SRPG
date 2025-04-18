package components

trait Event:
  //triggers when the conditions are met
  def trigger(fieldMap: FieldMap): Boolean =
    //If any of the conditions are met.
    val triggered = conditions.exists(_.met(fieldMap))
    if triggered then effect(fieldMap)
    triggered
  def effect(fieldMap: FieldMap): Unit
  val conditions: Vector[Condition]


class Reinforcement(bunch: Vector[(Units, (Int,Int))], team: Team, turns: Vector[Int]) extends Event:
  val conditions = turns.map(Survive(_))
  def effect(fieldMap: FieldMap): Unit =
    val units = bunch.map(_._1)
    // Place units on map
    bunch.foreach((u, p) =>
      fieldMap.theGrid.addUnitAt(u, p)
      u.equipFirst()
    )
    // then based on the team
    team match
      case Team.Player =>
        // add them to deployed
        fieldMap.addUnitListToDeployed(units)
      case _ =>
        // add them to additional groups
        fieldMap.addGroup(Group(units, Behaviour.Agressive, team))


//Often used conditions
trait Condition:
  var field: Option[FieldMap] = None
  def met(fieldMap: FieldMap): Boolean

//Met when the characters with target names are dead
class Kill(targets: Vector[String]) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    targets.forall(name => fieldMap.allCharacters.forall(c=>c.name!=name))

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