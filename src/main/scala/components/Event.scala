package components

trait Event:
  //triggers when the conditions are met
  def trigger(fieldMap: FieldMap): Vector[Action] =
    //If any of the conditions are met.
    val triggered = conditions.exists(_.met(fieldMap))
    if triggered then
      conditions = conditions.filterNot(_.met(fieldMap))
      Vector(effect(fieldMap))
    else Vector()

  def effect(fieldMap: FieldMap): Action
  var conditions: Vector[Condition]


class Reinforcement(bunch: Vector[(Units, (Int,Int))], team: Team, turns: Vector[Int]) extends Event:
  var conditions = turns.map(Survive(_))
  def effect(fieldMap: FieldMap): Action =
    val units = bunch.map(_._1)
    val names = units.map(_.name).mkString(", ")
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
    val act = EmptyAction()
    act.explain.addDialogue(s"$names join(s) $team")
    act
end Reinforcement


class Speech(lines: Vector[String], triggeredBy: Condition) extends Event:
  var conditions = Vector(triggeredBy)
  def effect(fieldMap: FieldMap): Action =
    val act = EmptyAction()
    //Sets the lines for the action to be stacked by the game
    lines.foreach(act.explain.addDialogue(_))
    act
end Speech


//Often used conditions
trait Condition:
  def met(fieldMap: FieldMap): Boolean
  def description: String

//Met when the characters with target names are dead
class Kill(targets: Vector[String]) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    targets.forall(name => fieldMap.allCharacters.forall(c=>c.name!=name))
  def description = s"Kill ${targets.mkString(", ")}"

//Remove ALL characters on a particular team on map.
class Route(targetTeam: Team) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.unitsOnTeam(targetTeam).isEmpty
  def description = s"Route $targetTeam}"

//Survive until a particular turn
class Survive(turnLimit: Int) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    fieldMap.currentTurn == turnLimit
  def description = s"Survive $turnLimit turns"

//Reach a particular set of tiles with a specific team. //class Reach(locations: Vector[Occupiable], side: Team) extends Condition:
class Reach(locations: Vector[Occupiable], side: Team) extends Condition:
  def met(fieldMap: FieldMap): Boolean =
    locations.exists(_ //If for any of the locations
      .occupantOnTile  //There is an occupant
      .forall(_.team == side)) //On given team
  def description = s"$side reach ${locations.mkString(", ")}."