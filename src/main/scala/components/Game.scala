package components


class Game:
  var currentMapNumber: Int = 0
  var currentMap: Option[FieldMap] = None
  var midBattle: Boolean = false
  var turnOf: Option[Team] = None
  var player: Option[Organization] = None
  var acting: Option[Units] = None
  var target: Option[Units] = None
  var stack: Iterator[Action] = Iterator()

  // ACTION STACK

  def addToStack(act: Action) =
    stack = stack ++ Iterable(act)

  def nextOnStack(): Action =
    if stack.nonEmpty then stack.next()
    else new EmptyAction()

  def clearStack(): Unit =
    stack = Iterator()

  def availableActions(unit: Units): Vector[Action] =
    currentMap match
      case None => Vector()
      case Some(fm) =>
        fm.movementRangeTiles(unit) //On movement range tiles --Tiles
          .flatMap(tile=>(fm.attackRangeUnitsAt(unit,tile))) //Who can be attacked? --(who, from)
          .toSet //all available unit, distance, tile combinations
          .map((targetable, distance, currentTile) =>
            val newAction = Combat(unit, targetable, distance)
            newAction.location = Some(currentTile)
            newAction)
          .toVector

  def initialize() =
    ()
  def handleTurn() =
    ()

  def move(u: Units): Option[Move] =
    currentMap match
      case Some(fm) => Some(Move(u, fm))
      case _ => None

  def refreshAll() =
    clearStack()
    currentMap.foreach(
      _.allCharacters.foreach(_.refresh())
    )

  //TESTING
  def enemyTurnOver =
    currentMap.forall(_.unitsOnTeam(Team.Enemy).forall(_.turnOver))

end Game