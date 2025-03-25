package components

import game.AI


class Game:
  var currentMapNumber: Int = 0
  var currentMap: Option[FieldMap] = None
  var midBattle: Boolean = false
  var turnOf: Team = Team.Player
  var player: Option[Organization] = None
  var acting: Option[Units] = None
  var target: Option[Units] = None
  var stack: Iterator[Action] = Iterator()


  // ACTION STACK

  def continue(): Vector[String] =
    val ret = nextOnStack().play()
    isBattleOver
    ret

  def addToStack(act: Action) =
    stack = stack ++ Iterable(act)

  def nextOnStack(): Action =
    if stack.nonEmpty then stack.next()
    else new EmptyAction()

  def clearStack(): Unit =
    stack = Iterator()

  // Actions available to a given unit

  def availableActions(unit: Units): Vector[Action] =
    currentMap match
      case None => Vector()
      case Some(fm) =>
        val combats = fm.movementRangeTiles(unit) //On movement range tiles --Tiles
          .flatMap(tile=>(fm.attackRangeUnitsAt(unit,tile,unit.Range))) //Who can be attacked? --(who, from)
          .toSet //all available unit, distance, tile combinations
          .map((targetable, distance, currentTile) =>
            val newAction = Combat(unit, targetable, distance)
            newAction.location = Some(currentTile)
            newAction)
          .toVector
        val heals = (for medkit <- unit.usableMedkits yield //Medkits that the character could use
          fm.movementRangeTiles(unit) //On movement range tiles --Tiles
          .flatMap(tile=>(fm.attackRangeUnitsAt(unit,tile,medkit.range)))
          .toSet//Who can be attacked? --(who, from)
          .map((targetable, distance, currentTile) =>  //all available unit, distance, tile combinations
            val newAction = Heal(unit, targetable, distance, medkit)
            newAction.location = Some(currentTile)
            newAction
          ).toVector
          ).flatten
        //all possible item uses for character
        val uses = (for c <- unit.consumables yield use(unit,c)) //use action for each item
          .flatten //remove option
          .toVector //to vector
        combats ++ heals ++ uses

  def initialize() =
    ()

  /** Called when the turn is continuing. */
  def handleTurn(): Unit =
    //all groups on a particular side on the current map
    var groupsWithTurn: Vector[Group] = Vector()
    currentMap.foreach(fm=>
      groupsWithTurn = fm.groups.filter(_.side==turnOf)
    )

    //If the AI has no groups to control yet, give them all to the AI so it can handle them
    if turnOf!=Team.Player && AI.groupsLeft.isEmpty then
      AI.groupsLeft = groupsWithTurn.iterator

    //if the turn of the current team is over then change to the next teams turn.
    if groupsWithTurn.forall(_.doneActing) then
      turnOf = turnOf match
        case Team.Player => Team.Enemy
        case Team.Enemy => Team.Ally
        case Team.Ally => Team.Player
      handleTurn() //If the turn is over, let the next ones act
  end handleTurn

  /** Checks whether if the battle is over or not */
  def isBattleOver: Boolean =
    var over = !midBattle
    currentMap.foreach(fm =>
      if fm.isLost then   //Priority lose so no "draws" after combat could happen
        over = true
        addToStack(GameOver())
      if fm.isCleared then//If player beats the map
        over = true
        midBattle = false
        addToStack(MapWon())
        currentMapNumber+=1//Advance to next map
    )
    over

  def battleStart() =
    midBattle = true


  //Methods for creating actions

  def move(u: Units): Option[Move] =
    currentMap match
      case Some(fm) => Some(Move(u, fm))
      case _ => None

  def use(u: Units, c: Consumable): Option[Use] =
    currentMap match
      case Some(fm) => Some(Use(u, c))
      case _ => None

  def trade(u: Units, i: Inventory, s1: Int, s2: Int): Option[Trade] =
    currentMap match
      case Some(fm) => Some(Trade(u, i, s1, s2))
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