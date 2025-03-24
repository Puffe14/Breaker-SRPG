package game
import components.*

object AI:
  var game = Game()
  var currentGroup: Option[Group] = None
  var groupsLeft = Iterator[Group]()
  var currentUnit: Option[Units] = None

  //The AI finds the best possible actions for the members of the current group
  def bestMemberActions(g: Group): Vector[Action] =
    for m <- g.haveNotActed yield
      unitBestAction(m)

  //Selects best action of th entire grouo next
  def selectNextAction(g: Group): Option[Action] =
    var find: Option[Action] = None
    val actions = bestMemberActions(g)
    val combats = actions.collect { case a: Combat => a }
    if combats.nonEmpty then find = Some(combats.maxBy(c=>c.forecast.aEV))
    else find = actions.headOption
    find

  def continue(g: Group) =
    val mongo: Vector[Action] = selectNextAction(g) match
      case Some(c: Combat) =>
        game.move(c.select) match
          case Some(move) =>
            move.location = c.location
            Vector(move, c)
          case _ => Vector(c)
      case Some(a: Action) => Vector(a)
      case _ => Vector()
    addToStack(mongo)

  //Checks the best action for a unit
  def unitBestAction(u: Units): Action =
    var chosen: Vector[Action] = Vector()
    val combats = game.availableActions(u)
                      .collect { case a: Combat => a }
                      .filter(_.target.team!=u.team)
    if combats.nonEmpty then
      val act: Action = combats
                        .maxBy(n => n.forecast.aEV*3 - n.forecast.bEV)
      act
    else
      Wait(u)

  def addToStack(actions: Vector[Action]) =
    actions.foreach(game.addToStack(_))

  def nextGroup() =
    groupsLeft.next()
  def setGroup() = ()
  def checkGroupCondition() = ()