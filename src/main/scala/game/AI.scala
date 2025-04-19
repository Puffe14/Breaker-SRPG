package game
import components.*
import components.Behaviour.*
import scala.util.Random

// yksikkö testit
// raportti


object AI:
  var game = Game()
  var currentGroup: Option[Group] = None
  var groupsLeft = Iterator[Group]()
  var currentUnit: Option[Units] = None

  def randomFrom[T, C[T] <: collection.Seq[T]](thingCollection: C[T]): T =
    val number = Random().nextInt(thingCollection.length)
    thingCollection(number)

  /**The AI finds the best possible actions for the members of the current group*/
  def bestMemberActions(g: Group): Vector[Action] =
    for m <- g.haveNotActed yield
      unitBestAction(m)

  /**Selects best action of the entire group*/
  def selectNextAction(g: Group): Option[Action] =
    var find: Option[Action] = None
    val actions = bestMemberActions(g)
    val combats = actions.collect { case a: Combat => a }
    val breaks = actions.collect { case a: Break => a }
    val wounds = actions.collect { case a: Wound => a }
    val treats = actions.collect { case a: Treat => a }
    //PRIORITIZE SKILLS
    //wound if available
    find = wounds.maxByOption(c=>c.forecast.aHit)
    //break if available
    find = breaks.maxByOption(c=>c.forecast.aHit)
    //treat the one with most wounds
    find = treats.maxByOption(c=>c.target.wounds.size)
    //otherwise pick a high value attack
    if find.nonEmpty && combats.nonEmpty then
      find = Some(combats.maxBy(c=>c.forecast.aEV))
    //or just whatever that is left
    else find = actions.headOption
    find


  /**Adds Groups next action to the action queue of the game.*/
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


  def play() =
    if (currentGroup.isEmpty || currentGroup.forall(_.doneActing)) && groupsLeft.nonEmpty then
      currentGroup = Some(nextGroup())
    if currentGroup.forall(_.doneActing) then
      currentGroup = None
    currentGroup.foreach(continue(_))

  //!!! could I add a way to track action priority based on if hp is critical or so on?
  /**Checks the best action for a unit*/
  def unitBestAction(u: Units): Action =
    var chosen: Vector[Action] = Vector()
    // equip the first weapon&medkit, all armor in inventory
    u.equipFirst()
    val availableActions = game.availableActions(u)
    //all possible combat scenarios
    val combats = availableActions
                      .collect { case a: Combat => a }
    //attacks on enemies
    val attacks = combats.filterNot(_.isInstanceOf[Heal]).filterNot(_.isInstanceOf[Treat])
                      .filter(_.target.team != u.team)
                      .filter(u.team!=Team.Ally || _.target.team != Team.Player)
    //healing teammates
    val heals =    combats.collect { case a: Heal => a }
                      .filter(_.target.team == u.team)
                      .filter(_.target.damageTaken!=0)
    //treating teammates
    val treats = availableActions.collect { case a: Treat => a }
                      .filter(_.target.team == u.team)
    //use items on self
    val uses = availableActions.collect { case a: Use => a }
    //wound an enemy
    val wounds = attacks.collect { case a: Wound => a }
      .filter(_.target.wounds.isEmpty) //if the target doesn't have any wounds
      .filter(_.target.lvl>u.lvl)       //only attack dangerous enemies
    //break enemy armor
    val breaks = attacks.collect { case a: Break => a }
    //combine actions to a total vector of actions
    val sensibleActions = attacks ++ heals ++ uses

    /*!!! confusion status from leader death not implemented
    if u.hasStatus(Confused) then
      randomFrom(sensibleActions)*/

    //Treat the highest level member of the group
    if treats.nonEmpty then
      treats.maxBy(_.target.lvl)
    //Select the best break attack by hitrate
    else if breaks.nonEmpty then
      breaks.maxBy(_.forecast.aHit)
    //or the best wound attack by hitrate
    else if wounds.nonEmpty then
      wounds.maxBy(_.forecast.aHit)
    //the attacks the one that will deal the most damage
    else if attacks.nonEmpty then
      attacks.maxBy(n => n.forecast.aEV*3 - n.forecast.bEV)
    //the heals the one who is most hurt
    else if heals.nonEmpty then
      heals.maxBy(_.select.damageTaken)
    //consume an item if hurt !!!(doesn't consider if it heals or not)
    else if uses.nonEmpty && u.damageTaken != 0 then
      uses.head
    //Nothing to do? End turn and wait.
    else
      Wait(u)

  /** Add action to the stack of the game. */
  def addToStack(actions: Vector[Action]) =
    actions.foreach(game.addToStack(_))


  def nextGroup() =
    groupsLeft.next()


  def setGroup() = ()
  def checkGroupCondition() = ()

  def bStand = currentGroup.forall(_.behaviour==Stand)
  def bOnSight = currentGroup.forall(_.behaviour==OnSight)