package components
import game.{AI, DataLibrary, IOHandler}


class Game:
  var currentMapNumber: Int = 1
  var currentMap: Option[FieldMap] = None
  var midBattle: Boolean = false
  var turnOf: Team = Team.Player
  var player: Option[Organization] = None
  var acting: Option[Units] = None
  var target: Option[Units] = None
  var inspected: Option[Units] = None
  var bout: Option[Combat] = None
  var forecast: Option[Forecast] = None
  var part: Part = Part.Head
  var stack: Iterator[Action] = Iterator()
  var zoom: Int = 2
  var direction: Int = 0
  var openMenus: Vector[Menu] = Vector()
  var selectorMenus: Vector[InstantMenu] = Vector()

  // CONTROLS

  def turnWise() =
    val newDir = (direction + 1)%4
    currentMap.foreach(_.setRotation(newDir))
    direction = newDir
  def turnAnti() =
    val newDir = ((direction - 1)%4+4)%4
    currentMap.foreach(_.setRotation(newDir))
    direction = newDir
  def dir: Int = direction

  def selectTile(tile: Tile) =
    tile match
      //If the character is selected again and it's not their turn
      case o: Occupiable if acting.nonEmpty && acting.forall(_.team!=turnOf) =>
        acting = None
      //If the character is selected again during the turn
      case o: Occupiable if acting == o.occupantOnTile =>
        menuPick()
      //Beat-em-up
      case o: Occupiable if target.nonEmpty && !acting.forall(_.turnOver) && targetInRangeOfActor =>
        attack()
      //Select target
      case o: Occupiable if o.occupied && acting.nonEmpty =>
        target = o.occupantOnTile
        //forecast = Some(fight())
      //Move acting unit to given tile
      case o: Occupiable if acting.nonEmpty =>
        unitToTile(o)
      //Select a new acting unit
      case o: Occupiable =>
        acting = o.occupantOnTile
        inspected = None
      case _ =>

  def inspectTile(tile: Tile) =
    tile match
      case o: Occupiable if o.occupied => inspected = o.occupantOnTile
      case _ =>

  def targetPart = part
  def setTarget(newTarget: Option[Units]) = target = newTarget
  def targetor: Option[Tile] =
    target match
      case Some(u) => tileOf(u)
      case None => None

  def cancel() =
    if inspected.nonEmpty then inspected = None
    else if   openMenus.nonEmpty then menuBack()
    else if target.nonEmpty then target = None
    else if acting.nonEmpty then acting = None
  
  def deSelect() =
    acting = None
    target = None

  def clearPostAction() =
    if acting.forall(_.turnOver) then
      deSelect()

  def clearMenuWhenActed() =
    if stack.nonEmpty then
      openMenus = Vector()
      setSelectMenus(Vector())
      forecast = None


  def unitToTile(o: Occupiable) =
    if currentMoveTiles.contains(o) then
      acting.foreach(move(_) match
          case Some(move) =>
            move.location = Some(o)
            addToStack(move)
          case None =>
        )

  def attack() =
    acting.foreach(a=>
      target.foreach(t=>
      fight(a,t).foreach(addToStack(_)))
    )

  // RETURN VALUES

  def allTiles: Vector[Tile] =
    currentMap match
      case Some(fm) => fm.theGrid.allTiles
      case _ => Vector()

  def currentMoveTiles: Set[Tile] =
    currentMap match
      case Some(fm) =>
        acting match
          case Some(unit) if !unit.moveOver =>
            fm.movementRangeTiles(unit)
          case _ => Set()
      case _ => Set()

  def allTilesWithUnits: Vector[Occupiable] =
    currentMap match
      case Some(fm) => fm.theGrid.tilesWithUnits
      case _ => Vector()

  def tileAt(x: Int, y: Int): Option[Tile] =
    currentMap match
      case Some(fm) => fm.theGrid.tileAt(x, y)
      case _ => None

  def tileOf(u: Units): Option[Tile] =
    currentMap match
      case Some(fm) => fm.tileOf(u)
      case _ => None


  // ACTION STACK

  def continue(): Explain =
    val ret = nextOnStack().play()
    isBattleOver
    ret

  def continueS(): Vector[String] =
    val ret = nextOnStack().playS()
    isBattleOver
    ret

  def addToStack(act: Action) =
    stack = stack ++ Iterable(act)

  def addOptionToStack(act: Option[Action]) =
    act.foreach(addToStack(_))

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
            val newActions: Vector[Combat] =
              // possible breaks
              val breaks = if unit.canBreak then for b <- targetable.breakableParts yield
                Break(unit, targetable, distance, b) else Vector()
              // possible wounds
              val wounds = if unit.canWound then for b <- targetable.woundableParts yield
                Wound(unit, targetable, distance, b) else Vector()
              // combine all of them
              Vector(Combat(unit, targetable, distance)) ++ breaks.toVector ++ wounds.toVector

            newActions.foreach(_.location = Some(currentTile))
            newActions)
          .toVector
        val heals = (for medkit <- unit.usableMedkits yield //Medkits that the character could use
          fm.movementRangeTiles(unit) //On movement range tiles --Tiles
          .flatMap(tile=>(fm.attackRangeUnitsAt(unit,tile,medkit.range)))
          .toSet//Who can be attacked? --(who, from)
          .map((targetable, distance, currentTile) =>  //all available unit, distance, tile combinations
            val newActions: Vector[Combat] =
              // possible treats
              val treats = for b <- targetable.wounds yield
                Treat(unit, targetable, distance, medkit, b)
              treats.toVector.appended(Heal(unit, targetable, distance, medkit))
            newActions.foreach(_.location = Some(currentTile))
            newActions
          ).toVector
          ).flatten
        //all possible item uses for character
        val uses = (for c <- unit.consumables yield use(unit,c)) //use action for each item
          .flatten //remove option
          .toVector //to vector
        combats.flatten ++ heals.flatten ++ uses

  def attackRangeUnitsFor(unit: Units): Vector[Units] =
    var guys = Vector[Units]()
    currentMap.foreach(fm=>fm.tileOf(unit)
              .foreach(tl=> guys = fm.attackRangeUnitsAt(unit,tl,unit.Range)
                                   .map(_(0)).toVector))
    guys
  def medRangeUnitsFor(unit: Units): Vector[Units] =
    var guys = Vector[Units]()
    currentMap.foreach(fm=>fm.tileOf(unit)
              .foreach(tl=> unit.medkit.foreach(medkit => guys = fm.attackRangeUnitsAt(unit,tl,medkit.range)
                                   .map(_(0)).toVector)))
    guys
  def breakRangeUnitsFor(unit: Units): Vector[Units] =
    attackRangeUnitsFor(unit).filter(_.breakableParts.nonEmpty)
  def woundRangeUnitsFor(unit: Units): Vector[Units] =
    attackRangeUnitsFor(unit).filter(_.woundableParts.nonEmpty)
  def treatRangeUnitsFor(unit: Units): Vector[Units] =
    medRangeUnitsFor(unit).filter(_.wounds.nonEmpty)


  def initialize() =
    IOHandler.buildClasses()
    IOHandler.buildCharacters()
    IOHandler.buildItems()
    IOHandler.buildInventory()
    IOHandler.buildTiles()
    IOHandler.buildFieldMaps()

  /** Called when the turn is continuing. */
  def handleTurn(): Unit =
    //all groups on a particular side on the current map
    var groupsWithTurn: Vector[Group] = Vector()
    currentMap.foreach(fm=>
      player.foreach(p => if fm.player!=p then
        fm.setPlayer(p)
        fm.deployPlayer()
      )
      fm.clearDead()
      fm.setLeaders()
      groupsWithTurn = fm.groups.filter(_.side==turnOf)
      groupsWithTurn.foreach(_.reduceTemporary())
    )

    //If the AI has no groups to control yet, give them all to the AI so it can handle them
    if turnOf!=Team.Player then
      if AI.currentGroup.isEmpty && !groupsWithTurn.forall(_.doneActing) then
        AI.game = this
        AI.groupsLeft = groupsWithTurn.iterator
      AI.play()


    //if the turn of the current team is over then change to the next teams turn.
    if groupsWithTurn.forall(_.doneActing) then
      turnOf = turnOf match
        case Team.Player =>Team.Enemy
        case Team.Enemy => Team.Ally
        case Team.Ally =>  turnCountUp(); Team.Player
      refreshAll()
      deSelect()
    clearPostAction()
    clearMenuWhenActed()
      //handleTurn() //If the turn is over, let the next ones act
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
        //midBattle = false
        addToStack(MapWon())
        currentMapNumber+=1//Advance to next map
        turnOf = Team.Player
        currentMap = DataLibrary.maps.get(currentMapNumber.toString)
        //currentMap.foreach(fm=>player.foreach(fm.setPlayer(_)))
    )
    over

  def battleStart() =
    midBattle = true

  def turnCountUp() =
    currentMap.foreach(_.tickTurn())
    currentMap.map(_.eventCheck())

  //Methods for creating actions

  def move(u: Units): Option[Move] =
    currentMap match
      case Some(fm) => Some(Move(u, fm))
      case _ => None

  def wait(u: Units): Option[Wait] =
    Some(Wait(u))

  def use(u: Units, c: Consumable): Option[Use] =
    Some(Use(u, c))

  def trade(u: Units, i: Inventory, s1: Int, s2: Int): Option[Trade] =
    currentMap match
      case Some(fm) => Some(Trade(u, i, s1, s2))
      case _ => None

  def fight(u: Units, t: Units): Option[Combat] =
    val range = unitToUnitDistance(u,t)
    currentMap match
      case Some(fm) => Some(Combat(u, t, range))
      case _ => None

    // Acting actions

  def actingUse(item: Item) =
    acting.foreach(u=>
      val act = item match
        case c: Consumable => use(u, c)
        case _ => None
      addOptionToStack(act)
    )

  def actingToggleEquip(item: Item) =
    acting.foreach(u=>
      u.toggleEquip(item)
    )

  def actingEquip(item: Item) =
    acting.foreach(u=>
      u.equip(item)
    )

  def actingDiscard(item: Item) =
    acting.foreach(u=>
      u.discard(item)
    )

  def actingWait() =
    acting.foreach(u=>
      addOptionToStack(wait(u))
    )


  def setBout(combat: Option[Combat]) =
    bout = combat
  def performBout() =
    addOptionToStack(bout)
    setBout(None)
  def setForecast() =
    forecast = bout match
      case Some(combat) => Some(combat.forecast)
      case None => None

  def unitToUnitDistance(u: Units, t: Units): Int =
    var range = 0
    currentMap.foreach(fm=>fm.tileOf(u).foreach(t1=>fm.tileOf(t)
      .foreach(t2=> range = fm.theGrid.tileDistance(t1,t2))))
    range

  def targetInRangeOfActor =
    var inRange = false
    acting.foreach(a=>
      target.foreach(t=>
        a.weapon.foreach(w=>
              val range = unitToUnitDistance(a,t)
              if range >= w.range(0) && range <= w.range(1) then inRange = true
            )))
    inRange

  def refreshAll() =
    clearStack()
    currentMap.foreach(
      _.allCharacters.foreach(_.refresh())
    )

 
  //MANU HANDLING

  def handleMenu() =
    openMenus.lastOption.foreach(m=>
      m.createSubMenus(this)
      if stack.nonEmpty && openMenus.length != 1 then addMenu(m)
      if openMenus.length>1 then
        openMenus(1) match
          case m: TargetMenu => bout = m.combat(this)
          case _ =>
    )
    openMenus.foreach(_.createSubMenus(this)) //update

  def addMenu(menu: Menu) =
    openMenus = openMenus.appended(menu)
  def setSelectMenus(menus: Vector[InstantMenu]) =
    selectorMenus = menus
    selectorMenus.foreach(_.createSubMenus(this))

  def menuPick() =
    if openMenus.nonEmpty then
      val latest = openMenus.last
      latest match
        case m: ConfirmMenu =>
          if latest.subMenus.nonEmpty then addMenu(latest.pick)
          else latest.pick
        case _ =>
          if latest.subMenus.nonEmpty then addMenu(latest.pick)
          else println("EMPTY MENU SUBS")
    else
      val menu: Menu = ActionsMenu()
      addMenu(menu)
    handleMenu()

  def menuBack() =
    openMenus = openMenus.take(openMenus.length-1)
    selectorMenus = Vector()
    forecast = None
    target = None
    bout = None
  def menuUp() =
    openMenus.last match
      case m: InstantMenu => m.selectUpEffect(this)
      case m: Menu => m.selectorUp()
  def menuDown() =
    openMenus.last match
      case m: InstantMenu => m.selectDownEffect(this)
      case m: Menu => m.selectorDown()
  def menuSUp() =
    selectorMenus.head match
      case m: InstantMenu => m.selectUpEffect(this)
      case m: Menu => m.selectorUp()
  def menuSDown() =
    selectorMenus.head match
      case m: InstantMenu => m.selectDownEffect(this)
      case m: Menu => m.selectorDown()
  def menuSLeft() =
    selectorMenus.last match
      case m: InstantMenu => m.selectUpEffect(this)
      case m: Menu => m.selectorUp()
  def menuSRight() =
    selectorMenus.last match
      case m: InstantMenu => m.selectDownEffect(this)
      case m: Menu => m.selectorDown()
  //If the player is in a menu
  def inMenu = openMenus.nonEmpty
  def menus = openMenus


  def setTargetedPart(tPart: Part) = part = tPart


  //TESTING

  def enemyTurnOver =
    currentMap.forall(_.unitsOnTeam(Team.Enemy).forall(_.turnOver))

end Game