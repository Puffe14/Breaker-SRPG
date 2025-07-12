package components
import components.Team.*
import game.DataLibrary
import os.{RelPath, read as or}
import ujson.Value.Value
import upickle.core.LinkedHashMap
import upickle.default.read

import scala.collection.mutable

class FieldMap(enemies: Vector[Group],
               allies: Vector[Group],
               grid: Grid,
               var player: Organization,
               clearCondition: Condition,
               loseConditions: Vector[Condition],
               var rotation: Int,
               var turnNumber: Int,
               deployment: Vector[(Int,Int)],
               joining: Vector[Units] = Vector(),
               events: Vector[Event] = Vector()):
  def allCharacters: Vector[Units] =
    grid.unitsOnTiles
  def groups: Vector[Group] = enemies ++ allies ++ Vector(player.group) ++ additions
  def setPlayer(org: Organization) =
    player = org
  def setLeaders() =
    val leadGroups = groups.filter(_.side==Enemy)
    leadGroups.foreach(_.setLeader())
  def isCleared: Boolean =
    clearCondition.met(this)
  def isLost: Boolean =
    loseConditions.exists(_.met(this))
  def clear = clearCondition.description
  def theGrid = grid
  def SAVEABLE: String = ""

  def currentRotation = rotation
  def setRotation(newDir: Int) = rotation = newDir%4
  def currentTurn = turnNumber

  //Methods for interfacing with units on field

  def tileOf(unit: Units): Option[Occupiable] =
    grid.occupiables
        .find(_.occupantOnTile == Some(unit))

  def unitDistanceFrom(mainTile: Tile, unit: Units): Int =
    tileOf(unit) match
      case Some(tile) => theGrid.tileDistance(mainTile, tile)
      case _ => 0

  def moveTo(unit: Units, target: Tile) =
    val former = tileOf(unit)
    target match
      case o: Occupiable =>
        former.foreach(_.removeOccupant())
        o.addOccupant(unit)
        if o.containsSoul && unit.canTakeSouls then
          o.spendSoul()
          unit.weapon.foreach(_.fix())
      case _ => println(s"$target cannot be occupied")

  def clearDead() =
    grid.tilesWithUnits.filter(_.occupantOnTile
                       .forall(_.isDead))
                       .foreach(t =>
                         t.occupantOnTile.foreach(u=>t.addCorpse(u.loot))
                         t.removeOccupant()
                       )
    giveBonuses(false) //set bonuses again to account for deaths

  def unitsOnTeam(team: Team) =
    allCharacters.filter(_.team == team)

  def deploymentTiles: Vector[Occupiable] =
    deployment.flatMap((x, y) => grid.tileAt(x, y))
              .collect { case a: Occupiable => a }

  /**Place player characters onto the deployment tiles on the map.*/
  def deployPlayer() =
    val tiles = deploymentTiles
    val deployed = player.deployed.take(tiles.size)
    grid.unitsOnTiles.filter(_.team == Team.Player) // Get any "player" team characters on map
        .foreach(addUnitToPlayerDeployed(_))        // and add them to the player deployds.
    for i <- deployed.indices do
      tiles(i).addOccupant(deployed(i))   // Add the characters chosen to be deployed onto the
      deployed(i).setTeam(Player)         // deployment map and set their team to player.

  /** add new groups onto field map*/
  var additions = Vector[Group]()
  def addGroup(group: Group) =
    if group.side != Player then group.setLeader()
    additions = additions.appended(group)

  /**add more characters to this maps current player organization*/
  def addUnitToPlayerDeployed(unit: Units) =
    player.addDeployed(unit)
  def addUnitListToDeployed(units: Vector[Units]) =
    units.foreach(addUnitToPlayerDeployed(_))

  /**Current turn number goes up*/
  def tickTurn() =
    turnNumber+=1

  /**Check if an event should be triggered on the map. */
  def eventCheck(): Vector[Action] =
    events.flatMap(_.trigger(this))

  /**Gives stat bonuses from tile, aura buffs, and debuffs
   * for all units on the map.*/
  def giveBonuses(includeHealth: Boolean) =
    theGrid.tilesWithUnits.foreach(t =>
      t.occupantOnTile.foreach(u =>
        calculateBonus(u, t, includeHealth)
      )
    )

  /**Give buffs and debuffs from the environment
   * like class auras or tile hp effect to a given unit on their given tile*/
  def calculateBonus(unit: Units, tile: Occupiable, includeHealth: Boolean) =
    unit.resetNearbyBonus()
    val bonus = mutable.Map[String, Int]()
    def addToStat(which: String, amount: Int) =
      val newTotal = bonus.getOrElse(which, 0) + amount
      bonus += (which -> newTotal)
    tile.statsMap.foreach((a,b)=>addToStat(a,b))
    val units = statusRangeUnitsFor(unit)
    unit.leader.foreach(l => if units.contains(l) then rules.leaderBonus.foreach((a,b)=>addToStat(a,b)))
    units.filter(_.team==unit.team).foreach(_.givenNearbyBuffs.foreach((a,b)=>addToStat(a,b)))
    units.filter(_.team!=unit.team).foreach(_.givenNearbyDebuffs.foreach((a,b)=>addToStat(a,b)))
    unit.setNearbyBonus(bonus.toMap)
    if includeHealth then
      unit.takeDamage(-tile.hpEffect)


  //Methdos for determining which tiles a unit could occupy with current MOVE

  /**Method for determining the tiles accessible based on movement, current tile and class types.
   * Used by movementRangeTiles to determine where a unit can move.*/
  def moveCheck(moveLeft: Double, tile: Tile, types: Vector[String], team: Team, elevation: Int, jump: Int): Vector[Tile] =

    def findSurrounding(thisOneOk: Boolean) =
      val accessibles = mutable.Buffer[Tile]()
      if thisOneOk then accessibles += tile
      val availableNeighbors = grid.neighbors(tile)
                                   .filter(t => grid.elevationDifference(elevation, t) <= jump)
      availableNeighbors
        .foreach(accessibles ++= moveCheck(moveLeft-tile.moveReduction(types), _, types, team, tile.pos(2), jump))
      accessibles.toVector
    end findSurrounding

    tile match
      //Empty if not enough move left
      case t if moveLeft < tile.moveReduction(types) =>
        t match
          case o: Occupiable if !o.occupied => Vector(tile)
          case _ => Vector()
      //In the case where the tile is occupiable
      case o: Occupiable if !o.occupied =>
        findSurrounding(true)
      case o: Occupiable if o.occupant.forall(_.team == team) =>
        findSurrounding(false)
      //If it can be flown over
      case u: Unoccupiable if u.canFlyOver && types.contains("flier") =>
        findSurrounding(false)
      //If other checks fail
      case _ =>
        Vector()

  end moveCheck


  //Useful methods for finding stuff

  def tilesVisible: Vector[Tile] =
    grid.visibleTiles(rotation)

  /**Returns a set of tiles which the given unit can move to during this turn. */
  def movementRangeTiles(mover: Units): Set[Tile] =
    //find the location of the moving unit and find their info
    val locationTile = tileOf(mover)
    val movementRange = mover.MOVE
    val movementType = mover.types
    var tilesFound = Set[Tile]()
    locationTile.foreach( t =>
      tilesFound = moveCheck(movementRange, t, movementType, mover.team, t.pos(2), mover.JUMP).toSet
      tilesFound += t
    )
    tilesFound

  /**Gives a set of who can a unit attack. */
  def attackRangeUnits(mover: Units): Set[Units] =
    //find the location of the moving unit and find their info
    val locationTile = tileOf(mover)
    val (minR, maxR) = mover.Range
    var unitsFound = Set[Units]()
    locationTile.foreach( t =>
      for i <- minR to maxR do
        unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(t,i)).toSet - mover
    )
    unitsFound

  /**Which units are close enough to impose buff or debuff to the given one.*/
  def statusRangeUnitsFor(selected: Units): Set[Units] =
    val locationTile = tileOf(selected)
    val (minR, maxR) = rules.statusAuraRange
    var unitsFound = Set[Units]()
    locationTile.foreach( t =>
      for i <- minR to maxR do
        unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(t,i)).toSet - selected
    )
    unitsFound

  /** Checks who can be attacked on a particular location.
   *  Returns the unit, distance from checked tile, and tile.    */
  def attackRangeUnitsAt(mover: Units, tile: Tile, range: (Int, Int)): Set[(Units,Int,Tile)] =
    val (minR, maxR) = range
    var unitsFound = Set[Units]()
    for i <- minR to maxR do
      unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(tile,i)).toSet - mover
    unitsFound.map(unit => (unit, unitDistanceFrom(tile, unit), tile))

end FieldMap



object MapHandler:
  var lastData = getData
  def getData = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/maps.json")))
  def create: Map[String, FieldMap] =
    lastData = getData
    val maps = lastData.obj
    val mapnames = maps.keys
    val nameToMap = for name <- mapnames yield
      val fmap = maps(name)

      //Create grid
      val gridMap = fmap("grid").obj
       // Get the tiles
      val tiles = read[Vector[String]](gridMap("tiles")).map(DataLibrary.tiles(_).copy)
      val grid = Grid(tiles,
                      read[Int](gridMap("row")),
                      read[Int](gridMap("column")),
                      read[Vector[Int]](gridMap("elevation"))
                 )
      grid.givePositionToTiles()
      
      //!!! Behaviour handling missing, all groups automatically Agressive.
      def makeGroup(memberInfo: Vector[(String,Int,(Int,Int))], team: Team): Group =
        val g = Group(memberInfo.map(makeUnit(_)), Behaviour.Agressive, team, false)
        if g.side != Player then g.setLeader()
        g
      def makeUnit(unitInfo: (String,Int,(Int,Int))): Units =
        val unitName = unitInfo(0)  //get the name
        val unit = Units(DataLibrary.characters(unitName).copyMe,         // Find the character
                         DataLibrary.inventories("inventory_"+unitName).copyMe)  // Find the inventory
        val (a,b,c) = unitInfo
        grid.addUnitAt(unit, c) // Place the character on the map
        unit.takeDamage(b)      // Harm them enough
        unit.equipFirst()       // Equip the weapon on their first slot
        //finally return the unit made so it can be used to make the group
        unit

      //Create enemy units
      val enemyList = read[Vector[Vector[(String,Int,(Int,Int))]]](fmap("enemies"))
      val enemies = enemyList.map(makeGroup(_, Team.Enemy))
      //Create ally units
      val allyList = read[Vector[Vector[(String,Int,(Int,Int))]]](fmap("allies"))
      val allies = allyList.map(makeGroup(_, Team.Ally))
      //Create joining player characters
      val joiningList = read[Vector[(String,Int,(Int,Int))]](fmap("joining"))
      val joining = joiningList.map(makeUnit(_))
      // Conditions
      val winCondition = readCondition(fmap("clear").obj)
      val loseConditions = Vector(Route(Team.Player)) //!!! reading lose conditions unimplemented
      // Events
      val mapEvents = fmap("events").obj
      val events = mapEvents.flatMap(n=>
        val currentMap = n._2.obj
        readEvent(currentMap)
      ).toVector
      // dummy player
      val dummyplayer = Organization(Vector(),Vector(),Inventory(0),Team.Player)
      val rotation = read[Int](fmap("rotation"))
      val turn = read[Int](fmap("turnNumber"))
      val deployment = read[Vector[(Int,Int)]](fmap("deploy"))
      // Finally create the FieldMap itself
      name -> FieldMap(enemies,
                       allies,
                       grid,
                       dummyplayer,
                       winCondition,
                       loseConditions,
                       rotation,
                       turn,
                       deployment,
                       joining,
                       events
              )
    nameToMap.toMap


  def readCondition(map: LinkedHashMap[String, Value]): Condition =
    read[String](map("title")) match
      case "survive" => Survive(read[Int](map("limit")))
      case "kill" => Kill(read[Vector[String]](map("target")))
      case _ => Route(Team.Enemy)

  def readEvent(map: LinkedHashMap[String, Value]): Option[Event] =
    read[String](map("title")) match
      case "reinforcement" =>
        val team = read[String](map("team")) match
          case "Player" => Team.Player
          case "Enemy" => Team.Enemy
          case "Ally" => Team.Ally
        val unitsCoords: Vector[(String,(Int,Int))] = read[Vector[(String,(Int,Int))]](map("units"))
        val bunch = unitsCoords.map((u,c) =>(
          Units(DataLibrary.characters(u).copyMe,         // Find the character
                DataLibrary.inventories("inventory_"+u).copyMe)  // Find the inventory
                .copyMe,
          c)
        )
        val turns = read[Vector[Int]](map("turns"))
        Some(Reinforcement(bunch, team, turns))
      case "message" =>
        val lines = read[Vector[String]](map("lines"))
        val condition = readCondition(map("when").obj)
        Some(Speech(lines, condition))
      case _ => None

      //where first lochagos? whyaynia wrong spot??