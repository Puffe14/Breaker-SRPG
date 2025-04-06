package components
import components.Team.*

import scala.collection.mutable

class FieldMap(enemies: Vector[Group],
               allies: Vector[Group],
               grid: Grid,
               var player: Organization,
               clearCondition: Condition,
               loseConditions: Vector[Condition],
               var rotation: Int,
               turnNumber: Int):
  def allCharacters: Vector[Units] =
    grid.unitsOnTiles
  def groups: Vector[Group] = enemies ++ allies ++ Vector(player.group)
  def setPlayer(org: Organization) =
    player = org
  def setLeaders() =
    val leadGroups = groups.filter(_.side==Enemy)
    leadGroups.foreach(_.setLeader())
  def isCleared: Boolean =
    clearCondition.met(this)
  def isLost: Boolean =
    loseConditions.exists(_.met(this))
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
      case _ => println(s"$target cannot be occupied")

  def clearDead() =
    grid.tilesWithUnits.filter(_.occupantOnTile
                       .forall(_.isDead))
                       .foreach(_.removeOccupant())

  def unitsOnTeam(team: Team) =
    allCharacters.filter(_.team == team)

  
  //Gives stat bonuses from tile, aura buffs, and debuffs
  def giveBonuses() =
    theGrid.tilesWithUnits.foreach(t =>
      t.occupantOnTile.foreach(u =>
        calculateBonus(u, t)
      )
    )

  //Has to be able to this for current unit when moving without recalculating EVERYONE
  def calculateBonus(unit: Units, tile: Occupiable) =
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


  //Methdos for determining which tiles a unit could occupy with current MOVE

  def moveCheck(moveLeft: Double, tile: Tile, types: Vector[String], team: Team, elevation: Int, jump: Int): Vector[Tile] =

    def findSurrounding(thisOneOk: Boolean) =
      val accessibles = mutable.Buffer[Tile]()
      if thisOneOk then accessibles += tile
      val availableNeighbors = grid.neighbors(tile)
                                   .collect { case a: Occupiable => a }
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
      case u: Unoccupiable if u.canFlyOver && types.contains("flyer") =>
        findSurrounding(false)
      //If other checks fail
      case _ =>
        Vector()

  end moveCheck


  //Useful methods for finding stuff

  def tilesVisible: Vector[Tile] =
    grid.visibleTiles(rotation)

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

  def statusRangeUnitsFor(selected: Units): Set[Units] =
    val locationTile = tileOf(selected)
    val (minR, maxR) = (1, 2)
    var unitsFound = Set[Units]()
    locationTile.foreach( t =>
      for i <- minR to maxR do
        unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(t,i)).toSet - selected
    )
    unitsFound

  /** Checks who can be attacked on a particular location.
   *  Returns the unit and distance from checked tile.    */
  def attackRangeUnitsAt(mover: Units, tile: Tile, range: (Int, Int)): Set[(Units,Int,Tile)] =
    val (minR, maxR) = range
    var unitsFound = Set[Units]()
    for i <- minR to maxR do
      unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(tile,i)).toSet - mover
    unitsFound.map(unit => (unit, unitDistanceFrom(tile, unit), tile))

end FieldMap