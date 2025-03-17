package components
import scala.collection.mutable

class FieldMap(enemies: Vector[Group],
               allies: Vector[Group],
               grid: Grid,
               var player: Organization,
               clearCondition: String,
               var rotation: Int):
  def allCharacters: Vector[Units] =
    grid.unitsOnTiles
  def groups: Vector[Group] = enemies ++ allies
  def setPlayer(org: Organization) =
    player = org
  def setLeaders() = ()
  def isCleared: Boolean =
    clearCondition match
      case "defend" => false
      case "defeat" => false
      case "route"  => grid.unitsOnTiles.forall(_.team!="enemy")
      case _ => false
  def theGrid = grid
  def SAVEABLE: String = ""

  def currentRotation = rotation
  def setRotation(newDir: Int) = rotation = newDir%4


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


  //Methdos for determining which tiles a unit could occupy with current MOVE

  def moveCheck(moveLeft: Double, tile: Tile, types: Vector[String], team: String, elevation: Int, jump: Int): Vector[Tile] =

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

  /**Checks who can be attacked on a particular location.
   * Returns the unit and distance from checked tile.*/
  def attackRangeUnitsAt(mover: Units, tile: Tile): Set[(Units,Int,Tile)] =

    val (minR, maxR) = mover.Range
    var unitsFound = Set[Units]()
    for i <- minR to maxR do
      unitsFound = unitsFound ++ grid.unitsFromTiles(grid.tileInRangeFrom(tile,i)).toSet - mover
    unitsFound.map(unit => (unit, unitDistanceFrom(tile, unit), tile))

end FieldMap