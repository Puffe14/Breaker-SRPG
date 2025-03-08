package components
import scala.collection.mutable

class FieldMap(enemies: Vector[Group],
               allies: Vector[Group],
               grid: Grid,
               var player: Organization,
               clearCondition: String,
               rotation: Int):
  def allCharacters: Vector[Units] =
    grid.unitsOnTiles
  def groups: Vector[Group] = Vector()
  def setPlayer(org: Organization) =
    player = org
  def setLeaders() = ()
  def isCleared() = ()
  def theGrid = grid
  def SAVEABLE: String = ""


  //Methods for interfacing with units on field

  def tileOf(unit: Units): Option[Occupiable] =
    grid.occupiables
        .find(_.occupantOnTile == Some(unit))

  def moveTo(unit: Units, target: Tile) =
    val former = tileOf(unit)
    target match
      case o: Occupiable =>
        former.foreach(_.removeOccupant())
        o.addOccupant(unit)
      case _ => println(s"$target cannot be occupied")



  //Methdos for determining which tiles a unit could occupy with current MOVE

  def moveCheck(moveLeft: Double, tile: Tile, types: Vector[String], team: String): Vector[Tile] =

    def findSurrounding(thisOneOk: Boolean) =
      val accessibles = mutable.Buffer[Tile]()
      if thisOneOk then accessibles += tile
      val availableNeighbors = grid.neighbors(tile).collect { case a: Occupiable => a }
      availableNeighbors
        .foreach(accessibles ++= moveCheck(moveLeft-tile.moveReduction(types), _, types, team))
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

  def tilesVisible: Vector[Tile] =
    grid.visibleTiles(rotation)


  def movementRangeTiles(mover: Units): Set[Tile] =
    //find the location of the moving unit and find their info
    val locationTile = tileOf(mover)
    val movementRange = mover.MOVE
    val movementType = mover.types
    var tilesFound = Set[Tile]()
    locationTile.foreach( t =>
      tilesFound = moveCheck(movementRange, t, movementType, mover.team).toSet
    )
    tilesFound

end FieldMap