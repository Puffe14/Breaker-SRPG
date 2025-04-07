package components

class Grid(tiles: Vector[Tile], row: Int, column: Int, elevation: Vector[Int]):
  def allTiles = tiles

  def visibleTiles(direction: Int): Vector[Tile] =
    tiles.filterNot(tileHidden(_, direction))

  def tileAt(x: Int, y: Int): Option[Tile] =
    val atPos = x + y*row
    if x>=0 && y>=0 && x < row && y < column then
     Some(tiles(atPos))
    else None

  def givePositionToTiles() =
    var i = 0
    if tiles.size == elevation.size then
      while i < row*column do
        tiles(i).setPos(i%row, i/row, elevation(i))
        i+=1
    else
      println("Incorrect size of elevation vector.")

  def occupiables: Vector[Occupiable] =
    allTiles
      .collect { case a: Occupiable => a }

  def unitsOnTiles: Vector[Units] =
    occupiables.flatMap(_.occupantOnTile)
    
  def tilesWithUnits: Vector[Occupiable] =
    occupiables.filter(_.occupied)

  def unitsFromTiles(tileList: Vector[Tile]): Vector[Units] =
    tileList.collect{case a: Occupiable => a}.filter(_.occupied).flatMap(_.occupantOnTile)

  
  def neighbors(chosenTile: Tile): Vector[Tile] =
    val x = chosenTile.pos(0)
    val y = chosenTile.pos(1)
    /*val found = tiles.map(Some(_)).filter(n =>
      n == tileAt(x-1, y)||
      n == tileAt(x+1, y)||
      n == tileAt(x, y-1)||
      n == tileAt(x, y+1))*/
    var total: Vector[Tile] = Vector()
    //found.foreach(n => total = total ++ n)*/
    total = tiles.filter(n =>
      val nxy = (n.pos(0), n.pos(1))
      nxy == (x-1, y)||
      nxy == (x+1, y)||
      nxy == (x, y-1)||
      nxy == (x, y+1))
    total

  /*      directions  0, 1, 2, 3
        (11)          (13)            (33)           (31)
  //  (21)(12)      (12)(23)        (23)(32)       (32)(21)
    (31)(22)(13)  (11)(22)(33)    (13)(22)(31)   (33)(22)(11)
      (32)(23)      (21)(32)        (12)(21)       (23)(12)
        (33)          (31)            (11)           (13)
  */

  def tileInFront(tile: Tile, dir: Int): Option[Tile] =
    val (x, y, z) = tile.pos
         if dir == 0 then tileAt(x+1, y+1)
    else if dir == 1 then tileAt(x+1, y-1)
    else if dir == 2 then tileAt(x-1, y-1)
    else if dir == 3 then tileAt(x-1, y+1)
    else None

  def tileHidden(tile: Tile, dir: Int): Boolean =
    tileInFront(tile, dir).forall(
      t => t.pos(2) > tile.pos(2) + 2
    )

  //positive means that it requires JUMP, negative might be used for something
  def elevationDifference(elevation: Int, tile: Tile): Int =
    tile.pos(2) - elevation

  //distance of tiles a to b based on their x and y
  def tileDistance(tiles: (Tile, Tile)): Int =
    val (a, b) = tiles
    val (ax, ay, az) = a.pos
    val (bx, by, bz) = b.pos
    ((ax-bx).abs+(ay-by).abs)

  def tileInRangeFrom(tile: Tile, range: Int): Vector[Tile] =
    allTiles.filter(tileDistance(_,tile)==range)

end Grid