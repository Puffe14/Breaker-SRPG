package components

class Grid(tiles: Vector[Tile], row: Int, column: Int, elevation: Vector[Int]):
  def allTiles = tiles

  def visibleTiles: Vector[Tile] =
    tiles

  def tileAt(x: Int, y: Int): Option[Tile] =
    val atPos = x + y*row
    if x>=0 && y>=0 && x < row && y < column then
     Some(tiles(atPos))
    else None

  def givePostitionToTiles() =
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

  def neighbors(chosenTile: Tile): Vector[Tile] =
    val x = chosenTile.pos(0)
    val y = chosenTile.pos(1)
    /*val found = tiles.map(Some(_)).filter(n =>
      n == tileAt(x-1, y)||
      n == tileAt(x+1, y)||
      n == tileAt(x, y-1)||
      n == tileAt(x, y+1))
    */
    var total: Vector[Tile] = Vector()
    //found.foreach(n => total = total ++ n)*/
    total = tiles.filter(n =>
      val nxy = (n.pos(0), n.pos(1))
      nxy == (x-1, y)||
      nxy == (x+1, y)||
      nxy == (x, y-1)||
      nxy == (x, y+1))
    total

end Grid