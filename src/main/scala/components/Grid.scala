package components

class Grid(tiles: Vector[Tile], rows: Int, column: Int, elevation: Vector[Int]):
  def visibleTiles: Vector[Tile] = Vector()
  def tileAt(x: Int, y: Int): Tile = visibleTiles.head
end Grid