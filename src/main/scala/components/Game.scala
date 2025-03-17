package components

import game.Combat


class Game:
  var currentMapNumber: Int = 0
  var currentMap: Option[FieldMap] = None
  var midBattle: Boolean = false
  var turnOf: Option[Team] = None
  var player: Option[Organization] = None
  var acting: Option[Units] = None
  var target: Option[Units] = None

  def availableActions(unit: Units): Vector[Action] =
    currentMap match
      case None => Vector()
      case Some(fm) =>
        fm.movementRangeTiles(unit) //On movement range tiles --Tiles
          .flatMap(tile=>(fm.attackRangeUnitsAt(unit,tile))) //Who can be attacked? --(who, from)
          .toSet //all available unit, distance, tile combinations
          .map((targetable, distance, currentTile) =>
            Combat(unit, targetable, distance))
          .toVector

  def initialize() =
    ()
  def handleTurn() =
    ()

end Game