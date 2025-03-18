package game
import components.*

object AI:
  var game = Game()
  var currentUnit: Option[Units] = None
  def chooseActions() = ()
  def executeActions() =
      currentUnit.foreach(u =>
        var chosen: Vector[Action] = Vector()
        val actions = game.availableActions(u)
        if actions.nonEmpty then
          val act: Action = actions
                            .collect { case a: Combat => a }
                            .maxBy(n => n.forecast.aEV*3 - n.forecast.bEV)
          game.currentMap.foreach(a =>
            val move = Move(u, a)
            move.location = act.location
            chosen = Vector(move,act)
          )
          chosen.foreach(game.addToStack(_))
      )
  def nextGroup() = ()
  def setGroup() = ()
  def checkGroupCondition() = ()