package components


class Group(var members: Vector[Units], var behaviour: Behaviour,
            var side: Team, var conditionMet: Boolean = false):
  def haveNotActed =
    members.filterNot(m=>m.turnOver||m.isDead)
  def doneActing =
    haveNotActed.isEmpty
  def conditionTrue: Boolean =
    conditionMet
  def changeSide(newSide: Team) = side = newSide
  def changeBehaviour(newBehaviour: Behaviour) =
    behaviour = newBehaviour
  def metCondition() =
    conditionMet = true
  def setLeader() =
    val leader = members.maxByOption(_.lvl)
    members.foreach(m => m.setLeader(leader))
  def reduceTemporary() =
    members.foreach(_.reduceTemporary())
end Group


enum Behaviour:
  //def a = ()
  case Agressive, Stand, OnSight, Reach, Control


//change unit and move methods to follow Team
enum Team:
  case Player, Enemy, Ally