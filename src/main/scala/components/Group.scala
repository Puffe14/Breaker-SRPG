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
end Group


enum Behaviour:
  //def a = ()
  case Agressive, Stand, OnSight, Reach


//change unit and move methods to follow Team
enum Team:
  case Player, Enemy, Ally