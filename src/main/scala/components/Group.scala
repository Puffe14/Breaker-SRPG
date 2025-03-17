package components


class Group(var members: Vector[Units], var behaviour: Behaviour,
            var side: Team, var conditionMet: Boolean = false):
  def conditionTrue: Boolean =
    conditionMet
  def changeSide() = ()
  def changeBehaviour(newBehaviour: Behaviour) =
    behaviour = newBehaviour
  def metCondition() =
    conditionMet = true
end Group


enum Behaviour:
  case Agressive, Stand, onSight, Reach


//change unit and move methods to follow Team
enum Team:
  case Player, Enemy, Ally