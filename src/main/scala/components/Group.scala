package components


class Group(var members: Vector[Units], var behaviour: Behaviour,
            var side: Team, var conditionMet: Boolean = false):
  // On group creation
  members.foreach(_.setTeam(side))

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
    val leader = members.filter(_.isAlive).maxByOption(_.lvl)
    members.foreach(m =>
                    m.setLeader(leader)
                    m.unstun())
  def handleLeader() =
    if side!=Team.Player && members.headOption.forall(_.leader.isEmpty) then
      setLeader()
    else if leaderDead then
      members.foreach(_.setLeader(None))
  def reduceTemporary() =
    members.foreach(_.reduceTemporary())
  def leaderDead = side!=Team.Player && members.headOption.forall(_.leader.forall(_.isDead))
  def stunLeaderless() =
    if leaderDead then
      members.foreach(_.stun())
end Group


enum Behaviour:
  case Agressive, Stand, OnSight, Reach, Control, Erratic


//change unit and move methods to follow Team
enum Team:
  case Player, Enemy, Ally