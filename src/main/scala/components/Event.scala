package components

trait Event:
  //triggers when the conditions are met
  def trigger(): Boolean

class Reach(location: Vector[Tile], side: Team):
  def trigger(): Boolean =
    true

//Often used conditions
trait Condition:
  var fieldMap: Option[FieldMap] = None
  def met: Boolean

//Met when these characters are dead
class Kill(targets: Vector[Units]) extends Condition:
  def met: Boolean =
    targets.forall(_.isDead)

//Survive until a particular turn
class Survive(turnLimit: Int) extends Condition:
  def met: Boolean =
    fieldMap match
      case Some(field) => field.currentTurn == turnLimit
      case _ => false