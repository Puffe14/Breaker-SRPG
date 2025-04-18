package components

class Explain(val name: String = ""):
  var series: Vector[AniSeries] = Vector()
  var acts: Vector[Act] = Vector()
  var totalTime = 0

  //Create a new act and add it to the list
  def addAnimation(unit: Units, animation: Animation, time: Int) =
    val start = totalTime
    totalTime += time
    acts = acts ++ Vector(Act(unit, animation, start, totalTime))
  //Create an act with a message
  def addAnimation(unit: Units, animation: Animation, time: Int, message: String) =
    val start = totalTime
    totalTime += time
    acts = acts ++ Vector(Act(unit, animation, start, totalTime, message))
end Explain


class Act(unit: Units, animation: Animation, start: Int, end: Int, val msg: String = ""):
  def actor = unit
  def frame = animation
  def done(time: Int) = time > end
  def show(time: Int) = time > start
  override def toString = unit.name + ": " + animation.toString + " at " + end


class AniSeries(val unit: Units):
  val animations: Vector[Animation] = Vector()
  override def toString = unit.name + ": " + animations.mkString(", ")


enum Animation:
  case Attack, Critical, Evade, Miss, Hurt, Idle, Stance