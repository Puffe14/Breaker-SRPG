package components

class Explain:
  val name: String = ""
  var series: Vector[AniSeries] = Vector()
  var acts: Vector[Act] = Vector()

  //Create a new act and add it to the list
  def addAnimation(unit: Units, animation: Animation, time: Double) =
    acts = acts.updated(acts.length, Act(unit, animation, time))

  /*def addAnimation(unit: Units, animation: Animation) =
    //If this character doesn't have an animation yet
    if series.exists(_.unit!=unit) then
      series = series.updated(series.length, AniSeries(unit))*/
end Explain


class Act(unit: Units, animation: Animation, time: Double):
  override def toString = unit.name + ": " + animation.toString + " at " + time


class AniSeries(val unit: Units):
  val animations: Vector[Animation] = Vector()
  override def toString = unit.name + ": " + animations.mkString(", ")


enum Animation:
  case Attack, Critical, Evade, Miss, Hurt