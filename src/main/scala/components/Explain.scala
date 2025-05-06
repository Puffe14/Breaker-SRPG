package components

class Explain(val name: String = ""):
  var series: Vector[AniSeries] = Vector()
  var acts: Vector[Act] = Vector()
  var lines: Vector[Dialogue] = Vector()
  var lineNumber: Int = 0
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

  //Create a new dialogue and add it to the list
  def addDialogoue(line: String) = //, face: Image) =
    lines = lines.appended(Dialogue(line))

  //returns the current dialogue
  def dialogue: Dialogue = lines(lineNumber)

  //Advance dialogue if there is some left. Return false if not.
  def advanceDialogue(): Boolean =
    //only increase the linen number if there are lines left to see
    if dialogueNotOver then
      lineNumber += 1
      true
    else
      false

  def dialogueNotOver: Boolean =
    lines.isEmpty && lineNumber < lines.size - 1

end Explain


class Dialogue(val line: String)//, val pic: Image)


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