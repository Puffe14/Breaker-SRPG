package components

trait Action:
  def play(): Vector[String]
end Action

class Move(unit: Units) extends Action:
  def play() =

    Vector()
end Move