package components

case class Status(name: String, file: String):
  def fileName = file
  override def toString = name
end Status


enum Part:
  case Head, Torso, Arms, Legs, AnyPart
  def similarTo(other: Part): Boolean =
    this.getClass == other.getClass
  def partType = this.getClass
  def name = "head"
end Part
