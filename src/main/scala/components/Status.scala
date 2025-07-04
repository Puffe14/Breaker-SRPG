package components

import scalafx.scene.image.Image
import java.io.FileInputStream

trait Status(name: String, file: String):
  def fileName = file
  def description = "a status effect"
  override def toString = name
  val image: Image =
    new Image(new FileInputStream("src/main/scala/resources/images/" + file + ".png"))
end Status


object Confused extends Status("confused","status_confused"):
  override def description = "The character is confused and acts at random."
object Stunned extends Status("stunned","status_stunned"):
  override def description = "The character is stunned and can't move or jump."


enum Part:
  case Head, Torso, Arms, Legs, AnyPart
  def similarTo(other: Part): Boolean =
    this.getClass == other.getClass
  def partType = this.getClass
  def name = this.toString.toLowerCase
end Part
