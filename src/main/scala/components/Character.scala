package components

import scala.util.Random

class Character(
   //character parameters
   val myName: String,
   var currentClass: Class,
   val possibleClass: Vector[Class],
   val growths: Map[String, Int],
   var stats:  Map[String, Int]):

  //Methods for effecting characters!
  def swapClass(newClass: Class) =
    currentClass = newClass

  //Rolls growths for level-ups and collects them for display
  def levelUp(): Vector[Int] =
    var levelUps = Vector[Int]()
    val roll = Random().nextInt(100)
    if roll > growths("strength") then
      levelUps = levelUps.appended(roll/100 + 1)
    levelUps

  //adds Int to a stat
  def addToStat(which: String, amount: Int) =
    //Calculates the new total stat
    val newTotal = stats(which) + amount
    //Changes stats map to reflect change
    stats += (which -> newTotal)

  //Methods for returning important info
  def name = myName
  def chrClass = currentClass

  //Methods for returning important stats (characters own stats + class stats bases)
  def maxHp: Int = stats("hitpoints") + currentClass.maxHp
  def str: Int =   stats("strength") + currentClass.str
  def mag: Int = stats("magic") + currentClass.mag
  def skl: Int =   stats("skill") + currentClass.skl
  def spd: Int = stats("speed") + currentClass.spd
  def dfn: Int =   stats("defence") + currentClass.dfn
  def res: Int = stats("resistance") + currentClass.res
  def move:Int = currentClass.move
end Character
