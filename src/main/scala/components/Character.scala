package components
import game.{DataLibrary, ReadingHandler}
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or

import scala.collection.mutable
import scala.util.Random










case class Character(
   //character parameters
   val myName: String,
   var currentClass: Class,
   val possibleClass: Vector[Class],
   var level: Int = 1,
   var exp: Int = 0,
   val growths: Map[String, Int],
   var stats:  Map[String, Int]):

  //Methods for effecting characters

  def swapClass(newClass: Class) =
    currentClass = newClass

  //!!! could be changed to an event that give message as a legible string
  //Increase experience and handle if reaches lvlup
  def expTrack(increase: Int): Vector[String] =
    val message: mutable.Buffer[String] = mutable.Buffer(s"$name gained $increase exp")
    exp += increase
    val lvlsUp = exp/100
    if lvlsUp > 0 then
      level += 1
      message += "LEVEL UP\n"
      for i <- 0 until lvlsUp do
        message += levelUp().map((k, v) => s"$k: $v").mkString(", ")
      exp = 0
    message.toVector
  end expTrack

  //Rolls growths for level-ups and collects them for display
  def levelUp(): Map[String, Int] =
    val levelUpsMap = mutable.Map[String, Int]()
    def roll = Random().nextInt(100)
    val leveled = growths.toMap
    leveled.keys.foreach(stat =>
      val currentG = leveled(stat)
      if roll < currentG then
        val up = (currentG-1)/100 + 1
        levelUpsMap += (stat->up)
        addToStat(stat, up)
    )
    levelUpsMap.toMap

  //adds Int to a stat
  def addToStat(which: String, amount: Int) =
    //Calculates the new total stat
    val newTotal = stats(which) + amount
    //Changes stats map to reflect change
    stats += (which -> newTotal)

  //Methods for returning important info
  def name = myName
  def chrClass = currentClass
  def ranks = chrClass.ranks

  //Methods for returning important stats (characters own stats + class stats bases)
  def maxHp: Int = stats.getOrElse("hitpoints",0) + currentClass.maxHp
  def str: Int =   stats.getOrElse("strength",0) + currentClass.str
  def mag: Int =   stats.getOrElse("magic",0) + currentClass.mag
  def skl: Int =   stats.getOrElse("skill",0) + currentClass.skl
  def spd: Int =   stats.getOrElse("speed",0) + currentClass.spd
  def dfn: Int =   stats.getOrElse("defence",0) + currentClass.dfn
  def res: Int =   stats.getOrElse("resistance",0) + currentClass.res
  def move:Int = currentClass.move
  def jump:Int = currentClass.jump

  def copyMe: Character = this.copy(myName=myName)
end Character





object CharacterHandler extends ReadingHandler:
  val fileName = "characters.json"

  // return all the classes to be created
  def create: Map[String, Character] =
    val classFilenames = lastData.obj.keys
    val nameToCharacter = for name <- classFilenames yield
     name -> characterRead(name)
    nameToCharacter.toMap

  // From the last getData, read the particular map and create a Character based on it.
  def characterRead(filename: String): Character =
    val dt = lastData(filename)
    val classLibrary = DataLibrary.classes
    // Get class from data library and possible class list as well
    val characterClass = classLibrary(read[String](dt("class")))
    val characterClassOptions = read[Vector[String]](dt("classes")).map(classLibrary(_))
    // Create new instance of the class
    new Character(read[String](dt("name")),
                  characterClass,
                  characterClassOptions,
                  read[Int](dt("level")),
                  read[Int](dt("exp")),
                  read[Map[String,Int]](dt("growth")),
                  read[Map[String,Int]](dt("stats"))
        )