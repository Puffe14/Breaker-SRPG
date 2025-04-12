package components
import game.DataLibrary
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or

import scala.collection.mutable
import scala.util.Random










class Character(
   //character parameters
   val myName: String,
   var currentClass: Class,
   val possibleClass: Vector[Class],
   var level: Int = 1,
   var exp: Int = 0,
   val growths: Map[String, Int],
   var stats:  Map[String, Int]):

  //Methods for effecting characters!
  def swapClass(newClass: Class) =
    currentClass = newClass

  //!!! to an event that give message as a legible string
  //Increase experience and handle if reaches lvlup
  def expTrack(increase: Int): Vector[String] =
    val message: mutable.Buffer[String] = mutable.Buffer()
    exp += increase
    val lvlsUp = exp/100
    if lvlsUp > 0 then
      message += "LEVEL UP\n"
      for i <- 0 until lvlsUp do
        message += levelUp().map((k, v) => s"$k: $v").mkString(", ")
      exp = 0
    message.toVector
  end expTrack

  //Rolls growths for level-ups and collects them for display
  def levelUp(): Map[String, Int] =
    val levelUpsMap = mutable.Map[String, Int]()
    val roll = Random().nextInt(100)
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

  //Methods for returning important stats (characters own stats + class stats bases)
  def maxHp: Int = stats("hitpoints") + currentClass.maxHp
  def str: Int =   stats("strength") + currentClass.str
  def mag: Int = stats("magic") + currentClass.mag
  def skl: Int =   stats("skill") + currentClass.skl
  def spd: Int = stats("speed") + currentClass.spd
  def dfn: Int =   stats("defence") + currentClass.dfn
  def res: Int = stats("resistance") + currentClass.res
  def move:Int = currentClass.move
  def jump:Int = currentClass.jump
end Character





object CharacterHandler:
  var lastData = getData
  def getData = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/characters.json")))

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





class UnitStats:
  var hitpoints = 0
  var strength = 0
  var magic = 0
  var defence = 0
  var resistance = 0
  var speed = 0
  var skill = 0
  var movement = 0
  var jump = 0

  def setFromMap(map: Map[String, Int]) =
    hitpoints = map.getOrElse("hitpoints", 0)
    strength = map.getOrElse("strength", 0)
    magic = map.getOrElse("magic", 0)
    defence = map.getOrElse("defence", 0)
    resistance = map.getOrElse("resistance", 0)
    speed = map.getOrElse("speed", 0)
    skill = map.getOrElse("skill", 0)
    movement = map.getOrElse("movement", 0)
    jump = map.getOrElse("jump", 0)

  def toMap: Map[String, Int] =
    Map("hitpoints"->hitpoints,
        "strength"->strength,
        "magic"->magic,
        "defence"->defence,
        "resistance"->resistance,
        "skill"->skill,
        "speed"->speed,
        "movement"->movement,
        "jump"->jump,
        )