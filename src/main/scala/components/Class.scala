package components
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or
import ujson.Value.Value
import upickle.core.LinkedHashMap


class Class(
  //Parameters needed to form a class.
  val className: String,
  val requiredLevel: Int,
  val classType: Vector[String],
  val classGrowth: Map[String, Int],
  val classStats: Map[String, Int],
  val classBuffs: Map[String, Int],
  val classDebuffs: Map[String, Int],
  val classRanks: Map[String, Int] = Map()):

  def name = className
  def stats = classStats
  //Methods that return the stats of the class in a useful form.
  def abilities: Vector[String] = classType
  def growths: Map[String, Int] = classGrowth
  def ranks: Map[String, Int] = classRanks
  //Buffs and debuffs givent to neaby units
  def buffs: Map[String, Int] = classBuffs
  def debuffs: Map[String, Int] = classDebuffs
  
  //Stats
  def move: Int = classStats("movement")
  def maxHp: Int = classStats("hitpoints")
  def str: Int =   classStats("strength")
  def mag: Int = classStats("magic")
  def skl: Int =   classStats("skill")
  def spd: Int = classStats("speed")
  def dfn: Int =   classStats("defence")
  def res: Int = classStats("resistance")
  def jump:Int = classStats("jump")
end Class



object ClassHandler:
  var lastData = getData
  def getData = ujson.read(or(os.pwd / RelPath(s"src/main/scala/resources/data/classes.json")))

  // return all the classes to be created
  def create: Map[String, Class] =
    val data = getData.obj
    val nameToClass = for (name, info) <- data yield
      name -> classRead(info.obj)
    nameToClass.toMap

  // From the given map, read it and create a Class based on it.
  def classRead(data: LinkedHashMap[String, Value]): Class =
    new Class(read[String](data("name")),
              read[Int](data("level")),
              read[Vector[String]](data("type")),
              read[Map[String,Int]](data("growth")),
              read[Map[String,Int]](data("stats")),
              read[Map[String,Int]](data("buffs")),
              read[Map[String,Int]](data("debuffs")),
              read[Map[String,Int]](data("ranks"))
        )