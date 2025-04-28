package components
import upickle.default.*
import os.{RelPath, pwd}
import os.read as or
import ujson.Value.Value
import upickle.core.LinkedHashMap
import game.ReadingHandler


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
  def move: Int = classStats.getOrElse("movement",0)
  def maxHp: Int = classStats.getOrElse("hitpoints",0)
  def str: Int =   classStats.getOrElse("strength",0)
  def mag: Int = classStats.getOrElse("magic",0)
  def skl: Int =   classStats.getOrElse("skill",0)
  def spd: Int = classStats.getOrElse("speed",0)
  def dfn: Int =   classStats.getOrElse("defence",0)
  def res: Int = classStats.getOrElse("resistance",0)
  def jump:Int = classStats.getOrElse("jump",0)
end Class



object ClassHandler extends ReadingHandler:
  val fileName: String = "classes.json"
  
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