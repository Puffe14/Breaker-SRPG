package components

class Class(
  //Parameters needed to form a class.
  val className: String,
  val requiredLevel: Int,
  val classType: Vector[String],
  val classGrowth: Map[String, Int],
  val classStats: Map[String, Int],
  val classBuffs: Map[String, Int],
  val classDebuffs: Map[String, Int]):

  //Methods that return the stats of the class in a useful form.
  def abilities: Vector[String] = classType
  def growths: Map[String, Int] = classGrowth
  
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
end Class
