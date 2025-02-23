package components

class Units(var character: Character):
  val inventory = Inventory(6)
  var leader: Option[Unit] = None
  var damageTaken: Int = 0

  def name: String = character.name

  //Check if the unit has been killed.
  def isDead =
    HP > 0


  //bonuses
  def dfnBonus = 0
  def resBonus = 0
  
  //Unit combat stats
  //Current HP
  def HP: Int = character.maxHp - damageTaken
  //Move
  def MOVE: Int = character.move
  //Range
  def Range: (Int, Int) =
    val bonusRange = 0
    inventory.equippedWeapon.foreach(n =>
      return (n.range(0), n.range(1) + bonusRange)
    )
    (0,0)
  //Attack depends on if weapon is magical or physical
  def AT: Int =
    inventory.equippedWeapon match
      case magical: Weapon if magical.typing == "magic" =>
         magical.power + character.mag
      case physical: Weapon if physical.typing == "force" =>
         physical.power + character.str
      case _ => 0
  //Rate of critical hits
  def CR: Int =
    inventory.equippedWeapon.foreach(n =>
      return (n.crit + character.skl * 0.5).toInt
    )
    0
  //Attack speed: Total speed - weight
  def AS: Int =
    inventory.equippedWeapon.foreach(n =>
      return character.spd - n.weight
    )
    0
  //Physical defence
  def PD: Int =
    inventory.equippedWeapon.foreach(n =>
      return character.dfn + dfnBonus
    )
    0
  //Magical defence
  def MD: Int =
    inventory.equippedWeapon.foreach(n =>
      return character.res + resBonus
    )
    0
  //Hit rate
  def HI: Int =
    inventory.equippedWeapon.foreach(n =>
      return (character.skl + character.spd*0.5).toInt
    )
    0
  //Rate of avoiding attacks
  def AV: Int =
    (character.spd + character.skl*0.5).toInt
  //Rate of avoiding critical hits
  def CA: Int =
    inventory.equippedWeapon.foreach(n =>
      return (10 - n.weight).toInt
    )
    0
end Units
