package components
import game.Rules
val rules = Rules()

class Units(var character: Character):
  val unitsInventory = Inventory(rules.unitInventoryLimit)
  var leader: Option[Unit] = None
  var damageTaken: Int = 0
  var woundsTaken: Set[String] = Set()
  var temporaryStats: Map[String, Int] = Map()
  var nearbyBonuses: Map[String, Int] = Map()
  var team: String = ""

  def name: String = character.name
  def weapon = unitsInventory.equippedWeapon
  def armor = unitsInventory.equippedArmors
  def medkit = unitsInventory.equippedMedkit
  def unitClass = character.currentClass
  def types = unitClass.classType
  def inventory = unitsInventory
  def setTeam(newTeam: String) = team = newTeam

  //Check if the unit has been killed.
  def isDead = !isAlive
  def isAlive = HP > 0

  //Hurt or heal

  def takeDamage(amount: Int) =
    damageTaken += amount
    if damageTaken > HP then damageTaken = MaxHP

  def healDamage(amount: Int) =
    damageTaken -= amount
    if damageTaken < 0 then
      damageTaken = 0

  def breakArmor(piece: Armor) =
    piece.break()
    
  def breakPiece(part: String) =
    inventory.armors
      .find(_.partName == part)
      .foreach(breakArmor(_))

  def wounds: Set[String] =
    woundsTaken
  def takeWound(wound: String) =
    woundsTaken = woundsTaken + wound
  def healWound(wound: String) =
    woundsTaken = woundsTaken - wound


  //Change stat collections
  def addPermanent(which: String, amount: Int) =
    character.addToStat(which, amount)
  def addTemporaryStat(which: String, amount: Int) =
    temporaryStats += (which -> amount)
  //Set based on fieldmap locations
  def setNearbyBonus(newBonus: Map[String, Int]) =
    nearbyBonuses = newBonus

  def reduceTemporary() =
    //increases negative stats and reduces positive ones
    def towardZero(target: Int, amount: Int): Int =
      if target > 0 then target - amount
      else if target == 0 then target
      else target + amount
    temporaryStats = temporaryStats.keys.zip(
      temporaryStats.values.map(towardZero(_,1))
    ).toMap

  def givenNearbyBuffs: Map[String, Int] =
    character.currentClass.classBuffs
  def givenNearbyDebuffs: Map[String, Int] =
    character.currentClass.classDebuffs

  def statusList: Vector[String] =
    wounds.map("wound "+_).toVector
    ++ armor.map("armor "+_.partName)


  //bonuses

  def bonus(status: String): Int =
    var total = 0
    val armors = unitsInventory.equippedArmors
    val weapon = unitsInventory.equippedWeapon

    //adds bonuses given from armor
    val armorBonus =
      for i <- armors
      yield
        if i.bonusGiven.contains(status) then i.bonusGiven(status)
        else 0

    //adds bonuses given from weapon
    var weaponBonus = 0
    weapon.foreach(n=> if n.bonusGiven.contains(status) then weaponBonus = n.bonusGiven(status))

    //totals all the bonuses together
    total += armorBonus.sum
    total += weaponBonus
    temporaryStats.get(status).foreach(total+=_)
    nearbyBonuses.get(status).foreach(total+=_)
    total
  end bonus


  //Item and loot handling

  def useItem(item: Item) =
    item match
      //heals unit
      case co: Healing =>
        healDamage(co.heal)
        co.use()
      //gives a temporary boost to stats
      case bo: Booster =>
        bo.effects.foreach(n => addTemporaryStat(n(0), n(1)))
        bo.use()
      //gives a permanent increase to character stat
      case br: Brand   =>
        br.effects.foreach(n => addPermanent(n(0), n(1)))
        br.use()
      case _ =>

  def equip(item: Item) =
    item match
      case weapon: Weapon =>
        unitsInventory.equipWeapon(weapon)
      case armor: Armor =>
        unitsInventory.equipArmor(armor)
      case _ =>

  def loot: Vector[Item] =
    unitsInventory.equippedArmors

  //effective stats totals
  def hp =  character.maxHp + bonus("hitpoints")
  def str = character.str + bonus("strength")
  def mag = character.mag + bonus("magic")
  def skl = character.skl + bonus("skill")
  def spd = character.spd + bonus("speed")
  def dfn = character.dfn + bonus("defence")
  def res = character.res + bonus("resistance")


  //Unit combat stats

  //Current and max HP
  def HP: Int = hp - damageTaken
  def MaxHP: Int = hp

  //Move
  def MOVE: Int = character.move + bonus("move")

  //Jump
  def JUMP: Int = character.move + bonus("jump")

  //Range
  def Range: (Int, Int) =
    val bonusRange = 0
    unitsInventory.equippedWeapon.foreach(n =>
      return (n.range(0), n.range(1) + bonusRange)
    )
    (0,0)

  //Attack depends on if weapon is magical or physical
  def AT: Int =
    unitsInventory.equippedWeapon match
      case Some(magical) if magical.typing == "magic" =>
         magical.power + mag + bonus("AT")
      case Some(physical) if physical.typing == "force" =>
         physical.power + str + bonus("AT")
      case _ => 0

  //Rate of critical hits
  def CR: Int =
    unitsInventory.equippedWeapon.foreach(n =>
      return (n.crit + skl * 0.5).toInt + bonus("CR")
    )
    0

  //Attack speed: Total speed - weight
  def AS: Int =
    unitsInventory.equippedWeapon.foreach(n =>
      return spd - n.weight + bonus("AS")
    )
    0

  //Combat skill
  def SK: Int =
    unitsInventory.equippedWeapon.foreach(n =>
      return skl - n.weight + bonus("SK")
    )
    0

  //Physical defence
  def PD: Int =
    dfn + bonus("PD")

  //Magical defence
  def MD: Int =
    res + bonus("MD")

  //Hit rate
  def HI: Int =
    unitsInventory.equippedWeapon.foreach(n =>
      return (n.hit + (skl + spd*0.5).toInt + bonus("HI")) /
        (if wounds.contains("head") then 2 else 1)
    )
    0

  //Rate of avoiding attacks
  def AV: Int =
    (spd + skl*0.5).toInt  + bonus("AV")

  //Rate of avoiding critical hits
  def CA: Int =
    unitsInventory.equippedWeapon.foreach(n =>
      return 10 - n.weight + bonus("CA")
    )
    0

  //Amount of healing given
  def HL: Int =
    mag/3 + skl/2

  def shortInfo =
    name + s" $HP/$MaxHP\n" +
    " Weapon: " + weapon.getOrElse("None").toString
  
  override def toString =
    name + s" $HP/$MaxHP  MV: $MOVE\n" +
    " Combat:\n" + s"  AT: $AT, HI: $HI, CR: $CR \n  AS: $AS, SK: $SK \n  PD: $PD, MD: $MD, AV: $AV, CA: $CA \n" +
    " Items: " + unitsInventory.toString()

end Units
