package components
import components.Part.Head
import components.Team.Ally
import game.Rules
val rules = Rules()

class Units(var character: Character):
  val unitsInventory = Inventory(rules.unitInventoryLimit)
  var leader: Option[Unit] = None
  var damageTaken: Int = 0
  var woundsTaken: Set[Part] = Set()
  var statusSet: Set[Status] = Set()
  var temporaryStats: Map[String, Int] = Map()
  var nearbyBonuses: Map[String, Int] = Map()
  var team: Team = Ally
  
  var moved = false
  var acted = false

  def name: String = character.name
  def weapon = unitsInventory.equippedWeapon
  def armor = unitsInventory.equippedArmors
  def medkit = unitsInventory.equippedMedkit
  def unitClass = character.currentClass
  def types = unitClass.classType
  def inventory = unitsInventory
  def setTeam(newTeam: Team) = team = newTeam

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
    
  def breakPiece(part: Part) =
    inventory.armors
      .find(_.bodyPart.similarTo(part))
      .foreach(breakArmor(_))

  def wounds: Set[Part] =
    woundsTaken
  def takeWound(wound: Part) =
    woundsTaken = woundsTaken + wound
  def healWound(wound: Part) =
    woundsTaken = woundsTaken - wound
  def status: Set[Status] =
    statusSet
  def takeStatus(effect: Status) =
    statusSet = statusSet + effect
  def healStatus(effect: Status) =
    statusSet = statusSet - effect

  //turn handling
  def endTurn() =
    moved = true
    acted = true
  def refresh() =
    moved = false
    acted = false
  def turnOver = acted
  def moveOver = moved

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
    wounds.map("wound "+_.name).toVector
    ++ armor.map("armor "+_.bodyPart.name)
    ++ status.map(_.fileName).toVector


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

  def useItem(item: Consumable) =
    item.utilize(this)

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
  def JUMP: Int = character.jump + bonus("jump")

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
        (if wounds.exists(p => p.similarTo(Head)) then 2 else 1)
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
