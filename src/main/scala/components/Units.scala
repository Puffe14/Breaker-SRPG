package components
import components.Part.Head
import components.Team.*
import game.Rules
val rules = Rules()
val parts = rules.allParts

class Units(var character: Character, val unitsInventory: Inventory = Inventory(rules.unitInventoryLimit)):
  var unitsLeader: Option[Units] = None
  var damageTaken: Int = 0
  var woundsTaken: Set[Part] = Set()
  var statusSet: Set[Status] = Set()
  var temporaryStats: Map[String, Int] = Map()
  var nearbyBonuses: Map[String, Int] = Map()
  var side: Team = Player
  
  var moved = false
  var acted = false

  //GUI STUFF
  var aniInt = 0
  def setAniInt(int: Int) = aniInt = int
  def frame = aniInt

  def name: String = character.name
  def weapon = unitsInventory.equippedWeapon
  def armor = unitsInventory.equippedArmors
  def consumables = unitsInventory.consumables
  def medkit = unitsInventory.equippedMedkit
  def leader = unitsLeader
  def team = side

  def usableWeapons =
    unitsInventory.weapons // of the weapons in inventory
                  .filterNot(w=>character.ranks.get(w.wpntyping) // which rank matches type?
                                      .forall(_<w.rank))     // if rank is high enough, can use.
  def usableMedkits = unitsInventory.medkits
  def equippables = usableWeapons ++ usableMedkits ++ armor
  def usableWeaponsAt(distance: Int) =
    usableWeapons.filter(w => w.range(0)<=distance && w.range(1)>=distance)
  def usableMedkitsAt(distance: Int) =
    usableMedkits.filter(m => m.range(0)<=distance && m.range(1)>=distance)

  def unitClass = character.currentClass
  def types = unitClass.classType
  def inventory = unitsInventory
  def setTeam(newTeam: Team) = side = newTeam
  def setLeader(newLeader: Option[Units]) = unitsLeader = newLeader

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
  def woundableParts: Set[Part] =
    parts--breakableParts--wounds
  def breakableParts: Set[Part] =
    armor.map(_.bodyPart).toSet
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
  def hasStatus(effect: Status): Boolean =
    status.exists(_.isInstanceOf[effect.type])

  //turn handling
  def endTurn() =
    moved = true
    acted = true
  def cancelMove() =
    moved = false
  def moves() =
    moved = true
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
  def resetNearbyBonus() = setNearbyBonus(Map())


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
    inventory.clean()

  def equip(item: Item) =
    item match
      case weapon: Weapon =>
        unitsInventory.equipWeapon(weapon, false)
      case medkit: Medkit =>
        unitsInventory.equipMedkit(medkit, false)
      case armor: Armor =>
        unitsInventory.equipArmor(armor, false)
      case _ =>

  def toggleEquip(item: Item) =
    item match
      case weapon: Weapon =>
        unitsInventory.equipWeapon(weapon, true)
      case medkit: Medkit =>
        unitsInventory.equipMedkit(medkit, true)
      case armor: Armor =>
        unitsInventory.equipArmor(armor, true)
      case _ =>

  def equipFirst() =
    usableWeapons.headOption.foreach(equip(_))
    usableMedkits.headOption.foreach(equip(_))
    unitsInventory.armors.foreach(equip(_))

  def discard(item: Item) =
    unitsInventory.remove(Some(item))

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

  def giveExp(gained:Int) =
    character.expTrack( if gained > 0 then gained else 0 )

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
      case Some(magical) if magical.dmgtyping == "magic" =>
         magical.power + mag + bonus("AT")
      case Some(physical) if physical.dmgtyping == "force" =>
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
      return skl - n.weight/3 + bonus("SK")
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

  def hpMhp = s"$HP/$MaxHP"
  def lvl = character.level
  def exp = character.exp
  def lvlExp = s"LVL: $lvl, EXP: $exp"

  def statsStaticVector: Vector[(String, Int)] =
    Vector(
      ("str", str),
      ("mag", mag),
      ("skl", skl),
      ("spd", spd),
      ("dfn", dfn),
      ("res", res)
    )
  def statsPersonalsVector: Vector[(String, Int)] =
      statsStaticVector ++ Vector(("mhp ", MaxHP))
  def statsUnitVector: Vector[(String, Int)] =
       Vector(("move", MOVE), ("jump", JUMP))++statsStaticVector
  def statsCombatVector: Vector[(String, Int)] =
    Vector(
      ("AT", AT),
      ("HI", HI),
      ("CR", CR),
      ("AS", AS),
      ("SK", SK),
      ("PD", PD),
      ("MD", MD),
      ("AV", AV),
      ("CA", CA)
    )

  override def toString =
    name + s" $HP/$MaxHP  MV: $MOVE\n" +
    " Combat:\n" + s"  AT: $AT, HI: $HI, CR: $CR \n  AS: $AS, SK: $SK \n  PD: $PD, MD: $MD, AV: $AV, CA: $CA \n" +
    " Items: " + unitsInventory.toString()

end Units
