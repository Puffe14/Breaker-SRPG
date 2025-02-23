package components

class Units(var character: Character):
  val inventory = Inventory(6)
  var leader: Option[Unit] = None
  var damageTaken: Int = 0
  var woundsTaken: Set[String] = Set()
  var temporaryStats: Map[String, Int] = Map()
  var nearbyBonuses: Map[String, Int] = Map()

  def name: String = character.name

  //Check if the unit has been killed.
  def isDead = !(HP > 0)

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

  def wounds: Set[String] =
    woundsTaken

  def takeWound(wound: String) =
    woundsTaken = woundsTaken ++ wounds

  def healWound(wound: String) =
    woundsTaken = woundsTaken -- wounds


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


  //bonuses

  def bonus(status: String): Int =
    var total = 0
    val armors = inventory.equippedArmors
    val weapon = inventory.equippedWeapon

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
    total += temporaryStats(status)
    total += nearbyBonuses(status)
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

  def loot: Vector[Item] =
    inventory.equippedArmors

  //effective totals


  //Unit combat stats

  //Current and max HP
  def HP: Int = character.maxHp - damageTaken
  def MaxHP: Int = character.maxHp

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
      case Some(magical) if magical.typing == "magic" =>
         magical.power + character.mag + bonus("magic")
      case Some(physical) if physical.typing == "force" =>
         physical.power + character.str + bonus("strength")
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
      return character.spd - n.weight + bonus("speed")
    )
    0

  //Physical defence
  def PD: Int =
    inventory.equippedWeapon.foreach(n =>
      return character.dfn + bonus("defence")
    )
    0

  //Magical defence
  def MD: Int =
    inventory.equippedWeapon.foreach(n =>
      return character.res + bonus("resistance")
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
