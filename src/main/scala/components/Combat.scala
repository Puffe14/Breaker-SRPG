package components

import components.*
import components.Part.AnyPart
import components.Animation.*
import game.Rules

import scala.collection.mutable
import scala.util.Random

  //evalution methods
  def lowest(evaluation: Int, minimum: Int): Int =
    if evaluation > minimum then evaluation
    else minimum
  def highest(evaluation: Int, maximum: Int): Int =
    if evaluation < maximum then evaluation
    else maximum
  def between(int: Int, pair: (Int,Int)) =
    pair(0) >= int && int <= pair(1)
  private def quick(unit: Units): Boolean =
   unit.weapon.forall(_.quick)
  def predictDmg(attacker: Units, defender: Units): Int =
    var damage = 0
    attacker.weapon.foreach( weapon =>
      val isEffective = //if the attackers weapon has an effectiveness against defenders type
         defender.types.exists(weapon.effective.contains(_))
      if      weapon.typing == "force" then
        damage = lowest(attacker.AT - defender.PD, 0)
      else if weapon.typing == "magic" then
        damage = lowest(attacker.AT - defender.MD, 0)
      if isEffective then damage *= rules.effectiveMultipllier
    )
    damage

val attackDuration = 20

class Combat(selectedUnit: Units, targetUnit: Units, range: Int) extends Action:
  val rules = Rules()
  var log: mutable.Buffer[String] = mutable.Buffer()
  
  def select = selectedUnit
  def target = targetUnit
  
  //Positive if attacker has more, negative if target
  def attackSpeedDifference: Int = selectedUnit.AS - targetUnit.AS
  def skillDifference: Int = selectedUnit.SK - targetUnit.SK
  def roll100: Int = Random().nextInt(100)
  def everyoneLived: Boolean = !selectedUnit.isDead && !targetUnit.isDead
  def inCounterRange: Boolean = targetUnit.weapon.forall(w => between(range, w.range))


  //methods for seeing if a unit can still fight
  def targetCanAttack:   Boolean =
    targetUnit.weapon.forall(_.intact)   && targetAttacks > 0 && everyoneLived && inCounterRange
  def selectedCanAttack: Boolean =
    selectedUnit.weapon.forall(_.intact) && selectedAttacks > 0 && everyoneLived
  def resetLog() =
    log = mutable.Buffer()


  //sets how many attacks the unit can perform

  var selectedAttacks =
    if selectedUnit.weapon.isEmpty ||
      !selectedUnit.weapon.forall(_.intact) then 0
    else if attackSpeedDifference > rules.doubleDiff then 2
    else 1//prevents from attaking if someone is not in range

  var targetAttacks =
    if targetUnit.weapon.isEmpty ||
      !targetUnit.weapon.forall(_.intact) ||
      !inCounterRange then 0
    else if attackSpeedDifference < -rules.doubleDiff then 2
    else 1

  def forecast =
    Forecast(selectedUnit,targetUnit,selectedAttacks,targetAttacks,skillDifference,attackSpeedDifference)

  def forecastString: String =
    val f = forecast
    val direction = f.arrow match
      case 1 => "->->"
      case 0 => "<-"
      case _ => "->"
    /*s"\n${select.name} x${f.aAtks}:\n DMG ${f.aDmg}, HIT ${f.aHit}, CRIT ${f.aCrit}" +
    s"\n$direction\n" +
    s"${target.name} x${f.bAtks}:\n DMG ${f.bDmg}, HIT ${f.bHit}, CRIT ${f.bCrit}"*/
    s"${select.name} x${f.aAtks}: DMG ${f.aDmg}, HIT ${f.aHit}, CRIT ${f.aCrit}" +
    s"   $direction   " +
    s"${target.name} x${f.bAtks}: DMG ${f.bDmg}, HIT ${f.bHit}, CRIT ${f.bCrit}"
  

  //method for the performing attacks
  def attack(attacker: Units, defender: Units, weapon: Weapon) =
    val isHit = roll100 < attacker.HI - defender.AV //if the attack hits
    explain.addAnimation(attacker,Stance,attackDuration/2)
    explain.addAnimation(attacker,Attack,attackDuration)
    if isHit then
      val isCritical = roll100 < attacker.CR //if a critical hit is rolled
      var damage = predictDmg(attacker, defender)
      if isCritical then damage *= rules.critMultiplier
      defender.takeDamage(damage)
      log += (s"${attacker.name} hit ${defender.name} with $damage damage")
      explain.addAnimation(defender,Hurt,attackDuration)
      weapon.spend(1)
    else
      log += (s"${attacker.name} misses ${defender.name}")
      explain.addAnimation(defender,Evade,attackDuration)
  end attack


  def playAttack(attacker: Units, defender: Units) =
    if everyoneLived then
      attacker.weapon.foreach(attack(attacker, defender, _))

  def targetStrikes() =
    if targetCanAttack then
      playAttack(targetUnit, selectedUnit)
      if targetUnit.weapon.forall(_.quick) && targetCanAttack then
        playAttack(targetUnit, selectedUnit)
      targetAttacks -= 1
      if !targetCanAttack then targetAttacks = 0

  def selectedStrikes() =
    if selectedCanAttack then
      playAttack(selectedUnit, targetUnit)
      if selectedUnit.weapon.forall(_.quick) && selectedCanAttack then
        playAttack(selectedUnit, targetUnit)
      selectedAttacks -= 1
      if !selectedCanAttack then selectedAttacks = 0

  //if the battle starter will be next
  def selectedNext =
    selectedAttacks > targetAttacks


  ////// plays out outcomes

  def play(): Explain =
    resetLog()
    log += (s"${selectedUnit.name} attacks ${targetUnit.name}")
    explain = Explain(s"${selectedUnit.name} attacks ${targetUnit.name}")
    log += (s"${selectedUnit.name} can $selectedCanAttack,  ${targetUnit.name} can $targetCanAttack")
    log += (s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")

    //who attacks first
    if skillDifference < -rules.vantageDiff then
      targetStrikes()
    if attackSpeedDifference > rules.alacrityDiff then
      selectedStrikes()

    //attackers first
    selectedStrikes()

    //keep attacking until both run out of attack
    while (targetAttacks > 0 || selectedAttacks > 0) && everyoneLived do
      log += (s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")
      if selectedNext then
        selectedStrikes()
      else
        targetStrikes()
    if selectedUnit.isDead then log += (s"${selectedUnit.name} died")
    else if targetUnit.isDead then log += (s"${targetUnit.name} died")
    log += ("battle ends")
    selectedUnit.endTurn()
    log.toVector
    explain
  end play

  override def toString =
    forecastString

end Combat



class Forecast(a: Units, b: Units, aAtkNum: Int, bAtkNum: Int, skill: Int, speed: Int):
  //Provides all calculated results for outside use.

  //predicted dmg
  val aDmg = predictDmg(a, b)
  val bDmg = predictDmg(b, a)
  //total hit and crit rate
  val (aHit, aCrit) = predictHitCrit(a, b)
  val (bHit, bCrit) = predictHitCrit(b, a)
  //the number of attacks including quick doubles
  val aAtks = predictAtks(a)
  val bAtks = predictAtks(b)
  //TOTAL dmg
  val aTotal = aDmg * aAtks
  val bTotal = bDmg * bAtks
  //Hidden expected value calculation, for AI (Let players make decisions, no value judgements)
  val aEV = EV(aDmg, aHit, aCrit, aAtks)
  val bEV = EV(bDmg, bHit, bCrit, bAtks)
  //the direction of attacks
  val arrow =
    if skill < -rules.vantageDiff && bAtkNum > 0 then
      0 //"<-"
    else if speed > rules.alacrityDiff then
      1 //"->->"
    else
      2 //"->"

  /** Estimated value for attacks. dmg is already calculated */
  private def EV(dmg: Int, hit: Int, crit: Int, times: Int): Double =
    var total = 0.0
    val dhit = hit/100.0
    val dcrit = crit/100.0
    val notcrit = 1 - dcrit
    //not critting possibilities
    total += notcrit*dmg
    //critting
    total += dcrit*dmg*rules.critMultiplier
    //hitting
    total *= dhit
    //damage hits or crits * chance for either * times executed
    total*times

  private def predictAtks(unit: Units) =
    val strikes = if quick(unit) then 2 else 1
    if unit == a then
      aAtkNum*strikes
    else if unit == b then
      bAtkNum*strikes
    else 0

  private def predictDmg(attacker: Units, defender: Units): Int =
    var damage = 0
    attacker.weapon.foreach( weapon =>
      val isEffective = //if the attackers weapon has an effectiveness against defenders type
         defender.types.exists(weapon.effective.contains(_))
      if      weapon.typing == "force" then
        damage = lowest(attacker.AT - defender.PD, 0)
      else if weapon.typing == "magic" then
        damage = lowest(attacker.AT - defender.MD, 0)
      if isEffective then damage *= rules.effectiveMultipllier
    )
    damage

  private def predictHitCrit(attacker: Units, defender: Units): (Int, Int) =
    var hit = 0
    var crit = 0
    attacker.weapon.foreach( weapon =>
      hit  = highest(lowest(attacker.HI - defender.AV, 0), 100)
      crit = highest(lowest(attacker.CR - defender.CA, 0), 100)
    )
    (hit, crit)
end Forecast



class Skill(selectedUnit: Units, targetUnit: Units, range: Int) extends Combat(selectedUnit, targetUnit, range):
  def skillEffect(attacker: Units, defender: Units) = ()

  def selectedAttemptSkill() =
    if everyoneLived then
      skill(selectedUnit, targetUnit)
      selectedAttacks -= 1
      if !selectedCanAttack then selectedAttacks = 0

  override def play(): Explain =
    resetLog()
    log += (s"${selectedUnit.name} ${this.toString}s ${targetUnit.name}")
    explain = Explain(s"${selectedUnit.name} ${this.toString}s ${targetUnit.name}")
    //selected treats target
    selectedAttemptSkill()
    log += ("battle ends")
    selectedUnit.endTurn()
    log.toVector
    explain

  // eri skillit objekteiks???, trait hit skill / no hit or sommin
  def skill(attacker: Units, defender: Units) =
    skillEffect(attacker, defender)
end Skill


class RollSkill(selectedUnit: Units, targetUnit: Units, range: Int) extends Skill(selectedUnit, targetUnit, range):
  override def skillEffect(attacker: Units, defender: Units) = ()
  //item spend loss based on its weight
  def skillCost: Int =
    var cost = 0
    selectedUnit.weapon.foreach(w=> cost = lowest(w.weight/2, 2))
    cost

  def spend(attacker: Units) = attacker.weapon.foreach(w => w.spend(skillCost))

  // eri skillit objekteiks???, trait hit skill / no hit or sommin
  override def skill(attacker: Units, defender: Units) =
    var bonusHit = 0
    var damage = 0
    //gives bonus to hitrate if the weapon is effective against enemy
    val isEffective = //if the attackers weapon has an effectiveness against defenders type
        attacker.weapon.forall(w => attacker.types.exists(w.effective.contains(_)))
    if isEffective then bonusHit += rules.skillBonusHitRateForEffective
    //checks if the attack hits, hitrate / ratio  - avoid + bonus
    val isHit = roll100 < attacker.HI / rules.skillHitRatePenaltyRatio - defender.AV + bonusHit
    explain.addAnimation(attacker,Stance,attackDuration/2)
    explain.addAnimation(attacker,Attack,attackDuration)
    //If the skill requires a hit check
    if isHit then
      skillEffect(attacker, defender)
      spend(attacker)
      explain.addAnimation(defender,Hurt,attackDuration)
    else
      log += s"${attacker.name} misses ${defender.name}"
      explain.addAnimation(defender,Evade,attackDuration)
end RollSkill


class Heal(selectedUnit: Units, targetUnit: Units, range: Int, medkit: Medkit) extends Skill(selectedUnit, targetUnit, range):
  override def skillEffect(attacker: Units, defender: Units) =
    val damage = lowest(attacker.HL + medkit.heal, 0)
    log += (s"${attacker.name} heals ${defender.name} with $damage")
    defender.healDamage(damage)
    explain.addAnimation(attacker,Attack,attackDuration)
    explain.addAnimation(defender,Hurt,attackDuration)
end Heal


class Treat(selectedUnit: Units, targetUnit: Units, range: Int, medkit: Medkit, part: Part) extends Skill(selectedUnit, targetUnit, range):
  override def skillEffect(attacker: Units, defender: Units) =
    log += (s"${attacker.name} treats ${defender.name}'s $part")
    target.healWound(part)
    explain.addAnimation(attacker,Attack,attackDuration)
    explain.addAnimation(defender,Hurt,attackDuration)
end Treat


class Wound(selectedUnit: Units, targetUnit: Units, range: Int, part: Part) extends RollSkill(selectedUnit, targetUnit, range):
  override def play(): Explain =
    resetLog()
    log += (s"${selectedUnit.name} wound attacks ${targetUnit.name}")
    log += (s"${selectedUnit.name} can $selectedCanAttack,  ${targetUnit.name} can $targetCanAttack")
    log += (s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")
    explain = Explain(s"${selectedUnit.name} wound attacks ${targetUnit.name}")

    //selected attempts break first
    selectedAttemptSkill()
    //target counters once if possible
    targetStrikes()
    log += ("battle ends")
    selectedUnit.endTurn()
    log.toVector
    explain

  override def skillEffect(attacker: Units, defender: Units) =
    log += (s"${attacker.name} wounds ${defender.name}'s $part")
        target.takeWound(part)
end Wound


class Break(selectedUnit: Units, targetUnit: Units, range: Int, part: Part) extends RollSkill(selectedUnit, targetUnit, range):
  override def play(): Explain =
    resetLog()
    log += (s"${selectedUnit.name} break attacks ${targetUnit.name}")
    log += (s"${selectedUnit.name} can $selectedCanAttack,  ${targetUnit.name} can $targetCanAttack")
    log += (s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")
    explain = Explain(s"${selectedUnit.name} break attacks ${targetUnit.name}")

    //selected attempts break first
    selectedAttemptSkill()
    //target counters once if possible
    targetStrikes()
    log += ("battle ends")
    selectedUnit.endTurn()
    log.toVector
    explain

  override def skillEffect(attacker: Units, defender: Units): Unit =
    log += (s"${attacker.name} breaks ${defender.name}'s $part")
    target.inventory.equippedArmors.find(_.bodyPart.similarTo(part)).foreach(target.breakArmor(_))

end Break