package game
import components.*
import scala.util.Random


class Combat(selectedUnit: Units, targetUnit: Units, range: Int):
  val rules = Rules()

  def select = selectedUnit
  def target = targetUnit
  
  //Positive if attacker has more, negative if target
  def attackSpeedDifference: Int = selectedUnit.AS - targetUnit.AS
  def skillDifference: Int = selectedUnit.SK - targetUnit.SK
  def roll100: Int = Random().nextInt(100)
  def everyoneLived: Boolean = !selectedUnit.isDead && !targetUnit.isDead
  def inCounterRange: Boolean = targetUnit.weapon.forall(w => !between(range, w.range))

  //evalution methods
  private def lowest(evaluation: Int, minimum: Int): Int =
    if evaluation > minimum then evaluation
    else minimum
  private def between(int: Int, pair: (Int,Int)) =
    pair(0) <= int && int >= pair(1)

  //methods for seeing if a unit can still fight
  def targetCanAttack:   Boolean =
    targetUnit.weapon.forall(_.intact)   && targetAttacks > 0 && everyoneLived && inCounterRange
  def selectedCanAttack: Boolean =
    selectedUnit.weapon.forall(_.intact) && selectedAttacks > 0 && everyoneLived


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

  //method for the performing attacks
  def attack(attacker: Units, defender: Units, weapon: Weapon) =
    val isHit = roll100 < attacker.HI - defender.AV //if the attack hits

    if isHit then
      val isCritical = roll100 < attacker.CR //if a critical hit is rolled
      val isEffective = //if the attackers weapon has an effectiveness against defenders type
        attacker.weapon.forall(w => attacker.types.exists(w.effective.contains(_)))
      var damage = 0

      if      weapon.typing == "force" then
        damage = lowest(attacker.AT - defender.PD, 0)
      else if weapon.typing == "magic" then
        damage = lowest(attacker.AT - defender.MD, 0)
      if isEffective then damage *= rules.effectiveMultipllier
      if isCritical then damage *= rules.critMultiplier
      defender.takeDamage(damage)
      println(s"${attacker.name} hit ${defender.name} with $damage damage")
      weapon.spend(1)
    else
      println(s"${attacker.name} misses ${defender.name}")

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

  def play() =
    println(s"${selectedUnit.name} attacks ${targetUnit.name}")
    println(s"${selectedUnit.name} can $selectedCanAttack,  ${targetUnit.name} can $targetCanAttack")
    println(s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")

    //who attacks first
    if skillDifference < -rules.vantageDiff then
      targetStrikes()
    if attackSpeedDifference > rules.alacrityDiff then
      selectedStrikes()

    //attackers first
    selectedStrikes()

    //keep attacking until both run out of attack
    while (targetAttacks > 0 || selectedAttacks > 0) && everyoneLived do
      println(s"${selectedUnit.name} left $selectedAttacks,  ${targetUnit.name} left $targetAttacks")
      if selectedNext then
        selectedStrikes()
      else
        targetStrikes()
    if selectedUnit.isDead then println(s"${selectedUnit.name} died")
    else if targetUnit.isDead then println(s"${targetUnit.name} died")
    println("battle ends")

end Combat