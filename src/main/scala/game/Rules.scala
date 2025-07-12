package game
import components.*

class Rules {
  val instantTrade = true
  //limits for difference based activations in combat
  val vantageDiff = 9
  val alacrityDiff = 9
  val doubleDiff = 4
  val critMultiplier = 3
  val effectiveMultipllier = 2
  //inventory limits
  val playerStorageLimit = 50
  val unitInventoryLimit = 6
  val lootDurabilityCost = 2 //by factor of 2
  //bonus
  val skillBonusHitRateForEffective = 20
  val skillHitRatePenaltyRatio = 2
  val wpnTypeAdvantageBonus = 15
  val statusAuraRange = (1, 2)
  //leader
  val leaderBonus = Map[String, Int]("HI"->15,"AV"->15)
  //parts
  val allParts = Set(Part.Head, Part.Torso, Part.Arms, Part.Legs)
  //exp gain
  val baseKillExp = 100
  val expLvlDiffMult = 3
  // advantage relations, scissors -> paper -> rock -> scissors
  val meleeAdvantages = Vector("Blunt","Long","Sharp","Blunt")
  //animation times
  val lvlAnimTime = 50
  val atkAnimTime = 40
}