package game
import components.*

class Rules {
  //limits for difference based activations in combat
  val vantageDiff = 9
  val alacrityDiff = 9
  val doubleDiff = 4
  val critMultiplier = 3
  val effectiveMultipllier = 2
  //inventory limits
  val playerStorageLimit = 50
  val unitInventoryLimit = 6
  //bonus
  val skillBonusHitRateForEffective = 20
  val skillHitRatePenaltyRatio = 2
  val typeAdvantageBonus = 15
  //leader
  val leaderBonus = Map[String, Int]("HI"->15,"AV"->15)
  //parts
  val allParts = Set(Part.Head, Part.Torso, Part.Arms, Part.Legs)
  //exp gain
  val baseKillExp = 40
  val expLvlDiffMult = 3
}