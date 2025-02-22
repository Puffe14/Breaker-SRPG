package components

class Units(var character: Character):
  val inventory = Inventory(6)
  var leader: Option[Unit] = None
  var damageTaken: Int = 0


  def HP = character.maxHp - damageTaken

  //Check if the unit has been killed.
  def isDead =
    HP > 0
    
end Units
