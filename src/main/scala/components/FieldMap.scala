package components

//noinspection AccessorLikeMethodIsUnit
class FieldMap(enemies: Vector[Group],
               allies: Vector[Group],
               grid: Grid,
               player: Organization,
               clearCondition: String):
  def allCharacters: Vector[Unit] = Vector()
  def groups: Vector[Group] = Vector()
  def setPlayer(org: Organization) = ()
  def setLeaders() = ()
  def isCleared() = ()
  def SAVEABLE: String = ""
end FieldMap