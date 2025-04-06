package game
import components.*

object IOHandler:
  //Read the classes (game term) from files and
  def buildClasses() =
    val classData = ClassHandler.getData
    DataLibrary.setClasses(ClassHandler.create)

  // POST CLASS BUILDING
  //Read the classes (game term) from files and
  def buildCharacters() =
    val characterData = CharacterHandler.getData
    DataLibrary.setCharacters(CharacterHandler.create)

  // Needs Group, Condition, Grid, Units
  def buildFieldMaps() = ()

end IOHandler

object DataLibrary:
  //Combines ALL classes (game term) read from files into a massive MAP(name -> Class)
  var classes: Map[String, Class] = Map()
  def setClasses(classMap: Map[String, Class]) =
    classes = classMap
  var characters: Map[String, Character] = Map()
  def setCharacters(characterMap: Map[String, Character]) =
    characters = characterMap
end DataLibrary