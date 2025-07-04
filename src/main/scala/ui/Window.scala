package ui
import com.sun.media.jfxmedia.events.PlayerStateEvent.PlayerState
import components.*
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Scene
import scalafx.scene.canvas.*
import scalafx.scene.image.*
import scalafx.scene.input.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color.*
import scalafx.scene.text.Font
import scalafx.Includes.*
import scalafx.scene.paint.Color


class Window:
  val x = 10
  val y = 10
  val pad = 10
  val margin = 2
  val width = 200
  val height = 400
  val font = 20
  val fontType = "Liberation Mono"

  def teamToColor(team: Team) =
    team match
      case Team.Player => Blue
      case Team.Enemy => DarkRed
      case Team.Ally => DarkGreen
end Window


class MenuWindow(menu: Menu) extends Window:
  def hidden = menu.hidden
  def draw(g: GraphicsContext, offset: Int) =
    val xo = x + offset
    val fm = font+margin
    val pos = menu.select+1
    val height = pad * 2 + (menu.itemTitles.length+1) * fm
    //val width = width//pad * 2 + menu.itemTitles.maxBy(_.length).length*6

    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, y, width, height)

    //Selector outline
    g.font = Font(fontType, font)
    g.fill = Purple // Set the fill color.
    val textY = y+pad-margin+fm*pos
    g.fillRect(xo+pad-margin, textY,
               width-pad*2+margin, fm+margin*2)

    //Write nonsense
    g.fillText(menu.title, xo+pad, y+pad-margin + font)
    g.fill = Purple
    g.font = Font(fontType, font) // Set text size
    for i <- menu.itemTitles.indices do
      g.fill = Blue
      g.font = Font(fontType, font) // Set text size
      g.fillText(menu.itemTitles(i), xo+pad, y+pad-margin + font + fm*(i+1))

end MenuWindow


class MessageWindow() extends Window:
  override val width = 600-(font+margin)*2
  def draw(g: GraphicsContext, offset: Int, message: Vector[String]) =
    val xo = x + offset
    val yp = font*2+margin
    val fm = font+margin
    val height = message.size*fm+fm

    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, yp-font, width, height+font)

    //Display the message
    g.fill = Purple
    g.font = Font(fontType, font)
    g.fillText(message.mkString("\n"),fm,yp+margin)

end MessageWindow


class ForecastWindow(fc: Forecast) extends Window:
  override val width = 500
  override val height = 150
  def draw(g: GraphicsContext, offset: Int) =
    val xo = x + offset
    val yp = 300
    val fm = font+margin

    //Create background!
    g.fill = Gray // Set the fill color.
    //g.fillRect(xo, yp, width, height)

    //Face off character windows
    MiniUnitWindow(fc.a).draw(g, 100, 220, fc.a.hpMhp, fc.aDmg, fc.aHit, fc.aCrit)
    MiniUnitWindow(fc.b).draw(g, 300, 220, fc.b.hpMhp, fc.bDmg, fc.bHit, fc.bCrit)
    val direction = fc.arrow match
      case 1 => "->->"
      case 0 => "<-"
      case _ => "->"
    //How many attacks and in what order?
    g.fill = Purple
    g.font = Font(fontType, font)
    g.fillText(s"x${fc.aAtks} $direction x${fc.bAtks}", 240, 260)

end ForecastWindow


class MiniUnitWindow(unit: Units) extends Window:
  override val height = 150
  def draw(g: GraphicsContext, offsetX: Int, offsetY: Int, HP:String, ATK:Int, HIT:Int, CRT:Int) =
    val xo = x + offsetX
    val yo = y + offsetY
    val fm = font+margin
    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, yo, width, height)
    //Vector of writables
    val writables = Vector(s"${unit.name}",s"${unit.lvlExp}",s"HP: $HP", s"ATK: $ATK", s"HIT: $HIT", s"CRT: $CRT")
    //Draw the panel
    for i <- writables.indices do
      g.fill = if unit.acted then Color.LightGray else teamToColor(unit.team)
      g.font = Font(fontType, font) // Set text size
      g.fillText(writables(i), xo+pad, yo+pad-margin + font + fm*(i))
end MiniUnitWindow


class TileWindow(tile: Tile) extends Window:
  override val x = 450
  override val y = 300
  override val font = 15
  override val width = 120

  def draw(g: GraphicsContext, offset: Int) =
    val xo = x + offset
    val fm = font+margin
    var occupant: Option[Units] = None
    //Vector of writables
    val title = tile.name + " - H" + tile.pos(2)
    val writables: Vector[Vector[String]] = tile match
      case o: Occupiable =>
        occupant = o.occupantOnTile
        o.statsVector.map((a,b)=>s"$a: $b").sliding(2,2).toVector
      case _ => Vector()
    //Create background!
    val height = pad*2 + (writables.length+1)*fm // Height based on number of stat rows
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, y, width, height)
    //Draw the panel text
    g.fill = Blue
    g.font = Font(fontType, font)
    g.fillText(title, xo+pad, y+pad-margin + font)
    for i <- writables.indices do
        for j <- writables(i).indices do
          g.fill = Blue
          g.font = Font(fontType, font) // Set text size
          g.fillText(writables(i)(j), xo+pad+width/2*j, y+pad-margin + font + fm*(i+1))
    // Draw occupant mini window
    occupant.foreach(drawOccupant(g, _))

  def drawOccupant(g: GraphicsContext, unit: Units) =
    MiniUnitWindow(unit).draw(g, 0, 240, unit.hpMhp, unit.AT, unit.HI, unit.CR)
end TileWindow


class CharacterWindow(unit: Units) extends Window:
  def draw(g: GraphicsContext, offset: Int) =
    val xo = x + offset
    val fm = font+margin
    
    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, y, width, height)

    // Vector text
    val title = unit.name + " " + unit.HP + "/" + unit.MaxHP + "\n class: " + unit.unitClass.className
    val combVek = unit.statsCombatVector.map((a,b)=>s"$a: $b").sliding(2,2).toVector
    val unitVek = unit.statsUnitVector.map((a,b)=>s"$a: $b").sliding(2,2).toVector
    val invVek = unit.inventory.listItems.sliding(2,2).toVector

    val teamColor = teamToColor(unit.team)

    // Write nonsense
    def pairbunch(vek: Vector[Vector[String]], fontsDown: Int, fontDivider: Int) =
      for i <- vek.indices do
        for j <- vek(i).indices do
          g.fill = Blue
          g.font = Font(fontType,font/fontDivider) // Set text size
          g.fillText(vek(i)(j), xo+pad+width/2*j, y+pad-margin + font + fm*(i+fontsDown))

    g.fill = teamColor
    g.font = Font(fontType, font)
    g.fillText(title, xo+pad, y+pad-margin + font)
    pairbunch(combVek,3,1)
    pairbunch(unitVek,9,1)
    pairbunch(invVek,14,2)

end CharacterWindow


class LeftRightMenuWindow(menu: Menu) extends Window:
  override val height: Int = font+pad*2
  def draw(g: GraphicsContext, offsetX: Int, offsetY: Int) =
    val xo = x + offsetX
    val yo = y + offsetY
    val fm = font+margin

    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, yo-pad, width, height)

    // Vector text
    val article =
      if menu.subMenus.nonEmpty then
        menu.subMenus(menu.select).title
      else
        "None"

    g.fill = Blue
    g.font = Font(fontType, font)
    g.fillText(article, xo+pad,yo+pad)
