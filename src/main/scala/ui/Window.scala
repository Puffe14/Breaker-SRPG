package ui
import components.*

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.Scene
import scalafx.scene.canvas.*
import scalafx.scene.image.*
import scalafx.scene.input.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color.*
import scalafx.scene.text.Font
import scalafx.Includes._


class Window:
  val x = 10
  val y = 10
  val pad = 10
  val margin = 2
  val width = 200
  val height = 400
  val font = 20
end Window


class MenuWindow(menu: Menu) extends Window:
  def draw(g: GraphicsContext, offset: Int) =
    val xo = x + offset
    val fm = font+margin
    val pos = menu.select

    //Create background!
    g.fill = Gray // Set the fill color.
    g.fillRect(xo, y, width, height)

    //Selector outline
    g.fill = Purple // Set the fill color.
    val textY = y+pad-margin+fm*pos
    g.fillRect(xo+pad-margin, textY,
               width-pad*2+margin, fm+margin*2)

    //Write nonsense
    for i <- menu.itemTitles.indices do
      g.fill = Blue
      g.font = Font(font) // Set text size
      g.fillText(menu.itemTitles(i), xo+pad, y+pad-margin + font + fm*i)

end MenuWindow