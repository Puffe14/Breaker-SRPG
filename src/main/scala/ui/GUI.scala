package ui

import components.*
import components.Animation.*
import components.Team.Enemy
import game.*
import scalafx.event.*
import scalafx.animation.AnimationTimer
import scalafx.application.{JFXApp, JFXApp3}
import scalafx.scene.Scene
import scalafx.scene.canvas.*
import scalafx.scene.image.*
import scalafx.scene.input.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color.*
import scalafx.scene.text.Font
import scalafx.Includes.*

import java.io.FileInputStream
import scala.collection.mutable

// !!! EI TÄMMÖSTÄ
val imagePath = "src/main/scala/resources/images/"
def imgMake(s: String) = new Image(new FileInputStream(imagePath + s + ".png"))
def pairImgString(s: String) = s -> imgMake(s)
var imgTiles: Map[String,Image] = Seq("field_cursor_ally",
                                      "field_cursor_enemy",
                                      "field_cursor_player",
                                      "field_cursor",
                                      "field_movet").map(pairImgString(_)).toMap
def boutImgSeq(name: String): Seq[Image] = Seq(new Image(new FileInputStream(imagePath + name + "_idle.png")),
                                              new Image(new FileInputStream(imagePath + name + "_atk_1.png")),
                                              new Image(new FileInputStream(imagePath + name + "_atk_2.png")),
                                              new Image(new FileInputStream(imagePath + name + "_hurt.png")),
                                              new Image(new FileInputStream(imagePath + "dead.png")))
val idleImages = Map("Cylna" -> new Image(new FileInputStream(imagePath + "warrior_idle.png")),
                     "bonk" -> new Image(new FileInputStream(imagePath + "guy_idle.png")),
                     "wrys" -> new Image(new FileInputStream(imagePath + "knight_idle.png")),
                     "ghost" -> new Image(new FileInputStream(imagePath + "guy_idle.png")),
                     "Geblah" -> new Image(new FileInputStream(imagePath + "guy_idle.png")))
val deadImg = new Image(new FileInputStream(imagePath + "dead.png"))
val classImageSets = Map("wilder" -> boutImgSeq("warrior"),
                         "slicer" -> boutImgSeq("swordsman"),
                         "singer" -> boutImgSeq("guy"),
                         "taker" -> boutImgSeq("ninja"),
                         "archer" -> boutImgSeq("gunner"),
                         "warrior" -> boutImgSeq("pirate"),
                         "channeler" -> boutImgSeq("guy"),
                         "shooter" -> boutImgSeq("ninja"),
                         "medic" -> boutImgSeq("guy"),
                         "rider" -> boutImgSeq("knight"),
                         "flier" -> boutImgSeq("hoplite"))

val iconImages =
  Map("wound head" -> new Image(new FileInputStream(imagePath + "wound head.png")),
      "wound arms" -> new Image(new FileInputStream(imagePath + "wound arms.png")),
      "wound torso" -> new Image(new FileInputStream(imagePath + "wound body.png")),
      "wound legs" -> new Image(new FileInputStream(imagePath + "wound legs.png")),
      "armor head" -> new Image(new FileInputStream(imagePath + "armor head.png")),
      "armor torso" -> new Image(new FileInputStream(imagePath + "armor body.png")),
      "armor arms" -> new Image(new FileInputStream(imagePath + "armor arms.png")),
      "armor legs" -> new Image(new FileInputStream(imagePath + "armor legs.png")))

def animationToInt(animation: Animation) =
  animation match
    case Idle => 0
    case Attack => 2
    case Stance => 1
    case Evade => 1
    case Hurt => 3
    case _ => 0

def setAniInt(unit: Units, animation: Animation) =
  unit.setAniInt(animationToInt(animation))



object GUI extends JFXApp3:

  val screenW = 600
  val screenH = 450
  var time = 0
  var delta = 0
  val refreshFrame = 10

  //"Camera" control variables
  var drawScale = 3                                     //The scale of the pictures drawn
  var middle = (screenW/drawScale, screenH/drawScale)   //The drawing location of the game map
  var cameraMoveIncrement = 10
  var mouseX = 0
  var mouseY = 0
  var cursorX = 0
  var cursorY = 0
  var infoText = "Hello player"
  var currentMessage = Vector[String]()
  val msgWindow = MessageWindow()
  var game = Game()
  var actList = Vector[Act]()
  startUp()


  def startUp() =
    // RESET GUI STUFF
    drawScale = 3
    middle = (screenW/drawScale, screenH/drawScale)
    cameraMoveIncrement = 10
    mouseX = 0
    mouseY = 0
    cursorX = 0
    cursorY = 0
    infoText = "Hello player"
    currentMessage = Vector[String]()
    // CONNECT TO GAME
    game = Game()
    game.initialize()
    game.battleStart()
    game.currentMap = DataLibrary.maps.get(game.currentMapNumber.toString)
    game.player = Some(new Organization(Vector(), Vector(), new Inventory(50), Team.Player))
    // Animation
    actList = Vector[Act]()


  // GRAPHICS AND INTERFACE ------------

  //   ImageView methods

  def tileImage(loc: (Int, Int, Int), img: Image) = new ImageView:
      val (lx, ly, lz) = loc
      x = tilePosX(loc)
      y = tilePosY(loc)
      image = img
      scaleX = drawScale*32
      scaleY = drawScale*32
      smooth = false
      viewOrder_(-lz.toDouble)

  def unitImage(imgNum: Int, images: Seq[Image], xpos: Int, ypos: Int, zpos: Int, flip: Boolean) = new ImageView:
      if flip then x = xpos + 16*drawScale else x = xpos
      val mirror = if flip then -1 else 1
      y = ypos + 8*drawScale*zpos
      image = images(imgNum)
      scaleX = drawScale*32 * mirror
      scaleY = drawScale*32
      smooth = false
      viewOrder_(-zpos.toDouble-1)

  def statusImage(loc: (Int, Int, Int), name: String, number: Int) = new ImageView:
      x = tilePosX(loc)
      y = tilePosY(loc)  -drawScale*4 + number*6*drawScale
      image = iconImages.getOrElse(name, deadImg)
      scaleX = drawScale*8
      scaleY = drawScale*6
      viewOrder_(-loc(2).toDouble-1)


  //   Larger methods

  def drawField =
      val toDraw = mutable.Buffer[ImageView]()
      game.allTiles         //Gather tiles to be drawn
        .foreach(t =>
          //img selection from tile photo value
          toDraw += tileImage(t.pos, t.photo)
          //add bottoms
          (1 to t.pos(2)).foreach(i =>
            val bottomPos = (t.pos(0), t.pos(1), i-1)
            toDraw += tileImage(bottomPos, t.bottom))
        )
      //Add the current units move tiles
      game.currentMoveTiles
        .foreach(t => toDraw += tileImage(t.pos, imgTiles("field_movet")))
      toDraw.toVector

  def drawCursor =
    game.tileAt(cursorX, cursorY) match
      case Some(tile) => Vector(tileImage(tile.pos, imgTiles("field_cursor")))
      case None => Vector()

  def drawTargetor =
    game.targetor match
      case Some(tile) if game.openMenus.length>1 =>
        var selected = imgTiles("field_cursor")
        if game.target.forall(_.team==Team.Player) then selected = imgTiles("field_cursor_player")
        if game.target.forall(_.team==Team.Enemy) then selected = imgTiles("field_cursor_enemy")
        if game.target.forall(_.team==Team.Ally) then selected = imgTiles("field_cursor_ally")
        Vector(tileImage(tile.pos,selected))
      case Some(_) => Vector()
      case _ => Vector()

  def drawUnits =
      val toDraw = mutable.Buffer[ImageView]()
      game.allTilesWithUnits.foreach(t =>
        t.occupantOnTile.foreach(u =>
          val pos = t.pos
          val flip = u.team != Team.Player
          val x = tilePosX(pos) + drawScale*8
          val y = tilePosY(pos) - drawScale*12   - pos(2)*8*drawScale
          val z = pos(2)
          toDraw += unitImage(u.frame, classImageSets.getOrElse(u.unitClass.className, Seq(deadImg)),x, y, z, flip)//status icons
          for i <- u.statusList.indices do
            toDraw += statusImage(pos,u.statusList(i),i)
        )
      )
      toDraw.toVector

  def drawAllMap(pics: Vector[ImageView], g: GraphicsContext) =
    pics.sortBy(i=>(-i.viewOrder(), i.y()))
        .foreach(t => g.drawImage(t.image(), t.x(),t.y(),t.scaleX(),t.scaleY()))

  def drawMenus(menus: Vector[Menu], g: GraphicsContext) =
    val windows = menus.map(MenuWindow(_)).filterNot(_.hidden) // Make menu window objects
    var offset = 0 // Offset from previous menu window
    for i <- windows do
      i.draw(g, offset)        // Then draw the menus
      offset += i.width+i.pad   // Set seperation to the next menu
    offset = 0
    // show tile being hovered
    val dontHideHoverMenus = game.turnOf == Team.Player && game.forecast.isEmpty && actList.isEmpty
    if dontHideHoverMenus then
      hoverTile.foreach(TileWindow(_).draw(g, 0))
    // show who is being inspected
    game.inspected.foreach(u=>
      val window = CharacterWindow(u)
      window.draw(g, 310)
    )
    //draw the battle forecast to ease player choice
    game.forecast.foreach(u=>
      ForecastWindow(u).draw(g, 50)
    )
    //draw the small selector windows for changing weapon or target part
    val selectorWindows = game.selectorMenus.map(LeftRightMenuWindow(_))
    for i <- selectorWindows do
      i.draw(g, offset+100, 380)
      offset += i.width+i.pad

    //Draw the message window
    if currentMessage.nonEmpty then
      msgWindow.draw(g,0,currentMessage)

  // CONTROL

  def cameraUp(increment: Int) = middle = (middle(0), middle(1)-increment)
  def cameraDown(increment: Int) = middle = (middle(0), middle(1)+increment)
  def cameraRight(increment: Int) = middle = (middle(0)+increment, middle(1))
  def cameraLeft(increment: Int) = middle = (middle(0)-increment, middle(1))

  def selectTile() =
    hoverTile.foreach(game.selectTile(_))
  def hoverTile: Option[Tile] =
    val (x, y) = (cursorX, cursorY)
    game.tileAt(x,y)
  def inspectTile() =
    hoverTile.foreach(game.inspectTile(_))

  def setMouseLocation(event: MouseEvent) =
    mouseX = event.x.toInt - middle(0)
    mouseY = event.y.toInt - middle(1)


  // START -----------------------------

  def start() =
    //Create stage
    stage = new JFXApp3.PrimaryStage:
      title = "Breaker"
      width = screenW
      height = screenH

    //Canvas init
    val canvas = Canvas(screenW, screenH)
    val bottomBox = HBox()
    val g = canvas.graphicsContext2D
    canvas.onMouseMoved = (event: MouseEvent) => setMouseLocation(event)

    //Connect rest to root
    val root = GridPane()
    root.add(canvas, 1, 0)
    val scene = Scene(parent = root)
    scene.onKeyPressed =  (event: KeyEvent) => handlePress(event)
    stage.scene = scene

    //Animation timer keeps track of the passage of time
    val timer = AnimationTimer(t => {
      if time % refreshFrame == 0 then
        //Clean background!
        g.fill = White // Set the fill color.
        g.fillRect(0, 0, screenW, screenH) // Fill rectangle at (0, 0) with width 600 and height 450.

        //Draw methods for groups of Images
        drawAllMap(drawField++drawUnits++drawCursor++drawTargetor, g)
        //Write nonsense
        g.fill = Blue
        g.font = Font(30) // Set text size
        g.fillText(infoText, 250, 50) // Fill text
        //Draw "UI" on top
        drawMenus(game.menus, g)

        //Keep game going on
        if game.isBattleOver then
          infoText = s"Battle OVER"
        else if actList.isEmpty then
          game.handleTurn()
          infoText = s"Turn ${game.currentTurn}, Pos $cursorX, $cursorY. ${game.turnOf}"
        while game.stack.hasNext && actList.isEmpty do
          val explain = game.continue()
          delta = 0
          //Capture acts from explain
          actList = explain.acts
        //handle animating units
        game.allTilesWithUnits.foreach(_.occupantOnTile.foreach(setAniInt(_, Idle)))
        actList = actList.filterNot(_.done(delta))
        val activeActs = actList.filter(_.show(delta))
        activeActs.foreach(a => setAniInt(a.actor, a.frame))
        currentMessage = activeActs.map(_.msg).filter(_!="")
        delta+=1
    })
    timer.start()


  // USEFUL METHODS ---------------------
  
  def tilePosX(loc: (Int, Int, Int)): Int =
      val (x,y,z) = loc
      val dir = game.dir
      if dir == 0 then       x*16*drawScale   - y*16*drawScale   + middle(0)
      else if dir == 1 then  x*16*drawScale   + y*16*drawScale   + middle(0)
      else if dir == 2 then -x*16*drawScale   + y*16*drawScale   + middle(0)
      else if dir == 3 then -x*16*drawScale   - y*16*drawScale   + middle(0)
      else 0

  def tilePosY(loc: (Int, Int, Int)): Int =
    val (x,y,z) = loc
    val dir = game.dir
    if dir == 0 then       x* 8*drawScale   + y*8*drawScale    + middle(1) - z*8*drawScale
    else if dir == 1 then -x* 8*drawScale   + y*8*drawScale    + middle(1) - z*8*drawScale
    else if dir == 2 then -x* 8*drawScale   - y*8*drawScale    + middle(1) - z*8*drawScale
    else if dir == 3 then  x* 8*drawScale   - y*8*drawScale    + middle(1) - z*8*drawScale
    else 0

  def mouseAsPos: (Int, Int) =
      val (x,y) = (mouseX/(drawScale*32), mouseY/(drawScale*16))
      val dir = game.dir
      if dir == 0 then      (Math.floor( (x + 2*y) / 2.0).toInt,
                             Math.floor((-x + 2*y) / 2.0).toInt)
      else if dir == 1 then (0, 0)//x*16*drawScale   + y*16*drawScale
      else if dir == 2 then (0, 0)//-x*16*drawScale   + y*16*drawScale
      else if dir == 3 then (0, 0)//-x*16*drawScale   - y*16*drawScale
      else (0, 0)

  def cursorUp() =
    game.dir match
      case 0 => cursorOnGridDown()
      case 1 => cursorOnGridRight()
      case 2 => cursorOnGridUp()
      case 3 => cursorOnGridLeft()
      case _ => ()
  def cursorDown() =
    game.dir match
      case 0 => cursorOnGridUp()
      case 1 => cursorOnGridLeft()
      case 2 => cursorOnGridDown()
      case 3 => cursorOnGridRight()
      case _ => ()
  def cursorRight() =
    game.dir match
      case 0 => cursorOnGridRight()
      case 1 => cursorOnGridUp()
      case 2 => cursorOnGridLeft()
      case 3 => cursorOnGridDown()
      case _ => ()
  def cursorLeft() =
    game.dir match
      case 0 => cursorOnGridLeft()
      case 1 => cursorOnGridDown()
      case 2 => cursorOnGridRight()
      case 3 => cursorOnGridUp()
      case _ => ()

  def unSelect() =
    game.cancel()

  //Zoom control
  def zoomIn() =
    drawScale+=1
  def zoomOut() =
    drawScale-=1

  def cursorOnGridUp() =    cursorY += 1
  def cursorOnGridDown() =  cursorY -= 1
  def cursorOnGridRight() = cursorX += 1
  def cursorOnGridLeft() =  cursorX -= 1

  def handlePress(event: KeyEvent) =
    if game.selectorMenus.nonEmpty then
      event.code match
        case KeyCode.W => game.menuSUp()
        case KeyCode.S => game.menuSDown()
        case KeyCode.A => game.menuSLeft()
        case KeyCode.D => game.menuSRight()
        case KeyCode.Enter => game.menuPick()
        case KeyCode.Space => game.menuBack()
        case _ =>
    else if game.inMenu then
      event.code match
        case KeyCode.W => game.menuUp()
        case KeyCode.S => game.menuDown()
        case KeyCode.Enter => game.menuPick()
        case KeyCode.Space => game.menuBack()
        case _ =>
    else if game.turnOf == Team.Player && actList.isEmpty then
      event.code match
        case KeyCode.W => cursorUp()
        case KeyCode.A => cursorLeft()
        case KeyCode.S => cursorDown()
        case KeyCode.D => cursorRight()
        case KeyCode.Enter => selectTile()
        case KeyCode.Tab => inspectTile()
        case KeyCode.Space => unSelect()
        case KeyCode.Q => game.turnAnti()
        case KeyCode.E => game.turnWise()
        case KeyCode.Z => zoomIn()
        case KeyCode.X => zoomOut()
        case KeyCode.M => startUp()
        case KeyCode.Down => cameraUp(cameraMoveIncrement)
        case KeyCode.Right => cameraLeft(cameraMoveIncrement)
        case KeyCode.Up => cameraDown(cameraMoveIncrement)
        case KeyCode.Left => cameraRight(cameraMoveIncrement)
        case _ =>