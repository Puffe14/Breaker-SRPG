import components.FieldMap
import scalafx.animation.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color.*

import scala.io.StdIn.readLine
import java.io.FileInputStream
import scala.collection.mutable

object Main extends JFXApp3:

  val screenW = 600
  val screenH = 450
  val characterScale = 4
  val tileScale = 4

  var warriorPos = (0, 0)
  var knightPos = (325, 225)
  val gridPos = (200, 100)

  var time   = 0
  var atkInt = 0
  var defInt = 0
  var atkName = ""
  var defName = ""
  var attacking = false
  var defending = false
  var miss = false
  var atkDead = false
  var defDead = false
  val distance = 50
  def processing = attacking || defending
  def over = iteratorLog.isEmpty

  val testObject = LogTest()
  testObject.setFight()
  testObject.setGrid()
  var log = testObject.log.filterNot(n => n.contains("left")||n.contains("can"))
  var iteratorLog = log.iterator

  def reset() =
    time   = 0
    atkInt = 0
    defInt = 0
    atkName = ""
    defName = ""
    attacking = false
    defending = false
    miss = false
    atkDead = false
    defDead = false
    log = testObject.log.filterNot(n => n.contains("left")||n.contains("can"))
    iteratorLog = log.iterator
    println(log)
  end reset

  def checkOver() =
    if over then
      println("\n \"fight\" / \"reset\":")
      val command = readLine()
      command match
        case "fight" => reset()
        case "reset" =>
          testObject.setFight()
          reset()
        case _ =>


  val imagePath = "src/main/scala/resources/images/"
  val img = new Image(new FileInputStream(imagePath + "warrior_atk_1.png"))
  val imgDef = new Image(new FileInputStream(imagePath + "knight_atk_1.png"))

  val imgAtkWar: Seq[Image] = Seq(new Image(new FileInputStream(imagePath + "warrior_idle.png")),
                                  new Image(new FileInputStream(imagePath + "warrior_atk_1.png")),
                                  new Image(new FileInputStream(imagePath + "warrior_atk_2.png")),
                                  new Image(new FileInputStream(imagePath + "warrior_hurt.png")),
                                  new Image(new FileInputStream(imagePath + "dead.png")))
  val imgDefKni: Seq[Image] = Seq(new Image(new FileInputStream(imagePath + "knight_idle.png")),
                                  new Image(new FileInputStream(imagePath + "knight_atk_1.png")),
                                  new Image(new FileInputStream(imagePath + "knight_atk_2.png")),
                                  new Image(new FileInputStream(imagePath + "knight_hurt.png")),
                                  new Image(new FileInputStream(imagePath + "dead.png")))
  val imgTiles: Seq[Image] =  Seq(new Image(new FileInputStream(imagePath + "field_gray.png")),
                                  new Image(new FileInputStream(imagePath + "field_movet.png")),
                                  new Image(new FileInputStream(imagePath + "field_red.png")),
                                  new Image(new FileInputStream(imagePath + "field_sand.png")))

  val attackerImages = imgAtkWar
  val defenderImages = imgDefKni


  def start() =

    stage = new JFXApp3.PrimaryStage:
      title = "Breaker"
      width = screenW
      height = screenH

    val root = Pane()

    val scene = Scene(parent = root)
    stage.scene = scene


    ////draw functions

    def rectangle = new Rectangle:
      x = 0
      y = 0
      width = screenW
      height = screenH
      fill = White

    def attacker = new ImageView:
      x = warriorPos(0)
      y = warriorPos(1)
      image = attackerImages(atkInt)
      scaleX = characterScale
      scaleY = characterScale

    def defender = new ImageView:
      x = knightPos(0) - 48
      y = knightPos(1)
      image = defenderImages(defInt)
      scaleX = -characterScale
      scaleY = characterScale



    def tileImage(loc: (Int, Int, Int), color: Int) = new ImageView:
      x = loc(0)*16*tileScale   - loc(1)*16*tileScale   + gridPos(0)
      y = loc(0)* 8*tileScale   + loc(1)*8*tileScale    + gridPos(1)
      image = imgTiles(color)
      scaleX = tileScale
      scaleY = tileScale

    def drawField() =
      val toDraw = mutable.Buffer[ImageView]()
      testObject.theField.theGrid.allTiles
        .foreach(t =>
          var tileInt = 0
          if t.name == "w" then tileInt = 2
          if t.name == "s" then tileInt = 3
          toDraw += tileImage(t.pos, tileInt))
      testObject.moveAreaTiles
        .foreach(t => toDraw += tileImage(t.pos, 1))
      //toDraw.sortBy(t => t.y.toInt)
      toDraw.foreach(t => root.children += t)

    def drawUnits() =
      val toDraw = mutable.Buffer[ImageView]()
      testObject.theField.theGrid.tilesWithUnits.foreach(t =>
        t.occupantOnTile.foreach(u =>
          //var tileInt = 0
          val pos = t.pos
          println(u.name +" at "+pos)
          val x = pos(0)*16*tileScale   - pos(1)*16*tileScale   + gridPos(0) + 32
          val y = pos(0)* 8*tileScale   + pos(1)* 8*tileScale   + gridPos(1) - 48
          if u.name == atkName then warriorPos = (x, y)
          if u.name == defName then knightPos = (x, y)
        )
          //toDraw += tileImage(t.pos, tileInt))
      )
      //toDraw.foreach(t => root.children += t)


    ////

    val emptyRootKids = root.children
    //root.children += rectangle
    root.children += attacker
    root.children += defender
    println(log)
    val timer = AnimationTimer(t => {
      update()
      if time % distance == 0 then

        root.children += rectangle
        drawField()
        drawUnits()
        root.children += attacker
        root.children += defender
      if time % distance == 1 then
        checkOver()
    })
    timer.start()

  end start


  def update() =
    time += 1

    if atkDead then atkInt = 4
    if defDead then defInt = 4

    ////////////goes through the log to process which animation is required
    if iteratorLog.hasNext && !processing then
      atkInt = 0
      defInt = 0

      val message = iteratorLog.next()
      message match
          //first one tells who is attacking who
          case str if str == log.head =>
            val split = str.split(" ")
            atkName = split(0)
            defName = split(2)
          case str if str.contains("misses") =>
            val split = str.split(" ")
            miss = true
            if split(0) == atkName then
              attacking = true
            else
              defending = true
          case str if str.contains("hit") =>
            val split = str.split(" ")
            miss = false
            if split(0) == atkName then
              attacking = true
            else
              defending = true
          case str if str.contains("died") =>
            val split = str.split(" ")
            if split(0) == atkName then
              atkDead = true
            else
              defDead = true
          case _ =>

    ////////////////////////////////////////
    //sets which photo is used for both
    if time % distance == 0 then

      if attacking then
        atkInt += 1
        if atkInt == 2 then
          if miss then
            defInt = 1
          else defInt = 3
          attacking = false

      if defending then
        defInt += 1
        if defInt == 2 then
          if miss then
            atkInt = 1
          else atkInt = 3
          defending = false

end Main

