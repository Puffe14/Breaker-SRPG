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
import components.Team.*

object Main extends JFXApp3:

  val screenW = 600
  val screenH = 450
  var characterScale = 2
  var tileScale = 2

  var warriorPos = (0, 0)
  var knightPos = (325, 225)
  val gridPos = (300, 200)

  var time   = 0
  var deadTime = 0
  var atkInt = 0
  var defInt = 0
  var atkName = ""
  var defName = ""
  var attacking = false
  var defending = false
  var miss = false
  var atkDead = false
  var defDead = false
  var event = ""
  val distance = 50
  def processing = attacking || defending
  def over = iteratorLog.isEmpty

  val testObject = LogTest()
  testObject.setF12()
  testObject.setGrid()
  var log = Vector("")
  var iteratorLog = log.iterator

  def reset() =
    time   = 0
    deadTime = 0
    atkInt = 0
    defInt = 0
    atkName = ""
    defName = ""
    attacking = false
    defending = false
    miss = false
    atkDead = false
    defDead = false
    log = testObject.log(event).filterNot(n => n.contains("left")||n.contains("can")||n.contains("Empty Action"))
    iteratorLog = log.iterator
    println(log)
    if log.exists(_.contains("waited")) then log = Vector()
  end reset

  def checkOver() =
    if over then
      println("\n \"fight\" / \"reset\" / \"move Int Int\" / \"equip Int\" / \"inventory\":"+
      s"\n${testObject.moveAreaPosString}\nwpns: ${testObject.weaponsUnit1String}\n${testObject.unit1CanAtkString}")
      val command = if testObject.AIcontrol then "pass" else readLine()
      testObject.theField.clearDead()
      command match

        case "fight" =>
          if testObject.unit1CanAtk then
            event = ""
            testObject.setF1C()
            reset()
          else println("not in range!")

        case "break" =>
          event = "break"
          testObject.setF12()
          reset()

        case "wound" =>
          event = "wound"
          testObject.setF12()
          reset()

        case "heal" =>
          testObject.setF31()
          event = "heal"
          reset()

        case "treat" =>
          testObject.setF32()
          event = "treat"
          reset()

        case "ai" =>
          event = "ai"
          reset()

        case "pass" =>
          event = "pass"
          reset()

        case "turn" =>
          event = "turn"
          testObject.game.refreshAll()

        case "reset" =>
          event = ""
          testObject.resetFighters()
          testObject.setF12()
          
        case "revive" =>
          event = ""
          testObject.resetFighters()
          testObject.setGrid()

        case "inventory" =>
          println(testObject.inventoryUnit1())
        
        case "info" => testObject.info

        case s if s.contains("equip") =>
          val equipCommand = s.split(" ")
          val slot = equipCommand(1).toIntOption
          slot.foreach(testObject.equipUnit1(_))
          if slot.isEmpty then println(s"Number not found. Give one of these:\n ${testObject.weaponsUnit1.indices}")

        case s if s.contains("dir") =>
          val dirCommand = s.split(" ")
          val direction = dirCommand(1).toIntOption
          direction.foreach(testObject.theField.setRotation(_))
          
        case s if s.contains("zoom") =>
          val dirCommand = s.split(" ")
          val zoom = dirCommand(1).toIntOption
          zoom.foreach(z =>
            tileScale = z
            characterScale = z
          )

        case s if s.contains("move") =>
          val moveCommand = s.split(" ")
          val x: Int = moveCommand(1).toInt
          val y: Int = moveCommand(2).toInt
          if moveCommand.size == 3 then
            testObject.moveUnit1(x, y)
            //testObject.setFight()
            //reset()
          else
            println("give command as \"move int int\"")

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
  val imgAtkGuy: Seq[Image] = Seq(new Image(new FileInputStream(imagePath + "guy_idle.png")),
                                  new Image(new FileInputStream(imagePath + "guy_atk_1.png")),
                                  new Image(new FileInputStream(imagePath + "guy_atk_2.png")),
                                  new Image(new FileInputStream(imagePath + "guy_hurt.png")),
                                  new Image(new FileInputStream(imagePath + "dead.png")))
  val imgTiles: Seq[Image] =  Seq(new Image(new FileInputStream(imagePath + "field_gray.png")),
                                  new Image(new FileInputStream(imagePath + "field_movet.png")),
                                  new Image(new FileInputStream(imagePath + "field_red.png")),
                                  new Image(new FileInputStream(imagePath + "field_sand.png")),
                                  new Image(new FileInputStream(imagePath + "field_base.png")))

  val deadImg = new Image(new FileInputStream(imagePath + "dead.png"))
  var attackerImages = imgAtkWar
  var defenderImages = imgDefKni
  val idleImages = Map("cylna" -> new Image(new FileInputStream(imagePath + "warrior_idle.png")),
                       "bonk" -> new Image(new FileInputStream(imagePath + "guy_idle.png")),
                       "wrys" -> new Image(new FileInputStream(imagePath + "knight_idle.png")),
                       "ghost" -> new Image(new FileInputStream(imagePath + "guy_idle.png")))

  val imageSets = Map("cylna" -> imgAtkWar,
                      "wrys" -> imgDefKni,
                      "bonk" -> imgAtkGuy,
                      "ghost" -> imgAtkGuy)

  val iconImages =
    Map("wound head" -> new Image(new FileInputStream(imagePath + "wound head.png")),
        "wound arms" -> new Image(new FileInputStream(imagePath + "wound arms.png")),
        "armor head" -> new Image(new FileInputStream(imagePath + "armor head.png")))


  def start() =

    stage = new JFXApp3.PrimaryStage:
      title = "Breaker"
      width = screenW
      height = screenH

    val root = Pane()

    val scene = Scene(parent = root)
    stage.scene = scene

    def tilePosX(loc: (Int, Int, Int)): Int =
      val (x,y,z) = loc
      val dir = testObject.theField.currentRotation
      if dir == 0 then       x*16*tileScale   - y*16*tileScale   + gridPos(0)
      else if dir == 1 then  x*16*tileScale   + y*16*tileScale   + gridPos(0)
      else if dir == 2 then -x*16*tileScale   + y*16*tileScale   + gridPos(0)
      else if dir == 3 then -x*16*tileScale   - y*16*tileScale   + gridPos(0)
      else 0

    def tilePosY(loc: (Int, Int, Int)): Int =
      val (x,y,z) = loc
      val dir = testObject.theField.currentRotation
      if dir == 0 then       x* 8*tileScale   + y*8*tileScale    + gridPos(1) - z*8*tileScale
      else if dir == 1 then -x* 8*tileScale   + y*8*tileScale    + gridPos(1) - z*8*tileScale
      else if dir == 2 then -x* 8*tileScale   - y*8*tileScale    + gridPos(1) - z*8*tileScale
      else if dir == 3 then  x* 8*tileScale   - y*8*tileScale    + gridPos(1) - z*8*tileScale
      else 0


    ////draw functions

    def rectangle = new Rectangle:
      x = 0
      y = 0
      width = screenW
      height = screenH
      fill = White
      viewOrder_(0.1)

    def attacker = new ImageView:
      x = warriorPos(0)
      y = warriorPos(1)
      image = attackerImages(atkInt)
      scaleX = characterScale
      scaleY = characterScale

    def defender = new ImageView:
      x = knightPos(0) - tileScale*12
      y = knightPos(1)
      image = defenderImages(defInt)
      scaleX = -characterScale
      scaleY = characterScale

    def unit(imgNum: Int, images: Seq[Image], xpos: Int, ypos: Int, zpos: Int, flip: Boolean) = new ImageView:
      if flip then x = xpos - tileScale*12 else x = xpos
      val mirror = if flip then -1 else 1
      y = ypos + 8*tileScale*zpos
      image = images(imgNum)
      scaleX = characterScale * mirror
      scaleY = characterScale
      smooth = false
      viewOrder_(-zpos.toDouble)

    def statusImage(loc: (Int, Int, Int), name: String, number: Int) = new ImageView:
      x = tilePosX(loc)  -tileScale*8
      y = tilePosY(loc)  -tileScale*8 + number*6*tileScale
      image = iconImages(name)
      scaleX = tileScale
      scaleY = tileScale
      viewOrder_(-loc(2).toDouble)



    def tileImage(loc: (Int, Int, Int), color: Int) = new ImageView:
      val (lx, ly, lz) = loc
      x = tilePosX(loc)
      y = tilePosY(loc)
      image = imgTiles(color)
      scaleX = tileScale
      scaleY = tileScale
      smooth = false
      viewOrder_(-lz.toDouble)

    def drawField() =
      val toDraw = mutable.Buffer[ImageView]()
      testObject.theField.theGrid.allTiles//.tilesVisible
        .foreach(t =>
          var tileInt = 0
          var bottomInt = 4
          if t.name == "w" then tileInt = 2
          if t.name == "s" then tileInt = 3
          toDraw += tileImage(t.pos, tileInt)
          //add bottom
          (1 to t.pos(2)).foreach(i =>
            val bottomPos = (t.pos(0), t.pos(1), i-1)
            toDraw += tileImage(bottomPos, bottomInt))
        )
      testObject.moveAreaTiles
        .foreach(t => toDraw += tileImage(t.pos, 1))
      toDraw.sortBy(t => t.y.toInt)
              .foreach(t => root.children += t)

    def drawUnits() =
      val toDraw = mutable.Buffer[ImageView]()
      testObject.theField.theGrid.tilesWithUnits.foreach(t =>
        t.occupantOnTile.foreach(u =>
          //var tileInt = 0
          val pos = t.pos
          val flip = u.team != Player
          //println(u.name +" at "+pos)
          val x = tilePosX(pos) + tileScale*8
          val y = tilePosY(pos) - tileScale*12   - pos(2)*8*tileScale
          val z = pos(2)
          if u.name == atkName then //warriorPos = (x, y)
            toDraw += unit(atkInt, imageSets.getOrElse(u.name, Seq(deadImg)),
            x, y, z, flip)
          else if u.name == defName then //knightPos = (x, y)
            toDraw += unit(defInt, imageSets.getOrElse(u.name, Seq(deadImg)),
            x, y, z, flip)
          else toDraw += unit(0, Seq(idleImages.getOrElse(u.name, deadImg)),
            x, y, z, flip)
          //status icons
          for i <- u.statusList.indices do
            toDraw += statusImage(pos,u.statusList(i),i)
        )
      )
      toDraw.foreach(t => root.children += t)


    ////ANIMATION

    val emptyRootKids = root.children
    println(log)
    val timer = AnimationTimer(t => {
      update()
      if time % distance == 0 then
        root.children.clear()
        root.children += rectangle
        drawField()
        drawUnits()
      if time % distance == 1 then
        checkOver()
    })
    timer.start()

  end start


  def update() =
    time += 1

    if atkDead then atkInt = 4
    if defDead then defInt = 4
    if atkDead || defDead then
      deadTime += 1
      if deadTime > distance then
        testObject.theField.clearDead()

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
          case str if str.contains("hit") || str.contains("heals") || str.contains("breaks")  || str.contains("wounds") || str.contains("treats")=>
            val split = str.split(" ")
            miss = false
            if split(0) == atkName then
              attacking = true
            else
              defending = true
          case str if str.contains("died") =>
            val split = str.split(" ")
            deadTime = 0
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

