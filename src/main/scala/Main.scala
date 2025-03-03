import scalafx.animation.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color.*
import scala.io.StdIn.readLine
import java.io.FileInputStream

object Main extends JFXApp3:

  val screenW = 600
  val screenH = 450

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
  testObject.set()
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
          testObject.set()
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

    def rectangle = new Rectangle:
      x = 0
      y = 0
      width = screenW
      height = screenH
      fill = White

    def attacker = new ImageView:
      x = 225
      y = 175
      image = attackerImages(atkInt)
      scaleX = 5
      scaleY = 5

    def defender = new ImageView:
      x = 325
      y = 225
      image = defenderImages(defInt)
      scaleX = -5
      scaleY = 5



    val emptyRootKids = root.children
    //root.children += rectangle
    root.children += attacker
    root.children += defender
    println(log)
    val timer = AnimationTimer(t => {
      update()
      if time % distance == 0 then
        root.children += rectangle
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

