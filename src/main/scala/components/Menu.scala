package components

trait Menu:
  val title: String
  private var selector = 0
  def hidden = false

  // Items shown in the menus
  var subMenus: Vector[Menu] = Vector()
  def setSubMenus(menus: Vector[Menu]) =
    subMenus = menus
  def createSubMenus(game: Game) =
    subMenus = Vector()
  def itemTitles: Vector[String] =
    subMenus.map(_.title)

  /** Decides what happens when the menu item is picked. */
  def effect(game: Game) = ()

  // For handling which menu item will be chosen
  def select = selector
  def selection(i: Int) =
    val sml = subMenus.length
    if sml>0 then
      selector = ((selector+i)%sml+sml)%sml
  def selectorUp() = selection(-1)
  def selectorDown() = selection(1)

  def pick: Menu =
    val next = subMenus(select)
    next
end Menu


class InventoryMenu extends Menu:
  val title: String = "Inventory"
  override def createSubMenus(game: Game) =
    game.acting.foreach(u=>
      val inventory = u.inventory
      setSubMenus(inventory.items.map(ItemMenu(_)))
    )

class ConfirmMenu extends Menu:
  val title: String = "Confirm"
  override def createSubMenus(game: Game) =
    effect(game)

class UnitMenu(actor: Units, targets: Vector[Units]) extends Menu:
  val title = "Unit"
  override def createSubMenus(game: Game) =
    setSubMenus(targets.map(CombatMenu(actor,_)))

class CombatMenu(actor: Units, target: Units) extends ConfirmMenu:
  def targetUnit = target
  override def createSubMenus(game: Game) =
    setSubMenus(Vector(BoutMenu()))
    game.setSelectMenus(Vector(WeaponsMenu(actor, target),PartsMenu(actor, target)))
    game.setForecast()

class BoutMenu extends ConfirmMenu:
  override def effect(game: Game) =
    game.performBout()


/*class ForecastMenu extends ConfirmMenu:
  override def effect(game: Game) =
end ForecastMenu*/


// Not part of menu tree

trait InstantMenu extends ConfirmMenu:
  def selectUpEffect(game: Game) =
    effect(game)
    selectorUp()
  def selectDownEffect(game: Game) =
    effect(game)
    selectorDown()
end InstantMenu

class WeaponsMenu(actor: Units, target: Units) extends InstantMenu:
  override val title = "Weapons"
  override def createSubMenus(game: Game) =
    val distance = game.unitToUnitDistance(actor, target)
    setSubMenus(actor.usableWeaponsAt(distance)
                     .map(WeaponMenu(_)))
  override def effect(game: Game) =
    subMenus(select).effect(game)

class WeaponMenu(weapon: Weapon) extends Menu:
  override val title = weapon.name
  override def effect(game: Game) =
    game.actingEquip(weapon)


class PartsMenu(actor: Units, target: Units) extends InstantMenu:
  override val title = "Parts"
  override def createSubMenus(game: Game) =
    val distance = game.unitToUnitDistance(actor, target)
    setSubMenus(target.woundableParts
                      .map(PartMenu(_)).toVector)
  override def effect(game: Game) =
    subMenus(select).effect(game)

class PartMenu(part: Part) extends InstantMenu:
  override val title = part.name
  override def effect(game: Game) =
    game.setTargetedPart(part)


trait TargetMenu extends InstantMenu:
  override def hidden = true
  def combat(game: Game): Option[Combat]
  override def createSubMenus(game: Game) =
    game.acting.foreach(actor =>
      val targetables = game.attackRangeUnitsFor(actor)
      setSubMenus(targetables.map(CombatMenu(actor,_))))
    effect(game)
  override def effect(game: Game) =
    this.subMenus(select) match
      case m: CombatMenu =>
        game.setTarget(Some(m.targetUnit))
      case _ =>

class AttackMenu extends TargetMenu:
  override val title = "Attack"
  def combat(game: Game): Option[Combat] =
    var found: Option[Combat] = None
    game.acting.foreach(a=>
      game.target.foreach(t=>
        val range = game.unitToUnitDistance(a,t)
        found = Some(Combat(a,t,range))
      )
    )
    found
class WoundMenu extends TargetMenu:
  override val title: String = "Wound"
    def combat(game: Game): Option[Combat] =
      var found: Option[Combat] = None
        game.acting.foreach(a=>
          game.target.foreach(t=>
            val range = game.unitToUnitDistance(a,t)
            found = Some(Wound(a,t,range,game.targetPart))
          )
        )
      found
class HealMenu extends TargetMenu:
  override val title: String = "Heal"
    def combat(game: Game): Option[Combat] =
      var found: Option[Combat] = None
        game.acting.foreach(a=>
          game.target.foreach(t=>
            a.medkit.foreach(m=>
              val range = game.unitToUnitDistance(a,t)
              found = Some(Heal(a,t,range,m))
            )
          )
        )
      found

class ActionsMenu extends Menu:
  val title: String = "Actions"
  override def createSubMenus(game: Game) =
    setSubMenus(Vector(AttackMenu(),WoundMenu(),HealMenu(),InventoryMenu(),WaitMenu()))

class WaitMenu extends ConfirmMenu:
  override val title: String = "Wait"
  override def effect(game: Game) =
    game.actingWait()

class EquipMenu(item: Item) extends ConfirmMenu:
  override val title = "Equip"
  override def effect(game: Game) =
    game.actingToggleEquip(item)
    game.menuBack()
    game.menuBack()
class UseMenu(item: Item) extends ConfirmMenu:
  override val title: String = "Use"
  override def effect(game: Game) =
    game.actingUse(item)
class DiscardMenu(item: Item) extends ConfirmMenu:
  override val title: String = "Discard"
  override def effect(game: Game) =
    game.actingDiscard(item)
    game.menuBack()
    game.menuBack()

class ItemMenu(item: Option[Item]) extends Menu:
  val name = item match
    case Some(i) => i.toString
    case _ => "Empty"
  val title: String = s"$name"

  override def createSubMenus(game: Game) =
    game.acting.foreach(u=>
      item.foreach(i=>setSubMenus(Vector(EquipMenu(i),UseMenu(i),DiscardMenu(i))))
    )