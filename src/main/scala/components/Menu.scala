package components

trait Menu:
  val title: String
  private var selector = 0

  // Items shown in the menus
  var subMenus: Vector[Menu] = Vector()
  def setSubMenus(menus: Vector[Menu]) =
    subMenus = menus
  def createSubMenus(game: Game) =
    subMenus = Vector()
  def itemTitles: Vector[String] =
    subMenus.map(_.title)

  // For handling which menu item will be chosen
  def select = selector
  def selection(i: Int) =
    val sml = subMenus.length
    if sml>0 then
      selector = ((selector+i)%sml+sml)%sml
  def selectorUp() = selection(-1)
  def selectorDown() = selection(1)
  def pick: Menu = subMenus(select)
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
class AttackMenu extends Menu:
  val title = "Attack"
class WoundMenu extends Menu:
  val title: String = "Wound"
class ActionsMenu extends Menu:
  val title: String = "Actions"
class WaitMenu extends ConfirmMenu:
  override val title: String = "Wait"

class EquipMenu extends ConfirmMenu:
  override val title = "Equip"
class UseMenu extends ConfirmMenu:
  override val title: String = "Use"
class DiscardMenu extends ConfirmMenu:
  override val title: String = "Discard"

class ItemMenu(item: Option[Item]) extends Menu:
  val name = item match
    case Some(i) => i.toString
    case _ => "Empty"
  val title: String = s"$name"

  override def createSubMenus(game: Game) =
    game.acting.foreach(u=>
      if item.nonEmpty then setSubMenus(Vector(EquipMenu(),UseMenu(),DiscardMenu()))
    )