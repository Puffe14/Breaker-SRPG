package components
import game.Rules

trait Menu:
  val title: String
  private var selector = 0 //Which menu item is selected?
  def hidden = false

  // Items shown in the menus
  var subMenus: Vector[Menu] = Vector()
  def setSubMenus(menus: Vector[Menu]) =
    subMenus = menus
    if selector >= subMenus.length then selector = subMenus.length-1
  def createSubMenus(game: Game) =
    subMenus = Vector()
  def itemTitles: Vector[String] =
    if selector >= subMenus.length then selector = subMenus.length-1
    subMenus.map(_.title)

  /** Decides what happens when the menu item is picked. */
  def effect(game: Game) = ()

  // For handling which menu item will be chosen
  def select: Int = selector
  def selection(i: Int) =
    val sml = subMenus.length
    if sml>0 then
      selector = ((selector+i)%sml+sml)%sml
  def selectorUp() = selection(-1)
  def selectorDown() = selection(1)

  /** Returns the submenu picked. */
  def pick: Menu =
    val next = subMenus(select)
    next
end Menu


class InventoryMenu(unit: Units) extends Menu:
  val title: String = "Inventory"
  override def createSubMenus(game: Game) =
    val inventory = unit.inventory
    setSubMenus(inventory.items.map(ItemMenu(_)))



class ConfirmMenu extends Menu:
  val title: String = "Confirm"
  override def createSubMenus(game: Game) =
    effect(game)


class CombatMenu(actor: Units, target: Units, previous: TargetMenu) extends ConfirmMenu:
  def targetUnit = target
  override def createSubMenus(game: Game) =
    setSubMenus(Vector(BoutMenu()))
    previous.setSelect(game, actor, target)
    game.setForecast()

class BoutMenu extends ConfirmMenu:
  override def effect(game: Game) =
    game.performBout()


// Not part of menu tree

trait InstantMenu extends ConfirmMenu:
  def selectUpEffect(game: Game) =
    selectorUp()
    effect(game)
  def selectDownEffect(game: Game) =
    selectorDown()
    effect(game)
end InstantMenu

class EquipsMenu(equips: Vector[Equipment]) extends InstantMenu:
  override val title = "Equips"
  override def createSubMenus(game: Game) =
    setSubMenus(equips.map(EquipmentMenu(_)))
  override def effect(game: Game) =
    subMenus(select).effect(game)
    game.setForecast()

class EquipmentMenu(equipment: Equipment) extends Menu:
  override val title = equipment.name
  override def effect(game: Game) =
    game.actingEquip(equipment)


class PartsMenu(parts: Vector[Part]) extends InstantMenu:
  override val title = "Parts"
  override def createSubMenus(game: Game) =
    setSubMenus(parts.map(PartMenu(_)).toVector)
  override def effect(game: Game) =
    subMenus(select).effect(game)
    game.setForecast()

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
      setSubMenus(filtered(targetables).map(CombatMenu(actor,_,this))))
    effect(game)
  override def effect(game: Game) =
    this.subMenus(select) match
      case m: CombatMenu =>
        game.setTarget(Some(m.targetUnit))
      case _ =>
  def filtered(before: Vector[Units]): Vector[Units] = before
  /** Sets games select menus to the correct ones. Weapon/medkit + part */
  def setSelect(game: Game, actor: Units, target: Units): Unit

  // helper functions
  def EquipsForWeapons(actor: Units, distance: Int) =
    EquipsMenu(actor.usableWeaponsAt(distance))

  def EquipsForMedkits(actor: Units, distance: Int) =
    EquipsMenu(actor.usableMedkitsAt(distance))

  def PartsVulnerable(target: Units) = PartsMenu(target.woundableParts.toVector)
  def PartsBreakable(target: Units) = PartsMenu(target.breakableParts.toVector)
  def PartsWounded(target: Units) = PartsMenu(target.wounds.toVector)

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
  def setSelect(game: Game, actor: Units, target: Units) =
    // can use the weapons at particular distance
    val distance = game.unitToUnitDistance(actor, target)
    game.setSelectMenus(Vector(EquipsForWeapons(actor,distance)))

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
  def setSelect(game: Game, actor: Units, target: Units) =
    val distance = game.unitToUnitDistance(actor, target)
    game.setSelectMenus(Vector(EquipsForWeapons(actor, distance),PartsVulnerable(target)))
  override def filtered(before: Vector[Units]) =
    before.filter(_.woundableParts.nonEmpty)

class BreakMenu extends TargetMenu:
  override val title: String = "Break"
  def combat(game: Game): Option[Combat] =
    var found: Option[Combat] = None
      game.acting.foreach(a=>
        game.target.foreach(t=>
          val range = game.unitToUnitDistance(a,t)
          found = Some(Break(a,t,range,game.targetPart))
        )
      )
    found
  def setSelect(game: Game, actor: Units, target: Units) =
    val distance = game.unitToUnitDistance(actor, target)
    game.setSelectMenus(Vector(EquipsForWeapons(actor, distance),PartsBreakable(target)))
  override def filtered(before: Vector[Units]) =
    before.filter(_.breakableParts.nonEmpty)

class HealMenu extends TargetMenu:
  override val title: String = "Heal"
  override def createSubMenus(game: Game) =
    game.acting.foreach(actor =>
      val targetables = game.medRangeUnitsFor(actor)
      setSubMenus(filtered(targetables).map(CombatMenu(actor,_,this))))
    effect(game)
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
  def setSelect(game: Game, actor: Units, target: Units) =
    val distance = game.unitToUnitDistance(actor, target)
    game.setSelectMenus(Vector(EquipsForMedkits(actor, distance)))

class TreatMenu extends TargetMenu:
  override val title: String = "Treat"
  override def createSubMenus(game: Game) =
    game.acting.foreach(actor =>
      val targetables = game.medRangeUnitsFor(actor)
      setSubMenus(filtered(targetables).map(CombatMenu(actor,_,this))))
    effect(game)
  def combat(game: Game): Option[Combat] =
    var found: Option[Combat] = None
      game.acting.foreach(a=>
        game.target.foreach(t=>
          a.medkit.foreach(m=>
            val range = game.unitToUnitDistance(a,t)
            found = Some(Treat(a,t,range,m,game.targetPart))
          )
        )
      )
    found
  def setSelect(game: Game, actor: Units, target: Units) =
      val distance = game.unitToUnitDistance(actor, target)
      game.setSelectMenus(Vector(EquipsForMedkits(actor, distance),PartsWounded(target)))
  override def filtered(before: Vector[Units]) =
    before.filter(_.wounds.nonEmpty)


class TradeMenu extends TargetMenu:
  override val title: String = "Trade"
  def combat(g: Game) = None
  def setSelect(game: Game, actor: Units, target: Units) =
      val distance = game.unitToUnitDistance(actor, target)
      /*game.setSelectMenus(Vector(TradingMenu(actor.inventory, actor.name),TradingMenu(target.inventory, target.name)))
      */
  override def createSubMenus(game: Game) =
    game.acting.foreach(actor =>
      val targetables = game.tradeRangeUnitsFor(actor)
      setSubMenus((filtered(targetables).map(_.unitsInventory)++game.actingLoot)
        .map(targeted=>TradingMenu(actor,targeted,s"Trade - ${actor.name}",None)))
     )
    effect(game)



class TradingMenu(unit: Units, inv: Inventory, name: String, val slot1: Option[Int] = None) extends InstantMenu:
  override val title: String = s"$name"
  def nameOrEmpty(item: Option[Item]): String = item match
    case Some(i) => i.toString
    case _ => "Empty"
  override def selectDownEffect(game: Game): Unit = selectorDown()
  override def selectUpEffect(game: Game): Unit = selectorUp()
  override def effect(game: Game): Unit =
    if slot1.nonEmpty then
      if rules.instantTrade then
        game.trade(unit, inv, slot1.getOrElse(0), select).foreach(_.play())
      else
        //actually finish the trade, this impelemtation of adding to stack
        game.performTrade(unit, inv, slot1.getOrElse(0), select)
  override def createSubMenus(game: Game) =
    if slot1.isEmpty then
      setSubMenus(unit.unitsInventory.items.zipWithIndex
        .map((i,n)=>TradingMenu(unit,inv,nameOrEmpty(i),Some(n))).toVector)
    else
      setSubMenus(inv.items.map(i=>ItemMenu(i)))


class ActionsMenu extends Menu:
  val title: String = "Actions"
  override def createSubMenus(game: Game) =
    var collector = Vector[Menu]()
    def atc(menu: Menu) = collector = collector.appended(menu)

    game.acting.foreach(actor =>
      if game.attackRangeUnitsFor(actor).nonEmpty then
        atc(AttackMenu())
        if actor.canWound &&
           game.woundRangeUnitsFor(actor).nonEmpty then
          atc(WoundMenu())
        if actor.canBreak &&
           game.breakRangeUnitsFor(actor).nonEmpty then
          atc(BreakMenu())
      if game.medRangeUnitsFor(actor).nonEmpty then
        atc(HealMenu())
      if game.treatRangeUnitsFor(actor).nonEmpty then
        atc(TreatMenu())
      if game.actingLoot.nonEmpty || game.tradeRangeUnitsFor(actor).nonEmpty then
        atc(TradeMenu())
      //put them all in the general action menu
      setSubMenus(collector ++ Vector(InventoryMenu(actor),WaitMenu()))
    )

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
      item.foreach(i=>
        var menus: Vector[Menu] = Vector()
        if u.equippables.contains(i) then menus = Vector(EquipMenu(i))
        if i.isInstanceOf[Consumable] then menus = menus ++ Vector(UseMenu(i))
        setSubMenus(menus++Vector(DiscardMenu(i))))
    )