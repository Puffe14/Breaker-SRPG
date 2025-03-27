package components

class Organization(var members: Vector[Units], var deployed: Vector[Units], storage: Inventory, side: Team):

  //Returns the vector including all members.
  def allMembers =
    members
    
  def group: Group =
    Group(deployed,Behaviour.Control,side)

  //Returns the vector including all deployed.
  def allDeployed =
    deployed

  //Adds a new unit to the organization.
  def addMember(unit: Units) =
    members.appended(unit)

  //Removes a new unit from the organization.
  def removeMember(unit: Units) =
    members = members.filter(_==unit)

  //Adds a new unit to the deployed team.
  def addDeployed(unit: Units) =
    deployed.appended(unit)

  //Removes a new unit from the deployed team.
  def removeDeployed(unit: Units) =
    deployed = deployed.filter(_==unit)

  //Removes all dead characters from an organization
  def clearDead() =
    members = members.filter(_.isDead)

end Organization
