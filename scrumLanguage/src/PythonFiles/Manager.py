from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner


class Manager:
    def __init__(self):
        self.scrum_master = ScrumMaster("Null")
        self.product_owner = ProductOwner("Null")
        self.team_members = []

    def getScrumMaster(self):
        return self.scrum_master

    def getProductOwner(self):
        return self.product_owner

    def getTeamMembers(self):
        return self.team_members

    def setScrumMaster(self, scrum_master):
        self.scrum_master = scrum_master

    def setProductOwner(self, product_owner):
        self.product_owner = product_owner

    def addTeamMember(self, team_member):
        self.team_members.append(team_member)