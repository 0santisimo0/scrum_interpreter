import random
from UserStory import UserStory
from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner

class Manager:
    def __init__(self):
        self.scrum_master = ScrumMaster("Null")
        self.product_owner = ProductOwner("Null")
        self.team_members = []
        self.user_stories = []

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

    def assignUserStories(self):
        if not self.team_members:
            print("No hay miembros en el equipo para asignar historias de usuario.")
            return
        
        self._shuffleAndAssignStories()

    def _shuffleAndAssignStories(self):
        random.shuffle(self.team_members)
        
        for story in self.user_stories:
            assigned_member = random.choice(self.team_members)
            story.assignMember(assigned_member)

        print("Historias de usuario asignadas correctamente.")

    def setUserStories(self, user_stories):
        self.user_stories = user_stories
