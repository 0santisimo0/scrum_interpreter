# Manager.py
import random
from collections import deque
from UserStory import UserStory, UserStoryType
from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner
from ScrumView import ScrumView

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
            print("No hay miembros en el equipo para asignar US.")
            return
        
        self._shuffleAndAssignStories()

    def _shuffleAndAssignStories(self):
        random.shuffle(self.team_members)
        queue = deque(self.team_members)

        for i, story in enumerate(self.user_stories):
            assigned_member = queue[i % len(queue)] 
            story.assign_member(assigned_member)
            
    def addUserStory(self, user_story):
        self.user_stories.append(user_story)

    def showViewIfScrumAdded(self):

        if self.user_stories and self.team_members:
            scrum_view = ScrumView(self)
            scrum_view.show_view()
