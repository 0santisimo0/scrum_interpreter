from enum import Enum
from TeamMember import TeamMember

class UserStoryType(Enum):
    FEATURE = "Feature"
    SPIKE = "Spike"
    POC = "POC"
    FIX = "Fix"
    HOTFIX = "HotFix"

class UserStory:
    def __init__(self, title, user_story_type, assigned_to, description, estimation, acceptance):
        self.title = title
        self.user_story_type = user_story_type
        self.assigned_to = assigned_to
        self.description = description
        self.estimation = estimation
        self.acceptance = acceptance

    def assign_member(self, member):
        self.assigned_to = member

    def __str__(self):
        if self.assigned_to:
            return f"{self.title} - Asignado a: {self.assigned_to}"
        else:
            return f"{self.title} - Sin asignar"