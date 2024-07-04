from enum import Enum
from TeamMember import TeamMember

class UserStoryType(Enum):
    FEATURE = "Feature"
    SPIKE = "Spike"
    POC = "POC"
    FIX = "Fix"
    HOTFIX = "HotFix"

class UserStory:
    def __init__(self, id, title, user_story_type, assigned_to, description, estimation, acceptance):
        self.id = id
        self.title = title
        self.user_story_type = user_story_type
        self.assigned_to = assigned_to
        self.description = description
        self.estimation = estimation
        self.acceptance = acceptance

    def assign_member(self, member):
        self.assigned_to = member

    def __str__(self):
        assigned_str = f"Asignado a: {self.assigned_to}" if self.assigned_to else "Sin asignar"
        return f"ID: {self.id}, Título: {self.title}, Tipo: {self.user_story_type.name}, {assigned_str}, Descripción: {self.description}, Estimación: {self.estimation}, Aceptación: {self.acceptance}"