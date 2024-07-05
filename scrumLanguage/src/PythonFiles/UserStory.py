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
        return (
            f"ID: {self.id}\n"
            f"Título: {self.title}\n"
            f"Tipo: {self.user_story_type.name}\n"
            f"{assigned_str}\n"
            f"Descripción: {self.description}\n"
            f"Estimación: {self.estimation}\n"
            f"Aceptación: {self.acceptance}\n"
        )
