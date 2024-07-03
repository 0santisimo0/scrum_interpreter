from TeamMember import TeamMember

class UserStory:
    def __init__(self, title, description):
        self.title = title
        self.description = description
        self.team_member = TeamMember("")

    def assignMember(self, member):
        self.team_member = member

    def __str__(self):
        if self.team_member:
            return f"{self.title} - Asignado a: {self.team_member.getName()}"
        else:
            return f"{self.title} - Sin asignar"