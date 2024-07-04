from ScrumMember import ScrumMember

class TeamMember(ScrumMember):
    def __init__(self, name):
        super().__init__(name)
    
    def __str__(self):
        return self.personName
