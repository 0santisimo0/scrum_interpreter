from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner
from Manager import Manager
from UserStory import UserStory, UserStoryType

manager = Manager()

12
12.3
True
"Hola"
sm = ScrumMaster("Santiago Caballero") 
manager.setScrumMaster(sm)
po = ProductOwner("Samuel Escalera") 
manager.setProductOwner(sm)
manager.addTeamMember(TeamMember("Jefferson Viejo"))
manager.addTeamMember(TeamMember("Bryant Mayers"))
manager.addTeamMember(TeamMember("Cristiano Ronaldo"))
manager.addTeamMember(TeamMember("Ronaldiño Gaucho"))
x = "hola"
m = 89
p = 0
if p == 5:
    6
    t = "watafac"
else:
    1
    "watafac"

d = "ppp"
def sum(d, p):
    ñ = 6
    78
    x = 54
    if p >= 4:
        return "mayor o igual a 4"
    else:
        return "menor a 4"
    

def rest(x, p):
    ñ = 6
    78
    d = 85
    if x >= 4:
        return "mayor o igual a 4"
    else:
        return "menor a 4"
    

rest(6, p)
sum(5, 9)
user_story = UserStory("POC-01")
us = UserStory(
    "Titulo",
    UserStoryType.SPIKE,
    TeamMember("Diego"),
    "Describcion",
    3,
    "aaaa"
)

manager.setUserStories([user_story])
user_story = UserStory("US-01")
us = UserStory(
    "Titulo",
    UserStoryType.FEATURE,
    None,
    "Hacer el feature",
    3,
    "aaaa"
)

manager.setUserStories([user_story])

manager.showViewIfScrumAdded()