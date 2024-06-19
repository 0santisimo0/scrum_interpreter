from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner
from Manager import Manager

manager = Manager()

12
12.3
True
"Hola"
sm = ScrumMaster("Santiago Caballero") 
manager.setScrumMaster(sm)
po = ProductOwner("Samuel Escalera") 
manager.setProductOwner(sm)
manager.addTeamMember(TeamMember("Jefferson Pelotas"))
manager.addTeamMember(TeamMember("Bryant Mayers"))
manager.addTeamMember(TeamMember("Cristiano Ronaldo"))
manager.addTeamMember(TeamMember("Ronaldiño Gaucho"))
x = "hola"
y = 89

if x == 5:
    6
    x = "watafac"
else:
    1
    "watafac"

def sum(x, y):
    x = 6
    78
    
    if x >= 4:
        z = 54
        return "mayor o igual a 4"
    else:
        return "menor a 4"
    
    return "error"

print(sum(6, y))
print(manager.getScrumMaster().getName())