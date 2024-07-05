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
user_story = UserStory(
    "POC-01",
    "Implementar Prueba de Concepto para Integracion de API Externa",
    UserStoryType.POC,
    TeamMember("Bryant Mayers"),
    "Realizar una prueba de concepto para integrar la API externa en nuestra aplicaci\243n.",
    3,
    "La API externa est\225 integrada correctamente"

)
manager.addUserStory(user_story)

user_story = UserStory(
    "US-01",
    "Desarrollar Funcionalidad de Autenticaci\243n de Usuarios",
    UserStoryType.FEATURE,
    None,
    "Implementar un sistema de autenticaci\243n de usuarios que permita a los usuarios registrarse.",
    5,
    "Los usuarios pueden registrarse, iniciar sesi\243n y cerrar sesi\243n correctamente."

)
manager.addUserStory(user_story)

user_story = UserStory(
    "US-02",
    "Crear Sistema de Notificaciones en Tiempo Real",
    UserStoryType.FEATURE,
    TeamMember("Cristiano Ronaldo"),
    "Desarrollar un sistema de notificaciones en tiempo real.",
    3,
    "El sistema de notificaciones est\225 funcionando correctamente"

)
manager.addUserStory(user_story)


manager.showViewIfScrumAdded()