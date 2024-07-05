from TeamMember import TeamMember
from ScrumMaster import ScrumMaster
from ProductOwner import ProductOwner
from Manager import Manager
from UserStory import UserStory, UserStoryType

manager = Manager()

intList = [10, 11, 12]
floatList = [10.3, 11.2, 12.4]
booleanLIst = [True, False]
stringList = ["Hola", "Mundo"]
yList = [1, 2, 3]
for j in yList:
    z = 12

sm = ScrumMaster("Santiago Caballero") 
manager.setScrumMaster(sm)
po = ProductOwner("Samuel Escalera") 
manager.setProductOwner(sm)
manager.addTeamMember(TeamMember("Jefferson Coronel"))
manager.addTeamMember(TeamMember("Bryant Mayers"))
manager.addTeamMember(TeamMember("Cristiano Ronaldo"))
manager.addTeamMember(TeamMember("Ronaldiño Gaucho"))
manager.addTeamMember(TeamMember("Diego Figueroa"))
manager.addTeamMember(TeamMember("Jorge Heredia"))
manager.addTeamMember(TeamMember("Henry Cavill"))
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
    TeamMember("Diego Figueroa"),
    "Desarrollar un sistema de notificaciones en tiempo real.",
    3,
    "El sistema de notificaciones est\225 funcionando correctamente"

)
manager.addUserStory(user_story)

user_story = UserStory(
    "US-03",
    "Implementar Funcionalidad de Carrito de Compras",
    UserStoryType.FEATURE,
    None,
    "Desarrollar un carrito de compras que permita a los usuarios agregar y eliminar productos.",
    4,
    "Los usuarios pueden agregar productos al carrito, eliminar productos del carrito y ver el resumen de la compra."

)
manager.addUserStory(user_story)

user_story = UserStory(
    "POC-02",
    "Evaluar Integraci\243n de Servicio de Mapas en la Aplicaci\243n",
    UserStoryType.POC,
    TeamMember("Cristiano Ronaldo"),
    "Realizar una evaluaci\243n para integrar un servicio de mapas en la aplicaci\243n m\243vil.",
    2,
    "Se eval\250an las ventajas y desventajas de integrar el servicio de mapas y se concluye con una decisi\243n informada."

)
manager.addUserStory(user_story)


manager.showViewIfScrumAdded()