intList<10, 11, 12>
floatList<10.3, 11.2, 12.4>
booleanLIst<True, False>
stringList<"Hola", "Mundo">

yList<1,2,3>
for (j in yList) { 
    z:=12
    }


SM : "Santiago Caballero"
PO : "Samuel Escalera"
TM : "Jefferson Coronel"
TM : "Bryant Mayers"
TM : "Cristiano Ronaldo"
TM : "Ronaldiño Gaucho"
TM : "Diego Figueroa"
TM : "Jorge Heredia"
TM : "Henry Cavill"


US "POC-01" { 
    T: "Implementar Prueba de Concepto para Integracion de API Externa", 
    TY: POC, 
    PS: (TM: "Bryant Mayers"), 
    DS: "Realizar una prueba de concepto para integrar la API externa en nuestra aplicación.", 
    ET: 3, 
    AC: "La API externa está integrada correctamente"  
    }

US "US-01" { 
    T: "Desarrollar Funcionalidad de Autenticación de Usuarios", 
    TY: Feature, 
    DS: "Implementar un sistema de autenticación de usuarios que permita a los usuarios registrarse.", 
    ET: 5, 
    AC: "Los usuarios pueden registrarse, iniciar sesión y cerrar sesión correctamente." 
    }

US "US-02" { 
    T: "Crear Sistema de Notificaciones en Tiempo Real", 
    TY: Feature,
    PS: (TM: "Diego Figueroa"),
    DS: "Desarrollar un sistema de notificaciones en tiempo real.", 
    ET: 3, 
    AC: "El sistema de notificaciones está funcionando correctamente" 
    }

US "US-03" { 
    T: "Implementar Funcionalidad de Carrito de Compras", 
    TY: Feature, 
    DS: "Desarrollar un carrito de compras que permita a los usuarios agregar y eliminar productos.", 
    ET: 4, 
    AC: "Los usuarios pueden agregar productos al carrito, eliminar productos del carrito y ver el resumen de la compra." 
    }

US "POC-02" {
    T: "Evaluar Integración de Servicio de Mapas en la Aplicación",
    TY: POC,
    PS: (TM: "Cristiano Ronaldo"),
    DS: "Realizar una evaluación para integrar un servicio de mapas en la aplicación móvil.",
    ET: 2,
    AC: "Se evalúan las ventajas y desventajas de integrar el servicio de mapas y se concluye con una decisión informada."
    }