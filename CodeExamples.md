 **Declaración de variables de tipos básicos**
```
// Declaración de una variable entera
Int: age = 25;

// Declaración de una variable de punto flotante
Float: height = 1.75;

// Declaración de una variable booleana
Bool: isStudent = True;

// Declaración de una variable de cadena
String: name = "Sam";
```

**Declaración de variables con expresiones**
```
// Declaración de una variable con una expresión aritmética
Int: result = 10 + 5 * 2;

// Declaración de una variable con una expresión condicional
Bool: isAdult = (age >= 18);

// Declaración de una variable con una función de llamada
Int: totalSum = sum(5, 10);

```
 **Definición de funciones y llamadas a funciones**

```
// Definición de función que suma dos números 
fun sum(Int: x, Int: y) { 
	x + y; 
	} 

// Llamada a la función con argumentos 
sum(5, 3);
```

**Expresiones condicionales**
```
// Definición de función que determina si un número es positivo, negativo
fun checkNumber(Int: n) { 
	if (n > 0) { 
	"Positive"; 
	} else { 
	 "Negative"; 
	} 

 // Llamada a la función 
 checkNumber(-5);
```

**Bucles y listas**
```
// Definición de una lista de enteros 
list<Int> myList = <1, 2, 3, 4, 5>; 

// Bucle que itera sobre la lista y suma los elementos 
fun sumList(list<Int>: myList) { 
	Int: sum = 0; 
	for (item in myList) {
		sum = sum + item; 
		} 
		sum; 
}

// Llamada a la función 
sumList(myList);
```

**Definición y uso de user stories y roles**

```
// Definición de roles
RS {
  SM: "Diego Figueroa",
  PO: "Leonardo Espada",
  TM: "Samuel Escalera"
}

// Definición de user stories
US "US1" {
  T: "Login Feature",
  TY: "Feature",
  PS: (SM: "Diego Figueroa"),
  DS: "As a user, I want to be able to log in so that I can access my account.",
  ET: 5,
  AC: "User can log in using username and password"
}

US "US2" {
  T: "Fix login bug",
  TY: "Fix",
  PS: (TM: "Developer1"),
  DS: "Fix the login bug that prevents users from logging in.",
  ET: 3,
  AC: "Bug is fixed and users can log in without issues"
}

```