Eg 
string ::= "\" characters "\""`
```
"HelloWorld"
```
Identifier ::= /[a-zA-Z_$][0-9a-zA-Z_$]*/
```
myVariable1
```
role ::=  role_name ":" role_type
```
 Samuel:DV
```
roles ::= ”RS” “{” role ("," role)* “}”
```
roles {

    SM : "Alice" ,

    PO : "Bob",

    DV : "Charlie" ,

    DV:  "Dave",

    QA : "Eve"
}
```
userStory ::= "US" userStoryID "{" 

UserStoryFormatBlock

 "}"
```
"US-1" { 
	T:"Implement login",
	TY:"Feature",
	PS:"Diego",
	DS:"Create login functionality",
	ET:3,
	AC:"Login works successfully"
	}
```
loop ::= "for" "(" acceptedData"in" iterable ")" "{" block "}"
```
for (DV in Roles) { 
	block 
}
```

statement ::= roles | userStories | conditional | loop | method
```
for (DV in Roles) { userStories }
```

```
roles {

    SM : "Alice" ,

    PO : "Bob",

    DV : "Charlie" ,

    DV:  "Dave",

    QA : "Eve"
}

// Definición de User Stories

userStories {

    US "US1" {

        T: "Implement login feature",

        TY: Feature,

        PS: DV : "Charlie"

        DS: "As a user, I want to log in to access my account.",

        ET: 5,

        AC: "User can log in successfully",

        UST: "Design", "Development", "Testing"

    },

    US "US2" {

        T: "Fix login bug",

        TY: Fix,

        PS: QA : "Eve",
        
        DS: "Fix the bug in the login feature.",

        ET: 3,

        AC: "User can log in without errors",

        UST: "Debugging", "Testing"

    }

}


// Método personalizado para asignación aleatoria

funct assignRandom {

    for (userStory in UserStories) {

        if (userStory.PS == "") {

            userStory.PS = roles[randomInt(0, len(roles)-1)]

        }

    }

}
```