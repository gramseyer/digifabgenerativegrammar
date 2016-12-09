# CMSC22100-1 Digital Fabrication Final Project

## Generative Grammar for 3D Models

### Geoffrey Ramseyer and Hannah Brodheim

#### Project Overview

This projects uses a description of a sphere's motion through space to generate a 3D model.
Specifically, it compiles to OpenSCAD code. 

The goal of the project is to create a language to allow the encoding of models that cannot
be easily or efficiently described in other languages.  For example, our language naturally 
encodes fractals, arbitrary curves through space, and hollow objects with varying wall thickness.
It's goal is not to represent all objects, but to fill in some of the gaps left by other 
modeling software.

Sample Basic File:

DRAW; MOVE (40\*t) (8\*t\*t) (12\*t\*t\*t\*t) (3\*t); MOVE (0) (30\*t) (0) (~1);


#### Examples of things we've encoded with just a few lines:

(More examples can be found in examples/)

* Pipe with varying internal radius

* Inverting partial or entire object

* Hollow or fill object -> list of records -> after each DRAW -> make an ERASE with pos modified appropriately and vice versa

* Scaling/Stretching  -> stretch arond the print head -> list of records -> replace each with an expression on radius -> push state onto stack before execution

* Change print head shape using a module to replace our call to sphere, requirement that the orientation of the print head doesn't change

* Reflections -> take a normal vector, use start point of the perturbation, modify the points in each draw, erase, move

#### Language Description

`
Program : Definition* Statement
        ;

Definition : "(" "DEFINE" "(" (Identifier*) ")" Statement ")"
		   ;

Statement : MOVE Expr Expr Expr Expr ";" Statement
          | "[" Statement "]" ";" Statement
          | "{" Identifier Expr* "}" ";" Statement
          | "IF" Expr "THEN" Statement "ELSE" Statement ";" Statement
          | "ROTATEX" Expr ";" Statement
          | "ROTATEY" Expr ";" Statement
          | "ROTATEZ" Expr ";" Statement
          | "ERASE" ";" Statement
          | "DRAW" ";" Statement
          | "FREEMOVE" ";" Statement
          | Perturb Statement
          |
          ;

Perturb : "HOLLOW" Expr Expr Statement ";"
        | "INVERT" Statement ";"
        ;

Identifier : [a-zA-Z]*
		   ;

Expr : (the usual rightmost expression parse tree, with proper associativity.  Allows some basis function calls like sin/cos)


`