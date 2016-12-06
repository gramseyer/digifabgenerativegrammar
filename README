### CMSC22100-1 Digital Fabrication Final Project

## Generative Grammar for 3D Models

# Geoffrey Ramseyer and Hannah Brodheim

Generative grammar uses a sphere print head to generate an openSCAD file.  

Grammar is turing complete and allows the declaration of functions at the beginning of input

Sample Basic File:
DRAW; MOVE (40*t) (8*t*t) (12*t*t*t*t) (3*t); MOVE (0) (30*t) (0) (~1);


Examples:
Pipe with varying internal radius
Inverting partial or entire object
Hollow or fill object -> list of records -> after each DRAW -> make an ERASE with pos modified appropriately and vice versa
Scaling/Stretching  -> stretch arond the print head -> list of records -> replace each with an expression on radius -> push state onto stack before execution
Change print head shape using a module to replace our call to sphere, requirement that the orientation of the print head doesn't change
Reflections -> take a normal vector, use start point of the perturbation, modify the points in each draw, erase, move

Add sin/cos
Potentially other base shapes 
Some cool programs 