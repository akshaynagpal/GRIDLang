# GridLang
GRIDLang is a language to design games in an intuitive and expressive manner. It enables developers to quickly prototype grid-based games and get a programmatic view of it.  It simplifies the process of defining rules for a game, creating a grid and manipulating them. There are in-built language components focused on game development that enables developers to express more with less lines of code.    
GRIDLang is a move-driven language. The control flow of the program depends on the movement of pieces on a grid structure. This allows for template functions that will be triggered based on pre-specified events, so that the programmer can focus on the game logic without having to worry about controlling the overall flow. 

## Language Tutorial
### Using the Compiler
Inside the GRIDLang directory, type `make`. This creates grid.native, which generates the LLVM IR when given a file in GRIDLang. A file can be run by using `./gridrun.sh -r filename.grid`
### Sample Programs
A simple **helloworld.grid** program in our language can be written as follows:
```
int setup() { 
  print("Hello World");
  return 0;
}
```
- _setup()_ is the mandatory function from where the execution of the program begins. It returns an int value.
- _print()_ is a built-in function that can be used to print string, int and bool.

Below is another sample program that demonstrates some additional features. It will print the string _z_ five times.
```
int checkGameEnd() {
    if (count > 0) {
        count = count -1;
        return 0;
    }
    return 1;
}

int setup() {
  return 0;
}

int gameloop() {
    string z;
    z = "Write this 5 times";
    print(z);
    return 0;
}
```
- The function _gameloop()_ is implicitly called from _setup()_.
- _gameloop()_ runs in a loop until the function _checkGameEnd()_ returns 1
- Once _checkGameEnd()_ returns 1, the program execution ends.

This final sample demonstrates the non-primitive data types _Player_, _Piece_ and _Grid_.
```
import gridBasics.grid

int checkGameEnd() {
  return 1;
}

Piece Token {
  string color;
}

Player { 
  Piece Token t;
}

Grid_Init<5,4>;
Player p1,p2;
Player[2] playerOrder;

int setup() { 
  playerOrder[0] = p1;
  playerOrder[1] = p2;
  playerOrderSize = 2;
  p1.t.color = "White";
  p2.t.color = "Black";
  p1.t.displayString = "Token1";
  p2.t.displayString = "Token2";
  Grid <1,2> <-- p1.t;
  Grid <3,2> <-- p2.t;
  printGrid();
  return 0;
}
```

- _gridBasics.grid_ is the library that must be included for any grid-related functionality.
- _Piece_ is a structure that can be placed on the grid. It has the default property _displayString_ (the string that represents it on the grid).
- A _Player_ is a structure that can hold _Pieces_ and other properties.
- `Grid\_Init<x,y>` creates a grid with _x_ rows and _y_ columns, on which Pieces can be placed.
- `Grid<x,y> <-- p1.t` adds the _Piece p1.t_ to the location <x,y> on the grid
- `printGrid()` is a library function that prints the grid.

For full fledged games written in GRIDLang, you can find a mini version of **chess** and **snakes and ladders** in [games](games) folder.     
The full report on the language along with the Language Reference Manual can be found [GRIDLang_FinalReport.pdf](GRIDLang_FinalReport.pdf)
