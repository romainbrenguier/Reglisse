var state : { x : int 4; y : int 4; time : int 4 };
var input : bool ;


if (state.y < 10 & input) 
then {state.x <- state.x + 1; } 
 else {state.x <- state.x - 1;}
state.y <- state.y;
when (state.y < 10 & input) {state.y <- state.y + 1;} 
state.time <- state.time + 1;



