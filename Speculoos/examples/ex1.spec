var request : bool;
var state : ready | busy;


init state <- ready; 

if (state = ready) & request then {state <- busy;} else {state <- ready;}
