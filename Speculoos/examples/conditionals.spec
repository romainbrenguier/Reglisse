var gate : bool[3];
init gate[1] <- true;

when (gate[0]) { gate[1] <- ! gate[2]; } 

if (gate[1]) then {gate[0] <- gate[2]; } else {gate[2] <- gate[0];}

