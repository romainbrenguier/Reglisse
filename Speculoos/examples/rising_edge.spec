var x : bool;
var previous : bool;
var rising_edge : bool;

rising_edge <- false;
when (! previous) { rising_edge <- x; }
previous <- x;
