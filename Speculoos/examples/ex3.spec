var gate : bool[3];

gate[0] <- ! gate[2];
gate[1] <- ! gate[0];
gate[2] <- ! gate[1];
