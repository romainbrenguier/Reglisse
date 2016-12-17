var bit : bool[3];
var a : bool[2];
var b : bool[2];

bit[0] <- a[0] ^ b[0];
bit[1] <- ((a[1]) ^ b[1]) ^ ((a[0]) & b[0]);
bit[2] <- ((a[1]) & b[1]) || ( (a[0]) & (b[0]) & ((a[1]) || b[1]));