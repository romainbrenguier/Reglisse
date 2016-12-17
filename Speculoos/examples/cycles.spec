var push : bool [3];
var controllable_a : bool [3]; 
var controllable_b : bool [3]; 
var cycle : (int 3)[3];
var controllable_lamp : bool;
var err : bool;

cycle[0] <- if (((cycle[0] = 0) & (! push[0])) || cycle[0] = 4) then 0 else cycle[0] + 1;
cycle[1] <- if (((cycle[1] = 0) & (! push[1])) || cycle[1] = 4) then 0 else cycle[1] + 1;
cycle[2] <- if (((cycle[2] = 0) & (! push[2])) || cycle[2] = 4) then 0 else cycle[2] + 1;
err &lt;-
(! (((cycle[0] = 1) &lt;-&gt; controllable_a[0]) &amp; ((cycle[0] = 3 || cycle[0] = 4) &lt;-&gt; controllable_b[0])))
|| 
(! (((cycle[1] = 1) &lt;-&gt; controllable_a[1]) &amp; ((cycle[1] = 3 || cycle[1] = 4) &lt;-&gt; controllable_b[1])))
|| 
(! (((cycle[2] = 1) &lt;-&gt; controllable_a[2]) &amp; ((cycle[2] = 3 || cycle[2] = 4) &lt;-&gt; controllable_b[2])))
|| 
(! (controllable_lamp &lt;-&gt; ((cycle[0] &gt; 0) || (cycle[1] &gt; 0) || (cycle[2] &gt; 0))))
;