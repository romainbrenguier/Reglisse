var uncontrollable_A : (int 4)[8][8];
var uncontrollable_B : (int 4)[8][8];
var controllable_C : (int 4)[8][8];
var err : bool;

err <- !(
    (controllable_C[0][0] = (uncontrollable_A[0][0] * uncontrollable_B[0][0] + uncontrollable_A[0][1] * uncontrollable_B[1][0]))
)
;

