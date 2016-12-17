var uncontrollable_request : int 64;
var request : int 64;
var device : int 64;
var controllable_write : int 64;
var err : bool;
var time_out : int 2;
var controllable_ack : bool;

device <- controllable_write;
time_out <- if time_out < 3 then time_out + 1 else time_out;
request <- if time_out = 0 then uncontrollable_request else request;
err <-  ((time_out = 3) & ! controllable_ack) || (controllable_ack & (! (device = request)));
