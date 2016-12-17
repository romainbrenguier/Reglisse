var uncontrollable_request : int 32;
var reqest : int 32;
var controllable_write : int 32;
var err : bool;

request <- uncontrollable_request;
err <-  ! (controllable_write = uncontrollable_request);
