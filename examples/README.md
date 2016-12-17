This directory contains different kind of examples illustrating the capacities of Reglisse.

# ex1.rgl 
Simple example, with only one module, whose output should reflect what happens to the input one step after.

# ex2.rgl 
Same kind of specifications but with two submodules.
The two modules can be synthesized independently and thus adversarial synthesis should succeed.

# flip-flop.rgl
Simple example with only one module, showing how to specify a flip-flop with reglisse.

# mux.rgl
Simple mux module with one module.

# request_response.rgl
Simple module with an arbitrer and a mutual exclusion condition.

# driver.rgl 
Models a system composed of a driver and two schedulers.
The driver cannot be synthesized without making some assumptions on the scheduler, hence adversary synthesis should fail.

# washing_cycle_*.rgl
Serie of examples with a several modules representing controllers for tank of water and a module for controlling a light.
Each of them should be synthesizable independently so adversarial synthesis should succeed.

# washing_cycle_mutex_*.rgl
Similar to the previous serie but with an additional mutual exclusion condition between tanks.
This should make adversarial synthesis fail while the other succeed.
