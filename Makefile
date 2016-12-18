
all: 
	cd aiger && make
	cd Speculoos && make
	cd src && make
	cp src/main.byte reglisse
	cd simulator && make
	cp simulator/simulation.byte simulation

clean:
	cd aiger && make clean
	cd Speculoos && make clean
	cd src && make clean


