
all: 
	cd src && make
	cp src/reglisse.byte reglisse
	cp src/simulation.byte simulation

clean:
	cd src && make clean


