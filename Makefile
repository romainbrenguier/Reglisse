
all: 
	cd src && make
	cp src/main.byte reglisse
	cp src/simulation.byte simulation

clean:
	cd src && make clean


