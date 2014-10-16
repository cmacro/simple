bar: bar.o main.o
	g++ -g -o bar main.o bar.o

bar.o: bar.cpp bar.h
	g++ -g -c bar.cpp
main.o: main.cpp bar.h
	g++ -g -c main.cpp

.PHONY: clean

clean:
	-del /q bar.exe 
	-del /q bar.o main.o
