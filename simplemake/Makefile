## ObjectSuffix           :=.o
##
## CXX                    := gcc
##CXXFLAGS               := -g
##
##
##
## OBJS = bar$(ObjectSuffix) main$(ObjectSuffix)
##OBJS = bar.o main.o
##
##
##
##%$(ObjectSuffix): %.c
##	$(CXX) -MMD -MP -MF"$(@:%$(ObjectSuffix)=%.d)"-MT"$@" -MT"$(@:%$(ObjectSuffix)=%.d)" -o "$@""$<"
##
##bar: $(OBJS)
##	$(CXX) $(CXXFLAGS) -o $@ $(OBJS)
##
##bar$(ObjectSuffix): bar.h
##	$(CXX) $(CXXFLAGS) -c bar.cpp
##
##main$(ObjectSuffix): main.cpp bar.h
##	$(CXX) $(CXXFLAGS) -c main.cpp
##
##OutputFile := bar
##all: $(OutputFile)


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
