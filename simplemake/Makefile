##
## Params
##

OutputFile := bar
ObjectSuffix           :=.o
IntermediateDirectory  :=./Debug
MakeDirCommand         :=md
ObjectsFileList        :="TestTreeView.txt"
IncludePath            :=
ObjectSwitch           :=-o 
DependSuffix           :=.d

##
CXX                    :=g++
CXXFLAGS               :=-g
##
##
##
OBJS = bar$(ObjectSuffix) main$(ObjectSuffix)


##
## build 
##
.PHONY: all clean

#all: $(OutputFile)

#$(OutputFile): $(OBJS)
#	@echo $(MakeDirCommand) $(@D)
#	@echo "" > $(IntermediateDirectory)/.d
#	@echo $(OBJS)  > $(ObjectsFileList)
#	$(CXX) $(CXXFLAGS) -o $@ $(OBJS)



#%$(ObjectSuffix): %.cpp
#	$(CXX) -MMD -MP -MF"$(@:%$(ObjectSuffix)=%.d)"-MT"$@" -MT"$(@:%$(ObjectSuffix)=%.d)" -o "$@""$<"

#$(IntermediateDirectory)/src_Mornlight$(ObjectSuffix): ../../src/Mornlight.c $(IntermediateDirectory)/src_Mornlight$(DependSuffix)
#	$(CC) $(SourceSwitch) "E:/CoolDisk/Dev/MornLight/src/Mornlight.c" $(CFLAGS) $(ObjectSwitch)$(IntermediateDirectory)/src_Mornlight$(ObjectSuffix) $(IncludePath)
#$(IntermediateDirectory)/src_Mornlight$(DependSuffix): ../../src/Mornlight.c
#	@$(CC) $(CFLAGS) $(IncludePath) -MG -MP -MT$(IntermediateDirectory)/src_Mornlight$(ObjectSuffix) -MF$(IntermediateDirectory)/src_Mornlight$(DependSuffix) -MM "../../src/Mornlight.c"


#$(ObjectSuffix): $.cpp
#	$(CXX) $(CXXFLAGS) -c $ <

bar: $(OBJS)
	g++ -g -o bar $(OBJS)

#bar$(ObjectSuffix): bar.h
#	$(CXX) $(CXXFLAGS) -c bar.cpp
bar$(ObjectSuffix): bar.cpp bar$(DependSuffix)
	$(CXX) -c bar.cpp $(CXXFLAGS) $(ObjectSwitch)bar$(ObjectSuffix)
bar$(DependSuffix): bar.cpp
	@$(CXX) $(CXXFLAGS) -MG -MP -MTbar$(ObjectSuffix) -MFbar$(DependSuffix) -MM bar.cpp


#bar$(ObjectSuffix): bar.cpp bar$(DependSuffix)
#	$(CXX) $(SourceSwitch) "bar.cpp" $(CXXFLAGS) $(ObjectSwitch)bar$(ObjectSuffix) $(IncludePath)
#bar$(DependSuffix): bar.cpp
#	@$(CXX) $(CFLAGS) $(IncludePath) -MG -MP -MTbar$(ObjectSuffix) -MFbar$(DependSuffix) -MM "bar.cpp"
#
#main$(ObjectSuffix): main.cpp main$(DependSuffix)
#	$(CXX) $(SourceSwitch) "main.cpp" $(CXXFLAGS) $(ObjectSwitch)main$(ObjectSuffix) $(IncludePath)
#main$(DependSuffix): main.cpp
#	@$(CXX) $(CXXFLAGS) $(IncludePath) -MG -MP -MTmain$(ObjectSuffix) -MFmain$(DependSuffix) -MM "main.cpp"

#bar.o: bar.cpp bar.d
#	$(CXX) -c "bar.cpp" $(CXXFLAGS) -obar.o
#bar.d: bar.cpp
#	@$(CXX) $(CXXFLAGS) -MG -MP -MTbar.o -obar.d -MM "bar.cpp"

#main.o: main.cpp main.d
#	$(CXX) -c "main.cpp" $(CXXFLAGS) $(ObjectSwitch)main.o $(IncludePath)
#main.d: main.cpp
#	@$(CXX) $(CXXFLAGS) -MG -MP -MTmain.o -omain.d -MM "main.cpp"



## -----------------------------
#bar$(ObjectSuffix): bar.h
#	$(CXX) $(CXXFLAGS) -c bar.cpp

main$(ObjectSuffix): main.cpp bar.h
	$(CXX) $(CXXFLAGS) -c main.cpp


## -----------------------------
##bar: $(OBJS)
##	g++ -g -o bar $(OBJS)
##
##bar.o: bar.cpp bar.h
##	g++ -g -c bar.cpp
##main.o: main.cpp bar.h
##	g++ -g -c main.cpp


clean:
	-del /q bar.exe 
	-del /q bar.o main.o
