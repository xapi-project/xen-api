default: all

clean: java-clean python-clean
	-rm *.svg
	-rm *.png
	-rm sanitycheckmetrics

#JAVA SECTION
#where is the source?
JAVA_OBJ=../java_binding/obj/
JAVA_XENAPI=$(JAVA_OBJ)com/xensource/xenapi/

PROGRAMJAVAFILES = $(wildcard *.java) 
BINDINGJAVAFILES = $(wildcard $(JAVA_XENAPI)*.java)

JAVAFILES  = $(PROGRAMJAVAFILES) $(BINDINGJAVAFILES) 
CLASSFILES = $(JAVAFILES:.java=.class)

CLASSPATH  = .:$(JAVA_OBJ):$(JAVA_OBJ)xmlrpc-client-3.1.jar:$(JAVA_OBJ)xmlrpc-common-3.1.jar:$(JAVA_OBJ)ws-commons-util-1.0.2.jar

#what are the build rules?
#.SECONDARY:  #stops make deleting the class files because they're intermediates

%.run: %.class binding SanityCheckSkeleton.class
	java -cp ${CLASSPATH} $*

%.class : %.java
	javac -cp ${CLASSPATH} $<


#what are the targets?
all: binding $(CLASSFILES)

binding: $(BINDINGJAVAFILES)
	javac -cp ${CLASSPATH} $(BINDINGJAVAFILES)
	touch binding

docs: $(JAVAFILES)
	javadoc -classpath ${CLASSPATH} -d doc -link http://java.sun.com/javase/6/docs/api $(JAVAFILES) 

java-clean:
	-rm *.class
	-rm $(JAVA_OBJ)*.class
	-rm $(XENAPI)*.class
	-rm -rf doc
	-rm binding

#special targets for running files with command line options
miamigetAllRecords: GetAllRecordsOfAllTypes.class binding SanityCheckSkeleton.class
	java -cp ${CLASSPATH} GetAllRecordsOfAllTypes miami


riogetAllRecords: GetAllRecordsOfAllTypes.class binding SanityCheckSkeleton.class
	java -cp ${CLASSPATH} GetAllRecordsOfAllTypes rio


#PYTHON SECTION
%.run: %.py
	./$<


python-clean:
	-rm *.pyc
