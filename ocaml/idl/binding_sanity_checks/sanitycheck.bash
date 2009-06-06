#!/bin/bash -x

#java and python binding sanity checks. Just a load of programs that should complete if everything's ok.
#Depends on three servers, two miami and one rio.

#The names and passwords of the servers are hard-coded (separately for the python and java programs). 
#Usually ivory and ebony, my test boxes, are the fall guys, and incubus is the rio server, which is why
#I don't run any destructive tests on rio.

#The miami servers will be substantially modified, and the secondary server will be completely blown away when it
#is ejected from the pool. Don't do this to servers that you love.
#The tests should all complete if these two start off as fresh identical miami installations, various starting configurations
#will result in one or two operations failing. (e.g. you can't make a pool with different versions...)

#The rio server will just be conversed with, hopefully not damaged.

set -e #so that we stop on the first fail

#rebuild all the java stuff from clean
make clean
make binding
make EventMonitor.class

#monitors up first, so they can report things about the other tasks, whilst testing themselves
xterm -hold -title "Sanity Check Event Monitor" -e "make EventMonitor.run" &
xterm -hold -title "Sanity Check Metrics Poller" -e "./metrics_monitor.py | tee -a sanitycheckmetrics" &

#some python scripts just to check everything's running
make getpifdetails.run sharedstorage.run storagerepositories.run

#build the java tests
make all

#some simple interrogatives
make Skeleton.run
make TwoConnections.run 
make miamigetAllRecords
make riogetAllRecords

#more general tests of state-changing things, but speedy
make DeprecatedMethod.run SharedStorage.run poolify.run
make checkpool.run metrics.run
make ConnectToRio.run 

#time consuming operations
make StartAllVMs.run
make create_vm.run powercycle.run
make CreateVM.run VMlifecycle.run
make AsyncVMCreate.run

#try this again now we've made some things
make metrics.run

#rats leave sinking ship
make migratetoprimary.run

#destroy the pool we made. quite bad for the secondary server
make unpoolify.run


