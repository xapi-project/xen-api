.PHONY : test
test :
	jbuilder build source.exe
	_build/default/source.exe
	! test -f bisect0001.out

.PHONY : clean
clean :
	jbuilder clean
	rm -f bisect*.out
