.PHONY : test
test : clean
	jbuilder build source.exe
	_build/default/source.exe
	! test -f bisect0001.out
	jbuilder clean
	BISECT_ENABLE=YES jbuilder build source.exe
	_build/default/source.exe
	test -f bisect0001.out

.PHONY : clean
clean :
	jbuilder clean
	rm -f bisect*.out
