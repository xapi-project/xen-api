#
#

# K&R style indentation
INDENT += -nbad -bap -nbc -br -brs -c33 -cd33 -ncdb -ce -ci4 -cli0
INDENT += -d0 -di1 -nfc1 -i4 -ip0 -l75 -lp -npcs
INDENT += -npsl -nsc -nsob

format:
	find . -name '*.[ch]' | xargs -n1 indent -nut $(INDENT)
