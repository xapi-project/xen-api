CC := gcc

CFLAGS := -O1
CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls -fno-strict-aliasing -std=gnu99
CFLAGS += -mno-tls-direct-seg-refs
CFLAGS += -Wall -Wstrict-prototypes -Wno-unused-value -Wdeclaration-after-statement -Werror -Wmissing-prototypes
CFLAGS += -D__XEN_TOOLS__ -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_GNU_SOURCE

all: xenctrl_osdep_xiu.so
	@:

xeninterrcept_xiu.o: xenctrl_osdep_xiu.c
	$(CC) $(CFLAGS) -c -o xenctrl_osdep_xiu.o xenctrl_osdep_xiu.c

xenctrl_osdep_xiu.so: xenctrl_osdep_xiu.o
	$(CC) $(CFLAGS) -shared -o xenctrl_osdep_xiu.so xenctrl_osdep_xiu.o -lxenctrl

install: xenctrl_osdep_xiu.so
	mkdir -p $(DESTDIR)/opt/xensource/lib/xenctrl/
	cp xenctrl_osdep_xiu.so $(DESTDIR)/opt/xensource/lib/xenctrl/osdep_xiu.so

clean:
	rm -f xeninterrcept_xiu.o
	rm -f xenctrl_osdep_xiu.so

uninstall:
	rm -f $(DESTDIR)/opt/xensource/lib/xenctrl/osdep_xiu.so
