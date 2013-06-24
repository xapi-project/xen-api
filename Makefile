TARGET = rrd2csv

.PHONY: build
build: $(TARGET)

$(TARGET):
	omake $@

.PHONY: clean
clean:
	omake clean

.PHONY: install
install: $(TARGET)
	omake install
