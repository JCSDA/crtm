.PHONY: all clean

all:
	cd cmdline_tools; make IDL=$(IDL)
	cd dist_tools; make IDL=$(IDL)

clean:
	cd cmdline_tools; make clean
	cd dist_tools; make clean
