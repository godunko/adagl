
all:
	gprbuild -p -P gnat/adagl.gpr --target=javascript --db ~/local/matreshka/share/gprconfig
	gprbuild -p -P gnat/adagl.gpr

clean:
	rm -rf .objs
