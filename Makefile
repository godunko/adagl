GPRBUILDDB ?= ~/local/matreshka/share/gprconfig

all:
	gprbuild -p -P gnat/adagl.gpr --target=javascript --db ${GPRBUILDDB}
	gprbuild -p -P gnat/adagl.gpr

clean:
	rm -rf .objs

example:
	gprbuild -p -P gnat/adagl_examples.gpr
