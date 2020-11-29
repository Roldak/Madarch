all:
	gprbuild madarch.gpr -XWindowing_System=x11 -Xmode=debug -p
	gprbuild examples/support_test/test.gpr -XWindowing_System=x11 -Xmode=debug -p
	gprbuild examples/simple_scene/simple.gpr -XWindowing_System=x11 -Xmode=debug -p

clean:
	gprclean madarch.gpr -XWindowing_System=x11 -Xmode=debug -p
	gprclean examples/support_test/test.gpr -XWindowing_System=x11 -Xmode=debug -p
	gprclean examples/simple_scene/simple.gpr -XWindowing_System=x11 -Xmode=debug -p

support_test: all
	./examples/support_test/bin/main

support_test_gdb: all
	gdb --args ./examples/support_test/bin/main

simple_scene: all
	./examples/simple_scene/bin/main
