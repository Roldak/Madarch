all:
	gprbuild madarch.gpr -XWindowing_System=x11 -Xmode=debug -p

run: all
	./bin/main
