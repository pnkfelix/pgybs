all: test-parsegen

test-%: %.bin
	./$<

#DEBUGGING=-Z debug-info
DEBUGGING=

%.bin: %.rs
	rustc --cfg test $(DEBUGGING) -o $@ $<
