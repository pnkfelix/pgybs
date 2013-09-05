all: test-serial-parsegen

test-serial-%: %.bin
	RUST_TEST_TASKS=1 ./$<

test-%: %.bin
	./$<

DEBUGGING=-Z debug-info
#DEBUGGING=

%.bin: %.rs
	rustc --cfg test $(DEBUGGING) -o $@ $<
