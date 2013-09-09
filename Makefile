all: test-serial-parsegen

test-serial-%: %.bin
	RUST_TEST_TASKS=1 time ./$<

test-%: %.bin
	time ./$<

DEBUGGING=-Z debug-info
#DEBUGGING=-O

%.bin: %.rs
	time rustc --cfg test $(DEBUGGING) -o $@ $<
