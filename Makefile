TOOL = stack

.PHONY: all
all: build exec

build:
	$(TOOL) setup
	$(TOOL) build

exec:
	$(TOOL) exec hcs-exe

clean-stack:
	$(TOOL) clean --full
