# Makefile for Cognitive Kernel
# Self-evolving scaffolding for GNU Hurd ecosystem

dir := cogkernel
makemode := misc

DISTFILES := README.md

include ../Makeconf

# Guile modules for the cognitive kernel
GUILE_MODULES = atomspace/atomspace.scm \
               agents/agents.scm \
               attention/ecan.scm \
               tensors/tensors.scm \
               core.scm

# Installation directory for Guile modules
GUILE_SITE_DIR = $(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/cogkernel
GUILE_CCACHE_DIR = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/cogkernel

# Test files
TEST_FILES = tests/test-atomspace.scm \
            tests/test-agents.scm \
            tests/test-attention.scm \
            tests/test-core.scm

# All targets
all: compile-modules

# Check if Guile is available
check-guile:
	@which guile > /dev/null || (echo "Error: Guile not found. Please install Guile 3.0+" && exit 1)
	@echo "Guile found: $$(guile --version | head -1)"

# Compile Guile modules  
compile-modules: check-guile $(GUILE_MODULES)
	@echo "Compiling Cognitive Kernel modules..."
	@echo "Adding current directory to Guile load path..."
	@export GUILE_LOAD_PATH=".:$$GUILE_LOAD_PATH"; \
	guile -c "(compile-file \"atomspace/atomspace.scm\" #:output-file \"atomspace/atomspace.go\")"
	@export GUILE_LOAD_PATH=".:$$GUILE_LOAD_PATH"; \
	guile -c "(compile-file \"agents/agents.scm\" #:output-file \"agents/agents.go\")"
	@export GUILE_LOAD_PATH=".:$$GUILE_LOAD_PATH"; \
	guile -c "(compile-file \"attention/ecan.scm\" #:output-file \"attention/ecan.go\")"
	@export GUILE_LOAD_PATH=".:$$GUILE_LOAD_PATH"; \
	guile -c "(compile-file \"tensors/tensors.scm\" #:output-file \"tensors/tensors.go\")"
	@export GUILE_LOAD_PATH=".:$$GUILE_LOAD_PATH"; \
	guile -c "(compile-file \"core.scm\" #:output-file \"core.go\")"
	@echo "Cognitive Kernel modules compiled successfully"

# Test the cognitive kernel
test: compile-modules
	@echo "Testing Cognitive Kernel..."
	@guile -c "(add-to-load-path \".\") \
	          (use-modules (cogkernel core)) \
	          (cognitive-demo!)"

# Install Guile modules
install: compile-modules
	@echo "Installing Cognitive Kernel modules..."
	@mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/atomspace
	@mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/agents  
	@mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/attention
	@mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/tensors
	@mkdir -p $(DESTDIR)$(GUILE_CCACHE_DIR)/atomspace
	@mkdir -p $(DESTDIR)$(GUILE_CCACHE_DIR)/agents
	@mkdir -p $(DESTDIR)$(GUILE_CCACHE_DIR)/attention
	@mkdir -p $(DESTDIR)$(GUILE_CCACHE_DIR)/tensors
	@install -m 644 atomspace/atomspace.scm $(DESTDIR)$(GUILE_SITE_DIR)/atomspace/
	@install -m 644 agents/agents.scm $(DESTDIR)$(GUILE_SITE_DIR)/agents/
	@install -m 644 attention/ecan.scm $(DESTDIR)$(GUILE_SITE_DIR)/attention/
	@install -m 644 tensors/tensors.scm $(DESTDIR)$(GUILE_SITE_DIR)/tensors/
	@install -m 644 core.scm $(DESTDIR)$(GUILE_SITE_DIR)/
	@if [ -f atomspace/atomspace.go ]; then \
		install -m 644 atomspace/atomspace.go $(DESTDIR)$(GUILE_CCACHE_DIR)/atomspace/; \
	fi
	@if [ -f agents/agents.go ]; then \
		install -m 644 agents/agents.go $(DESTDIR)$(GUILE_CCACHE_DIR)/agents/; \
	fi
	@if [ -f attention/ecan.go ]; then \
		install -m 644 attention/ecan.go $(DESTDIR)$(GUILE_CCACHE_DIR)/attention/; \
	fi
	@if [ -f tensors/tensors.go ]; then \
		install -m 644 tensors/tensors.go $(DESTDIR)$(GUILE_CCACHE_DIR)/tensors/; \
	fi
	@if [ -f core.go ]; then \
		install -m 644 core.go $(DESTDIR)$(GUILE_CCACHE_DIR)/; \
	fi
	@echo "Cognitive Kernel installed successfully"

# Clean compiled files
clean:
	@echo "Cleaning Cognitive Kernel..."
	@rm -f *.go atomspace/*.go agents/*.go attention/*.go tensors/*.go
	@echo "Cleaned"

# Demo of cognitive capabilities
demo: working-demo.scm
	@echo "=== Cognitive Kernel Demo ==="
	@guile -s working-demo.scm

# Meta-agentic integration demo
meta-demo: issue-implementation.scm
	@echo "=== Meta-Agentic Cognitive Kernel Integration Demo ==="
	@guile -s issue-implementation.scm

# Enhanced GUIX integration test
guix-test: build/guix-integration.scm
	@echo "=== Enhanced GUIX Build Integration Test ==="
	@guile -s build/guix-integration.scm

# Full integration test
integration-test: full-integration-test.scm
	@echo "=== Full Cognitive Kernel Integration Test ==="
	@guile -s full-integration-test.scm

# Interactive Guile REPL with cognitive kernel loaded
repl: compile-modules
	@echo "Starting Guile REPL with Cognitive Kernel..."
	@echo "(use-modules (cogkernel core)) to get started"
	@guile -c "(add-to-load-path \".\")"

.PHONY: all check-guile compile-modules test install clean demo meta-demo guix-test repl