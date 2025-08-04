#
# Makefile for Kokkos Integration Module
# Part of GNU Hurd Cognitive Architecture - Performance Framework
#

dir := performance/kokkos-integration
makemode := utility

SRCS = kokkos-hurd-bridge.cc kokkos-memory-space.cc kokkos-demo.cc
OBJS = $(SRCS:.cc=.o)
target = kokkos-hurd-demo

# Include main build configuration  
include ../../Makeconf

# C++ compilation settings
CXX = $(CC)
CXXFLAGS = $(CFLAGS) -std=c++14

# Kokkos-specific configuration
ifdef HAVE_KOKKOS
KOKKOS_CXXFLAGS = $(kokkos_CFLAGS)
KOKKOS_LIBS = $(kokkos_LIBS)
CPPFLAGS += -DHAVE_KOKKOS=1 $(KOKKOS_CXXFLAGS)
LDLIBS += $(KOKKOS_LIBS)
else
# Fallback when Kokkos is not available
CPPFLAGS += -DHAVE_KOKKOS=0
endif

# C++ compilation rule
%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

.PHONY: demo test

# Demonstration target
demo: $(target)
	@echo "🧠 === Running Kokkos-Hurd Integration Demo === 🧠"
	./$(target) --demo

# Test target  
test: $(target)
	@echo "⚡ === Testing Kokkos Parallel Computing Integration === ⚡"
	./$(target) --test