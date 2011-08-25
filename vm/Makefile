.PHONY: all
all:

Makefile: ;

no-dot-config-targets := clean mrproper help

config-targets := 0
mixed-targets  := 0
dot-config     := 1

ifneq ($(filter $(no-dot-config-targets), $(MAKECMDGOALS)),)
	ifeq ($(filter-out $(no-dot-config-targets), $(MAKECMDGOALS)),)
		dot-config := 0
	endif
endif

ifneq ($(filter config %config,$(MAKECMDGOALS)),)
	config-targets := 1
	ifneq ($(filter-out config %config,$(MAKECMDGOALS)),)
		mixed-targets := 1
	endif
endif

ifeq ($(mixed-targets),1)
# We're called with mixed targets (*config and build targets).
# Handle them one by one.

%:: FORCE
	make $@

else
ifeq ($(config-targets),1)
# *config targets only - make sure prerequisites are updated, and descend
# in scripts/kconfig to make the *config target

.PHONY: config_generated
config_generated:
	mkdir -p include/config include/generated

nconfig: config_generated FORCE
	make -C kconfig nconf
	./kconfig/nconf Kconfig

config: config_generated FORCE
	make -C kconfig conf
	./kconfig/conf Kconfig

%config: config_generated FORCE
	make -C kconfig conf
	./kconfig/conf --$@ Kconfig

else
# Build targets only -- generally all targets except *config targets.

ifneq ($(dot-config),0)

# Used to fix quoted Kconfig variables
unquote = $(patsubst "%,%,$(patsubst %",%,$(1)))

# Read in config
include include/config/auto.conf

# Read in dependencies to all Kconfig* files, make sure to run
# oldconfig if changes are detected.
include include/config/auto.conf.cmd

# To avoid any implicit rule to kick in, define an empty command
.config include/config/auto.conf.cmd: ;

# If .config is newer than include/config/auto.conf, someone tinkered
# with it and forgot to run make oldconfig.
# if auto.conf.cmd is missing then we are probably in a cleaned tree so
# we execute the config step to be sure to catch updated Kconfig files
include/config/%.conf: .config include/config/auto.conf.cmd
	make silentoldconfig

include/arch: include/config/auto.conf
	@rm -f include/arch
	@ln -sf ../$(arch)/include/ include/arch

CROSS  := $(call unquote,$(CONFIG_CROSS_COMPILER))
CFLAGS := -Iinclude/ -DPICOBIT -Wall -Werror -Os

arch  = arch/$(call unquote,$(CONFIG_ARCH))
obj-y = $(addprefix $(arch)/,$(arch-y)) \
	  $(addprefix core/,$(core-y))

arch-all:

include core/Makefile
include arch/Makefile

all: include/arch arch-all

endif # $(dot-config)

endif # $(config-targets)
endif # $(mixed-targets)

clean: FORCE
	rm -f include/arch picobit-vm picobit.elf picobit.hex

mrproper: clean FORCE
	make -C kconfig clean
	rm -f .config .config.old
	rm -rf include/config include/generated

help:
	@echo "Picobit Virtual Machine"
	@echo "  Picobit uses Kconfig for configuring its VM build"
	@echo "  process. Configuration options are mostly same."
	@echo
	@echo "Configuration:"
	@echo "  config: command-line configurator"
	@echo "  nconfig: ncurses-based configurator"
	@echo "  oldconfig: update configuration using current .config as base"
	@echo "  silentoldconfig: same as oldconfig, but do not ask about new options"
	@echo
	@echo "Building:"
	@echo "  all: build everything"
	@echo
	@echo "Cleaning:"
	@echo "  clean: remove intermediate files"
	@echo "  mrproper: remove intermediate files and configuration"

.PHONY: FORCE