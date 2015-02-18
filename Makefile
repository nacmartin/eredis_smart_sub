PROJECT = eredis_smart_sub

DEPS = eredis
dep_eredis = git git://github.com/wooga/eredis.git master

.PHONY: release clean-release

include erlang.mk

ERLC_OPTS= $(ERLC_COMPILE_OPTS) +debug_info

