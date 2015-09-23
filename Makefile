PROJECT = erateserver
ERLC_OPTS= "+{parse_transform, lager_transform}"

NID := 1
SHELL_OPTS = -sname ers$(NID) +P 2000000 +sbwt none +sbt ts +pc unicode -boot start_sasl -sasl errlog_type error -config sample.config

DEPS = erater cowlib ranch cowboy lager yamerl
dep_erater = git https://localhost/erater # Fake URL to avoid downloading package index
dep_cowlib = git https://github.com/ninenines/cowlib.git 0.6.2
dep_ranch = git https://github.com/ninenines/ranch.git 0.10.0
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_yamerl = git https://github.com/gleber/yamerl.git 283321909946dfd72da5ba88e5950326d4893fa2

include erlang.mk

START_OPTS = $(SHELL_OPTS) -s erateserver
CONFIG = logs/erateserver-$(NID).yml
start:
	sed 's|port: 2880|port: 288$(NID)|' erateserver.yml > $(CONFIG)
	$(gen_verbose) erl $(SHELL_PATH) $(START_OPTS) -erateserver config '"'$(CONFIG)'"' -lager log_root '"'logs/$(NID)'"'
