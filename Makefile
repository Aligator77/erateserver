PROJECT = erateserver
ERLC_OPTS= "+{parse_transform, lager_transform}"

DEPS = erater cowboy lager yamerl
dep_cowboy = pkg://cowboy 0.10.0
dep_lager = https://github.com/basho/lager.git 2.0.3
dep_yamerl = https://github.com/gleber/yamerl.git

include erlang.mk
