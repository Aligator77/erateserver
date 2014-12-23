PROJECT = erateserver
ERLC_OPTS= "+{parse_transform, lager_transform}"

DEPS = erater cowboy lager yamerl
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_yamerl = git https://github.com/gleber/yamerl.git

include erlang.mk
