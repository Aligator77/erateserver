PROJECT = erateserver
ERLC_OPTS= "+{parse_transform, lager_transform}"
SHELL_OPTS = +P 2000000 +sbwt none +sbt ts -config sample.config

DEPS = erater cowlib ranch cowboy lager yamerl
dep_erater = git https://localhost/erater # Fake URL to avoid downloading package index
dep_cowlib = git https://github.com/ninenines/cowlib.git 0.6.2
dep_ranch = git https://github.com/ninenines/ranch.git 0.10.0
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.0
dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_yamerl = git https://github.com/gleber/yamerl.git 283321909946dfd72da5ba88e5950326d4893fa2

include erlang.mk
