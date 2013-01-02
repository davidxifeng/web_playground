#update date Thu Jan  3 04:17:08 HKT 2013
all:
	cabal build

help:
	@echo "本功能不能直接使用ghc来简单编译了,需要指定版本,或者使用cabal"
	@echo "使用: config配置; all构建; sudo make run运行;sudo make kill关闭"

config:
	cabal configure

link:
	ln -s dist/build/david-happy/david-happy web

run:
	./web &

kill:
	pkill web
