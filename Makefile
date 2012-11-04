#use this file as root
all:
	ghc --make -threaded myhappy.hs -o mw
	./mw &

k:
	pkill mw
	# echo $(pkill mw && echo "kill it")
