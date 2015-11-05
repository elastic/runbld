package:
	./check.sh
	git rev-parse HEAD >resources/build.txt
	lein do clean, test :all, package
