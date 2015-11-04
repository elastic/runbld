package:
	git rev-parse HEAD >resources/build.txt
	lein do clean, package
