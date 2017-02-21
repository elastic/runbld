.PHONY: package test

package:
	./check.sh
	git rev-parse HEAD >resources/build.txt
	lein do clean, test :all, package

test:
	cd test/repo/java/no-errors && mvn test
	-cd test/repo/java/some-errors && mvn test
	lein test
