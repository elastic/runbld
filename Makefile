.PHONY: package test

package:
	./check.sh
	git rev-parse HEAD >resources/build.txt
	lein do clean, test :all, package

test:
	cd test/repo/java/no-errors && mvn test
	-cd test/repo/java/some-errors && mvn test
	-cd test/repo/python/no-errors && \
	nosetests --with-xunit --xunit-file=TEST-no-errors.xml -w tests/
	-cd test/repo/python/some-errors && \
	nosetests --with-xunit --xunit-file=TEST-some-errors.xml -w tests/
	lein test
