.PHONY: junit package test

GOLANG := $(shell command -v go 2>/dev/null)
GOJUNIT := $(shell command -v go-junit-report 2>/dev/null)
NOSE := $(shell command -v nosetests 2>/dev/null)

test:
	rm -rf tmp/git
	lein do clean, test

package: test
	./check.sh
	git rev-parse HEAD >resources/build.txt
	LEIN_SNAPSHOTS_IN_RELEASE=whynot lein package

junit:
ifndef GOLANG
	$(error "golang not installed")
endif
ifndef GOJUNIT
	$(error "go-junit-report not installed")
endif
ifndef NOSE
	$(error "nosetests not installed")
endif
	cd test/repo/java/no-errors && mvn test
	-cd test/repo/java/some-errors && mvn test
	-cd test/repo/python/no-errors && \
	nosetests --with-xunit --xunit-file=TEST-no-errors.xml -w tests/
	-cd test/repo/python/some-errors && \
	nosetests --with-xunit --xunit-file=TEST-some-errors.xml -w tests/
	-cd test/repo/go/no-errors && \
	go test -v | go-junit-report > TEST-no-errors.xml
	-cd test/repo/go/some-errors && \
	go test -v | go-junit-report > TEST-some-errors.xml
