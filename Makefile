.PHONY: all test lint lint2 watch clean

all: 	lint2 test

test:
	echo "spin:<span foreground='#ff3'>TEST</span>" > /tmp/spin-test-status
	cd spin-core; clojure -Atest && echo "spin:PASS" > /tmp/spin-test-status || echo "<span foreground='red'>spin:FAIL</span>" > /tmp/spin-test-status

lint:
	clojure -Alint

lint2:
	clj-kondo --lint src/juxt --lint test/juxt

watch:
	find . -name "*.clj" | entr make test

clean:
	rm /tmp/spin-test-status
