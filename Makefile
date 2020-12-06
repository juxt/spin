.PHONY: all test lint lint2 watch clean doc

all: 	lint2 test

test:
	echo "spin:<span foreground='#ff3'>TEST</span>" > /tmp/spin-test-status
	clojure -Atest && echo "spin:PASS" > /tmp/spin-test-status || echo "<span foreground='red'>spin:FAIL</span>" > /tmp/spin-test-status

lint:
	clojure -Alint

lint2:
	clj-kondo --lint src/juxt --lint test/juxt

watch:
	find . -name "*.clj" | entr make test

doc:	target/README.html

doc-deploy:	target/README.html
	aws s3 cp $< s3://rest.guide/README.html

target/README.adoc:	README.adoc doc/locate-resource.adoc doc/service-available.adoc
	bb build.clj

target/README.html:	target/README.adoc css/tufte.css
	cp css/* target
	asciidoctor $<


clean:
	rm /tmp/spin-test-status
