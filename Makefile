.PHONY: test deploy

test:
	bin/kaocha --no-watch

integrant.tools.jar: src/**/*
	clojure -Srepro -M:jar

pom.xml: deps.edn
	clojure -Srepro -Spom

deploy: pom.xml test integrant.tools.jar
	clojure -M:deploy

clean:
	rm integrant.tools.jar
