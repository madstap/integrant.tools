.PHONY: test deploy

test:
	bin/kaocha --no-watch

integrant.tools.jar: src/**/*
	clojure -Srepro -M:jar

pom.xml:
	clojure -Srepro -Spom

deploy: pom.xml test integrant.tools.jar
	clojure -M:deploy
