deploy:
	env CLOJARS_USERNAME=cjmurphy CLOJARS_PASSWORD=CLOJARS_go_to_clojars_to_see_this clj -X:deploy
build:
	clojure -X:jar :jar dev-lib.jar
