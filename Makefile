all:
	/usr/local/bin/elm-make addressing.elm gateway.elm tridonic.elm --output addressing.html
	/usr/local/bin/elm-make main.elm gateway.elm tridonic.elm --output index.html
	/usr/local/bin/elm-make proxy.elm gateway.elm tridonic.elm --output proxy.html

test:
	/usr/local/bin/elm-test TestRunner.elm

deploy: all
	scp *.html 6afd:/usr/www
