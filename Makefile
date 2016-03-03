all:
	elm-make addressing.elm gateway.elm tridonic.elm --output addressing.html
	elm-make main.elm gateway.elm tridonic.elm --output index.html
	elm-make proxy.elm gateway.elm tridonic.elm --output proxy.html

deploy: all
	scp *.html 6afd:/usr/www
