
# only new thing added here: make the compiled JS output
%.js: %.rkt
	cat $< | racket -t compile-stdin.rkt -m > $@

clean:
	rm *.js

%.test: %.run %.rkt
	@test "$(shell ./$(<))" = "$(shell racket $(word 2,$^))"
