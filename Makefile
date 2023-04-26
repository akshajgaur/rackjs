
# only new thing added here: make the compiled JS output
%.js: %.rkt compile.rkt
	cat $< | racket -t compile-stdin.rkt -m > $@

clean:
	rm *.js

%.test: %.run %.rkt
	@test "$(shell ./$(<))" = "$(shell racket $(word 2,$^))"
