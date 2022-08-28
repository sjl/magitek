.PHONY: deploy clean

lisps := $(shell ffind '\.(asd|lisp)$$')

bin/magitek:
	sbcl --disable-debugger --load 'src/build.lisp'

deploy: bin/magitek
	rsync -avz bin/ jam:magitek/bin

clean:
	rm -rf bin/
