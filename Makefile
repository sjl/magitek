.PHONY: vendor binary clean

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp|ros)$$')

binary: bin/magitek

bin/magitek: $(lisps)
	sbcl --load "src/build.lisp"

# Deploy ----------------------------------------------------------------------

# Server
update-deps:
	hg -R /home/sjl/lib/cl-losh -v pull -u
	hg -R /home/sjl/lib/chancery -v pull -u

# Local
deploy: build/magitek
	rsync --exclude=bin --exclude=.hg --exclude=database.sqlite  -avz . jam:/home/sjl/magitek
	ssh jam make -C /home/sjl/magitek update-deps binary

