.PHONY: vendor binary clean deploy update-deps force-binary

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

force-binary:
	rm bin/magitek
	sbcl --load "src/build.lisp"

bin/magitek: $(lisps)
	force-binary

# Deploy ----------------------------------------------------------------------

# Server
update-deps:
	hg -R /home/sjl/lib/cl-losh     -v pull -u
	hg -R /home/sjl/lib/chancery    -v pull -u
	hg -R /home/sjl/lib/trivial-ppm -v pull -u
	hg -R /home/sjl/lib/flax        -v pull -u

# Local
deploy:
	rsync --exclude=bin --exclude=.hg --exclude=database.sqlite --exclude='*.fasl' --exclude='*.png' --exclude='*.pnm'  -avz . jam:/home/sjl/src/magitek
	ssh jam make -C /home/sjl/src/magitek update-deps force-binary

