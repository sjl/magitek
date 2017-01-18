.PHONY: vendor binary

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp


# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp|ros)$$')

binary: build/magitek

build/magitek: $(lisps)
	ros build build/magitek.ros

update-deps:
	hg -R /home/sjl/chancery -v pull -u
	hg -R /home/sjl/cl-losh -v pull -u

deploy: build/magitek
	rsync --exclude=build/magitek --exclude=.hg --exclude=database.sqlite --exclude=corpora -avz . silt:/home/sjl/magitek
	ssh silt make -C /home/sjl/magitek update-deps
	ssh silt make -C /home/sjl/magitek build/magitek
