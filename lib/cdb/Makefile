MODULES = \
	src c_src

all clean docs install:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done
