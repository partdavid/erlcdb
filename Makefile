MODULES = \
	src c_src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done
