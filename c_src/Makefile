.SUFFIXES: .erl .beam .yrl

ERL_TOP_RUN = /usr/local/lib/erlang
ERL_INT_VERSION = 3.5.5.4
ERL_INT_INC = $(ERL_TOP_RUN)/lib/erl_interface-$(ERL_INT_VERSION)/include
ERL_INT_LIB = $(ERL_TOP_RUN)/lib/erl_interface-$(ERL_INT_VERSION)/lib

OBJECTS = cdb_drv.so

CDB_PREFIX = /usr/local

all: $(OBJECTS)

cdb_drv.so:
	test -d ../priv/lib || mkdir -p ../priv/lib
	$(CC) -I$(CDB_PREFIX)/include -I$(ERL_TOP_RUN)/usr/include -I$(ERL_INT_INC) -o ../priv/lib/cdb_drv.so -fpic -shared cdb_drv.c $(ERL_INT_LIB)/libei.a -L$(CDB_PREFIX)/lib -lcdb 

clean:
	rm -f *.o *.so ../priv/lib/*
