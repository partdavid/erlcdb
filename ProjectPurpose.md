Daniel J. Bernstein's [cdb file format](http://cr.yp.to/cdb.html) is a "fast, reliable, simple package for creating and reading constant databases."  [TinyCDB](http://www.corpit.ru/mjt/tinycdb.html) is somewhat more ergonomic implementation of the cdb format.

This project provides an Erlang linked-in port driver for searching cdb files. It's suitable for situations where large amounts of data from an external source need to be searched quickly, and the data is suitable only for regeneration, not incremental updating.

Initial benchmarks seem to show approximate parity between lookup times for dets and this cdb driver. This driver's operation can probably be improved (there are optimization opportunities in the driver itself, and there might be more efficient ways of communicating with it).

However, generating and regenerating cdb files is much faster than loading dets tables for large data sets.