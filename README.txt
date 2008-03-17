===
cdb
===

cdb is an OTP-compliant application using a linked-in driver to query
cdb (constant database) files. For more information about cdb, consult
Daniel J. Bernstein's page_. The driver uses the TinyCDB_ shared
library to query cdb files.

_page: http://cr.yp.to/cdb.html

_TinyCDB: http://www.corpit.ru/mjt/tinycdb.html

Requirements
============

The TinyCDB library must be installed.


Getting Started
===============

Building
--------

You can build the application by adjusting the paths in
``lib/cdb/c_src/Makefile`` and ``build/otp.mk``. Change directory
to the ``lib/cdb`` subdirectory and type ``make``. This will build
the application.

Running
-------

You can run the application by including the ``lib/cdb/ebin`` subdirectory
in Erlang's search path::

   erl -pz ebin

In the ``lib/cdb/example`` subdirectory are some files that can
be used to experiment with cdb files. You can also use these (and
files like them) to run the basic benchmark in **cdbbench**.

Installation
============

You can copy the built application into the system Erlang/OTP system using
the ``make install`` target, or you can use the OTP base build tools to create
a release in ``release`` subdirectory.

Documentation
=============

The application documentation can be built by using the ``docs`` make target
in the ``lib/spewf`` subdirectory (Note, you will first have to build the
**fslib** and **gas** applications by typing ``make`` in the ``lib``
subdirectory). This will build HTML documentation in ``lib/cdb/doc``.

License
=======

erlcdb is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

erlcdb is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
more details.

You should have received a copy of the GNU Lesser General Public License
along with erlcdb.  If not, see `http://www.gnu.org/licenses/`.
