/*
 * Erlang driver for cdb, using tinycdb library:
 * http://www.corpit.ru/mjt/tinycdb.html
 * Author: partdavid@gmail.com
 *
 * Copyright 2008 partdavid at gmail.com
 *
 * This file is part of erlcdb.
 *
 * erlcdb is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * erlcdb is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with erlcdb.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <ei.h>
#include <erl_driver.h>
#include <setjmp.h>
#include <cdb.h>

/* Internal errors */
#define ERR(prob) longjmp(env, prob)
#define IFERR(expr, prob) if ((expr) < 0) longjmp(env, prob)
#define WRONG_VERSION 100
#define NOT_TUPLE 101
#define NO_TYPE 102
#define NOT_ATOM 103
#define NOT_STRING 104
#define OPEN_WRONG_ARGS 105
#define LOOKUP_WRONG_ARGS 106
#define NOT_INTEGER 107
#define CLOSE_WRONG_ARGS 108
#define UNKNOWN_COMMAND 666

#define FREE(p) ((p) == NULL ? 0 : (free(p), p = NULL) )

/* Here's where I could store a correspondence between cdb handles and
 * whatever I want to get in my request--but let's just make them
 * file descriptors, since that's what cdb wants anyway. */

typedef struct {
   ErlDrvPort port;
} cdb_data;

static ErlDrvData cdb_drv_start(ErlDrvPort port, char *buf) {
   cdb_data *d = (cdb_data *) driver_alloc(sizeof (cdb_data));
   d->port = port;
   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   return (ErlDrvData) d;
}

static void cdb_drv_stop(ErlDrvData handle) {
   driver_free((char *) handle);
}

static void cdb_drv_output(ErlDrvData handle, char *buf, int buflen) {
   cdb_data *d = (cdb_data *)handle;
   jmp_buf env;
   int index, version, arity, type, size;
   ei_x_buff outb;
   ei_x_buff *out;
   int exc;
   FILE *log;
   char *command = NULL;
   char *filename = NULL;
   char *key = NULL;
   char *val = NULL;
   int fd, rv;
   unsigned klen, vlen, vpos;
   struct cdb cdb_handle;
   struct cdb *cdbh;
   struct cdb_find cdb_find_handle;
   struct cdb_find *cdbf;

   out = &outb;
   cdbh = &cdb_handle;
   cdbf = &cdb_find_handle;

   if (exc = setjmp(env)) {
      /* Something failed. If these fail I can't do much, don't bother
       * checking returns */
      ei_x_new_with_version(out);
      ei_x_encode_tuple_header(out, 2);
      ei_x_encode_atom(out, "error");
      ei_x_encode_long(out, (long) exc);
      driver_output(d->port, out->buff, out->index);
      ei_x_free(out);
   } else {
      index = 0;
      IFERR(ei_decode_version(buf, &index, &version), WRONG_VERSION);
      IFERR(ei_decode_tuple_header(buf, &index, &arity), NOT_TUPLE);
      IFERR(ei_get_type(buf, &index, &type, &size), NO_TYPE);

      command = malloc(size + 1);

      IFERR(ei_decode_atom(buf, &index, command), NOT_ATOM);
      if (strcmp(command, "open") == 0) {
         if (arity != 2) {
            ERR(OPEN_WRONG_ARGS);
         } else {
            IFERR(ei_get_type(buf, &index, &type, &size), NO_TYPE);
            filename = malloc(size + 1);
            IFERR(ei_decode_string(buf, &index, filename), NOT_STRING);
            IFERR(fd = do_open(filename), fd);
            FREE(filename);
            ei_x_new_with_version(out);
            ei_x_encode_tuple_header(out, 2);
            ei_x_encode_atom(out, "ok");
            ei_x_encode_long(out, (long) fd);
            driver_output(d->port, out->buff, out->index);
            ei_x_free(out);
         }
      } else if (strcmp(command, "lookup") == 0) {
         if (arity != 3) {
            ERR(LOOKUP_WRONG_ARGS);
         } else {
            /* For now, just strings */
            IFERR(ei_decode_long(buf, &index, (long *)&fd), NOT_INTEGER);
            IFERR(ei_get_type(buf, &index, &type, &size), NO_TYPE);
            key = malloc(size + 1);
            IFERR(ei_decode_string(buf, &index, key), NOT_STRING);
            klen = strlen(key);
            ei_x_new_with_version(out);
            IFERR(rv = cdb_init(cdbh, fd), rv);
            IFERR(rv = cdb_findinit(cdbf, cdbh, key, klen), rv);
            /* TODO: get rid of extraneous malloc-ing */
            while (cdb_findnext(cdbf) > 0) {
               vpos = cdb_datapos(cdbh);
               vlen = cdb_datalen(cdbh);
               val = malloc(vlen);
               cdb_read(cdbh, val, vlen, vpos);
               ei_x_encode_list_header(out, 1);
               ei_x_encode_string_len(out, val, vlen);
               FREE(val);
            }
            ei_x_encode_empty_list(out);
            driver_output(d->port, out->buff, out->index);
            FREE(key);
            cdb_free(cdbh);
         }
      } else if (strcmp(command, "close") == 0) {
         if (arity != 2) {
            ERR(CLOSE_WRONG_ARGS);
         } else {
            IFERR(ei_decode_long(buf, &index, (long *)&fd), NOT_INTEGER);
            close(fd);
            ei_x_new_with_version(out);
            ei_x_encode_atom(out, "ok");
            driver_output(d->port, out->buff, out->index);
         }
      } else {
         ERR(UNKNOWN_COMMAND);
      }
      FREE(command);
   }
}

int do_open(char *filename) {
   int fd = open(filename, O_RDONLY, 0);
   return fd;
}

ErlDrvEntry cdb_driver_entry = {
   NULL,               /* init */
   cdb_drv_start,      /* start */
   cdb_drv_stop,       /* stop */
   cdb_drv_output,     /* output */
   NULL,               /* ready_input */
   NULL,               /* ready_output */
   "cdb_drv",          /* driver_name */
   NULL,               /* finish */
   NULL,               /* control */
   NULL,
   NULL
};

DRIVER_INIT(cdb_drv) {
   return &cdb_driver_entry;
}
