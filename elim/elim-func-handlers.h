/*
Copyright © 2009 Vivek Dasmohapatra 

email : vivek@etla.org
irc   : fledermaus on freenode, oftc
jabber: fledermaus@jabber.earth.li

This file is part of elim.

elim is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

elim is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with elim.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef _ELIM_FUNC_HANDLERS_H_
#define _ELIM_FUNC_HANDLERS_H_

#include "handler-list.h"
#include "sexp/sexp-util.h"

typedef xmlnode * (*sexp_func) ( const char *name , 
                                 const char *id   ,
                                 SEXP_VALUE *args , 
                                 gpointer    data );

typedef struct _func_handler func_handler;
struct _func_handler
{
    char      *name;
    sexp_func  func;
};

extern func_handler handlers[];

#endif
