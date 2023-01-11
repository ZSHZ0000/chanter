#ifndef PARSER_H
#define PARSER_H

/* For type definitions I need & use. */
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/* Get the length of a file with a file handle, resets offset to zero. */
ptrdiff_t
zsize_file(FILE* file);

/* Read a whole file into memory, seeks from zero offset. */
ptrdiff_t
read_file(FILE* file, char** return_addr);

/* Get file from name. */
int
get_file(char* filename, char** ret_addr, ptrdiff_t* len_addr);

#endif /* parser.h */
