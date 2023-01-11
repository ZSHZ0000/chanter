#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <unistd.h>

/* For error reporting. */
#include <sysexits.h>
#include <string.h>
#include <errno.h>
#include <err.h>

#include "parser.h"

/* Get the length of a file with a file handle, resets offset to zero. */
ptrdiff_t
zsize_file(FILE* file)
{
  ptrdiff_t size = 0;
  /* Hop to end of file. */
  fseek(file, 0, SEEK_END);
  size = (ptrdiff_t) ftell(file);
  fseek(file, 0, SEEK_SET);
  return size;
}

/* Read a whole file into memory. */
ptrdiff_t
read_file(FILE* file, char** return_addr)
{
  /* Classical prelude. */
  fseek(file, 0, SEEK_SET);
  ptrdiff_t read_size = zsize_file(file);
  char* text = malloc(sizeof(char) * read_size);

  /* Soy obligatory NULL check, good practice. */
  if (!text)
	err(EX_UNAVAILABLE, "Malloc could not allocate memory");

  /* Reading less than read_size always means error, since EOF is at read_size
  ** offset (0-indexed analogy). */
  ptrdiff_t read_count = fread(text, sizeof(char), read_size, file);

  /* Abort if we can't read file completely, it'd be useless to try parsing an
  ** incomplete program, atleast that is what I think. */
  if (read_count != read_size)
	err(EX_IOERR, "Error while reading file");

  /* Prepare to exit. */
  *return_addr = text;
  return read_size;
}

/* Get file from name. */
int
get_file(char* filename, char** ret_addr, ptrdiff_t* len_addr)
{
  /* Read only. */
  FILE* file = fopen(filename, "r");
  /* Handle error. */
  if (!file)
	{
	  warn("Unable to open %s", filename);
	  return 1;
	}

  /* Read the file. */;
  *len_addr = read_file(file, ret_addr);
  return 0;
}
