#include <stdlib.h>
#include <stdio.h>
#include <sysexits.h>

#include "parser.h"

int
main(int argc, char** argv)
{
  if (argc != 2)
	return EX_USAGE;

  /* Read a file & print it. */
  char* text = NULL;
  ptrdiff_t len = 0;
  int status = get_file(argv[1], &text, &len);
  if (status)
	return EX_SOFTWARE;
  printf("%.*s", (int) len, text);
  /* Then free text. */
  free(text);
}
