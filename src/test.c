#include <stdlib.h>
#include <stdio.h>
#include <sysexits.h>

#include "parser.h"

/* Prints lexema node. */
void
print_node_type(struct lex_node* node)
{
  switch (node->type)
	{
	  /* Token none marks sentry nodes. */
	case TOKEN_NONE:
	  printf("NONE (This is most likely an error)\n");
	  break;
	  
	case TOKEN_LITERAL:
	  printf("NUMBER: %.*s\n", (int) node->len, node->begin);
	  break;

	case TOKEN_PLUS:
	  printf("PLUS\n");
	  break;

	case TOKEN_MINUS:
	  printf("MINUS\n");
	  break;
	}
}

/* Print whole list. */
void
print_list(struct lex_node* list)
{
  struct lex_node* current = list;
  while (current != NULL)
	{
	  printf("DD: PRINTING %lX\n", (uintptr_t) list);
	  print_node_type(current);
	  current = current->next;
	}
}

int
main(int argc, char** argv)
{
  /* Check for proper argument count. */
  if (argc != 2)
	{
	  fprintf(stderr, "%s: Usage: %s filename\n", argv[0], argv[0]);
	  return EX_USAGE;
	}

  /* Initialize context I. */
  struct scan_ctx context = { .src = NULL, .src_end = NULL, .current = NULL, .root_node = NULL,
							  .prev_node = NULL};
  /* Initialize context II. */
  init_scan_context(&context, argv[1]);
  /* Print lexema list that we scanned. */
  print_list(scan_text(&context));
  /* Then kill it. */
  kill_scan_context(&context);
}
