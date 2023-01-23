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

#include "lexer.h"

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
  /* TODO: Error handling. */
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

/* Create a lex_node & initialize it. */
static struct lex_node*
create_node(void)
{
  struct lex_node* node = malloc(sizeof(struct lex_node));
  node->type = 0;
  node->next = NULL;
  node->begin = NULL;
  node->len = 0;
  /* TODO: Error handling. */
  return node;
}

/* Peek at a character in the context. */
static int
peek_char(struct scan_ctx* context)
{
  if (context->current < context->src_end)
	return *context->current;
  return -1;
};

/* Advance in the source by K characters. */
static int
advance_char(struct scan_ctx* context)
{
  if (context->current + 1 < context->src_end)
	context->current++;
  return -1;
}

/* Get a number literal. */
static int
get_number_literal(struct scan_ctx* context)
{
  char* current = context->current;
  char* tkn_end = current + 1;

  /* Iterate through, until we get a non-digit. */
  while (IS_DIGIT(*tkn_end) && tkn_end < context->src_end)
	  tkn_end++;
  
  /* Create lex_node. */
  struct lex_node* node = create_node();
  context->prev_node->next = node;
  node->type = TOKEN_LITERAL;
  node->begin = current;
  node->len = ((intptr_t) tkn_end - (intptr_t) current);
  context->prev_node = node;

  /* So it doesn't loops infinitely. */
  context->current = tkn_end;

  /* TODO: Error handling. */
  return 0;
}

/* Get an identifer token. */
static int
get_identifier(struct scan_ctx* context)
{
  char* current = context->current;
  char* tkn_end = current + 1;

  /* Iterate through, until we get a non-digit. */
  while (IS_ALNUM(*tkn_end) && tkn_end < context->src_end)
	tkn_end++;

    /* Create lex_node. */
  struct lex_node* node = create_node();
  context->prev_node->next = node;
  node->type = TOKEN_IDENTIFIER;
  node->begin = current;
  node->len = ((intptr_t) tkn_end - (intptr_t) current);
  context->prev_node = node;

  /* So it doesn't loops infinitely. */
  context->current = tkn_end;
  
  /* TODO: Error handling. */
  return 0;
}

/* Skip whitespace. */
static void
skip_whitespace(struct scan_ctx* context)
{
  while (IS_WHITESPACE(*context->current))
	{ context->current++; }
}

/* Create a stringless token. */
struct lex_node*
create_token(struct scan_ctx* context, enum TOKEN_TYPE type)
{
  struct lex_node* node = create_node();
  context->prev_node->next = node;
  node->type = type;
  node->begin = NULL;
  node->len = 0;
  context->prev_node = node;
  return node;
}

/* Make the token consume a single character. */
static void
token_1char(struct scan_ctx* context, enum TOKEN_TYPE type)
{
  create_token(context, type);
  context->prev_node->begin = context->current;
  context->prev_node->len = sizeof(char);
  advance_char(context);
}

/* Initialize context. */
void
init_scan_context(struct scan_ctx* context, char* filename)
{
  /* FIle length. */
  get_file(filename, &context->src, &context->len);

  /* The root node. */
  struct lex_node* root_node = create_node();

  /* Actually initialize the fields of the object. */
  context->current = context->src;
  context->src_end = (char*) ((intptr_t) context->src + context->len);
  context->root_node = root_node;
  context->prev_node = root_node;
};

/* Destroy lexema list. */
void
destroy_lex_list(struct lex_node* list)
{
  struct lex_node* current = list;
  struct lex_node* next = NULL;
  while (current)
	{
	  /* Free the current element, then put the next element in the current one, unless the
	  ** current one is NULL, then we do nothing. */
	  next = current->next;
	  free(current);
	  current = next;
	}
}

/* Deallocate various things from the scan context, then set all things to 0 or NULL. */
void
kill_scan_context(struct scan_ctx* context)
{
  free(context->src);
  context->current = NULL;
  context->src_end = NULL;
  context->len = 0;
  context->prev_node = NULL;
  destroy_lex_list(context->root_node);
  context->root_node = NULL;
}

/* Create a stream of tokens from a text string, ATM it only supports +, - & number
** literals. */
struct lex_node*
scan_text(struct scan_ctx* context)
{
  int character = peek_char(context);
  while (character != -1)
	{
	  /* If it is digit try taking it as a literal number. */
	  if (IS_DIGIT(character))
		{
		  get_number_literal(context);
		  character = peek_char(context);
		}
	  else if (IS_ALPHA(character))
		{
		  get_identifier(context);
		  character = peek_char(context);
		}
	  else if (IS_WHITESPACE(character))
		{
		  skip_whitespace(context);
		  character = peek_char(context);
		}
	  /* TODO: make this less abhorrend to look at. */
	  else if (character == '+')
		{
		  token_1char(context, TOKEN_PLUS);
		  character = peek_char(context);
		}
	  else if (character == '-')
		{
		  token_1char(context, TOKEN_MINUS);
		  character = peek_char(context);
		}
	  else if (character == '*')
		{
		  token_1char(context, TOKEN_ASTERISK);
		  character = peek_char(context);
		}
	  else if (character == '/')
		{
		  token_1char(context, TOKEN_SLASH);
		  character = peek_char(context);
		}
	  else break;
	}
  return context->root_node;
}

/* Rewind previous node to root node of the token stream. */
void
context_rewind_prev_node(struct scan_ctx* context)
{
  context->prev_node = context->root_node;
}
