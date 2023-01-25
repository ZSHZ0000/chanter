#ifndef LEXER_H
#define LEXER_H

/* For type definitions I need & use. */
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/* Check if a character is within a range. */
#define IS_DIGIT(x) (x >= '0' && x <= '9')
#define IS_ALPHA(x) ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
#define IS_ALNUM(x) (IS_ALPHA(x) || IS_DIGIT(x))
#define IS_WHITESPACE(x) (x == ' ' || x == '\t' || x == '\n' || x == '\r')

/* Types of token. */
enum TOKEN_TYPE
  {
	TOKEN_NONE,
	TOKEN_EOF,
	TOKEN_LITERAL,
	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_ASTERISK,
	TOKEN_SLASH,
	TOKEN_IDENTIFIER
  };

/* Lexema node, linked lists because I don't really have anything better for a stream
** of lexemas. */
struct lex_node
{
  enum TOKEN_TYPE type; /* Token type. */
  struct lex_node* next; /* Next node in the linked list */
  char* begin; /* The beginning of the token in the source text. */
  ptrdiff_t len; /* The length of the token in the source text. */
};

/* Scanner context. */
struct scan_ctx
{
  char* src; /* Source text. */
  char* src_end; /* Source text end. */
  char* current; /* Current offset. */
  ptrdiff_t len; /* Length of the source, not necessary but it eases debugging. */
  struct lex_node* root_node; /* Root node. */
  struct lex_node* current_node; /* Current node. */
};

/* Get the length of a file with a file handle, resets offset to zero. */
ptrdiff_t
zsize_file(FILE* file);

/* Read a whole file into memory, seeks from zero offset. */
ptrdiff_t
read_file(FILE* file, char** return_addr);

/* Get file from name. */
int
get_file(char* filename, char** ret_addr, ptrdiff_t* len_addr);

/* Initialize scanner context. */
void
init_scan_context(struct scan_ctx* context, char* filename);

/* Destroy lexema linked list */
void
destroy_lex_list(struct lex_node* list);

/* Create a stringless token, added to the token list. */
struct lex_node*
create_token(struct scan_ctx* context, enum TOKEN_TYPE type);

/* Kill scan context. */
void
kill_scan_context(struct scan_ctx* context);

/* Rewind previous node to root node of the token stream. */
void
context_rewind_current_node(struct scan_ctx* context);

/* Scan & lex a text string, returning a list. first node is always a sentry node. */
struct lex_node*
scan_text(struct scan_ctx* context);

#endif /* lexer.h */
