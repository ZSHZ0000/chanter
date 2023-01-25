#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h> /* For memcpy. */

#include <sysexits.h>
#include <errno.h>
#include <err.h>

#include "lexer.h"
#include "parser.h"

/* Accept token if it matches the token type. */
struct lex_node*
accept_token(struct scan_ctx* context, enum TOKEN_TYPE type)
{
  struct lex_node* node = context->current_node;
  if (node || node->type == type)
	{
	  context->current_node = node->next;
	  return node;
	}
  else
	return NULL;
}

/* Accept token unconditionally, returns NULL if we got no left tokens. */
struct lex_node*
uncond_accept_token(struct scan_ctx* context)
{
  struct lex_node* node = context->current_node;
  if (node)
	{
	  context->current_node = node->next;
	  return node;
	}
  else
	return NULL;
}

/* Look N tokens ahead, 0 for current token. */
enum TOKEN_TYPE
peek_nth_token(struct scan_ctx* context, int nth)
{
  struct lex_node* current = context->current_node;
  int index = 0;
  while(index < nth && current->next)
	{
	  current = current->next;
	  index++;
	}
  if (index != nth)
	return TOKEN_EOF;
  return current->type;
}

/* Get identifier node. */
static struct primary_node*
get_identifier_node(struct scan_ctx* context)
{
  struct primary_node* node = malloc(sizeof (struct primary_node));
  ptrdiff_t len = 0;
  char* text = NULL;
  struct lex_node* token = accept_token(context, TOKEN_IDENTIFIER);
  text = token->begin;
  len = token->len;
  
  node->type = NODE_IDENTIFIER;
  node->value = (uintptr_t) malloc(len);
  node->len = len;
  memcpy((void*) node->value, text, len);
  return node;
}

/* Get number node. */
static struct primary_node*
get_number_node(struct scan_ctx* context)
{
  struct primary_node* node = malloc(sizeof (struct primary_node));
  intptr_t number = 0;
  struct lex_node* token = accept_token(context, TOKEN_LITERAL);
  number = strtoll(token->begin, NULL, 0);
  
  node->type = NODE_NUMBER;
  node->value = number;
  node->len = 0;
  return node;
}

/* Get a primary node. */
static struct primary_node*
get_primary_node(struct scan_ctx* context)
{
  switch (peek_nth_token(context, 0))
	{
	case TOKEN_LITERAL:
	  return get_number_node(context);
	  break;

	case TOKEN_IDENTIFIER:
	 return get_identifier_node(context);
	 break;

	case TOKEN_EOF:
	  err(EX_SOFTWARE, "FOUND END OF FILE TOKEN WHILE PARSING: ABORTING");
	  break;

	  /* TODO: This could be better, we must add line reporting. */
	default:
	  err(EX_SOFTWARE, "BAD TOKEN.");
	  break;
	}
}

/* Get factor node. */
static void*
get_factor_node(struct scan_ctx* context)
{
  /* Get left node. */
  struct primary_node* left = get_primary_node(context);
  struct binary_node* node = NULL;

  /* Check for operator at the right, if there ain't it then don't do anything
  ** interesting. */
  enum TOKEN_TYPE token_type = TOKEN_NONE;
  switch ((token_type = peek_nth_token(context, 0)))
	{
	case TOKEN_SLASH:
	case TOKEN_ASTERISK:
	  node = malloc(sizeof (struct binary_node));
	  node->left = node;
	  node->type = token_type == TOKEN_SLASH ? NODE_DIV : NODE_MUL;
	  uncond_accept_token(context);
	  node->right = get_primary_node(context);
	  return node;
	  break;

	case TOKEN_EOF:
	  err(EX_SOFTWARE, "FOUND END OF FILE TOKEN WHILE PARSING: ABORTING");
	  break;

	default:
	  return left;
	  break;
	}
  /* Should never reach here. */
  return NULL;
}

/* Make expression node. */
void*
get_expression_node(struct scan_ctx* context)
{
  /* Get left node. */
  void* left = get_factor_node(context);
  struct binary_node* node = NULL;

  /* Check for operator at the right, if there ain't it then don't do anything
  ** interesting. */
  enum TOKEN_TYPE token_type = TOKEN_NONE;
  switch ((token_type = peek_nth_token(context, 0)))
	{
	case TOKEN_PLUS:
	case TOKEN_MINUS:
	  node = malloc(sizeof (struct binary_node));
	  node->left = node;
	  node->type = token_type == TOKEN_PLUS ? NODE_ADD : NODE_SUB;
	  uncond_accept_token(context);
	  node->right = get_primary_node(context);
	  return node;
	  break;

	case TOKEN_EOF:
	  err(EX_SOFTWARE, "FOUND END OF FILE TOKEN WHILE PARSING: ABORTING");
	  break;

	default:
	  return left;
	  break;
	}
  /* Should never reach here. */
  return NULL;
}
