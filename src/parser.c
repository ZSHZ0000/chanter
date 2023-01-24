#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h> /* For memcpy. */

#include <sysexits.h>
#include <errno.h>
#include <err.h>

#include "lexer.h"
#include "parser.h"

/* Make an identifier node, the identifier text is copied for itself. */
struct __node_type*
make_identifier_node(char* identifier, ptrdiff_t len)
{
  /* TODO: Have error checking here, can't bother with it right now. */
  struct identifier_node* node = (struct identifier_node*) malloc(sizeof (struct identifier_node));
  node->type = NODE_IDENTIFIER;
  node->identifier = malloc(len);
  node->identifier = memcpy(node->identifier, identifier, len);
  node->len = len;
  return (struct __node_type*) node;
}

/* Make a number node, parsing the number beforehand. */
struct __node_type*
make_number_node(char* number)
{
  struct number_node* node = (struct number_node*) malloc(sizeof (struct number_node));
  node->type = NODE_IDENTIFIER;
  node->value = strtol(number, NULL, 0);
  return (struct __node_type*) node;
}

/* Make a primary node. */
struct __node_type*
make_primary_node(struct __node_type* value)
{
  struct primary_node* node = (struct primary_node*) malloc(sizeof (struct primary_node));
  node->type = NODE_PRIMARY;
  node->sub = value;
  return (struct __node_type*) node;
}

/* Make a factor node. */
struct __node_type*
make_factor_node(struct primary_node* lhs, enum FACTOR_TYPE op, struct __node_type* rhs)
{
  struct factor_node* node = (struct factor_node*) malloc(sizeof (struct factor_node));
  node->type = NODE_FACTOR;
  node->lhs = lhs;
  /* If this is not an operation node. */
  if (op == FACTOR_NONE)
	{
	  node->operation = FACTOR_NONE;
	  node->rhs = NULL;
	}
  else
	{
	  node->operation = op;
	  node->rhs = rhs;
	}
  return (struct __node_type*) node;
}

/* Make an expression node. */
struct __node_type*
make_expression_node(struct factor_node* lhs, enum EXPR_TYPE op, struct __node_type* rhs)
{
  struct expression_node* node = (struct expression_node*) malloc(sizeof (struct expression_node));
  node->type = NODE_EXPR;
  node->lhs = lhs;
  if (op == EXPR_NONE)
	{
	  node->operation = EXPR_NONE;
	  node->rhs = NULL;
	}
  else
	{
	  node->operation = op;
	  node->rhs = rhs;
	}
  return (struct __node_type*) node;
}

/* Prototype. */
static void
__destroy_expression_node(struct __node_type* node);

/* Destroy identifier node. */
static void
__destroy_identifier_node(struct __node_type* node)
{
  struct identifier_node* node_cast = (struct identifier_node*) node;
  /* Kill it's contents. */
  free(node_cast->identifier);
  free(node_cast);
}

/* Destroy number node. */
static void
__destroy_number_node(struct __node_type* node)
{
  free(node);
}

/* Destroy primary node. */
static void
__destroy_primary_node(struct __node_type* node)
{
  struct primary_node* node_cast = (struct primary_node*) node;
  /* Free it's contents based on the type of the content. */
  switch(node_cast->sub->type)
	{
	case NODE_IDENTIFIER:
	  __destroy_identifier_node(node_cast->sub);
	  break;

	case NODE_NUMBER:
	  __destroy_number_node(node_cast->sub);
	  break;

	case NODE_EXPR:
	  __destroy_expression_node(node_cast->sub);
	  break;

	  /* Report this as an error because this would be an internal inconsistency. */
	default:
	  err(EX_SOFTWARE, "Unpermitted node as subnode of a NODE_PRIMARY");
	  break;
	}
  free(node);
}

/* Destroy a factor node, if it has other factor sub-nodes, destroy them recursively. */
static void
__destroy_factor_node(struct __node_type* node)
{
  struct factor_node* node_cast = (struct factor_node*) node;
  switch(node_cast->operation)
	{
	case FACTOR_NONE:
	  __destroy_primary_node((struct __node_type*) node_cast->lhs);
	  break;

	case FACTOR_MUL:
	case FACTOR_DIV:
	  __destroy_primary_node((struct __node_type*) node_cast->lhs);
	  __destroy_factor_node(node_cast->rhs);
	  break;
	}
  free(node);
}

/* Destroy an expr node, if it has other expr sub-nodes, destroy them recursively. */
static void
__destroy_expression_node(struct __node_type* node)
{
  struct expression_node* node_cast = (struct expression_node*) node;
  switch(node_cast->operation)
	{
	case EXPR_NONE:
	  __destroy_factor_node((struct __node_type*) node_cast->lhs);
	  break;

	case EXPR_ADD:
	case EXPR_SUB:
	  __destroy_factor_node((struct __node_type*) node_cast->lhs);
	  __destroy_expression_node(node_cast->rhs);
	  break;
	}
  free(node);
}

/* Deallocate a parse tree. */
void
destroy_parse_tree(struct __node_type* tree)
{
  /* Dispatch destructors based on . */
  switch(tree->type)
	{
	case NODE_NUMBER:
	  __destroy_number_node(tree);
	  break;
		
	case NODE_IDENTIFIER:
	  __destroy_identifier_node(tree);
	  break;

	case NODE_PRIMARY:
	  __destroy_primary_node(tree);
	  break;

	case NODE_FACTOR:
	  __destroy_factor_node(tree);
	  break;

	case NODE_EXPR:
	  __destroy_expression_node(tree);
	  break;

	default:
	  err(EX_SOFTWARE, "Unknown type of node attempted to be destroyed.");
	  break;
	}
}

/* Get a lex token. */
struct lex_node*
accept_token(struct scan_ctx* context, enum TOKEN_TYPE type)
{
  /* If the type we need is the current node (called previous for some reason I forgot)
  ** it will be accepted & it's lex node address will be returned. */
  if (type == context->prev_node->type)
	{
	  struct lex_node* node = context->prev_node;
	  context->prev_node = node->next;
	  return node;
	}
  else return NULL;
}

/* Require a lex token. */
struct lex_node*
require_token(struct scan_ctx* context, enum TOKEN_TYPE type)
{
  struct lex_node* node = accept_token(context, type);
  if (!node)
	err(EX_SOFTWARE, "Token of unexpected position has been found");
  return node;
}

/* Get an expression, create according node. */
struct expression_node*
get_expression(struct scan_ctx* context);

/* Get an identifier token, create an identifier node. */
struct identifier_node*
get_identifier_token(struct scan_ctx* context)
{
  struct lex_node* node = accept_token(context, TOKEN_IDENTIFIER);
  return (struct identifier_node*) make_identifier_node(node->begin, node->len);
}

/* Get a number token, create according node. */
struct number_node*
get_number_token(struct scan_ctx* context)
{
  struct lex_node* node = accept_token(context, TOKEN_LITERAL);
  return (struct number_node*) make_number_node(node->begin);
}

/* Get a primary token, create according node. */
struct primary_node*
get_primary_token(struct scan_ctx* context)
{
  struct __node_type* node = NULL;
  /* mmmmmmmmmmmmmmmmmmm. */
  if ((node = (struct __node_type*) get_identifier_token(context)))
	return (struct primary_node*) make_primary_node(node);
  if ((node = (struct __node_type*) get_number_token(context)))
	return (struct primary_node*) make_primary_node(node);
  /* TODO: FIX THIS ONCE I IMPLEMENT PARENTHESIS SUPPORT, THE PROTOTYPE IS
  ** NEEDED TO BE DONE SO I CAN TEST THROUGHFUL THIS & DITCH ANY PIECES THAT ARE
  ** DEEMED TOO COMPLEX OR UNORTHOGONAL. */
  // if ((node = (struct __node_type*) get_expression(context)))
  // return (struct primary_node*) make_primary_node(node);
  else
	err(EX_SOFTWARE,"Synctatic error");
}

/* Get a factor node, create according node. */
struct factor_node*
get_factor(struct scan_ctx* context)
{
  struct primary_node* lhs = get_primary_token(context);
  struct lex_node* op = NULL;
  if ((op = accept_token(context, TOKEN_ASTERISK)))
	return (struct factor_node*)
	  make_factor_node(lhs, FACTOR_MUL, (struct __node_type*) get_factor(context));
  if ((op = accept_token(context, TOKEN_SLASH)))
	return (struct factor_node*)
	  make_factor_node(lhs, FACTOR_DIV, (struct __node_type*) get_factor(context));
  /* No operation related to the factor node. */
  return (struct factor_node*) make_factor_node(lhs, FACTOR_NONE, NULL);
}

/* TODO: REPEAT CODE IS TERRIBLE, BUT THIS IS A RECURSIVE DESCENT PARSER
** SO THE PARTS THAT REPEAT ARE NATURAL, THOUGH THIS MAY BE WORRYSOME IN
** SOME ASPECS, MAY OR MAY NOT NEED FIXING. */
/* Get a expression node, create according node. */
struct expression_node*
get_expression(struct scan_ctx* context)
{
  struct factor_node* lhs = get_factor(context);
  struct lex_node* op = NULL;
  if ((op = accept_token(context, TOKEN_PLUS)))
	return (struct expression_node*)
	  make_expression_node(lhs, EXPR_ADD, (struct __node_type*) get_factor(context));
  if ((op = accept_token(context, TOKEN_MINUS)))
	return (struct expression_node*)
	  make_expression_node(lhs, EXPR_SUB, (struct __node_type*) get_factor(context));
  return (struct expression_node*) make_expression_node(lhs, EXPR_NONE, NULL);
}
