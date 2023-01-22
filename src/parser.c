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
void __destroy_expression_node(struct __node_type* node);

/* Destroy identifier node. */
void
__destroy_identifier_node(struct __node_type* node)
{
  struct identifier_node* node_cast = (struct identifier_node*) node;
  /* Kill it's contents. */
  free(node_cast->identifier);
  free(node_cast);
}

/* Destroy number node. */
void
__destroy_number_node(struct __node_type* node)
{
  free(node);
}

/* Destroy primary node. */
void
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
void
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
void
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
