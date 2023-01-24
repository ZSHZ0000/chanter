#ifndef PARSER_H
#define PARSER_H

/* For type definitions I need & use. */
#include <stddef.h>
#include <stdint.h>

/* Include important definitions from lexer.h. */
#include "lexer.h"

/* Node type enumeration. */
enum NODE_TYPE
  {
	NODE_IDENTIFIER,
	NODE_NUMBER,
	NODE_PRIMARY,
	NODE_FACTOR,
	NODE_EXPR
  };

/* Factor type enumeration. */
enum FACTOR_TYPE
  {
	FACTOR_NONE,
	FACTOR_MUL,
	FACTOR_DIV
  };

/* Expression type enumeration. */
enum EXPR_TYPE
  {
	EXPR_NONE,
	EXPR_ADD,
	EXPR_SUB
  };

/* Generic structure we will pointer cast to to get the NODE_TYPE field. */
struct __node_type
{
  enum NODE_TYPE type;
};

/* Get the type of any structure, PASS A POINTER TO THIS OR BAD STUFF WILL HAPPEN. */
#define TYPE_OF_NODE(x) (((struct __node_type*) x)->type)

/* Expression struct prototype. */
struct expression_node;

/* Identifier node. */
struct identifier_node
{
  enum NODE_TYPE type;
  char* identifier;
  ptrdiff_t len;
};

/* Number node. */
struct number_node
{
  enum NODE_TYPE type;
  long value;
};

/* Primary node. */
struct primary_node
{
  enum NODE_TYPE type;
  struct __node_type* sub;
};

/* Factor node. */
struct factor_node
{
  enum NODE_TYPE type;
  enum FACTOR_TYPE operation;
  struct primary_node* lhs;
  /* Right-hand side. */
  union
  {
	struct __node_type* rhs;
	struct factor_node* factor;
	struct primary_node* primary;
  };
};

/* Expression node. */
struct expression_node
{
  enum NODE_TYPE type;
  enum EXPR_TYPE operation;
  struct factor_node* lhs;
  /* Right-hand side. */
  union
  {
	struct __node_type* rhs;
    struct factor_node* factor;
    struct expression_node* expression;
  };
};

/* Prototypes now. */

/* Make an identifier node, the identifier text is copied for itself. */
struct __node_type*
make_identifier_node(char* identifier, ptrdiff_t len);

/* Make a number node, parsing the number beforehand. */
struct __node_type*
make_number_node(char* number);

/* Make a primary node. */
struct __node_type*
make_primary_node(struct __node_type* value);

/* Make a factor node. */
struct __node_type*
make_factor_node(struct primary_node* lhs, enum FACTOR_TYPE op, struct __node_type* rhs);

/* Make an expression node. */
struct __node_type*
make_expression_node(struct factor_node* lhs, enum EXPR_TYPE op, struct __node_type* rhs);

/* Get a lex token. */
struct lex_node*
accept_token(struct scan_ctx* context, enum TOKEN_TYPE type);

/* Require a lex token. */
struct lex_node*
require_token(struct scan_ctx* context, enum TOKEN_TYPE type);

/* Get an identifier token, create an identifier node. */
struct identifier_node*
get_identifier_token(struct scan_ctx* context);

/* Get a number token, create according node. */
struct number_node*
get_number_token(struct scan_ctx* context);

/* Get a primary token, create according node. */
struct primary_node*
get_primary_token(struct scan_ctx* context);

/* Get a factor node, create according node. */
struct factor_node*
get_factor(struct scan_ctx* context);

/* Get a expression node, create according node. */
struct expression_node*
get_expression(struct scan_ctx* context);

#endif /* parser.h */
