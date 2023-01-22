#ifndef PARSER_H
#define PARSER_H

/* For type definitions I need & use. */
#include <stddef.h>
#include <stdint.h>

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

#endif /* parser.h */
