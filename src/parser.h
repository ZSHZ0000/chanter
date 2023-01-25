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
	NODE_ADD,
	NODE_SUB,
	NODE_MUL,
	NODE_DIV,
  };

/* Safe cast just to check struct type. */
struct node_typecast
{
  enum NODE_TYPE type;
};

/* Node structure. */
struct primary_node
{
  enum NODE_TYPE type;
  uintptr_t value;
  ptrdiff_t len;
};

/* Binary operator. */
struct binary_node
{
  enum NODE_TYPE type;
  void* left;
  void* right;
};

/* Parse expression. */
void*
get_expression_node(struct scan_ctx* context);

#endif /* parser.h */
