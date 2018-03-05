//===-- RustParse.cpp ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RustParse.h"
#include "lldb/Utility/Status.h"
#include <functional>

using namespace lldb_private::rust;
using namespace lldb_private;
using namespace lldb;
using namespace llvm;

// temporary
RustExpressionUP Parser::Unimplemented(Status &error) {
  error.SetErrorString("unimplemented");
  return RustExpressionUP();
}

template<RustUnaryOperator OP>
RustExpressionUP Parser::Unary(Status &error) {
  Advance();
  RustExpressionUP result = Term(error);
  if (!result) {
    return result;
  }
  return llvm::make_unique<RustUnaryExpression<OP>>(std::move(result));
}

bool Parser::ExprList(std::vector<RustExpressionUP> *exprs, Status &error) {
  while (true) {
    RustExpressionUP expr = Expr(error);
    if (!expr) {
      return false;
    }

    exprs->push_back(std::move(expr));
    if (CurrentToken().kind != ',') {
      break;
    }
    Advance();
  }

  return true;
}

// This handles both a parenthesized expression and a tuple
// expression.
RustExpressionUP Parser::Parens(Status &error) {
  assert(CurrentToken().kind == '(');
  Advance();

  if (CurrentToken().kind == ')') {
    // Unit tuple.
    Advance();
    return Unimplemented(error);
  }

  RustExpressionUP expr = Expr(error);
  if (!expr) {
    return expr;
  }

  if (CurrentToken().kind == ')') {
    // Parenthesized expression.
    return expr;
  } else if (CurrentToken().kind != ',') {
    error.SetErrorString("expected ')' or ','");
    return RustExpressionUP();
  }

  std::vector<RustExpressionUP> exprs;
  exprs.push_back(std::move(expr));
  Advance();

  if (!ExprList(&exprs, error)) {
    return RustExpressionUP();
  }

  if (CurrentToken().kind != ')') {
    error.SetErrorString("expected ')'");
    return RustExpressionUP();
  }
  Advance();

  return llvm::make_unique<RustTupleExpression>(std::move(exprs));
}

RustExpressionUP Parser::Array(Status &error) {
  assert(CurrentToken().kind == '[');
  Advance();

  RustExpressionUP expr = Expr(error);
  if (!expr) {
    return expr;
  }

  RustExpressionUP result;
  if (CurrentToken().kind == ';') {
    Advance();
    if (CurrentToken().kind != INTEGER) {
      error.SetErrorString("expected integer literal");
    } else {
      result = llvm::make_unique<RustArrayWithLength>(std::move(expr),
                                                      CurrentToken().uinteger);
      Advance();
    }
  } else if (CurrentToken().kind == ',') {
    std::vector<RustExpressionUP> exprs;
    exprs.push_back(std::move(expr));

    if (ExprList(&exprs, error)) {
      result = llvm::make_unique<RustArrayLiteral>(std::move(exprs));
    }
  } else {
    error.SetErrorString("expected ',' or ';'");
    return result;
  }

  if (CurrentToken().kind != ']') {
    error.SetErrorString("expected ']'");
    result.reset();
  } else {
    Advance();
  }

  return result;
}

RustExpressionUP Parser::Field(RustExpressionUP &&lhs, Status &error) {
  assert(CurrentToken().kind == '.');
  Advance();

  RustExpressionUP result;
  if (CurrentToken().kind == IDENTIFIER) {
    result = llvm::make_unique<RustFieldExpression>(std::move(lhs),
                                                    CurrentToken().str);
    Advance();
  } else if (CurrentToken().kind == INTEGER) {
    result = llvm::make_unique<RustTupleFieldExpression>(std::move(lhs),
                                                         CurrentToken().uinteger);
    Advance();
  } else {
    error.SetErrorString("identifier or integer expected");
  }

  return result;
}

RustExpressionUP Parser::Call(RustExpressionUP &&func, Status &error) {
  assert(CurrentToken().kind == '(');
  Advance();

  std::vector<RustExpressionUP> exprs;
  if (!ExprList(&exprs, error)) {
    return RustExpressionUP();
  }

  if (CurrentToken().kind != ')') {
    error.SetErrorString("expected ')'");
    return RustExpressionUP();
  }
  Advance();

  return llvm::make_unique<RustCall>(std::move(func), std::move(exprs));
}

RustExpressionUP Parser::Index(RustExpressionUP &&array, Status &error) {
  assert(CurrentToken().kind == '[');
  Advance();

  RustExpressionUP idx = Expr(error);
  if (!idx) {
    return idx;
  }

  if (CurrentToken().kind != ']') {
    error.SetErrorString("expected ']'");
    return RustExpressionUP();
  }
  Advance();

  return llvm::make_unique<RustBinaryExpression<ArrayIndex>>(std::move(array), std::move(idx));
}

CompilerType Parser::Type(Status &error) {
  // FIXME
  error.SetErrorString("type parsing unimplemented");
  return CompilerType();
}

RustExpressionUP Parser::Path(Status &error) {
  return Unimplemented(error);
}

RustExpressionUP Parser::Term(Status &error) {
  RustExpressionUP term;

  switch (CurrentToken().kind) {
  case INTEGER:
  case FLOAT:
  case STRING:
  case BYTESTRING:
    return Unimplemented(error);

  case TRUE:
    return Unimplemented(error);
  case FALSE:
    return Unimplemented(error);

  case '[':
    term = Array(error);
    break;

  case '(':
    term = Parens(error);
    break;

  case SUPER:
  case SELF:
  case IDENTIFIER:
  case COLONCOLON:
    term = Path(error);
    break;

  case SIZEOF:
    return Unimplemented(error);

  case '*':
    term = Unary<UnaryDereference>(error);
    break;
  case '+':
    term = Unary<UnaryPlus>(error);
    break;
  case '-':
    term = Unary<UnaryNegate>(error);
    break;
  case '!':
    term = Unary<UnaryComplement>(error);
    break;

  case '&':
    return Unimplemented(error);

  case DOTDOT:
    return Unimplemented(error);

  case INVALID:
    error.SetErrorString(CurrentToken().str);
    return RustExpressionUP();

  case THATSALLFOLKS:
    error.SetErrorString("unexpected EOF");
    return RustExpressionUP();

  default:
    error.SetErrorString("unexpected token");
    return RustExpressionUP();
  }

  bool done = false;
  while (!done) {
    switch (CurrentToken().kind) {
    case AS: {
      Advance();
      CompilerType type = Type(error);
      if (!type) {
        return RustExpressionUP();
      }
      term = llvm::make_unique<RustCast>(type, std::move(term));
      break;
    }

    case DOTDOT:
      // term = Range(std::move(term), error);
      term = Unimplemented(error);
      break;

    case '.':
      term = Field(std::move(term), error);
      break;

    case '[':
      term = Index(std::move(term), error);
      break;

    case '(':
      term = Call(std::move(term), error);
      break;

    default:
      done = true;
      break;
    }

    if (!term) {
      return term;
    }
  }

  return term;
}

#define BINOP(What) \
  RustBinaryExpression< BinaryOperation< What<Scalar> > >
#define COMP(What) \
  RustBinaryExpression< Comparison< What<Scalar> > >
#define ASSIGN(What) \
  RustAssignExpression< BinaryOperation< What<Scalar> > >

// Couldn't find these in <functional>.

template<typename T>
class left_shift {
  T operator()(const T &l, const T&r) {
    return l << r;
  }
};

template<typename T>
class right_shift {
  T operator()(const T &l, const T&r) {
    return l >> r;
  }
};


// Binary operators.  Each line has the form:
//   DEFINE(token, precedence, type)
#define BINARY_OPERATORS                        \
  DEFINE(OROR, 3, RustOrOrExpression)           \
  DEFINE(ANDAND, 4, RustAndAndExpression)       \
  DEFINE(EQEQ, 5, COMP(std::equal_to))          \
  DEFINE(NOTEQ, 5, COMP(std::not_equal_to))     \
  DEFINE(LTEQ, 5, COMP(std::less_equal))        \
  DEFINE(GTEQ, 5, COMP(std::greater_equal))     \
  DEFINE(LSH, 9, BINOP(left_shift))             \
  DEFINE(RSH, 9, BINOP(right_shift))            \
  DEFINE(PLUS_EQ, 1, ASSIGN(std::plus))         \
  DEFINE(MINUS_EQ, 1, ASSIGN(std::minus))       \
  DEFINE(SLASH_EQ, 1, ASSIGN(std::divides))     \
  DEFINE(STAR_EQ, 1, ASSIGN(std::multiplies))   \
  DEFINE(PERCENT_EQ, 1, ASSIGN(std::modulus))   \
  DEFINE(RSH_EQ, 1, ASSIGN(right_shift))        \
  DEFINE(LSH_EQ, 1, ASSIGN(left_shift))         \
  DEFINE(AND_EQ, 1, ASSIGN(std::bit_and))       \
  DEFINE(OR_EQ, 1, ASSIGN(std::bit_or))         \
  DEFINE(XOR_EQ, 1, ASSIGN(std::bit_xor))       \
  DEFINE('|', 6, BINOP(std::bit_or))            \
  DEFINE('&', 8, BINOP(std::bit_and))           \
  DEFINE('=', 1, RustAssignment)                \
  DEFINE('<', 5, COMP(std::less))               \
  DEFINE('>', 5, COMP(std::greater))            \
  DEFINE('+', 10, BINOP(std::plus))             \
  DEFINE('-', 10, BINOP(std::minus))            \
  DEFINE('*', 11, BINOP(std::multiplies))       \
  DEFINE('/', 11, BINOP(std::divides))          \
  DEFINE('%', 11, BINOP(std::modulus))

RustExpressionUP Parser::Binary(Status &error) {
  struct Operation {
    int precedence;
    int op;
    RustExpressionUP term;

    Operation(int precedence_, int op_, RustExpressionUP &&term_)
      : precedence(precedence_),
        op(op_),
        term(std::move(term_))
    {
    }
  };

  RustExpressionUP term = Term(error);
  if (!term) {
    return term;
  }

  std::vector<Operation> operations;
  operations.emplace_back(0, -1, std::move(term));

  bool done = false;
  while (!done) {
    int precedence;
    int kind = CurrentToken().kind;

    switch (kind) {
#define DEFINE(Token, Prec, Type)                       \
      case Token: precedence = Prec; Advance(); break;

      BINARY_OPERATORS
#undef DEFINE

    case INVALID:
      error.SetErrorString(CurrentToken().str);
      return RustExpressionUP();

    case THATSALLFOLKS:
    default:
      // Not a binary operator, so we're going to return.
      done = true;
      // Arrange to pop all the operations now.
      precedence = 0;
      break;
    }

    RustExpressionUP rhs;
    if (!done) {
      rhs = Term(error);
      if (!rhs) {
        return rhs;
      }
    }

    while (precedence < operations.back().precedence) {
      Operation top = std::move(operations.back());
      operations.pop_back();

      assert(!operations.empty());
      Operation &lhs = operations.back();

      switch (top.op) {
#define DEFINE(Token, Prec, Type)                                       \
        case Token:                                                     \
          lhs.term = llvm::make_unique<Type>(std::move(lhs.term), std::move(top.term)); \
          break;

        BINARY_OPERATORS
#undef DEFINE

      default:
        assert(0);
      }
    }

    // This won't be true in the "done" case.
    if (rhs) {
      operations.emplace_back(precedence, kind, std::move(rhs));
    }
  }

  assert(operations.size() == 1);
  return std::move(operations.back().term);
}
