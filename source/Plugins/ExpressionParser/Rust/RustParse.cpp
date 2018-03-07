//===-- RustParse.cpp ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RustParse.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Symbol/RustASTContext.h"
#include "lldb/Utility/Status.h"
#include <functional>

using namespace lldb_private::rust;
using namespace lldb_private;
using namespace lldb;
using namespace llvm;

static RustASTContext *
GetASTContext(ValueObjectSP val, Status &error)
{
  RustASTContext *result =
    llvm::dyn_cast_or_null<RustASTContext>(val->GetCompilerType().GetTypeSystem());
  if (!result) {
    error.SetErrorString("not a Rust type!?");
  }
  return result;
}

static ValueObjectSP
CreateValueFromScalar(ExecutionContext &exe_ctx, Scalar &scalar, CompilerType type,
		      Status &error)
{
  DataExtractor data;
  if (!scalar.GetData(data)) {
    error.SetErrorString("could not get data from scalar");
    return ValueObjectSP();
  }
  ValueObjectSP result = ValueObject::CreateValueObjectFromData("", data, exe_ctx, type);
  if (!result) {
    error.SetErrorString("could not create value object");
  }
  return result;
}

ValueObjectSP
lldb_private::rust::UnaryDereference(ExecutionContext &exe_ctx, ValueObjectSP addr, Status &error)
{
  return addr->Dereference(error);
}

ValueObjectSP
lldb_private::rust::UnaryAddr(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  return val->AddressOf(error);
}

ValueObjectSP
lldb_private::rust::UnaryPlus(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  if (RustASTContext *ast = GetASTContext(val, error)) {
    CompilerType type = val->GetCompilerType();
    if (type.IsScalarType() && !ast->IsBooleanType(type.GetOpaqueQualType())) {
      return val;
    }
    error.SetErrorString("not a scalar type");
  }
  return ValueObjectSP();
}

ValueObjectSP
lldb_private::rust::UnaryNegate(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  if (RustASTContext *ast = GetASTContext(val, error)) {
    CompilerType type = val->GetCompilerType();
    if (!type.IsScalarType() || ast->IsBooleanType(type.GetOpaqueQualType())) {
      error.SetErrorString("not a scalar type");
      return ValueObjectSP();
    }

    Scalar scalar;
    if (!val->ResolveValue(scalar)) {
      error.SetErrorString("could not resolve scalar value");
      return ValueObjectSP();
    }
    if (!scalar.UnaryNegate()) {
      error.SetErrorString("could not negate scalar value");
      return ValueObjectSP();
    }

    return CreateValueFromScalar(exe_ctx, scalar, type, error);
  }
  return ValueObjectSP();
}

ValueObjectSP
lldb_private::rust::UnaryComplement(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  CompilerType type = val->GetCompilerType();
  if (!type.IsScalarType()) {
    error.SetErrorString("not a scalar type");
    return ValueObjectSP();
  }

  Scalar scalar;
  if (!val->ResolveValue(scalar)) {
    error.SetErrorString("could not resolve scalar value");
    return ValueObjectSP();
  }
  if (!scalar.OnesComplement()) {
    error.SetErrorString("could not negate scalar value");
    return ValueObjectSP();
  }

  return CreateValueFromScalar(exe_ctx, scalar, type, error);
}

ValueObjectSP
lldb_private::rust::UnarySizeof(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  if (RustASTContext *ast = GetASTContext(val, error)) {
    uint32_t ptr_size = ast->GetPointerByteSize();
    CompilerType type = ast->CreateIntegralType(ConstString("usize"), false, ptr_size);
    Scalar size (val->GetByteSize());
    return CreateValueFromScalar(exe_ctx, size, type, error);
  }
  return ValueObjectSP();
}

template<typename T>
ValueObjectSP
lldb_private::rust::BinaryOperation (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
                                     lldb::ValueObjectSP right, Status &error)
{
  RustASTContext *ast = GetASTContext(left, error);
  if (!ast) {
    return ValueObjectSP();
  }

  if (!left->GetCompilerType().IsScalarType() || !right->GetCompilerType().IsScalarType()) {
    error.SetErrorString("not a scalar type");
    return ValueObjectSP();
  }

  Scalar sleft, sright;
  if (!left->ResolveValue(sleft) || !right->ResolveValue(sright)) {
    error.SetErrorString("could not resolve scalar value");
    return ValueObjectSP();
  }

  Scalar result = T()(sleft, sright);
  if (result.GetType() == Scalar::e_void) {
    error.SetErrorString("could not resolve scalar value");
    return ValueObjectSP();
  }

  size_t byte_size = result.GetByteSize();
  CompilerType type;

  // FIXME there has to be a better way.
  switch (result.GetType()) {
  case Scalar::e_sint:
  case Scalar::e_slong:
  case Scalar::e_slonglong:
    type = ast->CreateIntrinsicIntegralType(true, byte_size);
    break;

  case Scalar::e_uint:
  case Scalar::e_ulong:
  case Scalar::e_ulonglong:
    type = ast->CreateIntrinsicIntegralType(false, byte_size);
    break;

  case Scalar::e_float:
  case Scalar::e_double:
    if (byte_size == 4) {
      type = ast->CreateFloatType(ConstString("f32"), byte_size);
      break;
    } else if (byte_size == 8) {
      type = ast->CreateFloatType(ConstString("f64"), byte_size);
      break;
    }
    /* FALL THROUGH */

  default:
    error.SetErrorString("unknown type resulting from binary operation");
    return ValueObjectSP();
  }

  return CreateValueFromScalar(exe_ctx, result, type, error);
}

template<typename T>
ValueObjectSP
lldb_private::rust::Comparison (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
                                lldb::ValueObjectSP right, Status &error)
{
  RustASTContext *ast = GetASTContext(left, error);
  if (!ast) {
    return ValueObjectSP();
  }

  if (!left->GetCompilerType().IsScalarType() || !right->GetCompilerType().IsScalarType()) {
    error.SetErrorString("not a scalar type");
    return ValueObjectSP();
  }

  Scalar sleft, sright;
  if (!left->ResolveValue(sleft) || !right->ResolveValue(sright)) {
    error.SetErrorString("could not resolve scalar value");
    return ValueObjectSP();
  }

  bool result = T()(sleft, sright);
  Scalar value = int(result);

  CompilerType type = ast->CreateBoolType(ConstString("bool"));
  return CreateValueFromScalar(exe_ctx, value, type, error);
}

ValueObjectSP
lldb_private::rust::ArrayIndex (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
                                lldb::ValueObjectSP right, Status &error)
{
  if (!left->GetCompilerType().IsScalarType() || !right->GetCompilerType().IsScalarType()) {
    error.SetErrorString("not a scalar type");
    return ValueObjectSP();
  }
  if (RustASTContext *ast = GetASTContext(right, error)) {
    CompilerType type = right->GetCompilerType();
    if (ast->IsBooleanType(type.GetOpaqueQualType())) {
      error.SetErrorString("not a scalar type");
      return ValueObjectSP();
    }
  }

  Scalar sright;
  if (!right->ResolveValue(sright)) {
    error.SetErrorString("could not resolve scalar value");
    return ValueObjectSP();
  }
  unsigned long index = sright.ULong(-1);

  ValueObjectSP result = left->GetChildAtIndex(index, true);
  if (!result) {
    error.SetErrorString("array index out of bounds");
  }
  return result;
}

lldb::ValueObjectSP
RustLiteral::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  return CreateValueFromScalar(exe_ctx, m_value, m_type, error);
}

lldb::ValueObjectSP
RustAndAndExpression::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  ValueObjectSP vleft = m_left->Evaluate(exe_ctx, error);
  if (!vleft) {
    return vleft;
  }

  if (RustASTContext *ast = GetASTContext(vleft, error)) {
    CompilerType type = vleft->GetCompilerType();
    if (!ast->IsBooleanType(type.GetOpaqueQualType())) {
      error.SetErrorString("not a boolean type");
      return ValueObjectSP();
    }

    Scalar sleft;
    if (!vleft->ResolveValue(sleft)) {
      error.SetErrorString("could not resolve scalar value");
      return ValueObjectSP();
    }
    if (sleft.IsZero()) {
      return vleft;
    }
  } else {
    return ValueObjectSP();
  }

  ValueObjectSP vright = m_right->Evaluate(exe_ctx, error);
  if (!vright) {
    return vright;
  }

  // FIXME should probably error out if not boolean.
  return vright;
}

lldb::ValueObjectSP
RustOrOrExpression::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  ValueObjectSP vleft = m_left->Evaluate(exe_ctx, error);
  if (!vleft) {
    return vleft;
  }

  if (RustASTContext *ast = GetASTContext(vleft, error)) {
    CompilerType type = vleft->GetCompilerType();
    if (!ast->IsBooleanType(type.GetOpaqueQualType())) {
      error.SetErrorString("not a boolean type");
      return ValueObjectSP();
    }

    Scalar sleft;
    if (!vleft->ResolveValue(sleft)) {
      error.SetErrorString("could not resolve scalar value");
      return ValueObjectSP();
    }
    if (!sleft.IsZero()) {
      return vleft;
    }
  } else {
    return ValueObjectSP();
  }

  ValueObjectSP vright = m_right->Evaluate(exe_ctx, error);
  if (!vright) {
    return vright;
  }

  // FIXME should probably error out if not boolean.
  return vright;
}

lldb::ValueObjectSP
RustFieldExpression::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  ValueObjectSP left = m_left->Evaluate(exe_ctx, error);
  if (!left) {
    return left;
  }

  ValueObjectSP result = left->GetChildMemberWithName(ConstString(m_field.c_str()), true);
  if (!result) {
    error.SetErrorStringWithFormat("no field named %s", m_field.c_str());
  }
  return result;
}

lldb::ValueObjectSP
RustTupleFieldExpression::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  ValueObjectSP left = m_left->Evaluate(exe_ctx, error);
  if (!left) {
    return left;
  }

  ValueObjectSP result = left->GetChildAtIndex(m_field, true);
  if (!result) {
    error.SetErrorStringWithFormat("no field number %d", m_field);
  }
  return result;
}

lldb::ValueObjectSP
RustTupleExpression::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  error.SetErrorString("tuple expressions not supported");
  return ValueObjectSP();
}

lldb::ValueObjectSP
RustArrayLiteral::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  error.SetErrorString("array literals not supported");
  return ValueObjectSP();
}

lldb::ValueObjectSP
RustArrayWithLength::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  error.SetErrorString("array literals not supported");
  return ValueObjectSP();
}

lldb::ValueObjectSP
RustCall::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  error.SetErrorString("function calls not supported");
  return ValueObjectSP();
}

lldb::ValueObjectSP
RustCast::Evaluate(ExecutionContext &exe_ctx, Status &error)
{
  error.SetErrorString("casts not supported");
  return ValueObjectSP();
}

////////////////////////////////////////////////////////////////
// The parser

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
public:
  T operator()(const T &l, const T&r) {
    return l << r;
  }
};

template<typename T>
class right_shift {
public:
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
