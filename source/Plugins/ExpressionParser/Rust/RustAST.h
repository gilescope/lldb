//===-- RustAST.h -------------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustAST_h
#define liblldb_RustAST_h

#include <memory>

#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"
#include "lldb/Core/Scalar.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Utility/Stream.h"
#include "RustLex.h"

namespace lldb_private {

// Functions which are used in the templates below to construct
// various expression nodes.
namespace rust {

lldb::ValueObjectSP UnaryDereference(ExecutionContext &exe_ctx, lldb::ValueObjectSP addr,
				     Status &error);
lldb::ValueObjectSP UnaryAddr(ExecutionContext &exe_ctx, lldb::ValueObjectSP val,
			      Status &error);
lldb::ValueObjectSP UnaryPlus(ExecutionContext &exe_ctx, lldb::ValueObjectSP val,
			      Status &error);
lldb::ValueObjectSP UnaryNegate(ExecutionContext &exe_ctx, lldb::ValueObjectSP val,
				Status &error);
lldb::ValueObjectSP UnaryComplement(ExecutionContext &exe_ctx, lldb::ValueObjectSP val,
				    Status &error);
lldb::ValueObjectSP UnarySizeof(ExecutionContext &exe_ctx, lldb::ValueObjectSP val,
				Status &error);

template<typename T>
lldb::ValueObjectSP BinaryOperation (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
				     lldb::ValueObjectSP right, Status &error);

template<typename T>
lldb::ValueObjectSP Comparison (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
				lldb::ValueObjectSP right, Status &error);

lldb::ValueObjectSP ArrayIndex (ExecutionContext &exe_ctx, lldb::ValueObjectSP left,
				lldb::ValueObjectSP right, Status &error);

}

class RustExpression;
typedef std::unique_ptr<RustExpression> RustExpressionUP;

Stream &operator<< (Stream &stream, const RustExpressionUP &expr);
Stream &operator<< (Stream &stream, const Scalar &value);
Stream &operator<< (Stream &stream, const CompilerType &type);

class RustExpression {
protected:

  RustExpression() { }

public:

  virtual ~RustExpression() { }

  virtual void print(Stream &stream) = 0;

  virtual lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) = 0;
};

typedef lldb::ValueObjectSP (*RustUnaryOperator)(ExecutionContext &, lldb::ValueObjectSP,
                                                 Status &error);

template<char TAG, RustUnaryOperator OP>
class RustUnaryExpression : public RustExpression {
public:

  explicit RustUnaryExpression(RustExpressionUP &&expr)
    : m_expr(std::move(expr))
  {
  }

  void print(Stream &stream) override {
    stream << TAG << " (" << m_expr << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    lldb::ValueObjectSP value = m_expr->Evaluate(exe_ctx, error);
    if (!value)
      return value;
    return OP(exe_ctx, value, error);
  }

private:

  RustExpressionUP m_expr;
};

typedef lldb::ValueObjectSP (*RustBinaryOperator)(ExecutionContext &,
                                                  lldb::ValueObjectSP, lldb::ValueObjectSP,
                                                  Status &error);

template<int TAG, RustBinaryOperator OP>
class RustBinaryExpression : public RustExpression {
public:

  RustBinaryExpression(RustExpressionUP &&left,
		       RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_left << " ";
    lldb_private::rust::PrintTokenKind(stream, TAG);
    stream << " " << m_right << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    lldb::ValueObjectSP left = m_left->Evaluate(exe_ctx, error);
    if (!left)
      return left;
    lldb::ValueObjectSP right = m_right->Evaluate(exe_ctx, error);
    if (!right)
      return right;
    return OP(exe_ctx, left, right, error);
  }

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

template<int TAG, RustBinaryOperator OP>
class RustAssignExpression : public RustExpression {
public:

  void print(Stream &stream) override {
    stream << "(" << m_left << " ";
    lldb_private::rust::PrintTokenKind(stream, TAG);
    stream << " " << m_right << ")";
  }

  RustAssignExpression(RustExpressionUP &&left,
		       RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    // FIXME
    lldb::ValueObjectSP left = m_left->Evaluate(exe_ctx, error);
    if (!left)
      return left;
    lldb::ValueObjectSP right = m_right->Evaluate(exe_ctx, error);
    if (!right)
      return right;
    return OP(exe_ctx, left, right, error);
  }

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

class RustAssignment : public RustExpression {
public:

  RustAssignment(RustExpressionUP &&left,
                 RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_left << " = " << m_right << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    error.SetErrorString("assignment unimplemented");
    return lldb::ValueObjectSP();
  }

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

class RustAndAndExpression : public RustExpression {
public:

  RustAndAndExpression(RustExpressionUP &&left,
		       RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_left << " && " << m_right << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

class RustOrOrExpression : public RustExpression {
public:

  RustOrOrExpression(RustExpressionUP &&left,
		     RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_left << " || " << m_right << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

class RustRangeExpression : public RustExpression {
public:

  // Either or both can be NULL here.
  RustRangeExpression(RustExpressionUP &&left,
		      RustExpressionUP &&right)
    : m_left(std::move(left)),
      m_right(std::move(right))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_left << " .. " << m_right << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_left;
  RustExpressionUP m_right;
};

class RustFieldExpression : public RustExpression {
public:

  RustFieldExpression(RustExpressionUP &&left, llvm::StringRef field)
    : m_left(std::move(left)),
      m_field(field.str())
  {
  }

  void print(Stream &stream) override {
    stream << m_left << "." << m_field;
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_left;
  std::string m_field;
};

class RustTupleFieldExpression : public RustExpression {
public:

  RustTupleFieldExpression(RustExpressionUP &&left, uint32_t field)
    : m_left(std::move(left)),
      m_field(field)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  void print(Stream &stream) override {
    stream << m_left << "." << m_field;
  }

  RustExpressionUP m_left;
  uint32_t m_field;
};

class RustLiteral : public RustExpression {
public:

  RustLiteral(Scalar value, CompilerType type)
    : m_value(value),
      m_type(type)
  {
  }

  void print(Stream &stream) override {
    stream << m_value;
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  Scalar m_value;
  CompilerType m_type;
};

// FIXME maybe this should just be RustLiteral with the correct type.
class RustBooleanLiteral : public RustExpression {
public:

  RustBooleanLiteral(bool value)
    : m_value(value)
  {
  }

  void print(Stream &stream) override {
    stream << (m_value ? "true" : "false");
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  bool m_value;
};

// FIXME maybe this should just be RustLiteral with the correct type.
class RustCharLiteral : public RustExpression {
public:

  RustCharLiteral(uint32_t value, bool is_byte)
    : m_value(value),
      m_is_byte(is_byte)
  {
  }

  void print(Stream &stream) override {
    stream << (m_is_byte ? "b'" : "'");
    if (m_value >= ' ' && m_value < 128) {
      stream << char(m_value);
    } else {
      if (m_is_byte) {
        stream << "\\x";
        stream.PutHex8(m_value);
      } else {
        stream << "\\u{";
        stream.PutHex32(m_value);
        stream << "}";
      }
    }
    stream << "'";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  uint32_t m_value;
  bool m_is_byte;
};

class RustStringLiteral : public RustExpression {
public:

  RustStringLiteral(std::string &&value, bool is_byte)
    : m_value(std::move(value)),
      m_is_byte(is_byte)
  {
  }

  void print(Stream &stream) override {
    stream << (m_is_byte ? "b\"" : "\"") << m_value << "\"";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  std::string m_value;
  bool m_is_byte;
};

class RustTupleExpression : public RustExpression {
public:

  explicit RustTupleExpression(std::vector<RustExpressionUP> &&exprs)
    : m_exprs(std::move(exprs))
  {
  }

  void print(Stream &stream) override {
    stream << "(";
    for (auto &&iter : m_exprs) {
      // We want a trailing "," in one case, so just leave it in
      // always.
      stream << iter << ", ";
    }
    stream << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  std::vector<RustExpressionUP> m_exprs;

};

class RustArrayLiteral : public RustExpression {
public:

  explicit RustArrayLiteral(std::vector<RustExpressionUP> &&exprs)
    : m_exprs(std::move(exprs))
  {
  }

  void print(Stream &stream) override {
    stream << "[";
    bool first = true;
    for (auto &&iter : m_exprs) {
      if (!first) {
        stream << ", ";
      }
      first = false;
      stream << iter;
    }
    stream << "]";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  std::vector<RustExpressionUP> m_exprs;

};

class RustArrayWithLength : public RustExpression {
public:

  RustArrayWithLength(RustExpressionUP &&value, uint64_t length)
    : m_value(std::move(value)),
      m_length(length)
  {
  }

  void print(Stream &stream) override {
    stream << "[" << m_value << "; "
           // Work around Stream weirdness.
           << int64_t(m_length)
           << "]";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_value;
  uint64_t m_length;
};

class RustCall : public RustExpression {
public:

  RustCall(RustExpressionUP &&func, std::vector<RustExpressionUP> &&exprs)
    : m_func(std::move(func)),
      m_exprs(std::move(exprs))
  {
  }

  void print(Stream &stream) override {
    stream << m_func << " (";
    bool first = true;
    for (auto &&iter : m_exprs) {
      if (!first) {
        stream << ", ";
      }
      first = false;
      stream << iter;
    }
    stream << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_func;
  std::vector<RustExpressionUP> m_exprs;

};

class RustCast : public RustExpression {
public:

  RustCast(CompilerType type, RustExpressionUP &&expr)
    : m_type(type),
      m_expr(std::move(expr))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_expr << " as " << m_type << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  CompilerType m_type;
  RustExpressionUP m_expr;
};

} // namespace lldb_private

#endif // liblldb_RustAST_h
