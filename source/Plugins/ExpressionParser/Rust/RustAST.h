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

class RustTypeExpression;
typedef std::unique_ptr<RustTypeExpression> RustTypeExpressionUP;

Stream &operator<< (Stream &stream, const RustExpressionUP &expr);
Stream &operator<< (Stream &stream, const RustTypeExpressionUP &type);
Stream &operator<< (Stream &stream, const Scalar &value);

template<typename T>
Stream &operator<< (Stream &stream, const std::vector<T> &items) {
  bool first = true;
  for (const T &item : items) {
    if (!first) {
      stream << ", ";
    }
    first = false;
    stream << item;
  }
  return stream;
}

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
    stream << m_left << "." << int(m_field);
  }

  RustExpressionUP m_left;
  uint32_t m_field;
};

class RustLiteral : public RustExpression {
public:

  RustLiteral(Scalar value, RustTypeExpressionUP &&type)
    : m_value(value),
      m_type(std::move(type))
  {
  }

  void print(Stream &stream) override {
    stream << m_value;
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  Scalar m_value;
  RustTypeExpressionUP m_type;
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
    // Maybe emit an extra "," to differentiate from (expr).
    stream << "(" << m_exprs << (m_exprs.empty() ? "" : ", ") << ")";
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
    stream << "[" << m_exprs << "]";
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
    stream << m_func << " (" << m_exprs << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustExpressionUP m_func;
  std::vector<RustExpressionUP> m_exprs;

};

class RustCast : public RustExpression {
public:

  RustCast(RustTypeExpressionUP &&type, RustExpressionUP &&expr)
    : m_type(std::move(type)),
      m_expr(std::move(expr))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_expr << " as " << m_type << ")";
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  RustTypeExpressionUP m_type;
  RustExpressionUP m_expr;
};

class RustPathExpression : public RustExpression {
public:

  RustPathExpression(bool relative, int supers, std::vector<std::string> &&path,
                     std::vector<RustTypeExpressionUP> &&generic_params)
    : m_relative(relative),
      m_supers(supers),
      m_path(std::move(path)),
      m_generic_params(std::move(generic_params))
  {
  }

  void print(Stream &stream) override {
    if (!m_relative) {
      stream << "::";
    }
    for (int i = 0; i < m_supers; ++i) {
      stream << "super::";
    }
    bool first = true;
    for (const std::string &str : m_path) {
      if (!first) {
        stream << "::";
      }
      first = false;
      stream << str;
    }
    if (!m_generic_params.empty()) {
      stream << "::<" << m_generic_params << ">";
    }
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  bool m_relative;
  int m_supers;
  std::vector<std::string> m_path;
  std::vector<RustTypeExpressionUP> m_generic_params;
};


class RustTypeExpression {
protected:

  RustTypeExpression() { }

public:

  virtual ~RustTypeExpression() { }

  virtual void print(Stream &stream) = 0;

  virtual CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) = 0;
};

class RustPathTypeExpression : public RustTypeExpression {
public:

  RustPathTypeExpression(bool relative, int supers, std::vector<std::string> &&path,
                         std::vector<RustTypeExpressionUP> &&generic_params)
    : m_relative(relative),
      m_supers(supers),
      m_path(std::move(path)),
      m_generic_params(std::move(generic_params))
  {
  }

  explicit RustPathTypeExpression(std::string &&item)
    : m_relative(true),
      m_supers(0)
  {
    m_path.push_back(std::move(item));
  }

  void print(Stream &stream) override {
    if (!m_relative) {
      stream << "::";
    }
    for (int i = 0; i < m_supers; ++i) {
      stream << "super::";
    }
    bool first = true;
    for (const std::string &str : m_path) {
      if (!first) {
        stream << "::";
      }
      first = false;
      stream << str;
    }

    if (!m_generic_params.empty()) {
      stream << "<" << m_generic_params << ">";
    }
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  bool m_relative;
  int m_supers;
  std::vector<std::string> m_path;
  std::vector<RustTypeExpressionUP> m_generic_params;
};

class RustArrayTypeExpression : public RustTypeExpression {
public:

  RustArrayTypeExpression(RustTypeExpressionUP &&element, uint64_t len)
    : m_element(std::move(element)),
      m_len(len)
  {
  }

  void print(Stream &stream) override {
    stream << "[" << m_element << "; " << int64_t(m_len) << "]";
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  RustTypeExpressionUP m_element;
  uint64_t m_len;
};

class RustSliceTypeExpression : public RustTypeExpression {
public:

  RustSliceTypeExpression(RustTypeExpressionUP &&element)
    : m_element(std::move(element))
  {
  }

  void print(Stream &stream) override {
    stream << "&[" << m_element << "]";
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  RustTypeExpressionUP m_element;
};

class RustPointerTypeExpression : public RustTypeExpression {
public:

  RustPointerTypeExpression(RustTypeExpressionUP &&target, bool is_ref, bool is_mut = false)
    : m_target(std::move(target)),
      m_is_ref(is_ref),
      m_is_mut(is_mut)
  {
  }

  void print(Stream &stream) override {
    if (m_is_ref) {
      stream << "&" << m_target;
    } else {
      stream << "*" << (m_is_mut ? "mut " : "const ") << m_target;
    }
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  RustTypeExpressionUP m_target;
  bool m_is_ref;
  bool m_is_mut;
};

class RustFunctionTypeExpression : public RustTypeExpression {
public:

  RustFunctionTypeExpression(RustTypeExpressionUP &&result,
                             std::vector<RustTypeExpressionUP> &&arguments)
    : m_result(std::move(result)),
      m_arguments(std::move(arguments))
  {
  }

  void print(Stream &stream) override {
    stream << "fn (" << m_arguments << ") -> " << m_result;
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  RustTypeExpressionUP m_result;
  std::vector<RustTypeExpressionUP> m_arguments;
};

class RustTupleTypeExpression : public RustTypeExpression {
public:

  RustTupleTypeExpression(std::vector<RustTypeExpressionUP> &&arguments)
    : m_arguments(std::move(arguments))
  {
  }

  void print(Stream &stream) override {
    stream << "(" << m_arguments << ")";
  }

  CompilerType Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    return CompilerType();
  }

private:

  std::vector<RustTypeExpressionUP> m_arguments;
};

} // namespace lldb_private

#endif // liblldb_RustAST_h
