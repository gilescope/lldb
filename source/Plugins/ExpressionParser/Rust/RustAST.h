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

namespace lldb_private {

class RustExpression {
public:

  virtual lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) = 0;
};

typedef lldb::ValueObjectSP (*RustUnaryOperator)(ExecutionContext &, lldb::ValueObjectSP,
                                                 Status &error);

template<RustUnaryOperator OP>
class RustUnaryExpression : public RustExpression {
public:

  explicit RustUnaryExpression(RustExpression *expr)
    : m_expr(expr)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override {
    lldb::ValueObjectSP value = m_expr->Evaluate(exe_ctx, error);
    if (!value)
      return value;
    return OP(exe_ctx, value, error);
  }

private:

  std::unique_ptr<RustExpression> m_expr;
};

typedef lldb::ValueObjectSP (*RustBinaryOperator)(ExecutionContext &,
                                                  lldb::ValueObjectSP, lldb::ValueObjectSP,
                                                  Status &error);

template<RustBinaryOperator OP>
class RustBinaryExpression : public RustExpression {
public:

  RustBinaryExpression(RustExpression *left, RustExpression *right)
    : m_left(left),
      m_right(right)
  {
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

  std::unique_ptr<RustExpression> m_left;
  std::unique_ptr<RustExpression> m_right;
};

class RustRangeExpression : public RustExpression {
public:

  // Either or both can be NULL here.
  RustRangeExpression(RustExpression *left, RustExpression *right)
    : m_left(left),
      m_right(right)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  std::unique_ptr<RustExpression> m_left;
  std::unique_ptr<RustExpression> m_right;
};

class RustFieldExpression : public RustExpression {
public:

  RustFieldExpression(RustExpression *left, llvm::StringRef field)
    : m_left(left),
      m_field(field)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx, Status &error) override;

private:

  std::unique_ptr<RustExpression> m_left;
  llvm::StringRef m_field;
};


} // namespace lldb_private

#endif // liblldb_RustAST_h
