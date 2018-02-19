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

  virtual lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx) = 0;
};

typedef lldb::ValueObjectSP (*RustUnaryOperator)(ExecutionContext &, lldb::ValueObjectSP);

template<RustUnaryOperator OP>
class RustUnaryExpression : public RustExpression {
public:

  explicit RustUnaryExpression(RustExpression *expr)
    : m_expr(expr)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx) override {
    lldb::ValueObjectSP value = m_expr->Evaluate(exe_ctx);
    return OP(exe_ctx, value);
  }

private:

  std::unique_ptr<RustExpression> m_expr;
};

typedef lldb::ValueObjectSP (*RustBinaryOperator)(ExecutionContext &,
						  lldb::ValueObjectSP, lldb::ValueObjectSP);

template<RustBinaryOperator OP>
class RustBinaryExpression : public RustExpression {
public:

  RustBinaryExpression(RustExpression *left, RustExpression *right)
    : m_left(left),
      m_right(right)
  {
  }

  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx) override {
    lldb::ValueObjectSP left = m_left->Evaluate(exe_ctx);
    lldb::ValueObjectSP right = m_right->Evaluate(exe_ctx);
    return OP(exe_ctx, left, right);
  }

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

  // lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx) override {
  //   lldb::ValueObjectSP left = m_low_up->Evaluate(exe_ctx);
  //   fixme;
  // }

private:

  std::unique_ptr<RustExpression> m_left;
  llvm::StringRef m_field;
};


} // namespace lldb_private

#endif // liblldb_RustAST_h
