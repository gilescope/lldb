//===-- RustUserExpression.cpp ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RustUserExpression.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Symbol/RustASTContext.h"
#include "Plugins/ExpressionParser/Rust/RustParse.h"

using namespace lldb_private::rust;
using namespace lldb_private;
using namespace lldb;

bool RustUserExpression::Parse(DiagnosticManager &diagnostic_manager,
			       ExecutionContext &exe_ctx,
			       lldb_private::ExecutionPolicy execution_policy,
			       bool keep_result_in_memory, bool generate_debug_info)
{
  InstallContext(exe_ctx);

  Parser parser(m_expr_text);
  Status status;
  m_expr = parser.Expr(status);
  // if (!m_expr) {
  //   // Report error.
  //   return false;
  // }

  // blah blah
  return false;			// FIXME
}

lldb::ExpressionResults RustUserExpression::DoExecute(DiagnosticManager &diagnostic_manager,
						      ExecutionContext &exe_ctx,
						      const EvaluateExpressionOptions &options,
						      lldb::UserExpressionSP &shared_ptr_to_me,
						      lldb::ExpressionVariableSP &result)
{
  Status error;
  ValueObjectSP value = m_expr->Evaluate(exe_ctx, error);
}
