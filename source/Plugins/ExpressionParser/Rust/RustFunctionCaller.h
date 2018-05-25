//===-- RustFunctionCaller.h -----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustFunctionCaller_h_
#define liblldb_RustFunctionCaller_h_

#include "lldb/Core/Address.h"
#include "lldb/Core/ArchSpec.h"
#include "lldb/Core/Value.h"
#include "lldb/Core/ValueObjectList.h"
#include "lldb/Expression/FunctionCaller.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Target/Process.h"

namespace lldb_private {

class RustFunctionCaller : public FunctionCaller {

  class RustExpressionTypeSystemHelper : public ExpressionTypeSystemHelper {
  public:
    RustExpressionTypeSystemHelper()
      : ExpressionTypeSystemHelper(ExpressionTypeSystemHelper::eKindRustHelper)
    {}
  };

public:
  RustFunctionCaller(ExecutionContextScope &exe_scope,
                      const CompilerType &return_type,
                      const Address &function_address,
                      const ValueList &arg_value_list, const char *name);

  ~RustFunctionCaller() override;

  unsigned CompileFunction(lldb::ThreadSP thread_to_use_sp,
                           DiagnosticManager &diagnostic_manager) override;

  ExpressionTypeSystemHelper *GetTypeSystemHelper() override {
    return &m_type_system_helper;
  }

private:

  RustExpressionTypeSystemHelper m_type_system_helper;
};

} // namespace lldb_private

#endif // liblldb_RustFunctionCaller_h_
