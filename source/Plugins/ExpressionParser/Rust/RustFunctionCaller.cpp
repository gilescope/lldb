//===-- RustFunctionCaller.cpp ---------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RustFunctionCaller.h"

#include "Plugins/ExpressionParser/Clang/ClangExpressionParser.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Module.h"

#include "lldb/Core/Module.h"
#include "lldb/Core/State.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Core/ValueObjectList.h"
#include "lldb/Expression/DiagnosticManager.h"
#include "lldb/Expression/IRExecutionUnit.h"
#include "lldb/Interpreter/CommandReturnObject.h"
#include "lldb/Symbol/RustASTContext.h"
#include "lldb/Symbol/Function.h"
#include "lldb/Symbol/Type.h"
#include "lldb/Target/ExecutionContext.h"
#include "lldb/Target/Process.h"
#include "lldb/Target/RegisterContext.h"
#include "lldb/Target/Target.h"
#include "lldb/Target/Thread.h"
#include "lldb/Target/ThreadPlan.h"
#include "lldb/Target/ThreadPlanCallFunction.h"
#include "lldb/Utility/DataExtractor.h"
#include "lldb/Utility/Log.h"

using namespace lldb_private;

//----------------------------------------------------------------------
// RustFunctionCaller constructor
//----------------------------------------------------------------------
RustFunctionCaller::RustFunctionCaller(ExecutionContextScope &exe_scope,
                                       const CompilerType &return_type,
                                       const Address &functionAddress,
                                       const ValueList &arg_value_list,
                                       const char *name)
  : FunctionCaller(exe_scope, return_type, functionAddress, arg_value_list,
                   name) {
  m_jit_process_wp = lldb::ProcessWP(exe_scope.CalculateProcess());
  // Can't make a RustFunctionCaller without a process.
  assert(m_jit_process_wp.lock());
}

//----------------------------------------------------------------------
// Destructor
//----------------------------------------------------------------------
RustFunctionCaller::~RustFunctionCaller() {}

static bool
AppendType(std::string *output, RustASTContext *ast, const std::string &varname,
           CompilerType type) {
  std::string value;
  if (!ast->GetCABITypeDeclaration(type, varname, &value)) {
    return false;
  }
  output->append("    ");
  output->append(value);
  output->append(";\n");
  return true;
}

unsigned RustFunctionCaller::CompileFunction(lldb::ThreadSP thread_to_use_sp,
                                             DiagnosticManager &diagnostic_manager) {
  if (m_compiled)
    return 0;

  // Compilation might call code, make sure to keep on the thread the caller
  // indicated.
  ThreadList::ExpressionExecutionThreadPusher execution_thread_pusher(
                                                                      thread_to_use_sp);

  RustASTContext *ast =
    llvm::dyn_cast_or_null<RustASTContext>(m_function_return_type.GetTypeSystem());
  if (!ast) {
    diagnostic_manager.PutString(eDiagnosticSeverityError, "not in a Rust context!?");
    return 1;
  }

  // Cons up the function we're going to wrap our call in, then compile it...
  // We declare the function "extern "C"" because the compiler might be in C++
  // mode which would mangle the name and then we couldn't find it again...
  m_wrapper_function_text.clear();
  m_wrapper_function_text.append("extern \"C\" void ");
  m_wrapper_function_text.append(m_wrapper_function_name);
  m_wrapper_function_text.append(" (void *input)\n{\n  struct ");
  m_wrapper_function_text.append(m_wrapper_struct_name);
  m_wrapper_function_text.append(" {\n");

  CompilerType function_type = m_function_ptr->GetCompilerType().GetPointeeType();

  if (!AppendType(&m_wrapper_function_text, ast, "fn_ptr", function_type)
      || !AppendType(&m_wrapper_function_text, ast, "result",
                     function_type.GetFunctionReturnType())) {
    diagnostic_manager.PutString(eDiagnosticSeverityError,
                                 "could not compute Rust type declaration");
    return 1;
  }

  std::string arguments;
  for (int i = 0; i < function_type.GetFunctionArgumentCount(); ++i) {
    std::string argname = "__arg_" + std::to_string(i);
    if (!AppendType(&m_wrapper_function_text, ast, argname,
                    function_type.GetFunctionArgumentTypeAtIndex(i))) {
      diagnostic_manager.PutString(eDiagnosticSeverityError,
                                   "could not compute Rust type declaration");
      return 1;
    }
    if (i > 0) {
      arguments.append(", ");
    }
    arguments.append("__lldb_fn_data->");
    arguments.append(argname);
  }

  m_wrapper_function_text.append("  };\n");

  m_wrapper_function_text.append("  struct ");
  m_wrapper_function_text.append(m_wrapper_struct_name);
  m_wrapper_function_text.append(" *__lldb_fn_data = (");
  m_wrapper_function_text.append(m_wrapper_struct_name);
  m_wrapper_function_text.append(" *) input;\n");

  m_wrapper_function_text.append("__lldb_fn_data->result = __lldb_fn_data->fn_ptr(");
  m_wrapper_function_text.append(arguments);
  m_wrapper_function_text.append(");\n}\n");

  Log *log(lldb_private::GetLogIfAllCategoriesSet(LIBLLDB_LOG_EXPRESSIONS));
  if (log)
    log->Printf("Expression: \n\n%s\n\n", m_wrapper_function_text.c_str());

  // Okay, now compile this expression

  lldb::ProcessSP jit_process_sp(m_jit_process_wp.lock());
  unsigned num_errors;
  if (jit_process_sp) {
    m_parser.reset(new ClangExpressionParser(jit_process_sp.get(), *this, true));

    num_errors = m_parser->Parse(diagnostic_manager);
  } else {
    diagnostic_manager.PutString(eDiagnosticSeverityError,
                                 "no process - unable to inject function");
    num_errors = 1;
  }

  m_compiled = (num_errors == 0);
  return num_errors;
}
