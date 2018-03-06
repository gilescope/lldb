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
UnaryDereference(ExecutionContext &exe_ctx, ValueObjectSP addr, Status &error)
{
  return addr->Dereference(error);
}

ValueObjectSP
UnaryAddr(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
{
  return val->AddressOf(error);
}

ValueObjectSP
UnaryPlus(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
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
UnaryNegate(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
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
UnaryComplement(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
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
UnarySizeof(ExecutionContext &exe_ctx, ValueObjectSP val, Status &error)
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
BinaryOperation (ExecutionContext &exe_ctx, lldb::ValueObjectSP left, lldb::ValueObjectSP right,
		 Status &error)
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
Comparison (ExecutionContext &exe_ctx, lldb::ValueObjectSP left, lldb::ValueObjectSP right,
	    Status &error)
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
ArrayIndex (ExecutionContext &exe_ctx, lldb::ValueObjectSP left, lldb::ValueObjectSP right,
	    Status &error)
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
