//===-- RustASTContext.cpp ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <mutex>
#include <utility>
#include <vector>

#include "lldb/Core/Module.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/Core/StreamFile.h"
#include "lldb/Core/UniqueCStringMap.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/StringPrinter.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Symbol/RustASTContext.h"
#include "lldb/Symbol/ObjectFile.h"
#include "lldb/Symbol/SymbolFile.h"
#include "lldb/Symbol/Type.h"
#include "lldb/Target/ExecutionContext.h"
#include "lldb/Target/Target.h"
#include "lldb/Core/DumpDataExtractor.h"

#include "llvm/Support/Threading.h"

// #include "Plugins/ExpressionParser/Rust/RustUserExpression.h"
#include "Plugins/SymbolFile/DWARF/DWARFASTParserRust.h"

using namespace lldb;

namespace lldb_private {

#define NO_COPY(Name) \
  Name(const Name &) = delete; \
  Name &operator=(const Name &) = delete

class RustAggregateBase;
class RustArray;
class RustFunction;
class RustIntegral;
class RustPointer;
class RustStruct;
class RustTuple;
class RustTypedef;

class RustType {
protected:

  RustType(const ConstString &name) : m_name(name) {}
  NO_COPY (RustType);

public:

  virtual ~RustType() {}

  ConstString Name() const { return m_name; }

  virtual lldb::Format Format() const {
    return eFormatBytes;
  }

  virtual uint32_t TypeInfo(CompilerType *element_type) const = 0;
  virtual lldb::TypeClass TypeClass() const = 0;
  virtual uint64_t ByteSize() const = 0;

  virtual RustAggregateBase *AsAggregate() { return nullptr; }
  virtual RustArray *AsArray () { return nullptr; }
  virtual RustFunction *AsFunction() { return nullptr; }
  virtual RustIntegral *AsInteger () { return nullptr; }
  virtual RustPointer *AsPointer () { return nullptr; }
  virtual RustStruct *AsStruct() { return nullptr; }
  virtual RustTuple *AsTuple() { return nullptr; }
  virtual RustTypedef *AsTypedef() { return nullptr; }

  virtual bool IsAggregateType() const { return false; }
  virtual bool IsCharType() const { return false; }
  virtual bool IsFloatType() const { return false; }

private:
  ConstString m_name;
};

class RustBool : public RustType {
public:
  RustBool(const ConstString &name) : RustType(name) {}
  NO_COPY(RustBool);

  lldb::Format Format() const override {
    return eFormatBoolean;
  }

  uint32_t TypeInfo(CompilerType *) const override {
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassBuiltin;
  }

  uint64_t ByteSize() const override {
    return 1;
  }
};

class RustIntegral : public RustType {
public:
  RustIntegral(const ConstString &name, bool is_signed, uint64_t byte_size,
	       bool is_char = false)
    : RustType(name),
      m_is_signed(is_signed),
      m_byte_size(byte_size),
      m_is_char(is_char)
  {}
  NO_COPY(RustIntegral);

  lldb::Format Format() const override {
    return m_is_signed ? eFormatDefault : eFormatUnsigned;
  }

  bool IsSigned() const { return m_is_signed; }
  uint64_t ByteSize() const override { return m_byte_size; }

  RustIntegral *AsInteger () override { return this; }

  bool IsCharType() const override { return m_is_char; }

  uint32_t TypeInfo(CompilerType *) const override {
    uint32_t result = eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar | eTypeIsInteger;
    if (m_is_char)
      result |= eTypeIsSigned;
    return result;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassBuiltin;
  }

private:

  bool m_is_signed;
  uint64_t m_byte_size;
  bool m_is_char;
};

class RustFloat : public RustType {
public:
  RustFloat(const ConstString &name, uint64_t byte_size)
    : RustType(name),
      m_byte_size(byte_size)
  {}
  NO_COPY(RustFloat);

  lldb::Format Format() const override {
    return eFormatFloat;
  }

  bool IsFloatType() const override { return true; }

  uint32_t TypeInfo(CompilerType *) const override {
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsFloat;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassBuiltin;
  }

  uint64_t ByteSize() const override { return m_byte_size; }

private:

  uint64_t m_byte_size;
};

class RustPointer : public RustType {
public:
  // Pointers and references are handled similarly.
  RustPointer(const ConstString &name, const CompilerType &pointee, bool is_reference)
    : RustType(name),
      m_pointee(pointee),
      m_is_reference(is_reference)
  {}
  NO_COPY(RustPointer);

  lldb::Format Format() const override {
    return eFormatPointer;
  }

  bool IsReference() const { return m_is_reference; }

  CompilerType PointeeType() const { return m_pointee; }

  RustPointer *AsPointer() override { return this; }

  uint32_t TypeInfo(CompilerType *elem) const override {
    if (elem)
      *elem = m_pointee;
    return (eTypeIsBuiltIn | eTypeHasValue
	    | (m_is_reference ? eTypeIsReference : eTypeIsPointer));
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassPointer;
  }

  uint64_t ByteSize() const override {
    return 8;			// FIXME
  }

private:

  CompilerType m_pointee;
  bool m_is_reference;
};

class RustArray : public RustType {
public:
  RustArray(const ConstString &name, uint64_t length, const CompilerType &elem)
    : RustType(name),
      m_length(length),
      m_elem(elem)
  {}
  NO_COPY(RustArray);

  uint64_t Length() const { return m_length; }
  RustArray *AsArray() override { return this; }
  CompilerType ElementType() const { return m_elem; }
  bool IsAggregateType() const override { return true; }

  uint32_t TypeInfo(CompilerType *elem) const override {
    if (elem)
      *elem = m_elem;
    return eTypeHasChildren | eTypeIsArray;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassArray;
  }

  uint64_t ByteSize() const override {
    return m_elem.GetByteSize(nullptr) * m_length;
  }

private:
  uint64_t m_length;
  CompilerType m_elem;
};

// Base type for struct, tuple, and tuple struct.
class RustAggregateBase : public RustType {
protected:
  RustAggregateBase(const ConstString &name, uint64_t byte_size)
    : RustType(name),
      m_byte_size(byte_size)
  {}

  NO_COPY(RustAggregateBase);

public:

  RustAggregateBase *AsAggregate() override { return this; }

  bool IsAggregateType() const override { return true; }

  size_t FieldCount() const { return m_fields.size(); }

  uint32_t TypeInfo(CompilerType *) const override {
    return eTypeHasChildren | eTypeIsStructUnion;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassStruct;
  }

  uint64_t ByteSize() const override {
    return m_byte_size;
  }

  struct Field {
    Field(const ConstString &name, const CompilerType &type, uint64_t offset)
      : m_name(name),
	m_type(type),
	m_offset(offset)
    {
    }

    ConstString m_name;
    CompilerType m_type;
    uint64_t m_offset;
  };

  void AddField(const ConstString &name, const CompilerType &type, uint64_t offset) {
    m_fields.emplace_back(name, type, offset);
  }

  const Field *FieldAt(size_t idx) {
    if (idx >= m_fields.size())
      return nullptr;
    return &m_fields[idx];
  }

private:

  uint64_t m_byte_size;
  std::vector<Field> m_fields;
};

class RustTuple : public RustAggregateBase {
public:
  // FIXME differentiate tuple struct?
  RustTuple(const ConstString &name, uint64_t byte_size)
    : RustAggregateBase(name, byte_size)
  {}

  NO_COPY(RustTuple);

  RustTuple *AsTuple() override { return this; }

  void AddField(const CompilerType &type, uint64_t offset) {
    RustAggregateBase::AddField(ConstString(), type, offset);
  }
};

class RustStruct : public RustAggregateBase {
public:
  RustStruct(const ConstString &name, uint64_t byte_size)
    : RustAggregateBase(name, byte_size)
  {}

  NO_COPY(RustStruct);

  RustStruct *AsStruct() override { return this; }
};

class RustFunction : public RustType {
public:
  RustFunction (const ConstString &name, const CompilerType &return_type,
		const std::vector<CompilerType> &&arguments)
    : RustType(name),
      m_arguments(std::move(arguments))
  {
  }
  NO_COPY(RustFunction);

  // do we care about the names?
  void AddArgument(const CompilerType &type) {
    m_arguments.push_back(type);
  }

  RustFunction *AsFunction() override { return this; }

  CompilerType ReturnType() const { return m_return_type; }
  size_t ArgumentCount() { return m_arguments.size(); }
  CompilerType Argument(size_t i) { return m_arguments[i]; }

  uint32_t TypeInfo(CompilerType *) const override {
    return eTypeIsFuncPrototype | eTypeHasValue;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassFunction;
  }

  uint64_t ByteSize() const override {
    return 8;			// FIXME
  }

private:

  CompilerType m_return_type;
  std::vector<CompilerType> m_arguments;
};

class RustTypedef : public RustType {
public:

  RustTypedef(const ConstString &name, const CompilerType &type)
    : RustType(name),
      m_type(type)
  {
  }

  NO_COPY(RustTypedef);

  RustTypedef *AsTypedef() override { return this; }
  CompilerType UnderlyingType() const { return m_type; }

  uint32_t TypeInfo(CompilerType *) const override {
    return eTypeIsTypedef;
  }

  lldb::TypeClass TypeClass() const override {
    return eTypeClassTypedef;
  }

  uint64_t ByteSize() const override {
    return m_type.GetByteSize(nullptr);
  }

private:
  CompilerType m_type;
};

} // namespace lldb_private
using namespace lldb_private;

RustASTContext::RustASTContext()
    : TypeSystem(eKindRust),
      m_pointer_byte_size(0),
      m_int_byte_size(0)
{
}

RustASTContext::~RustASTContext() {}

//------------------------------------------------------------------
// PluginInterface functions
//------------------------------------------------------------------

ConstString RustASTContext::GetPluginNameStatic() {
  return ConstString("rust");
}

ConstString RustASTContext::GetPluginName() {
  return RustASTContext::GetPluginNameStatic();
}

uint32_t RustASTContext::GetPluginVersion() {
  return 1;
}

lldb::TypeSystemSP RustASTContext::CreateInstance(lldb::LanguageType language,
						  Module *module,
						  Target *target) {
  if (language == eLanguageTypeRust) {
    ArchSpec arch;
    std::shared_ptr<RustASTContext> astc;
    if (module) {
      arch = module->GetArchitecture();
      astc = std::shared_ptr<RustASTContext>(new RustASTContext);
    } else if (target) {
      arch = target->GetArchitecture();
      astc = std::shared_ptr<RustASTContextForExpr>(
          new RustASTContextForExpr(target->shared_from_this()));
    }

    if (arch.IsValid()) {
      astc->SetAddressByteSize(arch.GetAddressByteSize());
      return astc;
    }
  }
  return lldb::TypeSystemSP();
}

void RustASTContext::EnumerateSupportedLanguages(
    std::set<lldb::LanguageType> &languages_for_types,
    std::set<lldb::LanguageType> &languages_for_expressions) {
  static std::vector<lldb::LanguageType> s_supported_languages_for_types(
      {lldb::eLanguageTypeRust});

  static std::vector<lldb::LanguageType> s_supported_languages_for_expressions(
      {});

  languages_for_types.insert(s_supported_languages_for_types.begin(),
                             s_supported_languages_for_types.end());
  languages_for_expressions.insert(
      s_supported_languages_for_expressions.begin(),
      s_supported_languages_for_expressions.end());
}

void RustASTContext::Initialize() {
  PluginManager::RegisterPlugin(GetPluginNameStatic(), "Rust AST context plug-in",
                                CreateInstance, EnumerateSupportedLanguages);
}

void RustASTContext::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

//----------------------------------------------------------------------
// Tests
//----------------------------------------------------------------------

bool RustASTContext::IsArrayType(lldb::opaque_compiler_type_t type,
				 CompilerType *element_type, uint64_t *size,
				 bool *is_incomplete) {
  if (element_type)
    element_type->Clear();
  if (size)
    *size = 0;
  if (is_incomplete)
    *is_incomplete = false;
  RustArray *array = static_cast<RustType *>(type)->AsArray();
  if (array) {
    if (size)
      *size = array->Length();
    if (element_type)
      *element_type = array->ElementType();
    return true;
  }
  return false;
}

bool RustASTContext::IsVectorType(lldb::opaque_compiler_type_t type,
				  CompilerType *element_type, uint64_t *size) {
  if (element_type)
    element_type->Clear();
  if (size)
    *size = 0;
  return false;
}

bool RustASTContext::IsAggregateType(lldb::opaque_compiler_type_t type) {
  return static_cast<RustType *>(type)->IsAggregateType();
}

bool RustASTContext::IsBeingDefined(lldb::opaque_compiler_type_t type) {
  return false;
}

bool RustASTContext::IsCharType(lldb::opaque_compiler_type_t type) {
  return static_cast<RustType *>(type)->IsCharType();
}

bool RustASTContext::IsCompleteType(lldb::opaque_compiler_type_t type) {
  return bool(type);
}

bool RustASTContext::IsConst(lldb::opaque_compiler_type_t type) {
  return false;
}

bool RustASTContext::IsCStringType(lldb::opaque_compiler_type_t type,
				   uint32_t &length) {
  return false;
}

bool RustASTContext::IsDefined(lldb::opaque_compiler_type_t type) {
  return type != nullptr;
}

bool RustASTContext::IsFloatingPointType(lldb::opaque_compiler_type_t type,
					 uint32_t &count, bool &is_complex) {
  is_complex = false;
  if (static_cast<RustType *>(type)->IsFloatType()) {
    count = 1;
    return true;
  }
  count = 0;
  return false;
}

bool RustASTContext::IsFunctionType(lldb::opaque_compiler_type_t type,
				    bool *is_variadic_ptr) {
  if (is_variadic_ptr)
    *is_variadic_ptr = false;
  return static_cast<RustType *>(type)->AsFunction() != nullptr;
}

uint32_t RustASTContext::IsHomogeneousAggregate(lldb::opaque_compiler_type_t type,
						CompilerType *base_type_ptr) {
  // FIXME should detect "homogeneous floating-point aggregates".
  return false;
}

size_t
RustASTContext::GetNumberOfFunctionArguments(lldb::opaque_compiler_type_t type) {
  RustFunction *func = static_cast<RustType *>(type)->AsFunction();
  if (func) {
    return func->ArgumentCount();
  }
  return -1;
}

CompilerType
RustASTContext::GetFunctionArgumentAtIndex(lldb::opaque_compiler_type_t type,
					   const size_t index) {
  RustFunction *func = static_cast<RustType *>(type)->AsFunction();
  if (func) {
    return func->Argument(index);
  }
  return CompilerType();
}

bool RustASTContext::IsFunctionPointerType(lldb::opaque_compiler_type_t type) {
  return IsFunctionType(type);
}

bool RustASTContext::IsBlockPointerType(lldb::opaque_compiler_type_t type,
					CompilerType *function_pointer_type_ptr) {
  return false;
}

bool RustASTContext::IsIntegerType(lldb::opaque_compiler_type_t type,
				   bool &is_signed) {
  if (!type)
    return false;

  // FIXME bool?
  RustIntegral *inttype = static_cast<RustType *>(type)->AsInteger();
  if (inttype) {
    is_signed = inttype->IsSigned();
    return true;
  }
  return false;
}

bool RustASTContext::IsPolymorphicClass(lldb::opaque_compiler_type_t type) {
  return false;
}

bool RustASTContext::IsPossibleDynamicType(lldb::opaque_compiler_type_t type,
					   CompilerType *target_type, // Can pass NULL
					   bool check_cplusplus, bool check_objc) {
  if (target_type)
    target_type->Clear();
  // FIXME eventually we'll handle trait object pointers here?
  // FIXME and enums
  return false;
}

bool RustASTContext::IsRuntimeGeneratedType(lldb::opaque_compiler_type_t type) {
  return false;
}

bool RustASTContext::IsPointerType(lldb::opaque_compiler_type_t type,
				   CompilerType *pointee_type) {
  if (!type)
    return false;
  RustPointer *ptr = static_cast<RustType *>(type)->AsPointer();
  if (ptr && !ptr->IsReference()) {
    if (pointee_type)
      *pointee_type = ptr->PointeeType();
    return true;
  }
  return false;
}

bool RustASTContext::IsPointerOrReferenceType(lldb::opaque_compiler_type_t type,
					      CompilerType *pointee_type) {
  if (!type)
    return false;
  RustPointer *ptr = static_cast<RustType *>(type)->AsPointer();
  if (ptr) {
    if (pointee_type)
      *pointee_type = ptr->PointeeType();
    return true;
  }
  return false;
  return IsPointerType(type, pointee_type);
}

bool RustASTContext::IsReferenceType(lldb::opaque_compiler_type_t type,
				     CompilerType *pointee_type,
				     bool *is_rvalue) {
  if (!type)
    return false;
  RustPointer *ptr = static_cast<RustType *>(type)->AsPointer();
  if (ptr && ptr->IsReference()) {
    if (pointee_type)
      *pointee_type = ptr->PointeeType();
    return true;
  }
  return false;
}

bool RustASTContext::IsScalarType(lldb::opaque_compiler_type_t type) {
  return !IsAggregateType(type);
}

bool RustASTContext::IsTypedefType(lldb::opaque_compiler_type_t type) {
  if (type)
    return static_cast<RustType *>(type)->AsTypedef() != nullptr;
  return false;
}

bool RustASTContext::IsVoidType(lldb::opaque_compiler_type_t type) {
  if (!type)
    return false;
  // FIXME do we need to consider the "!" type as well?
  RustTuple *tuple = static_cast<RustType *>(type)->AsTuple();
  return tuple && tuple->Name().IsEmpty() && tuple->FieldCount() == 0;
}

bool RustASTContext::SupportsLanguage(lldb::LanguageType language) {
  return language == eLanguageTypeRust;
}

//----------------------------------------------------------------------
// Type Completion
//----------------------------------------------------------------------

bool RustASTContext::GetCompleteType(lldb::opaque_compiler_type_t type) {
  return bool(type);
}

//----------------------------------------------------------------------
// AST related queries
//----------------------------------------------------------------------

uint32_t RustASTContext::GetPointerByteSize() {
  return m_pointer_byte_size;
}

//----------------------------------------------------------------------
// Accessors
//----------------------------------------------------------------------

ConstString RustASTContext::GetTypeName(lldb::opaque_compiler_type_t type) {
  if (type)
    return static_cast<RustType *>(type)->Name();
  return ConstString();
}

uint32_t
RustASTContext::GetTypeInfo(lldb::opaque_compiler_type_t type,
			    CompilerType *pointee_or_element_compiler_type) {
  if (pointee_or_element_compiler_type)
    pointee_or_element_compiler_type->Clear();
  if (!type)
    return 0;
  return static_cast<RustType *>(type)->TypeInfo(pointee_or_element_compiler_type);
}

lldb::TypeClass RustASTContext::GetTypeClass(lldb::opaque_compiler_type_t type) {
  if (!type)
    return eTypeClassInvalid;
  return static_cast<RustType *>(type)->TypeClass();
}

lldb::BasicType
RustASTContext::GetBasicTypeEnumeration(lldb::opaque_compiler_type_t type) {
  ConstString name = GetTypeName(type);
  if (name.IsEmpty()) {
    // Nothing.
  } else if (strcmp(name.AsCString(), "()") == 0) {
    return eBasicTypeVoid;
  } else if (strcmp(name.AsCString(), "bool") == 0) {
    return eBasicTypeBool;
  }
  return eBasicTypeInvalid;
}

lldb::LanguageType
RustASTContext::GetMinimumLanguage(lldb::opaque_compiler_type_t type) {
  return lldb::eLanguageTypeRust;
}

unsigned RustASTContext::GetTypeQualifiers(lldb::opaque_compiler_type_t type) {
  return 0;
}

//----------------------------------------------------------------------
// Creating related types
//----------------------------------------------------------------------

CompilerType
RustASTContext::GetArrayElementType(lldb::opaque_compiler_type_t type,
				    uint64_t *stride) {
  RustArray *array = static_cast<RustType *>(type)->AsArray();
  if (array) {
    if (stride) {
      *stride = array->ElementType().GetByteSize(nullptr);
    }
    return array->ElementType();
  }
  return CompilerType();
}

CompilerType RustASTContext::GetCanonicalType(lldb::opaque_compiler_type_t type) {
  RustTypedef *t = static_cast<RustType *>(type)->AsTypedef();
  if (t)
    return t->UnderlyingType();
  return CompilerType(this, type);
}

CompilerType
RustASTContext::GetFullyUnqualifiedType(lldb::opaque_compiler_type_t type) {
  return CompilerType(this, type);
}

// Returns -1 if this isn't a function or if the function doesn't have a
// prototype.
// Returns a value >= 0 if there is a prototype.
int RustASTContext::GetFunctionArgumentCount(lldb::opaque_compiler_type_t type) {
  return GetNumberOfFunctionArguments(type);
}

CompilerType
RustASTContext::GetFunctionArgumentTypeAtIndex(lldb::opaque_compiler_type_t type,
                                             size_t idx) {
  return GetFunctionArgumentAtIndex(type, idx);
}

CompilerType
RustASTContext::GetFunctionReturnType(lldb::opaque_compiler_type_t type) {
  CompilerType result;
  if (type) {
    RustFunction *t = static_cast<RustType *>(type)->AsFunction();
    if (t)
      result = t->ReturnType();
  }
  return result;
}

size_t RustASTContext::GetNumMemberFunctions(lldb::opaque_compiler_type_t type) {
  return 0;
}

TypeMemberFunctionImpl
RustASTContext::GetMemberFunctionAtIndex(lldb::opaque_compiler_type_t type,
                                       size_t idx) {
  return TypeMemberFunctionImpl();
}

CompilerType
RustASTContext::GetNonReferenceType(lldb::opaque_compiler_type_t type) {
  return CompilerType(this, type);
}

CompilerType RustASTContext::GetPointeeType(lldb::opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();
  RustPointer *p = static_cast<RustType *>(type)->AsPointer();
  if (p)
    return p->PointeeType();
  return CompilerType();
}

CompilerType RustASTContext::GetPointerType(lldb::opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();
  ConstString type_name = GetTypeName(type);
  ConstString pointer_name(std::string("*") + type_name.GetCString());
  RustType *pointer = m_types[pointer_name].get();
  if (pointer == nullptr) {
    pointer = new RustPointer(pointer_name, CompilerType(this, type), false);
    m_types[pointer_name].reset(pointer);
  }
  return CompilerType(this, pointer);
}

// If the current object represents a typedef type, get the underlying type
CompilerType RustASTContext::GetTypedefedType(lldb::opaque_compiler_type_t type) {
  if (type) {
    RustTypedef *t = static_cast<RustType *>(type)->AsTypedef();
    if (t)
      return t->UnderlyingType();
  }
  return CompilerType();
}

//----------------------------------------------------------------------
// Create related types using the current type's AST
//----------------------------------------------------------------------
CompilerType RustASTContext::GetBasicTypeFromAST(lldb::BasicType basic_type) {
  return CompilerType();
}

CompilerType
RustASTContext::GetBuiltinTypeForEncodingAndBitSize(lldb::Encoding encoding,
                                                  size_t bit_size) {
  return CompilerType();
}

//----------------------------------------------------------------------
// Exploring the type
//----------------------------------------------------------------------

uint64_t RustASTContext::GetBitSize(lldb::opaque_compiler_type_t type,
				    ExecutionContextScope *exe_scope) {
  if (!type)
    return 0;
  RustType *t = static_cast<RustType *>(type);
  return t->ByteSize() * 8;
}

lldb::Encoding RustASTContext::GetEncoding(lldb::opaque_compiler_type_t type,
					   uint64_t &count) {
  count = 1;
  bool is_signed;
  if (IsIntegerType(type, is_signed))
    return is_signed ? lldb::eEncodingSint : eEncodingUint;
  bool is_complex;
  uint32_t complex_count;
  if (IsFloatingPointType(type, complex_count, is_complex)) {
    count = complex_count;
    return eEncodingIEEE754;
  }
  if (IsPointerType(type))
    return eEncodingUint;
  return eEncodingInvalid;
}

lldb::Format RustASTContext::GetFormat(lldb::opaque_compiler_type_t type) {
  if (!type)
    return eFormatDefault;
  return static_cast<RustType *>(type)->Format();
}

size_t RustASTContext::GetTypeBitAlign(lldb::opaque_compiler_type_t type) {
  return 0;
}

uint32_t RustASTContext::GetNumChildren(lldb::opaque_compiler_type_t type,
					bool omit_empty_base_classes) {
  if (!type)
    return 0;

  RustType *t = static_cast<RustType *>(type);
  uint32_t result = 0;
  if (RustPointer *ptr = t->AsPointer()) {
    result = ptr->PointeeType().GetNumChildren(omit_empty_base_classes);
    // If the pointee is not an aggregate, return 1 because the
    // pointer has a child.  Not totally sure this makes sense.
    if (result == 0)
      result = 1;
  } else if (RustArray *array = t->AsArray()) {
    result = array->Length();
  } else if (RustTypedef *typ = t->AsTypedef()) {
    result = typ->UnderlyingType().GetNumChildren(omit_empty_base_classes);
  }

  return result;
}

uint32_t RustASTContext::GetNumFields(lldb::opaque_compiler_type_t type) {
  if (!type)
    return 0;
  RustType *t = static_cast<RustType *>(type);
  if (RustTypedef *tdef = t->AsTypedef())
    return tdef->UnderlyingType().GetNumFields();
  if (RustAggregateBase *a = t->AsAggregate())
    return a->FieldCount();
  // FIXME arrays?
  return 0;
}

CompilerType RustASTContext::GetFieldAtIndex(lldb::opaque_compiler_type_t type,
					     size_t idx, std::string &name,
					     uint64_t *bit_offset_ptr,
					     uint32_t *bitfield_bit_size_ptr,
					     bool *is_bitfield_ptr) {
  if (bit_offset_ptr)
    *bit_offset_ptr = 0;
  if (bitfield_bit_size_ptr)
    *bitfield_bit_size_ptr = 0;
  if (is_bitfield_ptr)
    *is_bitfield_ptr = false;

  if (!type || !GetCompleteType(type))
    return CompilerType();

  RustType *t = static_cast<RustType *>(type);
  if (RustTypedef *typ = t->AsTypedef())
    return typ->UnderlyingType().GetFieldAtIndex(
        idx, name, bit_offset_ptr, bitfield_bit_size_ptr, is_bitfield_ptr);

  if (RustAggregateBase *s = t->AsAggregate()) {
    const auto *field = s->FieldAt(idx);
    if (field) {
      name = field->m_name.GetStringRef();
      if (bit_offset_ptr)
        *bit_offset_ptr = field->m_offset * 8;
      return field->m_type;
    }
  }
  return CompilerType();
}

CompilerType RustASTContext::GetChildCompilerTypeAtIndex(
    lldb::opaque_compiler_type_t type, ExecutionContext *exe_ctx, size_t idx,
    bool transparent_pointers, bool omit_empty_base_classes,
    bool ignore_array_bounds, std::string &child_name,
    uint32_t &child_byte_size, int32_t &child_byte_offset,
    uint32_t &child_bitfield_bit_size, uint32_t &child_bitfield_bit_offset,
    bool &child_is_base_class, bool &child_is_deref_of_parent,
    ValueObject *valobj, uint64_t &language_flags) {
  child_name.clear();
  child_byte_size = 0;
  child_byte_offset = 0;
  child_bitfield_bit_size = 0;
  child_bitfield_bit_offset = 0;
  child_is_base_class = false;
  child_is_deref_of_parent = false;
  language_flags = 0;

  if (!type || !GetCompleteType(type))
    return CompilerType();

  RustType *t = static_cast<RustType *>(type);
  if (t->AsAggregate()) {
    uint64_t bit_offset;
    CompilerType ret =
        GetFieldAtIndex(type, idx, child_name, &bit_offset, nullptr, nullptr);
    child_byte_size = ret.GetByteSize(
        exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr);
    child_byte_offset = bit_offset / 8;
    return ret;
  } else if (RustPointer *ptr = t->AsPointer()) {
    CompilerType pointee = ptr->PointeeType();
    if (!pointee.IsValid() || pointee.IsVoidType())
      return CompilerType();
    if (transparent_pointers && pointee.IsAggregateType()) {
      bool tmp_child_is_deref_of_parent = false;
      return pointee.GetChildCompilerTypeAtIndex(
          exe_ctx, idx, transparent_pointers, omit_empty_base_classes,
          ignore_array_bounds, child_name, child_byte_size, child_byte_offset,
          child_bitfield_bit_size, child_bitfield_bit_offset,
          child_is_base_class, tmp_child_is_deref_of_parent, valobj,
          language_flags);
    } else {
      child_is_deref_of_parent = true;
      const char *parent_name = valobj ? valobj->GetName().GetCString() : NULL;
      if (parent_name) {
        child_name.assign(1, '*');
        child_name += parent_name;
      }

      // We have a pointer to an simple type
      if (idx == 0 && pointee.GetCompleteType()) {
        child_byte_size = pointee.GetByteSize(
            exe_ctx ? exe_ctx->GetBestExecutionContextScope() : NULL);
        child_byte_offset = 0;
        return pointee;
      }
    }
  } else if (RustArray *a = t->AsArray()) {
    if (ignore_array_bounds || idx < a->Length()) {
      CompilerType element_type = a->ElementType();
      if (element_type.GetCompleteType()) {
        char element_name[64];
        ::snprintf(element_name, sizeof(element_name), "[%zu]", idx);
        child_name.assign(element_name);
        child_byte_size = element_type.GetByteSize(
            exe_ctx ? exe_ctx->GetBestExecutionContextScope() : NULL);
        child_byte_offset = (int32_t)idx * (int32_t)child_byte_size;
        return element_type;
      }
    }
  } else if (RustTypedef *typ = t->AsTypedef()) {
    return typ->UnderlyingType().GetChildCompilerTypeAtIndex(
        exe_ctx, idx, transparent_pointers, omit_empty_base_classes,
        ignore_array_bounds, child_name, child_byte_size, child_byte_offset,
        child_bitfield_bit_size, child_bitfield_bit_offset, child_is_base_class,
        child_is_deref_of_parent, valobj, language_flags);
  }
  return CompilerType();
}

// Lookup a child given a name. This function will match base class names
// and member member names in "clang_type" only, not descendants.
uint32_t
RustASTContext::GetIndexOfChildWithName(lldb::opaque_compiler_type_t type,
					const char *name,
					bool omit_empty_base_classes) {
  if (!type || !GetCompleteType(type))
    return UINT_MAX;

  RustType *t = static_cast<RustType *>(type);
  if (RustAggregateBase *agg = t->AsAggregate()) {
    for (uint32_t i = 0; i < agg->FieldCount(); ++i) {
      const RustAggregateBase::Field *f = agg->FieldAt(i);
      if (f->m_name.GetStringRef() == name)
        return i;
    }
  } else if (RustPointer *typ = t->AsPointer()) {
    return typ->PointeeType().GetIndexOfChildWithName(name, omit_empty_base_classes);
  }
  return UINT_MAX;
}

// Lookup a child member given a name. This function will match member names
// only and will descend into "clang_type" children in search for the first
// member in this class, or any base class that matches "name".
// TODO: Return all matches for a given name by returning a
// vector<vector<uint32_t>>
// so we catch all names that match a given child name, not just the first.
size_t RustASTContext::GetIndexOfChildMemberWithName(
    lldb::opaque_compiler_type_t type, const char *name,
    bool omit_empty_base_classes, std::vector<uint32_t> &child_indexes) {
  uint32_t index = GetIndexOfChildWithName(type, name, omit_empty_base_classes);
  if (index == UINT_MAX)
    return 0;
  child_indexes.push_back(index);
  return 1;
}

// Converts "s" to a floating point value and place resulting floating
// point bytes in the "dst" buffer.
size_t
RustASTContext::ConvertStringToFloatValue(lldb::opaque_compiler_type_t type,
                                        const char *s, uint8_t *dst,
                                        size_t dst_size) {
  assert(false);
  return 0;
}

//----------------------------------------------------------------------
// Dumping types
//----------------------------------------------------------------------
#define DEPTH_INCREMENT 2

void RustASTContext::DumpValue(lldb::opaque_compiler_type_t type,
                             ExecutionContext *exe_ctx, Stream *s,
                             lldb::Format format, const DataExtractor &data,
                             lldb::offset_t data_byte_offset,
                             size_t data_byte_size, uint32_t bitfield_bit_size,
                             uint32_t bitfield_bit_offset, bool show_types,
                             bool show_summary, bool verbose, uint32_t depth) {
  if (IsTypedefType(type))
    type = GetTypedefedType(type).GetOpaqueQualType();
  if (!type)
    return;
  RustType *t = static_cast<RustType *>(type);

  if (RustAggregateBase *agg = t->AsAggregate()) {
    if (GetCompleteType(type)) {
      uint32_t field_idx = 0;
      for (auto *field = agg->FieldAt(field_idx); field != nullptr; field_idx++) {
        // Print the starting squiggly bracket (if this is the
        // first member) or comma (for member 2 and beyond) for
        // the struct/union/class member.
        if (field_idx == 0)
          s->PutChar('{');
        else
          s->PutChar(',');

        // Indent
        s->Printf("\n%*s", depth + DEPTH_INCREMENT, "");

        // Print the member type if requested
        if (show_types) {
          ConstString field_type_name = field->m_type.GetTypeName();
          s->Printf("(%s) ", field_type_name.AsCString());
        }
        // Print the member name and equal sign
        s->Printf("%s = ", field->m_name.AsCString());

        // Dump the value of the member
        CompilerType field_type = field->m_type;
        field_type.DumpValue(
            exe_ctx,
            s, // Stream to dump to
            field_type.GetFormat(), // The format with which to display the member
            data,             // Data buffer containing all bytes for this type
            data_byte_offset + field->m_offset, // Offset
            field->m_type.GetByteSize(
                exe_ctx->GetBestExecutionContextScope()), // Size of this type
                                                          // in bytes
            0,                                            // Bitfield bit size
            0,                                            // Bitfield bit offset
            show_types,   // Boolean indicating if we should show the variable
                          // types
            show_summary, // Boolean indicating if we should show a summary for
                          // the current type
            verbose,      // Verbose output?
            depth + DEPTH_INCREMENT); // Scope depth for any types that have
                                      // children
      }

      // Indent the trailing squiggly bracket
      if (field_idx > 0)
        s->Printf("\n%*s}", depth, "");
    }
  }

  if (RustArray *a = t->AsArray()) {
    CompilerType element_clang_type = a->ElementType();
    lldb::Format element_format = element_clang_type.GetFormat();
    uint32_t element_byte_size =
        element_clang_type.GetByteSize(exe_ctx->GetBestExecutionContextScope());

    uint64_t element_idx;
    for (element_idx = 0; element_idx < a->Length(); ++element_idx) {
      // Print the starting squiggly bracket (if this is the
      // first member) or comman (for member 2 and beyong) for
      // the struct/union/class member.
      if (element_idx == 0)
        s->PutChar('{');
      else
        s->PutChar(',');

      // Indent and print the index
      s->Printf("\n%*s[%" PRIu64 "] ", depth + DEPTH_INCREMENT, "",
                element_idx);

      // Figure out the field offset within the current struct/union/class type
      uint64_t element_offset = element_idx * element_byte_size;

      // Dump the value of the member
      element_clang_type.DumpValue(
          exe_ctx,
          s,              // Stream to dump to
          element_format, // The format with which to display the element
          data,           // Data buffer containing all bytes for this type
          data_byte_offset +
              element_offset, // Offset into "data" where to grab value from
          element_byte_size,  // Size of this type in bytes
          0,                  // Bitfield bit size
          0,                  // Bitfield bit offset
          show_types, // Boolean indicating if we should show the variable types
          show_summary, // Boolean indicating if we should show a summary for
                        // the current type
          verbose,      // Verbose output?
          depth +
              DEPTH_INCREMENT); // Scope depth for any types that have children
    }

    // Indent the trailing squiggly bracket
    if (element_idx > 0)
      s->Printf("\n%*s}", depth, "");
  }

  if (show_summary)
    DumpSummary(type, exe_ctx, s, data, data_byte_offset, data_byte_size);
}

bool RustASTContext::DumpTypeValue(lldb::opaque_compiler_type_t type, Stream *s,
				   lldb::Format format, const DataExtractor &data,
				   lldb::offset_t byte_offset, size_t byte_size,
				   uint32_t bitfield_bit_size,
				   uint32_t bitfield_bit_offset,
				   ExecutionContextScope *exe_scope) {
  if (!type)
    return false;
  if (IsAggregateType(type)) {
    return false;
  } else {
    RustType *t = static_cast<RustType *>(type);
    if (RustTypedef *typ = t->AsTypedef()) {
      CompilerType typedef_compiler_type = typ->UnderlyingType();
      if (format == eFormatDefault)
        format = typedef_compiler_type.GetFormat();
      uint64_t typedef_byte_size = typedef_compiler_type.GetByteSize(exe_scope);

      return typedef_compiler_type.DumpTypeValue(
          s,
          format,            // The format with which to display the element
          data,              // Data buffer containing all bytes for this type
          byte_offset,       // Offset into "data" where to grab value from
          typedef_byte_size, // Size of this type in bytes
          bitfield_bit_size, // Size in bits of a bitfield value, if zero don't
                             // treat as a bitfield
          bitfield_bit_offset, // Offset in bits of a bitfield value if
                               // bitfield_bit_size != 0
          exe_scope);
    }

    uint32_t item_count = 1;
    // A few formats, we might need to modify our size and count for depending
    // on how we are trying to display the value...
    switch (format) {
    default:
    case eFormatBoolean:
    case eFormatBinary:
    case eFormatComplex:
    case eFormatCString: // NULL terminated C strings
    case eFormatDecimal:
    case eFormatEnum:
    case eFormatHex:
    case eFormatHexUppercase:
    case eFormatFloat:
    case eFormatOctal:
    case eFormatOSType:
    case eFormatUnsigned:
    case eFormatPointer:
    case eFormatVectorOfChar:
    case eFormatVectorOfSInt8:
    case eFormatVectorOfUInt8:
    case eFormatVectorOfSInt16:
    case eFormatVectorOfUInt16:
    case eFormatVectorOfSInt32:
    case eFormatVectorOfUInt32:
    case eFormatVectorOfSInt64:
    case eFormatVectorOfUInt64:
    case eFormatVectorOfFloat32:
    case eFormatVectorOfFloat64:
    case eFormatVectorOfUInt128:
      break;

    case eFormatChar:
    case eFormatCharPrintable:
    case eFormatCharArray:
    case eFormatBytes:
    case eFormatBytesWithASCII:
      item_count = byte_size;
      byte_size = 1;
      break;

    case eFormatUnicode16:
      item_count = byte_size / 2;
      byte_size = 2;
      break;

    case eFormatUnicode32:
      item_count = byte_size / 4;
      byte_size = 4;
      break;
    }
    return DumpDataExtractor(data, s, byte_offset, format, byte_size, item_count,
			     UINT32_MAX, LLDB_INVALID_ADDRESS, bitfield_bit_size,
			     bitfield_bit_offset, exe_scope);
  }
  return 0;
}

void RustASTContext::DumpSummary(lldb::opaque_compiler_type_t type,
				 ExecutionContext *exe_ctx, Stream *s,
				 const DataExtractor &data,
				 lldb::offset_t data_offset,
				 size_t data_byte_size) {
  // FIXME
}

void RustASTContext::DumpTypeDescription(lldb::opaque_compiler_type_t type) {
  // Dump to stdout
  StreamFile s(stdout, false);
  DumpTypeDescription(type, &s);
}

void RustASTContext::DumpTypeDescription(lldb::opaque_compiler_type_t type, Stream *s) {
  if (!type)
    return;
  ConstString name = GetTypeName(type);
  RustType *t = static_cast<RustType *>(type);

  if (RustAggregateBase *agg = t->AsStruct()) {
    if (NULL == strchr(name.AsCString(), '{'))
      s->Printf("type %s ", name.AsCString());
    s->PutCString("struct {");
    if (agg->FieldCount() == 0) {
      s->PutChar('}');
      return;
    }
    s->IndentMore();
    uint32_t field_idx = 0;
    for (auto *field = agg->FieldAt(field_idx); field != nullptr; field_idx++) {
      s->PutChar('\n');
      s->Indent();
      s->Printf("%s %s", field->m_name.AsCString(),
		field->m_type.GetTypeName().AsCString());
    }
    s->IndentLess();
    s->PutChar('\n');
    s->Indent("}");
    return;
  }

  s->PutCString(name.AsCString());
}

RustType *RustASTContext::FindCachedType(const lldb_private::ConstString &name) {
  auto result = m_types.find(name);
  if (result == m_types.end ())
    return nullptr;
  return result->second.get();
}

CompilerType RustASTContext::CreateBoolType(const lldb_private::ConstString &name) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustBool(name);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType RustASTContext::CreateIntegralType(const lldb_private::ConstString &name,
						bool is_signed,
						uint64_t byte_size) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustIntegral(name, is_signed, byte_size);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType RustASTContext::CreateFloatType(const lldb_private::ConstString &name,
					     uint64_t byte_size) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustFloat(name, byte_size);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType RustASTContext::CreateArrayType(const ConstString &name,
					     const CompilerType &element_type,
					     uint64_t length) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustArray(name, length, element_type);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType RustASTContext::CreateTypedefType(const ConstString &name, CompilerType impl) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustTypedef(name, impl);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType
RustASTContext::CreateStructType(const lldb_private::ConstString &name, uint32_t byte_size) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustStruct(name, byte_size);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

void RustASTContext::AddFieldToStruct(const CompilerType &struct_type,
				      const lldb_private::ConstString &name,
				      const CompilerType &field_type,
				      uint32_t byte_offset) {
  if (!struct_type)
    return;
  RustASTContext *ast =
      llvm::dyn_cast_or_null<RustASTContext>(struct_type.GetTypeSystem());
  if (!ast)
    return;
  RustType *type = static_cast<RustType *>(struct_type.GetOpaqueQualType());
  if (RustStruct *s = type->AsStruct())
    s->AddField(name, field_type, byte_offset);
}

CompilerType
RustASTContext::CreateFunctionType(const lldb_private::ConstString &name,
				   const CompilerType &return_type,
				   const std::vector<CompilerType> &&params) {
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustFunction(name, return_type, std::move(params));
  m_types[name].reset(type);
  return CompilerType(this, type);
}

CompilerType
RustASTContext::CreateVoidType() {
  ConstString name("()");
  if (RustType *cached = FindCachedType(name))
    return CompilerType(this, cached);
  RustType *type = new RustTuple(name, 0);
  m_types[name].reset(type);
  return CompilerType(this, type);
}

DWARFASTParser *RustASTContext::GetDWARFParser() {
  if (!m_dwarf_ast_parser_ap)
    m_dwarf_ast_parser_ap.reset(new DWARFASTParserRust(*this));
  return m_dwarf_ast_parser_ap.get();
}

UserExpression *RustASTContextForExpr::GetUserExpression(
    llvm::StringRef expr, llvm::StringRef prefix, lldb::LanguageType language,
    Expression::ResultType desired_type,
    const EvaluateExpressionOptions &options) {
  // TargetSP target = m_target_wp.lock();
  // if (target)
  //   return new RustUserExpression(*target, expr, prefix, language, desired_type,
  //                               options);
  return nullptr;
}
