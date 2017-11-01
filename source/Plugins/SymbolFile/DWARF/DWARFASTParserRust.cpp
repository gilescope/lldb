//===-- DWARFASTParserRust.cpp ---------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "DWARFASTParserRust.h"

#include "DWARFASTParserRust.h"
#include "DWARFCompileUnit.h"
#include "DWARFDIE.h"
#include "DWARFDIECollection.h"
#include "DWARFDebugInfo.h"
#include "DWARFDeclContext.h"
#include "DWARFDefines.h"
#include "SymbolFileDWARF.h"
#include "SymbolFileDWARFDebugMap.h"
#include "UniqueDWARFASTType.h"

#include "clang/Basic/Specifiers.h"

#include "lldb/Core/Module.h"
#include "lldb/Core/Value.h"
#include "lldb/Symbol/CompileUnit.h"
#include "lldb/Symbol/Function.h"
#include "lldb/Symbol/ObjectFile.h"
#include "lldb/Symbol/TypeList.h"

using namespace lldb;
using namespace lldb_private;

#define INVALID_ATTR dw_attr_t(-1)

// A way to iterate over DIE attrs.
class IterableDIEAttrs
{
public:
  IterableDIEAttrs(const DWARFDIE &die)
  {
    m_size = die.GetAttributes(m_attrs);
  }

  IterableDIEAttrs(const IterableDIEAttrs &) = delete;
  IterableDIEAttrs &operator=(const IterableDIEAttrs &) = delete;

  class iterator
  {
  public:

    iterator(const IterableDIEAttrs *die, size_t offset)
      : m_die(die)
      , m_offset(offset)
    {
    }

    iterator(const iterator &other)
      : m_die(other.m_die)
      , m_offset(other.m_offset)
    {
    }

    iterator &operator=(const iterator &other)
    {
      m_die = other.m_die;
      m_offset = other.m_offset;
      return *this;
    }

    iterator &operator++()
    {
      ++m_offset;
      return *this;
    }

    bool operator!=(const iterator &other) const
    {
      return m_die != other.m_die || m_offset != other.m_offset;
    }

    std::pair<dw_attr_t, DWARFFormValue> operator*() const
    {
      dw_attr_t attr = m_die->m_attrs.AttributeAtIndex(m_offset);
      DWARFFormValue value;
      if (!m_die->m_attrs.ExtractFormValueAtIndex(m_offset, value))
	attr = INVALID_ATTR;
      return std::make_pair(attr, value);
    }

  private:

    const IterableDIEAttrs *m_die;
    size_t m_offset;
  };

  iterator begin() const
  {
    return iterator(this, 0);
  }

  iterator end() const
  {
    return iterator(this, m_size);
  }

private:

  size_t m_size;
  DWARFAttributes m_attrs;
};

// A way to iterate over a DIE's direct children.
class IterableDIEChildren
{
public:
  IterableDIEChildren(const DWARFDIE &die)
    : m_die(die)
  {
  }

  IterableDIEChildren(const IterableDIEChildren &) = delete;
  IterableDIEChildren &operator=(const IterableDIEChildren &) = delete;

  class iterator
  {
  public:

    iterator(const DWARFDIE &die)
      : m_die(die)
    {
    }

    ~iterator()
    {
    }

    iterator(const iterator &other)
      : m_die(other.m_die)
    {
    }

    iterator &operator=(const iterator &other)
    {
      m_die = other.m_die;
      return *this;
    }

    iterator &operator++()
    {
      m_die = m_die.GetSibling();
      return *this;
    }

    bool operator!=(const iterator &other) const
    {
      return m_die != other.m_die;
    }

    DWARFDIE operator*() const
    {
      return m_die;
    }

  private:

    DWARFDIE m_die;
  };

  iterator begin() const
  {
    return iterator(m_die.GetFirstChild());
  }

  iterator end() const
  {
    return iterator(DWARFDIE());
  }

private:

  DWARFDIE m_die;
};

TypeSP DWARFASTParserRust::ParseSimpleType(const DWARFDIE &die) {
  lldb::user_id_t encoding_uid = LLDB_INVALID_UID;
  const char *type_name_cstr = NULL;
  ConstString type_name_const_str;
  uint64_t byte_size = 0;
  uint64_t encoding = 0;

  for (auto &&value : IterableDIEAttrs(die)) {
    switch (value.first) {
    case DW_AT_name:
      type_name_cstr = value.second.AsCString();
      if (type_name_cstr)
	type_name_const_str.SetCString(type_name_cstr);
      break;
    case DW_AT_byte_size:
      byte_size = value.second.Unsigned();
      break;
    case DW_AT_encoding:
      encoding = value.second.Unsigned();
      break;
    case DW_AT_type:
      encoding_uid = value.second.Reference();
      break;
    }
  }

  SymbolFileDWARF *dwarf = die.GetDWARF();
  Type::ResolveState resolve_state = Type::eResolveStateUnresolved;
  CompilerType compiler_type;
  Type::EncodingDataType encoding_data_type = Type::eEncodingIsUID;
  switch (die.Tag()) {
  case DW_TAG_unspecified_type:
    resolve_state = Type::eResolveStateFull;
    compiler_type = m_ast.CreateVoidType();
    break;

  case DW_TAG_base_type:
    resolve_state = Type::eResolveStateFull;
    if (encoding == DW_ATE_boolean)
      compiler_type = m_ast.CreateBoolType(type_name_const_str);
    else if (encoding == DW_ATE_float)
      compiler_type = m_ast.CreateFloatType(type_name_const_str, byte_size);
    else if (encoding == DW_ATE_signed || encoding == DW_ATE_unsigned)
      compiler_type = m_ast.CreateIntegralType(type_name_const_str,
					       encoding == DW_ATE_signed,
					       byte_size);
    break;

    // Note that, currently, rustc does not emit DW_TAG_reference_type
    // - references are distinguished by name.
  case DW_TAG_pointer_type:
    m_ast.SetAddressByteSize(die.GetCU()->GetAddressByteSize());
    encoding_data_type = Type::eEncodingIsPointerUID;
    break;

  case DW_TAG_typedef:
  case DW_TAG_template_type_parameter: {
    encoding_data_type = Type::eEncodingIsTypedefUID;
    CompilerType impl;
    Type *type = dwarf->ResolveTypeUID(encoding_uid);
    if (type) {
      impl = type->GetForwardCompilerType();
      compiler_type = m_ast.CreateTypedefType(type_name_const_str, impl);
    }
    break;
  }

  default:
    // Should have been filtered by the caller.
    assert(0);
  }

  return TypeSP(new Type(die.GetID(), dwarf, type_name_const_str,
			 byte_size, NULL, encoding_uid,
			 encoding_data_type, Declaration(), compiler_type,
			 resolve_state));
}

TypeSP DWARFASTParserRust::ParseArrayType(const DWARFDIE &die) {
  const char *type_name_cstr = NULL;
  ConstString type_name_const_str;

  lldb::user_id_t type_die_offset = DW_INVALID_OFFSET;

  for (auto &&value : IterableDIEAttrs(die)) {
    switch (value.first) {
    case DW_AT_name:
      type_name_cstr = value.second.AsCString();
      type_name_const_str.SetCString(type_name_cstr);
      break;
    case DW_AT_type:
      type_die_offset = value.second.Reference();
      break;
    }
  }

  SymbolFileDWARF *dwarf = die.GetDWARF();
  Type *element_type = dwarf->ResolveTypeUID(type_die_offset);
  if (!element_type)
    return TypeSP(nullptr);

  CompilerType compiler_type;
  uint64_t count = 0;

  for (auto &&child_die : IterableDIEChildren(die)) {
    if (child_die.Tag() == DW_TAG_subrange_type) {
      for (auto &&value : IterableDIEAttrs(child_die)) {
	if (value.first == DW_AT_count) {
	  count = value.second.Unsigned();
	  break;
	}
      }
      break;
    }
  }

  CompilerType array_element_type = element_type->GetForwardCompilerType();
  compiler_type = m_ast.CreateArrayType(type_name_const_str, array_element_type, count);

  TypeSP type_sp(new Type(die.GetID(), dwarf, type_name_const_str,
			  element_type->GetByteSize(), NULL, type_die_offset,
			  Type::eEncodingIsUID, Declaration(), compiler_type,
			  Type::eResolveStateFull));
  type_sp->SetEncodingType(element_type);
  return type_sp;
}

TypeSP DWARFASTParserRust::ParseFunctionType(const DWARFDIE &die) {
  clang::StorageClass storage = clang::SC_None; //, Extern, Static, PrivateExtern
  const char *type_name_cstr = NULL;
  ConstString type_name_const_str;
  Declaration decl;

  CompilerType return_type;
  for (auto &&attr : IterableDIEAttrs(die)) {
    switch (attr.first) {
    case DW_AT_name:
      type_name_cstr = attr.second.AsCString();
      type_name_const_str.SetCString(type_name_cstr);
      break;

    case DW_AT_external:
      if (attr.second.Unsigned()) {
	if (storage == clang::SC_None)
	  storage = clang::SC_Extern;
	else
	  storage = clang::SC_PrivateExtern;
      }
      break;

    case DW_AT_type: {
      Type *type = die.ResolveTypeUID(DIERef(attr.second));
      if (type) {
	return_type = type->GetForwardCompilerType();
      }
      break;
    }
    }
  }

  std::vector<CompilerType> function_param_types;
  for (auto &&child_die : IterableDIEChildren(die)) {
    if (child_die.Tag() == DW_TAG_formal_parameter) {
      for (auto &&attr : IterableDIEAttrs(child_die)) {
	if (attr.first == DW_AT_type) {
	  Type *type = die.ResolveTypeUID(DIERef(attr.second));
	  if (type) {
	    function_param_types.push_back(type->GetForwardCompilerType());
	  }
	  break;
	}
      }
    }
  }

  CompilerType compiler_type = m_ast.CreateFunctionType(type_name_const_str, return_type,
							std::move(function_param_types));

  SymbolFileDWARF *dwarf = die.GetDWARF();
  TypeSP type_sp(new Type(die.GetID(), dwarf, type_name_const_str, 0, NULL,
			  LLDB_INVALID_UID, Type::eEncodingIsUID, &decl,
			  compiler_type, Type::eResolveStateFull));

  return type_sp;
}

struct Field {
  Field() : name(nullptr), byte_offset(-1)
  {
  }

  const char *name;
  DWARFFormValue type;
  uint32_t byte_offset;
};

TypeSP DWARFASTParserRust::ParseStructureType(const DWARFDIE &die) {
  bool byte_size_valid = false;
  uint64_t byte_size = 0;
  const char *type_name_cstr = NULL;
  ConstString type_name_const_str;
  SymbolFileDWARF *dwarf = die.GetDWARF();
  Declaration decl;

  for (auto &&attr : IterableDIEAttrs(die)) {
    switch (attr.first) {
    case DW_AT_name:
      type_name_cstr = attr.second.AsCString();
      type_name_const_str.SetCString(type_name_cstr);
      break;

    case DW_AT_byte_size:
      byte_size = attr.second.Unsigned();
      byte_size_valid = true;
      break;
    }
  }

  UniqueDWARFASTType ast_entry;
  TypeSP type_sp;

  // Only try and unique the type if it has a name.
  if (type_name_const_str &&
      dwarf->GetUniqueDWARFASTTypeMap().Find(type_name_const_str, die, &decl,
					     byte_size_valid ? byte_size : -1, ast_entry)) {
    // We have already parsed this type.
    type_sp = ast_entry.m_type_sp;
    if (type_sp) {
      dwarf->m_die_to_type[die.GetDIE()] = type_sp.get();
      return type_sp;
    }
  }

  // We construct a list of fields and then apply them later so that
  // we can analyze the fields to see what sort of structure this
  // really is.
  std::vector<Field> fields;
  // Currently, rustc emits tuples with a name starting with "("; but
  // there's no way to distinguish a zero-length struct from a
  // zero-length tuple struct.
  bool is_tuple = type_name_cstr && type_name_cstr[0] == '(';
  bool numeric_names = true;
  unsigned offset = 0;

  ModuleSP module_sp = die.GetDWARF()->GetObjectFile()->GetModule();
  for (auto &&child_die : IterableDIEChildren(die)) {
    if (child_die.Tag() == DW_TAG_member) {
      Field new_field;

      for (auto &&attr : IterableDIEAttrs(child_die)) {
	switch (attr.first) {
	case DW_AT_name:
	  new_field.name = attr.second.AsCString();
	  break;
	case DW_AT_type:
	  new_field.type = attr.second;
	  break;
	case DW_AT_data_member_location:
	  if (attr.second.BlockData()) {
	    Value initialValue(0);
	    Value memberOffset(0);
	    const DWARFDataExtractor &debug_info_data =
	      child_die.GetDWARF()->get_debug_info_data();
	    uint32_t block_length = attr.second.Unsigned();
	    uint32_t block_offset = attr.second.BlockData() - debug_info_data.GetDataStart();
	    if (DWARFExpression::Evaluate(
                    NULL, // ExecutionContext *
		    NULL, // RegisterContext *
		    module_sp, debug_info_data, die.GetCU(), block_offset,
		    block_length, eRegisterKindDWARF, &initialValue, NULL,
		    memberOffset, NULL)) {
	      new_field.byte_offset = memberOffset.ResolveValue(NULL).UInt();
	    }
	  } else {
	    new_field.byte_offset = attr.second.Unsigned();
	  }
	  break;
	}
      }

      if (numeric_names) {
	char buf[32];
	snprintf (buf, sizeof (buf), "__%d", offset);
	if (!new_field.name || strcmp(new_field.name, buf) != 0)
	  numeric_names = false;
      }

      fields.push_back(new_field);
    }
  }

  if (!numeric_names) {
    // If the field name checking failed, maybe we don't have a tuple
    // after all, somehow.
    is_tuple = false;
  } else if (!is_tuple) {
    // If we saw numeric names in sequence, we may have a tuple
    // struct; but if there were no fields, then we can't tell and so
    // we arbitrarily choose an empty struct.
    is_tuple = !fields.empty();
  }

  bool compiler_type_was_created = false;
  CompilerType compiler_type(&m_ast,
			     dwarf->m_forward_decl_die_to_clang_type.lookup(die.GetDIE()));
  if (!compiler_type) {
    compiler_type_was_created = true;
    if (is_tuple)
      compiler_type = m_ast.CreateTupleType(type_name_const_str, byte_size);
    else
      compiler_type = m_ast.CreateStructType(type_name_const_str, byte_size);
  }

  type_sp.reset(new Type(die.GetID(), dwarf, type_name_const_str,
			 byte_size, NULL, LLDB_INVALID_UID,
			 Type::eEncodingIsUID, &decl, compiler_type,
			 Type::eResolveStateForward));


  for (auto &&field : fields) {
    Type *type = die.ResolveTypeUID(DIERef(field.type));
    if (type) {
      ConstString name(field.name);
      CompilerType member_type = type->GetFullCompilerType();
      m_ast.AddFieldToStruct(compiler_type, name, member_type, field.byte_offset);
    }
  }

  // Add our type to the unique type map so we don't
  // end up creating many copies of the same type over
  // and over in the ASTContext for our module
  ast_entry.m_type_sp = type_sp;
  ast_entry.m_die = die;
  ast_entry.m_declaration = decl;
  ast_entry.m_byte_size = byte_size;
  dwarf->GetUniqueDWARFASTTypeMap().Insert(type_name_const_str, ast_entry);

  if (compiler_type_was_created) {
    // Leave this as a forward declaration until we need
    // to know the details of the type. lldb_private::Type
    // will automatically call the SymbolFile virtual function
    // "SymbolFileDWARF::CompleteType(Type *)"
    // When the definition needs to be defined.
    dwarf->m_forward_decl_die_to_clang_type[die.GetDIE()] =
      compiler_type.GetOpaqueQualType();
    dwarf->m_forward_decl_clang_type_to_die[compiler_type.GetOpaqueQualType()] =
      die.GetDIERef();
  }

  return type_sp;
}

TypeSP DWARFASTParserRust::ParseTypeFromDWARF(
    const lldb_private::SymbolContext &sc, const DWARFDIE &die,
    lldb_private::Log *log, bool *type_is_new_ptr) {
  TypeSP type_sp;

  if (type_is_new_ptr)
    *type_is_new_ptr = false;

  if (die) {
    SymbolFileDWARF *dwarf = die.GetDWARF();
    if (log) {
      dwarf->GetObjectFile()->GetModule()->LogMessage(
          log, "DWARFASTParserRust::ParseTypeFromDWARF (die = 0x%8.8x) %s name = "
               "'%s')",
          die.GetOffset(), DW_TAG_value_to_name(die.Tag()), die.GetName());
    }

    Type *type_ptr = dwarf->m_die_to_type.lookup(die.GetDIE());
    TypeList *type_list = dwarf->GetTypeList();
    if (type_ptr == NULL) {
      if (type_is_new_ptr)
        *type_is_new_ptr = true;

      const dw_tag_t tag = die.Tag();

      // Set a bit that lets us know that we are currently parsing this
      dwarf->m_die_to_type[die.GetDIE()] = DIE_IS_BEING_PARSED;

      switch (tag) {
      case DW_TAG_base_type:
      case DW_TAG_pointer_type:
      case DW_TAG_typedef:
      case DW_TAG_template_type_parameter:
      case DW_TAG_unspecified_type:
	type_sp = ParseSimpleType(die);
	break;

	// case DW_TAG_union_type:  FIXME
      case DW_TAG_structure_type:
	type_sp = ParseStructureType(die);
	break;

      case DW_TAG_subprogram:
      case DW_TAG_subroutine_type:
	type_sp = ParseFunctionType(die);
	break;

      case DW_TAG_array_type:
	type_sp = ParseArrayType(die);
	break;

      // case DW_TAG_enumeration_type:
      // 	// FIXME
      // 	break;

      default:
        dwarf->GetObjectFile()->GetModule()->ReportError(
            "{0x%8.8x}: unhandled type tag 0x%4.4x (%s), "
            "please file a bug and attach the file at the "
            "start of this error message",
            die.GetOffset(), tag, DW_TAG_value_to_name(tag));
        break;
      }

      if (type_sp.get()) {
        DWARFDIE sc_parent_die =
            SymbolFileDWARF::GetParentSymbolContextDIE(die);
        dw_tag_t sc_parent_tag = sc_parent_die.Tag();

        SymbolContextScope *symbol_context_scope = NULL;
        if (sc_parent_tag == DW_TAG_compile_unit) {
          symbol_context_scope = sc.comp_unit;
        } else if (sc.function != NULL && sc_parent_die) {
          symbol_context_scope =
              sc.function->GetBlock(true).FindBlockByID(sc_parent_die.GetID());
          if (symbol_context_scope == NULL)
            symbol_context_scope = sc.function;
        }

        if (symbol_context_scope != NULL) {
          type_sp->SetSymbolContextScope(symbol_context_scope);
        }

        // We are ready to put this type into the uniqued list up at the module
        // level
        type_list->Insert(type_sp);
      }
      dwarf->m_die_to_type[die.GetDIE()] = type_sp.get();
    } else if (type_ptr != DIE_IS_BEING_PARSED) {
      type_sp = type_ptr->shared_from_this();
    }
  }
  return type_sp;
}

bool DWARFASTParserRust::CompleteTypeFromDWARF(const DWARFDIE &die,
					       lldb_private::Type *type,
					       CompilerType &compiler_type) {
  // We don't currently use type completion for Rust.
  return bool(die);
}

Function *DWARFASTParserRust::ParseFunctionFromDWARF(const SymbolContext &sc,
						     const DWARFDIE &die) {
  DWARFRangeList func_ranges;
  const char *name = NULL;
  const char *mangled = NULL;
  int decl_file = 0;
  int decl_line = 0;
  int decl_column = 0;
  int call_file = 0;
  int call_line = 0;
  int call_column = 0;
  DWARFExpression frame_base(die.GetCU());

  assert(die.Tag() == DW_TAG_subprogram);

  if (die.Tag() != DW_TAG_subprogram)
    return NULL;

  if (die.GetDIENamesAndRanges(name, mangled, func_ranges, decl_file, decl_line,
                               decl_column, call_file, call_line, call_column,
                               &frame_base)) {
    // Union of all ranges in the function DIE (if the function is
    // discontiguous)
    AddressRange func_range;
    lldb::addr_t lowest_func_addr = func_ranges.GetMinRangeBase(0);
    lldb::addr_t highest_func_addr = func_ranges.GetMaxRangeEnd(0);
    if (lowest_func_addr != LLDB_INVALID_ADDRESS &&
        lowest_func_addr <= highest_func_addr) {
      ModuleSP module_sp(die.GetModule());
      func_range.GetBaseAddress().ResolveAddressUsingFileSections(
          lowest_func_addr, module_sp->GetSectionList());
      if (func_range.GetBaseAddress().IsValid())
        func_range.SetByteSize(highest_func_addr - lowest_func_addr);
    }

    if (func_range.GetBaseAddress().IsValid()) {
      Mangled func_name;
      func_name.SetValue(ConstString(name), false);

      FunctionSP func_sp;
      std::unique_ptr<Declaration> decl_ap;
      if (decl_file != 0 || decl_line != 0 || decl_column != 0)
        decl_ap.reset(new Declaration(
            sc.comp_unit->GetSupportFiles().GetFileSpecAtIndex(decl_file),
            decl_line, decl_column));

      SymbolFileDWARF *dwarf = die.GetDWARF();
      // Supply the type _only_ if it has already been parsed
      Type *func_type = dwarf->m_die_to_type.lookup(die.GetDIE());

      assert(func_type == NULL || func_type != DIE_IS_BEING_PARSED);

      if (dwarf->FixupAddress(func_range.GetBaseAddress())) {
        const user_id_t func_user_id = die.GetID();
        func_sp.reset(new Function(sc.comp_unit,
                                   func_user_id, // UserID is the DIE offset
                                   func_user_id, func_name, func_type,
                                   func_range)); // first address range

        if (func_sp.get() != NULL) {
          if (frame_base.IsValid())
            func_sp->GetFrameBaseExpression() = frame_base;
          sc.comp_unit->AddFunction(func_sp);
          return func_sp.get();
        }
      }
    }
  }
  return NULL;
}
