//===-- DWARFASTParserRust.h --------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef SymbolFileDWARF_DWARFASTParserRust_h_
#define SymbolFileDWARF_DWARFASTParserRust_h_

// C Includes
// C++ Includes
// Other libraries and framework includes
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

// Project includes
#include "DWARFASTParser.h"
#include "DWARFDIE.h"
#include "DWARFDefines.h"
#include "DIERef.h"
#include "lldb/Core/PluginInterface.h"
#include "lldb/Symbol/RustASTContext.h"
#include "DWARFFormValue.h"

class DWARFDebugInfoEntry;
class DWARFDIECollection;

class DWARFASTParserRust : public DWARFASTParser {
public:
  DWARFASTParserRust(lldb_private::RustASTContext &ast)
    : m_ast(ast)
  {
  }

  lldb::TypeSP ParseTypeFromDWARF(const lldb_private::SymbolContext &sc,
                                  const DWARFDIE &die, lldb_private::Log *log,
                                  bool *type_is_new_ptr) override;

  lldb_private::Function *
  ParseFunctionFromDWARF(const lldb_private::SymbolContext &sc,
                         const DWARFDIE &die) override;

  bool CompleteTypeFromDWARF(const DWARFDIE &die, lldb_private::Type *type,
                             lldb_private::CompilerType &rust_type) override;

  lldb_private::CompilerDeclContext
  GetDeclContextForUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDeclContext
  GetDeclContextContainingUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDecl
  GetDeclForUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDecl();
  }

  std::vector<DWARFDIE> GetDIEForDeclContext(
      lldb_private::CompilerDeclContext decl_context) override {
    return std::vector<DWARFDIE>();
  }

private:
  lldb::TypeSP ParseSimpleType(lldb_private::Log *log, const DWARFDIE &die);
  lldb::TypeSP ParseArrayType(const DWARFDIE &die);
  lldb::TypeSP ParseFunctionType(const DWARFDIE &die);
  lldb::TypeSP ParseStructureType(const DWARFDIE &die);
  lldb::TypeSP ParseCLikeEnum(lldb_private::Log *log, const DWARFDIE &die);

  std::vector<size_t> ParseDiscriminantPath(const char **in_str);
  void FindDiscriminantLocation(lldb_private::CompilerType type,
				std::vector<size_t> &&path,
				uint64_t &offset, uint64_t &byte_size);

  lldb_private::RustASTContext &m_ast;

  struct Field {
    Field()
      : is_discriminant(false),
	is_elided(false),
	name(nullptr),
	byte_offset(-1)
    {
    }

    bool is_discriminant;
    // True if this field is the field that was elided by the non-zero
    // optimization.
    bool is_elided;
    const char *name;
    DWARFFormValue type;
    lldb_private::CompilerType compiler_type;
    uint32_t byte_offset;
  };

  std::vector<Field> ParseFields(const DWARFDIE &die,
				 std::vector<size_t> &discriminant_path,
				 bool &is_tuple);

  // The Rust compiler will emit a DW_TAG_enumeration_type for the
  // type of an enum discriminant.  However, this type will have the
  // same name as the enum type itself.  So, when we expect to read
  // the enumeration type, we set this member, and ParseCLikeEnum
  // avoids giving the name to the enumeration type.
  DIERef m_discriminant;
};

#endif // SymbolFileDWARF_DWARFASTParserRust_h_
