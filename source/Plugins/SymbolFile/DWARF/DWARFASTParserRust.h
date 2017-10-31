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
#include "lldb/Core/PluginInterface.h"
#include "lldb/Symbol/RustASTContext.h"

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
  lldb::TypeSP ParseSimpleType(const DWARFDIE &die);
  lldb::TypeSP ParseArrayType(const DWARFDIE &die);
  lldb::TypeSP ParseFunctionType(const DWARFDIE &die);
  lldb::TypeSP ParseStructureType(const DWARFDIE &die);

  lldb_private::RustASTContext &m_ast;
};

#endif // SymbolFileDWARF_DWARFASTParserRust_h_
