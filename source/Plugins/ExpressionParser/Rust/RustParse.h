//===-- RustParse.h -------------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustParse_h
#define liblldb_RustParse_h

#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"
#include "RustLex.h"
#include "RustAST.h"

namespace lldb_private {

namespace rust {

class Parser {
public:

  explicit Parser(llvm::StringRef ref)
    : m_lexer(ref),
      // fixme this seems not good
      m_current(m_lexer.Next())
  {
  }

private:

  template<RustUnaryOperator OP> RustExpressionUP Unary(Status &error);
  bool ExprList(std::vector<RustExpressionUP> *exprs, Status &error);
  RustExpressionUP Parens(Status &error);
  RustExpressionUP Path(Status &error);
  RustExpressionUP Array(Status &error);
  RustExpressionUP Field(RustExpressionUP &&lhs, Status &error);
  RustExpressionUP Call(RustExpressionUP &&func, Status &error);
  RustExpressionUP Index(RustExpressionUP &&array, Status &error);
  RustExpressionUP Term(Status &error);
  RustExpressionUP Binary(Status &error);
  CompilerType Type(Status &error);

  Token &CurrentToken() { return m_current; }
  void Advance() { m_current = m_lexer.Next(); }

  RustExpressionUP Unimplemented(Status &error);

  Lexer m_lexer;
  Token m_current;
};

} // namespace rust
} // namespace lldb_private

#endif // liblldb_RustParse_h
