" Vim syntax file

" Types.
" syn match   piType /\<I\d\+\>/
" syn match   piType /\<U\d\+\>/
" syn match   piType /\<F\d\+\>/

" Keywords.
syn keyword piKeyword new all any loop match
syn match piKeyword /->\|<-/
" syn match piSymbol /[.~?&∈,;—@★{}()\[\]:≡=<>+*/\\|-]/

" syn keyword piError TODO

" Misc syntax.
" syn match   piNoName /[%@!]\d\+\>/
syn match   piNumber /-\?\d\+\([ui]\d\+\)\?/
" syn match   piFloat  /-\?\<\d\+\.\d*\(e[+-]\d\+\)\?\>/
" syn match   piFloat  /\<0x\x\+\>/
" syn match   piBoolean /[A-Z][-a-zA-Z._0-9']*/
" syn keyword piConstant undef null none
" syn match   piComment /--.*$/
" syn region  piString start=/"/ skip=/\\"/ end=/"/
" syn match   piLabel /[-a-zA-Z$._][-a-zA-Z$._0-9']*:/
syn match   piIdentifier /[a-zA-Z_][a-zA-Z_0-9']*/

syn match   piType /[A-Z][a-zA-Z_0-9']*/

if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink piType Type
  HiLink piTypeSymbol Type
  HiLink piStatement Statement
  HiLink piNumber Number
  HiLink piComment Comment
  HiLink piString String
  HiLink piLabel Label
  HiLink piKeyword Keyword
  HiLink piExprKeyword Identifier
  HiLink piSymbol Statement
  " HiLink piBoolean Boolean
  " HiLink piFloat Float
  " HiLink piNoName Identifier
  " HiLink piConstant Constant
  " HiLink piSpecialComment SpecialComment
  " HiLink piError Error
  HiLink piIdentifier Normal

  delcommand HiLink
endif

let b:current_syntax = "pi"
