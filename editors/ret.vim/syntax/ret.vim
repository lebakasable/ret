" Vim syntax file
" Language: ret

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword retKeyword load
syn keyword retStrategy all first deep

syn match retFunction "\zs\([A-Za-z_][A-Za-z0-9_]*\)*\s*\ze\(<.*>\)\?("
syn match retRule "\zs\([A-Za-z_][A-Za-z0-9_]*\)*\s*\ze\s*::"

syn match retString "\"[^\"]*\"" display
syn match retNumber "\<[0-9][0-9_]*\(u8\)\?\>" display

syntax keyword retTodos TODO NOTE
syn region retComment start=";" end=/$/ contains=retTodos

" links
hi def link retKeyword  Keyword
hi def link retStrategy Keyword
hi def link retFunction Function
hi def link retRule 	  Function
hi def link retString   String
hi def link retNumber   Number
hi def link retComment  Comment

let b:current_syntax = "ret"
