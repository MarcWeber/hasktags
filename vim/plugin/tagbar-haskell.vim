" This is a Haskell configuration for the Vim Tagbar plugin that uses
" hasktags.

if executable('hasktags')
  let g:tagbar_type_haskell = {
      \ 'ctagsbin'  : 'hasktags',
      \ 'ctagsargs' : '-x -c --ignore-close-implementation -o-',
      \ 'kinds'     : [
          \  'm:modules:0:1',
          \  'd:data: 0:1',
          \  'd_gadt: data GADT:0:1',
          \  't:type names:0:1',
          \  'nt:newtypes:0:1',
          \  'c:classes:0:1',
          \  'cons:constructors:1:1',
          \  'c_gadt:constructors (GADT):1:1',
          \  'c_a:accessors:1:1',
          \  'ft:function types:0:1',
          \  'fi:function implementations:1:1',
          \  'o:others:0:1'
      \ ],
      \ 'sro'        : '.',
      \ 'kind2scope' : {
          \ 'm' : 'module',
          \ 'c' : 'class',
          \ 'd' : 'data',
          \ 'd_gadt' : 'GADT',
          \ 'nt' : 'newtype',
          \ 't' : 'type'
      \ },
      \ }
endif
