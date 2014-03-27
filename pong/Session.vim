let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
map! <S-Insert> <MiddleMouse>
nnoremap <silent>  :CtrlP
nnoremap   
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <Plug>FireplaceSource :Source 
nnoremap <S-Space> 
map <S-Insert> <MiddleMouse>
let &cpo=s:cpo_save
unlet s:cpo_save
set background=dark
set backspace=indent,eol,start
set clipboard=unnamedplus
set fileencodings=ucs-bom,utf-8,default,latin1
set guifont=Ubuntu\ Mono\ 13
set guioptions=aegimt
set helplang=en
set hidden
set history=50
set hlsearch
set iminsert=0
set incsearch
set laststatus=2
set lispwords=bound-fn,def,definline,definterface,defmacro,defmethod,defmulti,defn,defn-,defonce,defprotocol,defrecord,defstruct,deftest,deftest-,deftype,extend,extend-protocol,extend-type,fn,ns,proxy,reify,set-test,as->,binding,doall,dorun,doseq,dotimes,doto,for,if-let,let,letfn,locking,loop,testing,when-first,when-let,with-bindings,with-in-str,with-local-vars,with-open,with-precision,with-redefs,with-redefs-fn,with-test,case,cond->,cond->>,condp,if,if-not,when,when-not,while,catch
set nomodeline
set mouse=a
set operatorfunc=PareditDelete
set printoptions=paper:a4
set ruler
set runtimepath=~/.vim/bundle/vundle,~/.vim/bundle/vim-fireplace,~/.vim/bundle/vim-classpath,~/.vim/bundle/vim-clojure-static,~/.vim/bundle/rainbow_parentheses.vim,~/.vim/bundle/paredit.vim,~/.vim/bundle/vimerl,~/.vim/bundle/ctrlp.vim,~/.vim,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.vim/bundle/vundle/,~/.vim/bundle/vundle/after,~/.vim/bundle/vim-fireplace/after,~/.vim/bundle/vim-classpath/after,~/.vim/bundle/vim-clojure-static/after,~/.vim/bundle/rainbow_parentheses.vim/after,~/.vim/bundle/paredit.vim/after,~/.vim/bundle/vimerl/after,~/.vim/bundle/ctrlp.vim/after
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set termencoding=utf-8
set viminfo=!,'100,<50,s10,h
set window=40
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/clojure/pong
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 src/pong/core.clj
badd +7 project.clj
badd +1 test/pong/core_test.clj
silent! argdel *
edit test/pong/core_test.clj
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 1 + 75) / 151)
exe 'vert 2resize ' . ((&columns * 149 + 75) / 151)
argglobal
let s:cpo_save=&cpo
set cpo&vim
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
nmap <buffer> gd <Plug>FireplaceDtabjump
nmap <buffer> d <Plug>FireplaceDsplit
nmap <buffer>  <Plug>FireplaceDsplit
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")
nnoremap <buffer> <silent> ,J :call PareditJoin()
nnoremap <buffer> <silent> ,O :call PareditSplit()
nnoremap <buffer> <silent> ,> :call PareditMoveRight()
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()
vnoremap <buffer> <silent> ,w{ :call PareditWrapSelection("{","}")
nnoremap <buffer> <silent> ,w{ :call PareditWrap("{","}")
vnoremap <buffer> <silent> ,w[ :call PareditWrapSelection("[","]")
nnoremap <buffer> <silent> ,w[ :call PareditWrap("[","]")
nmap <buffer> <silent> ,I :call PareditRaise()
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)
nmap <buffer> K <Plug>FireplaceK
nnoremap <buffer> <silent> P :call PareditPut('P')
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nmap <buffer> [d <Plug>FireplaceSource
nmap <buffer> [ <Plug>FireplaceDjump
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nmap <buffer> ]d <Plug>FireplaceSource
nmap <buffer> ] <Plug>FireplaceDjump
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> cpr :Require
nmap <buffer> cqc <Plug>FireplacePrompti
nmap <buffer> cqp <Plug>FireplacePrompt
nmap <buffer> cqq <Plug>FireplaceEditab
nmap <buffer> cq <Plug>FireplaceEdit
nmap <buffer> c1mm <Plug>FireplaceMacroExpand1ab
nmap <buffer> c1m <Plug>FireplaceMacroExpand1
nmap <buffer> cmm <Plug>FireplaceMacroExpandab
nmap <buffer> cm <Plug>FireplaceMacroExpand
nmap <buffer> c!! <Plug>FireplaceFilterab
nmap <buffer> c! <Plug>FireplaceFilter
nmap <buffer> cpp <Plug>FireplacePrintab
nmap <buffer> cp <Plug>FireplacePrint
nnoremap <buffer> <silent> dd :call PareditDeleteLines()
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut('p')
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
inoremap <buffer> <expr>  PareditEnter()
map! <buffer> ( <Plug>FireplaceRecall
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
inoremap <buffer> <expr> [ PareditInsertOpening('[',']')
inoremap <buffer> <silent> ] =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('[',']'):let &ve=save_ve
inoremap <buffer> <expr> { PareditInsertOpening('{','}')
inoremap <buffer> <silent> } =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('{','}'):let &ve=save_ve
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=n:;
setlocal commentstring=;\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=clojurecomplete#Complete
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\w*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%+G
setlocal expandtab
if &filetype != 'clojure'
setlocal filetype=clojure
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=2
setlocal include=
setlocal includeexpr=tr(v:fname,'.-','/_')
setlocal indentexpr=GetClojureIndent()
setlocal indentkeys=!,o,O
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255,?,-,*,!,+,/,=,<,>,.,:,$
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=lein
setlocal matchpairs=(:),{:},[:]
setlocal nomodeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=fireplace#omnicomplete
setlocal path=~/clojure/pong/test,~/clojure/pong/src,~/clojure/pong/dev-resources,~/clojure/pong/resources,~/clojure/pong/target/classes,~/.m2/repository/org/swinglabs/swingx/swingx-painters/1.6.3/swingx-painters-1.6.3.jar,~/.m2/repository/org/fife/ui/rsyntaxtextarea/2.0.4.1/rsyntaxtextarea-2.0.4.1.jar,~/.m2/repository/org/swinglabs/swingx/swingx-action/1.6.3/swingx-action-1.6.3.jar,~/.m2/repository/com/miglayout/miglayout/3.7.4/miglayout-3.7.4.jar,~/.m2/repository/org/swinglabs/swingx/swingx-autocomplete/1.6.3/swingx-autocomplete-1.6.3.jar,~/.m2/repository/com/jgoodies/forms/1.2.1/forms-1.2.1.jar,~/.m2/repository/seesaw/seesaw/1.4.4/seesaw-1.4.4.jar,~/.m2/repository/org/clojure/tools.nrepl/0.2.3/tools.nrepl-0.2.3.jar,~/.m2/repository/j18n/j18n/1.0.2/j18n-1.0.2.jar,~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar,~/.m2/repository/clojure-complete/clojure-complete/0.2.3/clojure-complete-0.2.3.jar,~/.m2/repository/org/swinglabs/swingx/swingx-core/1.6.3/swingx-core-1.6.3.jar,~/.m2/repository/org/swinglabs/swingx/swingx-plaf/1.6.3/swingx-plaf-1.6.3.jar,~/.m2/repository/org/swinglabs/swingx/swingx-common/1.6.3/swingx-common-1.6.3.jar
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=.clj,.java
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'clojure'
setlocal syntax=clojure
endif
setlocal tabstop=8
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit src/pong/core.clj
let s:cpo_save=&cpo
set cpo&vim
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
nmap <buffer> gd <Plug>FireplaceDtabjump
nmap <buffer> d <Plug>FireplaceDsplit
nmap <buffer>  <Plug>FireplaceDsplit
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")
nnoremap <buffer> <silent> ,J :call PareditJoin()
nnoremap <buffer> <silent> ,O :call PareditSplit()
nnoremap <buffer> <silent> ,> :call PareditMoveRight()
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()
vnoremap <buffer> <silent> ,w{ :call PareditWrapSelection("{","}")
nnoremap <buffer> <silent> ,w{ :call PareditWrap("{","}")
vnoremap <buffer> <silent> ,w[ :call PareditWrapSelection("[","]")
nnoremap <buffer> <silent> ,w[ :call PareditWrap("[","]")
nmap <buffer> <silent> ,I :call PareditRaise()
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)
nmap <buffer> K <Plug>FireplaceK
nnoremap <buffer> <silent> P :call PareditPut('P')
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nmap <buffer> [d <Plug>FireplaceSource
nmap <buffer> [ <Plug>FireplaceDjump
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nmap <buffer> ]d <Plug>FireplaceSource
nmap <buffer> ] <Plug>FireplaceDjump
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> cpr :Require
nmap <buffer> cqc <Plug>FireplacePrompti
nmap <buffer> cqp <Plug>FireplacePrompt
nmap <buffer> cqq <Plug>FireplaceEditab
nmap <buffer> cq <Plug>FireplaceEdit
nmap <buffer> c1mm <Plug>FireplaceMacroExpand1ab
nmap <buffer> c1m <Plug>FireplaceMacroExpand1
nmap <buffer> cmm <Plug>FireplaceMacroExpandab
nmap <buffer> cm <Plug>FireplaceMacroExpand
nmap <buffer> c!! <Plug>FireplaceFilterab
nmap <buffer> c! <Plug>FireplaceFilter
nmap <buffer> cpp <Plug>FireplacePrintab
nmap <buffer> cp <Plug>FireplacePrint
nnoremap <buffer> <silent> dd :call PareditDeleteLines()
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut('p')
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
inoremap <buffer> <expr>  PareditEnter()
map! <buffer> ( <Plug>FireplaceRecall
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
inoremap <buffer> <expr> [ PareditInsertOpening('[',']')
inoremap <buffer> <silent> ] =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('[',']'):let &ve=save_ve
inoremap <buffer> <expr> { PareditInsertOpening('{','}')
inoremap <buffer> <silent> } =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('{','}'):let &ve=save_ve
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=n:;
setlocal commentstring=;\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=clojurecomplete#Complete
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\w*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%+G
setlocal expandtab
if &filetype != 'clojure'
setlocal filetype=clojure
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=2
setlocal include=
setlocal includeexpr=tr(v:fname,'.-','/_')
setlocal indentexpr=GetClojureIndent()
setlocal indentkeys=!,o,O
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255,?,-,*,!,+,/,=,<,>,.,:,$
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=lein
setlocal matchpairs=(:),{:},[:]
setlocal nomodeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=fireplace#omnicomplete
setlocal path=~/clojure/pong/test,~/clojure/pong/src,~/clojure/pong/dev-resources,~/clojure/pong/resources,~/clojure/pong/target/classes,~/.m2/repository/clojure-complete/clojure-complete/0.2.3/clojure-complete-0.2.3.jar,~/.m2/repository/org/clojure/tools.nrepl/0.2.3/tools.nrepl-0.2.3.jar,~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=2
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=.clj,.java
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'clojure'
setlocal syntax=clojure
endif
setlocal tabstop=8
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 57 - ((0 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
57
normal! 011|
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 1 + 75) / 151)
exe 'vert 2resize ' . ((&columns * 149 + 75) / 151)
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
