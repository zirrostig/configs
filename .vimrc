"-> All Important
set nocompatible
filetype plugin indent on

"-> Set my leader
let mapleader = ","

"-> General Niceities
"   -> autoread - automatically re-read file if it has changed outside the buffer
"   -> formatoptions - does many things to have vim auto format stuff for me
set mouse=n
set backspace=eol,indent,start
set autoread
set hidden
set history=500
set formatoptions=croql
" I hit these too often accidently, and this will save my sanity
map Q <Nop>
imap <C-w> <Nop>
"Space clears highlighting
noremap <silent> <Space> :silent nohl<Bar>echo<CR>

"-> Files
"   -> UndoFile - allows for a persistant undo between sessions
set undofile
set undodir=~/.vim/.undo
set nobackup

"-> Appearance
syntax on
colorscheme wombat256
"hi Normal ctermbg=NONE
"209 = #e5786d
hi Search term=NONE cterm=underline ctermfg=209 ctermbg=NONE
set number
set nowrap
set ruler
set showmatch

"-> Set number/relativenumber smartly
autocmd InsertEnter * set number
autocmd InsertLeave * set relativenumber

set wildmenu
"-> Make vim colorschemes work with Gnome-Terminal
set wildmode=list:longest
set wildchar=<Tab>

"-> Make vim colorschemes work with Gnome-Terminal
if $COLORTERM == ('gnome-terminal' || 'xterm')
    set t_Co=256
endif

"-> Show invisibles
"   -> This shows
"       Trailing spaces
"       Empty lines with spaces
"       Also The foreground and background of these chars are set
set list listchars=tab:\|\ ,trail:¤,nbsp:¤
hi SpecialKey ctermbg=NONE
hi SpecialKey ctermfg=red

"-> Movement
set scrolloff=4
set virtualedit=onemore
"   -> Between Panes - Maps C-w, direction to move to pane in that direction
    map <silent>,h <C-w>h
    map <silent>,j <C-w>j
    map <silent>,k <C-w>k
    map <silent>,l <C-w>l

"-> Tab Key
"   -> expandtab - use spaces inplace of tab characters, this setting will occasionally bite you in the ass
set noexpandtab
set tabstop=4
set shiftwidth=4
set autoindent
set smartindent
"   -> Language Specific options
"       -> Now found in ~/.vim/ftplugin/<language>.vim

"-> Searching
set incsearch
set hlsearch
set smartcase" Some tricks for mutt

"-> Spelling
set spelllang=en_us
set nospell

"-> Happy Config for Mutt
" F1 through F3 re-wraps paragraphs in useful ways
augroup MUTT
  au BufRead ~/.mutt/temp/mutt* set spell
  au BufRead ~/.mutt/temp/mutt* nmap  <F1>  gqap
  au BufRead ~/.mutt/temp/mutt* nmap  <F2>  gqqj
  au BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj
  au BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
  au BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
  au BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji
augroup END

"-> Guioptions
set guifont="Inconsolata Medium 9"

"-> StatusLine
set laststatus=2
set statusline=%f%m%r%=%02.v\|%03.l/%03.L
