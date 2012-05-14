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
"MiddleClick paste doesn't get screwed up **No longer need to set paste**
imap <MiddleMouse> "+p`]a

"-> Files
"   -> UndoFile - allows for a persistant undo between sessions
set undofile
set undodir=~/.vim_undo
set nobackup

"-> Appearance
syntax on
"Improve zenburn's appearance
let g:zenburn_old_Visual = 1
let g:zenburn_alternate_Visual = 1
let g:zenburn_high_Contrast = 1
colorscheme tir_black
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

"-> Setup wildmenu so that I can see my tab completion options
set wildmenu
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
set expandtab
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
  au BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj au BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
  au BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
  au BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji
augroup END

"-> Guioptions
set guifont="Inconsolata Medium 9"

"-> StatusLine
set laststatus=2
set statusline=%f%m%r%=%02.v\|%03.l/%03.L


"-> Plugins Galore

"-> Taglist
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 40
noremap <F4> :TlistToggle<cr>
noremap <F5> :TlistUpdate<cr>

"-> Yankring
let yankring_min_element_length = 2 "prevents the addition of single character deletes
let yankring_window_auto_close=0
let yankring_window_use_horiz=0
let yankring_window_use_right=1
let yankring_window_width=30
let yankring_manual_clipboard_check=1 "Makes it check system clipboard for changes when running in terminal
noremap <silent> <F2> :YRShow<CR>



