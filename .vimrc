runtime! archlinux.vim
"let skip_defaults_vim=1

set backspace=indent,eol,start 
set number 
nnoremap S :%s//g<Left><Left>
nnoremap <C-p> :<C-u>FZF<CR>
