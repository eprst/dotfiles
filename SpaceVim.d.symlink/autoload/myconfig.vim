nnoremap <silent> <M-1> :NERDTreeFind<CR>
nnoremap <silent> 1 :NERDTreeFind<CR>
nnoremap <silent> ¡ :NERDTreeFind<CR>
nnoremap <silent> <M-!> :NERDTreeFind<CR>

nnoremap <leader>o :<C-u>Unite -buffer-name=outline -vertical -start-insert outline<cr>

nnoremap <silent><F5> :MaximizerToggle<CR>
vnoremap <silent><F5> :MaximizerToggle<CR>gv
inoremap <silent><F5> <C-o>:MaximizerToggle<CR>
