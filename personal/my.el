;; 设置默认路径
(setq default-directory "D:\\CODE\\trunk")

;;编码设置
(set-language-environment 'utf-8)
(set-locale-environment "utf-8")
;; 将utf-8设置为优先级最高的编码格式
(setq prefer-coding-system 'utf-8)
;; 设置buffer的默认编码格式为utf-8
(setq default-buffer-file-coding-system 'utf-8)

;; Emacs win8 x64 英文版下打开文件乱码解决
(set-fontset-font "fontset-default"
'unicode '("Microsoft YaHei" . "unicode-bmp"))

;; 最侧面显示行号:
(global-linum-mode t)

;; 启动时自动最大化 windows平台
(run-with-idle-timer 1 nil 'w32-send-sys-command 61488)

;; Kill the previous word(backward-kill-word). (as in Bash/Zsh)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; 开启 ido-mode 模式，快速的搜索。
(ido-mode t)

;; 能够快速打开我的配置文件
(set-register ?e '(file . "~/.emacs.d/personal/my.el"))
;; 快速查看我的配置文件
(set-register ?d '(file . "D:/CODE/lib/dict/emacs-keybinding.el"))

(defun recentf-ido-find-file () 
  "Find a recent file using Ido." 
  (interactive) 
  (let* ((file-assoc-list 
          (mapcar (lambda (x) 
                    (cons (file-name-nondirectory x) 
                          x)) 
                  recentf-list)) 
         (filename-list 
          (remove-duplicates (mapcar #'car file-assoc-list) 
                             :test #'string=)) 
         (filename (ido-completing-read "Choose recent file: " 
                                        filename-list 
                                        nil 
                                        t))) 
    (when filename 
      (find-file (cdr (assoc filename 
                             file-assoc-list))))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

;; Aiqier 能够切换显示菜单栏和工具栏

;; (defun switch-bar()
;;   (interactive)
;;   (let ((flag (if( equal tool-bar-mode t) -1 t )))
;;   (tool-bar-mode flag)
;;   (menu-bar-mode flag)))

;; (global-set-key (kbd "<f5>")   'switch-bar)


;;insert-current-date
;;插入当前时间
(defun insert-current-date ()
    "Insert the current date"
    (interactive "*")
    ;(insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
    (insert (format-time-string "%Y-%m-%d" (current-time))))
(global-set-key (kbd "C-x t") 'insert-current-date)

;; 插入当前文件完整路径
(defun insert-current-file-fill-path ()
  "Insert the current file fill path"
  (interactive "*")
  (insert (format "%s" (buffer-file-name))))
(global-set-key (kbd "<f8>") 'insert-current-file-fill-path)


;; 转到某一行
(global-set-key (kbd "M-g")  'goto-line)

;; 复制一行
(defun copy-line (&optional arg)
 "Save current line into Kill-Ring without mark the line"
 (interactive "P")
 (let ((beg (line-beginning-position)) 
  (end (line-end-position arg)))
 (copy-region-as-kill beg end))
)

;; 复制一个单词
(defun copy-word (&optional arg)
 "Copy words at point"
 (interactive "P")
 (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point))) 
  (end (progn (forward-word arg) (point))))
 (copy-region-as-kill beg end))
)

;; 设置复制的键绑定
(global-set-key (kbd "C-x w") 'copy-word)
(global-set-key (kbd "C-x y") 'copy-line)

;; 按照某个xx对其
(global-set-key (kbd "C-x j") 'align-regexp)

(require 'org)


;; 这在done的时候，会加时间标签。
(setq org-log-done t)

;; org-id-store-link
;; (global-set-key (kbd "C-c C-l") 'org-store-link)

;; 添加注释与反注释
(global-set-key (kbd "C-c C-b") 'comment-or-uncomment-region)  

;; 开启yasnippet

(require 'yasnippet)

(add-to-list 'yas/root-directory "snippets")
(yas-reload-all)

(add-hook 'python-mode-hook
          '(lambda ()
             (yas-minor-mode)))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; 好代码一行不应该超过80个字符
(add-hook 'python-mode-hook
          (lambda () (highlight-lines-matching-regexp ".\\{81\\}" "red")))

;; 快速的使用bing查找
(prelude-install-search-engine "bing"       "https://cn.bing.com/search?q="    "Search Bing:" )
(global-set-key (kbd "C-c b") 'prelude-bing)

;; 快速的使用bing字典
(prelude-install-search-engine "bing-dict" "http://cn.bing.com/dict/search?q=" "Bing Dict:")
(global-set-key (kbd "C-c v") 'prelude-bing-dict)

;; emacs 弹窗
(defvar popup-terminal-command
  (cond ((memq system-type '(windows-nt cygwin))
	 '("cmd" "/c" "start" ))
	(t '("x-terminal-emulator"))))
(defun popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command))
(global-set-key (kbd "<f5>") 'popup-term)

;; emacs弹窗,执行，当前的python
(defun popup-expython()
  (interactive)
  (let ((filename "test.py")
        (args (list "cmd" "/c" "start" "python" (buffer-name))))
  (apply 'start-process "terminal" nil args)))
(global-set-key (kbd "<f6>") 'popup-expython)

(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-c a") 'wy-go-to-char)
