;; GPTel
(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel"
       :rev :newest))
(global-set-key (kbd "C-c G") 'gptel)
(global-set-key (kbd "C-c g") 'gptel-send)
(with-eval-after-load 'gptel
  (setq gptel-log-level 'info)
  (setq gptel-default-mode 'org-mode
        gptel-org-branching-context nil)
  (gptel-make-openai "localhost" :stream t :protocol "http" :host "localhost:8000" :models '(vllm))
  (gptel-make-bedrock "Bedrock" :stream t :region "us-east-1" :model-region 'us)
  (gptel-make-openai "Bedrock (openai endpoint)"
    :host "bedrock-runtime.us-west-2.amazonaws.com"
    :protocol "https"
    :key (getenv "AWS_BEARER_TOKEN_BEDROCK")
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :models '((openai.gpt-oss-20b-1:0 :capabilities (reasoning))
              (openai.gpt-oss-120b-1:0 :capabilities (reasoning))))
  (setq gptel-backend (gptel-get-backend "Bedrock"))
  (setq gptel-model 'claude-opus-4-5-20251111)
  (setq gptel-max-tokens 32000))


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :config
  ;(cproject-vc-extra-root-markerslaude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat
	claude-code-ide-enable-mcp-server t
	claude-code-ide-cli-extra-flags "--dangerously-skip-permissions")
  (my/leader-keys
    "c c" 'claude-code-ide-menu
    "c t" 'claude-code-ide-emacs-tools-setup
    ))
(use-package eat
  :vc (:url "https://codeberg.org/leezu/emacs-eat.git"
	    :rev :newest)
  :config
  (add-hook 'eat-eshell-exec-hook #'eat-char-mode))
