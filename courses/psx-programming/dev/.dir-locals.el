((nil
  (eval . (let ((root (concat (projectile-project-root)
			      "courses/psx-programming/dev/")))
	    (setq-local flycheck-clang-include-path
			(list (concat root "thirdparty/nugget/psyq/include")
			      (concat root "psyq/include")))
	    (setq-local flycheck-gcc-include-path
			(list (concat root "thirdparty/nugget/psyq/include")
			      (concat root "psyq/include")))))))
