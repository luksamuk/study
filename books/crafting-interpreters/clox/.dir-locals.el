((nil
  (eval . (let ((root (concat (projectile-project-root)
			      "books/crafting-interpreters/clox/")))
            (setq-local flycheck-clang-include-path
                        (list (concat root "src/include")))
            (setq-local flycheck-gcc-include-path
                        (list (concat root "src/include")))))))

