(when (equal major-mode 'c++-mode)
  ;; auto-complete
  (setq include-path
        (list
         (expand-file-name
          (concat lds-dir "library"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/base"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/physics"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/math"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/2d"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/ui"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/network"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/audio/include"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/editor-support"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/extensions"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/external"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/platform/ios"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/cocos/platform/ios/Simulation"))
         (expand-file-name
          (concat lds-dir "library/cocos2d-x/external/chipmunk/include/chipmunk"))))


  (setq cflags
        (list
         "-std=c++14"))
  (setq ac-clang-cflags
        (append
         cflags
         include-path))

  (ac-clang-update-cmdlineargs)

  ;; flycheck
  (setq flycheck-clang-include-path include-path)
  (setq flycheck-cppcheck-include-path include-path)
  )
