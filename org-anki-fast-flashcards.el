;;; org-anki-fast-flashcards.el --- org-mode configuration -*- lexical-binding: t; -*-

(defvar oaff-cursor-position nil
  "The saved cursor position and buffer name.")

(defun oaff-save-cursor-position ()
  "Save the current cursor position and buffer name."
  (setq oaff-cursor-position (cons (buffer-name) (point))))

(defun oaff-goto-cursor-position ()
  "Go to the saved cursor position in the saved buffer."
  (if oaff-cursor-position
      (let ((buffer (car oaff-cursor-position))
            (position (cdr oaff-cursor-position)))
        (if (get-buffer buffer)
            (progn
              (switch-to-buffer buffer)
              (goto-char position))
          (message "Buffer %s does not exist" buffer)))
    (message "No cursor position has been saved.")))

(defun oaff-clear-cursor-position ()
  "Clear the saved cursor position."
  (setq oaff-cursor-position nil)
  (message "Saved cursor position cleared."))


(defun oaff-goto-or-create-heading ()
  "Go to the 'Questions & Answers' heading, or create it if it doesn't exist, prompting for deck name if needed."
  (interactive)
  (save-restriction
    (widen)
    (condition-case err
        (let ((heading "Questions & Answers")
              (heading-found nil))
          (save-excursion
            (goto-char (point-min))
            (while (and (not heading-found) (re-search-forward org-heading-regexp nil t))
              (when (string-equal (org-get-heading t t t t) heading)
                (setq heading-found t)
                (goto-char (match-beginning 0)))))
          (if heading-found
              (progn
                (goto-char (match-beginning 0))
                ;; fix wrong formatting when q&a is folded
                (outline-show-subtree))
            (let ((deck (read-string "Enter the Anki deck name: ")))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "* " heading " :ANKI:" "\n")
              (insert ":PROPERTIES:\n")
              (insert ":ANKI_DECK: " deck "\n")
              (insert ":END:\n")
              )))
      (error (message "An error occurred: %s" err)))))

(defun oaff-set-current-heading-to-previous-level ()
  (interactive)
  "Set the current heading to the same level as the previous heading."
  (save-excursion
    (org-previous-visible-heading 1)
    (let* ((current-level (org-current-level))
           (previous-level (save-excursion
                             (org-previous-visible-heading 1)
                             (org-current-level))))
      (when previous-level
        (let ((difference (- current-level previous-level)))
          (cond

           ((> difference 0)
            (dotimes ( _ difference) (org-do-promote)))

           ((< difference 0)
            (dotimes ( _ ( - difference)) (org-do-demote)))))))))

(defun oaff-check-previous-heading-level ()
  "Check if previous heading level is 1, demote to 2"
  (save-excursion
    (org-previous-visible-heading 1)
    (cond
     ((= (org-current-level) 1)
      (org-do-demote)))))


(defun oaff-open-temp-buffer ()
  "Open the current heading as a flashcard editing buffer"
  (let ((original-buffer (current-buffer))
        (copied-content (current-kill 0))
        (temp-buffer-name "*Temporary Buffer*"))

    ;; Create and display the temporary buffer at the bottom
    (split-window-below)
    (other-window 1)
    (with-current-buffer (get-buffer-create temp-buffer-name)
      (switch-to-buffer temp-buffer-name)
      (erase-buffer)
      (org-mode)

      ;; set modeline-format after org-mode is loaded, so that isn't overwritten
      (setq-local header-line-format "FLASHCARDS | `C-c C-c' to finish editing")

      (insert copied-content)
      (org-previous-visible-heading 1)
      (org-edit-headline (read-string "[Question?] "))
      (next-line)
      (local-set-key (kbd "C-c C-c") 'oaff-close-temp-buffer)

      ;; Store the original buffer as a buffer-local variable
      (setq-local my-original-buffer original-buffer))))

(defun oaff-close-temp-buffer ()
  "Close the flashcard buffer and yank the content to the Q&A heading"
  (interactive)

  ;; Copy whole buffer
  (save-excursion
    (goto-char (point-min))   ;; Move point to the beginning of the buffer
    (push-mark (point-max))   ;; Mark the end of the buffer
    (kill-ring-save (point) (mark))) ;; Copy the region to the kill ring

  ;; Kill buffer and close window
  (kill-buffer)
  (delete-window)

  ;; Restore default keybinding
  (local-set-key (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)

  ;; Goto to end q&a heading and insert the content in the appropriate position
  (oaff-goto-or-create-heading)
  (org-end-of-subtree)
  (newline)
  (yank)

  ;; Check the level of the heading inserted
  (oaff-set-current-heading-to-previous-level)
  (oaff-check-previous-heading-level)

  ;; hide q&a heading
  (oaff-goto-or-create-heading)
  (outline-hide-subtree)

  ;; Go back to original position
  (oaff-goto-cursor-position)
  )

;;;###autoload
(defun oaff-create-flashcard ()
  "Create a flashcard from the current heading under the Q&A heading"
  (interactive)
  (oaff-clear-cursor-position)
  (oaff-save-cursor-position)
  (org-copy-subtree 1 nil nil t)
  (oaff-open-temp-buffer)
  )

(provide 'org-anki-fast-flashcards)
