(defun adf-parser--children (children)
  "Parse all children"
  (cl-mapcar 'adf-parser-to-org children))

(defun adf-parser-to-org (node)
  "Parses an ADF tree to org-mode style text."
  (let-alist node
    (cond ((string= .type "blockquote")
           (format "#+begin_quote\n%s\n#+end_quote\n"
                   (string-join (adf-parser--children .content) "\n")))
          ((string= .type "bulletList")
           (string-join (adf-parser--children .content) "\n"))
          ((string= .type "codeBlock")
           (format
            "#+begin_src %s\n%s\n"
            .attrs.language
            (string-join (adf-parser--children .content) "")))
          ((string= .type "date")
           (format-time-string
            "%a %e %b %Y %R\n"
            (string-to-number .attr.timestamp)))
          ((string= .type "doc")
           (string-join (adf-parser--children .content) ""))
          ((string= .type "emoji")
           "")
          ((string= .type "expand")
           (string-join (adf-parser--children .content) ""))
          ((string= .type "hardBreak")
           "\n")
          ((string= .type "heading")
           (format "%s %s" (string-utils-string-repeat "*" .attrs.level) (adf-parser--children .content)))
          ((string= .type "inlineCard")
           (format "<%s>" .attrs.url))
          ((string= .type "listItem")
           (format "- %s" (string-join (adf-parser--children .content) "")))
          ((string= .type "media")
           (format "[[%s]]" .attrs.alt))
          ((string= .type "mediaGroup")
           "[media group]")
          ((string= .type "mediaSingle")
           (string-join (adf-parser--children .content) "\n"))
          ((string= .type "mention")
           .attr.text)
          ((string= .type "orderedList")
           (string-join (adf-parser--children .content) "\n"))
          ((string= .type "rule")
           "\n-----\n")
          ((string= .type "paragraph")
           (format "%s\n" (string-join (adf-parser--children .content) " ")))
          ((string= .type "text")
           .text))))
