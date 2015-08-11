(prodigy-define-service
  :name "Government/Web Server"
  :command "gulp"
  :args '("webserver")
  :tags '(government gulp)
  :cwd "~/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Government/Gulp"
  :command "gulp"
  :tags '(government gulp)
  :cwd "~/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Government/Server processing"
  :command "nodemon"
  :args '("--watch" "server/app.js" "server/app.js")
  :tags '(government node)
  :cwd "~/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t
)

(prodigy-define-service
  :name "Government/Firebase stub"
  :command "nodemon"
  :args '("--watch" "firebase-app.js" "firebase-app.js")
  :tags '(government node)
  :cwd "~/Desktop/Government"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Personal Blog/webserver"
  :command "bundle"
  :args '("exec" "jekyll" "serve" "--watch" "--drafts")
  :tags '(blog jekyll)
  :cwd "~/Desktop/tylerdiaz.github.io"
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)
