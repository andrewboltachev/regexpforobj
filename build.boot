#!/usr/bin/env boot


(set-env!
  :project      'regexpforobj
  :version      "0.1.0-SNAPSHOT"
  :dependencies '[
                  [ns-tracker "0.2.2"]
                  [adzerk/boot-reload        "0.2.6"]
                  [fipp "0.5.2"]
                  ]
  :source-paths    #{"src"}
  )

(use '[ns-tracker.core :only [ns-tracker]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[regexpforobj.core])

(deftask wrap-reload
  "Reload namespaces of modified files before the request is passed to the
  supplied handler.
  Accepts the following options:
 :dirs - A list of directories that contain the source files.
          Defaults to [\"src\"]."
  ;{:arglists '([handler] [handler options])}
  ;[handler & [options]]
  [d dirs [str] "Dirs"]
  (let [source-dirs (or dirs ["src"])
        modified-namespaces (ns-tracker source-dirs)]
    (fn [handler]
      (fn [request]
      (doseq [ns-sym (modified-namespaces)]
        (require ns-sym :reload))
      (handler request)))))

(deftask main
  []
  (with-pre-wrap fileset
    (regexpforobj.core/main)
    fileset
    )
  )


(deftask dev
  "Development loop"
  []
  (comp
    (watch)
    (speak)
    (reload)
    (wrap-reload "src")
    (main)
    )
  )
