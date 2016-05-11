#!/usr/bin/env boot


(set-env!
  :project      'regexpforobj
  :version      "0.1.0-SNAPSHOT"
  :dependencies '[
                  [ns-tracker "0.2.2"]
                  [adzerk/boot-reload        "0.2.6"]
                  [fipp "0.5.2"]
                  [adzerk/bootlaces          "0.1.10"]
                  [org.clojure/core.typed "0.3.18"]
                  [aprint "0.1.3"]
                  [io.aviso/pretty "0.1.24"]
                  ]
  :source-paths    #{"src"}
  )


(require '[adzerk.bootlaces :refer :all])

(def +version+ "1.0.0-alpha2-SNAPSHOT")

(bootlaces! +version+)

(task-options!
  pom  {:project     'regexpforobj
        :version     +version+
        :description "Objectified regular expressions parser library."
        ;:url         ""
        ;:scm         {:url ""}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})


(use '[ns-tracker.core :only [ns-tracker]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[regexpforobj.core])
(require '[regexpforobj.main])


(deftask main []
  (with-pre-wrap fileset
    (regexpforobj.main/main)
    fileset)
  )




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
