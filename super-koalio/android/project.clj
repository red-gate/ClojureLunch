(defproject super-koalio "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  
  :dependencies [[com.badlogicgames.gdx/gdx "1.6.0" :use-resources true]
                 [com.badlogicgames.gdx/gdx-backend-android "1.6.0"]
                 [com.badlogicgames.gdx/gdx-box2d "1.6.0"]
                 [com.badlogicgames.gdx/gdx-bullet "1.6.0"]
                 [neko/neko "3.2.0"]
                 [org.clojure-android/clojure "1.7.0-RC1" :use-resources true]
                 [play-clj "0.4.7"]]
  :profiles {:dev {:dependencies [[org.clojure-android/tools.nrepl "0.2.6-lollipop"]]
                   :android {:aot :all-with-unused}}
             :release {:android
                       {;; Specify the path to your private
                        ;; keystore and the the alias of the
                        ;; key you want to sign APKs with.
                        ;; :keystore-path "/home/user/.android/private.keystore"
                        ;; :key-alias "mykeyalias"
                        :aot :all}}}
  
  :android {;; Specify the path to the Android SDK directory either
            ;; here or in your ~/.lein/profiles.clj file.
            ;; :sdk-path "/home/user/path/to/android-sdk/"
            
            ;; Uncomment this if dexer fails with OutOfMemoryException
            ;; :force-dex-optimize true
            
            :assets-paths ["../desktop/resources"]
            :native-libraries-paths ["libs"]
            :target-version "15"
            :aot-exclude-ns ["clojure.parallel" "clojure.core.reducers"
                             "cljs-tooling.complete" "cljs-tooling.info"
                             "cljs-tooling.util.analysis" "cljs-tooling.util.misc"
                             "cider.nrepl" "cider-nrepl.plugin"]
            :dex-opts ["-JXmx4096M"]}
  
  :source-paths ["src/clojure" "../desktop/src-common"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"])
