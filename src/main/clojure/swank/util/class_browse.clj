;;; class-browse.clj -- Java classpath and Clojure namespace browsing

;; Scans the classpath for all class files, and provides functions for
;; categorizing them.

;; See the following for JVM classpath and wildcard expansion rules:
;;   http://java.sun.com/javase/6/docs/technotes/tools/findingclasses.html
;;   http://java.sun.com/javase/6/docs/technotes/tools/solaris/classpath.html

(ns #^{:doc "Provides Java classpath and (compiled) Clojure namespace browsing.
  Scans the classpath for all class files, and provides functions for
  categorizing them. Classes are resolved on the start-up classpath only.
  Calls to 'add-classpath', etc are not considered.

  Class information is built as a list of maps of the following keys:
    :name   Qualified Java class or Clojure namespace name
    :sname  Simple class name, or nil if class is anonymous
    :loc    Classpath entry (directory or jar) on which the class is located
    :file   Path of the class file, relative to :loc"}

  swank.util.class-browse
  (:import [java.io File FilenameFilter]
           [java.util StringTokenizer]
           [java.util.jar JarFile JarEntry]))

;;; Class file naming, categorization

(defn jar-file? [#^String p] (.endsWith p ".jar"))
(defn class-file? [#^String p] (.endsWith p ".class"))
(defn clojure-ns-file? [#^String p] (.endsWith p "__init.class"))
(defn clojure-fn-file? [#^String p] (re-find #"\$.*__\d+\.class" p))
(defn top-level-class-file? [#^String p] (re-find #"^[^\$]+\.class$" p))
(defn anonymous-class-file? [#^String p] (re-find #"\$\d+.*\.class$" p))
(defn anonymous-class-name? [#^String n] (re-find #"\$\d+" n))
(defn nested-class-file? [#^String p] (re-find #"^[^\$]+(\$[^\d]\w*)+\.class$" p))
                                        ; ^ excludes anonymous classes

(def clojure-ns? (comp clojure-ns-file? :file))
(def clojure-fn? (comp clojure-fn-file? :file))
(def top-level-class? (comp top-level-class-file? :file))
(def anonymous-class? (comp anonymous-class-file? :file))
(def nested-class? (comp nested-class-file? :file))

(defn class-simple-name
  "Returns the simple name of a qualified class name, or nil if the class is
  anonymous."
  [#^String n]
  (if (anonymous-class-name? n) nil
      (.substring n (inc (.lastIndexOf n ".")))))

(defn class-or-ns-name
  "Returns the qualified Java class or Clojure namespace name for a class file
  relative path."
  [#^String p]
  (.replaceAll
   (if (clojure-ns-file? p)
     (-> p (.replace "__init.class" "") (.replace "_" "-"))
     (.replace p ".class" ""))
   (str "[/" File/separator "]") "."))	; jar separator is always '/'

;;; Path scanning

(defn class-info
  "Creates a map of class info having the following keys:
    :name   Qualified Java class or Clojure namespace name
    :sname  Simple class name, or nil if class is anonymous
    :loc    Classpath entry (directory or jar) on which the class is located
    :file   Path of the class file, relative to :loc"
  [fp lp]
  (let [n (class-or-ns-name fp), sn (class-simple-name n)]
    {:file fp :loc lp :name n :sname sn}))

(defmulti path-class-files
  "Returns a list of classes found on the specified path location
  (jar or directory), each comprised of a class-info map."
  (fn [#^ File f _]
    (cond (.isDirectory f)           :dir
          (jar-file? (.getName f))   :jar
          (class-file? (.getName f)) :class)))

(defmethod path-class-files :default
  [& _] [])

(defmethod path-class-files :jar
  ;; Build class info for all jar entry class files.
  [#^File f #^File loc]
  (let [lp (.getPath loc)]
    (try
     (map (fn [fp] (class-info fp lp))
          (filter class-file?
                  (map #(.getName #^JarEntry %)
                       (enumeration-seq (.entries (JarFile. f))))))
     (catch Exception e []))))			; fail gracefully if jar is unreadable

(defmethod path-class-files :dir
  ;; Dispatch directories and files (excluding jars) recursively.
  [#^File d #^File loc]
  (let [fs (.listFiles d (proxy [FilenameFilter] []
                           (accept [d n] (not (jar-file? n)))))]
    (reduce concat (for [f fs] (path-class-files f loc)))))

(defmethod path-class-files :class
  ;; Build class info using file path relative to parent classpath entry
  ;; location. Make sure it decends; a class can't be on classpath directly.
  [#^File f #^File loc]
  (let [fp (.getPath f), lp (.getPath loc)
        m (re-matcher (re-pattern (str "^" lp File/separator)) fp)]
    (if (.find m)						; must be descendent of loc
      (let [fpr (.substring fp (.end m))]
        [(class-info fpr lp)])
      [])))

;;; Classpath expansion

(def java-version
     (Float/parseFloat (.substring (System/getProperty "java.version") 0 3)))

(defn expand-wildcard
  "Expands a wildcard path entry to its matching .jar files (JDK 1.6+).
  If not expanding, returns the path entry as a single-element vector."
  [#^String p]
  (let [f (File. p)]
    (if (and (= (.getName f) "*") (>= java-version 1.6))
      (-> f .getParentFile
          (.list (proxy [FilenameFilter] []
                   (accept [d n] (jar-file? n)))))
      [f])))

(defn scan-paths
  "Takes one or more classpath strings, scans each classpath entry location, and
  returns a list of all class file paths found, each relative to its parent
  directory or jar on the classpath."
  ([cp]
     (let [entries (enumeration-seq
                    (StringTokenizer. cp File/pathSeparator))
           locs (mapcat expand-wildcard entries)]
       (reduce concat (for [loc locs] (path-class-files loc loc)))))
  ([cp & more]
     (reduce #(concat %1 (scan-paths %2)) (scan-paths cp) more)))

;;; Class browsing

(def available-classes
     (filter (complement clojure-fn?)  ; omit compiled clojure fns
             (scan-paths (System/getProperty "sun.boot.class.path")
                         (System/getProperty "java.ext.dirs")
                         (System/getProperty "java.class.path"))))

;; Force lazy seqs before any user calls, and in background threads; there's
;; no sense holding up SLIME init. (It's usually quick, but a monstrous
;; classpath could concievably take a while.)

(def top-level-classes
     (future (doall (filter top-level-class?
                            available-classes))))
(def nested-classes
     (future (doall (filter nested-class?
                            available-classes))))

;; (def top-level-simple-names (future (doall (map :sname @top-level-classes))))
;; (def top-level-class-names (future (doall (map :name @top-level-classes))))
;; (def nested-simple-names (future (doall (map :sname @nested-classes))))
;; (def nested-class-names (future (doall (map :name @nested-classes))))

(defn resolve-class-name
  "Returns the list of qualified class names that match the specifed search.
  Search will be against qualified names is a '.' is present in the search
  string, and against simple names otherwise. Nested classes are only considered
  if a '$' is present in the search string."
  [s]
  (let [classes (if (.contains s "$") @nested-classes @top-level-classes)
        attrib  (if (.contains s ".") :name :sname)]
    (concat
     (sort (map :name (filter #(.startsWith (attrib %) s) classes))) ; prefer these
     (sort (map :name (filter #(re-find (re-pattern s) (attrib %)) classes))))))

(comment
  (in-ns 'swank.util.class-browse)
  (use 'clojure.test)

  (deftest test-class-names
    ;; Simple names should be nil (anonymous) or correspond to qualified name.
    (is (every?   #(or (nil? (:sname %)) (.endsWith (:name %) (:sname %)))
                  available-classes))
    (is (not-any? #(and (:sname %) (.contains (:sname %) "."))
                  available-classes)))

  (deftest test-top-level
    (is (not-any? #(.contains (:name %) "$")
                  (filter top-level-class? available-classes))))

  (deftest test-nested
    (is (every? #(.contains (:name %) "$")
                (filter nested-class? available-classes))))

  (run-tests 'swank.util.class-browse)
  )
