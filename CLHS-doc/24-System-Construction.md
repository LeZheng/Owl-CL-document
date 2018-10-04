# 24 系统构造

> * 24.1 [系统构造的概念](#SystemConstructionConcepts)
> * 24.2 [系统构造的字典](#SystemConstructionDictionary)

## 24.1 <span id="SystemConstructionConcepts">系统构造的概念</span>

> * 24.1.1 [加载](#Loading)
> * 24.1.2 [特性](#Features)

### 24.1.1 <span id="Loading">加载</span>

去加载一个文件就是去吧它的内容当作代码并且执行那些代码. 这个文件可能包含源代码或编译后的代码.

一个包含源代码的文件称为源码文件. 加载一个源码文件由顺序读取这个文件中的表达式形式, 在读取后立即求值每一个来完成的.

一个包含编译后代码的文件被称为一个编译后的文件. 加载一个编译后的文件类似于加载一个源码文件, 除了这个文件不会包含文本而是一种由编译器创建的预处理的依赖于具体实现的表示. 通常, 一个编译后的文件可以比一个源码文件更快地加载. 见章节 3.2 (Compilation).

区分源码文件和编译后文件的方式是依赖于具体实现的.

### 24.1.2 <span id="Features">特性</span>

一个特性是一个 Common Lisp 的, 具体实现的或者环境的样子或属性. 一个特性由一个符号标识.

当且仅当一个命名一个特性的符号是变量 \*features* 持有的列表的一个元素时, 就说这个特性存在于一个 Lisp 镜像中, 这个列表被称为特性列表.

#### 24.1.2.1 特性表达式

称为特性表达式的特性的 boolean 组合被 #+ 和 #- 读取器宏用来指导表达式被 Lisp 读取器条件读取.

解释一个特性表达式的规则如下:

feature

    如果命名一个特性的符号被用作一个特性表达式, 如果那个特性存在, 那么这个特性表达式成功; 否则就是失败.

(not feature-conditional)

    如果一个 not 特性表达式的参数 feature-conditional 失败了, 那么这个 not 特性表达式成功; 负责, 它失败.<!--原文好像不对-->

(and feature-conditional*)

    如果一个 and 特性表达式的所有参数都成功, 那么这个 and 特性表达式成功; 否则, 它就是失败的.

(or feature-conditional*)

    如果一个 or 特性表达式的任意参数成功, 那么这个 or 特性表达式成功; 负责, 它失败.

##### 24.1.2.1.1 特性表达式的示例

比如, 假设在实现 A 中, 存在特性 spice 和 perq, 但是特性 lispm 不存在; 在实现 B 中, 存在特性 lispm, 但是不存在特性 spice 和 perq; 并且在实现 C 中, 特性 spice, lispm, 或 perq 都不存在. 下一段展示了一些简单表达式, 以及它们在这些实现中如何会被读取.

    (cons #+spice "Spice" #-spice "Lispm" x)                             
                                                          
    in implementation A ...  (CONS "Spice" X)             
    in implementation B ...  (CONS "Lispm" X)             
    in implementation C ...  (CONS "Lispm" X)             
                                                          
    (cons #+spice "Spice" #+LispM "Lispm" x)                             
                                                          
    in implementation A ...  (CONS "Spice" X)             
    in implementation B ...  (CONS "Lispm" X)             
    in implementation C ...  (CONS X)                     
                                                          
    (setq a '(1 2 #+perq 43 #+(not perq) 27))                             
                                                          
    in implementation A ...  (SETQ A '(1 2 43))           
    in implementation B ...  (SETQ A '(1 2 27))           
    in implementation C ...  (SETQ A '(1 2 27))           
                                                          
    (let ((a 3) #+(or spice lispm) (b 3)) (foo a))                             
                                                          
    in implementation A ...  (LET ((A 3) (B 3)) (FOO A))  
    in implementation B ...  (LET ((A 3) (B 3)) (FOO A))  
    in implementation C ...  (LET ((A 3)) (FOO A))        
                                                          
    (cons #+Lispm "#+Spice" #+Spice "foo" #-(or Lispm Spice) 7 x)                             
                                                          
    in implementation A ...  (CONS "foo" X)               
    in implementation B ...  (CONS "#+Spice" X)           
    in implementation C ...  (CONS 7 X)                   

    Figure 24-1. 特性示例

## 24.2 <span id="SystemConstructionDictionary">系统构造的字典</span>

> * [函数 COMPILE-FILE](#F-COMPILE-FILE)
> * [函数 COMPILE-FILE-PATHNAME](#F-COMPILE-FILE-PATHNAME)
> * [函数 LOAD](#F-LOAD)
> * [宏 WITH-COMPILATION-UNIT](#M-WITH-COMPILATION-UNIT)
> * [变量 *FEATURES*](#V-FEATURES)
> * [变量 *COMPILE-FILE-PATHNAME*, *COMPILE-FILE-TRUENAME*](#V-CF-TRUENAME-PATHNAME)
> * [变量 *LOAD-PATHNAME*, *LOAD-TRUENAME*](#V-LOAD-PATHNAME-TRUENAME)
> * [变量 *COMPILE-PRINT*, *COMPILE-VERBOSE*](#V-COMPILE-PRINT-VERBOSE)
> * [变量 *LOAD-PRINT*, *LOAD-VERBOSE*](#V-LOAD-PRINT-VERBOSE)
> * [变量 *MODULES*](#V-MODULES)
> * [函数 PROVIDE, REQUIRE](#F-PROVIDE-REQUIRE)

### <span id="F-COMPILE-FILE">函数 COMPILE-FILE</span>

* 语法(Syntax):

        compile-file input-file &key output-file verbose print external-format
        => output-truename, warnings-p, failure-p

* 参数和值(Arguments and Values):

        input-file---一个路径名标识符. (未指定的成员的默认填充是取自 *default-pathname-defaults*.)
        output-file---一个路径名标识符. 默认是具体实现定义的.
        verbose---一个广义 boolean. 默认是 *compile-verbose* 的值.
        print---一个广义 boolean. 默认是 *compile-print* 的值.
        external-format---一个外部文件格式标识符. 默认是 :default.
        output-truename---一个路径名 (输出文件的真实名字 truename), 或 nil.
        warnings-p---一个广义 boolean.
        failure-p---一个广义 boolean.

* 描述(Description):

        compile-file 把由 input-file 指定的文件的内容转化为依赖于具体实现的二进制数据, 放置在由 output-file 指定的文件中.

        这个 input-file 引用的文件应该是一个源码文件. output-file 可以被用于指定一个输出路径名; 编译后的代码要被输出到的编译后文件的实际路径名被计算, 就像是通过调用 compile-file-pathname 一样.

        如果 input-file 或 output-file 是一个逻辑路径名, 那么它会被转化为一个物理路径名, 就像是通过调用 translate-logical-pathname 一样.

        如果 verbose 是 true, compile-file 用一个注释的形式打印一个信息 (换句话说, 用一个前导分号) 到标准输出来表示什么文件要被编译以及其他有用的信息. 如果 verbose 是 false, compile-file 不打印这个信息.

        如果 print 是 true, 关于这个文件中要被编译的顶层表达式形式的信息会被打印到标准输出. 确切地说, 打印的内容依赖于具体实现, 但是仍然会打印一些信息. 如果 print 是 nil, 没有信息要被打印.

        这个 external-format 指定了当打开这个文件时要被使用的外部文件格式; 见函数 open. compile-file 以及 load 必须以这样一种方式互操作: 产生的编译后文件可以不重新指定一个外部文件格式来加载; 见函数 load.

        compile-file 绑定 *readtable* 和 *package* 为它们在处理这个文件之前的值.

        *compile-file-truename* 被 compile-file 绑定来持有这个要被编译的文件的真实名字.

        *compile-file-pathname* 被 compile-file 绑定来持有传递给 compile-file 的第一个参数所表示的路径名, 这个路径名和默认的合并; 这也就是说, (pathname (merge-pathnames input-file)).

        这个编译后文件包含的编译后的函数在这个编译后文件被加载入 Lisp 中时是可用的. 任何由编译器处理的函数定义, 包括 #'(lambda ...) 表达式形式和由 flet, labels 创建的局部函数定义以及 defun 表达式形式, 产生一个 compiled-function 类型的对象.

        由 compile-file 返回的主要值, output-truename, 是那个输出文件的真实名字, 或者如果这个文件没有被创建那么就是 nil.

        第二个值, warnings-p, 如果编译器没有检测到 error 或 warning 类型的状况, 那么就是 false, 否则就是 true.

        第三个值, failure-p, 如果编译器没有检测到 error 或 warning 类型的状况 (除了 style-warning), 那么就是 false, 否则就是 true.

        关于这个文件编译器如何处理文集的一般信息, 见章节 3.2.3 (File Compilation).

        要被文件编译器编译的程序必须只包含可外部化的对象; 关于这样的对象的详细信息, 见章节 3.2.4 (Literal Objects in Compiled Files). 关于如何去扩展可外部化对象集合的信息, 见函数 make-load-form 和章节 3.2.4.4 (Additional Constraints on Externalizable Objects).

* 示例(Examples): None.

* 受此影响(Affected By):

        *error-output*, *standard-output*, *compile-verbose*, *compile-print*

        计算机的文件系统.

* 异常情况(Exceptional Situations):

        关于编译处理期间的错误检测的信息 For informaftion about errors detected during the compilation procss, 见章节 3.2.5 (Exceptional Situations in the Compiler).

        如果 (wild-pathname-p input-file) 返回 true, 那么可能会发出一个 file-error 类型的错误.

        如果尝试去打开一个用于输入的源码文件或者尝试去打开一个用于输出的编译后文件失败了, 那么就会发出一个 file-error 类型的错误.

* 也见(See Also):

        compile, declare, eval-when, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None.

### <span id="F-COMPILE-FILE-PATHNAME">函数 COMPILE-FILE-PATHNAME</span>

* 语法(Syntax):

        compile-file-pathname input-file &key output-file &allow-other-keys => pathname

* 参数和值(Arguments and Values):

        input-file---一个路径名标识符. (对于未指定成员的填充取自于 *default-pathname-defaults*.)
        output-file---一个路径名标识符. 默认是具体实现定义的.
        pathname---一个路径名.

* 描述(Description):

        返回这个 compile-file 会写入的路径名, 如果给定相同参数的话.

        这个 output-file 的默认值取自于合并 input-file 和 *default-pathname-defaults* 的值所产生的路径名, 除了那个 type 成员应该为适当的具体实现定义用于编译后文件的默认类型.

        如果 input-file 是一个逻辑路径名并且 output-file 没有被提供, 那么结果也是一个逻辑路径名. 如果 input-file 是一个逻辑路径名, 它会被转化为一个物理路径名, 就像是通过 calling translate-logical-pathname 一样. 如果 input-file 是一个流, 那个流可以是打开的或关闭的. compile-file-pathname 在一个文件被关闭后返回和那个文件开始时返回的相同的路径名. 如果 input-file 是一个用 make-two-way-stream, make-echo-stream, make-broadcast-stream, make-concatenated-stream, make-string-input-stream, make-string-output-stream 创建的流那么就是一个错误.

        如果一个具体实现支持给 compile-file 的额外参数, compile-file-pathname 必须接受相同的参数.

* 示例(Examples):

        见 logical-pathname-translations.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 input-file 或 output-file 是通配符, 那么就会发出一个 file-error 类型的错误.

* 也见(See Also):

        compile-file, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None.

### <span id="F-LOAD">函数 LOAD</span>

* 语法(Syntax):

        load filespec &key verbose print if-does-not-exist external-format
        => generalized-boolean

* 参数和值(Arguments and Values):

        filespec---一个流, 或者一个路径名标识符. 默认值取自于 *default-pathname-defaults*.
        verbose---一个广义 boolean. 默认是 *load-verbose* 的值.
        print---一个广义 boolean. 默认是 *load-print* 的值.
        if-does-not-exist---一个广义 boolean. 默认是 true.
        external-format---一个外部文件格式标识符. 默认是 :default.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        load 加载由 filespec 命名的文件到这个 Lisp 环境中.

        一个源码文件和一个编译后文件的区分方式是依赖于具体实现的. 如果这个文件说明是不完整的并且匹配的源码文件和编译后文件都存在, 那么这些文件中的哪一个被选择是依赖于具体实现的.

        如果 filespec 是一个流, load 确定流的种类并且直接从那个流中加载. 如果 filespec 是一个逻辑路径名, 它会被转化为一个物理路径名, 就像是通过调用 translate-logical-pathname 一样.

        load 顺序依次执行在这个由 filespec 命名的文件中它遇到的每一个表达式形式. 如果这个文件是一个源码文件并且这个具体实现选择去执行隐式的编译, load 必须按章节 3.2.3.1 (Processing of Top Level Forms) 中描述的那样识别顶层表达式形式并且安排每一个顶层表达式形式在开始下一个隐式编译前被执行. (注意, 然而, load 处理那个 eval-when 表达式形式是由 :execute 情况控制的.)

        如果 verbose 是 true, compile-file 用一个注释的形式打印一个信息 (换句话说, 用一个前导分号) 到标准输出来表示什么文件要被编译以及其他有用的信息. 如果 verbose 是 false, compile-file 不打印这个信息.

        如果 print 是 true, load 递增地打印信息到标准输出来展示这个加载的进度. 对于一个源码文件, 这个信息可能意味着这个文件中的每一个表达式形式产生的值在这些值返回时被打印. 对于一个编译后文件, 打印到东西可能不会准确反映这个源码文件的内容, 但有些信息通常是打印出来的. 如果 print 是 false, load 不会打印这个信息.

        如果这个由 filespec 命名的文件被成功加载, load 返回 true.

        如果这个文件不存在, 采取依赖于 if-does-not-exist 的特定动作: 如果它是 nil, load 返回 nil; 否则, load 发出一个错误.

        这个 external-format 指定了当打开这个文件时要被使用的外部文件格式 (见函数 open), 除了当 filespec 命名的文件是一个编译后文件时, 这个 external-format 会被忽略. compile-file 和 load 以一种依赖于具体实现的方式互操作, 来确保这个源码文件被文件编译器以给定外部文件格式处理时, 在这个源码文件中引用的字符的相似性的保留, 不管这个 external-format 的值在编译后文件被加载时是什么.

        load 绑定 *readtable* 和 *package* 为它们在处理这个文件之前的值.

        *load-truename* 被 load 绑定来持有那个要被加载的文件的路径名的真实名字.

        *load-pathname* 被 load 绑定来持有一个路径名, 这个路径名表示 filespec 和默认值合并. 这也就是说, (pathname (merge-pathnames filespec)).

* 示例(Examples):

    ```LISP
    ;Establish a data file...
    (with-open-file (str "data.in" :direction :output :if-exists :error)
      (print 1 str) (print '(setq a 888) str) t)
    =>  T
    (load "data.in") =>  true
    a =>  888
    (load (setq p (merge-pathnames "data.in")) :verbose t)
    ; Loading contents of file /fred/data.in
    ; Finished loading /fred/data.in
    =>  true
    (load p :print t) 
    ; Loading contents of file /fred/data.in
    ;  1
    ;  888
    ; Finished loading /fred/data.in
    =>  true

    ;----[Begin file SETUP]----
    (in-package "MY-STUFF")
    (defmacro compile-truename () `',*compile-file-truename*)
    (defvar *my-compile-truename* (compile-truename) "Just for debugging.")
    (defvar *my-load-pathname* *load-pathname*)
    (defun load-my-system ()
      (dolist (module-name '("FOO" "BAR" "BAZ"))
        (load (merge-pathnames module-name *my-load-pathname*))))
    ;----[End of file SETUP]----

    (load "SETUP")
    (load-my-system)
    ```

* 受此影响(Affected By):

        具体实现, 以及主机计算机的文件系统.

* 异常情况(Exceptional Situations):

        如果 :if-does-not-exist 被提供并且是 true, 或者没有被提供, 那么如果这个由 filespec 命名的文件不存在或者如果这个文件系统不能处理这个请求的操作, 那么就会发出一个 file-error 类型的错误.

        如果 (wild-pathname-p filespec) 返回 true, 那么就可能发出一个 file-error 类型的错误.

* 也见(See Also):

        error, merge-pathnames, *load-verbose*, *default-pathname-defaults*, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None.

### <span id="M-WITH-COMPILATION-UNIT">宏 WITH-COMPILATION-UNIT</span>

* 语法(Syntax):

        with-compilation-unit ([[option]]) form* => result*

        option::= :override override

* 参数和值(Arguments and Values):

        override---一个广义 boolean; 求值的. 默认是 nil.
        forms---一个隐式 progn.
        results---由这些表达式形式 forms 返回的值.

* 描述(Description):

        从左到右执行表达式形式 forms. 在这个 with-compilation-unit 的动态环境中, 被这个编译器推迟到这个编译结束的动作会被推迟到这个对 with-compilation-unit 的最外部调用的结束.

        允许的选项集合可能被具体实现扩展, 但是仅有的标准关键字是 :override.

        如果是动态嵌套, 那么只有那个更外部的对 with-compilation-unit 的调用有效, 除非和 :override 关联的值是 true, 在这个情况下警告只会被推迟到最内部调用的结束, 而 override 是 true.

        函数 compile-file 提供了

        (with-compilation-unit (:override nil) ...)

        围绕在它的代码周围的效果.

        任何依赖于具体实现的扩展只能被提供作为通过使用依赖于具体实现的关键字的显式程序员请求的结果. 具体实现禁止去为这个没有涉及关键字或只有关键字 :override 的宏的使用添加额外的意义.

* 示例(Examples):

        如果一个具体实现会正常地推迟特定种类的警告, 例如关于未定义函数的警告, 到这个编译单元的结束 (例如一个文件), 那么下面这个示例展示了如果使这些警告被推迟到几个文件的编译结束.

    ```LISP
    (defun compile-files (&rest files)
      (with-compilation-unit ()
        (mapcar #'(lambda (file) (compile-file file)) files)))

    (compile-files "A" "B" "C")
    ```

        但是注意, 如果具体实现没有正常地推迟任何警告, with-compilation-unit 的使用可能没有任何效果.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compile, compile-file

* 注意(Notes): None.

### <span id="V-FEATURES">变量 *FEATURES*</span>

* 值类型(Value Type):

        一个 proper 列表.

* 初始值(Initial Value):

        依赖于具体实现的.

* 描述(Description):

        这个 *features* 的值被称为特性列表. 它是一个被称为特性的符号的列表, 这些符号对应于这个实现或环境的某个方面.

        大部分特性有着依赖于具体实现的意义; 特性名称的含义如下:

        :cltl1

            如果存在, 表示这个 LISP 包符合 1984 规范 Common Lisp: The Language. 对于一个符合规范的实现, 可能但不是必须有这个特性, 因为这个规范指定它的符号在 COMMON-LISP 包中, 而不是 LISP 包.

        :cltl2

            如果存在, 表示这个实现符合 Common Lisp: The Language, Second Edition. 这个特性一定不会出现在任何符合规范的实现中, 因为那个文档的符合性和这个规范的符合性不兼容. 但是, 这个规范保留了这个名称, 以便帮助程序区分符合该文档的实现和符合该规范的实现.

        :ieee-floating-point

            如果存在, 表示这个具体实现符合 IEEE Standard for Binary Floating-Point Arithmetic 的要求.

        :x3j13

            如果存在, 表示这个实现符合此规范的某些特定工作草案, 或者符合某些特性子集, 这些特性近似于对该规范可能包含的内容的理念. 一个符合规范的实现可能或可能不会包含这样的特性. (这个特性主要是作为一种权宜之计, 以便在标准草案可用之前为实现者提供一些可用的东西, 来阻止他们过早地引入 :draft-ansi-cl 和 :ansi-cl 特性.)

        :draft-ansi-cl

            如果存在, 表示这个实现符合这个规范的第一个完整草案, 它在 1992 年被公开复审. 一个有着 :draft-ansi-cl-2 或 :ansi-cl 特性的符合规范的实现不允许去保留这个 :draft-ansi-cl 特性, 因为在第一个草案之后进行了不兼容的修改.

        :draft-ansi-cl-2

            如果存在, 表示这个规范的第二个完整草案已经公开复审, 并且这个实现符合这个规范. (如果产生另外的公开复审草案, 这个关键字会继续与引用第二个草案, 而额外的关键字会被添加来标识这些后面的草案的符合性. 同样地, 这个关键字的意义可以被依赖不随时间改变). 如果最终被认可的标准和这个标准草案不兼容, 一个有着 :ansi-cl 特性的符合规范的实现只允许去保留 :draft-ansi-cl 特性.

        :ansi-cl

            如果存在, 表示这个规范已经被 ANSI 采用作为官方标准, 并且这个实现符合了.

        :common-lisp

            对于有着 :x3j13, :draft-ansi-cl, 或 :ansi-cl 中的一个或多个特性的任何实现, 这个特性也必须出现在 *features* 中. 它还应该出现在具有 :cltl1 或 :cltl2 特性的实现中, 但是这个规范不能强制这样的行为. 其目的是, 该特性应该识别名为 "Common Lisp" 的语言家族, 而不是该家族中的某些特定方言.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 也见(See Also):

        章节 1.5.2.1.1 (Use of Read-Time Conditionals), 章节 2.4 (Standard Macro Characters)

* 注意(Notes):

        被 #+ 和 #- 读取器宏所使用的 *features* 的值.

        在特性列表中的符号可能在任何包中, 但是实际上它们通常在 KEYWORD 包中. 这是因为 KEYWORD 是在 #+ 和 #- 读取器宏中读取特性表达式默认使用的包. 需要在一个包 P 中 (而不是 KEYWORD) 命名一个特性的代码可以通过显式使用 P 的包前缀来完成, 但是主意这样的代码必须确保这个包 P 存在, 以便读取特性表达式---即使在特性表达式预期失败的情况下也是如此.

        通常认为, 具体实现包含一个或多个标识特定实现的特性是明智的, 这样就可以写出条件表达式来区分一个实现的特性和另一个实现的特性. 由于特性通常是 KEYWORD 包中的符号, 在那里名字冲突可能很容易发生, 并且由于没有设计独特已定义的机制去决定谁有权使用哪些符号出于什么原因, 一个保守的策略是更喜欢使用来源于自己的公司或产品名称的名字, 因为那些名字通常是商标, 因此不太可能无意中被另一个实现使用.

### <span id="V-CF-TRUENAME-PATHNAME">变量 *COMPILE-FILE-PATHNAME*, *COMPILE-FILE-TRUENAME*</span>

* 值类型(Value Type):

        这个 *compile-file-pathname* 的值总是为一个路径名或 nil. 这个 *compile-file-truename* 的值总是为一个物理路径名或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        在对 compile-file 的调用期间, *compile-file-pathname* 被绑定为给 compile-file 的第一个参数所表示的路径名和默认值合并的路径名; 就是说, 它被绑定为 (pathname (merge-pathnames input-file)). 在相同的时间间隔期间, *compile-file-truename* 被绑定为这个要被编译的文件的真实名字.

        在其他时间, 这些变量的值都是 nil.

        如果在 compile-file 正在进行时进入一个 break loop, 那么这些变量是否保留在进入 break loop 之前的值, 或者它们是否被绑定为 nil, 都取决于具体实现.

        如果尝试去赋值或绑定这些变量的任意一个, 后果是未定义的.

* 示例(Examples): None.

* 受此影响(Affected By):

        文件系统.

* 也见(See Also):

        compile-file

* 注意(Notes): None.

### <span id="V-LOAD-PATHNAME-TRUENAME">变量 *LOAD-PATHNAME*, *LOAD-TRUENAME*</span>

* 值类型(Value Type):

        这个 *load-pathname* 的值总是为一个路径名或 nil. 这个 *load-truename* 的值总是为一个物理路径名或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        在一个对 load 调用期间, *load-pathname* 被绑定为给 load 的第一个参数所表示的路径名和默认值合并的路径名; 这也就是说, 它被绑定为 (pathname (merge-pathnames filespec)). 在相同的时间间隔期间, *load-truename* 被绑定为这个要被处理的文件的真是名字.

        在其他时间, 这些变量的值都是 nil.

        如果在 load 正在进行时进入一个 break loop, 那么这些变量是否保留在进入 break loop 之前的值, 或者它们是否被绑定为 nil, 都取决于具体实现.

        如果尝试去赋值或绑定这些变量的任意一个, 后果是未定义的.

* 示例(Examples): None.

* 受此影响(Affected By):

        文件系统.

* 也见(See Also):

        load

* 注意(Notes): None.

### <span id="V-COMPILE-PRINT-VERBOSE">变量 *COMPILE-PRINT*, *COMPILE-VERBOSE*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        依赖于具体实现.

* 描述(Description):

        这个 *compile-print* 的值是给 compile-file 的 :print 参数的默认值. 这个 *compile-verbose* 的值是给 compile-file 的 :verbose 参数的默认值.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 也见(See Also):

        compile-file

* 注意(Notes): None.

### <span id="V-LOAD-PRINT-VERBOSE">变量 *LOAD-PRINT*, *LOAD-VERBOSE*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        这个 *load-print* 的初始值是 false. 这个 *load-verbose* 的初始值是依赖于具体实现的.

* 描述(Description):

        这个 *load-print* 的值是给 load 的 :print 参数的默认值. 这个 *load-verbose* 的值是给 load 的 :verbose 参数的默认值.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 也见(See Also):

        load

* 注意(Notes): None.

### <span id="V-MODULES">变量 *MODULES*</span>

* 值类型(Value Type):

        一个字符串列表.

* 初始值(Initial Value):

        依赖于具体实现.

* 描述(Description):

        这个 *modules* 的值是一个已经被加载到当前 Lisp 镜像中的模块名称列表.

* 示例(Examples): None.

* 受此影响(Affected By):

        provide

* 也见(See Also):

        provide, require

* 注意(Notes):

        这个 *modules* 已经被废弃.

### <span id="F-PROVIDE-REQUIRE">函数 PROVIDE, REQUIRE</span>

* 语法(Syntax):

        provide module-name => implementation-dependent

        require module-name &optional pathname-list => implementation-dependent

* 参数和值(Arguments and Values):

        module-name---一个字符串标识符.
        pathname-list---nil, 或者一个非空路径名标识符列表的标识符. 默认是 nil.

* 描述(Description):

        如果 module-name 还没有出现在由 *modules* 持有的列表中, 那么 provide 添加这个 module-name 到该列表中.

        require 检验 module-name 在由 *modules* 持有的列表中的存在性. 如果它存在, require 立即返回. 否则, 尝试去加载一个如下文件的适当集合: 这个 pathname-list 参数, 如果不是 nil, 就指定一个要被从左到右依次加载的路径名列表. 如果这个 pathname-list 是 nil, 一个依赖于具体实现的机制会被调用来加载名为 module-name 的模块; 如果没有这样的模块可以被加载, 会发出一个 error 类型的错误.

        两个函数都使用 string= 来检测一个 module-name 的存在性.

* 示例(Examples):

    ```LISP
    ;;; This illustrates a nonportable use of REQUIRE, because it
    ;;; depends on the implementation-dependent file-loading mechanism.

    (require "CALCULUS")

    ;;; This use of REQUIRE is nonportable because of the literal 
    ;;; physical pathname.  

    (require "CALCULUS" "/usr/lib/lisp/calculus")

    ;;; One form of portable usage involves supplying a logical pathname,
    ;;; with appropriate translations defined elsewhere.

    (require "CALCULUS" "lib:calculus")

    ;;; Another form of portable usage involves using a variable or
    ;;; table lookup function to determine the pathname, which again
    ;;; must be initialized elsewhere.

    (require "CALCULUS" *calculus-module-pathname*)
    ```

* 副作用(Side Effects):

        provide 修改 *modules*.

* 受此影响(Affected By):

        The specific action taken 由 require 采取的特定通过是受对 provide 的调用影响 (或者, 一般而言, 任何对 *modules* 的值的改变).

* 异常情况(Exceptional Situations):

        如果 module-name 不是一个字符串标识符, 那么就应该发出一个 type-error 类型的错误.

        如果 require 由于一个在和文件系统交互时的问题而未完成那个请求的操作, 那么就会发出一个 file-error 类型的错误.

        如果在 pathname-list 中的任意路径名是一个通配符路径名的标识符, 那么就会发出一个 file-error 类型的错误.

* 也见(See Also):

        *modules*, 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

        函数 provide 和 require 已经被废弃.

        如果一个模块由单独的包组成, 照惯例这个包和模块名是相同的.