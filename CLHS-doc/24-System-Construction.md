# 24. 系统构造

> * 24.1 [系统构造的概念](#SystemConstructionConcepts)
> * 24.2 [系统构造的字典](#SystemConstructionDictionary)

## 24.1 <span id="SystemConstructionConcepts">系统构造的概念</span>

> * 24.1.1 [加载](#Loading)
> * 24.1.2 [特性](#Features)

### 24.1.1 <span id="Loading">加载</span>

去加载(load)一个文件[file]就是去把它的内容当作代码[code]并且执行[execute]那个代码[code]. 这个文件[file]可能包含源代码[source code]或编译后的代码[compiled code].

一个包含源代码[source code]的文件[file]称为源码文件[source file]. 加载一个源码文件[source file]通过顺序读取[read[2]]这个文件中的表达式形式[forms], 在读取[read]后立即求值其中的每一个来完成的.

一个包含编译后代码[compiled code]的文件[file]被称为一个编译后的文件[compiled file]. 加载一个编译后的文件[compiled file]类似于加载一个源码文件[source file], 除了这个文件[file]不会包含文本而是一种由编译器[compiler]创建的预处理表达式[expression]的依赖于具体实现[implementation-dependent]的表示. 通常, 一个编译后的文件[compiled file]可以比一个源码文件[source file]更快地加载. 见章节 3.2 (编译).

区分源码文件[source file]和编译后文件[compiled file]的方式是依赖于具体实现的[implementation-dependent].

### 24.1.2 <span id="Features">特性</span>

一个特性[feature]是 Common Lisp、具体实现[implementation]或者环境[environment]的方面或属性. 一个特性[feature]由一个符号[symbol]来标识.

当且仅当一个命名特性[feature]的符号[symbol]是变量[variable] \*features* 持有的列表[list]的一个元素[element]时, 就说这个特性[feature]出现[present]在一个 Lisp 镜像[Lisp image]中, 这个列表被称为特性列表[features list].

#### 24.1.2.1 特性表达式

称为特性表达式[feature expression]的特性[feature]的 boolean 组合被 #+ 和 #- 读取器宏[reader macro]用来指导表达式[expression]被 Lisp 读取器[Lisp reader]选择性读取.

解释一个特性表达式[feature expression]的规则如下:

特性[feature]

    如果命名一个特性[feature]的符号[symbol]被用作一个特性表达式[feature expression], 如果那个特性[feature]存在[present], 那么这个特性表达式[feature]成功; 否则就是失败.

(not feature-conditional)

    如果一个 not 特性表达式[feature expression]的参数 feature-conditional 失败了, 那么这个 not 特性表达式成功; 否则, 它失败.

(and feature-conditional*)

    如果一个 and 特性表达式[feature expression]的所有参数都成功, 那么这个 and 特性表达式成功; 否则, 它就失败.

(or feature-conditional*)

    如果一个 or 特性表达式[feature expression]的任意参数成功, 那么这个 or 特性表达式成功; 负责, 它失败.


##### 24.1.2.1.1 特性表达式的示例

比如, 假设在实现[implementation] A 中, 存在[present]特性[feature] spice 和 perq, 但是特性[feature] lispm 不存在[present]; 在实现[implementation] B 中, 存在[present]特性[feature] lispm, 但是不存在[present]特性[feature] spice 和 perq; 并且在实现[implementation] C 中, 特性[feature] spice, lispm, 或 perq 都不存在[present]. 下一段展示了一些简单表达式[expression], 以及它们在这些实现[implementation]中如何会被读取[read[2]].

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

        input-file---一个路径名标识符[pathname designator]. (未指定的成员的默认填充是取自 *default-pathname-defaults*.)
        output-file---一个路径名标识符[pathname designator]. 默认值是具体实现定义的[implementation-defined].
        verbose---一个广义 boolean [generalized boolean]. 默认是 *compile-verbose* 的值[value].
        print---一个广义 boolean [generalized boolean]. 默认是 *compile-print* 的值[value].
        external-format---一个外部文件格式标识符[external file format designator]. 默认是 :default.
        output-truename---一个路径名[pathname] (输出文件[file]的真实名字 truename), 或 nil.
        warnings-p---一个广义 boolean [generalized boolean].
        failure-p---一个广义 boolean [generalized boolean].

* 描述(Description):

        compile-file 把由 input-file 指定的文件的内容转化为依赖于具体实现的[implementation-dependent]二进制数据, 放置在由 output-file 指定的文件中.

        这个 input-file 引用的文件[file]应该是一个源码文件[source file]. output-file 可以被用于指定一个输出路径名[pathname]; 编译后的代码[compiled code]要被输出到的编译后文件[compiled file]的实际路径名[pathname]就像是通过调用 compile-file-pathname 计算的一样.

        如果 input-file 或 output-file 是一个逻辑路径名[logical pathname], 那么它会像调用 translate-logical-pathname 一样被转化为一个物理路径名[physical pathname].

        如果 verbose 是 true, compile-file 用一个注释的形式打印一条信息 (换句话说, 用一个前导分号[semicolon]) 到标准输出[standard output]来表示什么文件[file]要被编译以及其他有用的信息. 如果 verbose 是 false, compile-file 不打印这个信息.

        如果 print 是 true, 关于这个要被编译的文件中的顶层表达式形式[top level form]的信息会被打印到标准输出[standard output]. 确切地说, 打印的内容依赖于具体实现[implementation-dependent], 但是仍然会打印一些信息. 如果 print 是 nil, 没有信息要被打印.

        这个 external-format 指定了当打开这个文件时要被使用的外部文件格式[external file format]; 见函数 open. compile-file 以及 load 必须以这样一种方式互操作: 产生的编译后文件[compiled file]可以不重新指定一个外部文件格式[external file format]来加载; 见函数[function] load.

        compile-file 将 *readtable* 和 *package* 绑定为它们在处理这个文件之前的值.

        *compile-file-truename* 被 compile-file 绑定来持有这个要被编译的文件路径名[pathname]的真实名字[truename].

        *compile-file-pathname* 被 compile-file 绑定来持有传递给 compile-file 的第一个参数所表示的路径名[pathname], 这个路径名和默认的合并; 这也就是说, (pathname (merge-pathnames input-file)).

        这个编译后文件[compiled file]包含的编译后的函数[function]在这个编译后文件[compiled file]被加载入 Lisp 中时是可用的. 任何由编译器处理的函数定义, 包括 #'(lambda ...) 表达式形式和由 flet, labels 创建的局部函数定义以及 defun 表达式形式, 产生一个 compiled-function 类型[type]的对象[object].

        由 compile-file 返回的主值[primary value] output-truename 是那个输出文件的真实名字[truename], 或者如果这个文件没有被创建那么就是 nil.

        如果编译器没有检测到 error 或 warning 类型[type]的状况[condition], 第二个值[secondary value] warnings-p 就是 false, 否则就是 true.

        如果编译器没有检测到 error 或 warning 类型的状况 (除了 style-warning), 第三个值[tertiary value] failure-p 就是 false, 否则就是 true.

        关于这个文件编译器[file compiler]如何处理文件[file]的一般信息, 见章节 3.2.3 (文件编译).

        要被文件编译器编译[file compiler]的程序[program]必须只包含可外部化对象[externalizable object]; 关于这样的对象[object]的详细信息, 见章节 3.2.4 (编译后文件中的字面化对象). 关于如何去扩展可外部化对象[externalizable object]集合的信息, 见函数[function] make-load-form 和章节 3.2.4.4 (外部化对象的附加约束).

* 示例(Examples): None.

* 受此影响(Affected By):

        *error-output*, *standard-output*, *compile-verbose*, *compile-print*

        计算机的文件系统.

* 异常情况(Exceptional Situations):

        关于编译处理期间的错误检测的信息, 见章节 3.2.5 (编译器中的异常情况).

        如果 (wild-pathname-p input-file) 返回 true, 那么可能会发出一个 file-error 类型[type]的错误.

        如果尝试去打开一个用于输入的源码文件[source file]或者尝试去打开一个用于输出的编译后文件[compiled file]失败了, 那么就会发出一个 file-error 类型[type]的错误.

* 参见(See Also):

        compile, declare, eval-when, pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None.


### <span id="F-COMPILE-FILE-PATHNAME">函数 COMPILE-FILE-PATHNAME</span>

* 语法(Syntax):

        compile-file-pathname input-file &key output-file &allow-other-keys => pathname

* 参数和值(Arguments and Values):

        input-file---一个路径名标识符[pathname designator]. (对于未指定成员的填充取自于 *default-pathname-defaults*.)
        output-file---一个路径名标识符[pathname designator]. 默认是具体实现定义的[implementation-defined].
        pathname---一个路径名[pathname].

* 描述(Description):

        如果给定相同参数的话, 这个函数返回 compile-file 会写入的路径名[pathname].

        这个 output-file 的默认值取自于合并 input-file 和 *default-pathname-defaults* 的值[value]所产生的路径名[pathname], 除了那个 type 成员应该是适当的具体实现定义的[implementation-defined]用于编译后文件[compiled file]的默认类型.

        如果 input-file 是一个逻辑路径名[logical pathname]并且 output-file 没有被提供, 那么结果也是一个逻辑路径名[logical pathname]. 如果 input-file 是一个逻辑路径名[logical pathname], 它会像调用 translate-logical-pathname 一样转化为一个物理路径名. 如果 input-file 是一个流[stream], 那个流[stream]可以是打开的或关闭的. compile-file-pathname 在一个文件被关闭后返回和那个文件开始时返回的相同的路径名[pathname]. 如果 input-file 是一个用 make-two-way-stream, make-echo-stream, make-broadcast-stream, make-concatenated-stream, make-string-input-stream, make-string-output-stream 创建的流[stream], 那么就是一个错误.

        如果一个具体实现支持给 compile-file 的额外参数, 那么 compile-file-pathname 必须接受相同的参数.

* 示例(Examples):

        见 logical-pathname-translations.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 input-file 或 output-file 是通配符[wild], 那么就会发出一个 file-error 类型[type]的错误.

* 参见(See Also):

        compile-file, pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None.


### <span id="F-LOAD">函数 LOAD</span>

* 语法(Syntax):

        load filespec &key verbose print if-does-not-exist external-format
        => generalized-boolean

* 参数和值(Arguments and Values):

        filespec---一个流[stream], 或者一个路径名标识符[pathname designator]. 默认值取自于 *default-pathname-defaults*.
        verbose---一个广义 boolean [generalized boolean]. 默认是 *load-verbose* 的值[value].
        print---一个广义 boolean [generalized boolean]. 默认是 *load-print* 的值[value].
        if-does-not-exist---一个广义 boolean. 默认是 true.
        external-format---一个外部文件格式标识符[external file format designator]. 默认是 :default.
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        load 加载[load]由 filespec 命名的文件[name]到这个 Lisp 环境中.

        一个源码文件[source file]和一个编译后文件[compiled file]的区分方式是依赖于具体实现的[implementation-dependent]. 如果这个文件说明是不完整的并且匹配的源码文件[source file]和编译后文件[compiled file]都存在, 那么这些文件中的哪一个被 load 选择是依赖于具体实现的[implementation-dependent].

        如果 filespec 是一个流[stream], load 确定流[stream]的种类并且直接从那个流[stream]中加载. 如果 filespec 是一个逻辑路径名[logical pathname], 它会像调用 translate-logical-pathname 一样被转化为一个物理路径名[physical pathname].

        load 顺序依次执行在这个由 filespec 命名的文件[file]中它遇到的每一个表达式形式[form]. 如果这个文件[file]是一个源码文件[source file]并且这个具体实现[implementation]选择去执行隐式编译[implicit compilation], load 必须按章节 3.2.3.1 (顶层表达式形式的处理) 中描述的那样识别顶层表达式形式[top level form]并且安排每一个顶层表达式形式[top level form]在开始下一个隐式编译前被执行. (然而, 注意由 load 处理的 eval-when 表达式形式是由 :execute 情况控制的.)

        如果 verbose 是 true, compile-file 用一个注释的形式打印一个信息 (换句话说, 用一个前导分号[semicolon]) 到标准输出[standard output]来表示什么文件要被编译以及其他有用的信息. 如果 verbose 是 false, compile-file 不打印这个信息.

        如果 print 是 true, load 递增地打印信息到标准输出[standard output]来展示这个加载的进度. 对于一个源码文件[source file], 这个信息可能意味着这个文件[file]中的每一个表达式形式[form]产生的值[value]在这些值[value]返回时被打印. 对于一个编译后文件[compiled file], 打印到东西可能不会准确反映这个源码文件[source file]的内容, 但有些信息通常是打印出来的. 如果 print 是 false, load 不会打印这个信息.

        如果这个由 filespec 命名的文件被成功加载, load 返回 true.

        如果这个文件不存在, 采取依赖于 if-does-not-exist 的特定动作: 如果它是 nil, load 返回 nil; 否则, load 发出一个错误.

        这个 external-format 指定了当打开这个文件[file]时要被使用的外部文件格式[external file format] (见函数[file] open), 除了当 filespec 命名的文件[file]是一个编译后文件[complied file]时, 这个 external-format 会被忽略. compile-file 和 load 以一种依赖于具体实现的[implementation-dependent]方式互操作, 来确保这个源码文件[source file]被文件编译器[file compiler]以给定外部文件格式[external file format]处理时, 在这个源码文件[source file]中引用的字符[character]的相似性[similarity]的保留, 不管这个 external-format 的值在编译后文件[compiled file]被加载时是什么.

        load 将 *readtable* 和 *package* 绑定为它们在处理这个文件之前的值.

        *load-truename* 被 load 绑定[bound]来持有那个要被加载的文件路径名[pathname]的真实名字[truename].

        *load-pathname* 被 load 绑定[bound]来持有一个路径名[name], 这个路径名表示 filespec 和默认值合并的结果. 这也就是说, (pathname (merge-pathnames filespec)).

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

        如果 :if-does-not-exist 被提供并且是 true, 或者没有被提供, 那么如果这个由 filespec 命名的文件不存在或者文件系统[file system]不能处理这个请求的操作, 那么就会发出一个 file-error 类型[type]的错误.

        如果 (wild-pathname-p filespec) 返回 true, 那么就可能发出一个 file-error 类型[type]的错误.

* 参见(See Also):

        error, merge-pathnames, *load-verbose*, *default-pathname-defaults*, pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None.


### <span id="M-WITH-COMPILATION-UNIT">宏 WITH-COMPILATION-UNIT</span>

* 语法(Syntax):

        with-compilation-unit ([[option]]) form* => result*

        option::= :override override

* 参数和值(Arguments and Values):

        override---一个广义 boolean [generalized boolean]; 求值的. 默认是 nil.
        forms---一个隐式 progn [implicit progn].
        results---由这些表达式形式[form]返回的值[value].

* 描述(Description):

        从左到右执行表达式形式 forms. 在这个 with-compilation-unit 的动态环境[dynamic environment]中, 被编译器推迟到这个编译结束的动作会被推迟到这个对 with-compilation-unit 的最外部调用的结束.

        具体实现可以扩展允许的 options 选项集合, 但是仅有的标准[standardized]关键字是 :override.

        如果是动态嵌套, 那么只有那个更外部的对 with-compilation-unit 的调用有效, 除非和 :override 关联的值是 true, 在这个情况下警告只会被推迟到 override 为 true 的最内部调用的结束.

        函数 compile-file 提供了

        (with-compilation-unit (:override nil) ...)

        围绕在它的代码[code]周围的效果.

        任何依赖于具体实现[implementation-dependent]的扩展只能通过使用依赖于具体实现[implementation-dependent]的关键字作为程序员显式请求的结果提供. 具体实现[implementation]禁止去为没有涉及关键字或只有关键字 :override 的这个宏的使用添加额外的意义.

* 示例(Examples):

        如果一个具体实现[implementation]会正常地推迟特定种类的警告(例如关于未定义函数的警告)到这个编译单元的结束 (例如一个文件[file]), 那么下面这个示例展示了如何将这些警告推迟到几个文件的编译结束.

    ```LISP
    (defun compile-files (&rest files)
      (with-compilation-unit ()
        (mapcar #'(lambda (file) (compile-file file)) files)))

    (compile-files "A" "B" "C")
    ```

        但是注意, 如果具体实现没有正常地推迟任何警告, 使用 with-compilation-unit 可能没有任何效果.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        compile, compile-file

* 注意(Notes): None.


### <span id="V-FEATURES">变量 *FEATURES*</span>

* 值类型(Value Type):

        一个正规列表[proper list].

* 初始值(Initial Value):

        依赖于具体实现的[implementation-dependent].

* 描述(Description):

        这个 *features* 的值[value]被称为特性列表[feature list]. 它是一个被称为特性[feature]的符号[symbol]列表[list], 这些符号对应于具体实现[implementation]或环境[environment]的某个方面.

        大部分特性[feature]有着依赖于具体实现[implementation-dependent]的意义; 特性名称被赋予以下含义:

        :cltl1

            如果出现, 表示这个 LISP 包声称符合[purports to conform] 1984 Common Lisp : The Language 规范. 对于一个符合规范的实现[conforming implementation], 可能但不是必须有这个特性, 因为这个规范指定它的符号[symbol]在 COMMON-LISP 包中, 而不是 LISP 包.

        :cltl2

            如果出现, 表示这个实现声称符合[purports to conform] Common Lisp: The Language, Second Edition. 这个特性一定不会出现在任何符合规范的实现[conforming implementation]中, 因为那个文档的规范性和这个说明的规范性不兼容. 但是, 这个说明保留了这个名称, 以便帮助程序区分符合该文档的实现和符合该这个说明的实现.

        :ieee-floating-point

            如果出现, 表示这个具体实现声称符合[purports to conform] IEEE Standard for Binary Floating-Point Arithmetic 的要求.

        :x3j13

            如果出现, 表示这个实现符合此规范的某些特定工作草案, 或者符合某些特性子集, 这些特性近似于对该规范可能包含的内容的理念. 一个符合规范的实现可能或可能不会包含这样的特性. (这个特性主要是作为一种权宜之计, 以便在标准草案可用之前为实现者提供一些可用的东西, 来阻止他们过早地引入 :draft-ansi-cl 和 :ansi-cl 特性.)

        :draft-ansi-cl

            如果出现, 表示这个实现[implementation]声称符合[purports to conform]这个规范的第一个完整草案, 它在 1992 年被公开复审. 一个有着 :draft-ansi-cl-2 或 :ansi-cl 特性的符合规范的实现[conforming implementation]不允许去保留这个 :draft-ansi-cl 特性[feature], 因为在第一个草案之后进行了不兼容的修改.

        :draft-ansi-cl-2

            如果出现, 表示这个规范的第二个完整草案已经公开复审, 并且这个实现[implementation]声称符合[purports to conform]这个规范. (如果产生另外的公开复审草案, 这个关键字会继续与引用第二个草案, 而额外的关键字会被添加来标识后面的这些草案的规范性. 同样地, 这个关键字的意义可以被认为不随时间改变). 如果最终被认可的标准和这个标准草案不兼容, 一个有着 :ansi-cl 特性[feature]的符合规范的实现[conforming implementation]只允许去保留 :draft-ansi-cl 特性[feature].

        :ansi-cl

            如果出现, 表示这个规范已经被 ANSI 采用作为官方标准, 并且这个实现[implementation]声称符合[purports to conform]了.

        :common-lisp

            这个特性必须出现在任何有着 :x3j13, :draft-ansi-cl, 或 :ansi-cl 中的一个或多个特性的实现的 *features* 中. 它还应该出现在具有 :cltl1 或 :cltl2 特性的实现中, 但是这个规范不能强制这样的行为. 其目的是, 该特性应该识别名为 "Common Lisp" 的语言家族, 而不是该家族中的某些特定方言.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 参见(See Also):

        章节 1.5.2.1.1 (读取时条件的使用), 章节 2.4 (标准宏字符)

* 注意(Notes):

        这个 *features* 的值[value]被 #+ 和 #- 读取器语法所使用.

        特性列表[feature list]中的符号[symbol]可能在任何包[package]中, 但是实际上它们通常在 KEYWORD 包中. 这是因为 KEYWORD 是在 #+ 和 #- 读取器宏[reader macro]中读取[read[2]]特性表达式[feature expression]默认使用的包[package]. 需要在一个包[pacakge] P 中 (除了 KEYWORD 以外) 命名一个特性[feature[2]]的代码[code]可以通过显式使用 P 的包前缀[package prefix]来完成, 但是注意这样的代码必须确保这个包[package] P 存在, 以便读取[read[2]]特性表达式[feature expression]---即使在特性表达式[feature expression]预期失败的情况下也是如此.

        通常认为, 具体实现[implementation]包含一个或多个标识特定实现[implementation]的特性[feature]是明智的, 这样就可以写出条件表达式来区分一个实现[implementation]的特质和另一个实现的那些特质. 由于特性通常是 KEYWORD 包中的符号[symbol], 在那里名字冲突可能很容易发生, 并且由于没有设计独特已定义的机制去决定谁有权使用哪些符号[symbol]出于什么原因, 一个保守的策略是更喜欢使用来源于自己的公司或产品名称的名字, 因为那些名字通常是商标, 因此不太可能无意中被另一个实现[implementation]使用.


### <span id="V-CF-TRUENAME-PATHNAME">变量 *COMPILE-FILE-PATHNAME*, *COMPILE-FILE-TRUENAME*</span>

* 值类型(Value Type):

        这个 *compile-file-pathname* 的值[value]总是为一个路径名[pathname]或 nil. 这个 *compile-file-truename* 的值[value]总是为一个物理路径名[physical pathname]或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        在对 compile-file 的调用期间, *compile-file-pathname* 被绑定[bound]为 compile-file 的第一个参数所表示的路径名[pathname]和默认值合并的路径名[pathname]; 就是说, 它被绑定[bound]为 (pathname (merge-pathnames input-file)). 在相同的时间间隔期间, *compile-file-truename* 被绑定[bound]为这个要被编译的文件[file]的真实名字[truename].

        在其他时间, 这些变量的值[value]都是 nil.

        如果在 compile-file 正在进行时进入一个中断循环[break loop], 那么这些变量[variable]是否保留在进入中断循环[break loop]之前的值[value]还是被绑定[bound]为 nil, 都依赖于具体实现[implementation-dependent].

        如果尝试去对这些变量的任意一个赋值[assign]或绑定[bind], 后果是未定义的.

* 示例(Examples): None.

* 受此影响(Affected By):

        文件系统[file system].

* 参见(See Also):

        compile-file

* 注意(Notes): None.


### <span id="V-LOAD-PATHNAME-TRUENAME">变量 *LOAD-PATHNAME*, *LOAD-TRUENAME*</span>

* 值类型(Value Type):

        这个 *load-pathname* 的值[value]总是为一个路径名[pathname]或 nil. 这个 *load-truename* 的值[pathname]总是为一个物理路径名[physical pathname]或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        在一个对 load 调用期间, *load-pathname* 被绑定[bound]为 load 的第一个参数所表示的路径名[pathname]和默认值合并的路径名[pathname]; 这也就是说, 它被绑定[bound]为 (pathname (merge-pathnames filespec)). 在相同的时间间隔期间, *load-truename* 被绑定[bound]为这个要被加载的文件[file]的真实名字[truename].

        在其他时间, 这些变量[variable]的值[value]都是 nil.

        如果在 load 正在进行时进入一个中断循环[break loop], 那么这些变量[variable]是否保留在进入中断循环[break loop]之前的值[value]还是被绑定[bound]为 nil, 都依赖于具体实现[implementation-dependent].

        如果尝试去对这些变量的任意一个赋值[assign]或绑定[bind], 后果是未定义的.

* 示例(Examples): None.

* 受此影响(Affected By):

        文件系统[file system].

* 参见(See Also):

        load

* 注意(Notes): None.


### <span id="V-COMPILE-PRINT-VERBOSE">变量 *COMPILE-PRINT*, *COMPILE-VERBOSE*</span>

* 值类型(Value Type):

        一个广义 boolean [generalized boolean].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这个 *compile-print* 的值[value]是给 compile-file 的 :print 参数[argument]的默认值. 这个 *compile-verbose* 的值[value]是给 compile-file 的 :verbose 参数[argument]的默认值.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 参见(See Also):

        compile-file

* 注意(Notes): None.


### <span id="V-LOAD-PRINT-VERBOSE">变量 *LOAD-PRINT*, *LOAD-VERBOSE*</span>

* 值类型(Value Type):

        一个广义 boolean [generalized boolean].

* 初始值(Initial Value):

        这个 *load-print* 的初始值[value]是 false. 这个 *load-verbose* 的初始值[value]是依赖于具体实现的[implementation-dependent].

* 描述(Description):

        这个 *load-print* 的值[value]是给 load 的 :print 参数[argument]的默认值. 这个 *load-verbose* 的值[value]是给 load 的 :verbose 参数[argument]的默认值.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 参见(See Also):

        load

* 注意(Notes): None.


### <span id="V-MODULES">变量 *MODULES*</span>

* 值类型(Value Type):

        一个字符串[string]列表[list].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这个 *modules* 的值[value]是一个已经被加载到当前 Lisp 镜像[Lisp image]中的模块名称列表.

* 示例(Examples): None.

* 受此影响(Affected By):

        provide

* 参见(See Also):

        provide, require

* 注意(Notes):

        这个 *modules* 变量已经被废弃.


### <span id="F-PROVIDE-REQUIRE">函数 PROVIDE, REQUIRE</span>

* 语法(Syntax):

        provide module-name => 依赖于具体实现[implementation-dependent]

        require module-name &optional pathname-list => 依赖于具体实现[implementation-dependent]

* 参数和值(Arguments and Values):

        module-name---一个字符串标识符[string designator].
        pathname-list---nil, 或者一个非空[non-empty]路径名标识符[pathname designator]列表[list]的标识符[designator]. 默认是 nil.

* 描述(Description):

        如果 module-name 还没有出现在由 *modules* 持有的列表[list]中, 那么 provide 添加这个 module-name 到该列表[list]中.

        require 检验 module-name 在 *modules* 所持有的列表[list]中的存在性. 如果它存在, require 立即返回. 否则, 按照下面所述尝试去加载一个文件[file]的适当集合: 这个 pathname-list 参数, 如果非 nil [non-nil], 就指定一个要被从左到右依次加载的路径名[pathname]列表. 如果这个 pathname-list 是 nil, 一个依赖于具体实现[implementation-dependent]的机制会被调用来加载名为 module-name 的模块; 如果没有这样的模块可以被加载, 会发出一个 error 类型[type]的错误.

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

        由 require 采取的特定动作受 provide 调用影响 (或者, 一般而言, 任何对 *modules* 的值[value]的改变).

* 异常情况(Exceptional Situations):

        如果 module-name 不是一个字符串标识符[string designator], 那么就应该发出一个 type-error 类型[type]的错误.

        如果 require 由于一个和文件系统[file system]交互时的问题而未完成那个请求的操作, 那么就会发出一个 file-error 类型[type]的错误.

        如果在 pathname-list 中的任意路径名[pathname]是一个通配符[wild]路径名[pathname]的标识符[designator], 那么就会发出一个 file-error 类型[type]的错误.

* 参见(See Also):

        *modules*, 章节 19.1.2 (路径名作为文件名)

* 注意(Notes):

        函数 provide 和 require 已经被废弃.

        如果一个模块由单独的包[package]组成, 照惯例这个包和模块名是相同的.