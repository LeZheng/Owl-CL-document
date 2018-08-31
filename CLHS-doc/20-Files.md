# 20 文件

> * 20.1 [文件系统概念](#FileSystemConcepts)
> * 20.2 [文件的字典](#TheFilesDictionary)

## 20.1 <span id="FileSystemConcepts">文件系统概念</span>

这个章节描述到文件系统的 Common Lisp 接口. 这个接口所使用的模型假定这些文件由文件名来命名, 这样一个文件名可以由一个路径名对象来表示, 并且给定一个路径名, 可以构造一条流, 连接到它所代表的文件名的文件.

关于打开和关闭文件, 以及操作它们的内容的信息, 见章节 21 (Streams).

下面这段列出了可应用于文件和目录的一些操作符.

    compile-file  file-length      open            
    delete-file   file-position    probe-file      
    directory     file-write-date  rename-file     
    file-author   load             with-open-file  

    Figure 20-1. 文件和目录操作符

> * 20.1.1 [从流到路径名的强制转换](#CoercionStreamsPathnames)
> * 20.1.2 [在打开或关闭的流上的文件操作](#FileOptOpenClosedStreams)
> * 20.1.3 [真实的名字](#Truenames)

### 20.1.1 <span id="CoercionStreamsPathnames">从流到路径名的强制转换</span>

与文件相关联的流要么是文件流, 要么是 synonym-stream, 其目标是与文件相关联的流. 这样的流可以被用作路径名标识符.

通常, 当与文件相关联的流用作路径名标识符时, 它表示用来打开文件的路径名; 这可能是; 但不必须是文件的实际名称.

一些函数, 如 truename 和 delete-file, 以一种不同的方式强制把流转换到路径名, 在涉及到引用打开的实际文件时, 可能也可能不是最初打开的文件. 这样的特殊情况总是会被特别标记出来并且不会是默认的. 

### 20.1.2 <span id="FileOptOpenClosedStreams">在打开或关闭的流上的文件操作</span>

执行文件操作的许多函数都接受打开或关闭的流作为参数; 见章节 21.1.3 (Stream Arguments to Standardized Functions).

这些函数中, 下面这段列出来的这些区别对待打开和关闭的流.

    delete-file  file-author      probe-file  
    directory    file-write-date  truename    

    Figure 20-2. 区别对待打开和关闭的流的文件函数

由于文件系统对打开的流的处理可能在不同的实现之间有很大的不同, 但是对于某些函数来说, 关闭的流可能是最可靠的参数---特别是在下一段中的那些. 比如, 在某些文件系统中, 打开的文件是用临时的名称写的, 直到关闭时才重命名 和/或 隐藏. 通常情况下, 任何想要移植的代码都应该小心地使用这些函数.

    directory  probe-file  truename  

    Figure 20-3. 关闭的流可以更好工作的文件函数 


### 20.1.3 <span id="Truenames">真实的名字</span>

很多文件系统允许不止一个文件名来表示一个特定的文件.

即使在可能有多个名称的地方, 大多数文件系统都有一个在这种情况下生成规范文件名的惯例. 这样一个规范的文件名(或者表示这样一个文件名的路径名)被称为真实的名字(truename).

一个文件的真实的名字可能有别于这个文件的其他文件名, 因为存在符号链接, 版本号, 文件系统中的逻辑设备转换, Common Lisp 中的逻辑路径名转换或文件系统的其他因素.

对于每个文件来说, 文件的真实的名字通常是唯一的, 但不是必须是唯一的. 比如, 一个带有硬链接的 Unix 文件可以有多个真实的名字.

#### 20.1.3.1 真实的名字的示例

比如, 一个带有文件 PS:<JOE>FOO.TXT.1 和 PS:<JOE>FOO.TXT.2 的 DEC TOPS-20 系统可能允许第二个文件被引用为 PS:<JOE>FOO.TXT.0, 因为这个 ".0" 标记表示几个文件的 "最新" 版本. 在同一个文件系统中, 一个 "逻辑设备" "JOE:" 可能被用来引用 PS:<JOE> 并且这样一来名字 JOE:FOO.TXT.2 或 JOE:FOO.TXT.0 可能引用 PS:<JOE>FOO.TXT.2. 在所有这些情况中, 这个文件的真实的名字可能是 PS:<JOE>FOO.TXT.2.

如果一个文件是到另一个文件的符号链接 (在一个允许这东西的文件系统中), 跟在任何符号链接之后, truename 是该文件的规范名称 it is conventional for the truename to be the canonical name of the file after any symbolic links have been followed; 这就是说, 它是那个如果打开该文件的输入流, 内容将是可用的文件的规范名称.<!--TODO 待校对-->

在要去创建文件的情况下 (这就是说, 对这样一个文件打开的输出流), 这个文件的确切的真实名字知道这个流被关闭可能是不知道的. 在这个情况下, 对于这样一个流, 在它关闭前后函数 truename 可能返回不同的值. 事实上, 在它关闭前, 返回的名字在那个文件系统中可能不是一个有效的名字---比如, 当一个文件要被写入时, 即使在所有文件都有数字版本的文件系统中, 它可能有版本的 :newest 并且可能只在文件关闭时才会使用特定的数字值. 


## 20.2 <span id="TheFilesDictionary">文件的字典</span>

> * [函数 DIRECTORY](#F-DIRECTORY)
> * [函数 PROBE-FILE](#F-PROBE-FILE)
> * [函数 ENSURE-DIRECTORIES-EXIST](#F-ENSURE-DIRECTORIES-EXIST)
> * [函数 TRUENAME](#F-TRUENAME)
> * [函数 FILE-AUTHOR](#F-FILE-AUTHOR)
> * [函数 FILE-WRITE-DATE](#F-FILE-WRITE-DATE)
> * [函数 RENAME-FILE](#F-RENAME-FILE)
> * [函数 DELETE-FILE](#F-DELETE-FILE)
> * [状况类型 FILE-ERROR](#CT-FILE-ERROR)
> * [函数 FILE-ERROR-PATHNAME](#F-FILE-ERROR-PATHNAME)


### <span id="F-DIRECTORY">函数 DIRECTORY</span>

* 语法(Syntax):

        directory pathspec &key => pathnames

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符, 它可能包含通配符成员.
        pathnames---一个物理路径名列表.

* 描述(Description):

        确定文件系统中存在的文件是否具有匹配 pathspec 的名称, 并且返回一个对应那些文件的真实名字的新路径名列表.

        一个具体实现可以去扩展来接受给 directory 的具体实现定义的关键字参数.

* 示例(Examples): None.

* 受此影响(Affected By):

        主机计算机的文件系统.

* 异常情况(Exceptional Situations):

        如果尝试去获取一个目录列表不成功, 就会发出一个 file-error 类型的错误.

* 也见(See Also):

        pathname, logical-pathname, ensure-directories-exist, 章节 20.1 (文件系统概念), 章节 20.1.2 (在打开或关闭的流上的文件操作), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

        如果这个 pathspec 不是通配的, 产生的列表会包含零或一个元素.

        Common Lisp 在给 directory 的参数列表中指定 "&key", 即便没有定义给 directory 的标准关键字参数. 在符合规范的程序中可能使用 ":allow-other-keys t" 来忽略任何由程序传递的具体实现不支持的额外的关键字.

### <span id="F-PROBE-FILE">函数 PROBE-FILE</span>

* 语法(Syntax):

        probe-file pathspec => truename

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符.
        truename---一个物理路径名或 nil.

* 描述(Description):

        probe-file 测试一个文件是否存在.

        如果这里没有名为 pathspec 的文件, 那么 probe-file 返回 false, 否则返回这个 pathspec 的真实名字.

        如果这个 pathspec 标识符是一个打开的流, 这个 probe-file 产生它关联的文件的真实名字. 如果 pathspec 是一个流, 不管打开或是关闭的, 它会被强制转换为一个路径名, 就像是通过函数 pathname 一样.

* 示例(Examples): None.

* 受此影响(Affected By):

        主机计算机的文件系统.

* 异常情况(Exceptional Situations):

        如果 pathspec 是通配的, 那么应该发出一个 file-error 的错误.

        如果文件系统不能执行请求的操作, 那么应该发出一个 file-error 的错误.

* 也见(See Also):

        truename, open, ensure-directories-exist, pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 20.1.2 (在打开或关闭的流上的文件操作), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 

### <span id="F-ENSURE-DIRECTORIES-EXIST">函数 ENSURE-DIRECTORIES-EXIST</span>

* 语法(Syntax):

        ensure-directories-exist pathspec &key verbose => pathspec, created

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符.
        verbose---一个广义 boolean.
        created---一个广义 boolean.

* 描述(Description):

        检验这些包含了那个指定的文件的目录是否实际存在, 如果不存在就尝试去创建它们.

        如果那些包含目录不存在并且如果 verbose 是 true, 那么这个实现允许 (但不是必须) 去执行输出到标准输出中来说明哪些目录被创建. 如果那些包含目录存在, 或者如果 verbose 是 false, 这个函数不执行输出.

        主要的值是那个给定的 pathspec 这样一来这个操作符和直接和其他文件操作表达式组合起来. 如果创建了任何目录, 那么第二个值 created 就是 true.

* 示例(Examples): None.

* 受此影响(Affected By):

        主机计算机的文件系统.

* 异常情况(Exceptional Situations):

        如果这个 pathspec 的主机, 设备, 或目录部分是通配的, 那么就会发出一个 file-error 类型的错误.

        如果这个目录创建不成功, 那么就会发出一个 file-error 类型的错误; 如果发生了这个, 可能是这样的情况, 没有, 一部分, 或者所有被请求的创建实际上发生在文件系统中.

* 也见(See Also):

        probe-file, open, 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-TRUENAME">函数 TRUENAME</span>

* 语法(Syntax):

        truename filespec => truename

* 参数和值(Arguments and Values):

        filespec---一个路径名标识符.
        truename---一个物理路径名.

* 描述(Description):

        truename 尝试去找打由 filespec 表示的文件并且返回它的真实名字. 如果这个 filespec 标识符是一个打开的流, 就使用它关联的文件. 如果 filespec 是一个流, 不管这个流是打开还是关闭的, truename 都可以被使用. 允许 truename 在这个流被关闭后返回比这个流打开时更具体的信息. 如果 filespec 是一个路径名, 那么它表示被用来打开文件的名字. 这可能是, 但不必须是, 文件的实际名称.

* 示例(Examples):

    ```LISP
    ;; An example involving version numbers.  Note that the precise nature of
    ;; the truename is implementation-dependent while the file is still open.
    (with-open-file (stream ">vistor>test.text.newest")
      (values (pathname stream)
              (truename stream)))
    =>  #P"S:>vistor>test.text.newest", #P"S:>vistor>test.text.1"
    OR=>  #P"S:>vistor>test.text.newest", #P"S:>vistor>test.text.newest"
    OR=>  #P"S:>vistor>test.text.newest", #P"S:>vistor>_temp_._temp_.1"

    ;; In this case, the file is closed when the truename is tried, so the
    ;; truename information is reliable.
    (with-open-file (stream ">vistor>test.text.newest")
      (close stream)
      (values (pathname stream)
              (truename stream)))
    =>  #P"S:>vistor>test.text.newest", #P"S:>vistor>test.text.1"

    ;; An example involving TOP-20's implementation-dependent concept 
    ;; of logical devices -- in this case, "DOC:" is shorthand for
    ;; "PS:<DOCUMENTATION>" ...
    (with-open-file (stream "CMUC::DOC:DUMPER.HLP")
      (values (pathname stream)
              (truename stream)))
    =>  #P"CMUC::DOC:DUMPER.HLP", #P"CMUC::PS:<DOCUMENTATION>DUMPER.HLP.13"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果不能为给定的 filespec 在这个文件系统中定位到合适的文件, 或者如果这个文件系统没有执行这个请求的操作, 那么就会发出一个 file-error 类型的错误

        如果路径名是通配的, 那么就会发出一个 file-error 类型的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

        truename 可以用来解释由文件系统执行的任何文件名转换. 


### <span id="F-FILE-AUTHOR">函数 FILE-AUTHOR</span>

* 语法(Syntax):

        file-author pathspec => author

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符.
        author---一个字符串或 nil.

* 描述(Description):

        返回一个命名由 pathspec 指定的文件的作者的字符串, 如果不能确定作者的名字, 就返回 nil.

* 示例(Examples):

    ```LISP
    (with-open-file (stream ">relativity>general.text")
      (file-author s))
    =>  "albert"
    ```

* 受此影响(Affected By):

        主机计算机的文件系统.

        由 pathspec 命名的文件的其他用户.

* 异常情况(Exceptional Situations):

        如果 pathspec 是通配的, 那么应该发出一个 file-error 的错误.

        如果文件系统不能执行请求的操作, 那么应该发出一个 file-error 的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-FILE-WRITE-DATE">函数 FILE-WRITE-DATE</span>

* 语法(Syntax):

        file-write-date pathspec => date

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符.
        date---一个通用时间或 nil.

* 描述(Description):

        返回一个表示由 pathspec 指定的文件最后一次被写入时的通用时间, 或者如果不能确定这样一个时间就返回 nil.

* 示例(Examples):

    ```LISP
    (with-open-file (s "noel.text" 
                        :direction :output :if-exists :error)
      (format s "~&Dear Santa,~2%I was good this year.  ~
                    Please leave lots of toys.~2%Love, Sue~
                ~2%attachments: milk, cookies~%")
      (truename s))
    =>  #P"CUPID:/susan/noel.text"
    (with-open-file (s "noel.text")
      (file-write-date s))
    =>  2902600800
    ```

* 受此影响(Affected By):

        主机计算机的文件系统.

* 异常情况(Exceptional Situations):

        如果 pathspec 是通配的, 那么应该发出一个 file-error 的错误.

        如果文件系统不能执行请求的操作, 那么应该发出一个 file-error 的错误.

* 也见(See Also):

        章节 25.1.4.2 (Universal Time), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-RENAME-FILE">函数 RENAME-FILE</span>

* 语法(Syntax):

        rename-file filespec new-name => defaulted-new-name, old-truename, new-truename

* 参数和值(Arguments and Values):

        filespec---一个路径名标识符.
        new-name---一个路径名标识符而不是一个流.
        defaulted-new-name---一个路径名.
        old-truename---一个物理路径名.
        new-truename---一个物理路径名.

* 描述(Description):

        rename-file 以这样一种方式修改文件系统: 由 filespec 表示的文件被重命名为 defaulted-new-name.

        指定一个包含通配符成员的文件名是一个错误的, 由于在文件系统不允许一个 nil 成员的情况中 filespec 包含一个 nil 成员, 或者由于在文件系统不允许一个 nil 成员的情况中来自 filespec 的 new-name 的默认缺失成员包含一个.<!--TODO 待校对-->

        如果 new-name 是一个逻辑路径名, rename-file 返回一个逻辑路径名作为它的主要的值.

        如果成功 rename-file 三个值. 主要的值, defaulted-new-name, 是 new-name 和任何缺失的成员组合而成的名字, 这些缺失成员通过执行一个 merge-pathnames 操作来填充, 使用 filespec 作为默认值. 第二个值, old-truename, 是那个文件被重命名之前的真实名字. 第三个值, new-truename, 是那个文件被重命名之后的真实名字.

        如果这个 filespec 标识符是一个打开的流, 那么这个流自身和那个和它关联的文件会被影响 (如果这个文件系统允许的话).

* 示例(Examples):

    ```LISP
    ;; An example involving logical pathnames.
    (with-open-file (stream "sys:chemistry;lead.text"
                            :direction :output :if-exists :error)
      (princ "eureka" stream)
      (values (pathname stream) (truename stream)))
    =>  #P"SYS:CHEMISTRY;LEAD.TEXT.NEWEST", #P"Q:>sys>chem>lead.text.1"
    (rename-file "sys:chemistry;lead.text" "gold.text")
    =>  #P"SYS:CHEMISTRY;GOLD.TEXT.NEWEST",
      #P"Q:>sys>chem>lead.text.1",
      #P"Q:>sys>chem>gold.text.1"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果重命名操作不成功, 那么就会发出一个 file-error 的错误.

        如果 filespec 是通配的, 那么可能会发出一个 file-error 的错误.

* 也见(See Also):

        truename, pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-DELETE-FILE">函数 DELETE-FILE</span>

* 语法(Syntax):

delete-file filespec => t

* 参数和值(Arguments and Values):

filespec---一个路径名标识符.

* 描述(Description):

        删除由 filespec 指定的文件.

        如果这个 filespec 标识符是一个打开的流, 那么 filespec 和与它关联的文件都会被影响 (如果文件系统允许的话), 在这个情况下 filespec 可能被立即关闭, 并且这个删除可能是立即的或延时的知道 filespec 被显式关闭, 依赖于这个文件系统的需要.

        一个删除一个不存在文件的尝试是否被认为成功是依赖于具体实现的.

        如果 delete-file 成功了就返回 true, 如果没有就发出一个 file-error 类型的错误.

        如果 filespec 有一个通配的成员, 或者如果 filespec 有一个 nil 成员并且这个文件系统不允许一个 nil 成员, 那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (with-open-file (s "delete-me.text" :direction :output :if-exists :error))
    =>  NIL
    (setq p (probe-file "delete-me.text")) =>  #P"R:>fred>delete-me.text.1"
    (delete-file p) =>  T
    (probe-file "delete-me.text") =>  false
    (with-open-file (s "delete-me.text" :direction :output :if-exists :error)
      (delete-file s))
    =>  T
    (probe-file "delete-me.text") =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这个删除操作不成功, 那么就会发出一个 file-error 的错误

        如果 filespec 是通配的, 那么可能会发出一个 file-error 的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (文件系统概念), 章节 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="CT-FILE-ERROR">状况类型 FILE-ERROR</span>

* 类优先级列表(Class Precedence List):

        file-error, error, serious-condition, condition, t

* 描述(Description):

        这个 file-error 类型由发生在尝试去打开或关闭一个文件期间, 或者在某些与一个文件系统的底层交互中的一个错误状况组成. 这个 "违规路径名(offending pathname)" 通过给 make-condition 的 :pathnameinitialization 参数来初始化, 并且可以通过函数 file-error-pathname 来访问.

* 也见(See Also):

        file-error-pathname, open, probe-file, directory, ensure-directories-exist 


### <span id="F-FILE-ERROR-PATHNAME">函数 FILE-ERROR-PATHNAME</span>

* 语法(Syntax):

        file-error-pathname condition => pathspec

* 参数和值(Arguments and Values):

        condition---一个 file-error 类型的状况.
        pathspec---一个路径名标识符.

* 描述(Description):

        返回一个 file-error 类型的状况的 "违规路径名(offending pathname)".

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

* 也见(See Also):

        file-error, 章节 9 (Conditions)

* 注意(Notes): None. 

