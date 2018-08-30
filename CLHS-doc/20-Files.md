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

如果一个文件是到另一个文件的符号链接 (在一个允许这东西的文件系统中), 跟在任何符号链接之后, truename 是该文件的规范名称 it is conventional for the truename to be the canonical name of the file after any symbolic links have been followed; 这就是说, 它是那个如果打开该文件的输入流, 内容将可用的文件的规范名称.<!--待校对-->

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

pathspec---a pathname designator, which may contain wild components.

pathnames---a list of physical pathnames.

* 描述(Description):

Determines which, if any, files that are present in the file system have names matching pathspec, and returns a fresh list of pathnames corresponding to the truenames of those files.

An implementation may be extended to accept implementation-defined keyword arguments to directory.

* 示例(Examples): None.

* 受此影响(Affected By):

The host computer's file system.

* 异常情况(Exceptional Situations):

If the attempt to obtain a directory listing is not successful, an error of type file-error is signaled.

* 也见(See Also):

pathname, logical-pathname, ensure-directories-exist, Section 20.1 (文件系统概念), Section 20.1.2 (在打开或关闭的流上的文件操作), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

If the pathspec is not wild, the resulting list will contain either zero or one elements.

Common Lisp specifies ``&key'' in the argument list to directory even though no standardized keyword arguments to directory are defined. ``:allow-other-keys t'' may be used in conforming programs in order to quietly ignore any additional keywords which are passed by the program but not supported by the implementation.

### <span id="F-PROBE-FILE">函数 PROBE-FILE</span>

* 语法(Syntax):

probe-file pathspec => truename

* 参数和值(Arguments and Values):

pathspec---a pathname designator.

truename---a physical pathname or nil.

* 描述(Description):

probe-file tests whether a file exists.

probe-file returns false if there is no file named pathspec, and otherwise returns the truename of pathspec.

If the pathspec designator is an open stream, then probe-file produces the truename of its associated file. If pathspec is a stream, whether open or closed, it is coerced to a pathname as if by the function pathname.

* 示例(Examples): None.

* 受此影响(Affected By):

The host computer's file system.

* 异常情况(Exceptional Situations):

An error of type file-error is signaled if pathspec is wild.

An error of type file-error is signaled if the file system cannot perform the requested operation.

* 也见(See Also):

truename, open, ensure-directories-exist, pathname, logical-pathname, Section 20.1 (文件系统概念), Section 20.1.2 (在打开或关闭的流上的文件操作), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-ENSURE-DIRECTORIES-EXIST">函数 ENSURE-DIRECTORIES-EXIST</span>

* 语法(Syntax):

ensure-directories-exist pathspec &key verbose => pathspec, created

* 参数和值(Arguments and Values):

pathspec---a pathname designator.

verbose---a generalized boolean.

created---a generalized boolean.

* 描述(Description):

Tests whether the directories containing the specified file actually exist, and attempts to create them if they do not.

If the containing directories do not exist and if verbose is true, then the implementation is permitted (but not required) to perform output to standard output saying what directories were created. If the containing directories exist, or if verbose is false, this function performs no output.

The primary value is the given pathspec so that this operation can be straightforwardly composed with other file manipulation expressions. The secondary value, created, is true if any directories were created.

* 示例(Examples): None.

* 受此影响(Affected By):

The host computer's file system.

* 异常情况(Exceptional Situations):

An error of type file-error is signaled if the host, device, or directory part of pathspec is wild.

If the directory creation attempt is not successful, an error of type file-error is signaled; if this occurs, it might be the case that none, some, or all of the requested creations have actually occurred within the file system.

* 也见(See Also):

probe-file, open, Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-TRUENAME">函数 TRUENAME</span>

* 语法(Syntax):

truename filespec => truename

* 参数和值(Arguments and Values):

filespec---a pathname designator.

truename---a physical pathname.

* 描述(Description):

truename tries to find the file indicated by filespec and returns its truename. If the filespec designator is an open stream, its associated file is used. If filespec is a stream, truename can be used whether the stream is open or closed. It is permissible for truename to return more specific information after the stream is closed than when the stream was open. If filespec is a pathname it represents the name used to open the file. This may be, but is not required to be, the actual name of the file.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error of type file-error is signaled if an appropriate file cannot be located within the file system for the given filespec, or if the file system cannot perform the requested operation.

An error of type file-error is signaled if pathname is wild.

* 也见(See Also):

pathname, logical-pathname, Section 20.1 (文件系统概念), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

truename may be used to account for any filename translations performed by the file system. 


### <span id="F-FILE-AUTHOR">函数 FILE-AUTHOR</span>

* 语法(Syntax):

file-author pathspec => author

* 参数和值(Arguments and Values):

pathspec---a pathname designator.

author---a string or nil.

* 描述(Description):

Returns a string naming the author of the file specified by pathspec, or nil if the author's name cannot be determined.

* 示例(Examples):

 (with-open-file (stream ">relativity>general.text")
   (file-author s))
=>  "albert"

* 受此影响(Affected By):

The host computer's file system.

Other users of the file named by pathspec.

* 异常情况(Exceptional Situations):

An error of type file-error is signaled if pathspec is wild.

An error of type file-error is signaled if the file system cannot perform the requested operation.

* 也见(See Also):

pathname, logical-pathname, Section 20.1 (文件系统概念), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-FILE-WRITE-DATE">函数 FILE-WRITE-DATE</span>

* 语法(Syntax):

file-write-date pathspec => date

* 参数和值(Arguments and Values):

pathspec---a pathname designator.

date---a universal time or nil.

* 描述(Description):

Returns a universal time representing the time at which the file specified by pathspec was last written (or created), or returns nil if such a time cannot be determined.

* 示例(Examples):

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

* 受此影响(Affected By):

The host computer's file system.

* 异常情况(Exceptional Situations):

An error of type file-error is signaled if pathspec is wild.

An error of type file-error is signaled if the file system cannot perform the requested operation.

* 也见(See Also):

Section 25.1.4.2 (Universal Time), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-RENAME-FILE">函数 RENAME-FILE</span>

* 语法(Syntax):

rename-file filespec new-name => defaulted-new-name, old-truename, new-truename

* 参数和值(Arguments and Values):

filespec---a pathname designator.

new-name---a pathname designator other than a stream.

defaulted-new-name---a pathname

old-truename---a physical pathname.

new-truename---a physical pathname.

* 描述(Description):

rename-file modifies the file system in such a way that the file indicated by filespec is renamed to defaulted-new-name.

It is an error to specify a filename containing a wild component, for filespec to contain a nil component where the file system does not permit a nil component, or for the result of defaulting missing components of new-name from filespec to contain a nil component where the file system does not permit a nil component.

If new-name is a logical pathname, rename-file returns a logical pathname as its primary value.

rename-file returns three values if successful. The primary value, defaulted-new-name, is the resulting name which is composed of new-name with any missing components filled in by performing a merge-pathnames operation using filespec as the defaults. The secondary value, old-truename, is the truename of the file before it was renamed. The tertiary value, new-truename, is the truename of the file after it was renamed.

If the filespec designator is an open stream, then the stream itself and the file associated with it are affected (if the file system permits).

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If the renaming operation is not successful, an error of type file-error is signaled.

An error of type file-error might be signaled if filespec is wild.

* 也见(See Also):

truename, pathname, logical-pathname, Section 20.1 (文件系统概念), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-DELETE-FILE">函数 DELETE-FILE</span>

* 语法(Syntax):

delete-file filespec => t

* 参数和值(Arguments and Values):

filespec---a pathname designator.

* 描述(Description):

Deletes the file specified by filespec.

If the filespec designator is an open stream, then filespec and the file associated with it are affected (if the file system permits), in which case filespec might be closed immediately, and the deletion might be immediate or delayed until filespec is explicitly closed, depending on the requirements of the file system.

It is implementation-dependent whether an attempt to delete a nonexistent file is considered to be successful.

delete-file returns true if it succeeds, or signals an error of type file-error if it does not.

The consequences are undefined if filespec has a wild component, or if filespec has a nil component and the file system does not permit a nil component.

* 示例(Examples):

 (with-open-file (s "delete-me.text" :direction :output :if-exists :error))
=>  NIL
 (setq p (probe-file "delete-me.text")) =>  #P"R:>fred>delete-me.text.1"
 (delete-file p) =>  T
 (probe-file "delete-me.text") =>  false
 (with-open-file (s "delete-me.text" :direction :output :if-exists :error)
   (delete-file s))
=>  T
 (probe-file "delete-me.text") =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If the deletion operation is not successful, an error of type file-error is signaled.

An error of type file-error might be signaled if filespec is wild.

* 也见(See Also):

pathname, logical-pathname, Section 20.1 (文件系统概念), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="CT-FILE-ERROR">状况类型 FILE-ERROR</span>

* 类优先级列表(Class Precedence List):

file-error, error, serious-condition, condition, t

* 描述(Description):

The type file-error consists of error conditions that occur during an attempt to open or close a file, or during some low-level transactions with a file system. The ``offending pathname'' is initialized by the :pathnameinitialization argument to make-condition, and is accessed by the function file-error-pathname.

* 也见(See Also):

file-error-pathname, open, probe-file, directory, ensure-directories-exist 


### <span id="F-FILE-ERROR-PATHNAME">函数 FILE-ERROR-PATHNAME</span>

* 语法(Syntax):

file-error-pathname condition => pathspec

* 参数和值(Arguments and Values):

condition---a condition of type file-error.

pathspec---a pathname designator.

* 描述(Description):

Returns the ``offending pathname'' of a condition of type file-error.

* 示例(Examples): None.

Side Effects: None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

* 也见(See Also):

file-error, Section 9 (Conditions)

* 注意(Notes): None. 

