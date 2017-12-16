# 章节 31. 平台不相关的扩展

> * 31.1 [定制CLISP进程初始化和终止行为](#CustInitTerm)
> * 31.2 [保存为镜像](#SaveAnImage)
> * 31.3 [退出 CLISP](#QuittingCLISP)
> * 31.4 [CLISP 国际化](#Internationalization)
> * 31.5 [编码](#Encodings)
> * 31.6 [通用流](#GenericStreams)
> * 31.7 [弱对象](#WeakObjects)
> * 31.8 [资源释放](#Finalization)
> * 31.9 [提示语](#Prompt)
> * 31.10 [最大 ANSI CL 合规](#MaxCLCompliance)
> * 31.11 [额外奇特的宏和函数](#AdditionMacroFun)
> * 31.12 [定制 CLISP 行为](#CustClispBehavior)
> * 31.13 [Code Walker](#CodeWalker)

## 31.1 <span id = "CustInitTerm">定制CLISP进程初始化和终止行为</span>

>> * 31.1.1 [开始到结束](#CradleToGrave)
>> * 31.1.2 [定制初始化](#CustInit)
>> * 31.1.3 [定制结束](#CustTerm)

### 31.1.1 <span id = "CradleToGrave">开始到结束</span>
* 解析命令行的参数，直到第一个位置 (see :SCRIPT in Section 31.2, “Saving an Image”).
* 加载内存镜像.
* 安装内部的信号处理者.
* 初始化时间变量.
* 初始化本地依赖编码.
* 初始化流变量.
* 初始化路径名变量.
* 初始化 “FFI”.
* 初始化模块.
* 运行 CUSTOM:\*INIT-HOOKS*指定的所有函数.
* 打印 “hi”, 除非被 -q所抑制.
* 加载 RC file, 除非被 -norc所抑制.
* 处理命令行选项: 文件加载 或 汇编,生成评价（form evaluation）,脚本的执行以及 read-eval-print loop.
* 展开 STACK, 在 UNWIND-PROTECT里清理表。
* 运行 CUSTOM:\*FINI-HOOKS*里的所有函数
* 在标准流里调用 FRESH-LINE .
* 打印 “bye”， 除非被-q抑制.
* 如果被 -w所请求，等待按键按下.
* 关闭所有打开的文件流.
* 析构模块.
* 关闭所有打开的 DLLs.


## 31.1.2 <span id = "CustInit">定制初始化</span>

CUSTOM:\*INIT-HOOKS* 像这样运行:

```Lisp
(IGNORE-ERRORS (MAPC #'FUNCALL CUSTOM:*INIT-HOOKS*))
```

>>> * 31.1.2.1 [CUSTOM:\*INIT-HOOKS* 和初始化函数的区别](#DiffInitHookIInitFun)

#### 31.1.2.1 <span id = "DiffInitHookIInitFun">CUSTOM:\*INIT-HOOKS* 和初始化函数的区别</span>

CUSTOM:\*INIT-HOOKS* 无视命令行选项总是运行的，甚至在标语被打印之前.
init function 只有在进入 read-eval-print loop 后运行 而且只是在第一个提示符被打印之前.

## 31.1.3 <span id = "CustTerm">定制结束</span>

CUSTOM:\*FINI-HOOKS* 像这样运行:

```Lisp
(MAPC #'FUNCALL CUSTOM:*FINI-HOOKS*)
```

## 31.2 <span id = "SaveAnImage">保存为镜像</span>

>> * 31.2.1 [镜像的移植性](#ImagePortability)

这个函数 ``(EXT:SAVEINITMEM &OPTIONAL (filename "lispinit.mem") &KEY :KEEP-GLOBAL-HANDLERS :QUIET :INIT-FUNCTION :LOCKED-PACKAGES :START-PACKAGE :EXECUTABLE :NORC :SCRIPT :DOCUMENTATION :VERBOSE)`` 保存运行的 CLISP内存到文件 filename; 建议使用扩展的 #P".mem"  (如果 filename 没有这个扩展, #P".mem" 扩展会自动添加除非这个文件被构建成可执行文件).

:QUIET

    如果这个参数不是 NIL, 启动标语和结束信息会被抑制, 就像是 -q.

    在交互式应用交付这个是不建议的, 请将你的标语追加进去 (通过使用 init function) 而不是取代它.

:VERBOSE

    在写文件后打印一条信息. 默认是 CUSTOM:*SAVEINITMEM-VERBOSE*; 初始值为 T.

:NORC

    如果这个值不是 NIL,  RC file 载入就会被抑制, 等同于 -norc.

:INIT-FUNCTION

    这个参数指定一个函数，在保存镜像被启动时执行, 在进入标准的 read-eval-print loop 之前(但是在其他初始化之后, 见 Section 31.1.1, “Cradle to Grave”); 因此, 如果你想避开 read-eval-print loop, 你需要去调用 EXT:EXIT 在你自己的 init function之后 (这个不会阻止 CUSTOM:*FINI-HOOKS* 被运行).

    通过命令行参数给这个函数见 the manual.

    同样见 CUSTOM:*INIT-HOOKS* 和 CUSTOM:*FINI-HOOKS*.

:SCRIPT

    这个选项决定了镜像被调用时参数位置的处理。

    * 如果是 T, 第一个参数是一个脚本名字，然后剩下的参数被填入 EXT:*ARGS*, 像这里描述的一样 Section 32.6.2, “Scripting with CLISP”.
    * 如果是 NIL, 所有参数都会被填入 EXT:*ARGS* 待 init function处理。

    当 init function 是 NIL时默认选项为 T ;当 init function 不是 NIL 时默认 NIL .

:DOCUMENTATION

    这个描述来这个镜像的信息, 被 -help-image 选项打印.

    默认是 (DOCUMENTATION init function 'FUNCTION)

:LOCKED-PACKAGES

    这个参数指定了保存镜像时的锁定的包; 当你不想你的用户去搅乱你的产品时，对软件发布，这是很方便的. 这个参数默认为 CUSTOM:*SYSTEM-PACKAGE-LIST*.

:START-PACKAGE

    这个参数在镜像保存时指定  *PACKAGE* 的初始值, 默认为当前 *PACKAGE*的值。

:KEEP-GLOBAL-HANDLERS

    如果不是NIL, 当前已建立的 global handlers会被镜像所继承去 (包括 用 EXT:SET-GLOBAL-HANDLER 或者 用 -on-error) . 默认为 NIL, 因此

    $ clisp -i myfile -x '(EXT:SAVEINITMEM)'

    会产生一个没有任何 global handlers 继承自以上批处理命令.

:EXECUTABLE

    如果不是NIL, 保存的文件会是单独的可执行文件. 举个例子,  #P".mem" 扩展没有被添加. 在 Win32 和 Cygwin 会被这个扩展所取代 #P".exe" .

    另外, 如果参数是 0, 标准 CLISP 命令行参数就不会被这个可执行文件处理，但是会被填入 EXT:*ARGS*. 你的基于clisp的应用可以接受例如 -x的参数选项，这个对软件的发布是很方便的。要覆盖镜像的这个特点, 你需要以 "--clisp"这个选项作前缀，例如使用 --clisp-x 而不是 -x. 这一点，基于 CLISP 的应用都提供, 你可以回到 CLISP read-eval-print loop 通过

    $ application --clisp-x '(EXT:SAVEINITMEM "myclisp" :executable t :init-function nil)'
    $ ./myclisp
    [1]> (! 20)
    2432902008176640000

    这些说明也会被 --clisp--help所打印。

    当然,这个特点导致了一个安全漏洞 ，如果应用运行 setuid root, 因此 CLISP 重置实际的 组 和 用户ID 为真实的那个 如果它考虑 "--clisp-*" 选项.

你可以通过 -M 选项使用这份内存镜像. 在 UNIX 系统上, 你可以用 GNU gzip 压缩然后存储在磁盘上.

### <span id = "SaveAnImage">31.2.1. 镜像的移植性</span>

内存镜像不是可以跨平台的 (和平台无关的 #P".fas" 文件对比). 他们甚至在 linking set之间也不通用: 通过 full linking set 保存的镜像不能在 base runtime 中使用:

```shell
$ clisp -K full -x '(EXT:SAVEINITMEM)'
$ clisp -K base -M lispinit.mem
base/lisp.run: initialization file `lispinit.mem' was not created by this version of CLISP runtime
```

## 31.3 <span id = "QuittingCLISP">退出 CLISP</span>

这几个函数

```Lisp
(EXT:EXIT &OPTIONAL status)
(EXT:QUIT &OPTIONAL status)
(EXT:BYE &OPTIONAL status)
```

- 都是同义的 - 表示中断 CLISP. 如果 status 不是NIL, CLISP 以返回一个数值形式的错误 status终止, i.e., 操作系统环境会被通知这个CLISP 会话失败.

最后的分隔符( Final delimiter )也会中断 CLISP.


## 31.4 <span id = "Internationalization">CLISP 国际化</span>

>> * 31.4.1 [语言](#Language)

词汇表

    Internationalization (“i18n”)
        预备程序以便它可以在不改动更多代码的情况下，使用多国语言以及国际文化的交流.

    Localization (“l10n”)
        提供数据 - 大部分为原文翻译 - 对于一个工作在流行语言下包括流行文化交流的国际化应用来说是有必要的.

CLISP 是国际化的, 对这些语言 English, German, French, Spanish, Dutch, Russian, and Danish也是地区化的. CLISP 也支持国际化的 Lisp 程序, 通过 GNU gettext, 见 Section 33.2, “Internationalization of User Programs”.

### 31.4.1 <span id = "Language">语言</span>

**注意**

    这个章节描述的工具只工作在已经地区化的clisp的语言下.

CLISP 与用户交流的语言为以下之一：

    ENGLISH
    DEUTSCH (i.e., German)
    FRANÇAIS (i.e., French)
    ESPAÑOL (i.e., Spanish)
    NEDERLANDS (i.e., Dutch)
    РУССКИЙ (i.e. Russian)
    DANSK (i.e., Danish)

这是被 SYMBOL-MACRO CUSTOM:\*CURRENT-LANGUAGE*所控制的,可以通过运行时设置也可以通过使用 -L 命令行选项设置. 如果你想在运行时改变 locale directory, 你可以通过设置 CUSTOM:\*CURRENT-LANGUAGE* 为一个 CAR 是语言 (一个 SYMBOL, 以上所述的其中一个)、 CDR 是一个新的 locale directory的 CONS .

可以通过 I18N:DEFLANGUAGE: ``(I18N:DEFLANGUAGE language)`` 这个宏定义更多语言。 为了这些额外添加的语言生效, 你必须安装对应的消息目录( message catalog ), 或者通过 GNU gettext 和 Emacs (or XEmacs) po-mode 自己翻译这些消息 .

这个只对字符串有用. 对于任意语言依赖的Lisp对象, 你可以通过这个宏 I18N:DEFINTERNATIONAL: ``(I18N:DEFINTERNATIONAL symbol &OPTIONAL (default-language T))`` 来定义，通过 I18N:DEFLOCALIZED: ``(I18N:DEFLOCALIZED symbol language value-form)`` 这个宏来添加语言依赖的值(对于每种语言都是这样一种形式. 没有被赋值的语言会被当作 default-language 对待). 可以通过调用 I18N:LOCALIZED: ``(I18N:LOCALIZED symbol &OPTIONAL language)``  访问本地化的值。

## 31.5 <span id = "Encodings">编码</span>

> * 31.5.1 [介绍](#EncodingIntroduction)
> * 31.5.2 [字符集](#CharacterSets)
> * 31.5.3 [行终止](#LineTerminators)
> * 31.5.4 [函数 EXT:MAKE-ENCODING](#FunMakeEncoding)
> * 31.5.5 [函数 EXT:ENCODING-CHARSET](#FunEncodingCharset)
> * 31.5.6 [默认的 encodings](#DefaultEncodings)
>> * 31.5.6.1 [默认的行终止符](#DefaultLineTerminator)
> * 31.5.7 [字符串和字节向量的转换](#CovertStrToByte)

### 31.5.1 <span id = "EncodingIntroduction">介绍</span>

一种编码描述了以 STREAM-ELEMENT-TYPE CHARACTER 形式的 STREAMc 传输的字母与原始字节的对应关系.

一个 EXT:ENCODING 是一个由以下几个方面组成的对象:

character set（字符集）

    这个指出了可以表示并通过 I/O 管道的字符的集合，以及转换为原始字节的方式, 例如,   CHARACTER 序列 和 STRINGs 结构中的 (UNSIGNED-BYTE 8)  以及 (VECTOR (UNSIGNED-BYTE 8)) 字符和字节 STREAMs 之间的映射. 在这个条件下, 比方说, CHARSET:UTF-8 和 CHARSET:UCS-4 被认为时不同的, 虽然它们可以表示相同的字符集.
line terminator mode（行结束模式）

    这个指明了表示新起一行的方式.

EXT:ENCODINGs 同样也是 TYPEs. 它们指定了可以在这个字符集下可以编码的字符的集合. 这个情况下, 将字母转换成原始字节的方式会被忽略, 而且行结束模式也会被忽略. TYPEP 和 SUBTYPEP 可以对 encodings 使用:

```Lisp
(SUBTYPEP CHARSET:UTF-8 CHARSET:UTF-16)
⇒ T ;
⇒ T
(SUBTYPEP CHARSET:UTF-16 CHARSET:UTF-8)
⇒ T ;
⇒ T
(SUBTYPEP CHARSET:ASCII CHARSET:ISO-8859-1)
⇒ T ;
⇒ T
(SUBTYPEP CHARSET:ISO-8859-1 CHARSET:ASCII)
⇒ NIL ;
⇒ T
```

“1:1” encodings. 在字符和字节序列之间定义的一个双向映射称之为Encodings . CHARSET:ISO-8859-1 就是 encoding 的一个例子: 任何字节序列对应的一些字符序列，反之亦然. 然而 ASCII, 不是一个 “1:1” encoding: 没有字符对应字节区间 [128;255]. CHARSET:UTF-8 也不是一个 “1:1” encoding : 一些字节序列没有对应任何的字符序列.

### 31.5.2 <span id = "CharacterSets">字符集</span>

平台依赖的:只有在没用编译时(compile-time) 标志 UNICODE构建的 CLISP

    只有一种字符集被接受的: 平台的本地 (8-bit) 字符集合. 见 Chapter 13, Characters .

平台依赖的:只有在用编译时(compile-time) 标志 UNICODE构建的 CLISP 

    以下字符集是被支持的, 这些常量的值被定义在 “CHARSET” 包里:

    包 “CHARSET” 中的符号

    具体字符集见：http://www.clisp.org/impnotes/encoding.html

平台依赖: 只有在 GNU 系统 包括了 GNU libc 2.2 或者更高，还有其他的系统 (UNIX and Win32)  GNU libiconv C library 已经被安装了的。

        库函数 iconv 提供的字符集合可以被当作是 encodings. 为了构造这样的 encodings, 调用 EXT:MAKE-ENCODING 把字符集的名字（string）设置给 :CHARSET 参数.

        当一个 EXT:ENCODING 在 内置的( built-in )和 iconv都可用时,使用内置的( built-in ), 因为这个更高效且更多平台可用.

        这些 encodings 不要赋值给全局变量, 因为没有在 iconv 的支持下方便的方法去获取所有的字符集的集合.

        在一些标准的 UNIX 系统 (例如, GNU systems, GNU/Linux 和 GNU/Hurd) 以及在拥有 GNU libiconv 的系统上你可以通过调用 iconv -l 来获取这个列表

        我们只使用 GNU libc 2.2 或者 GNU libiconv 的原因是其他 iconv 实现会出现各种异常以及我们不想再去处理那些bug导致的随机的clisp崩溃. 如果你的系统提供一个可以通过 GNU libiconv's 测试套件的 iconv 实现 , 请报告给 clisp-list ,未来的 CLISP 版本会使用你系统上的那个 iconv.

### 31.5.3 <span id = "LineTerminators">行终止</span>

行终止模式可以是下面三个关键字中的一个:

:UNIX

    新起一行用 ASCII LF 字符 (U000A) 表示.

:MAC

    新起一行用 ASCII CR 字符 (U000D)表示.

:DOS

    新起一行用 ASCII CR 后面跟 ASCII LF.

Windows 的程序通常使用 :DOS 行终止符, 有时它们也接受 :UNIX 行终止符 或者 :MAC 行终止符.

 HTTP 协议也需要 :DOS 行终止符.

行终止符只有在输出的时候是有重大意义的 (写到一个 file/pipe/socket STREAM). 输入的时候, 所有这三种行终止符都是被识别的. 见 Section 13.10, “Treatment of Newline during Input and Output ”.

### 31.5.4 <span id = "FunMakeEncoding">函数 EXT:MAKE-ENCODING</span>

    函数 (EXT:MAKE-ENCODING &KEY :CHARSET :LINE-TERMINATOR :INPUT-ERROR-ACTION :OUTPUT-ERROR-ACTION) 返回一个 EXT:ENCODING. 这个 :CHARSET 参数可能是encoding, 一个字符串, 或者 :DEFAULT. 这个 line terminator 参数可能的值是关键字 :UNIX, :MAC, :DOS.

    这个 :INPUT-ERROR-ACTION 参数指明在转换字节到字符时遇到非法字节序列做什么. 它的值可能是 :ERROR, :IGNORE 或者是一个字符会被用到的. 这个 UNICODE 字符 #\uFFFD 通常表示在输入序列中有个异常.

    这个 :OUTPUT-ERROR-ACTION 参数指明在转换字符到字节时遇到非法字节序列做什么. 它的值可能是 :ERROR, :IGNORE, 或者是一个字节. 如果这个UNICODE 字符 #\uFFFD 在这个字符集中是可编码的，就可以在这里使用.

### 31.5.5 <span id = "FunEncodingCharset">函数 EXT:ENCODING-CHARSET</span>

平台依赖: 仅限在用编译时( compile-time )标志 UNICODE构建的 CLISP

这个函数 (EXT:ENCODING-CHARSET encoding) 返回 encoding的字符集,一个 SYMBOL 或者一个 STRING.
注意

    警告
    (STRING (EXT:ENCODING-CHARSET encoding)) 不一定是一个合法的 MIME 名字.

### 31.5.6 <span id = "DefaultEncodings">默认的 encodings</span>

除了每个 file/pipe/socket STREAM 包含了一个 encoding, 以下 SYMBOL-MACRO 的地方包含了全局的 EXT:ENCODINGs:

SYMBOL-MACRO CUSTOM:\*DEFAULT-FILE-ENCODING\*.当没有指定这个 :EXTERNAL-FORMAT 参数时，这个 SYMBOL-MACRO place CUSTOM:\*DEFAULT-FILE-ENCODING* 就是那个被用于 file/pipe/socket STREAM 的 encoding.

平台依赖: 仅限在用编译时( compile-time )使用标志 UNICODE 构建的 CLISP

    以下为符号宏的place.

    CUSTOM:*PATHNAME-ENCODING*

        是被用于转换文件系统的文件名(被操作系统表示为字节序列) 为 lisp的路径名成分 (STRINGs). 如果这个在你的系统上对于有些文件名是冲突的, 文件系统访问 (e.g., DIRECTORY) 可能 SIGNAL ERRORs, 因此如果这不是一个 encoding 推荐使用警告. 有时候这个encoding被调用可能一点也不明显. E.g., on Win32:

        当 CUSTOM:*PATHNAME-ENCODING* 是 CHARSET:UTF-16 ，因为 #\ARMENIAN_SMALL_LETTER_RA 相当于4 字节 #(255 254 124 5) 然而这个 124 对于 Win32 文件名不是合法的，因为这个表示 ASCII 的| .

        这个合法路径名字节集合会在配置的时候被 GNU autoconf 测试 src/m4/filecharset.m4 决定. 前 127 字节稳定, 但是在 Win32 128-256 字节疯狂地变化, 取决于操作系统版本和文件系统.

        这个行终止符在 CUSTOM:*PATHNAME-ENCODING* 是被忽略的.
    CUSTOM:*TERMINAL-ENCODING*
        是和终端交互的 encoding , in particular by *TERMINAL-IO*.
    CUSTOM:*MISC-ENCODING*
        是访问环境变量、命令行参数等等的 encoding. 它的行终止符被忽略.
    CUSTOM:*FOREIGN-ENCODING*
        是用于通过 “FFI” 传递的字符串(仅限部分平台) 的 encoding. 如果是一个 “1:1” encoding, i.e. 一个每个字符被表示为一个字节的 encoding, 它也被用于在 “FFI” 传输字节.

这个默认的 encoding 对象根据 -Edomain encoding 来初始化。

**提醒**

    对于SYMBOL-MACROs你不得不用 EXT:LETF/EXT:LETF* ; LET/LET* 会不起作用!

#### 31.5.6.1 <span id = "DefaultLineTerminator">默认的行终止符</span>

在 EXT:ENCODINGs 方面的行终止符通过以下逻辑确定: 自从 CLISP 识别所有输入的可能的行终止符 (see Section 13.10, “Treatment of Newline during Input and Output ”), 问题在于哪个 line terminator 是其他大部分程序期望的?

平台依赖: 仅限 UNIX 平台.

    如果一个非零 O_BINARY cpp 常量被定义, 我们认为系统区别二进制文件和文本文件, 然后, 因为 encodings只跟文本文件有关,因此我们使用 :DOS; 否则默认的就是 :UNIX.

平台依赖: 仅限 Win32 平台.

    因为大部分 Win32 程序期望 CRLF, 所以推荐的行终止符是 :DOS.

这个归结为以下 src/encoding.d 代码:

```
#if defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
```

Cygwin上的默认行终止符

    以上的测试在 Cygwin 上也是通过的, 所以默认的行终止符 :DOS. 如果你希望, 你可以在你的 RC file 里改变它.

### 31.5.7 <span id = "CovertStrToByte">字符串和字节向量的转换</span>

Encodings 也能用于根据encoding直接转换字符串和它们对应的字节序列.

``(EXT:CONVERT-STRING-FROM-BYTES vector encoding &KEY :START :END)``

    根据给的 encoding 转换 vector (a (VECTOR (UNSIGNED-BYTE 8))) 从 start 到 end 的子序列为一个 STRING,然后返回结果字符串.

``(EXT:CONVERT-STRING-TO-BYTES string encoding &KEY :START :END)``

    根据给的 encoding 转换 string 从 start 到 end 子序列为 (VECTOR (UNSIGNED-BYTE 8)), 然后返回一个结果字节向量.

## 31.6 <span id = "GenericStreams">通用流</span>

这个接口是 CLISP-特有的而现在已经过时了. 请使用 Gray streams 接口.

Generic streams 是用户可编程的流. 这些是编程接口:

``(gstream:make-generic-stream controller)``

    返回一个generic stream.

``(gstream:generic-stream-controller stream)``

    返回调用方法的stream对应的私有对象. 这个通常的用法是将用户提供给 gstream:make-generic-stream 的对象取回。

``(gstream:generic-stream-p stream)``

    确定一个流是不是 generic stream, 如果是返回 T ,否则就是 NIL .
    为了指定 generic stream 的行为, 用户必须在以下几个 CLOS 广义函数上定义 CLOS 方法 . 这个 gstream:generic-stream-x 函数相当于 Common Lisp 函数 x. 它们都接受一个 controller 和其他数量的参数.

``(gstream:generic-stream-read-char controller)``

    返回消耗的下一个字节,到文件末端时返回 NIL . 唯一的参数是 controller 对象.

``(gstream:generic-stream-peek-char controller)``

    返回下一个字符,到达文件末端返回 NIL . 第二个值表示这个消费字符的副作用是否被执行: T 表示完成了一次 READ-CHAR , NIL 没有产生任何副作用. 需要一个参数是controller对象.

``(gstream:generic-stream-read-byte controller)``

    消耗并返回下一个整形,到达末端时为 NIL . 唯一的参数是 controller 对象.

``(gstream:generic-stream-read-char-will-hang-p controller)``

    这个广义函数用来查询流的输入状态. 如果 gstream:generic-stream-read-char 和 gstream:generic-stream-peek-char 确定立即返回则返回 NIL . 否则返回 true.

``(gstream:generic-stream-write-char controller char)``

    第一个参数是controller 对象. 第二个参数是要写的字符.

``(gstream:generic-stream-write-byte controller by)``

    第一个参数是controller 对象. 第二个参数是要写的整形.

``(gstream:generic-stream-write-string controller string start length)``

    写入一个从 start 开始长度为 length 的 string.第一个参数是controller 对象.

``(gstream:generic-stream-clear-input controller)``
``(gstream:generic-stream-clear-output controller)``
``(gstream:generic-stream-finish-output controller)``
``(gstream:generic-stream-force-output controller)``
``(gstream:generic-stream-close controller)``
    
    一个参数，为 controller 对象.

## 31.7 <span id = "WeakObjects">弱对象</span>

回想两个方面:一个对象如果用户或者程序可以通过任何方式的引用，包括全局的或者局部的变量来找到，那么这个对象就是alive的. (如果一个对象不消耗堆存储, 也被认为是 “"immediate objects"”, 就像 CHARACTERs, FIXNUMs, and SHORT-FLOATs, 都是无限期存活的.) 据说当它的存储被回收时则这个对象被垃圾收集了，有时是在它成为 “"dead"” 之后.

> * 31.7.1 [弱指针](#WeakPointers)
> * 31.7.2 [弱列表](#WeakLists)
> * 31.7.3 [弱"与"关系](#WeakAndRela)
> * 31.7.4 [弱"或"关系](#WeakOrRela)
> * 31.7.5 [弱关联](#WeakAssoc)
> * 31.7.6 [弱"与"映射](#WeakAndMap)
> * 31.7.7 [弱"或"映射](#WeakOrMap)
> * 31.7.8 [弱关联列表](#WeakAssociation)
> * 31.7.9 [弱哈希表](#WeakHashTable)

### 31.7.1 <span id = "WeakPointers">弱指针</span>

一个 EXT:WEAK-POINTER 就是一个持有一个对象引用的对象, 在垃圾回收后不会保留.

Weak Pointer API

``(EXT:MAKE-WEAK-POINTER value)``

    返回一个新的 EXT:WEAK-POINTER 引用了 value.

``(EXT:WEAK-POINTER-P object)``

    如果 object 是 EXT:WEAK-POINTER这个类型就返回true.

``(EXT:WEAK-POINTER-VALUE weak-pointer)``

    返回两个值: 如果值没有被垃圾回收，就返回原始的值和 T, 否则就是 NIL 和 NIL. 这个可以用 SETF: 你可以改变这个弱指针指向的值.

弱指针在软件模块中基于通知的交流协议是很有用的, e.g. 当对一个对象 x 的改变需要通知到一个已存活的对象 y, 如果已消亡则不通知.

### 31.7.2 <span id = "WeakLists">弱列表</span>

一个 EXT:WEAK-LIST 是不会阻止对象被垃圾回收的集合. 它在语义上等价于 EXT:WEAK-POINTERs 的列表, 然而相比 EXT:WEAK-POINTERs 的列表在内存中有更高效的表现.

Weak List API

``(EXT:MAKE-WEAK-LIST list)``

    构建一个指向每一个所给 list 的 EXT:WEAK-LIST .

``(EXT:WEAK-LIST-P object)``

    如果 object 是 EXT:WEAK-LIST 类型，返回T.

``(EXT:WEAK-LIST-LIST weak-list)``

    从 weak-list 返回那些存活对象的列表.

``(SETF (EXT:WEAK-LIST-LIST weak-list) list)``

    替换 weak-list 中的那些对象.

弱列表在软件模块中基于通知的交流协议是很有用的, e.g. 当对一个对象 x 的改变需要通知到 k1, k2, ...,  kn 这些活着的对象时.
单个元素的 EXT:WEAK-LIST 语义上等价于单个 EXT:WEAK-POINTER.

### 31.7.3 <span id = "WeakAndRela">弱"与"关系</span>

一个弱 “与” 关系是不会阻止对象被垃圾回收的集合, 当它们都存活时可以被访问. 当其中一个被垃圾回收时，整个集合都会变空.

Weak “And” Relation API

``(EXT:MAKE-WEAK-AND-RELATION list)``

    为给定的 list 中的对象创建一个 EXT:WEAK-AND-RELATION.

``(EXT:WEAK-AND-RELATION-P object)``

    如果 object 是 EXT:WEAK-AND-RELATION 类型返回true

``(EXT:WEAK-AND-RELATION-LIST weak-and-relation)``

    返回存在 weak-and-relation 中的对象, 返回的列表一定不能被破坏性的修改.
    EXT:WEAK-AND-RELATIONs 在一个对象死亡对象关联就无效的场景下是很有用的.

一个单元素的 EXT:WEAK-AND-RELATION 语义上等价于 EXT:WEAK-POINTER.

### 31.7.4 <span id = "WeakOrRela">弱"或"关系</span>

一个弱 “or” 关系是一个对象集合，如果它们中有一个存活，就保护所有对象不被回收. 换句话说, 它们中的任意一个都保护其他的不被回收. 如果它们都被解除引用，这个集合就会变空.

Weak “Or” Relation API

``(EXT:MAKE-WEAK-OR-RELATION list)``

    为给定的对象 list 创建一个 EXT:WEAK-OR-RELATION .

``(EXT:WEAK-OR-RELATION-P object)``

    如果 object 是 EXT:WEAK-OR-RELATION 类型，返回true.

``(EXT:WEAK-OR-RELATION-LIST weak-or-relation)``

    返回存在 weak-or-relation 中的所有对象的列表. 返回的列表一定不能被破坏性的修改.

EXT:WEAK-OR-RELATIONs 在一个对象死亡对象关联仍然有效的场景下是很很有效的.
只包含一个元素 EXT:WEAK-OR-RELATION 语义上等价于 EXT:WEAK-POINTER.

### 31.7.5 <span id = "WeakAssoc">弱关联</span>

一个弱关联是从一个叫 key 的对象到一个叫 value 的对象的映射，key存活时它也存活. 换句话说, key只要活着, 它就会阻止value被回收.

Weak Association API

``(EXT:MAKE-WEAK-MAPPING key value)``

    创建一个 EXT:WEAK-MAPPING.

``(EXT:WEAK-MAPPING-P object)``

    如果对象是 EXT:WEAK-MAPPING 类型返回true.

``(EXT:WEAK-MAPPING-PAIR weak-mapping)``

    返回三个值: 如果key没有被回收的情况下，就是原始的key, 原始的 value, 还有 T, 否则就是 NIL, NIL, NIL.

``(EXT:WEAK-MAPPING-VALUE weak-mapping)``

    如果key还没有被回收，返回 value, 否则就是 NIL.

``(SETF (EXT:WEAK-MAPPING-VALUE weak-mapping) value)``

    替换存在 weak-mapping 里的value. 如果key已被回收，那么就是无效的.

弱关联在用存在对象外部的额外信息来填充对象时是很有用的.

### 31.7.6 <span id = "WeakAndMap">弱"与"映射</span>

一个弱 “与” 映射是一个从叫做 keys 的元组对象到一个叫做 value 的对象的映射，所有key存活时不会阻止key被回收. 当其中一个key被回收，整个mapping也会被回收.

Weak “And” Mapping API

``(EXT:MAKE-WEAK-AND-MAPPING keys value)``

    为给定的 keys 对象和 value. 之间创建映射  EXT:WEAK-AND-MAPPING 。 这个 keys 列表不能为空.

``(EXT:WEAK-AND-MAPPING-P object)``

    如果 object 是 EXT:WEAK-AND-MAPPING 类型，返回true.

``(EXT:WEAK-AND-MAPPING-PAIR weak-and-mapping)``

    返回3个值: 如果keys中没有被回收的返回 keys 的列表,  value, 还有 T,  否则就是 NIL, NIL, NIL. 返回的 keys 一定不能被破坏性修改.

``(EXT:WEAK-AND-MAPPING-VALUE weak-and-mapping)``

    如果没有key被回收就返回value，否则返回 NIL.

``(SETF (EXT:WEAK-AND-MAPPING-VALUE weak-and-mapping) value)``

    替换存在 weak-and-mapping 里的值。 如果有一些key已经被回收，则是无效的.

EXT:WEAK-AND-MAPPINGs 为一群对象的集合的属性建模是很有用的，当其中一个死亡就无效.
一个单元素key的 EXT:WEAK-AND-MAPPING 语义上等价于弱关联.

### 31.7.7 <span id = "WeakOrMap">弱"或"映射</span>

一个弱 “或” 映射是一个从叫做 keys 的元组对象到一个叫做 value 的对象的映射，当有一个key还存活时，就防止所有当keys和 value被回收.换句话说，任何一个key都保护其他key和value被回收. 当它们都解除引用时,整个映射就消失.

Weak “Or” Mapping API

``(EXT:MAKE-WEAK-OR-MAPPING keys value)``

    为给定的 keys 对象列表和 value 创建一个 EXT:WEAK-OR-MAPPING . 这个 keys 列表不能为空.

``(EXT:WEAK-OR-MAPPING-P object)``

    如果这个 object 是 EXT:WEAK-OR-MAPPING 类型返回true.

``(EXT:WEAK-OR-MAPPING-PAIR weak-or-mapping)``

    返回三个值: 如果keys没有被回收，返回 keys 列表, value, 以及 T, 否则 NIL, NIL, NIL. 返回当keys 列表不能被破坏性的修改.

``(EXT:WEAK-OR-MAPPING-VALUE weak-or-mapping)``

    如果keys没有被回收返回 value, 否则为 NIL.

``(SETF (EXT:WEAK-OR-MAPPING-VALUE weak-or-mapping) value)``

    替换存在 weak-or-mapping 的值. 当keys被回收时是无效的.

EXT:WEAK-OR-MAPPINGs 为一群对象的集合的属性建模是很有用的，当其中一个死亡时并不会变得无效。
一个只有一个key的 EXT:WEAK-OR-MAPPING 语义上等价于 weak association.

### 31.7.8 <span id = "WeakAssociation">弱关联列表</span>

一个弱关联列表是一个有规律的成对的集合, 每一对都是从一个叫 key 的对象和一个叫 value 的对象构建. 每一对的生命周期取决于 weak association list 的类型:

    :KEY
    key没有被回收，pair对就会存活. 就像 key 存活的时候,保护 value 不会回收.

    :VALUE
    value没有被回收，pair对就会存活. 就像 value 存活的时候,保护 key 不会回收.

    :KEY-AND-VALUE
    value和key都没有被回收，pair对就会存活.

    :KEY-OR-VALUE
    value和key有一个没有被回收，pair对就会存活. 就像 key 存活的时候,保护 value 不会回收, 同样的 value 存活的时候,保护 key 不会回收.

换句话说, 每一对都是:

    :KEY
    一个从 key 到 value 的 EXT:WEAK-MAPPING .

    :VALUE
    一个从 value 到 key 的 EXT:WEAK-MAPPING .

    :KEY-AND-VALUE
    一个 key 和 value 的 EXT:WEAK-AND-RELATION .

    :KEY-OR-VALUE
    一个 key 和 value 的 EXT:WEAK-OR-RELATION .

Weak Association List API

``(EXT:MAKE-WEAK-ALIST :type :initial-contents)``

    创建一个 EXT:WEAK-ALIST. 这个 type 参数必须是以上四个参数中的一个; 默认的是 :KEY. 这个 initial-contents 必须是一个 association list.

``(EXT:WEAK-ALIST-P object)``

    如果 object 是 EXT:WEAK-ALIST 类型返回true.

``(EXT:WEAK-ALIST-TYPE weak-alist)``

    返回这个 weak-alist 的type，创建时指定的:type.

``(EXT:WEAK-ALIST-CONTENTS weak-alist)``

    返回一个相当于当前 weak-alist 的内容  的 association list.

``(SETF (EXT:WEAK-ALIST-CONTENTS weak-alist) contents)``

    替换 weak-alist 的内容. 这个 contents 必须是一个 association list.

``(EXT:WEAK-ALIST-ASSOC item weak-alist [:test] [:test-not] [:key])``

    等价于 (ASSOC item (EXT:WEAK-ALIST-CONTENTS weak-alist) [:test] [:test-not] [:key]).

``(EXT:WEAK-ALIST-RASSOC item weak-alist [:test] [:test-not] [:key])``

    等价于 (RASSOC item (EXT:WEAK-ALIST-CONTENTS weak-alist) [:test] [:test-not] [:key]).

``(EXT:WEAK-ALIST-VALUE item weak-alist [:test] [:test-not])``

    等价于 (CDR (EXT:WEAK-LIST-ASSOC item weak-alist [:test] [:test-not])).

``(SETF (EXT:WEAK-ALIST-VALUE item weak-alist [:test] [:test-not]) value)``

    替换存在 weak-alist 里的 item 的值. 如果给定的 item 作为key的对不存在或者被回收, 一个新的对会被添加进 association list.

弱关联列表在用存在对象外部的额外信息来填充对象且这些外部对象数量已知且比较小时是很有用的.

### 31.7.9 <span id = "WeakHashTable">弱哈希表</span>

一个弱 HASH-TABLE 是一个无序的对集合, 每一对都是从一个叫 key 的对象和一个叫 value 的对象构建. 在一个弱 HASH-TABLE 里一个给定的 key 只能有一个对. 每一个对的生命周期取决于HASH-TABLE的类型.

    :KEY
    key没有被回收，pair对就会存活. 就像 key 存活的时候,保护 value 不会回收.

    :VALUE
    value没有被回收，pair对就会存活. 就像 value 存活的时候,保护 key 不会回收.

    :KEY-AND-VALUE
    value和key都没有被回收，pair对就会存活.

    :KEY-OR-VALUE
    value和key有一个没有被回收，pair对就会存活. 就像 key 存活的时候,保护 value 不会回收, 同样的 value 存活的时候,保护 key 不会回收.;;原文这里有个差异，应该是原文的错误.

换句话说, 每一对都是:

    :KEY
    一个从 key 到 value 的 EXT:WEAK-MAPPING .

    :VALUE
    一个从 value 到 key 的 EXT:WEAK-MAPPING .

    :KEY-AND-VALUE
    一个 key 和 value 的 EXT:WEAK-AND-RELATION .

    :KEY-OR-VALUE
    一个 key 和 value 的 EXT:WEAK-OR-RELATION .

见 Section 18.2, “Function MAKE-HASH-TABLE”.
弱哈希表在用存在对象外部的额外信息来填充对象是很有用的. 如果这个对的数量很大，这个数据结构不需要执行破坏性操作就能扩展.
弱 HASH-TABLEs 实现标准化表的时候也是很有用的.

## 31.8 <span id = "Finalization">资源释放</span>

调用 (EXT:FINALIZE object function) 后，当这里指定的对象被回收时, (FUNCALL function object) 会被调用.

调用 (EXT:FINALIZE object function guardian) 也有类似效果, 但是只有在 guardian 还没有被回收: 当 object 正要被回收, (FUNCALL function object guardian) 会被执行. 如果 guardian 在 object 之前被回收, 就什么都不会发生.

注意

    “这个 object 正在被回收” 的时间不能被确切地定义. (事实上, 这个有可能从来不发生.) 这取决于没有object 的引用存在于其他的 Lisp 对象中的时间. 当这个 function 被调用, object (有可能还有 guardian) 再一次进入 “存活的 Lisp 对象区域” .

 finalization 请求最多执行一次.

 ## 31.9 <span id = "Prompt">提示语</span>

CLISP 提示由三部分组成: “start”, “body”, 和 “finish”; 以及 2 可选的部分: 只在 debugger 中出现的“break”(在 BREAK 或 ERROR 后面), 还有只出现在 STEP 的 “step”. 每部分都被自定义变量控制, 这个变量可以是一个 STRING 或者一个无参、返回一个 STRING 的 FUNCTION  (如果是其他的 - 或者返回值不是 STRING - 就会被 PRINC 打印). 按照调用顺序为:

    CUSTOM:*PROMPT-START*
    默认的一个空字符串.

    CUSTOM:*PROMPT-STEP*
    只在 STEP 时使用. 默认是 “Step n ”, 其中的 n 是 EXT:STEP-LEVEL 返回的stepping 级别.

    CUSTOM:*PROMPT-BREAK*
    只在break loop (在debugging期间) 使用. 默认是 “Break n ”, 其中的 n 是 EXT:BREAK-LEVEL 返回的break级别.

    CUSTOM:*PROMPT-BODY*
    如果它和开始的时候不同(由 EXT:PROMPT-NEW-PACKAGE 确定) 或者 如果不包含符号 T (如果是后一种情况，你需要去注意当前的包有一点怪异), 那么默认是 “package[n]” 其中的 package 是当前包 *PACKAGE* 的最短的名字 (由 EXT:PACKAGE-SHORTEST-NAME 返回) ; 以及 n 是当前提示的索引, 记录在 EXT:*COMMAND-INDEX*.

    CUSTOM:*PROMPT-FINISH*
    默认是 “> ”.

为了帮助创建你自己的自定义提示, 有以下函数和变量可用:

    EXT:BREAK-LEVEL
    这个 FUNCTION 返回当前 BREAK/ERROR 级别.

    EXT:STEP-LEVEL
    这个 FUNCTION 返回当前 STEP 级别.

    EXT:PROMPT-NEW-PACKAGE
    这个 FUNCTION 返回 *PACKAGE* ，如果当前的包和最初的时候一样返回 NIL .

    EXT:PACKAGE-SHORTEST-NAME
    这个 FUNCTION 需要一个参数, 就是一个 PACKAGE, 然后返回它的最短名称或者昵称.

    EXT:*COMMAND-INDEX*
    包含了当前提示的数目; 去增加它是你的职责 (在保存内存镜像之前，这个变量会被绑定为0).

## 31.10 <span id = "MaxCLCompliance">最大 ANSI CL 合规</span>

一些 [ANSI CL standard] 特性为了方便和向后兼容默认是关掉的. 它们可以切换至开启, 同时, 通过设置 SYMBOL-MACRO CUSTOM:\*ANSI* 到 T, 或者它们可以被单独开启. 设置 CUSTOM:\*ANSI* 到 T 意味着以下几条:

    设置 CUSTOM:*PRINT-PATHNAMES-ANSI* 为 T.
    设置 CUSTOM:*PRINT-SPACE-CHAR-ANSI* 为 T.
    设置 CUSTOM:*COERCE-FIXNUM-CHAR-ANSI* 为 T.
    设置 CUSTOM:*SEQUENCE-COUNT-ANSI* 为 T.
    设置 CUSTOM:*MERGE-PATHNAMES-ANSI* 为 T.
    设置 CUSTOM:*PARSE-NAMESTRING-ANSI* 为 T.
    设置 CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* 为 T.
    设置 CUSTOM:*FLOATING-POINT-RATIONAL-CONTAGION-ANSI* 为 T.
    设置 CUSTOM:*PHASE-ANSI* 为 T.
    设置 CUSTOM:*LOOP-ANSI* 为 T.
    设置 CUSTOM:*PRINT-EMPTY-ARRAYS-ANSI* 为 T.
    设置 CUSTOM:*PRINT-UNREADABLE-ANSI* 为 T.
    设置 CUSTOM:*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST* 为 NIL.

注意

    如果你通过 -ansi 运行 CLISP 或者设置 SYMBOL-MACRO CUSTOM:*ANSI* 为 T 然后保存内存镜像, 所有随后的通过这个镜像对 CLISP 的调用会像设置了 -ansi 一样(无论事实上你是否打开来 -ansi 开关). 你也可以总是设置 SYMBOL-MACRO CUSTOM:*ANSI* 为 NIL, 或者通过 -traditional 调用 CLISP, 来倒转以上的设置, 换句话说,

    设置 CUSTOM:*PRINT-PATHNAMES-ANSI* 为 NIL.
    设置 CUSTOM:*PRINT-SPACE-CHAR-ANSI* 为 NIL.
    设置 CUSTOM:*COERCE-FIXNUM-CHAR-ANSI* 为 NIL.
    设置 CUSTOM:*SEQUENCE-COUNT-ANSI* 为 NIL.
    设置 CUSTOM:*MERGE-PATHNAMES-ANSI* 为 NIL.
    设置 CUSTOM:*PARSE-NAMESTRING-ANSI* 为 NIL.
    设置 CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* 为 NIL.
    设置 CUSTOM:*FLOATING-POINT-RATIONAL-CONTAGION-ANSI* 为 NIL.
    设置 CUSTOM:*PHASE-ANSI* 为 NIL.
    设置 CUSTOM:*LOOP-ANSI* 为 NIL.
    设置 CUSTOM:*PRINT-EMPTY-ARRAYS-ANSI* 为 NIL.
    设置 CUSTOM:*PRINT-UNREADABLE-ANSI* 为 NIL.
    设置 CUSTOM:*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST* 为 T.

## 31.11 <span id = "AdditionMacroFun">额外奇特的宏和函数</span>

CLISP 自带一些扩展宏, 大部分定义在了包 macros3.lisp 里面，在make 时从 init.lisp 加载:

> * 31.11.1 [宏 EXT:ETHE](#MacroThe)
> * 31.11.2 [宏 EXT:LETF & EXT:LETF*](#MacroLetf)
> * 31.11.3 [宏 EXT:MEMOIZED](#MacroMemoized)
> * 31.11.4 [宏 EXT:WITH-COLLECT](#MacroWithCollect)
> * 31.11.5 [宏 EXT:COMPILE-TIME-VALUE](#MacroCompleTimeValue)
> * 31.11.6 [宏 EXT:WITH-GENSYMS](#MacroWithGensyms)
> * 31.11.7 [函数 EXT:REMOVE-PLIST](#FunRemovePlist)
> * 31.11.8 [宏 EXT:WITH-HTML-OUTPUT 和 EXT:WITH-HTTP-OUTPUT](#MacroWithOutput)
> * 31.11.9 [函数 EXT:OPEN-HTTP 和 宏 EXT:WITH-HTTP-INPUT](#OpenHttpAndInput)
> * 31.11.10 [变量 CUSTOM:\*HTTP-LOG-STREAM*](#VarHttpLogStream)
> * 31.11.11 [函数 EXT:BROWSE-URL](#FunBrowseUrl)
> * 31.11.12 [变量 CUSTOM:\*HTTP-PROXY*](#VarHttpProxy)
> * 31.11.13 [函数 EXT:CANONICALIZE](#FunCanonicalize)

### 31.11.1 <span id = "MacroThe">宏 EXT:ETHE</span>

(EXT:ETHE value-type form) 对编译和解释的代码强制进行类型检查.

### 31.11.2 <span id = "MacroLetf">宏 EXT:LETF & EXT:LETF*</span>

这些宏类似 LET 和 LET*, 除此之外它们可以绑定 places, 甚至是 multiple values 的 places. 比如:

``` LISP
(letf (((values a b) form)) ...)
```

等价于

```LISP
(letf (((values a b) form)) ...)
```

当

```LISP
(letf (((first l) 7)) ...)
```

近似等价于

```LISP
(LET* ((#:g1 l) (#:g2 (first #:g1)))
   (UNWIND-PROTECT (PROGN (SETF (first #:g1) 7) ...)
      (SETF (first #:g1) #:g2)))
```

### 31.11.3 <span id = "MacroMemoized">宏 EXT:MEMOIZED</span>

(EXT:MEMOIZED form) 从 form 第一次赋值就记录了这个主要的值.

### 31.11.4 <span id = "MacroWithCollect">宏 EXT:WITH-COLLECT</span>

跟 LOOP's COLLECT 结构类似, 除此之外这个看上去更加的 "Lispy" 而且可以出现在任意深度. 这个定义了局部的宏 (通过 MACROLET) ,去搜集对象放入多个list中，然后以多值的方式返回多个list. 比如,

```LISP
(ext:with-collect (c0 c1)
  (dotimes (i 10) (if (oddp i) (c0 i) (c1 i))))
⇒ (1 3 5 7 9) ;
⇒ (0 2 4 6 8)
```

以多值的方式返回两个列表.

### 31.11.5 <span id = "MacroCompleTimeValue">宏 EXT:COMPILE-TIME-VALUE</span>

有时候有人想要在编译的时候调用一个开销较大的函数来给 #P".fas" 文件写入一些私有的值, 来对加载 #P".fas" 文件进行加速. 比如, 让你的 primes.lisp 文件成这样：
```LISP
(defun primes-list (limit)
  "Return the list of all primes smaller than LIMIT."
  ...)
(defvar *all-primes* (compile-time-value (primes-list MOST-POSITIVE-FIXNUM)))
```

然后

``(LOAD "primes.lisp")``
    
    不会调用 primes-list 而且 *all-primes*会是 NIL.

``(COMPILE-FILE "primes.lisp")``

    会调用 primes-list (而且很可能需要很长时间) 而且会将结果写入到 (COMPILE-FILE-PATHNAME "primes.lisp")

``(LOAD (COMPILE-FILE-PATHNAME "primes.lisp"))``

    不会调用 primes-list 但是 *all-primes* 会是编译时计算的值.

一种替代方式是 memory image, 比 #P".fas" 文件的方式快但是不便携.

### 31.11.6 <span id = "MacroWithGensyms">宏 EXT:WITH-GENSYMS</span>

和 Paul Graham 的书 “On Lisp” 中同名的宏类似, 这个宏在写其他宏时很有用:
```LISP
(with-gensyms ("FOO-" bar baz zot) ...)
```
展开为

```LISP
(let ((bar (gensym "FOO-BAR-"))
      (baz (gensym "FOO-BAZ-"))
      (zot (gensym "FOO-ZOT-")))
  ...)
```

### 31.11.7 <span id = "FunRemovePlist">函数 EXT:REMOVE-PLIST</span>

与 REMOVE 和 REMF 类似，这个函数从一个属性列表移除属性. 它不是破坏性的所以可以用于 &REST 参数去移除一些关键字参数, 比如,

```LISP
(defmacro with-foo ((&KEY foo1 foo2) &BODY body)
  `(... ,foo1 ... ,foo2 ... ,@body))
(defmacro with-foo-bar ((&REST opts &KEY bar1 bar2
                         &ALLOW-OTHER-KEYS)
                        &BODY body)
  `(with-foo (,@(remove-plist opts :bar1 :bar2)
     ... ,bar1 ... ,bar2 ... ,@body)))
(defun foo-bar ()
  (with-foo-bar (:bar1 1 :foo2 2) ...))
```
在这里 WITH-FOO 将不会从 FOO-BAR 收到 :BAR1 1 参数.

### 31.11.8 <span id = "MacroWithOutput">宏 EXT:WITH-HTML-OUTPUT 和 EXT:WITH-HTTP-OUTPUT</span>

在 inspect.lisp 里定义, 这些宏在定义基础http服务器时很有用.

### 31.11.9. <span id = "OpenHttpAndInput">函数 EXT:OPEN-HTTP 和 宏 EXT:WITH-HTTP-INPUT</span>

在 clhs.lisp 中定义, 它们允许通过http协议从互联网上下载数据. (EXT:OPEN-HTTP url &KEY :IF-DOES-NOT-EXIST :LOG) 打开一个通往 url 主机的 socket 连接, 发送 GET 请求, 然后返回2个值: 一个 SOCKET:SOCKET-STREAM 和 content length. (EXT:WITH-HTTP-INPUT (variable url) &BODY body) 把 EXT:OPEN-HTTP 返回的 SOCKET:SOCKET-STREAM 绑定给 variable 然后执行 body. (EXT:WITH-HTTP-INPUT ((variable contents) url) &BODY body) 另外把内容长度绑定给 contents .

EXT:OPEN-HTTP 会在开始时检查 CUSTOM:\*HTTP-PROXY* ，如果 CUSTOM:\*HTTP-PROXY* 是 NIL 然后解析环境变量 HTTP_PROXY .

这个 :LOG 参数绑定给 CUSTOM:\*HTTP-LOG-STREAM*.

### 31.11.10 <span id = "VarHttpLogStream">变量 CUSTOM:\*HTTP-LOG-STREAM*</span>

函数 EXT:OPEN-HTTP 用 CUSTOM:\*HTTP-LOG-STREAM* 记录了它的动作，初始值给 \*TERMINAL-IO*.

### 31.11.11 <span id = "FunBrowseUrl">函数 EXT:BROWSE-URL</span>

函数 (EXT:BROWSE-URL url &KEY :BROWSER :OUT) 用一个URL来调用浏览器. browser (默认是 CUSTOM:\*BROWSER\*) 应该是 CUSTOM:\*BROWSERS* association list 中一个合法的关键字. :OUT 指定了进度信息打印的流 (默认是\*STANDARD-OUTPUT*).

### 31.11.12 <span id = "VarHttpProxy">变量 CUSTOM:\*HTTP-PROXY*</span>

如果你在代理服务器下, 你需要设置给 CUSTOM:\*HTTP-PROXY* 一个 LIST (name:password host port). 默认情况下,使用环境变量中的  http_proxy , 期望的格式是 "name:password@host:port". 如果没有 #\@ , name 和 password 就是 NIL. 如果没有 #\: , password (或者 port) 也是 NIL.

可以通过 (EXT:HTTP-PROXY &OPTIONAL (STRING (EXT:GETENV "http_proxy"))) 函数去重置 CUSTOM:\*HTTP-PROXY*.

### 31.11.13 <span id = "FunCanonicalize">函数 EXT:CANONICALIZE</span>

如果你想在远程传输之前规范化一个值，你可以把它和一个函数序列一起传给 EXT:CANONICALIZE : (EXT:CANONICALIZE value functions &KEY (test 'EQL) (max-iter 1024)) 会对 value 调用 functions 直到它在 test 下稳定(那个应该是一个合法的 HASH-TABLE-TEST) and 然后返回一个稳定的值以及这个稳定性需要的迭代次数.

比如, clx/new-clx 在把 X Window System 返回的损坏的编码名字传给 EXT:MAKE-ENCODING 之前会和 XLIB:\*CANONICALIZE-ENCODING* 一起使用去修复这个名字 (比如, 转换 "iso8859-1" 为 "ISO-8859-1") . 如果你在 clx/new-clx 遇到一个 EXT:ENCODING ERROR , 你可以通过增加这个变量来避免.

## 31.12 <span id = "CustClispBehavior">定制 CLISP 行为</span>

用户定制需要的变量 和 函数 都位于包 “CUSTOM” 中，所以可以通过 (APROPOS "" "CUSTOM") 列举出来:


|         |          |
| ------------- |-------------|
|CUSTOM:\*ANSI*	|CUSTOM:\*APPLYHOOK*|
|CUSTOM:\*APROPOS-DO-MORE*	|CUSTOM:\*APROPOS-MATCHER*|
|CUSTOM:\*BREAK-ON-WARNINGS*	|CUSTOM:\*BROWSER*|
|CUSTOM:\*BROWSERS*	|CUSTOM:CLHS-ROOT|
|CUSTOM:\*CLHS-ROOT-DEFAULT*	|CUSTOM:\*COERCE-FIXNUM-CHAR-ANSI*|
|CUSTOM:\*COMPILE-WARNINGS*	|CUSTOM:\*COMPILED-FILE-TYPES*|
|CUSTOM:\*CURRENT-LANGUAGE*	|CUSTOM:\*DEFAULT-FILE-ENCODING*|
|CUSTOM:\*DEFAULT-FLOAT-FORMAT*	|CUSTOM:\*DEFAULT-TIME-ZONE*|
|CUSTOM:\*DEFTYPE-DEPTH-LIMIT*	|CUSTOM:\*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST*|
|CUSTOM:\*DEVICE-PREFIX*	|CUSTOM:\*EDITOR*|
|CUSTOM:\*EQ-HASHFUNCTION*	|CUSTOM:\*EQL-HASHFUNCTION*|
|CUSTOM:\*EQUAL-HASHFUNCTION*	|CUSTOM:\*ERROR-HANDLER*|
|CUSTOM:\*EVALHOOK*	|CUSTOM:\*FILL-INDENT-SEXP*|
|CUSTOM:\*FINI-HOOKS*	|CUSTOM:\*FLOATING-POINT-CONTAGION-ANSI*|
|CUSTOM:\*FLOATING-POINT-RATIONAL-CONTAGION-ANSI*	|CUSTOM:\*FOREIGN-ENCODING*|
|CUSTOM:\*HTTP-LOG-STREAM*	|CUSTOM:\*HTTP-PROXY*|
|CUSTOM:IMPNOTES-ROOT	|CUSTOM:\*IMPNOTES-ROOT-DEFAULT*|
|CUSTOM:\*INIT-HOOKS*	|CUSTOM:\*INSPECT-BROWSER*|
|CUSTOM:\*INSPECT-FRONTEND*	|CUSTOM:\*INSPECT-LENGTH*|
|CUSTOM:\*INSPECT-PRINT-LENGTH*	|CUSTOM:\*INSPECT-PRINT-LEVEL*|
|CUSTOM:\*INSPECT-PRINT-LINES*	|CUSTOM:\*LIB-DIRECTORY*|
|CUSTOM:\*LOAD-COMPILING*	|CUSTOM:\*LOAD-ECHO*|
|CUSTOM:\*LOAD-LOGICAL-PATHNAME-TRANSLATIONS-DATABASE*	|CUSTOM:\*LOAD-OBSOLETE-ACTION*|
|CUSTOM:\*LOAD-PATHS*	|CUSTOM:\*LOOP-ANSI*|
|CUSTOM:\*MERGE-PATHNAMES-ANSI*	|CUSTOM:\*MISC-ENCODING*|
|CUSTOM:\*PACKAGE-TASKS-TREAT-SPECIALLY*	|CUSTOM:\*PARSE-NAMESTRING-ANSI*|
|CUSTOM:\*PARSE-NAMESTRING-DOT-FILE*	|CUSTOM:\*PATHNAME-ENCODING*|
|CUSTOM:\*PHASE-ANSI*	|CUSTOM:\*PPRINT-FIRST-NEWLINE*|
|CUSTOM:\*PRINT-CLOSURE*	|CUSTOM:\*PRINT-EMPTY-ARRAYS-ANSI*|
|CUSTOM:\*PRINT-INDENT-LISTS*	|CUSTOM:\*PRINT-SYMBOL-PACKAGE-PREFIX-SHORTEST*|
|CUSTOM:\*PRINT-PATHNAMES-ANSI*	|CUSTOM:\*PRINT-PRETTY-FILL*|
|CUSTOM:\*PRINT-RPARS*	|CUSTOM:\*PRINT-SPACE-CHAR-ANSI*|
|CUSTOM:\*PRINT-UNREADABLE-ANSI*	|CUSTOM:\*PROMPT-BODY*|
|CUSTOM:\*PROMPT-BREAK*	|CUSTOM:\*PROMPT-FINISH*|
|CUSTOM:\*PROMPT-START*	|CUSTOM:\*PROMPT-STEP*|
|CUSTOM:\*REOPEN-OPEN-FILE*	|CUSTOM:\*REPORT-ERROR-PRINT-BACKTRACE*|
|CUSTOM:\*SAVEINITMEM-VERBOSE*	|CUSTOM:\*SEQUENCE-COUNT-ANSI*|
|CUSTOM:\*SOURCE-FILE-TYPES*	|CUSTOM:\*SUPPRESS-CHECK-REDEFINITION*|
|CUSTOM:\*SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING*	|CUSTOM:\*SYSTEM-PACKAGE-LIST*|
|CUSTOM:\*TERMINAL-ENCODING*	|CUSTOM:\*TRACE-INDENT*|
|CUSTOM:\*USER-COMMANDS*	|CUSTOM:\*USER-LIB-DIRECTORY*|
|CUSTOM:\*USER-MAIL-ADDRESS*	|CUSTOM:\*WARN-ON-FLOATING-POINT-CONTAGION*|
|CUSTOM:\*WARN-ON-HASHTABLE-NEEDING-REHASH-AFTER-GC*	|CUSTOM:\*WITH-HTML-OUTPUT-DOCTYPE*|

注意

    这些变量中有一些是只能用在特定平台的.

你可以在构建clisp之前在构建目录的 config.lisp 文件里去设置这些变量 (以及做任何你想做的定制) . 要不然, 在构建 CLISP 后, 或者你正在使用一个 CLISP 的二进制发行版，你可以修改 config.lisp, 编译然后加载它, 然后保存为 memory image. 最后, 你可以创建一个 RC file 无论何时 CLISP 启动时会被加载.

## 31.13 <span id = "CodeWalker">Code Walker</span>
你可以使用 EXT:EXPAND-FORM 去展开所有在一个结构里的宏,符号宏等等, 配置:

```LISP
(EXT:EXPAND-FORM '(macrolet ((bar (x) `(print ,x)))
                    (macrolet ((baz (x) `(bar ,x)))
                      (symbol-macrolet ((z 3))
                        (baz z)))))
⇒ (locally (print 3))     ; the expansion
⇒ T     ; indicator: some expansion has actually been done
```

有时这个称之为 “code walker”, 除此之外一个code walker很可能会原封不动地离开 MACROLET 和 SYMBOL-MACROLET 结构，只是做了展开.

警告

    函数 EXT:EXPAND-FORM 是 CLISP 解释器的输出部分 (或者说是 EVAL), 所以它假定 EVAL-WHEN 的 :EXECUTE 情况来展开结构,因此不适合那些稍后传递给编译器的结构 :

```LISP
(EXT:EXPAND-FORM '(EVAL-WHEN (:COMPILE-TOPLEVEL) (foo)))
⇒ NIL ;
⇒ T
(EXT:EXPAND-FORM '(EVAL-WHEN (:LOAD-TOPLEVEL) (foo)))
⇒ NIL ;
⇒ T
```
