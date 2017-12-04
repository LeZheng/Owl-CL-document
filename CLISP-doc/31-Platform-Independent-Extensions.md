# 章节 31. 平台不相关的扩展

> * 31.1. [定制CLISP进程初始化和终止行为](#CustInitTerm)
>> * 31.1.1. [开始到结束](#CradleToGrave)
>> * 31.1.2. [定制初始化](#CustInit)
>>> * 31.1.2.1. [CUSTOM:\*INIT-HOOKS* 和初始化函数的区别](#DiffInitHookIInitFun)
>> * 31.1.3. [定制结束](#CustTerm)
> * 31.2. [保存为镜像](#SaveAnImage)
>> * 31.2.1. [镜像的移植性](#ImagePortability)
> * 31.3. [退出 CLISP](#QuittingCLISP)
> * 31.4. [CLISP 国际化](#Internationalization)
>> * 31.4.1. [语言](#Language)
> * 31.5. [编码](#Encodings)
>> * 31.5.1. [介绍](#EncodingIntroduction)
>> * 31.5.2. [字符集](#CharacterSets)
>> * 31.5.3. [行终止](#LineTerminators)
>> * 31.5.4. [函数 EXT:MAKE-ENCODING](#FunMakeEncoding)
>> * 31.5.5. [函数 EXT:ENCODING-CHARSET](#FunEncodingCharset)
>> * 31.5.6. [默认的 encodings](#DefaultEncodings)
>>> * 31.5.6.1. [默认的行终止符](#DefaultLineTerminator)
>> * 31.5.7. [字符串和字节向量的转换](#CovertStrToByte)
> * 31.6. [Generic streams](#GenericStreams)
> * 31.7. [Weak Objects](#WeakObjects)
>> * 31.7.1. [Weak Pointers](#WeakPointers)
>> * 31.7.2. [Weak Lists](#WeakLists)
>> * 31.7.3. [Weak “And” Relations](#WeakAndRela)
>> * 31.7.4. [Weak “Or” Relations](#WeakOrRela)
>> * 31.7.5. [Weak Associations](#WeakAssoc)
>> * 31.7.6. [Weak “And” Mappings](#WeakAndMap)
>> * 31.7.7. [Weak “Or” Mappings](#WeakOrMap)
>> * 31.7.8. [Weak Association Lists](#WeakAssociation)
>> * 31.7.9. [Weak Hash Tables](#WeakHashTable)
> * 31.8. [Finalization](#Finalization)
> * 31.9. [The Prompt](#Prompt)
> * 31.10. [Maximum ANSI CL compliance](#MaxCLCompliance)
> * 31.11. [Additional Fancy Macros and Functions](#AdditionMacroFun)
>> * 31.11.1. [Macro EXT:ETHE](#MacroThe)
>> * 31.11.2. [Macros EXT:LETF & EXT:LETF*](#MacroLetf)
>> * 31.11.3. [Macro EXT:MEMOIZED](#MacroMemoized)
>> * 31.11.4. [Macro EXT:WITH-COLLECT](#MacroWithCollect)
>> * 31.11.5. [Macro EXT:COMPILE-TIME-VALUE](#MacroCompleTimeValue)
>> * 31.11.6. [Macro EXT:WITH-GENSYMS](#MacroWithGensyms)
>> * 31.11.7. [Function EXT:REMOVE-PLIST](#FunRemovePlist)
>> * 31.11.8. [Macros EXT:WITH-HTML-OUTPUT and EXT:WITH-HTTP-OUTPUT](#MacroWithOutput)
>> * 31.11.9. [Function EXT:OPEN-HTTP and macro EXT:WITH-HTTP-INPUT](#OpenHttpAndInput)
>> * 31.11.10. [Variable CUSTOM:\*HTTP-LOG-STREAM*](#VarHttpLogStream)
>> * 31.11.11. [Function EXT:BROWSE-URL](#FunBrowseUrl)
>> * 31.11.12. [Variable CUSTOM:\*HTTP-PROXY*](#VarHttpProxy)
>> * 31.11.13. [Function EXT:CANONICALIZE](#FunCanonicalize)
> * 31.12. [Customizing CLISP behavior](#CustClispBehavior)
> * 31.13. [Code Walker](#CodeWalker)

## 31.1. <span id = "CustInitTerm">定制CLISP进程初始化和终止行为</span>
### 31.1.1. <span id = "CradleToGrave">开始到结束</span>
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
## 31.1.2. <span id = "CustInit">定制初始化</span>

CUSTOM:\*INIT-HOOKS* 像这样运行:<br>
```Lisp
(IGNORE-ERRORS (MAPC #'FUNCALL CUSTOM:*INIT-HOOKS*))
```
#### 31.1.2.1. <span id = "DiffInitHookIInitFun">CUSTOM:\*INIT-HOOKS* 和初始化函数的区别</span>

CUSTOM:\*INIT-HOOKS* 无视命令行选项总是运行的，甚至在标语被打印之前.
init function 只有在进入 read-eval-print loop 后运行 而且只是在第一个提示符被打印之前.

## 31.1.3. <span id = "CustTerm">定制结束</span>

CUSTOM:\*FINI-HOOKS* 像这样运行:
```Lisp
(MAPC #'FUNCALL CUSTOM:*FINI-HOOKS*)
```

## 31.2. <span id = "SaveAnImage">保存为镜像</span>

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

## 31.3. <span id = "QuittingCLISP">退出 CLISP</span>

这几个函数

```Lisp
(EXT:EXIT &OPTIONAL status)
(EXT:QUIT &OPTIONAL status)
(EXT:BYE &OPTIONAL status)
```

- 都是同义的 - 表示中断 CLISP. 如果 status 不是NIL, CLISP 以返回一个数值形式的错误 status终止, i.e., 操作系统环境会被通知这个CLISP 会话失败.

最后的分隔符( Final delimiter )也会中断 CLISP.


## 31.4. <span id = "Internationalization">CLISP 国际化</span>

词汇表

    Internationalization (“i18n”)
        预备程序以便它可以在不改动更多代码的情况下，使用多国语言以及国际文化的交流.

    Localization (“l10n”)
        提供数据 - 大部分为原文翻译 - 对于一个工作在流行语言下包括流行文化交流的国际化应用来说是有必要的.

CLISP 是国际化的, 对这些语言 English, German, French, Spanish, Dutch, Russian, and Danish也是地区化的. CLISP 也支持国际化的 Lisp 程序, 通过 GNU gettext, 见 Section 33.2, “Internationalization of User Programs”.

### 31.4.1. <span id = "Language">语言</span>

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

## 31.5. <span id = "Encodings">编码</span>

### 31.5.1. <span id = "EncodingIntroduction">介绍</span>

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

### 31.5.2. <span id = "CharacterSets">字符集</span>

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

### 31.5.3. <span id = "LineTerminators">行终止</span>

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

### 31.5.4. <span id = "FunMakeEncoding">函数 EXT:MAKE-ENCODING</span>

    函数 (EXT:MAKE-ENCODING &KEY :CHARSET :LINE-TERMINATOR :INPUT-ERROR-ACTION :OUTPUT-ERROR-ACTION) 返回一个 EXT:ENCODING. 这个 :CHARSET 参数可能是encoding, 一个字符串, 或者 :DEFAULT. 这个 line terminator 参数可能的值是关键字 :UNIX, :MAC, :DOS.

    这个 :INPUT-ERROR-ACTION 参数指明在转换字节到字符时遇到非法字节序列做什么. 它的值可能是 :ERROR, :IGNORE 或者是一个字符会被用到的. 这个 UNICODE 字符 #\uFFFD 通常表示在输入序列中有个异常.

    这个 :OUTPUT-ERROR-ACTION 参数指明在转换字符到字节时遇到非法字节序列做什么. 它的值可能是 :ERROR, :IGNORE, 或者是一个字节. 如果这个UNICODE 字符 #\uFFFD 在这个字符集中是可编码的，就可以在这里使用.

### 31.5.5. <span id = "FunEncodingCharset">函数 EXT:ENCODING-CHARSET</span>

平台依赖: 仅限在用编译时( compile-time )标志 UNICODE构建的 CLISP

这个函数 (EXT:ENCODING-CHARSET encoding) 返回 encoding的字符集,一个 SYMBOL 或者一个 STRING.
注意

    警告
    (STRING (EXT:ENCODING-CHARSET encoding)) 不一定是一个合法的 MIME 名字.

### 31.5.6. <span id = "DefaultEncodings">默认的 encodings</span>

除了每个 file/pipe/socket STREAM 包含了一个 encoding, 以下 SYMBOL-MACRO 的地方包含了全局的 EXT:ENCODINGs:

SYMBOL-MACRO CUSTOM:*DEFAULT-FILE-ENCODING*.当没有指定这个 :EXTERNAL-FORMAT 参数时，这个 SYMBOL-MACRO place CUSTOM:*DEFAULT-FILE-ENCODING* 就是那个被用于 file/pipe/socket STREAM 的 encoding.

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

#### 31.5.6.1. <span id = "DefaultLineTerminator">默认的行终止符</span>

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

### 31.5.7. <span id = "CovertStrToByte">字符串和字节向量的转换</span>

Encodings 也能用于根据encoding直接转换字符串和它们对应的字节序列.

(EXT:CONVERT-STRING-FROM-BYTES vector encoding &KEY :START :END)

    根据给的 encoding 转换 vector (a (VECTOR (UNSIGNED-BYTE 8))) 从 start 到 end 的子序列为一个 STRING,然后返回结果字符串.

(EXT:CONVERT-STRING-TO-BYTES string encoding &KEY :START :END)

    根据给的 encoding 转换 string 从 start 到 end 子序列为 (VECTOR (UNSIGNED-BYTE 8)), 然后返回一个结果字节向量.
