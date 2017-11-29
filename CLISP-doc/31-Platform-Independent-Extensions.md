# 章节 31. 平台不相关的扩展

> * 31.1. [定制CLISP进程初始化和终止行为](#CustInitTerm)
>> * 31.1.1. [开始到结束](#CradleToGrave)
>> * 31.1.2. [定制初始化](#CustInit)
>>> * 31.1.2.1. [CUSTOM:\*INIT-HOOKS* 和初始化函数的区别](#DiffInitHookIInitFun)
>> * 31.1.3. [定制结束](#CustTerm)
> * 31.2. [Saving an Image](#SaveAnImage)
>> * 31.2.1. [Image Portability](#ImagePortability)
> * 31.3. [Quitting CLISP](#QuittingCLISP)
> * 31.4. [Internationalization of CLISP](#Internationalization)
>> * 31.4.1. [The Language](#Language)
> * 31.5. [Encodings](#Encodings)
>> * 31.5.1. [Introduction](#EncodingIntroduction)
>> * 31.5.2. [Character Sets](#CharacterSets)
>> * 31.5.3. [Line Terminators](#LineTerminators)
>> * 31.5.4. [Function EXT:MAKE-ENCODING](#FunMakeEncoding)
>> * 31.5.5. [Function EXT:ENCODING-CHARSET](#FunEncodingCharset)
>> * 31.5.6. [Default encodings](#DefaultEncodings)
>>> * 31.5.6.1. [Default line terminator](#DefaultLineTerminator)
>> * 31.5.7. [Converting between strings and byte vectors](#CovertStrToByte)
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



