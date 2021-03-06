# 章节 32. 平台相关的扩展

> * 32.1 [随机屏幕访问](#RandomScreenAccess)
> * 32.2 [扩展模块](#ExternalModules)
> * 32.3 [外部函数调用机制](#ForeignFunctionCall)
> * 32.4 [套接字流](#SocketStreams)
> * 32.5 [多线程执行](#MultipleThreadsExecution)
> * 32.6 [clisp 快速启动的交付](#QuickstartingCLISP)
> * 32.7 [Shell, 管道和打印](#ShellPipesPrinting)
> * 32.8 [操作系统环境](#OSEnvironment)

## 32.1. <span id = "RandomScreenAccess">随机屏幕访问</span>

平台依赖: 仅限 UNIX, Win32.

``(SCREEN:MAKE-WINDOW)``
    
    返回一个 WINDOW-STREAM. 只要这个流被打开, 这个终端就一直在 cbreak/noecho 模式. *TERMINAL-IO* 在这段时间里不应该被用于输入输出. (应该使用 EXT:WITH-KEYBOARD 和 EXT:*KEYBOARD-INPUT* .)

``(SCREEN:WITH-WINDOW . body)``

    绑定 SCREEN:*WINDOW* 给一个 WINDOW-STREAM 然后执行 body. 当离开body时，这个流保证会被关闭. 在执行期间, *TERMINAL-IO* 不应该被使用, 同上.

``(SCREEN:WINDOW-SIZE window-stream)``

    返回这个窗口的size, 以两个值返回: height (= ymax+1) 和 width (= xmax+1).

``(SCREEN:WINDOW-CURSOR-POSITION window-stream)``

    返回这个窗口中的光标位置, 以两个值返回: line (≥0, ≤ymax, 0 意味着顶层), column (≥0, ≤xmax, 0 意味着左边缘).

``(SCREEN:SET-WINDOW-CURSOR-POSITION window-stream line column)``

    设置窗口中的光标位置.

``(SCREEN:CLEAR-WINDOW window-stream)``

    清理窗口内容，把光标放在左上角.

``(SCREEN:CLEAR-WINDOW-TO-EOT window-stream)``

    清理窗口中从光标位置至窗口结束内容.

``(SCREEN:CLEAR-WINDOW-TO-EOL window-stream)``

    清理窗口中从光标位置到行结束.

``(SCREEN:DELETE-WINDOW-LINE window-stream)``

    移除光标行, 把以后的行上移，清除最后行.

``(SCREEN:INSERT-WINDOW-LINE window-stream)``

    在光标行插入一行, 以后的行下移一位.

``(SCREEN:HIGHLIGHT-ON window-stream)``

    打开输出高亮.

``(SCREEN:HIGHLIGHT-OFF window-stream)``

    关闭输出高亮.

``(SCREEN:WINDOW-CURSOR-ON window-stream)``

    使光标可见, 大部分实现里是一个光标块.

``(SCREEN:WINDOW-CURSOR-OFF window-stream)``

    使光标不可见, 在实现中可能可用.

## 32.2. <span id = "ExternalModules">扩展模块</span>

> * 32.2.1 [概述](#Overview)
> * 32.2.2 [模块初始化](#ModuleInitialization)
> * 32.2.3 [模块释放](#ModuleFinalization)
> * 32.2.4 [函数 EXT:MODULE-INFO](#FunModuleInfo)
> * 32.2.5 [函数 SYS::DYNLOAD-MODULES](#FunDynloadModule)
> * 32.2.6 [示例](#ExternalModulesExample)
> * 32.2.7 [模块工具](#ModuleTools)
> * 32.2.8 [平衡比较: “FFI” vs. C modules](#FFIVSCModule)
> * 32.2.9 [包含在源码发行版中的模块](#ModulesIncludeSrcDist)

平台依赖:仅限 UNIX, Win32.

**Win32 上的模块**

    当使用 Cygwin 或者 MinGW 时，本章节所述的所有东西都可以不经变换工作在 Win32, 除了一样 - 你需要去替换 lisp.run  的扩展名 run 为 Win32 可执行文件的扩展名 exe.
    出于历史原因, 所有示例都假定 UNIX 并为 CLISP runtime使用 run 文件类型 (“extension”) . 这个并不意味着它不能工作在 Win32 上.

### 32.2.1 <span id = "Overview">概述</span>

外部模块（External modules） 是一种为 CLISP 添加扩展的机制 (比方说用 C 写的 ) . 通过使用外部模块扩展 CLISP 需要去创建一个 module set 并且 通过使用 clisp-link 去创建一个包含扩展的新的 linking set 来加到一个已存在的 linking set .
模块集（Module Set）

一个模块是一个定义了额外（非核心）Lisp对象、符号和函数的代码片段 (C 或者 Lisp) . 和 link.sh 一起, 它描述来如何添加这个模块到已经存在的 CLISP 中,这就组成了一个模块集（ module set ）.

更形式地说, 模块集是包含了以下部分的目录:

link.sh

        一些 /bin/sh 命令, 准备链接用的目录, 并且设置一些环境变量, 见 the section called "Module set variables"

**定义这个模块功能的所有其他文件**

        被 link.sh 所需要的

在 link.sh 中这个模块集目录被 $modulename/ 这个变量所引用.

一个模块的名字必须用这些字符定义： A-Z, a-z, _, 0-9.

模块名字 “clisp” 是预留的.

**链接集（Linking Set）**

一个链接集（ linking set ）是一个文件集合 (runtime, memory image &c) ，它执行两个主要任务:

    1.运行 CLISP: 去运行一个被包含在链接集目录的 CLISP , 调用
    $ directory/lisp.run -M directory/lispinit.mem
    或者
    $ clisp -K directory
    (这个是比较推荐的, 因为这种方式也传递 -B 给 runtime).

    2.添加一个模块集 module set 去创建一个新的包含这个模块功能的链接集 linking set.

这个 CLISP 构建目录包含了三个链接集在目录 boot, base, 以及 full, 另外一个 CLISP 安装版通常包含两个链接集: base, 以及 full

更加形式地说, 一个链接集 linking set 是一个至少包含以下文件的目录:

lisp.run

        一个可执行的 runtime

lispinit.mem

        是一个 memory image

modules.h

        这个 linking set包含的模块列表

modules.o

        这个 linking set 中的模块编译链表

makevars

        一些 /bin/sh 命令, 配置的变量

    CC          C编译器
    CPPFLAGS    预处理和编译时C编译器的标识
    CFLAGS      编译和链接时C编译器的标识
    CLFLAGS     链接时C编译器的标识
    LIBS        链接时用的库 (包括当前链接集目录中的, 或者是系统范围的)
    X_LIBS      额外要用的的 X Window System 库
    RANLIB      ranlib 命令
    FILES       链接时需要的文件列表

所有这些文件
        
        列在 makevars

**模块集和链接集上的操作**

通过使用 clisp-link 来创建, 添加和安装模块集.

见 Section 32.2.6, “Example”.

**模块集中的变量**

以下这些变量应该被定义在 link.sh.

NEW_FILES
    
    属于这个模块集的空格分割的对象文件列表，并且属于每一个链接这个模块集的链接集 .

NEW_LIBS

    这个空格分割的对象文件、库还有需要在链接一个属于新的 linking set 的 lisp.run 时给C编译器的C编译器开关列表.

NEW_MODULES

    属于这个模块集的模块名字列表，以空格分割. 通常, 每一个这个模块集中的  #P".c" 文件定义了一个它自己的模块. 这个模块名字通常来源于模块名.

TO_LOAD
    
    在构建一个属于新的链接集的 lispinit.mem 之前，需要加载的Lisp文件的列表，用空格分割.

TO_PRELOAD (optional)

    在构建这个属于一个新的 linking set 的 lispinit.mem 之前，这个变量里列出的Lisp文件的列表会被加载进一个中间 lispinit.mem 文件. 这个变量通常被用于创建 (或者解锁) 当一个新的 #P".c" 文件初始化时需要用到的  Lisp 包 . E.g., 这个 FFI:DEF-CALL-IN 函数必须归属于一个已定义的包; 见 Example 32.7, “Calling Lisp from C”. 你可以在 modules/syscalls/preload.lisp 和 modules/syscalls/link.sh.in 找到一个现成的例子。
    警告

**警告**

    如果你想解锁一个包, 你也必须在这里从 CUSTOM:*SYSTEM-PACKAGE-LIST* 里  DELETE 它 (见 Section 31.2, “Saving an Image”)  然后在其中一个 TO_LOAD 文件中再把它加到 CUSTOM:*SYSTEM-PACKAGE-LIST* 中. 见, 比如, modules/i18n/preload.lisp 和 modules/i18n/link.sh.in.

### 32.2.2 <span id = "ModuleInitialization">模块初始化</span>

每一个模块都有2个初始化函数:

void module__name__init_function_1 (struct module_t* module)

    在 CLISP 在加载一个 memory image 时发现在执行文件 (lisp.run) 中有一个模块在这个镜像保存时没有出现，就会去调用一次这个函数 . 它可以被用于创建Lisp对象, 比如函数或关键字，另外也被modprep 用于这个目的.

    你不需要自己定义这个函数; modprep 和 “FFI” 会帮你做这事.

    你如果使用 “FFI”, (FFI:C-LINES :init-once ...) 会给这个函数添加代码.
    警告

    这个包必须已存在并且被解锁, cf. TO_PRELOAD.
    警告

    如果你使用 modprep 并且定义了你自己的 “init-once” 函数, 它必须调用 module__name__init_function_1__modprep 函数!

void module__name__init_function_2 (struct module_t* module)

    每一次 CLISP 启动都会调用. 它可以被用于给外部地址绑定名字, 因为在每次 CLISP 被调用这个地址都可能会不同, 而且确实被 “FFI” (比如, 通过 FFI:DEF-CALL-OUT) 用于这个目的. 它也可以用于给这个模块对接的库设置参数, e.g., pcre 模块设置 pcre_malloc 和 pcre_free.

    你不需要自己去定义这个函数; modprep 和 “FFI” 帮你做这事.

    如果你使用 “FFI”, (FFI:C-LINES :init-always ...) 会给这个函数添加代码.

name 是这个模块的模块名 module name.

见 Section 31.1, “Customizing CLISP Process Initialization and Termination”.

### 32.2.3 <span id = "ModuleFinalization">模块释放</span>

每一个模块都有一个析构函数

void module__name__fini_function (struct module_t* module)

    在退出 CLISP 之前被调用.
    你不需要自己去定义这个函数; modprep 和 “FFI” 帮你做这事.

name 是这个模块的模块名 module name.

见 Section 31.1, “Customizing CLISP Process Initialization and Termination”.

### 32.2.4. <span id = "FunModuleInfo">函数 EXT:MODULE-INFO</span>

函数 (EXT:MODULE-INFO &OPTIONAL name verbose) 允许去查询一个模块在当前运行的镜像中是否可用. 当调用时没有传参,它会返回所有模块的名字, 以 “clisp” 作为第一个. 当 name 被指定而且是一个模块, 3个值会被返回 - name, subr-count, object-count. 当 verbose 不是NIL, 用C写的整个模块函数名列表(Subrs) 以及另外返回在C中可用的Lisp对象列表，总计5个值.

当 name 是 :FFI,返回用 :LIBRARY 打开的动态库列表. 当 verbose 不是 NIL, 返回DLL 名字和所有外部对象的关联列表.

### 32.2.5 <span id = "FunDynloadModule">函数 SYS::DYNLOAD-MODULES</span>

平台依赖: 仅限在构建时没有配置标志 --without-dynamic-modules 的 CLISP .
注意

动态加载不会工作在所有操作系统下 (需要 dlopen 或者等价的操作).
注意

你可能从来不会去显式地调用函数 (这就是为什么在 “SYSTEM” 里而不是 “EXT”). 你应该通过以下来安装你的模块

```SHELL
$ clisp-link install name
```
通过 (REQUIRE name). 来加载。

(SYS::DYNLOAD-MODULES filename ({name}+)) 函数加载共享对象或者库文件其中包含了一些扩展  CLISP 模块.
注意

这个方式不能被用于访问任意的动态库. 如果要达到这个目的, 应该传递 :LIBRARY 参数给 FFI:DEF-CALL-OUT 和FFI:DEF-C-VAR

这个 CLISP 的扩展模块是共享对象 (动态库)，其中包含了 module__name__subr_tab 变量. 这个为注册操作在lisp级别的结构的扩展函数提供机制.

为了 dlopen 这模块, 你应该给模块的编译选项添加 -fPIC . 大致就像

```SHELL
$ cc -shared -o name.so name.o
```

可能需要生成一个共享对象文件.

### 32.2.6 <span id = "ExternalModulesExample">示例</span>

32.1 创建一个带有GNU libc 绑定的模块集（ Create a module set with GNU libc bindings ）

示例 32.1 用 GNU libc 绑定创建一个 module set

为了在 GNU/Linux 操作系统下链接 “FFI” 绑定,需要以下几个步骤. (Step 1 和 step 2 不需要在这里执行.)

    $ clisp-link create linux /pathname/bindings/linux.c

        替换
        NEW_LIBS="$file_list"

        为
        NEW_LIBS="$file_list -lm"

        替换
        TO_LOAD=''

        为
        TO_LOAD='/pathname/bindings/linux.fas' 

    $ clisp -c /pathname/bindings/linux.lisp
    $ clisp-link add linux base base+linux
    $ base+linux/lisp.run -M base+linux/lispinit.mem -x '(linux:stat "/tmp")' 

### 32.2.7 <span id = "ModuleTools">模块工具</span>

这里有一些工具来帮助编写模块.

> * 32.2.7.1 [Modprep](#Modprep)
> * 32.2.7.2 [clisp.h](#CLISPH)
> * 32.2.7.3 [导出](#Exporting)

#### 32.2.7.1 <span id = "Modprep">Modprep</span>

如果你的模块是用 C 写的, 你可以在clisp发行版中用 modprep 预处理你的代码，用 DEFUN 宏来定义你的lisp函数:

```
DEFUN(MY-PACKAGE:MY-FUNCTION-NAME, arg1 arg2 &KEY FOO BAR) {
  if (!boundp(STACK_0)) STACK_0 = fixnum(0); /* BAR */
  if (!boundp(STACK_1)) STACK_1 = fixnum(1); /* FOO */
  pushSTACK(`MY-PACKAGE::SOME-SYMBOL`); /* create a symbol in the package */
  pushSTACK(`#(:THIS :IS :A :VECTOR)`); /* some vector, created once */
  pushSTACK(``MY-PACKAGE::MY-FUNCTION-NAME``); /* double `` means FUNCTION */
  VALUES1(listof(7)); /* cons up a new list and clean up the STACK */
}
```

然后 (MY-PACKAGE:MY-FUNCTION-NAME 'A 12 :FOO T) 会返回 (A 12 T 0 MY-PACKAGE::SOME-SYMBOL #(:THIS :IS :A :VECTOR) #<ADD-ON-SYSTEM-FUNCTION MY-PACKAGE:MY-FUNCTION-NAME>) (假定你从 “MY-PACKAGE” ﻿导出 MY-FUNCTION-NAME).

注意这个参数是传递到 STACK 上(最后一个参数会在最上面) ，因此在栈退出时会被清理.

另一些有用的宏是:

DEFVAR

    创建一个GC可见的私有对象。

DEFFLAGSET

    定义一个 C 函数，它会从 STACK 上移除几个标志参数然后返回组合的标志值.
    
DEFCHECKER
    定义一个从cpp常量到lisp符号和函数的映射，这个映射检查参数是否合适。

见 modules/syscalls/calls.c 还有其他包含进来的模块为例，以及文件 modprep 来看完整的文档说明.
警告

如果你操纵lisp对象, 你需要去注意 GC-safety.

#### 32.2.7.2 <span id = "CLISPH">clisp.h</span>

如果你的模块是用 C 写的, 你可能会要去 #include "clisp.h" 去访问 CLISP 对象. 你当然需要去读 "clisp.h" 以及一些已包含模块的代码, 但是这里有一些重要的建议，你需要牢记在心:

    Lisp 对象有一个类型对象.
    这个类型的变量被 lisp memory allocation 时是无效的(allocate_*() functions) - 但是 C 分配是有效的 (malloc et al) - 必须用cpp的宏pushSTACK(), popSTACK() 和 skipSTACK() 来把它保存在栈上.
    使用适当的 TheFoo() 宏来访问对象的槽, e.g., TheCons(my_cons)->Car, 最好先用 consp() 检查下类型.
    就像以上例子表明，参数是被传递到栈上。

    用 begin_system_call()/end_system_call() 对包装你的系统调用. 这些宏, 定义在 "clisp.h", 保存并复原被 CLISP 使用的可能被系统调用改变的寄存器.

    如果这个系统调用会阻塞 (e.g., read) 你就应该改用 begin_blocking_system_call() 和 end_blocking_system_call() . 当你在这个系统调用时，这种方式会允许其他线程运行. 这也就意味着，当这个系统调用时，有可能发生垃圾回收, 所以, 对于这个调用所有的对象类型的对象都是无效的. 也见 Section 35.5, “The burden of garbage-collection upon the rest of CLISP” 和 Section 35.7, “Garbage Collection and Multithreading”.

 
#### 32.2.7.3 <span id = "Exporting">导出</span>

如果你的模块用 “FFI” 对接一个 C 库, 你可能需要去使你的模块包 :CASE-SENSITIVE 然后在clisp发行版用 exporting.lisp 来创建 “FFI” 结构还有导出defun和defmacro等定义的符号. 以 modules/netica/, modules/matlab/ 还有 modules/bindings/ 为例.

### 32.2.8 <span id = "FFIVSCModule">平衡比较: “FFI” vs. C modules</span>

当决定了如何去写一个模块: 不管是 “FFI” 还是 C 配合 modprep, 都需要去考虑以下几个问题:

速度: C 获胜

    “FFI” 有显而易见的开销: 比较 RAWSOCK:HTONS (定义在 modules/rawsock/rawsock.c) 和

    (FFI:DEF-CALL-OUT htons (:name "htons") (:LIBRARY :default)
      (:ARGUMENTS (s ffi:short)) (:RETURN-TYPE ffi:short) (:LANGUAGE :stdc))

    然后注意到 RAWSOCK:HTONS 差不多快了3倍 (这个确实比较了 “FFI” 和正常的lisp函数调用的开销 ， 因为 htons 在计算上很平常). 只有在你多次调用一个简单的函数时，这个差异会比较大, 在这种情况下把循环多次调用放到 C 里面是比较合理的.

可移植性: C 获胜

    “FFI” 不像CLISP 那样广泛的移植.所以可能面临在一个平台上 CLISP 运行但是 “FFI” 不可用的情况.
    在 C 里面实现轻便性更容易些： 查看modules/rawsock/rawsock.c 中的  htonl 等等可替换的函数.
    考虑 C 结构在不同平台上有不同的布局, 以及在一些平台上函数需要64位的参数，而在另一些平台上需要32位的参数; 当 C 为你处理这些时， “FFI” 代码仍然需要去面对这些差异.

代码大小: “FFI” 获胜

    使用 “FFI” 你可以敲打更少的字符, 而且,如果你使用了 :LIBRARY 参数给 FFI:DEF-CALL-OUT 和 FFI:DEF-C-VAR, 你在调试你的代码时不需要离开 CLISP 会话. 这个在快速原型构建的时候有巨大的优越性.

UI: C 获胜

    去构建一个新颖的像Lisp的 UI (通过使用 &OPTIONAL 和 &KEY参数等等), 当你在C中直接做这个时，你会需要去为你的 FFI:FOREIGN-FUNCTIONs 写一个lisp 包装.  “多态” 同理: 接受不同类型的参数 (就像 e.g., POSIX:RESOLVE-HOST-IPADDR 所做的) 会需要为 FFI:FOREIGN-FUNCTION 提供个外部的Lisp包装.

学习曲线: 不明

    如果你对 C 特别熟悉, 你可能会发现 CLISP C 模块工具 (e.g., modprep) 非常容易使用.
    CLISP “FFI”, 从另一方面说是高层次的, 因此, 如果你对高级语言更熟悉,你可能发现 “FFI” 结构比 C 代码更熟悉.

安全性: 不明

    当导致了一个段错误:如果你的 FFI:DEF-CALL-OUT 结构没有描述这个 C 函数的遵守参数和返回值的原型 (包括 ALLOCATION), 你可能要去学习这个困难的方式. 如果这个模块是用 C 写的, 所有对自己不利的部分是公开的 (虽然对于大部分 C 用户是众所周知的). 然而, 用 C 时，也需要去关注 GC-safety.

注意

    选择的粒度是每一个函数: 同一个模块可以同时在 modprep 和 “FFI”  方式使用使用.

注意

    在一个模块中同时有 foo.lisp 和 foo.c 文件是一个好主意, 因为如果你之前添加一个 “FFI” 结构, COMPILE-FILE 会在后面重写它.


### 32.2.9<span id = "ModulesIncludeSrcDist">包含在源码发行版中的模块</span>

CLISP 的源代码发行版中自带一些模块(在构建一个二进制发行版中并不是必须的).

为了使用这些模块, 阅读 unix/INSTALL 然后在在一个 build-dir 目录构建 CLISP , 比如,

```SHELL
$ ./configure --with-module=pcre --with-module=clx/new-clx --cbc build-dir
```

然后用这个运行

```SHELL
./build-dir/clisp -K full
```

这会构建一个 base 链接集包含了 i18n, regexp 和 syscalls (可能还有 readline); 以及一个 full 链接集在base 的3或4个模块基础上包含了模块 clx/new-clx 和 pcre .

根据模块的主题，我们在这里把它们列举出来. 各自的文档见 Chapter 33, Extensions Implemented as Modules .

> * 32.2.9.1 [Base 模块](#BaseModules)
> * 32.2.9.2 [数据库, 目录等等](#DatabaseDir)
> * 32.2.9.3 [数学, 数据采集等等](#MathDateMin)
> * 32.2.9.4 [匹配, 文件处理等等](#MatchFileProc)
> * 32.2.9.5 [通信, 网络](#CommunicationNet)
> * 32.2.9.6 [图形](#Graphics)
> * 32.2.9.7 [绑定](#Bindings)
> * 32.2.9.8 [玩具和游戏](#ToyAndGame)

#### 32.2.9.1 <span id = "BaseModules">Base 模块</span>

不管是 base 还是 full 的 linking sets，默认的构建过程包含了以下的模块:

i18n

    用户程序的国际化.

regexp

    一个 POSIX 正则表达式匹配, 编译, 执行.

syscalls

    以平台依赖的方式进行系统调用.

readline (仅限 GNU readline 和 “FFI” 可用时可用)

    用这个模块一些先进的和历史的特征被导出。

这个 full 链接集的构成取决于平台和提供方的配置.

#### 32.2.9.2 <span id = "DatabaseDir">数据库, 目录等等</span>

gdbm

    Masayuki Onjo的 GNU DataBase Manager 接口 .

berkeley-db

    Berkeley DB 接口.

dirkey

    目录访问 (LDAP, Win32 registry etc).

postgresql

    从 CLISP 访问 PostgreSQL .

oracle

    John Hinsdale 的从 CLISP 访问 Oracle RDBMS.

#### 32.2.9.3 <span id = "MathDateMin">数学, 数据采集等等</span>

libsvm

    在 CLISP 里用 LibSVM 构建 Support Vector Machine 模型.

pari

    PARI 计算机代数系统的接口

matlab

    通过 MATLAB 做矩阵运算.

netica

    用 Netica C API 来使用贝叶斯网络和影响图.

#### 32.2.9.4 <span id = "MatchFileProc">匹配, 文件处理等等</span>

pcre

    这是 Perl Compatible Regular Expressions 匹配, 编译, 执行.

wildcard

    Shell (/bin/sh) 通配符 (路径名匹配).

zlib

    通过 ZLIB 压缩序列.

#### 32.2.9.5 <span id = "CommunicationNet">通信, 网络</span>

rawsock

    原始套接字的访问.

dbus

    D-Bus 的接口

fastcgi

    从 CLISP 访问 FastCGI.

#### 32.2.9.6 <span id = "Graphics">图形</span>

CLX

    从 CLISP 调用 Xlib 函数. 提供两种实现:
        clx/mit-clx, from MIT ftp://ftp.x.org/R5contrib/CLX.R5.02.tar.Z
            这个是标准实现
        clx/new-clx, by Gilbert Baumann
            更快而且有额外特性, 但是还没有完成. 请先尝试下，如果 clx/new-clx 对于你无法使用，请使用  clx/mit-clx . clx/new-clx 携带了很多例子, 请通过以下方式使用

        $ clisp -K full -i modules/clx/new-clx/demos/clx-demos.lisp -x '(clx-demos:run-all-demos)'

        并参照说明.

    这个功能文档在 http://www.stud.uni-karlsruhe.de/~unk6/clxman/, 在 CLISP 源代码发行版中的 modules/clx/clx-manual.tar.gz 也是可用的。

gtk2

     James Bailey写的使用 GTK+ 和 Glade 来创建GUI.

#### 32.2.9.7 <span id = "Bindings">绑定</span>

从 CLISP 调用操作系统函数.支持以下平台:

bindings/glibc

    Linux/GNU libc

bindings/win32

    Win32

#### 32.2.9.8 <span id = "ToyAndGame">玩具和游戏</span>

queens

    在 n×n 的棋盘上计算 n-queens 的解决方案数量 (供用户探索 CLISP 模块系统 一个娱乐例子).

modules/clx/new-clx/demos/sokoban.lisp

    一个自带 clx/new-clx 的例子.

## 32.3 <span id = "ForeignFunctionCall">外部函数调用机制</span>

平台依赖: 仅限大部分 UNIX, Win32 平台 .


> * 32.3.1 [引言](#FFCIntroduction)
> * 32.3.2 [概览](#FFCOverview)
> * 32.3.3 [(外部) C 类型](#ForeignCTypes)
> * 32.3.4 [关于 C 偏好的选项](#ChoiceOfCFlavor)
> * 32.3.5 [外部变量](#ForeignVar)
> * 32.3.6 [外部地址place的操作](#OperationsOnForeign)
> * 32.3.7 [外部函数](#ForeignFun)
> * 32.3.8 [参数和结果传递中的转换](#ArgAndResConv)
> * 32.3.9 [参数的模式](#ParameterMode)
> * 32.3.10 [示例](#FFCExamples)

### 32.3.1 <span id = "FFCIntroduction">引言</span>

这个机制, 也被认为是 “外部语言接口”, 允许在clisp里调用C实现的函数然后做一些相关的事, 像检查和修改外部内存, 定义一个 “callback” (换句话说, 可以使一个 lisp 函数在 C 环境中可用), 等等. 要使用这种机制, 需要在一个普通的lisp文件中写一个外部函数（ foreign function）描述, 这个文件会像平常一样编译; 或者只是在 read-eval-print loop 里求值适当的结构.

这里有两种方式去定义一个外部函数:

    使用 dlopen 和 dlsym 到达一个动态库里函数的位置.为了访问这个机制, 需要传递 :LIBRARY 选项给 FFI:DEF-CALL-OUT 还有 FFI:DEF-C-VAR.

    不幸的是, 这个功能在一些操作系统不可用, 而且也只提供了一部分外部的功能: cpp 宏以及 inline 函数不能通过这个方式访问. 另一方面, 这个功能在 read-eval-print loop 可用，不需要依赖 C 编译器.
    
    用一种不那么直接的方法: 当你没有使用 :LIBRARY 参数, COMPILE-FILE 提供一个 #P".c" 文件 (除了一个 #P".fas" 和一个 #P".lib"). 当你编译 (通过一个 C 编译器) 链接到 CLISP (静态地,链接到 lisp.a,或者动态地, 运行 CLISP 时用 dlopen 和 dlsym 加载). 这种方法你可以使用外部库导出的各种功能, 不管是普通函数, inline 函数, 或者是cpp的宏 (见 Example 32.6, “Accessing cpp macros”).

所有跟外部函数接口有关的符号都被包 “FFI” 导出. 为了使用它们,请使用 (USE-PACKAGE “FFI”).

特殊的 “FFI” 结构可能会出现在 Lisp 文件的任何地方.

### 32.3.2 <span id = "FFCOverview">概览</span>

这些是特殊的 “FFI” 结构. 我们已经采取务实的态度: 仅支持外部语言 C 和 ANSI C.
注意

除非明确指出, 指定类型的参数不被评估求值, 所以它们可以在宏展开时被FFI:PARSE-C-TYPE 编译到内部的表示形式.

高级别的 “FFI” 结构; name 是任何 Lisp符号; c-name 是一个字符串

``(FFI:DEF-C-TYPE name &OPTIONAL c-type)``

    这个结构为 c-type 提供一个快捷访问的 name . 注意这个 c-type 可能已经关联到 name. 然而，类型的前置申明是不可能的.

    当 c-type 被遗漏了, 这个类型会被假定为 integer整型, 而且它的大小的有无符号在链接的时候确定, 比如, (FFI:DEF-C-TYPE size_t).
    
``(FFI:DEF-C-VAR name {option}*)``

    这个结构定义了一个 FFI:FOREIGN-VARIABLE. name 是一个Lisp名字, 一个规范的Lisp符号.

     FFI:DEF-C-VAR的选项如下：

    (:NAME c-name)
        用字符串指定一个从C中看到的名字. 如果没有指定, 它就从这个Lisp名字打印的结果中提取.
    (:TYPE c-type)
        指定这个变量的外部类型.
    (:READ-ONLY BOOLEAN)
        如果这个选项被指定而且非nil, 在Lisp里就不可能修改这个变量的值 (通过 SETQ 或者类似的都不能改变).
    (:ALLOC ALLOCATION)
        这个选项可以是 :NONE 或者 :MALLOC-FREE ，默认是 :NONE. 如果是 :MALLOC-FREE, 任何 FFI:C-STRING, FFI:C-PTR, FFI:C-PTR-NULL, FFI:C-ARRAY-PTR 类型值被假定为指向 malloc 分配的内存的指针, 而且当 SETQ 将旧值取代为新值的时候, 旧的存储被 free 释放而新的存储通过 malloc 分配. 如果是 :NONE, SETQ 假定指针指向正常可用的存储 (非 NULL) 并用新值重写旧值. 这是危险的 (只要回想下用一个更长的字符串重写了一个字符串或者给一个空指针赋值...) 且不赞成的.
    (:LIBRARY name)
        指定包含了这个变量的 (可选) 动态库, 默认被设置为 FFI:DEFAULT-FOREIGN-LIBRARY.
    (:VERSION version)
        指定在这个库中的符号版本 (可选)  (因此, 如果 :VERSION 被指定, :LIBRARY 也必须提供)
    (:DOCUMENTATION string)
        指定 (可选) VARIABLE 文档说明.

``(FFI:DEF-C-CONST name {option}*)``

    这个结构定义了一个 Lisp 常量的 name ，它的值在构建时候通过内部的 FFI:FOREIGN-FUNCTION 来决定.

     FFI:DEF-C-CONST  的选项如下：

    (:NAME c-name)
        以一个 STRING 的形式指定从 C 中看到名字 . 如果没有指定, 它从这个Lisp名字的打印形式中衍生.
    (:TYPE c-type)
        指定这个常量的外部类型, 以下其中之一：
        FFI:INT
        FFI:C-STRING
        FFI:C-POINTER
    (:GUARD string)
        指定这个 cpp 检测去包装 c-name, 默认是 "defined(c-name)"; 可以是 NIL 来省略这个检测. 如果测试失败, name 就是没有绑定的.
    (:DOCUMENTATION string)
        指定这个 (可选的) VARIABLE 文档.

    见 Example 32.6, “Accessing cpp macros”.﻿
    
``(FFI:DEF-CALL-OUT name {option}*)``

    这个结构定义了一个call-out函数 (一个在Lisp里调用的外部函数: 控制流暂时离开Lisp).

     FFI:DEF-CALL-OUT 的选项:

    (:NAME c-name)
        任何Lisp里的调用 #'name 函数会重定向为调用 C 函数 c-name.
    (:ARGUMENTS {(argument c-type [PARAM-MODE [ALLOCATION]])}*)
    (:RETURN-TYPE c-type [ALLOCATION])
        参数列表和返回值, 见 Section 32.3.8, “Argument and result passing conventions” and Section 32.3.9, “Parameter Mode”.
    (:LANGUAGE language)
        见 Section 32.3.4, “The choice of the C flavor”.
    (:BUILT-IN BOOLEAN)
        当这个函数是 C 内置的, 这个完整的原型会被打印出来 (除非被 FFI:*OUTPUT-C-FUNCTIONS* 抑制).
    (:LIBRARY name)
        指定 (可选的) 包含这个函数的动态库, 默认被 FFI:DEFAULT-FOREIGN-LIBRARY 设置.
    (:VERSION version)
        指定 (可选的) 库中的符号版本 (因此, 如果指定了 :VERSION, :LIBRARY 也必须被指定)
    (:DOCUMENTATION string)
        指定 (可选的) FUNCTION 文档.

    见 Section 32.3.7, “Foreign functions”.
    
``(FFI:DEF-CALL-IN function {option}*)``

    这个结构定义了一个 callback - 从外部调用进来的函数 (i.e., 从外部语言调用一个Lisp函数: 控制流暂时进入到Lisp)

     FFI:DEF-CALL-IN 选项：

    (:NAME c-name)
        任何 C 函数调用 C 函数 c-name 就重定向为调用 Common Lisp 函数 function, 这个function应该是一个函数名.
    (:ARGUMENTS {(argument c-type [PARAM-MODE [ALLOCATION]])}*)
    (:RETURN-TYPE c-type [ALLOCATION])
        参数列表和返回值, 见 Section 32.3.8, “Argument and result passing conventions” and Section 32.3.9, “Parameter Mode”.
    (:LANGUAGE language)
        见 Section 32.3.4, “The choice of the C flavor”.

    也见 Section 32.3.7, “Foreign functions”.

``(FFI:OPEN-FOREIGN-LIBRARY name &KEY :REQUIRE)``

    打开 (加载) 一个外部共享库.一些共享库依赖其他共享库的话，这个依赖可以通过 :REQUIRE 参数指定.除非这个库已经被依赖,这个只会在不创建外部对象仅测试这个库是否存在时被需要. 当你用包含  :LIBRARY 参数的 FFI:DEF-C-VAR 或FFI:DEF-CALL-OUT 创建一个 FFI:FOREIGN-VARIABLE 或者一个 FFI:FOREIGN-FUNCTION , 这个 name 库会被自动打开.比如, libgsl.so 依赖 libgslcblas.so:

    (FFI:OPEN-FOREIGN-LIBRARY "libgsl.so")
    *** - FFI:OPEN-FOREIGN-LIBRARY: Cannot open library "libgsl.so":
      "/usr/lib64/libgsl.so: undefined symbol: cblas_ctrmv"

    所以一种提前打开依赖的常用方式是：

```LISP
(FFI:OPEN-FOREIGN-LIBRARY "libgslcblas.so")
(FFI:DEF-CALL-OUT gsl_cheb_alloc (:LIBRARY "libgsl.so") (:language :stdc)
  (:arguments (n ffi:int)) (:return-type ffi:c-pointer))
⇒ GSL_CHEB_ALLOC
```

这种方式只能用于当前镜像：如果你保存镜像，这个GSL_CHEB_ALLOC 就不会正常工作因为clisp会去重新打开libgsl.so 但是会像上面所说的会失败，使用:REQUIRE参数就是告诉clisp以正确的顺序重新打开那些库：

```SHELL
$ clisp
> (FFI:OPEN-FOREIGN-LIBRARY "libgsl.so" :require '("libgslcblas.so"))
> (FFI:DEF-CALL-OUT gsl_cheb_alloc (:library "libgsl.so") (:language :stdc)
  (:arguments (n ffi:int)) (:return-type ffi:c-pointer))
> (EXT:SAVEINITMEM "foo" :executable t)
> (EXT:EXIT)
$ ./foo
> (gsl_cheb_alloc 10)
#<FFI:FOREIGN-ADDRESS #x0000000017AC38A0>
```

``(FFI:CLOSE-FOREIGN-LIBRARY name)``

    关闭 (卸载) 共享库 (被 FFI:OPEN-FOREIGN-LIBRARY 打开或指定 :LIBRARY 参数给 FFI:DEF-CALL-OUT 或 FFI:DEF-C-VAR).

    如果你修改了共享库, 首先你需要通过 FFI:CLOSE-FOREIGN-LIBRARY 来关闭它. 当你使用 FFI:FOREIGN-VARIABLE 或 FFI:FOREIGN-FUNCTION 属于 name 库里时, 它会自动再次打开.
    
``(FFI:DEFAULT-FOREIGN-LIBRARY library-name)``

    这个宏设置默认的 :LIBRARY 参数给  FFI:DEF-CALL-OUT 和 FFI:DEF-C-VAR. library-name 应该是 NIL (意味着使用 COMPILE-FILE 提供的 C 文件), 一个 STRING, 或者, 取决于底层 dlsym 或 dlvsym 实现, :DEFAULT 或者 :NEXT.

    默认设置为每一个 compilation unit, 所以, 如果你对接单个的库, 你可以在你的lisp文件开头设置这个变量，然后就可以在这个文件中省略 :LIBRARY 参数了.
    
``(FFI:DEF-C-STRUCT name (symbol c-type)*)``

    这个结构定义了 name 的 STRUCTURE-CLASS 和一个带有给定槽的外部 C 类型. 如果这个出现在上面的类没有被需要，就应该考虑用 (FFI:DEF-C-TYPE name (FFI:C-STRUCT {LIST | VECTOR} (symbol c-type)*)) . name 是一个 SYMBOL (结构体名字) 或者一个 LIST ，它的 FIRST 元素时一个结构体名字，剩余的元素 REST 是可选的. 这时支持两个选项:

     FFI:DEF-C-STRUCT 的选项

    :TYPEDEF
        意味着这个 C 类型结构体的名字是用 typedef 在其他地方定义的.
    :EXTERNAL
        意味着这个结构体定义在你包含进来的 C 头文件里, 比如, (FFI:C-LINES "#include <filename.h>~%").

    这些选项决定了如何写到 #P".c".
    
``(FFI:DEF-C-ENUM name {symbol | (symbol [value])}*)``

    这个结构定义了常量 symbol , 类似于 C 声明枚举 { symbol [= value], ... };

    你可以使用 (FFI:ENUM-FROM-VALUE name value) 和 (FFI:ENUM-TO-VALUE name symbol) 在数值和符号表述之间转换 (当然,后面的函数归结为 SYMBOL-VALUE ，外加检查这个 symbol 确实是一个定义在 FFI:DEF-C-ENUM name 的常量).

``(FFI:C-LINES format-string {argument}*)``

    这个结构输出字符串 (FORMAT NIL format-string {argument}*) 到 C 输出文件的顶层. 这个经常用于包含相关的头文件, 见 :EXTERNAL 和 FFI:*OUTPUT-C-FUNCTIONS*.

    当 format-string 不是一个 STRING, 那就应该是一个 SYMBOL, 然后这个 STRING (FORMAT NIL {argument}*) 加到适当的 C 函数:

    :INIT-ALWAYS
    :INIT-ONCE
        initialization function 初始方法
    :FINI
        finalization function 析构方法

``(FFI:ELEMENT c-place index1 ... indexn)``

    数组元素: 如果 c-place 是一个外部类型 (FFI:C-ARRAY c-type (dim1 ... dimn)) 而且 0 ≤ index1 < dim1, ..., 0 ≤ indexn < dimn, 这就相当于 (AREF c-place index1 ... indexn) 或者 c-place[index1]...[indexn]. 它就是 c-type 类型的place.
    如果 c-place 是外部类型 (FFI:C-ARRAY-MAX c-type dim) 而且 0 ≤ index < dim, 这就相当于 (AREF c-place index) 或者 c-place[index]. 它就是 c-type 类型的place.
    
``(FFI:DEREF c-place)``

    间接引用指针: 如果 c-place 是一个外部类型 (FFI:C-PTR c-type), (FFI:C-PTR-NULL c-type) 或者 (FFI:C-POINTER c-type), 这个就是指针指向的地址. 这是一个类型 c-type 的地址. 对于 (FFI:C-PTR-NULL c-type), 这个 c-place 可能是 NULL.
    
``(FFI:SLOT c-place slot-name)``

    结构体或者联合体: 如果 c-place 是一个外部类型 (FFI:C-STRUCT class ... (slot-name c-type) ...) 或者类型 (FFI:C-UNION ... (slot-name c-type) ...), 这个就是类型 c-type.
    
``(FFI:CAST c-place c-type)``

    类型转化: 返回一个地址和最初的 c-place 相同的内存空间，但是是类型 c-type.
    
``(FFI:OFFSET c-place offset c-type)``

    类型改变或替换: 以 c-type 类型返回一个指向 c-place 偏移 offset 个字节的地址, 以类型 c-type 的形式. 这个可以用于重置数组的大小, 比如对于 c-type (FFI:C-ARRAY uint16 n) 可以通过这种方式 (FFI:OFFSET c-place 0 '(FFI:C-ARRAY uint16 k)).
    
``(FFI:C-VAR-ADDRESS c-place)``

    以Lisp类型 FFI:FOREIGN-ADDRESS 对象返回 c-place 的地址. 如果一个外部函数期望一个 C 类型 FFI:C-POINTER 的参数，这个是很有用的.
    
``(FFI:C-VAR-OBJECT c-place)``

    返回 c-place 表示的底层的 FFI:FOREIGN-VARIABLE 对象. 对于一个 FFI:C-POINTER 声明，这个也是一个可接受的参数类型.
    
``(FFI:TYPEOF c-place)``

    返回 c-place 相对应的 c-type .
    
``(FFI:SIZEOF c-type)``
``(FFI:SIZEOF c-place)``

    第一个结构返回那个 C 类型 c-type 的字节计算的大小和对齐方式.

    第二个结构返回那个 C 类型 c-place 的字节计算的大小和对齐方式.
    
``(FFI:BITSIZEOF c-type)``
``(FFI:BITSIZEOF c-place)``

    第一个结构返回那个 C 类型 c-type 的位计算的大小和对齐方式.

    第二个结构返回那个 C 类型 c-place 的位计算的大小和对齐方式.
    
``(FFI:FOREIGN-ADDRESS-UNSIGNED foreign-entity)``
``(FFI:UNSIGNED-FOREIGN-ADDRESS number)``

    FFI:FOREIGN-ADDRESS-UNSIGNED 返回 INTEGER 地址，包含在 FFI:FOREIGN-ADDRESS, FFI:FOREIGN-POINTER, FFI:FOREIGN-VARIABLE 或者 FFI:FOREIGN-FUNCTION 这些Lisp对象类型里的地址。

    FFI:UNSIGNED-FOREIGN-ADDRESS 返回一个 FFI:FOREIGN-ADDRESS 指向给定的 INTEGER 地址.
    
``(FFI:FOREIGN-ADDRESS foreign-entity)``

    FFI:FOREIGN-ADDRESS 同时时类型名字和一个选择器/构造器函数. 它是对应 FFI:C-POINTER 额外类型声明的Lisp对象类型, 比如一个使用 (:RETURN-TYPE FFI:C-POINTER) 的外部调用函数产生一个 FFI:FOREIGN-ADDRESS 类型的Lisp对象.

    这个函数提取任何 FFI:FOREIGN-VARIABLE 或 FFI:FOREIGN-FUNCTION 对象中的 FFI:FOREIGN-ADDRESS 类型的对象. 如果 foreign-entity 已经是一个 FFI:FOREIGN-ADDRESS, 它就返回foreign-entity. 如果是一个 FFI:FOREIGN-POINTER (比如一个基本外部库的地址), 它将被封装到一个 FFI:FOREIGN-ADDRESS 对象里, 更合适地和 FFI:C-POINTER 额外类型声明一起使用. 它不会用 NUMBER 来构成地址, FFI:UNSIGNED-FOREIGN-ADDRESS 必须被用于那个目的. ;;;???
    
``(FFI:FOREIGN-VARIABLE foreign-entity c-type-internal &KEY name)``

    这个构造器从给定的 FFI:FOREIGN-ADDRESS 或 FFI:FOREIGN-VARIABLE 以及内置的C类型 (就像从 FFI:PARSE-C-TYPE 获取到的) 描述来创建一个新的 FFI:FOREIGN-VARIABLE . name, 是一个 STRING, 因为它出现在 FFI:FOREIGN-VARIABLE 对象打印的地方，所以在记录文档和交互调试的时候是很有用的, 就像 #<FFI:FOREIGN-VARIABLE "foo" #x0ADD4E55>. 事实上, 这个类似于 FFI:CAST (或者对于place相当于 (FFI:OFFSET ... 0 ...)), 除非它和 FFI:FOREIGN-ADDRESS 对象一起使用并且允许缓存内置的 C 类型.;;;???
    
``(FFI:FOREIGN-FUNCTION foreign-entity c-type-internal &KEY name)``

    这个构造器从给定的 FFI:FOREIGN-ADDRESS 或 FFI:FOREIGN-FUNCTION 以及内置的C类型描述来创建一个新的 FFI:FOREIGN-FUNCTION 对象 (就像从 (FFI:PARSE-C-TYPE '(FFI:C-FUNCTION ...)) 获得, 在这种情况下指定 :LANGUAGE 是很重要的因为这个表达式很可能在运行时被求值, 在编译单元之外). 这个 name, 是一个 STRING, 因为它出现在 FFI:FOREIGN-FUNCTION 对象的打印中，所以在记录文档和交互调试的时候是很有用的, 比如, #<FFI:FOREIGN-FUNCTION "foo" #x0052B060>. 当可用时，这个在给定的 FFI:FOREIGN-FUNCTION 对象中继承下来.

    见 Section 32.3.7, “Foreign functions”. 
    
``(FFI:VALIDP foreign-entity)``
``(SETF (FFI:VALIDP foreign-entity) value)``

    如果 foreign-entity (比如一个 FFI:C-POINTER Lisp等价物)引用一个非法指针时，这个断言返回 NIL (例如, 因为它来自于先前的Lisp会话). 如果 foreign-entity 在当前的Lisp进程可用，它返回 T  (所以对所有非外部的参数它返回 T ).

    你可以通过 (SETF FFI:VALIDP) 来使一个外部对象作废. 你不能复活一个已死对象, 你也不能去杀掉一个非外部对象.
    
``(FFI:FOREIGN-POINTER foreign-entity)``

    FFI:FOREIGN-POINTER 返回和Lisp类型 FFI:FOREIGN-ADDRESS, FFI:FOREIGN-POINTER, FFI:FOREIGN-VARIABLE or FFI:FOREIGN-FUNCTION 关联的 FFI:FOREIGN-POINTER 对象.
    
``(FFI:SET-FOREIGN-POINTER foreign-entity {foreign-entity | :COPY})``

    FFI:SET-FOREIGN-POINTER 把 FFI:FOREIGN-ADDRESS, FFI:FOREIGN-VARIABLE 或者 FFI:FOREIGN-FUNCTION 关联的 FFI:FOREIGN-POINTER 对象改为另一个元素. 当使用了 :COPY, 一个新的 FFI:FOREIGN-POINTER 被分配. 原来的 foreign-entity 一直指向同一个对象并且被返回. 和 (SETF FFI:VALIDP) 一起是非常有用的, 见 Example 32.11, “Controlling validity of resources”.
    
``(FFI:WITH-FOREIGN-OBJECT (variable c-type [initarg]) body)``
``(FFI:WITH-C-VAR (variable c-type [initarg]) body)``

    这些结构会在 C 执行栈上分配空间, 分别绑定一个 FFI:FOREIGN-VARIABLE 对象或者一个本地的 SYMBOL-MACRO 给 variable 并执行 body.

    当没有提供 initarg时, 它们就只分配 (FFI:SIZEOF c-type) 个字节. 这个空间用零填充. 比如, 用一个 FFI:C-STRING 的  c-type 或者甚至是 (FFI:C-PTR (FFI:C-ARRAY uint8 32)) (!) 都会分配一个空间给一个指针, 初始化为 NULL.

    当提供了 initarg 时, 它们会为 c-type 的任意复杂的集合根结构分配空间. 因此, FFI:C-ARRAY-MAX, #() 和 "" 在创建一个指向空数组的指针时很有用:

    (with-c-var (v '(c-ptr (c-array-max uint8 32)) #())
      (setf (element (deref v) 0) 127) v)

    c-type 被求值, 使得由变量决定长度的缓冲区创建变得简单:
    (with-c-var (fv `(c-array uint8 ,(length my-vector)) my-vector)
      (print fv)) 

``(FFI:FOREIGN-VALUE FFI:FOREIGN-VARIABLE)``
``SETF (FFI:FOREIGN-VALUE FFI:FOREIGN-VARIABLE) ...)``

    这个函数用于将 FFI:FOREIGN-VARIABLE 描述的 C 数据结构引用转换为Lisp。 这样的引用通常从 FFI:ALLOCATE-SHALLOW, FFI:ALLOCATE-DEEP, FFI:FOREIGN-ALLOCATE 获取或者通过 (FFI:C-POINTER c-type) C 类型描述. 或者, 类似 FFI:WITH-C-PLACE 或 FFI:WITH-C-VAR 的宏以及外部地址的概念隐藏了很多这个方法的使用.

    这个 SETF 结构演示了从 Lisp 转到 C, 根据 FFI:FOREIGN-VARIABLE's 类型描述.
    
``(FFI:WITH-FOREIGN-STRING (foreign-address char-count byte-count string &KEY encoding null-terminated-p start end) &BODY body)``

    这个结构根据 encoding 转化 string , 在 C 执行栈上分配空间. encoding 可以是任何 EXT:ENCODING, 比如 CHARSET:UTF-16 或 CHARSET:UTF-8, 然而 CUSTOM:*FOREIGN-ENCODING* 必须是一个 ASCII-兼容的编码.

    body 会在后面被执行，三个变量 foreign-address, char-count 和 byte-count 分别绑定给一个弱类型的 FFI:FOREIGN-ADDRESS (从 FFI:C-POINTER 外部类型说明中得知) 指向栈空间, 这个 Lisp string 的 CHARACTERs 数量已经被考虑进去，并且(UNSIGNED-BYTE 8) 字节的数量也在 C 栈上被分配.

    当 null-terminated-p 是默认值true时, 一个可变数量的零字节会被追加到后面, 取决于它的编码, 比如 对于 CHARSET:UTF-16 是2, 并且 byte-count, 还有 char-count 的值递增1.

    当离开这个结构时，这个绑定给 foreign-address 的 FFI:FOREIGN-ADDRESS 是无效的.

    一个愚蠢的例子 (一个 mblen 的很昂贵的接口):

    (with-foreign-string (fv elems bytes string
                          :encoding charset:jis... :null-terminated-p nil
                          :end 5)
     (declare (ignore fv elems))
     (format t "This string would take ~D bytes." bytes))

     
``(FFI:PARSE-C-TYPE c-type)``
``(FFI:DEPARSE-C-TYPE c-type-internal)``

    在扩展的(LIST)和内置的(VECTOR)  C 类型表示之间转换 (被 DESCRIBE 使用).
    注意

    虽然你可以记住一个 c-type-internal (见 Section 31.11.3, “Macro EXT:MEMOIZED” - 但是不要期望类型重新定义会在多个记忆之间起作用!), 但是你不能去序列化它 (写到硬盘里) 因为反序列化会丢失对象标识.
    
``(FFI:ALLOCATE-SHALLOW c-type &KEY :COUNT :READ-ONLY)``
``(FFI:ALLOCATE-DEEP c-type contents &KEY :COUNT :READ-ONLY)``
``(FFI:FOREIGN-FREE foreign-entity &KEY :FULL)``
``(FFI:FOREIGN-ALLOCATE c-type-internal &KEY :INITIAL-CONTENTS :COUNT :READ-ONLY)``

    宏 FFI:ALLOCATE-SHALLOW 分配 (FFI:SIZEOF c-type) 字节在 C 堆上并把它们置为零 (类似 calloc). 当 :COUNT 被提供时, c-type 会被  (FFI:C-ARRAY c-type count) 替代, 除非当 c-type 是 CHARACTER, 在这种情况下应该使用 (FFI:C-ARRAY-MAX CHARACTER count) . 当 :READ-ONLY 被提供时, Lisp这边会被阻止去修改这部分内容. 这个可以被用作指示一些外部调用将会填充这块内存 (比如 通过 read).

    返回一个实际的 c-type 类型的 FFI:FOREIGN-VARIABLE 对象, 它的地址部分指向新分配的空间.

    FFI:ALLOCATE-DEEP 有必要的话会调用多次 C malloc 去构建一个给定 c-type 类型的结构体到 C 堆中, 从给定的 contents 来初始化.

    比如, (FFI:ALLOCATE-DEEP 'FFI:C-STRING "ABCDE") 演示了2个分配: 一个指向字符串的 C 指针, 另一个是这个字符串的内容. 这个在 char** C 类型声明时会很有用. (FFI:ALLOCATE-SHALLOW 'FFI:C-STRING) 为单个指针分配空间 (很可能为4字节).

    (FFI:ALLOCATE-DEEP 'CHARACTER "ABCDEF" :count 10) 为类型 (FFI:C-ARRAY-MAX CHARACTER 10) 分配并初始化空间， 对应 char* , 或者更特殊的, 在 C 中的 char[10].

    函数 FFI:FOREIGN-FREE 释放给定的 foreign-entity 所表示的内存. 如果 :FULL 被提供并且参数类型是 FFI:FOREIGN-VARIABLE, 递归地释放这个变量所表示的整个结构体.

    如果给定的 FFI:FOREIGN-FUNCTION 对象对应于一个 CLISP 回调, 就会把它释放. 每次你通过 “FFI” 传递Lisp函数，回调就会被自动创建.

    从Lisp中使用 (SETF FFI:VALIDP) 去使指向这个地址的引用失效. 这个目前不会自动完成. 如果给定的指针已经非法了, FFI:FOREIGN-FREE (当前的) 意味着一个 ERROR. 这个可能使它更容易与 EXT:FINALIZE 整合.

    函数 FFI:FOREIGN-ALLOCATE 是一个低层次的结构，它需要一个 FFI:PARSE-C-TYPE 返回的内置的C类型描述.
    
``(FFI:WITH-C-PLACE (variable foreign-entity) body)``

    在给定的 FFI:FOREIGN-VARIABLE 对象外创建一个 place 所以在place上的操作 (比如 FFI:CAST, FFI:DEREF, FFI:SLOT etc.) 可以在 body 里被使用. FFI:WITH-C-VAR 的出现就好像是 FFI:WITH-FOREIGN-OBJECT 和 FFI:WITH-C-PLACE 的组合.

    这样的一个place可以被用于访问一个 foreign-entity 对象引用的内存:

    (setq foo (allocate-deep '(c-array uint8 3) rgb))
    (with-c-place (place foo) (element place 0))

     
``FFI:*OUTPUT-C-FUNCTIONS*``
``FFI:*OUTPUT-C-VARIABLES*``

    CLISP 会为外部函数 (使用 FFI:DEF-CALL-OUT 定义) 和外部变量 (使用 FFI:DEF-C-VAR 定义) 编写额外的声明到这个输出 #P".c" (当这个Lisp文件被 COMPILE-FILE 编译时) ﻿除非这些变量是 NIL. 它们默认是 NIL , 所以这个额外的声明没有写; 建议你使用 FFI:C-LINES 去包含适当的 C 头文件. 如果这个头文件不存在或不可用，设置这些变量为非 NIL .
    
``FFI:*FOREIGN-GUARD*``

    当这个变量在编译时为非 NIL , CLISP 会在输出文件里利用 GNU autoconf 特点声明并用cpp条件式检测 C 声明. 比如,

    (EVAL-WHEN (compile) (setq *foreign-guard* t))
    (FFI:DEF-CALL-OUT some-function (:name "function_name") ...)

    会产生

    # if defined(HAVE_FUNCTION_NAME)
      register_foreign_function((void*)&function_name,"function_name",1024);
    # endif 

    并且会在任何系统上编译链接.

    当你想要你的模块可以在任何系统上构建甚至当中的一些特性是不可用时，这在产品交付时是很有用的.

    FFI:*FOREIGN-GUARD* 默认是 NIL ，为了向后兼容.
    
``FFI:FOREIGN-POINTER-INFO``

    这是一个 dladdr 结构并且它 multiple values 以多值的形式返回Dl_info的 4 个域.

低层次的 “FFI” 结构

``(FFI:MEMORY-AS foreign-address c-type-internal &OPTIONAL offset)``
``(SETF (FFI:MEMORY-AS foreign-address c-type-internal &OPTIONAL offset) value)``

    当用弱类型的外部指针 (FFI:FOREIGN-ADDRESS) 与一个有类型的相对 (用 FFI:FOREIGN-VARIABLE 表示), 这个访问方式去操作是很有用的. 它允许在不需要创建 FFI:FOREIGN-VARIABLE 类型的对象下，去归类并间接引用给定的指针.

    要不然, 可以使用 (FFI:FOREIGN-VALUE (FFI:FOREIGN-VARIABLE foreign-entity c-type-internal)) (这个也是支持 SETF 的).

    注意那个 c-type-internal 是一个外部类型的内置表示, 所以 FFI:PARSE-C-TYPE 需要一个字面的名字或类型, 比如 (FFI:MEMORY-AS foreign-address (FFI:PARSE-C-TYPE '(FFI:C-ARRAY uint8 3))) 或者 (SETF (FFI:MEMORY-AS foreign-address (FFI:PARSE-C-TYPE 'uint32)) 0).

### 32.3.3 <span id = "ForeignCTypes">(外部) C 类型</span>

在 “FFI” 使用外部 C 类型. 它们不是正常的 Common Lisp 类型或者 CLOS 类.

一个 c-type 可以是一个预定义的 C 类型或者一个 FFI:DEF-C-TYPE 定义的类型的名字.

这个预定义的 C 类型如下 (c-type)

simple-c-type

**简单 C 类型**
|Lisp 名字	|Lisp 等价物	|C 等价物	|ILU 等价物	|备注|
|----|----|----|----|----|
|NIL	|NIL	|void	| 	|as a result type only|
|BOOLEAN	|BOOLEAN	|int	|BOOLEAN	 ||
|CHARACTER	|CHARACTER	|char	|SHORT CHARACTER	 ||
|char	|INTEGER	|signed char|||	 	 
|uchar	|INTEGER	|unsigned char	||| 	 
|short	|INTEGER	|short	 	 |||
|ushort	|INTEGER	|unsigned short	 	 |||
|int	|INTEGER	|int	 	 |||
|uint	|INTEGER	|unsigned int	||| 	 
|long	|INTEGER	|long	 	 |||
|ulong	|INTEGER	|unsigned long	||| 	 
|uint8	|(UNSIGNED-BYTE 8)	|uint8	| BYTE||
|sint8	|(SIGNED-BYTE 8)	|sint8	 	 |||
|uint16	|(UNSIGNED-BYTE 16)	|uint16	|SHORT CARDINAL	 ||
|sint16	|(SIGNED-BYTE 16)	|sint16	|SHORT INTEGER	 ||
|uint32	|(UNSIGNED-BYTE 32)	|uint32	|CARDINAL	 ||
|sint32	|(SIGNED-BYTE 32)	|sint32	|INTEGER	|| 
|uint64	|(UNSIGNED-BYTE 64)	|uint64	|LONG CARDINAL|	does not work on all platforms|
|sint64	|(SIGNED-BYTE 64)	|sint64	|LONG INTEGER	|does not work on all platforms|
|SINGLE-FLOAT	|SINGLE-FLOAT	|float	 |||	 
|DOUBLE-FLOAT	|DOUBLE-FLOAT	|double	 	 |||

``FFI:C-POINTER``

    这个类型对应 C 里的 void*, 一个模糊指针. 当被用作参数时, NIL 被作为 FFI:C-POINTER 参数接收时被当作是 NULL; 当一个函数想要返回一个 NULL FFI:C-POINTER, 它实际返回了 NIL.
    
``(FFI:C-POINTER c-type)``

    这个类型等价于 C 里的 c-type *: 一个指向给定的 c-type 单个实例. 它区别于 (FFI:C-PTR-NULL c-type) (见下方) 不会发生和Lisp之间的变换 ( C NULL 指针与 Lisp NIL 之间的转换). 然而, 一个 FFI:FOREIGN-VARIABLE 类型的对象被用于表示这个外部 place. 它可以同化成一个有类型的指针. 
    
``FFI:C-STRING``

    这个类型对应 C 中的 char*, 一个0终止的字符串. 它的Lisp等价是一个, 不包括结束的0字符.
    
``(FFI:C-STRUCT class (ident1 c-type1) ... (identn c-typen))``

    这个类型等价于 C 中的 struct { c-type1 ident1; ...; c-typen identn; }. 它的Lisp等价是: 如果 class 是 VECTOR, 那就是一个 SIMPLE-VECTOR; 如果 class 是 LIST, 就是一个 proper list; 如果 class 是一个结构或者 CLOS 类名, 那就是这个类的一个实例, 包括 ident1, ..., identn 这些槽.

    class 可能是 SYMBOL 的  CONS (如上) 以及一个 FFI:DEF-C-STRUCT 的选项列表.
    
``(FFI:C-UNION (ident1 c-type1) ... (identn c-typen))``

    这个类型等价于 C 中的 union { c-type1 ident1; ...; c-typen identn; }. 从Lisp或者到Lisp的转化Conversion to and from Lisp 都假定这个值可以看作是 c-type1.
    
``(FFI:C-ARRAY c-type dim1)``
``(FFI:C-ARRAY c-type (dim1 ... dimn))``

    这个类型等价于 C 中的 c-type [dim1] ... [dimn]. 注意当一个数组作为参数传递给 C 中的函数时, 通常传递的是一个指针; 所以对于这样的参数类型，你需要写成 (FFI:C-PTR (FFI:C-ARRAY ...)) .
    
``(FFI:C-ARRAY-MAX c-type maxdimension)``

    这个类型等价于 C 中的 c-type [maxdimension], 一个最大包含 maxdimension 元素的数组. 这个数组如果包含的元素少于 maxdimension ，那就是以0终止的. 当转换一个大于 maxdimension 的Lisp数组时会忽略掉多余的部分.
    
``(FFI:C-FUNCTION (:ARGUMENTS {(argument a-c-type [PARAM-MODE [ALLOCATION]])}*) (:RETURN-TYPE r-c-type [ALLOCATION]) (:LANGUAGE language))``

    这个类型定义了一个根据给定的原型 (r-c-type (*) (a-c-type1, ...)) 可以被调用的 C 函数. 在 C 函数和 Lisp 函数之间的转换是透明的, 而且 NULL/NIL 是可以被识别和接受的.
    
``(FFI:C-PTR c-type)``

    这个类型等价于 C 中的 c-type *: 一个指向单个给定的 c-type 的指针.
    
``(FFI:C-PTR-NULL c-type)``

    这个类型等价于 C 中的 c-type *: 一个指向单个给定的 c-type 的指针, 包含了 C NULL 对应 Lisp NIL 的异常.
    
``(FFI:C-ARRAY-PTR c-type)``

    这个类型等价于 C 中的 c-type (*)[]: 一个指向给定的 c-type 的零终结的数组的指针.

这里 FFI:C-STRING, (FFI:C-ARRAY CHARACTER dim1), (FFI:C-ARRAY-MAX CHARACTER maxdimension), (FFI:C-ARRAY-PTR CHARACTER) 的转换被 CUSTOM:\*FOREIGN-ENCODING* 所控制并且给定的规格是字节. 这里 CHARACTER 的转换, 还有 (FFI:C-PTR CHARACTER), 或者 (FFI:C-PTR-NULL CHARACTER), 还有多种规格的数组 (FFI:C-ARRAY CHARACTER (dim1 ... dimn)), 如果后者是 “1:1” encoding,都是 CUSTOM:\*FOREIGN-ENCODING* 所管理控制, 否则就是 ASCII 编码.

**注意**

    记住 C 类型 char 是一个数字类型所以不要到对其使用 CHARACTER EXT:ENCODING.

### 32.3.4 <span id = "ChoiceOfCFlavor">关于 C 偏好的选项</span>

FFI:C-FUNCTION, FFI:DEF-CALL-IN, FFI:DEF-CALL-OUT 带有一个 :LANGUAGE 参数. 这个 language 是 :C (表示 K&R C) 或者 :STDC (表示 ANSI C) 或者 :STDC-STDCALL (表示带有 stdcall 调用转化的  ANSI C ). 它指明了这个 C 函数 (调用或者被调用) 被 K&R C 编译器还是被 ANSI C 编译器编译, 并可能还有调用惯例.

默认的语言通过 FFI:DEFAULT-FOREIGN-LANGUAGE 来设置. 如果这个宏在当前的 compilation unit (通常是一个文件)没有被调用, 会发出一个警告并且这个单元的剩余部分会被使用 :STDC .

### 32.3.5 <span id = "ForeignVar">外部变量</span>

外部变量（Foreign variables） 是存储分配在外部语言模块中的变量. 它们仍然可以通过 SETQ 来求值或修改, 就像正常的变量一样, 除非这个值允许的范围根据外部变量的类型被限制.
外部变量的相等性

对于一个外部变量 x ， (EQL x x) 这个结构不一直是true, 因为从外部值转为Lisp值的时候 x 每次都被求值. 因此, (SETF (AREF x n) y) 修改这个Lisp的值 (立刻丢弃), 不是那个外部的数据. 应该使用 FFI:ELEMENT 等等, 见 Section 32.3.6, “Operations on foreign places”.

外部变量通过 FFI:DEF-C-VAR 和 FFI:WITH-C-VAR 来定义.

### 32.3.6 <span id = "OperationsOnForeign">外部地址place的操作</span>

FFI:DEF-C-VAR, FFI:WITH-C-VAR 或 FFI:WITH-C-PLACE 定义的 FFI:FOREIGN-VARIABLE name 是一个 place, 换句话说, 一个可以被用作 SETF 参数的结构 (在 C 术语中是 “lvalue” .) ,在外部的 places 以下操作是可用的:

    FFI:ELEMENT	FFI:C-VAR-ADDRESS
    FFI:DEREF	FFI:C-VAR-OBJECT
    FFI:SLOT	FFI:TYPEOF
    FFI:CAST	FFI:SIZEOF
    FFI:OFFSET	FFI:BITSIZEOF

### 32.3.7 <span id = "ForeignFun">外部函数</span>

外部函数（Foreign functions）是外部语言里定义的函数. 这里包括 已命名函数 ( named foreign functions ) (通过 FFI:DEF-CALL-OUT 引入或者通过 FFI:DEF-CALL-IN 创建)和匿名外部函数( nonymous foreign functions ); 它们通过使用 FFI:FOREIGN-FUNCTION 来转化函数指针而出现.

一个 call-out function 从Lisp里调用的外部函数: 控制流暂时离开Lisp. 一个 call-in function (也认为是 callback) 是一个从外部语言里调用的Lisp函数: 控制流暂时进入 Lisp.

下面的这些操作会定义外部函数:

    FFI:DEF-CALL-IN
    FFI:DEF-CALL-OUT
    FFI:FOREIGN-FUNCTION

#### 32.3.7.1 回调和内存管理

回调Callbacks (C 函数调用Lisp函数) 创建一个称为 trampolines 的东西. 一个 trampoline 是一段 C 代码，这段代码知道如何调用一个特定的Lisp函数. (这就是外部语言接口如何工作, 不只是我们的). 这个外部库接收到的 C 指针是一个指向这段代码的. 这些都不归垃圾回收管理, 因为这里没有方案告诉垃圾回收器一个给定的回调何时不再需要了 (不像 Lisp objects).

把一个已命名的函数(i.e., 通过 FFI:DEF-CALL-IN 创建的结构，其中 function 参数是一个函数名) 作为回调几乎是无害的, 因为 function 不可能被重定义.

把一个匿名函数 (换句话说, 通过 FFI:FOREIGN-FUNCTION 创建或者一个 FFI:DEF-CALL-IN 结构其中的 function 参数是一个 lambda expression)作为回调, 它们被一起动态创建出来可能会成为一个问题, 比如在一个循环里, 导致很多 trampolines 被生成出来.

你可以使用 FFI:FOREIGN-FREE 去释放一个用  FFI:FOREIGN-FUNCTION 分配的 trampoline , 但是当你传递一个 lambda 表达式 作为 FFI:C-FUNCTION 类型的参数给一个 FFI:DEF-CALL-OUT  , 这样一个 trampoline 就被分配了, 但是你却不能持有分配的 trampoline 对象, 所有你不能 (通常) 释放它. 因此你可以发现创建 FFI:FOREIGN-FUNCTION 对象, 传递给 FFI:DEF-CALL-OUT, 然后手动 FFI:FOREIGN-FREE 是比较简单的.

### 32.3.8 <span id = "ArgAndResConv">参数和结果传递中的转换</span>

当传递参数给函数或者从函数获取返回值时, 结果和参数的分配通过以下方式持有:

SIMPLE-C-TYPE, FFI:C-POINTER 类型放栈上, 是动态范围的. 这个 ALLOCATION 事实上被忽略.

FFI:C-STRING, FFI:C-PTR, FFI:C-PTR-NULL, FFI:C-ARRAY-PTR 类型需要存储. 这个 ALLOCATION 指定分配策略:

:NONE

    没有存储被分配.
    
:ALLOCA

    分配在栈上, 带有动态范围的.
    
:MALLOC-FREE

    通过 malloc 分配，通过 free 释放.

如果没有指定 ALLOCATION , 对于大部分类型默认 ALLOCATION 是 :NONE , 但是对于 FFI:C-STRING 和 FFI:C-PTR 以及 FFI:C-PTR-NULL 还有 FFI:C-ARRAY-PTR 还有对于 :OUT 参数是 :ALLOCA . 这个 :MALLOC-FREE 机制提供了在单次变化中传递嵌套结构的能力.

Call-out 函数的参数: 

从Lisp到 C 的参数:

    :MALLOC-FREE
        Lisp 用 malloc 分配存储并且从来不解除分配. 这个 C 函数在处理完后建议调用 free .
    :ALLOCA
        Lisp 在栈上分配存储, 带有动态范围的. 当 C 函数返回时被释放.
    :NONE
        Lisp 认为这个指针已经指向大小合适的合法的区域然后把结果值放在这里.

        这是危险不赞成的.

对于从 C 到 Lisp的结果:

    :MALLOC-FREE
        Lisp 在结束时调用 free.
    :NONE
        Lisp什么也不做.

Call-in 函数的参数: 

对于从 C 到 Lisp 的参数:

    :MALLOC-FREE
        Lisp 结束时调用 free.
    :ALLOCA
    :NONE
        Lisp 什么也不做.

对于从 Lisp to C 的结果:

    :MALLOC-FREE
        Lisp 通过 malloc 分配存储从不释放. 这个 C 建议在处理完后调用 free.
    :NONE
        Lisp 认为这个指针已经指向大小合适的合法的区域然后把结果值放在这里并且把结果放在这个空间.这是危险不赞成的.

**警告**

传递 FFI:C-STRUCT, FFI:C-UNION, FFI:C-ARRAY, FFI:C-ARRAY-MAX 值作为参数 (不通过指针) 对于 C 编译器只是可能支持. 大部分 C 编译器正确地完成了这件事, 但是有一些 C 编译器 (比如hppa, x86_64 和 Win32 上的 gcc ) 就存在问题. 建议的方式是通过指针; 这是完全支持的. 也见 clisp-devel (SFmail/200307141526.26925.bruno%40clisp.org/Gmane/devel/10089).

### 32.3.9 <span id = "ParameterMode">参数的模式</span>

一个函数参数的 PARAM-MODE 可能是

:IN (表示: read-only):

    调用者传递信息给被调用者.
    
:OUT (表示: write-only):

    被调用者在返回时传递信息给调用者. 当看作Lisp函数时, 这里没有对应的Lisp参数, 同时意味着一个额外的返回值. 需要 ALLOCATION = :ALLOCA.
    
:IN-OUT (表示: read-write):

    信息从调用者传递给被调用者，然后再返回信息给调用者. 当看作Lisp函数时, 这个 :OUT 值作为额外的返回值返回.

默认值是 :IN.

### 32.3.10 <span id = "FFCExamples">示例</span>

示例 32.2. 简单的声明和访问

这是 C 声明

```C
struct foo {
  int a;
  struct foo * b[100];
};
```

对应

```LISP
(FFI:DEF-C-STRUCT foo
  (a int)
  (b (c-array (c-ptr foo) 100)))
```

内部元素的访问

```C
struct foo f;
f.b[7].a
```

对应

```LISP
(declare (type foo f))
(foo-a (aref (foo-b f) 7))     ; or
(slot-value (aref (slot-value f 'b) 7) 'a)
```

示例 32.3. 外部的 C 变量和访问

```C
struct bar {
  short x, y;
  char a, b;
  int z;
  struct bar * n;
};

extern struct bar * my_struct;

my_struct->x++;
my_struct->a = 5;
my_struct = my_struct->n;
```

对应

```LISP
(FFI:DEF-C-STRUCT bar
  (x short)
  (y short)
  (a char)
  (b char)     ; or (b character) if it represents a character, not a number
  (z int)
  (n (c-ptr bar)))

(FFI:DEF-C-VAR my_struct (:type (c-ptr bar)))

(setq my_struct (let ((s my_struct)) (incf (slot-value s 'x)) s))     ; or
(incf (slot my_struct 'x))
(setq my_struct (let ((s my_struct)) (setf (slot-value s 'a) 5) s))     ; or
(setf (slot my_struct 'a) 5)
(setq my_struct (slot-value my_struct 'n))     ; or
(setq my_struct (deref (slot my_struct 'n)))
```

示例 32.4. 调用一个外部函数

在 ANSI C 系统上, <stdlib.h> 包含了这个声明:

```C
typedef struct {
  int quot;   /* Quotient */
  int rem;    /* Remainder */
} div_t;
extern div_t div (int numer, int denom);
```

这个转换为

```LISP
(FFI:DEF-C-STRUCT (div_t :typedef)
  (quot int)
  (rem int))
(FFI:DEFAULT-FOREIGN-LANGUAGE :stdc)
(FFI:DEF-CALL-OUT div (:ARGUMENTS (numer int) (denom int))
  (:RETURN-TYPE div_t))
```

Lisp 里调用(在运行 clisp-link 后):

```LISP
(div 20 3)
⇒ #S(DIV_T :QUOT 6 :REM 2)
```

示例 32.5. 另一个调用一个外部函数的示例

假定以下定义在一个 cfun.c:

```C
struct cfunr { int x; char *s; };
struct cfunr * cfun (int i,char *s,struct cfunr * r,int a[10]) {
  int j;
  struct cfunr * r2;
  printf("i = %d\n", i);
  printf("s = %s\n", s);
  printf("r->x = %d\n", r->x);
  printf("r->s = %s\n", r->s);
  for (j = 0; j < 10; j++) printf("a[%d] = %d.\n", j, a[j]);
  r2 = (struct cfunr *) malloc (sizeof (struct cfunr));
  r2->x = i+5;
  r2->s = "A C string";
  return r2;
}
```

从Lisp里通过 callcfun.lisp 文件调用这个函数是可能的(不要去调用 cfun.lisp - COMPILE-FILE 会 overwrite cfun.c) ,文件的内容是:

```LISP
(DEFPACKAGE "TEST-C-CALL" (:use “COMMON-LISP” “FFI”))
(IN-PACKAGE "TEST-C-CALL")
(EVAL-WHEN (compile) (setq FFI:*OUTPUT-C-FUNCTIONS* t))
(FFI:DEF-C-STRUCT cfunr (x int) (s c-string))
(FFI:DEFAULT-FOREIGN-LANGUAGE :stdc)
(FFI:DEF-CALL-OUT cfun (:RETURN-TYPE (c-ptr cfunr))
  (:ARGUMENTS (i int)
              (s c-string)
              (r (c-ptr cfunr) :in :alloca)
              (a (c-ptr (c-array int 10)) :in :alloca)))
(defun call-cfun ()
  (cfun 5 "A Lisp string" (make-cfunr :x 10 :s "Another Lisp string")
        '#(0 1 2 3 4 5 6 7 8 9)))
```

使用 module 机制:

```SHELL
$ clisp-link create cfun callcfun.c
$ cc -O -c cfun.c
$ cd cfun
$ ln -s ../cfun.o cfun.o
Add cfun.o to NEW_LIBS and NEW_FILES in link.sh.
$ cd ..
$ base/lisp.run -M base/lispinit.mem -c callcfun.lisp
$ clisp-link add base base+cfun cfun
$ base+cfun/lisp.run -M base+cfun/lispinit.mem -i callcfun
> (test-c-call::call-cfun)
i = 5
s = A Lisp string
r->x = 10
r->s = Another Lisp string
a[0] = 0.
a[1] = 1.
a[2] = 2.
a[3] = 3.
a[4] = 4.
a[5] = 5.
a[6] = 6.
a[7] = 7.
a[8] = 8.
a[9] = 9.
#S(TEST-C-CALL::CFUNR :X 10 :S "A C string")
>
$ rm -r base+cfun
```

注意这里有个内存泄漏: cfun() 的返回值 r2 被 malloc 但是从未 free. 指明
``(:RETURN-TYPE (c-ptr cfunr) :malloc-free)``

不是一个替代方案，因为这个也会 free(r2->x) 但是 r2->x 是一个指向静态数据的指针.

这个内存泄漏可以通过以下方式来避免
``(:RETURN-TYPE (c-pointer cfunr))``

与下面这个方式一起用

```LISP
(defun call-cfun ()
  (let ((data (cfun ...)))
    (UNWIND-PROTECT (FFI:FOREIGN-VALUE data)
      (FFI:FOREIGN-FREE data :FULL nil))))
```

示例 32.6. 访问 cpp 宏

假设你在接合一个库 mylib.so ，它的类型、宏还有 inline 函数定义在 mylib.h:

```C
#define FOO(x)  .....
#define BAR ...
struct zot { ... }
inline int bar (int x) { ... }
```

为了使它们在 CLISP 可用，在lisp文件 my.lisp 里写:

```LISP
(FFI:C-LINES "#include <mylib.h>
int my_foo (int x) { return FOO(x); }
int my_bar (int x) { return bar(x); }~%")
(FFI:DEF-C-CONST bar)
(FFI:DEF-C-CONST zot-size (:name "sizeof(struct zot)") (:guard nil))
(FFI:DEF-CALL-OUT my-foo (:name "my_foo") (:ARGUMENTS (x ffi:int)) (:RETURN-TYPE ffi:int))
(FFI:DEF-CALL-OUT my-bar (:name "my_bar") (:ARGUMENTS (x ffi:int)) (:RETURN-TYPE ffi:int))
```

编译这个文件会提供 my.c 和 my.fas 然后你有两个选项:

    使用以下命令编译 my.c 为 my.o
    $ gcc -c my.c -lmylib

    然后使用 clisp-link 去构建一个新的 CLISP linking set.

    添加 (:LIBRARY "my.dll") 给 FFI:DEF-CALL-OUT 结构, 用以下命令编译 my.c 为 my.so (或者win32上的 my.dll)
    $ gcc -shared -o my.so my.c -lmylib

    然后载入 my.fas.

当然, 你可以手动创建一个 my1.c ，它包含了

```C
#include <mylib.h>
int my_foo (int x) { return FOO(x); }
int my_bar (int x) { return bar(x); }
```

但是 FFI:C-LINES 允许你一起关闭 my_foo 和 my-foo ，以便于更好地管理.

示例 32.7. 从 C 中调用Lisp

为了使用Lisp函数 SORT 而不是C库函数 qsort 去给一个双浮点数组排序，可以适用以下 sort1.c 的接口代码. 主要的问题是传递一个可变长度的数组.

```C
extern void lispsort_begin (int);
void* lispsort_function;
void lispsort_double (int n, double * array) {
  double * sorted_array;
  int i;
  lispsort_begin(n); /* store #'sort2 in lispsort_function */
  sorted_array = ((double * (*) (double *)) lispsort_function) (array);
  for (i = 0; i < n; i++) array[i] = sorted_array[i];
  free(sorted_array);
}
```

这个也伴随着 sort2.lisp:

```LISP
(DEFPACKAGE "FFI-TEST" (:use “COMMON-LISP” “FFI”))
(IN-PACKAGE "FFI-TEST")
(EVAL-WHEN (compile) (setq FFI:*OUTPUT-C-FUNCTIONS* t))
(FFI:DEF-CALL-IN lispsort_begin (:ARGUMENTS (n int))
  (:RETURN-TYPE nil)
  (:LANGUAGE :stdc))
(FFI:DEF-C-VAR lispsort_function (:type c-pointer))
(defun lispsort_begin (n)
  (setf (cast lispsort_function
              `(c-function
                 (:ARGUMENTS (v (c-ptr (c-array double-float ,n))))
                 (:RETURN-TYPE (c-ptr (c-array double-float ,n))
                               :malloc-free)))
        #'sort2))
(defun sort2 (v)
  (declare (type vector v))
  (sort v #'<))
```

为了测试这个, 使用以下测试文件 sorttest.lisp:

```LISP
(EVAL-WHEN (compile) (setq FFI:*OUTPUT-C-FUNCTIONS* t))
(FFI:DEF-CALL-OUT sort10
  (:name "lispsort_double")
  (:LANGUAGE :stdc)
  (:ARGUMENTS (n int)
              (array (c-ptr (c-array double-float 10)) :in-out)))
```
              
尝试用

``SHELL
$ clisp-link create sort sort2.c sorttest.c
$ cc -O -c sort1.c
$ cd sort
$ ln -s ../sort1.o sort1.o
```

添加 sort1.o 到 link.sh 中的 NEW_LIBS 和 NEW_FILES 里. 创建一个文件 package.lisp 包含了以下结构
``(MAKE-PACKAGE "FFI-TEST" :use '(“COMMON-LISP” “FFI”))``

然后添加 package.lisp 到 link.sh 中的 TO_PRELOAD 里. 执行:

```SHELL
$ cd ..
$ base/lisp.run -M base/lispinit.mem -c sort2.lisp sorttest.lisp
$ clisp-link add base base+sort sort
$ base+sort/lisp.run -M base+sort/lispinit.mem -i sort2 sorttest
> (sort10 10 '#(0.501d0 0.528d0 0.615d0 0.550d0 0.711d0
                0.523d0 0.585d0 0.670d0 0.271d0 0.063d0))
#(0.063d0 0.271d0 0.501d0 0.523d0 0.528d0 0.55d0 0.585d0 0.615d0 0.67d0 0.711d0)
$ rm -r base+sort
```

示例 32.8. 从 C 动态地调用Lisp

创建一个包含了以下函数的动态库 lispdll (#P".dll" 对应 Win32, #P".so" 对应 UNIX) :

```C
typedef int (*LispFunc)(int parameter);
int CallInFunc(LispFunc f) {
  return f(5)+11;
}
```

然后从Lisp里调用它:

```LISP
(ffi:def-call-out callout
  (:name "CallInFunc")
  (:LIBRARY "lispdll.dll")
  (:ARGUMENTS (function-arg
               (ffi:c-function (:ARGUMENTS (number ffi:int))
                               (:RETURN-TYPE ffi:int) (:LANGUAGE :stdc))))
  (:RETURN-TYPE ffi:int)
  (:LANGUAGE :stdc))
(defun f (x) (* x 2))
⇒ F
(callout #'f)
⇒ 21
```
 

示例 32.9. 变化长度的参数: 从Clisp中调用 gethostname

这个标准的 UNIX 函数

这个标准的 UNIX 函数
int gethostname(name,   length); 
char* name;
size_t length;

沿用了典型的 C “out”-parameter 惯例模式: 它需要一个指向一个需要填充的缓冲区指针. 所以你必须指明这个参数为 :OUT 或者 :IN-OUT. 另外, 你必须告知这个函数缓冲区的大小. 这里的 length 只是一个 :IN 参数. 有时候会是一个 :IN-OUT 参数, 返回实际填充字节的数目.

所以 name 实际上是一个最大是 length 个字符的数组的指针, 不管 char* C 原型怎么描述的, 把它看作 C string (NULL-终结) 来使用. UNIX 指明  “主机名被限制为 HOST_NAME_MAX 字节的大小”, 当然这个大小取决于系统, 但是256是足够的了.

在这个例子中, 你可以使用 :ALLOCA 分配, 就像你在 C 里做的那样: 临时在栈上分配:

```LISP
(FFI:DEF-CALL-OUT gethostname
  (:ARGUMENTS (name (FFI:C-PTR (FFI:C-ARRAY-MAX ffi:char 256))
                    :OUT :ALLOCA)
              (length ffi:int))
  (:LANGUAGE :stdc) (:LIBRARY :default)
  (:RETURN-TYPE ffi:int))
⇒ GETHOSTNAME
(defun myhostname ()
  (multiple-value-bind (success name)    ; :OUT and :IN-OUT parameters are returned as multiple values
      (gethostname 256)
    (if (zerop success) name
      (error "~S: ~S: ~S" 'myhostname (os:errno) (os:strerror)))))    ; See Section 33.1.14, “Error handling”
⇒ MYHOSTNAME
(myhostname)
⇒ #(97 98 97 122 111 110 107)
```

它是一个 SIMPLE-VECTOR, 不是 STRING, 因为这个 name 参数是一个 char 数组(an INTEGER type, see Section 32.3.3, “(Foreign) C types”), 而不是character.

```LISP
(FFI:DEF-CALL-OUT gethostname
  (:ARGUMENTS (name (FFI:C-PTR (FFI:C-ARRAY-MAX character 256))
                    :OUT :ALLOCA)
              (length ffi:int))
  (:LANGUAGE :stdc) (:LIBRARY :default)
  (:RETURN-TYPE ffi:int))
⇒ GETHOSTNAME
(myhostname)
⇒ "abazonk"
```

现在我们有个不同的问题: 如果 gethostname 失败了, 然后分配给 name 的缓冲区会充满垃圾, 但是在我们检查 success 这个状态之前它会转为string. 如果 CUSTOM:\*FOREIGN-ENCODING* 是 CHARSET:ISO-8859-1, 这没有问题因为没有实际的转变发生, 但是用 CHARSET:UTF-8 可能导致一个 ERROR . 一种安全合适的方法是传递一个自己栈上分配的缓冲区给外部函数, 然后只在这个函数成功时转换缓冲区为string:

```LISP
(FFI:DEF-CALL-OUT gethostname
  (:ARGUMENTS (name FFI:C-POINTER)
              (length ffi:int))
  (:LANGUAGE :stdc) (:LIBRARY :default)
  (:RETURN-TYPE ffi:int))
⇒ GETHOSTNAME
(defun myhostname ()
  (FFI:WITH-FOREIGN-OBJECT (name '(FFI:C-ARRAY-MAX character 256))
    (let ((success (gethostname name 256)))
      (if (zerop success) (FFI:FOREIGN-VALUE name)
        (error "~S: ~S: ~S" 'myhostname (os:errno) (os:strerror))))))
⇒ MYHOSTNAME
(myhostname)
⇒ "abazonk"
```

注意这里的这个 FFI:WITH-FOREIGN-OBJECT 的 type 参数已经被求值, 所以我们不需要去做关于 HOST_NAME_MAX 的任何假设:

```LISP
﻿(defun myhostname ()
  (let ((host-name-max (os:sysconf :host-name-max)))
    (FFI:WITH-FOREIGN-OBJECT (name `(FFI:C-ARRAY-MAX character ,host-name-max))
      (let ((success (gethostname name host-name-max)))
        (if (zerop success) (FFI:FOREIGN-VALUE name)
          (error "~S: ~S: ~S" 'myhostname (os:errno) (os:strerror)))))))
⇒ MYHOSTNAME
(myhostname)
⇒ "abazonk" ﻿
```

示例 32.10. 访问动态库中的变量

想象一个人想要去访问和修改动态库中的变量:

```C
struct bar {
  double x, y;
  double out;
};

struct bar my_struct = {10.0, 20.5, 0.0};

double test_dll(struct bar *ptr)
{
  return ptr->out = ptr->out + ptr->x + ptr->y;
}
```

这个会编译为 libtest.so (或者 libtest.dll, 取决于平台).

使用以下的Lisp代码:

```LISP
(USE-PACKAGE “FFI”)

(FFI:DEF-C-STRUCT bar
  (x double-float)
  (y double-float)
  (out double-float))

(FFI:DEF-CALL-OUT get-own-c-float
  (:LIBRARY "libtest.so")
  (:LANGUAGE :stdc)
  (:name "test_dll")
  (:ARGUMENTS (ptr c-pointer :in :alloca))
  (:RETURN-TYPE double-float))

(FFI:DEF-C-VAR my-c-var (:name "my_struct")
  (:LIBRARY "libtest.so") (:type (c-ptr bar)))
```
  
注意这里的 get-own-c-float 需要一个 FFI:C-POINTER 作为参数, 不是 (FFI:C-PTR bar) .

现在你可以在 my-c-var 上调用 get-own-c-float  来访问:

```LISP
(FFI:C-VAR-ADDRESS my-c-var)
⇒ #<FOREIGN-ADDRESS #x282935D8>
(get-own-c-float (FFI:C-VAR-ADDRESS my-c-var))
⇒ 30.5d0
(get-own-c-float (FFI:C-VAR-ADDRESS my-c-var))
⇒ 61.0d0
(get-own-c-float (FFI:C-VAR-ADDRESS my-c-var))
⇒ 91.5d0
(get-own-c-float (FFI:C-VAR-ADDRESS my-c-var))
⇒ 122.0d0
```

示例 32.11. 控制资源的合法性

FFI:SET-FOREIGN-POINTER 和 (SETF FFI:VALIDP) 一起使用在限制扩展资源的范围是很有用的. 通过 FFI:VALIDP 检查可以避免第二次关闭. 所有根据这个资源的指针可以通过使用 FFI:SET-FOREIGN-POINTER 共享它们的 FFI:FOREIGN-POINTER 来使其在关闭时无效化.

```LISP
(FFI:DEF-C-TYPE PGconn c-pointer)    ; opaque pointer
(FFI:DEF-CALL-OUT PQconnectdb (:RETURN-TYPE PGconn)
  (:ARGUMENTS (conninfo c-string)))
(defun sql-connect (conninfo)
  (let ((conn (PQconnectdb conninfo)))
    (unless conn (error "NULL pointer"))
    ;; may wish to use EXT:FINALIZE as well
    (FFI:SET-FOREIGN-POINTER conn :COPY)))
(defun sql-dependent-resource (conn arg1)
  (FFI:SET-FOREIGN-POINTER (PQxxx conn arg1) conn))
(defun sql-close (connection)
  (when (FFI:VALIDP connection)
    (PQfinish connection)
    (setf (FFI:VALIDP connection) nil)
    T))
```
 
**警告**

    双向地共享 FFI:FOREIGN-POINTER : 依赖的资源非法化时要把主要的那个也无效化.

 
**注意**

    一种可替代的、更加适合非-“FFI” modules 的资源管理方式 , 在 berkeley-db 模块里实现了, 见 Section 33.6.2, “Closing handles”.

示例 32.12. 浮点数组指针

保存这个代码到 sum.c:

```C
double sum (int len, double *vec) {
  int i;
  double s=0;
  for (i=0; i<len; i++) s+= vec[i];
  return s;
}
```

然后用以下命令编译

```SHELL
$ gcc -shared -o libsum.so sum.c
```

现在可以用以下来对浮点数求和:

```LISP
(FFI:DEF-CALL-OUT sum (:name "sum") (:LIBRARY "libsum.so") (:LANGUAGE :stdc)
  (:RETURN-TYPE ffi:double-float)
  (:ARGUMENTS (len ffi:int) (vec (FFI:C-ARRAY-PTR ffi:double-float))))
(sum 3 #(1d0 2d0 3d0))
⇒ 6d0
```

32.3.10.1 更多示例

你可以在以下 clisp-list 信息中找到更多的信息和示例:

variable size values

    SFmail/9F8582E37B2EE5498E76392AEDDCD3FE05F4B8F7%40G8PQD.blf01.telekom.de/Gmane/general/7278
    
variable length arrays

    SFmail/9F8582E37B2EE5498E76392AEDDCD3FE0287A252%40G8PQD.blf01.telekom.de/Gmane/general/6626 SFmail/9F8582E37B2EE5498E76392AEDDCD3FE0287A3B1%40G8PQD.blf01.telekom.de/Gmane/general/6628

甚至更多的示例可以在 CLISP 源码发行版中的 tests/ffi.tst 文件找到.

## 32.4 <span id = "SocketStreams">套接字流</span>

平台依赖: 仅限 UNIX, Win32 平台

> * 32.4.1 [引言](#SSIntroduction)
> * 32.4.2 [套接字API参考](#SocketAPIReference)
> * 32.4.3 [参数 :TIMEOUT](#ArgumentTIMEOUT)

### 32.4.1 <span id = "SSIntroduction">引言</span>

Sockets 被用于单主机或者计算机网络间多主机的进程间通信. 最常见的套接字种类是网络流套接字, 这里描述的它们的高级接口. 一个更加底层的接近C系统调用的接口也是可用的, 见 Section 33.17, “Raw Socket Access”.

两个主要可连接的socket的变种:

“active” sockets

    对应 SOCKET:SOCKET-STREAM，它是双向的 STREAMs
    
“passive” sockets

    对应 SOCKET:SOCKET-SERVER，它是一种被用于允许另一边和Lisp发起联系的特殊种类的对象.

示例 32.13. Lisp read-eval-print loop server

这里有个简单的 read-eval-print loop 服务器，他会等待远程链接并且执行读到的Lisp结构:

```LISP
(LET ((server (SOCKET:SOCKET-SERVER)))
  (FORMAT t "~&Waiting for a connection on ~S:~D~%"
          (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
  (UNWIND-PROTECT
      ;; infinite loop, terminate with Control+C
      (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server))
              (MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
                (MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
                  (FORMAT T "~&Connection: ~S:~D -- ~S:~D~%"
                          remote-host remote-port local-host local-port)))
              ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
              (LOOP (WHEN (EQ :eof (SOCKET:SOCKET-STATUS (cons socket :input))) (RETURN))
                    (PRINT (EVAL (READ socket)) socket)
                    ;; flush everything left in socket
                    (LOOP :for c = (READ-CHAR-NO-HANG socket nil nil) :while c)
                    (TERPRI socket))))
    ;; make sure server is closed
    (SOCKET:SOCKET-SERVER-CLOSE server)))
```
 
这个导致一个安全上的漏洞!

类似 EXT:SHELL, EXT:EXECUTE, EXT:RUN-SHELL-COMMAND 函数会允许用一个远程主机以你的权限去执行任意的代码. 虽然lisp里定义的函数 (像EXT:RUN-SHELL-COMMAND) 可以被移除 (通过 FMAKUNBOUND), 但是内置的函数 (像 EXT:SHELL 和 EXT:EXECUTE) 不能被永久地从 runtime 中移除, 所以一个有经验的黑客可能会调用后者即便你把它们的名字 FMAKUNBOUND 了.

你应该通过把 STRING "127.0.0.1" 作为 :INTERFACE 参数给 SOCKET:SOCKET-SERVER 来限制socket server 为本地连接.

示例 32.14. Lisp HTTP 客户端

这里有几个简易的 lisp HTTP 客户端用于取得web页面，一个二进制文件，还有上传一个文件:

```LISP
(DEFUN wget-text (host page file &OPTIONAL (port 80))
  ;; HTTP requires the :DOS line terminator
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
     (FORMAT socket "GET ~A HTTP/1.0~2%" page)
     ;; dump the whole thing - header+data - into the output file
     (WITH-OPEN-FILE (out file :direction :output)
       (LOOP :for line = (READ-LINE socket nil nil) :while line
          :do (WRITE-LINE line out)))))
(DEFUN wget-binary (host page file &OPTIONAL (port 80))
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
    (FORMAT socket "GET ~A HTTP/1.0~2%" page)
    (LOOP :with content-length :for line = (READ-LINE socket nil nil)
      ;; header is separated from the data with a blank line
      :until (ZEROP (LENGTH line)) :do
      (WHEN (STRING= line #1="Content-length: " :end1 #2=#.(LENGTH #1#))
        (SETQ content-length (PARSE-INTEGER line :start #2#))
      ;; this will not work if the server does not supply the content-length header
      :finally (RETURN (LET ((data (MAKE-ARRAY content-length
                                               :element-type '(UNSIGNED-BYTE 8))))
                           ;; switch to binary i/o on socket
                           (SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
                           ;; read the whole file in one system call
                           (EXT:READ-BYTE-SEQUENCE data socket)
                           (WITH-OPEN-FILE (out file :direction :output
                                                :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                             ;; write the whole file in one system call
                             (EXT:WRITE-BYTE-SEQUENCE data out))
                           data))))))
(DEFUN wput (host page file &OPTIONAL (port 80))
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
    (WITH-OPEN-FILE (in file :direction :inptut :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
      (LET* ((length (FILE-LENGTH in))
             (data (MAKE-ARRAY length :element-type '(UNSIGNED-BYTE 8))))
        ;; some servers may not understand the "Content-length" header
        (FORMAT socket "PUT ~A HTTP/1.0~%Content-length: ~D~2%" page length)
        (SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
        (EXT:READ-BYTE-SEQUENCE data in)
        (EXT:WRITE-BYTE-SEQUENCE data socket)))
    ;; not necessary if the server understands the "Content-length" header
    (SOCKET:SOCKET-STREAM-SHUTDOWN socket :output)
    ;; get the server response
    (LOOP :for line = (READ-LINE socket nil nil) :while line :collect line)))
```

### 32.4.2 <span id = "SocketAPIReference">套接字API参考</span>

``(SOCKET:SOCKET-SERVER &OPTIONAL port &KEY :INTERFACE :BACKLOG)``

    这个函数创建一个 passive socket 绑定一个端口. 这个服务器会等待客户端的连接尝试.

    这个可选的参数是要使用的端口 (非负的 FIXNUM, 0 意味着由系统来赋值).

    这个 :BACKLOG 参数定义了等待连接队列最大长度 (见 listen) 默认是1.

    这个 :INTERFACE 参数指明了socket server监听的接口, 它是一个 STRING, 被解释为接口绑定的 IP 地址, 或者是一个socket, 创建对应的连接.

    默认是 (为了向后兼容) 绑定所有本地接口, 但是如果你只要本地的连接，出于安全考虑应该绑定本地回环接口 "127.0.0.1" .

``(SOCKET:SOCKET-SERVER-CLOSE socket-server)``

    关闭服务器的套接字. 就像流一样, SOCKET:SOCKET-SERVER 在垃圾回收时关闭. 然而你不应该依靠这个, 因为垃圾回收的时间不确定而且这个绑定的端口在被关闭前都不可使用.
    
``(SOCKET:SOCKET-SERVER-HOST socket-server)``
``(SOCKET:SOCKET-SERVER-PORT socket-server)``

    返回这个主机掩码，它指明了哪些主机可以连接到这个服务器以及使用 SOCKET:SOCKET-SERVER 绑定的端口.
    
``(SOCKET:SOCKET-WAIT socket-server &OPTIONAL [seconds [microseconds]])``

    以一个合适的时间等待新的连接到 socket-server (一个 SOCKET:SOCKET-SERVER). 没有 timeout 参数时, SOCKET:SOCKET-WAIT 会无限期阻塞下去. 当 timeout 是0时, poll. 当一个连接可用时返回 T  (i.e., SOCKET:SOCKET-ACCEPT 不会阻塞) ，timeout时返回 NIL .
    
``(SOCKET:SOCKET-ACCEPT socket-server &KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED :TIMEOUT)``

    等待一个连接到 socket-server 的尝试然后为这个连接创建一个服务端的双向的 SOCKET:SOCKET-STREAM .

    如果这时没有连接被创建就发送一个 ERROR .
    
``(SOCKET:SOCKET-CONNECT port &OPTIONAL [host] &KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED :TIMEOUT)``

    尝试去创建一个客户端的双向 SOCKET:SOCKET-STREAM. 直到服务器接受这个连接之前会阻塞, 不会超过 :TIMEOUT 的秒数. 如果是0, 会立即返回并可能阻塞在下一次 i/o 操作 (你可以使用 SOCKET:SOCKET-STATUS 去检查它是否会阻塞).
    
``(SOCKET:SOCKET-STATUS socket-stream-or-list &OPTIONAL [seconds [microseconds]])``

    非阻塞地检查是否对一个 SOCKET:SOCKET-STREAM 可读或可写，或者一个 SOCKET:SOCKET-SERVER 上的连接是否可用.

    这个类似于 LISTEN, 它检查 STREAM 并且只对于输入, 也类似于 SOCKET:SOCKET-WAIT, 它只与 SOCKET:SOCKET-SERVER 一起工作.

    如果任何i/o 操作会导致一个 ERROR ，我们为一个 SOCKET:SOCKET-SERVER 或一个 SOCKET:SOCKET-STREAM 定义 status 为 :ERROR .

    另外, 对于一个 SOCKET:SOCKET-SERVER, 如果一个连接可用我们定义 status 为 T , 换句话说, 如果 SOCKET:SOCKET-ACCEPT 不会阻塞, 连接不可用就是 NIL.

    另外, 对于一个 SOCKET:SOCKET-STREAM, 我们定义 status 为一个给定的 direction ( :INPUT, :OUTPUT, 还有 :IO 的其中之一)

    可变方向的可能的状态值:

    :INPUT status:
    	

    NIL
    	读取时阻塞

    :INPUT
    	一些输入可用

    :EOF
    	这个流已经到达末尾

    :OUTPUT status:
    	

    NIL
    	写的时候阻塞

    :OUTPUT
    	输出到流里不会阻塞

    :IO status:
    	
    output status	input status
    NIL	:INPUT	:EOF
    NIL	NIL	:INPUT	:EOF
    :OUTPUT	:OUTPUT	:IO	:APPEND

    socket-stream-or-list 可能的值:

    SOCKET:SOCKET-STREAM 或 SOCKET:SOCKET-SERVER
            返回合适的状态, 像上面定义的 (对于 SOCKET:SOCKET-STREAM 的 :IO )
    (SOCKET:SOCKET-STREAM . direction)
            返回指定方向的状态
    以上所述的非空列表
            返回一个列表, 其中的值是参数列表的每一个值 (按 MAPCAR 方式)

    如果你想避免构建新的列表, 你可以使 socket-stream-or-list 的元素为 (socket-stream direction . x) 或 (socket-server . x). 然后 SOCKET:SOCKET-STATUS 会破坏性地修改它的参数并用状态替换 x 或 NIL 然后返回被修改的列表. 你可以再次传递这个被修改的列表给 SOCKET:SOCKET-STATUS .

    这个可选的参数指定了超时时间. NIL 意味着一直等待, 0 表示 poll.

    返回的第二个值是非 NIL 状态的对象的数量, i.e., “actionable” 对象. SOCKET:SOCKET-STATUS 由于超时或者这个数字是正的而返回, i.e., 如果 timeout 是 NIL 并且 SOCKET:SOCKET-STATUS 确实返回了, 然后第二个值是一个正数 (这个就是 NIL 不被认为是空 LIST 的原因, 但是是一个非法的参数).

    注意这个 SOCKET:SOCKET-STATUS 可能导致一个 STREAM-ERROR. 如果 SOCKET:SOCKET-STREAM 收到一个 RST 包就会发生, 见 tests/econnreset.lisp.

    这是 select 的接口(在一些平台上, poll), 所以它会工作在基于文件描述符的任何 CLISP STREAM 上, e.g., EXT:*KEYBOARD-INPUT* 和 file/pipe/socket STREAM, 在 raw sockets 上也是.
    
``(SOCKET:SOCKET-STREAM-HOST socket-stream)``
``(SOCKET:SOCKET-STREAM-PORT socket-stream)``

    这两个函数返回关于 SOCKET:SOCKET-STREAM 的信息.
    
``(SOCKET:SOCKET-STREAM-PEER socket-stream [do-not-resolve-p])``

    给一个 SOCKET:SOCKET-STREAM, 这个函数返回对面连接的主机名和端口号; 服务端可以用这个来看谁已经连接了.

    当这个可选的第二个参数是非 NIL, 主机名解析就会无效化，只返回 IP 地址, 没有 FQDN.

    这个 socket-stream 参数也可以是一个 raw socket.
    
``(SOCKET:SOCKET-STREAM-LOCAL socket-stream [do-not-resolve-p])``

    对应于 SOCKET:SOCKET-STREAM-PEER - 相同的信息，主机名和端口号，但是只是本地主机. 这个 SOCKET:SOCKET-STREAM-HOST 和 SOCKET:SOCKET-STREAM-PORT 的差别是这个函数请求操作系统 (所以返回正确可信的值) 而另外两个只是访问网络数据结构, 基本上就是返回传递给创建 socket-stream 的函数的参数.

    这个 socket-stream 参数也可以是一个 raw socket.
    
``(SOCKET:SOCKET-STREAM-SHUTDOWN socket-stream direction)``

    一些协议提供了 shutdown 关闭连接上的一个 direction . 这个函数提供了一个 UNIX 系统调用的接口. direction 应该是 :INPUT 或者 :OUTPUT. 注意这里使用完 socket-stream 后你应该始终调用 CLOSE ; 使用 WITH-OPEN-STREAM 是最好的.

    所有 SOCKET:SOCKET-STREAM 都是双向的 STREAMs (i.e., 对于它们， INPUT-STREAM-P 和 OUTPUT-STREAM-P 都返回 T ). SOCKET:SOCKET-STREAM-SHUTDOWN 打破这个规则，把它的参数流变成一个 input STREAM (如果 direction 是 :OUTPUT) 或者 output STREAM (如果 direction 是 :INPUT). 所以, 无论何时以下重要的不变规则是要遵守的:

        一个 STREAM 打开 (i.e., OPEN-STREAM-P 返回 T) 并且
        一个 STREAM 是一个 input STREAM (i.e., INPUT-STREAM-P 返回 T)

    这个 STREAM 可以被读取 (e.g., 使用 READ-CHAR 或者 READ-BYTE).

    这个 socket-stream 参数也可以是一个 raw socket.
    
``(SOCKET:SOCKET-OPTIONS socket-server &REST {option}*)``

    通过 getsockopt 和 setsockopt 查询或者设置socket的选项. 一个 option 是一个关键字, 可能后面跟一个新值. 当这个新的值没有提供时, setsockopt 不会调用. 对每一个选项返回一个旧值 (或者没有提供新值时就是当前的)  . E.g., (SOCKET:SOCKET-OPTIONS socket-server :SO-LINGER 1 :SO-RCVLOWAT) 返回2个值: NIL, 这个 :SO-LINGER 选项的旧值, 以及 1, 当前 :SO-RCVLOWAT 选项的新值.

    这个 socket-stream 参数也可以是一个 raw socket.
    
``(SOCKET:STREAM-HANDLES stream)``

    以多值的形式返回这个流的输入输出OS文件描述符. 见 Section 33.17, “Raw Socket Access”.

### 32.4.3 <span id = "ArgumentTIMEOUT">参数 :TIMEOUT</span

这个 :TIMEOUT 参数指明了等待事件的秒数. 它的值可能是

    一个非负的 REAL 或者
    一个 LIST (sec usec) 或者
    一个 CONS (sec . usec).

## 32.5 <span id = "MultipleThreadsExecution">多线程执行</span>

平台依赖: 仅限使用了编译期标识 --with-threads 构建的 CLISP .

**警告**

    这个功能是实验性的.

    使用这个需要你自己承担风险.

可以在 clisp-devel 上讨论这个问题

> * 32.5.1. [引言](#MTEIntroduction)
> * 32.5.2. [通用准则](#GeneralPrinciples)
> * 32.5.3. [线程 API 参考](#ThreadAPIReference)

### 32.5.1 <span id = "MTEIntroduction">引言</span>

CLISP 使用操作系统线程去实现多线程调度. 支持两种风格: POSIX 和 Win32. 都是抢占式的.

所有符号都导出自 “THREADS” 包, 它有两个别名： “MT” (针对多线程) 和 “MP” (针对多进程). 当这个功能可用时, \*FEATURES* 包含了符号 :MT.

见 Section 35.7, “Garbage Collection and Multithreading”.

### 32.5.2 <span id = "GeneralPrinciples">通用准则</span>

> * 32.5.2.1 [并发性](#Parallelizability)
> * 32.5.2.2 [特殊变量值](#SpecialVariableValues)
> * 32.5.2.3 [包](#GPPackages)
> * 32.5.2.4 [CLOS](#GPCLOS)
> * 32.5.2.5 [哈希表，序列，以及其他可变对象](#HashTablesSequences)
> * 32.5.2.6 [线程不安全代码示例](#ExamplesThreadUnsafe)

#### 32.5.2.1 <span id = "Parallelizability">并发性</span>

一个单线程程序不会和运行在其他线程的程序共享对象，运行没有问题.

特别地: 如果在单线程的情况下，在语义上先执行A程序再执行B程序和先执行B程序再执行A程序结果是一样的，然后在多线程情况下，很可能在不同线程同时运行A和B， 这时的结果和前面两个单线程例子是一样的 (A 先于 B，或者 B 先于 A).

总结

    如果A和B没有共同的对象, 然后这个实现确保满足这个原则.
    如果A和B共享一些对象, 这个实现允许程序使用更少的努力满足这个原则.

#### 32.5.2.2 <span id = "SpecialVariableValues">特殊变量值</span>

每一个自由变量有一个全局的值可以被跨 MT:THREAD 共享.

这里自由变量的绑定(通过 LET/LET*/MULTIPLE-VALUE-BIND) 是 MT:THREADs 本地的, 换句话说. 每一个 SYMBOL 在每一个 MT:THREAD 都有一个不同的单元 . MT:SYMBOL-VALUE-THREAD 可以被用于查看和修改这些线程本地绑定.

线程不会同父线程中继承动态绑定.

示例:

```LISP
(defvar *global* 1)    ; create a Global Variable
(defun thread-1 ()
       ; here *global* and (SYMBOL-VALUE *global*) will be 1 not 2!
  (setq *global* 5)    ; change the Global Variable value
  (let ((*global* 10))    ; Per-Thread Variable value is initialized
    (setq *global* 20)    ; Per-Thread Variable value is changed
        ; Global Variable value is not accessible here (only via MT:SYMBOL-VALUE-THREAD)
    )
  (setq *global* 30))    ; Global Variable value is modified again
(let ((*global* 2))    ; Per-Thread Variable value is initialized
  (MT:MAKE-THREAD #'thread-1))
```  

#### 32.5.2.3 <span id = "GPPackages">包</span>
注意

这个章节讨论的锁定和 EXT:PACKAGE-LOCK 无关.

PACKAGE 对象有一个内部的 MT:MUTEX 并且在添加符号前被 INTERN 锁定 (如果 FIND-SYMBOL 失败了). 所有包内数据的修改都被 MT:MUTEX 所监控。

当使用 DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS, WITH-PACKAGE-ITERATOR 或者 LOOP for-as-package subclause 迭代包符号时也是被锁定的.

#### 32.5.2.4 <span id = "GPCLOS">CLOS</span>
这个信息可能在最近会被修改

CLOS 不是线程安全的. DEFCLASS, DEFGENERIC, DEFMETHOD, DEFSTRUCT 修改 CLOS 时没有任何锁定可能会影响其他的任何一个.

建议在任何 MT:THREAD 生产出来前，把所有代码都加载进来。

#### 32.5.2.5 <span id = "HashTablesSequences">哈希表，序列，以及其他可变对象</span>
如果你想射击你自己，你需要穿上盔甲。

没有东西会被自动锁定 (自动加锁会对线程本地的 HASH-TABLE 和 SEQUENCE ﻿实施不合理的惩罚), 所以用户在线程间共享 HASH-TABLEs, SEQUENCEs 还有其他用户定义的可变对象时必须锁定.

这个方法和常见的 Common Lisp 是一致的: 
 	

    当代码在object-traversing操作时破坏性地修改了这个对象时，可能会影响正在进行的traversal操作，结果不可预料
	 
 	    --[sec_3-6]

    如果一个对象O1被用于一个哈希表H的一个key，然后在H的等效性检验被修改了, 这时如果O1在后面H的操作上被用于一个key，结果是无法预料的

 	    --[sec_18-1-2]

##### 32.5.2.5.1 RANDOM 和 RANDOM-STATE

RANDOM 没有锁定就修改了一个 RANDOM-STATE ， 这也就意味着你不能随意地在线程间共享这写对象. 然而, \*RANDOM-STATE* 在每个线程绑定 (见 MT:MAKE-THREAD 和 MT:\*DEFAULT-SPECIAL-BINDINGS*), 换句话说., 每一个线程都有它自己的值，所以 RANDOM 都是线程安全的.

#### 32.5.2.6 <span id = "ExamplesThreadUnsafe">线程不安全代码示例</span>

这里有一些代码结构，如果两个线程执行没有加锁时，它的结果是无法确定的:

```LISP
(INCF (GETHASH x global-ht 0))    ; see Section 32.5.2.5, “Hash Tables, Sequences, and other   mutable objects”
(SETF (AREF global-array ...) ...)    ; ditto
(DEFMETHOD generic-function (...) ...)    ; see Section 32.5.2.4, “CLOS”
```

### 32.5.3 <span id = "ThreadAPIReference">线程 API 参考</span>

MT:THREAD

    这个类型的对象被 MT:MAKE-THREAD 返回。

    每一个 MT:THREAD 表示一个分离的计算单元, 和其他的被一起执行.
    
``(MT:MAKE-THREAD function &KEY :NAME :INITIAL-BINDINGS :CSTACK-SIZE :VSTACK-SIZE)``

    启动一个新的 MT:THREAD 来运行 function.

    :INITIAL-BINDINGS
        一个 (symbol . form) 的关联列表. 这些 form 在新的线程里是已经求值的并且这些 symbol 在 function 被调用前已经绑定到线程的结果上了. 默认的值是 MT:*DEFAULT-SPECIAL-BINDINGS*.

        这个参数的主要目的是初始化一些不应该在线程间共享的全局数据， 比如, *RANDOM-STATE*, *READTABLE*.

        使用 :INITIAL-BINDINGS 时，最好在 MT:*DEFAULT-SPECIAL-BINDINGS* 之前 CONS 应用特殊的数据或者拷贝修改它来适应应用的需要.
        注意

        当相同的 symbol 多次出现在这个关联列表, 第一次出现决定了这个符号的值.

    :CSTACK-SIZE
        这个控制 (C) 栈的字节数. 如果是 0, 这个值由操作系统定义.

    :VSTACK-SIZE
        这个是 CLISP STACK 对象大小. 默认值是基于启动 CLISP 指定的 -m 选项计算出来的. 如果是 0, 这个值会和调用线程的对应值一样.

    如果 MT:THREAD 创建失败 (比如, 由于系统内存不够), 一个 CONTROL-ERROR 会被发出来.

    参照 pthread_create.
    
``(MT:THREADP object)``

    检查这个对象是否为 MT:THREAD 类型.
    
``(MT:THREAD-YIELD thread)``

    让出 CPU. 这个 thread 会被放到运行队列的末尾，然后另一个线程会被调度过来运行.

    参照 sched_yield.
    
``(MT:THREAD-INTERRUPT thread &KEY :FUNCTION :OVERRIDE :ARGUMENTS)``

    中断 thread 中正常的调度流然后请求它来 APPLY function 带 arguments 参数.

    使用 (MT:THREAD-INTERRUPT thread :FUNCTION NIL) 来调试 thread ，用 (MT:THREAD-INTERRUPT thread :FUNCTION T) 来终止 thread.

    这个 :OVERRIDE 参数重写 MT:WITH-DEFERRED-INTERRUPTS 并且应该特别小心.

    线程只能在可以发生垃圾回收的点上被打断, 见 Section 35.7, “Garbage Collection and Multithreading”.

    目前 Win32 上的阻塞 I/O 不能被打断. 这个调用返回后这个中断就会被处理.
    警告

    线程可能在 UNWIND-PROTECT 的清理结构中还有从 function 里的非本地退出中被中断 - 它们没有彻底执行. 为了防止这个情况,提供了 MT:WITH-DEFERRED-INTERRUPTS.
    
``(MT:THREAD-NAME thread)``

    返回这个 thread 的名字.
    
``(MT:THREAD-ACTIVE-P thread)``

    如果这个线程已经终止了就返回NIL，否则就是T.
    警告

    到这个函数返回 T 的时候, thread 可能已经终止了.
    
``(MT:CURRENT-THREAD)``

    返回调用的当前线程的这个 MT:THREAD 对象.
    
``(MT:LIST-THREADS)``

    返回所有当前运行着的 MT:THREAD 列表.
    警告

    到这个函数返回时, 这个返回值中实际运行的线程可能有一个单个的交集 - 就是 MT:CURRENT-THREAD.
    
``MT:MUTEX``

    这个类型的对象被 MT:MAKE-MUTEX 返回.

    这个表示一个锁, 换句话说, 阻止不同线程在同一时间做一些事的方法, 比如, 修改同一个对象.
    
``(MT:MUTEXP object)``
    检查这个对象是否为 MT:MUTEX.
    
``(MT:MAKE-MUTEX &KEY :NAME :RECURSIVE-P)``

    创建一个新的 MT:MUTEX 对象 - 还没有被任何线程上锁. :NAME 应该是一个 STRING 描述了互斥量 (这个真的会帮助调试死锁). 当 RECURSIVE-P 不是 NIL, 一个支持递归上锁的 MT:MUTEX 会被创建 换句话说, 一个线程可以重复获取这个互斥量 (当然，对于每一次成功的获取都要释放).

    参照 pthread_mutex_init.
    
``(MT:MUTEX-NAME thread)``

    返回这个 MT:MUTEX 的名字.
    
``(MT:MUTEX-LOCK mutex &KEY :TIMEOUT)``

    获取这个 mutex. 如果 mutex 被另一个线程所占, 这个调用会阻塞并等待 :TIMEOUT 秒数.如果 :TIMEOUT 没有被指定, 就一直等下去.

    在一个成功的锁定 mutex 后返回 T , 超时返回 NIL .

    如果这个调用的线程已经获取 mutex, 然后

        如果 mutex 是递归的, T 会被返回 (对于这里每一个递归的 MT:MUTEX-LOCK 应该对应一个独立的 MT:MUTEX-UNLOCK);
        如果 mutex 不是递归的的话，为了避免死锁一个 ERROR 会被发出.

    参照 pthread_mutex_lock.
    
``(MT:MUTEX-UNLOCK mutex)``

    释放 (解锁) mutex. 如果调用的线程没有锁定 mutex, 一个 ERROR 会被发出.

    参照 pthread_mutex_unlock.
    
``(MT:MUTEX-OWNER mutex)``

    返回持有这个互斥量的 MT:THREAD , 如果 mutex 没有被锁定返回NIL.
    警告

    这个函数返回 mutex 的所有者可能已经改变了 (除非锁定者就是 MT:CURRENT-THREAD). 这个函数在调试死锁时很有帮助.
    
``(MT:MUTEX-RECURSIVE-P mutex)``

    返回这个 mutex 是否递归的断言.
    
``(MT:WITH-LOCK (mutex) &BODY body)``

    锁定 mutex 里执行 body. 离开时 mutex 被释放. 在 body 返回时返回.
    
``MT:EXEMPTION``

    这个类型的对象由 MT:MAKE-EXEMPTION 创建.  这些对应 POSIX 状态变量, 见 <pthread.h>.

    这些对象允许从一个 MT:THREAD 广播对象到其他的.
    
``(MT:EXEMPTIONP object)``

    检查这个对象是否为 MT:EXEMPTION 类型.
    
``(MT:MAKE-EXEMPTION &KEY :NAME)``

    创建一个 MT:EXEMPTION 对象. :NAME 应该是一个描述这个exemption的字符串 (这个有助于调试死锁).

    参照 pthread_cond_init.
    
``(MT:EXEMPTION-NAME thread)``

    返回这个 exemption 的名字.
    
``(MT:EXEMPTION-SIGNAL exemption)``

    发送 exemption 对象, 换句话说, 唤醒一个等待 exemption 的阻塞线程.

    参照 pthread_cond_signal.
    
``(MT:EXEMPTION-WAIT exemption mutex &KEY :TIMEOUT)``

    等待另一个 MT:THREAD 在 exemption 去调用 MT:EXEMPTION-SIGNAL 或者 MT:EXEMPTION-BROADCAST . mutex 应该被调用者锁定; 否则发出一个 ERROR . 这个函数会释放这个 mutex 然后等待一个 exemption. 在返回时 mutex 再一次被获取.

    这个函数等待了 :TIMEOUT 秒数. 如果没有指定超时, 就一直等下去.

    如果 exemption 被发出了并且超时为nil，就返回 T .

    在 POSIX 上它可能会虚假唤醒,换句话说, 即使没有线程调用 MT:EXEMPTION-BROADCAST 或 MT:EXEMPTION-SIGNAL 这个函数也可能返回 T . 因此, 使用这个函数的一种常用的用法 (LOOP :while (some-condition-is-satisfied) :do (MT:EXEMPTION-WAIT exemption mutex)).

    参照. pthread_cond_wait.
    
``(MT:EXEMPTION-BROADCAST exemption)``

    发送 exemption 给所有等待它的线程.

    参见. pthread_cond_broadcast.
    
``(MT:Y-OR-N-P-TIMEOUT seconds default &REST arguments)``
``(MT:YES-OR-NO-P-TIMEOUT seconds default &REST arguments)``

    类似于 Y-OR-N-P 和 YES-OR-NO-P, 但是当超时期间没有给予回复的使用 MT:WITH-TIMEOUT 来返回 DEFAULT .
    
``(MT:WITH-TIMEOUT (seconds &BODY timeout-forms) &BODY body)``

    执行 body. 如果它在指定的seconds指定的秒数里没有完成, 它会中断并且 timeout-forms 会被执行.

    返回 body 或者 timeout-forms 最后求值的结构的值。
    警告

    因为超时的时候当前线程被打断, 需要注意确保在 body 合适的清理. 见 MT:THREAD-INTERRUPT 和 MT:WITH-DEFERRED-INTERRUPTS.
    
``(SETF (MT:SYMBOL-VALUE-THREAD symbol thread) value)``
``(MT:SYMBOL-VALUE-THREAD symbol thread)``

    访问或者设置 Per-Thread Variable 值. 当 thread 是 T, 就使用 MT:CURRENT-THREAD; 如果它是 NIL, 使用 Global Variable 绑定. 返回两个值: 一个 symbol 绑定和一个指示:如果没有在 thread 中绑定就是 NIL ,绑定了就是 T , 并且 如果这个 Per-Thread Variable 绑定通过 MAKUNBOUND 移除了就把 SYMBOL 也 MAKUNBOUND . 
    
``MT:*DEFAULT-SPECIAL-BINDINGS*``

    这个是 MT:MAKE-THREAD 的参数 :INITIAL-BINDINGS 默认值.
    
``(MT:WITH-DEFERRED-INTERRUPTS &BODY body)``

    当 body 被执行时延迟线程中断 (但是不是线程的优先级) . 如果当 body 运行时有一个中断, 它会放到队列里然后在 body 完成后被执行.
    警告

    如果在 body 中等待或者阻塞就需要多加注意, 因为这里无法去中断它 (防备死锁). 这个宏被用于那些偏爱执行 UNWIND-PROTECT 的清理结构中，以免它被非本地退出所打断.
    
``(MT:THREAD-JOIN thread &KEY :TIMEOUT)``

    等待 thread 去终止然后返回两个值: thread 的值列表以及一个 BOOLEAN 来表示 thread 是正常完成还是被 MT:THREAD-INTERRUPT 打断.

    这个函数使用 MT:EXEMPTION-WAIT (不是 pthread_join), 所以用这个函数正常没有分配的资源泄漏.

    超时的时候，返回 NIL 还有 :TIMEOUT.

    这个函数可以在同一线程重复使用, 所以这是访问结束线程的返回值的常用方法.

## 32.6 <span id = "QuickstartingCLISP">clisp 快速启动的交付</span>

这个章节描述了三种将 CLISP 程序变为可执行程序的方式, 这个生成程序可以像其他语言写的程序一样快速执行.

> * 32.6.1 [概要](#QSSummary)
> * 32.6.2 [clisp 脚本](#ScriptingCLISP)
> * 32.6.3 [桌面环境](#DesktopEnvironments)
> * 32.6.4 [通过内核来关联 CLISP 扩展名](#AssociatingCLISP)

### 32.6.1 <span id = "QSSummary">概要</span>

UNIX

        CLISP 可以表现得像一个脚本解释器.
        
类似 KDE, Gnome, Mac OS X or Win32 桌面环境中.

        通过 CLISP 创建的文件可以被关联为 CLISP 可执行文件所以点击它们会使 CLISP 执行适当的代码.
        
带有 CONFIG_BINFMT_MISC=y 的 Linux 内核

        为 CLISP关联扩展名 #P".fas" 和  #P".lisp" ; 然后你可以使这些文件可执行然后在命令行运行.


多文件应用

这三种方式可以应用于 #P".lisp" 或 #P".fas" 文件. 如果你的应用由多个 #P".lisp" 或者 #P".fas" 文件构成, 你可以简单地把它们拼接到一个文件 (通过 cat); 这个手法可以应用到那个拼接的文件里.
Lisp-less target

这三种手法都假定目标机器中已经安装了 CLISP 并且你可以只发行你的应用, 而不是 CLISP 自身. 如果你想不考虑目标机器的情况发行你的应用， 你可以去创建可执行的内存镜像.

### 32.6.2 <span id = "ScriptingCLISP">clisp 脚本</span>
平台依赖: 仅限 UNIX 平台.

在 UNIX 平台上,一个文本文件 (#P".fas" or #P".lisp") 可以通过在第一行添加如下内容成为可执行文件

1
#!interpreter [interpreter-arguments]

然后通过 chmod 来使这个文件可执行.

OS 需要. CLISP 可以在以下情况下用作脚本解释器:

    这个 interpreter 必须是 CLISP 完整路径名. 推荐的路径是 /usr/local/bin/clisp, 如果 CLISP 已经安装在其他地方, 使 /usr/local/bin/clisp 成为一个指向真实的 CLISP 的符号链接.
    这个 interpreter 真实的二进制可执行文件, 不是一个脚本. 不幸的是, 在Solaris 上的 CLISP 二进制发行版 , clisp 命令是一个脚本因为在这个平台上C编译不能假定已经安装. 如果你确认已安装C编译器, 从源码自己构建 CLISP ; make install 会安装一个真实的clisp可执行文件.

    在一些平台, 制定解释器的第一行被限制长度:
        SunOS 4 最大 32 个字 ,
        HP-UX 最大 80 个字 ,
        Linux 最大 127 个字.

    超过这个限制的字符集会被操作系统截取掉. Solaris, IRIX, AIX, OSF/1至少支持128个字 . 这里没有变通方案: 你不得不去保持解释器路径和参数短小.
    在 Solaris 和 HP-UX 上, 只有第一个 interpreter-arg 传递给 interpreter. 为了传递超过一个选项 (比如, -M 和 -C) 给 CLISP, 用不中断空格分隔它们，而不是正常的空格. (但是分隔 interpreter 和 interpreter-arguments 必须是正常的空格!) CLISP 在不中断空格和正常空格都会分割 interpreter-arguments .

脚本的执行. 

    这个脚本应该包含 Lisp 结构, 除了 #! 行.
    这个文件通过函数 LOAD 正常加载(特别指出, 这个脚本文件的名字, 在 /bin/sh 中是 $0 , 在这里是 *LOAD-TRUENAME* 和 *LOAD-PATHNAME*).
    在被加载以前, 变量 EXT:*ARGS* 绑定给字符串列表 , 表示传递给Lisp脚本的参数 (换句话说, 类似在 /bin/sh 中的 $1 , 这里是  (FIRST EXT:*ARGS*) 等).
    标准 UNIX i/o 功能 (见 <stdio.h>) 也可以使用: *STANDARD-INPUT* 被绑定为 stdin, *STANDARD-OUTPUT* 绑定为 stdout, 还有 *ERROR-OUTPUT* 绑定为 stderr. 注意 Section 25.2.10.1, “Scripting and DRIBBLE”.
    可继续的错误会被转换为警告 (通过使用 EXT:APPEASE-CERRORS).
    不可继续的错误和 Control+C 中断会以错误状态 (通过 EXT:EXIT-ON-ERROR)来终止这个Lisp脚本的执行.
    如果你希望你的脚本在加载前被编译, 添加-C 给 interpreter-arguments.

如果什么现象都没有. 另一种不太好的方式就是, 在文件中添加以下内容:

    #!/bin/sh
    exec clisp <<EOF
    (lisp-form)
    (another-lisp-form)
    (yet-another-lisp-form)
    EOF

这个方法的问题是每个结构的返回值都会被打印到 \*STANDARD-OUTPUT*. 另外一个问题是没有用户输入可用.

### 32.6.3 <span id = "DesktopEnvironments">桌面环境</span>

平台依赖:仅限 Win32, Gnome, KDE, Mac OS X 桌面平台.

**符号**

    虽然我们使用win32特有的符号，但是这个功能在其他桌面上也可以正常工作.

这里有两种不同的方式使 CLISP 在桌面平台下可执行.

    为 #P".mem" 扩展名关联 c:\clisp\clisp.exe -M "%s".
    为 #P".fas" 扩展名关联 c:\clisp\clisp.exe -i "%s", 你可能在你的 #P".fas" 文件中有main函数并且为 #P".fas" 扩展名关联  c:\clisp\clisp.exe -i %s -x (main).

然后点击编译后的lisp文件 (带有 #P".fas" 扩展名) 会加载这个文件 (所以会执行这个文件里的代码), 当点击 CLISP 内存镜像(带有 #P".mem" 扩展名) 会用给定的内存镜像启动 CLISP .
注意

在 Win32 上, CLISP 用文件 src/install.bat 来分配安装, 它会运行 src/install.lisp 在你的桌面上创建 clisp.lnk 并且也会为 #P".fas", #P".lisp", 还有 #P".mem" 文件关联 CLISP.

### 32.6.4 <span id = "AssociatingCLISP">通过内核来关联 CLISP 扩展名</span>
平台依赖: 仅限 Linux 平台.

你可以使用 CONFIG_BINFMT_MISC=y 还有 CONFIG_PROC_FS=y 来构建你的内核. 然后你会有一个 /proc/sys/fs/binfmt_misc/ 目录并且你可能做 (以 root 来运行; 你可能会想添加下面几行到 /etc/rc.d/rc.local):

```SHELL
# echo ":CLISP:E::fas::/usr/local/bin/clisp:" >> /proc/sys/fs/binfmt_misc/register
# echo ":CLISP:E::lisp::/usr/local/bin/clisp:" >> /proc/sys/fs/binfmt_misc/register
```

然后你可以执行以下内容:

```SHELL
$ cat << EOF > hello.lisp
(print "hello, world!")
EOF
$ clisp -c hello.lisp
;; Compiling file hello.lisp ...
;; Wrote file hello.fas
0 errors, 0 warnings
$ chmod +x hello.fas
$ hello.fas

"hello, world!"
```

请阅读 /usr/src/linux/Documentation/binfmt_misc.txt 获取详细信息.

## 32.7 <span id = "ShellPipesPrinting">Shell, 管道和打印</span>

这个章节阐述 CLISP 如何调用外部的可执行文件以及和触发的进程交互.

### 32.7.1 Shell

平台依赖:仅限 UNIX .

``(EXT:EXECUTE program arg1 arg2 ...)`` 

    执行一个外部的程序. 它的名字是 program (一个完整的路径名). 提供 STRINGs arg1, arg2, ... 作为参数给它.
    
平台依赖:仅限 UNIX, Win32 .

``(EXT:SHELL [command])`` 

    调用一个操作系统的shell, 它的值在 UNIX 上是环境变量 SHELL ，在 Win32 上是环境变量 COMSPEC . (EXT:SHELL) 发起一个shell供交互. (EXT:SHELL command) 调用shell只执行给定的 command.
    
平台依赖:仅限 UNIX, Win32 .

    函数 EXT:RUN-SHELL-COMMAND 和 EXT:RUN-PROGRAM 是对 EXT:SHELL 的通用接口，以上所述:

``(EXT:RUN-SHELL-COMMAND command &KEY :MAY-EXEC :INDIRECTP :INPUT :OUTPUT :IF-OUTPUT-EXISTS :WAIT)``

    运行一个shell命令 (包括shell内置的命令, 像 Win32 上的 DIR  和 UNIX 上的 for/do/done ).

``(EXT:RUN-PROGRAM program &KEY :MAY-EXEC :INDIRECTP :ARGUMENTS :INPUT :OUTPUT :IF-OUTPUT-EXISTS :WAIT)``

    运行一个额外的程序.

    command

        shell命令.

        平台依赖:仅限 UNIX 平台.
            这个命令传递给的shell是环境变量 SHELL 的值，正常情况下是 /bin/sh. 这个命令应该是一个 “简单命令”; 一个 “命令列表” 应该用 "{ ... ; }" (对于 /bin/sh) 或者 "( ... )" (对于 /bin/csh) 围起来.

    program
        表示程序. 环境变量 PATH 列出的目录都会被搜索到.
    :ARGUMENTS
        一个传递给程序的参数的列表 (STRINGs) .
    :INPUT
        程序的输入来自于:  :TERMINAL (stdin, 默认的) 或者 :STREAM (一个被创建的lisp  STREAM) 或者一个 pathname designator (一个输入文件) 或者 NIL (没有输入).
    :OUTPUT
        程序的输出的出口:  :TERMINAL (stdout, 默认的) 或者 :STREAM (一个被创建的lisp STREAM ) 或者一个 pathname designator (一个输入出文件) or NIL (忽略输出).
    :IF-OUTPUT-EXISTS
        如果 :OUTPUT 文件已经存在需要做的事. 这个可能的值可以是 :OVERWRITE, :APPEND, :ERROR, 和 OPEN 中的意思相同。默认是 :OVERWRITE.
    :WAIT
        是否等待程序终止 (当没有对这个程序有IO操作时这是有用的); 默认是 T, i.e., 同步地执行.
    :MAY-EXEC
        传递 exec 给深层的 shell (仅限 UNIX).
    :INDIRECTP
        使用一个shell去运行命令, e.g., (EXT:RUN-PROGRAM "dir" :indirectp T) 会运行一个shell内置的命令 DIR. 这个参数对于 EXT:RUN-SHELL-COMMAND 默认是 T ，对于 EXT:RUN-PROGRAM 默认是 NIL . (仅限 Win32).

    如果为 :INPUT 或者 :OUTPUT 指定 :STREAM , 返回一个 Lisp STREAM . 如果都没有为 :INPUT 和 :OUTPUT 指定 :STREAM , 三个Lisp STREAM 会返回, 类似函数 EXT:MAKE-PIPE-IO-STREAM. 否则, 返回的值依赖进程的终止状态: 如果以信号退出或者一个段错误,信号的数字以负整数返回, 另外, 如果它以正常的0状态码退出, NIL 被返回; 否则, 返回的状态就是一个正整数.

    这个 EXT:RUN-PROGRAM 的使用可能导致死锁, 见 EXT:MAKE-PIPE-IO-STREAM.

### 32.7.2 管道

平台依赖:仅限 UNIX, Win32 平台.

``(EXT:MAKE-PIPE-INPUT-STREAM command &KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED)``
        
    返回一个 input STREAM ，它会提供执行的操作系统命令的输出.
    
``(EXT:MAKE-PIPE-OUTPUT-STREAM command &KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED)``

    返回一个 output STREAM ，它会传递它的输出给执行的操作系统命令的输入.
    
``(EXT:MAKE-PIPE-IO-STREAM command &KEY :ELEMENT-TYPE :EXTERNAL-FORMAT :BUFFERED)``

    返回三个值. 这个 primary value 是一个 bidirectional STREAM ，它会把它的输出作为输入给执行的操作系统命令，把操作系统的输出作为它的输入. 第二个和第三个值分别是构成这个 bidirectional STREAM 的 input STREAM 和 output STREAM .
        
    警告

        这三个流必须被分别关闭, 见 CLOSE-CONSTRUCTED-STREAM:ARGUMENT-STREAM-ONLY.
        
    警告

        不适当地使用这个函数会导致死锁. 恕不负责!

        如果这个命令和你的lisp程序同时从对方读或写的时候会发生一个死锁.

        为了避免死锁, 建议在你的lisp程序和命令之间指定一个协议来避免任何隐藏的缓冲: 使用 READ-CHAR, READ-CHAR-NO-HANG, LISTEN, SOCKET:SOCKET-STATUS 而不是 READ-LINE 和 READ 来从另一边读数据, 以及通过 FINISH-OUTPUT 来完成任何的输出操作。 同样的预防措施也应用到调用的命令.

### 32.7.3 打印

这个宏 EXT:WITH-OUTPUT-TO-PRINTER:

(EXT:WITH-OUTPUT-TO-PRINTER (variable [:EXTERNAL-FORMAT])
  {declaration}*
  {form}*)

给变量 variable 绑定一个输出给 printer 的 output STREAM .

## 32.8 <span id = "OSEnvironment">操作系统环境</span>

大部分现代操作系统支持关联字符串(“variables”)到字符串(“values”)的环境变量. 这些变量在某些方面类似于 Common Lisp 中的 SPECIAL 变量: 它们的值从它们的父进程中继承下来.

你可以通过函数 (EXT:GETENV &OPTIONAL string) 访问你的操作系统环境变量, 其中的 string 是要访问的环境变量的名字. 当 string 省略了或者是 NIL, 所有的环境变量和它们的值都会以一个关联列表返回.

你可以通过 (SETF (EXT:GETENV string) new-value) 改变一个已存在的环境变量的值或者创建一个新的。
