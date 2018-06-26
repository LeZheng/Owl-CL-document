# 11. 包

> * 11.1 [包概念](#PackageConcepts)
> * 11.2 [包字典](#ThePackagesDictionary)

## 11.1 <span id="PackageConcepts">包概念</span>

> * 11.1.1 [包的介绍](#IntroductionPackages)
> * 11.1.2 [标准包](#StandardizedPackages)

### 11.1.1 <span id="IntroductionPackages">包的介绍</span>

一个包建立一个从名字到符号的映射. 在给定的任何时间, 都有一个包是当前的. 当前包是那个 \*package\* 的值的那个包. 当使用 Lisp 读取器时, 可能去引用不是当前包的包中的符号, 通过使用那个符号的打印表示中的包前缀.

下面这段列出了一些可应用于包的已定义的名字. 在一个操作符接收一个符号或一个符号列表作为参数的地方, 一个 nil 参数被当作是一个空的符号列表. 任何包参数可能是一个字符串, 一个符号, 或者一个包. 如果提供了一个符号, 它的名字会被用作这个包的名字.

    *modules*            import                     provide           
    *package*            in-package                 rename-package    
    defpackage           intern                     require           
    do-all-symbols       list-all-packages          shadow            
    do-external-symbols  make-package               shadowing-import  
    do-symbols           package-name               unexport          
    export               package-nicknames          unintern          
    find-all-symbols     package-shadowing-symbols  unuse-package     
    find-package         package-use-list           use-package       
    find-symbol          package-used-by-list                         

    Figure 11-1. 一些和包相关的已定义的名字

> * 11.1.1.1 [包名和别名](#PackageNamesNicknames)
> * 11.1.1.2 [一个包中的符号](#SymbolsInPackage)

#### 11.1.1.1 <span id="PackageNamesNicknames">包名和别名</span>

每个包都有一个名字 (agiel字符串) 并且或许有一些别名 (也是字符串). 在这个包被创建它们被赋值并且可以在后面更改.

这里有一个包的单独的命名空间. 函数 find-package 转换一个包的名字或别名为关联的包. 函数 package-name 返回一个包的名字. 函数 package-nicknames 返回一个包的所有别名的列表. rename-package 移除一个包的当前名字和别名并且用调用者指定的新的那个替换它们. 

#### 11.1.1.2 <span id="SymbolsInPackage">一个包中的符号</span>
<!--TODO present 出现??-->
> * 11.1.1.2.1 [内部和外部符号](#InternalExternalSymbols)
> * 11.1.1.2.2 [包的继承](#PackageInheritance)
> * 11.1.1.2.3 [一个包中符号的访问](#AccessSymbolsPackage)
> * 11.1.1.2.4 [查找一个包中的一个符号](#LocatingSymbolPackage)
> * 11.1.1.2.5 [包中的名字冲突的避免](#PreventionNameConflictsPackages)

##### 11.1.1.2.1 <span id="InternalExternalSymbols">内部和外部符号</span>

在一个包中的映射被分为两类, 外部的和内部的. 这些不同的映射针对的符号被称为这个包的外部符号和内部符号. 在一个包中, 一个名字引用一个符号或者没有引用; 如果它确实引用一个符号, 那么它可能是那个包中的外部符号或内部符号, 但是不可能都是. 外部符号是这个包对于其他包的公共接口部分. 如果符号被从一个给定包中导出, 那么它们就是这个包的外部符号.

一个符号不管出现在那个包中它都有着相同的名字, 但是它可能是某些包的一个外部符号, 其他包的内部符号. 

##### 11.1.1.2.2 <span id="PackageInheritance">包的继承</span>

包可以被成层构建. 从某个角度, 一个包是一个从字符串到内部符号和外部符号的映射的单个集合. 然而, 这些映射中的一部分可能在这个包自身中被建立, 而其他映射可能是从其他包中通过 use-package 继承而来. 如果一个映射是在那个包自身中的并且不是从某个其他地方继承来的, 就说一个符号出现在一个包中.

这里没有方法去继承另一个包的内部符号; 在使用 Lisp 读取器时, 为了引用一个内部符号, 一个包含了那个符号的包必须是当前包, 必须使用一个包前缀, 或者这个符号必须被导入到当前包中. 

##### 11.1.1.2.3 <span id="AccessSymbolsPackage">一个包中符号的访问</span>

如果一个包在一个符号被创建时是这个符号的 home 包, 或者它被导入到这个包中, 或者通过 use-package 继承, 那么这个符号在这个包中就是可访问的.

如果一个符号在一个包中是可访问的, 当那个包是当前包使用 Lisp 读取器时, 它可以在不带包前缀的情况下被引用, 不是它是出现在那个包中或是继承的.

来自一个包中的符号可以用两种方式使得它在另一个包中可以被访问.

-- 任何单个符号可以通过使用 import 被添加到一个包中. 在那个对 import 的调用后那个符号就出现在那个导入的包中. 在这个符号来自的包中(如果有的话)的这个符号的状况是不会被改变<!--TODO 语法不通顺-->, 并且这个符号的 home 包没有被改变. 一旦被导入, 一个符号就出现在那个导入的包中并且只能通过调用 unintern 来移除.

    如果一个符号可以访问是通过继承而不是因为出现在一个包中的另一个同名符号, 那么这个符号在那个包中会被那个符号所遮蔽. 见 shadowing-import.

-- 使一个包中的符号在另一个包中是可访问的第二种机制是通过 use-package 提供的. 那个被使用的包中的所有外部符号都被那个使用的包所继承. 函数 unuse-package 撤销一个前面的 use-package 的影响. 

##### 11.1.1.2.4 <span id="LocatingSymbolPackage">查找一个包中的一个符号</span>

当一个给定的包中的一个符号要被查找时会发生以下这些事:

-- 对这个包的外部符号和内部符号进行搜索来查找这个符号.
-- 所使用的包的外部符号会以某个未指定的顺序进行搜索. 顺序无关紧要; 见下面列出的用于处理名字冲突的规则. 

##### 11.1.1.2.5 <span id="PreventionNameConflictsPackages">包中的名字冲突的避免</span>

在一个包中, 任何独有的名字最多只能引用一个符号. 当这里有超过一个候选符号时就说发生了名字冲突. 任何一个名字冲突将要发生时, 就会发出一个可校正的错误.

以下规则应用于名字冲突:

-- 当名字冲突称为可能时它们会被检测到, 这也就是说, 在这个包结构被修改时. 在每次名字查找时名字冲突不会被检测.

-- 如果对于一个包相同的符号可以通过超过一种途径访问, 那么这里没有名字冲突. 一个符号不能和自身冲突. 名字冲突只发生在有着相同名字(在 string= 下)的不同符号之间.

-- 每个包都有一个遮蔽符号的列表. 一个遮蔽符号优先于其他在那个包中可访问的任何相同名字的符号. 一个涉及到遮蔽符号的名字冲突总是在没有发出一个错误的情况下, 以选择遮蔽符号的形式得到解决 (除了一个涉及 import 的例外情况). 见 shadow 和 shadowing-import.

-- 函数 use-package, import, 和 export 检测名字冲突.

-- shadow 和 shadowing-import 从不发出一个名字冲突的错误.

-- unuse-package 和 unexport 不需要去做任何名字冲突的检测. unintern 只有在一个被解除捕捉的符号是一个遮蔽符号时执行名字冲突检测.

-- 给 unintern 传递一个遮蔽符号可以发现一个之前通过遮蔽解决的名字冲突.

-- 包函数在对包结构做任何改动之前 Package functions signal name-conflict errors of type package-error before making any change to the package structure. 当需要进行多次更改时, 允许具体实现分别处理每一个更改. 比如, 当 export 给定一个符号的列表时, 从列表中第二个符号导致的名字冲突终止可能仍然会导出这个列表的第一个符号. 然而, 一个由导出单个符号导致的名字冲突错误会在在任何包中那个符号的访问被改变之前被发出.

-- 从一个名字冲突错误中继续必须为用户提供一个机会去选择任何一个候选符号. 包结构应该被修改, 以反映名字冲突的解决方案, 通过 shadowing-import, unintern, 或 unexport.

-- use-package 中在出现在使用包中的符号和被使用包中的外部符号之前的名字冲突, 通过使第一个符号变为遮蔽符号而选择第一个符号或者通过在使用包中解绑第一个符号而选择第二个符号来解决.

-- 在 export 或 unintern 中由于一个包从两个其他包中继承相同名字(在 string= 下)的不同符号而导致的一个名字冲突, 可以通过把任意一个符号导出到使用包中并且使它称为一个遮蔽符号来解决, 就像使用 use-package 一样. 

### 11.1.2 <span id="StandardizedPackages">标准包</span>

这个章节描述了在每个符合规范的实现中都是可用的包. 这些标准包的名字和别名的汇总在下面这段中给定.

    名称              别名  
    COMMON-LISP       CL         
    COMMON-LISP-USER  CL-USER    
    KEYWORD           none       

    Figure 11-2. 标准包名称

> * 11.1.2.1 [COMMON-LISP 包](#COMMON-LISP-Package)
> * 11.1.2.2 [COMMON-LISP-USER 包](#COMMON-LISP-USER-Package)
> * 11.1.2.3 [KEYWORD 包](#KEYWORD-Package)
> * 11.1.2.4 [依赖于具体实现的包](#Implementation-Defined-Packages)

#### 11.1.2.1 <span id="COMMON-LISP-Package">COMMON-LISP 包</span>

这个 COMMON-LISP 包包含了这个规范定义的 Common Lisp 系统的基本. 它的外部符号包含了出现在 Common Lisp 系统中的所有已定义的名字 (除了 KEYWORD 包中已定义的名字外), 例如 car, cdr, \*package\*, 等等. 这个 COMMON-LISP 有着别名 CL.

这个 COMMON-LISP 包有着在章节 1.9 (Symbols in the COMMON-LISP Package) 中枚举的那些外部符号, 并且没有其他的. 这些外部符号出现在 COMMON-LISP 包中但是它们的 home 包不需要是 COMMON-LISP 包.

比如, 符号 HELP 不能是 COMMON-LISP 包的一个外部符号因为它没有在章节 1.9 (Symbols in the COMMON-LISP Package) 中被提及. 与此相反, 符号 variable 必须是 COMMON-LISP 包的一个外部符号即便它没有定义, 因为它在那个章节中被列出来了 (为了支持它被用作给函数 documentation 的第二个有效参数).

这个 COMMON-LISP 包有着额外的内部符号.


##### 11.1.2.1.1 符合规范的具体实现的 COMMON-LISP 包的约束

在一个符合规范的具体实现中, 这个 COMMON-LISP 包的一个外部符号可以有一个函数, 宏, 或者特殊操作符定义, 一个全局变量定义 (或者其他状态就像由于一个 special 全局声明作为一个动态变量), 或者只有在这个标准中显式允许时可以是一个类型定义. 比如, fboundp 对于 COMMON-LISP 包的任何不是一个标准函数, 宏或特殊操作符的名字的外部符号产生 false, 而 boundp 对于 COMMON-LISP 包中的任何不是标准全局变量名字的外部符号返回 false. 此外, 符合规范的程序可以使用 COMMON-LISP 包的外部符号作为局部词法变量的名字, 并相信这些名字没有被具体实现声明为 special, 除非这些符号是标准全局变量的名字.

一个符合规范的具体实现一定不能在 COMMON-LISP 包的外部符号上使用一个属性指示符放置任何属性,  A conforming implementation must not place any property on an external symbol of the COMMON-LISP package using a property indicator that is either an external symbol of any standardized package or a symbol that is otherwise accessible in the COMMON-LISP-USER package. <!--TODO 待翻译-->


##### 11.1.2.1.2 符合规范的程序的 COMMON-LISP 包的约束

除了显式允许的地方, 如果在一个 COMMON-LISP 包的外部符号上执行以下任何动作, 后果都是未定义的:

1. 绑定或修改它的值 (词法地或动态地). (下面记录一些异常情况.)

2. 定义, 解除定义, 或者绑定它为一个函数. (下面记录一些异常情况.)

3. 定义, 解除定义, 或绑定它为一个宏或编译器宏. (下面记录一些异常情况.)

4. 定义它为一个类型特化符 (通过 defstruct, defclass, deftype, define-condition).

5. 定义它为一个结构体 (通过 defstruct).

6. 使用 declaration 全局声明定义它为一个声明.

7. 定义它为一个符号宏.

8. 修改它的 home 包.

9. 追踪它 (通过 trace).

10. 声明或全局声明它为 special (通过 declare, declaim, 或 proclaim).

11. 声明或全局声明它的 type 或 ftype (通过 declare, declaim, or proclaim). (下面记录一些异常情况.)

12. 从 COMMON-LISP 包中移除它.

13. 为它定义一个 setf 展开 (提供 defsetf 或 define-setf-method).

14. 定义, 解除定义, 或绑定它的 setf 函数名.

15. 定义它为一个方法组合类型 (通过 define-method-combination).

16. 使用它作为给 find-class 的 setf 的 class-name 参数.

17. 绑定它为一个捕捉标记.

18. 绑定它为一个重启动名字.

19. 当左右参数都是标准化类的直接实例时, 为一个可应用的标准广义函数定义一个方法.


###### 11.1.2.1.2.1 符合规范的程序的 COMMON-LISP 包的约束的一些异常

如果 COMMON-LISP 包的一个外部符号没有被全局定义为一个标准动态变量或常变量, 那么允许词法上绑定它并且声明那个绑定的类型, 并且允许去局部地建立它为一个符号宏 (比如, 使用 symbol-macrolet).

除非显式指定, 否则如果一个 COMMON-LISP 包的外部符号被全局定义为一个标准化动态变量, 允许去绑定或分配那个提供的动态变量, 前提是保持这个动态变量的 "值类型(Value Type)" 约束, 并且那个变量的新值和该变量声明的目的一致.

如果一个 COMMON-LISP 包的外部符号没有被定义为一个标准化函数, 宏, 或特殊操作符, 允许词法上绑定它为一个函数 (比如, 使用 flet), 去声明那个绑定的 ftype, 并且 (在提供了执行这个的能力的具体实现中) 去追踪那个绑定.

如果一个 COMMON-LISP 包的外部符号没有被定义为一个标准化函数, 宏, 或特殊操作符, 允许去词法上绑定它为一个宏 (比如, 使用 macrolet).

如果一个 COMMON-LISP 包的外部符号没有被定义为一个标准化函数, 宏, 或特殊操作符, 允许去词法绑定它的 setf 函数名为一个函数, 并且去声明那个绑定的 ftype. 


#### 11.1.2.2 <span id="COMMON-LISP-USER-Package">COMMON-LISP-USER 包</span>

在 Common Lisp 系统启动时, 这个 COMMON-LISP-USER 包是当前包. 这个包使用了 COMMON-LISP 包. 这个 COMMON-LISP-USER 包有着别名 CL-USER. 这个 COMMON-LISP-USER 包可以有捕捉在它内部的额外符号; 它可以使用其他具体实现定义的包. 


#### 11.1.2.3 <span id="KEYWORD-Package">KEYWORD 包</span>

这个 KEYWORD 包包含的称之为关键字的符号, 通常被用作程序以及它们关联的数据表达式中的特殊标记.

以一个包标记开始的符号 token被 Lisp 读取器解析为 KEYWORD 包中的符号; 见章节 2.3.4 (Symbols as Tokens). 这使得在不同包间的程序之间进行通信时使用关键字变得方便. 比如, 在调用中传递关键字参数的机制使用关键字来命名相应的参数; 见章节 3.4.1 (Ordinary Lambda Lists).

在 KEYWORD 包中的符号按照定义是 keyword 类型的.


##### 11.1.2.3.1 在 KEYWORD 包中捕捉一个符号

KEYWORD 包的处理方式与其他包不同, 因为在其中插入一个符号时, 会采取特殊的操作. 特别地, 当一个符号被捕捉到 KEYWORD 包中时, 它自动成为一个外部符号并且自动成为一个以它自身作为值的常变量. 

##### 11.1.2.3.2 KEYWORD 包的注意点

一般情况下, 最好将关键字的使用限制在存在要被选择的有限可枚举的名称集合的情况下. 比如, 如果一个亮度切换有两个状态, 它们可能被称作 :on 和 :off.

在这个名称集合不是有限可枚举的情况下 (也就是说, 可能发生名字冲突的情况下) 最好去使用某个不是 KEYWORD 的包中的符号, 这样冲突自然会被避免. 比如, 一个程序去使用一个关键字作为属性指示符通常是不明智的, 因为如果这里有另一个程序做了相同的事, 每一个都会把另一个的数据重写掉. 


#### 11.1.2.4 <span id="Implementation-Defined-Packages">依赖于具体实现的包</span>

其他的, 具体实现定义的包可能出现在最初的 Common Lisp 环境中.

一个符合规范的具体实现的文档建议但不是必须去包含一个最初出现在那个具体实现但没有在这个规范中指定的所有包名的列表. (也见函数 list-all-packages.) 


## 11.2 <span id="ThePackagesDictionary">包字典</span>

> * [系统类 PACKAGE](#SC-PACKAGE)
> * [函数 EXPORT](#F-EXPORT)
> * [函数 FIND-SYMBOL](#F-FIND-SYMBOL)
> * [函数 FIND-PACKAGE](#F-FIND-PACKAGE)
> * [函数 FIND-ALL-SYMBOLS](#F-FIND-ALL-SYMBOLS)
> * [函数 IMPORT](#F-IMPORT)
> * [函数 LIST-ALL-PACKAGES](#F-LIST-ALL-PACKAGES)
> * [函数 RENAME-PACKAGE](#F-RENAME-PACKAGE)
> * [函数 SHADOW](#F-SHADOW)
> * [函数 SHADOWING-IMPORT](#F-SHADOWING-IMPORT)
> * [函数 DELETE-PACKAGE](#F-DELETE-PACKAGE)
> * [函数 MAKE-PACKAGE](#F-MAKE-PACKAGE)
> * [宏 WITH-PACKAGE-ITERATOR](#M-WITH-PACKAGE-ITERATOR)
> * [函数 UNEXPORT](#F-UNEXPORT)
> * [函数 UNINTERN](#F-UNINTERN)
> * [宏 IN-PACKAGE](#M-IN-PACKAGE)
> * [函数 UNUSE-PACKAGE](#F-UNUSE-PACKAGE)
> * [函数 USE-PACKAGE](#F-USE-PACKAGE)
> * [宏 DEFPACKAGE](#M-DEFPACKAGE)
> * [宏 DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS](#M-DS-DES-DAS)
> * [函数 INTERN](#F-INTERN)
> * [函数 PACKAGE-NAME](#F-PACKAGE-NAME)
> * [函数 PACKAGE-NICKNAMES](#F-PACKAGE-NICKNAMES)
> * [函数 PACKAGE-SHADOWING-SYMBOLS](#F-PACKAGE-SHADOWING-SYMBOLS)
> * [函数 PACKAGE-USE-LIST](#F-PACKAGE-USE-LIST)
> * [函数 PACKAGE-USED-BY-LIST](#F-PACKAGE-USED-BY-LIST)
> * [函数 PACKAGEP](#F-PACKAGEP)
> * [变量 *PACKAGE*](#V-PACKAGE)
> * [状况类型 PACKAGE-ERROR](#CT-PACKAGE-ERROR)
> * [函数 PACKAGE-ERROR-PACKAGE](#F-PACKAGE-ERROR-PACKAGE)


### <span id="SC-PACKAGE">系统类 PACKAGE</span>

* 类优先级列表(Class Precedence List):

        package, t

* 描述(Description):

        一个包是一个映射符号名到符号的命名空间; 见章节 11.1 (Package Concepts).

* 也见(See Also):

        章节 11.1 (Package Concepts), 章节 22.1.3.13 (Printing Other Objects), 章节 2.3.4 (Symbols as Tokens) 


### <span id="F-EXPORT">函数 EXPORT</span>

* 语法(Syntax):

        export symbols &optional package => t

* 参数和值(Arguments and Values):

        symbols---一个符号列表的标识符.
        package---一个包标识符. 默认是当前包.

* 描述(Description):

        export 使一个或多个在这个包 package 中可访问的符号 (whether directly or by inheritance) 变为包 package 的外部符号.

        如果这些符号 symbols 中的任何一个已经是包 package 中可访问的外部符号, export 在那个符号上没有效果. 如果那个符号 symbol 是作为一个内部符号出现在包 package 中, 它就被简单地改为外部状态. 如果它是通过 use-package 作为一个可访问的内部符号, 它首先被导入到包 package 中, 然后再导出. (不管接下来包 package 是否继续使用那个符号最初继承而来的包, 这个符号 symbol 接下来都会出现在包 package 中.)

        export 使得每个符号 symbol 对于所有使用包 package 的包都是可访问的. 所有这些包都会检测命名冲突: (export s p) 为每一个在 (package-used-by-list p) 中的包执行 (find-symbol (symbol-name s) q). 注意, 一个包的初始化定义期间, 通常情况下的一个 export Note that in the usual case of an export during the initial definition of a package<!--TODO 待翻译-->, package-used-by-list 的结果是 nil 并且名字冲突检测需要的时间忽略不计. 当执行了多次更改时, 比如当给 export 一个符号列表时, 允许具体实现去单独处理每一个更改, 这样一来跳过除了这个列表中的第一个符号以外的任何一个导致的名字冲突都不会解除导出这个列表中的第一个符号. 然而, 跳过一个有这些符号 symbols 中的其中一个导致的名字冲突错误不会让那个符号对于某些包是可访问的但对于其他是不可访问的; 对于被处理的符号 symbols 中的每一个, export 表现的就好像它是一个原子操作.

        在 export 中, 要被导出的符号 symbols 的其中一个和已经出现在一个会继承那个新导出的符号的包中的符号之间的名字冲突, 可以通过解除捕捉另一个来选择导出的符号, 或者通过使那个已存在的符号变为一个遮蔽符号来选择那个已存在的符号.

* 示例(Examples):

    ```LISP
    (make-package 'temp :use nil) =>  #<PACKAGE "TEMP">
    (use-package 'temp) =>  T
    (intern "TEMP-SYM" 'temp) =>  TEMP::TEMP-SYM, NIL
    (find-symbol "TEMP-SYM") =>  NIL, NIL
    (export (find-symbol "TEMP-SYM" 'temp) 'temp) =>  T
    (find-symbol "TEMP-SYM") =>  TEMP-SYM, :INHERITED
    ```

* 副作用(Side Effects):

        这个包系统被修改.

* 受此影响(Affected By):

        可访问的符号.

* 异常情况(Exceptional Situations): 

        如果符号 symbols 中的任何一个包 package 中不可访问, 会发出一个 package-error 类型的错误, 通过允许用户去交互式地指定是否应该导入该符号, 这是可校正的.

* 也见(See Also):

        import, unexport, 章节 11.1 (Package Concepts)

* 注意(Notes): None. 


### <span id="F-FIND-SYMBOL">函数 FIND-SYMBOL</span>

* 语法(Syntax):

        find-symbol string &optional package => symbol, status

* 参数和值(Arguments and Values):

        string---一个符号.
        package---一个包标识符. 默认是包标识符.
        symbol---在包 package 中可访问的一个符号, 或者 nil.
        status---:inherited, :external, :internal 其中之一, 或 nil.

* 描述(Description):

        find-symbol 查找一个包中的名为字符串 string 的符号. 如果在包 package 中找到名为字符串 string 的一个符号, 不管是直接地或是通过继承, 找到的符号会作为第一个值被返回; 第二个值如下:

        :internal

            如果这个符号是作为一个内部符号出现在包 package.

        :external

            如果这个符号是作为一个外部符号出现在包 package.

        :inherited

            如果这个符号是被包 package 通过 use-package 继承而来, 但是不是出现在包 package.

        如果在包 package 中没有这样的符号, 两个值都是 nil.

* 示例(Examples):

    ```LISP
    (find-symbol "NEVER-BEFORE-USED") =>  NIL, NIL
    (find-symbol "NEVER-BEFORE-USED") =>  NIL, NIL
    (intern "NEVER-BEFORE-USED") =>  NEVER-BEFORE-USED, NIL
    (intern "NEVER-BEFORE-USED") =>  NEVER-BEFORE-USED, :INTERNAL
    (find-symbol "NEVER-BEFORE-USED") =>  NEVER-BEFORE-USED, :INTERNAL
    (find-symbol "never-before-used") =>  NIL, NIL
    (find-symbol "CAR" 'common-lisp-user) =>  CAR, :INHERITED
    (find-symbol "CAR" 'common-lisp) =>  CAR, :EXTERNAL
    (find-symbol "NIL" 'common-lisp-user) =>  NIL, :INHERITED
    (find-symbol "NIL" 'common-lisp) =>  NIL, :EXTERNAL
    (find-symbol "NIL" (prog1 (make-package "JUST-TESTING" :use '())
                              (intern "NIL" "JUST-TESTING")))
    =>  JUST-TESTING::NIL, :INTERNAL
    (export 'just-testing::nil 'just-testing)
    (find-symbol "NIL" 'just-testing) =>  JUST-TESTING:NIL, :EXTERNAL
    (find-symbol "NIL" "KEYWORD")
    =>  NIL, NIL
    OR=>  :NIL, :EXTERNAL
    (find-symbol (symbol-name :nil) "KEYWORD") =>  :NIL, :EXTERNAL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        intern, import, export, use-package, unintern, unexport, unuse-package

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        intern, find-all-symbols

* 注意(Notes):

        find-symbol 操作上等价于 intern, 除了它从不创建一个新的符号. 


### <span id="F-FIND-PACKAGE">函数 FIND-PACKAGE</span>

* 语法(Syntax):

        find-package name => package

* 参数和值(Arguments and Values):

        name---一个字符串标识符或者一个包对象.
        package---一个包对象或者 nil.

* 描述(Description):

        如果名字 name 是一个字符串标识符, find-package 查找并返回这个名字或别名为 name 的包. 这个查找是大小写敏感的. 如果这里没有这样的包, find-package 返回 nil.

        如果 name 是一个包对象, 那么就返回那个包对象.

* 示例(Examples):

    ```LISP
    (find-package 'common-lisp) =>  #<PACKAGE "COMMON-LISP">
    (find-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (find-package 'not-there) =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        这个具体实现创建的包的集合.

        defpackage, delete-package, make-package, rename-package

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        make-package

* 注意(Notes): None. 


### <span id="F-FIND-ALL-SYMBOLS">函数 FIND-ALL-SYMBOLS</span>

* 语法(Syntax):

        find-all-symbols string => symbols

* 参数和值(Arguments and Values):

        string---一个字符串标识符.
        symbols---一个符号列表.

* 描述(Description):

        find-all-symbols 在每一个已注册的包中搜索有着名字和字符串 string 相同(在 string= 下)的符号. 一个所有这样的符号列表会被返回. 这个列表是否排序或如何排序是依赖于具体实现的.

* 示例(Examples):

    ```LISP
    (find-all-symbols 'car)
    =>  (CAR)
    OR=>  (CAR VEHICLES:CAR)
    OR=>  (VEHICLES:CAR CAR)
    (intern "CAR" (make-package 'temp :use nil)) =>  TEMP::CAR, NIL
    (find-all-symbols 'car)
    =>  (TEMP::CAR CAR)
    OR=>  (CAR TEMP::CAR)
    OR=>  (TEMP::CAR CAR VEHICLES:CAR)
    OR=>  (CAR TEMP::CAR VEHICLES:CAR)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        find-symbol

* 注意(Notes): None. 


### <span id="F-IMPORT">函数 IMPORT</span>

* 语法(Syntax):

        import symbols &optional package => t

* 参数和值(Arguments and Values):

        symbols---一个符号列表的标识符.
        package---一个包标识符. 默认是当前包.

* 描述(Description):

        import 添加 symbol 或 symbols 包 package 的内部, 和包 package 中出现的或者可访问的已存在符号检查名字冲突. 一旦这些符号 symbols 已经被导入, 在使用 Lisp 读取器时, 它们可能在导入的包中被直接引用而不使用包前缀.

        在 import 中, 这个要被导入的符号 symbol 和从某个其他包中继承而来的符号之间的名字冲突, 可以通过是这个要被导入的符号为遮蔽符号来选择这个要被导入的符号, 或者不执行这个 import 来选择已经可访问的那个符号来解决. 在 import 中, 一个和一个已经出现在包 package 的符号之间的名字冲突可以通过解除捕捉那个符号或者不执行这个 import 来解决.

        这个导入的符号不会被自动从这个当前包中导出, 但是如果它已经出现在包这栋并且是外部的, 那么它是外部的这个事实不会被改变. 如果要被导入的任何符号没有 home 包 (也就是说, (symbol-package symbol) => nil), import 会设置这个符号 symbol 的 home 包为 包 package.

        如果这个符号 symbol 已经出现在这个导入的包中, import 就没有效果.

* 示例(Examples):

    ```LISP
    (import 'common-lisp::car (make-package 'temp :use nil)) =>  T
    (find-symbol "CAR" 'temp) =>  CAR, :INTERNAL
    (find-symbol "CDR" 'temp) =>  NIL, NIL 
    ```

        表达式形式 (import 'editor:buffer) 接受 EDITOR 包中名为 buffer 的外部符号 (当这个表达式形式被 Lisp 读取器读取时这个符号会被查找) 并且添加它到当前包中作为一个内部符号. 然后这个符号 buffer 就会出现在当前包中.

* 副作用(Side Effects):

        这个包系统会被修改.

* 受此影响(Affected By):

        这个包系统的当前状态.

* 异常情况(Exceptional Situations): 

        如果这些要被导入的符号 symbols 中的任何一个有着和包 package 中某个已经可以访问的不同(在 eql 下)符号相同的名字(在 string= 下), 那么 import 机会发出一个 package-error 类型的可校正错误, 即便这个冲突是和一个包 package 中的一个遮蔽符号.

* 也见(See Also):

        shadow, export

* 注意(Notes): None. 


### <span id="F-LIST-ALL-PACKAGES">函数 LIST-ALL-PACKAGES</span>

* 语法(Syntax):

        list-all-packages <no arguments> => packages

* 参数和值(Arguments and Values):

        packages---一个包对象列表.

* 描述(Description):

        list-all-packages 返回一个所有已注册包的新列表.

* 示例(Examples):

    ```LISP
    (let ((before (list-all-packages)))
        (make-package 'temp)
        (set-difference (list-all-packages) before)) =>  (#<PACKAGE "TEMP">)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        defpackage, delete-package, make-package

* 异常情况(Exceptional Situations):  None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-RENAME-PACKAGE">函数 RENAME-PACKAGE</span>

* 语法(Syntax):

        rename-package package new-name &optional new-nicknames => package-object

* 参数和值(Arguments and Values):

        package---一个包标识符.
        new-name---一个包标识符.
        new-nicknames---一个字符串标识符列表. 默认是空列表.
        package-object---重命名的包对象.

* 描述(Description):

        替换包 package 的名字和别名. 包 package 的旧名字和所有旧别名会被消除并且被替换为 new-name 和 new-nicknames.

        如果 new-name 或 any new-nickname 和任何已存在包名冲突, 那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (make-package 'temporary :nicknames '("TEMP")) =>  #<PACKAGE "TEMPORARY">
    (rename-package 'temp 'ephemeral) =>  #<PACKAGE "EPHEMERAL">
    (package-nicknames (find-package 'ephemeral)) =>  ()
    (find-package 'temporary) =>  NIL
    (rename-package 'ephemeral 'temporary '(temp fleeting))
    =>  #<PACKAGE "TEMPORARY">
    (package-nicknames (find-package 'temp)) =>  ("TEMP" "FLEETING")
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        make-package

* 注意(Notes): None. 


### <span id="F-SHADOW">函数 SHADOW</span>

* 语法(Syntax):

shadow symbol-names &optional package => t

* 参数和值(Arguments and Values):

symbol-names---a designator for a list of string designators.

package---a package designator. The default is the current package.

* 描述(Description):

shadow assures that symbols with names given by symbol-names are present in the package.

Specifically, package is searched for symbols with the names supplied by symbol-names. For each such name, if a corresponding symbol is not present in package (directly, not by inheritance), then a corresponding symbol is created with that name, and inserted into package as an internal symbol. The corresponding symbol, whether pre-existing or newly created, is then added, if not already present, to the shadowing symbols list of package.

* 示例(Examples):

 (package-shadowing-symbols (make-package 'temp)) =>  NIL
 (find-symbol 'car 'temp) =>  CAR, :INHERITED
 (shadow 'car 'temp) =>  T
 (find-symbol 'car 'temp) =>  TEMP::CAR, :INTERNAL
 (package-shadowing-symbols 'temp) =>  (TEMP::CAR)

 (make-package 'test-1) =>  #<PACKAGE "TEST-1">
 (intern "TEST" (find-package 'test-1)) =>  TEST-1::TEST, NIL
 (shadow 'test-1::test (find-package 'test-1)) =>  T
 (shadow 'TEST (find-package 'test-1)) =>  T
 (assert (not (null (member 'test-1::test (package-shadowing-symbols
                                            (find-package 'test-1))))))
 
 (make-package 'test-2) =>  #<PACKAGE "TEST-2">
 (intern "TEST" (find-package 'test-2)) =>  TEST-2::TEST, NIL
 (export 'test-2::test (find-package 'test-2)) =>  T
 (use-package 'test-2 (find-package 'test-1))    ;should not error
 

* 副作用(Side Effects):

shadow changes the state of the package system in such a way that the package consistency rules do not hold across the change.

* 受此影响(Affected By):

Current state of the package system.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

package-shadowing-symbols, Section 11.1 (Package Concepts)

* 注意(Notes):

If a symbol with a name in symbol-names already exists in package, but by inheritance, the inherited symbol becomes shadowed[3] by a newly created internal symbol. 


### <span id="F-SHADOWING-IMPORT">函数 SHADOWING-IMPORT</span>

* 语法(Syntax):

shadowing-import symbols &optional package => t

* 参数和值(Arguments and Values):

symbols---a designator for a list of symbols.

package ---a package designator. The default is the current package.

* 描述(Description):

shadowing-import is like import, but it does not signal an error even if the importation of a symbol would shadow some symbol already accessible in package.

shadowing-import inserts each of symbols into package as an internal symbol, regardless of whether another symbol of the same name is shadowed by this action. If a different symbol of the same name is already present in package, that symbol is first uninterned from package. The new symbol is added to package's shadowing-symbols list.

shadowing-import does name-conflict checking to the extent that it checks whether a distinct existing symbol with the same name is accessible; if so, it is shadowed by the new symbol, which implies that it must be uninterned if it was present in package.

* 示例(Examples):

 (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
 (setq sym (intern "CONFLICT")) =>  CONFLICT
 (intern "CONFLICT" (make-package 'temp)) =>  TEMP::CONFLICT, NIL
 (package-shadowing-symbols 'temp) =>  NIL
 (shadowing-import sym 'temp) =>  T 
 (package-shadowing-symbols 'temp) =>  (CONFLICT)

* 副作用(Side Effects):

shadowing-import changes the state of the package system in such a way that the consistency rules do not hold across the change.

package's shadowing-symbols list is modified.

* 受此影响(Affected By):

Current state of the package system.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

import, unintern, package-shadowing-symbols

* 注意(Notes): None. 


### <span id="F-DELETE-PACKAGE">函数 DELETE-PACKAGE</span>

* 语法(Syntax):

delete-package package => generalized-boolean

* 参数和值(Arguments and Values):

package---a package designator.

generalized-boolean---a generalized boolean.

* 描述(Description):

delete-package deletes package from all package system data structures. If the operation is successful, delete-package returns true, otherwise nil. The effect of delete-package is that the name and nicknames of package cease to be recognized package names. The package object is still a package (i.e., packagep is true of it) but package-name returns nil. The consequences of deleting the COMMON-LISP package or the KEYWORD package are undefined. The consequences of invoking any other package operation on package once it has been deleted are unspecified. In particular, the consequences of invoking find-symbol, intern and other functions that look for a symbol name in a package are unspecified if they are called with *package* bound to the deleted package or with the deleted package as an argument.

If package is a package object that has already been deleted, delete-package immediately returns nil.

After this operation completes, the home package of any symbol whose home package had previously been package is implementation-dependent. Except for this, symbols accessible in package are not modified in any other way; symbols whose home package is not package remain unchanged.

* 示例(Examples):

 (setq *foo-package* (make-package "FOO" :use nil))
 (setq *foo-symbol*  (intern "FOO" *foo-package*))
 (export *foo-symbol* *foo-package*)

 (setq *bar-package* (make-package "BAR" :use '("FOO")))
 (setq *bar-symbol*  (intern "BAR" *bar-package*))
 (export *foo-symbol* *bar-package*)
 (export *bar-symbol* *bar-package*)

 (setq *baz-package* (make-package "BAZ" :use '("BAR")))

 (symbol-package *foo-symbol*) =>  #<PACKAGE "FOO">
 (symbol-package *bar-symbol*) =>  #<PACKAGE "BAR">

 (prin1-to-string *foo-symbol*) =>  "FOO:FOO"
 (prin1-to-string *bar-symbol*) =>  "BAR:BAR"

 (find-symbol "FOO" *bar-package*) =>  FOO:FOO, :EXTERNAL

 (find-symbol "FOO" *baz-package*) =>  FOO:FOO, :INHERITED
 (find-symbol "BAR" *baz-package*) =>  BAR:BAR, :INHERITED

 (packagep *foo-package*) =>  true
 (packagep *bar-package*) =>  true
 (packagep *baz-package*) =>  true

 (package-name *foo-package*) =>  "FOO"
 (package-name *bar-package*) =>  "BAR"
 (package-name *baz-package*) =>  "BAZ"

 (package-use-list *foo-package*) =>  ()
 (package-use-list *bar-package*) =>  (#<PACKAGE "FOO">)
 (package-use-list *baz-package*) =>  (#<PACKAGE "BAR">)

 (package-used-by-list *foo-package*) =>  (#<PACKAGE "BAR">)
 (package-used-by-list *bar-package*) =>  (#<PACKAGE "BAZ">)
 (package-used-by-list *baz-package*) =>  ()

 (delete-package *bar-package*)
>>  Error: Package BAZ uses package BAR.
>>  If continued, BAZ will be made to unuse-package BAR,
>>  and then BAR will be deleted.
>>  Type :CONTINUE to continue.
>>  Debug> :CONTINUE
=>  T

 (symbol-package *foo-symbol*) =>  #<PACKAGE "FOO">
 (symbol-package *bar-symbol*) is unspecified

 (prin1-to-string *foo-symbol*) =>  "FOO:FOO"
 (prin1-to-string *bar-symbol*) is unspecified

 (find-symbol "FOO" *bar-package*) is unspecified

 (find-symbol "FOO" *baz-package*) =>  NIL, NIL
 (find-symbol "BAR" *baz-package*) =>  NIL, NIL

 (packagep *foo-package*) =>  T
 (packagep *bar-package*) =>  T
 (packagep *baz-package*) =>  T

 (package-name *foo-package*) =>  "FOO"
 (package-name *bar-package*) =>  NIL
 (package-name *baz-package*) =>  "BAZ"

 (package-use-list *foo-package*) =>  ()
 (package-use-list *bar-package*) is unspecified
 (package-use-list *baz-package*) =>  ()

 (package-used-by-list *foo-package*) =>  ()
 (package-used-by-list *bar-package*) is unspecified
 (package-used-by-list *baz-package*) =>  ()

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

If the package designator is a name that does not currently name a package, a correctable error of type package-error is signaled. If correction is attempted, no deletion action is attempted; instead, delete-package immediately returns nil.

If package is used by other packages, a correctable error of type package-error is signaled. If correction is attempted, unuse-package is effectively called to remove any dependencies, causing package's external symbols to cease being accessible to those packages that use package. delete-package then deletes package just as it would have had there been no packages that used it.

* 也见(See Also):

unuse-package

* 注意(Notes): None. 


### <span id="F-MAKE-PACKAGE">函数 MAKE-PACKAGE</span>

* 语法(Syntax):

make-package package-name &key nicknames use => package

* 参数和值(Arguments and Values):

package-name---a string designator.

nicknames---a list of string designators. The default is the empty list.

use---a list of package designators. The default is implementation-defined.

package---a package.

* 描述(Description):

Creates a new package with the name package-name.

Nicknames are additional names which may be used to refer to the new package.

use specifies zero or more packages the external symbols of which are to be inherited by the new package. See the function use-package.

* 示例(Examples):

 (make-package 'temporary :nicknames '("TEMP" "temp")) =>  #<PACKAGE "TEMPORARY">
 (make-package "OWNER" :use '("temp")) =>  #<PACKAGE "OWNER">
 (package-used-by-list 'temp) =>  (#<PACKAGE "OWNER">)
 (package-use-list 'owner) =>  (#<PACKAGE "TEMPORARY">)

* 副作用(Side Effects): None.

* 受此影响(Affected By):

The existence of other packages in the system.

* 异常情况(Exceptional Situations): 

The consequences are unspecified if packages denoted by use do not exist.

A correctable error is signaled if the package-name or any of the nicknames is already the name or nickname of an existing package.

* 也见(See Also):

defpackage, use-package

* 注意(Notes):

In situations where the packages to be used contain symbols which would conflict, it is necessary to first create the package with :use '(), then to use shadow or shadowing-import to address the conflicts, and then after that to use use-package once the conflicts have been addressed.

When packages are being created as part of the static definition of a program rather than dynamically by the program, it is generally considered more stylistically appropriate to use defpackage rather than make-package. 


### <span id="M-WITH-PACKAGE-ITERATOR">宏 WITH-PACKAGE-ITERATOR</span>

* 语法(Syntax):

with-package-iterator (name package-list-form &rest symbol-types) declaration* form*

=> result*

* 参数和值(Arguments and Values):

name---a symbol.

package-list-form---a form; evaluated once to produce a package-list.

package-list---a designator for a list of package designators.

symbol-type---one of the symbols :internal, :external, or :inherited.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values of the forms.

* 描述(Description):

Within the lexical scope of the body forms, the name is defined via macrolet such that successive invocations of (name) will return the symbols, one by one, from the packages in package-list.

It is unspecified whether symbols inherited from multiple packages are returned more than once. The order of symbols returned does not necessarily reflect the order of packages in package-list. When package-list has more than one element, it is unspecified whether duplicate symbols are returned once or more than once.

Symbol-types controls which symbols that are accessible in a package are returned as follows:

:internal

    The symbols that are present in the package, but that are not exported.

:external

    The symbols that are present in the package and are exported.

:inherited

    The symbols that are exported by used packages and that are not shadowed.

When more than one argument is supplied for symbol-types, a symbol is returned if its accessibility matches any one of the symbol-types supplied. Implementations may extend this syntax by recognizing additional symbol accessibility types.

An invocation of (name) returns four values as follows:

1. A flag that indicates whether a symbol is returned (true means that a symbol is returned).
2. A symbol that is accessible in one the indicated packages.
3. The accessibility type for that symbol; i.e., one of the symbols :internal, :external, or :inherited.
4. The package from which the symbol was obtained. The package is one of the packages present or named in package-list.

After all symbols have been returned by successive invocations of (name), then only one value is returned, namely nil.

The meaning of the second, third, and fourth values is that the returned symbol is accessible in the returned package in the way indicated by the second return value as follows:

:internal

    Means present and not exported.

:external

    Means present and exported.

:inherited

    Means not present (thus not shadowed) but inherited from some used package.

It is unspecified what happens if any of the implicit interior state of an iteration is returned outside the dynamic extent of the with-package-iterator form such as by returning some closure over the invocation form.

Any number of invocations of with-package-iterator can be nested, and the body of the innermost one can invoke all of the locally established macros, provided all those macros have distinct names.

* 示例(Examples):

The following function should return t on any package, and signal an error if the usage of with-package-iterator does not agree with the corresponding usage of do-symbols.

 (defun test-package-iterator (package)
   (unless (packagep package)
     (setq package (find-package package)))
   (let ((all-entries '())
         (generated-entries '()))
     (do-symbols (x package) 
       (multiple-value-bind (symbol accessibility) 
           (find-symbol (symbol-name x) package)
         (push (list symbol accessibility) all-entries)))
     (with-package-iterator (generator-fn package 
                             :internal :external :inherited)
       (loop     
         (multiple-value-bind (more? symbol accessibility pkg)
             (generator-fn)
           (unless more? (return))
           (let ((l (multiple-value-list (find-symbol (symbol-name symbol) 
                                                      package))))
             (unless (equal l (list symbol accessibility))
               (error "Symbol ~S not found as ~S in package ~A [~S]"
                      symbol accessibility (package-name package) l))
             (push l generated-entries)))))
     (unless (and (subsetp all-entries generated-entries :test #'equal)
                  (subsetp generated-entries all-entries :test #'equal))
      (error "Generated entries and Do-Symbols entries don't correspond"))
     t))

The following function prints out every present symbol (possibly more than once):

 (defun print-all-symbols () 
   (with-package-iterator (next-symbol (list-all-packages)
                           :internal :external)
     (loop
       (multiple-value-bind (more? symbol) (next-symbol)
         (if more? 
            (print symbol)
            (return))))))

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

with-package-iterator signals an error of type program-error if no symbol-types are supplied or if a symbol-type is not recognized by the implementation is supplied.

The consequences are undefined if the local function named name established by with-package-iterator is called after it has returned false as its primary value.

* 也见(See Also):

Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-UNEXPORT">函数 UNEXPORT</span>

* 语法(Syntax):

unexport symbols &optional package => t

* 参数和值(Arguments and Values):

symbols---a designator for a list of symbols.

package---a package designator. The default is the current package.

* 描述(Description):

unexport reverts external symbols in package to internal status; it undoes the effect of export.

unexport works only on symbols present in package, switching them back to internal status. If unexport is given a symbol that is already accessible as an internal symbol in package, it does nothing.

* 示例(Examples):

 (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
 (export (intern "CONTRABAND" (make-package 'temp)) 'temp) =>  T
 (find-symbol "CONTRABAND") =>  NIL, NIL 
 (use-package 'temp) =>  T 
 (find-symbol "CONTRABAND") =>  CONTRABAND, :INHERITED
 (unexport 'contraband 'temp) =>  T
 (find-symbol "CONTRABAND") =>  NIL, NIL

* 副作用(Side Effects):

Package system is modified.

* 受此影响(Affected By):

Current state of the package system.

* 异常情况(Exceptional Situations): 

If unexport is given a symbol not accessible in package at all, an error of type package-error is signaled.

The consequences are undefined if package is the KEYWORD package or the COMMON-LISP package.

* 也见(See Also):

export, Section 11.1 (Package Concepts)

* 注意(Notes): None. 


### <span id="F-UNINTERN">函数 UNINTERN</span>

* 语法(Syntax):

unintern symbol &optional package => generalized-boolean

* 参数和值(Arguments and Values):

symbol---a symbol.

package---a package designator. The default is the current package.

generalized-boolean---a generalized boolean.

* 描述(Description):

unintern removes symbol from package. If symbol is present in package, it is removed from package and also from package's shadowing symbols list if it is present there. If package is the home package for symbol, symbol is made to have no home package. Symbol may continue to be accessible in package by inheritance.

Use of unintern can result in a symbol that has no recorded home package, but that in fact is accessible in some package. Common Lisp does not check for this pathological case, and such symbols are always printed preceded by #:.

unintern returns true if it removes symbol, and nil otherwise.

* 示例(Examples):

 (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
 (setq temps-unpack (intern "UNPACK" (make-package 'temp))) =>  TEMP::UNPACK 
 (unintern temps-unpack 'temp) =>  T
 (find-symbol "UNPACK" 'temp) =>  NIL, NIL 
 temps-unpack =>  #:UNPACK 

* 副作用(Side Effects):

unintern changes the state of the package system in such a way that the consistency rules do not hold across the change.

* 受此影响(Affected By):

Current state of the package system.

* 异常情况(Exceptional Situations): 

Giving a shadowing symbol to unintern can uncover a name conflict that had previously been resolved by the shadowing. If package A uses packages B and C, A contains a shadowing symbol x, and B and C each contain external symbols named x, then removing the shadowing symbol x from A will reveal a name conflict between b:x and c:x if those two symbols are distinct. In this case unintern will signal an error.

* 也见(See Also):

Section 11.1 (Package Concepts)

* 注意(Notes): None. 


### <span id="M-IN-PACKAGE">宏 IN-PACKAGE</span>

* 语法(Syntax):

in-package name => package

* 参数和值(Arguments and Values):

name---a string designator; not evaluated.

package---the package named by name.

* 描述(Description):

Causes the the package named by name to become the current package---that is, the value of *package*. If no such package already exists, an error of type package-error is signaled.

Everything in-package does is also performed at compile time if the call appears as a top level form.

* 示例(Examples): None.

* 副作用(Side Effects):

The variable *package* is assigned. If the in-package form is a top level form, this assignment also occurs at compile time.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

An error of type package-error is signaled if the specified package does not exist.

* 也见(See Also):

*package*

* 注意(Notes): None. 


### <span id="F-UNUSE-PACKAGE">函数 UNUSE-PACKAGE</span>

* 语法(Syntax):

unuse-package packages-to-unuse &optional package => t

* 参数和值(Arguments and Values):

packages-to-unuse---a designator for a list of package designators.

package---a package designator. The default is the current package.

* 描述(Description):

unuse-package causes package to cease inheriting all the external symbols of packages-to-unuse; unuse-package undoes the effects of use-package. The packages-to-unuse are removed from the use list of package.

Any symbols that have been imported into package continue to be present in package.

* 示例(Examples):

 (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
 (export (intern "SHOES" (make-package 'temp)) 'temp) =>  T
 (find-symbol "SHOES") =>  NIL, NIL
 (use-package 'temp) =>  T
 (find-symbol "SHOES") =>  SHOES, :INHERITED
 (find (find-package 'temp) (package-use-list 'common-lisp-user)) =>  #<PACKAGE "TEMP">
 (unuse-package 'temp) =>  T
 (find-symbol "SHOES") =>  NIL, NIL

* 副作用(Side Effects):

The use list of package is modified.

* 受此影响(Affected By):

Current state of the package system.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

use-package, package-use-list

* 注意(Notes): None. 


### <span id="F-USE-PACKAGE">函数 USE-PACKAGE</span>

* 语法(Syntax):

use-package packages-to-use &optional package => t

* 参数和值(Arguments and Values):

packages-to-use---a designator for a list of package designators. The KEYWORD package may not be supplied.

package---a package designator. The default is the current package. The package cannot be the KEYWORD package.

* 描述(Description):

use-package causes package to inherit all the external symbols of packages-to-use. The inherited symbols become accessible as internal symbols of package.

Packages-to-use are added to the use list of package if they are not there already. All external symbols in packages-to-use become accessible in package as internal symbols. use-package does not cause any new symbols to be present in package but only makes them accessible by inheritance.

use-package checks for name conflicts between the newly imported symbols and those already accessible in package. A name conflict in use-package between two external symbols inherited by package from packages-to-use may be resolved in favor of either symbol by importing one of them into package and making it a shadowing symbol.

* 示例(Examples):

 (export (intern "LAND-FILL" (make-package 'trash)) 'trash) =>  T
 (find-symbol "LAND-FILL" (make-package 'temp)) =>  NIL, NIL
 (package-use-list 'temp) =>  (#<PACKAGE "TEMP">)
 (use-package 'trash 'temp) =>  T
 (package-use-list 'temp) =>  (#<PACKAGE "TEMP"> #<PACKAGE "TRASH">)
 (find-symbol "LAND-FILL" 'temp) =>  TRASH:LAND-FILL, :INHERITED

* 副作用(Side Effects):

The use list of package may be modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

unuse-package, package-use-list, Section 11.1 (Package Concepts)

* 注意(Notes):

It is permissible for a package P1 to use a package P2 even if P2 already uses P1. The using of packages is not transitive, so no problem results from the apparent circularity. 


### <span id="M-DEFPACKAGE">宏 DEFPACKAGE</span>

* 语法(Syntax):

defpackage defined-package-name [[option]] => package

option::= (:nicknames nickname*)* |  
          (:documentation string) |  
          (:use package-name*)* |  
          (:shadow {symbol-name}*)* |  
          (:shadowing-import-from package-name {symbol-name}*)* |  
          (:import-from package-name {symbol-name}*)* |  
          (:export {symbol-name}*)* |  
          (:intern {symbol-name}*)* |  
          (:size integer) 

* 参数和值(Arguments and Values):

defined-package-name---a string designator.

package-name---a package designator.

nickname---a string designator.

symbol-name---a string designator.

package---the package named package-name.

* 描述(Description):

defpackage creates a package as specified and returns the package.

If defined-package-name already refers to an existing package, the name-to-package mapping for that name is not changed. If the new definition is at variance with the current state of that package, the consequences are undefined; an implementation might choose to modify the existing package to reflect the new definition. If defined-package-name is a symbol, its name is used.

The standard options are described below.

:nicknames

    The arguments to :nicknames set the package's nicknames to the supplied names.

:documentation

    The argument to :documentation specifies a documentation string; it is attached as a documentation string to the package. At most one :documentation option can appear in a single defpackage form.

:use

    The arguments to :use set the packages that the package named by package-name will inherit from. If :use is not supplied, it defaults to the same implementation-dependent value as the :use argument to make-package.

:shadow

    The arguments to :shadow, symbol-names, name symbols that are to be created in the package being defined. These symbols are added to the list of shadowing symbols effectively as if by shadow.

:shadowing-import-from

    The symbols named by the argument symbol-names are found (involving a lookup as if by find-symbol) in the specified package-name. The resulting symbols are imported into the package being defined, and placed on the shadowing symbols list as if by shadowing-import. In no case are symbols created in any package other than the one being defined.

:import-from

    The symbols named by the argument symbol-names are found in the package named by package-name and they are imported into the package being defined. In no case are symbols created in any package other than the one being defined.

:export

    The symbols named by the argument symbol-names are found or created in the package being defined and exported. The :export option interacts with the :use option, since inherited symbols can be used rather than new ones created. The :export option interacts with the :import-from and :shadowing-import-from options, since imported symbols can be used rather than new ones created. If an argument to the :export option is accessible as an (inherited) internal symbol via use-package, that the symbol named by symbol-name is first imported into the package being defined, and is then exported from that package.

:intern

    The symbols named by the argument symbol-names are found or created in the package being defined. The :intern option interacts with the :use option, since inherited symbols can be used rather than new ones created.

:size

    The argument to the :size option declares the approximate number of symbols expected in the package. This is an efficiency hint only and might be ignored by an implementation.

The order in which the options appear in a defpackage form is irrelevant. The order in which they are executed is as follows:

1. :shadow and :shadowing-import-from.
2. :use.
3. :import-from and :intern.
4. :export.

Shadows are established first, since they might be necessary to block spurious name conflicts when the :use option is processed. The :use option is executed next so that :intern and :export options can refer to normally inherited symbols. The :export option is executed last so that it can refer to symbols created by any of the other options; in particular, shadowing symbols and imported symbols can be made external.

If a defpackage form appears as a top level form, all of the actions normally performed by this macro at load time must also be performed at compile time.

* 示例(Examples):

 (defpackage "MY-PACKAGE"
   (:nicknames "MYPKG" "MY-PKG")
   (:use "COMMON-LISP")
   (:shadow "CAR" "CDR")
   (:shadowing-import-from "VENDOR-COMMON-LISP"  "CONS")
   (:import-from "VENDOR-COMMON-LISP"  "GC")
   (:export "EQ" "CONS" "FROBOLA")
   )
 
 
 (defpackage my-package
   (:nicknames mypkg :MY-PKG)  ; remember Common Lisp conventions for case
   (:use common-lisp)          ; conversion on symbols
   (:shadow CAR :cdr #:cons)                              
   (:export "CONS")            ; this is the shadowed one.
   )

* 受此影响(Affected By):

Existing packages.

* 异常情况(Exceptional Situations): 

If one of the supplied :nicknames already refers to an existing package, an error of type package-error is signaled.

An error of type program-error should be signaled if :size or :documentation appears more than once.

Since implementations might allow extended options an error of type program-error should be signaled if an option is present that is not actually supported in the host implementation.

The collection of symbol-name arguments given to the options :shadow, :intern, :import-from, and :shadowing-import-from must all be disjoint; additionally, the symbol-name arguments given to :export and :intern must be disjoint. Disjoint in this context is defined as no two of the symbol-names being string= with each other. If either condition is violated, an error of type program-error should be signaled.

For the :shadowing-import-from and :import-from options, a correctable error of type package-error is signaled if no symbol is accessible in the package named by package-name for one of the argument symbol-names.

Name conflict errors are handled by the underlying calls to make-package, use-package, import, and export. See Section 11.1 (Package Concepts).

* 也见(See Also):

documentation, Section 11.1 (Package Concepts), Section 3.2 (Compilation)

* 注意(Notes):

The :intern option is useful if an :import-from or a :shadowing-import-from option in a subsequent call to defpackage (for some other package) expects to find these symbols accessible but not necessarily external.

It is recommended that the entire package definition is put in a single place, and that all the package definitions of a program are in a single file. This file can be loaded before loading or compiling anything else that depends on those packages. Such a file can be read in the COMMON-LISP-USER package, avoiding any initial state issues.

defpackage cannot be used to create two ``mutually recursive'' packages, such as:

 (defpackage my-package
   (:use common-lisp your-package)    ;requires your-package to exist first
   (:export "MY-FUN"))                
 (defpackage your-package
   (:use common-lisp)
   (:import-from my-package "MY-FUN") ;requires my-package to exist first
   (:export "MY-FUN"))

However, nothing prevents the user from using the package-affecting functions such as use-package, import, and export to establish such links after a more standard use of defpackage.

The macroexpansion of defpackage could usefully canonicalize the names into strings, so that even if a source file has random symbols in the defpackage form, the compiled file would only contain strings.

Frequently additional implementation-dependent options take the form of a keyword standing by itself as an abbreviation for a list (keyword T); this syntax should be properly reported as an unrecognized option in implementations that do not support it. 


### <span id="M-DS-DES-DAS">宏 DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS</span>

* 语法(Syntax):

do-symbols (var [package [result-form]]) declaration* {tag | statement}*

=> result*

do-external-symbols (var [package [result-form]]) declaration* {tag | statement}*

=> result*

do-all-symbols (var [result-form]) declaration* {tag | statement}*

=> result*

* 参数和值(Arguments and Values):

var---a variable name; not evaluated.

package---a package designator; evaluated. The default in do-symbols and do-external-symbols is the current package.

result-form---a form; evaluated as described below. The default is nil.

declaration---a declare expression; not evaluated.

tag---a go tag; not evaluated.

statement---a compound form; evaluated as described below.

results---the values returned by the result-form if a normal return occurs, or else, if an explicit return occurs, the values that were transferred.

* 描述(Description):

do-symbols, do-external-symbols, and do-all-symbols iterate over the symbols of packages. For each symbol in the set of packages chosen, the var is bound to the symbol, and the statements in the body are executed. When all the symbols have been processed, result-form is evaluated and returned as the value of the macro.

do-symbols iterates over the symbols accessible in package. Statements may execute more than once for symbols that are inherited from multiple packages.

do-all-symbols iterates on every registered package. do-all-symbols will not process every symbol whatsoever, because a symbol not accessible in any registered package will not be processed. do-all-symbols may cause a symbol that is present in several packages to be processed more than once.

do-external-symbols iterates on the external symbols of package.

When result-form is evaluated, var is bound and has the value nil.

An implicit block named nil surrounds the entire do-symbols, do-external-symbols, or do-all-symbols form. return or return-from may be used to terminate the iteration prematurely.

If execution of the body affects which symbols are contained in the set of packages over which iteration is occurring, other than to remove the symbol currently the value of var by using unintern, the consequences are undefined.

For each of these macros, the scope of the name binding does not include any initial value form, but the optional result forms are included.

Any tag in the body is treated as with tagbody.

* 示例(Examples):

 (make-package 'temp :use nil) =>  #<PACKAGE "TEMP">
 (intern "SHY" 'temp) =>  TEMP::SHY, NIL ;SHY will be an internal symbol
                                         ;in the package TEMP
 (export (intern "BOLD" 'temp) 'temp)  =>  T  ;BOLD will be external  
 (let ((lst ()))
   (do-symbols (s (find-package 'temp)) (push s lst))
   lst)
=>  (TEMP::SHY TEMP:BOLD)
OR=>  (TEMP:BOLD TEMP::SHY)
 (let ((lst ()))
   (do-external-symbols (s (find-package 'temp) lst) (push s lst))
   lst) 
=>  (TEMP:BOLD)
 (let ((lst ()))                                                     
   (do-all-symbols (s lst)
     (when (eq (find-package 'temp) (symbol-package s)) (push s lst)))
   lst)
=>  (TEMP::SHY TEMP:BOLD)
OR=>  (TEMP:BOLD TEMP::SHY)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

intern, export, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-INTERN">函数 INTERN</span>

* 语法(Syntax):

        intern string &optional package => symbol, status

* 参数和值(Arguments and Values):

        string---一个字符串.
        package---一个包指示符. 默认为当前包.
        symbol---一个符号.
        status---:inherited, :external, :internal, 或者 nil 的其中之一.

* 描述(Description):

        intern 输入一个名为字符串 string 的符号到包 package 中. 如果一个名字和字符串 string 相同的符号在包 package 中已经可访问了, 就把它返回. 如果在包 package 中没有这样的符号, 那么带有这个给定名字的新的符号会被创建并输入到包 package 中作为一个内部符号, 如果包 package 是 KEYWROD 包就作为一个外部符号; 包 package 就成为创建的这个符号的 home 包.

        由 intern 返回的第一个值, symbol, 是这个被找到或者被创建的符号. 第二个值, status, 分别是:

        :internal

            这个符号被找到并且出现在包 package 中作为一个内部符号.

        :external

            这个符号被找到并且出现在包 package 中作为一个外部符号.

        :inherited

            这个符号被找到并且是通过 use-package 继承而来 (这也意味着这个符号是内部的).

        nil

            没有找到之前存在的符号, 所以创建一个.

            成为这个新符号名字的字符串是给定的字符串 string 还是它的一个拷贝是依赖于具体实现的. 在一个新符号被创建的情况下, 一旦一个字符串已经被给定作为给 intern 的 string 参数, 如果后面尝试去修改这个字符串那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (intern "Never-Before") =>  |Never-Before|, NIL
    (intern "Never-Before") =>  |Never-Before|, :INTERNAL 
    (intern "NEVER-BEFORE" "KEYWORD") =>  :NEVER-BEFORE, NIL
    (intern "NEVER-BEFORE" "KEYWORD") =>  :NEVER-BEFORE, :EXTERNAL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        find-symbol, read, symbol, unintern, Section 2.3.4 (Symbols as Tokens)

* 注意(Notes):

        如果这里已经有一个给定名字的可访问符号, intern 不需要去做任何名字冲突检测, 因为它不会创建一个新符号. 


### <span id="F-PACKAGE-NAME">函数 PACKAGE-NAME</span>

* 语法(Syntax):

        package-name package => name

* 参数和值(Arguments and Values):

        package---一个包标识符.
        name---一个字符串或 nil.

* 描述(Description):

        package-name 返回命名这个包 package 的字符串, 如果这个包标识符是一个没有名字包对象(见函数 delete-package)就是 nil.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (package-name *package*) =>  "COMMON-LISP-USER"
    (package-name (symbol-package :test)) =>  "KEYWORD"
    (package-name (find-package 'common-lisp)) =>  "COMMON-LISP"

    (defvar *foo-package* (make-package "FOO"))
    (rename-package "FOO" "FOO0")
    (package-name *foo-package*) =>  "FOO0"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 package 不是一个包, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-PACKAGE-NICKNAMES">函数 PACKAGE-NICKNAMES</span>

* 语法(Syntax):

        package-nicknames package => nicknames

* 参数和值(Arguments and Values):

        package---一个包标识符.
        nicknames---一个字符串列表.

* 描述(Description):

        返回包 package 的别名字符串列表, 不包括包 package 的名字.

* 示例(Examples):

 (package-nicknames (make-package 'temporary
                                   :nicknames '("TEMP" "temp")))
=>  ("temp" "TEMP") 

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 package 不是一个包, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-PACKAGE-SHADOWING-SYMBOLS">函数 PACKAGE-SHADOWING-SYMBOLS</span>

* 语法(Syntax):

        package-shadowing-symbols package => symbols

* 参数和值(Arguments and Values):

        package---一个包标识符.
        symbols---一个符号列表.

* 描述(Description):

        返回一个在包 package 中已经通过 shadow 或 shadowing-import (或等价的 defpackage 选项)被声明为遮蔽的符号的列表. 这个列表中的所有符号都出现在这个包中.

* 示例(Examples):

    ```LISP
    (package-shadowing-symbols (make-package 'temp)) =>  ()
    (shadow 'cdr 'temp) =>  T
    (package-shadowing-symbols 'temp) =>  (TEMP::CDR)
    (intern "PILL" 'temp) =>  TEMP::PILL, NIL
    (shadowing-import 'pill 'temp) =>  T
    (package-shadowing-symbols 'temp) =>  (PILL TEMP::CDR)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 package 不是一个包, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        shadow, shadowing-import

* 注意(Notes):

        这个符号列表是否是新的依赖于具体实现. 


### <span id="F-PACKAGE-USE-LIST">函数 PACKAGE-USE-LIST</span>

* 语法(Syntax):

        package-use-list package => use-list

* 参数和值(Arguments and Values):

        package---一个包标识符.
        use-list---一个包对象列表.

* 描述(Description):

        返回被包 package 使用的其他包的列表.

* 示例(Examples):

    ```LISP
    (package-use-list (make-package 'temp)) =>  (#<PACKAGE "COMMON-LISP">)
    (use-package 'common-lisp-user 'temp) =>  T
    (package-use-list 'temp) =>  (#<PACKAGE "COMMON-LISP"> #<PACKAGE "COMMON-LISP-USER">)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 package 不是一个包, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        use-package, unuse-package

* 注意(Notes): None. 


### <span id="F-PACKAGE-USED-BY-LIST">函数 PACKAGE-USED-BY-LIST</span>

* 语法(Syntax):

        package-used-by-list package => used-by-list

* 参数和值(Arguments and Values):

        package---一个包标识符.
        used-by-list---一个包对象列表.

* 描述(Description):

        package-used-by-list 返回其他使用包 package 的包的列表.

* 示例(Examples):

    ```
    (package-used-by-list (make-package 'temp)) =>  ()
    (make-package 'trash :use '(temp)) =>  #<PACKAGE "TRASH">
    (package-used-by-list 'temp) =>  (#<PACKAGE "TRASH">)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 package 不是一个包, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        use-package, unuse-package

* 注意(Notes): None. 


### <span id="F-PACKAGEP">函数 PACKAGEP</span>

* 语法(Syntax):

        packagep object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果对象 object 是 package 类型的就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (packagep *package*) =>  true 
    (packagep 'common-lisp) =>  false 
    (packagep (find-package 'common-lisp)) =>  true 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also): None.

* 注意(Notes):

        (packagep object) ==  (typep object 'package)



### <span id="V-PACKAGE">变量 *PACKAGE*</span>

* 值类型(Value Type):

        一个包对象.

* 初始值(Initial Value):

        COMMON-LISP-USER 包.

* 描述(Description):

        不管哪个包对象当前是 *package* 的值, 都会被引用作为当前包.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    *package* =>  #<PACKAGE "COMMON-LISP-USER">
    (make-package "SAMPLE-PACKAGE" :use '("COMMON-LISP"))
    =>  #<PACKAGE "SAMPLE-PACKAGE">
    (list 
      (symbol-package
        (let ((*package* (find-package 'sample-package)))
          (setq *some-symbol* (read-from-string "just-testing"))))
      *package*)
    =>  (#<PACKAGE "SAMPLE-PACKAGE"> #<PACKAGE "COMMON-LISP-USER">)
    (list (symbol-package (read-from-string "just-testing"))
          *package*)
    =>  (#<PACKAGE "COMMON-LISP-USER"> #<PACKAGE "COMMON-LISP-USER">)
    (eq 'foo (intern "FOO")) =>  true
    (eq 'foo (let ((*package* (find-package 'sample-package)))
                (intern "FOO")))
    =>  false
    ```

* 受此影响(Affected By):

        load, compile-file, in-package

* 也见(See Also):

        compile-file, in-package, load, package

* 注意(Notes): None. 


### <span id="CT-PACKAGE-ERROR">状况类型 PACKAGE-ERROR</span>

* 类优先级列表(Class Precedence List):

        package-error, error, serious-condition, condition, t

* 描述(Description):

        这个 package-error 类型由在包上操作相关的错误状况组成. 这个违规的包 (或者包名) 是通过给 make-condition 的 :package 初始化参数来初始化的, 并且可以通过函数 package-error-package 来访问.

* 也见(See Also):

        package-error-package, Section 9 (Conditions) 


### <span id="F-PACKAGE-ERROR-PACKAGE">函数 PACKAGE-ERROR-PACKAGE</span>

* 语法(Syntax):

        package-error-package condition => package

* 参数和值(Arguments and Values):

        condition---一个 package-error 类型的状况.
        package---一个包标识符.

* 描述(Description):

        返回这个状况 condition 表示的情况中出问题的包的标识符.

* 示例(Examples):

    ```LISP
    (package-error-package 
      (make-condition 'package-error
        :package (find-package "COMMON-LISP")))
    =>  #<Package "COMMON-LISP">
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        package-error

* 注意(Notes): None. 


