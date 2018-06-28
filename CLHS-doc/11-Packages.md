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

        symbol-names---一个 string 标识符列表的标识符.
        package---一个包标识符. 默认是当前包.

* 描述(Description):

        shadow 确保带有 symbol-names 给定的名字的符号出现在包 package 中.

        特别地, 包 package 被搜索带有符号名 symbol-names 中所提供名字的符号. 对于每一个这样的名字, 如果一个对应的符号没有出现在包 package 中 (直接地, 或者通过继承), 那么会用这个名字创建一个对应符号, 并且插入到包 package 中作为一个内部符号. 这个对应的符号, 不管是之前存在的或是新创建的, 如果没有出现在那个包 package 中就会被添加到包 package 的遮蔽符号列表中.

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects):

        shadow 改变这个包系统的状态, 以一种这个包一致性规则不适用于这个改变的方式.

* 受此影响(Affected By):

        这个包系统的当前状态.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        package-shadowing-symbols, 章节 11.1 (Package Concepts)

* 注意(Notes):

        如果一个带有 symbol-names 中的一个名字的符号已经存在于包 package 中, 但是是通过继承而来的, 这个继承的符号会被一个新创建的内部符号所遮蔽. 


### <span id="F-SHADOWING-IMPORT">函数 SHADOWING-IMPORT</span>

* 语法(Syntax):

        shadowing-import symbols &optional package => t

* 参数和值(Arguments and Values):

        symbols---一个符号列表的标识符.
        package ---一个包标识符. 默认是当前包.

* 描述(Description):

        shadowing-import 就像是 import, 但是它不会发出一个错误, 即便一个符号的导入会遮蔽某个在包 package 中已经可访问的某个符号.

        shadowing-import 将这些符号 symbols 中的每一个插入到包 package 中作为一个内部符号, 不管另一个相同符号的名字是否被这个动作遮蔽. 如果一个相同名字的不同符号已经出现在包 package 中, 那个符号首先被从包 package 解除捕捉. 这个新的符号会被添加到包 package 的遮蔽符号列表中.

        shadowing-import 对它要检测的范围执行名字冲突检测, 不管一个相同名字的不同的已存在符号是否可以访问; 如果这样, 它被这个新符号所遮蔽, 这意味着如果它出现在包 package 中它必须被解除捕捉.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (setq sym (intern "CONFLICT")) =>  CONFLICT
    (intern "CONFLICT" (make-package 'temp)) =>  TEMP::CONFLICT, NIL
    (package-shadowing-symbols 'temp) =>  NIL
    (shadowing-import sym 'temp) =>  T 
    (package-shadowing-symbols 'temp) =>  (CONFLICT)
    ```

* 副作用(Side Effects):

        shadowing-import 以一种一致性规则不适用这个改变的方式修改这个包系统的状态.

        包 package 的遮蔽符号列表会被修改.

* 受此影响(Affected By):

        这个包系统的当前状态.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        import, unintern, package-shadowing-symbols

* 注意(Notes): None. 


### <span id="F-DELETE-PACKAGE">函数 DELETE-PACKAGE</span>

* 语法(Syntax):

        delete-package package => generalized-boolean

* 参数和值(Arguments and Values):

        package---一个包标识符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        delete-package 从所有包系统数据结构中删除包 package. 如果这个操作成功, delete-package 返回 true, 否则就是 nil. 这个 delete-package 的效果是包 package 的名字和别名不再被识别包名. 这个包对象仍然是个包 (换句话说, packagep 对于它是 true 的) 但是 package-name 返回 nil. 删除这个 COMMON-LISP 包或 KEYWORD 包的后果是未定义的. 一旦包 package 被删除, 在这个包上调用任何其他包操作的后果是未指定的. 特别地, 如果在 *package* 绑定给那个删除的包或者用这个删除的包作为参数的情况下调用 find-symbol, intern 和其他在一个包中查找符号名的函数的后果是未指定的.

        如果包 package 是一个已经被删除的包对象, delete-package 立即返回 nil.

        在这个操作完成后, 之前 home 包为包 package 的符号的 home 包是依赖于具体实现的. 除了这个, 在包 package 中可访问的符号不会以其他任何方式被修改; home 包不是包 package 的符号保持不变.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果这个包标识符是一个当期没有命名一个包的名字, 就会发出一个 package-error 类型的可校正错误. 如果尝试了这个校正, 就不会尝试去做删除动作; 反而, delete-package 立即返回 nil.

        如果包 package 被其他包所使用, 就会发出一个 package-error 类型的可校正错误. 如果尝试了一个校正, unuse-package 会被调用来移除任何依赖, 导致包 package 的外部符号对于那些使用了包 package 的包不再是可访问的. delete-package 接下来删除包 package 就好像已经没有包使用它了一样.

* 也见(See Also):

        unuse-package

* 注意(Notes): None. 


### <span id="F-MAKE-PACKAGE">函数 MAKE-PACKAGE</span>

* 语法(Syntax):

        make-package package-name &key nicknames use => package

* 参数和值(Arguments and Values):

        package-name---一个字符串标识符.
        nicknames---一个字符串标识符列表. 默认是空列表.
        use---一个包标识符列表. 默认是具体实现定义的.
        package---一个包.

* 描述(Description):

        创建一个名为 package-name 的新包.

        别名 nicknames 是可以被用于引用这个新的包的额外的名字.

        use 指定了 0 个或多个包, 它们的外部符号会被这个新包所继承. 见函数 use-package.

* 示例(Examples):

    ```LISP
    (make-package 'temporary :nicknames '("TEMP" "temp")) =>  #<PACKAGE "TEMPORARY">
    (make-package "OWNER" :use '("temp")) =>  #<PACKAGE "OWNER">
    (package-used-by-list 'temp) =>  (#<PACKAGE "OWNER">)
    (package-use-list 'owner) =>  (#<PACKAGE "TEMPORARY">)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        在这个系统中其他包的存在.

* 异常情况(Exceptional Situations): 

        如果 use 指定的包不存在, 那么后果是未指定的.

        如果包名 package-name 或这些别名 nicknames 中的任何一个已经是一个已存在的包的包名或别名那么就会发出一个.

* 也见(See Also):

        defpackage, use-package

* 注意(Notes):

        在要被使用的包包含了会冲突的符号的情况下, 有必要先用 :use '() 来创建这个包, 然后使用 shadow 或 shadowing-import 来定位这些冲突, 在这之后一旦这些冲突已经被定位就使用 use-package.

        当要被创建的包是一个程序的静态定义的一部分而不是动态地被程序创建, 在文体上更适合使用 defpackage 而不是 make-package. 


### <span id="M-WITH-PACKAGE-ITERATOR">宏 WITH-PACKAGE-ITERATOR</span>

* 语法(Syntax):

        with-package-iterator (name package-list-form &rest symbol-types) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        name---一个符号.
        package-list-form---一个表达式形式; 求值一次来产生 package-list.
        package-list---一个包标识符列表的标识符.
        symbol-type---符号 :internal, :external, 或 :inherited 的其中之一.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式 progn.
        results---这些表达式形式 forms 的值.

* 描述(Description):

        在这个主体表达式形式 forms 的词法作用域中, 这个名字 name 是通过 macrolet 定义的, 这么一来后续的 (name) 调用会一个接一个依次返回来自 package-list 中包的符号.

        从多个包中继承的符号是否会被返回超过一次是未指定的. 返回符号的顺序没有必要反映 package-list 中包的顺序. 当 package-list 有着不止一个元素时, 重复符号会被返回一次还是不止一次是未指定的.

        如下的符号类型 Symbol-types 控制一个包中可以访问的哪些符号会被返回:

        :internal

            出现在包中但是没有被导出的符号.

        :external

            出现在包中并且被导出的符号.

        :inherited

            被使用的包导出并且没有被遮蔽的符号.

        当为符号类型 symbol-types 提供了超过一个参数时, 如果一个符号的可访问性符合提供的其中一个 symbol-type, 它就会被返回. 具体实现可能通过识别额外的符号可访问性类型来扩展这个语法.

        一个 (name) 调用返回如下四个值:

        1. 一个表示一个符号是否被返回的标志 (true 表示一个符号被返回).
        2. 一个在指定的包中是可访问的符号.
        3. 那个符号的可访问性类型; 换句话说, 这些符号 :internal, :external, 或 :inherited 的其中一个.
        4. 获取这个符号的包. 这个包是 package-list 中出现或命名的其中一个包.

        通过依次调用 (name) 所有符号已经被返回后, 然后只有一个值会被返回, 也就是 nil.

        第二, 第三, 和第四个值的意义是, 返回的符号在返回的包中是可访问的, 其方式是由第二个返回值所指示的, 如下所示:

        :internal

            意味着出现但未导出.

        :external

            意味着出现并导出.

        :inherited

            意味着没有出现 (因此没有被遮蔽) 但是从某个使用的包中继承.

        如果一个迭代的任何隐式的内部状态在这个 with-package-iterator 表达式形式的动态范围之外被返回, 比如通过在调用表达式形式中返回某个闭包, 那么会发生什么是不确定的.

        任何数量的 with-package-iterator 调用可以被嵌套, 并且最内部的那个的主体可以调用所有这些局部建立的宏, 假设所有这些符号有着不同的名字.

* 示例(Examples):

        下面函数应该在任何包上都返回 t, 如果这个 with-package-iterator 的使用和对应 do-symbols 的使用不一致就会发出一个错误.

    ```LISP
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
    ```

        下面函数打印出每个出现的符号 (可能不止一次):

    ```LISP
    (defun print-all-symbols () 
      (with-package-iterator (next-symbol (list-all-packages)
                              :internal :external)
        (loop
          (multiple-value-bind (more? symbol) (next-symbol)
            (if more? 
                (print symbol)
                (return))))))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果没有提供符号类型 symbol-types 或者提供了一个不被具体实现所识别的符号类型 symbol-type, 那么 with-package-iterator 会发出一个 program-error 类型的错误.

        如果通过 with-package-iterator 建立的名为 name 的局部函数在它返回 false 作为它的主要值之后被调用, 那么后果是未定义的.

* 也见(See Also):

        章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-UNEXPORT">函数 UNEXPORT</span>

* 语法(Syntax):

        unexport symbols &optional package => t

* 参数和值(Arguments and Values):

        symbols---一个符号列表的标识符.
        package---一个包标识符. 默认是当前包.

* 描述(Description):

        unexport 回复包 package 中的外部符号为内部状态; 它撤销了 export 的效果.

        unexport 只工作在出现在包 package 中的符号上, 把它们转换回内部状态. 如果给 unexport 一个已经作为内部符号出现在包 package 的一个符号, 它什么都不做.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (export (intern "CONTRABAND" (make-package 'temp)) 'temp) =>  T
    (find-symbol "CONTRABAND") =>  NIL, NIL 
    (use-package 'temp) =>  T 
    (find-symbol "CONTRABAND") =>  CONTRABAND, :INHERITED
    (unexport 'contraband 'temp) =>  T
    (find-symbol "CONTRABAND") =>  NIL, NIL
    ```

* 副作用(Side Effects):

        包系统会被修改.

* 受此影响(Affected By):

        包系统的当前状态

* 异常情况(Exceptional Situations): 

        如果给 unexport 一个在包 package 中不可访问的符号, 就会发出一个 package-error 类型的错误.

        如果包 package 是 KEYWORD 包或者 COMMON-LISP 包, 那么后果是未定义的.

* 也见(See Also):

        export, 章节 11.1 (Package Concepts)

* 注意(Notes): None. 


### <span id="F-UNINTERN">函数 UNINTERN</span>

* 语法(Syntax):

        unintern symbol &optional package => generalized-boolean

* 参数和值(Arguments and Values):

        symbol---一个符号.
        package---一个包标识符. 默认是当前包.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        unintern 从包 package 中移除符号 symbol. 如果符号 symbol 出现在包 package 中, 它被从包 package 中被移除并且如果出现在包 package 的遮蔽符号列表中那么也从其中移除. 如果包 package 是这个符号 symbol 的 home 包, 符号 symbol 会被变成没有 home 包. 符号 Symbol 可能在包 package 中通过继承仍然是可以访问的.

        unintern 的使用可以导致一个没有记录 home 包的符号, 但是事实上在某些包中是可以访问的. Common Lisp 没有检查这个异常情况, 并且这些符号总是在 #: 之前打印出来.

        如果 unintern 移除了符号那么它就返回 true, 否则就是 nil.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (setq temps-unpack (intern "UNPACK" (make-package 'temp))) =>  TEMP::UNPACK 
    (unintern temps-unpack 'temp) =>  T
    (find-symbol "UNPACK" 'temp) =>  NIL, NIL 
    temps-unpack =>  #:UNPACK 
    ```

* 副作用(Side Effects):

        unintern 以一种这个一致性规则不适用的方式改变这个包系统的状态.

* 受此影响(Affected By):

        包系统的当前状态

* 异常情况(Exceptional Situations): 

        给 unintern 一个遮蔽符号可以揭露一个之前通过遮蔽解决的名字冲突. 如果包 A 包 B 和 C, A 包含了一个遮蔽符号 x, 并且 B 和 C 每个都包含名为 x 的外部符号, 如果这两个符号是不同的, 那么从 A 中移除这个遮蔽的符号会揭露一个在 b:x 和 c:x 之间的名字冲突. 在这个情况中 unintern 会发出一个错误.

* 也见(See Also):

        章节 11.1 (Package Concepts)

* 注意(Notes): None. 


### <span id="M-IN-PACKAGE">宏 IN-PACKAGE</span>

* 语法(Syntax):

        in-package name => package

* 参数和值(Arguments and Values):

        name---一个字符串标识符; 不求值.
        package---名为 name 的包.

* 描述(Description):

        导致这个名为 name 的包称为当前包---这也就是说, 这个 *package* 的值. 如果不存在这样的包, 那么就会发出一个 package-error 类型的错误.

        如果这个 in-package 调用作为顶层表达式形式出现, 那么 in-package 做的所有事也都在编译时执行.

* 示例(Examples): None.

* 副作用(Side Effects):

        变量 *package* 会被赋值. 如果这个 in-package 表达式形式是一个顶层表达式形式, 这个赋值也会在编译时发生.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果这个指定的包不存在, 就会发出一个 package-error 类型的错误.

* 也见(See Also):

        *package*

* 注意(Notes): None. 


### <span id="F-UNUSE-PACKAGE">函数 UNUSE-PACKAGE</span>

* 语法(Syntax):

        unuse-package packages-to-unuse &optional package => t

* 参数和值(Arguments and Values):

        packages-to-unuse---一个包标识符列表的标识符.
        package---一个包标识符. 默认是当前包.

* 描述(Description):

        unuse-package 导致包 package 不再从包 packages-to-unuse 中继承所有外部符号; unuse-package 撤销 use-package 的效果. 包 packages-to-unuse 被从包 package 的使用列表中移除.

        任何已经被导入到包 package 中的符号仍然会出现在包 package 中.

* 示例(Examples):

    ```LISP
    (in-package "COMMON-LISP-USER") =>  #<PACKAGE "COMMON-LISP-USER">
    (export (intern "SHOES" (make-package 'temp)) 'temp) =>  T
    (find-symbol "SHOES") =>  NIL, NIL
    (use-package 'temp) =>  T
    (find-symbol "SHOES") =>  SHOES, :INHERITED
    (find (find-package 'temp) (package-use-list 'common-lisp-user)) =>  #<PACKAGE "TEMP">
    (unuse-package 'temp) =>  T
    (find-symbol "SHOES") =>  NIL, NIL
    ```

* 副作用(Side Effects):

        包 package 的使用列表会被修改.

* 受此影响(Affected By):

        包系统的当前状态

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        use-package, package-use-list

* 注意(Notes): None. 


### <span id="F-USE-PACKAGE">函数 USE-PACKAGE</span>

* 语法(Syntax):

        use-package packages-to-use &optional package => t

* 参数和值(Arguments and Values):

        packages-to-use---一个包标识符列表的标识符. 这个 KEYWORD 包不能被提供.
        package---一个包标识符. 默认是当前包. 这个包不能是 KEYWORD 包.

* 描述(Description):

        use-package 导致包 package 去继承包列表 packages-to-use 的所有外部符号. 这些继承的符号成为包 package 中可访问的内部符号.

        如果 packages-to-use 还没有出现在包 package 的使用列表中, 那么它会被添加到包 package 的使用列表中. 在 packages-to-use 中的所有外部符号都成为包 package 中可访问的内部符号. use-package 不会导致任何新的符号出现在包 package 中但是会使它们变为可以通过继承访问的.

        use-package 检测那些新导入符号和包 package 中已经可以访问的符号之间的名字冲突. 在 use-package 中的两个被包 package 从 packages-to-use 继承而来的外部符号之间的一个名字冲突可以通过导入它们中的其中一个符号到包 package 中并使它成为一个遮蔽符号来解决.

* 示例(Examples):

    ```LISP
    (export (intern "LAND-FILL" (make-package 'trash)) 'trash) =>  T
    (find-symbol "LAND-FILL" (make-package 'temp)) =>  NIL, NIL
    (package-use-list 'temp) =>  (#<PACKAGE "TEMP">)
    (use-package 'trash 'temp) =>  T
    (package-use-list 'temp) =>  (#<PACKAGE "TEMP"> #<PACKAGE "TRASH">)
    (find-symbol "LAND-FILL" 'temp) =>  TRASH:LAND-FILL, :INHERITED
    ```

* 副作用(Side Effects):

        包 package 的使用列表会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        unuse-package, package-use-list, 章节 11.1 (Package Concepts)

* 注意(Notes):

        即便一个包 P2 已经使用了包 P1, 包 P1 去使用包 P2 也是允许的. 包的使用不会被传递, 所以这个明显的循环不会导致问题. 


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

        var---一个变量名; 不求值.
        package---一个包标识符; 求值. 在 do-symbols 和 do-external-symbols 中默认为当前包.
        result-form---一个表达式形式; 按如下所述求值. 默认为 nil.
        declaration---一个 declare 表达式; 不求值.
        tag---一个 go 标签; 不求值.
        statement---一个复合表达式形式; 按如下所述求值.
        results---如果发生了正常的返回, 就是 result-form 返回的值, 否则如果发生一个显式的 return, 就是传递的值.

* 描述(Description):

        do-symbols, do-external-symbols, 和 do-all-symbols 遍历包中的符号. 对于每一个选择的包集合中的符号, 这个 var 都会被绑定为那个符号, 然后在主体中的语句 statements 会被执行. 当所有符号都已经被处理时, result-form 被求值并且并且作为这个宏的值被返回.

        do-symbols 遍历包 package 中可访问的符号. 对于从多个包中继承的符号语句 statements 可能被执行不止一次.

        do-all-symbols 在每个注册的包上遍历. do-all-symbols 无论如何不会处理每个符号, 应为一个在任何注册的包中都不可访问的符号不会被处理. do-all-symbols 可能导致一个出现在多个包中的符号被处理不止一次.

        do-external-symbols 遍历包 package 中的外部符号.

        当 result-form 被求值时, var 被绑定并且值为 nil.

        一个名为 nil 的隐式 block 在整个 do-symbols, do-external-symbols, 或 do-all-symbols 表达式形式周围. return 或 return-from 可能被用于提前终止这个迭代.

        如果主体的执行影响了被包含在正在被迭代的包的集合中的符号, 除了使用 unintern 去移除 var 当前的值的符号之外, 后果是未定义的.

        对于这些宏中的每一个, 名称绑定的作用域不包括任何初始值表达式形式, 但是包括了那个可选的结果表达式形式.

        在主体中的任何 tag 都被和 tagbody 中一样对待.

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        intern, export, 章节 3.6 (Traversal Rules and Side Effects)

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


