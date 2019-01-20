# 6. 循环

> * 6.1 [LOOP 机制](#TheLOOPFacility)
> * 6.2 [迭代字典](#TheIterationDictionary)

## 6.1 <span id="TheLOOPFacility">LOOP 机制</span>

> * 6.1.1 [Loop 机制概述](#OverviewLoopFacility)
> * 6.1.2 [变量初始化和步进子句](#VarInitAndStepClauses)
> * 6.1.3 [值累积子句](#ValueAccumulationClauses)
> * 6.1.4 [终止测试子句](#TerminationTestClauses)
> * 6.1.5 [无条件执行子句](#UnconditionalExecutionClauses)
> * 6.1.6 [条件执行子句](#ConditionalExecutionClauses)
> * 6.1.7 [其他子句](#MiscellaneousClauses)
> * 6.1.8 [其他 Loop 特性的示例](#ExamplesMLF)
> * 6.1.9 [Loop 的注意事项](#NotesAboutLoop)

### 6.1.1 <span id="OverviewLoopFacility">Loop 机制概述</span>

这个 loop 宏[macro]执行循环.

> * 6.1.1.1 [简单 vs 扩展 Loop](#SimpleExtendedLoop)
> * 6.1.1.2 [Loop 关键字](#LoopKeywords)
> * 6.1.1.3 [解析 Loop 子句](#ParsingLoopClauses)
> * 6.1.1.4 [展开 Loop 表达式形式](#ExpandingLoopForms)
> * 6.1.1.5 [Loop 子句综述](#SummaryLoopClauses)
> * 6.1.1.6 [执行顺序](#OrderExecution)
> * 6.1.1.7 [解构](#Destructuring)
> * 6.1.1.8 [副作用的限制](#RestrictionsSideEffects)

#### 6.1.1.1 <span id="SimpleExtendedLoop">简单 vs 扩展 Loop</span>

loop 表达式形式[form]可以被分为两类: 简单 loop 表达式形式[form]和扩展 loop 表达式形式[form].

##### 6.1.1.1.1 简单 Loop

一个简单 loop 表达式形式[form]是有着只包含复合表达式形式[compound form]的主体(body)的 loop 表达式形式. 每个表达式形式[form]都按从左到右的次序依次求值. 当最后一个表达式形式 form 被求值后, 第一个表达式形式 form 被再一次求值, 以此类推, 在一个不终止的循环中. 一个简单 loop 表达式形式[form]建立一个名为 nil 的隐式块[implicit block]. 这个简单 loop 的执行可以通过显式控制转移到这个隐式块[implicit block] (使用 return 或 return-from) 或者到这个块[block]外部的某个退出点[exit point] (比如, 使用 throw, go, 或 return-from)来终止. 

##### 6.1.1.1.2 扩展 Loop

一个扩展 loop 表达式形式[form]是指有着包含原子[atomic]表达式[expression]的主体(body)的一个 loop 表达式形式[form]. 当这个 loop 宏[macro]处理这样一个表达式形式[form]时, 它调用一个通常称为 "Loop 工具" 的工具.

这个 Loop 工具通过循环模式提供对迭代中常用机制的标准化访问, 循环模式是由 loop 关键字[loop keyword]引入的.

扩展 loop 表达式形式[form]的主体(body)被分为 loop 子句, 每一个子句都由 loop 关键字[loop keyword]和表达式形式[form]组成. 

#### 6.1.1.2 <span id="LoopKeywords">Loop 关键字</span>

Loop 关键字[loop keyword]不是真的关键字[keyword[1]]; 它们是特殊的符号[symbol], 通过名字[name]而不是对象[object]标识来识别, 并只有在 loop 工具中有意义. 一个 loop 关键字[loop keyword]是一个通过它的名字[name] (而不是它的标识) 来识别的符号[symbol], 不管它在哪个包[package]中是可访问的[accessible].

通常, loop 关键字[loop keyword]不是 COMMON-LISP 包中的外部符号[external symbol], 除非在一种巧合的情况下 Common Lisp 需要使用与 loop 关键字[loop keyword]相同名称的符号[symbol]来实现其他目的. 比如, 在 COMMON-LISP 包中有一个符号名字[name]为 "UNLESS" 但是没有名字[name]为 "UNTIL" 的符号[symbol].

如果在一个 loop 表达式形式[form]中没有提供 loop 关键字[loop keyword], 那么这个 Loop 工具就会重复地执行主体(body); 见章节 6.1.1.1.1 (简单 Loop). 


#### 6.1.1.3 <span id="ParsingLoopClauses">解析 Loop 子句</span>

扩展 loop 表达式形式[form]的语法部分称之为子句(clause); 解析的规则由子句的关键字决定. 下面这个例子展示了有着 6 个子句的 loop 表达式形式[form]:

```LISP
(loop for i from 1 to (compute-top-value)       ; first clause
      while (not (unacceptable i))              ; second clause
      collect (square i)                        ; third clause
      do (format t "Working on ~D now" i)       ; fourth clause
      when (evenp i)                            ; fifth clause
        do (format t "~D is a non-odd number" i)
      finally (format t "About to exit!"))      ; sixth clause
```

每个 loop 关键字[loop keyword]都引入了一个复合 loop 子句或一个简单的 loop 子句, 它可以由一个 loop 关键字[loop keyword]后面跟着一个单独的表达式形式[form]所组成. 一个子句中表达式形式[form]的数量由开始这个子句的 loop 关键字[loop keyword]和这个子句中的辅助关键字决定. 关键字 do, doing, initially, 和 finally 是仅有的可以接受任何数量的表达式形式[form]并以一个隐式的 progn [implicit progn]组织起来的 loop 关键字.

Loop 子句可以包含辅助关键字, 这个关键字有时候称之为介词. 比如, 上面这段代码中的第一个子句包含了介词 from 和 to, 它们标记出了步进开始和结束的值.

关于 loop 语法的详细信息, 见宏[macro] loop. 


#### 6.1.1.4 <span id="ExpandingLoopForms">展开 Loop 表达式形式</span>

一个 loop 宏表达式形式[macro form]展开为一个包含一个或多个绑定表达式(这个建立[establish]循环变量的绑定[binding])和一个 block 和一个 tagbody(这个表示循环控制结构) 的表达式形式[form]. 在 loop 中建立的变量就像是通过 let 或者 lambda 绑定的一样.

具体实现可以将初始值的设置和绑定[binding]交错在一起. 然而, 初始值的赋值总是按照用户指定的顺序被计算. 因此, 变量有时会被绑定到正确类型[type]的无意义值, 然后在开始时使用 setq 将其设置为真正的初始值. 这个交错的一个含义是, 在除了 for-as-equals-then 之外的任何 for-as-subclause 中的初始值表达式形式[form] (不同地称为 form1, form2, form3, step-fun, vector, hash-table, 和 package) 被求值所在的词法环境[lexical environment]是只包含在该表达式形式之前的循环变量, 还是包含更多或全部的循环变量, 是依赖于具体实现的[implementation-dependent]; 在一个 for-as-equals-then 表达式形式的 form1 和 form2 包括了所有循环变量的词法环境[lexical environment].

在这个表达式形式[form]展开后, 它由 tagbody 中的三个基本部分组成: 循环序言(the loop prologue), 循环体(the loop body), 还有循环结尾(the loop epilogue).

循环序言(the loop prologue)

    循环序言包含在循环开始之前执行的表达式形式[form], 例如 variable 子句中指定的任何自动变量初始化, 以及任何 initially 子句出现在源代码中的位置.<!--TODO 待校对-->

循环体(the loop body)

    循环体包含了在循环期间执行的表达式形式[form], 包括应用特定的计算, 终止测试, 还有变量步进[step[1]].

循环结尾(the loop epilogue)

    循环结尾包含循环终止后执行的表达式形式[form], 比如 finally 子句, 如果存在的话, 还有任何来自于 accumulation 子句或者一个 termination-test 子句的隐式的返回值.

一些来自于源表达式形式[form]的子句只对循环序言贡献代码; 这个子句必须在 loop 表达式主体中的其他子句之前. 其他子句只对循环结尾贡献代码. 所有其他的子句都以和原始的 loop 源表达式形式[form]中给出的相同的顺序为最终的转换表达式形式[form]贡献代码.

除非提供了 named, 否则 loop 宏的展开式产生一个名为 nil 的隐式块[implicit block]. 因此, return-from (有时为 return) 可以被用于从 loop 中返回值或退出 loop. 

#### 6.1.1.5 <span id="SummaryLoopClauses">Loop 子句综述</span>

Loop 子句属于以下类别之一:

> * 6.1.1.5.1 [变量初始化和步进子句综述](#SummaryVarInitStepClauses)
> * 6.1.1.5.2 [值累积子句综述](#SummaryValueAccumulationClauses)
> * 6.1.1.5.3 [终止测试子句综述](#SummaryTerminationTestClauses)
> * 6.1.1.5.4 [无条件执行子句综述](#SummaryUncondExecClauses)
> * 6.1.1.5.5 [条件执行子句综述](#SummaryCondExecClauses)
> * 6.1.1.5.6 [其他子句综述](#SummaryMiscellaneousClauses)

##### 6.1.1.5.1 <span id="SummaryVarInitStepClauses">变量初始化和步进子句综述</span>

这个 for 和 as 构造提供一个循环控制子句, 它建立一个要被初始化的变量. for 和 as 子句可以和 loop 关键字 and 组合使用来获得并行[parallel]初始化和步进[step[1]]. 否则, 这个初始化和步进[step[1]]就是顺序的[sequential].

这个 with 构造类似于单独的 let 子句. with 子句可以使用 loop 关键字[loop keyword] and 来组合使用进而获得并行[parallel]初始化 .

关于更多信息, 见章节 6.1.2 (变量初始化和步进子句). 

##### 6.1.1.5.2 <span id="SummaryValueAccumulationClauses">值累积子句综述</span>

这个 collect (或 collecting) 构造接受一个在它的子句中的表达式形式[form]并且添加这个表达式形式[form]的值到一个值列表[list]的末尾. 默认情况下, 当这个 loop 结束时这个值列表[list]被返回.

这个 append (或 appending) 构造接受一个在它的子句中的表达式形式[form]并追加这个表达式形式[form]的值到一个值列表[list]的末尾. 默认情况下, 当这个 loop 结束时这个值列表[list]被返回.

这个 nconc (或 nconcing) 构造类似于 append 构造, 但是它的列表[list]值是被串联起来的就像是通过函数 nconc 一样. 默认情况下, 当这个 loop 结束时这个值列表[list]被返回.

这个 sum (或 summing) 构造接受一个在它的子句中求值为数字[number]的表达式形式[form], 它累积所有这些数字[number]的和 . 默认情况下, 当这个 loop 结束时这个累积的总和被返回.

这个 count (或 counting) 构造接受一个在它的子句中的表达式形式[from]并且计算这个表达式形式[form]被求值为 true 的次数. 默认情况下, 当这个 loop 结束时这个计数被返回.

这个 minimize (或 minimizing) 构造接受一个在它的子句中的表达式形式[form]并且通过求值这个表达式形式[form]来决定获取最小值. 默认情况下, 当这个 loop 结束时这个最小值被返回.

这个 maximize (或 maximizing) 构造接受一个在它的子句中的表达式形式[form]并且通过求值这个表达式形式[form]来决定获取最大值. 默认情况下, 当这个 loop 结束时这个最大值被返回.

关于更多信息, 见章节 6.1.3 (值累积子句). 

##### 6.1.1.5.3 <span id="SummaryTerminationTestClauses">终止测试子句综述</span>

这个 for 和 as 构造提供一个由这个循环控制子句决定的终止测试.

这个 repeat 构造导致在指定次数的循环后终止. (它使用一个内部变量来跟踪迭代次数.)

这个 while 构造接受一个表达式形式[form], 一个测试 test, 如果这个测试 test 求值为 false 就终止这个循环. 一个 while 子句等价于表达式 (if (not test) (loop-finish)).

这个 until 构造是 while 的倒转; 如果这个测试 test 求值为任何非 nil [non-nil]的值就终止这个循环. 一个 until 子句等价于表达式 (if test (loop-finish)).

这个 always 构造接受一个表达式形式[form], 当这个表达式形式[form]曾经求值为 false 就终止这个 loop; 在这个情况下, 这个 loop 表达式形式[form]返回 nil. 否则, 它提供一个 t 作为默认的返回值.

这个 never 构造接受一个表达式形式[form], 如果这个表达式形式[form]曾经求值为 true 就终止这个 loop; 在这个情况下, 这个 loop 表达式形式[form]返回 nil. 否则, 它提供一个 t 作为默认的返回值.

这个 thereis 构造接受一个表达式形式[form], 如果这个表达式形式[form]曾经求值为一个非 nil [non-nil]的对象[object]就终止这个 loop; 在这个情况下, 这个 loop 表达式形式[form]返回那个对象[object]. 否则, 它提供一个 nil 作为默认的返回值.

如果指定了多个终止测试子句, 那个任何一个满足的情况下这个 loop 表达式形式[form]就终止.

关于更多信息, 见章节 6.1.4 (终止测试子句). 

##### 6.1.1.5.4 <span id="SummaryUncondExecClauses">无条件执行子句综述</span>

这个 do (或 doing) 构造求值它的子句中的所有表达式形式[form].

这个 return 构造接受一个表达式形式[form]. 这个表达式形式[form]返回的任何值[value]都立即被 loop 表达式形式返回. 它等价于子句 do (return-from block-name value), 其中 block-name 是在 named 子句中指定的名字, 如果没有 named 子句那么就是 nil.

关于更多信息, 见章节 6.1.5 (无条件执行子句). 

##### 6.1.1.5.5 <span id="SummaryCondExecClauses">条件执行子句综述</span>

这个 if 和 when 构造接受一个表达式形式[form]作为测试以及一个在这个测试表达式产生[yield] true 的时候执行的子句. 这个子句可以是一个值累积子句, 无条件执行子句, 或者另一个条件执行子句; 它也可以是通过 loop 关键字 and 连接的任何这样的子句的组合.

这个 loop unless 构造类似于 loop when 构造, 除了它互补那个检验结果.

这个 loop else 构造提供一个可选的 if, when, 和 unless 子句成分, 当一个 if 或 when 测试产生[yield] false 或者当一个 unless 测试产生[yield] true 的时候被执行. 这个成分是 if 下面描述的子句之一.

这个 loop end 构造提供一个可选的成分用于标记一个条件子句的结束.

关于更多信息, 见章节 6.1.6 (条件执行子句). 

##### 6.1.1.5.6 <span id="SummaryMiscellaneousClauses">其他子句综述</span>

这个 loop named 构造为这个 loop 的块[block]提供一个名字.

这个 loop initially 构造导致它的那些表达式形式[form]在循环序言中被求值, 也就是在除了由 with, for, or as 构造提供的初始设置之外的所有 loop 代码之前被求值.

这个 loop finally 构造导致它的表达式形式[form]在循环结尾中被求值, 也就是在正常循环终止后.

关于更多信息, 见章节 6.1.7 (其他子句). 


#### 6.1.1.6 <span id="OrderExecution">执行顺序</span>

除了下面列出的异常情况, 子句在循环体中以它们出现在源码中的顺序执行. 重复执行直到一个子句终止了这个 loop 或者直到遇到一个 return, go, 或 throw 表达式形式转移控制到 loop 外的一个点. 下面这些操作是执行的线性顺序的例外:

* 所有变量首先被初始化, 不管建立的子句出现在源码中的什么位置. 初始化顺序遵循这些子句的顺序.

* 任何 initially 子句的代码以它们出现在源码中的顺序被集中到一个 progn 中. 这个集中后的代码只在任何隐式变量初始化之后在循环序言中执行一次.

* 任何 finally 子句的代码以它们出现在源码中的顺序被集中到一个 progn 中. 这个集中后的代码只在任何隐式的来自累积子句的值被返回之前在循环结尾被执行一次. 但是, 在源码中的任何地方显式的返回会在没有执行结尾代码的情况下退出这个 loop.

* 一个 with 子句引入一个变量绑定[binding]和一个可选的初始值. 这个初始值会按照 with 子句出现的顺序被计算.

* 循环控制子句隐式执行下面这些动作:

    -- 初始化变量;

    -- 步进[step]变量, 通常在每个循环体执行之间;

    -- 执行终止检验, 通常只是在循环体执行之前. 


#### 6.1.1.7 <span id="Destructuring">解构</span>

这个 d-type-spec 参数被用于解构. 如果这个 d-type-spec 参数仅仅由类型[type] fixnum, float, t, 或 nil 构成, 那么这个 of-type 关键字是可选的. 在这些情况下, of-type 构造是可选的, 以提供向后兼容性; 因此, 下面两个表达式是一样的:

```LISP
;;; This expression uses the old syntax for type specifiers.
 (loop for i fixnum upfrom 3 ...)
 
;;; This expression uses the new syntax for type specifiers.
 (loop for i of-type fixnum upfrom 3 ...)

;; Declare X and Y to be of type VECTOR and FIXNUM respectively.
 (loop for (x y) of-type (vector fixnum) 
       in l do ...)
```

用于解构匹配模式的类型指定符[type specifier]是一种类型指定符[type specifier]的树[tree], 其形状与变量[variable]名[name]的树[tree]的形状相同, 但是具有以下例外:

* 当对齐这些树[tree]的时候, 和变量树中的一个 cons 匹配的类型指定符[type specifier]的树[tree]中的一个原子[atom]声明了以 cons 为根的子树中的每一个变量为相同的类型[type].

* 和变量[variable]名称[name]的树[tree]中的一个原子[atom]的匹配的类型指定符[type specifier]的树中的一个 cons 是一个复合类型指定符[compound type specifer].

在一个值可以正常地绑定到一个单独变量的任何地方, 解构允许将一组变量绑定[binding]为相应的一组值. 在 loop 展开期间, 在变量列表中的每个变量和值列表中的值匹配. 如果变量列表中的变量数超过值列表中的值的数量, 剩余的变量会被赋予 nil 值. 如果值的数量超过列出的变量数, 多余的值会被丢弃.

为了将一个列表中的值赋给变量 a, b, 还有 c, 这个 for 子句可以被用于绑定变量 numlist 到这个提供的表达式形式 form 的 car 部分, 而另一个 for 子句可以被用于顺序地[sequentially]绑定变量 a, b, 和 c.

```LISP
;; Collect values by using FOR constructs.
(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      for a of-type integer = (first numlist)
      and b of-type integer = (second numlist)
      and c of-type float = (third numlist)
      collect (list c b a))
=>  ((4.0 2 1) (8.3 6 5) (10.4 9 8))
```

解构通过允许变量在每个循环迭代中被绑定来使这个过程变得更容易. 类型[type]可以通过使用一个 type-spec 参数的列表来声明. 如果所有的类型[type]都是相同的, 可以使用一个简写解构语法, 就像第二个例子说明的.

```LISP
;; Destructuring simplifies the process.
(loop for (a b c) of-type (integer integer float) in
      '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      collect (list c b a))
=>  ((4.0 2 1) (8.3 6 5) (10.4 9 8))


;; If all the types are the same, this way is even simpler.
(loop for (a b c) of-type float in
      '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
      collect (list c b a))
=>  ((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0))
```

如果解构被用来声明或初始化多个变量组到类型, 可以使用 loop 关键字[loop keyword] and 来进一步简化这个过程.

```LISP
;; Initialize and declare variables in parallel by using the AND construct.
(loop with (a b) of-type float = '(1.0 2.0)
      and (c d) of-type integer = '(3 4)
      and (e f)
      return (list a b c d e f))
=>  (1.0 2.0 3 4 NIL NIL)
```

如果在一个解构列表中使用 nil, 不会为它的位置提供一个变量.

```LISP
(loop for (a nil b) = '(1 2 3)
      do (return (list a b)))
=>  (1 3)
```

注意这个点对列表[dotted list]可以指定解构.

```LISP
(loop for (x . y) = '(1 . 2)
      do (return y))
=>  2
(loop for ((a . b) (c . d)) of-type ((float . float) (integer . integer)) in
      '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b c d))
=>  ((1.2 2.4 3 4) (3.4 4.6 5 6))
```

如果同一个变量在一个单独的 loop 表达式的任何变量绑定子句中被绑定两次, 会发出一个 program-error 类型[type]的错误(在宏展开的时候). 这些变量包括局部变量, 循环控制变量和通过解构找到的变量. 

#### 6.1.1.8 <span id="RestrictionsSideEffects">副作用的限制</span>

见章节 3.6 (遍历规则和副作用). 

### 6.1.2 <span id="VarInitAndStepClauses">变量初始化和步进子句</span>

> * 6.1.2.1 [循环控制](#IterationControl)
> * 6.1.2.2 [局部变量初始化](#LocalVarInit)

#### 6.1.2.1 <span id="IterationControl">循环控制</span>

循环控制子句允许有 loop 循环的方向. loop 关键字[loop keyword] for 和 as 指定循环控制子句. 循环控制子句的区别在于终止检验的说明以及 loop 变量的初始化和步进[step[1]]. 循环子句自身不会导致这个 loop 工具返回一些值, 但是它们可以和值累积子句协同使用来返回那些值.

所有变量都在循环序言中被初始化. 一个变量[variable]绑定[binding]有着词法作用域[lexical scope]除非它被公告为 special; 因此, 默认情况下, 这个变量只能被文本形式出现在 loop 中的那些表达式形式[form]访问. 在其他表达式形式[form]在主体中被求值之前, 在 loop 主体中进行步进任务.

循环控制子句中的变量参数可以是一个解构列表. 一个解构列表是一个非 nil [non-nil]原子是变量[variable]名称[name]的树[tree]. 见章节 6.1.1.7 (解构).

循环控制子句 for, as, 和 repeat 必须在除了 initially, with, 和 named 以外的其他 loop 子句之前, 因为它们建立变量绑定[binding]. 当循环控制子句被用于一个 loop 中时, 这个 loop 主体中的对应终止测试在任何其他 loop 主体代码被执行前求值.

如果多个循环子句被用于控制循环, 变量初始化和步进[step[1]]默认是顺序[sequentially]发生的. 当没有必要进行顺序[sequential]绑定[binding]和步进[step[1]]时, 这个 and 构造可以被用于连接两个或更多循环子句. 被 and 加入的子句的循环行为类似于宏 do 相对于 do* 的行为.

这个 for 和 as 子句通过使用一个或多个局部循环变量来循环, 这些变量被初始化为某个值并且可以在每次循环后被修改或步进[step[1]]. 对于这些子句, 当一个局部变量达到某个被提供的值时或者当某个其他 loop 子句终止循环时, 循环会停止. 在每次循环中, 变量可以通过一个递增或递减来步进[step[1]]或通过一个表达式形式[form]的求值来赋予新值. 在循环期间可以利用解构来给变量赋值.

这个 for 和 as 关键字是是同义词; 它们可以被交换使用. 对于这些构造, 有七种语法格式. 在每一个语法格式中, var 的类型[type]可以通过这个可选的 type-spec 参数来提供. 如果 var 是一个解构列表 list, 通过 type-spec 参数提供的类型[type]必须适当地匹配列表中的元素. 按照惯例, for 引入新的循环而 as 引入的循环依赖于前面循环说明.

> * 6.1.2.1.1 [for-as-arithmetic 分子句](#FAARSubclause)
> * 6.1.2.1.2 [for-as-in-list 分子句](#FAILSubclause)
> * 6.1.2.1.3 [for-as-on-list 分子句](#FAOLSubclause)
> * 6.1.2.1.4 [for-as-equals-then 分子句](#FAETSubclause)
> * 6.1.2.1.5 [for-as-across 分子句](#FAACSubclause)
> * 6.1.2.1.6 [for-as-hash 分子句](#FAHSubclause)
> * 6.1.2.1.7 [for-as-package 分子句](#FAPSubclause)


##### 6.1.2.1.1 <span id="FAARSubclause">for-as-arithmetic 分子句</span>

在 for-as-arithmetic 分子句中, 这个 for 或者 as 构造从 form1 提供的值以 form3 表示的递增或递减循环到 form2 提供的值. 每个表达式只被求值一次并且必须被求值为一个数字[number]. 变量 var 在第一次循环中被绑定为 form1 的值并且在随后的每次循环中步进[step[1]] form3 的值, 如果 form3 没有提供的话就是步进 1. 下面的 loop 关键字[loop keyword]被当作是这个语法中合法的介词. 必须使用至少一个介词; 在一个单独的分子句中下面的每一行最多使用一个.

    from | downfrom | upfrom

    to | downto | upto | below | above

    by

每个分子句中的介词短语可能以任何顺序出现. 比如, 不管是 "from x by y" 或是 "by y from x" 都是允许的. 然而, 因为保留从左到右的求值顺序, 在副作用上的效果可能会是不同的. 细想:

```LISP
(let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
=>  (1 3 5 7 9)
(let ((x 1)) (loop for i by (incf x) from x to 10 collect i))
=>  (2 4 6 8 10)
```

以下是介词的描述:

* from

        loop 关键字[loop keyword] from 指定步进[step[1]]开始的值, 由 form1 提供. 步进[step[1]]默认是递增的. 如果需要递减的步进[step[1]], 介词 downto 或 above 必须和 form2 一起使用. 对于递增步进[step[1]], 默认的 from 值是 0.

* downfrom, upfrom

        loop 关键字[loop keyword] downfrom 表示变量 var 通过 from3 提供的衰减量递减; loop 关键字[loop keyword] upfrom 表示 var 通过 form3 提供的增量递增.

* to

        loop 关键字[loop keyword] to 标记由 form2 提供的步进[step[1]]的结束值. 步进[step[1]]默认是递增的. 如果需要递减的步进[step[1]], 介词 downfrom 必须和 form1 一起使用, 或者和 form2 一起使用介词 downto 或 above.

* downto, upto

        loop 关键字[loop keyword] downto 指定递减步进[step]; loop 关键字[loop keyword] upto 指定递增步进[step]. 在这两种情况下, 每一步改变的数额由 form3 指定, 当变量 var 超过 form2 的值时这个 loop 终止. 因为这里没有为递减步进[step[1]]提供 form1 的默认值, 所以当 downto 被提供时也必须提供 form1 的值(使用 from 或 downfrom).

* below, above

        loop 关键字[loop keyword] below 和 above 分别与 upto 和 downto 类似. 这些关键字仅仅在变量 var 的值达到 form2 提供的值前停止循环; 这个 form2 的结束值不会被包含进去. 因为这里没有为递减步进[step[1]]提供 form1 的默认值, 当 above 被提供时也必须提供 form1 的值(使用 from 或 downfrom).

* by

        loop 关键字[loop keyword] by 标记 form3 提供的递增或递减值. 这个 form3 的值可以是任何正数[number]. 默认值是 1.

在一个循环控制子句中, 当到达提供的限制时, 这个 for 或 as 构造导致终止. 这也就是说, 循环一直持续到值 var 被逐步执行到 form2 提供的排他性或包容性的限制. 如果 form3 递增或递减变量到 form2 的值而没有到达那个值那么这个范围就是排他的; loop 关键字 below 和 above 提供排他的限制. 一个包容的限制允许 var 达到 form2 的值; to, downto, 和 upto 提供包容的限制.

###### 6.1.2.1.1.1 for-as-arithmetic 分子句的示例

```LISP
;; Print some numbers.
 (loop for i from 1 to 3
       do (print i))
>>  1
>>  2
>>  3
=>  NIL
 
;; Print every third number.
 (loop for i from 10 downto 1 by 3
       do (print i))
>>  10 
>>  7 
>>  4 
>>  1 
=>  NIL
 
;; Step incrementally from the default starting value.
 (loop for i below 3
       do (print i))
>>  0
>>  1
>>  2
```


##### 6.1.2.1.2 <span id="FAILSubclause">for-as-in-list 分子句</span>

在这个 for-as-in-list 分子句中, 这个 for 或 as 构造遍历一个列表[list]的内容. 它检查这个列表[list]的末尾, 就像使用 endp 一样. 在每次循环前变量 var 被绑定为 form1 中列表[list]的连续元素. 在每次循环结束后, 函数 step-fun 被应用到这个列表[list]; step-fun 的默认值是 cdr. loop 关键字[loop keyword] in 和 by 在这个语法中是合法的介词. 当到达这个列表[list]的末尾时, 这个 for 或 as 构造就终止.

###### 6.1.2.1.2.1 for-as-in-list 分子句的示例

```LISP
;; Print every item in a list.
 (loop for item in '(1 2 3) do (print item))
>>  1
>>  2
>>  3
=>  NIL
 
;; Print every other item in a list.
 (loop for item in '(1 2 3 4 5) by #'cddr
       do (print item))
>>  1
>>  3
>>  5
=>  NIL
 
;; Destructure a list, and sum the x values using fixnum arithmetic.
 (loop for (item . x) of-type (t . fixnum) in '((A . 1) (B . 2) (C . 3))
       unless (eq item 'B) sum x)
=>  4
```

##### 6.1.2.1.3 <span id="FAOLSubclause">for-as-on-list 分子句</span>

在这个 for-as-on-list 分子句中, for 或 as 构造遍历一个列表[list]. 它检查这个列表[list]的末尾, 就像使用 atom 一样. 变量 var 被绑定为 form1 中的列表[list]的后续尾部. 在每次循环后, 函数 step-fun 被应用到这个列表[list]; 这个 step-fun 的默认值是 cdr. loop 关键字[loop keyword] on 和 by 在这个语法中被当作合法的介词. 当到达这个列表[list]的末尾时, 这个 for 或 as 构造就终止.

###### 6.1.2.1.3.1 for-as-on-list 分子句的示例

```LISP
;; Collect successive tails of a list.
 (loop for sublist on '(a b c d)
       collect sublist)
=>  ((A B C D) (B C D) (C D) (D))
 
;; Print a list by using destructuring with the loop keyword ON.
 (loop for (item) on '(1 2 3)
       do (print item))
>>  1 
>>  2 
>>  3 
=>  NIL
```

##### 6.1.2.1.4 <span id="FAETSubclause">for-as-equals-then 分子句</span>

在这个 for-as-equals-then 分子句中, 这个 for 或者 as 构造通过设置第一次循环时 form1 的求值结果到变量 var 来初始化它, 然后在第二次及后续的循环中将它设置为 form2 的求值结果. 如果省略了 form2, 这个构造在第二次及后续的循环中就使用 form1. loop 关键字[loop keyword] = 和 then 在这个语法中被当作合法介词. 这个构造不会提供任何的终止检验.

###### 6.1.2.1.4.1 for-as-equals-then 分子句的示例

```LISP
;; Collect some numbers.
(loop for item = 1 then (+ item 10)
      for iteration from 1 to 5
      collect item)
=>  (1 11 21 31 41)
```

##### 6.1.2.1.5 <span id="FAACSubclause">for-as-across 分子句</span>

在这个 for-as-across 分子句中, 这个 for 或 as 构造绑定变量 var 到这个数组 vector 的每一个元素的值. loop 关键字[loop keyword] across 标记这个数组 vector; across 在这个语法中被用作一个介词. 当提供的数组[array]中没有更多数据可以被引用时循环停止. 某些具体实现可能识别一个 vector 表达式形式中的 the 特殊表达式形式来产生更高效的代码.

###### 6.1.2.1.5.1 for-as-across 分子句的示例

```LISP
(loop for char across (the simple-string (find-message channel))
      do (write-char char stream))
```

##### 6.1.2.1.6 <span id="FAHSubclause">for-as-hash 分子句</span>

在这个 for-as-hash 分子句中, 这个 for 或 as 构造遍历一个哈希表[hash-table]的元素, 键, 还有值. 在这个语法中, 一个复合介词用于指定对哈希表的访问. 变量 var 呈现为提供的哈希表 hash-table 中的每一个哈希键或者哈希值. 在这个语法中以下 loop 关键字[loop keyword]被当作合法介词:

* being

        关键字 being 引入了这个 Loop 模式 hash-key 或 hash-value.

* each, the

        当 hash-key 或 hash-value 被使用时, loop 关键字[loop keyword] each 跟在 loop 关键字[loop keyword] being 后面. loop 关键字[loop keyword] the 和 hash-keys 还有 hash-values 一起使用仅为了便于阅读. 这个协议不是必须的.

* hash-key, hash-keys

        这些 loop 关键字[loop keyword]访问这个哈希表[hash table]的每个键条目. 如果 using 构造中提供名字 hash-value 和这些 Loop 模式中的一个, 那么这个循环可以选择性的访问这些键表示的值. 这些键访问的顺序是没有定义的; 哈希表[hash table]中空的槽会被忽略.

* hash-value, hash-values

        这些 loop 关键字[loop keyword]访问这个哈希表[hash table]的每个值条目. 如果 using 构造中提供名字 hash-key 和这些 Loop 模式中的一个, 这个循环可以选择性的访问这些值对应的键. 这些键访问的顺序是没有定义的; 哈希表[hash table]中空的槽会被忽略.

* using

        这个 loop 关键字[loop keyword] using 引入用于访问的可选的键或键表示的值. 如果循环到了哈希值, 那么它允许访问哈希键, 如果循环到了哈希键, 则可以访问哈希值.

* in, of

        这些 loop 介词引入 hash-table.

实际上

    being {each | the} {hash-value | hash-values | hash-key | hash-keys} {in | of}

是一个复合介词.

当提供的 hash-table 中没有更多的哈希键或哈希值要被引用时, 循环停止. 


##### 6.1.2.1.7 <span id="FAPSubclause">for-as-package 分子句</span>

在这个 for-as-package 分子句中 for 或 as 构造遍历一个包[package]中的符号[symbol]. 在这个语法中, 使用一个复合的介词来指定对一个包[package]的访问. 变量 var 接收这个提供的包[package]中每一个符号[symbol]的值. 在这个语法中下面的 loop 关键字[loop keyword]被当作合法的介词:

* being

        关键字 being 引入 Loop 模式 symbol, present-symbol, 或 external-symbol.

* each, the

        当使用 symbol, present-symbol, 或 external-symbol 时 loop 关键字[loop keyword] each 跟在 loop 关键字[loop keyword] being 后面. loop 关键字[loop keyword] the 和 symbols, present-symbols, 还有 external-symbols 一起使用仅方便于阅读. 这个参数不是必须的.

* present-symbol, present-symbols

        这些 Loop 模式遍历出现[present]在一个包[package]中的符号[symbol]. 要被循环的包 package 以提供给 find-package 的包[package]参数相同的方式被提供. 如果没有提供用于循环的包 package, 就使用当前包[current package]. 如果提供一个不存在的包 package, 会发出一个 package-error 类型[type]的错误.

* symbol, symbols

        这些 Loop 模式遍历这个给定包 package 中可访问的[accessible]符号[symbol]. 要被循环的包 package 以提供给 find-package 的包[package]参数相同的方式被提供. 如果没有提供用于循环的包 package, 就使用当前包[current package]. 如果提供一个不存在的包 package, 会发出一个 package-error 类型[type]的错误.

* external-symbol, external-symbols

        这些 Loop 模式遍历一个包 package 中的外部符号[external symbol]. 要被循环的包以提供给 find-package 的包参数相同的方式被提供. 如果没有提供用于循环的包, 就使用当前包. 如果提供一个不存在的包, 会发出一个 package-error 类型[type]的错误.

* in, of

        这些 loop 介词引入 package.

实际上

    being {each | the} {symbol | symbols | present-symbol | present-symbols | external-symbol | external-symbols} {in | of}

是一个复合介词.

当提供的包 package 中没有更多符号[symbol]要被引用时循环停止.

###### 6.1.2.1.7.1 for-as-package 分子句

```LISP
(let ((*package* (make-package "TEST-PACKAGE-1")))
  ;; For effect, intern some symbols
  (read-from-string "(THIS IS A TEST)")
  (export (intern "THIS"))
  (loop for x being each present-symbol of *package*
        do (print x)))
>>  A 
>>  TEST 
>>  THIS
>>  IS 
=>  NIL
```

#### 6.1.2.2 <span id="LocalVarInit">局部变量初始化</span>

当执行一个 loop 表达式形式[form]时, 局部变量会被绑定并且初始化到某个值. 这些局部变量直到 loop 循环结束都会存在, 在那个时候它们会消失. 隐式的变量也通过循环控制子句和累积子句的 into 介词来建立.

with 构造初始化的变量是一个 loop 的局部变量. 这个变量只被初始化一次. 如果为这个变量 var 提供了可选的 type-spec 参数, 而这里没有相关的表达式来求值, var 被初始化为它类型[type]的一个合适的默认值. 比如, 对于类型 t, number, 和 float, 默认值分别就是 nil, 0, 和 0.0. 如果为 var 提供了一个 type-spec 参数而相关表达式返回的值不是那个提供的类型[type], 那么结果是未定义的. 默认情况下, with 构造顺序[sequentially]初始化变量; 这也就是说, 一个变量在下一个表达式被求值之间被赋值. 然而, 通过使用 loop 关键字[loop keyword] and 连接几个 with 子句, 初始化可以强制并行[parallel]发生; 这也就是说, 提供的所有表达式形式都被求值, 并且结果被同时绑定给对应变量.

当需要某些变量的初始化依赖于前面绑定变量的值时, 就使用顺序[sequential]绑定[binding]. 比如, 假设变量 a, b, 和 c 被依次绑定:

```LISP
(loop with a = 1 
      with b = (+ a 2) 
      with c = (+ b 3)
      return (list a b c))
=>  (1 3 6)
```

上面的 loop 的执行等价于下面代码的执行:

```LISP
(block nil
  (let* ((a 1)
        (b (+ a 2))
        (c (+ b 3)))
    (tagbody
        (next-loop (return (list a b c))
                  (go next-loop)
                  end-loop))))
```

如果初始化其他变量不需要之前绑定的变量的值, 一个 and 子句可以被用于指定这些绑定并行[parallel]地发生:

```LISP
(loop with a = 1 
      and b = 2 
      and c = 3
      return (list a b c))
=>  (1 2 3)
```

上面的 loop 的执行等价于下面代码的执行:

```LISP
(block nil
  (let ((a 1)
        (b 2)
        (c 3))
    (tagbody
        (next-loop (return (list a b c))
                  (go next-loop)
                  end-loop))))
```

##### 6.1.2.2.1 WITH 子句的示例

```LISP
;; These bindings occur in sequence.
(loop with a = 1 
      with b = (+ a 2) 
      with c = (+ b 3)
      return (list a b c))
=>  (1 3 6)

;; These bindings occur in parallel.
(setq a 5 b 10)
=>  10
(loop with a = 1
      and b = (+ a 2)
      and c = (+ b 3)
      return (list a b c))
=>  (1 7 13)

;; This example shows a shorthand way to declare local variables 
;; that are of different types.
(loop with (a b c) of-type (float integer float)
      return (format nil "~A ~A ~A" a b c))
=>  "0.0 0 0.0"

;; This example shows a shorthand way to declare local variables 
;; that are the same type.
(loop with (a b c) of-type float 
      return (format nil "~A ~A ~A" a b c))
=>  "0.0 0.0 0.0"
```

### 6.1.3 <span id="ValueAccumulationClauses">值累积子句</span>

构造 collect, collecting, append, appending, nconc, nconcing, count, counting, maximize, maximizing, minimize, minimizing, sum, 和 summing, 允许在一个 loop 中累积值.

构造 collect, collecting, append, appending, nconc, 还有 nconcing, 用于指定将这些值累积到列表[list]中并返回它们的子句. 构造 count, counting, maximize, maximizing, minimize, minimizing, sum, 和 summing 指定累积并返回数字值的子句.

在每一次循环期间, 构造 collect 和 collecting 收集提供的表达式形式 form 的值到一个列表[list]中. 循环停止时, 返回这个列表[list]. 参数 var 被设置为收集值的列表[list]; 如果提供了 var, 这个 loop 不会自动返回最终的列表[list]. 如果 var 没有被提供, 等价于为 var 提供一个内部的名字并且在一个 finally 子句中返回它的值. 这个 var 参数就像是通过 with 构造一样被绑定. 没有为声明 var 的类型[type]提供机制; 它必须是类型[type] list.

除了提供的表达式形式 form 的那些值一定是列表[list]之外, 构造 append, appending, nconc, 和 nconcing 类似于 collect.

* append 关键字导致它的列表[list]值被连接到一个单独的列表[list]中, 就好像它们是给函数[function] append 的参数一样.

* nconc 关键字导致它的列表[list]值被连接到一个单独的列表[list]中, 就好像它们是给函数[function] nconc 的参数一样.

参数 var 被设置为连接的值的列表[list]; 如果提供了 var, loop 不会自动返回最终的列表[list]. var 参数就像是通过 with 构造一样被绑定. 不能为 var 提供一个类型; 它必须是 list 类型. 构造 nconc 破坏性地修改它的参数列表.

count 构造计算提供的表达式形式 form 返回 true 的次数. 参数 var 累计发生的次数; 如果提供了 var, loop 不会自动返回最终的计数. var 参数就像是通过 with 构造一样被绑定到一个 0 的适当的类型. 后面的值 (包括任何必要的强制转换) 就像是通过函数[function] 1+ 一样被计算. 如果使用了 into var, 可以用 type-spec 参数为 var 提供一个类型[type]; 如果提供了一个不是数字的类型[type], 那么结果是不可预料的. 如果这里没有 into 变量, 可选的 type-spec 参数应用给保留这个计数的内部变量. 默认类型[type]是依赖于具体实现的[implementation-dependent]; 但是它一定是 fixnum 的子类型[subtype].

maximize 和 minimize 构造比较第一次循环提供的表达式形式 form 的值和后续循环获取到的值. 确定遇到的最大 (对于 maximize) 或最小 (对于 minimize) 值 (对于 maximize 就像是通过函数[function] max, 对于 minimize 就像是通过函数[function] min) 并且返回. 如果这个 maximize 或 minimize 子句从来没有执行, 累计的值是未指定的. 参数 var 累计这个最大或最小值; 如果提供了 var, loop 不会自动返回这个最大值或最小值. 参数 var 就像是通过 with 构造一样被绑定. 如果使用了 into var, 可以使用 type-spec 参数为 var 指定一个类型[ty[e]]; 如果指定了非数字的类型[type], 那么结果是未定义的. 如果这也里没有 into 变量, 可选的 type-spec 参数应用给保留这个最大或最小值的内部变量. 默认类型[type]是依赖于具体实现的[implementation-dependent]; 但是它一定是 real 的子类型[subtype].

sum 构造在每次循环中形成了所提供 form 的连续主值[primary values]的累积总和. 参数 var 被用于累计这个总和; 如果提供了 var, loop 不会自动返回最终的总和. 参数 var 就像是通过 with 构造一样被绑定到一个 0 的适当类型. 后面的值 (包括任何必要的强制转换) 就像是通过函数[function] + 一样被计算. 如果使用了 into var, 可以使用 type-spec 参数为 var 指定一个类型[type]; 如果指定了非数字的类型[type], 那么结果是未定义的. 如果这也里没有 into 变量, 可选的 type-spec 参数应用给保留这个总和的内部变量. 默认类型[type]是依赖于具体实现的[implementation-dependent]; 但是它一定是 number 的子类型[subtype].

如果使用了 into, 这个构造不会提供默认的返回值; 然而, 在任何 finally 子句中这个变量都是可用的.

如果在一个 loop 中的累积子句它们的目标是相同的(loop 的结果或者一个 into var), 那么适当种类的累积子句可以被合并, 因为它们被认为累积概念上兼容的量. 尤其是, 在一个 loop 表达式形式[form]中, 下列集合子句的任何元素都可以与同一目标的其他元素混合在一起:

* collect, append, nconc

* sum, count

* maximize, minimize

```LISP
;; Collect every name and the kids in one list by using 
;; COLLECT and APPEND.
(loop for name in '(fred sue alice joe june)
      for kids in '((bob ken) () () (kris sunshine) ())
      collect name
      append kids)
=>  (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)
```

在一个 loop 中当且仅当每个子句累积它们的值到不同变量[variable]时, 任何两个不累积相同类型[type]对象[object]的子句可以共存.

> * 6.1.3.1 [COLLECT 子句的示例](#ExamplesCOLLECTClause)
> * 6.1.3.2 [APPEND 和 NCONC 子句的示例](#ExamplesAPPENDNCONCClauses)
> * 6.1.3.3 [COUNT 子句的示例](#ExamplesCOUNTClause)
> * 6.1.3.4 [MAXIMIZE 和 MINIMIZE 子句的示例](#ExamplesMAXIMIZEMINIMIZEClauses)
> * 6.1.3.5 [SUM 子句的示例](#ExamplesSUMClause)

#### 6.1.3.1 <span id="ExamplesCOLLECTClause">COLLECT 子句的示例</span>

```LISP
;; Collect all the symbols in a list.
(loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
      when (symbolp i) collect i)
=>  (BIRD TURTLE HORSE CAT)

;; Collect and return odd numbers.
(loop for i from 1 to 10
      if (oddp i) collect i)
=>  (1 3 5 7 9)

;; Collect items into local variable, but don't return them.
(loop for i in '(a b c d) by #'cddr
      collect i into my-list
      finally (print my-list))
>>  (A C) 
=>  NIL
```

#### 6.1.3.2 <span id="ExamplesAPPENDNCONCClauses">APPEND 和 NCONC 子句的示例</span>

```LISP
;; Use APPEND to concatenate some sublists.
(loop for x in '((a) (b) ((c)))
      append x)
=>  (A B (C))

;; NCONC some sublists together.  Note that only lists made by the
;; call to LIST are modified.
(loop for i upfrom 0 
      as x in '(a b (c))
      nconc (if (evenp i) (list x) nil))
=>  (A (C))
```

#### 6.1.3.3 <span id="ExamplesCOUNTClause">COUNT 子句的示例</span>

```LISP
(loop for i in '(a b nil c nil d e)
      count i)
=>  5
```

#### 6.1.3.4 <span id="ExamplesMAXIMIZEMINIMIZEClauses">MAXIMIZE 和 MINIMIZE 子句的示例</span>

```LISP
(loop for i in '(2 1 5 3 4)
      maximize i)
=>  5
(loop for i in '(2 1 5 3 4)
      minimize i)
=>  1

;; In this example, FIXNUM applies to the internal variable that holds
;; the maximum value.
(setq series '(1.2 4.3 5.7))
=>  (1.2 4.3 5.7)
(loop for v in series 
      maximize (round v) of-type fixnum)
=>  6

;; In this example, FIXNUM applies to the variable RESULT.
(loop for v of-type float in series
      minimize (round v) into result of-type fixnum
      finally (return result))
=>  1
```

#### 6.1.3.5 <span id="SUM 子句的示例">SUM 子句的示例</span>

```LISP
(loop for i of-type fixnum in '(1 2 3 4 5)
      sum i)
=>  15
(setq series '(1.2 4.3 5.7))
=>  (1.2 4.3 5.7)
(loop for v in series 
      sum (* 2.0 v))
=>  22.4
```

### 6.1.4 <span id="TerminationTestClauses">终止测试子句</span>

repeat 构造导致循环在指定的次数后终止. 这个 loop 主体执行了 n 次, 其中 n 是表达式 form 的值. 这个 form 参数只在循环序言中求值一次. 如果这个表达式求值为 0 或者是一个负数[number], 这个 loop 主体不会被求值.

构造 always, never, thereis, while, until, 和宏 loop-finish 允许一个 loop 中循环的条件终止.

构造 always, never, 和 thereis 提供那些当一个 loop 终止时要被返回的值. 在一个带有不是 into 子句的值累积子句的 loop 中使用 always, never, 或 thereis, 会发出一个 program-error 类型[type]的错误 (在宏展开期间). 由于 always, never, 和 thereis 使用 return-from 特殊操作符[special operator]来终止循环, 当由于这些构造中的任何一个导致退出时, 提供的任何 finally 子句不会被求值. 在所有方面这些构造都表现地像 while 和 until 构造.

always 构造接收一个表达式形式[form]并且如果这个表达式形式[form]曾求值为 nil 就终止这个 loop; 在这个情况下, 它返回 nil. 否则, 它提供一个默认返回值 t. 如果提供的表达式形式[form]的从来不返回 nil, 某个其他构造可以终止这个循环.

never 构造在提供的表达式形式 form 的值第一次不是 nil [non-bil]的时候终止循环; 这个 loop 返回 nil. 如果这个提供的表达式形式 form 总是为 nil, 某个其他构造可以终止这个循环. 除非某个其他子句提供一个返回值, 否则默认返回值就是 t.

thereis 构造在提供的表达式形式 form 的值第一次不是 nil [non-bil]的时候终止循环; 这个 loop 返回提供的表达式形式 form 的值. 如果这个提供的表达式形式 form 的值总是为 nil, 某个其他构造可以终止这个循环. 除非某个其他子句提供一个返回值, 否则默认返回值就是 nil.

在 thereis 和 until 构造中有两个区别:

* until 构造不会基于提供的表达式形式 form 的值返回一个值或者 nil.

* until 构造执行任何 finally 子句. 由于 thereis 使用 return-from 特殊操作符[special operator]来终止循环, 当 thereis 导致退出时, 任何提供的 finally 子句不会被求值.

while 构造允许循环直到提供的表达式形式 form 求值为 false 之前继续下去. 提供的表达式形式 form 在 while 子句的位置重复求值.

until 构造等价于 while (not form).... 如果提供表达式形式 form 的值不是 nil [non-nil], 循环终止.

终止测试控制构造可以被用于 loop 主体的任何位置. 终止测试以它们出现的顺序被使用. 如果一个 until 或 while 子句导致终止, 任何在源代码中先于它的子句都被求值. 如果 until 和 while 构造导致了终止, 控制会传递到 loop 结尾, 其中任何 finally 子句会被执行.

在 never 和 until 构造中有两个差别:

* until 构造不会返回基于提供表达式形式 form 的值的 t 或 nil.

* until 构造不会忽视任何 finally 子句. 由于 never 使用 return-from 特殊操作符[special operator]来终止循环, 任何提供的 finally 子句在 never 导致退出时不会被求值.

在大部分情况下没有必要去使用 loop-finish 因为其他 loop 控制子句会终止这个 loop. 宏 loop-finish 被用于提供一个从 loop 中的嵌套条件句中正常的退出. 由于 loop-finish 转移控制到 loop 结尾, 在一个 finally 表达式中使用 loop-finish 会导致无穷的循环.

> * 6.1.4.1 [REPEAT 子句的示例](#ExamplesREPEATClause)
> * 6.1.4.2 [ALWAYS, NEVER, 和 THEREIS 子句的示例](#ExamplesANTClauses)
> * 6.1.4.3 [WHILE 和 UNTIL 子句的示例](#ExamplesWHILEUNTILClauses)

#### 6.1.4.1 <span id="ExamplesREPEATClause">REPEAT 子句的示例</span>

```LISP
(loop repeat 3
      do (format t "~&What I say three times is true.~%"))
>>  What I say three times is true.
>>  What I say three times is true.
>>  What I say three times is true.
=>  NIL
(loop repeat -15
  do (format t "What you see is what you expect~%"))
=>  NIL
```

#### 6.1.4.2 <span id="ExamplesANTClauses">ALWAYS, NEVER, 和 THEREIS 子句的示例</span>

```LISP
;; Make sure I is always less than 11 (two ways).
;; The FOR construct terminates these loops.
(loop for i from 0 to 10
      always (< i 11))
=>  T
(loop for i from 0 to 10
      never (> i 11))
=>  T

;; If I exceeds 10 return I; otherwise, return NIL.
;; The THEREIS construct terminates this loop.
(loop for i from 0
      thereis (when (> i 10) i) )
=>  11

;;; The FINALLY clause is not evaluated in these examples.
(loop for i from 0 to 10
      always (< i 9)
      finally (print "you won't see this"))
=>  NIL
(loop never t
      finally (print "you won't see this"))
=>  NIL
(loop thereis "Here is my value"
      finally (print "you won't see this"))
=>  "Here is my value"

;; The FOR construct terminates this loop, so the FINALLY clause 
;; is evaluated.
(loop for i from 1 to 10
      thereis (> i 11)
      finally (prin1 'got-here))
>>  GOT-HERE
=>  NIL

;; If this code could be used to find a counterexample to Fermat's
;; last theorem, it would still not return the value of the
;; counterexample because all of the THEREIS clauses in this example
;; only return T.  But if Fermat is right, that won't matter
;; because this won't terminate.

(loop for z upfrom 2
      thereis
        (loop for n upfrom 3 below (log z 2)
              thereis
                (loop for x below z
                      thereis
                        (loop for y below z
                              thereis (= (+ (expt x n) (expt y n))
                                        (expt z n))))))
```

#### 6.1.4.3 <span id="ExamplesWHILEUNTILClauses">WHILE 和 UNTIL 子句的示例</span>

```LISP
(loop while (hungry-p) do (eat))

;; UNTIL NOT is equivalent to WHILE.
(loop until (not (hungry-p)) do (eat))

;; Collect the length and the items of STACK.
(let ((stack '(a b c d e f)))
  (loop for item = (length stack) then (pop stack)
        collect item
        while stack))
=>  (6 A B C D E F)

;; Use WHILE to terminate a loop that otherwise wouldn't terminate.
;; Note that WHILE occurs after the WHEN.
(loop for i fixnum from 3
      when (oddp i) collect i
      while (< i 5))
=>  (3 5)
```

### 6.1.5 <span id="UnconditionalExecutionClauses">无条件执行子句</span>

do 和 doing 构造无论出现在 loop 的展开形式的什么位置都会求值提供的那些 forms. 这个 form 参数可以是任何复合表达式形式[compound form]. 每个表达式形式 form 在每次循环都被求值. 因为每个 loop 子句必须以一个 loop 关键字[loop keyword]开始, 关键字 do 是在没有控制动作只需要执行的情况下使用的.

return 构造接收一个表达式形式[form]. 这个表达式形式[form]返回的任何值[value]都会被这个 loop 表达式形式立即返回. 它等价于子句 do (return-from block-name value), 其中 block-name 是 named 子句中指定的名字, 如果这里没有 named 子句那么就是 nil.

#### 6.1.5.1 无条件执行的示例

```LISP
;; Print numbers and their squares.
;; The DO construct applies to multiple forms.
(loop for i from 1 to 3
      do (print i)
        (print (* i i)))
>>  1 
>>  1 
>>  2 
>>  4 
>>  3 
>>  9 
=>  NIL
```

### 6.1.6 <span id="ConditionalExecutionClauses">条件执行子句</span>

if, when, 和 unless 构造在一个 loop 中建立条件控制. 如果通过了检验测试, 后面的 loop 子句会被执行. 如果没有通过检验测试, 后面的子句会被跳过, 并且程序控制会转移到 loop 关键字[loop keyword] else 后面的子句. 如果没有通过检验测试并且没有提供 else 子句, 控制会转移到整个条件子句后面的子句或构造.

如果条件子句是嵌套的, 每个 else 和前面最接近的没有关联 else 或 end 的条件子句组成一对.

在 if 和 when 子句中, 它们是同义的, 如果表达式形式 form 的值是 true 那么检验测试通过.

在 unless 子句中, 如果表达式形式 form 的值是 false 那么检验测试通过.

测试表达式后面的那些子句可以通过使用 loop 关键字[loop keyword] and 产生一个由复合子句组成的条件块来分组.

loop 关键字[loop keyword] it 可以用于引用一个子句中测试表达式的结果. 使用这个 loop 关键字[loop keyword] it 代替一个条件执行子句中的一个 return 子句或一个累积子句中的表达式形式. 如果多个子句用 and 连接, 这个 it 构造必须在这个块的第一个子句中.

可选的 loop 关键字[loop keyword] end 标记这个子句的结束. 如果没有提供这个关键字, 下一个 loop 关键字[loop keyword]标记着结束. 这个构造 end 可以被用于区分复合子句的作用域.

#### 6.1.6.1 WHEN 子句的示例

```LISP
;; Signal an exceptional condition.
(loop for item in '(1 2 3 a 4 5)
      when (not (numberp item))
      return (cerror "enter new value" "non-numeric value: ~s" item))
Error: non-numeric value: A

;; The previous example is equivalent to the following one.
(loop for item in '(1 2 3 a 4 5)
      when (not (numberp item))
      do (return 
          (cerror "Enter new value" "non-numeric value: ~s" item)))
Error: non-numeric value: A

;; This example parses a simple printed string representation from 
;; BUFFER (which is itself a string) and returns the index of the
;; closing double-quote character.
(let ((buffer "\"a\" \"b\""))
  (loop initially (unless (char= (char buffer 0) #\")
                    (loop-finish))
        for i of-type fixnum from 1 below (length (the string buffer))
        when (char= (char buffer i) #\")
        return i))
=>  2

;; The collected value is returned.
(loop for i from 1 to 10
      when (> i 5)
        collect i
      finally (prin1 'got-here))
>>  GOT-HERE
=>  (6 7 8 9 10) 

;; Return both the count of collected numbers and the numbers.
(loop for i from 1 to 10
      when (> i 5)
        collect i into number-list
        and count i into number-count
      finally (return (values number-count number-list)))
=>  5, (6 7 8 9 10)
```

### 6.1.7 <span id="MiscellaneousClauses">其他子句</span>

> * 6.1.7.1 [控制转移子句](#ControlTransferClauses)
> * 6.1.7.2 [初始化和最后执行](#InitialFinalExecution)


#### 6.1.7.1 <span id="ControlTransferClauses">控制转移子句</span>

named 构造为包在整个 loop 周围的隐式块[implicit block]建立一个名字, 这样 return-from 特殊操作符[special operator]可以被用于从 loop 返回值或者退出 loop. 一个 loop 表达式形式[form]只能被赋予一个名字. 如果使用了, 这个 named 构造必须是这个 loop 表达式中的第一个子句.

return 构造接收一个表达式形式[form]. 这个表达式形式[form]返回的任何值[value]都被这个 loop 表达式形式立即返回. 这个构造类似于 return-from 特殊操作符[special operator]和 return 宏[macro]. return 构造不会执行这个 loop 表达式形式[form]给定的任何 finally 子句.

##### 6.1.7.1.1 NAMED 子句的示例

```LISP
;; Just name and return.
(loop named max
      for i from 1 to 10
      do (print i)
      do (return-from max 'done))
>>  1 
=>  DONE
```

#### 6.1.7.2 <span id="InitialFinalExecution">初始化和最后执行</span>

initially 和 finally 构造在这个 loop 主体之前和之后求值表达式形式.

initially 构造导致提供的复合表达式形式 compound-forms 在循环序言中被求值, 它在除了构造 with, for, 或 as 提供的初始化设置以外的所有 loop 代码之前. 任何 initially 子句的代码都是按照子句出现在这个 loop 中的顺序执行的.

finally 构造导致提供的复合表达式形式 compound-forms 在正常循环终止的循环结尾中被求值. 任何 finally 子句的代码都是按照子句出现在这个 loop 中的顺序执行的. 收集起来的代码只在任何累积子句返回隐式的值之前在循环结尾执行一次. 但是一个从 loop 主体中显式的控制转移 (比如, 通过 return, go, 或 throw) 会在没有执行结尾代码的情况下退出 loop.

像 return, always, never, 和 thereis 这样的子句可以绕开 finally 子句. return (或者 return-from, 如果提供了 named 选项的话) 可以被用于在 finally 之后从一个 loop 返回值. 这样一个 finally 子句中显式返回[explicit return]优先于从通过诸如关键字 collect, nconc, append, sum, count, maximize, 和 minimize 提供的子句中返回累积值; 如果使用了 return 或 return-from, 这些子句累积的值不会被 loop 返回. 

### 6.1.8 <span id="ExamplesMLF">其他 Loop 特性的示例</span>

```LISP
(let ((i 0))                     ; no loop keywords are used
  (loop (incf i) (if (= i 3) (return i)))) =>  3
(let ((i 0)(j 0))
  (tagbody
    (loop (incf j 3) (incf i) (if (= i 3) (go exit)))
    exit)
  j) =>  9
```

在下面的例子中, 变量 x 在 y 被步进之前步进; 因此, y 的值反映了 x 的更新值:

```LISP
(loop for x from 1 to 10 
      for y = nil then x 
      collect (list x y))
=>  ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))
```

在这个示例中, x 和 y 并行步进:

```LISP
(loop for x from 1 to 10 
      and y = nil then x 
      collect (list x y))
=>  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))
```

#### 6.1.8.1 子句分组示例

```LISP
;; Group conditional clauses.
(loop for i in '(1 324 2345 323 2 4 235 252)
      when (oddp i)
        do (print i)
        and collect i into odd-numbers
        and do (terpri)
      else                              ; I is even.
        collect i into even-numbers
      finally
        (return (values odd-numbers even-numbers)))
>>  1 
>>  
>>  2345 
>>  
>>  323 
>>  
>>  235 
=>  (1 2345 323 235), (324 2 4 252)

;; Collect numbers larger than 3.
(loop for i in '(1 2 3 4 5 6)
      when (and (> i 3) i)
      collect it)                      ; IT refers to (and (> i 3) i).
=>  (4 5 6)

;; Find a number in a list.
(loop for i in '(1 2 3 4 5 6)
      when (and (> i 3) i)
      return it)
=>  4
    
;; The above example is similar to the following one.
(loop for i in '(1 2 3 4 5 6)
      thereis (and (> i 3) i))
=>  4


;; Nest conditional clauses.
(let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
  (loop for i in list
        when (numberp i)
          when (floatp i)
            collect i into float-numbers
          else                                  ; Not (floatp i)
            collect i into other-numbers
        else                                    ; Not (numberp i)
          when (symbolp i) 
            collect i into symbol-list
          else                                  ; Not (symbolp i)
            do (error "found a funny value in list ~S, value ~S~%" list i)
        finally (return (values float-numbers other-numbers symbol-list))))
=>  (3.0 9.8), (0 4 5), (APPLE ORANGE BANANA)

;; Without the END preposition, the last AND would apply to the
;; inner IF rather than the outer one.
(loop for x from 0 to 3 
      do (print x)
      if (zerop (mod x 2))
        do (princ " a")
        and if (zerop (floor x 2))
              do (princ " b")
              end
        and do (princ " c"))
>>  0  a b c
>>  1 
>>  2  a c
>>  3 
=>  NIL
```

### 6.1.9 <span id="NotesAboutLoop">Loop 的注意事项</span>

可以为 loop 变量提供类型[type]. 没有必要为任何变量提供一个类型[type], 但是提供类型可以确保这个变量有正确类型的初始值, 并且这样也可以启用编译器优化 (取决于具体实现[implementation]).

子句 repeat n ... 粗略等价于下面这个子句

    (loop for internal-variable downfrom (- n 1) to 0 ...)

但是在某些实现[implementation]中, repeat 构造可能更高效.

在 loop 子句的可执行部分中和整个 loop 表达式形式周围, 可以使用 let 来绑定变量.

在与 loop 有关时使用名为 IT (在任何包[package]中) 的变量时要谨慎, 因为它是一个 loop 关键字[loop keyword], 可以在某些上下文中替换表达式形式[form].

这里没有让用户给 loop 添加扩展的标准化[standardized]机制. 

## 6.2 <span id="TheIterationDictionary">迭代字典</span>

> * [宏 DO, DO*](#MacroDODO)
> * [宏 DOTIMES](#MacroDOTIMES)
> * [宏 DOLIST](#MacroDOLIST)
> * [宏 LOOP](#MacroLOOP)
> * [局部宏 LOOP-FINISH](#LocalMacroLOOPFINISH)


### <span id="MacroDODO">宏 DO, DO*</span>

* 语法(Syntax):

        do ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) declaration* {tag | statement}*
        => result*

        do* ({var | (var [init-form [step-form]])}*) (end-test-form result-form*) declaration* {tag | statement}*
        => result*

* 参数和值(Arguments and Values):

        var---一个符号[symbol].
        init-form---一个表达式形式[form].
        step-form---一个表达式形式[form].
        end-test-form---一个表达式形式[form].
        result-forms---一个隐式的 progn [implicit progn].
        declaration---一个 declare 表达式[expression]; 不求值.
        tag---一个 go 标签[go tag]; 不求值.
        statement---一个复合表达式形式[compound form]; 按以下所述求值.
        results---如果执行了一个 return 或 return-from 表达式形式, 就是从这个表达式形式[form]传出来的那些值[value]; 否则, 就是通过 result-forms 返回的那些值[value].

* 描述(Description):

        当测试条件保持时满足时, do 在一组语句 statements 上迭代. do 接受任意数量大循环变量 vars, 它们在这个循环中并行绑定和步进. 可以通过使用一个 init-form 来为每个循环变量提供一个初始值. step-forms 可以被用于指定这个循环中后续的循环如何更新变量 vars. step-forms 可能被用于可以被用于产生后续的值或累积结果. 如果 end-test-form 条件在一个主体的执行前满足, 这个循环终止. 这些 tags 标记这些语句 statements.

        do* 和 do 一样除了这些变量 vars 的绑定[binding]和步进是顺序执行而不是并行执行.

        在第一次循环前, 所有初始化表达式形式 init-forms 被求值, 如果提供了 init-form, 对应的每一个变量 var 被绑定到它的对应 init-form 的值. 这是一个绑定[binding], 不是一个赋值; 当这个循环终止时, 这些变量的旧值会被恢复. 对于 do, 所有的这些 init-forms 在任何一个变量 var 绑定前被求值. 这些 init-forms 可以引用 do 开始执行之前的可见的这些 vars 的绑定. 对于 do*, 第一个 init-form 被求值, 然后第一个 var 被绑定到那个值, 然后第二个 init-form 被求值, 接着第二个 var 被绑定, 以此类推; 通常, 如果 j < k, 那么第 k 个 init-form 可以引用第 j 个 var 的绑定, 否则引用的是第 j 个 var 的旧绑定.

        在每个循环开始前, 在处理这些变量后, 这个 end-test-form 被求值. 如果结果是 false, 执行 do (或 do*) 表达式形式的主体. 如果结果是 true, 这个 result-forms 按照一个隐式的 progn 的顺序被求值, 然后 do 或 do* 返回.

        在除了第一次以外的每个循环开始时, vars 按照如下更新. 所有的步进表达式形式 step-forms, 如果提供了就从左到右求值, 并且结果值被赋给对应变量 vars. 任何没有关联 step-form 的变量 var 不会被赋值. 对于 do, 所有 step-forms 在任何 var 更新前被求值; 给 vars 的赋值并行执行, 就像是通过 psetq 一样. 因为所有的 step-forms 在任何变量被修改前求值, 所以一个 step-form 求值时可以访问所有这些 vars 的旧值, 即便其他的 step-forms 在它之前. 对于 do*, 第一个 step-form 被求值, 然后这个值赋给第一个 var, 然后第二个 step-form 被求值, 接着值赋给第二个 var, 以此类推; 这个给变量的赋值是顺序执行, 就像是通过 setq 一样. 不管是对于 do 还是 do*, 在变量 vars 被更新后, end-test-form 按照如上所述被求值, 然后这个循环继续.

        剩余的 do (或 do*) 表达式形式部分构成一个隐式的 tagbody [implicit tagbody]. 这些 tags 可能出现在一个 do 循环的主体中, 供出现在主体中的 go 语句使用 (但是这样的 go 语句可能不会出现在变量标识符, end-test-form, 或 result-forms 中). 当到达 do 主体的结尾时, 开始下一个循环周期 (以 step-forms 的求值开始).

        一个名为 nil 的隐式块[implicit block]包在整个 do (或 do*) 表达式形式周围. 一个 return 语句可以在任何点被使用来立即退出这个循环.

        初始化表达式形式 init-form 是和 var 关联的初始值. 如果 init-form 省略了, 那么这个 var 的初始值就是 nil. 如果为 var 提供了一个声明 declaration, init-form 必须与这个声明 declaration 一致.

        声明可以出现在 do (或 do*) 主体的开始位置. 它们适用于 do (or do*) 主体中的代码, 这个 do (or do*) vars 的绑定[binding], step-forms, end-test-form, 还有 result-forms.

* 示例(Examples):

    ```LISP
    (do ((temp-one 1 (1+ temp-one))
          (temp-two 0 (1- temp-two)))
        ((> (- temp-one temp-two) 5) temp-one)) =>  4

    (do ((temp-one 1 (1+ temp-one))
          (temp-two 0 (1+ temp-one)))     
        ((= 3 temp-two) temp-one)) =>  3

    (do* ((temp-one 1 (1+ temp-one))
          (temp-two 0 (1+ temp-one)))
          ((= 3 temp-two) temp-one)) =>  2                     

    (do ((j 0 (+ j 1)))
        (nil)                       ;Do forever.
      (format t "~%Input ~D:" j)
      (let ((item (read)))
        (if (null item) (return)   ;Process items until NIL seen.
            (format t "~&Output ~D: ~S" j item))))
    >>  Input 0: banana
    >>  Output 0: BANANA
    >>  Input 1: (57 boxes)
    >>  Output 1: (57 BOXES)
    >>  Input 2: NIL
    =>  NIL

    (setq a-vector (vector 1 nil 3 nil))
    (do ((i 0 (+ i 1))     ;Sets every null element of a-vector to zero.
        (n (array-dimension a-vector 0)))
        ((= i n))
      (when (null (aref a-vector i))
        (setf (aref a-vector i) 0))) =>  NIL
    a-vector =>  #(1 0 3 0)

    (do ((x e (cdr x))
        (oldx x x))
        ((null x))
      body)
    ```

        是一个对索引变量并行赋值的示例. 在第一个循环时, oldx 的值是 x 在进入 do 之前的值. 在后续的循环中, oldx 包含了 x 在上一次循环中的值.

    ```LISP
    (do ((x foo (cdr x))
          (y bar (cdr y))
          (z '() (cons (f (car x) (car y)) z)))
        ((or (null x) (null y))
          (nreverse z)))
    ```

        和 (mapcar #'f foo bar) 做了一样的事. z 的步进计算就是变量是并行步进的一个例子. 并且, 这个循环的主体是空的.

    ```LISP
    (defun list-reverse (list)
            (do ((x list (cdr x))
                (y '() (cons (car x) y)))
                ((endp x) y)))
    ```

        作为一个嵌套迭代的示例, 细想一个 cons 列表[list]的数据结构. 每个 cons 的 car 是一个符号[symbol]列表[list], 而每个 cons 的 cdr 是等长度的包含对应值的列表[list]. 这样一个数据结构类似于关联列表, 但是被划分为 "帧(frame)"; 整体结构类似于胸腔(rib-cage). 这样一个数据结构的一个查找函数可能是:

    ```LISP
    (defun ribcage-lookup (sym ribcage)           
            (do ((r ribcage (cdr r)))
                ((null r) nil)
              (do ((s (caar r) (cdr s))
                  (v (cdar r) (cdr v))) 
                  ((null s))
                (when (eq (car s) sym)
                  (return-from ribcage-lookup (car v)))))) =>  RIBCAGE-LOOKUP
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        其他循环函数 (dolist, dotimes, 和 loop) 和更原始的功能 (tagbody, go, block, return, let, 和 setq)

* 注意(Notes):

        如果 end-test-form 是 nil, 这个测试条件从来不会成功. 这为 "永远执行" 提供了一个惯用语法: 这个 do 或 do* 的主体被重复执行. 这个无限循环可以通过使用 return, return-from, go 到一个外部层级, 或 throw 来终止.

        一个 do 表达式形式[form]可能被解释为如下更原始的表达式形式[form] block, return, let, loop, tagbody, 和 psetq:

    ```LISP
    (block nil        
      (let ((var1 init1)
            (var2 init2)
            ...
            (varn initn))
        declarations
        (loop (when end-test (return (progn . result)))
              (tagbody . tagbody)
              (psetq var1 step1
                      var2 step2
                      ...
                      varn stepn))))
    ```

        do* 是类似的, 除了 let* 和 setq 分别替换 let 和 psetq. 

### <span id="MacroDOTIMES">宏 DOTIMES</span>

* 语法(Syntax):

        dotimes (var count-form [result-form]) declaration* {tag | statement}*
        => result*

* 参数和值(Arguments and Values):

        var---一个符号[symbol].
        count-form---一个表达式形式[form].
        result-form---一个表达式形式[form].
        declaration---一个 declare 表达式[expression]; 不求值.
        tag---一个 go 标签[go tag]; 不求值.
        statement---一个复合表达式形式[compound form]; 按如下所述求值.
        results---如果执行了一个 return 或 return-from 表达式形式, 那么就是从这个表达式形式[form]传出来的那些值[value]; 否则, 就是通过 result-form 返回的那些值[value], 如果没有 result-form 就是 nil.

* 描述(Description):

        dotimes 遍历一系列整数[integer].

        dotimes 求值 count-form, 它应该产生一个整数[integer]. 如果 count-form 是 zero 或者负的, 这个主体不会被执行. dotimes 对于每一个从 0 到 count-form 的值但是不包括那个值的整数[integer]执行一次主体, 以这些 tag 和 statement 出现的顺序执行, 其中 var 绑定到每一个 integer. 然后 result-form 被求值. 在 result-form 被处理时, var 被绑定为主体执行的次数. 这些 tags 标记 statements.

        名为 nil 的隐式块[implicit block]包在 dotimes 周围. return 可以被用于在没有执行进一步循环的情况下立即终止循环, 返回 0 个或多个值.

        这个循环的主体是一个隐式 tagbody [implicit tagbody]; 它可能包含被当作 go 语句目标的标签. 声明可能出现在这个循环的主体之前.

        var 绑定的作用域[scope]不包括 count-form, 但是包括 result-form.

        dotimes 是在每次循环为 var 建立[establish]一个新的绑定[binding], 还是在开始的时候为 var 建立一次绑定[binding]而后续的循环对它赋值, 这是依赖于具体实现的[implementation-dependent].

* 示例(Examples):

    ```LISP
    (dotimes (temp-one 10 temp-one)) =>  10
    (setq temp-two 0) =>  0
    (dotimes (temp-one 10 t) (incf temp-two)) =>  T
    temp-two =>  10
    ```

        这是使用 dotimes 来处理字符串的示例:

    ```LISP
    ;;; True if the specified subsequence of the string is a
    ;;; palindrome (reads the same forwards and backwards).
    (defun palindromep (string &optional
                              (start 0)
                              (end (length string)))
      (dotimes (k (floor (- end start) 2) t)
        (unless (char-equal (char string (+ start k))
                            (char string (- end k 1)))
          (return nil))))
    (palindromep "Able was I ere I saw Elba") =>  T
    (palindromep "A man, a plan, a canal--Panama!") =>  NIL
    (remove-if-not #'alpha-char-p          ;Remove punctuation.
                  "A man, a plan, a canal--Panama!")
    =>  "AmanaplanacanalPanama"
    (palindromep
      (remove-if-not #'alpha-char-p
                    "A man, a plan, a canal--Panama!")) =>  T
    (palindromep
      (remove-if-not
      #'alpha-char-p
      "Unremarkable was I ere I saw Elba Kramer, nu?")) =>  T
    (palindromep
      (remove-if-not
      #'alpha-char-p
      "A man, a plan, a cat, a ham, a yak,
                      a yam, a hat, a canal--Panama!")) =>  T
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        do, dolist, tagbody

* 注意(Notes):

        在 dotimes 的主体中 go 可能被用于转移控制到一个 tag 标记的语句上. 

### <span id="MacroDOLIST">宏 DOLIST</span>

* 语法(Syntax):

        dolist (var list-form [result-form]) declaration* {tag | statement}*
        => result*

* 参数和值(Arguments and Values):

        var---一个符号[symbol].
        list-form---一个表达式形式[form].
        result-form---一个表达式形式[form].
        declaration---一个 declare 表达式[expression]; 不求值.
        tag---一个 go 标签[go tag]; 不求值.
        statement---一个复合表达式形式[compound form]; 按如下所述求值.
        results---如果执行了一个 return 或 return-from 表达式形式, 就是从这个表达式形式[form]传出来的值[value]; 否则, 就是通过 result-form 返回的值[value], 如果没有 result-form 就是 nil.

* 描述(Description):

        dolist 遍历一个列表[list]的元素. dolist 的主体类似于一个 tagbody. 它由一系列的标签 tag 和语句 statement 组成.

        dolist 求值 list-form, 它应该产生一个列表[list]. 对于列表[list]中的每个元素执行一次主体, 以那些 tag 和 statement 出现的顺序求值, 其中 var 绑定为这个元素. 然后 result-form 被求值. 那些 tag 标记那些 statement.

        在 result-form 被处理时, var 绑定为 nil.

        一个名为 nil 的隐式块[implicit block]包在 dolist 周围. return 可以被用于在没有执行进一步循环的情况下立即终止循环, 返回 0 个或多个值[value].

        这个 var 的绑定的作用域[scope]不包括 list-form, 但是包括 result-form.

        dolist 是在每次循环为 var 建立[establish]一个新的绑定[binding]还是在开始的时候为 var 建立[establish]一次绑定而后续的循环对它赋值, 这是依赖于具体实现的[implementation-dependent].

* 示例(Examples):

    ```LISP
    (setq temp-two '()) =>  NIL
    (dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two)) =>  (4 3 2 1)

    (setq temp-two 0) =>  0
    (dolist (temp-one '(1 2 3 4)) (incf temp-two)) =>  NIL
    temp-two =>  4

    (dolist (x '(a b c d)) (prin1 x) (princ " ")) 
    >>  A B C D 
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        do, dotimes, tagbody, 章节 3.6 (遍历规则和副作用)

* 注意(Notes):

        在 dolist 的主体中 go 可能被用于转移控制到一个由 tag 标记的语句上. 

### <span id="MacroLOOP">宏 LOOP</span>

* 语法(Syntax):

        "简单" loop 表达式形式:

        loop compound-form* => result*

        "扩展" loop 表达式形式:

        loop [name-clause] {variable-clause}* {main-clause}* => result*

        name-clause::= named name 

        variable-clause::= with-clause | initial-final | for-as-clause 

        with-clause::= with var1 [type-spec] [= form1] {and var2 [type-spec] [= form2]}* 

        main-clause::= unconditional | accumulation | conditional | termination-test | initial-final 

        initial-final::= initially compound-form+ | finally compound-form+ 

        unconditional::= {do | doing} compound-form+ | return {form | it} 

        accumulation::= list-accumulation | numeric-accumulation 

        list-accumulation::= {collect | collecting | append | appending | nconc | nconcing} {form | it}  
                            [into simple-var] 

        numeric-accumulation::= {count | counting | sum | summing | } 
                                maximize | maximizing | minimize | minimizing {form | it} 
                                [into simple-var] [type-spec] 

        conditional::= {if | when | unless} form selectable-clause {and selectable-clause}*  
                      [else selectable-clause {and selectable-clause}*]  
                      [end] 

        selectable-clause::= unconditional | accumulation | conditional 

        termination-test::= while form | until form | repeat form | always form | never form | thereis form 

        for-as-clause::= {for | as} for-as-subclause {and for-as-subclause}* 

        for-as-subclause::= for-as-arithmetic | for-as-in-list | for-as-on-list | for-as-equals-then | 
                            for-as-across | for-as-hash | for-as-package 

        for-as-arithmetic::= var [type-spec] for-as-arithmetic-subclause 

        for-as-arithmetic-subclause::= arithmetic-up | arithmetic-downto | arithmetic-downfrom 

        arithmetic-up::= [[{from | upfrom} form1 |   {to | upto | below} form2 |   by form3]]+ 

        arithmetic-downto::= [[{{from form1}}1  |   {{{downto | above} form2}}1  |   by form3]] 

        arithmetic-downfrom::= [[{{downfrom form1}}1  |   {to | downto | above} form2 |   by form3]] 

        for-as-in-list::= var [type-spec] in form1 [by step-fun] 

        for-as-on-list::= var [type-spec] on form1 [by step-fun] 

        for-as-equals-then::= var [type-spec] = form1 [then form2] 

        for-as-across::= var [type-spec] across vector 

        for-as-hash::= var [type-spec] being {each | the}  
                      {{hash-key | hash-keys} {in | of} hash-table  
                        [using (hash-value other-var)] |  
                        {hash-value | hash-values} {in | of} hash-table  
                        [using (hash-key other-var)]} 

        for-as-package::= var [type-spec] being {each | the}  
                          {symbol | symbols | 
                          present-symbol | present-symbols | 
                          external-symbol | external-symbols} 
                          [{in | of} package] 

        type-spec::= simple-type-spec | destructured-type-spec 

        simple-type-spec::= fixnum | float | t | nil 

        destructured-type-spec::= of-type d-type-spec 

        d-type-spec::= type-specifier | (d-type-spec . d-type-spec) 

        var::= d-var-spec 

        var1::= d-var-spec 

        var2::= d-var-spec 

        other-var::= d-var-spec 

        d-var-spec::= simple-var | nil | (d-var-spec . d-var-spec) 

* 参数和值(Arguments and Values):

        compound-form---一个复合表达式形式[compound form].
        name---一个符号[symbol].
        simple-var---一个符号[symbol] (一个变量[variable]名).
        form, form1, form2, form3---一个表达式形式[form].
        step-fun---一个求值为单参数[argument]函数[function]的表达式形式[form].
        vector---一个求值为一个向量[vector]的表达式形式[form].
        hash-table---一个求值为一个哈希表[hash table]的表达式形式[form].
        package---一个求值为一个包标识符[package designator]的表达式形式[form].
        type-specifier---一个类型指定符[type specifier]. 这个可能是原子类型指定符[atomic type specifier]也可能是复合类型指定符[compound type specifier], 在解构时它为正确解析引入了一些额外的复杂性; 关于更多信息, 见章节 6.1.1.7 (解构).
        result---一个对象[object].

* 描述(Description):

        关于详细信息, 见章节 6.1 (LOOP 机制).

* 示例(Examples):

    ```LISP
    ;; An example of the simple form of LOOP.
    (defun sqrt-advisor ()
      (loop (format t "~&Number: ")
            (let ((n (parse-integer (read-line) :junk-allowed t)))
              (when (not n) (return))
              (format t "~&The square root of ~D is ~D.~%" n (sqrt n)))))
    =>  SQRT-ADVISOR
    (sqrt-advisor)
    >>  Number: 5<NEWLINE>
    >>  The square root of 5 is 2.236068.
    >>  Number: 4<NEWLINE>
    >>  The square root of 4 is 2.
    >>  Number: done<NEWLINE>
    =>  NIL

    ;; An example of the extended form of LOOP.
    (defun square-advisor ()
      (loop as n = (progn (format t "~&Number: ")
                          (parse-integer (read-line) :junk-allowed t))
            while n
            do (format t "~&The square of ~D is ~D.~%" n (* n n))))
    =>  SQUARE-ADVISOR
    (square-advisor)
    >>  Number: 4<NEWLINE>
    >>  The square of 4 is 16.
    >>  Number: 23<NEWLINE>
    >>  The square of 23 is 529.
    >>  Number: done<NEWLINE>
    =>  NIL

    ;; Another example of the extended form of LOOP.
    (loop for n from 1 to 10
          when (oddp n)
            collect n)
    =>  (1 3 5 7 9)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        do, dolist, dotimes, return, go, throw, 章节 6.1.1.7 (解构)

* 注意(Notes):

        除了 loop-finish 不能被用在一个简单 loop 表达式形式[form]中, 一个简单 loop 表达式形式[form]和一个扩展 loop 表达式形式[form]的关系如下:

        (loop compound-form*) ==  (loop do compound-form*)


### <span id="LocalMacroLOOPFINISH">局部宏 LOOP-FINISH</span>

* 语法(Syntax):

        loop-finish <no arguments> =>|

* 描述(Description):

        可以在词法上位于一个扩展 loop 表达式形式[form]中的地方使用这个 loop-finish 宏[macro]来"正常"地终止这个表达式形式[form]. 这也就是说, 它转移控制到词法上最内部的扩展 loop 表达式形式[form]的循环结尾. 这允许执行任何 finally 子句 (为了得到效果) 以及返回任何累积结果.

* 示例(Examples):

    ```LISP
    ;; Terminate the loop, but return the accumulated count.
    (loop for i in '(1 2 3 stop-here 4 5 6)
          when (symbolp i) do (loop-finish)
          count i)
    =>  3
    
    ;; The preceding loop is equivalent to:
    (loop for i in '(1 2 3 stop-here 4 5 6)
          until (symbolp i)
          count i)
    =>  3

    ;; While LOOP-FINISH can be used can be used in a variety of 
    ;; situations it is really most needed in a situation where a need
    ;; to exit is detected at other than the loop's `top level'
    ;; (where UNTIL or WHEN often work just as well), or where some 
    ;; computation must occur between the point where a need to exit is
    ;; detected and the point where the exit actually occurs.  For example:
    (defun tokenize-sentence (string)
      (macrolet ((add-word (wvar svar)
                    `(when ,wvar
                      (push (coerce (nreverse ,wvar) 'string) ,svar)
                      (setq ,wvar nil))))
        (loop with word = '() and sentence = '() and endpos = nil
              for i below (length string)
              do (let ((char (aref string i)))
                    (case char
                      (#\Space (add-word word sentence))
                      (#\. (setq endpos (1+ i)) (loop-finish))
                      (otherwise (push char word))))
              finally (add-word word sentence)
                      (return (values (nreverse sentence) endpos)))))
    =>  TOKENIZE-SENTENCE
    
    (tokenize-sentence "this is a sentence. this is another sentence.")
    =>  ("this" "is" "a" "sentence"), 19
    
    (tokenize-sentence "this is a sentence")
    =>  ("this" "is" "a" "sentence"), NIL
    ```

* 副作用(Side Effects):

        转移控制.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        loop-finish 在全局环境[global environment]中是否被 fbound 是依赖于具体实现的[implementation-dependent]; 然而, loop-finish 的遮蔽[shadow]和重定义的限制条件跟 COMMON-LISP 包中在全局环境[global environment]中被 fbound 的符号[symbol]一样. 在 loop 的外部去尝试使用 loop-finish 的结果是未定义的.

* 也见(See Also):

        loop, 章节 6.1 (LOOP 机制)

* 注意(Notes):


