# 3. 编译和求值

> * 3.1 [求值](#Evaluation)
> * 3.2 [编译](#Compilation)
> * 3.3 [声明](#Declarations)
> * 3.4 [Lambda列表](#LambdaLists)
> * 3.5 [函数调用中的错误检测](#ErrorChecking)
> * 3.6 [遍历规则和副作用](#TraversalRulesSideEffects)
> * 3.7 [破坏性操作](#DestructiveOperations)
> * 3.8 [求值和编译的字典条目](#EvaluationCompilationDictionary)

## 3.1 <span id = "Evaluation">求值</span>

代码的执行可以通过多种方式来完成, 从对一个程序的直接解释到一个编译器生成的编译代码的调用都可以.

求值是一个程序在Common Lisp中执行的过程. 求值机制通过 Lisp read-eval-print 循环的效果来隐式表现, 并且通过函数 eval, compile, compile-file 和 load 的存在显式表现出来. 这些设备中的任何一个都可以共享相同的执行策略, 或者每个都可能使用不同的执行策略.

符合规范的程序被 eval 和 被 compile-file 处理后的行为可能不同; 见章节 3.2.2.3 (Semantic Constraints).

可以通过一个模型来理解求值, 在这个模型中, 解释器递归地遍历一个执行计算过程中的每一个步骤的表达式形式. 这个描述了 Common Lisp 程序语义的模型, 被描述在章节 3.1.2 (The Evaluation Model).

> * 3.1.1 [环境的介绍](#IntroductionEnvironments)
> * 3.1.2 [求值模型](#TheEvaluationModel)
> * 3.1.3 [Lambda表达式](#LambdaExpressions)
> * 3.1.4 [闭包和词法绑定](#ClosuresLexicalBinding)
> * 3.1.5 [遮蔽](#Shadowing)
> * 3.1.6 [范围](#Extent)
> * 3.1.7 [返回值](#ReturnValues)

### 3.1.1 <span id = "IntroductionEnvironments">环境的介绍</span>

一个绑定是一个名字和它表示的东西的关联. 绑定由一个词法作用域或动态作用域中的特定的特殊操作符确定.

一个环境是一个绑定的集合和求值过程中使用的其他信息 (比如, 用来关联名字和意义的信息).

一个环境中的绑定被用命名空间来划分. 单个的名字在每一个环境中可以同时有超过一个关联的绑定, 每一个命名空间只能有一个关联绑定.

> * 3.1.1.1 [全局的环境](#TheGlobalEnvironment)
> * 3.1.1.2 [动态环境](#DynamicEnvironments)
> * 3.1.1.3 [词法环境](#LexicalEnvironments)
> * 3.1.1.4 [环境对象](#EnvironmentObjects)

#### 3.1.1.1 <span id = "TheGlobalEnvironment">全局的环境</span>

全局环境是包含不定作用域和不确定范围的绑定的环境的一部分. 全局环境包括以下部分:

    动态变量和常量的绑定.
    函数, 宏, 特殊操作符的绑定.
    编译器宏绑定.
    类型和类名绑定.
    声明相关的信息. 

#### 3.1.1.2 <span id = "DynamicEnvironments">动态环境</span>

求值的动态环境是包含绑定的环境的一部分, 该环境的持续时间受限于这个建立绑定的表达式形式执行过程中绑定建立和解除的点. 一个动态环境包括以下内容:

    动态变量的绑定.
    活跃的捕捉标签的信息.
    unwind-protect 确定的退出点.
    活跃的处理者和重启器的信息.

在程序执行过程中任何给定的时间点, 动态环境都被指定为"the current dynamic environment", 或者有时只是"the dynamic environment".

在给定的命名空间中, 如果在动态环境中有一个与其名称相关联的绑定, 那么名称就被绑定到动态环境中, 否则, 在全局环境中有一个与它的名称相关联的绑定. 

#### 3.1.1.3 <span id = "LexicalEnvironments">词法环境</span>

在一个程序中, 一些位置的求值的词法环境是包含这个位置的表达式形式的词法作用域信息的环境的一部分. 一个词法环境包含以下内容:

    词法变量和符号宏的绑定.
    函数和宏的绑定. (这是关于本地禁用的编译器宏的信息.)
    block块标记的绑定.
    go标记的绑定.
    声明的信息.

在被语义处理的程序中，任何给定位置的词汇环境都被称为"the current lexical environment"，或者有时只是"the lexical environment".

在给定的命名空间中, 如果在词法环境中有一个与其名称相关联的绑定, 那么名称就被绑定到词法环境中, 否则, 在全局环境中有一个与它的名称相关联的绑定.

##### 3.1.1.3.1 空的词法环境

空的词法环境等价于全局环境.

虽然通常一个环境对象的表示是取决于具体实现的, 但是 nil 可以被用于任何情况表示空的词法环境的环境对象. 

#### 3.1.1.4 <span id = "EnvironmentObjects">环境对象</span>

一些操作符使用一个称为环境对象的对象, 该对象表示在给定的词法环境中对表达式形式执行语义分析所需的词法绑定集合. 环境对象中的绑定集合可能是实际执行求值所需的绑定的子集; 比如, 在相应的词法环境中，与变量名和函数名相关联的值可能无法在环境对象中使用.

一个环境对象的类型和性质是取决于具体实现的. 给宏函数的环境参数的值就是环境对象的示例.

当 nil 对象被用于一个环境对象时, 表示空的词法环境; 见章节 3.1.1.3.1 (The Null Lexical Environment). 

### 3.1.2 <span id = "TheEvaluationModel">求值模型</span>

Common Lisp 系统对词法, 动态和全局环境的表达式形式进行求值. 下面章节描述的 Common Lisp 求值模型的组件.

#### 3.1.2.1 表达式求值

表达式形式分为三个类别: 符号, conses和自求值对象. 下面的章节介绍这些类别.

> * 3.1.2.1.1 [符号表达式](#SymbolsForms)
> * 3.1.2.1.2 [Cons表达式](#ConsesForms)
> * 3.1.2.1.3 [自求值对象](#SelfEvaluatingObjects)

##### 3.1.2.1.1 <span id = "SymbolsForms">符号表达式</span>

如果一个表达式形式是一个符号, 那么它要么是一个符号宏, 要么是一个变量.

如果在当前的词法环境中有一个符号作为符号宏的符号, 那么这个符号就会命名一个符号宏(见 define-symbol-macro 和 symbol-macrolet). 如果这个符号是一个符号宏, 得到它的展开函数. 展开函数是一个带有两个参数的函数, 通过调用宏展开函数来调用它, 并把它作为宏展开钩子的第一个参数, 这个符号作为第二个参数, 并且一个环境对象(对应当前词法环境)作为第三个参数. 然后, 这个宏展开钩子函数调用展开函数, 把这个表达式作为第一个参数并且把环境作为它的第二个参数. 传递给宏展开钩子函数的展开函数的值是一个表达式形式. 这个结果表达式形式会替换到原来符号的位置.

如果一个表达式形式是一个不是符号宏的符号, 那么它就是一个变量的名字, 并且返回这个变量的值. 这里有三种变量: 词法变量, 动态变量, 还有常变量. 一个变量可以存储一个对象. 在一个变量上的主要操作是 read 和  write 它的值.

如果引用到一个未绑定的变量, 会发出一个 unbound-variable 类型的错误.

非常变量可以通过 setq 或者 let 来绑定赋值. 下面这段列出了可以赋值, 绑定, 和定义变量的名字.

    boundp        let                  progv         
    defconstant   let*                 psetq         
    defparameter  makunbound           set           
    defvar        multiple-value-bind  setq          
    lambda        multiple-value-setq  symbol-value  

Figure 3-1. 可应用于变量的一些定义的名字

下面是对每种变量的描述.

> * 3.1.2.1.1.1 [词法变量](#LexicalVariables)
> * 3.1.2.1.1.2 [动态变量](#DynamicVariables)
> * 3.1.2.1.1.3 [常变量](#ConstantVariables)
> * 3.1.2.1.1.4 [同时命名词法和动态变量的符号](#SymbolsNamingLDVariables)

###### 3.1.2.1.1.1 <span id = "LexicalVariables">词法变量</span>

词法变量是一个变量，它只能在建立该变量的表达式形式的词法作用域内引用; 词法变量有词法作用域. 每次一个表达式形式创建一个变量的词法绑定, 就确定一个新的绑定.

在一个词法绑定的变量名的作用域里, 把这个名字作为变量使用被认为是对这个绑定的引用除非这个变量被一个确定该变量新的绑定的表达式形式所遮蔽, 或者被一个表达式形式将该名字声明为 special.

一个词法变量总是有一个值. 这里没有引入一个词法变量的绑定而没有给初始值的操作, 也没有任何操作符可以使一个词法变量解绑.

词法变量的绑定可以在词法环境中找到. 

###### 3.1.2.1.1.2 <span id = "DynamicVariables">动态变量</span>

如果一个变量满足下面条件的其中之一, 那么这个变量就是一个动态变量:

    它被局部或全局声明为 special.

    它以文本形式出现在一个表达式中, 为同名变量创建一个动态绑定, 并且这个绑定没有被一个对相同变量名创建词法绑定的表达式所遮蔽.

一个动态变量可以在这个程序的任何时间被引用; 对动态变量的引用没有文本限制. 在任何给定时间, 具有给定名称的所有动态变量都是在动态环境中或全局环境中确切地引用一个绑定.

一个动态变量绑定的值部分可能是空的; 在这个情况下, 这个动态变量被说成没有值或者没被绑定. 一个动态变量可以通过使用 makunbound 来解除绑定.

绑定一个动态变量的效果是创建一个新的绑定, 在创建这个动态绑定的表达式形式求值的持续时间中, 任何程序中所有引用该动态变量都是指新创建的绑定.

动态变量可以在绑定它的表达式的动态范围之外引用. 这种变量有时被称为"全局变量"("global variable"), 但它仍然在所有方面都只是一个动态变量, 它的绑定恰好存在于全局环境中, 而不是在某些动态环境中.

动态变量除非明确赋值，除了在该规范或具体实现中定义了初始值的变量，否则就是未绑定的. 

###### 3.1.2.1.1.3 <span id = "ConstantVariables">常变量</span>

某些变量, 称为常量变量, 被保留为"命名常量"("named constants"). 如果尝试去给它赋值, 或者给一个常变量创建一个绑定, 结果是未定义的, 除了使用兼容的 defconstant 去重定义一个常变量是允许的; 见宏 defconstant.

关键字, Common Lisp或者具体实现定义的符号作为常量 (就像 nil, t, 和 pi), 并且符号通过 defconstant 定义的符号也作为常变量. 

###### 3.1.2.1.1.4 <span id = "SymbolsNamingLDVariables">同时命名词法和动态变量的符号</span>

同一个符号可以命名一个词法变量和动态变量, 但是从来不出现在相同的词法环境下.

下面的例子中, 符号 x 被使用, 在不同的时间, 作为词法变量的名字和动态变量的名字.

```LISP
(let ((x 1))            ;Binds a special variable X
  (declare (special x))
  (let ((x 2))          ;Binds a lexical variable X
    (+ x                ;Reads a lexical variable X
      (locally (declare (special x))
                x))))   ;Reads a special variable X
=>  3
```

##### 3.1.2.1.2 <span id = "ConsesForms">Cons表达式</span>

一个 cons 被用于作为一个表达式形式时, 称这个表达式为复合表达式形式.

如果这个复合表达式的 car 是一个符号, 这个符号是一个操作符的名字, 那么这个表达式形式是一个特殊表达式, 一个宏表达式, 还是一个函数表达式, 取决于这个操作符在当前词法作用域的函数绑定. 如果这个操作符不是一个特殊操作符也不是一个宏的名字, 它被假定为一个函数名字 (甚至在这里没有定义这样的函数的情况下).

如果这个复合表达式形式的 car 部分不是一个符号, 那么这个 car 必须是一个 lambda 表达式, 在这个情况下这个表达式就是一个lambda表达式形式.

一个复合表达式如何被处理取决于它被归类为特殊表达式, 宏表达式, 函数表达式, 还是一个lambda表达式.

> * 3.1.2.1.2.1 [特殊表达式](#SpecialForms)
> * 3.1.2.1.2.2 [宏表达式](#MacroForms)
> * 3.1.2.1.2.3 [函数表达式](#FunctionForms)
> * 3.1.2.1.2.4 [Lambda表达式](#LambdaForms)

###### 3.1.2.1.2.1 <span id = "SpecialForms">特殊表达式</span>

一个特殊表达式是一个带有特殊语法或者特殊求值规则或者两者都有的表达式形式, 可能控制求值环境, 控制流, 或者都控制. 一个特殊操作符可以访问当前的词法环境和当前的动态环境. 每个特殊操作符都定义了它的子表达式被当成的种类---哪些是表达式形式, 哪些是特殊表达式, 等等.

一些特殊操作符创建新的词法或动态环境, 以便在对特殊表达式形式的子表达式进行求值时使用. 比如, block 会创建一个新的词法环境, 该环境与block表达式形式的求值时的作用相同, 它添加了block名称的绑定到block的退出点.

特殊操作符的名字的集合被固定于Common Lisp中; 没有给用户提供定义特殊操作符的方法. 下面这段列出了所有被定义为特殊操作符的Common Lisp符号.

    block      let*                  return-from      
    catch      load-time-value       setq             
    eval-when  locally               symbol-macrolet  
    flet       macrolet              tagbody          
    function   multiple-value-call   the              
    go         multiple-value-prog1  throw            
    if         progn                 unwind-protect   
    labels     progv                                  
    let        quote                                  

Figure 3-2. Common Lisp 特殊操作符

###### 3.1.2.1.2.2 <span id = "宏表达式">Macro Forms</span>

如果这个操作符命名了一个宏, 它的关联宏函数会被应用于整个表达式形式并且将结果替换原来的表达式.

具体来说, a symbol names a macro in a given lexical environment if macro-function is true of the symbol and that environment. 这个宏函数返回的函数是一个2个参数的函数, 称之为展开函数. 这个展开函数通过调用宏展开钩子函数而调用, 作为它的第一个参数, 这个完整的宏表达式作为它的第二个参数, 并且一个环境对象(对应当前词法环境)作为第三个参数. 这个宏展开钩子函数反过来调用这个展开函数, 将这个表达式形式作为第一个参数并将环境对象作为第二个参数. 这个传递给宏展开钩子函数的展开函数的值是一个表达式形式. 返回的表达式形式在原来的表达式的位置被求值.

如果这个宏函数破坏性地修改它的表达式参数的任何部分, 那么结果是无法预料的.

一个宏的名字不是一个函数描述符, 并且不能被用于函数例如apply, funcall, 或者 map 的参数.

一个具体实现可以自由地将Common Lisp特殊操作符实现为一个宏. 一个实现也能自由地将任何宏操作符实现为一个特殊操作符, 但是只能在一个定义上等价的宏被提供的情况下.

下面这段列出了一些可应用于宏的已定义名字.

    *macroexpand-hook*  macro-function  macroexpand-1  
    defmacro            macroexpand     macrolet       

Figure 3-3. 应用于宏的定义的名字

###### 3.1.2.1.2.3 <span id = "FunctionForms">函数表达式</span>

如果这个操作符是一个函数名的符号, 这个表达式就表示一个函数表达式形式, 并且这个列表的 cdr 部分包含了求值后作为参数传递给函数的的表达式.

当一个函数名字没有定义的时候, 应该会在运行时发出一个 undefined-function 的错误; 见章节 3.2.2.3 (Semantic Constraints).

一个函数表达式会像下面所述的被求值:

原始表达式的 cdr 的子表达式按照从左到右的顺序在词法和动态环境中被求值. 每次求值的主要的值作为参数传递给命名的函数; 任何子表达式返回的额外的值会被丢弃.

从词法环境中检索操作符的函数值, 并使用指定的参数调用该函数.

虽然参数子表达式的求值顺序是严格的从左到右, 但是如果有多个参数子表达式, 没有指定一个函数表达式中查询操作符的定义是在参数子表达式之前, 之后, 又或者是任意两个参数子表达式之间. 比如, 下面可能返回 23 或者 24.

```LISP
(defun foo (x) (+ x 3))
(defun bar () (setf (symbol-function 'foo) #'(lambda (x) (+ x 4))))
(foo (progn (bar) 20))
```

一个函数名字的绑定可以通过几种方式的一种来确定. 一个全局环境中的函数名可以通过defun ,fdefinition 的 setf, symbol-function 的 setf, ensure-generic-function, defmethod (隐式的, 由于ensure-generic-function), 或者 defgeneric 来确定. 一个词法环境中的函数名可以通过 flet 或者 labels 来确定.

下一段中列出了可应用于函数的一些定义的名字.

    apply                 fdefinition  mapcan               
    call-arguments-limit  flet         mapcar               
    complement            fmakunbound  mapcon               
    constantly            funcall      mapl                 
    defgeneric            function     maplist              
    defmethod             functionp    multiple-value-call  
    defun                 labels       reduce               
    fboundp               map          symbol-function      

Figure 3-4. 一些已定义的函数相关的名字 

###### 3.1.2.1.2.4 <span id = "LambdaForms">Lambda表达式</span>

一个lambda表达式类似于函数表达式, 除了函数名被一个lambda表达式替换.

一个lambda表达式等价于使用一个lambda表达式词法闭包的给定参数的 funcall 调用. (在实践中, 一些编译器很可能生成lambda表达式的内联代码, 而不是一个已声明为内联的任意命名函数; 然而, 这样的区别不是语义上的.)

关于更多信息, 见章节 3.1.3 (Lambda Expressions). 

##### 3.1.2.1.3 <span id = "SelfEvaluatingObjects">自求值对象</span>

一个表达式形式既不是一个符号也不是一个cons, 就被定义为一个自求值对象. 对这样的对象进行求值, 会产生结果相同的对象.

某些特定的符号和集合也可能是"自求值"但只是作为一个更普遍的规则的特殊案例用来求值符号和集合; 这样的对象不被认为是自求值对象.

如果字面上的对象(包括自求值对象)被破坏性的修改, 结果是无法定义的.

3.1.2.1.3.1 自求值对象的示例

数字, 路径名, 还有数组都是自求值对象的示例.

```LISP
3 =>  3
#c(2/3 5/8) =>  #C(2/3 5/8)
#p"S:[BILL]OTHELLO.TXT" =>  #P"S:[BILL]OTHELLO.TXT"
#(a b c) =>  #(A B C)
"fred smith" =>  "fred smith"
```

### 3.1.3 <span id = "LambdaExpressions">Lambda表达式</span>

在lambda表达式中, 通过在lambda列表中添加每个参数的绑定, 以及从参数到当前词法环境的相应值，将 body 在该词法环境中进行求值.

有关如何基于lambda列表建立绑定的进一步讨论, 见章节 3.4 (Lambda Lists).

一个lambda表达式的body是一个隐式的progn; 它返回的值会被这个lambda表达式返回. 

### 3.1.4 <span id = "ClosuresLexicalBinding">闭包和词法绑定</span>

一个词法闭包是一个可以引用和更改由包含函数定义的绑定表达式形式所确定的词法绑定的值的函数.

细想这段 x 没有声明为 special 的代码:

```LISP
(defun two-funs (x)
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))
(setq funs (two-funs 6))
(funcall (car funs)) =>  6
(funcall (cadr funs) 43) =>  43
(funcall (car funs)) =>  43
```

函数的特殊表达式形式将lambda表达式强制转换为闭包, 在这个闭包中, 当对特殊表达式形式进行求值时, 词法环境与lambda表达式一起被捕获.

这个 two-funs 函数返回两个函数的列表, 它们中的每一个都引用了函数 two-funs 被调用时创建的变量 x 的绑定. 这个变量有初始值 6, 但是 setq 可以修改这个绑定. 为第一个lambda表达式创建的词法闭包在创建闭包时不会"快照" x 的值 6;而是抓住了 x 的绑定. 第二个函数可以被用于修改同样的(捕获的)绑定(在示例中改为 43), 并且这个修改后的变量绑定之后影响了第一个函数的返回值.

在同一组绑定中, 一个lambda表达式的闭包可能不止一次地产生, 产生的多个闭包可能是相同的也可能是不同的, 由具体实现来决定. 也就是说, 两种行为无法区分的函数可能或不完全相同. 行为上可区分的两个函数是截然不同的. 示例:

```LISP
(let ((x 5) (funs '()))
  (dotimes (j 10)                          
    (push #'(lambda (z)                        
              (if (null z) (setq x 0) (+ x z)))
          funs))
  funs)
```

上面表达式形式的列表是一个十个闭包的列表. 每一个都只需要 x 的绑定. 每个情况下都是相同的绑定, 但是这十个闭包对象可能是相同也可能不同. 另一方面, 下面表达式的结果

```LISP
(let ((funs '()))     
  (dotimes (j 10)
    (let ((x 5))
      (push (function (lambda (z)
                      (if (null z) (setq x 0) (+ x z))))
            funs)))
funs)
```

也是十个闭包的列表. 然而, 在这个情况下没有两个闭包对象是相同的因为每一个闭包都隐藏了一个不同的 x 的绑定, 并且由于 setq 的使用这些绑定可以从行为上区分.

下面这个表达式的结果

```LISP
(let ((funs '()))
  (dotimes (j 10)
    (let ((x 5))
      (push (function (lambda (z) (+ x z)))
          funs)))
  funs)
```

是一个可能相同也可能不同的十个闭包对象的列表. 每一个闭包都有一个不同的 x 的绑定, 但是这个绑定是不能区分的, 因为它们的值是相同的且不可变 (这里没有在 x 上进行 setq操作). 一个编译器可以在内部把这个表达式形式转换成下面这种

```LISP
(let ((funs '()))
  (dotimes (j 10)
    (push (function (lambda (z) (+ 5 z)))
          funs))
funs)
```

其中的闭包可能是一样的.

一个闭包没有包含任何变量绑定也是可能的. 在这个代码片段中

```LISP
(mapcar (function (lambda (x) (+ x 2))) y)
```

函数 (lambda (x) (+ x 2)) 没有包含任何外部对象的引用. 在这个情况下, 对于所有的函数表达式可能返回相同的闭包. 

### 3.1.5 <span id = "Shadowing">遮蔽</span>

如果用相同的名字 N 确定词法绑定的两个表达式形式是文本上嵌套的, 那么内部表达式对 N 的引用会引用到内部表达式确定的绑定; 这个内部 N 绑定会遮蔽外部的 N 绑定. 内部表达式外面但是外部表达式的里面对 N d的引用会引用到外部表达式确定的绑定上. 比如:

 (defun test (x z)
   (let ((z (* x 2)))
     (print z))
   z)

通过 let 创建的这个变量 z 的绑定遮蔽了函数 test 的参数绑定. 在 print 表达式中对变量 z 的引用会引用 let 绑定的. 在函数 test 的最后对 z 的引用会引用到 z 命名的参数上.

词法作用域作用的构造就像在每次执行时为每个对象生成新的名称一样. 因此, 动态遮蔽不会发生. 比如:

```LISP
(defun contorted-example (f g x)
  (if (= x 0)
      (funcall f)
      (block here
        (+ 5 (contorted-example g
                                #'(lambda () (return-from here 4))
                                (- x 1))))))
```

细想 (contorted-example nil nil 2) 这个调用. 这个产生 4. 在执行过程中, 这个有三个对 contorted-example 的调用, 通过 2 个 block 交叉存取:

```LISP
(contorted-example nil nil 2)
  (block here1 ...)
    (contorted-example nil #'(lambda () (return-from here1 4)) 1)
      (block here2 ...)
        (contorted-example #'(lambda () (return-from here1 4))
                          #'(lambda () (return-from here2 4))
                          0)
            (funcall f)
                  where f =>  #'(lambda () (return-from here1 4))
                (return-from here1 4)
```

在执行 funcall 的时候, 有两个明显的退出点, 每个显然都在这里. 这个 return-from 表达式被作为 funcall 操作的结果执行时引用引用外面的退出点(here1), 不是内部的那个(here2). 它引用那个退出点在函数的执行中原文可见 (这里通过 #' 语法缩写了) 导致了函数对象是 funcall 调用创建的. <!-- TODO 需要重新校对-->

如果在这个例子中, 一个人打算把 (funcall f) 改为 (funcall g), 那么调用 (contorted-example nil nil 2) 的值会是 9. 这个值会改变是因为 funcall 会导致 (return-from here2 4) 的执行, 从而导致一个从内部的退出点(here2)返回. 当这个发生时, 值 4 会被中间的 contorted-example 调用返回, 5 被加到它上面成了 9, 并且这个值会被外部的 contorted-example 调用返回. 重点是, 退出点的选择与它的最内部或最外层无关; 而是, 它取决于在执行函数时使用lambda表达式打包的词法环境. 

### 3.1.6 <span id = "Extent">范围</span>

Contorted-example 可以工作仅因为在退出点的范围内调用了由f指定的函数. 一旦这个执行的控制流离开了这个 block, 这个退出点就会被废除. 比如:

```LISP
(defun invalid-example ()
  (let ((y (block here #'(lambda (z) (return-from here z)))))
    (if (numberp y) y (funcall y 5))))
```

您可能期望调用 (invalid-example) 通过以下不正确的推理生成5: let 绑定 y 给 block 的值; 这个值是一个从lambda表达式得到的函数. 因为 y 不是一个数字, 它被在值 5 上被调用. 这个 return-from 应该从 here 命名的退出点返回这个值, 从而再一次退出 block 并且把值 5 给 y, 这时 y 是number, 然后把这个值作为调用 invalid-example 的结果返回.

这个论证失败仅因为退出点有动态范围. 这个论证直到 return-from 的执行还是正确的. 这个 return-from 的执行应该会发出一个 control-error 类型的错误, 然而, 不是因为它引用到那个退出点而发出, 但是因为它确实引用到一个退出点并且那个退出点已经被废弃.

对动态退出点绑定的引用, 如catch标记, 指的是最近已建立的该名称的绑定, 该绑定没有被认为是废弃的. 比如:

```LISP
(defun fun1 (x)
  (catch 'trap (+ 3 (fun2 x))))
(defun fun2 (y)
  (catch 'trap (* 5 (fun3 y))))
(defun fun3 (z)
  (throw 'trap z))
```

细想这个调用 (fun1 7). 结果是 10. 在那个 throw 被执行的时候, 这里有两个外部的 catch 带有名字 trap: 一个在程序 fun1 中确定, 并且另外一个在程序 fun2 中确定, 所以这个值 7 是从 fun2 的catch中被返回. 从 fun3 中来看, 在 fun2 中的那个 catch 遮蔽了在 fun1 中的那个. fun2 被定义为

```LISP
(defun fun2 (y)
  (catch 'snare (* 5 (fun3 y))))
```

那么两个退出点会有不同的名字, 因此 fun1 的那个不会被遮蔽. 这个结果会是 7. 

### 3.1.7 <span id = "ReturnValues">返回值</span>

通常调用函数的结果是一个单个的对象. 有时候, 然而, 对于一个函数来说, 计算几个对象并返回它们是很方便的.

为了从一个表达式形式接受一个以上的返回值, 几个特殊表达式中的一个必须被用于请求那些值. 如果一个表达式形式产生了多个值但是没有被这个方式请求, 那么第一个返回值会被给到调用者并且其他的会被丢弃; 如果这个表达式形式提供 0 个值, 那么调用者会收到一个 nil 作为值. <!-- TODO request 怎么翻译-->

下面这段列出了接收多个值的操作符. 这些操作符可以被用于指明去求值的一个或多个表达式形式并且接收这些表达式返回的多个值.

    multiple-value-bind  multiple-value-prog1  return-from  
    multiple-value-call  multiple-value-setq   throw        
    multiple-value-list  return                             

Figure 3-5. 一些可应用于接收多值的操作符

函数 values 可以被用于产生多值. (values) 返回 0 个值; (values form) 返回 form 返回的主要的值; (values form1 form2) 返回两个值, 都是 form1 和 form2 的主要的值; 诸如此类.

见 multiple-values-limit 和 values-list. 

## 3.2 <span id = "Compilation">编译</span>

> * 3.2.1 [Compiler Terminology](#CompilerTerminology)
> * 3.2.2 [编译语义](#CompilationSemantics)
> * 3.2.3 [文件编译](#FileCompilation)
> * 3.2.4 [编译后文件中的字面对象](#LiteralObjectsInCompiledFiles)
> * 3.2.5 [编译器中的异常情况](#ExceptionalSituationsCompiler)

 3.2.1 Compiler Terminology

以下术语被用于这个章节.

编译器是一个将代码转换为一个平台相关的可以被有效的表示和执行的表达式形式的工具. 术语编译器 (compiler) 指的是 compile 和 compile-file 的两个函数.

术语编译后的代码(compiled code)指的是表示编译后程序的对象, 就像被 compile 或者加载一个编译后文件时 load 构造的对象.

术语隐式编译(implicit compilation)指的是在求值的时候编译.

术语字面化对象(literal object) 指的是一个引用的对象或者一个自求值对象或者一个对象是另一个的底层构造. 一个常变量自身不是一个字面化对象.

术语合并(coalesce) 按如下定义. 假定 A 和 B 是源代码中的两个字面化常量, 并且 A' 和 B' 是对应编译后代码中的对象. 如果 A' 和 B' 是 eql 但是 A 和 B 不是 eql, 那么就说 A 和 B 被编译器所合并.

术语最小编译(minimal compilation)指的是编译器必须在编译期执行的动作. 这些动作声明在章节 3.2.2 (Compilation Semantics).

动词过程(verb process)指的是执行最小编译, 确定一个表达式形式的求值时间，并可能求值该表达式(如果需要的话).

术语进一步编译(further compilation)指的是具体实现相关依赖的编译, 而不仅仅是最小编译. 这就意味着, 处理并不意味着完整编译. 块编译和机器特定指令的生成是进一步编译的示例. 进一步编译允许发生在运行时.

与编译相关的4个不同的环境: 启动环境(the startup environment), 编译环境(the compilation environment), 求值环境(the evaluation environment), 还有运行时环境(the run-time environment).

启动环境是编译被调用的 Lisp 镜像的环境.

编译环境由编译器维护, 并用于保存编译器内部使用的定义和声明. 只有正确编译所需的定义部分才会被保存. 编译环境用作编译器调用的宏展开函数的环境参数. 在编译环境中可用的定义是否可以用于启动环境或求值环境中所启动的求值中, 这是未知的.

求值环境是一个运行时环境, 在这个环境中, 要求值的 eval-when 指定的求值的宏展开和代码会被求值. 由编译器发起的所有求值都在求值环境中进行.

运行时环境是将被编译的程序被执行的环境.<!-- TODO 是将被编译还是已被编译-->

编译环境从求值环境继承而来, 并且编译环境和求值环境可能是相同的. 这个求值环境从启动环境中继承而来, 并且这个启动环境和求值环境可能是一样的.

术语编译时(compile time) 指的是编译器处理源代码的那段时间. 在编译时, 只有编译环境和求值环境可用.

术语编译期定义(compile-time definition)指的是编译环境中的定义. 比如, 编译一个文件时, 一个函数如果声明为内联的, 那么它的定义可能被保留在编译环境中. 这个定义可能在求值环境中不可用.

术语运行时(run time) 指的是加载器加载编译后的代码和编译后的代码被执行的那段时间. 在运行时, 只有运行时环境可用.

术语运行时定义(run-time definition)指的是运行时环境中的定义.

术语运行时编译器(run-time compiler) 指的是 compile 函数或者隐式编译, 对于那些编译环境和运行时环境是在同一个Lisp镜像中. 注意当运行时编译器被使用时, 运行时环境和启动环境是一样的. <!-- TODO 需核对-->

### 3.2.2 <span id = "CompilationSemantics">编译语义</span>

从概念上讲, 编译是一个遍历代码的过程, 它使用在编译环境中提供的信息(如声明和宏定义)执行特定的语法和语义分析，并生成等价的、可能更有效的代码.

> * 3.2.2.1 [编译器宏](#CompilerMacros)
> * 3.2.2.2 [最小化编译](#MinimalCompilation)
> * 3.2.2.3 [语义约束](#SemanticConstraints)

#### 3.2.2.1 <span id = "CompilerMacros">编译器宏</span>

编译器宏可以定义一个名称, 它也可以命名一个函数或宏. 这也就是说, 一个函数名可能同时命名函数和编译器宏.

如果 compiler-macro-function 在出现在词法环境中的一个函数名是 true, 这个函数命名一个编译器宏A function name names a compiler macro if compiler-macro-function is true of the function name in the lexical environment in which it appears. 为这个函数名字创建一个词法绑定不止创建一个本地函数或宏定义, 并且也遮蔽了这个名字的编译器宏.

这个 compiler-macro-function 返回的函数是一个两个参数的函数, 称为展开函数(expansion function). 为了展开一个编译器宏, 这个展开函数被宏展开钩子函数所调用, 这个展开函数作为第一个函数, 这个完整的编译器宏表达式作为第二个参数, 并且当前的编译环境(或者是当前词法环境, 如果表达式在 compile-file 后被其他处理过) 作为第三个参数. 然后, 宏展开钩子函数将展开函数称为第一个参数而环境则是第二个参数. 扩展函数的返回值, 由宏展开钩子函数传递, 可能是相同的表达式, 或者是另一种表达式, 在执行展开的代码中, 可以替换到原始表达式形式的位置上.

    *macroexpand-hook*  compiler-macro-function  define-compiler-macro  

Figure 3-6. 应用于编译器宏的定义的名字

##### 3.2.2.1.1 编译器宏的目的

这个编译器宏机制的目的是允许选择性的源代码转换为编译器的优化建议. 当一种复合表达式被处理(如由编译器), 如果操作符命名了一个编译器宏, 那么这个编译器宏函数可能在这个表达式上被调用, 而结果的展开则递归地处理, 按照其通常的解释为函数形式或宏形式进行处理.

一个编译器宏函数, 就像一个宏函数, 是一个两个参数的函数: 整个调用的表达式和环境对象. 不像一个普通的宏函数, 一个编译器宏函数可以通过返回与原始表单相同的值来提供扩展. 如果编译器宏破坏性地修改的它的表达式部分的参数, 那么结果是未定义的.

传递给编译器宏函数的表达式可以是一个 car 部分是一个函数名的列表或者一个 car 部分是 funcall 并且 cadr 部分是一个列表 (或函数名); 注意, 这会影响编译器宏函数对表单参数的破坏. define-compiler-macro 会为两种可能的格式正确地执行对参数的破坏. <!-- TODO 需校对-->

当 compile-file 选择展开顶级表达式作为编译器宏表达式时, 展开后的也被当作顶级表达式来处理, 以便在 eval-when 的处理; 见章节 3.2.3.1 (Processing of Top Level Forms). 

##### 3.2.2.1.2 编译器宏的命名

编译器宏可能定义Compiler macros may be defined for function names that name macros as well as functions.

编译器宏定义是严格的全局定义. 在 macrolet 定义本地宏的方式中, 没有定义本地编译器宏的规定. 一个函数名字的词法绑定会遮蔽任何和这个名字关联的编译器宏, 还有和这个名字关联的全局的函数和宏定义.

注意，编译器宏定义的存在不会影响访问函数定义(比如, fboundp)或宏定义(比如, macroexpand)的函数返回的值. 编译器宏是全局的, 并且函数 compiler-macro-function 足以解决它们与其他词法和全局定义之间的交互作用. 

##### 3.2.2.1.3 编译器宏被使用的时候

一个函数或宏的编译器宏定义的存在表明, 编译器应该使用编译器宏的展开, 而不是原来的函数表达式或宏表达式. 然而, 任何语言处理器(编译器, 求值器, 或者其他的 code walker)都不需要实际调用编译器宏函数, 或者如果它调用了编译器宏函数, 就可以使用结果展开.

当编译器在处理过程中遇到一个对编译器宏名称的调用(这不是声明的非内联)时, 编译器可能会展开编译器宏, 并可能使用展开的表达式代替原来的表达式.

当 eval 在处理过程中遇到一个对编译器宏名称的调用(不是声明为非内联的)的调用时, eval 可能会展开编译器宏, 并可能使用展开的表达式代替原来的表达式.

这里有编译器宏定义一定不能被任何语言处理器所使用的两种情况:

    与编译器宏关联的全局函数名绑定被函数名的词法绑定所遮蔽.

    函数名已被声明或声明为非内联, 而调用表达式出现在声明的范围内.

在其他情况下, 编译器宏是否被展开或使用是未知的.

###### 3.2.2.1.3.1 关于编译器宏实现的注意事项

虽然从技术上讲, 这在技术上是允许的, 但是对于 eval 在与编译器相同的情况下处理编译器宏, 这在解释具体实现中并不一定是个好主意.

编译器宏的存在目的是为了用编译时速度换取运行时速度. 编写编译器宏的程序员倾向于假设编译器宏比正常函数和宏花费更多的时间来生成代码, 因此正常的函数和宏在运行时是特别适合的. 由于 eval 在解释具体实现中可能多次执行相同形式的语义分析, 因此在每次这样的求值中选择调用编译器宏可能会很低效.

然而, 关于在这些情况下应该做什么的决定留给每个具体实现. 

#### 3.2.2.2 <span id = "MinimalCompilation">最小化编译</span>

最小化编译根据如下定义:

    在编译时, 正在编译的源代码中出现的所有编译器宏调用都被展开了; 它们不会在运行时展开.

    正在编译的源代码中出现的所有宏和符号宏调用都在编译时进行了展开, 这样它们就不会在运行时再次被展开. macrolet 和 symbol-macrolet 实际上被替换为与它们的主体相对应的表达式，在这些表达式中，对宏的调用被它们的展开表达式所取代.

    在 compile 处理的源代码中一个 load-time-value 表达式的第一个参数在编译时被求值; 在 compile-file 处理的源代码中, 编译器安排它在加载时被求值. 不论发生何种情况, 这个求值的结果会被记住并且在执行时被用于后面 load-time-value 表达式的值. 

#### 3.2.2.3 <span id = "SemanticConstraints">语义约束</span>

所有符合规范的程序必须符合以下约束, 这些被设计用于最小化编译和解释程序的可观测差异:

    任何引用的宏的定义必须在编译环境中出现. 任何在编译环境中不是以特殊操作符或宏定义的符号开始的表达式, 编译器将其作为函数调用来处理.

    必须在编译环境中对动态变量进行特殊的声明. 编译环境中没有特殊声明或声明的任何绑定都被编译器视为词法绑定.

    在编译环境中定义和声明为内联的函数的定义在运行时必须是相同的.

    在一个名为 F 的函数中, 这个编译器可能 (但不是必须) 假定一个对名为 F 的函数明显的递归调用指向 F 的相同的定义, 除非这个函数已经被声明为非内联的. 在执行时, 重定义这样一个递归函数 F 的结果是没有定义的.<!-- TODO 待校对 -->

    一个文件内的一个调用函数, 这个函数在同一个文件中定义, 除非该函数被声明为非内联的, 否则指的就是该函数. 如果函数在运行时被单独定义或者在一个文件里多次定义, 那么结果是未知的.

    所有在编译时声明 ftype 的函数的参数语法和返回值数量必须在运行时保持不变.

    在编译环境中定义的常量变量在运行时必须具有相似的值. 源代码中一个常量变量的引用等价于对一个常量变量值的对象的引用.

    用 deftype 或者 defstruct 在编译环境中定义的类型必须和运行时保持相同的定义. 被 defclass 在编译环境中定义的类必须和运行时定义的有相同的超类和元类.

    这个也就意味着 subtype/supertype 类型声明的关系必须在编译时和运行时保持不变.

    在编译环境中出现的类型声明必须在运行时准确地描述对应的值; 否则, 结果就是未定义的. 在编译时, 一个未知类型出现在声明中是允许的, 但是在这种情况下可能会发出警告.

    除了上面明确列出的情况外, 在求值环境中定义的函数可以在运行时具有不同的定义或不同的签名, 并且运行时定义生效.

不应该使用任何关于运行时环境与启动、评估和编译环境之间一致性的附加假设来编写符合标准的程序.

除非另行注明, 当一个编译期定义和运行时不同, 会在运行出现以下情况的一种:

    an error of type error is signaled
    the compile-time definition prevails
    the run-time definition prevails

如果编译器处理的是在编译时没有定义操作符的函数表达式, 那么编译时就不会发出错误. 

### 3.2.3 <span id = "FileCompilation">文件编译</span>

函数 compile-file 对一个文件中的表达式根据章节 3.2.2 (Compilation Semantics) 中的规则执行编译, 并且产生一个通过 load 被载入的输出考虑文件.

通常情况下, 在 compile-file 编译的文件中出现的顶级表达式只在加载编译后文件时才进行求值, 而不是在编译文件时才进行评估. 但是, 通常情况下, 文件中的某些表达式需要在编译时进行求值，以便能够正确地读取和编译文件的其余部分.

这个 eval-when 特殊表达式可以被用于控制一个顶级表达式形式是否在编译时, 加载时, 或者都求值. 用 eval-when 指定这三种情况的任何几个都是可以的, 通过符号 :compile-toplevel, :load-toplevel, 还有 :execute 来指明. 对于顶层的 eval-when 表达式, :compile-toplevel 表示编译器必须在编译时求值这个主体部分, 并且 :load-toplevel 表示这个编译器必须安排在加载时求值这个主体. 关于非顶级的 eval-when 表达式, :execute 表示这个主体必须在运行时环境被求值.

这种表达式的行为可以更精确地理解为 compile-file 如何处理一个文件要被编译的表达式的模型. 这个有两种处理模型, 称之为 "not-compile-time" 还有 "compile-time-too".

文件中的连续表达式被 compile-file 读取并且在 not-compile-time 模式下被处理; 在这个模式下, compile-file 安排表达式只在加载时被求值而不是在编译时. 当 compile-file 在 compile-time-too 模式下, 表达式在加载和编译时都求值.

#### 3.2.3.1 顶层表达式的处理

在文件编译器中对顶级表达式的处理定义如下:

    如果这个表达式是一个编译器宏表达式 (没有被一个非内联声明所禁用), 具体实现可能或可能不选择计算该表达式的编译器宏展开, 并且执行了展开, 可能也可能不会选择在相同的处理模式(compile-time-too 或 not-compile-time)下将结果作为顶级表达式进行处理. 如果它拒绝获取或使用展开, 它必须处理原始的表达式.

    如果这个表达式是一个宏表达式, 它的宏展开会被作为顶层表达式在相同的处理模式下(compile-time-too 或 not-compile-time)计算并处理.

    如果表达式是一个 progn 表达式, 它的主体表达式中的每一个子表达式会被依次当作顶层表达式在相同的处理模式下处理.

    如果这个表达式是一个 locally, macrolet, 或者 symbol-macrolet, compile-file 建立适当的绑定并将主体表达式作为顶级表单处理, 并在相同的处理模式下执行这些绑定. (注意, 这意味着顶层表达式处理的词法环境不一定是空词法环境.)

    如果表达式是一个 eval-when 表达式, 它会根据下面这段被处理.

CT | LT | E | Mode | Action | New Mode
- | :-: | -: | - | - | -
Yes | Yes | --- | --- |  Process |  compile-time-too 
No |  Yes | Yes | CTT |  Process |  compile-time-too  
No  | Yes | Yes | NCT |  Process |  not-compile-time  
No  | Yes | No |  --- |  Process |  not-compile-time  
Yes | No |  --- | --- |  Evaluate | ---               
No |  No |  Yes | CTT |  Evaluate | ---               
No |  No |  Yes | NCT |  Discard |  ---               
No |  No |  No |  --- |  Discard |  ---               

Figure 3-7. EVAL-WHEN 处理

    列 CT 表示 :compile-toplevel 是否被指定. 列 LT 表示 :load-toplevel 是否被指定. 列 E 表示 :execute 是否被指定. 列 Mode 表示处理模式; 一个代字号 (---) 表示处理模式是不相关的.

    这个 Action 列指明三种动作中的一种:

    Process: 在指定模式下把主体作为顶层表达式处理.

    Evaluate: 在编译器的动态执行上下文中对主体进行求值, 使用求值环境作为全局环境和 eval-when 出现的词法环境.

    Discard: 忽略这个表达式.

    这个 New Mode 列表示新的处理模式. 一个代字符 (---) 表示编译器保留当前模式.

    否则, 这个表达式就是一个顶层表达式而不是特殊情况下的一个表达式. 在 compile-time-too 模式下, 这个编译器首先在求值环境中对表达式进行求值, 然后对其进行最低限度的编译. 在 not-compile-time 模式下, 表达式只是简单地编译了. 所有子表达式被当作非顶层(non-top-level) 表达式.

    注意, 顶层表达式是按照在文件中显示的顺序处理的, 并且编译器读取的每个顶层表达式都在读取下一个之前进行处理. 然而, 只要 Common Lisp 的语义被保留, 就不指定非顶层子表达式(包括宏展开的)的处理顺序还有进一步编译的顺序.

eval-when 表达式导致的编译期求值仅限顶层. 对于非顶层表达式 :compile-toplevel 和 :load-toplevel 情况声明都会被忽略. 对于非顶层表达式, 一个 eval-when 指定为 :execute 的情况被当作一个隐式的 progn 包括 eval-when 表达式的主体部分; 否则, 在主体中的表达式就被忽略.

3.2.3.1.1 定义宏的处理

出现在一个文件中的定义宏 (就像 defmacro 或者 defvar) 会被 compile-file 处理, 正常情况下会有编译期副作用, 影响同一个文件中后面的表达式的编译. 解释这些副作用是如何发生的一个很方便的模型是, 定义宏被展开为一个或多个 eval-when 表达式, 然后这个调用会导致发生在一个 (eval-when (:compile-toplevel) ...) 表达式的主体部分的编译器副作用.

编译期的副作用可能会导致对定义的信息的存储方式不同于定义宏以"正常"方式处理 (要么是解释, 要么是加载已编译的文件).

特别地, 编译时定义宏所存储的信息可能或可能无法用于解释器 (不管是编译时或是编译后), 或者是后续的编译器的调用. 比如, 下面这段代码是不可移植的因为它假定对于解释器可用时编译器也存储了 foo 宏定义:

```LISP
(defmacro foo (x) `(car ,x))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (print (foo '(a b c))))
```

一种完成相同工作的可移植的方式是把宏定义包含在 eval-when 表达式里, 就像:

```LISP
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro foo (x) `(car ,x))
  (print (foo '(a b c))))
```

下面这段列出了使得定义可以在编译时和运行时环境都可用的宏. 没有指定在编译环境中可用的定义是否在求值环境中可用, 也没有指定它们在后续编译单元中是否可用, 或者在编译器的后续调用中是否可用. 与 eval-when 一样, 这些编译期副作用只发生在定义宏出现在顶层时.

    declaim                define-modify-macro   defsetf    
    defclass               define-setf-expander  defstruct  
    defconstant            defmacro              deftype    
    define-compiler-macro  defpackage            defvar     
    define-condition       defparameter                     

Figure 3-8. 影响编译环境的定义宏

##### 3.2.3.1.2 对宏和编译器宏的限制

除了在显式声明之外, 在 Common Lisp 标准中定义的宏都不会产生展开式, 这个展开式可能导致宏表达式的任何子表达式都被当作顶层表达式来处理. 如果一个具体实现还提供了一个 Common Lisp 宏的特殊操作符定义, 那么这个特殊操作符定义必须在这方面是语义上等价的.

编译器宏展开还必须具有与它们所取代的表达式相同的顶层求值语义. 这是对符合规范的实现和符合规范的程序的都需要关注的. 

### 3.2.4 <span id = "LiteralObjectsInCompiledFiles">编译后文件中的字面对象</span>

函数 eval 和 compile 需要去确保字面对象引用的解释和编译结果的对象和对应源代码中的是一样的. 从另一方面说, compile-file 必须产生一个编译后的文件, 它被 load 加载时, 构建源代码中定义的对象并且产生指向它们的引用.

在 compile-file 的情况下, 编译后的文件所构建的对象不能被认为与编译时构造的对象相同, 因为编译后的文件可能被加载到不同的 Lisp 镜像中，而不是它编译时的镜像. 这个部分定义了相似的概念, 它将求值环境中的对象与运行时环境中的相应对象关联起来.

在这个章节中描述的字面对象约束只能应用于 compile-file; eval 和 compile 不会拷贝或合并常量.

> * 3.2.4.1 [可外部化的对象](#ExternalizableObjects)
> * 3.2.4.2 [字面化对象的相似性](#SimilarityLiteralObjects)
> * 3.2.4.3 [相似性规则的扩展](#ExtensionsSimilarityRules)
> * 3.2.4.4 [Additional Constraints on Externalizable Objects](#ACEO)

#### 3.2.4.1 <span id = "ExternalizableObjects">可外部化的对象</span>

事实上文件编译器在编译后的文件中额外表示字面对象并且在文件被加载时重构这些对象和适合的等价对象, 这也就需要去约束文件编译器处理的代码中的可以被用作字面化对象的对象特性.

可以在一个将要被文件编译器处理的代码中作为一个文本对象使用的对象被称为可外部化对象.

如果两个对象满足概念上等价断言(下面定义的), 我们就定义这两个对象是相似的, 不依赖于 Lisp 镜像所以两个在不同 Lisp 镜像中的对象可以在这个断言下理解为等价的. 更进一步, 通过检查这个概念性断言的定义, 程序员可以通过文件编译来预测对象的哪些方面是可靠的.

文件编译器必须与加载器合作, 以确保在每个情况下，一个可外部化的对象作为一个字面化对象处理，加载器将构造一个类似的对象.

新概念术语"similar"为了可外部化对象的对象集合而定义, 如此以致于编译后的文件被加载时, 一个和文件编译器操作时存在的原始对象相似的对象会被构建. 

#### 3.2.4.2 <span id = "SimilarityLiteralObjects">字面化对象的相似性</span>

##### 3.2.4.2.1 聚合对象的相似性

在已定义相似性以外类型中, 一些被当作是聚合对象Of the types over which similarity is defined, some are treated as aggregate objects. 对于这些类型, 相似性是递归定义的. 我们说这些类型的对象有着确定的基本特性并且为了满足相似性关系, 两个对象的对应特性的值也必须是相似的. <!-- TODO 待校对 -->

##### 3.2.4.2.2 相似性的定义

两个对象 S (在源代码中) 和 C (在编译后的代码中) 仅当它们同时都是这里列出的类型(或者具体实现定义的)中的一个并且它们都满足该定义指明的所有额外需求就被定义为相似的.

number

    两个数字 S and C 如果它们是相同的类型并且表示相同的数学上的值, 那么它们就是相似的.

character

    如果两个简单字符 S 和 C 有相似的代码属性, 那么它们就是相似的.

    具体实现提供的额外的依赖于实现的属性必须定义非简单字符是否可以被当作相似的并且如何被当作相似的.

symbol

    如果两个显而易见的非内部符号它们的名字是相似的, 那么它们就是相似的.

    如果两个内部符号 S 和 C 名字是相似的并且编译时 S 可以在当前包访问而加载时 C 可以在当前包访问, 或者 C 在 S home包的相似包中可访问, 那么它们就是相似的.<!-- TODO home package ?? -->

    (注意, 符号的相似性不依赖于当前的读取表, 也不依赖函数 read 如何来解析符号名称中的字符.)

package

    如果两个包 S 和 C 的名字是相似的, 那么它们就是相似的.

    注意, 虽然一个包对象是一个可外部化的对象, 但是当将代码引用为一个字面化对象时, 程序员有责任确保相应的包已经存在. 加载器通过调用带有该名称的 find-package 来查找相应的包对象. 如果加载时包不存在, 则加载器会发出一个错误.

random-state

    如果两个随机状态 S 和 C 中假定每种情况下 limit 参数等价, 当一个 C 的拷贝作为 random-state 参数给函数 random 时 S 一直产生相同的伪随机数序列, 那么它们就是等价的.

    (注意, 因为 C 已经被文件处理器处理过了, 它不能被直接用于 random 的参数因为 random 会有副作用.)

cons

    两个cons S 和 C 如果它们的 car 和 cdr 部分分别都是相似的, 那么它们就是相似的.

array

    如果两个一维的数组 S 和 C 的长度以及实际中数组元素的类型还有每一个可用的元素对应也是相似的, 那么它们就是相似的.

    两个一维以外的数组 S 和 C 如果它们的维度是相似的, 每一个对应的维度也是相似的, 每一个对应实际数组元素的类型也是相似的, 并且每一个对应的元素也是相似的, 那么它们就是相似的.

    另外, 如果 S 是一个简单数组, 那么 C 也必须是一个简单数组. 如果 S 是一个带有一个填充指针或者实际的 adjustable 内容的偏移数组, 那么 C 被允许缺乏任何或所有这些特性.

hash-table

    如果两个哈希表 S 和 C 满足下面三条需求, 那么它们就是相似的:

        它们都有相同的 test 部分(不如, 它们都是 eql 哈希表).

        两个哈希表的键之间存在唯一的一对一匹配, 这样对应的键就是相似的.

        对于所有的键, 与两个表中对应键关联的值是相似的.

    如果在 S 和 C 中的键不止一个一对一匹配, 结果是无法定义的. 一个符合规范的程序不能使用像 S 的表作为可外部化常量.

pathname

    如果两个路径名 S 和 C 对应的路径名成分是相似的, 那么它们就是相似的.

function

    函数不是可外部化对象.

structure-object and standard-object

    对于结构体和标准对象不存在相似性的通用概念. 然而, 一个符合规范的程序允许去为这个程序所定义的 structure-object 或 standard-object 的子类 K 定义一个 make-load-form 方法. 这样一个方法的影响就是定义当源代码中类型 K 的对象 S 和 编译后的代码中类型 K 的对象中 C 是通过对 S 调用 make-load-form 构造出来的时, S 和 C 就是相似的. 

#### 3.2.4.3 <span id = "ExtensionsSimilarityRules">相似性规则的扩展</span>

一些对象, 就像流, 读取表还有方法在上面给定的相似性定义下不是可外部化的对象. 这就是说, 这些对象可能不能作为文件编译器处理的代码中的可移植文字对象出现.

一个具体实现允许去扩展相似性规则, 这样其他种类的对象对于这个具体实现也是可外部化对象.

如果对于一些类型的对象, 相似性没有被这个说明文档和具体实现所定义, 那么文件编译器在遇到这样一个对象作为字面化对象时会发出一个错误. 

#### 3.2.4.4 <span id = "ACEO">外部化对象的附加约束</span>

如果在文件编译器处理的单个文件中出现的两个字面化对象是相同的, 那么编译后的代码中相应的对象也必须是相同的. 除了符号和包, 文件编译器处理的代码中的任何两个相似的文本对象都可以合并; 如果它们是两个符号或者两个包, 它们可能只有在它们是相同的前提下才会合并.

包含循环引用的对象可以是可外部化对象. 文件编译器需要在一个文件中保存子结构的 eqlness. 保留 eqlness 意味着在源代码中相同的子对象在相应的编译代码中必须是相同的.

另外, 下面是文件编译器对字面化对象处理的约束:

array: 如果一个在源代码中的数组是一个简单数组, 那么在编译后代码中的对应数组也是一个简单数组. 如果一个数组在源代码中是一个填充数组, 带有一个填充指针或者一个实际的 adjustable 值, 那么编译后代码中的对应数组可能缺少任何或者所有这些特性. 如果一个源代码中的数组有一个填充指针, 那么编译后的代码中对应的数组可能只是填充指针所暗示的大小.

packages: 加载器需要去找到对应包对象, 就像通过调用以包名为参数的 find-package 一样. 如果加载的时候这个名字对应的包不存在, 那么就会发出一个 package-error 类型的错误.

random-state: 一个不变的 random 状态对象不能作为函数 random 的状态的参数应为这个函数会修改这个数据结构.

structure, standard-object: 如果存在类型适合的 make-load-form 方法那么 structure-object 和 standard-object 类型的对象也可能出现在编译后的常量中.

    如果被引用为文字对象的对象是 standard-object, structure-object, condition, 或者任何依赖于实现的类 (可能是 empty) ,那么文件编译器就会在这个对象上调用 make-load-form 对于单个文件中任何给定的对象文件编译器只会调用一次 make-load-form 方法.

symbol: 为了保证编译后的文件能够正确加载, 用户必须确保这些文件中引用的包在编译时和加载时一致地定义. 符合规范的程序必须满足以下要求:

        被 compile-file 处理的文件中的顶层表达式所在的当前包必须和编译后文件中对应顶层表达式被 load 执行时所在当前包相同. 特别地:

        a. 任何修改当前包的文件中的顶层表达式都必须在编译时和加载时将其更改为同一名称的包.

        b. 如果文件中的第一个非原子级顶层表达式不是一个 in-package 的表达式, 那么调用 load 时的当前包的名称必须是与 compile-file 被调用时当前包的名字相同.

        在一个顶层表达式在编译期被处理期间, 对于词法上出现在这个顶层表达式并且可以通过当前包来访问而 home 包是另一个包的所有符号, 在加载时当前包和编译时 home 包也必须存在一个可以访问的, 相同名字的符号. <!-- TODO 待校对-->

        对于所有出现在编译后的文件中, 编译时在 home 包里是外部符号的符号, 在加载时这个包里也必须存在名字相同的外部符号. <!-- TODO 待校对-->

    如果其中任何一个条件都不成立, 那么加载器查找受影响的符号的包就不确定了. 具体实现允许去发出一个错误或者定义这个行为. 

### 3.2.5 <span id = "ExceptionalSituationsCompiler">编译器中的异常情况</span>

compile 和 compile-file 允许去发出错误和警告, 包括由于处理编译期的 (eval-when (:compile-toplevel) ...) 表达式, 宏展开, 还有 conditions 时编译器自身发出的警告.

在不进行干预编译就不能处理的情况下, 类型错误的状况可能由编译器发出.

除了这个标准指定的必须或者可能发出警告类型状况的情况外, 在编译器可以确定结果未定义或者一个运行时错误会发出情况下也可能发出警告. 以下是这种情况的示例: 违反类型声明, 对 defconstant 定义的常量的值赋值或修改, 用错误数量的参数或者残缺的关键字列表调用内置的 Lisp 函数, 还有不可识别的声明标识.

编译器允许去提出一个关于编程风格问题, 作为 style-warning 类型的状况警告. 以下是这个情况的示例: 使用不同的参数列表重定义一个函数, 用错误数量的参数调用一个函数, 没有对一个没有引用到的本地变量声明 ignore, 还有引用一个声明为 ignore 的变量.

compile 和 compile-file 都允许(但不是必须)去为一个类型错误状况确定一个处理者. 比如, 它们可能会发出一个警告, 并且从一些依赖于具体实现的点重启来让编译在没有手动干预的情况下进行下去.

compile 和 compile-file 都返回 3 个值, 前两个表示被编译的源代码中是否有错误还有是否提出风格警告.

一些警告可能会被推迟到编译结束的时候. 见 with-compilation-unit. 

## 3.3 <span id = "Declarations">声明</span>

声明(Declarations)提供了一种指定例如求值器或者编译器这样的程序处理器使用的信息的方式.

局部声明可以通过 declare 嵌入到可执行的代码中. 全局声明, 或者 proclamations, 可以通过 proclaim 或者 declaim 来确定.

这个 the 特殊表达式提供了一种简写标记来创建一个关于给定表达式的值的类型的局部声明.

如果一个程序违反了声明和公告, 结果是没有定义的.

> * 3.3.1 [最低的声明处理需求](#MDPR)
> * 3.3.2 [声明指定](#DeclarationSpecifiers)
> * 3.3.3 [声明标识](#DeclarationIdentifiers)
> * 3.3.4 [声明的作用域](#DeclarationScope)

### 3.3.1 <span id = "MDPR">最低的声明处理需求</span>

通常, 一个具体实现可以自由地忽视除了 declaration, notinline, safety 还有 special 以外的声明指定.

一个 declaration 声明必须抑制关于不识别这个种类的声明的警告. 如果一个实现没有产生关于不识别声明的警告, 它可能安全地忽视了这个声明.

一个 notinline 声明必须被任何支持内联函数或者编译器宏的实现所识别进而废弃那些机制. 一个不使用内联函数或者编译器宏的实现可能会安全地忽略这个声明.

一个 safety 声明必须被识别, 它会提高当前的安全等级. 一个始终对代码进行处理的实现, 就像 safety 高一样, 可能安全地忽略这个声明.

一个 special 声明必须被所有实现所处理declaration must be processed by all implementations. 

### 3.3.2 <span id = "DeclarationSpecifiers">声明指定</span>

一个声明指定是一个可以出现在层定的 declare 表达式或者一个 declaim 表达式或者作为参数给 proclaim. 它是一个的列表, 其中 car 部分为声明标识, cdr 部分为根据这个声明标识所指定的规则解释出来的数据.
<!-- TODO declaration specifier ??--> 

### 3.3.3 <span id = "DeclarationIdentifiers">声明标识</span>

下面这段展示了这个标准定义的所有的声明标识.

    declaration     ignore     special  
    dynamic-extent  inline     type     
    ftype           notinline           
    ignorable       optimize            

Figure 3-9. Common Lisp 声明标识

一个具体实现可以自由地去支持其他(依赖实现)声明标识. 如果一个声明标识没有在上面定义, 也没有被具体实现所定义, 不是一个类型名字, 也没有在 declaration proclamation 中声明, 可能会发出一个警告.<!-- TODO declaration proclamation ?? -->

#### 3.3.3.1 类型声明的简写标记

类型说明符可以用作声明标识符. (type-specifier var\*) 可以当作 (type type-specifier var\*) 的简写. 

### 3.3.4 <span id = "DeclarationScope">声明的作用域</span>

声明可以被分成两种类型: 一些适用于变量或函数的绑定; 一些则不适用于绑定.

一个出现在绑定表达式的头部并且适用于这个表达式创建的变量或函数的绑定的声明称之为绑定声明; 这个绑定会影响这个声明作用域内的绑定和任何绑定的引用.

不绑定声明的声明称为自由声明(free declarations).

在表达式 F1 中, 一个自由声明, 它适用于由某些表达式 F2 所建立的一个名字 N 的绑定, 其中F1是一个子表达式, 它只影响 N 在F1中的引用; 它不适用于其他在 F1 以外的绑定, 也不影响 F2 中建立的 N 绑定的行为.

不适用于绑定的声明只能以自由绑定出现.

一个绑定声明的作用域和它对应的绑定的词法作用域相同; 对于特殊变量, 这意味着绑定的作用域是一个词法绑定 this means the scope that the binding would have had had it been a lexical binding.<!-- TODO 待校对 -->

除非明确声明, 自由声明的作用域只包括它出现在头部的表达式的主体的子表达式, 不包括其他的子表达式. 自由声明的作用域不包括包含声明的表达式所建立的绑定的初始化表达式.

一些循环表达式包含 step, end-test, 或者 result 子表达式, 这些子表达式也包含在循环表达式中出现的声明作用域内. 具体地说, 调用的循环表达式和子表达式是:

    do, do*: step-forms, end-test-form, and result-forms.
    dolist, dotimes: result-form
    do-all-symbols, do-external-symbols, do-symbols: result-form

#### 3.3.4.1 声明作用域的示例

下面是一个示例, 说明了绑定声明的作用域.

```LISP
(let ((x 1))                ;[1] 1st occurrence of x
  (declare (special x))     ;[2] 2nd occurrence of x
  (let ((x 2))              ;[3] 3rd occurrence of x
    (let ((old-x x)         ;[4] 4th occurrence of x
          (x 3))            ;[5] 5th occurrence of x
      (declare (special x)) ;[6] 6th occurrence of x
      (list old-x x))))     ;[7] 7th occurrence of x
=>  (2 3)
```

由于 x 的 special 声明在第二行, 第一个出现的 x 确定一个 x 的动态绑定. 第三个出现的 x 确定是一个 x 的词法绑定 (因为在对应的 let 表达式里没有 special 声明). 第四个出现的 x 是一个指向第三行确定的 x 的词法绑定的引用. 第五个出现的 x 为 let 表达式的 body 部分建立一个 x 的动态绑定因为这个 x 的特殊声明在第六行. 第四行的 x 引用没有被第六行的 special 声明影响因为这个引用不是在第五行变量 x 的词法作用域内. 第七行 x 的引用是第五行建立的 x 的动态绑定的引用.

这里是另一个示例, 用来介绍自由声明的作用域. 如下:

```LISP
(lambda (&optional (x (foo 1))) ;[1]
  (declare (notinline foo))     ;[2]
  (foo x))                      ;[3]
```

第一行对 foo 的调用可能会被编译为内联即便第三行对 foo 的调用一定不会的情况下. 这是因为第二行对 foo 的 notinline 声明只适用于第三行的主体部分. 为了抑制每次调用的内联, 一种方式是这么写:

```LISP
(locally (declare (notinline foo)) ;[1]
  (lambda (&optional (x (foo 1)))  ;[2]
    (foo x)))                      ;[3]
```

或者, 换种方式:

```LISP
(lambda (&optional                               ;[1]
          (x (locally (declare (notinline foo)) ;[2]
                (foo 1))))                       ;[3]
  (declare (notinline foo))                      ;[4]
  (foo x))                                       ;[5]
```

最后, 这里有一个循环表达式声明的作用域的示例.

```LISP
(let ((x  1))                     ;[1]
  (declare (special x))           ;[2]
    (let ((x 2))                  ;[3]
      (dotimes (i x x)            ;[4]
        (declare (special x)))))  ;[5]
=>  1
```

在这个例子, 第四行的第一个 x 引用是只第三行建立的 x 的词法绑定. 然而, 出现在第四行的第二个 x 位于第五行的自由声明的作用域内 (因为这个是 dotimes 的 结果表达式(result-form)) 并且因此引用 x 的动态绑定. 

## 3.4 <span id = "LambdaLists">Lambda列表</span>

一个lambda列表是一个指明了参数集合(有时也称为lambda变量)和一个用于接收和这些参数有关的值的规程.

这里有几种lambda列表的类型.

    Context                                      Kind of Lambda List                              
    defun form                                   ordinary lambda list                             
    defmacro form                                macro lambda list                                
    lambda expression                            ordinary lambda list                             
    flet local function definition               ordinary lambda list                             
    labels local function definition             ordinary lambda list                             
    handler-case clause specification            ordinary lambda list                             
    restart-case clause specification            ordinary lambda list                             
    macrolet local macro definition              macro lambda list                                
    define-method-combination                    ordinary lambda list                             
    define-method-combination :arguments option  define-method-combination arguments lambda list  
    defstruct :constructor option                boa lambda list                                  
    defgeneric form                              generic function lambda list                     
    defgeneric method clause                     specialized lambda list                          
    defmethod form                               specialized lambda list                          
    defsetf form                                 defsetf lambda list                              
    define-setf-expander form                    macro lambda list                                
    deftype form                                 deftype lambda list                              
    destructuring-bind form                      destructuring lambda list                        
    define-compiler-macro form                   macro lambda list                                
    define-modify-macro form                     define-modify-macro lambda list                  

Figure 3-10. 要使用的lambda列表的种类

下面这段列出了可应用于lambda列表的定义的名字.

    lambda-list-keywords  lambda-parameters-limit    

Figure 3-11. 可应用于lambda列表的定义的名字

> * 3.4.1 [普通lambda列表](#OrdinaryLambdaLists)
> * 3.4.2 [广义函数lambda列表](#GenericFunctionLambdaLists)
> * 3.4.3 [特定的lambda列表](#SpecializedLambdaLists)
> * 3.4.4 [宏lambda列表](#MacroLambdaLists)
> * 3.4.5 [解构lambda列表](#DestructuringLambdaLists)
> * 3.4.6 [Boa Lambda列表](#BoaLambdaLists)
> * 3.4.7 [Defsetf Lambda列表](#DefsetfLambdaLists)
> * 3.4.8 [Deftype Lambda列表](#DeftypeLambdaLists)
> * 3.4.9 [Define-modify-macro Lambda列表](#DefineMMLambdaLists)
> * 3.4.10 [Define-method-combination 参数Lambda列表](#DefineMCArgumentsLambdaLists)
> * 3.4.11 [文档字符串和声明的语法交互](#SIDSD)

### 3.4.1 <span id = "OrdinaryLambdaLists">普通lambda列表</span>

一个普通lambda列表被用于描述一个参数集合如何被普通函数所接收. 下面定义的名字使用普通lambda列表:

    define-method-combination  handler-case  restart-case  
    defun                      labels                      
    flet                       lambda                      

Figure 3-12. 使用普通lambda的标准操作

一个普通lambda列表可以包含下面这段展示的lambda列表关键字.

    &allow-other-keys  &key       &rest  
    &aux               &optional         

Figure 3-13. 普通lambda列表使用的lambda列表

lambda列表中的每一个元素是一个参数说明符或者一个lambda列表关键字. 具体实现可以自由地提供额外的lambda列表关键字. 对于一个实现中使用的lambda列表关键字, 见 lambda-list-keywords.

普通lambda列表的语法如下:

    lambda-list::= (var* 
                    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                    [&aux {var | (var [init-form])}*]) 

一个 var 或 supplied-p-parameter 必须是一个不是常变量的名字的符号.

一个 init-form 可以是任何表达式形式. 无论何时对于任何参数说明符任何的 init-form 的求值, 那个表达式形式可能引用任何这个说明符左边的参数变量, 包括任何 supplied-p-parameter 变量, 并且依赖没有其他参数变量已经被绑定的事实(包括它自己的参数变量) .<!-- TODO 待校验 -->

一个 keyword-name 可以使任何符号, 但是按照惯例是一个正常的关键字; 所有标准化的实现遵守这个惯例.

一个普通的lambda列表有5个部分, 其中的任何部分或者全部可以是空的. 关于不匹配参数处理的信息, 见章节 3.5 (Error Checking in Function Calls).

> * 3.4.1.1 [必要参数指定符](#SpecifiersRequiredParameters)
> * 3.4.1.2 [可选参数指定符](#SpecifiersOptionalParameters)
> * 3.4.1.3 [剩余参数指定符](#SpecifierRestParameter)
> * 3.4.1.4 [关键字参数指定符](#SpecifiersKeywordParameters)
> * 3.4.1.5 [&aux变量的指定符](#SpecifiersAuxVariables)
> * 3.4.1.6 [普通lambda列表的示例](#ExamplesOrdinaryLambdaLists)

#### 3.4.1.1 <span id = "SpecifiersRequiredParameters">必要参数指定符</span>

这些指的是直到第一个lambda列表关键字为止的参数指定符; 如果这里没有lambda列表关键字, 那么所有指定符都是必要参数. 每一个必要参数被参数变量 var 指定. var 绑定给一个词法变量除非它被声明为 special.

如果这里有 n 个必要参数 (n 可能是 0), 这里至少必须传递 n 个参数, 并且必要参数绑定给前 n 个传递的参数; 见章节 3.5 (Error Checking in Function Calls). 其他参数保留给后面的剩余参数.

#### 3.4.1.2 <span id = "SpecifiersOptionalParameters">可选参数指定符</span>

如果出现 &optional, 可选参数指定符就是那些跟在 &optional 后面直到下一个lambda列表关键字或者直到列表结束的指定符. 如果指定了可选参数, 然后每一个都被处理如下. 如果存在未处理的参数, 则参数变量 var 将绑定到后面的剩余参数, 就像必要参数一样. 如果没有参数剩下, 不管怎样, 那么 init-form 被求值, 并且参数变量被绑定给结果值(如果没有 init-form 出现在参数指定符就是 nil). 如果另一个变量名 supplied-p-parameter 出现在这个指定符, 如果有一个参数可用它会被绑定为 true, 如果没有参数剩余它会被绑定为 false (并且因此 init-form 需要被求值<!-- TODO 待校验 -->). Supplied-p-parameter 不是绑定一个参数而是一个值, 它表示是否为相应的 var 提供了一个对应的参数. 

#### 3.4.1.3 <span id = "SpecifierRestParameter">剩余参数指定符</span>

&rest, 如果出现, 后面必须跟着单个的剩余参数指定符, 反过来后面必须跟着另一个lambda列表关键字或者到lambda列表的末尾<!-- TODO 待校验 -->. 在所有可选参数被处理后, 这里可能是一个剩余参数. 如果这里是一个剩余参数, 它给绑定给一个所有 as-yet-unprocessed 参数的列表. 如果没有未处理参数剩下, 这个剩余参数绑定给空列表. 如果这里没有剩余参数和关键字参数并且有任何未处理参数剩余, 会发出一个错误; 见章节 3.5 (Error Checking in Function Calls). 剩余参数的值是允许的, 但不是必需的, 以便与 apply 最后一个参数共享结构. 

#### 3.4.1.4 <span id = "SpecifiersKeywordParameters">关键字参数指定符</span>

如果出现 &key , 所有直到下一个lambda列表或者列表末尾的指定符都是关键字参数指定符. 当关键字参数被处理, 相同被处理的参数会被做成一个列表作为剩余参数. 同时指定 &rest 和 &key 是允许的. 在这个情况下剩下的参数被同时用于这两种目的; 这就是说, 所有剩下的参数被做成lambda列表作为剩余参数, 也被当作关键字参数处理. 如果指定了 &key, 必须有偶数个参数; 见章节 3.5.1.6 (Odd Number of Keyword Arguments). 这些参数被当作对, 每一对中的第一个参数被解释为一个名字而第二个作为对应的值. 每个对中的第一个对象必须是一个符号; 见章节 3.5.1.5 (Invalid Keyword Arguments). 这个关键字参数指定符可能可选地跟着lambda列表关键字 &allow-other-keys.

在每一个关键字参数指定符中必须是一个名字 var 给参数变量. 如果这个 var 单独出现或者在一个 (var init-form) 组合中, 当匹配参数时参数是一个 KEYWORD 包中名字和 var 相同的符号时这个关键字名字会被使用. 如果这个 ((keyword-name var) init-form) 表示法被使用, 那么这个用于匹配参数的关键字名字是 keyword-name, 它可能是任何包中的符号(当然, 如果它不是 KEYWORD 包中的符号, 它没有必要自求值, 所以当调用这个函数时必须额外关心来确保正常的求值一直跳过这个关键字名字). 因此

```LISP
(defun foo (&key radix (type 'integer)) ...)
```

与下面这个意义相同

```LISP
(defun foo (&key ((:radix radix)) ((:type type) 'integer)) ...)
```

关键字参数指定符和所有参数指定符一样, 实际上从左到右进行处理. 对于每一个关键字参数指定符, 如果这里有一个名字匹配这个指定符的名字的参数对 (这就是说, 名字是 eq 的), 那么这个指定符对应的参数变量绑定给这个参数对的第二项(值部分). 如果不止一个这样的参数对匹配, 会使用最左边的参数对. 如果不存在这样的参数对, 那么这个指定符的对应 init-form 被求值并且这个参数变量绑定给那个值 (如果没有指定 init-form , 那就是 nil). supplied-p-parameter 和 &optional 参数一样的处理方式: 如果这里有匹配的参数对它被绑定为 true, 否则就是 false.

除非关键字参数检测被抑制, 一个参数对必须和一个参数指定符名字匹配; 见章节 3.5.1.4 (Unrecognized Keyword Arguments).

如果关键字参数检测被抑制, 那么一个参数对没有匹配的参数指定符是允许的, 并且这个参数对会被忽略, 但是如果提供了剩余参数, 这样的一个参数是可以通过剩余参数访问的. 这些机制的目的是去允许在多个lambda表达式之间共享参数列表并且允许调用者和被调用的lambda表达式去指定这样的共享是可能发生的.

注意如果 &key 出现了, 一个 :allow-other-keys 关键字参数总是是允许的---不管关联的值是 true 或者 false. 然而, 如果这个值是 false, 其他不匹配的关键字是不接受的 (除非使用了 &allow-other-keys).

此外, 如果接收的参数列表指定了一个普通的会被 :allow-other-keys 标记的参数, 那么 :allow-other-keys 同时有它的 special-cased 意义(确定是否允许附加的关键字)和它的正常意义(data flow into the function in question).

##### 3.4.1.4.1 抑制参数检测

如果一个函数的lambda列表中指定了 &allow-other-keys, 对这个函数的调用中关键字参数检测会被抑制.

如果在一个函数的调用中 :allow-other-keys 参数是 true, 在这个调用中关键字参数检测是被抑制的.

这个 :allow-other-keys 在所有涉及关键字参数的地方都是允许的, 甚至当它关联的值是 false.<!-- TODO 待校验 -->

###### 3.4.1.4.1.1 抑制关键字参数检测的例子

```LISP
;;; The caller can supply :ALLOW-OTHER-KEYS T to suppress checking.
((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t) =>  1
;;; The callee can use &ALLOW-OTHER-KEYS to suppress checking.
((lambda (&key x &allow-other-keys) x) :x 1 :y 2) =>  1
;;; :ALLOW-OTHER-KEYS NIL is always permitted.
((lambda (&key) t) :allow-other-keys nil) =>  T
;;; As with other keyword arguments, only the left-most pair
;;; named :ALLOW-OTHER-KEYS has any effect.
((lambda (&key x) x) 
:x 1 :y 2 :allow-other-keys t :allow-other-keys nil)
=>  1
;;; Only the left-most pair named :ALLOW-OTHER-KEYS has any effect,
;;; so in safe code this signals a PROGRAM-ERROR (and might enter the
;;; debugger).  In unsafe code, the consequences are undefined.
((lambda (&key x) x)                   ;This call is not valid
:x 1 :y 2 :allow-other-keys nil :allow-other-keys t)
```

#### 3.4.1.5 <span id = "SpecifiersAuxVariables">&aux变量的指定符</span>

这些不是真的参数. 如果这个lambda列表出现 &aux, 所有在它后面的指定符都是辅助变量指定符. 在所有参数指定符被处理后, 辅助变量指定符(那些在 &aux 后面的)从左到右被处理. 对于其中的每一个, init-form 被求值并且 var 被绑定给那个值 (如果没有 init-form 就是 nil). &aux 变量处理类似于 let* 处理.

```LISP
(lambda (x y &aux (a (car x)) (b 2) c) (list x y a b c))
  ==  (lambda (x y) (let* ((a (car x)) (b 2) c) (list x y a b c)))
```

#### 3.4.1.6 <span id = "ExamplesOrdinaryLambdaLists">普通lambda列表的示例</span>

这里是可选参数和剩余参数的例子:

```LISP
((lambda (a b) (+ a (* b 3))) 4 5) =>  19
((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5) =>  19
((lambda (a &optional (b 2)) (+ a (* b 3))) 4) =>  10
((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))
=>  (2 NIL 3 NIL NIL)
((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)
=>  (6 T 3 NIL NIL)
((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)
=>  (6 T 3 T NIL)
((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3 8)
=>  (6 T 3 T (8))
((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x))
6 3 8 9 10 11)
=>  (6 t 3 t (8 9 10 11))
```

这里是关键字参数的例子:

```LISP
((lambda (a b &key c d) (list a b c d)) 1 2) =>  (1 2 NIL NIL)
((lambda (a b &key c d) (list a b c d)) 1 2 :c 6) =>  (1 2 6 NIL)
((lambda (a b &key c d) (list a b c d)) 1 2 :d 8) =>  (1 2 NIL 8)
((lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8) =>  (1 2 6 8)
((lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6) =>  (1 2 6 8)
((lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6) =>  (:a 1 6 8)
((lambda (a b &key c d) (list a b c d)) :a :b :c :d) =>  (:a :b :d NIL)
((lambda (a b &key ((:sea c)) d) (list a b c d)) 1 2 :sea 6) =>  (1 2 6 NIL)
((lambda (a b &key ((c c)) d) (list a b c d)) 1 2 'c 6) =>  (1 2 6 NIL)
```

这里是同时启用可选参数和剩余参数还有关键字参数的示例:

```LISP
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) 1)   
=>  (1 3 NIL 1 ()) 
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) 1 2)
=>  (1 2 NIL 1 ())
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) :c 7)
=>  (:c 7 NIL :c ())
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) 1 6 :c 7)
=>  (1 6 7 1 (:c 7))
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) 1 6 :d 8)
=>  (1 6 NIL 8 (:d 8))
((lambda (a &optional (b 3) &rest x &key c (d a))
  (list a b c d x)) 1 6 :d 8 :c 9 :d 10)
=>  (1 6 9 8 (:d 8 :c 9 :d 10))
```

作为 &allow-other-keys 和 :allow-other-keys 使用的示例, 细想一个函数, 它接受两个命名的参数, 并接受附加的命名参数, 以将其传递给 make-array:

```LISP
(defun array-of-strings (str dims &rest named-pairs
                        &key (start 0) end &allow-other-keys)
  (apply #'make-array dims
        :initial-element (subseq str start end)
        :allow-other-keys t
        named-pairs))
```

这个函数需要一个字符串和一个维度信息并且返回一个指定维度的数组, 它的每一个指定的元素是指定的字符串. 然而, :start 和 :end 命名的参数可能被用于指定应该使用的给定字符串中的子字符串. 另外, 在这个lambda列表中出现的 &allow-other-keys 表示调用者可能提供额外的命名参数; 这个剩余参数提供对它们的访问. 这些额外的命名的参数被传递给 make-array. 这个 make-array 函数正常不允许命名参数 :start 和 :end 被使用, 并且如果这样命名的参数提供给 make-array 会发出一个错误. 然而, 对 make-array 的调用中参数 :allow-other-keys 带有一个 true 值导致任何额外的命名参数, 包括 :start 和 :end, 是可接受的并且忽略掉. 

### 3.4.2 <span id = "GenericFunctionLambdaLists">广义函数lambda列表</span>

一个广义函数lambda列表被用于描述被一个广义函数接受的参数列表的整体形状. 个别方法签名可能为有效方法的lambda列表提供额外的关键字参数.

一个广义函数lambda列表被 defgeneric 所使用.

一个广义函数lambda列表有着以下语法:

    lambda-list::= (var* 
                    [&optional {var | (var)}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]]) 

一个广义函数lambda列表可以包含下面这段中的lambda列表关键字.

    &allow-other-keys  &optional    
    &key               &rest        

Figure 3-14. 广义函数lambda列表使用的lambda列表关键字

一个广义函数lambda列表在以下方面有别于普通lambda列表:

必要参数

    0个或更多必要参数必须被指定.

可选和个关键字参数

    可选参数和关键字参数可能没有默认的初始值和使用 supplied-p 参数.

&aux 的使用

    &aux 的使用是不允许的. 

### 3.4.3 <span id = "SpecializedLambdaLists">特定的lambda列表</span>

一个特定的lambda列表被用于为一个特定的签名特化一个方法并且去描述匹配这个签名的参数如何被方法接收. 下一段中定义的名字以某种方式使用特定的lambda列表; 关于其中的每一个怎样处理的信息见字典条目.

    defmethod  defgeneric    

Figure 3-15. 使用特定的lambda列表的标准化操作符

一个特定的lambda列表可以包含下面这段中展示的lambda列表关键字.

    &allow-other-keys  &key       &rest  
    &aux               &optional         

Figure 3-16. 特定lambda列表使用的lambda列表关键字

一个特定的lambda列表是语法上等价于一个普通的lambda列表除了每一个必要参数可能可选地和一个类或者一个对象关联, 该参数是特定的.

    lambda-list::= ({var | (var [specializer])}* 
                    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                    [&aux {var | (var [init-form])}*]) 

### 3.4.4 <span id = "MacroLambdaLists">宏lambda列表</span>

一个宏lambda列表被用于描述下面这段中的操作符定义的宏A macro lambda list is used in describing macros defined by the operators in the next figure.

    define-compiler-macro  defmacro  macrolet  
    define-setf-expander                       

Figure 3-17. 使用宏lambda列表的操作符

对于一个环境参数可能只出现一次(在描述的任何位置)的附加限制, 一个宏lambda列表有以下语法:

    reqvars::= var* 

    optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 

    restvar::= [{&rest | &body} var] 

    keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
                [&allow-other-keys]] 

    auxvars::= [&aux {var | (var [init-form])}*] 

    envvar::= [&environment var] 

    wholevar::= [&whole var] 

    lambda-list::= (wholevar envvar  reqvars envvar  optvars envvar 
                    restvar envvar  keyvars envvar  auxvars envvar) | 
                  (wholevar envvar  reqvars envvar  optvars envvar .  var) 

    pattern::= (wholevar reqvars optvars restvar keyvars auxvars) | 
              (wholevar reqvars optvars . var) 

一个宏lambda列表可以包含下面这段展示的lambda列表关键字.

    &allow-other-keys  &environment  &rest   
    &aux               &key          &whole  
    &body              &optional             

Figure 3-18. 宏lambda列表使用的lambda列表参数

可选参数 (introduced by &optional) 和关键字参数 (introduced by &key) 可以在一个宏lambda列表中被提供, 就像在普通lambda列表中一样. 每一个都可能包含默认初始化表达式和 supplied-p 参数.

&body 在函数中和 &rest 一样, 但是它可以被用于通知确定的输出格式化和编辑函数这个表达式的剩余部分被当作一个主体(body), 并且应该相应地缩进. 在任何特定的级别 &body 或者 &rest 只有一个可以被使用; 见章节 3.4.4.1 (Destructuring by Lambda Lists). &body 可以出现在一个宏lambda表达式的任何级别; 关于详细情况, 见章节 3.4.4.1 (Destructuring by Lambda Lists).

&whole 跟着一个绑定给整个 macro-call 表达式的单个变量; 这是这个宏函数收到的第一个参数的值. 如果出现 &whole 和一个跟在后面的变量, 它们必须出现在lambda列表的最前面, 在任何其他参数或者lambda列表关键字之前. &whole 可以出现在一个宏lambda列表的任何级别. 在内部级别, 这个 &whole 变量绑定给参数的对应部分, 正如 &rest, 但是不像 &rest, 其他参数也是允许的. 这个 &whole 的使用不影响参数指定的模式.

&environment 后面跟着一个绑定给表示当前词法环境的环境, 这个环境是这个宏调用被解释时所处的环境. 这个环境应该和 macro-function, get-setf-expansion, compiler-macro-function, 还有 macroexpand (for example) 在计算宏展开式一起使用, 来确保这个编译环境中确定的任何词法绑定或定义被考虑进去. &environment 只能出现在宏lambda列表的顶层, 并且只能出现一次, 但是可以出现在这个列表的任何地方; 这个 &environment 和 &whole 被在这个lambda列表的任何其他变量之前被绑定, 不管 &environment 出现在这个lambda列表的什么地方. 绑定到环境参数的对象具有动态范围.

解构允许一个宏lambda列表去表达宏调用语法结构. 如果没有出现lambda列表关键字, 那么这个宏lambda列表是在叶子中包含参数名称的树. 模式和宏表达式必须具有兼容的树结构; 这就是说, 它们的树结构必须是等价的, 或者它只能在模式的某些叶节点与宏形式的非原子对象匹配时有所不同. 关于这种情况下的错误检测的信息, 见章节 3.5.1.7 (Destructuring Mismatch).

一个解构的lambda列表(不管在顶层还是嵌入的)可以被点标记, 以一个参数名结束. 这种情况的处理方式与结束列表的参数名称在 &rest 前面出现的情况完全相同.

对于一个宏表达式(或者是一个宏表达式的子表达式)是一个标有点的列表是允许的, 只有在和 (... &rest var) 或 (... . var) 匹配时. 宏需要去识别和处理这种情况.

#### 3.4.4.1 lambda列表的解构
<!-- TODO 整块待校验 -->
在一个宏lambda列表中任何参数名字可以出现的地方, 还有普通lambda列表语法中(在章节 3.4.1 (Ordinary Lambda Lists) 描述的)不允许一个列表的地方, 一个解构的lambda列表可以出现在参数名字的地方. 当这个完成后, 与参数匹配的参数被当作一个(可能是点标记的)列表, 作为一个参数列表, 用于满足内嵌的lambda列表中的参数. 这就被认为是解构.

解构是将一个复合对象分解为它的组件部分的过程, 使用一种缩写的声明式语法, 而不是用原始的组件访问函数. 每一个组件部分绑定给一个变量.

一个解构操作需要一个将要解构的对象, 一个指定要提取哪些组件的模式, 以及那些值为组件的变量的名称.

##### 3.4.4.1.1 lambda列表的数据导向解构

在数据导向的解构中, 模式是一个要被分解的类型的对象. 无论在哪里提取组件, 在模式中对应地方都会出现一个符号; 这个符号是变量的名称它的值是那个组件.

###### 3.4.4.1.1.1 lambda列表的数据导向解构示例

一个示例模式是

```LISP
(a b c)
```

它解构了一个三个元素的列表. 这个变量 a 被赋值第一个元素, b 给赋值第二个, 等等. 一个更加复杂的例子是

```LISP
((first . rest) . more)
```

简单的语法和扩展到lambda列表导向的能力是数据导向解构的重要特性. 

##### 3.4.4.1.2 lambda列表的lambda列表导向结构

树的数据导向结构的一个延伸是lambda列表导向的解构. 这是从三元素的解构模式的类比中得出的

```LISP
(first second third)
```

并且这个三个参数的lambda列表

```LISP
(first second third)
```

如果没有lambda列表关键字出现在模式中那么lambda列表导向的解构和数据导向的结构是相同的. 任何在这个模式中的列表(不管是一个子列表或是整个模式本身)包含lambda列表关键字就会被特别地解释. 这个列表中第一个lambda列表关键字左边的元素被当作解构模式处理, 像平常一样, 但是列表中剩下的元素被当作函数lambda列表一样处理, 除了在通常需要一个变量的情况下, 可以使用任意的解构模式. 注意, 在不确定的情况下，lambda列表语法优于解构语法. 因此, 在 &optional 之后，一个元素列表是一个解构模式和一个默认值表达式的列表.

每个lambda列表关键字在lambda列表导向的解构模式中的具体行为如下:

&optional

    每一个后面的元素是一个变量或者一个解构模式, 一个默认值的表达式和一个 supplied-p 变量的列表. 这个默认值和 supplied-p 可以被省略. 如果这个被解构列表提前结束, , 所以它没有一个元素来匹配这个解构模式或子模式, 而是这个默认表达式会求值并解构. 如果这个默认表达式被使用了 supplied-p 变量会收到值 nil, 否则就是 t.

&rest, &body

    下一个元素是一个匹配这个列表剩余部分的解构模式. &body 和 &rest 一样但是声明所匹配的是构成表达式主体的表达式列表. 这下一个元素必须是最后一个除非后面跟着一个lambda列表关键字.

&aux

    其余的元素根本不是解构模式, 而是辅助变量绑定.

&whole

    下一个元素是一个匹配一个宏里的整个表达式的解构模式, 或者内部层级的整个子表达式.

&key

    后面跟着的元素是以下其中之一

    一个变量,

    或者一个变量, 一个可选的初始化表达式, 和一个可选的 supplied-p 变量的列表.

    或者一个关键字列表和一个解构模式, 一个可选初始化表达式, 和一个可选 supplied-p 变量的列表.

    被解构的列表的其余部分被认为是交替的关键字和值并且被适当地分开了.

&allow-other-keys

    Stands by itself. 

### 3.4.5 <span id = "DestructuringLambdaLists">解构lambda列表</span>

一个解构lambda列表可以被 destructuring-bind 使用.

解构lambda列表与宏lambda列表密切相关; 见章节 3.4.4 (Macro Lambda Lists). 除了 &environment 之外, 一个解构lambda列表可以包含所有其他用于宏lambda列表的lambda列表关键字, 并且支持相同方式下的解构. 在宏lambda列表中嵌套的内部lambda列表具有解构lambda列表的语法.

一个解构lambda列表具有以下语法:

    reqvars::= var* 

    optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*] 

    restvar::= [{&rest | &body} var] 

    keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* 
                [&allow-other-keys]] 

    auxvars::= [&aux {var | (var [init-form])}*] 

    envvar::= [&environment var] 

    wholevar::= [&whole var] 

    lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars) | 
                  (wholevar reqvars optvars . var) 

### 3.4.6 <span id = "BoaLambdaLists">Boa Lambda列表</span>

一个 boa lambda 列表是一个语法上像普通lambda列表的lambda列表, 但是这个是按照参数的顺序风格处理.

一个 boa lambda 列表只被 defstruct 表达式使用, 当明确指定构造器函数的lambda列表时 (有时称之为 `"boa constructor").

这个 &optional, &rest, &aux, &key, 还有 &allow-other-keys lambda列表关键字在boa lambda表达式中是被识别的. 这些lambda列表关键字有别于在普通lambda列表中的使用方式.

细想这个示例, 它描述了解构如何处理它的 :constructor.

```LISP
(:constructor create-foo
        (a &optional b (c 'sea) &rest d &aux e (f 'eff)))
```

这个定义了 create-foo 去做为一个或更多参数的构造器. 第一个参数被用于初始化 a 槽. 第二个参数备用于初始化 b 槽. 如果这里没有第二个参数, 那么 defstruct 主体中给定的默认值(如果给了的话)被使用. 第三个参数被用于初始化 c 槽. 如果这里没有任何第三个参数, 那么符号 sea 就会被使用. 任何跟在第三个参数后面的参数被收集到一个列表中然后被用于初始化 d 槽. 如果这里有三个或更少的参数, 那么 d 槽的内容就是 nil. e 槽没有被初始化; 它的初始化值是实现定义的. 最后, f 槽被初始化去包含符号 eff. &key 和 &allow-other-keys 参数默认类似于 &optional 参数: 如果在这个lambda列表中没有提供默认值, 那么使用 defstruct 主体中给定的默认值(如果给了的话). 举例说:

```LISP
(defstruct (foo (:constructor CREATE-FOO (a &optional b (c 'sea)
                                            &key (d 2)
                                            &aux e (f 'eff))))
  (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))

(create-foo 10) =>  #S(FOO A 10 B 2 C SEA D 2 E implemention-dependent F EFF)
(create-foo 10 'bee 'see :d 'dee) 
=>  #S(FOO A 10 B BEE C SEE D DEE E implemention-dependent F EFF)
```

如果这个表达式的关键字参数 ((key var) [default [svar]]) 被指定了, 这个槽的名字和 var 匹配(不是 key).

在 b 和 e 情况下采取的行动被仔细选择, 以允许用户指定所有可能的行为. 这个 &aux 变量被用于完全重写主体中给定的默认的初值.

如果没有给一个 aux 变量提供默认值, 在槽被明确赋值前尝试去读取对应槽的值那么结果是为定义的. 如果这样一个槽指定了一个 :type 选项, 这种被抑制的初始化并不意味着类型不匹配的情况; 声明的类型只有在槽最终赋值时才需要应用.

在这个定义下, 下面的可以被写成:

```LISP
(create-foo 1 2)
```

而不是

```LISP
(make-foo :a 1 :b 2)
```

并且 create-foo 提供了和 make-foo 不同的 defaulting. <!--TODO defaulting ?? -->

附加的参数不对应于槽名, 但只提供在随后的初始化计算中使用的值. 比如, 在这个定义中

```LISP
(defstruct (frob (:constructor create-frob
                (a &key (b 3 have-b) (c-token 'c) 
                        (c (list c-token (if have-b 7 2))))))
        a b c)
```

这个 c-token 参数只是给 c 槽的初始化提供一个值. 这个与可选参数和关键字参数相关的 supplied-p 参数也可以使用这种方式. 

### 3.4.7 <span id = "DefsetfLambdaLists">Defsetf Lambda列表</span>

一个 defsetf lambda列表被 defsetf 所使用.

一个 defsetf lambda列表遵循以下语法:

```LISP
lambda-list::= (var* 
                [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                [&rest var] 
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                [&environment var] 
```

一个 defsetf lambda列表可以包含下面这段展示的lambda列表关键字.

    &allow-other-keys  &key       &rest  
    &environment       &optional         

Figure 3-19. Defsetf Lambda列表使用的lambda列表关键字

一个 defsetf lambda列表和普通lambda列表的区别仅在于它不允许使用 &aux, 并且它允许使用表示环境参数的 &environment. 

### 3.4.8 <span id = "DeftypeLambdaLists">Deftype Lambda列表</span>

一个 deftype lambda列表被 deftype 所使用.

一个 deftype lambda列表有着像宏lambda列表相同的语法, 并且可以包含和宏lambda列表相同的lambda列表关键字.

一个 deftype lambda列表和宏lambda列表的区别仅在于如果没有给一个可选参数或关键字参数提供 init-from, 那么这个参数的默认值就是符号 * (而不是 nil). 

### 3.4.9 <span id = "DefineMMLambdaLists">Define-modify-macro Lambda列表</span>

一个 define-modify-macro lambda列表被 define-modify-macro 使用.

一个 define-modify-macro lambda列表可以包含下面这段展示的lambda列表关键字.

    &optional  &rest  

Figure 3-20. Define-modify-macro Lambda列表使用的lambda列表关键字

Define-modify-macro lambda列表类似于普通lambda列表, 但是不支持关键字参数. define-modify-macro 不需要去匹配关键字, 并且一个剩余参数就足够了. Aux 变量也不支持, 因为 define-modify-macro 没有主体表达式来引用这些绑定. 见宏 define-modify-macro. 

### 3.4.10 <span id = "DefineMCArgumentsLambdaLists">Define-method-combination 参数Lambda列表</span>

一个 define-method-combination 参数lambda列表被 define-method-combination 的 :arguments 选项所使用.

一个 define-method-combination 参数lambda列表可以包含下面这段中展示的lambda列表关键字.

    &allow-other-keys  &key       &rest   
    &aux               &optional  &whole  

Figure 3-21. Define-method-combination 参数Lambda列表使用的lambda列表关键字

Define-method-combination 参数lambda列表类似于普通lambda列表, 但是也允许使用 &whole. 

### 3.4.11 <span id = "SIDSD">文档字符串和声明的语法交互</span>

在许多情况下, 文档字符串可以在一系列表达式形式之前出现在一系列声明表达式中.

在这个情况下, 如果字符串 S 出现在允许文档字符串被允许的地方而后面没有声明表达式或表达式形式那么 S 就被认为是一种表达式; 否则, S 被当作一个文档字符串. 如果出现不止一个文档字符串那么结果是未定义的. 

## 3.5 <span id = "ErrorChecking">函数调用中的错误检测</span>

### 3.5.1 参数匹配检测

> * 3.5.1.1 [安全和非安全调用](#SafeUnsafeCalls)
> * 3.5.1.2 [参数太少(Too Few Arguments)](#TooFewArguments)
> * 3.5.1.3 [参数太多(Too Many Arguments)](#TooManyArguments)
> * 3.5.1.4 [不识别的关键字参数(Unrecognized Keyword Arguments)](#UnrecognizedKeywordArguments)
> * 3.5.1.5 [非法的关键字参数(Invalid Keyword Arguments)](#InvalidKeywordArguments)
> * 3.5.1.6 [奇数数量的关键字参数(Odd Number of Keyword Arguments)](#OddNumberKeywordArguments)
> * 3.5.1.7 [解构不匹配(Destructuring Mismatch)](#DestructuringMismatch)
> * 3.5.1.8 [调用下一个方法时的错误(Errors When Calling a Next Method)](#ErrorsWhenCallingNextMethod)

#### 3.5.1.1 <span id = "SafeUnsafeCalls">安全和非安全调用</span>

如果下面中的每一个都是安全代码或者系统代码那么这个调用就是安全的调用 (除了由程序员代码的宏展开所导致的系统代码之外):

* 这个调用.
* 被调用函数的定义.
* 函数求值的点

以下特殊情况需要一些细化:

* 如果被调用的函数是一个广义函数, 如果下面列出的所有部分都是安全的代码或者系统代码那么它就被认为是安全的:

    -- 它的定义 (如果它被明确定义).</br>
    -- 所有适用方法的方法定义.</br>
    -- 它的方法组合的定义.

* 对于表达式 (coerce x 'function), 其中 x 是一个 lambda 表达式, 当强制执行时, 全局环境中优化质量安全的值也适用于产生的函数.

* 对于一个函数 ensure-generic-function 的调用, 在作为 :environment 参数传递的环境对象中优化质量安全的值也适用于产生的广义函数.

* 对于一个对lambda表达式作为参数的 compile 的调用, 在 compile 被调用时全局环境中优化质量安全的值适用于编译出来的函数.

* 对于一个单个参数的 compile 调用, 如果函数的原始定义是安全的, 那么作为结果编译后的函数也必须是安全的.

* 一个被 call-next-method 调用的方法如果下面的每一个都被认为是安全代码或者系统代码那么这个方法就被认为是安全的:

    -- 这个广义函数的定义 (如果它被明确定义).
    -- 所有适用方法的方法定义.
    -- 方法组合的定义.
    -- 方法定义表达式主体部分的入口点, 即确定 call-next-method 绑定的地方.
    -- 名字 call-next-method 功能求值的点.

一个不安全调用就是一个不是安全调用的调用.

非正式的意图是, 即使在涉及到系统代码的情况下, 程序员也可以依靠调用来保证安全, 如果已经采取了所有合理的步骤来确保调用是安全的The informal intent is that the programmer can rely on a call to be safe, even when system code is involved, if all reasonable steps have been taken to ensure that the call is safe. 比如, 如果一个程序员从安全的代码中调用 mapcar 并且提供了一个被编译为安全的函数, 那么这个具体实现也需要去确保这个 mapcar 是一个安全的调用. <!-- TODO 待校对 -->

3.5.1.1.1 安全调用的错误检测时间

如果在安全调用中发出一个错误, 这个准确的发出点是依赖于实现的. 尤其, 它可能在编译时或运行时发出, 如果在运行时发出, 它可能在执行这个调用时, 或之前, 或之后发出. 然而它总是在这个被调用函数的主体执行之前. 

#### 3.5.1.2 <span id = "TooFewArguments">参数太少(Too Few Arguments)</span>

对一个函数提供的参数太少是不允许的. 太少的参数意味着参数少于这个函数需要到参数数量.

如果这个情况发生在一个安全调用中, 一定会发出一个 program-error 类型的错误; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.3 <span id = "TooManyArguments">参数太多(Too Many Arguments)</span>

对一个函数提供的参数太多是不允许的. 太多的参数意味着更多的参数, 而不仅仅是所需参数的数量加上可选参数的数量; 然而, 如果函数使用 &rest 或者 &key, 它不可能接受太多参数.

如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.4 <span id = "UnrecognizedKeywordArguments">不识别的关键字参数(Unrecognized Keyword Arguments)</span>

向一个函数提供一个不被识别的关键字参数是不允许的, 除非就像章节 3.4.1.4.1 (Suppressing Keyword Argument Checking) 描述的那样关键字参数检测被抑制.

如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.5 <span id = "InvalidKeywordArguments">非法的关键字参数(Invalid Keyword Arguments)</span>

通过使用一个不是符号的名字给函数传递关键字参数是不允许的.

如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误, 除非就像章节 3.4.1.4.1 (Suppressing Keyword Argument Checking) 描述的那样关键字参数检测被抑制; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.6 <span id = "OddNumberKeywordArguments">奇数数量的关键字参数(Odd Number of Keyword Arguments)</span>

奇数数量的关键字一定不能提供给关键字参数.

如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误, 除非就像章节 3.4.1.4.1 (Suppressing Keyword Argument Checking) 描述的那样关键字参数检测被抑制; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.7 <span id = "DestructuringMismatch">解构不匹配(Destructuring Mismatch)</span>

当一个结构lambda列表和一个表达式匹配时, 这个解构模式和表达式必须有像章节 3.4.4 (Macro Lambda Lists) 描述的兼容的树结构.

否则, 如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.8 <span id = "ErrorsWhenCallingNextMethod">调用下一个方法时的错误(Errors When Calling a Next Method)</span>

如果 call-next-method 调用时带了参数, 用于 call-next-method 的变更后的参数集合的可适用方法集必须与这个广义函数的原始参数的可适用方法集相同, 否则应该会发出一个错误.

对新参数的一组方法和适用于原始参数的方法集合之间的比较, 其中相同指示符的方法次序是不敏感的. <!-- TODO 待校验 -->

如果 call-next-method 的参数指定了不同的可适用方法的不同排序集, 并且没有可用的下一个方法, 那么对不同方法的测试和相关错误信号的发出(存在的话)的将优先于调用 no-next-method. . 

## 3.6 <span id = "TraversalRulesSideEffects">遍历规则和副作用</span>

当在一个对象遍历操作中执行的代码以一种可能影响正在进行的遍历操作的方式修改对象时, 其后果是未定义的. 尤其, 适用于以下规则.

列表遍历(List traversal)

    对于列表遍历操作, 列表中的 cdr 链是不允许被破坏性修改的.

数组遍历(Array traversal)

    对于数组遍历操作, 不允许对数组进行调整, 并且填充指针不允许被修改.

哈希表遍历(Hash-table traversal)

    对于哈希表遍历操作, 新元素可能不会被添加或删除, 除非与当前散列键对应的元素可以被更改或删除.

包遍历(Package traversal)

    对于包遍历操作 (比如, do-symbols), 新的符号不能从被遍历的包或者它使用的任何包中被 intern 或者 uninterned, 除非当前的符号可以从被遍历的包中被 unintern. <!-- TODO 待校验 intern --> 

## 3.7 <span id = "DestructiveOperations">破坏性操作</span>

### 3.7.1 字面化对象的修改

如果字面化对象被破坏性地修改那么结果是不可预料的. 出于这个目的, 以下操作被认为是破坏性的:

random-state

    使用它作为函数 random 的一个参数.

cons

    修改 cons 的 car 或者 cdr 部分, 或者对一个 cons 的 car 或者 cdr 部分对象执行破坏性操作.

array

    将一个新值存储到数组的某个元素中, 或者对已经是该元素的对象执行破坏性操作.

    改变数组的填充指针, 维度或位移 (不管这个数组实际上是否为 adjustable).

    对一个数组执行破坏性操作, 这个数组的内容被转移到另一个数组或者和另一个数组共享内容.

hash-table

    对任何 key 做破坏性操作.

    为任何 key 存储一个新的 value, 或者对这样的 value 对象执行破坏性操作.

    从这个hash表中添加或删除元素.

structure-object

    存储一个新的值到任何槽中, 或者对一些槽的值对象执行破坏性的操作.

standard-object

    存储一个新的值到任何槽中, 或者对一些槽的值对象执行破坏性的操作.

    改变这个对象的类 (比如, 使用函数 change-class).

readtable

    更改 readtable 用例.

    修改这个 readtable 中的任何字符的语法类型.

    修改与 readtable 中任何字符相关的读取器宏函数, 或修改与在 readtable 中指定的字符相关的字符读取器宏函数.

stream

    对 stream 执行 I/O 操作, 或者关闭这个 stream.

All other standardized types

    [这个范畴包括, 比如, character, condition, function, method-combination, method, number, package, pathname, restart, 还有 symbol.]

    在这些类型的对象上没有标准化的破坏性操作. 

### 3.7.2 破坏性操作期间的控制转移

如果将控制从破坏性的操作中转移出来(比如, 由于一个错误)就会发生被修改的对象的状态是实现依赖的.

#### 3.7.2.1 破坏性操作期间的控制转移的示例

下面的示例演示了许多方法中的一部分, 在这些方法下修改的实现依赖的本质得以展现.

```LISP
(let ((a (list 2 1 4 3 7 6 'five)))
  (ignore-errors (sort a #'<))
  a)
=>  (1 2 3 4 6 7 FIVE)
OR=>  (2 1 4 3 7 6 FIVE)
OR=>  (2)

(prog foo ((a (list 1 2 3 4 5 6 7 8 9 10)))
  (sort a #'(lambda (x y) (if (zerop (random 5)) (return-from foo a) (> x y)))))
=>  (1 2 3 4 5 6 7 8 9 10)
OR=>  (3 4 5 6 2 7 8 9 10 1)
OR=>  (1 2 4 3)
```

## 3.8 <span id = "EvaluationCompilationDictionary">求值和编译的字典条目</span>

 * [Symbol LAMBDA](#SymbolLAMBDA)
 * [Macro LAMBDA](#MacroLAMBDA)
 * [Function COMPILE](#FunctionCOMPILE)
 * [Function EVAL](#FunctionEVAL)
 * [Special Operator EVAL-WHEN](#SpecialOperatorEVALWHEN)
 * [Special Operator LOAD-TIME-VALUE](#SpecialOperatorLOADTIMEVALUE)
 * [Special Operator QUOTE](#SpecialOperatorQUOTE)
 * [Accessor COMPILER-MACRO-FUNCTION](#AccessorCOMPILERMACROFUNCTION)
 * [Macro DEFINE-COMPILER-MACRO](#MacroDEFINECOMPILERMACRO)
 * [Macro DEFMACRO](#MacroDEFMACRO)
 * [Accessor MACRO-FUNCTION](#AccessorMACROFUNCTION)
 * [Function MACROEXPAND, MACROEXPAND-1](#FunctionMACROEXPANDMACROEXPAND1)
 * [Macro DEFINE-SYMBOL-MACRO](#MacroDEFINESYMBOLMACRO)
 * [Special Operator SYMBOL-MACROLET](#SpecialOperatorSYMBOLMACROLET)
 * [Variable *MACROEXPAND-HOOK*](#VariableMACROEXPANDHOOK)
 * [Function PROCLAIM](#FunctionPROCLAIM)
 * [Macro DECLAIM](#MacroDECLAIM)
 * [Symbol DECLARE](#SymbolDECLARE)
 * [Declaration IGNORE, IGNORABLE](#DeclarationIGNOREIGNORABLE)
 * [Declaration DYNAMIC-EXTENT](#DeclarationDYNAMICEXTENT)
 * [Declaration TYPE](#DeclarationTYPE)
 * [Declaration INLINE, NOTINLINE](#DeclarationINLINENOTINLINE)
 * [Declaration FTYPE](#DeclarationFTYPE)
 * [Declaration DECLARATION](#DeclarationDECLARATION)
 * [Declaration OPTIMIZE](#DeclarationOPTIMIZE)
 * [Declaration SPECIAL](#DeclarationSPECIAL)
 * [Special Operator LOCALLY](#SpecialOperatorLOCALLY)
 * [Special Operator THE](#SpecialOperatorTHE)
 * [Function SPECIAL-OPERATOR-P](#FunctionSPECIALOPERATORP)
 * [Function CONSTANTP](#FunctionCONSTANTP)


Symbol LAMBDA

Syntax:

lambda lambda-list [[declaration* | documentation]] form*

Arguments:

lambda-list---an ordinary lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

Description:

A lambda expression is a list that can be used in place of a function name in certain contexts to denote a function by directly describing its behavior rather than indirectly by referring to the name of an established function.

Documentation is attached to the denoted function (if any is actually created) as a documentation string.

See Also:

function, documentation, Section 3.1.3 (Lambda Expressions), Section 3.1.2.1.2.4 (Lambda Forms), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

Notes:

The lambda form

 ((lambda lambda-list . body) . arguments)

is semantically equivalent to the function form

 (funcall #'(lambda lambda-list . body) . arguments)

Macro LAMBDA

Syntax:

lambda lambda-list [[declaration* | documentation]] form* => function

Arguments and Values:

lambda-list---an ordinary lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

function---a function.

Description:

Provides a shorthand notation for a function special form involving a lambda expression such that:

    (lambda lambda-list [[declaration* | documentation]] form*)
 ==  (function (lambda lambda-list [[declaration* | documentation]] form*))
 ==  #'(lambda lambda-list [[declaration* | documentation]] form*)

Examples:

 (funcall (lambda (x) (+ x 3)) 4) =>  7

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

See Also:

lambda (symbol)

Notes:

This macro could be implemented by:

(defmacro lambda (&whole form &rest bvl-decls-and-body)
  (declare (ignore bvl-decls-and-body))
  `#',form)

Function COMPILE

Syntax:

compile name &optional definition => function, warnings-p, failure-p

Arguments and Values:

name---a function name, or nil.

definition---a lambda expression or a function. The default is the function definition of name if it names a function, or the macro function of name if it names a macro. The consequences are undefined if no definition is supplied when the name is nil.

function---the function-name, or a compiled function.

warnings-p---a generalized boolean.

failure-p---a generalized boolean.

Description:

Compiles an interpreted function.

compile produces a compiled function from definition. If the definition is a lambda expression, it is coerced to a function. If the definition is already a compiled function, compile either produces that function itself (i.e., is an identity operation) or an equivalent function.

If the name is nil, the resulting compiled function is returned directly as the primary value. If a non-nil name is given, then the resulting compiled function replaces the existing function definition of name and the name is returned as the primary value; if name is a symbol that names a macro, its macro function is updated and the name is returned as the primary value.

Literal objects appearing in code processed by the compile function are neither copied nor coalesced. The code resulting from the execution of compile references objects that are eql to the corresponding objects in the source code.

compile is permitted, but not required, to establish a handler for conditions of type error. For example, the handler might issue a warning and restart compilation from some implementation-dependent point in order to let the compilation proceed without manual intervention.

The secondary value, warnings-p, is false if no conditions of type error or warning were detected by the compiler, and true otherwise.

The tertiary value, failure-p, is false if no conditions of type error or warning (other than style-warning) were detected by the compiler, and true otherwise.

Examples:

 (defun foo () "bar") =>  FOO
 (compiled-function-p #'foo) =>  implementation-dependent
 (compile 'foo) =>  FOO 
 (compiled-function-p #'foo) =>  true
 (setf (symbol-function 'foo)
       (compile nil '(lambda () "replaced"))) =>  #<Compiled-Function>
 (foo) =>  "replaced"

Affected By:

*error-output*, *macroexpand-hook*.

The presence of macro definitions and proclamations.

Exceptional Situations:

The consequences are undefined if the lexical environment surrounding the function to be compiled contains any bindings other than those for macros, symbol macros, or declarations.

For information about errors detected during the compilation process, see Section 3.2.5 (Exceptional Situations in the Compiler).

See Also:

compile-file

Notes: None. 

Function EVAL

Syntax:

eval form => result*

Arguments and Values:

form---a form.

results---the values yielded by the evaluation of form.

Description:

Evaluates form in the current dynamic environment and the null lexical environment.

eval is a user interface to the evaluator.

The evaluator expands macro calls as if through the use of macroexpand-1.

Constants appearing in code processed by eval are not copied nor coalesced. The code resulting from the execution of eval references objects that are eql to the corresponding objects in the source code.

Examples:

 (setq form '(1+ a) a 999) =>  999
 (eval form) =>  1000
 (eval 'form) =>  (1+ A)
 (let ((a '(this would break if eval used local value))) (eval form))
=>  1000

Affected By: None.

Exceptional Situations: None.

See Also:

macroexpand-1, Section 3.1.2 (The Evaluation Model)

Notes:

To obtain the current dynamic value of a symbol, use of symbol-value is equivalent (and usually preferable) to use of eval.

Note that an eval form involves two levels of evaluation for its argument. First, form is evaluated by the normal argument evaluation mechanism as would occur with any call. The object that results from this normal argument evaluation becomes the value of the form parameter, and is then evaluated as part of the eval form. For example:

 (eval (list 'cdr (car '((quote (a . b)) c)))) =>  b

The argument form (list 'cdr (car '((quote (a . b)) c))) is evaluated in the usual way to produce the argument (cdr (quote (a . b))); eval then evaluates its argument, (cdr (quote (a . b))), to produce b. Since a single evaluation already occurs for any argument form in any function form, eval is sometimes said to perform ``an extra level of evaluation.'' 

Special Operator EVAL-WHEN

Syntax:

eval-when (situation*) form* => result*

Arguments and Values:

situation---One of the symbols :compile-toplevel, :load-toplevel, :execute, compile, load, or eval.

The use of eval, compile, and load is deprecated.

forms---an implicit progn.

results---the values of the forms if they are executed, or nil if they are not.

Description:

The body of an eval-when form is processed as an implicit progn, but only in the situations listed.

The use of the situations :compile-toplevel (or compile) and :load-toplevel (or load) controls whether and when evaluation occurs when eval-when appears as a top level form in code processed by compile-file. See Section 3.2.3 (File Compilation).

The use of the situation :execute (or eval) controls whether evaluation occurs for other eval-when forms; that is, those that are not top level forms, or those in code processed by eval or compile. If the :execute situation is specified in such a form, then the body forms are processed as an implicit progn; otherwise, the eval-when form returns nil.

eval-when normally appears as a top level form, but it is meaningful for it to appear as a non-top-level form. However, the compile-time side effects described in Section 3.2 (Compilation) only take place when eval-when appears as a top level form.

Examples:

One example of the use of eval-when is that for the compiler to be able to read a file properly when it uses user-defined reader macros, it is necessary to write

 (eval-when (:compile-toplevel :load-toplevel :execute)
   (set-macro-character #\$ #'(lambda (stream char)
                                (declare (ignore char))
                                (list 'dollar (read stream))))) =>  T

This causes the call to set-macro-character to be executed in the compiler's execution environment, thereby modifying its reader syntax table.

;;;     The EVAL-WHEN in this case is not at toplevel, so only the :EXECUTE
;;;     keyword is considered. At compile time, this has no effect.
;;;     At load time (if the LET is at toplevel), or at execution time
;;;     (if the LET is embedded in some other form which does not execute
;;;     until later) this sets (SYMBOL-FUNCTION 'FOO1) to a function which
;;;     returns 1.
 (let ((x 1))
   (eval-when (:execute :load-toplevel :compile-toplevel)
     (setf (symbol-function 'foo1) #'(lambda () x))))

;;;     If this expression occurs at the toplevel of a file to be compiled,
;;;     it has BOTH a compile time AND a load-time effect of setting
;;;     (SYMBOL-FUNCTION 'FOO2) to a function which returns 2.
 (eval-when (:execute :load-toplevel :compile-toplevel)
   (let ((x 2))
     (eval-when (:execute :load-toplevel :compile-toplevel)
       (setf (symbol-function 'foo2) #'(lambda () x)))))

;;;     If this expression occurs at the toplevel of a file to be compiled,
;;;     it has BOTH a compile time AND a load-time effect of setting the
;;;     function cell of FOO3 to a function which returns 3.
 (eval-when (:execute :load-toplevel :compile-toplevel)
   (setf (symbol-function 'foo3) #'(lambda () 3)))
 
;;; #4: This always does nothing. It simply returns NIL.
 (eval-when (:compile-toplevel)
   (eval-when (:compile-toplevel) 
     (print 'foo4)))

;;;     If this form occurs at toplevel of a file to be compiled, FOO5 is
;;;     printed at compile time. If this form occurs in a non-top-level
;;;     position, nothing is printed at compile time. Regardless of context,
;;;     nothing is ever printed at load time or execution time.
 (eval-when (:compile-toplevel) 
   (eval-when (:execute)
     (print 'foo5)))
 
;;;     If this form occurs at toplevel of a file to be compiled, FOO6 is
;;;     printed at compile time.  If this form occurs in a non-top-level
;;;     position, nothing is printed at compile time. Regardless of context,
;;;     nothing is ever printed at load time or execution time.
 (eval-when (:execute :load-toplevel)
   (eval-when (:compile-toplevel)
     (print 'foo6)))

Affected By: None.

Exceptional Situations: None.

See Also:

compile-file, Section 3.2 (Compilation)

Notes:

The following effects are logical consequences of the definition of eval-when:

* Execution of a single eval-when expression executes the body code at most once.

* Macros intended for use in top level forms should be written so that side-effects are done by the forms in the macro expansion. The macro-expander itself should not do the side-effects.

    For example:

    Wrong:

     (defmacro foo ()
       (really-foo)
       `(really-foo))

    Right:

     (defmacro foo ()
       `(eval-when (:compile-toplevel :execute :load-toplevel) (really-foo)))

    Adherence to this convention means that such macros behave intuitively when appearing as non-top-level forms.

* Placing a variable binding around an eval-when reliably captures the binding because the compile-time-too mode cannot occur (i.e., introducing a variable binding means that the eval-when is not a top level form). For example,

     (let ((x 3))
       (eval-when (:execute :load-toplevel :compile-toplevel) (print x)))

    prints 3 at execution (i.e., load) time, and does not print anything at compile time. This is important so that expansions of defun and defmacro can be done in terms of eval-when and can correctly capture the lexical environment.

     (defun bar (x) (defun foo () (+ x 3)))

    might expand into

     (defun bar (x) 
       (progn (eval-when (:compile-toplevel) 
                (compiler::notice-function-definition 'foo '(x)))
              (eval-when (:execute :load-toplevel)
                (setf (symbol-function 'foo) #'(lambda () (+ x 3))))))

    which would be treated by the above rules the same as

     (defun bar (x) 
       (setf (symbol-function 'foo) #'(lambda () (+ x 3))))

    when the definition of bar is not a top level form. 

Special Operator LOAD-TIME-VALUE

Syntax:

load-time-value form &optional read-only-p => object

Arguments and Values:

form---a form; evaluated as described below.

read-only-p---a boolean; not evaluated.

object---the primary value resulting from evaluating form.

Description:

load-time-value provides a mechanism for delaying evaluation of form until the expression is in the run-time environment; see Section 3.2 (Compilation).

Read-only-p designates whether the result can be considered a constant object. If t, the result is a read-only quantity that can, if appropriate to the implementation, be copied into read-only space and/or coalesced with similar constant objects from other programs. If nil (the default), the result must be neither copied nor coalesced; it must be considered to be potentially modifiable data.

If a load-time-value expression is processed by compile-file, the compiler performs its normal semantic processing (such as macro expansion and translation into machine code) on form, but arranges for the execution of form to occur at load time in a null lexical environment, with the result of this evaluation then being treated as a literal object at run time. It is guaranteed that the evaluation of form will take place only once when the file is loaded, but the order of evaluation with respect to the evaluation of top level forms in the file is implementation-dependent.

If a load-time-value expression appears within a function compiled with compile, the form is evaluated at compile time in a null lexical environment. The result of this compile-time evaluation is treated as a literal object in the compiled code.

If a load-time-value expression is processed by eval, form is evaluated in a null lexical environment, and one value is returned. Implementations that implicitly compile (or partially compile) expressions processed by eval might evaluate form only once, at the time this compilation is performed.

If the same list (load-time-value form) is evaluated or compiled more than once, it is implementation-dependent whether form is evaluated only once or is evaluated more than once. This can happen both when an expression being evaluated or compiled shares substructure, and when the same form is processed by eval or compile multiple times. Since a load-time-value expression can be referenced in more than one place and can be evaluated multiple times by eval, it is implementation-dependent whether each execution returns a fresh object or returns the same object as some other execution. Users must use caution when destructively modifying the resulting object.

If two lists (load-time-value form) that are the same under equal but are not identical are evaluated or compiled, their values always come from distinct evaluations of form. Their values may not be coalesced unless read-only-p is t.

Examples:

;;; The function INCR1 always returns the same value, even in different images.
;;; The function INCR2 always returns the same value in a given image, 
;;; but the value it returns might vary from image to image.
(defun incr1 (x) (+ x #.(random 17)))
(defun incr2 (x) (+ x (load-time-value (random 17))))

;;; The function FOO1-REF references the nth element of the first of 
;;; the *FOO-ARRAYS* that is available at load time.  It is permissible for
;;; that array to be modified (e.g., by SET-FOO1-REF); FOO1-REF will see the
;;; updated values.
(defvar *foo-arrays* (list (make-array 7) (make-array 8)))
(defun foo1-ref (n) (aref (load-time-value (first *my-arrays*) nil) n))
(defun set-foo1-ref (n val) 
  (setf (aref (load-time-value (first *my-arrays*) nil) n) val))

;;; The function BAR1-REF references the nth element of the first of 
;;; the *BAR-ARRAYS* that is available at load time.  The programmer has
;;; promised that the array will be treated as read-only, so the system 
;;; can copy or coalesce the array.
(defvar *bar-arrays* (list (make-array 7) (make-array 8)))
(defun bar1-ref (n) (aref (load-time-value (first *my-arrays*) t) n))

;;; This use of LOAD-TIME-VALUE permits the indicated vector to be coalesced
;;; even though NIL was specified, because the object was already read-only
;;; when it was written as a literal vector rather than created by a constructor.
;;; User programs must treat the vector v as read-only.
(defun baz-ref (n)
  (let ((v (load-time-value #(A B C) nil)))
    (values (svref v n) v)))

;;; This use of LOAD-TIME-VALUE permits the indicated vector to be coalesced
;;; even though NIL was specified in the outer situation because T was specified
;;; in the inner situation.  User programs must treat the vector v as read-only.
(defun baz-ref (n)
  (let ((v (load-time-value (load-time-value (vector 1 2 3) t) nil)))
    (values (svref v n) v)))

Affected By: None.

Exceptional Situations: None.

See Also:

compile-file, compile, eval, Section 3.2.2.2 (Minimal Compilation), Section 3.2 (Compilation)

Notes:

load-time-value must appear outside of quoted structure in a ``for evaluation'' position. In situations which would appear to call for use of load-time-value within a quoted structure, the backquote reader macro is probably called for; see Section 2.4.6 (Backquote).

Specifying nil for read-only-p is not a way to force an object to become modifiable if it has already been made read-only. It is only a way to say that, for an object that is modifiable, this operation is not intended to make that object read-only. 

Special Operator QUOTE

Syntax:

quote object => object

Arguments and Values:

object---an object; not evaluated.

Description:

The quote special operator just returns object.

The consequences are undefined if literal objects (including quoted objects) are destructively modified.

Examples:

 (setq a 1) =>  1
 (quote (setq a 3)) =>  (SETQ A 3)
 a =>  1
 'a =>  A
 ''a =>  (QUOTE A) 
 '''a =>  (QUOTE (QUOTE A))
 (setq a 43) =>  43
 (list a (cons a 3)) =>  (43 (43 . 3))
 (list (quote a) (quote (cons a 3))) =>  (A (CONS A 3)) 
 1 =>  1
 '1 =>  1
 "foo" =>  "foo"
 '"foo" =>  "foo"
 (car '(a b)) =>  A
 '(car '(a b)) =>  (CAR (QUOTE (A B)))
 #(car '(a b)) =>  #(CAR (QUOTE (A B)))
 '#(car '(a b)) =>  #(CAR (QUOTE (A B)))

Affected By: None.

Exceptional Situations: None.

See Also:

Section 3.1 (Evaluation), Section 2.4.3 (Single-Quote), Section 3.2.1 (Compiler Terminology)

Notes:

The textual notation 'object is equivalent to (quote object); see Section 3.2.1 (Compiler Terminology).

Some objects, called self-evaluating objects, do not require quotation by quote. However, symbols and lists are used to represent parts of programs, and so would not be useable as constant data in a program without quote. Since quote suppresses the evaluation of these objects, they become data rather than program. 

Accessor COMPILER-MACRO-FUNCTION

Syntax:

compiler-macro-function name &optional environment => function

(setf (compiler-macro-function name &optional environment) new-function)

Arguments and Values:

name---a function name.

environment---an environment object.

function, new-function---a compiler macro function, or nil.

Description:

Accesses the compiler macro function named name, if any, in the environment.

A value of nil denotes the absence of a compiler macro function named name.

Examples: None.

Affected By: None.

Exceptional Situations:

The consequences are undefined if environment is non-nil in a use of setf of compiler-macro-function.

See Also:

define-compiler-macro, Section 3.2.2.1 (Compiler Macros)

Notes: None. 

Macro DEFINE-COMPILER-MACRO

Syntax:

define-compiler-macro name lambda-list [[declaration* | documentation]] form*

=> name

Arguments and Values:

name---a function name.

lambda-list---a macro lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

Description:

This is the normal mechanism for defining a compiler macro function. Its manner of definition is the same as for defmacro; the only differences are:

* The name can be a function name naming any function or macro.

* The expander function is installed as a compiler macro function for the name, rather than as a macro function.

* The &whole argument is bound to the form argument that is passed to the compiler macro function. The remaining lambda-list parameters are specified as if this form contained the function name in the car and the actual arguments in the cdr, but if the car of the actual form is the symbol funcall, then the destructuring of the arguments is actually performed using its cddr instead.

* Documentation is attached as a documentation string to name (as kind compiler-macro) and to the compiler macro function.

* Unlike an ordinary macro, a compiler macro can decline to provide an expansion merely by returning a form that is the same as the original (which can be obtained by using &whole).

Examples:

 (defun square (x) (expt x 2)) =>  SQUARE
 (define-compiler-macro square (&whole form arg)
   (if (atom arg)
       `(expt ,arg 2)
       (case (car arg)
         (square (if (= (length arg) 2)
                     `(expt ,(nth 1 arg) 4)
                     form))
         (expt   (if (= (length arg) 3)
                     (if (numberp (nth 2 arg))
                         `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
                         `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
                     form))
         (otherwise `(expt ,arg 2))))) =>  SQUARE
 (square (square 3)) =>  81
 (macroexpand '(square x)) =>  (SQUARE X), false
 (funcall (compiler-macro-function 'square) '(square x) nil)
=>  (EXPT X 2)
 (funcall (compiler-macro-function 'square) '(square (square x)) nil)
=>  (EXPT X 4)
 (funcall (compiler-macro-function 'square) '(funcall #'square x) nil)
=>  (EXPT X 2)

 (defun distance-positional (x1 y1 x2 y2)
   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
=>  DISTANCE-POSITIONAL
 (defun distance (&key (x1 0) (y1 0) (x2 x1) (y2 y1))
   (distance-positional x1 y1 x2 y2))
=>  DISTANCE
 (define-compiler-macro distance (&whole form
                                  &rest key-value-pairs
                                  &key (x1 0  x1-p)
                                       (y1 0  y1-p)
                                       (x2 x1 x2-p)
                                       (y2 y1 y2-p)
                                  &allow-other-keys
                                  &environment env)
   (flet ((key (n) (nth (* n 2) key-value-pairs))
          (arg (n) (nth (1+ (* n 2)) key-value-pairs))
          (simplep (x)
            (let ((expanded-x (macroexpand x env)))
              (or (constantp expanded-x env)
                  (symbolp expanded-x)))))
     (let ((n (/ (length key-value-pairs) 2)))
       (multiple-value-bind (x1s y1s x2s y2s others)
           (loop for (key) on key-value-pairs by #'cddr
                 count (eq key ':x1) into x1s
                 count (eq key ':y1) into y1s
                 count (eq key ':x2) into x2s
                 count (eq key ':y1) into y2s
                 count (not (member key '(:x1 :x2 :y1 :y2)))
                   into others
                 finally (return (values x1s y1s x2s y2s others)))
         (cond ((and (= n 4)
                     (eq (key 0) :x1)
                     (eq (key 1) :y1)
                     (eq (key 2) :x2)
                     (eq (key 3) :y2))
                `(distance-positional ,x1 ,y1 ,x2 ,y2))
               ((and (if x1-p (and (= x1s 1) (simplep x1)) t)
                     (if y1-p (and (= y1s 1) (simplep y1)) t)
                     (if x2-p (and (= x2s 1) (simplep x2)) t)
                     (if y2-p (and (= y2s 1) (simplep y2)) t)
                     (zerop others))
                `(distance-positional ,x1 ,y1 ,x2 ,y2))
               ((and (< x1s 2) (< y1s 2) (< x2s 2) (< y2s 2)
                     (zerop others))
                (let ((temps (loop repeat n collect (gensym))))
                  `(let ,(loop for i below n
                               collect (list (nth i temps) (arg i)))
                     (distance
                       ,@(loop for i below n
                               append (list (key i) (nth i temps)))))))
               (t form))))))
=>  DISTANCE
 (dolist (form
           '((distance :x1 (setq x 7) :x2 (decf x) :y1 (decf x) :y2 (decf x))
             (distance :x1 (setq x 7) :y1 (decf x) :x2 (decf x) :y2 (decf x))
             (distance :x1 (setq x 7) :y1 (incf x))
             (distance :x1 (setq x 7) :y1 (incf x) :x1 (incf x))
             (distance :x1 a1 :y1 b1 :x2 a2 :y2 b2)
             (distance :x1 a1 :x2 a2 :y1 b1 :y2 b2)
             (distance :x1 a1 :y1 b1 :z1 c1 :x2 a2 :y2 b2 :z2 c2)))
   (print (funcall (compiler-macro-function 'distance) form nil)))
>>  (LET ((#:G6558 (SETQ X 7))
>>        (#:G6559 (DECF X))
>>        (#:G6560 (DECF X))
>>        (#:G6561 (DECF X)))
>>    (DISTANCE :X1 #:G6558 :X2 #:G6559 :Y1 #:G6560 :Y2 #:G6561)) 
>>  (DISTANCE-POSITIONAL (SETQ X 7) (DECF X) (DECF X) (DECF X)) 
>>  (LET ((#:G6567 (SETQ X 7))
>>        (#:G6568 (INCF X)))
>>    (DISTANCE :X1 #:G6567 :Y1 #:G6568)) 
>>  (DISTANCE :X1 (SETQ X 7) :Y1 (INCF X) :X1 (INCF X)) 
>>  (DISTANCE-POSITIONAL A1 B1 A2 B2) 
>>  (DISTANCE-POSITIONAL A1 B1 A2 B2) 
>>  (DISTANCE :X1 A1 :Y1 B1 :Z1 C1 :X2 A2 :Y2 B2 :Z2 C2) 
=>  NIL

Affected By: None.

Exceptional Situations: None.

See Also:

compiler-macro-function, defmacro, documentation, Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

Notes:

The consequences of writing a compiler macro definition for a function in the COMMON-LISP package are undefined; it is quite possible that in some implementations such an attempt would override an equivalent or equally important definition. In general, it is recommended that a programmer only write compiler macro definitions for functions he or she personally maintains--writing a compiler macro definition for a function maintained elsewhere is normally considered a violation of traditional rules of modularity and data abstraction. 

Macro DEFMACRO

Syntax:

defmacro name lambda-list [[declaration* | documentation]] form*

=> name

Arguments and Values:

name---a symbol. lambda-list---a macro lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

Description:

Defines name as a macro by associating a macro function with that name in the global environment. The macro function is defined in the same lexical environment in which the defmacro form appears.

The parameter variables in lambda-list are bound to destructured portions of the macro call.

The expansion function accepts two arguments, a form and an environment. The expansion function returns a form. The body of the expansion function is specified by forms. Forms are executed in order. The value of the last form executed is returned as the expansion of the macro. The body forms of the expansion function (but not the lambda-list) are implicitly enclosed in a block whose name is name.

The lambda-list conforms to the requirements described in Section 3.4.4 (Macro Lambda Lists).

Documentation is attached as a documentation string to name (as kind function) and to the macro function.

defmacro can be used to redefine a macro or to replace a function definition with a macro definition.

Recursive expansion of the form returned must terminate, including the expansion of other macros which are subforms of other forms returned.

The consequences are undefined if the result of fully macroexpanding a form contains any circular list structure except in literal objects.

If a defmacro form appears as a top level form, the compiler must store the macro definition at compile time, so that occurrences of the macro later on in the file can be expanded correctly. Users must ensure that the body of the macro can be evaluated at compile time if it is referenced within the file being compiled.

Examples:

 (defmacro mac1 (a b) "Mac1 multiplies and adds" 
            `(+ ,a (* ,b 3))) =>  MAC1 
 (mac1 4 5) =>  19 
 (documentation 'mac1 'function) =>  "Mac1 multiplies and adds" 
 (defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x) `'(,a ,b ,c ,d ,x)) =>  MAC2 
 (mac2 6) =>  (6 T 3 NIL NIL) 
 (mac2 6 3 8) =>  (6 T 3 T (8)) 
 (defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
    `'(,r ,a ,b ,c ,d ,x)) =>  MAC3 
 (mac3 1 6 :d 8 :c 9 :d 10) =>  ((MAC3 1 6 :D 8 :C 9 :D 10) 1 6 9 8 (:D 8 :C 9 :D 10)) 

The stipulation that an embedded destructuring lambda list is permitted only where ordinary lambda list syntax would permit a parameter name but not a list is made to prevent ambiguity. For example, the following is not valid:

 (defmacro loser (x &optional (a b &rest c) &rest z)
   ...)

because ordinary lambda list syntax does permit a list following &optional; the list (a b &rest c) would be interpreted as describing an optional parameter named a whose default value is that of the form b, with a supplied-p parameter named &rest (not valid), and an extraneous symbol c in the list (also not valid). An almost correct way to express this is

 (defmacro loser (x &optional ((a b &rest c)) &rest z)
   ...)

The extra set of parentheses removes the ambiguity. However, the definition is now incorrect because a macro call such as (loser (car pool)) would not provide any argument form for the lambda list (a b &rest c), and so the default value against which to match the lambda list would be nil because no explicit default value was specified. The consequences of this are unspecified since the empty list, nil, does not have forms to satisfy the parameters a and b. The fully correct definition would be either

 (defmacro loser (x &optional ((a b &rest c) '(nil nil)) &rest z)
   ...)

or

 (defmacro loser (x &optional ((&optional a b &rest c)) &rest z)
   ...)

These differ slightly: the first requires that if the macro call specifies a explicitly then it must also specify b explicitly, whereas the second does not have this requirement. For example,

 (loser (car pool) ((+ x 1)))

would be a valid call for the second definition but not for the first.

 (defmacro dm1a (&whole x) `',x)
 (macroexpand '(dm1a))  =>  (QUOTE (DM1A))
 (macroexpand '(dm1a a)) is an error.
 
 (defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b))
 (macroexpand '(dm1b))  is an error.
 (macroexpand '(dm1b q))  =>  (QUOTE ((DM1B Q) Q NIL))
 (macroexpand '(dm1b q r)) =>  (QUOTE ((DM1B Q R) Q R))
 (macroexpand '(dm1b q r s)) is an error.

 (defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b))
 (macroexpand '(dm2a x y)) =>  (QUOTE (FORM (DM2A X Y) A X B Y))
 (dm2a x y) =>  (FORM (DM2A X Y) A X B Y)

 (defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5)) 
                 &body f &environment env)
   ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))
 ;Note that because backquote is involved, implementations may differ
 ;slightly in the nature (though not the functionality) of the expansion.
 (macroexpand '(dm2b x1 (((incf x2) x3 x4)) x5 x6))
 =>  (LIST* '(DM2B X1 (((INCF X2) X3 X4))
                   X5 X6)
            X1
            '((((INCF X2) X3 X4)) (SETQ X2 (+ X2 1)) (X3 X4) 5 (X5 X6))),
     T
 (let ((x1 5))
   (macrolet ((segundo (x) `(cadr ,x)))
     (dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
 =>  ((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
      5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6))

Affected By: None.

Exceptional Situations: None.

See Also:

define-compiler-macro, destructuring-bind, documentation, macroexpand, *macroexpand-hook*, macrolet, macro-function, Section 3.1 (Evaluation), Section 3.2 (Compilation), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

Notes: None. 

Accessor MACRO-FUNCTION

Syntax:

macro-function symbol &optional environment => function

(setf (macro-function symbol &optional environment) new-function)

Arguments and Values:

symbol---a symbol.

environment---an environment object.

function---a macro function or nil.

new-function---a macro function.

Description:

Determines whether symbol has a function definition as a macro in the specified environment.

If so, the macro expansion function, a function of two arguments, is returned. If symbol has no function definition in the lexical environment environment, or its definition is not a macro, macro-function returns nil.

It is possible for both macro-function and special-operator-p to return true of symbol. The macro definition must be available for use by programs that understand only the standard Common Lisp special forms.

Examples:

 (defmacro macfun (x) '(macro-function 'macfun)) =>  MACFUN 
 (not (macro-function 'macfun)) =>  false 

 (macrolet ((foo (&environment env)
               (if (macro-function 'bar env)
                  ''yes
                  ''no)))
    (list (foo)
          (macrolet ((bar () :beep))
             (foo))))
 
=>  (NO YES)

Affected By:

(setf macro-function), defmacro, and macrolet.

Exceptional Situations:

The consequences are undefined if environment is non-nil in a use of setf of macro-function.

See Also:

defmacro, Section 3.1 (Evaluation)

Notes:

setf can be used with macro-function to install a macro as a symbol's global function definition:

 (setf (macro-function symbol) fn)

The value installed must be a function that accepts two arguments, the entire macro call and an environment, and computes the expansion for that call. Performing this operation causes symbol to have only that macro definition as its global function definition; any previous definition, whether as a macro or as a function, is lost. 

Function MACROEXPAND, MACROEXPAND-1

Syntax:

macroexpand form &optional env => expansion, expanded-p

macroexpand-1 form &optional env => expansion, expanded-p

Arguments and Values:

form---a form.

env---an environment object. The default is nil.

expansion---a form.

expanded-p---a generalized boolean.

Description:

macroexpand and macroexpand-1 expand macros.

If form is a macro form, then macroexpand-1 expands the macro form call once.

macroexpand repeatedly expands form until it is no longer a macro form. In effect, macroexpand calls macroexpand-1 repeatedly until the secondary value it returns is nil.

If form is a macro form, then the expansion is a macro expansion and expanded-p is true. Otherwise, the expansion is the given form and expanded-p is false.

Macro expansion is carried out as follows. Once macroexpand-1 has determined that the form is a macro form, it obtains an appropriate expansion function for the macro or symbol macro. The value of *macroexpand-hook* is coerced to a function and then called as a function of three arguments: the expansion function, the form, and the env. The value returned from this call is taken to be the expansion of the form.

In addition to macro definitions in the global environment, any local macro definitions established within env by macrolet or symbol-macrolet are considered. If only form is supplied as an argument, then the environment is effectively null, and only global macro definitions as established by defmacro are considered. Macro definitions are shadowed by local function definitions.

Examples:

 (defmacro alpha (x y) `(beta ,x ,y)) =>  ALPHA
 (defmacro beta (x y) `(gamma ,x ,y)) =>  BETA
 (defmacro delta (x y) `(gamma ,x ,y)) =>  EPSILON
 (defmacro expand (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand form env)
     `(values ',expansion ',expanded-p))) =>  EXPAND
 (defmacro expand-1 (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand-1 form env)
     `(values ',expansion ',expanded-p))) =>  EXPAND-1

;; Simple examples involving just the global environment
 (macroexpand-1 '(alpha a b)) =>  (BETA A B), true
 (expand-1 (alpha a b)) =>  (BETA A B), true
 (macroexpand '(alpha a b)) =>  (GAMMA A B), true
 (expand (alpha a b)) =>  (GAMMA A B), true
 (macroexpand-1 'not-a-macro) =>  NOT-A-MACRO, false
 (expand-1 not-a-macro) =>  NOT-A-MACRO, false
 (macroexpand '(not-a-macro a b)) =>  (NOT-A-MACRO A B), false
 (expand (not-a-macro a b)) =>  (NOT-A-MACRO A B), false

;; Examples involving lexical environments
 (macrolet ((alpha (x y) `(delta ,x ,y)))
   (macroexpand-1 '(alpha a b))) =>  (BETA A B), true
 (macrolet ((alpha (x y) `(delta ,x ,y)))
   (expand-1 (alpha a b))) =>  (DELTA A B), true
 (macrolet ((alpha (x y) `(delta ,x ,y)))
   (macroexpand '(alpha a b))) =>  (GAMMA A B), true
 (macrolet ((alpha (x y) `(delta ,x ,y)))
   (expand (alpha a b))) =>  (GAMMA A B), true
 (macrolet ((beta (x y) `(epsilon ,x ,y)))
   (expand (alpha a b))) =>  (EPSILON A B), true
 (let ((x (list 1 2 3)))
   (symbol-macrolet ((a (first x)))
     (expand a))) =>  (FIRST X), true
 (let ((x (list 1 2 3)))
   (symbol-macrolet ((a (first x)))
     (macroexpand 'a))) =>  A, false
 (symbol-macrolet ((b (alpha x y)))
   (expand-1 b)) =>  (ALPHA X Y), true
 (symbol-macrolet ((b (alpha x y)))
   (expand b)) =>  (GAMMA X Y), true
 (symbol-macrolet ((b (alpha x y))
                   (a b))
   (expand-1 a)) =>  B, true
 (symbol-macrolet ((b (alpha x y))
                   (a b))
   (expand a)) =>  (GAMMA X Y), true

;; Examples of shadowing behavior
 (flet ((beta (x y) (+ x y)))
   (expand (alpha a b))) =>  (BETA A B), true
 (macrolet ((alpha (x y) `(delta ,x ,y)))
   (flet ((alpha (x y) (+ x y)))
     (expand (alpha a b)))) =>  (ALPHA A B), false
 (let ((x (list 1 2 3)))
   (symbol-macrolet ((a (first x)))
     (let ((a x))
       (expand a)))) =>  A, false

Affected By:

defmacro, setf of macro-function, macrolet, symbol-macrolet

Exceptional Situations: None.

See Also:

*macroexpand-hook*, defmacro, setf of macro-function, macrolet, symbol-macrolet, Section 3.1 (Evaluation)

Notes:

Neither macroexpand nor macroexpand-1 makes any explicit attempt to expand macro forms that are either subforms of the form or subforms of the expansion. Such expansion might occur implicitly, however, due to the semantics or implementation of the macro function. 

Macro DEFINE-SYMBOL-MACRO

Syntax:

define-symbol-macro symbol expansion

=> symbol

Arguments and Values:

symbol---a symbol.

expansion---a form.

Description:

Provides a mechanism for globally affecting the macro expansion of the indicated symbol.

Globally establishes an expansion function for the symbol macro named by symbol. The only guaranteed property of an expansion function for a symbol macro is that when it is applied to the form and the environment it returns the correct expansion. (In particular, it is implementation-dependent whether the expansion is conceptually stored in the expansion function, the environment, or both.)

Each global reference to symbol (i.e., not shadowed[2] by a binding for a variable or symbol macro named by the same symbol) is expanded by the normal macro expansion process; see Section 3.1.2.1.1 (Symbols as Forms). The expansion of a symbol macro is subject to further macro expansion in the same lexical environment as the symbol macro reference, exactly analogous to normal macros.

The consequences are unspecified if a special declaration is made for symbol while in the scope of this definition (i.e., when it is not shadowed[2] by a binding for a variable or symbol macro named by the same symbol).

Any use of setq to set the value of the symbol while in the scope of this definition is treated as if it were a setf. psetq of symbol is treated as if it were a psetf, and multiple-value-setq is treated as if it were a setf of values.

A binding for a symbol macro can be shadowed[2] by let or symbol-macrolet.

Examples:

(defvar *things* (list 'alpha 'beta 'gamma)) =>  *THINGS*

(define-symbol-macro thing1 (first *things*)) =>  THING1
(define-symbol-macro thing2 (second *things*)) =>  THING2
(define-symbol-macro thing3 (third *things*)) =>  THING3

thing1 =>  ALPHA
(setq thing1 'ONE) =>  ONE
*things* =>  (ONE BETA GAMMA)
(multiple-value-setq (thing2 thing3) (values 'two 'three)) =>  TWO
thing3 =>  THREE
*things* =>  (ONE TWO THREE)

(list thing2 (let ((thing2 2)) thing2)) =>  (TWO 2)

Affected By: None.

Exceptional Situations:

If symbol is already defined as a global variable, an error of type program-error is signaled.

See Also:

symbol-macrolet, macroexpand

Notes: None. 

Special Operator SYMBOL-MACROLET

Syntax:

symbol-macrolet ((symbol expansion)*) declaration* form*

=> result*

Arguments and Values:

symbol---a symbol.

expansion---a form.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

Description:

symbol-macrolet provides a mechanism for affecting the macro expansion environment for symbols.

symbol-macrolet lexically establishes expansion functions for each of the symbol macros named by symbols. The only guaranteed property of an expansion function for a symbol macro is that when it is applied to the form and the environment it returns the correct expansion. (In particular, it is implementation-dependent whether the expansion is conceptually stored in the expansion function, the environment, or both.)

Each reference to symbol as a variable within the lexical scope of symbol-macrolet is expanded by the normal macro expansion process; see Section 3.1.2.1.1 (Symbols as Forms). The expansion of a symbol macro is subject to further macro expansion in the same lexical environment as the symbol macro invocation, exactly analogous to normal macros.

Exactly the same declarations are allowed as for let with one exception: symbol-macrolet signals an error if a special declaration names one of the symbols being defined by symbol-macrolet.

When the forms of the symbol-macrolet form are expanded, any use of setq to set the value of one of the specified variables is treated as if it were a setf. psetq of a symbol defined as a symbol macro is treated as if it were a psetf, and multiple-value-setq is treated as if it were a setf of values.

The use of symbol-macrolet can be shadowed by let. In other words, symbol-macrolet only substitutes for occurrences of symbol that would be in the scope of a lexical binding of symbol surrounding the forms.

Examples:

;;; The following is equivalent to
;;;   (list 'foo (let ((x 'bar)) x)),
;;; not
;;;   (list 'foo (let (('foo 'bar)) 'foo))
 (symbol-macrolet ((x 'foo))
   (list x (let ((x 'bar)) x))) 
=>  (foo bar)
NOT=>  (foo foo) 
 
 (symbol-macrolet ((x '(foo x)))
   (list x))
=>  ((FOO X))

Affected By: None.

Exceptional Situations:

If an attempt is made to bind a symbol that is defined as a global variable, an error of type program-error is signaled.

If declaration contains a special declaration that names one of the symbols being bound by symbol-macrolet, an error of type program-error is signaled.

See Also:

with-slots, macroexpand

Notes:

The special form symbol-macrolet is the basic mechanism that is used to implement with-slots.

If a symbol-macrolet form is a top level form, the forms are also processed as top level forms. See Section 3.2.3 (File Compilation). 

Variable *MACROEXPAND-HOOK*

Value Type:

a designator for a function of three arguments: a macro function, a macro form, and an environment object.

Initial Value:

a designator for a function that is equivalent to the function funcall, but that might have additional implementation-dependent side-effects.

Description:

Used as the expansion interface hook by macroexpand-1 to control the macro expansion process. When a macro form is to be expanded, this function is called with three arguments: the macro function, the macro form, and the environment in which the macro form is to be expanded. The environment object has dynamic extent; the consequences are undefined if the environment object is referred to outside the dynamic extent of the macro expansion function.

Examples:

 (defun hook (expander form env)
    (format t "Now expanding: ~S~%" form)
    (funcall expander form env)) =>  HOOK 
 (defmacro machook (x y) `(/ (+ ,x ,y) 2)) =>  MACHOOK 
 (macroexpand '(machook 1 2)) =>  (/ (+ 1 2) 2), true 
 (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
>>  Now expanding (MACHOOK 1 2) 
=>  (/ (+ 1 2) 2), true

Affected By: None.

See Also:

macroexpand, macroexpand-1, funcall, Section 3.1 (Evaluation)

Notes:

The net effect of the chosen initial value is to just invoke the macro function, giving it the macro form and environment as its two arguments.

Users or user programs can assign this variable to customize or trace the macro expansion mechanism. Note, however, that this variable is a global resource, potentially shared by multiple programs; as such, if any two programs depend for their correctness on the setting of this variable, those programs may not be able to run in the same Lisp image. For this reason, it is frequently best to confine its uses to debugging situations.

Users who put their own function into *macroexpand-hook* should consider saving the previous value of the hook, and calling that value from their own. 

Function PROCLAIM

Syntax:

proclaim declaration-specifier => implementation-dependent

Arguments and Values:

declaration-specifier---a declaration specifier.

Description:

Establishes the declaration specified by declaration-specifier in the global environment.

Such a declaration, sometimes called a global declaration or a proclamation, is always in force unless locally shadowed.

Names of variables and functions within declaration-specifier refer to dynamic variables and global function definitions, respectively.

The next figure shows a list of declaration identifiers that can be used with proclaim.

declaration  inline     optimize  type  
ftype        notinline  special         

Figure 3-22. Global Declaration Specifiers

An implementation is free to support other (implementation-defined) declaration identifiers as well.

Examples:

 (defun declare-variable-types-globally (type vars)
   (proclaim `(type ,type ,@vars))
   type)

 ;; Once this form is executed, the dynamic variable *TOLERANCE*
 ;; must always contain a float.
 (declare-variable-types-globally 'float '(*tolerance*))
=>  FLOAT

Affected By: None.

Exceptional Situations: None.

See Also:

declaim, declare, Section 3.2 (Compilation)

Notes:

Although the execution of a proclaim form has effects that might affect compilation, the compiler does not make any attempt to recognize and specially process proclaim forms. A proclamation such as the following, even if a top level form, does not have any effect until it is executed:

(proclaim '(special *x*))

If compile time side effects are desired, eval-when may be useful. For example:

 (eval-when (:execute :compile-toplevel :load-toplevel)
   (proclaim '(special *x*)))

In most such cases, however, it is preferrable to use declaim for this purpose.

Since proclaim forms are ordinary function forms, macro forms can expand into them. 

Macro DECLAIM

Syntax:

declaim declaration-specifier* => implementation-dependent

Arguments and Values:

declaration-specifier---a declaration specifier; not evaluated.

Description:

Establishes the declarations specified by the declaration-specifiers.

If a use of this macro appears as a top level form in a file being processed by the file compiler, the proclamations are also made at compile-time. As with other defining macros, it is unspecified whether or not the compile-time side-effects of a declaim persist after the file has been compiled.

Examples:

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

See Also:

declare, proclaim

Notes: None. 

Symbol DECLARE

Syntax:

declare declaration-specifier*

Arguments:

declaration-specifier---a declaration specifier; not evaluated.

Description:

A declare expression, sometimes called a declaration, can occur only at the beginning of the bodies of certain forms; that is, it may be preceded only by other declare expressions, or by a documentation string if the context permits.

A declare expression can occur in a lambda expression or in any of the forms listed in the next figure.

defgeneric                 do-external-symbols   prog                      
define-compiler-macro      do-symbols            prog*                     
define-method-combination  dolist                restart-case              
define-setf-expander       dotimes               symbol-macrolet           
defmacro                   flet                  with-accessors            
defmethod                  handler-case          with-hash-table-iterator  
defsetf                    labels                with-input-from-string    
deftype                    let                   with-open-file            
defun                      let*                  with-open-stream          
destructuring-bind         locally               with-output-to-string     
do                         macrolet              with-package-iterator     
do*                        multiple-value-bind   with-slots                
do-all-symbols             pprint-logical-block                            

Figure 3-23. Standardized Forms In Which Declarations Can Occur

A declare expression can only occur where specified by the syntax of these forms. The consequences of attempting to evaluate a declare expression are undefined. In situations where such expressions can appear, explicit checks are made for their presence and they are never actually evaluated; it is for this reason that they are called ``declare expressions'' rather than ``declare forms.''

Macro forms cannot expand into declarations; declare expressions must appear as actual subexpressions of the form to which they refer.

The next figure shows a list of declaration identifiers that can be used with declare.

dynamic-extent  ignore     optimize  
ftype           inline     special   
ignorable       notinline  type      

Figure 3-24. Local Declaration Specifiers

An implementation is free to support other (implementation-defined) declaration identifiers as well.

Examples:

 (defun nonsense (k x z)
   (foo z x)                     ;First call to foo
   (let ((j (foo k x))           ;Second call to foo
         (x (* k k)))
     (declare (inline foo) (special x z))
     (foo x j z)))               ;Third call to foo

In this example, the inline declaration applies only to the third call to foo, but not to the first or second ones. The special declaration of x causes let to make a dynamic binding for x, and causes the reference to x in the body of let to be a dynamic reference. The reference to x in the second call to foo is a local reference to the second parameter of nonsense. The reference to x in the first call to foo is a local reference, not a special one. The special declaration of z causes the reference to z in the third call to foo to be a dynamic reference; it does not refer to the parameter to nonsense named z, because that parameter binding has not been declared to be special. (The special declaration of z does not appear in the body of defun, but in an inner form, and therefore does not affect the binding of the parameter.)

Affected By: None.

Exceptional Situations:

The consequences of trying to use a declare expression as a form to be evaluated are undefined.

See Also:

proclaim, Section 4.2.3 (Type Specifiers), declaration, dynamic-extent, ftype, ignorable, ignore, inline, notinline, optimize, type

Notes: None. 

Declaration IGNORE, IGNORABLE

Syntax:

(ignore {var | (function fn)}*)

(ignorable {var | (function fn)}*)

Arguments:

var---a variable name.

fn---a function name.

Valid Context:

declaration

Binding Types Affected:

variable, function

Description:

The ignore and ignorable declarations refer to for-value references to variable bindings for the vars and to function bindings for the fns.

An ignore declaration specifies that for-value references to the indicated bindings will not occur within the scope of the declaration. Within the scope of such a declaration, it is desirable for a compiler to issue a warning about the presence of either a for-value reference to any var or fn, or a special declaration for any var.

An ignorable declaration specifies that for-value references to the indicated bindings might or might not occur within the scope of the declaration. Within the scope of such a declaration, it is not desirable for a compiler to issue a warning about the presence or absence of either a for-value reference to any var or fn, or a special declaration for any var.

When not within the scope of a ignore or ignorable declaration, it is desirable for a compiler to issue a warning about any var for which there is neither a for-value reference nor a special declaration, or about any fn for which there is no for-value reference.

Any warning about a ``used'' or ``unused'' binding must be of type style-warning, and may not affect program semantics.

The stream variables established by with-open-file, with-open-stream, with-input-from-string, and with-output-to-string, and all iteration variables are, by definition, always ``used''. Using (declare (ignore v)), for such a variable v has unspecified consequences.

See Also:

declare 

Declaration DYNAMIC-EXTENT

Syntax:

(dynamic-extent [[var* | (function fn)*]])

Arguments:

var---a variable name.

fn---a function name.

Valid Context:

declaration

Binding Types Affected:

variable, function

Description:

In some containing form, F, this declaration asserts for each vari (which need not be bound by F), and for each value vij that vari takes on, and for each object xijk that is an otherwise inaccessible part of vij at any time when vij becomes the value of vari, that just after the execution of F terminates, xijk is either inaccessible (if F established a binding for vari) or still an otherwise inaccessible part of the current value of vari (if F did not establish a binding for vari). The same relation holds for each fni, except that the bindings are in the function namespace.

The compiler is permitted to use this information in any way that is appropriate to the implementation and that does not conflict with the semantics of Common Lisp.

dynamic-extent declarations can be free declarations or bound declarations.

The vars and fns named in a dynamic-extent declaration must not refer to symbol macro or macro bindings.

Examples:

Since stack allocation of the initial value entails knowing at the object's creation time that the object can be stack-allocated, it is not generally useful to make a dynamic-extent declaration for variables which have no lexically apparent initial value. For example, it is probably useful to write:

 (defun f ()
   (let ((x (list 1 2 3)))
     (declare (dynamic-extent x))
         ...))

This would permit those compilers that wish to do so to stack allocate the list held by the local variable x. It is permissible, but in practice probably not as useful, to write:

 (defun g (x) (declare (dynamic-extent x)) ...)
 (defun f () (g (list 1 2 3)))

Most compilers would probably not stack allocate the argument to g in f because it would be a modularity violation for the compiler to assume facts about g from within f. Only an implementation that was willing to be responsible for recompiling f if the definition of g changed incompatibly could legitimately stack allocate the list argument to g in f.

Here is another example:

 (declaim (inline g))
 (defun g (x) (declare (dynamic-extent x)) ...)
 (defun f () (g (list 1 2 3)))
 
 (defun f ()
   (flet ((g (x) (declare (dynamic-extent x)) ...))
     (g (list 1 2 3))))
 

In the previous example, some compilers might determine that optimization was possible and others might not.

A variant of this is the so-called ``stack allocated rest list'' that can be achieved (in implementations supporting the optimization) by:

 (defun f (&rest x)
   (declare (dynamic-extent x))
   ...)

Note that although the initial value of x is not explicit, the f function is responsible for assembling the list x from the passed arguments, so the f function can be optimized by the compiler to construct a stack-allocated list instead of a heap-allocated list in implementations that support such.

In the following example,

 (let ((x (list 'a1 'b1 'c1))
       (y (cons 'a2 (cons 'b2 (cons 'c2 nil)))))
   (declare (dynamic-extent x y))
   ...)

The otherwise inaccessible parts of x are three conses, and the otherwise inaccessible parts of y are three other conses. None of the symbols a1, b1, c1, a2, b2, c2, or nil is an otherwise inaccessible part of x or y because each is interned and hence accessible by the package (or packages) in which it is interned. However, if a freshly allocated uninterned symbol had been used, it would have been an otherwise inaccessible part of the list which contained it.

;; In this example, the implementation is permitted to stack allocate
;; the list that is bound to X.
 (let ((x (list 1 2 3)))
   (declare (dynamic-extent x))
   (print x)
   :done)
>>  (1 2 3)
=>  :DONE
 
;; In this example, the list to be bound to L can be stack-allocated.
 (defun zap (x y z)
   (do ((l (list x y z) (cdr l)))
       ((null l))
     (declare (dynamic-extent l))
     (prin1 (car l)))) =>  ZAP
 (zap 1 2 3)
>>  123
=>  NIL

;; Some implementations might open-code LIST-ALL-PACKAGES in a way
;; that permits using stack allocation of the list to be bound to L.
 (do ((l (list-all-packages) (cdr l)))
     ((null l))
   (declare (dynamic-extent l))
   (let ((name (package-name (car l))))
     (when (string-search "COMMON-LISP" name) (print name))))
>>  "COMMON-LISP"
>>  "COMMON-LISP-USER"
=>  NIL

;; Some implementations might have the ability to stack allocate 
;; rest lists.  A declaration such as the following should be a cue
;; to such implementations that stack-allocation of the rest list
;; would be desirable.
 (defun add (&rest x)
   (declare (dynamic-extent x))
   (apply #'+ x)) =>  ADD
 (add 1 2 3) =>  6

 (defun zap (n m)
   ;; Computes (RANDOM (+ M 1)) at relative speed of roughly O(N).
   ;; It may be slow, but with a good compiler at least it
   ;; doesn't waste much heap storage.  :-}
   (let ((a (make-array n)))
     (declare (dynamic-extent a))
     (dotimes (i n) 
       (declare (dynamic-extent i))
       (setf (aref a i) (random (+ i 1))))
     (aref a m))) =>  ZAP
 (< (zap 5 3) 3) =>  true

The following are in error, since the value of x is used outside of its extent:

 (length (list (let ((x (list 1 2 3)))  ; Invalid
                (declare (dynamic-extent x))
                x)))

 (progn (let ((x (list 1 2 3)))  ; Invalid
          (declare (dynamic-extent x))
          x)
        nil)

See Also:

declare

Notes:

The most common optimization is to stack allocate the initial value of the objects named by the vars.

It is permissible for an implementation to simply ignore this declaration. 

Declaration TYPE

Syntax:

(type typespec var*)

(typespec var*)

Arguments:

typespec---a type specifier.

var---a variable name.

Valid Context:

declaration or proclamation

Binding Types Affected:

variable

Description:

Affects only variable bindings and specifies that the vars take on values only of the specified typespec. In particular, values assigned to the variables by setq, as well as the initial values of the vars must be of the specified typespec. type declarations never apply to function bindings (see ftype).

A type declaration of a symbol defined by symbol-macrolet is equivalent to wrapping a the expression around the expansion of that symbol, although the symbol's macro expansion is not actually affected.

The meaning of a type declaration is equivalent to changing each reference to a variable (var) within the scope of the declaration to (the typespec var), changing each expression assigned to the variable (new-value) within the scope of the declaration to (the typespec new-value), and executing (the typespec var) at the moment the scope of the declaration is entered.

A type declaration is valid in all declarations. The interpretation of a type declaration is as follows:

1. During the execution of any reference to the declared variable within the scope of the declaration, the consequences are undefined if the value of the declared variable is not of the declared type.

2. During the execution of any setq of the declared variable within the scope of the declaration, the consequences are undefined if the newly assigned value of the declared variable is not of the declared type.

3. At the moment the scope of the declaration is entered, the consequences are undefined if the value of the declared variable is not of the declared type.

A type declaration affects only variable references within its scope.

If nested type declarations refer to the same variable, then the value of the variable must be a member of the intersection of the declared types.

If there is a local type declaration for a dynamic variable, and there is also a global type proclamation for that same variable, then the value of the variable within the scope of the local declaration must be a member of the intersection of the two declared types.

type declarations can be free declarations or bound declarations.

A symbol cannot be both the name of a type and the name of a declaration. Defining a symbol as the name of a class, structure, condition, or type, when the symbol has been declared as a declaration name, or vice versa, signals an error.

Within the lexical scope of an array type declaration, all references to array elements are assumed to satisfy the expressed array element type (as opposed to the upgraded array element type). A compiler can treat the code within the scope of the array type declaration as if each access of an array element were surrounded by an appropriate the form.

Examples:

 (defun f (x y)
   (declare (type fixnum x y))
   (let ((z (+ x y)))
     (declare (type fixnum z))
     z)) =>  F
 (f 1 2) =>  3
 ;; The previous definition of F is equivalent to
 (defun f (x y)
   ;; This declaration is a shorthand form of the TYPE declaration
   (declare (fixnum x y))
   ;; To declare the type of a return value, it's not necessary to
   ;; create a named variable.  A THE special form can be used instead.
   (the fixnum (+ x y))) =>  F
 (f 1 2) =>  3

 (defvar *one-array* (make-array 10 :element-type '(signed-byte 5)))
 (defvar *another-array* (make-array 10 :element-type '(signed-byte 8)))
  
 (defun frob (an-array)
   (declare (type (array (signed-byte 5) 1) an-array))
   (setf (aref an-array 1) 31)
   (setf (aref an-array 2) 127)
   (setf (aref an-array 3) (* 2 (aref an-array 3)))
   (let ((foo 0))
     (declare (type (signed-byte 5) foo))
     (setf foo (aref an-array 0))))
  
 (frob *one-array*)
 (frob *another-array*)

The above definition of frob is equivalent to:

 (defun frob (an-array)
   (setf (the (signed-byte 5) (aref an-array 1)) 31)
   (setf (the (signed-byte 5) (aref an-array 2)) 127)
   (setf (the (signed-byte 5) (aref an-array 3))
         (* 2 (the (signed-byte 5) (aref an-array 3))))
   (let ((foo 0))
     (declare (type (signed-byte 5) foo))
     (setf foo (the (signed-byte 5) (aref an-array 0)))))

Given an implementation in which fixnums are 29 bits but fixnum arrays are upgraded to signed 32-bit arrays, the following could be compiled with all fixnum arithmetic:

 (defun bump-counters (counters)
   (declare (type (array fixnum *) bump-counters))
   (dotimes (i (length counters))
     (incf (aref counters i))))

See Also:

declare, declaim, proclaim

Notes:

(typespec var*) is an abbreviation for (type typespec var*).

A type declaration for the arguments to a function does not necessarily imply anything about the type of the result. The following function is not permitted to be compiled using implementation-dependent fixnum-only arithmetic:

 (defun f (x y) (declare (fixnum x y)) (+ x y))

To see why, consider (f most-positive-fixnum 1). Common Lisp defines that F must return a bignum here, rather than signal an error or produce a mathematically incorrect result. If you have special knowledge such ``fixnum overflow'' cases will not come up, you can declare the result value to be in the fixnum range, enabling some compilers to use more efficient arithmetic:

 (defun f (x y)
   (declare (fixnum x y))
   (the fixnum (+ x y)))

Note, however, that in the three-argument case, because of the possibility of an implicit intermediate value growing too large, the following will not cause implementation-dependent fixnum-only arithmetic to be used:

 (defun f (x y)
   (declare (fixnum x y z))
   (the fixnum (+ x y z)))

To see why, consider (f most-positive-fixnum 1 -1). Although the arguments and the result are all fixnums, an intermediate value is not a fixnum. If it is important that implementation-dependent fixnum-only arithmetic be selected in implementations that provide it, consider writing something like this instead:

 (defun f (x y)
   (declare (fixnum x y z))
   (the fixnum (+ (the fixnum (+ x y)) z)))


Declaration INLINE, NOTINLINE

Syntax:

(inline function-name*)

(notinline function-name*)

Arguments:

function-name---a function name.

Valid Context:

declaration or proclamation

Binding Types Affected:

function

Description:

inline specifies that it is desirable for the compiler to produce inline calls to the functions named by function-names; that is, the code for a specified function-name should be integrated into the calling routine, appearing ``in line'' in place of a procedure call. A compiler is free to ignore this declaration. inline declarations never apply to variable bindings.

If one of the functions mentioned has a lexically apparent local definition (as made by flet or labels), then the declaration applies to that local definition and not to the global function definition.

While no conforming implementation is required to perform inline expansion of user-defined functions, those implementations that do attempt to recognize the following paradigm:

To define a function f that is not inline by default but for which (declare (inline f)) will make f be locally inlined, the proper definition sequence is:

 (declaim (inline f))
 (defun f ...)
 (declaim (notinline f))

The inline proclamation preceding the defun form ensures that the compiler has the opportunity save the information necessary for inline expansion, and the notinline proclamation following the defun form prevents f from being expanded inline everywhere.

notinline specifies that it is undesirable to compile the functions named by function-names in-line. A compiler is not free to ignore this declaration; calls to the specified functions must be implemented as out-of-line subroutine calls.

If one of the functions mentioned has a lexically apparent local definition (as made by flet or labels), then the declaration applies to that local definition and not to the global function definition.

In the presence of a compiler macro definition for function-name, a notinline declaration prevents that compiler macro from being used. An inline declaration may be used to encourage use of compiler macro definitions. inline and notinline declarations otherwise have no effect when the lexically visible definition of function-name is a macro definition.

inline and notinline declarations can be free declarations or bound declarations. inline and notinline declarations of functions that appear before the body of a flet or labels form that defines that function are bound declarations. Such declarations in other contexts are free declarations.

Examples:

 ;; The globally defined function DISPATCH should be open-coded,
 ;; if the implementation supports inlining, unless a NOTINLINE 
 ;; declaration overrides this effect.
 (declaim (inline dispatch))
 (defun dispatch (x) (funcall (get (car x) 'dispatch) x))
 ;; Here is an example where inlining would be encouraged.
 (defun top-level-1 () (dispatch (read-command)))
 ;; Here is an example where inlining would be prohibited.
 (defun top-level-2 ()
   (declare (notinline dispatch))
   (dispatch (read-command)))
 ;; Here is an example where inlining would be prohibited.
 (declaim (notinline dispatch))
 (defun top-level-3 () (dispatch (read-command)))
 ;; Here is an example where inlining would be encouraged.
 (defun top-level-4 () 
   (declare (inline dispatch))
   (dispatch (read-command)))

See Also:

declare, declaim, proclaim 

Declaration FTYPE

Syntax:

(ftype type function-name*)

Arguments:

function-name---a function name.

type---a type specifier.

Valid Context:

declaration or proclamation

Binding Types Affected:

function

Description:

Specifies that the functions named by function-names are of the functional type type. For example:

 (declare (ftype (function (integer list) t) ith)
          (ftype (function (number) float) sine cosine))

If one of the functions mentioned has a lexically apparent local definition (as made by flet or labels), then the declaration applies to that local definition and not to the global function definition. ftype declarations never apply to variable bindings (see type).

The lexically apparent bindings of function-names must not be macro definitions. (This is because ftype declares the functional definition of each function name to be of a particular subtype of function, and macros do not denote functions.)

ftype declarations can be free declarations or bound declarations. ftype declarations of functions that appear before the body of a flet or labels form that defines that function are bound declarations. Such declarations in other contexts are free declarations.

See Also:

declare, declaim, proclaim 

Declaration DECLARATION

Syntax:

(declaration name*)

Arguments:

name---a symbol.

Binding Types Affected: None.

Valid Context:

proclamation only

Description:

Advises the compiler that each name is a valid but potentially non-standard declaration name. The purpose of this is to tell one compiler not to issue warnings for declarations meant for another compiler or other program processor.

Examples:

 (declaim (declaration author target-language target-machine))
 (declaim (target-language ada))
 (declaim (target-machine IBM-650))
 (defun strangep (x)
   (declare (author "Harry Tweeker"))
   (member x '(strange weird odd peculiar)))

See Also:

declaim, proclaim 

Declaration OPTIMIZE

Syntax:

(optimize {quality | (quality value)}*)

Arguments:

quality---an optimize quality.

value---one of the integers 0, 1, 2, or 3.

Valid Context:

declaration or proclamation

Binding Types Affected: None.

Description:

Advises the compiler that each quality should be given attention according to the specified corresponding value. Each quality must be a symbol naming an optimize quality; the names and meanings of the standard optimize qualities are shown in the next figure.

Name               Meaning                            
compilation-speed  speed of the compilation process   
debug              ease of debugging                  
safety             run-time error checking            
space              both code size and run-time space  
speed              speed of the object code           

Figure 3-25. Optimize qualities

There may be other, implementation-defined optimize qualities.

A value 0 means that the corresponding quality is totally unimportant, and 3 that the quality is extremely important; 1 and 2 are intermediate values, with 1 the neutral value. (quality 3) can be abbreviated to quality.

Note that code which has the optimization (safety 3), or just safety, is called safe code.

The consequences are unspecified if a quality appears more than once with different values.

Examples:

 (defun often-used-subroutine (x y)
   (declare (optimize (safety 2)))
   (error-check x y)
   (hairy-setup x)
   (do ((i 0 (+ i 1))
        (z x (cdr z)))
       ((null z))
     ;; This inner loop really needs to burn.
     (declare (optimize speed))
     (declare (fixnum i))
     ))

See Also:

declare, declaim, proclaim, Section 3.3.4 (Declaration Scope)

Notes:

An optimize declaration never applies to either a variable or a function binding. An optimize declaration can only be a free declaration. For more information, see Section 3.3.4 (Declaration Scope). 

Declaration SPECIAL

Syntax:

(special var*)

Arguments:

var---a symbol.

Valid Context:

declaration or proclamation

Binding Types Affected:

variable

Description:

Specifies that all of the vars named are dynamic. This specifier affects variable bindings and affects references. All variable bindings affected are made to be dynamic bindings, and affected variable references refer to the current dynamic binding. For example:

 (defun hack (thing *mod*)    ;The binding of the parameter
   (declare (special *mod*))  ; *mod* is visible to hack1,
   (hack1 (car thing)))       ; but not that of thing.
 (defun hack1 (arg)
   (declare (special *mod*))  ;Declare references to *mod*
                              ;within hack1 to be special.
   (if (atom arg) *mod*
       (cons (hack1 (car arg)) (hack1 (cdr arg)))))

A special declaration does not affect inner bindings of a var; the inner bindings implicitly shadow a special declaration and must be explicitly re-declared to be special. special declarations never apply to function bindings.

special declarations can be either bound declarations, affecting both a binding and references, or free declarations, affecting only references, depending on whether the declaration is attached to a variable binding.

When used in a proclamation, a special declaration specifier applies to all bindings as well as to all references of the mentioned variables. For example, after

 (declaim (special x))

then in a function definition such as

 (defun example (x) ...)

the parameter x is bound as a dynamic variable rather than as a lexical variable.

Examples:

(defun declare-eg (y)                 ;this y is special
 (declare (special y))
 (let ((y t))                         ;this y is lexical
      (list y
            (locally (declare (special y)) y)))) ;this y refers to the
                                                 ;special binding of y
=>  DECLARE-EG 
 (declare-eg nil) =>  (T NIL) 

(setf (symbol-value 'x) 6)
(defun foo (x)                         ;a lexical binding of x
  (print x)
  (let ((x (1+ x)))                    ;a special binding of x
    (declare (special x))              ;and a lexical reference
    (bar))
  (1+ x))
(defun bar () 
  (print (locally (declare (special x))
           x)))
(foo 10) 
>>  10
>>  11
=>  11

(setf (symbol-value 'x) 6)
(defun bar (x y)            ;[1] 1st occurrence of x
  (let ((old-x x)           ;[2] 2nd occurrence of x -- same as 1st occurrence
        (x y))              ;[3] 3rd occurrence of x
    (declare (special x))
    (list old-x x)))
(bar 'first 'second) =>  (FIRST SECOND)

 (defun few (x &optional (y *foo*))
   (declare (special *foo*))
   ...)

The reference to *foo* in the first line of this example is not special even though there is a special declaration in the second line.

 (declaim (special prosp)) =>  implementation-dependent
 (setq prosp 1 reg 1) =>  1
 (let ((prosp 2) (reg 2))         ;the binding of prosp is special
    (set 'prosp 3) (set 'reg 3)   ;due to the preceding proclamation,
    (list prosp reg))             ;whereas the variable reg is lexical
=>  (3 2)
 (list prosp reg) =>  (1 3)

 (declaim (special x))          ;x is always special.
 (defun example (x y)                                 
   (declare (special y))
   (let ((y 3) (x (* x 2)))
     (print (+ y (locally (declare (special y)) y)))
     (let ((y 4)) (declare (special y)) (foo x)))) =>  EXAMPLE

In the contorted code above, the outermost and innermost bindings of y are dynamic, but the middle binding is lexical. The two arguments to + are different, one being the value, which is 3, of the lexical variable y, and the other being the value of the dynamic variable named y (a binding of which happens, coincidentally, to lexically surround it at an outer level). All the bindings of x and references to x are dynamic, however, because of the proclamation that x is always special.

See Also:

defparameter, defvar 

Special Operator LOCALLY

Syntax:

locally declaration* form* => result*

Arguments and Values:

Declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values of the forms.

Description:

Sequentially evaluates a body of forms in a lexical environment where the given declarations have effect.

Examples:

 (defun sample-function (y)  ;this y is regarded as special
   (declare (special y))                                
   (let ((y t))              ;this y is regarded as lexical
     (list y
           (locally (declare (special y))
             ;; this next y is regarded as special
             y))))
=>  SAMPLE-FUNCTION
 (sample-function nil) =>  (T NIL) 
 (setq x '(1 2 3) y '(4 . 5)) =>  (4 . 5)

;;; The following declarations are not notably useful in specific.
;;; They just offer a sample of valid declaration syntax using LOCALLY.
 (locally (declare (inline floor) (notinline car cdr))
          (declare (optimize space))
    (floor (car x) (cdr y))) =>  0, 1

;;; This example shows a definition of a function that has a particular set
;;; of OPTIMIZE settings made locally to that definition.
 (locally (declare (optimize (safety 3) (space 3) (speed 0)))
   (defun frob (w x y &optional (z (foo x y)))
     (mumble x y z w)))
=>  FROB

;;; This is like the previous example, except that the optimize settings
;;; remain in effect for subsequent definitions in the same compilation unit.
 (declaim (optimize (safety 3) (space 3) (speed 0)))
 (defun frob (w x y &optional (z (foo x y)))
   (mumble x y z w))
=>  FROB

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

See Also:

declare

Notes:

The special declaration may be used with locally to affect references to, rather than bindings of, variables.

If a locally form is a top level form, the body forms are also processed as top level forms. See Section 3.2.3 (File Compilation). 

Special Operator THE

Syntax:

the value-type form => result*

Arguments and Values:

value-type---a type specifier; not evaluated.

form---a form; evaluated.

results---the values resulting from the evaluation of form. These values must conform to the type supplied by value-type; see below.

Description:

the specifies that the values[1a] returned by form are of the types specified by value-type. The consequences are undefined if any result is not of the declared type.

It is permissible for form to yield a different number of values than are specified by value-type, provided that the values for which types are declared are indeed of those types. Missing values are treated as nil for the purposes of checking their types.

Regardless of number of values declared by value-type, the number of values returned by the the special form is the same as the number of values returned by form.

Examples:

 (the symbol (car (list (gensym)))) =>  #:G9876
 (the fixnum (+ 5 7)) =>  12
 (the (values) (truncate 3.2 2)) =>  1, 1.2
 (the integer (truncate 3.2 2)) =>  1, 1.2
 (the (values integer) (truncate 3.2 2)) =>  1, 1.2
 (the (values integer float) (truncate 3.2 2))   =>  1, 1.2
 (the (values integer float symbol) (truncate 3.2 2)) =>  1, 1.2
 (the (values integer float symbol t null list) 
      (truncate 3.2 2)) =>  1, 1.2
 (let ((i 100))
    (declare (fixnum i))
    (the fixnum (1+ i))) =>  101
 (let* ((x (list 'a 'b 'c))
        (y 5))
    (setf (the fixnum (car x)) y)
    x) =>  (5 B C)

Affected By: None.

Exceptional Situations:

The consequences are undefined if the values yielded by the form are not of the type specified by value-type.

See Also:

values

Notes:

The values type specifier can be used to indicate the types of multiple values:

 (the (values integer integer) (floor x y))
 (the (values string t)
      (gethash the-key the-string-table))

setf can be used with the type declarations. In this case the declaration is transferred to the form that specifies the new value. The resulting setf form is then analyzed. 

Function SPECIAL-OPERATOR-P

Syntax:

special-operator-p symbol => generalized-boolean

Arguments and Values:

symbol---a symbol.

generalized-boolean---a generalized boolean.

Description:

Returns true if symbol is a special operator; otherwise, returns false.

Examples:

 (special-operator-p 'if) =>  true
 (special-operator-p 'car) =>  false
 (special-operator-p 'one) =>  false

Side Effects: None.

Affected By: None.

Exceptional Situations:

Should signal type-error if its argument is not a symbol.

See Also: None.

Notes:

Historically, this function was called special-form-p. The name was finally declared a misnomer and changed, since it returned true for special operators, not special forms. 

Function CONSTANTP

Syntax:

constantp form &optional environment => generalized-boolean

Arguments and Values:

form---a form.

environment---an environment object. The default is nil.

generalized-boolean---a generalized boolean.

Description:

Returns true if form can be determined by the implementation to be a constant form in the indicated environment; otherwise, it returns false indicating either that the form is not a constant form or that it cannot be determined whether or not form is a constant form.

The following kinds of forms are considered constant forms:

* Self-evaluating objects (such as numbers, characters, and the various kinds of arrays) are always considered constant forms and must be recognized as such by constantp.

* Constant variables, such as keywords, symbols defined by Common Lisp as constant (such as nil, t, and pi), and symbols declared as constant by the user in the indicated environment using defconstant are always considered constant forms and must be recognized as such by constantp.

* quote forms are always considered constant forms and must be recognized as such by constantp.

* An implementation is permitted, but not required, to detect additional constant forms. If it does, it is also permitted, but not required, to make use of information in the environment. Examples of constant forms for which constantp might or might not return true are: (sqrt pi), (+ 3 2), (length '(a b c)), and (let ((x 7)) (zerop x)).

If an implementation chooses to make use of the environment information, such actions as expanding macros or performing function inlining are permitted to be used, but not required; however, expanding compiler macros is not permitted.

Examples:

 (constantp 1) =>  true
 (constantp 'temp) =>  false
 (constantp ''temp)) =>  true
 (defconstant this-is-a-constant 'never-changing) =>  THIS-IS-A-CONSTANT 
 (constantp 'this-is-a-constant) =>  true
 (constantp "temp") =>  true
 (setq a 6) =>  6 
 (constantp a) =>  true
 (constantp '(sin pi)) =>  implementation-dependent
 (constantp '(car '(x))) =>  implementation-dependent
 (constantp '(eql x x)) =>  implementation-dependent
 (constantp '(typep x 'nil)) =>  implementation-dependent
 (constantp '(typep x 't)) =>  implementation-dependent
 (constantp '(values this-is-a-constant)) =>  implementation-dependent
 (constantp '(values 'x 'y)) =>  implementation-dependent
 (constantp '(let ((a '(a b c))) (+ (length a) 6))) =>  implementation-dependent

Side Effects: None.

Affected By:

The state of the global environment (e.g., which symbols have been declared to be the names of constant variables).

Exceptional Situations: None.

See Also:

defconstant

Notes: None. 

