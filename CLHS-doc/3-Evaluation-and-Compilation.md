# 3. 编译和求值
<!--TODO 范围[extent] ??-->
> * 3.1 [求值](#Evaluation)
> * 3.2 [编译](#Compilation)
> * 3.3 [声明](#Declarations)
> * 3.4 [Lambda 列表](#LambdaLists)
> * 3.5 [函数调用中的错误检测](#ErrorChecking)
> * 3.6 [遍历规则和副作用](#TraversalRulesSideEffects)
> * 3.7 [破坏性操作](#DestructiveOperations)
> * 3.8 [求值和编译的字典条目](#EvaluationCompilationDictionary)

## 3.1 <span id = "Evaluation">求值</span>

代码[code]的执行可以通过多种方式来完成, 从对表示一个程序[program]的表达式形式[form]的直接解释到一个编译器[compiler]生成的编译代码[compiled code]的调用都可以.

求值[evaluation]是一个过程, 程序[program]通过这个过程在 Common Lisp 中执行. 求值[evaluation]机制通过 Lisp read-eval-print 循环[Lisp read-eval-print loop]的效果来隐式表现, 并且可以通过函数[function] eval, compile, compile-file 和 load 显式表现. 其中的任何一个都可能共享相同的执行策略, 或者每个都可能使用不同的执行策略.

符合规范的程序[conforming program]被 eval 和被 compile-file 处理后的行为可能不同; 见章节 3.2.2.3 (语义约束).

可以通过一个模型来理解求值, 在这个模型中, 解释器递归地遍历一个表达式形式[form], 它执行计算过程中的每一个步骤. 这个描述了 Common Lisp 程序[program]语义的模型, 被描述在章节 3.1.2 (求值模型).

> * 3.1.1 [环境的介绍](#IntroductionEnvironments)
> * 3.1.2 [求值模型](#TheEvaluationModel)
> * 3.1.3 [lambda 表达式](#LambdaExpressions)
> * 3.1.4 [闭包和词法绑定](#ClosuresLexicalBinding)
> * 3.1.5 [遮蔽](#Shadowing)
> * 3.1.6 [范围](#Extent)
> * 3.1.7 [返回值](#ReturnValues)

### 3.1.1 <span id = "IntroductionEnvironments">环境的介绍</span>

一个绑定[binding]是一个名字和它所表示的东西的关联. 绑定[binding]由特定的特殊操作符[special operators]在一个词法环境[lexical environment]或动态环境[dynamic environment]中建立.

一个环境[environment]由一个绑定[binding]集合和求值过程中使用的其他信息 (比如, 用来关联名字和意义的信息) 组成.

一个环境[environment]中的绑定[binding]被用命名空间[namespace]来划分. 单独的名字在每一个环境[environment]中可以同时有超过一个关联的绑定[binding], 每一个命名空间[namespace]只能有一个关联的绑定[binding].

> * 3.1.1.1 [全局的环境](#TheGlobalEnvironment)
> * 3.1.1.2 [动态环境](#DynamicEnvironments)
> * 3.1.1.3 [词法环境](#LexicalEnvironments)
> * 3.1.1.4 [环境对象](#EnvironmentObjects)

#### 3.1.1.1 <span id = "TheGlobalEnvironment">全局的环境</span>

全局环境[global environment]是环境[environment]中包含无限作用域[indefinite scope]和无限范围[indefinite extent]的绑定[binding]的部分. 全局环境[global environment]包括以下部分:

    动态变量[dynamic variable]和常变量[constant variable]的绑定[binding].
    函数[function], 宏[macro], 特殊操作符[special operator]的绑定[binding].
    编译器宏[compiler macro]绑定[binding].
    类型[type]和类[class]名[name]绑定[binding].
    全局声明[proclamation]相关的信息. 

#### 3.1.1.2 <span id = "DynamicEnvironments">动态环境</span>

求值[evaluation]的动态环境[dynamic environment]是环境[environment]中包含了持续时间受限于建立绑定[binding]的表达式形式[form]执行期间绑定建立和解除的点的那些绑定[binding]的部分. 一个动态环境[dynamic environment]包括以下内容:

    动态变量[dynamic variable]的绑定[binding].
    活跃[active]的捕捉标签[catch tag]的信息.
    unwind-protect 建立的退出点[exit point]的信息.
    活跃的处理者[handler]和重启动[restart]的信息.

在程序[program]执行过程中, 在任何明确引用"当前动态环境(the current dynamic environment)", 或者有时只是"动态环境(the dynamic environment)"的地方, 动态环境[dynamic environment]都是可用的.

在给定的命名空间[namespace]中, 如果在动态环境[dynamic environment]中有一个与一个名称[name]相关联的绑定[binding], 或者在全局环境[global environment]中有一个与这个名称[name]相关联的绑定[binding], 那么就说这个名称[name]被绑定到这个动态环境[dynamic environment]中. 

#### 3.1.1.3 <span id = "LexicalEnvironments">词法环境</span>

在一个程序[program]中, 某个位置的求值[evaluation]的词法环境[lexical environment]是环境[environment]中包含了特定信息的部分, 这些信息有着在包含这个位置的那些表达式形式[form]中的词法作用域[lexical environment]. 一个词法环境[lexical environment]包含以下内容:

    词法变量[lexical variable]和符号宏[symbol macro]的绑定[binding].
    函数[function]和宏[macro]的绑定. (这里隐含了关于局部禁用的编译器宏[compiler macro]的信息.)
    block 标记[block tag]的绑定[binding].
    go 标记[go tag]的绑定[binding].
    声明[declaration]的信息.

在一个正在被语义处理的程序中被明确引用为"当前词法环境(the current lexical environment)"或者有时只是"词法环境(the lexical environment)"的任何给定位置, 词法环境[lexical environment]都是有效的.

在给定的命名空间[namespace]中, 如果在词法环境[lexical environment]中有一个与一个名称[name]相关联的绑定[binding], 或者在全局环境[global environment]中有一个与这个名称[name]相关联的绑定[binding], 那么就说这个名称[name]被绑定到这个词法环境[lexical environment]中.

##### 3.1.1.3.1 空的词法环境

空的词法环境[null lexical environment]等价于全局环境[[global environment]].

虽然通常一个环境[environment]对象[object]的表示是依赖于具体实现的[implementation-dependent], 但是 nil 可以被用于任何需要一个环境[environment]对象[object]来表示表示空的词法环境[null lexical environment]的情况. 

#### 3.1.1.4 <span id = "EnvironmentObjects">环境对象</span>

一些操作符[operator]使用一个称为环境对象[environment object]的对象[object], 这表示对给定的词法环境[lexical environment]中的一个表达式形式[form]上执行语义分析所需的词法绑定[lexical binding]的集合. 环境对象[environment object]中的绑定[binding]集合可能是实际执行求值所需的绑定[binding]的子集; 比如, 在相应的词法环境[lexical environment]中与变量[variable]名[name]和函数名[function name]相关联的值[value]可能无法在一个环境对象[environment object]中使用.

一个环境对象[environment object]的类型[type]和性质是依赖于具体实现的[implementation-dependent]. 给宏函数[macro function]的环境参数[environment parameter]的值[value]就是环境对象[environment object]的示例.

当 nil 对象[object]被用于一个环境对象[environment object]时, 表示空的词法环境[null lexical environment]; 见章节 3.1.1.3.1 (空的词法环境). 

### 3.1.2 <span id = "TheEvaluationModel">求值模型</span>

一个 Common Lisp 系统根据词法, 动态和全局环境[environment]对表达式形式[form]进行求值. 下面章节描述的 Common Lisp 求值模型的组成部分.

#### 3.1.2.1 表达式形式求值

表达式形式[form]分为三个类别: 符号[symbol], cons 和自求值对象[self-evaluating object]. 下面的章节介绍这些类别.

> * 3.1.2.1.1 [符号表达式形式](#SymbolsForms)
> * 3.1.2.1.2 [cons 表达式形式](#ConsesForms)
> * 3.1.2.1.3 [自求值对象](#SelfEvaluatingObjects)

##### 3.1.2.1.1 <span id = "SymbolsForms">符号表达式形式</span>

如果一个表达式形式[form]是一个符号[symbol], 那么它要么是一个符号宏[symbol macro], 要么是一个变量[variable].

如果在当前的词法环境[lexical environment]中有这个符号[symbol]作为符号宏[symbol macro]的绑定[binding](见 define-symbol-macro 和 symbol-macrolet), 那么这个符号[symbol]就会命名一个符号宏[symbol macro]. 如果这个符号[symbol]是一个符号宏[symbol macro], 就会得到它的展开函数. 展开函数是一个带有两个参数的函数, 通过把这个展开函数作为第一个参数, 这个符号[symbol]作为第二个参数, 以及一个环境对象[environment object] (对应当前词法环境[lexical environment])作为第三个参数调用宏展开钩子[macroexpand hook]来调用它. 然后, 这个宏展开钩子[macroexpand hook]调用展开函数, 把这个表达式形式[form]作为第一个参数并且把环境[environment]作为它的第二个参数. 传递给宏展开钩子[macroexpand hook]的展开函数的值[value]是一个表达式形式[form]. 产生的这个表达式形式[form]会替换到原来符号[symbol]的位置.

如果一个表达式形式[form]是一个符号[symbol], 但不是符号宏[symbol macro], 那么它就是一个变量[variable]的名字[name], 并且返回这个变量[variable]的值[name]. 这里有三种变量: 词法变量[lexical variable], 动态变量[dynamic variable], 还有常变量[constant variable]. 一个变量[variable]可以存储一个对象[object]. 在一个变量[variable]上的主要操作是读取[read[1]]和写入[write[1]]它的值[value].

如果引用到一个未绑定的变量[unbound variable], 会发出一个 unbound-variable 类型[type]的错误.

非常量[non-constant variable]可以通过 setq 来赋值或者 let 来绑定[bound[3]]. 下面这段列出了可以赋值, 绑定, 和定义变量[variable]的一些已定义名字[defined name].

    boundp        let                  progv         
    defconstant   let*                 psetq         
    defparameter  makunbound           set           
    defvar        multiple-value-bind  setq          
    lambda        multiple-value-setq  symbol-value  

Figure 3-1. 可应用于变量的一些已定义的名字

下面是对每个变量种类的描述.

> * 3.1.2.1.1.1 [词法变量](#LexicalVariables)
> * 3.1.2.1.1.2 [动态变量](#DynamicVariables)
> * 3.1.2.1.1.3 [常变量](#ConstantVariables)
> * 3.1.2.1.1.4 [同时命名词法和动态变量的符号](#SymbolsNamingLDVariables)

###### 3.1.2.1.1.1 <span id = "LexicalVariables">词法变量</span>

词法变量[lexical variable]是一个只能在建立该变量[variable]的表达式形式[form]的词法作用域[lexical scope]内引用的变量[variable]; 词法变量[lexical variable]有词法作用域[lexical scope]. 每次一个表达式形式[form]创建一个变量[variable]的词法绑定, 就建立一个新的绑定[fresh binding].

在一个词法变量[lexical variable]名字[name]的绑定[binding]的作用域[scope]里, 这个名字[name]作为变量[variable]的使用被认为是对这个绑定[binding]的引用, 除了这个变量[variable]被遮蔽[shadow[2]]的地方, 通过一个建立[establish]该变量[variable]名[name]的新的[fresh]绑定[binding]的表达式形式[form], 或者通过一个表达式形式[form]将该名字[name]局部声明[declare]为 special.

一个词法变量[lexical variable]总是有一个值[value]. 这里没有可以引入一个词法变量[lexical variable]的绑定[binding]而没有给初始值[value]的操作符[operator], 也没有任何操作符[operator]可以使一个词法变量[lexical variable]解绑[unbound].

词法变量[lexical variable]的绑定[binding]可以在词法环境[lexical environment]中找到. 

###### 3.1.2.1.1.2 <span id = "DynamicVariables">动态变量</span>

如果一个变量[variable]满足下面条件的其中之一, 那么这个变量[variable]就是一个动态变量[dynamic variable]:

    它被局部声明或全局声明为 special.

    它以文本形式出现在一个为相同[same]名字[name]的变量[variable]创建一个动态绑定[dynamic binding]的表达式形式[form]中, 并且这个绑定[binding]没有被一个对相同变量[variable]名[name]创建词法绑定[lexical binding]的表达式形式[form]所遮蔽[shadow[2]].

一个动态变量[dynamic variable]可以在任何程序[program]的任何时间被引用; 对动态变量[dynamic variable]的引用没有文本限制. 在任何给定时间, 具有给定名称的所有动态变量[dynamic variable]都精确地引用一个绑定[binding], 不管是在动态环境[dynamic environment]中或全局环境[global environment]中.

一个动态变量[dynamic variable]的绑定[binding]的值[value]部分可能是空的; 在这个情况下, 就说这个动态变量[dynamic variable]没有值[value]或者被解除绑定[unbound]. 一个动态变量[dynamic variable]可以通过使用 makunbound 来解除绑定[unbound].

绑定[binding]一个动态变量[dynamic variable]的效果是创建一个新的绑定[binding], 在创建这个动态绑定[dynamic binding]的表达式形式[form]求值[evaluation]期间, 任何程序[program]中所有对该动态变量[dynamic variable]的引用都是指新创建的绑定.

动态变量[dynamic variable]可以在绑定[bind]它的表达式形式[form]的动态范围[dynamic extent]之外引用. 这种变量[variable]有时被称为"全局变量(global variable)", 但它仍然在所有方面都只是一个动态变量[dynamic variable], 它的绑定[binding]恰好存在于全局环境[global environment]中, 而不是在某些动态环境[dynamic environment]中.

除了这个规范或具体实现[implementation]中定义了初始值的变量以外的动态变量[dynamic variable]除非显式赋值, 否则就是未绑定的[unbound]. 

###### 3.1.2.1.1.3 <span id = "ConstantVariables">常变量</span>

某些变量, 称为常变量[constant variable], 被保留为"已命名常量(named constants)". 如果尝试去给它赋值, 或者给一个常变量[constant variable]创建一个绑定[binding], 后果是未定义的, 除了使用兼容的 defconstant 去重定义一个常变量[constant variable]是允许的; 见宏[macro] defconstant.

关键字[keyword], Common Lisp 或者具体实现[implementation]定义为常量的符号[symbol] (就像 nil, t, 和 pi), 还有通过 defconstant 声明为常量的符号也是常变量[constant variable]. 

###### 3.1.2.1.1.4 <span id = "SymbolsNamingLDVariables">同时命名词法和动态变量的符号</span>

同一个符号[symbol]可以命名一个词法变量[lexical variable]和动态变量[dynamic variable], 但是从来不出现在相同的词法环境[lexical environment]中.

下面的例子中, 符号 x 在不同的时间被用作词法变量[lexical variable]的名字[name]和动态变量[dynamic variable]的名字[name].

```LISP
(let ((x 1))            ;Binds a special variable X
  (declare (special x))
  (let ((x 2))          ;Binds a lexical variable X
    (+ x                ;Reads a lexical variable X
      (locally (declare (special x))
                x))))   ;Reads a special variable X
=>  3
```

##### 3.1.2.1.2 <span id = "ConsesForms">cons 表达式形式</span>

被用作一个表达式形式[form]的一个 cons, 被称为为复合表达式形式[compound form].

如果这个复合表达式形式[compound form]的 car 是一个符号[symbol], 这个符号[symbol]是一个操作符[operator]的名字[name], 那么这个表达式形式[form]是一个特殊表达式形式[special form], 一个宏表达式[macro form], 或者是一个函数表达式[function form], 取决于这个操作符[operator]在当前词法环境[lexical environment]的函数[function]绑定[binding]. 如果这个操作符[operator]既不是一个特殊操作符[special operator]也不是一个宏名字[macro name], 那么它被假定为一个函数名字[function name] (即便在这里没有这样的一个函数[function]定义).

如果这个复合表达式形式[compound form]的 car 部分不是一个符号[symbol], 那么这个 car 必须是一个 lambda 表达式[lambda expression], 在这个情况下这个复合表达式形式[compound form]就是一个 lambda 表达式形式[lambda form].

一个复合表达式[compound form]如何被处理取决于它被归类为特殊表达式形式[special form], 宏表达式形式[macro form], 函数表达式形式[function form], 还是一个 lambda 表达式形式[lambda form].

> * 3.1.2.1.2.1 [特殊表达式形式](#SpecialForms)
> * 3.1.2.1.2.2 [宏表达式形式](#MacroForms)
> * 3.1.2.1.2.3 [函数表达式形式](#FunctionForms)
> * 3.1.2.1.2.4 [lambda 表达式形式](#LambdaForms)

###### 3.1.2.1.2.1 <span id = "SpecialForms">特殊表达式形式</span>

一个特殊表达式形式[special form]是一个带有特殊语法或者特殊求值规则或者两者都有的表达式形式[form], 可能控制求值环境, 控制流, 或者都控制. 一个特殊操作符[special operator]可以访问当前的词法环境[lexical environment]和当前的动态环境[dynamic environment]. 每个特殊操作符[special operator]都定义了它的那些子表达式[subexpression]的处理方式---哪些是表达式形式[form], 哪些是特殊语法, 等等.

一些特殊操作符[special operator]创建新的词法或动态环境[environment], 以便在对这个特殊表达式形式[special form]的那些子表达式形式[subform]的求值[evaluation]期间使用. 比如, block 会创建一个新的词法环境[lexical environment], 该环境与 block 表达式形式[form]的求值时的那个相同, 只是在 block 的退出点[exit point]添加了 block 名称的绑定[binding].

特殊操作符[special operator]的名字[name]的集合在 Common Lisp 中是固定的; 没有给用户提供定义特殊操作符[special operator]的方法. 下面这段列出了所有被定义为特殊操作符[special operator]的 Common Lisp 符号[symbol].

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

###### 3.1.2.1.2.2 <span id = "MacroForms">宏表达式形式</span>

如果这个操作符[operator]命名了一个宏[macro], 它关联的宏函数[macro function]会被应用于整个表达式形式[form]并且将这个应用的结果替换原来的表达式形式[form].

具体来说, 如果对于一个符号[symbol]和一个给定的词法环境[lexical environment] macro-function 返回 true 那么在这个环境中这个符号[symbol]命名了一个宏[macro]. 这个 macro-function 返回的函数[function]是一个 2 个参数的函数[function], 称之为展开函数. 通过把这个展开函数作为第一个参数, 整个宏表达式形式[macro form]作为第二个参数, 以及一个环境对象[environment object] (对应当前词法环境[lexical environment])作为第三个参数调用宏展开钩子[macroexpand hook]来调用这个展开函数. 这个宏展开钩子[macroexpand hook]反过来调用这个展开函数, 将这个表达式形式[form]作为第一个参数并将环境对象[environment object]作为第二个参数. 这个传递给宏展开钩子[macroexpand hook]的展开函数的值[value]是一个表达式形式[form]. 返回的表达式形式[form]被求值来取代原来的表达式形式[form].

如果这个宏函数[macro function]破坏性地修改它的表达式形式[form]参数的任何部分, 那么后果是未定义的.

一个宏名字[macro name]不是一个函数标识符[function designator], 并且不能被用于例如 apply, funcall, 或者 map 这样的函数[function]的 function 参数.

一个具体实现[implementation]可以自由地将 Common Lisp 特殊操作符[special operator]实现为一个宏[macro]. 一个实现[implementation]也能自由地将任何宏[macro]操作符[operator]实现为一个特殊操作符[special operator], 但是只能在这个宏[macro]的一个等价定义被提供的情况下.

下面这段列出了一些可应用于宏[macro]的一些已定义名字[defined name].

    *macroexpand-hook*  macro-function  macroexpand-1  
    defmacro            macroexpand     macrolet       

Figure 3-3. 应用于宏的定义的名字

###### 3.1.2.1.2.3 <span id = "FunctionForms">函数表达式形式</span>

如果这个操作符[operator]是一个命名一个函数[function]的符号[symbol], 那么这个表达式形式[form]就表示一个函数表达式形式[function form], 并且这个列表的 cdr 部分包含了求值后作为参数传递给这个函数[function]的表达式形式[form].

当一个函数名字[function name]没有定义的时候, 应该会在运行时发出一个 undefined-function 类型[type]的错误; 见章节 3.2.2.3 (语义约束).

一个函数表达式形式[function form]会按如下所述被求值:

原始表达式形式[form]的 cdr 中的子表达式形式[subform]按照从左到右的顺序在词法和动态环境[environment]中被求值. 每次这样的求值[evaluation]的主值[primary value]作为参数传递给命名的函数[function]; 任何子表达式形式[subform]返回的额外的值[value]会被丢弃.

从词法环境[lexical environment]中检索操作符[operator]的函数值[functional value], 并使用指定的参数调用该函数[function].

虽然实参[argument]子表达式形式[subform]的求值[evaluation]顺序是严格的从左到右, 但是没有指定一个函数表达式形式[function form]中操作符[operator]的定义是在实参[argument]子表达式形式[subform]求值之前还是之后被查找, 或者不止一个实参[argument]子表达式形式[subform]时任意两个实参[argument]子表达式形式[subform]求值之间被查找. 比如, 下面可能返回 23 或者 24.

```LISP
(defun foo (x) (+ x 3))
(defun bar () (setf (symbol-function 'foo) #'(lambda (x) (+ x 4))))
(foo (progn (bar) 20))
```

一个函数名字[function name]的绑定[binding]可以通过几种方式的一种来建立. 一个全局环境[global environment]中的函数名[function name]的绑定[binding]可以通过 defun ,fdefinition 的 setf, symbol-function 的 setf, ensure-generic-function, defmethod (隐式的, 由于ensure-generic-function), 或者 defgeneric 来建立. 一个词法环境[lexical environment]中的函数名[function name]的绑定[binding]可以通过 flet 或者 labels 来建立.

下一段中列出了可应用于函数[function]的一些已定义的名字[defined name].

    apply                 fdefinition  mapcan               
    call-arguments-limit  flet         mapcar               
    complement            fmakunbound  mapcon               
    constantly            funcall      mapl                 
    defgeneric            function     maplist              
    defmethod             functionp    multiple-value-call  
    defun                 labels       reduce               
    fboundp               map          symbol-function      

Figure 3-4. 一些已定义的函数相关的名字 

###### 3.1.2.1.2.4 <span id = "LambdaForms">lambda 表达式形式</span>

一个 lambda 表达式形式[lambda form]类似于函数表达式[function form], 除了函数名[function name]被一个 lambda 表达式[lambda expression]替换.

一个 lambda 表达式形式[lambda form]等价于在给定的那些实参[argument]上使用一个 lambda 表达式[lambda expression]的词法闭包[lexical closure]的 funcall 调用. (在实践中, 比起已声明为 inline 的任意命名函数, 一些编译器更有可能为  lambda 表达式形式[lambda form]生成内联代码.)

关于更多信息, 见章节 3.1.3 (lambda 表达式). 

##### 3.1.2.1.3 <span id = "SelfEvaluatingObjects">自求值对象</span>

一个既不是一个符号[symbol]也不是一个 cons 的表达式形式[form]被定义为一个自求值对象[self-evaluating object]. 对这样的对象[object]进行求值, 会产生[yield]相同的[same]对象[object]来作为结果.

某些特定的符号[symbol]和 cons 偶尔也可能是"自求值", 但只是作为关于符号[symbol]和 cons 求值[evaluation]的一个更普遍的规则集合的特例; 这样的对象[object]不被认为是自求值对象[self-evaluating object].

如果字面[literal]对象[object] (包括自求值对象[self-evaluating object])被破坏性的修改, 结果是无法定义的.

###### 3.1.2.1.3.1 自求值对象的示例

数字[number], 路径名[pathname], 还有数组[array]都是自求值对象[self-evaluating object]的示例.

```LISP
3 =>  3
#c(2/3 5/8) =>  #C(2/3 5/8)
#p"S:[BILL]OTHELLO.TXT" =>  #P"S:[BILL]OTHELLO.TXT"
#(a b c) =>  #(A B C)
"fred smith" =>  "fred smith"
```

### 3.1.3 <span id = "LambdaExpressions">lambda 表达式</span>

在 lambda 表达式[lambda expression]中, 主体部分在一个词法环境[lexical environment]中求值, 这个词法环境[lexical environment]是通过添加 lambda 列表[lambda list]中的每个形参[parameter]和来自实参[argument]的对应值[value]的绑定[binding]到当前词法环境[environment]中来构成的.

有关如何基于 lambda 列表[lambda list]建立绑定[binding]的进一步讨论, 见章节 3.4 (Lambda 列表).

一个 lambda 表达式[lambda expression]的主体是一个隐式的 progn[implicit progn]; 它返回的值会被这个 lambda 表达式[lambda expression]返回. 

### 3.1.4 <span id = "ClosuresLexicalBinding">闭包和词法绑定</span>

一个词法闭包[lexical closure]是一个函数[function], 它可以引用和更改由包含函数定义的绑定[binding]表达式形式[form]所建立的词法绑定[lexical binding]的值.

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

这个 function 的特殊表达式形式[special form]将一个 lambda 表达式[lambda expression]强制转换为闭包[closure], 当对这个特殊表达式形式[special form]进行求值时, 事实上这个词法环境[lexical environment]与这个 lambda 表达式[lambda expression]一起被捕获到这个闭包[closure]中.

这个 two-funs 函数返回一个两函数[function]的列表[list], 它们中的每一个都引用了函数 two-funs 被调用时在入口创建的变量 x 的绑定[binding]. 这个变量有初始值 6, 但是 setq 可以修改这个绑定[binding]. 为第一个 lambda 表达式[lambda expression]创建的词法闭包[lexical closure]在创建这个闭包[closure]时不会"快照(snapshot)" x 的值[value] 6;而是捕捉了 x 的绑定. 第二个函数[function]可以被用于修改相同的(被捕获的)绑定[binding]中的值[value] (在示例中改为 43), 并且这个修改后的变量绑定接下来影响了第一个函数[function]的返回值.

在同一组绑定中, 一个 lambda 表达式[lambda expression]的闭包[closure]可能不止一次地产生的情况下, 产生的多个闭包[closure]可能或可能不相同[identical], 由具体实现来决定. 也就是说, 两个行为无法区分的函数[function]可能或可能不相同[identical]. 行为上可区分的两个函数[function]是截然不同的[distinct]. 示例:

```LISP
(let ((x 5) (funs '()))
  (dotimes (j 10)                          
    (push #'(lambda (z)                        
              (if (null z) (setq x 0) (+ x z)))
          funs))
  funs)
```

上面表达式形式[form]的结果是是一个十个闭包[closure]的列表[list]. 每一个都只需要 x 的绑定[binding]. 每个情况下都是相同的绑定[binding], 但是这十个闭包[closure]对象[object]可能或可能不相同[identical]. 另一方面, 下面这个表达式形式[form]的结果

```LISP
(let ((funs '()))     
  (dotimes (j 10)
    (let ((x 5))
      (push (function (lambda (z)
                      (if (null z) (setq x 0) (+ x z))))
            funs)))
funs)
```

也是十个闭包[closure]的列表[list]. 然而, 在这个情况下没有两个闭包[closure]对象[object]是相同的[identical]因为每一个闭包[closure]都封闭了一个不同的 x 的绑定[binding], 并且由于 setq 的使用这些绑定[binding]可以从行为上区分.

下面这个表达式形式[form]的结果

```LISP
(let ((funs '()))
  (dotimes (j 10)
    (let ((x 5))
      (push (function (lambda (z) (+ x z)))
          funs)))
  funs)
```

是一个可能或可能不相同[identical]的十个闭包[closure]对象[object]的列表[list]. 每一个闭包[closure]都涉及一个不同的 x 的绑定[binding], 但是这些绑定[binding]是不能区分的, 因为它们的值是相同的[same]且不可变 (这里没有在 x 上进行 setq 操作). 一个编译器可以在内部把这个表达式形式[form]转换成下面这种

```LISP
(let ((funs '()))
  (dotimes (j 10)
    (push (function (lambda (z) (+ 5 z)))
          funs))
funs)
```

其中的闭包[closure]可能是一样的.

一个闭包[closure]没有封闭任何变量绑定也是可能的. 在这个代码片段中

```LISP
(mapcar (function (lambda (x) (+ x 2))) y)
```

函数 (lambda (x) (+ x 2)) 没有包含任何外部对象的引用. 在这个情况下, 对于所有的 function 表达式形式[form]的求值, 可能返回相同的闭包[closure]. 

### 3.1.5 <span id = "Shadowing">遮蔽</span>

如果用相同的名字[name] N 建立[establish]词法绑定[lexical binding]的两个表达式形式[form]在文本上是嵌套的, 那么内部表达式形式[form]对 N 的引用会引用到内部表达式形式[form]建立的绑定[binding]; 这个内部 N 的绑定[binding]会遮蔽[shadow]外部 N 的绑定[binding]. 内部表达式形式[form]外面但是外部那个的里面对 N 的引用会引用到外部表达式形式[form]建立的绑定[binding]上. 比如:

```LISP
(defun test (x z)
(let ((z (* x 2)))
    (print z))
z)
```

由 let 创建的这个变量 z 的绑定[binding]遮蔽了函数 test 的形参[parameter]绑定. 在 print 表达式形式[form]中对变量 z 的引用会引用 let 绑定的. 在函数 test 的最后对 z 的引用会引用到名为 z 的形参[parameter]上.

具有词法作用域的构造就像在每次执行时为每个对象[object]生成新的名称一样. 因此, 动态遮蔽不会发生. 比如:

```LISP
(defun contorted-example (f g x)
  (if (= x 0)
      (funcall f)
      (block here
        (+ 5 (contorted-example g
                                #'(lambda () (return-from here 4))
                                (- x 1))))))
```

细想 (contorted-example nil nil 2) 这个调用. 这个产生 4. 在执行过程中, 这个有三个对 contorted-example 的调用, 带有 2 个 block 交错:

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

在执行 funcall 的时候, 有两个未偿还的 block 退出点[exit point], 每个都明显命名为 here. 这个被作为 funcall 操作的结果执行的 return-from 表达式形式[form]引用外面的未偿还的退出点[exit point] (here1), 不是内部的那个(here2). 它指的是在 function (这里通过 #' 语法缩写了)执行时文本上可见的退出点[exit point], 这个 function 导致了实际被 funcall 调用的函数[function]对象[object]的创建.

如果在这个例子中, 一个人打算把 (funcall f) 改为 (funcall g), 那么调用 (contorted-example nil nil 2) 的值会是 9. 这个值会改变是因为 funcall 会导致 (return-from here2 4) 的执行, 从而导致一个从内部的退出点[exit point] (here2)的返回. 当这个发生时, 值 4 会被中间的 contorted-example 调用返回, 5 被加到它上面成了 9, 并且这个值会被外部 block 和外部 contorted-example 调用返回. 重点是, 返回的退出点[exit point]的选择与它是最内部或最外层无关; 而是, 它取决于在执行 function 时和 lambda 表达式[lambda expression]一起打包的词法环境[lexical environment]. 

### 3.1.6 <span id = "Extent">范围</span>

这个 contorted-example 可以工作仅因为在退出点[exit point]的范围[extent]内调用了由 f 命名的函数[function]. 一旦这个执行的控制流离开了这个 block, 这个退出点[exit point]就会被废除. 比如:

```LISP
(defun invalid-example ()
  (let ((y (block here #'(lambda (z) (return-from here z)))))
    (if (numberp y) y (funcall y 5))))
```

您可能期望调用 (invalid-example) 根据以下不正确的推理生成 5: let 把 y 绑定到 block 的值; 这个值是一个由 lambda 表达式[lambda expression]产生的函数. 因为 y 不是一个数字, 所以它被在值 5 上被调用. 这个 return-from 应该从名为 here 的退出点[exit point]返回这个值, 从而再一次退出 block 并且把值 5 给 y, 成为一个数字, 然后把这个值作为调用 invalid-example 的结果返回.

这个论证失败仅因为退出点[exit point]有动态范围[dynamic extent]. 这个论证直到 return-from 的执行还是正确的. 这个 return-from 的执行应该会发出一个 control-error 类型[type]的错误, 然而, 不是因为它不能引用到那个退出点[exit point]而发出, 而是因为它正确地引用到那个退出点[exit point]而那个退出点[exit point]已经被废弃.

一个根据名字对动态退出点[exit point]绑定的引用, 如 catch 标记[catch tag], 引用的是最近已建立的还没有被废弃的该名称的绑定[binding]. 比如:

```LISP
(defun fun1 (x)
  (catch 'trap (+ 3 (fun2 x))))
(defun fun2 (y)
  (catch 'trap (* 5 (fun3 y))))
(defun fun3 (z)
  (throw 'trap z))
```

细想这个调用 (fun1 7). 结果是 10. 在那个 throw 被执行的时候, 这里有两个未偿还的带有名字 trap 的捕捉者(catcher): 一个在程序 fun1 中建立, 并且另外一个在程序 fun2 中建立. 后面那个更接近, 所以这个值 7 是从 fun2 的 catch 中被返回. 从 fun3 中来看, 在 fun2 中的那个 catch 遮蔽了在 fun1 中的那个. fun2 被定义为

```LISP
(defun fun2 (y)
  (catch 'snare (* 5 (fun3 y))))
```

那么两个退出点[exit point]会有不同的名字[name], 因此 fun1 的那个不会被遮蔽. 这个结果会是 7. 

### 3.1.7 <span id = "ReturnValues">返回值</span>

通常调用函数[function]的结果是一个单独的对象[object]. 然而, 有时候, 一个函数可以很方便地计算多个对象[object]并返回它们.

为了从一个表达式形式[form]接受一个以上的返回值, 几个特殊表达式形式[special form]或宏[macro]中的一个必须被用于请求那些值. 如果一个表达式形式[form]产生了多值[multiple value]但是没有以这种方式接收, 那么第一个返回值会被给到调用者并且其他的会被丢弃; 如果这个表达式形式[form]产生零个值, 那么调用者会收到一个 nil 作为值.

下面这段列出了接收多值[multiple value]的操作符[operator]. 这些操作符[operator]可以被用于指定一个或多个表达式形式[form]去求值[evaluate]以及指定这些表达式形式[form]返回的那些值[value]放置到哪里.

    multiple-value-bind  multiple-value-prog1  return-from  
    multiple-value-call  multiple-value-setq   throw        
    multiple-value-list  return                             

Figure 3-5. 一些可应用于接收多值的操作符

函数[function] values 可以被用于产生多值[multiple values[2]]. (values) 返回 0 个值; (values form) 返回 form 返回的主值[primary value]; (values form1 form2) 返回两个值, form1 的主值[primary value]和 form2 的主值[primary value]; 以此类推.

见 multiple-values-limit 和 values-list. 

## 3.2 <span id = "Compilation">编译</span>

> * 3.2.1 [编译器术语](#CompilerTerminology)
> * 3.2.2 [编译语义](#CompilationSemantics)
> * 3.2.3 [文件编译](#FileCompilation)
> * 3.2.4 [编译后文件中的字面化对象](#LiteralObjectsInCompiledFiles)
> * 3.2.5 [编译器中的异常情况](#ExceptionalSituationsCompiler)

### 3.2.1 <span id = "CompilerTerminology">编译器术语</span>

以下术语被用于这个章节.

编译器[compiler]是一个将代码转换为一个具体实现相关的[implementation-dependent]可以被有效地表示和执行的形式的工具. 术语编译器[compiler]指的是 compile 和 compile-file 这两个函数[function].

术语编译后的代码[compiled code]指的是表示编译后程序的对象[object], 例如加载一个编译后文件[compiled file]时被 compile 或者 load 构造的对象[object].

术语隐式编译[implicit compilation]指的是在求值[evaluation]期间执行的编译[compilation].

术语字面化[literal]对象[object]指的是一个引用的对象[object]或者一个自求值对象[self-evaluating object]或者这样一个对象的底层结构对象<!--TODO 待校对-->. 一个常变量[constant variable]自身不是一个字面化[literal]对象[object].

术语合并[coalesce]按如下定义. 假定 A 和 B 是源代码[source code]中的两个字面化[literal]常量[constant], 并且 A' 和 B' 是编译后代码[compiled code]中对应的对象[object]. 如果 A' 和 B' 是 eql 但是 A 和 B 不是 eql, 那么就说 A 和 B 被编译器所合并.

术语最小编译[minimal compilation]指的是编译器必须在编译期[compile time]执行的动作. 这些动作声明在章节 3.2.2 (编译语义).

动词处理[process]指的是执行最小编译[minimal compilation], 确定一个表达式形式[form]的求值时间, 并可能求值该表达式形式[form] (如果需要的话).

术语进一步编译[further compilation]指的是超出最小编译[minimal compilation]的依赖于具体实现的[ implementation-dependent]的编译. 这就意味着, 处理并不意味着完整编译. 块编译和机器特定指令的生成是进一步编译的示例. 进一步编译允许发生在运行时[run time].

区分与编译相关的 4 个不同的环境[environment]: 启动环境[startup environment], 编译环境[compilation environment], 求值环境[evaluation environment], 还有运行时环境[run-time environment].

启动环境[startup environment]是编译器被调用的那个 Lisp 镜像[Lisp image]的环境[environment].

编译环境[compilation environment]由编译器维护, 并用于保存编译器内部使用的定义和声明. 只有正确编译所需的定义部分才会被保存. 编译环境[compilation environment]用作编译器调用的宏展开函数的环境[environment]实参[argument]. 在编译环境[compilation environment]中可用的定义是否可以用于启动环境[startup environment]或求值环境[evaluation environment]中所启动的求值[evaluation]中, 这是未知的.

求值环境[evaluation environment]是一个运行时环境[run-time environment], 在这个环境中, 由 eval-when 指定的要被求值的代码和宏展开会被求值. 由编译器[compiler]发起的所有求值都在求值环境[evaluation environment]中进行.

运行时环境[run-time environment]是被编译后的的程序将被执行的环境[environment].

编译环境[compilation environment]从求值环境[evaluation environment]继承而来, 并且编译环境[compilation environment]和求值环境[evaluation environment]可能是相同的[identical]. 这个求值环境[evaluation environment]从启动环境[startup environment]中继承而来, 并且这个启动环境[startup environment]和求值环境[evaluation environment]可能是一样的[identical].

术语编译时[compile time]指的是编译器处理源代码[source code]的那段时间. 在编译期[compile time], 只有编译环境[compilation environment]和求值环境[evaluation environment]可用.

术语编译期定义[compile-time definition]指的是编译环境[compilation environment]中的定义. 比如, 编译一个文件时, 一个函数如果声明为 inline, 那么它的定义可能被保留在编译环境[compilation environment]中. 这个定义可能在求值环境[evaluation environment]中不可用.

术语运行时[run time]指的是加载器加载编译后的代码或编译后的代码被执行的那段时间. 在运行时, 只有运行时环境[run-time environment]可用.

术语运行时定义[run-time definition]指的是运行时环境[run-time environment]中的一个定义.

术语运行时编译器[run-time compiler]指的是 compile 函数[function]或者隐式编译[implicit compilation], 对于它们来说编译和运行时环境[environment]是在同一个 Lisp 镜像[Lisp image]中. 注意当运行时编译器[[run-time compiler]]被使用时, 运行时环境[run-time environment]和启动环境[startup environment]是一样的. 

### 3.2.2 <span id = "CompilationSemantics">编译语义</span>

从概念上讲, 编译是一个遍历代码, 使用在编译环境[compilation environment]中提供的信息(如全局声明和宏[macro]定义)来执行特定的语法和语义分析, 并生成等价的, 可能更有效的代码的过程.

> * 3.2.2.1 [编译器宏](#CompilerMacros)
> * 3.2.2.2 [最小化编译](#MinimalCompilation)
> * 3.2.2.3 [语义约束](#SemanticConstraints)

#### 3.2.2.1 <span id = "CompilerMacros">编译器宏</span>

编译器宏[compiler macro]可以定义给一个同时也命名一个函数[function]或宏[macro]的名称[name]. 这也就是说, 一个函数名[function name]可能同时命名函数[function]和编译器宏[compiler macro].

如果 compiler-macro-function 对于出现在词法环境[lexical environment]中的一个函数名[function name]返回 true, 这个函数名[function name]命名一个编译器宏[compiler macro]. 为这个函数名[function name]创建一个词法绑定[lexical binding]不止创建一个局部函数[function]或宏[macro]定义, 并且也遮蔽[shadows[2]]了这个编译器宏[compiler macro].

这个 compiler-macro-function 返回的函数[function]是一个两个参数的函数[function], 称为展开函数(expansion function). 为了展开一个编译器宏[compiler macro], 通过用这个展开函数作为第一个参数, 整个编译器宏表达式形式[form]作为第二个参数, 当前编译环境(或者如果这个表达式形式[form]被 compile-file 以外的其他处理过, 那么就是当前的词法环境[environment])作为第三个参数来调用宏展开钩子[macroexpand hook]进而调用这个展开函数. 宏展开钩子[macroexpand hook]反过来将这个表达式形式[form]作为第一个参数而环境[environment]作为第二个参数来调用这个展开函数. 展开函数的返回值, 由宏展开钩子函数传递, 可能是相同的[same]表达式形式[form], 或者是另一种表达式形式[form], 由执行展开的代码[code]决定, 被用于替换到原始表达式形式[form].

    *macroexpand-hook*  compiler-macro-function  define-compiler-macro  

Figure 3-6. 应用于编译器宏的定义的名字

##### 3.2.2.1.1 编译器宏的目的

这个编译器宏[compiler macro]机制的目的是允许选择性的源代码转换为编译器的优化建议. 当一种复合表达式形式[compound form]被处理(如由编译器), 如果这个操作符[operator]命名了一个编译器宏[compiler macro], 那么这个编译器宏函数[compiler macro function]可能在这个表达式形式[form]上被调用, 而产生的展开进行递归处理优先于在原始表达式形式[form]上根据它作为函数表达式形式[function form]或宏表达式形式[macro form]的解释执行常规处理.

一个编译器宏函数[compiler macro function], 就像一个宏函数[macro function], 是一个两个实参[argument]的函数[function]: 整个调用的表达式形式[form]和那个环境[environment]. 不像一个普通的宏函数[macro function], 一个编译器宏函数[compiler macro function]可以通过只返回与原始表达式形式[form]相同的值来拒绝提供展开式. 如果编译器宏函数[compiler macro function]破坏性地修改的它的表达式形式[form]参数的任何部分, 那么后果是未定义的.

传递给编译器宏函数[compiler macro function]的表达式形式[form]可以是一个 car 部分为一个函数名的列表[list]或者一个 car 部分是 funcall 并且 cadr 部分是一个列表 (function name) 的列表; 注意, 这会影响编译器宏函数[compiler macro function]对表达式形式参数的解构. define-compiler-macro 会为两种可能的格式的参数正确解构做准备.

当 compile-file 选择展开一个编译器宏[compiler macro]表达式形式[form]的顶层表达式形式[top level form]时, 那么展开式出于 eval-when 处理的目的也被当作顶层表达式形式[top level form]来处理; 见章节 3.2.3.1 (顶层表达式形式的处理). 

##### 3.2.2.1.2 编译器宏的命名

编译器宏[compiler macro]可以被定义为命名了函数[function]和宏[macro]的函数名[function name].

编译器宏[compiler macro]定义是严格的全局定义. 没有提供 macrolet 定义局部宏[macro]的方式来定义局部编译器宏[compiler macro]. 一个函数名字的词法绑定会遮蔽任何和这个名字关联的编译器宏[compiler macro], 还有和这个名字关联的全局的函数[function]和宏[macro]定义.

注意, 编译器宏定义的存在不会影响访问函数定义(比如, fboundp)或宏定义(比如, macroexpand)的函数返回的值. 编译器宏是全局的, 并且函数 compiler-macro-function 足以解决它们与其他词法和全局定义之间的交互作用. 

##### 3.2.2.1.3 编译器宏被使用的时机

一个函数[function]或宏[macro]的编译器宏[compiler macro]定义的存在表明, 编译器应该使用编译器宏[compiler macro]的展开, 而不是原来的函数表达式形式[function form]或宏表达式形式[macro form]. 然而, 任何语言处理器(编译器, 求值器, 或者其他的 code walker)都不需要实际调用编译器宏函数[compiler macro function], 或者如果调用编译器宏函数[compiler macro function]也不使用产生的展开.

当编译器[compiler]在处理时遇到一个对编译器宏[compiler macro]名称[name]调用的表达式形式[form] (这个没有被声明为 notinline)时, 编译器[compiler]可能会展开编译器宏[compiler macro], 并可能使用展开式代替原始表达式形式[form].

当 eval 在处理时遇到一个对编译器宏[compiler macro]名称[name]调用的表达式形式[form] (这个没有被声明为 notinline)时, eval 可能会展开编译器宏[compiler macro], 并可能使用展开式代替原始表达式形式[form].

这里有两个编译器宏[compiler macro]定义一定不能被任何语言处理器所使用的情况:

    与编译器宏关联的全局函数名称绑定被那个函数名的词法绑定所遮蔽.

    函数名已被全局声明或局部声明为 notinline, 而调用表达式形式出现在声明的作用域内.

在其他情况下, 编译器宏[compiler macro]是否被展开或使用是未知的.

###### 3.2.2.1.3.1 关于编译器宏实现的注意事项

如上所述, 虽然从技术上对于 eval 在与编译器[compiler]相同的情况下处理编译器宏[compiler macro]是允许的, 但是这在解释型具体实现[interpreted implementation]中并不一定是个好主意.

编译器宏[compiler macro]的存在是为了用编译时速度换取运行时速度的目的. 编写编译器宏[compiler macro]的程序员倾向于假设编译器宏[compiler macro]比正常函数[function]和宏[macro]花费更多的时间进而来生成用于运行时的最佳的代码. 由于 eval 在解释型具体实现[interpreted implementation]中可能多次执行相同表达式形式的语义分析, 因此具体实现[implementation]在每次这样的求值[evaluation]中选择调用编译器宏[compiler macro]可能会很低效.

然而, 关于在这些情况下应该做什么的决定留给每个具体实现[implementation]. 

#### 3.2.2.2 <span id = "MinimalCompilation">最小化编译</span>

最小化编译[minimal compilation]根据如下定义:

    在编译时, 正在编译的源代码[source code]中出现的所有编译器宏[compiler macro]调用都被展开; 它们不会在运行时展开.

    正在编译的源代码中出现的所有宏[macro]和符号宏[symbol macro]调用都在编译时进行展开, 这样它们就不会在运行时再次被展开. macrolet 和 symbol-macrolet 实际上被替换为与它们的主体相对应的表达式，在这里，对宏[macro]的调用被它们的展开式所取代.

    在被 compile 处理的源代码[source code]中一个 load-time-value 表达式形式[form]的第一个实参[argument]在编译期[compile time]被求值; 在被 compile-file 处理的源代码[source code]中, 编译器安排它在加载时[load time]被求值. 不论发生何种情况, 这个求值[evaluation]的结果会被记住并且在执行时[execution time]被用于后面 load-time-value 表达式形式[form]的值. 

#### 3.2.2.3 <span id = "SemanticConstraints">语义约束</span>

所有符合规范的程序[conforming program]必须遵守以下约束, 这些被设计用于减少编译和解释的程序的可观测差异:

  * 任何被引用的宏[macro]的定义必须出现在编译环境[compilation environment]中. 任何以一个不是命名编译环境[compilation environment]中定义的特殊操作符[special operator]或宏[macro]的符号[symbol]开始的列表[list]表达式形式[form]会被编译器当作一个函数调用.

  * 必须在编译环境[compilation environment]中对动态变量[dynamic variable]进行 special 全局声明. 在编译环境[compilation environment]中没有 special 声明或全局声明的任何绑定[binding]都被编译器视为词法绑定[lexical binding].

  * 在编译环境[compilation environment]中定义和声明为 inline 的函数的定义在运行时必须是相同的.

  * 在一个名为 F 的函数[function]中, 这个编译器可能 (但不是必须) 假定一个对名为 F 的函数的明显递归调用指向 F 的相同定义, 除非这个函数已经被声明为 notinline. 在函数[function] F 执行时, 重定义这样一个递归定义函数[function] F 的后果是没有定义的.

  * 在一个文件中对一个定义在相同文件中的已命名函数的调用指的就是那个函数, 除非该函数被声明为 notinline. 如果函数在运行时被单独重定义或者在一个相同文件里多次定义, 那么后果是未指定的.

  * 所有在编译时声明 ftype 的函数的参数语法和返回值数量必须在运行时保持不变.

  * 在编译环境[compilation environment]中定义的常变量[constant variable]在运行时必须具有相似[similar]的值. 源代码[source code]中对一个常变量[constant variable]的引用等价于对一个常变量[constant variable]值[value]的字面化[literal]对象[object]的引用.

  * 用 deftype 或者 defstruct 在编译环境[compilation environment]中所做的类型定义必须在运行时保持相同的定义. 由 defclass 在编译环境[compilation environment]中定义的类必须在运行时被定义有着相同的超类[superclasse]和元类[metaclass].

    这个也就意味着类型指定符[type specifier]的子类型[subtype]/超类型[supertype]关系必须在编译期[compile time]和运行期[run time]保持不变.

  * 在编译环境[environment]中出现的类型声明必须准确地描述在运行时的对应值; 否则, 后果就是未定义的. 在编译时, 允许一个未知类型[type]出现在一个声明中, 但是在这种情况下可能会发出一个警告.

  * 除了上面显式列出的情况外, 在求值环境[evaluation environment]中定义的一个函数[function]允许在运行时具有不同的定义或不同的签名, 并且运行时定义生效.

不应该使用额外的任何关于运行时环境[environment]与启动、求值和编译环境[environment]之间一致性的假设来编写符合规范的程序[conforming programs].

除非另行注明, 当一个编译期定义和运行时不同, 会在运行时出现以下情况的一种:

  * 发出一个 error 类型[type]的错误
  * 那个编译器定义生效
  * 那个运行时定义生效

如果编译器[compiler]处理一个在编译时没有定义操作符[operator]的函数表达式形式[function form], 那么编译时就不会发出错误. 

### 3.2.3 <span id = "FileCompilation">文件编译</span>

函数[function] compile-file 对一个文件中的表达式形式[form]根据章节 3.2.2 (Compilation Semantics) 中的规则执行编译, 并且产生一个可以通过 load 被载入的输出文件.

通常情况下, 在 compile-file 编译的文件中出现的顶层表达式形式[top level form]只在加载编译后文件时才进行求值, 而不是在编译文件时进行求值. 但是, 通常情况下, 文件中的某些表达式需要在编译时进行求值，以便能够正确地读取和编译文件的其余部分.

这个 eval-when 特殊表达式[special form]可以被用于控制一个顶层表达式形式[top level form]是否在编译时, 加载时, 或者两个时机都求值. 用 eval-when 指定这三种情况的任何几个都是可以的, 通过符号 :compile-toplevel, :load-toplevel, 还有 :execute 来表示. 对于顶层的 eval-when 表达式形式, :compile-toplevel 表示编译器必须在编译期求值这个主体部分, 而 :load-toplevel 表示这个编译器必须安排在加载时求值这个主体. 关于非顶层的 eval-when 表达式, :execute 表示这个主体必须在运行时环境[environment]被执行.

这种表达式形式[form]的行为可以从 compile-file 如何处理一个要被编译的文件中的表达式形式的模型的方面来更精确地理解. 这个有两种处理模式, 称之为 "not-compile-time" 还有 "compile-time-too".

文件中的连续表达式形式被 compile-file 读取并且在 not-compile-time 模式下被处理; 在这个模式下, compile-file 安排表达式形式只在加载时被求值而不是在编译时. 当 compile-file 在 compile-time-too 模式下时, 表达式形式在加载和编译时都求值.

#### 3.2.3.1 顶层表达式形式的处理

在文件编译器中对顶层表达式形式[top level form]的处理定义如下:

* 如果这个表达式形式[form]是一个编译器宏表达式形式[compiler macro form] (没有被一个 notinline 声明[declaration]所禁用), 具体实现[implementation]可能或可能不选择计算该表达式形式[form]的编译器宏展开式[ compiler macro expansion], 并且执行那个展开式, 可能也可能不会选择在相同的处理模式(compile-time-too 或 not-compile-time)下将结果作为顶层表达式形式[top level form]进行处理. 如果它拒绝获取或使用展开式, 它必须处理原始的表达式[form].

* 如果这个表达式形式是一个宏表达式形式[macro form], 它的宏展开会在相同的处理模式下(compile-time-too 或 not-compile-time)按照顶层表达式形式[top level form]被计算并处理.

* 如果表达式形式是一个 progn 表达式形式, 它的主体表达式形式[form]中的每一个会被依次当作顶层表达式形式[top level form]在相同的处理模式下处理.

* 如果这个表达式形式是一个 locally, macrolet, 或者 symbol-macrolet, 那么 compile-file 建立适当的绑定并实际上用这些绑定在相同的处理模式下按照顶层表达式形式[top level form]处理主体表达式形式. (注意, 这意味着被处理的顶层表达式形式的词法环境[environment]没有必要是空词法环境[null lexical environment].)

* 如果表达式形式是一个 eval-when 表达式形式, 它会根据下面这段被处理.

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

    Process: 在指定模式下按照顶层表达式形式[top level form]处理主体.

    Evaluate: 在编译器的动态执行上下文中对主体进行求值, 使用求值环境[evaluation environment]作为全局环境以及 eval-when 出现的词法环境[lexical environment].

    Discard: 忽略这个表达式形式[form].

    这个 New Mode 列表示新的处理模式. 一个代字符 (---) 表示编译器保留当前模式.

* 否则, 这个表达式形式就是一个不是这些特殊情况之一的顶层表达式形式[top level form]. 在 compile-time-too 模式下, 这个编译器首先在求值环境[environment]中对表达式形式进行求值, 然后对其进行最低限度的编译. 在 not-compile-time 模式下, 表达式形式[form]只是简单地最低限度地编译了. 所有子表达式形式[subform]被当作非顶层表达式形式[non-top-level form]. 

    注意, 顶层表达式形式[top level form]是按照在文件中显示的顺序处理的, 并且编译器读取的每个顶层表达式形式[top level form]都在读取下一个之前进行处理. 然而, 只要 Common Lisp 的语义被保留, 不是顶层表达式形式[top level form]的子表达式[subform]的处理(包括宏展开的)顺序还有进一步编译的顺序是未指定的.

eval-when 表达式形式[form]导致的编译期求值仅限顶层. 对于非顶层表达式形式[non-top-level form] :compile-toplevel 和 :load-toplevel 情况声明都会被忽略. 对于非顶层表达式形式[non-top-level form], 一个指定了 :execute 情况的 eval-when 被当作一个包括 eval-when 表达式形式[form]的主体部分的隐式 progn[implicit progn ]; 否则, 在主体中的表达式形式[form]就被忽略.

##### 3.2.3.1.1 定义宏的处理

出现在一个要被 compile-file 处理的文件中的那些定义宏[macro] (就像 defmacro 或者 defvar), 通常会有编译期副作用, 影响同一个文件[file]中后续表达式形式[form]的编译. 解释这些副作用是如何发生的一个很方便的模型是, 定义宏被展开为一个或多个 eval-when 表达式形式[form], 然后导致编译期副作用发生的这些调用会出现在一个 (eval-when (:compile-toplevel) ...) 表达式形式[form]的主体部分.

编译期的副作用可能会导致对定义的信息的存储方式不同于以"正常"方式处理定义宏 (要么是解释, 要么是加载已编译的文件).

具体来说, 在编译期由定义宏[macro]所存储的信息可能或可能无法用于解释器 (不管是编译时或是编译后), 或者是后续对编译器[compiler]的调用中. 比如, 下面这段代码是不可移植的因为它假定在对于解释器可用的地方编译器[compiler]也存储了 foo 宏定义:

```LISP
(defmacro foo (x) `(car ,x))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (print (foo '(a b c))))
```

一种完成相同工作的可移植的方式是把宏定义包含在 eval-when 表达式形式[form]里, 就像:

```LISP
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro foo (x) `(car ,x))
  (print (foo '(a b c))))
```

下面这段列出了使得定义可以在编译时和运行时环境[environment]都可用的宏. 既没有指定在编译环境[compilation environment]中可用的定义是否在求值环境[environment]中可用, 也没有指定它们在后续编译单元中或者在编译器的后续调用中是否可用. 与 eval-when 一样, 这些编译期副作用只发生在定义宏出现在顶层时.

    declaim                define-modify-macro   defsetf    
    defclass               define-setf-expander  defstruct  
    defconstant            defmacro              deftype    
    define-compiler-macro  defpackage            defvar     
    define-condition       defparameter                     

Figure 3-8. 影响编译期环境的定义宏

##### 3.2.3.1.2 对宏和编译器宏的限制

除了在显式声明之外, 在 Common Lisp 标准中定义的宏都不会产生导致这个宏表达式形式[macro form]的任何子表达式形式[subform]被当作顶层表达式形式[top level form]来处理的展开式. 如果一个具体实现[implementation]还提供了一个 Common Lisp 宏[macro]的特殊操作符[special operator]定义, 那么这个特殊操作符[special operator]定义在这方面必须是语义上等价的.

编译器宏[compiler macro]展开式也必须具有与它们所取代的表达式形式[form]相同的顶层求值语义. 这对符合规范的实现[conforming implementation]和符合规范的程序[conforming program]都需要去关注的. 

### 3.2.4 <span id = "LiteralObjectsInCompiledFiles">编译后文件中的字面化对象</span>

函数 eval 和 compile 需要去确保引用的字面化[literal]对象[object]在产生的解释后或编译后的代码对象和源代码[source code]中的对应对象[object]是相同的[same]. 从另一方面说, compile-file 必须产生一个编译后的文件[compiled file], 它被 load 加载时, 构建源代码[source code]中定义的对象并且产生指向它们的引用.

在 compile-file 的情况下, 编译后的文件[compiled file]被 load 加载所构建的对象[object]不能被认为与编译时构造的对象[object]相同[same], 因为编译后的文件[compiled file]可能被加载到不同的 Lisp 镜像[Lisp image]中, 而不是它编译时所在的那个. 这个部分定义了相似性[similarity]的概念, 它将求值环境[evaluation environment]中的对象[object]与运行时环境[run-time environment]中的相应对象[object]关联起来.

在这个章节中描述的字面化[literal]对象[object]上的约束只能应用于 compile-file; eval 和 compile 不会拷贝或合并常量.

> * 3.2.4.1 [可外部化的对象](#ExternalizableObjects)
> * 3.2.4.2 [字面化对象的相似性](#SimilarityLiteralObjects)
> * 3.2.4.3 [相似性规则的扩展](#ExtensionsSimilarityRules)
> * 3.2.4.4 [外部化对象的附加约束](#ACEO)

#### 3.2.4.1 <span id = "ExternalizableObjects">可外部化的对象</span>

事实上文件编译器[file compiler]在编译后的文件[compiled file]中外部表示字面化[literal]对象[object]并且在文件[file]被加载时重构这些对象[object]的适合的等价对象, 这也意味着就需要去约束被文件编译器[file compiler]处理的代码[code]中的可以被用作字面化[literal]对象[object]的那些对象[object]的特性.

一个可以被用作要被文件编译器[file compiler]处理的代码[code]中的字面化[literal]对象[object]的对象[object]被称为可外部化对象[externalizable object].

如果两个对象[object]满足一个二位的概念上等价断言(下面定义的), 我们就定义它们是相似的[similar], 这个断言不依赖于 Lisp 镜像[Lisp image], 所以两个在不同 Lisp 镜像[Lisp image]中的对象[object]可以在这个断言下理解为等价的. 更进一步, 通过检查这个概念性断言的定义, 程序员可以预测一个对象[object]的哪些方面确实被文件[file]编译[compilation]保留了.

文件编译器[file compiler]必须与加载器[loader]合作, 以确保可外部化的对象[externalizable object]作为一个字面化[literal]对象[object]处理的每个情况中, 加载器[loader]会构造一个类似的[similar]对象[object].

新的概念术语"相似[similar]"就是为了可外部化对象[externalizable object]的对象[object]集合而定义, 这样一来当编译后的文件[compiled file]被加载时, 一个和文件编译器[file compiler]操作时存在的原始对象[object]相似的对象[object]会被构建. 

#### 3.2.4.2 <span id = "SimilarityLiteralObjects">字面化对象的相似性</span>

##### 3.2.4.2.1 聚合对象的相似性

在已定义相似性[similarity]之外的类型[type]中, 一些被当作是聚合对象. 对于这些类型, 相似性[similarity]是递归定义的. 我们说这些类型的对象有着确定的"基本特性(basic qualities)"并且为了满足相似性[similarity]关系, 两个对象[object]的对应特性的值也必须是相似的.

##### 3.2.4.2.2 相似性的定义

两个对象[object] S (在源代码[source code]中) 和 C (在编译后的代码[compiled code]中) 仅当它们同时都是这里列出的类型[type] (或者具体实现[implementation]定义的)之一并且它们都满足该类型[type]的相似性[similarity]的所有额外需求, 那么它们就被定义为相似的[similar].

number

    两个数字[number] S 和 C 如果它们是相同的类型[type]并且表示相同的数学上的值, 那么它们就是相似的[similar].

character

    如果两个简单[simple]字符[character] S 和 C 有相似的[similar]字符码[code]属性[attribute], 那么它们就是相似的[similar].

    具体实现[implementation]假定额外的, 依赖于具体实现的[implementation-defined]属性[attribute]必须定义非简单[non-simple]字符[character]是否可以被当作相似的[similar]并且如何被当作相似的[similar].

symbol

    如果两个显而易见的未捕捉[apparently uninterned]符号[sumbol] S 和 C 的名字[name]是相似的[similar], 那么它们就是相似的[similar].

    如果两个被捕捉的[interned]符号 S 和 C 名字[name]是相似的[similar]并且编译时 S 可以在当前包[current package]是访问的而加载时 C 可以在当前包[current package]也是可访问的, 或者 C 在 S home 包[home package]的相似的[similar]包[package]中是可访问, 那么它们就是相似的[similar].

    (注意, 符号[symbol]的相似性[similarity]不依赖于当前的读取表[current readtable], 也不依赖函数[function] read 如何来解析符号[symbol]名称[name]中的字符[character].)

package

    如果两个包[package] S 和 C 的名字[name]是相似的[similar], 那么它们就是相似的[similar].

    注意, 虽然一个包[package]对象[object]是一个可外部化的对象[externalizable object], 但是当代码将其作为一个字面化[literal]对象[object]引用时, 程序员有责任确保相应的包[package]已经存在. 加载器[loader]就像是通过使用该名称[name]作为实参[argument]调用的 find-package 来查找相应的包[package]对象[object]. 如果加载时包[package]不存在, 则加载器[loader]会发出一个错误.

random-state

    如果两个随机状态[random state] S 和 C 中, 假定每种情况下 limit 实参[argument]等价, 当给定作为函数[function] random 的 random-state 实参[argument]时, S 总是产生和 C 的拷贝[copy[5]]相同的伪随机数序列, 那么它们就是相似的[similar].

    (注意, 因为 C 已经被文件编译器[file compiler]处理过了, 它不能被直接用于 random 的实参[argument]因为 random 会有副作用.)

cons

    两个 cons S 和 C 如果它们的 car 和 cdr 部分分别都是相似的[similar], 那么它们就是相似的[similar].

array

    如果两个一维的数组[array] S 和 C 的长度[length]以及实际数组元素类型[actual array element type]还有每一个对应可用的[active]元素[element]也是相似的[similar], 那么它们就是相似的[similar].

    两个一维[rank]以外的数组 S 和 C 如果它们的维度[rank]是相似的[similar], 每一个对应维度的规模[dimension[1]]也是相似的[similar], 每一个对应实际数组元素类型[actual array element type]也是相似的[similar], 并且每一个对应的元素[element]也是相似的[similar], 那么它们就是相似的[similar].

    另外, 如果 S 是一个简单数组[simple array], 那么 C 也必须是一个简单数组[ simple array]. 如果 S 是一个带有一个填充指针[fill pointer]或者实际可调整[actually adjustable]的偏移数组[displaced array], 那么 C 被允许缺乏任何或所有这些特性.

hash-table

    如果两个哈希表[hash table] S 和 C 满足下面三条需求, 那么它们就是相似的[similar]:

        它们都有相同的 test 部分(比如, 它们都是 eql 哈希表[hash table]).

        两个哈希表[hash table]的键之间存在唯一的一对一匹配, 这样一来对应的键就是相似的[similar].

        对于所有的键, 与两个对应键关联的值是相似的[similar].

    如果在 S 和 C 中的键之间不止一个一对一对应, 结果是未指定的. 一个符合规范的程序[conforming program]不能使用像 S 这个的表作为可外部化常量.

pathname

    如果两个路径名[pathname] S 和 C 对应的所有路径名[pathname]成分是相似的[similar], 那么它们就是相似的[similar].

function

    函数[function]不是可外部化对象[externalizable object].

structure-object 和 standard-object

    对于结构体[structure]和标准对象[standard object]不存在相似性[similarity]的通用概念. 然而, 一个符合规范的程序[conforming program]允许去为这个程序[program]所定义的 structure-object 或 standard-object 的子类[subclass] K 定义一个 make-load-form 方法[method]. 这样一个方法[method]的效果就是定义当源代码[source code]中类型[type] K 的对象[object] S 和编译后的代码[compiled code]中类型[type] K 的对象[object] C 是通过对 S 调用 make-load-form 的代码[code]构造出来的时, S 和 C 就是相似的[similar]. 

#### 3.2.4.3 <span id = "ExtensionsSimilarityRules">相似性规则的扩展</span>

在上面给定的相似性定义下, 一些对象[object], 例如流[stream], 读取表还有方法不是可外部化的对象[externalizable object]. 这就是说, 这些对象[object]可能不能像要被文件编译器[file compiler]处理的代码[code]中的字面化[literal]对象[object]那样可移植地出现.

一个具体实现[implementation]允许去扩展相似性规则, 这样一来其他种类的对象[object]对于这个具体实现[implementation]也是可外部化对象[externalizable object].

如果对于某个类型的对象[object], 相似性[similarity]既没有被这个说明文档也没有被具体实现[implementation]所定义, 那么文件编译器[file compiler]在遇到这样一个对象[object]作为字面化[literal]常量[constant]时会发出一个错误. 

#### 3.2.4.4 <span id = "ACEO">外部化对象的附加约束</span>

如果在文件编译器[file compiler]处理的单个文件源代码中出现的两个字面化[literal]对象[object]是相同的[identical], 那么编译后的代码[compiled code]中相应的对象[object]也必须是相同的[identical]. 除了符号[symbol]和包[package], 文件编译器[file compiler]处理的代码中的任何两个字面化[literal]对象[object]仅当它们是相似的[similar]时候可以被合并[coalesce]; 如果它们两个都是符号[symbol]或者包[package], 它们可能只有在它们是相同的[identical]时候才会被合并[coalesced].

包含循环引用的对象[object]可以是可外部化对象[externalizable object]. 文件编译器[file compiler]需要在一个文件[file]中保存子结构的相等性(eqlness). 保留相等性(eqlness)意味着在源代码[source code]中相同的[same]子对象在相应的编译后代码[compiled code]中必须是相同的[same].

另外, 下面是文件编译器[file compiler]对字面化[literal]对象[object]处理上的约束:

  **array**: 如果在源代码中的一个数组[array]是一个简单数组[simple array], 那么在编译后代码中的对应数组[array]也是一个简单数组[simple array]. 如果一个数组在源代码中是偏移的, 带有一个填充指针[fill pointer]或者一个实际可调整的[actually adjustable], 那么编译后代码中的对应数组[array]可能缺少任何或者所有这些特性. 如果一个源代码中的数组[array]有一个填充指针, 那么编译后的代码中对应的数组[array]可能只是填充指针所暗示的大小.

  **packages**: 加载器需要去找到对应包[package]对象[object], 就像通过用这个包名为参数调用 find-package 一样. 如果加载的时候这个名字对应的包[package]不存在, 那么就会发出一个 package-error 类型[type]的错误.

  **random-state**: 一个不变的随机状态[random state]对象不能用作函数[function] random 的状态参数, 因为 random 会修改这个数据结构.

  **structure, standard-object**: 如果存在类型[type]适合的 make-load-form 方法那么 structure-object 和 standard-object 类型[type]的对象[object]也可能出现在编译后的常量中.

      文件编译器[file compiler]在任何被引用作为字面化[literal]对象[object]的对象[object]上调用 make-load-form, 如果这个对象[object]是 standard-object, structure-object, condition, 或者任何依赖于具体实现的[implementation-dependent]其他类[class]集合(可能是空的)的任何一个的广义实例[generalized instance]的话. 对于单独文件[file]中任何给定的对象[object]文件编译器[file compiler]只会调用一次 make-load-form.

  **symbol**: 为了保证那些编译后的文件[compiled file]能够正确加载, 用户必须确保这些文件[file]中引用的包[package]在编译时和加载时被一致地定义. 符合规范的程序[conforming program]必须满足以下要求:

          1.当文件[file]中的顶层表达式形式[top level form]被 compile-file 处理时的当前包[current package]必须和编译后文件[compiled file]中对应顶层表达式形式[top level form]被 load 执行时的当前包相同[current package]. 特别地:

              a. 任何在一个文件[file]中修改当前包[current package]的顶层表达式形式[top level form]都必须在编译时和加载时将其更改为同一名称[name]的包[package].

              b. 如果在这个文件[file]中的第一个非原子[non-atomic]顶层表达式形式[top level form]不是一个 in-package 的表达式形式[form], 那么调用 load 时的当前包[current package]必须与 compile-file 被调用时的当前包[current package]有着相同名称[name].

          2.在编译期一个顶层表达式形式[top level form]被处理期间, 对于词法上出现在这个顶层表达式形式[top level form]中并且在当前包[current package]中是可访问的[accessible]而 home 包[home package]是另一个包[package]的所有符号[symbol], 在加载时当前包[current package]以及和编译时 home 包[home package]有着相同名字[name]的包[package]中也必须存在一个可访问的[accessible]相同名字[name]的符号[symbol].

          3.对于编译后的文件[compiled file]中出现的所有在编译时是它们 home 包[home package]的外部符号[external symbol]的符号[symbol], 在加载时相同名字[name]的这个包[package]里也必须存在名字[name]相同的外部符号[external symbol].

      如果其中任何一个条件都不成立, 那么加载器[loader]查找受影响的符号[symbol]的包[package]就是未指定的. 具体实现[implementation]允许去发出一个错误或者定义这个行为. 

### 3.2.5 <span id = "ExceptionalSituationsCompiler">编译器中的异常情况</span>

compile 和 compile-file 允许去发出错误和警告, 包括由于处理编译期的 (eval-when (:compile-toplevel) ...) 表达式形式的处理, 宏展开, 还有编译器自身发出的状况(condition)引起的错误.

编译在没有干预就不能处理的情况下, error 类型[type]的状况[condition]可能由编译器发出.

除了这个标准指定的必须或者可能发出 warning 类型[type]的状况[condition]的情况外, 在编译器可以确定结果未定义或者一个运行时错误会发出情况下也可能发出警告. 以下是这种情况的示例: 违反类型声明, 对 defconstant 定义的常量的值进行赋值或修改, 用错误数量的参数或者残缺的关键字列表调用内置的 Lisp 函数, 还有使用不可识别的声明标识符.

编译器允许以 style-warning 类型[type]的状况去提出一个关于编程风格问题的警告. 以下是这个情况的示例: 使用不同的参数列表重定义一个函数, 用错误数量的参数调用一个函数, 没有对一个没有引用到的局部变量声明 ignore, 还有引用一个声明为 ignore 的变量.

compile 和 compile-file 都允许(但不是必须)去为一个 error 类型[type]的状况[condition]建立[establish]一个处理者[handler]. 比如, 它们可能会发出一个警告, 并且从某个依赖于具体实现的[implementation-dependent]点重启编译来让这个编译在没有手动干预的情况下进行下去.

compile 和 compile-file 都返回 3 个值, 前两个表示被编译的源代码中是否包含错误以及是否提出风格警告.

一些警告可能会被推迟到编译结束的时候. 见 with-compilation-unit. 

## 3.3 <span id = "Declarations">声明</span>

声明(Declarations)提供了一种指定例如求值器或者编译器这样的程序处理器使用的信息的方式.

局部声明可以通过 declare 嵌入到可执行的代码中. 全局声明, 或者公告(proclamation), 可以通过 proclaim 或者 declaim 来建立.

这个 the 特殊表达式提供了一种简写标记来创建一个关于给定表达式的值的类型的局部声明.

如果一个程序违反了声明和公告, 结果是没有定义的.

> * 3.3.1 [最低的声明处理需求](#MDPR)
> * 3.3.2 [声明指定符](#DeclarationSpecifiers)
> * 3.3.3 [声明标识符](#DeclarationIdentifiers)
> * 3.3.4 [声明的作用域](#DeclarationScope)

### 3.3.1 <span id = "MDPR">最低的声明处理需求</span>

通常, 一个具体实现可以自由地忽视除了 declaration, notinline, safety 还有 special 以外的声明指定符.

一个 declaration 声明必须抑制关于不识别这个种类的声明的警告. 如果一个实现没有产生关于不识别声明的警告, 它可能安全地忽视了这个声明.

一个 notinline 声明必须被任何支持内联函数或者编译器宏的实现所识别进而废弃那些机制. 一个不使用内联函数或者编译器宏的实现可能会安全地忽略这个声明.

一个 safety 声明必须被识别, 它会提高当前的安全等级. 一个始终对代码进行处理的实现, 就像 safety 高一样, 可能安全地忽略这个声明.

一个 special 声明必须被所有实现所处理. 

### 3.3.2 <span id = "DeclarationSpecifiers">声明指定符</span>

一个声明指定符是一个可以出现在层定的 declare 表达式或者一个 declaim 表达式或者作为参数给 proclaim. 它是一个的列表, 其中 car 部分为声明标识符, cdr 部分为根据这个声明标识符所指定的规则解释出来的数据.

### 3.3.3 <span id = "DeclarationIdentifiers">声明标识符</span>

下面这段展示了这个标准定义的所有的声明标识符.

    declaration     ignore     special  
    dynamic-extent  inline     type     
    ftype           notinline           
    ignorable       optimize            

Figure 3-9. Common Lisp 声明标识符

一个具体实现可以自由地去支持其他(依赖实现)声明标识符. 如果一个声明标识符没有在上面定义, 也没有被具体实现所定义, 不是一个类型名字, 也没有在 declaration 全局公告中声明, 可能会发出一个警告.

#### 3.3.3.1 类型声明的简写标记

类型说明符可以用作声明标识符. (type-specifier var\*) 可以当作 (type type-specifier var\*) 的简写. 

### 3.3.4 <span id = "DeclarationScope">声明的作用域</span>

声明可以被分成两种类型: 一些适用于变量或函数的绑定; 一些则不适用于绑定.

一个出现在绑定表达式的头部并且适用于这个表达式创建的变量或函数的绑定的声明称之为绑定声明; 这个声明会影响这个声明作用域内的该绑定和任何该绑定的引用.

不是绑定声明的声明称为自由声明(free declarations).

在表达式 F1 中, 一个自由声明, 它适用于由某些表达式 F2 所建立的一个名字 N 的绑定, 其中 F1 是 F2 的一个子表达式, 它只影响 N 在F1中的引用; 它不适用于其他在 F1 以外的绑定, 也不影响 F2 中建立的 N 绑定的行为.

不适用于绑定的声明只能以自由绑定出现.

一个绑定声明的作用域和它对应的绑定的词法作用域相同; 对于特殊变量, 这意味拥有这个绑定的作用域所拥有的是一个词法绑定.

除非明确声明, 自由声明的作用域只包括它出现在头部的表达式的主体的子表达式, 不包括其他的子表达式. 自由声明的作用域不包括包含声明的表达式所建立的绑定的初始化表达式.

一些循环表达式包含 step, end-test, 或者 result 子表达式, 这些子表达式也包含在循环表达式中出现的声明作用域内. 具体地说, 涉及的循环表达式和子表达式是:

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

在这个例子, 第四行的第一个 x 引用是指第三行建立的 x 的词法绑定. 然而, 出现在第四行的第二个 x 位于第五行的自由声明的作用域内 (因为这个是 dotimes 的 结果表达式(result-form)) 并且因此引用 x 的动态绑定. 

## 3.4 <span id = "LambdaLists">Lambda 列表</span>

一个lambda列表是一个指明了参数集合(有时也称为lambda变量)和一个用于接收和这些参数有关的值的规程.

这里有几种lambda列表的类型.

|    上下文               |                       lambda 列表的种类   |
|   -   | - |
|defun form                                   |ordinary lambda list                             |
|defmacro form                                |macro lambda list                                |
|lambda expression                            |ordinary lambda list                             |
|flet local function definition               |ordinary lambda list                             |
|labels local function definition             |ordinary lambda list                             |
|handler-case clause specification            |ordinary lambda list                             |
|restart-case clause specification            |ordinary lambda list                             |
|macrolet local macro definition              |macro lambda list                                |
|define-method-combination                    |ordinary lambda list                             |
|define-method-combination :arguments option  |define-method-combination arguments lambda list  |
|defstruct :constructor option                |boa lambda list                                  |
|defgeneric form                              |generic function lambda list                     |
|defgeneric method clause                     |specialized lambda list                          |
|defmethod form                               |specialized lambda list                          |
|defsetf form                                 |defsetf lambda list                              |
|define-setf-expander form                    |macro lambda list                                |
|deftype form                                 |deftype lambda list                              |
|destructuring-bind form                      |destructuring lambda list                        |
|define-compiler-macro form                   |macro lambda list                                |
|define-modify-macro form                     |define-modify-macro lambda list                  |

Figure 3-10. 要使用的lambda列表的种类

下面这段列出了可应用于lambda列表的定义的名字.

    lambda-list-keywords  lambda-parameters-limit    

Figure 3-11. 可应用于lambda列表的定义的名字

> * 3.4.1 [普通lambda列表](#OrdinaryLambdaLists)
> * 3.4.2 [广义函数lambda列表](#GenericFunctionLambdaLists)
> * 3.4.3 [特化的lambda列表](#SpecializedLambdaLists)
> * 3.4.4 [宏lambda列表](#MacroLambdaLists)
> * 3.4.5 [解构lambda列表](#DestructuringLambdaLists)
> * 3.4.6 [Boa Lambda 列表](#BoaLambdaLists)
> * 3.4.7 [Defsetf Lambda 列表](#DefsetfLambdaLists)
> * 3.4.8 [Deftype Lambda 列表](#DeftypeLambdaLists)
> * 3.4.9 [Define-modify-macro Lambda 列表](#DefineMMLambdaLists)
> * 3.4.10 [Define-method-combination 参数 Lambda 列表](#DefineMCArgumentsLambdaLists)
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

一个 init-form 可以是任何表达式形式. 无论何时对于任何参数说明符任何的 init-form 的求值, 那个表达式形式可能引用任何这个说明符左边的参数变量, 包括任何 supplied-p-parameter 变量, 并且可能依赖没有其他参数变量已经被绑定的事实(包括它自己的参数变量) .

一个 keyword-name 可以是任何符号, 但是按照惯例是一个正常的关键字; 所有标准化的实现遵守这个惯例.

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

如果出现 &optional, 可选参数指定符就是那些跟在 &optional 后面直到下一个lambda列表关键字或者直到列表结束的那些指定符. 如果指定了可选参数, 然后每一个都按如下处理. 如果存在未处理的参数, 则参数变量 var 将绑定到后面的剩余参数, 就像必要参数一样. 如果没有参数剩下, 不管怎样, 那么 init-form 被求值, 并且参数变量被绑定给结果值(如果没有 init-form 出现在参数指定符就是 nil). 如果另一个变量名 supplied-p-parameter 出现在这个指定符中, 如果有一个参数可用它会被绑定为 true, 如果没有参数剩余它会被绑定为 false (因此 init-form 需要被求值). Supplied-p-parameter 不是绑定一个参数而是一个值, 它表示是否为相应的 var 提供了一个对应的参数. 

#### 3.4.1.3 <span id = "SpecifierRestParameter">剩余参数指定符</span>

&rest, 如果出现, 后面必须跟着单个的剩余参数指定符, 后面依次必须跟着另一个lambda列表关键字或者到lambda列表的末尾. 在所有可选参数被处理后, 这里可能是一个剩余参数. 如果这里是一个剩余参数, 它给绑定给一个所有 as-yet-unprocessed 参数的列表. 如果没有未处理参数剩下, 这个剩余参数绑定给空列表. 如果这里没有剩余参数和关键字参数并且有任何未处理参数剩余, 会发出一个错误; 见章节 3.5 (Error Checking in Function Calls). 剩余参数的值是允许的, 但不是必需的, 以便与 apply 最后一个参数共享结构. 

#### 3.4.1.4 <span id = "SpecifiersKeywordParameters">关键字参数指定符</span>

如果出现 &key , 所有直到下一个lambda列表关键字或者列表末尾的指定符都是关键字参数指定符. 当关键字参数被处理, 同样被处理的参数会被做成一个列表作为剩余参数. 同时指定 &rest 和 &key 是允许的. 在这个情况下剩下的参数被同时用于这两种目的; 这就是说, 所有剩下的参数被做成lambda列表作为剩余参数, 也被当作关键字参数处理. 如果指定了 &key, 必须有偶数个参数; 见章节 3.5.1.6 (Odd Number of Keyword Arguments). 这些参数被当作对, 每一对中的第一个参数被解释为一个名字而第二个作为对应的值. 每个对中的第一个对象必须是一个符号; 见章节 3.5.1.5 (Invalid Keyword Arguments). 这个关键字参数指定符可能可选地跟着lambda列表关键字 &allow-other-keys.

在每一个关键字参数指定符中必须是一个名字 var 作为参数变量. 如果这个 var 单独出现或者在一个 (var init-form) 组合中, 当匹配参数时参数是一个 KEYWORD 包中名字和 var 相同的符号时, 这个关键字名字会被使用. 如果这个 ((keyword-name var) init-form) 表示法被使用, 那么这个用于匹配参数的关键字名字是 keyword-name, 它可能是任何包中的符号(当然, 如果它不是 KEYWORD 包中的符号, 它没有必要自求值, 所以当调用这个函数时必须额外关心来确保正常的求值一直跳过这个关键字名字). 因此

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

此外, 如果接收的参数列表指定了一个普通的会被 :allow-other-keys 标记的参数, 那么 :allow-other-keys 同时有它的 special-cased 意义(确定是否允许附加的关键字)和它的正常意义(数据流入到提及的函数中).

##### 3.4.1.4.1 抑制参数检测

如果一个函数的lambda列表中指定了 &allow-other-keys, 对这个函数的调用中关键字参数检测会被抑制.

如果在一个函数的调用中 :allow-other-keys 参数是 true, 在这个调用中关键字参数检测是被抑制的.

这个 :allow-other-keys 在所有涉及关键字参数的地方都是允许的, 甚至当它关联的值是 false 时.

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

    可选参数和关键字参数可能没有默认的初始值表达式也没有使用 supplied-p 参数.

&aux 的使用

    &aux 的使用是不允许的. 

### 3.4.3 <span id = "SpecializedLambdaLists">特化的lambda列表</span>

一个特化的lambda列表被用于为一个特定的签名特化一个方法并且去描述匹配这个签名的参数如何被方法接收. 下一段中定义的名字以某种方式使用特化的lambda列表; 关于其中的每一个怎样处理的信息见字典条目.

    defmethod  defgeneric    

Figure 3-15. 使用特化的lambda列表的标准化操作符

一个特化的lambda列表可以包含下面这段中展示的lambda列表关键字.

    &allow-other-keys  &key       &rest  
    &aux               &optional         

Figure 3-16. 特定lambda列表使用的lambda列表关键字

一个特化的lambda列表是语法上等价于一个普通的lambda列表除了每一个必要参数可能可选地和一个类或者一个对象关联, 该参数是特定的.

    lambda-list::= ({var | (var [specializer])}* 
                    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                    [&aux {var | (var [init-form])}*]) 

### 3.4.4 <span id = "MacroLambdaLists">宏lambda列表</span>

一个宏lambda列表被用于描述下面这段中的操作符定义的宏.

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

可选参数 (通过 &optional 引入) 和关键字参数 (通过 &key 引入) 可以在一个宏lambda列表中被提供, 就像在普通lambda列表中一样. 每一个都可能包含默认初始化表达式和 supplied-p 参数.

&body 在函数中和 &rest 一样, 但是它可以被用于通知确定的输出格式化和编辑函数这个表达式的剩余部分被当作一个主体(body), 并且应该相应地缩进. 在任何特定的级别 &body 或者 &rest 只有一个可以被使用; 见章节 3.4.4.1 (Destructuring by Lambda Lists). &body 可以出现在一个宏lambda表达式的任何级别; 关于详细情况, 见章节 3.4.4.1 (Destructuring by Lambda Lists).

&whole 跟着一个绑定给整个 macro-call 表达式的单个变量; 这是这个宏函数收到的第一个参数的值. 如果出现 &whole 和一个跟在后面的变量, 它们必须出现在lambda列表的最前面, 在任何其他参数或者lambda列表关键字之前. &whole 可以出现在一个宏lambda列表的任何级别. 在内部级别, 这个 &whole 变量绑定给参数的对应部分, 正如 &rest, 但是不像 &rest, 其他参数也是允许的. 这个 &whole 的使用不影响参数指定的模式.

&environment 后面跟着一个绑定给表示当前词法环境[lexical environment]的环境, 这个环境是这个宏调用被解释时所处的环境. 这个环境应该和 macro-function, get-setf-expansion, compiler-macro-function, 还有 macroexpand (比如) 在计算宏展开式一起使用, 来确保这个编译环境中确定的任何词法绑定或定义被考虑进去. &environment 只能出现在宏lambda列表的顶层, 并且只能出现一次, 但是可以出现在这个列表的任何地方; 这个 &environment 和 &whole 被在这个lambda列表的任何其他变量之前被绑定, 不管 &environment 出现在这个lambda列表的什么地方. 绑定到环境参数的对象具有动态范围.

解构允许一个宏lambda列表去表达宏调用语法结构. 如果没有出现lambda列表关键字, 那么这个宏lambda列表是在叶子中包含参数名称的树. 匹配模式和宏表达式必须具有兼容的树结构; 这就是说, 它们的树结构必须是等价的, 或者它只能在匹配模式的某些叶节点与宏形式的非原子对象匹配时有所不同. 关于这种情况下的错误检测的信息, 见章节 3.5.1.7 (Destructuring Mismatch).

一个解构的lambda列表(不管在顶层还是嵌入的)可以是点对的(dotted), 以一个参数名结束. 这种情况的处理方式与结束列表的参数名称在 &rest 前面出现的情况完全相同.

对于一个宏表达式(或者是一个宏表达式的子表达式)是一个点对列表是允许的, 只有在和 (... &rest var) 或 (... . var) 匹配时. 宏需要去识别和处理这种情况.

#### 3.4.4.1 lambda列表的解构

在一个宏lambda列表中任何参数名字可以出现的地方, 还有普通lambda列表语法中(在章节 3.4.1 (Ordinary Lambda Lists) 描述的)不允许一个列表的地方, 一个解构lambda列表可以出现在参数名字的地方. 在这样做时, 与参数匹配的参数被当作一个(可能是点对的)列表, 作为一个参数列表, 用于满足内嵌的lambda列表中的参数. 这就被认为是解构(destructuring).

解构是将一个复合对象分解为它的组件部分的过程, 使用一种缩写的声明式语法, 而不是用原始的组件访问函数. 每一个组件部分绑定给一个变量.

一个解构操作需要一个将要解构的对象, 一个指定要提取哪些组件的匹配模式, 以及那些值为组件的变量的名称.

##### 3.4.4.1.1 lambda列表的数据导向解构

在数据导向的解构中, 匹配模式是一个要被分解的类型的对象. 无论在哪里提取组件, 在匹配模式中对应地方都会出现一个符号; 这个符号是变量的名称, 它的值是那个组件.

###### 3.4.4.1.1.1 lambda列表的数据导向解构示例

一个示例匹配模式是

```LISP
(a b c)
```

它解构了一个三个元素的列表. 这个变量 a 被赋值第一个元素, b 给赋值第二个, 等等. 一个更加复杂的例子是

```LISP
((first . rest) . more)
```

简单的语法和扩展到lambda列表导向的能力是数据导向解构的重要特性. 

##### 3.4.4.1.2 lambda列表的lambda列表导向解构

树的数据导向解构的一个延伸是lambda列表导向的解构. 这是从三元素的解构模式的类比中得出的

```LISP
(first second third)
```

并且这个三个参数的lambda列表

```LISP
(first second third)
```

如果没有lambda列表关键字出现在匹配模式中那么lambda列表导向的解构和数据导向的结构是相同的. 任何在这个匹配模式中的列表(不管是一个子列表或是整个匹配模式本身)包含lambda列表关键字就会被特别地解释. 这个列表中第一个lambda列表关键字左边的元素被当作解构匹配模式处理, 像平常一样, 但是列表中剩下的元素被当作函数lambda列表一样处理, 除了在通常需要一个变量的情况下, 可以使用任意的解构匹配模式. 注意, 在不确定的情况下，lambda列表语法优于解构语法. 因此, 在 &optional 之后，一个元素列表是一个解构匹配模式和一个默认值表达式的列表.

每个lambda列表关键字在lambda列表导向的解构匹配模式中的具体行为如下:

&optional

    每一个后面的元素是一个变量或者一个解构匹配模式, 一个默认值的表达式和一个 supplied-p 变量的列表. 这个默认值和 supplied-p 可以被省略. 如果这个被解构列表提前结束, 而它没有一个元素来匹配这个解构匹配模式或子模式, 那么这个默认表达式会求值并解构. 如果这个默认表达式被使用了 supplied-p 变量会收到值 nil, 否则就是 t.

&rest, &body

    下一个元素是一个匹配这个列表剩余部分的解构匹配模式. &body 和 &rest 一样但是声明所匹配的是构成表达式主体的表达式列表. 这下一个元素必须是最后一个除非后面跟着一个lambda列表关键字.

&aux

    其余的元素根本不是解构匹配模式, 而是辅助变量绑定.

&whole

    下一个元素是一个匹配一个宏里的整个表达式的解构匹配模式, 或者内部层级的整个子表达式.

&key

    后面跟着的元素是以下其中之一

    一个变量,

    或者一个变量, 一个可选的初始化表达式, 和一个可选的 supplied-p 变量的列表.

    或者一个关键字列表和一个解构匹配模式, 一个可选初始化表达式, 和一个可选 supplied-p 变量的列表.

    被解构的列表的其余部分被认为是交替的关键字和值并且被适当地分开了.

&allow-other-keys

    根据它自身理解. 

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

### 3.4.6 <span id = "BoaLambdaLists">Boa Lambda 列表</span>

一个 boa lambda 列表是一个语法上像普通lambda列表的lambda列表, 但是这个是按照参数的顺序风格处理.

一个 boa lambda 列表只被 defstruct 表达式使用, 当明确指定构造器函数的lambda列表时 (有时称之为 "boa constructor").

这个 &optional, &rest, &aux, &key, 还有 &allow-other-keys lambda列表关键字在boa lambda表达式中是被识别的. 这些lambda列表关键字有别于在普通lambda列表中的使用方式.

细想这个示例, 它描述了解构如何处理它的 :constructor.

```LISP
(:constructor create-foo
        (a &optional b (c 'sea) &rest d &aux e (f 'eff)))
```

这个定义了 create-foo 去做为一个或更多参数的构造器. 第一个参数被用于初始化 a 槽. 第二个参数用于初始化 b 槽. 如果这里没有第二个参数, 那么 defstruct 主体中给定的默认值(如果给了的话)被使用. 第三个参数被用于初始化 c 槽. 如果这里没有任何第三个参数, 那么符号 sea 就会被使用. 任何跟在第三个参数后面的参数被收集到一个列表中然后被用于初始化 d 槽. 如果这里有三个或更少的参数, 那么 d 槽的内容就是 nil. e 槽没有被初始化; 它的初始化值是实现定义的. 最后, f 槽被初始化去包含符号 eff. &key 和 &allow-other-keys 参数默认类似于 &optional 参数: 如果在这个lambda列表中没有提供默认值, 那么使用 defstruct 主体中给定的默认值(如果给了的话). 举例说:

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

并且 create-foo 提供了和 make-foo 不同的默认设置.

附加的参数不对应于槽名, 但只提供在随后的初始化计算中使用的值. 比如, 在这个定义中

```LISP
(defstruct (frob (:constructor create-frob
                (a &key (b 3 have-b) (c-token 'c) 
                        (c (list c-token (if have-b 7 2))))))
        a b c)
```

这个 c-token 参数只是给 c 槽的初始化提供一个值. 这个与可选参数和关键字参数相关的 supplied-p 参数也可以使用这种方式. 

### 3.4.7 <span id = "DefsetfLambdaLists">Defsetf Lambda 列表</span>

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

Figure 3-19. Defsetf Lambda 列表使用的lambda列表关键字

一个 defsetf lambda列表和普通lambda列表的区别仅在于它不允许使用 &aux, 并且它允许使用表示环境参数的 &environment. 

### 3.4.8 <span id = "DeftypeLambdaLists">Deftype Lambda 列表</span>

一个 deftype lambda列表被 deftype 所使用.

一个 deftype lambda列表有着像宏lambda列表相同的语法, 并且可以包含和宏lambda列表相同的lambda列表关键字.

一个 deftype lambda列表和宏lambda列表的区别仅在于如果没有给一个可选参数或关键字参数提供 init-from, 那么这个参数的默认值就是符号 * (而不是 nil). 

### 3.4.9 <span id = "DefineMMLambdaLists">Define-modify-macro Lambda 列表</span>

一个 define-modify-macro lambda列表被 define-modify-macro 使用.

一个 define-modify-macro lambda列表可以包含下面这段展示的lambda列表关键字.

    &optional  &rest  

Figure 3-20. Define-modify-macro Lambda 列表使用的lambda列表关键字

Define-modify-macro lambda列表类似于普通lambda列表, 但是不支持关键字参数. define-modify-macro 不需要去匹配关键字, 并且一个剩余参数就足够了. Aux 变量也不支持, 因为 define-modify-macro 没有主体表达式来引用这些绑定. 见宏 define-modify-macro. 

### 3.4.10 <span id = "DefineMCArgumentsLambdaLists">Define-method-combination 参数 Lambda 列表</span>

一个 define-method-combination 参数lambda列表被 define-method-combination 的 :arguments 选项所使用.

一个 define-method-combination 参数lambda列表可以包含下面这段中展示的lambda列表关键字.

    &allow-other-keys  &key       &rest   
    &aux               &optional  &whole  

Figure 3-21. Define-method-combination 参数 Lambda 列表使用的lambda列表关键字

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

        -- 它的定义 (如果它被明确定义).
        -- 所有适用方法的方法定义.
        -- 它的方法组合的定义.

* 对于表达式 (coerce x 'function), 其中 x 是一个 lambda 表达式, 当 coerce 执行时, 全局环境中优化质量 safety 的值也适用于产生的函数.

* 对于一个函数 ensure-generic-function 的调用, 在作为 :environment 参数传递的环境对象中优化质量 safety 的值也适用于产生的广义函数.

* 对于一个对lambda表达式作为参数的 compile 的调用, 在 compile 被调用时全局环境中优化质量 safety 的值适用于编译出来的函数.

* 对于一个单个参数的 compile 调用, 如果函数的原始定义是安全的, 那么作为结果编译后的函数也必须是安全的.

* 一个被 call-next-method 调用的方法如果下面的每一个都被认为是安全代码或者系统代码那么这个方法就被认为是安全的:

    -- 这个广义函数的定义 (如果它被明确定义).
    -- 所有适用方法的方法定义.
    -- 方法组合的定义.
    -- 方法定义表达式主体部分的入口点, 即确定 call-next-method 绑定的地方.
    -- 名字 call-next-method 函数求值的点.

一个不安全调用就是一个不是安全调用的调用.

非正式的意图是, 如果已经采取了所有合理的步骤来确保调用是安全的, 即使在涉及到系统代码的情况下, 程序员也可以依赖于一个安全的调用. 比如, 如果一个程序员从安全的代码中调用 mapcar 并且提供了一个被编译为安全的函数, 那么这个具体实现也需要去确保这个 mapcar 是一个安全的调用.

3.5.1.1.1 安全调用的错误检测时间

如果在安全调用中发出一个错误, 这个准确的发出点是依赖于实现的. 具体来说, 它可能在编译时或运行时发出, 如果在运行时发出, 它可能在执行这个调用时, 或之前, 或之后发出. 然而它总是在这个被调用函数的主体执行之前. 

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

当一个结构lambda列表和一个表达式匹配时, 这个解构匹配模式和表达式必须有像章节 3.4.4 (Macro Lambda Lists) 描述的兼容的树结构.

否则, 如果情况发生在安全的调用里, 一定会发出一个 program-error 类型的错误; 如果发生在一个不安全的调用中结果是不可预料的. 

#### 3.5.1.8 <span id = "ErrorsWhenCallingNextMethod">调用下一个方法时的错误(Errors When Calling a Next Method)</span>

如果 call-next-method 调用时带了参数, 用于 call-next-method 的变更后的参数集合的可适用方法集必须与这个广义函数的原始参数的可适用方法集相同, 否则应该会发出一个错误.

对新参数的一组方法和适用于原始参数的方法集合之间的比较, 其中相同指示符的方法次序是不敏感的.

如果 call-next-method 的参数指定了不同的可适用方法的不同排序集, 并且没有可用的下一个方法, 那么对不同方法的测试和相关错误信号的发出(存在的话)的将优先于调用 no-next-method. 

## 3.6 <span id = "TraversalRulesSideEffects">遍历规则和副作用</span>

当在一个对象遍历操作中执行的代码以一种可能影响正在进行的遍历操作的方式修改对象时, 其后果是未定义的. 尤其, 适用于以下规则.

列表遍历(List traversal)

    对于列表遍历操作, 列表中的 cdr 链是不允许被破坏性修改的.

数组遍历(Array traversal)

    对于数组遍历操作, 不允许对数组进行调整, 并且填充指针不允许被修改.

哈希表遍历(Hash-table traversal)

    对于哈希表遍历操作, 新元素可能不会被添加或删除, 除非与当前散列键对应的元素可以被更改或删除.

包遍历(Package traversal)

    对于包遍历操作 (比如, do-symbols), 新的符号不能从被遍历的包或者它使用的任何包中被捕捉或者 解除捕捉, 除非当前的符号可以从被遍历的包中被解除捕捉.

## 3.7 <span id = "DestructiveOperations">破坏性操作</span>

### 3.7.1 字面化对象的修改

如果字面化对象被破坏性地修改那么结果是不可预料的. 出于这个目的, 以下操作被认为是破坏性的:

random-state

    使用它作为函数 random 的一个参数.

cons

    修改 cons 的 car 或者 cdr 部分, 或者对一个 cons 的 car 或者 cdr 部分对象执行破坏性操作.

array

    将一个新值存储到数组的某个元素中, 或者对已经是该元素的对象执行破坏性操作.

    改变数组的填充指针, 维度或位移 (不管这个数组实际上是否为可调整的).

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

所有其他标准化类型

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

 * [符号 LAMBDA](#SymbolLAMBDA)
 * [宏 LAMBDA](#MacroLAMBDA)
 * [函数 COMPILE](#FunctionCOMPILE)
 * [函数 EVAL](#FunctionEVAL)
 * [特殊操作符 EVAL-WHEN](#SpecialOperatorEVALWHEN)
 * [特殊操作符 LOAD-TIME-VALUE](#SpecialOperatorLOADTIMEVALUE)
 * [特殊操作符 QUOTE](#SpecialOperatorQUOTE)
 * [访问器 COMPILER-MACRO-FUNCTION](#AccessorCOMPILERMACROFUNCTION)
 * [宏 DEFINE-COMPILER-MACRO](#MacroDEFINECOMPILERMACRO)
 * [宏 DEFMACRO](#MacroDEFMACRO)
 * [访问器 MACRO-FUNCTION](#AccessorMACROFUNCTION)
 * [函数 MACROEXPAND, MACROEXPAND-1](#FunctionMACROEXPANDMACROEXPAND1)
 * [宏 DEFINE-SYMBOL-MACRO](#MacroDEFINESYMBOLMACRO)
 * [特殊操作符 SYMBOL-MACROLET](#SpecialOperatorSYMBOLMACROLET)
 * [Variable *MACROEXPAND-HOOK*](#VariableMACROEXPANDHOOK)
 * [函数 PROCLAIM](#FunctionPROCLAIM)
 * [宏 DECLAIM](#MacroDECLAIM)
 * [符号 DECLARE](#SymbolDECLARE)
 * [声明 IGNORE, IGNORABLE](#DeclarationIGNOREIGNORABLE)
 * [声明 DYNAMIC-EXTENT](#DeclarationDYNAMICEXTENT)
 * [声明 TYPE](#DeclarationTYPE)
 * [声明 INLINE, NOTINLINE](#DeclarationINLINENOTINLINE)
 * [声明 FTYPE](#DeclarationFTYPE)
 * [声明 DECLARATION](#DeclarationDECLARATION)
 * [声明 OPTIMIZE](#DeclarationOPTIMIZE)
 * [声明 SPECIAL](#DeclarationSPECIAL)
 * [特殊操作符 LOCALLY](#SpecialOperatorLOCALLY)
 * [特殊操作符 THE](#SpecialOperatorTHE)
 * [函数 SPECIAL-OPERATOR-P](#FunctionSPECIALOPERATORP)
 * [函数 CONSTANTP](#FunctionCONSTANTP)


### <span id = "SymbolLAMBDA">符号 LAMBDA</span>

* * 语法(Syntax):

        lambda lambda-list [[declaration* | documentation]] form*

* * 参数(Arguments):

        lambda-list---一个普通lambda列表.
        declaration---一个声明表达式; 没有被求值的.
        documentation---一个字符串; 没有被求值的.
        form---一个表达式形式.

* * 描述(Description):

        lambda表达式是一种列表, 可以替代特定上下文中使用函数名来表示的函数, 通过直接描述其行为而不是间接地引用已建立函数的名称来表示函数.

        Documentation 作为文档字符串被附加到所表示的函数(如果有实际创建的话).

* * 也见(See Also):

        function, documentation, Section 3.1.3 (Lambda Expressions), Section 3.1.2.1.2.4 (Lambda Forms), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* * 注意(Notes):

    * 这个lambda表达式形式

        ```LISP
        ((lambda lambda-list . body) . arguments)
        ```

    * 语法上等价于函数表达式

        ```LISP
        (funcall #'(lambda lambda-list . body) . arguments)
        ```

### <span id = "MacroLAMBDA">宏 LAMBDA</span>

* 语法(Syntax):

        lambda lambda-list [[declaration* | documentation]] form* => function

* 参数和值(Arguments and Values):

        lambda-list---一个普通lambda表达式.
        declaration---一个声明表达式; 没有被求值.
        documentation---一个字符串; 没有被求值.
        form---一个表达式形式.
        function---一个函数.

* 描述(Description):

    * 为一个包含 lambda 表达式的函数的特殊表达式提供了一个简短的符号标记:

        ```LISP
        (lambda lambda-list [[declaration* | documentation]] form*)
        ==  (function (lambda lambda-list [[declaration* | documentation]] form*))
        ==  #'(lambda lambda-list [[declaration* | documentation]] form*)
        ```

* 示例(Examples):

    ```LISP
    (funcall (lambda (x) (+ x 3)) 4) =>  7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        lambda (symbol)

* 注意(Notes):

    * 这个宏可以这样实现:

        ```LISP
        (defmacro lambda (&whole form &rest bvl-decls-and-body)
        (declare (ignore bvl-decls-and-body))
        `#',form)
        ```

### <span id = "FunctionCOMPILE">函数 COMPILE</span>

* 语法(Syntax):

        compile name &optional definition => function, warnings-p, failure-p

* 参数和值(Arguments and Values):

        name---一个函数名字, 或者 nil.
        definition---一个lambda表达式或者一个函数. 如果 name 命名一个函数那么默认就是这个名字的函数定义, 或者如果 name 命名一个宏那么默认就是这个名字的宏函数. 当 name 是 nil 时没有提供这个参数那么结果是不可预料的.
        function---这个 function-name, 或者一个编译后的函数.
        warnings-p---一个普通的 boolean 值.
        failure-p---一个普通的 boolean 值.

* 描述(Description):

        编译一个解释的函数.

        compile 从(definition)定义中产生一个编译后的函数. 如果定义(definition)是一个lambda表达式, 它被强制为一个函数. 如果这个定义(definition)已经是一个编译后的函数, compile 会产出那个函数自身(换句话说, 是一个 identity 操作)或者一个等价的函数.

        如果这个名字(name)是 nil, 编译后的函数直接作为主要返回值返回. 如果给定一个非 nil 的名字(name), 那么编译后的函数替换这个名字对应的已存在的函数定义并且这个名字作为主要值返回; 如果名字(name)是一个命名了宏的符号, 它的宏函数会被更新并且名字(name)作为主要返回值返回.

        在 compile 函数处理的代码中出现的字面化对象既不是复制的也不是合并的. 从 compile 的执行中返回的代码引用和源代码中对应对象 eql 的对象.

        compile 允许但不是必须去确定 error 类型状况的处理者. 比如, 处理者可能会发出警告, 并从一些与实现相关的点重新启动编译, 以便在无需人工干预的情况下进行编译.

        第二个返回值, warnings-p, 如果编译器没有检测到 error 或者 warning 类型的状况那么就是 false, 否则就是 true.

        第三个返回值, failure-p, 如果编译器没有检测到 error 或者 warning(除了 style-warning) 类型的状况那么就是 false, 否则就是 true.

* 示例(Examples):

    ```LISP
    (defun foo () "bar") =>  FOO
    (compiled-function-p #'foo) =>  implementation-dependent
    (compile 'foo) =>  FOO 
    (compiled-function-p #'foo) =>  true
    (setf (symbol-function 'foo)
        (compile nil '(lambda () "replaced"))) =>  #<Compiled-Function>
    (foo) =>  "replaced"
    ```

* 受此影响(Affected By):

    * \*error-output\*, \*macroexpand-hook\*.

    * 宏定义和公告(proclamation)的存在.

* 异常情况(Exceptional Situations):

        如果要编译的函数的词汇环境包含除宏、符号宏或声明之外的任何绑定, 则其结果是未定义的.

        关于在编译过程中的错误检测信息, 见章节 3.2.5 (Exceptional Situations in the Compiler).

* 也见(See Also):

        compile-file

* 注意(Notes): None. 

### <span id = "FunctionEVAL">函数 EVAL</span>

* 语法(Syntax):

        eval form => result*

* 参数和值(Arguments and Values):

        form---一个表达式形式.
        results---表达式形式求值产生的值.

* 描述(Description):

        在当前动态作用域和 null 词法作用域下求值表达式.

        eval 是一个求值器的用户接口.

        这个求值器展开宏调用就像通过使用 macroexpand-1 一样.

        出现在被 eval 处理的代码中的常量既不会被复制也不会被合并. 从 eval 返回的结果代码引用的对象和源代码中对应的对象是 eql 的.

* 示例(Examples):

    ```LISP
    (setq form '(1+ a) a 999) =>  999
    (eval form) =>  1000
    (eval 'form) =>  (1+ A)
    (let ((a '(this would break if eval used local value))) (eval form))
    =>  1000
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        macroexpand-1, Section 3.1.2 (The Evaluation Model)

* 注意(Notes):

    * 为了获得一个符号的当前动态值, 使用 symbol-value 等价于(而且通常更可取的)使用 eval.

    * 注意, eval表达式需要对其参数进行两个级别的求值. 第一, 表达式是通过常规的参数求值机制来求值的, 就像任何调用一样. 从这个普通的参数求值得出的对象成为表达式参数的值, 然后作为 eval 表达式的一部分进行求值. 比如:

        ```LISP
        (eval (list 'cdr (car '((quote (a . b)) c)))) =>  b
        ```

    * 这个参数表达式 (list 'cdr (car '((quote (a . b)) c))) 以正常方式求值产生参数 (cdr (quote (a . b))); 然后 eval 求值它的参数, (cdr (quote (a . b))), 产生 b. 因为一个单次求值已经发生在任何函数表达式的任何参数表达式中, 所以 eval 有时被称为执行"一个额外级别的求值".

### <span id = "SpecialOperatorEVALWHEN">特殊操作符 EVAL-WHEN</span>

* 语法(Syntax):

        eval-when (situation*) form* => result*

* 参数和值(Arguments and Values):

        situation---这些符号中的一个 :compile-toplevel, :load-toplevel, :execute, compile, load, 或 eval. 这里 eval, compile, 和 load 被废弃了.
        forms---一个隐式的 progn.
        results---如果表达式被执行, 这个就是它们的值, 如果没有就是 nil.

* 描述(Description):

        一个 eval-when 表达式形式的主体被当作一个隐式的 progn 处理, 但是只有在列出的情况(situations)下.

        这里情况 :compile-toplevel (或 compile) 还有 :load-toplevel (或 load) 的使用控制着当 eval-when 作为顶层表达式出现在被 compile-file 处理的代码中时是否会被求值.

        这里情况 :execute (或者 eval) 的使用控制着其他 eval-when 表达式是否被求值; 这指的是, 那些不是顶层表达式的, 或者那些在被 eval 或 compile 处理的代码中. 如果这个 :execute 情况(situation) 在这样一个表达式中被指定, 那么其中的主体表达式作为一个隐式的 progn 处理; 否则, 这个 eval-when 表达式形式返回 nil.

        eval-when 正常出现在顶层表达式, 但是对于它出现在非顶层表达式也是有意义的. 然而, 这个描述在章节 3.2 (Compilation) 的编译时副作用只发生在当 eval-when 出现在顶层表达式的时候.

* 示例(Examples):

    * 使用 eval-when 的一个示例是, 当编译器使用用户定义的读取器宏时, 可以正确地读取文件, 它有必要写成

        ```LISP
        (eval-when (:compile-toplevel :load-toplevel :execute)
        (set-macro-character #\$ #'(lambda (stream char)
                                        (declare (ignore char))
                                        (list 'dollar (read stream))))) =>  T
        ```

    * 这个导致对 set-macro-character 的调用在编译器的执行环境中被执行, 从而修改它的读取器语法表.

        ```LISP
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
        ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compile-file, Section 3.2 (Compilation)

* 注意(Notes):

    * 以下的影响是对 eval-when 的定义的逻辑结果:

        * 一个单个的 eval-when 的执行对主体代码的执行不超过一次.

        * 用于在顶层表达式中使用的宏应该被编写, 以便在宏展开中使用各种表达式形式的副作用. 这个 macro-expander 自身不应该产生副作用.

            比如:

            错误的:

            ```LISP
            (defmacro foo ()
            (really-foo)
            `(really-foo))
            ```

            正确的:

            ```LISP
            (defmacro foo ()
            `(eval-when (:compile-toplevel :execute :load-toplevel) (really-foo)))
            ```

            遵守这个约定意味着这些宏在出现在非顶层表达式时表现得很直观.

        * 在一个 eval-when 周围放置一个变量的绑定确实会捕获这个绑定因为 compile-time-too 模式不会发生 (换句话说, 引入一个变量绑定意味着 eval-when 不是一个顶级表达式). 比如,

            ```LISP
            (let ((x 3))
            (eval-when (:execute :load-toplevel :compile-toplevel) (print x)))
            ```

            在执行的时候(换句话说, load)打印 3, 并且在编译时不会打印任何东西. 这很重要, 因此 defun 和 defmacro 的扩展可以用 eval-when 来完成并且可以正确地捕获词汇环境.

            ```LISP
            (defun bar (x) (defun foo () (+ x 3)))
            ```

            可能被展开成

            ```LISP            
            (defun bar (x) 
            (progn (eval-when (:compile-toplevel) 
                        (compiler::notice-function-definition 'foo '(x)))
                    (eval-when (:execute :load-toplevel)
                        (setf (symbol-function 'foo) #'(lambda () (+ x 3))))))
            ```

            它会被上面的规则当成和下面的这个相同

            ```LISP
            (defun bar (x) 
            (setf (symbol-function 'foo) #'(lambda () (+ x 3))))
            ```

            当这个 bar 的定义不是一个顶层表达式时. 

### <span id = "SpecialOperatorLOADTIMEVALUE">特殊操作符 LOAD-TIME-VALUE</span>

* 语法(Syntax):

        load-time-value form &optional read-only-p => object

* 参数和值(Arguments and Values):

        form---一个表达式形式; 按照以下描述的被求值.
        read-only-p---一个boolean类型; 没有求值的.
        object---从这个求值表达式返回的主要的值.

* 描述(Description):

        load-time-value 为表达式直到表达式在运行时环境中才求值提供一个延时求值的机制; 见章节 3.2 (Compilation).

        Read-only-p 指定这个结果是否可以被当作常量对象. 如果是 t, 结果就是一个 read-only 的量 that can, 如果适用于实现, 它可以被拷贝到只读空间 并且/或 和来自其他程序的相似的常量对象惊醒合并. 如果是 nil (默认的), 结果必须既不被复制, 也不能被合并; 它必须被认为是可能修改的数据.

        如果一个 load-time-value 表达式被 compile-file 处理, 这个编译器在这个表达式上执行它正常的语义处理 (就像宏展开并转为机器码), 但是为这个表达式的执行安排在了加载时在一个 null 的词法环境[lexical environment]中, 这个求值的结果被当作是一个运行时的字面化对象. 确保表达式的求值只有在文件加载时才会发生, 但是对文件中顶级表达式的求值的求值顺序是由依赖实现的.

        如果一个 load-time-value 表达式出现在被 compile 编译的函数中, 这个表达式在编译时的一个 null 词法环境[lexical environment]中被求值. 这个编译时求值的结果被当作是编译后代码中的字面化对象.

        如果一个 load-time-value 表达式被 eval 处理, 表达式在一个 null 词法环境中被求值, 并且返回一个值. 由 eval 处理的隐式编译(或部分编译)表达式的实现可能只在编译完成时才计算一次.

        如果相同的列表 (load-time-value form) 被求值或编译超过一次, 这个表达式被求值一次或者超过一次是依赖于具体实现的. 当一个表达式被求值或编译了共享子结构时, 当相同的表达式形式被 eval 或 compile 多次处理时, 就会发生这种情况. 由于一个 load-time-value 表达式可以不止一个地方被引用并且可以被 eval 超过一次求值, 每一次执行返回新的对象还是返回和其他执行一样的对像是依赖于具体实现的. 当对结果对象进行破坏性修改时, 用户必须谨慎使用.

        如果两个列表 (load-time-value form) 在 equal 下是相同的但是在求值后和编译后是不同的, 它们的值总是来自于对表达式形式的不同的求值. 它们的值可能不会被合并除非 read-only-p 是 t.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compile-file, compile, eval, Section 3.2.2.2 (Minimal Compilation), Section 3.2 (Compilation)

* 注意(Notes):

        load-time-value 必须出现在一个"for evaluation"位置, 以及被引用的结构外面. 果在一个引用结构中调用 load-time-value 的情况下，backquote 读取器宏可能会被调用; 见章节 2.4.6 (Backquote).

        为 read-only-p 指定为 nil 不是一个强制对象为可修改的方法, 如果它已经被当作只读的. 这只是一种说法, 对于一个可修改的对象, 这个操作不是为了使对象只读. 

### <span id = "SpecialOperatorQUOTE">特殊操作符 QUOTE</span>

* 语法(Syntax):

        quote object => object

* 参数和值(Arguments and Values):

        object---一个对象; 没有求值.

* 描述(Description):

        这个 quote 特殊操作符只是返回 object.

        如果字面化对象(包括 quoted 的转引对象)被破坏性修改那么结果是未定义的.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        Section 3.1 (Evaluation), Section 2.4.3 (Single-Quote), Section 3.2.1 (Compiler Terminology)

* 注意(Notes):

        这个 'object 文本表示法和 (quote object) 是等价的; 见章节 3.2.1 (Compiler Terminology).

        一些称之为自求值对象的对象不需要被 quote 引用. 然而, 符号和列表用来表示程序的一部分, 因此不能作为一个程序中的常量数据而不用 quote. 由于引用抑制了这些对象的求值, 它们变成了数据而不是程序. 

### <span id = "AccessorCOMPILERMACROFUNCTION">访问器 COMPILER-MACRO-FUNCTION</span>

* 语法(Syntax):

        compiler-macro-function name &optional environment => function

        (setf (compiler-macro-function name &optional environment) new-function)

* 参数和值(Arguments and Values):

        name---一个函数名字.
        environment---一个环境对象.
        function, new-function---一个编译器宏函数, 或者 nil.

* 描述(Description):

        在环境(environment)中访问 name 命名的编译器宏函数, 如果有的话.

        一个 nil 值表示 name 命名的编译器宏函数的缺失.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        在 compiler-macro-function 的 setf 使用中如果 environment 不是 nil 那么结果是不可预料的.

* 也见(See Also):

        define-compiler-macro, Section 3.2.2.1 (Compiler Macros)

* 注意(Notes): None. 

### <span id = "MacroDEFINECOMPILERMACRO">宏 DEFINE-COMPILER-MACRO</span>

* 语法(Syntax):

        define-compiler-macro name lambda-list [[declaration* | documentation]] form*
        => name

* 参数和值(Arguments and Values):

        name---一个函数名字.
        lambda-list---一个宏lambda列表.
        declaration---一个声明表达式; 没有求值.
        documentation---一个字符串; 没有求值.
        form---一个表达式形式.

* 描述(Description):

    * 这是一个正常的机制去定义编译器宏函数. 它的方式和 defmacro 一样; 仅有的区别是:

        * 这个 name 可以使一个命名了任何函数或宏的函数名字.

        * 展开函数被安装为一个编译器宏函数, 而不是一个宏函数.

        * 这个 &whole 参数绑定到传递给编译器宏函数的 form 参数. 其余被指定的 lambda-list 参数就好像一个表达式, 该表达式包含 car 中的函数名和 cdr 中的实际参数, 但是如果实际表达式形式的 car 部分是一个符号 funcall, 那么参数的解构实际上通过使用它的 cddr 来执行.

        * Documentation 被关联到 name(就像 compiler-macro) 和编译器宏函数作为一个文档字符串.

        * 不像一个普通的宏, 编译器宏可以拒绝通过返回与原始状态相同(那个可以通过使用 &whole 获取到的)的表达式来提供展开式.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compiler-macro-function, defmacro, documentation, Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

        为一个 COMMON-LISP 包中的函数写一个编译器宏定义结果是无法预料的; 很有可能在某些实现中, 这样的尝试会覆盖相同或同等重要的定义. 一般来说, 建议程序员只编写自己维护的函数的编译器宏定义--为其他地方维护的函数编写一个编译器宏定义通常被认为违背了模块化和数据抽象的传统规则. 

### <span id = "MacroDEFMACRO">宏 DEFMACRO</span>

* 语法(Syntax):

        defmacro name lambda-list [[declaration* | documentation]] form*
        => name

* 参数和值(Arguments and Values):

        name---一个符号. 
        lambda-list---一个宏 lambda 列表.
        declaration---一个声明表达式; 不求值.
        documentation---字符串; 不求值.
        form---一个表达式形式.

* 描述(Description):

        通过把一个宏函数关联到全局环境中的这个 name 来把 name 定义为一个宏. 这个宏函数被定义在 defmacro 表达式形式出现的相同的词法环境[lexical environment]中.

        在 lambda-list 列表里的参数变量被绑定到这个宏调用的解构的部分里.

        这个展开函数接受两个参数, 一个表达式形式和一个环境. 这个展开函数返回一个表达式形式. 这个展开函数的主体是由表达式形式指定的. 表达式按顺序被执行. 最后一个表达式执行的值作为这个宏的展开返回. 这个展开函数的主体表达式 (但是不是 lambda-list) 被隐式地附在一个名字为 name 的块上.

        这个 lambda-list 符合 3.4.4 章节(Macro Lambda Lists)所描述的需求.

        Documentation 作为文档字符串被关联到 name(像 function 一样) 和这个宏函数.

        defmacro 可以被用于重定义一个宏或者用一个宏定义替换一个函数定义.

        返回的表达式的递归展开必须终止, 其中包括其他表达式的子表达式的展开.

        如果完全宏展开表达式的结果包含任何圆形列表结构, 除了字面化对象之外, 结果是不可预料的.

        如果一个 defmacro 表达式形式作为顶层表达式出现, 编译器一定会在编译时把宏定义存储起来, 这样在这个文件中出现的这个宏就可以被正确地展开. 如果这个宏在这个文件被编译时引用到, 用户必须确保这个宏的主体可以在编译时被求值.

* 示例(Examples):

    ```LISP
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
    ```

    * 只有在普通的lambda列表语法允许参数名而不是列表的情况下, 才允许使用嵌入的解构lambda列表来防止歧义. 比如, 下面的是非法的:

        ```LISP
        (defmacro loser (x &optional (a b &rest c) &rest z)
        ...)
        ```

    * 因为普通 lambda 列表语法不允许一个列表跟着 &optional ; 列表 (a b &rest c) 会被解释为描述一个名字为 a 的可选参数, 它的默认值是表达式 b, 带有一个 supplied-p 参数命名为 &rest (不合法), 还有一个无关的符号 c (也是不合法的). 一种几乎正确的方式来表达是

        ```LISP
        (defmacro loser (x &optional ((a b &rest c)) &rest z)
        ...)
        ```

    * 额外的括号消除了歧义. 然而, 这个定义现在是不正确的因为一个像 (loser (car pool)) 这样的宏调用不会提供任何参数表达式给lambda列表(a b &rest c), 因此，与 lambda 列表相匹配的默认值是 nil 因为没有指定明确的默认值. 这个结果的结果是不可预料的, 因为空的列表, nil, 没有表达式来满足参数 a 和 b. 完全正确的定义应该是

        ```LISP
        (defmacro loser (x &optional ((a b &rest c) '(nil nil)) &rest z)
        ...)
        ```

        或者

        ```LISP
        (defmacro loser (x &optional ((&optional a b &rest c)) &rest z)
        ...)
        ```

    * 这些略有不同: 第一个要求如果宏调用显式指定了 a, 那么它也必须显式地指定 b, 而第二个则没有这个要求. 比如,

        ```LISP
        (loser (car pool) ((+ x 1)))
        ```

    * 对于第二个定义是个合法的调用但是对于第一个则不是.

        ```LISP
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
        ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-compiler-macro, destructuring-bind, documentation, macroexpand, *macroexpand-hook*, macrolet, macro-function, Section 3.1 (Evaluation), Section 3.2 (Compilation), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes): None. 

### <span id = "AccessorMACROFUNCTION">访问器 MACRO-FUNCTION</span>

* 语法(Syntax):

        macro-function symbol &optional environment => function

        (setf (macro-function symbol &optional environment) new-function)

* 参数和值(Arguments and Values):

        symbol---一个符号.
        environment---一个环境对象.
        function---一个宏参数或者 nil.
        new-function---一个宏函数.

* 描述(Description):

        确定 symbol 在指定的环境中是否有一个函数定义作为一个宏.

        如果是这样, 则返回宏展开函数, 即两个参数的函数. 如果 symbol 在这个词法环境[lexical environment]中没有函数定义, 或者它的定义不是一个宏, macro-function 返回 nil.

        对于 symbol, macro-function 和 special-operator-p 都返回 true 是有可能的. 宏定义必须可用于那些只理解标准的 Common Lisp 特殊表达式的程序.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By):

        (setf macro-function), defmacro, and macrolet.

* 异常情况(Exceptional Situations):

        在一个 macro-function 的 setf 的使用中如果 environment 不是 nil 那么结果是不可预料的.

* 也见(See Also):

        defmacro, Section 3.1 (Evaluation)

* 注意(Notes):

    * setf 可以和 macro-function 一起使用来设置一个宏作为 symbol 的全局函数定义:

        ```LISP
        (setf (macro-function symbol) fn)
        ```

        设置的值必须是一个函数, 该函数接受两个参数, 整个宏调用和一个环境, 并计算该调用的展开. 执行这个操作会导致符号只有这个宏定义作为全局函数定义; 任何之前的定义, 不管是宏还是函数, 都会丢失. 

### <span id = "FunctionMACROEXPANDMACROEXPAND1">函数 MACROEXPAND, MACROEXPAND-1</span>

* 语法(Syntax):

        macroexpand form &optional env => expansion, expanded-p

        macroexpand-1 form &optional env => expansion, expanded-p

* 参数和值(Arguments and Values):

        form---一个表达式形式.
        env---一个环境对象. 默认是 nil.
        expansion---一个表达式形式.
        expanded-p---一个普通的 boolean.

* 描述(Description):

        macroexpand 和 macroexpand-1 展开宏.

        如果 form 是一个宏表达式形式, 那么 macroexpand-1 只展开宏表达式调用一次.

        macroexpand 重复地展开 form 直到它不再是一个宏表达式形式. 事实上, macroexpand 重复调用 macroexpand-1 直到第二个值返回的是 nil.

        如果 form 是一个宏表达式, 那么展开就是一个宏展开并且 expanded-p 是 true. 否则, 这个展开就是给定的表达式形式并且 expanded-p 是 false.

        宏展开是按照以下方式进行的. 一旦 macroexpand-1 确定表达式是一个宏表达式形式, 它就会获得一个用于宏或符号宏的适当展开函数. 这个 *macroexpand-hook* 的值强制为一个函数并且作为一个三个参数的函数调用: 展开函数, 这个 form, 还有这个 env. 从这个调用返回的值被认为是这个表达式的展开.

        除了全局环境中的宏定义之外, 在 env 中建立的任何本地宏定义都被认为是由 macrolet 或 symbol-macrolet 所建立的. 如果只提供了 form 作为一个参数, 那么环境实际上就是 null, 只有通过 defmacro 建立的全局宏定义才会被考虑. 宏定义被局部函数定义所遮蔽.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By):

        defmacro, setf of macro-function, macrolet, symbol-macrolet

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        *macroexpand-hook*, defmacro, setf of macro-function, macrolet, symbol-macrolet, Section 3.1 (Evaluation)

* 注意(Notes):

        无论是 macroexpand 还是 macroexpand-1 都没有明确的尝试去展开宏表达式, 这些要么是表达式形式的子形式，要么是展开式的子形式. 然而, 由于宏函数的语义或实现, 这样的展开可能隐式发生. 

### <span id = "MacroDEFINESYMBOLMACRO">宏 DEFINE-SYMBOL-MACRO</span>

* 语法(Syntax):

        define-symbol-macro symbol expansion
        => symbol

* 参数和值(Arguments and Values):

        symbol---一个符号.
        expansion---一个表达式形式.

* 描述(Description):

        为全局影响指示符号的宏展开提供了一种机制.

        为 symbol 命名的符号宏全局地确定一个展开函数. 对于符号宏来说, 展开函数的唯一确保属性是当它被应用到表达式形式和环境时它会返回正确的展开式. (具体来说，这个展开式是在概念上存储在展开函数, 环境, 还是两者中, 这是依赖于实现的.)

        每个对 symbol 的全局引用(换句话说, 不被由同一个符号所命名的变量或符号宏的绑定所遮蔽)都是通过常规的宏展开过程来展开的; 见章节 3.1.2.1.1 (Symbols as Forms). 符号宏的展开受限于符号宏引用相同的词汇环境中进行的进一步的宏展开, 与普通宏类似.

        如果在这个定义的范围内为 symbol 做一个 special 声明, 后果是未知的 (换句话说, 当它没有被绑定到一个由相同符号命名的变量或符号宏的绑定所遮蔽时).

        任何在这个定义的作用域中使用 setq 来设置 symbol 的值都被当作 setf. symbol 的 psetq 被当作是 psetf, 并且 multiple-value-setq 被当作是多个值的 setf.

        一个符号宏的绑定可以被 let 或 symbol-macrolet 所遮蔽.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 symbol 已经被定义为一个全局的变量, 会发出一个 program-error 类型的错误.

* 也见(See Also):

        symbol-macrolet, macroexpand

* 注意(Notes): None. 

### <span id = "SpecialOperatorSYMBOLMACROLET">特殊操作符 SYMBOL-MACROLET</span>

* 语法(Syntax):

        symbol-macrolet ((symbol expansion)*) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        symbol---一个符号.
        expansion---一个表达式形式.
        declaration---一个声明表达式; 不求值.
        forms---一个隐式的 progn.
        results---forms 返回的值.

* 描述(Description):

        symbol-macrolet 为影响符号的宏展开环境提供一种机制.

        symbol-macrolet 为每一个 symbol 命名的符号宏词法上确定展开函数. 对于符号宏来说, 展开函数的唯一保证属性是当它被应用到表达式和环境时它会返回正确的展开式. (具体来说, 它在概念上是存储在展开函数, 环境, 还是两者中, 这是依赖于实现的.)

        在 symbol-macrolet 词法作用域内, 变量作为 symbol 的每个引用都被常规的宏展开过程所展开; 见章节 3.1.2.1.1 (Symbols as Forms). 符号宏的展开受限于于符号宏引用相同的词汇环境中进行的进一步的宏展开, 与普通宏类似.

        对于 let 完全相同的声明是允许的, 除了一个例外: 如果一个 special 声明命名了 symbol-macrolet 已经定义的一个符号名字, symbol-macrolet 会发出一个错误.

        当 symbol-macrolet 形式中的表达式被展开, 任何使用 setq 来设置已指定变量中一个值被当作是 setf 一样. 一个符号定义的符号的 psetq 被当作它是一个 psetf, 并且 multiple-value-setq 被当作是多个值的 setf.

        这个 symbol-macrolet 的使用可以被 let 遮蔽. 换句话说, symbol-macrolet 只是替代了 forms 周围的 symbol 的词法绑定作用域中的 symbol.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果试图绑定一个被定义为全局变量的符号, 会发出一个 program-error 类型的错误.

        如果声明中包含一个对 symbol-macrolet 绑定符号的 special 声明, 那么会发出一个 program-error 类型的错误.

* 也见(See Also):

        with-slots, macroexpand

* 注意(Notes):

        这个特殊的表达式形式 symbol-macrolet 是用于实现 with-slots 的基础机制.

        如果一个 symbol-macrolet 形式是一个顶层表达式形式, 这个表达式形式也被当作顶层表达式处理. 见章节 3.2.3 (File Compilation). 

### <span id = "VariableMACROEXPANDHOOK">变量 \*MACROEXPAND-HOOK\*</span>

值类型(Value Type):

        一个三个参数的函数的标志符: 一个宏函数, 一个宏表达式形式, 和一个环境对象.

初始值(Initial Value):

        一个等价于函数 funcall 的函数的标志符, 但是可能有额外的依赖于实现的副作用.

* 描述(Description):

        被用作 macroexpand-1 的展开接口钩子来控制宏展开过程. 当一个宏表达式要被展开时, 这个函数会被调用, 并传递三个参数: 这个宏函数, 宏的表达式形式, 这个要被展开的宏形式所在的环境对象. 这个环境对象具有动态范围; 如果环境对象在宏展开函数的动态范围之外被引用, 则其后果是未定义的.

* 示例(Examples):

    ```LISP
    (defun hook (expander form env)
        (format t "Now expanding: ~S~%" form)
        (funcall expander form env)) =>  HOOK 
    (defmacro machook (x y) `(/ (+ ,x ,y) 2)) =>  MACHOOK 
    (macroexpand '(machook 1 2)) =>  (/ (+ 1 2) 2), true 
    (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
    >>  Now expanding (MACHOOK 1 2) 
    =>  (/ (+ 1 2) 2), true
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        macroexpand, macroexpand-1, funcall, Section 3.1 (Evaluation)

* 注意(Notes):

        选择的初始值的净效果是仅调用宏函数, 将宏形式和环境作为它的两个参数.

        用户或用户程序可以指定这个变量来定制或跟踪宏展开机制. 注意, 但是, 这个变量是一个全局资源, 可能由多个程序共享; 因此, 如果两个程序在这个变量的设置上依赖于它们的正确性, 那么这些程序可能无法在相同的Lisp镜像中运行. 由于这个原因, 通常最好将其用途限制在调试情况下.

        把函数设置到 *macroexpand-hook* 的用户应该考虑把这个钩子的之前的值保存起来, 并且从他们自己的函数里调用这个值.

### <span id = "FunctionPROCLAIM">函数 PROCLAIM</span>

* 语法(Syntax):

        proclaim declaration-specifier => implementation-dependent

* 参数和值(Arguments and Values):

        declaration-specifier---一个声明说明符.

* 描述(Description):

        在全局环境中建立 declaration-specifier 指定的声明.

        这样一个声明, 有时也称之为全局声明或公告(proclamation), 除非局部被遮蔽, 否则永远是有效的.

        在 declaration-specifier 中，变量和函数的名称分别引用动态变量和全局函数定义.

        下一个图显示了可用于 proclaim 的声明标识符列表.

            declaration  inline     optimize  type  
            ftype        notinline  special         

            Figure 3-22. 全局声明标识符

        一个具体实现也可以自由地支持其他(依赖于实现的)声明标识符.

* 示例(Examples):

    ```LISP
    (defun declare-variable-types-globally (type vars)
    (proclaim `(type ,type ,@vars))
    type)

    ;; Once this form is executed, the dynamic variable *TOLERANCE*
    ;; must always contain a float.
    (declare-variable-types-globally 'float '(*tolerance*))
    =>  FLOAT
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declaim, declare, Section 3.2 (Compilation)

* 注意(Notes):

    虽然 proclaim 的执行有着可能影响编译的效果, 编译器不会尝试识别和特别地处理 proclaim 形式. 像下面这样的公告(proclamation), 即便是一个顶层表达式, 直到它被执行完也不会有任何效果:

    ```LISP
    (proclaim '(special *x*))
    ```

    如果编译时的副作用是需要的, eval-when 可能会有用. 比如:

    ```LISP
    (eval-when (:execute :compile-toplevel :load-toplevel)
    (proclaim '(special *x*)))
    ```

    然而, 在大多数这样的情况下, 使用 declaim 来实现这一目的是比较容易的.

    因为 proclaim 表达式是普通的函数形式，所以宏表达式可以展开到它们. 

### <span id = "MacroDECLAIM">宏 DECLAIM</span>

* 语法(Syntax):

        declaim declaration-specifier* => implementation-dependent

* 参数和值(Arguments and Values):

        declaration-specifier---一个声明说明符; 不求值的.

* 描述(Description):

        确定一个被 declaration-specifiers 指定的声明.

        如果在文件编译器处理的文件中使用该宏作为顶层表达式, 公告(proclamations)也在编译时进行. 与其他定义宏一样, 在编译完文件后, 编译时的副作用是否存在仍然是未知的.

* 示例(Examples):

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declare, proclaim

* 注意(Notes): None. 

### <span id = "SymbolDECLARE">符号 DECLARE</span>

* 语法(Syntax):

        declare declaration-specifier*

* 参数(Arguments):

        declaration-specifier---一个声明说明符; 不求值的.

* 描述(Description):

        一个 declare 表达式, 有时也称之为一个声明(declaration), 只能出现在合适的表达式形式的主体部分; 那也就是说, 它可能只在其他声明表达式之前出现, 或者如果上下文允许前面还有文档字符串.

        一个 declare 表达式可以出现在一个lambda表达式中或者下面这段列出的任何表达式中.

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

        Figure 3-23. 声明可以出现的标准表达式

        一个 declare 表达式只能出现在这些表达式形式的语法所指定的位置. 尝试去求值一个 declare 表达式的后果是未定义的. 在这种表达式出现的情况下, 为它们的存在而进行显式检查, 并且它们从来没有真正被求值过; 正是因为这个原因它们被称之为 "declare expressions" 而不是 "declare forms".

        Macro 表达式形式不能被展开成声明; declare 表达式必须作为它们所引用的表达式的实际的子表达式形式出现.

        下一段展示了可以被用于 declare 的声明标识符的列表.

            dynamic-extent  ignore     optimize  
            ftype           inline     special   
            ignorable       notinline  type      

            Figure 3-24. 局部声明标识符

        一个具体实现也可以自由地支持其他声明标识符(依赖于实现的).

* 示例(Examples):

    ```LISP
    (defun nonsense (k x z)
    (foo z x)                     ;First call to foo
    (let ((j (foo k x))           ;Second call to foo
            (x (* k k)))
        (declare (inline foo) (special x z))
        (foo x j z)))               ;Third call to foo
    ```

        在这个例子里, 这个 inline 声明只应用于第三个 foo 调用, 不是第一个和第二个.这个 x 的 special 声明导致 let 为 x 创建了一个动态绑定, 并且导致 let 主体中对 x 的引用是一个动态引用. 在对 foo 的第二个调用中的 x 的引用是一个对 nonsense 第二个参数的局部引用. 在第一个 foo 调用中对 x 的引用也是一个局部引用, 不是一个 special 的. 这个 z 的 special 声明导致第三个 foo 调用中的 z 引用是一个动态引用; 它不引用 nonsense 中名为 z 的参数, 因为这个参数绑定没有被声明为 special. (这个 z 的 special 声明不是出现在 defun 的主体部分, 而是在一个内部的表达式形式中, 因此不影响参数的绑定.)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        试着把 declare 表达式当作一个表达式形式来求值后果是不可预料的.

* 也见(See Also):

        proclaim, Section 4.2.3 (Type Specifiers), declaration, dynamic-extent, ftype, ignorable, ignore, inline, notinline, optimize, type

* 注意(Notes): None. 

### <span id = "DeclarationIGNOREIGNORABLE">声明 IGNORE, IGNORABLE</span>

* 语法(Syntax):

        (ignore {var | (function fn)}*)

        (ignorable {var | (function fn)}*)

* 参数(Arguments):

        var---一个变量名字.
        fn---一个函数名字.

* 合法上下文(Valid Context):

        declaration

* 绑定类型的影响(Binding Types Affected):

        variable, function

* 描述(Description):

        这个 ignore 和 ignorable 声明指向多个 var 的变量绑定还有多个 fns 的函数绑定的值引用.

        一个 ignore 声明说明指定绑定的值引用不会发生在声明作用域里. 在这样一个声明作用域里, 编译器针对任何 var 或 fn 的值引用, 或者一个 var 的 special 声明去发出一个警告是合适的.

        一个 ignorable 声明表示指定绑定的值引用可能或可能不会出现在声明的作用域里. 在这样的作用域里, 编译器针对任何 var 或 fn 的值引用, 或者一个 var 的 special 声明去发出一个警告是不合适的.

        当不在一个 ignore 或 ignorable 声明的作用域时, 编译器针对没有任何 var 或 fn 的值引用, 也没有一个 var 的 special 声明的情况发出一个警告是合适的.

        任何关于一个 "used" 或者 "unused" 绑定必须是 style-warning 类型的, 并且不影响程序的语义.

        由 with-open-file, with-open-stream, with-input-from-string, 和 with-output-to-string 确定的流变量, 还有所有循环变量, 根据定义总是是 "used". 使用 (declare (ignore v)), 对于这样一个变量 v 有未指明的结果.

* 也见(See Also):

        declare 

### <span id = "DeclarationDYNAMICEXTENT">声明 DYNAMIC-EXTENT</span>

* 语法(Syntax):

        (dynamic-extent [[var* | (function fn)*]])

* 参数(Arguments):

        var---一个变量名字.
        fn---一个函数名字.

* 合法上下文(Valid Context):

        declaration

* 绑定类型的影响(Binding Types Affected):

        variable, function

* 描述(Description):

        在一些包含的表达式中, F, 这个声明为每一个 vari 断言 (不需要被 F 绑定), 并且为 vari 呈现的每一个值断言, 还有当 vij 为 vari 的值的任何时候为 vij 的另外不可访问部分(otherwise inaccessible part)的 对象 xijk 断言, 这只是在 F 的执行终止后, xijk 要么是不可访问的(如果 F 为 vari 确定一个绑定), 要么是 vari 的当前值的一个另外不可访问部分(otherwise inaccessible part) (如果 F 没有为 vari 确定一个绑定). 每个 fni 都有相同的关系, 除了这些在函数命名空间中的绑定. 

        编译器被允许以任何适合于实现的方式使用该信息, 这与 Common Lisp 的语义不冲突.

        dynamic-extent 声明可以是自由声明或绑定声明.

        在 dynamic-extent 声明中命名的这个 vars 和 fns 一定不能引用符号宏或宏绑定.

* 示例(Examples):

    * 由于初始值的栈上分配需要在对象的创建时知道对象可以是栈上分配的, 对于没有词法上显而易见的初始值的变量, 对变量进行 dynamic-extent 声明通常是不太有用的. 比如, 这可能是很有用的:

        ```LISP
        (defun f ()
        (let ((x (list 1 2 3)))
            (declare (dynamic-extent x))
                ...))
        ```

    * 这将允许那些希望这样做的编译器来栈上分配由本地变量 x 所持有的列表. 这是允许的, 但在实践中可能没有那么有用, 写成:

        ```LISP
        (defun g (x) (declare (dynamic-extent x)) ...)
        (defun f () (g (list 1 2 3)))
        ```

    * 大部分编译器不会在 f 中去栈上分配给 g 的参数因为对于编译器来说, 从 f 中假设关于 g 的事实是违背模块化的. 只有当 g 的定义做了不兼容的修改而一个实现可以重编译 f 时可以合理地在 f 中栈上分配这个列表参数给 g.

    * 这里有另一个例子:

        ```LISP
        (declaim (inline g))
        (defun g (x) (declare (dynamic-extent x)) ...)
        (defun f () (g (list 1 2 3)))

        (defun f ()
        (flet ((g (x) (declare (dynamic-extent x)) ...))
            (g (list 1 2 3))))
        ```

        在上面的例子里, 一些编译器可能会确定优化是可以的, 而另一些可能不会.

    * 这个的一个变体是 "栈上分配剩余列表"(stack allocated rest list) 可以通过下面来实现 (支持优化的实现中):

        ```LISP
        (defun f (&rest x)
        (declare (dynamic-extent x))
        ...)
        ```

        注意, 虽然 x 的初始值不是显式的, 但是 f 函数负责将列表 x 从传递的参数中组合起来, 因此可以通过编译器对 f 函数进行优化, 以构建一个栈上分配的列表, 而不是在支持这样的实现中使用堆分配的列表.

    * 在下面的示例中,

        ```LISP
        (let ((x (list 'a1 'b1 'c1))
            (y (cons 'a2 (cons 'b2 (cons 'c2 nil)))))
        (declare (dynamic-extent x y))
        ...)
        ```

        这个 x 的另外不可访问部分是三个 cons, 而 y 的另外不可访问部分是另外三个 cons. 所有 a1, b1, c1, a2, b2, c2, 或者 nil 这些符号中没有是 x 或 y 的另外不可访问部分, 因为每一个都被捕捉因此在它被捕捉的包(或者多个包)中是可访问的. 然而, 如果使用了一个新分配的未捕捉的符号, 那么它将是包含它的列表的另外不可访问部分.

        ```LISP
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
        ```

    * 下面的是错误的, 因为 x 的值在它的范围外被使用:

        ```LISP
        (length (list (let ((x (list 1 2 3)))  ; Invalid
                        (declare (dynamic-extent x))
                        x)))

        (progn (let ((x (list 1 2 3)))  ; Invalid
                (declare (dynamic-extent x))
                x)
                nil)
        ```

* 也见(See Also):

        declare

* 注意(Notes):

        最常见的优化是对由 vars 命名的对象的初始值进行栈上分配.

        一个实现允许完全可以忽略这个声明. 

### <span id = "DeclarationTYPE">声明 TYPE</span>

* 语法(Syntax):

        (type typespec var*)

        (typespec var*)

* 参数(Arguments):

        typespec---一个类型说明.
        var---一个变量名字.

* 合法上下文(Valid Context):

        declaration 或者 proclamation

* 绑定类型的影响(Binding Types Affected):

        variable

* 描述(Description):

        只影响变量绑定, 并指定 vars 只接受指定的 typespec 的值. 具体来说, 由 setq 分配给变量的值, 以及 vars 的初始值必须是指定的 typespec. 类型声明从来不应用于函数绑定 (见 ftype).

        一个被 symbol-macrolet 定义的类型声明等价于在该符号的展开周围封装这样一个表达式, 尽管这个符号的宏展开实际上并没有受到影响.

        类型声明的意义等价于修改声明作用域里的每一个变量 (var) 的引用为 (the typespec var), 修改声明作用域里的每一个赋值给变量 (new-value) 的表达式为 (the typespec new-value), 并且在进入声明作用域的时候执行 (the typespec var).

        在所有声明中一个类型声明是合法的. 对类型声明的解释如下:

        1. 在对声明范围内的声明变量的任何引用执行期间, 如果声明的变量的值不是声明的类型, 后果是未定义的.
        2. 在声明的范围内执行声明变量的任何 setq 时, 如果声明的变量的新的赋值不属于声明的类型, 那么后果将是未定义的.
        3. 当进入声明的范围时, 如果声明的变量的值不是已声明的类型, 那么后果将是未定义的.

        一个类型声明只影响它的作用域内的变量引用.

        如果嵌套类型声明引用相同的变量, 那么该变量的值必须是声明类型的交集的成员.

        如果对于一个动态变量这里有一个局部类型声明, 并且对于相同的变量这里也有一个全局的类型公告, 那么在局部声明的作用域中的那个变量的值必须是两种类型声明的交集.

        类型声明可以是自由声明或绑定声明.

        符号既不能是类型的名称, 也不能是声明的名称. 在定义一个符号为 class, structure, condition, 或 type 名字时, 如果这个符号已经被声明为一个声明的名字, 或反过来, 都会发出一个错误.

        在一个数组类型声明的词法作用域中, 所有对数组元素的引用都被假定为满足表达的数组元素类型 (与升级的数组元素类型相反). 编译器可以在数组类型声明的范围内处理代码, 就好像数组元素的每个访问都被合适的这个表达式形式包围一样.

* 示例(Examples):

    ```LISP
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
    ```

    * 上面的 frob 定义等价于:

        ```LISP
        (defun frob (an-array)
        (setf (the (signed-byte 5) (aref an-array 1)) 31)
        (setf (the (signed-byte 5) (aref an-array 2)) 127)
        (setf (the (signed-byte 5) (aref an-array 3))
                (* 2 (the (signed-byte 5) (aref an-array 3))))
        (let ((foo 0))
            (declare (type (signed-byte 5) foo))
            (setf foo (the (signed-byte 5) (aref an-array 0)))))
        ```

    * 给定一个实现,其中 fixnums 是29位，但是 fixnum 数组被升级到有符号的32位数组, 下面可以用所有的 fixnum 算法来编译:

        ```LISP
        (defun bump-counters (counters)
        (declare (type (array fixnum *) bump-counters))
        (dotimes (i (length counters))
            (incf (aref counters i))))
        ```

* 也见(See Also):

        declare, declaim, proclaim

* 注意(Notes):

    * (typespec var\*) 是 (type typespec var\*) 的一个缩写.

    * 对于一个函数的参数的类型声明并不一定意味着结果的类型. 下面的函数不允许使用依赖于实现的 fixnum-only 算法来编译:

        ```LISP
        (defun f (x y) (declare (fixnum x y)) (+ x y))
        ```

    * 为说明原因, 细想 (f most-positive-fixnum 1). Common Lisp 定义这个 F 必须返回一个 bignum, 而不是发出一个错误或产生一个数学上不正确的结果. 如果你有特殊的知识, 那么 "fixnum overflow" 情况就不会出现, 您可以在 fixnum 范围内声明结果值, 使一些编译器可以使用更有效的算法:

        ```LISP
        (defun f (x y)
        (declare (fixnum x y))
        (the fixnum (+ x y)))
        ```

    * 但是, 请注意, 在三个参数的情况下, 由于隐式中间值增长的可能性太大, 下面的内容不会导致使用依赖于实现的 fixnum-only 算法:

        ```LISP
        (defun f (x y)
        (declare (fixnum x y z))
        (the fixnum (+ x y z)))
        ```

    * 为说明原因, 细想 (f most-positive-fixnum 1 -1). 尽管参数和结果都是 fixnums，但中间值不是 fixnum. 如果在提供它的实现中选择依赖于实现的 fixnum-only 算法是很重要的, 那么考虑编写这样的代码:

        ```LISP
        (defun f (x y)
        (declare (fixnum x y z))
        (the fixnum (+ (the fixnum (+ x y)) z)))
        ```

### <span id = "DeclarationINLINENOTINLINE">声明 INLINE, NOTINLINE</span>

* 语法(Syntax):

        (inline function-name*)

        (notinline function-name*)

* 参数(Arguments):

        function-name---一个函数名字.

* 合法上下文(Valid Context):

        declaration 或 proclamation

* 绑定类型的影响(Binding Types Affected):

        function

* 描述(Description):

        inline 指定对于编译器需要去为 function-name 命名的函数产生内联调用; 这就是说, 指定的函数名的代码应该集成到调用例程中, 内联出现并替换程序调用. 一个编译器可以自由地忽略这个声明. inline 声明从不应用于变量.

        如果其中一个函数有一个词法上明显的局部定义 (像是被 flet 或 labels), 那么这个声明应用于这个局部定义而不是那个全局函数定义.

        虽然没有符合规范的实现需要执行用户定义函数的内联展开, 那些实现试图识别以下范例:

        去定义一个函数 f 默认不是内联但是对于 (declare (inline f)) 会使 f 是局部内联的, 合适的定义是:

    ```LISP
        (declaim (inline f))
        (defun f ...)
        (declaim (notinline f))
    ```

        在 defun 表达式前面的这个 inline 的公告确保编译器有机会保存内联展开所必需的信息, 并且跟在 defun 后面的这个 notinline 公告防止 f 在任何地方被内联展开.

        notinline 指明这个被 function-name 命名的函数不需要被内联编译. 一个编译器可以自由地忽略这个声明; 这个指定的函数的调用必须被实现为非内联调用.

        如果其中一个函数有一个词法上明显的局部定义 (像是被 flet 或 labels), 那么这个声明应用于局部定义而不是全局的函数定义.

        在对 function-name 进行编译器宏定义的情况下, notinline 声明阻止了编译器宏的使用. 可以使用 inline 声明来鼓励使用编译器宏定义. inline 和 notinline 声明在 function-name 在词法可见的定义是一个宏定义时是没有效果的.

        inline 和 notinline 声明可以是自由声明或绑定声明. 出现在一个 flet 或 labels 表达式形式的主体前的 inline 和 notinline 函数声明是绑定声明. 这样的声明在其他上下文中是自由声明.

* 示例(Examples):

    ```LISP
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
    ```

* 也见(See Also):

        declare, declaim, proclaim 

### <span id = "DeclarationFTYPE">声明 FTYPE</span>

* 语法(Syntax):

        (ftype type function-name*)

* 参数(Arguments):

        function-name---一个函数名字.
        type---一个类型说明符.

* 合法上下文(Valid Context):

        declaration 或 proclamation

* 绑定类型的影响(Binding Types Affected):

        function

* 描述(Description):

        指定被 function-names 命名的函数是 type 函数类型. 比如:

    ```LISP
    (declare (ftype (function (integer list) t) ith)
            (ftype (function (number) float) sine cosine))
    ```

        如果其中一个函数有一个词法上明显的局部定义 (像是被 flet 或 labels), 那么这个声明应用于局部定义并且不是全局函数定义. ftype 声明从不应用于变量绑定 (见 type).

        这个 function-names 的词法上明显的绑定不能是宏定义. (这是因为 ftype 声明每个函数名的函数定义是一个特定的函数子类型, 而宏不表示函数.)

        ftype 声明可以是自由声明或绑定声明. 出现在一个 flet 或 labels 表达式形式的主体前的 ftype 函数声明是绑定声明. 这样的声明在其他上下文中是自由声明.

* 也见(See Also):

        declare, declaim, proclaim 

### <span id = "DeclarationDECLARATION">声明 DECLARATION</span>

* 语法(Syntax):

        (declaration name*)

* 参数(Arguments):

        name---一个符号.

* 绑定类型的影响(Binding Types Affected): None.

* 合法上下文(Valid Context):

        仅限proclamation

* 描述(Description):

        建议编译器, 每个名称都是有效的, 但可能是非标准的声明名. 这样做的目的是告诉一个编译器不要发出针对用于另一个编译器或其他程序处理器的声明的警告.

* 示例(Examples):

    ```LISP
    (declaim (declaration author target-language target-machine))
    (declaim (target-language ada))
    (declaim (target-machine IBM-650))
    (defun strangep (x)
    (declare (author "Harry Tweeker"))
    (member x '(strange weird odd peculiar)))
    ```

* 也见(See Also):

        declaim, proclaim 

### <span id = "DeclarationOPTIMIZE">声明 OPTIMIZE</span>

* 语法(Syntax):

        (optimize {quality | (quality value)}*)

* 参数(Arguments):

        quality---一个优化质量.
        value--- 0, 1, 2, 或 3 这些整数的其中之一.

* 合法上下文(Valid Context):

        declaration 或 proclamation

* 绑定类型的影响(Binding Types Affected): None.

* 描述(Description):

        建议编译器应该根据指定的相应值给予每个质量的关注. 每一种质量都必须是一种名为优化质量的符号; 标准优化质量的名称和含义在下一段中展示.

            Name               Meaning                            
            compilation-speed  speed of the compilation process   
            debug              ease of debugging                  
            safety             run-time error checking            
            space              both code size and run-time space  
            speed              speed of the object code           

            Figure 3-25. 优化质量

        这里可能有其他的, 具体实现定义的优化质量.

        一个 0 值意味着对应的质量是完全不重要的, 而这个 3 表示极其重要的; 1 和 2 是中间的值, 这里 1 是中立的值. (quality 3) 可以缩写成 quality.

        注意有着优化 (safety 3), 或者只是 safety 的代码, 成为安全代码 (safe code).

        如果 quality 以超过一种不同的值出现那么后果是不可预料的.

* 示例(Examples):

    ```LISP
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
    ```

* 也见(See Also):

        declare, declaim, proclaim, Section 3.3.4 (Declaration Scope)

* 注意(Notes):

        一个优化声明从不适用于一个变量或一个函数绑定. 一个优化声明只能是自由声明. 关于更多信息, 见章节 3.3.4 (Declaration Scope). 

### <span id = "DeclarationSPECIAL">声明 SPECIAL</span>

* 语法(Syntax):

        (special var*)

* 参数(Arguments):

        var---一个符号.

* 合法上下文(Valid Context):

        declaration 或 proclamation

* 绑定类型的影响(Binding Types Affected):

        variable

* 描述(Description):

        指定所有 var 命名的变量是动态的. 这个声明符影响变量绑定和引用. 所有受影响的变量绑定都成为动态绑定, 并且受影响的变量引用指向当前的动态绑定. 比如:

    ```LISP
        (defun hack (thing *mod*)    ;The binding of the parameter
        (declare (special *mod*))  ; *mod* is visible to hack1,
        (hack1 (car thing)))       ; but not that of thing.
        (defun hack1 (arg)
        (declare (special *mod*))  ;Declare references to *mod*
                                    ;within hack1 to be special.
        (if (atom arg) *mod*
            (cons (hack1 (car arg)) (hack1 (cdr arg)))))
    ```

        一个 special 声明不影响一个 var 的内部绑定; 内部绑定隐式地遮蔽一个 special 声明, 并且必须显式地重新声明为 special 声明. special 声明从不应用于函数绑定.

        special 声明可以是绑定声明, 影响绑定和引用, 或自由声明, 指影响引用, 取决于声明是否关联到一个变量绑定.

        当使用一个公告, 一个 special 声明符应用于所有的绑定以及所有提到的变量的引用. 比如, 在

        (declaim (special x))

        后面有一个这样的函数定义

        (defun example (x) ...)

        这个参数 x 被绑定为一个动态变量而不是一个词法变量.

* 示例(Examples):

    ```LISP
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
    ```

        这个例子第一行对 *foo* 的引用不是 special 即便在第二行有一个 special 声明.

    ```LISP
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
    ```

        在上面的扭曲代码中, y 的最外层和最内层的绑定是动态的, 但是中间绑定是词法. 给 + 的两个参数是不一样的, 一个是一个值, 3, 是词法变量 y 的, 并且另一个是动态变量 y 的值 (巧合的是, 一个绑定在词法上围绕在它外层). 然而, 由于这个公告 x 总是是 special, 所有 x 的绑定和 x 的引用都是动态的.

* 也见(See Also):

        defparameter, defvar 

### <span id = "SpecialOperatorLOCALLY">特殊操作符 LOCALLY</span>

* 语法(Syntax):

        locally declaration* form* => result*

* 参数和值(Arguments and Values):

        Declaration---一个声明表达式; 不求值.
        forms---一个隐式的 progn.
        results---这个表达式形式的值.

* 描述(Description):

        在一个词法环境[lexical environment]中, 在给定的声明具有效果的情况下, 对表达式中的主体进行求值.

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declare

* 注意(Notes):

        locally 可以和 special 声明一起使用来影响引用, 而不是对变量的绑定.

        如果一个 locally 表达式是一个顶层表达式, 这个 body 表达式形式也被当作顶层表达式处理. 见章节 3.2.3 (File Compilation). 

### <span id = "SpecialOperatorTHE">特殊操作符 THE</span>

* 语法(Syntax):

        the value-type form => result*

* 参数和值(Arguments and Values):

        value-type---一个类型说明符; 不求值.
        form---一个表达式形式; 求值的.
        results---从表达式形式的求值得出的值. 这些值必须符合 value-type 所提供的类型; 见下文.

* 描述(Description):

        the 指定 form 返回的值是 value-type 表示的类型. 如果没有声明类型的结果, 后果是没有定义的.

        如果声明类型的值确实是这些类型的值, 那么 form 可以产生不同于 value-type 指定的数量的值, 这是允许的. 出于检查它们的类型的目的, 缺少的值被当作 nil.

        不考虑 value-type 声明的值的数量, 这个特殊表达式返回到值的数量和 form 返回的值的数量一样.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果表达式产生的值不是由 value-type 指定的类型，那么后果是没有定义的.

* 也见(See Also):

        values

* 注意(Notes):

        值类型说明符可以用来表示多个值的类型:

    ```LISP
    (the (values integer integer) (floor x y))
    (the (values string t)
        (gethash the-key the-string-table))
    ```

        setf 可以和 the 类型声明一起使用. 在这种情况下，这个声明被转换为指定新值的表达式形式. 然后分析产生的 setf 表达式. 

### <span id = "FunctionSPECIALOPERATORP">函数 SPECIAL-OPERATOR-P</span>

* 语法(Syntax):

        special-operator-p symbol => generalized-boolean

* 参数和值(Arguments and Values):

        symbol---一个符号.
        generalized-boolean---一个普通的 boolean.

* 描述(Description):

        如果 symbol 是一个特殊操作符就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (special-operator-p 'if) =>  true
    (special-operator-p 'car) =>  false
    (special-operator-p 'one) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个符号应该发出一个 type-error 错误.

* 也见(See Also): None.

* 注意(Notes):

        从历史观点上说, 这个函数被称为 special-form-p. 这个名字最终被声明为用词不当并且修改, 因为它是对特殊操作符返回 true, 而不是特殊表达式. 

### <span id = "FunctionCONSTANTP">函数 CONSTANTP</span>

* 语法(Syntax):

        constantp form &optional environment => generalized-boolean

* 参数和值(Arguments and Values):

        form---一个表达式形式.
        environment---一个环境对象. 默认是 nil.
        generalized-boolean---一个普通的 boolean.

* 描述(Description):

        如果 form 可以被实现确定为在指定的环境(environment)中的一个常量形式，则返回true; 否则, 它返回 false 表示表达式不是一个常量形式或者它不能确定是否表达式形式是一个常量形式.

        下面这些种类的表达式形式被当作常量形式:

        * 自求值对象 (像数字, 字符, 以及各种类型的数组) 总是被当作常量形式并且一定被 constantp 识别.

        * 常变量, 像关键字还有 Common Lisp 定义作为常量的符号 (像 nil, t, 还有 pi), 还有在指定的环境中, 使用 defconstant 的用户声明为常量的符号总是被认为是常量形式, 因此必须被 constantp 所识别.

        * quote 表达式总是被当作常量并且一定被 constantp 所识别.

        * 一个实现允许, 但不是必须去检测额外的常量表达式形式. 如果确实如此, 那么使用环境(environment)中信息也是允许的, 但不是必需的. 对于常量形式 constantp 可能或可能不会返回 true 的例子是: (sqrt pi), (+ 3 2), (length '(a b c)), 还有 (let ((x 7)) (zerop x)).


        如果一个实现选择使用环境(environment)信息, 可以使用诸如展开宏或执行函数内联之类的操作，但不是必须; 然而, 展开编译器宏是不允许的.

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        全局环境的状态 (比如, 这些被声明为常数变量的名字的符号).

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defconstant

* 注意(Notes): None. 

