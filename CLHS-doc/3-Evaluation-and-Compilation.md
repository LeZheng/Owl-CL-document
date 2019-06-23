# 3. 编译和求值

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

一个环境[environment]中的绑定[binding]用命名空间[namespace]来划分. 单独的名字在每一个环境[environment]中可以同时有超过一个关联的绑定[binding], 每一个命名空间[namespace]只能有一个关联的绑定[binding].

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

在一个程序[program]中, 某个位置的求值[evaluation]的词法环境[lexical environment]是环境[environment]中包含了特定信息的部分, 这些信息有着包含这个位置的那些表达式形式[form]中的词法作用域[lexical scope]. 一个词法环境[lexical environment]包含以下内容:

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

用作一个表达式形式[form]的一个 cons, 被称为为复合表达式形式[compound form].

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

一个 lambda 表达式[lambda expression]的主体是一个隐式 progn [implicit progn]; 它返回的值会被这个 lambda 表达式[lambda expression]返回. 

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

术语字面化[literal]对象[object]指的是一个引用的对象[object]或者一个自求值对象[self-evaluating object]或者一个这样的对象的底层结构对象[object]. 一个常变量[constant variable]自身不是一个字面化[literal]对象[object].

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

术语编译期[compile time]指的是编译器处理源代码[source code]的那段时间. 在编译期[compile time], 只有编译环境[compilation environment]和求值环境[evaluation environment]可用.

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

编译器宏[compiler macro]可以定义给一个名字[name], 这个名字[name]同时命名一个函数[function]或宏[macro]. 这也就是说, 一个函数名[function name]可能同时命名函数[function]和编译器宏[compiler macro].

如果 compiler-macro-function 对于出现在词法环境[lexical environment]中的一个函数名[function name]返回 true, 这个函数名[function name]命名一个编译器宏[compiler macro]. 为这个函数名[function name]创建一个词法绑定[lexical binding]不止创建一个局部函数[function]或宏[macro]定义, 并且也遮蔽[shadows[2]]了这个编译器宏[compiler macro].

这个 compiler-macro-function 返回的函数[function]是一个两个参数的函数[function], 称为展开函数(expansion function). 为了展开一个编译器宏[compiler macro], 通过用这个展开函数作为第一个参数, 整个编译器宏表达式形式[form]作为第二个参数, 当前编译环境(或者如果这个表达式形式[form]被 compile-file 以外的其他处理过, 那么就是当前的词法环境[environment])作为第三个参数来调用宏展开钩子[macroexpand hook]进而调用这个展开函数. 宏展开钩子[macroexpand hook]反过来将这个表达式形式[form]作为第一个参数而环境[environment]作为第二个参数来调用这个展开函数. 展开函数的返回值, 由宏展开钩子函数传递, 可能是相同的[same]表达式形式[form], 或者是另一种表达式形式[form], 由执行展开的代码[code]决定, 被用于替换到原始表达式形式[form].

    *macroexpand-hook*  compiler-macro-function  define-compiler-macro  

Figure 3-6. 应用于编译器宏的定义的名字

##### 3.2.2.1.1 编译器宏的目的

这个编译器宏[compiler macro]机制的目的是允许选择性的源代码转换为编译器的优化建议. 当一种复合表达式形式[compound form]被处理(例如被编译器), 如果这个操作符[operator]命名了一个编译器宏[compiler macro], 那么这个编译器宏函数[compiler macro function]可能在这个表达式形式[form]上被调用, 而产生的展开进行递归处理优先于在原始表达式形式[form]上根据它作为函数表达式形式[function form]或宏表达式形式[macro form]的解释执行常规处理.

一个编译器宏函数[compiler macro function], 就像一个宏函数[macro function], 是一个两个实参[argument]的函数[function]: 整个调用的表达式形式[form]和那个环境[environment]. 不像一个普通的宏函数[macro function], 一个编译器宏函数[compiler macro function]可以通过只返回与原始表达式形式[form]相同的值来拒绝提供展开式. 如果编译器宏函数[compiler macro function]破坏性地修改的它的表达式形式[form]参数的任何部分, 那么后果是未定义的.

传递给编译器宏函数[compiler macro function]的表达式形式[form]可以是一个 car 部分为一个函数名的列表[list]或者一个 car 部分是 funcall 并且 cadr 部分是一个列表 (function name) 的列表; 注意, 这会影响编译器宏函数[compiler macro function]对表达式形式参数的解构. define-compiler-macro 会为两种可能的格式的参数正确解构做准备.

当 compile-file 选择展开一个是编译器宏[compiler macro]表达式形式[form]的顶层表达式形式[top level form]时, 那么展开式出于 eval-when 处理的目的也被当作顶层表达式形式[top level form]来处理; 见章节 3.2.3.1 (顶层表达式形式的处理). 

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

如上所述, 虽然从技术上允许 eval 在和编译器[compiler]相同的情况下去处理编译器宏[compiler macro], 但是这在解释型具体实现[interpreted implementation]中并不一定是个好主意.

编译器宏[compiler macro]的存在是为了用编译时速度换取运行时速度的目的. 编写编译器宏[compiler macro]的程序员倾向于假设编译器宏[compiler macro]比正常函数[function]和宏[macro]花费更多的时间进而来生成用于运行时的最佳的代码. 由于 eval 在解释型具体实现[interpreted implementation]中可能多次执行相同表达式形式的语义分析, 因此具体实现[implementation]在每次这样的求值[evaluation]中选择调用编译器宏[compiler macro]可能会很低效.

然而, 关于在这些情况下应该做什么的决定留给每个具体实现[implementation]. 

#### 3.2.2.2 <span id = "MinimalCompilation">最小化编译</span>

最小化编译[minimal compilation]根据如下定义:

    在编译时, 正在编译的源代码[source code]中出现的所有编译器宏[compiler macro]调用都被展开; 它们不会在运行时展开.

    正在编译的源代码中出现的所有宏[macro]和符号宏[symbol macro]调用都在编译时进行展开, 这样它们就不会在运行时再次被展开. macrolet 和 symbol-macrolet 实际上被替换为与它们的主体相对应的表达式, 在这里, 对宏[macro]的调用被它们的展开式所取代.

    在被 compile 处理的源代码[source code]中一个 load-time-value 表达式形式[form]的第一个实参[argument]在编译期[compile time]被求值; 在被 compile-file 处理的源代码[source code]中, 编译器安排它在加载时[load time]被求值. 不论发生何种情况, 这个求值[evaluation]的结果会被记住并且在执行时[execution time]被用于后面 load-time-value 表达式形式[form]的值. 

#### 3.2.2.3 <span id = "SemanticConstraints">语义约束</span>

所有符合规范的程序[conforming program]必须遵守以下约束, 这些被设计用于减少编译和解释的程序的可观测差异:

  * 任何被引用的宏[macro]的定义必须出现在编译环境[compilation environment]中. 任何以一个不是命名该编译环境[compilation environment]中定义的特殊操作符[special operator]或宏[macro]的符号[symbol]开始的列表[list]表达式形式[form]会被编译器当作一个函数调用.

  * 动态变量[dynamic variable]的 special 公告必须在编译环境[compilation environment]中做出. 在编译环境[compilation environment]中没有 special 声明或公告的任何绑定[binding]都被编译器视为词法绑定[lexical binding].

  * 在编译环境[compilation environment]中定义和声明为 inline 的函数的定义在运行时必须是相同的.

  * 在一个名为 F 的函数[function]中, 这个编译器可能 (但不是必须) 假定一个对名为 F 的函数的明显递归调用指向 F 的相同定义, 除非这个函数已经被声明为 notinline. 在函数[function] F 执行时, 重定义这样一个递归定义函数[function] F 的后果是没有定义的.

  * 在一个文件中对一个定义在相同文件中的已命名函数的调用指的就是那个函数, 除非该函数被声明为 notinline. 如果函数在运行时被单独重定义或者在一个相同文件里多次定义, 那么后果是未指定的.

  * 所有在编译时声明 ftype 的函数的参数语法和返回值数量必须在运行时保持不变.

  * 在编译环境[compilation environment]中定义的常变量[constant variable]在运行时必须具有相似[similar]的值. 源代码[source code]中对一个常变量[constant variable]的引用等价于对一个常变量[constant variable]值[value]的字面化[literal]对象[object]的引用.

  * 用 deftype 或者 defstruct 在编译环境[compilation environment]中所做的类型定义必须在运行时保持相同的定义. 由 defclass 在编译环境[compilation environment]中定义的类必须在运行时被定义有着相同的超类[superclasse]和元类[metaclass].

    这个也就意味着类型指定符[type specifier]的子类型[subtype]/超类型[supertype]关系必须在编译期[compile time]和运行期[run time]保持不变.

  * 在编译环境[environment]中出现的类型声明必须准确地描述在运行时的对应值; 否则, 后果就是未定义的. 在编译时, 允许一个未知类型[type]出现在一个声明中, 但是在这种情况下可能会发出一个警告.

  * 除了上面显式列出的情况外, 在求值环境[evaluation environment]中定义的一个函数[function]允许在运行时具有不同的定义或不同的签名, 并且运行时定义生效.

不应该使用额外的关于运行时环境[environment]与启动、求值和编译环境[environment]之间一致性的任何假设来编写符合规范的程序[conforming programs].

除非另行注明, 当一个编译期定义和运行时不同, 会在运行时出现以下情况的一种:

  * 发出一个 error 类型[type]的错误
  * 那个编译器定义生效
  * 那个运行时定义生效

如果编译器[compiler]处理一个在编译时没有定义操作符[operator]的函数表达式形式[function form], 那么编译时就不会发出错误. 

### 3.2.3 <span id = "FileCompilation">文件编译</span>

函数[function] compile-file 对一个文件中的表达式形式[form]根据章节 3.2.2 (编译语义) 中的规则执行编译, 并且产生一个可以通过 load 被载入的输出文件.

通常情况下, 在 compile-file 编译的文件中出现的顶层表达式形式[top level form]只在加载编译后文件时才进行求值, 而不是在编译文件时进行求值. 但是, 通常情况下, 文件中的某些表达式需要在编译时进行求值, 以便能够正确地读取和编译文件的其余部分.

这个 eval-when 特殊表达式[special form]可以被用于控制一个顶层表达式形式[top level form]是否在编译时, 加载时, 或者两个时机都求值. 用 eval-when 指定这三种情况的任何几个都是可以的, 通过符号 :compile-toplevel, :load-toplevel, 还有 :execute 来表示. 对于顶层的 eval-when 表达式形式, :compile-toplevel 表示编译器必须在编译期求值这个主体部分, 而 :load-toplevel 表示这个编译器必须安排在加载时求值这个主体. 关于非顶层的 eval-when 表达式, :execute 表示这个主体必须在运行时环境[environment]被执行.

这种表达式形式[form]的行为可以从 compile-file 如何处理一个要被编译的文件中的表达式形式的模型的方面来更精确地理解. 这个有两种处理模式, 称之为 "not-compile-time" 还有 "compile-time-too".

文件中的连续表达式形式被 compile-file 读取并且在 not-compile-time 模式下被处理; 在这个模式下, compile-file 安排表达式形式只在加载时被求值而不是在编译时. 当 compile-file 在 compile-time-too 模式下时, 表达式形式在加载和编译时都求值.

#### 3.2.3.1 顶层表达式形式的处理

在文件编译器中对顶层表达式形式[top level form]的处理定义如下:

* 如果这个表达式形式[form]是一个编译器宏表达式形式[compiler macro form] (没有被一个 notinline 声明[declaration]所禁用), 具体实现[implementation]可能也可能不选择计算该表达式形式[form]的编译器宏展开式[compiler macro expansion], 并且执行那个展开式, 可能也可能不会选择在相同的处理模式(compile-time-too 或 not-compile-time)下将结果作为顶层表达式形式[top level form]进行处理. 如果它拒绝获取或使用展开式, 它必须处理原始的表达式[form].

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

eval-when 表达式形式[form]导致的编译期求值仅限顶层. 对于非顶层表达式形式[non-top-level form] :compile-toplevel 和 :load-toplevel 情况声明都会被忽略. 对于非顶层表达式形式[non-top-level form], 一个指定了 :execute 情况的 eval-when 被当作一个包括 eval-when 表达式形式[form]的主体部分的隐式 progn[implicit progn]; 否则, 在主体中的表达式形式[form]就被忽略.

##### 3.2.3.1.1 定义宏的处理

出现在一个要被 compile-file 处理的文件中的那些定义宏[macro] (就像 defmacro 或者 defvar), 通常会有编译期副作用, 影响同一个文件[file]中后续表达式形式[form]的编译. 解释这些副作用是如何发生的一个很方便的模型是, 定义宏被展开为一个或多个 eval-when 表达式形式[form], 然后导致编译期副作用发生的这些调用会出现在一个 (eval-when (:compile-toplevel) ...) 表达式形式[form]的主体部分.

编译期的副作用可能会导致对定义的信息的存储方式不同于以"正常"方式处理定义宏时的方式 (要么是解释, 要么是加载已编译的文件).

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

事实上文件编译器[file compiler]在编译后的文件[compiled file]中外部地表示字面化[literal]对象[object]并且在文件[file]被加载时重构这些对象[object]的适合的等价对象, 这也意味着就需要去约束被文件编译器[file compiler]处理的代码[code]中的可以被用作字面化[literal]对象[object]的那些对象[object]的特性.

一个可以被用作要被文件编译器[file compiler]处理的代码[code]中的字面化[literal]对象[object]的对象[object]被称为可外部化对象[externalizable object].
<!--TODO 二位 ??-->
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

    具体实现[implementation]假定额外的, 具体实现定义的[implementation-defined]属性[attribute]必须定义非简单[non-simple]字符[character]是否可以被当作相似的[similar]并且如何被当作相似的[similar].

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

声明[Declaration]提供了一种指定像求值器或者编译器这样的程序处理器使用的信息的方式.

局部声明[local declaration]可以通过 declare 嵌入到可执行的代码中. 全局声明[global declaration], 或者公告[proclamation], 可以通过 proclaim 或者 declaim 来建立.

这个 the 特殊表达式形式[special form]提供了一种简写标记来创建一个关于给定表达式形式[form]的值[type]的类型[type]的局部声明[local declaration].

如果一个程序违反了声明[declaration]和公告[proclamation], 结果是没有定义的.

> * 3.3.1 [最低的声明处理需求](#MDPR)
> * 3.3.2 [声明指定符](#DeclarationSpecifiers)
> * 3.3.3 [声明标识符](#DeclarationIdentifiers)
> * 3.3.4 [声明的作用域](#DeclarationScope)

### 3.3.1 <span id = "MDPR">最低的声明处理需求</span>

通常, 一个具体实现[implementation]可以自由地忽视除了 declaration, notinline, safety 还有 special 以外的声明指定符[declaration specifier].

一个 declaration 声明[declaration]必须抑制关于不识别这个种类的声明[declaration]的警告. 如果一个实现[implementation]不产生关于不识别声明的警告, 它可以安全地忽视了这个声明[declaration].

一个 notinline 声明[declaration]必须被任何支持内联函数或者编译器宏[compiler macro]的具体实现[implementation]所识别进而废弃那些机制. 一个不使用内联函数或者编译器宏[compiler macro]的具体实现[implementation]可能会安全地忽略这个声明[declaration].

一个 safety 声明[declaration]必须总是被识别, 它会提高当前的安全等级. 一个始终进行像 safety 是高的一样处理代码的实现, 可以安全地忽略这个声明.

一个 special 声明[declaration]必须被所有实现[implementation]所处理. 

### 3.3.2 <span id = "DeclarationSpecifiers">声明指定符</span>

一个声明指定符[declaration specifier]是一个表达式[expression], 它可以出现在 declare 表达式或者一个 declaim 表达式形式的顶层, 或者作为给 proclaim 的参数. 它是一个的列表[list], 其中 car 部分为声明标识符[declaration identifier], cdr 部分为根据特定于这个声明标识符[declaration identifier]的规则解释的数据.

### 3.3.3 <span id = "DeclarationIdentifiers">声明标识符</span>

下面这段展示了这个标准定义的所有的声明标识符[declaration identifier].

    declaration     ignore     special  
    dynamic-extent  inline     type     
    ftype           notinline           
    ignorable       optimize            

Figure 3-9. Common Lisp 声明标识符

一个具体实现可以自由地去支持其他(依赖于具体实现的[implementation-defined])声明标识符[declaration identifier]. 如果一个声明标识符[declaration identifier]没有在上面定义, 也没有被具体实现[implementation]所定义, 不是一个类型[type]名字[name], 也没有在 declaration 全局公告[proclamation]中声明, 那么可能会发出一个警告.

#### 3.3.3.1 类型声明的简写标记

类型指定符[type specifier]可以用作声明标识符[declaration identifier]. (type-specifier var\*) 可以当作 (type type-specifier var\*) 的简写. 

### 3.3.4 <span id = "DeclarationScope">声明的作用域</span>

声明[declaration]可以被分成两种类型: 一些适用于变量[variable]或函数[function]的绑定[binding]; 一些则不适用于绑定[binding].

一个出现在绑定表达式形式[form]的头部并且适用于这个表达式形式[form]创建的变量[variable]或函数[function]的绑定[binding]的声明[declaration]称之为绑定声明[bound declaration]; 这个声明[declaration]会影响这个声明[declaration]的作用域[scope]内的该绑定[binding]和任何该绑定的引用.

不是绑定声明[bound declaration]的声明[declaration]称为自由声明[free declarations].

在表达式形式[form] F1 中的一个自由声明[free declarations], 它应用于由某个表达式形式[from] F2 所建立的一个名字[name] N 的绑定[binding], 其中 F1 是 F2 的一个子表达式形式[subform], 那么它只影响 N 在 F1 中的引用; 它不应用于其他在 F1 以外的对 N 的引用, 也不影响 F2 中 N 的绑定[binding]被建立[establish]的方式.

不应用于绑定[binding]的声明[declaration]只能以自由声明[free declarations]的形式出现.

一个绑定声明[bound declaration]的作用域[scope]和它应用的绑定[binding]的词法作用域[lexical scope]相同; 对于特殊变量[special variable], 这意味拥有这个绑定[binding]会拥有的作用域[scope]使得它成为一个词法绑定[lexical binding].<!--TODO 不理解-->

除非明确声明, 一个自由声明[free declaration]的作用域[scope]只包括它出现在头部的表达式形式[form]的主体子表达式形式[subform], 不包括其他的子表达式形式[subform]. 自由声明[free declaration]的作用域[scope]不包括包含这些声明[declaration]的表达式形式[form]所建立的绑定[binding]的初始化表达式形式[initialization form].

一些循环表达式形式[iteration form]包含步进(step), 终止条件(end-test), 或者结果(result)子表达式形式[subform], 这些子表达式形式[subform]也被包含在循环表达式形式[iteration form]中出现的那些声明[declaration]的作用域[scope]内. 具体地说, 涉及的循环表达式形式[iteration form]和子表达式形式[subform]是:

    do, do*: step-forms, end-test-form, and result-forms.
    dolist, dotimes: result-form
    do-all-symbols, do-external-symbols, do-symbols: result-form

#### 3.3.4.1 声明作用域的示例

下面是一个示例, 说明了绑定声明[bound declaration]的作用域[scope].

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

由于 x 在第二行的 special 声明[declaration], 第一个出现的 x 建立[establish]一个 x 的动态绑定[dynamic binding]. 第三个出现的 x 建立[establish]一个 x 的词法绑定[lexical binding] (因为在对应的 let 表达式形式[form]里没有 special 声明[declaration]). 第四个出现的 x 是一个引用第三行建立[establish]的 x 的词法绑定[lexical binding]. 因为在第六行的这个 x 的 special 声明[declaration], 第五个出现的 x 为 let 表达式形式[form]的 body 部分建立一个 x 的动态绑定[dynamic binding]. 第四行的 x 引用没有被第六行的 special 声明[declaration]影响因为这个引用不是在第五行变量[variable] x 的"潜在的词法作用域(would-be lexical scope)"内. 第七行 x 的引用是第五行建立[establish]的 x 的动态绑定[dynamic binding]的一个引用.

这里是另一个示例, 用来介绍自由声明[free declaration]的作用域[scope]. 如下:

```LISP
(lambda (&optional (x (foo 1))) ;[1]
  (declare (notinline foo))     ;[2]
  (foo x))                      ;[3]
```

第一行对 foo 的调用[call]可能会被编译为内联的, 即便第三行对 foo 的调用一定不会的情况下. 这是因为第二行对 foo 的 notinline 声明[declaration]只适用于第三行的主体部分. 为了抑制两个调用[call]的内联, 一种方式是这么写:

```LISP
(locally (declare (notinline foo)) ;[1]
  (lambda (&optional (x (foo 1)))  ;[2]
    (foo x)))                      ;[3]
```

或者, 换种方式:

```LISP
(lambda (&optional                               ;[1]
          (x (locally (declare (notinline foo))  ;[2]
                (foo 1))))                       ;[3]
  (declare (notinline foo))                      ;[4]
  (foo x))                                       ;[5]
```

最后, 这里有一个循环表达式形式[iteration form]中声明[declaration]的作用域[scope]的示例.

```LISP
(let ((x  1))                     ;[1]
  (declare (special x))           ;[2]
    (let ((x 2))                  ;[3]
      (dotimes (i x x)            ;[4]
        (declare (special x)))))  ;[5]
=>  1
```

在这个例子, 第四行的第一个 x 引用是指第三行建立的 x 的词法绑定[lexical binding]. 然而, 出现在第四行的第二个 x 位于第五行的自由声明[declaration]的作用域[scope]内 (因为这个是 dotimes 的结果表达式形式(result-form)) 并且因此引用 x 的动态绑定[dynamic binding]. 

## 3.4 <span id = "LambdaLists">Lambda 列表</span>

一个 lambda 列表[lambda list]是一个列表[list], 它指明了形参[parameter]集合(有时也称为 lambda 变量[lambda variable])和一个用于接收和这些形参[parameter]有关的值[value]的协议.

这里有几种 lambda 列表[lambda list]的类型.

  |    上下文               |                       lambda 列表的种类   |
  |   -   | - |
  |defun 表达式形式[form]             |普通 lambds 列表[ordinary lambda list] |
  |defmacro 表达式形式[form]          |宏 lambda 列表[macro lambda list]    |
  |lambda 表达式[lambda expression]  |普通 lambds 列表[ordinary lambda list]  |
  |flet 局部函数[function]定义        |普通 lambds 列表[ordinary lambda list]  |
  |labels 局部函数[function]定义      |普通 lambds 列表[ordinary lambda list]  |
  |handler-case 子句说明             |普通 lambds 列表[ordinary lambda list]  |
  |restart-case 子句说明             |普通 lambds 列表[ordinary lambda list]  |
  |macrolet 局部宏[macro]定义         |宏 lambda 列表[macro lambda list]    |
  |define-method-combination        |普通 lambds 列表[ordinary lambda list]  |
  |define-method-combination :arguments 选项  |define-method-combination 参数 lambda 列表[define-method-combination arguments lambda list]  |
  |defstruct :constructor 选项       |boa lambda 列表[boa lambda list]     |
  |defgeneric 表达式形式[form]        |广义函数 lambda 列表[generic function lambda list]   |
  |defgeneric 方法[method]子句       |特化 lambda 列表[specialized lambda list]   |
  |defmethod 表达式形式[form]         |特化 lambda 列表[specialized lambda list]  |
  |defsetf 表达式形式[form]           |defsetf lambda 列表[defsetf lambda list]       |
  |define-setf-expander 表达式形式[form] |宏 lambda 列表[macro lambda list]               |
  |deftype 表达式形式[form]           |deftype lambda 列表[deftype lambda list]     |
  |destructuring-bind 表达式形式[form] |解构 lambda 列表[destructuring lambda list]   |
  |define-compiler-macro 表达式形式[form] |宏 lambda 列表[macro lambda list]            |
  |define-modify-macro 表达式形式[form]  |define-modify-macro lambda 列表[define-modify-macro lambda list]  |

Figure 3-10. 要使用的lambda列表的种类

下面这段列出了可应用于 lambda 列表[lambda list]的已定义的名字[defined name].

    lambda-list-keywords  lambda-parameters-limit    

Figure 3-11. 可应用于 lambda 列表的已定义的名字

> * 3.4.1 [普通 lambda 列表](#OrdinaryLambdaLists)
> * 3.4.2 [广义函数 lambda 列表](#GenericFunctionLambdaLists)
> * 3.4.3 [特化的 lambda 列表](#SpecializedLambdaLists)
> * 3.4.4 [宏 lambda 列表](#MacroLambdaLists)
> * 3.4.5 [解构 lambda 列表](#DestructuringLambdaLists)
> * 3.4.6 [Boa Lambda 列表](#BoaLambdaLists)
> * 3.4.7 [Defsetf Lambda 列表](#DefsetfLambdaLists)
> * 3.4.8 [Deftype Lambda 列表](#DeftypeLambdaLists)
> * 3.4.9 [Define-modify-macro Lambda 列表](#DefineMMLambdaLists)
> * 3.4.10 [Define-method-combination 参数 Lambda 列表](#DefineMCArgumentsLambdaLists)
> * 3.4.11 [文档字符串和声明的语法交互](#SIDSD)

### 3.4.1 <span id = "OrdinaryLambdaLists">普通 lambda 列表</span>

一个普通 lambda 列表[ordinary lambda list]被用于描述一个实参[argument]集合如何被普通函数[functioni]所接收. 下面是使用普通 lambda 列表[ordinary lambda list]的已定义的名字[defined name]:

    define-method-combination  handler-case  restart-case  
    defun                      labels                      
    flet                       lambda                      

Figure 3-12. 使用普通 lambda 列表的标准操作

一个普通 lambda 列表[ordinary lambda list]可以包含下面这段展示的 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &key       &rest  
    &aux               &optional         

Figure 3-13. 被普通 lambda 列表使用的 lambda 关键字列表

一个 lambda 列表[lambda list]中的每一个元素[element]是一个参数指定符或者一个 lambda 列表关键字[lambda list keyword]. 具体实现可以自由地提供额外的 lambda 列表关键字[lambda list keyword]. 关于一个实现中使用的 lambda 列表关键字[lambda list keyword], 见 lambda-list-keywords.

普通 lambda 列表[ordinary lambda list]的语法如下:

    lambda-list::= (var* 
                    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                    [&aux {var | (var [init-form])}*]) 

一个 var 或 supplied-p-parameter 必须是一个符号[symbol], 并且不是常变量[constant variable]的名字.

一个 init-form 可以是任何表达式形式[form]. 无论何时为任何参数指定符求值任何 init-form, 那个表达式形式[form]可能引用这个 init-form 出现指定符左边的任何参数变量, 包括任何 supplied-p-parameter 变量, 并且可能依赖于没有其他参数变量已经被绑定的事实(包括它自己的参数变量) .

一个 keyword-name 可以是任何符号[symbol], 但是按照惯例通常是一个关键字[keyword[1]]; 所有标准化[standardized]的函数[function]都遵守这个惯例.

一个普通 lambda 列表[ordinary lambda list]有5个部分, 其中的任何部分或者全部可以是空的. 关于不匹配参数处理的信息, 见章节 3.5 (函数调用中的错误检测).

> * 3.4.1.1 [必要参数的指定符](#SpecifiersRequiredParameters)
> * 3.4.1.2 [可选参数的指定符](#SpecifiersOptionalParameters)
> * 3.4.1.3 [剩余参数的指定符](#SpecifierRestParameter)
> * 3.4.1.4 [关键字参数的指定符](#SpecifiersKeywordParameters)
> * 3.4.1.5 [&aux 变量的指定符](#SpecifiersAuxVariables)
> * 3.4.1.6 [普通 lambda 列表[ordinary lambda list]的示例](#ExamplesOrdinaryLambdaLists)

#### 3.4.1.1 <span id = "SpecifiersRequiredParameters">必要参数的指定符</span>

这些指的是直到第一个 lambda 列表关键字[lambda list keyword]为止的那些参数指定符; 如果这里没有 lambda 列表关键字[lambda list keyword], 那么所有指定符都是必要参数. 每一个必要参数被参数变量 var 指定. var 被绑定为一个词法变量, 除非它被声明为 special.

如果这里有 n 个必要参数 (n 可能是 0), 这里至少必须传递 n 个实参, 并且必要参数绑定给前 n 个传递的实参; 见章节 3.5 (函数调用中的错误检测). 其他参数保留给后面的剩余参数.

#### 3.4.1.2 <span id = "SpecifiersOptionalParameters">可选参数的指定符</span>

如果出现 &optional, 可选参数指定符就是那些跟在 &optional 后面直到下一个 lambda 列表关键字[lambda list keyword]或者直到列表结束的那些指定符. 如果指定了可选参数, 那么每一个都按如下处理. 如果存在未处理的参数, 则参数变量 var 将绑定到下一个的剩余参数, 就像必要参数一样. 如果没有参数剩下, 不管怎样, 那么 init-form 被求值, 并且那个参数变量被绑定给产生的值(如果没有 init-form 出现在参数指定符中就绑定为 nil). 如果另一个变量名 supplied-p-parameter 出现在这个指定符中, 如果有一个参数可用它会被绑定为 true, 如果没有参数剩余它会被绑定为 false (因此 init-form 需要被求值). supplied-p-parameter 不是绑定一个参数而是一个值, 它表示是否为相应的 var 提供了一个对应的参数. 

#### 3.4.1.3 <span id = "SpecifierRestParameter">剩余参数的指定符</span>

&rest, 如果出现, 后面必须跟着单独的剩余参数[rest parameter]指定符, 而那个指定符后面必须跟着另一个 lambda 列表关键字[lambda list keyword]或者 lambda 列表[lambda list]的末尾. 在所有可选参数被处理后, 这里可能由也可能没有一个剩余参数[rest parameter]. 如果这里有一个剩余参数[rest parameter], 它给绑定给一个所有还没有被处理(as-yet-unprocessed)参数的列表[list]. 如果没有未处理参数剩下, 这个剩余参数[rest parameter]绑定给空列表[empty list]. 如果这里没有剩余参数[rest parameter]和关键字参数[keyword parameter], 那么如果有任何未处理参数剩余就会发出一个错误; 见章节 3.5 (函数调用中的错误检测). 允许剩余参数[rest parameter]的值去和给 apply 的最后一个参数共享结构, 但不是必需. 

#### 3.4.1.4 <span id = "SpecifiersKeywordParameters">关键字参数的指定符</span>

如果出现 &key , 所有直到下一个 lambda 列表关键字[lambda list keyword]或者列表[list]末尾的指定符都是关键字参数指定符. 当关键字参数被处理, 同样被处理的参数会被做到一个列表[list]中作为剩余参数[rest parameter]. 允许同时指定 &rest 和 &key. 在这个情况下剩下的参数被同时用于这两种目的; 这就是说, 所有剩下的参数被做到剩余参数[rest parameter]的列表[list]中, 也被当作 &key 参数被处理. 如果指定了 &key, 必须有偶数个参数; 见章节 3.5.1.6 (奇数数量的关键字参数). 这些参数被当作对(pair), 每一对中的第一个参数被解释为一个名字而第二个作为对应的值. 每个对中的第一个对象[object]必须是一个符号[symbol]; 见章节 3.5.1.5 (非法的关键字参数). 这个关键字参数指定符后面可能可选地跟着 lambda 列表关键字[lambda list keyword] &allow-other-keys.

在每一个关键字参数指定符中参数变量必须是一个名字 var. 如果这个 var 单独出现或者在一个 (var init-form) 组合中, 当匹配实参[argument]到形参[parameter]时, 使用的关键字名字是一个 KEYWORD 包中名字[name]和 var 相同[same] (在 string= 下)的符号[symbol]. 如果使用了这个 ((keyword-name var) init-form) 表示法, 那么用于匹配实参[argument]到形参[parameter]的关键字名字是 keyword-name, 它可能是任何包[package]中的符号[symbol] (当然, 如果它不是 KEYWORD 包中的符号[symbol], 它没有必要自求值, 所以当调用这个函数时必须额外关心来确保正常的求值一直产生这个关键字名字). 因此

```LISP
(defun foo (&key radix (type 'integer)) ...)
```

与下面这个意义相同

```LISP
(defun foo (&key ((:radix radix)) ((:type type) 'integer)) ...)
```

关键字参数指定符和所有参数指定符一样, 实际上从左到右进行处理. 对于每一个关键字参数指定符, 如果这里有一个名字匹配这个指定符的名字的实参对 (这就是说, 名字之间是 eq 的), 那么这个指定符的参数变量绑定为这个实参对的第二项(值部分). 如果不止一个这样的参数对匹配, 会使用最左边的实参对. 如果不存在这样的实参对, 那么这个指定符的对应 init-form 被求值并且这个参数变量绑定为那个值 (如果没有指定 init-form , 那就绑定为 nil). supplied-p-parameter 和 &optional 参数一样的处理方式: 如果这里有匹配的实参对它被绑定为 true, 否则就是 false.

除非关键字参数检测被抑制, 一个实参对必须和一个参数指定符名字匹配; 见章节 3.5.1.4 (不识别的关键字参数).

如果关键字参数检测被抑制, 那么允许一个实参对没有匹配的参数指定符, 并且这个实参对会被忽略, 但是如果提供了剩余参数[rest parameter], 这样的一个参数是可以通过剩余参数[rest parameter]访问的. 这些机制的目的是去允许在多个 lambda 表达式[lambda expression]之间共享参数列表并且允许调用者和被调用的 lambda 表达式[lambda expression]去指定这样的共享是可能发生的.

注意如果 &key 出现了, 一个 :allow-other-keys 的关键字参数总是是允许的---不管关联的值是 true 或者 false. 然而, 如果这个值是 false, 其他不匹配的关键字是不接受的 (除非使用了 &allow-other-keys).

此外, 如果接收的参数列表指定了一个会被 :allow-other-keys 标记的普通参数, 那么 :allow-other-keys 同时有它的特殊情况意义(标识是否允许附加的关键字)和它的正常意义(数据流入到提及的函数中).

##### 3.4.1.4.1 抑制关键字参数检测

如果一个函数[function]的 lambda 列表[lambda list]中指定了 &allow-other-keys, 对这个函数[function]的调用中关键字[keyword[2]]实参[argument]检测会被抑制.

如果在一个函数[function]的调用中 :allow-other-keys 参数[argument]是 true, 在这个调用中关键字[keyword[2]]实参[argument]检测是被抑制的.

这个 :allow-other-keys 实参[argument]在所有涉及关键字[keyword[2]]实参[argument]的地方都是允许的, 即便当它关联的值[value]是 false 时.

###### 3.4.1.4.1.1 抑制关键字参数检测的示例

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

#### 3.4.1.5 <span id = "SpecifiersAuxVariables">&aux 变量的指定符</span>

这些不是真的参数. 如果出现这个 lambda 列表关键字[lambda list keyword] &aux, 所有在它后面的指定符都是辅助变量指定符. 在所有参数指定符被处理后, 这些辅助变量指定符(那些跟在 &aux 后面的)从左到右被处理. 对于其中的每一个, init-form 被求值并且 var 被绑定为那个值 (如果没有 init-form 就绑定为 nil). &aux 变量处理类似于 let* 处理.

```LISP
(lambda (x y &aux (a (car x)) (b 2) c) (list x y a b c))
  ==  (lambda (x y) (let* ((a (car x)) (b 2) c) (list x y a b c)))
```

#### 3.4.1.6 <span id = "ExamplesOrdinaryLambdaLists">普通 lambda 列表的示例</span>

这里是可选参数[optional parameter]和剩余参数[rest parameter]的例子:

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

这里是关键字参数[keyword parameter]的例子:

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

这里是同时启用可选参数[optional parameter]和剩余参数[rest parameter]还有关键字参数[keyword parameter]的示例:

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

作为 &allow-other-keys 和 :allow-other-keys 使用的示例, 细想一个函数[function], 它接受它自身的两个已命名的参数, 并接受附加的命名参数, 以将其传递给 make-array:

```LISP
(defun array-of-strings (str dims &rest named-pairs
                        &key (start 0) end &allow-other-keys)
  (apply #'make-array dims
        :initial-element (subseq str start end)
        :allow-other-keys t
        named-pairs))
```

这个函数[function]需要一个字符串[string]和一个维度信息并且返回一个指定维度的数组[array], 它的每一个元素都是指定的字符串[string]. 然而, :start 和 :end 命名的参数可能被用于指定给定字符串[string]中应该使用的子字符串. 另外, 在这个 lambda 列表[lambda list]中出现的 &allow-other-keys 表示调用者可能提供额外的已命名参数; 这个剩余参数[rest parameter]提供对它们的访问. 这些额外已命名的参数被传递给 make-array. 这个 make-array 函数[function]正常不允许使用已命名参数 :start 和 :end, 并且如果这样的已命名参数提供给 make-array 应该会发出一个错误. 然而, 对 make-array 的调用中已命名参数 :allow-other-keys 以及一个 true 值的出现导致任何额外的已命名参数, 包括 :start 和 :end, 是可接受的并且被忽略掉. 

### 3.4.2 <span id = "GenericFunctionLambdaLists">广义函数 lambda 列表</span>

一个广义函数 lambda 列表[generic function lambda list]被用于描述被一个广义函数[generic function]接受的实参列表的整体形状. 个别方法[method]签名[signature]可能为有效方法[effective method]的 lambda 列表[lambda list]提供额外的关键字参数[keyword parameter].

一个广义函数 lambda 列表[generic function lambda list]被 defgeneric 所使用.

一个广义函数 lambda 列表[generic function lambda list]有着以下语法:

    lambda-list::= (var* 
                    [&optional {var | (var)}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]]) 

一个广义函数 lambda 列表[generic function lambda list]可以包含下面这段展示的 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &optional    
    &key               &rest        

Figure 3-14. 广义函数 lambda 列表使用的 lambda 列表关键字

一个广义函数 lambda 列表[generic function lambda list]在以下方面有别于普通 lambda 列表[ordinary lambda list]:

必要参数

    0个或多个必要参数[required parameter]必须被指定.

可选参数和关键字参数

    可选参数[optional parameter]和关键字参数[keyword parameter]可能没有默认的初始值表达式也没有使用 supplied-p 参数.

&aux 的使用

    &aux 的使用是不允许的. 

### 3.4.3 <span id = "SpecializedLambdaLists">特化的 lambda 列表</span>

一个特化的 lambda 列表[specialized lambda list]被用于为一个特定的签名[signature]特化[specialize]一个方法[method]并且去描述匹配这个签名[signature]的那些实参[argument]如何被这个方法[method]接收. 下一段中已定义的名字[defined name]以某种方式使用特化的 lambda 列表[specialized lambda list]; 关于其中的每一个怎样处理的信息见字典条目.

    defmethod  defgeneric    

Figure 3-15. 使用特化的 lambda 列表的标准化操作符

一个特化的 lambda 列表[specialized lambda list]可以包含下面这段中展示的 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &key       &rest  
    &aux               &optional         

Figure 3-16. 特化 lambda 列表使用的 lambda 列表关键字

一个特化的 lambda 列表[specialized lambda list]是语法上等价于一个普通 lambda 列表[ordinary lambda list]除了每一个必要参数[required parameter]可能可选地和一个该形参[parameter]被特化[specialize]的类[class]或者一个对象[object]关联.

    lambda-list::= ({var | (var [specializer])}* 
                    [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                    [&rest var] 
                    [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                    [&aux {var | (var [init-form])}*]) 

### 3.4.4 <span id = "MacroLambdaLists">宏 lambda 列表</span>

一个宏 lambda 列表[macro lambda list]被用于描述下面这段中的这些操作符[operator]定义的宏[macro].

    define-compiler-macro  defmacro  macrolet  
    define-setf-expander                       

Figure 3-17. 使用宏 lambda 列表[macro lambda list]的操作符

在一个环境参数[environment parameter]可能只出现一次(在描述的任何位置)的附加限制下, 一个宏 lambda 列表[macro lambda list]有以下语法:

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

一个宏 lambda 列表[macro lambda list]可以包含下面这段展示的 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &environment  &rest   
    &aux               &key          &whole  
    &body              &optional             

Figure 3-18. 宏 lambda 列表[macro lambda list]使用的lambda列表参数

可选参数[optional parameter] (通过 &optional 引入) 和关键字参数[keyword parameter] (通过 &key 引入) 可以在一个宏 lambda 列表[macro lambda list]中被提供, 就像在普通 lambda 列表[ordinary lambda list]中一样. 两个中的每一个都可能包含默认初始化表达式形式和 supplied-p 参数[supplied-p parameter].

&body 在函数中和 &rest 一样, 但是它可以被用于通知特定的输出格式化和编辑函数: 这个表达式形式[form]的剩余部分被当作一个主体(body), 并且应该相应地缩进. 在任何特定的级别 &body 或者 &rest 只有一个可以被使用; 见章节 3.4.4.1 (lambda 列表的解构). &body 可以出现在一个宏 lambda 列表[macro lambda list]的任何级别; 关于详细情况, 见章节 3.4.4.1 (lambda 列表的解构).

&whole 后面跟着一个绑定为整个宏调用表达式形式的单独变量; 这是这个宏函数[macro function]作为第一个参数收到的值. 如果出现 &whole 和一个跟在后面的变量, 它们必须出现在 lambda 列表的最前面, 在任何其他参数或者 lambda 列表关键字[lambda list keyword]之前. &whole 可以出现在一个宏 lambda 列表[macro lambda list]的任何级别. 在内部级别, 这个 &whole 变量绑定为参数的对应部分, 正如 &rest, 但是不像 &rest, 其他参数也是允许的. 这个 &whole 的使用不影响指定参数的匹配.

&environment 后面跟着一个绑定为一个环境[environment]的单独变量, 这个环境表示这个宏调用被解释时所处的当前词法环境[lexical environment]. 这个环境[environment]应该和 macro-function, get-setf-expansion, compiler-macro-function, 还有 macroexpand (比如) 在计算宏展开式一起使用, 来确保这个编译环境[compilation environment]中建立的任何词法绑定[lexical binding]或定义被考虑进去. &environment 只能出现在宏 lambda 列表[macro lambda list]的顶层, 并且只能出现一次, 但是可以出现在这个列表的任何位置; 这个 &environment 和 &whole 形参[parameter]在这个 lambda 列表[lambda list]的任何其他变量[variable]之前被绑定[bound], 不管 &environment 出现在这个 lambda 列表[lambda list]的什么地方. 绑定为环境参数[environment parameter]的对象[object]具有动态范围[dynamic extent].

解构允许一个宏 lambda 列表[macro lambda list]去表达宏调用语法的结构. 如果没有出现 lambda 列表关键字[lambda list keyword], 那么这个宏 lambda 列表[macro lambda list]是在叶子中包含参数名称的树[tree]. 匹配模式和宏表达式形式[macro form]必须具有兼容的树结构[tree structure]; 这就是说, 它们的树结构[tree structure]必须是等价的, 或者它只能在匹配模式的某些叶节点与宏表达式形式[macro form]的非原子[non-atomic]对象[object]匹配时有所不同. 关于这种求值情况[situation]下的错误检测的信息, 见章节 3.5.1.7 (解构不匹配).

一个解构 lambda 列表[lambda list] (不管在顶层还是嵌入的)可以是点对的(dotted), 以一个参数名结束. 这种情况被视为结束列表的参数名称出现在 &rest 前面.

只有在和 (... &rest var) 或 (... . var) 匹配时, 允许一个宏表达式形式[macro form] (或者是一个宏表达式形式[macro form]的子表达式[subexpression])是一个点对列表. 宏[macro]需要去识别和处理这种情况.

#### 3.4.4.1 lambda 列表的解构

在一个宏 lambda 列表[macro lambda list]中参数名字可以出现的任何地方, 以及普通 lambda 列表[ordinary lambda list]语法中(在章节 3.4.1 (普通 lambda 列表) 描述的)不允许一个列表[list]的地方, 一个解构 lambda 列表[destructuring lambda list]可以出现来替换这个参数名字. 在这样做时, 会与形参匹配的实参被当作一个(可能是点对的)列表[list], 作为一个参数列表, 用作满足内嵌的 lambda 列表[lambda list]中的参数的实参列表. 这就被认为是解构(destructuring).

解构是将一个复合对象[object]分解为它的组件部分的过程, 使用一种简短的声明式语法, 而不是用原始的组件访问函数书写出来. 每一个组件部分绑定给一个变量.

一个解构操作需要一个将要解构的对象[object], 一个指定要提取哪些组件的匹配模式, 以及那些值为那些组件的变量的名称.

##### 3.4.4.1.1 lambda 列表的数据导向解构

在数据导向的解构中, 匹配模式是一个要被分解的类型[type]的简单对象[object]. 无论在哪里提取组件, 在匹配模式中对应地方都会出现一个符号[symbol]; 这个符号[symbol]是变量的名称, 这个变量的值就是那个组件.

###### 3.4.4.1.1.1 lambda 列表的数据导向解构示例

一个匹配模式示例是

```LISP
(a b c)
```

它解构了一个三个元素的列表. 这个变量 a 被赋值为第一个元素, b 给赋值为第二个, 等等. 一个更加复杂的例子是

```LISP
((first . rest) . more)
```

语法简单性和扩展到 lambda 列表导向解构的能力是数据导向解构的重要特性. 

##### 3.4.4.1.2 lambda 列表的 lambda 列表导向解构

树[tree]的数据导向解构的一个延伸是 lambda 列表导向的解构. 这是从三元素的解构模式

```LISP
(first second third)
```

以及下面这个三参数的 lambda 列表的类比中得出的

```LISP
(first second third)
```

如果没有 lambda 列表关键字[lambda list keyword]出现在匹配模式中, 那么 lambda 列表导向的解构和数据导向的结构是相同的. 在这个包含 lambda 列表关键字[lambda list keyword]的匹配模式中的任何列表(不管是一个子列表或是整个匹配模式本身)就会被特别地解释. 这个列表中第一个 lambda 列表关键字[lambda list keyword]左边的那些元素被当作解构匹配模式处理, 像平常一样, 但是列表中剩下的元素被当作函数的 lambda 列表[lambda list]一样处理, 除了在通常需要一个变量的情况下, 允许使用任意的解构匹配模式. 注意, 在不确定的情况下, lambda 列表[lambda list]语法优于解构语法. 因此, 在 &optional 之后, 是一个列表, 它的元素是一个解构匹配模式和一个默认值表达式形式的列表.

每个 lambda 列表关键字[lambda list keyword]在 lambda 列表导向的解构匹配模式中的具体行为如下:

&optional

    每一个后面的元素是一个变量或者一个解构匹配模式, 一个默认值表达式形式和一个 supplied-p 变量的列表. 这个默认值和 supplied-p 可以被省略. 如果这个被解构的列表提前结束, 而它没有一个元素来匹配这个解构匹配模式或子模式, 那么这个默认表达式形式会求值并解构. 如果这个默认表达式形式被使用了, 那么 supplied-p 变量会收到值 nil, 否则就是 t.

&rest, &body

    下一个元素是一个匹配这个列表剩余部分的解构匹配模式. &body 和 &rest 一样但是声明所匹配的是构成表达式形式[form]主体的表达式形式列表. 这下一个元素必须是最后一个除非后面跟着一个 lambda 列表关键字[lambda list keyword].

&aux

    其余的元素根本不是解构匹配模式, 而是辅助变量绑定.

&whole

    下一个元素是一个匹配一个宏里的整个表达式形式的解构匹配模式, 或者内部层级的整个子表达式[subexpression].

&key

    后面跟着的元素是以下其中之一

    一个变量[variable],

    或者一个变量, 一个可选的初始化表达式形式, 和一个可选的 supplied-p 变量的列表.

    或者一个关键字和一个解构匹配模式的列表, 一个可选初始化表达式形式, 和一个可选 supplied-p 变量的列表.

    被解构的列表的其余部分被认为是交替的关键字和值, 并且被适当地分开.

&allow-other-keys

    根据它自身理解. 

### 3.4.5 <span id = "DestructuringLambdaLists">解构lambda列表</span>

一个解构 lambda 列表[destructuring lambda list]被 destructuring-bind 使用.

解构 lambda 列表[destructuring lambda list]与宏 lambda 列表[macro lambda list]密切相关; 见章节 3.4.4 (宏 lambda 列表). 除了 &environment 之外, 一个解构 lambda 列表[destructuring lambda list]可以包含所有其他用于宏 lambda 列表[macro lambda list]的 lambda 列表关键字[lambda list keyword], 并且支持相同方式下的解构. 在宏 lambda 列表[macro lambda list]中嵌套的内部 lambda 列表[lambda list]具有解构 lambda 列表[destructuring lambda list]的语法.

一个解构 lambda 列表[destructuring lambda list]具有以下语法:

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

一个 boa lambda 列表[boa lambda list]是一个语法上像普通 lambda 列表[ordinary lambda list]的 lambda 列表[lambda list], 但是这个是 "根据参数顺序" 风格处理.

一个 boa lambda 列表[boa lambda list]只被 defstruct 表达式形式[form]使用, 在显式指定构造器函数[function]的 lambda 列表[lambda list]的时候 (有时称之为 "boa 构造器(boa constructor)").

在 boa lambda 列表[boa lambda list]中的这些 &optional, &rest, &aux, &key, 还有 &allow-other-keys lambda 列表关键字[lambda list keyword]是被识别的. 这些 lambda 列表关键字[lambda list keyword]的使用方式有别于在普通 lambda 列表[ordinary lambda list]中.

细想这个示例, 它描述了解构(destruct)如何处理它的 :constructor.

```LISP
(:constructor create-foo
        (a &optional b (c 'sea) &rest d &aux e (f 'eff)))
```

这个定义了 create-foo 去作为一个或多个参数的构造器. 第一个参数被用于初始化 a 槽. 第二个参数用于初始化 b 槽. 如果这里没有第二个参数, 那么 defstruct 主体中给定的默认值(如果给了的话)被使用. 第三个参数被用于初始化 c 槽. 如果这里没有任何第三个参数, 那么符号 sea 就会被使用. 任何跟在第三个参数后面的参数被收集到一个列表[list]中然后被用于初始化 d 槽. 如果这里只有三个或更少的参数, 那么 d 槽的内容就是 nil. e 槽没有被初始化; 它的初始化值是具体实现定义的[implementation-defined]. 最后, f 槽被初始化去包含符号 eff. &key 和 &allow-other-keys 参数默认类似于 &optional 参数: 如果在这个 lambda 列表[lambda list]中没有提供默认值, 那么使用 defstruct 主体中给定的默认值(如果给了的话). 举例说:

```LISP
(defstruct (foo (:constructor CREATE-FOO (a &optional b (c 'sea)
                                            &key (d 2)
                                            &aux e (f 'eff))))
  (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))

(create-foo 10) =>  #S(FOO A 10 B 2 C SEA D 2 E implemention-dependent F EFF)
(create-foo 10 'bee 'see :d 'dee) 
=>  #S(FOO A 10 B BEE C SEE D DEE E implemention-dependent F EFF)
```

如果指定了这个表达式形式 ((key var) [default [svar]]) 的关键字参数, 这个槽[slot]的名字[name]和 var 匹配(不是 key).

仔细选择在 b 和 e 情况下采取的行动, 来允许用户指定所有可能的行为. 这个 &aux 变量被用于完全重写主体中给定的默认的初值.

如果没有给一个 aux 变量[aux variable]提供默认值, 那么如果在槽被显式赋值前尝试去读取对应槽[slot]的值的话, 结果是未定义的. 如果这样一个槽[slot]指定了一个 :type 选项, 这种被抑制的初始化并不意味着类型不匹配的情况; 声明的类型只有在槽最终赋值时才需要应用.

在这个定义下, 可以写成下面这样:

```LISP
(create-foo 1 2)
```

而不是

```LISP
(make-foo :a 1 :b 2)
```

并且 create-foo 提供了和 make-foo 不同的默认值.

不对应于槽名但只提供在随后的初始化计算中使用的值的附加参数是允许的. 比如, 在这个定义中

```LISP
(defstruct (frob (:constructor create-frob
                (a &key (b 3 have-b) (c-token 'c) 
                        (c (list c-token (if have-b 7 2))))))
        a b c)
```

这个 c-token 参数只是给 c 槽的初始化提供一个值. 与可选参数[optional parameter]和关键字参数[keyword parameter]相关的这些 supplied-p 参数[supplied-p parameter]也可以使用这种方式. 

### 3.4.7 <span id = "DefsetfLambdaLists">Defsetf Lambda 列表</span>

一个 defsetf lambda 列表[defsetf lambda list]被 defsetf 所使用.

一个 defsetf lambda 列表[defsetf lambda list]遵循以下语法:

```LISP
lambda-list::= (var* 
                [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                [&rest var] 
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                [&environment var] 
```

一个 defsetf lambda 列表[defsetf lambda list]可以包含下面这段展示的这些 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &key       &rest  
    &environment       &optional         

Figure 3-19. Defsetf Lambda 列表使用的 lambda 列表关键字

一个 defsetf lambda 列表[defsetf lambda list]和普通 lambda 列表[ordinary lambda list]的区别仅在于它不允许使用 &aux, 并且它允许使用引入环境参数[environment parameter]的 &environment. 

### 3.4.8 <span id = "DeftypeLambdaLists">Deftype Lambda 列表</span>

一个 deftype lambda 列表[deftype lambda list]被 deftype 所使用.

一个 deftype lambda 列表[deftype lambda list]有着像宏 lambda 列表[macro lambda list]相同的语法, 并且因此可以包含和宏 lambda 列表[macro lambda list]相同的 lambda 列表关键字[lambda list keyword].

一个 deftype lambda 列表[deftype lambda list]和宏 lambda 列表[macro lambda list]的区别仅在于如果没有给一个可选参数[optional parameter]和关键字参数[keyword parameter]提供初始化表达式形式 init-from, 那么这个形参[parametaer]的默认值[value]就是符号[symbol] * (而不是 nil). 

### 3.4.9 <span id = "DefineMMLambdaLists">Define-modify-macro Lambda 列表</span>

一个 define-modify-macro lambda 列表[deftype lambda list]被 define-modify-macro 所使用.

一个 define-modify-macro lambda 列表[deftype lambda list]可以包含下面这段展示的 lambda 列表关键字[lambda list keyword].

    &optional  &rest  

Figure 3-20. Define-modify-macro Lambda 列表使用的 lambda 列表关键字

Define-modify-macro lambda 列表[deftype lambda list]类似于普通 lambda 列表[ordinary lambda list], 但是不支持关键字参数. define-modify-macro 不需要去匹配关键字, 并且一个剩余参数[rest parameter]就足够了. aux 变量[aux variable]也不支持, 因为 define-modify-macro 没有主体表达式来引用这些绑定[binding]. 见宏 define-modify-macro. 

### 3.4.10 <span id = "DefineMCArgumentsLambdaLists">Define-method-combination 参数 Lambda 列表</span>

一个 define-method-combination 参数 lambda 列表[define-method-combination arguments lambda list]被 define-method-combination 的 :arguments 选项所使用.

一个 define-method-combination 参数 lambda 列表[define-method-combination arguments lambda list]可以包含下面这段中展示的 lambda 列表关键字[lambda list keyword].

    &allow-other-keys  &key       &rest   
    &aux               &optional  &whole  

Figure 3-21. Define-method-combination 参数 Lambda 列表使用的 lambda 列表关键字

define-method-combination 参数 lambda 列表[define-method-combination arguments lambda list]类似于普通 lambda 列表[ordinary lambda list], 但是也允许使用 &whole. 

### 3.4.11 <span id = "SIDSD">文档字符串和声明的语法交互</span>

在许多情况下, 文档字符串[documentation string]可以出现在一系列表达式形式之前的一系列 declare 表达式[expression]中.

在这个情况下, 如果字符串[string] S 出现在允许文档字符串[documentation string]的地方并且后面既没有 declare 表达式[expression]或一个表达式形式[form]那么 S 就被认为是一种表达式形式[form]; 否则, S 被当作一个文档字符串[documentation string]. 如果出现不止一个文档字符串[documentation string]那么结果是未定义的. 

## 3.5 <span id = "ErrorChecking">函数调用中的错误检测</span>

### 3.5.1 参数匹配检测

> * 3.5.1.1 [安全和非安全调用](#SafeUnsafeCalls)
> * 3.5.1.2 [参数太少](#TooFewArguments)
> * 3.5.1.3 [参数太多](#TooManyArguments)
> * 3.5.1.4 [不识别的关键字参数](#UnrecognizedKeywordArguments)
> * 3.5.1.5 [非法的关键字参数](#InvalidKeywordArguments)
> * 3.5.1.6 [奇数数量的关键字参数](#OddNumberKeywordArguments)
> * 3.5.1.7 [解构不匹配](#DestructuringMismatch)
> * 3.5.1.8 [调用下一个方法时的错误](#ErrorsWhenCallingNextMethod)

#### 3.5.1.1 <span id = "SafeUnsafeCalls">安全和非安全调用</span>

如果下面中的每一个都是安全[safe]代码[code]或者系统代码[system code] (除了由程序员代码[programmer code]的宏展开[macro expansion]所产生的系统代码[system code]之外)那么这个调用[call]就是安全调用[safe call] :

* 这个调用[call].
* 要被调用的函数[function]的定义.
* 函数性求值[functional evaluation]的点

以下特殊情况需要一些细化:

* 如果被调用的函数[function]是一个广义函数[generic function], 如果下面列出的所有部分都是安全[safe]代码[code]或者系统代码[system code]那么它就被认为是安全的[safe]:

        -- 它的定义 (如果它被显式定义的话).
        -- 所有可应用[applicable]方法[method]的方法[method]定义.
        -- 它的方法组合[method combination]的定义.

* 对于表达式形式 (coerce x 'function), 其中 x 是一个 lambda 表达式[lambda expression], 当 coerce 执行时, 全局环境中优化质量[optimize quality] safety 的值应用于产生的函数[function].

* 对于一个函数[function] ensure-generic-function 的调用, 在作为 :environment 参数[argument]传递的环境[environment]对象[object]中优化质量[optimize quality] safety 的值应用于产生的广义函数[generic function].

* 对于一个用 lambda 表达式[lambda expression]作为实参[argument]的 compile 的调用, 在 compile 被调用时全局环境[global environment]中优化质量[optimize quality] safety 的值应用于编译后的函数[compiled function].

* 对于一个单参数的 compile 调用, 如果函数[function]的原始定义是安全的[safe], 那么产生的编译后的函数[compiled function]也必须是安全的[safe].

* 一个通过 call-next-method 的对方法[method]的调用[call]中, 如果下面的每一个都被认为是安全[safe]代码[code]或者系统代码[system code], 那么这个调用一定被认为是安全的[safe]:

    -- 这个广义函数[generic function]的定义 (如果它被显式定义的话).
    -- 所有可应用[applicable]方法[method]的方法[method]定义.
    -- 方法组合[method combination]的定义.
    -- 方法[method]定义表达式形式[defining form]主体部分的入口点, 即建立 call-next-method 绑定[binding]的地方.
    -- 名字 call-next-method 函数性求值[functional evaluation]的点.

一个不安全调用[unsafe call]就是一个不是安全调用[safe call]的调用[call].

非正式的意图是, 如果已经采取了所有合理的步骤来确保调用[call]是安全的[safe], 即使在涉及到系统代码[system code]的情况下, 程序员[programmer]也可以依赖于一个调用[call]是安全的[safe]. 比如, 如果一个程序员[programmer]从安全[safe]代码[code]中调用 mapcar 并且提供了一个被编译为安全的[safe]函数[function], 那么这个具体实现[implementation]也需要去确保这个 mapcar 是一个安全调用[safe call].

##### 3.5.1.1.1 安全调用的错误检测时间

如果在安全调用[safe call]中发出一个错误, 这个发出[signal]的准确的点是依赖于具体实现的[implementation-dependent]. 具体来说, 它可能在编译时或运行时发出, 如果在运行时发出, 它可能在执行这个调用[call]时, 或之前, 或之后发出. 然而它总是在这个被调用函数[function]的主体执行之前. 

#### 3.5.1.2 <span id = "TooFewArguments">参数太少</span>

不允许对一个函数[function]提供过少的实参[argument]. 过少的参数意味着实参[argument]少于这个函数必要参数[required parameter]的数量.

如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果. 

#### 3.5.1.3 <span id = "TooManyArguments">参数太多</span>

不允许对一个函数[function]提供过多的实参[argument]. 太多的参数意味实参[argument]多于这个函数必要参数[required parameter]加上可选参数[optional parameter]的数量; 然而, 如果这个函数[function]使用 &rest 或者 &key, 它不可能接受过多参数.

如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果. 

#### 3.5.1.4 <span id = "UnrecognizedKeywordArguments">不识别的关键字参数</span>

不允许向一个函数[function]提供一个名字不被识别的关键字参数, 除非就像章节 3.4.1.4.1 (抑制关键字参数检测) 描述的那样抑制关键字参数检测.

如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果. 

#### 3.5.1.5 <span id = "InvalidKeywordArguments">非法的关键字参数</span>

不允许使用一个不是符号[symbol]的名字来给函数提供关键字参数.

如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误, 除非就像章节 3.4.1.4.1 (抑制关键字参数检测) 描述的那样抑制关键字参数检测; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果. 

#### 3.5.1.6 <span id = "OddNumberKeywordArguments">奇数数量的关键字参数</span>

一定不能提供奇数数量的实参[argument]给关键字参数[keyword parameter].

如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误, 除非就像章节 3.4.1.4.1 (抑制关键字参数检测) 描述的那样抑制关键字参数检测; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果. 

#### 3.5.1.7 <span id = "DestructuringMismatch">解构不匹配</span>

当匹配一个解构 lambda 列表[destructuring lambda list]和一个表达式形式[form]时, 这个解构匹配模式和表达式形式[form]必须有着兼容的树结构[tree structur], 像章节 3.4.4 (宏 lambda 列表) 描述的那样.

否则, 如果这个求值情况[situation]发生在一个安全调用[safe call]中, 一定会发出一个 program-error 类型[type]的错误; 如果在一个不安全的调用[unsafe call]中这个求值情况[situation]有着未定义的后果.

#### 3.5.1.8 <span id = "ErrorsWhenCallingNextMethod">调用下一个方法时的错误(Errors When Calling a Next Method)</span>

如果用实参[argument]调用 call-next-method, 用于 call-next-method 的变更后的参数集合的可应用[applicable]方法[method]的有序集合必须与这个广义函数[generic function]的原始实参[argument]的可应用[applicable]方法[method]有序集合相同, 否则应该会发出一个错误.

对于应用于新的那些参数的方法集合和应用于原始参数的方法集合之间的比较, 其中相同特化符的方法之间的次序差别是不敏感的.

如果使用指定了不同的可应用方法的有序集的实参[argument]来调用 call-next-method , 并且没有可用的下一个方法[next method], 那么对不同方法的测试和相关错误的发出(存在的话)的将优先于调用 no-next-method. 

## 3.6 <span id = "TraversalRulesSideEffects">遍历规则和副作用</span>

当在一个对象遍历[object-traversing]操作期间执行的代码[code]以一种可能影响正在进行的遍历操作的方式修改对象[object]时, 其后果是未定义的. 具体来说, 应用以下规则.

列表遍历(List traversal)

    对于列表[list]遍历操作, 列表[list]中的 cdr 链是不允许被破坏性修改的.

数组遍历(Array traversal)

    对于数组[array]遍历操作, 不允许对数组[array]进行调整, 并且填充指针不允许被修改, 如果有的话.

哈希表遍历(Hash-table traversal)

    对于哈希表[hash table]遍历操作, 新元素可能不会被添加或删除, 除了当前散列键对应的元素可以被更改或删除的话.

包遍历(Package traversal)

    对于包[package]遍历操作 (比如, do-symbols), 新的符号[symbol]不能从被遍历的包[package]或者它使用的任何包[package]中被捕捉[interned]或者解除捕捉[uninterned], 除了当前的符号[symbol]可以从被遍历的包[package]中被解除捕捉[uninterned].

## 3.7 <span id = "DestructiveOperations">破坏性操作</span>

### 3.7.1 字面化对象的修改

如果字面化[literal]对象[object]被破坏性地修改那么后果是未定义的. 出于这个目的, 以下操作被认为是破坏性的[destructive]:

random-state

    使用它作为函数[function] random 的一个参数[argument].

cons

    修改 cons 的 car 或者 cdr 部分, 或者对一个 cons 的 car 或者 cdr 部分的对象[object]执行破坏性[destructive]操作.

array

    将一个新值存储到数组[array]的某个元素中, 或者对已经是这样一个元素[element]的对象[object]执行破坏性[destructive]操作.

    改变数组[array]的填充指针[fill pointer], 规模[dimension]或位移 (不管这个数组[array]是否为实际上可调整的[actually adjustable]).

    在另一个数组[array]上执行破坏性[destructive]操作, 它被转移到这个数组或者和这个数组[array]共享内容.

hash-table

    对任何键[key]做破坏性[destructive]操作.

    为任何键[key]存储一个新的值[value[4]], 或者在这样一个值[value]的任何对象上执行破坏性[destructive]操作.

    从这个哈希表[hash table]中添加或删除元素.

structure-object

    存储一个新的值到任何槽中, 或者对一些槽的值对象[object]执行破坏性[destructive]操作.

standard-object

    存储一个新的值到任何槽中, 或者对一些槽的值对象[object]执行破坏性[destructive]操作.

    改变这个对象[object]的类 (比如, 使用函数[function] change-class).

readtable

    更改读写表大小写[readtable case].

    修改这个读取表中任何字符的语法类型.

    修改与读取表[readtable]中任何字符[character]关联的读取器宏函数[reader macro function], 或修改在读取表[readtable]被定义为分派宏字符[dispatching macro character]的字符[character]关联的读取器宏函数[reader macro function].

stream

    在流[stream]上执行 I/O 操作, 或者关闭这个流[stream].

所有其他标准化类型

    [这个范围包括, 比如, character, condition, function, method-combination, method, number, package, pathname, restart, 还有 symbol.]

    在这些类型[type]的对象[object]上没有标准化[standardized]破坏性[destructive]操作. 

### 3.7.2 破坏性操作期间的控制转移

如果将控制从破坏性[destructive]的操作中转移出来(比如, 由于一个错误), 就会被修改对象的状态是依赖于具体实现的[implementation-dependent].

#### 3.7.2.1 破坏性操作期间的控制转移的示例

下面的示例演示了许多方法中的一部分, 在这些方法下这个修改的依赖于具体实现的[implementation-dependent]本质得以展现.

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
 * [变量 *MACROEXPAND-HOOK*](#VariableMACROEXPANDHOOK)
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

* 语法(Syntax):

        lambda lambda-list [[declaration* | documentation]] form*

* 参数(Arguments):

        lambda-list---一个普通 lambda 列表[ordinary lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 没有被求值的.
        form---一个表达式形式[form].

* 描述(Description):

        lambda 表达式[lambda expression]是一种列表[list], 可以被用于替代特定上下文中的一个函数名[function name], 通过直接描述其行为而不是间接地引用一个已建立函数[function]的名称来表示一个函数[function].

        documentation 作为文档字符串[documentation string]被附加到所表示的函数 function (如果有实际创建的话).

* 参见(See Also):

        function, documentation, 章节 3.1.3 (lambda 表达式), 章节 3.1.2.1.2.4 (lambda 表达式形式), 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        这个 lambda 表达式形式[lambda form]

    ```LISP
    ((lambda lambda-list . body) . arguments)
    ```

        语义上等价于函数表达式[function form]

    ```LISP
    (funcall #'(lambda lambda-list . body) . arguments)
    ```

### <span id = "MacroLAMBDA">宏 LAMBDA</span>

* 语法(Syntax):

        lambda lambda-list [[declaration* | documentation]] form* => function

* 参数和值(Arguments and Values):

        lambda-list---一个普通 lambda 表达式[ordinary lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        form---一个表达式形式[form].
        function---一个函数[function].

* 描述(Description):

        为一个涉及一个 lambda 表达式[lambda expression]的 function 特殊表达式形式[special form]提供一个简写表示, 例如:

        (lambda lambda-list [[declaration* | documentation]] form*)
        ==  (function (lambda lambda-list [[declaration* | documentation]] form*))
        ==  #'(lambda lambda-list [[declaration* | documentation]] form*)

* 示例(Examples):

    ```LISP
    (funcall (lambda (x) (+ x 3)) 4) =>  7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        lambda (symbol)

* 注意(Notes):

        这个宏可以这样实现:

    ```LISP
    (defmacro lambda (&whole form &rest bvl-decls-and-body)
    (declare (ignore bvl-decls-and-body))
    `#',form)
    ```

### <span id = "FunctionCOMPILE">函数 COMPILE</span>

* 语法(Syntax):

        compile name &optional definition => function, warnings-p, failure-p

* 参数和值(Arguments and Values):

        name---一个函数名字[function name], 或者 nil.
        definition---一个 lambda 表达式[lambda expression]或者一个函数[function]. 如果 name 命名一个函数[function]那么默认就是这个名字 name 的函数定义, 或者如果 name 命名一个宏[macro]那么默认就是这个名字 name 的宏函数[macro function]. 当 name 是 nil 时如果没有提供这个定义 definition 那么后果是未定义的.
        function---这个函数名 function-name, 或者一个编译后的函数[compiled function].
        warnings-p---一个广义 boolean[generalized boolean].
        failure-p---一个广义 boolean[generalized boolean].

* 描述(Description):

        编译一个解释的函数[interpreted function].

        compile 从定义 definition 中产生一个编译后的函数[compiled function]. 如果定义 definition 是一个 lambda 表达式[lambda expression], 它被强制转为一个函数[function]. 如果这个定义 definition 已经是一个编译后的函数[compiled function], compile 会产生那个函数自身(换句话说, 是一个恒等(identity)操作)或者一个等价的函数.

        如果这个名字 name 是 nil, 产生的编译后的函数[compiled function]作为主值[primary value]直接返回. 如果给定一个非 nil[non-nil]的名字 name, 那么产生的编译后的函数[compiled function]替换这个名字 name 对应的已存在的函数[function]定义并且作为主值[primary value]返回这个名字 name; 如果名字 name 是一个命名一个宏[macro]的符号[symbol], 它的宏函数[macro function]会被更新并且名字 name 作为主值[primary value]返回.

        在 compile 函数处理的代码中出现的字面化[literal]对象[object]既没有被复制也没有被合并[coalesce]. 从 compile 的执行中产生的代码引用和源代码中对应对象[object] eql 的对象[object].

        compile 允许但不是必须去为 error 类型[type]的状况[condition]建立[establish]处理者[handler]. 比如, 处理者[handler]可能会发出警告, 并从某个依赖于具体实现的[implementation-dependent]点重新启动编译, 以便在无需人工干预的情况下进行编译.

        第二个返回值[secondary value], warnings-p, 如果编译器没有检测到 error 或者 warning 类型[type]的状况[condition]那么就是 false, 否则就是 true.

        第三个返回值[tertiary value], failure-p, 如果编译器没有检测到 error 或者 warning(除了 style-warning) 类型[type]的状况[condition]那么就是 false, 否则就是 true.

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

        *error-output*, *macroexpand-hook*.

        宏定义和全局声明(proclamation)的存在.

* 异常情况(Exceptional Situations):

        如果包围在要编译的函数上的词法环境[lexical environment]包含除了宏[macros], 符号宏[symbol macros]或声明[declaration]之外的任何绑定[binding], 则其结果是未定义的.

        关于在编译过程中的错误检测信息, 见章节 3.2.5 (编译器中的异常情况).

* 参见(See Also):

        compile-file

* 注意(Notes): None. 

### <span id = "FunctionEVAL">函数 EVAL</span>

* 语法(Syntax):

        eval form => result*

* 参数和值(Arguments and Values):

        form---一个表达式形式[form].
        results---表达式形式 form 的求值[evaluation]产生[yield]的值[value].

* 描述(Description):

        在当前动态环境[dynamic environment]和空词法环境[null lexical environment]下求值表达式形式 form.

        eval 是一个求值器的用户接口.

        这个求值器展开宏调用就像通过使用 macroexpand-1 一样.

        出现在被 eval 处理的代码中的常量既不会被复制也不会被合并. 从 eval 的执行产生的代码引用的对象[object]和源代码中对应的对象[object]是 eql 的.

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

* 参见(See Also):

        macroexpand-1, 章节 3.1.2 (求值模型)

* 注意(Notes):

        为了获得一个符号[symbol]的当前动态值, 使用 symbol-value 等价于(而且通常更可取的)使用 eval.

        注意, eval 表达式形式[form]需要对其实参[argument]进行两个级别的求值[evaluation]. 第一, 表达式形式 form 通过常规的参数求值机制来求值[evaluation], 就像任何调用[call]一样. 从这个普通的实参[argument]求值[evaluation]得出的对象[object]成为表达式形式 form 形参[parameter]的值[value], 然后作为 eval 表达式形式[form]的一部分进行求值[evaluate]. 比如:

    ```LISP
    (eval (list 'cdr (car '((quote (a . b)) c)))) =>  b
    ```

        这个实参[argument]表达式形式[form] (list 'cdr (car '((quote (a . b)) c))) 以正常方式求值来产生实参 (cdr (quote (a . b))); 然后 eval 求值它的实参[argument], (cdr (quote (a . b))), 产生 b. 因为一个单独求值[evaluation]已经发生在任何函数表达式形式[function form]中的任何实参[argument]表达式形式[form]中, 所以有时称 eval 执行 "一个额外级别的求值".

### <span id = "SpecialOperatorEVALWHEN">特殊操作符 EVAL-WHEN</span>

* 语法(Syntax):

        eval-when (situation*) form* => result*

* 参数和值(Arguments and Values):

        situation---这些 :compile-toplevel, :load-toplevel, :execute, compile, load, 或 eval 符号[symbol]中的一个. 这里 eval, compile, 和 load 被废弃了.
        forms---一个隐式 progn [implicit progn].
        results---如果这些表达式形式[form]被执行, 这个就是它们的值, 如果没有就是 nil.

* 描述(Description):

        一个 eval-when 表达式形式的主体被当作一个隐式 progn [implicit progn]处理, 但是只有在列出的那些求值情况 situation 下.

        这里求值情况 :compile-toplevel (或 compile) 还有 :load-toplevel (或 load) 的使用控制着当 eval-when 作为顶层表达式形式[top level form]出现在被 compile-file 处理的代码中时是否会发生求值[evaluation]以及何时发生求值[evaluation].

        这里求值情况 :execute (或者 eval) 的使用控制着其他 eval-when 表达式形式[form]是否发生求值; 这就是说, 那些不是顶层表达式形式[top level form]的, 或者那些在被 eval 或 compile 处理的代码中的. 如果这个 :execute 求值情况在这样一个表达式形式[form]中被指定, 那么其中的主体表达式形式作为一个隐式 progn [implicit progn]处理; 否则, 这个 eval-when 表达式形式[form]返回 nil.

        eval-when 通常作为顶层表达式形式[top level form]出现, 但是对于它作为非顶层表达式形式[non-top-level form]出现在也是有意义的. 然而, 这个描述在章节 3.2 (编译) 的编译时副作用只发生在当 eval-when 作为顶层表达式形式[top level form]出现的时候.

* 示例(Examples):

        eval-when 使用的一个示例是, 为了当编译器使用用户定义的读取器宏[reader macro]时可以正确地读取文件, 它有必要写成

    ```LISP
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (set-macro-character #\$ #'(lambda (stream char)
                                      (declare (ignore char))
                                      (list 'dollar (read stream))))) =>  T
    ```

        这个导致对 set-macro-character 的调用在编译器的执行环境中被执行, 从而修改它的读取器语法表.

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

* 参见(See Also):

        compile-file, 章节 3.2 (编译)

* 注意(Notes):

        以下效果是 eval-when 的定义的逻辑结果:

        * 一个单独的 eval-when 表达式执行主体代码不超过一次.

        * 应该编写用于顶层表达式形式[top level form]中的宏[macro], 以便处理在宏展开中的那些表达式形式[form]的副作用. 这个宏展开器(macro-expander)自身不应该执行这些副作用.

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

            遵守这个约定意味着这样的宏[macro]在作为非顶层表达式形式[non-top-level form]出现在时直观地表现.

        * 在一个 eval-when 周围放置一个变量的绑定确实会捕获这个绑定, 因为 compile-time-too 模式不会发生 (换句话说, 引入一个变量绑定意味着 eval-when 不是一个顶层表达式形式[top level form]). 比如,

    ```LISP
    (let ((x 3))
    (eval-when (:execute :load-toplevel :compile-toplevel) (print x)))
    ```

            在执行的时候(换句话说, 加载时)打印 3, 而在编译时不会打印任何东西. 这很重要, 因此 defun 和 defmacro 的展开可以根据 eval-when 来完成并且可以正确地捕获词法环境[lexical environment].

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

            当这个 bar 的定义不是一个顶层表达式形式[top level form]时, 它会被上面的规则当成和下面的这个相同

    ```LISP
    (defun bar (x) 
    (setf (symbol-function 'foo) #'(lambda () (+ x 3))))
    ```

### <span id = "SpecialOperatorLOADTIMEVALUE">特殊操作符 LOAD-TIME-VALUE</span>

* 语法(Syntax):

        load-time-value form &optional read-only-p => object

* 参数和值(Arguments and Values):

        form---一个表达式形式[form]; 按照以下描述的被求值.
        read-only-p---一个 boolean; 不求值.
        object---从求值表达式形式 form 返回的主值[primary value].

* 描述(Description):

        load-time-value 为表达式形式 form 直到这个表达式在运行时环境中才求值提供一个延时求值的机制; 见章节 3.2 (编译).

        read-only-p 指定这个结果是否可以被认为是常量对象[constant object]. 如果是 t, 结果就是一个只读的量, 如果适用于具体实现[implementation], 它可以被拷贝到只读空间 并且/或 和来自其他程序[program]的相似的[similar]常量对象[constant object]进行合并. 如果是 nil (默认的), 结果一定既不被复制, 也不能被合并; 它一定被认为是潜在可修改的数据.

        如果一个 load-time-value 表达式被 compile-file 处理, 这个编译器在这个表达式形式 form 上执行它正常的语义处理 (就像宏展开和转为机器码), 但是安排这个表达式形式 form 的执行发生在加载时的一个空词法环境[null lexical environment]中, 这个求值的结果在运行时被当作是一个字面化[literal]对象[object]. 确保这个表达式形式 form 的求值只有在文件[file]加载时才会发生, 但是对该文件中顶层表达式形式[top level form]的求值的求值顺序是由依赖实现的[implementation-dependent].

        如果一个 load-time-value 表达式出现在被 compile 编译的函数中, 这个表达式形式 form 在编译时的一个空词法环境[null lexical environment]中被求值. 这个编译时求值的结果被当作是编译后代码中的字面化[literal]对象[object].

        如果一个 load-time-value 表达式被 eval 处理, 表达式形式 form 在一个空词法环境[null lexical environment]中被求值, 并且返回一个值. 隐式编译(或部分编译)由 由 eval 处理表达式的具体实现可能只求值表达式形式 form 一次, 在这个编译被执行的时候.

        如果相同的[same]列表[list] (load-time-value form) 被求值或编译超过一次, 这个表达式形式 form 被只求值一次或者超过一次是依赖于具体实现的[implementation-dependent]. 当一个表达式被求值或编译共享子结构时, 当相同的[same]表达式形式[form]被 eval 或 compile 多次处理时, 都可能发生这种情况. 由于一个 load-time-value 表达式可以在不止一个地方被引用并且可以被 eval 求值超过一次, 因此每一次执行返回新的对象还是返回和其他执行一样的对象[object]是依赖于具体实现的[implementation-dependent]. 当对产生的对象[object]进行破坏性修改时, 用户必须谨慎使用.

        如果两个在 equal 下是相同的[same]但是不相等[identical]列表 (load-time-value form) 被求值或编译, 它们的值总是来自于对表达式形式 form 的不同的求值. 它们的那些值[value]可能不会被合并除非 read-only-p 是 t.

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

* 参见(See Also):

        compile-file, compile, eval, 章节 3.2.2.2 (最小化编译), 章节 3.2 (编译)

* 注意(Notes):

        load-time-value 必须出现在一个"用于[evaluation]"位置中的引用结构的外面. 在出现需要在被引用结构中使用 load-time-value 的情况下, 反引号[backquote]读取器宏[reader macro]可能会被调用; 见章节 2.4.6 (反引号).

        如果一个对象已经是只读的, 为 read-only-p 指定为 nil 不是强制一个对象成为可修改的方法. 这只是一种方式来说明, 对于一个可修改的对象, 这个操作不是为了使这个对象变得只读. 

### <span id = "SpecialOperatorQUOTE">特殊操作符 QUOTE</span>

* 语法(Syntax):

        quote object => object

* 参数和值(Arguments and Values):

        object---一个对象[object]; 不求值.

* 描述(Description):

        这个 quote 特殊操作符[special operator]只是返回对象 object.

        如果那些字面化[literal]对象[object] (包括被引用对象[quoted object])被破坏性修改那么结果是未定义的.

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

* 参见(See Also):

        章节 3.1 (求值), 章节 2.4.3 (单引号), 章节 3.2.1 (编译器术语)

* 注意(Notes):

        这个 'object 文本表示法和 (quote object) 是等价的; 见章节 3.2.1 (编译器术语).

        一些称之为自求值对象[self-evaluating object]的对象[object]不需要被 quote 引用. 然而, 符号[symbol]和列表[list]用来表示程序的一部分, 因此如果没有 quote, 程序中的常量数据就无法使用. 由于 quote 抑制了这些对象[object]的求值[evaluation], 它们变成了数据而不是程序. 

### <span id = "AccessorCOMPILERMACROFUNCTION">访问器 COMPILER-MACRO-FUNCTION</span>

* 语法(Syntax):

        compiler-macro-function name &optional environment => function

        (setf (compiler-macro-function name &optional environment) new-function)

* 参数和值(Arguments and Values):

        name---一个函数名字[function name].
        environment---一个环境[environment]对象[object].
        function, new-function---一个编译器宏函数[compiler macro function], 或者 nil.

* 描述(Description):

        在环境 environment 中访问[access]名为 name 的编译器宏函数[compiler macro function], 如果有的话.

        一个 nil 值表示名为 name 的编译器宏函数[compiler macro function]的缺失.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        在一个 compiler-macro-function 的 setf 使用中, 如果环境 environment 是非 nil[non-nil]那么结果是未定义的.

* 参见(See Also):

        define-compiler-macro, 章节 3.2.2.1 (编译器宏)

* 注意(Notes): None. 

### <span id = "MacroDEFINECOMPILERMACRO">宏 DEFINE-COMPILER-MACRO</span>

* 语法(Syntax):

        define-compiler-macro name lambda-list [[declaration* | documentation]] form*
        => name

* 参数和值(Arguments and Values):

        name---一个函数名字[function name].
        lambda-list---一个宏 lambda 列表[macro lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        form---一个表达式形式[form].

* 描述(Description):

        这是一个定义编译器宏函数[compiler macro function]的普通机制. 它的定义方式和 defmacro 一样; 仅有的区别是:

            * 这个 name 可以是一个命名了任何函数[function]或宏[macro]的函数名字[function name].

            * 展开器函数作为 name 的编译器宏函数[compiler macro function]被安装, 而不是作为一个宏函数[macro function].

            * 这个 &whole 参数绑定为传递给这个编译器宏函数[compiler macro function]的 form 参数. 剩余的 lambda-list 参数被指定为好像一个表达式形式包含了在 car 中的函数名和在 cdr 中的那些实际参数, 但是如果实际表达式形式的 car 部分是一个符号 funcall, 那么参数的解构实际上通过使用它的 cddr 来执行.

            * 这个 documentation 作为一个文档字符串被关联到名字 name (作为 compiler-macro 种类) 和这个编译器宏函数[compiler macro function].

            * 不像一个普通的宏[macro], 编译器宏[compiler macro]可以仅仅通过返回与原始那个(可以通过使用 &whole 获取到的) 相同的[same]表达式形式来拒绝提供展开式.

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

* 参见(See Also):

        compiler-macro-function, defmacro, documentation, 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        为一个 COMMON-LISP 包中的函数写一个编译器宏[compiler macro]的定义的结果是未定义的; 很有可能在某些实现[implementation]中, 这样的尝试会覆盖相同或同等重要的定义. 一般来说, 建议程序员只编写自己维护的函数[function]的编译器宏[compiler macro]的定义--为其他地方维护的函数编写一个编译器宏[compiler macro]的定义通常被认为违背了模块化和数据抽象的传统规则. 

### <span id = "MacroDEFMACRO">宏 DEFMACRO</span>

* 语法(Syntax):

        defmacro name lambda-list [[declaration* | documentation]] form*
        => name

* 参数和值(Arguments and Values):

        name---一个符号[symbol]. 
        lambda-list---一个宏 lambda 列表[macro lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        form---一个表达式形式[form].

* 描述(Description):

        通过关联一个宏函数[macro function]到全局环境中的这个名字 name 来把 name 定义为一个宏[macro]. 这个宏函数[macro function]被定义在 defmacro 表达式形式[form]出现的相同的词法环境[lexical environment]中.

        在 lambda-list 里的参数变量被绑定为这个宏调用的解构的部分里.

        这个展开函数接受两个参数, 一个表达式形式[form]和一个环境[environment]. 这个展开函数返回一个表达式形式[form]. 这个展开函数的主体是由这些表达式形式 form 指定的. 这些表达式形式 form 按顺序被执行. 最后一个表达式形式 form 执行的值作为这个宏[macro]的展开返回. 这个展开函数的主体表达式形式 form (但不是那个 lambda-list) 被隐式地闭合在一个名字为 name 的块[block]中.

        这个 lambda-list 符合 3.4.4 章节(宏 lambda 列表)所描述的要求.

        这个 documentation 作为文档字符串[documentation string]被关联到 name(作为 function 种类) 和这个宏函数[macro function]上.

        defmacro 可以被用于重定义一个宏[macro]或者用一个宏[macro]定义替换一个函数[functioni]定义.

        返回的表达式形式[form]的递归展开必须终止, 其中包括作为返回的其他表达式形式[form]的子表达式形式[subform]的宏的展开.

        如果完全宏展开一个表达式形式[form]的结果包含除了在字面化[literal]对象[object]里以外的任何环状[circular]列表[list]结构[structure], 结果是未定义的.

        如果一个 defmacro 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须在编译时把宏[macro]定义存储起来, 这样一来在这个文件中稍后出现的这个宏就可以被正确地展开. 如果这个宏在这个要被被编译的文件[file]中被引用到, 用户必须确保这个宏[macro]的主体可以在编译时被求值.

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

        只有在普通 lambda 列表[ordinary lambda list]语法允许一个参数名而不是列表[list]的情况下才允许使用内嵌的解构 lambda 列表[destructuring lambda list]的这个规则被用来防止歧义. 比如, 下面的是非法的:

    ```LISP
    (defmacro loser (x &optional (a b &rest c) &rest z)
      ...)
    ```

        因为普通 lambda 列表[ordinary lambda list]语法不允许一个列表[list]跟在 &optional 后面; 这个列表[list] (a b &rest c) 会被解释为描述一个名字为 a 的可选参数, 它的默认值是表达式形式 b, 带有一个命名为 &rest 的 supplied-p 参数(不合法), 还有一个列表中的无关符号 c (也是不合法的). 一种几乎正确的方式来表达是

    ```LISP
    (defmacro loser (x &optional ((a b &rest c)) &rest z)
      ...)
    ```

        额外的括号集消除了歧义. 然而, 这个定义现在是不正确的因为一个像 (loser (car pool)) 这样的宏调用不会提供任何参数表达式形式给 lambda 列表 (a b &rest c), 因此, 与 lambda 列表[lambda list]相匹配的默认值是 nil, 因为没有指定显式的默认值. 这个结果是未定义的, 因为空列表 nil 没有表达式形式[form]来满足参数 a 和 b. 完全正确的定义应该是

    ```LISP
    (defmacro loser (x &optional ((a b &rest c) '(nil nil)) &rest z)
      ...)
    ```

        或者

    ```LISP
    (defmacro loser (x &optional ((&optional a b &rest c)) &rest z)
      ...)
    ```

        这些略有不同: 第一个要求如果宏调用显式指定了 a, 那么它也必须显式地指定 b, 而第二个则没有这个要求. 比如,

    ```LISP
    (loser (car pool) ((+ x 1)))
    ```

        这对于第二个定义是个合法的调用但是对于第一个则不是.

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

* 参见(See Also):

        define-compiler-macro, destructuring-bind, documentation, macroexpand, *macroexpand-hook*, macrolet, macro-function, 章节 3.1 (求值), 章节 3.2 (编译), 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes): None. 

### <span id = "AccessorMACROFUNCTION">访问器 MACRO-FUNCTION</span>

* 语法(Syntax):

        macro-function symbol &optional environment => function

        (setf (macro-function symbol &optional environment) new-function)

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        environment---一个环境[environment]对象[object].
        function---一个宏函数[macro function]或者 nil.
        new-function---一个宏函数[macro function].

* 描述(Description):

        确定符号 symbol 在指定的环境 environment 中是否有一个函数定义作为一个宏.

        如果是这样, 则返回宏展开函数, 是一个两参数的函数. 如果符号 symbol 在这个词法环境 environment 中没有函数定义, 或者它的定义不是一个宏[macro], macro-function 返回 nil.

        对于符号 symbol, macro-function 和 special-operator-p 都返回 true 是有可能的. 宏定义必须可被那些只理解那些标准 Common Lisp 特殊表达式形式[special form]的程序所使用.

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

        在一个 macro-function 的 setf 的使用中如果环境 environment 是非 nil 的[non-nil], 那么结果是未定义的.

* 参见(See Also):

        defmacro, 章节 3.1 (求值)

* 注意(Notes):

        setf 可以和 macro-function 一起使用来设置一个宏[macro]为一个符号的全局函数定义:

    ```LISP
    (setf (macro-function symbol) fn)
    ```

        设置的值必须是一个函数, 该函数接受两个参数, 整个宏调用和一个环境[environment], 并计算该调用的展开. 执行这个操作会导致符号 symbol 只有这个宏定义作为全局函数定义; 任何之前的定义, 不管是宏[macro]还是函数[function], 都会丢失. 

### <span id = "FunctionMACROEXPANDMACROEXPAND1">函数 MACROEXPAND, MACROEXPAND-1</span>

* 语法(Syntax):

        macroexpand form &optional env => expansion, expanded-p

        macroexpand-1 form &optional env => expansion, expanded-p

* 参数和值(Arguments and Values):

        form---一个表达式形式[form].
        env---一个环境[environment]对象[object]. 默认是 nil.
        expansion---一个表达式形式[form].
        expanded-p---一个广义 boolean[generalized boolean].

* 描述(Description):

        macroexpand 和 macroexpand-1 把宏[macro]展开.

        如果表达式形式 form 是一个宏表达式形式[macro form], 那么 macroexpand-1 只展开宏表达式形式[macro form]调用一次.

        macroexpand 重复地展开表达式形式 form 直到它不再是一个宏表达式形式[macro form]. 事实上, macroexpand 重复调用 macroexpand-1 直到返回的第二个值[secondary value]是 nil.

        如果表达式形式 form 是一个宏表达式形式[macro form], 那么展开式 expansion 就是一个宏展开式[macro expansion]并且 expanded-p 是 true. 否则, 这个展开式 expansion 就是给定的表达式形式 form 并且 expanded-p 是 false.

        宏展开是按照以下方式进行的. 一旦 macroexpand-1 确定表达式形式 from 是一个宏表达式形式[macro form], 它就会获得一个用于这个宏[macro]或符号宏[symbol macro]的合适的展开函数[function]. 这个 *macroexpand-hook* 的值强制转为一个函数[function]并且作为一个三个参数的函数[function]被调用: 展开函数[function], 这个 form, 还有这个 env. 从这个调用返回的值[value]被认为是这个表达式形式 form 的展开.

        除了全局环境中的宏[macro]定义之外, 在 env 中由 macrolet 或 symbol-macrolet 所建立的任何局部宏定义都被考虑到. 如果只提供了表达式形式 form 作为一个参数, 那么环境实际上就是 null, 那么只有通过 defmacro 建立的全局宏定义才会被考虑进去. 宏[macro]定义被局部函数[function]定义所遮蔽.

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

* 参见(See Also):

        *macroexpand-hook*, defmacro, setf of macro-function, macrolet, symbol-macrolet, 章节 3.1 (求值)

* 注意(Notes):

        无论是 macroexpand 还是 macroexpand-1 都没有做任何显式的尝试去展开那些是 form 的子表达式形式[subform]或 expansion 的子表达式形式[subform]的宏表达式[macro form]. 然而, 由于这个宏函数[macro function]的语义或实现, 这样的展开可能隐式发生. 

### <span id = "MacroDEFINESYMBOLMACRO">宏 DEFINE-SYMBOL-MACRO</span>

* 语法(Syntax):

        define-symbol-macro symbol expansion
        => symbol

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        expansion---一个表达式形式[form].

* 描述(Description):

        为全局地影响所表示符号 symbol 的宏展开[macro expansion]提供了一种机制.

        为由符号 symbol 命名的符号宏[symbol macro]全局地建立一个展开函数. 对于符号宏[symbol macro]来说, 展开函数[function]的唯一确保属性是当它被应用到表达式形式[form]和环境[environment]时它会返回正确的展开式. (具体来说, 这个展开在概念上是存储在展开函数, 环境[environment], 还是两者都存储了, 这是依赖于实现的[implementation-dependent].)

        每个对符号 symbol 的全局引用(换句话说, 没有被同一个符号所命名的变量[variable]或符号宏[symbol macro]的绑定[binding]所遮蔽[shadow[2]])都是通过常规的宏展开过程来展开的; 见章节 3.1.2.1.1 (符号表达式形式). 符号宏[symbol macro]的展开在与符号宏[symbol macro]引用所在相同的词法环境[lexical environment]中受到进一步的宏展开[macro expansion], 与普通宏[macro]类似.

        如果在这个定义的作用域内为符号 symbol 做一个 special 声明, 后果是未定义的 (换句话说, 当它没有被一个由相同符号[symbol]命名的变量[variable]或符号宏[symbol macro]的绑定[binding]所遮蔽[shadow[2]]时).

        任何在这个定义的作用域中使用 setq 来设置符号 symbol 的值都被当作 setf. 符号 symbol 的 psetq 被当作是 psetf, 并且 multiple-value-setq 被当作是 values 的 setf.

        一个符号宏[symbol macro]的绑定[binding]可以被 let 或 symbol-macrolet 所遮蔽[shadow[2]].

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

        如果符号 symbol 已经被定义为一个全局变量[global variable], 那么就会发出一个 program-error 类型[type]的错误.

* 参见(See Also):

        symbol-macrolet, macroexpand

* 注意(Notes): None. 

### <span id = "SpecialOperatorSYMBOLMACROLET">特殊操作符 SYMBOL-MACROLET</span>

* 语法(Syntax):

        symbol-macrolet ((symbol expansion)*) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        expansion---一个表达式形式[form].
        declaration---一个 declare 表达式[expressioni]; 不求值.
        forms---一个隐式 progn[implicit progn].
        results---那些表达式形式 forms 返回的那些值[value].

* 描述(Description):

        symbol-macrolet 为影响符号[symbol]的宏展开[macro expansion]环境提供一种机制.

        symbol-macrolet 为由那些符号中的每一个符号 symbol 命名的符号宏[symbol macro]词法上建立展开函数. 对于一个符号宏[symbol macro]来说, 展开函数[function]的唯一保证属性是当它被应用到表达式形式[form]和环境[environment]时它会返回正确的展开式. (具体来说, 它在概念上是存储在展开函数, 环境[environment], 还是两者都存储了, 这是依赖于具体实现的[implementation-dependent].)

        在 symbol-macrolet 词法作用域[scope]内, 作为一个变量的对 symbol 的每个引用都被常规的宏展开过程所展开; 见章节 3.1.2.1.1 (符号表达式形式). 一个符号宏的展开式在这个符号宏调用相同的词法环境中受到进一步的宏展开, 与普通宏[macro]类似.

        允许和 let 相同的那些声明 declarations, 除了一个例外: 如果一个 special 声明命名了要被 symbol-macrolet 定义的这些符号中的一个, 那么 symbol-macrolet 会发出一个错误.

        当 symbol-macrolet 表达式形式中的那些表达式形式 forms 被展开, 任何使用 setq 来设置这些指定变量的其中一个的值被当作是 setf 一样. 一个被定义为一个符号宏的符号的 psetq 被当作它是一个 psetf, 并且 multiple-value-setq 被当作是 values 的 setf.

        这个 symbol-macrolet 的使用可以被 let 遮蔽. 换句话说, symbol-macrolet 只是替代了包在那些表达式形式 forms 周围的符号 symbol 的词法绑定的作用域[scope]中的 symbol 的出现.

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

        如果试图绑定一个已经被定义为全局变量[global variable]的符号[symbol], 会发出一个 program-error 类型[type]的错误.

        如果声明 declaration 中包含一个对 symbol-macrolet 绑定那些符号[symbol]中的一个的 special 声明, 那么会发出一个 program-error 类型[type]的错误.

* 参见(See Also):

        with-slots, macroexpand

* 注意(Notes):

        这个特殊表达式形式 symbol-macrolet 是用于实现 with-slots 的基础机制.

        如果一个 symbol-macrolet 表达式形式[form]是一个顶层表达式形式[top level form], 那么这些表达式形式 forms 也被当作顶层表达式形式[top level form]处理. 见章节 3.2.3 (文件编译). 

### <span id = "VariableMACROEXPANDHOOK">变量 \*MACROEXPAND-HOOK\*</span>

* 值类型(Value Type):

        一个三参数[argument]的函数[function]的标识符[designator]: 一个宏函数[macro function], 一个宏表达式形式[macro form], 和一个环境[environment]对象[object].

* 初始值(Initial Value):

        一个等价于函数[function] funcall 的函数的标识符[designator], 但是可能有额外的依赖于具体实现的[implementation-dependent]副作用.

* 描述(Description):

        被 macroexpand-1 用作展开接口钩子来控制宏展开[macro expansion]过程. 当一个宏表达式形式[macro form]要被展开时, 这个函数[function]会被调用, 并传递三个参数: 这个宏函数[macro function], 宏表达式形式[macro form], 这个要被展开的宏表达式形式[macro form]所在的环境[environment]. 这个环境[environment]对象[object]具有动态范围[dynamic extent]; 如果环境[environment]对象[object]在宏展开函数的动态范围[dynamic extent]之外被引用, 则其后果是未定义的.

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

* 参见(See Also):

        macroexpand, macroexpand-1, funcall, 章节 3.1 (求值)

* 注意(Notes):

        选择初始值的净效果是仅调用宏函数[macro function], 将宏表达式形式[macro form]和环境[environment]作为它的两个参数.

        用户或用户程序可以给这个变量[variable]赋值[assign]来定制或跟踪宏展开[macro expansion]机制. 注意, 但是, 这个变量[variable]是一个全局资源, 可能由多个程序[program]共享; 因此, 如果两个程序依赖于它们在这个变量的设置上的正确性, 那么这些程序[program]可能无法在相同的 Lisp 镜像[Lisp image]中运行. 由于这个原因, 通常最好将其用途限制在调试情况下.

        把自己的函数设置到 *macroexpand-hook* 的用户应该考虑把这个钩子的之前的值保存起来, 并且从他们自己的函数里调用这个值.

### <span id = "FunctionPROCLAIM">函数 PROCLAIM</span>

* 语法(Syntax):

        proclaim declaration-specifier => implementation-dependent

* 参数和值(Arguments and Values):

        declaration-specifier---一个声明指定符[declaration specifier].

* 描述(Description):

        在全局环境[global environment]中建立[establish] declaration-specifier 指定的声明[declaration].

        这样一个声明, 有时也称之为全局声明[global declaration]或公告[proclamation], 除非被局部遮蔽[shadow], 否则总是有效的.

        在 declaration-specifier 中, 变量[variable]和函数[function]的名称[name]分别引用动态变量[dynamic variable]和全局函数[function]定义.

        下一个图显示了可用于 proclaim 的声明标识符[declaration identifier]列表.

            declaration  inline     optimize  type  
            ftype        notinline  special         

            Figure 3-22. 全局声明标识符

        一个具体实现也可以自由地支持其他(具体实现定义的[implementation-defined])声明标识符[declaration identifier].

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

* 参见(See Also):

        declaim, declare, 章节 3.2 (编译)

* 注意(Notes):

    虽然 proclaim 表达式形式[form]的执行有着可能影响编译的效果, 但是编译器不会尝试识别和特别地处理 proclaim 表达式形式[form]. 像下面这样的公告[proclamation], 即便是一个顶层表达式形式[top level form], 直到它被执行之前不会有任何效果:

    ```LISP
    (proclaim '(special *x*))
    ```

    如果需要编译时的副作用, eval-when 可能会有用. 比如:

    ```LISP
    (eval-when (:execute :compile-toplevel :load-toplevel)
      (proclaim '(special *x*)))
    ```

    然而, 在大多数这样的情况下, 使用 declaim 来实现这一目的更好.

    因为 proclaim 表达式形式[form]是普通的函数形式[function form], 所以宏表达式形式[macro form]可以展开到它们. 

### <span id = "MacroDECLAIM">宏 DECLAIM</span>

* 语法(Syntax):

        declaim declaration-specifier* => implementation-dependent

* 参数和值(Arguments and Values):

        declaration-specifier---一个声明指定符[declaration specifier]; 不求值.

* 描述(Description):

        建立一个由 declaration-specifiers 指定的声明[declaration].

        如果这个宏的使用以顶层表达式形式[top level form]出现在文件编译器[file compiler]处理的文件[file]中, 这些公告[proclamations]也在编译时进行. 与其他定义宏一样, 在编译完文件后, 编译时的副作用是否存在仍然是未知的.

* 示例(Examples):

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        declare, proclaim

* 注意(Notes): None. 

### <span id = "SymbolDECLARE">符号 DECLARE</span>

* 语法(Syntax):

        declare declaration-specifier*

* 参数(Arguments):

        declaration-specifier---一个声明指定符[declaration specifier]; 不求值.

* 描述(Description):

        一个 declare 表达式[expression], 有时也称之为一个声明[declaration], 只能出现在某些表达式形式[form]的主体部分的开头; 那也就是说, 它的前面只可能是其他 declare 表达式[expression], 或者如果上下文允许前面还可能有文档字符串[documentation string].

        一个 declare 表达式可以出现在一个 lambda 表达式[lambda expression]中或者下面这段列出的任何表达式形式[form]中.

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

        Figure 3-23. 声明可以出现的标准表达式形式

        一个 declare 表达式[expression]只能出现在这些表达式形式[form]的语法所指定的位置. 尝试去求值一个 declare 表达式[expression]的后果是未定义的. 在这些表达式[expression]出现的情况下, 会为它们的存在进行显式检查, 并且它们从来没有真正被求值过; 正是因为这个原因它们被称之为 "declare 表达式[expression]" 而不是 "declare 表达式形式[form]".

        macro 表达式形式[macro form]不能被展开成声明; declare 表达式[expression]必须作为它们所引用的表达式形式[form]的实际的子表达式[subexpression]出现.

        下一段展示了可以被用于 declare 的声明标识符[declaration identifier]的列表.

            dynamic-extent  ignore     optimize  
            ftype           inline     special   
            ignorable       notinline  type      

            Figure 3-24. 局部声明标识符

        一个具体实现也可以自由地支持其他(具体实现定义的[implementation-defined])声明标识符[declaration identifier].

* 示例(Examples):

    ```LISP
    (defun nonsense (k x z)
      (foo z x)                     ;First call to foo
      (let ((j (foo k x))           ;Second call to foo
            (x (* k k)))
        (declare (inline foo) (special x z))
        (foo x j z)))               ;Third call to foo
    ```

        在这个例子里, 这个 inline 声明只应用于第三个 foo 调用, 不是第一个和第二个.这个 x 的 special 声明导致 let 为 x 创建了一个动态绑定[binding], 并且导致 let 主体中对 x 的引用是一个动态引用. 在对 foo 的第二个调用中的 x 的引用是一个对 nonsense 第二个参数的局部引用. 在第一个 foo 调用中对 x 的引用也是一个局部引用, 不是一个 special 的. 这个 z 的 special 声明导致第三个 foo 调用中的 z 引用是一个动态引用; 它不引用 nonsense 中名为 z 的参数, 因为这个参数绑定[binding]没有被声明为 special. (这个 z 的 special 声明不是出现在 defun 的主体部分, 而是在一个内部的表达式形式[form]中, 因此不影响形参[parameter]的绑定[binding].)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        试着把 declare 表达式[expression]当作一个表达式形式[form]来求值后果是不可预料的.

* 参见(See Also):

        proclaim, 章节 4.2.3 (类型指定符), declaration, dynamic-extent, ftype, ignorable, ignore, inline, notinline, optimize, type

* 注意(Notes): None. 

### <span id = "DeclarationIGNOREIGNORABLE">声明 IGNORE, IGNORABLE</span>

* 语法(Syntax):

        (ignore {var | (function fn)}*)

        (ignorable {var | (function fn)}*)

* 参数(Arguments):

        var---一个变量[variable]名字[name].
        fn---一个函数[function]名字[name].

* 合法上下文(Valid Context):

        声明[declaration]

* 绑定类型的影响(Binding Types Affected):

        变量[variable], 函数[function]

* 描述(Description):

        这个 ignore 和 ignorable 声明指向多个 var 的变量[variable]绑定[binding]还有多个 fns 的函数[function]绑定[binding]的值[for-value]引用[reference].

        一个 ignore 声明[declaration]指出对那些表示的绑定[binding]的值[for-value]引用[reference]不会发生在这个声明[declaration]的作用域里. 在这样一个声明[declaration]的作用域[scope]里, 编译器针对任何 var 或 fn 的值[for-value]引用[reference], 或者一个任意 var 的 special 声明[declaration]的出现去发出一个警告是可取的.

        一个 ignorable 声明[declaration]指出对那些表示的绑定[binding]的值[for-value]引用[reference]可能或可能不会出现在这个声明[declaration]的作用域里. 在这样一个声明[declaration]的作用域[scope]里, 编译器针对任何 var 或 fn 的值[for-value]引用[reference], 或者一个任意 var 的 special 声明[declaration]的出现或缺失去发出一个警告是可取的.

        当不在一个 ignore 或 ignorable 声明[declaration]的作用域[scope]时, 对于任何既没有值[for-value]引用[reference]也没有 special 声明[declaration]的 var, 或者没有值[for-value]引用[reference]的 fn, 编译器去发出一个警告是可取的.

        任何关于一个 "used" 或者 "unused" 绑定[binding]的警告必须是 style-warning 类型[type]的, 并且不影响程序的语义.

        由 with-open-file, with-open-stream, with-input-from-string, 和 with-output-to-string 建立的流变量[stream variable], 还有所有循环变量[iteration variable], 根据定义总是是 "used" 的. 对于这样一个变量[variable] v 使用 (declare (ignore v)), 有着未指明的结果.

* 参见(See Also):

        declare 

### <span id = "DeclarationDYNAMICEXTENT">声明 DYNAMIC-EXTENT</span>

* 语法(Syntax):

        (dynamic-extent [[var* | (function fn)*]])

* 参数(Arguments):

        var---一个变量[variable]名字[name].
        fn---一个函数[variable]名字[name].

* 合法上下文(Valid Context):

        声明[declaration]

* 绑定类型的影响(Binding Types Affected):

        变量[variable], 函数[function]

* 描述(Description):

        在某个包含的表达式形式 F 中, 这个声明为每一个 vari 断言 (不需要被 F 绑定), 并且为 vari 呈现的每一个值[value] vij 断言, 还有当 vij 成为 vari 的值的任何时候为 vij 的其他不可访问部分[otherwise inaccessible part]的对象[object] xijk 断言, 在 F 的执行终止后, xijk 要么是不可访问的(如果 F 为 vari 建立一个绑定[binding]), 要么是 vari 的当前值的一个其他不可访问部分[otherwise inaccessible part] (如果 F 没有为 vari 建立一个绑定[binding]). 每个 fni 都有相同的关系, 除了这些在函数[function]命名空间[namespace]中的绑定. 

        允许编译器以任何适合于具体实现[implementation]并且不与 Common Lisp 的语义冲突的方式使用该信息.

        dynamic-extent 声明可以是自由声明[free declaration]或绑定声明[bound declaration].

        在 dynamic-extent 声明中命名的这些 vars 和 fns 一定不能引用符号宏[symbol macro]或宏[macro]绑定.

* 示例(Examples):

        由于初始值的栈上分配需要在对象[object]的创建时知道对象[object]可以是栈上分配的[stack-allocated], 对于没有词法上显而易见的初始值的变量[variable], 对变量进行 dynamic-extent 声明[declaration]通常是不太有用的. 比如, 这可能是很有用的:

    ```LISP
    (defun f ()
      (let ((x (list 1 2 3)))
        (declare (dynamic-extent x))
            ...))
    ```

        这将允许那些希望这样做的编译器来栈上分配[stack allocate]由局部变量 x 所持有的列表. 写成下面这样是允许的, 但在实践中可能没有那么有用:

    ```LISP
    (defun g (x) (declare (dynamic-extent x)) ...)
    (defun f () (g (list 1 2 3)))
    ```

        大部分编译器不会在 f 中去栈上分配[stack allocate]给 g 的实参[argument], 因为对于编译器来说, 从 f 中假设关于 g 的事实是违背模块化的. 只有当 g 的定义不兼容地改变时, 愿意重编译 f 的一个具体实现可以合理地栈上分配[stack allocate]这个在 f 中给 g 的列表[list]参数.

        这里有另一个例子:

    ```LISP
    (declaim (inline g))
    (defun g (x) (declare (dynamic-extent x)) ...)
    (defun f () (g (list 1 2 3)))
    
    (defun f ()
      (flet ((g (x) (declare (dynamic-extent x)) ...))
        (g (list 1 2 3))))
    ```

        在上面的例子里, 一些编译器可能会确定优化是可以的, 而另一些可能不会.

        这个的一个变体是 "栈上分配剩余列表(stack allocated rest list)", 可以通过下面来实现 (在支持这个优化的实现中):

    ```LISP
    (defun f (&rest x)
      (declare (dynamic-extent x))
      ...)
    ```

        注意, 虽然 x 的初始值不明确, 但是 f 函数负责将列表 x 从传递的参数中装配起来, 因此在支持这些的实现中可以通过编译器对 f 函数进行优化, 以构建一个栈上分配[stack-allocated]的列表, 而不是使用堆上分配的列表.

        在下面的示例中,

    ```LISP
    (let ((x (list 'a1 'b1 'c1))
          (y (cons 'a2 (cons 'b2 (cons 'c2 nil)))))
      (declare (dynamic-extent x y))
      ...)
    ```

        这个 x 的其他不可访问部分[otherwise inaccessible part]是三个 cons, 而 y 的其他不可访问部分[otherwise inaccessible part]是另外三个 cons. 符号 a1, b1, c1, a2, b2, c2, 或者 nil 中没有是 x 或 y 的其他不可访问部分[otherwise inaccessible part], 因为每一个都被捕捉[interned]因此可以通过它被捕捉[interned]到的包[package] (或者多个包[package])是可访问的[accessible]. 然而, 如果使用了一个新分配的未捕捉[uninterned]的符号[symbol], 那么它将是包含它的列表[list]的其他不可访问部分[otherwise inaccessible part].

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

        下面的是错误的, 因为 x 的值在它的范围[extent]外被使用:

    ```LISP
    (length (list (let ((x (list 1 2 3)))  ; Invalid
                    (declare (dynamic-extent x))
                    x)))

    (progn (let ((x (list 1 2 3)))  ; Invalid
              (declare (dynamic-extent x))
              x)
            nil)
    ```

* 参见(See Also):

        declare

* 注意(Notes):

        最常见的优化是对由 vars 命名的那些对象[object]的初始值进行栈上分配[stack allocate].

        一个实现允许完全可以忽略这个声明. 

### <span id = "DeclarationTYPE">声明 TYPE</span>

* 语法(Syntax):

        (type typespec var*)

        (typespec var*)

* 参数(Arguments):

        typespec---一个类型指定符[type specifier].
        var---一个变量[variable]名字[name].

* 合法上下文(Valid Context):

        声明[declaration]或者公告[proclamation]

* 绑定类型的影响(Binding Types Affected):

        变量[variable]

* 描述(Description):

        只影响变量绑定[binding], 并指出那些变量 vars 只具有指定的类型指定符 typespec 的值. 具体来说, 通过 setq 给变量赋的值, 以及那些变量 vars 的初始值必须是指定的类型指定符 typespec. type 声明从来不应用于函数绑定[binding] (见 ftype).

        一个由 symbol-macrolet 定义的符号[symbol]的类型声明等价于在该符号的展开式周围包一个 the 表达式, 尽管这个符号[symbol]的宏展开[macro expansion]实际上并没有受到影响.

        一个类型声明的意义等价于修改声明作用域里的每一个变量 (var) 的引用为 (the typespec var), 修改声明作用域里的每一个赋值给变量 (new-value) 的表达式为 (the typespec new-value), 以及在进入声明作用域的时候执行 (the typespec var).

        一个类型[type]声明在所有声明中都是合法的. 对类型声明的解释如下:

        1. 在这个声明的作用域内对这个声明变量的任何引用执行期间, 如果声明的变量的值不是声明的类型[type], 后果是未定义的.
        2. 在这个声明的作用域内对这个声明变量的任何 setq 的执行期间, 如果被赋给声明变量的新值不属于声明的类型[type], 那么后果将是未定义的.
        3. 当进入声明的作用域时, 如果声明的变量的值不是声明的类型[type], 那么后果将是未定义的.

        一个类型[type]声明只影响它的作用域内的变量引用.

        如果嵌套类型[type]声明引用相同的变量, 那么该变量的值必须是那些声明类型[type]的交集的成员.

        如果对于一个动态变量这里有一个局部 type 声明, 并且对于相同的变量这里也有一个全局的类型[type]公告, 那么在这个局部声明的作用域中的那个变量的值必须是两种声明类型[type]的交集.

        type 声明可以是自由声明[free declaration]或绑定声明[bound declaration].

        一个符号[symbol]既不能是类型[type]的名称, 也不能是声明的名称. 一个符号[symbol]已经被声明为一个声明的名字时, 定义这个符号[symbol]为类[class], 结构体[structure], 状况[condition], 或类型[type]的名字[name]时, 如果这个符号已经被声明为一个声明的名字, 或反过来, 都会发出一个错误.

        在一个 array 类型声明的词法作用域[lexical scope]中, 所有对数组[array]元素[element]的引用都被假定为满足表达数组元素类型[expressed array element type] (与提升数组元素类型[upgraded array element type]相反). 编译器可以认为在 array 类型声明的范围内的代码就好像数组[array]元素[element]的每个访问[access]都被合适的 the 表达式形式包围一样.

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

        上面的 frob 定义等价于:

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

        给定一个实现,其中 fixnums 是29位, 但是 fixnum 数组[array]被升级到有符号的32位数组[array], 下面可以用所有的 fixnum 运算来编译:

    ```LISP
    (defun bump-counters (counters)
      (declare (type (array fixnum *) bump-counters))
      (dotimes (i (length counters))
        (incf (aref counters i))))
    ```

* 参见(See Also):

        declare, declaim, proclaim

* 注意(Notes):

        (typespec var*) 是 (type typespec var*) 的一个缩写.

        对于一个函数的参数的 type 声明并不一定意味着结果的类型. 下面的函数不允许使用依赖于具体实现的[implementation-dependent] 仅限 fixnum 的算法来编译:

    ```LISP
    (defun f (x y) (declare (fixnum x y)) (+ x y))
    ```

        为说明原因, 细想 (f most-positive-fixnum 1). Common Lisp 定义这个 F 必须返回一个 bignum, 而不是发出一个错误或产生一个数学上不正确的结果. 如果你有特殊的知识, "fixnum overflow" 情况就不会出现, 您可以在 fixnum 范围内声明结果值, 使一些编译器可以使用更有效的算法:

    ```LISP
    (defun f (x y)
      (declare (fixnum x y))
      (the fixnum (+ x y)))
    ```

        但是, 请注意, 在三个参数的情况下, 由于隐式中间值增长太大的可能性, 下面的内容不会导致使用依赖于具体实现的[implementation-dependent]仅限 fixnum 的算法:

    ```LISP
    (defun f (x y)
      (declare (fixnum x y z))
      (the fixnum (+ x y z)))
    ```

        为说明原因, 细想 (f most-positive-fixnum 1 -1). 尽管参数和结果都是 fixnums, 但一个中间值不是 fixnum. 如果在提供它的实现中选择依赖于具体实现的[implementation-dependent]仅限 fixnum 的算法是很重要的, 那么考虑编写这样的代码:

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

        function-name---一个函数名字[function name].

* 合法上下文(Valid Context):

        声明[declaration]或公告[proclamation]

* 绑定类型的影响(Binding Types Affected):

        函数[function]

* 描述(Description):

        inline 指出期望编译器去为 function-name 命名的函数产生内联调用; 这就是说, 指定的函数名  function-name  的代码应该集成到调用例程中, 内联出现并替换程序调用. 一个编译器可以自由地忽略这个声明. inline 声明从不应用于变量绑定[binding].

        如果其中一个提及的函数有着词法上明显的局部定义 (例如通过 flet 或 labels), 那么这个声明应用于这个局部定义而不是那个全局函数定义.

        虽然没有符合规范的实现[conforming implementation]被要求去执行用户定义函数的内联展开, 但那些具体实现[implementation]尝试去识别以下范例:

        为了定义一个默认不是 inline 的函数[function] f, 但是 (declare (inline f)) 会使 f 成为局部内联的, 合适的定义顺序是:

    ```LISP
    (declaim (inline f))
    (defun f ...)
    (declaim (notinline f))
    ```

        在 defun 表达式形式[form]前面的这个 inline 公告确保编译器[compiler]有机会保存内联展开所必需的信息, 并且跟在 defun 表达式形式[form]后面的这个 notinline 公告防止 f 在任何地方被内联展开.

        notinline 指出这个由 function-name 命名的函数不需要被内联编译. 一个编译器可以自由地忽略这个声明; 对这个指定的函数的调用必须被实现为非内联子程序调用.

        如果其中一个提及的函数有着词法上明显的局部定义 (例如通过 flet 或 labels), 那么这个声明应用于这个局部定义而不是那个全局函数定义.

        在 function-name 的编译器宏[compiler macro]定义存在的情况下, 一个 notinline 声明阻止了编译器宏[compiler macro]的使用. 可以使用 inline 声明来鼓励使用编译器宏[compiler macro]定义. inline 和 notinline 声明在 function-name 的词法可见定义是一个宏[macro]定义时是没有效果的.

        inline 和 notinline 声明可以是自由声明[free declaration]或绑定声明[bound declaration]. 出现在一个 flet 或 labels 表达式形式[form]的主体前的 inline 和 notinline 函数声明是绑定声明[bound declaration]. 在其他上下文中的这些声明是自由声明[free declaration].

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

* 参见(See Also):

        declare, declaim, proclaim 

### <span id = "DeclarationFTYPE">声明 FTYPE</span>

* 语法(Syntax):

        (ftype type function-name*)

* 参数(Arguments):

        function-name---一个函数名字[function name].
        type---一个类型指定符[type specifier].

* 合法上下文(Valid Context):

        声明[declaration]或公告[proclamation]

* 绑定类型的影响(Binding Types Affected):

        函数[function]

* 描述(Description):

        指出由那些函数名 function-names 命名的函数是 type 函数类型. 比如:

    ```LISP
    (declare (ftype (function (integer list) t) ith)
            (ftype (function (number) float) sine cosine))
    ```

        如果其中一个提及的函数有着词法上明显的局部定义 (例如通过 flet 或 labels), 那么这个声明应用于这个局部定义而不是那个全局函数定义. ftype 声明从不应用于变量绑定[binding] (见 type).

        这个 function-names 的词法上明显绑定不能是宏[macro]定义. (这是因为 ftype 声明每个函数名[function name]的函数定义是一个 function 的特定子类型, 而宏[macro]不表示函数[function].)

        ftype 声明可以是自由声明[free declaration]或绑定声明[bound declaration]. 出现在一个 flet 或 labels 表达式形式[form]的主体前的 ftype 函数声明是绑定声明[bound declaration]. 在其他上下文中的这些声明是自由声明[free declaration].

* 参见(See Also):

        declare, declaim, proclaim 

### <span id = "DeclarationDECLARATION">声明 DECLARATION</span>

* 语法(Syntax):

        (declaration name*)

* 参数(Arguments):

        name---一个符号[symbol].

* 绑定类型的影响(Binding Types Affected): None.

* 合法上下文(Valid Context):

        仅限公告[proclamation]

* 描述(Description):

        建议编译器, 每个名字 name 都是有效的, 但可能是非标准的声明名. 这样做的目的是告诉一个编译器不要发出用于另一个编译器或其他程序处理器的声明的警告.

* 示例(Examples):

    ```LISP
    (declaim (declaration author target-language target-machine))
    (declaim (target-language ada))
    (declaim (target-machine IBM-650))
    (defun strangep (x)
      (declare (author "Harry Tweeker"))
      (member x '(strange weird odd peculiar)))
    ```

* 参见(See Also):

        declaim, proclaim 

### <span id = "DeclarationOPTIMIZE">声明 OPTIMIZE</span>

* 语法(Syntax):

        (optimize {quality | (quality value)}*)

* 参数(Arguments):

        quality---一个优化质量[optimize quality].
        value--- 0, 1, 2, 或 3 这些整数[integer]的其中之一.

* 合法上下文(Valid Context):

        声明[declaration]或公告[proclamation]

* 绑定类型的影响(Binding Types Affected): None.

* 描述(Description):

        建议编译器应该根据指定的相应值 value 注意每个质量 quality. 每一个质量 quality 都必须是一种名为优化质量[optimize quality]的符号[symbol]; 标准优化质量[optimize quality]的名称和含义在下一段中展示.

            名称                意义
            compilation-speed  编译处理的速度
            debug              易于调试
            safety             运行时(run-time)错误检查
            space              代码大小和运行时(run-time)空间
            speed              对象代码的速度           

            Figure 3-25. 优化质量

        这里可能有其他的, 具体实现定义的[implementation-defined]优化质量[optimize quality].

        一个 0 的值 value 意味着对应的质量 quality 是完全不重要的, 而这个 3 表示极其重要的; 1 和 2 是中间的值, 这里 1 是中立的值. (quality 3) 可以缩写成 quality.

        注意有着优化 (safety 3), 或者只是 safety 的代码[code], 称为安全代码[safe code].

        如果质量 quality 以出现不止一种并且不同的[different]值, 那么后果是不可预料的.

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

* 参见(See Also):

        declare, declaim, proclaim, 章节 3.3.4 (声明的作用域)

* 注意(Notes):

        一个 optimize 声明从不适用于一个变量[variable]或一个函数[function]绑定[binding]. 一个 optimize 声明只能是自由声明[free declaration]. 关于更多信息, 见章节 3.3.4 (声明的作用域). 

### <span id = "DeclarationSPECIAL">声明 SPECIAL</span>

* 语法(Syntax):

        (special var*)

* 参数(Arguments):

        var---一个符号[symbol].

* 合法上下文(Valid Context):

        声明[declaration]或公告[proclamation]

* 绑定类型的影响(Binding Types Affected):

        变量[variable]

* 描述(Description):

        指定所有 var 命名的都是动态的. 这个指定符影响变量绑定[binding]和引用. 所有受影响的变量绑定[binding]都成为动态绑定[binding], 并且影响指向当前的动态绑定的变量引用. 比如:

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

        一个 special 声明不影响一个变量 var 的内部绑定; 内部绑定[binding]隐式地遮蔽一个 special 声明, 并且必须显式地重新声明为 special. special 声明从不应用于函数绑定[binding].

        special 声明可以是同时影响绑定和引用的绑定声明[bound declaration], 或只影响引用的自由声明[free declaration], 取决于声明是否被关联到一个变量绑定.

        当使用一个公告[proclamation]时, 一个 special 声明指定符[declaration specifier]应用于所有的绑定[binding]以及所有提到的变量的引用. 比如, 在

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

        在上面的扭曲代码中, 最外层和最内层的 y 的绑定[binding]是动态的, 但是中间绑定是词法的. 给 + 的两个参数是不一样的, 一个是一个值, 3, 是词法变量 y 的, 而另一个是名为 y 的动态变量的值 (巧合的是, 它的一个绑定在词法上围绕在它外层). 然而, 由于这个 x 总是是 special 的公告, 所有 x 的绑定和 x 的引用都是动态的.

* 参见(See Also):

        defparameter, defvar 

### <span id = "SpecialOperatorLOCALLY">特殊操作符 LOCALLY</span>

* 语法(Syntax):

        locally declaration* form* => result*

* 参数和值(Arguments and Values):

        Declaration---一个 declare 表达式[expression]; 不求值.
        forms---一个隐式的 progn[implicit progn].
        results---这些表达式形式[form]的值[value].

* 描述(Description):

        在这些给定声明 declarations 有效的一个词法环境[lexical environment]中, 依次对这些表达式形式  form 主体进行求值.

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

* 参见(See Also):

        declare

* 注意(Notes):

        locally 可以和 special 声明一起使用来影响变量[variable]的引用, 而不是绑定[binding].

        如果一个 locally 表达式形式[form]是一个顶层表达式形式[top level form], 这个主体表达式形式[form]也被当作顶层表达式形式[top level form]处理. 见章节 3.2.3 (文件编译). 

### <span id = "SpecialOperatorTHE">特殊操作符 THE</span>

* 语法(Syntax):

        the value-type form => result*

* 参数和值(Arguments and Values):

        value-type---一个类型指定符[type specifier]; 不求值.
        form---一个表达式形式[form]; 求值.
        results---从表达式形式 form 的求值[evaluation]得出的值[value]. 这些值[value]必须符合 value-type 所提供的类型[type]; 见下文.

* 描述(Description):

        the 指出了表达式形式 form 返回的值[values[1a]]是 value-type 表示的类型[type]. 如果任意结果 result 不是声明的类型, 后果是未定义的.

        假设 type 声明的那些值[value]确实是那些类型[type], 允许表达式形式 form 产生[yield]不同于 value-type 指定的数量的值. 出于检查它们的类型[type]的目的, 缺少的值被当作 nil.

        不考虑 value-type 声明的值[value]的数量, 这个 the 特殊表达式形式[special form]返回到值[value]的数量和表达式形式 form 返回的值[value]的数量一样.

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

        如果表达式形式 form 产生[yield]的值[value]不是由 value-type 指定的类型[type], 那么后果是没有定义的.

* 参见(See Also):

        values

* 注意(Notes):

        values 类型指定符可以用来表示多值[multiple values]的类型:

    ```LISP
    (the (values integer integer) (floor x y))
    (the (values string t)
        (gethash the-key the-string-table))
    ```

        setf 可以和 the 类型声明一起使用. 在这种情况下, 这个声明被转换为指定新值的表达式形式. 然后分析产生的 setf 表达式形式[form]. 

### <span id = "FunctionSPECIALOPERATORP">函数 SPECIAL-OPERATOR-P</span>

* 语法(Syntax):

        special-operator-p symbol => generalized-boolean

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        generalized-boolean---一个广义 boolean[generalized boolean].

* 描述(Description):

        如果符号 symbol 是一个特殊操作符[special operator]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (special-operator-p 'if) =>  true
    (special-operator-p 'car) =>  false
    (special-operator-p 'one) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个符号[symbol]应该发出一个 type-error 错误.

* 参见(See Also): None.

* 注意(Notes):

        从历史观点上说, 这个函数被称为 special-form-p. 这个名字最终被声明为用词不当并且修改, 因为它是对特殊操作符[special operator]返回 true, 而不是特殊表达式形式[special form]. 

### <span id = "FunctionCONSTANTP">函数 CONSTANTP</span>

* 语法(Syntax):

        constantp form &optional environment => generalized-boolean

* 参数和值(Arguments and Values):

        form---一个表达式形式[form].
        environment---一个环境[environment]对象[object]. 默认是 nil.
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果表达式形式 form 可以被这个具体实现[implementation]确定为在指定的环境 environment 中的一个常量形式[constant form], 则返回 true; 否则, 它返回 false 表示这个表达式形式[form]不是一个常量形式[constant form]或者它不能确定表达式形式[form]是否为一个常量形式[constant form].

        下面这些种类的表达式形式[form]被当作常量形式[constant form]:

        * 自求值对象[self-evaluating object] (像数字[number], 字符[character], 以及各种类型的数组[array]) 总是被当作常量形式[constant form]并且一定被 constantp 识别.

        * 常变量[constant variable], 例如关键字, 被 Common Lisp 定义作为常量的符号 (像 nil, t, 还有 pi), 还有在指定的环境 environment 中, 被用户使用 defconstant 声明为常量的符号总是被认为是常量形式[constant form], 并且一定被 constantp 所识别.

        * quote 表达式形式[form]总是被当作常量并且一定被 constantp 所识别.

        * 一个实现[implementation]允许, 但不是必须去检测额外的常量形式[constant form]. 如果确实如此, 那么使用环境 environment 中信息也是允许的, 但不是必需的. 对于 constantp 可能或可能不会返回 true 的常量形式[constant form]的例子是: (sqrt pi), (+ 3 2), (length '(a b c)), 还有 (let ((x 7)) (zerop x)).

        如果一个实现[implementation]选择使用环境 environment 信息, 可以使用诸如展开宏[macro]或执行函数内联之类的操作, 但不是必须; 然而, 展开编译器宏[compiler macro]是不允许的.

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

        全局环境的状态 (比如, 那些已经被声明为常变量[constant variable]的名字[name]的符号[symbol]).

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        defconstant

* 注意(Notes): None. 

