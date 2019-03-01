# 9. 状况

> * 9.1 [状况系统的概念](#ConditionSystemConcepts)
> * 9.2 [状况字典](#ConditionsDictionary)

## 9.1 <span id="ConditionSystemConcepts">状况系统的概念</span>

描述 Common Lisp 构造的不仅仅是它们旨在被使用情况下的行为 (见每个操作符[operator]声明的 "描述(Description)" 部分), 还有其他所有情况 (见每个操作符[operator]说明的 "异常情况(Exceptional Situations)").

一个情况(situation)是一个表达式在一个特定上下文中的求值. 一个状况[condition]是表示一个已经被检测到的特定情况的一个对象[object]. 状况[condition]是类[class] condition 的一个广义实例[generalized instance]. 在 Common Lisp 定义了一个状况[condition]类的层次结构. 一个状况[condition]有槽[slot], 可以包含这个状况[condition]表示的情况的相关数据.

一个错误(error)是在一个没有某个形式的干预下(不管是由用户交互还是在程序控制下), 正常程序执行不能正确地继续下去的情况. 不是所有错误都被检测到. 当一个错误未被检测到, 它的效果可以是依赖于具体实现的[implementation-dependent], 具体实现定义的[implementation-defined], 未指定(unspecified), 或者未定义(undefined)的. 见章节 1.4 (定义). 所有检测到的错误可以由状况[condition]表示, 但不是所有状况[condition]都表示错误.

发送(Signaling) 是一个过程, 通过这个过程一个状况[condition]可以修改一个程序中的控制流, 通过提升这个接下来可以被处理的状况[condition]. 函数 error, cerror, signal, 还有 warn 被用于发出状况[condition].

这个发送的过程涉及从一组活跃[active]处理者[handler]中选择和调用一个处理者[handler]. 一个处理者[handler]是一个单参数(也就是这个状况[condition])的函数[function], 它被调用来处理一个状况[condition]. 每个处理者[handler]都和一个状况[condition]类型[type]关联, 并且一个处理者[handler]只有在这个处理者[condition]关联类型[type]的状况[condition]上被调用.

活跃[active]处理者[handler]被动态地建立 (见 handler-bind 或 handler-case). 处理者[handler]在和这个发送者(signaler)所处的等价动态环境[dynamic environment]中被调用, 除了这个活跃的处理者集合只包含在这个要被调用的处理者被建立时活跃的那些.<!--TODO 待校对整理--> 发出一个状况[condition]在这个状况[condition]上没有副作用, 并且这里没有动态的状态被包含在一个状况[condition]中.

如果一个处理者[handler]被调用, 它可以通过以下三种方式来处理这个情况[situation]:

拒绝(Decline)

    它可以拒绝去处理[handle]这个状况[condition]. 这个方式通过简单地返回而不是转移控制来完成. 当这个发生的时候, 由这个处理者返回的任何值都会被忽略并且下一个最近被建立的处理者会被调用. 如果这里没有这样的处理者并且发送函数是 error 或 cerror, 那么在这个发送者的动态环境[dynamic environment]中进入调试器. 如果这里没有这样的处理者并且发送函数是 signal 或 warn, 那么这个发送函数简单地返回 nil.

处理(Handle)

    它可以通过执行一个控制的非局部转移来处理[handle]这个状况[condition]. 这个可以通过简单地使用 go, return, throw 来完成, 或者通过使用例如 abort 或 invoke-restart 函数更抽象地完成.

推迟(Defer)

    它可以推迟一个关于是否处理[handle]或拒绝[decline]的决定, 通过任何一种动作, 但是最常见的是通过发送另一个状况, 重发相同的状况, 或者强制进入调试器.

> * 9.1.1 [状况类型](#ConditionTypes)
> * 9.1.2 [创建状况](#CreatingConditions)
> * 9.1.3 [打印状况](#PrintingConditions)
> * 9.1.4 [发送和处理状况](#SignalingHandlingConditions)
> * 9.1.5 [断言](#Assertions)
> * 9.1.6 [关于状况系统的背景的注意事项](#NotesConditionSystemBackground)


### 9.1.1 <span id="ConditionTypes">状况类型</span>

下一段中列出了标准[standardized]状况[condition]类型[type]. 额外的状况[condition]类型[type]可以通过使用 define-condition 来定义.

    arithmetic-error                  floating-point-overflow   simple-type-error   
    cell-error                        floating-point-underflow  simple-warning      
    condition                         package-error             storage-condition   
    control-error                     parse-error               stream-error        
    division-by-zero                  print-not-readable        style-warning       
    end-of-file                       program-error             type-error          
    error                             reader-error              unbound-slot        
    file-error                        serious-condition         unbound-variable    
    floating-point-inexact            simple-condition          undefined-function  
    floating-point-invalid-operation  simple-error              warning             

    Figure 9-1. 标准状况类型

所有状况[condition]类型[type]都是类型[type] condition 的子类型[subtype]. 这也就是说, 当且仅当 c 是一个状况[condition]时以下形式成立

```LISP
(typep c 'condition) =>  true
```

具体实现[implementation]必须定义所有指定的子类型[subtype]关系. 除非特别注解, 本文档中所示的所有子类型[subtype]关系都不是相互排斥的. 一个状况[condition]继承自它的超类型[supertype]的结构.

类[class] condition 的元类没有被指定. 状况[condition]类型[type]的名字[name]可能被用于指定 define-condition 中的超类型[supertype]关系, 但是如果去尝试使用一个状况[condition]类型[type]作为一个 defclass 表达式形式[form]中的一个超类[superclass], 那么结果是未定义的.

下面这段中展示了定义状况[condition]类型[type]和创建状况[condition]的操作符[operator].

    define-condition  make-condition    

    Figure 9-2. 定义和创建状况的操作符.

下面这段展示了读取状况[condition]槽[slot]的值[value]的操作符[operator].

    arithmetic-error-operands   simple-condition-format-arguments  
    arithmetic-error-operation  simple-condition-format-control    
    cell-error-name             stream-error-stream                
    file-error-pathname         type-error-datum                   
    package-error-package       type-error-expected-type           
    print-not-readable-object   unbound-slot-instance              

    Figure 9-3. 读取状况槽的操作符.

#### 9.1.1.1 严重状况

一个严重状况[serious condition]是一个严重到如果没有处理就需要交互式干预的状况[condition]. 严重状况[serious condition]典型地通过 error 或 cerror 发出; 非严重状况[condition]通常用 signal 或 warn 发出. 

### 9.1.2 <span id="CreatingConditions">创建状况</span>

函数 make-condition 可以被用于显式构造一个状况[condition]对象[object]. 像 error, cerror, signal, 还有 warn 这样的函数在状况[condition]上操作并且可能隐式创建状况[condition]对象[object]. 像 ccase, ctypecase, ecase, etypecase, check-type, 还有 assert 这样的宏也可能隐式地创建 (以及发送[signal]) 状况[condition].

#### 9.1.2.1 状况标识符

状况系统中的许多函数都采用被标识为状况标识符[condition designator]的参数. 按照惯例, 那些参数被记作

    datum &rest arguments

合起来, 这个 datum 和 arguments 是 "一个默认类型 default-type 状况[condition]的标识符[designator]". 表示的状况[condition]如何被计算取决于这个 datum 的类型:

* 如果这个 datum 是一个命名状况[condition]类型[type]的符号[symbol] ...

    表示的状况[condition]是下面这个的结果

     (apply #'make-condition datum arguments)

* 如果这个 datum 是一个格式化控制[format control] ...

    表示的状况[condition]是下面这个的结果

     (make-condition defaulted-type 
                     :format-control datum
                     :format-arguments arguments)

    其中 defaulted-type 是 default-type 的一个子类型[subtype].

* 如果这个 datum 是一个状况[condition] ...

    这个表示的状况[condition]就是这个 datum 自身. 在这个情况下, 除非这个讨论中的操作符[operator]描述中另有说明, 否则实参[argument必须为 null; 这也就是说, 如果提供了任何其他实参 arguments 那么结果是未定义的.

注意, 这个 default-type 只有在 datum 字符串[string]被提供的情况下才被使用. 在其他情况中, 产生的状况不必是类型[type] default-type.

这里有一些说明, 关于不同的状况标识符[condition designator]如何表示等价的状况[condition]对象[object]:

```LISP
(let ((c (make-condition 'arithmetic-error :operator '/ :operands '(7 0))))
  (error c))
==  (error 'arithmetic-error :operator '/ :operands '(7 0))

(error "Bad luck.")
==  (error 'simple-error :format-control "Bad luck." :format-arguments '())
```

### 9.1.3 <span id="PrintingConditions">打印状况</span>

如果使用了给 define-condition 的 :report 参数, 就会定义一个打印函数, 无论何时当 \*print-escape\* 的值为 false 而且定义的状况[condition]要被打印时, 它会被调用. 这个函数被称为状况汇报器[condition reporter]; 它输出的文本被称为一个报告消息[report message].

当一个状况[condition]被打印并且 \*print-escape\* 是 false, 这个状况[condition]的状况汇报器[condition reporter]会被调用. 使用像 invoke-debugger, break, 和 warn 这样的函数来自动打印状况[condition].

当 \*print-escape\* 是 true 时, 这个对象[object]应该根据这个具体实现的风格以一种简短的方式打印 (比如, print-unreadable-object). 没有要求一个状况[condition]可以通过读取它的打印表示来重新构造.

没有为直接访问或调用状况汇报器[condition reporter]提供函数[function].

#### 9.1.3.1 状况汇报中的推荐风格

为了在向用户呈现报告消息[report message]时确保正确美观的结果, 推荐一些风格的惯例.

对于通过状况汇报器[condition reporter]输出的消息内容, 有一些风格上的建议, 但是在那些程序[program]上没有正式的需求. 如果一个程序[program]违反了某些消息的建议, 这条消息的显示可能没有遵循指导方针的那样美观, 但是这个程序[program]仍然被认为是符合规范的程序[conforming program].

在一个调用一个状况汇报器[condition reporter]的程序[program]或具体实现[implementation]上的这个要求更为强烈. 一个符合规范的程序[conforming program]必须被允许假设如果遵循这些样式准则, 将保持适当的美观. 在适当的情况下, 关于此类程序的任何具体要求都在下面明确提到.

> * 9.1.3.1.1 [在状况汇报中的大写和标点符号](#CPCR)
> * 9.1.3.1.2 [在状况汇报中领导和尾随的新行](#LTNCR)
> * 9.1.3.1.3 [在状况汇报中内嵌的新行](#ENCR)
> * 9.1.3.1.4 [关于在状况汇报中的 tab 的注意事项](#NTCR)
> * 9.1.3.1.5 [在状况汇报中提及包含函数](#MCFCR)

##### 9.1.3.1.1 <span id="CPCR">在状况汇报中的大写和标点符号</span>

一个报告消息[report message]建议为一个完整的句子, 以适当的大小写并且加标点. 在英语中, 比如, 这个意味着第一个字符应该为大写, 并且这里应该有一个尾部的句号.

```LISP
(error "This is a message")  ; Not recommended
(error "this is a message.") ; Not recommended

(error "This is a message.") ; Recommended instead
```

##### 9.1.3.1.2 <span id="LTNCR">在状况汇报中领导和尾随的新行</span>

一个报告消息[report message]不建议以任何引导文本开始, 像 "Error:" 或 "Warning:" 这样或仅为 freshline 或 newline. 如果对于这个上下文合适, 这样的文本由调用这个状况汇报器[condition reporter]的程序来添加.

一个报告消息[report message]不建议跟着一个尾部的 freshline 或 newline. 如果对于这个上下文合适, 这样的文本由调用这个状况汇报器[condition reporter]的程序来添加.

```LISP
(error "This is a message.~%")   ; Not recommended
(error "~&This is a message.")   ; Not recommended
(error "~&This is a message.~%") ; Not recommended

(error "This is a message.")     ; Recommended instead
```

##### 9.1.3.1.3 <span id="ENCR">在状况汇报中内嵌的新行</span>

如果报告消息[report message]尤其的长, 那么它包含一个或多个内嵌的换行[newline]是允许且适当的.

如果调用程序在消息的第一行中插入了一些额外的前缀(像 "Error:" 或 ";; Error:") , 它也必须确保一个合适的前缀会被添加到这个输出的后续每一行中, 这样一来被状况汇报器[condition reporter]输出的信息的左边界始终会是正确对齐的.

```LISP
(defun test ()
  (error "This is an error message.~%It has two lines."))

;; Implementation A
(test)
This is an error message.
It has two lines.

;; Implementation B
(test)
;; Error: This is an error message.
;;        It has two lines.

;; Implementation C
(test)
>> Error: This is an error message. 
          It has two lines.
```

##### 9.1.3.1.4 <span id="NTCR">关于在状况汇报中的 tab 的注意事项</span>

因为报告消息[report message]的缩进可能会以任意数量转移到右边或左边, 应该对这个不完全标准的字符 <Tab> 特别关注(在支持这样的一个字符[character]的那些具体实现[implementation]中). 除非这个具体实现[implementation]在这个上下文中特别定义了它的行为, 否则应该避免它的使用. 

##### 9.1.3.1.5 <span id="MCFCR">在状况汇报中提及包含函数</span>

这个包含函数的名字通常不应该在报告消息[report message]中被提及. 假定调试器将使这些信息在必要和适当的情况下可访问. 

### 9.1.4 <span id="SignalingHandlingConditions">发送和处理状况</span>

状况系统的操作依赖于活跃的可应用处理者[applicable handler]的顺序, 从最近的到最久的.

每个处理者[handler]和一个类型指定符[type specifier]相关联, 这个指定符必须指定一个类型[type] condition 的子类型[subtype]. 如果一个状况[condition]是由关联的那个类型指定符[type specifier]指定的类型[type], 那么就说这个处理者[handler]对于这个状况[condition]是可应用的[application].

活跃[active]处理者[handler]通过使用 handler-bind (或者一个基于 handler-bind 的简写, 比如 handler-case 或 ignore-errors) 来建立.

活跃[active]处理者[handler]可以被建立在其他活跃[active]处理者[handler]的动态作用域中. 在程序执行期间的任何点, 这里都有一组活跃[active]处理者[handler]. 当发出一个状况[condition]时, 针对这个状况[condition]的最新近[most recent]的活跃可应用处理者[applicable handler]会从这个组中被选择出来. 给定一个状况[condition], 活跃可应用处理者[applicable handler]的最新近顺序通过下面两条规则来定义:

1. 如果在活跃处理者集合 H1 中的那些处理者被建立时处理者集合 H2 中的那些处理者是活跃的, 那么 H1 中的每个处理者比 H2 中的更新近.

2. 使 h1 和 h2 是相同表达式形式[form]建立的两个可应用的活跃处理者. 如果在这个建立它们的表达式形式[form]中 h1 被定义在 h2 的左边, 那么 h1 比 h2 更新近.

一旦一个处理者绑定表达式形式[form] (例如 handler-bind 或 handler-case) 中的一个处理者被选择, 那个表达式形式[form]中的所有处理者变成对于这个发送过程的剩余部分是非活跃的. 当这个选择的处理者[handler]运行时, 那个表达式形式[form]建立的其他处理者[handler]没有是活跃的. 这也就是说, 如果这个处理者[handler]拒绝处理, 那个表达式形式[form]建立的其他处理者不会被考虑为可能的调用.

下面这一段中展示了和处理状况[condition]相关的操作符[operator].

    handler-bind  handler-case  ignore-errors  

    Figure 9-4. 状况处理相关的操作符.

> * 9.1.4.1 [发送](#Signaling)
> * 9.1.4.2 [重启动](#Restarts)

#### 9.1.4.1 <span id="Signaling">发送</span>

当发送一个状况[condition]时, 最新近的可应用的活跃[active]处理者[handler]会被调用. 有时候一个处理者会通过没有控制转移的简单返回来拒绝. 在这样的情况下, 下一个最新近的活跃可应用的处理者会被调用.

如果对于一个被发送的状况[condition]这里没有可应用的处理者, 或者如果所有可应用的处理者都拒绝了, 那么这个状况[condition]就是未处理的.

如果发送的状况[condition]没有被处理, 不管它们的类型[type], 那么函数 cerror 和 error 调用这个交互式的状况[condition]处理者 (就是这个调试器) 而不是返回. 相比之下, 如果发送的状况[condition]没有被处理, 不管它们的类型[type], signal 就返回 nil.

变量[variable] \*break-on-signals\* 可以被用于在这个发送过程开始前进入调试器.

下面这段展示了和状况[condition]的发送相关的已定义的名字[defined name].

    *break-on-signals*  error   warn  
    cerror              signal        

    Figure 9-5. 状况的发送相关的定义的名字.

##### 9.1.4.1.1 重发一个状况

在一个特定的状况[condition]对象[object]的发送过程的动态范围[dynamic extent]期间, 当且仅当两种情况下表示的情况[situation]相同时允许再次发送同一个状况[condition]对象[object].

比如, 一个处理者[handler]可能合理地发送[signal]这个作为它的实参[argument]的状况[condition]对象[object]来允许更外部的处理者[handler]第一时机去处理[handler]这个状况. (这样一个处理者[handler]有时被称作 "默认处理者(default handler)".) 这个行为是允许的因为第二个发送过程处理的情况[situation]确实是相同的情况[situation].

另一方面, 在一个通过用一个对 signal 的调用来打断用户进程进而实现异步键盘事件的具体实现[implementation]中, 不允许两个不同的异步键盘事件在同一时间对不同情况发送[signal]相同[identical]状况[condition]. 

#### 9.1.4.2 <span id="Restarts">重启动</span>

交互式状况处理者只通过控制的非局部转移到专门定义的重启动[restart]来退出, 这个重启动可以通过系统或用户代码来设置. 转移控制到一个重启动被称为 "调用" 这个重启动. 类似于处理者, 活跃的重启动[restart]也被动态地确立, 并且只有活跃的重启动[restart]可以被调用. 一个活跃的重启动[restart]可以被用户从调试器中或者被程序使用 invoke-restart 来调用.

一个重启动[restart]包含一个在这个重启动[restart]被调用时要被调用的函数[function], 一个被用于查找或调用这个重启动[restart]的可选名字, 以及一个用于调试器来使户手动调用一个重启动[restart]的可选交互式信息集合.

一个重启动[restart]的名字被 invoke-restart 使用. 只能在调试器中调用的重启动[restart]不需要名字.

重启动[restart]可以通过使用 restart-bind, restart-case, 和 with-simple-restart 来建立. 一个重启动[restart]函数自身可以调用任何其他的在这个函数所属的重启动[restart]建立时是活跃的重启动[restart].

通过一个 restart-bind 表达式形式[form], 一个 restart-case 表达式形式[form], 或者一个 with-simple-restart 表达式形式[form]建立的重启动[restart]有着动态范围[dynamic extent], 这个范围延伸到这个表达式形式[form]执行期间.

相同名字的重启动[restart]可以根据下面两条规则来从最久的到最近的排序:

1. 如果活跃的重启动集合 R1 中的那些重启动[restart]被建立时集合 R2 中的重启动[restart]是活跃的, 那么 R1 中的每个重启动[restart]都比 R2 中的每个重启动[restart]更近.

2. 使 r1 和 r2 为相同表达式形式[form]建立的两个相同名字的活跃重启动[restart]. 如果在建立它们的表达式形式[form]中 r1 被定义在 r2 的左边, 那么 r1 比 r2 更新近.

如果一个重启动[restart]被调用但是没有转移控制, 那么这个重启动[restart]函数产生的值会被调用这个重启动的函数返回, 不管是 invoke-restart 还是 invoke-restart-interactively.

> * 9.1.4.2.1 [重启动的交互式使用](#InteractiveUseRestarts)
> * 9.1.4.2.2 [重启动的接口](#InterfacesRestarts)
> * 9.1.4.2.3 [重启动测试](#RestartTests)
> * 9.1.4.2.4 [关联重启动和状况](#AssociatingRestartCondition)

##### 9.1.4.2.1 <span id="InteractiveUseRestarts">重启动的交互式使用</span>

关于交互式处理, 一个重启动[restart]需要两个信息: 一个汇报函数和一个交互式函数.

这个汇报函数被一个例如调试器的程序使用来呈现这个重启动[restart]会采取的动作的描述. 这个汇报函数是通过给 restart-bind 的 :report-function 关键字或者给 restart-case 的 :report 关键字来指定和建立.

这个交互式函数可以使用给 restart-bind 的 :interactive-function 关键字或给 restart-case 的 :interactive 关键字来指定, 它在重启动[restart]被交互式调用时, 例如从调试器中, 会被用来产生一个合适的参数列表.

invoke-restart 调用和给 invoke-restart 的第一个参数相同名字的最近建立的重启动[restart]. 如果一个重启被调试器交互式调用并且没有转移控制而是返回值, 那么这个调试器在这些值上的准确动作是由具体实现定义的[implementation-defined]. 

##### 9.1.4.2.2 <span id="InterfacesRestarts">重启动的接口</span>

一些重启动[restart]有着函数接口, 例如 abort, continue, muffle-warning, store-value, 还有 use-value. 它们是内部使用 find-restart 和 invoke-restart 的普通函数, 有着和它们操纵的重启动[restart]相同的名字, 并且简单地出于标记方便而被提供.

下面这段中展示了和重启动[restart]相关的已定义的名字[defined name].

    abort             invoke-restart-interactively  store-value          
    compute-restarts  muffle-warning                use-value            
    continue          restart-bind                  with-simple-restart  
    find-restart      restart-case                                       
    invoke-restart    restart-name                                       

    Figure 9-6. 重启动相关的定义的名字. 

##### 9.1.4.2.3 <span id="RestartTests">重启动测试</span>

每个重启动[restart]都有一个关联的测试, 它是一个单参数(一个状况[condition]或者 nil)的函数, 如果这个重启动[restart]在当前情况[situation]下应该是可见的就返回 true. 这个测试通过给 restart-bind 的 :test-function 选项或者给 restart-case 的 :test 选项创建. 

##### 9.1.4.2.4 <span id="AssociatingRestartCondition">关联重启动和状况</span>

一个重启动[restart]可以通过 with-condition-restarts 来和一个状况[condition]显式关联, 或者通过 restart-case 来隐式关联. 因此一个关联有着动态范围[dynamic extent].

一个单个重启动[restart]可以同时和多个状况[condition]关联. 一个单独状况[condition]同时也可以和多个重启动[restart]关联.

和一个特定状况[condition]关联的活跃重启动可以通过调用例如 find-restart 函数[function]并提供这个状况[condition]作为 condition 实参[argument]来检测. 没有任何关联状况的活跃重启动也可以通过以无 condition 实参[argument]或者为这个实参[argument]提供 nil 值来调用这样一个函数来检测.

### 9.1.5 <span id="Assertions">断言</span>

基于键匹配, 表达式形式求值, 以及类型[type]的状况[condition]的条件发送由断言操作符[operator]处理. 下一段中展示了和断言相关的操作符[operator].

    assert  check-type  ecase      
    ccase   ctypecase   etypecase  

    Figure 9-7. 断言相关的操作符. 


### 9.1.6 <span id="NotesConditionSystemBackground">关于状况系统的背景的注意事项</span>

有关本节中详细描述的抽象概念的背景引用, 见 Exceptional Situations in Lisp. 尽管这篇论文的详情对这份文档没有约束力, 但可能有助于为理解这种资料建立概念基础. 


## 9.2 <span id="ConditionsDictionary">状况字典</span>

> * [状况类型 CONDITION](#CT-CONDITION)
> * [状况类型 WARNING](#CT-WARNING)
> * [状况类型 STYLE-WARNING](#CT-STYLE-WARNING)
> * [状况类型 SERIOUS-CONDITION](#CT-SERIOUS-CONDITION)
> * [状况类型 ERROR](#CT-ERROR)
> * [状况类型 CELL-ERROR](#CT-CELL-ERROR)
> * [函数 CELL-ERROR-NAME](#F-CELL-ERROR-NAME)
> * [状况类型 PARSE-ERROR](#CT-PARSE-ERROR)
> * [状况类型 STORAGE-CONDITION](#CT-STORAGE-CONDITION)
> * [宏 ASSERT](#M-ASSERT)
> * [函数 ERROR](#F-ERROR)
> * [函数 CERROR](#F-CERROR)
> * [宏 CHECK-TYPE](#M-CHECK-TYPE)
> * [状况类型 SIMPLE-ERROR](#CT-SIMPLE-ERROR)
> * [函数 INVALID-METHOD-ERROR](#F-INVALID-METHOD-ERROR)
> * [函数 METHOD-COMBINATION-ERROR](#F-METHOD-COMBINATION-ERROR)
> * [函数 SIGNAL](#F-SIGNAL)
> * [状况类型 SIMPLE-CONDITION](#CT-SIMPLE-CONDITION)
> * [函数 SIMPLE-CONDITION-FORMAT-CONTROL, SIMPLE-CONDITION-FORMAT-ARGUMENTS](#F-SCFC-SCFA)
> * [函数 WARN](#F-WARN)
> * [状况类型 SIMPLE-WARNING](#CT-SIMPLE-WARNING)
> * [函数 INVOKE-DEBUGGER](#F-INVOKE-DEBUGGER)
> * [函数 BREAK](#F-BREAK)
> * [变量 *DEBUGGER-HOOK*](#V-DEBUGGER-HOOK)
> * [变量 *BREAK-ON-SIGNALS*](#V-BREAK-ON-SIGNALS)
> * [宏 HANDLER-BIND](#M-HANDLER-BIND)
> * [宏 HANDLER-CASE](#M-HANDLER-CASE)
> * [宏 IGNORE-ERRORS](#M-IGNORE-ERRORS)
> * [宏 DEFINE-CONDITION](#M-DEFINE-CONDITION)
> * [函数 MAKE-CONDITION](#F-MAKE-CONDITION)
> * [System Class RESTART](#SC-RESTART)
> * [函数 COMPUTE-RESTARTS](#F-COMPUTE-RESTARTS)
> * [函数 FIND-RESTART](#F-FIND-RESTART)
> * [函数 INVOKE-RESTART](#F-INVOKE-RESTART)
> * [函数 INVOKE-RESTART-INTERACTIVELY](#F-INVOKE-RESTART-INTERACTIVELY)
> * [宏 RESTART-BIND](#M-RESTART-BIND)
> * [宏 RESTART-CASE](#M-RESTART-CASE)
> * [函数 RESTART-NAME](#F-RESTART-NAME)
> * [宏 WITH-CONDITION-RESTARTS](#M-WITH-CONDITION-RESTARTS)
> * [宏 WITH-SIMPLE-RESTART](#M-WITH-SIMPLE-RESTART)
> * [重启动 ABORT](#R-ABORT)
> * [重启动 CONTINUE](#R-CONTINUE)
> * [重启动 MUFFLE-WARNING](#R-MUFFLE-WARNING)
> * [重启动 STORE-VALUE](#R-STORE-VALUE)
> * [重启动 USE-VALUE](#R-USE-VALUE)
> * [函数 ABORT, CONTINUE, MUFFLE-WARNING, STORE-VALUE, USE-VALUE](#F-ABORT-CONTINUE-MW-SV-UV)


### <span id="CT-CONDITION">状况类型 CONDITION</span>

* 类优先级列表(Class Precedence List):

        condition, t

* 描述(Description):

        所有状况[condition]类型, 不管是错误或是非错误, 必须继承自这个类型[type].

        在指定的类型[type] condition 的子类型[subtype]中不允许额外的子类型[subtype]关系, 除非在这个文本中明确提及; 然而具体实现允许去引入一些额外类型[type]并且这些类型[type]中的一个可以是类型[type] condition 任何数量子类型[subtype]的一个子类型[subtype].

        一个用户定义的状况[condition]类型[type]是否有着可以被 with-slots 访问的槽[slot]是依赖于具体实现的[implementation-dependent]. 此外, 即便在一个具体实现[implementation]中用户定义的状况[condition]类型[type]有着槽[slot], 但是这个文档中定义的任何状况[condition]类型[type]是否有着槽是依赖于具体实现的[implementation-dependent], 如果它们确实有, 那么它们的名字[name]也是依赖于具体实现的[implementation-dependent]; 只有这个规范中记录的读取器函数可以被可移植代码所依赖.

        符合规范的代码[conforming code]必须遵守下面这个和状况[condition]相关的约束:

        * define-condition, 而不是 defclass, 必须被用于定义新的状况[condition]类型[type].

        * make-condition, 而不是 make-instance, 必须被用于显式创建状况[condition]对象[object].

        * define-condition 的这个 :report 选项, 而不是 defmethod 对于 print-object, 必须被用于定义一个状况汇报器.

        * slot-value, slot-boundp, slot-makunbound, 和 with-slots 一定不能在状况[condition]对象[object]上使用. 反而, 应该使用合适的访问器函数 (通过 define-condition 定义). 


### <span id="CT-WARNING">状况类型 WARNING</span>

* 类优先级列表(Class Precedence List):

        warning, condition, t

* 描述(Description):

        类型[type] warning 包含所有警告的类型.

* 也见(See Also):

        style-warning 


### <span id="CT-STYLE-WARNING">状况类型 STYLE-WARNING</span>

* 类优先级列表(Class Precedence List):

        style-warning, warning, condition, t

* 描述(Description):

        类型[type] style-warning 包括那些表示代码[code]是符合规范的代码[conforming code]但是仍然被认为是错误的或者不符合标准的情况[situation]的状况[condition].

* 也见(See Also):

        muffle-warning

* 注意(Notes):

        如果一个具体实现[implementation]遇到使用废弃特性的代码[code]或者不美观的或无效的代码[code], 它可能发出这样一个状况[condition].

        一个 '没有被使用的变量(unused variable)' 警告必须是 style-warning 类型[type].

        一般而言, 代码是错误的还是不合规范的问题是由处理代码[code]的工具做出的一个主观决定. 这样的意图是, 无论何时这样的一个工具想在主观理由上抱怨代码, 它应该使用这个状况[condition]类型[type], 以便那些希望去重定向或者抑制多余警告的用户可以做这些而不用担心他们被重定向或抑制其他更严重的警告. 

### <span id="CT-SERIOUS-CONDITION">状况类型 SERIOUS-CONDITION</span>

* 类优先级列表(Class Precedence List):

serious-condition, condition, t

* 描述(Description):

        所有严重到如果没被处理就需要交互式干预的状况[condition]应该继承自类型[type] serious-condition. 提供这种状况类型主要是为了使它可以被包含作为其他状况[condition]类型[type]的超类[superclass]; 它不打算被直接发送.

* 注意(Notes):

        发送一个严重状况[serious condition]自身不会强制进入调试器. 然而, 除非在程序员可以确保处理[handle]不了严重状况[serious condition]不会造成伤害的不寻常情况下, 否则这样一个状况[condition]通常使用 error 来发送而不是 signal, 进而确保这个程序在没有处理这个状况[condition]的情况下不会继续下去. (但反之, 使用 signal 而不是 error 去发送不是严重状况[serious condition]的状况是传统做法, 因为正常情况下, 处理不了非严重情况并不是进入调试器的原因.) 


### <span id="CT-ERROR">状况类型 ERROR</span>

* 类优先级列表(Class Precedence List):

        error, serious-condition, condition, t

* 描述(Description):

        这个类型[type] error 由所有表示错误[error]的状况[condition]组成. 


### <span id="CT-CELL-ERROR">状况类型 CELL-ERROR</span>

* 类优先级列表(Class Precedence List):

        cell-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] cell-error 由发生在位置访问[eccess]期间的错误状况组成. 违规的存储格(cell)的名字由 make-condition 的 :name 初始化参数来初始化, 通过函数[function] cell-error-name 来访问.

* 也见(See Also):

        cell-error-name 


### <span id="F-CELL-ERROR-NAME">函数 CELL-ERROR-NAME</span>

* 语法(Syntax):

        cell-error-name condition => name

* 参数和值(Arguments and Values):

        condition---一个 cell-error 类型[type]的状况[condition].
        name---一个对象[object].

* 描述(Description):

        返回那个状况 condition 所表示的情况[situation]中违规的存储格(cell)的名字[name].

        结果的性质取决于状况 condition 具体类型. 比如, 如果这个状况 condition 是 unbound-variable 类型[type], 那么这个结果是那个要被访问的未绑定变量[unbound variable]的名字[name], 如果这个状况 condition 是 undefined-function 类型[type], 那么这个就是那个要被访问的未绑定函数[undefined function ]的名字[name], 而当这个状况 condition 是 unbound-slot 类型[type], 这个就是要被访问的槽[slot]的名字[name].

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cell-error, unbound-slot, unbound-variable, undefined-function, 章节 9.1 (状况系统的概念)

* 注意(Notes): None. 


### <span id="CT-PARSE-ERROR">状况类型 PARSE-ERROR</span>

* 类优先级列表(Class Precedence List):

        parse-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] parse-error 由解析相关的错误状况组成.

* 也见(See Also):

        parse-namestring, reader-error 


### <span id="CT-STORAGE-CONDITION">状况类型 STORAGE-CONDITION</span>

* 类优先级列表(Class Precedence List):

        storage-condition, serious-condition, condition, t

* 描述(Description):

        类型 storage-condition 由内存管理问题相关的严重状况组成, 这些状况可能是由于依赖于具体实现[implementation-dependent]的限制而不是符合规范的程序[conforming program]中的语义错误, 并且如果没有被处理通常需要进入到调试器中. 根据具体实现[implementation]的细节, 这些可能包括堆栈溢出, 内存区域溢出和存储耗尽等问题.

* 注意(Notes):

        尽管由于一些 Common Lisp 操作符被定义用来创建对象[object], 可能会发出存储状况[storage-condition], 但是那些不是被定义用来创建对象[object]的操作符是否会创建它们并且是否也可能发出存储状况[storage-condition]是未指定的. 同样的, 求值器自身也可能创建对象[object]并且因此可能发出 storage-condition. (自然的假设可能是, 这样的对象[object]创建自然是低效的, 但即使这样做也是依赖于具体实现的[implementation-dependent].) 通常, 存储分配如何完成的整个问题都是依赖于具体实现的[implementation-dependent], 因此任何操作符在任何时间都可能发出 storage-condition. 由于这样一个状况[condition]是具体实现[implementation]或镜像的限制的象征而不是一个程序[program]中的错误, 因此类型[type] storage-condition 的对象[object]不是类型[type] error. 


### <span id="M-ASSERT">宏 ASSERT</span>

* 语法(Syntax):

        assert test-form [(place*) [datum-form argument-form*]]

=> nil

* 参数和值(Arguments and Values):

        test-form---一个表达式形式[form]; 总是求值的.
        place---一个位置[place]; 如果发出一个错误就求值.
        datum-form---一个求值为一个数据 datum 的表达式形式[form]. 每次要发出一个错误就求值, 如果没有错误发出就一次都不求值.
        argument-form---求值为一个参数 argument 的一个表达式形式[form]. 每次要发出一个错误就求值, 如果没有错误发出就一次都不求值.
        datum, arguments---一个默认类型 error 的状况[condition]的标识符[designator]. (这些标识符[designator]是求值数据表达式形式 datum-form 和每个参数表达式形式 argument-form 的结果.)

* 描述(Description):

        assert 确保这个 test-form 求值为 true. 如果 test-form 求值为 false, assert 发出一个可校正的[correctable]错误[error] (用 datum 和 arguments 来表示). 从这个错误中使用 continue 重启动[restart]来继续下去使得用户在 assert 再一次求值 test-form 之前修改位置 places 的值成为可能. 如果这个 test-form 的值不是 nil [non-nil], assert 返回 nil.

        这些位置 places 是 test-form 所依赖的对数据的广义引用[generalized reference], 在试图校正这个错误时, 用户可以改变它们的值. 每个位置 place 的子表达式形式[subform]只有在一个错误被发出时被求值, 并且在这个错误被再次发出时可能被再次求值 (在没有实际修正这个问题的情况下继续之后). 这些位置 places 的求值顺序没有被指定; 见章节 5.1.1.1 (位置的子表达式形式求值). 如果一个被提供的位置 place 表达式形式[form]产生值的数量超出了这些存储变量的数量, 额外的值会被忽略. 如果提供的这个表达式形式[form]产生的值的数量少于这些存储变量, 那么缺少的值被设置为 nil.

* 示例(Examples):

    ```LISP
    (setq x (make-array '(3 5) :initial-element 3))
    =>  #2A((3 3 3 3 3) (3 3 3 3 3) (3 3 3 3 3))
    (setq y (make-array '(3 5) :initial-element 7))
    =>  #2A((7 7 7 7 7) (7 7 7 7 7) (7 7 7 7 7))
    (defun matrix-multiply (a b)
      (let ((*print-array* nil))
        (assert (and (= (array-rank a) (array-rank b) 2)
                      (= (array-dimension a 1) (array-dimension b 0)))
                (a b)
                "Cannot multiply ~S by ~S." a b)
                (really-matrix-multiply a b))) =>  MATRIX-MULTIPLY
    (matrix-multiply x y)
    >>  Correctable error in MATRIX-MULTIPLY: 
    >>  Cannot multiply #<ARRAY ...> by #<ARRAY ...>.
    >>  Restart options:
    >>   1: You will be prompted for one or more new values.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Value for A: x
    >>  Value for B: (make-array '(5 3) :initial-element 6)
    =>  #2A((54 54 54 54 54)
          (54 54 54 54 54)
          (54 54 54 54 54)
          (54 54 54 54 54)
          (54 54 54 54 54))

    (defun double-safely (x) (assert (numberp x) (x)) (+ x x))
    (double-safely 4) 
    =>  8
    
    (double-safely t)
    >>  Correctable error in DOUBLE-SAFELY: The value of (NUMBERP X) must be non-NIL.
    >>  Restart options:
    >>   1: You will be prompted for one or more new values.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Value for X: 7
    =>  14
    ```

* 受此影响(Affected By):

        *break-on-signals*

        活跃的状况处理者集合.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        check-type, error, 章节 5.1 (广义引用)

* 注意(Notes):

        调试器不需要在错误信息中包含这个 test-form , 并且这些位置 places 也不应该被包含在这个信息中, 但是它们应该供用户查看. 如果用户给了  "continue" 命令, 那么任何这些引用的值都能被修改. 这个细节取决于具体实现的用户接口风格. 


### <span id="F-ERROR">函数 ERROR</span>

* 语法(Syntax):

        error datum &rest arguments =>|

* 参数和值(Arguments and Values):

        datum, arguments---一个默认类型 simple-error 的状况[condition]的标识符[designator].

* 描述(Description):

        error 实际上在表示的状况[condition]上调用 signal.

        如果这个状况[condition]没有被处理, (invoke-debugger condition) 就会被执行. 作为调用 invoke-debugger 的后果, error 不能直接返回; 从 error 中仅有的退出方式可以通过在一个处理者中非局部转移控制或使用交互式调试命令来实现.

* 示例(Examples):

    ```LISP
    (defun factorial (x)
      (cond ((or (not (typep x 'integer)) (minusp x))
              (error "~S is not a valid argument to FACTORIAL." x))
            ((zerop x) 1)
            (t (* x (factorial (- x 1))))))
    =>  FACTORIAL
    (factorial 20)
    =>  2432902008176640000
    (factorial -1)
    >>  Error: -1 is not a valid argument to FACTORIAL.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return to Lisp Toplevel.
    >>  Debug> 

    (setq a 'fred)
    =>  FRED
    (if (numberp a) (1+ a) (error "~S is not a number." A))
    >>  Error: FRED is not a number.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return to Lisp Toplevel.
    >>  Debug> :Continue 1
    >>  Return to Lisp Toplevel.
    
    (define-condition not-a-number (error) 
                      ((argument :reader not-a-number-argument :initarg :argument))
      (:report (lambda (condition stream)
                  (format stream "~S is not a number."
                          (not-a-number-argument condition)))))
    =>  NOT-A-NUMBER
    
    (if (numberp a) (1+ a) (error 'not-a-number :argument a))
    >>  Error: FRED is not a number.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return to Lisp Toplevel.
    >>  Debug> :Continue 1
    >>  Return to Lisp Toplevel.
    ```

* 副作用(Side Effects):

        那个指定的状况的那些处理者[handler], 如果存在, 会被调用并且可能有副作用. 程序执行可能停止, 并且可能进入到调试器中.

* 受此影响(Affected By):

        已存在的处理者绑定.

        *break-on-signals*

* 异常情况(Exceptional Situations): 

        如果 datum 和 arguments 不是一个状况[condition]的标识符[designator], 就发出一个类型[type] type-error 的错误.

* 也见(See Also):

        cerror, signal, format, ignore-errors, *break-on-signals*, handler-bind, 章节 9.1 (状况系统的概念)

* 注意(Notes):

        某些具体实现可能为从单独的堆栈结构中交互式返回提供调试器命令. 然而, 程序员应该有信心去编写像下面这样的代码:

        (defun wargames:no-win-scenario ()
          (if (error "pushing the button would be stupid."))
          (push-the-button))

        在这种情况下, error 不可能返回, 按钮不会被按下.

        虽然这个程序的意义是明确的, 并且它可能被正式的定理证明是"安全的", 但是这样的证明并不能保证程序的执行是安全的. 众所周知, 编译器有bug, 计算机有信号故障, 而人类则以不可能预测的方式进行手动干预. 这些种类的错误, 虽然超出了状况系统对于正式建模的范围, 但它不会超出编写代码时应该认真考虑的范围, 而这些代码可能会有本例所暗示的那种广泛的影响. 


### <span id="F-CERROR">函数 CERROR</span>

* 语法(Syntax):

        cerror continue-format-control datum &rest arguments => nil

* 参数和值(Arguments and Values):

        Continue-format-control---一个格式化控制字符串[format control].
        datum, arguments---一个默认类型 simple-error 的状况[condition]的标识符[designator].

* 描述(Description):

        cerror 实际上在名为 datum 的状况[condition]上调用 error. 就像任何隐式调用 error 的函数一样, 如果这个状况[condition]没有被处理, (invoke-debugger condition) 会被执行. 正在发送中, 以及在调试器中, 如果到达的话, 使用 continue 重启动[restart]来继续代码的执行(换句话说, 从 cerror 中返回)是可能的.

        如果 datum 是一个状况[condition], 可以提供 arguments, 但是要和格式化控制 continue-format-control 一起使用.

* 示例(Examples):

    ```LISP
    (defun real-sqrt (n)
      (when (minusp n)
        (setq n (- n))
        (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
      (sqrt n))

    (real-sqrt 4)
    =>  2.0

    (real-sqrt -9)
    >>  Correctable error in REAL-SQRT: Tried to take sqrt(-9).
    >>  Restart options:
    >>   1: Return sqrt(9) instead.
    >>   2: Top level.
    >>  Debug> :continue 1
    =>  3.0
    
    (define-condition not-a-number (error)
      ((argument :reader not-a-number-argument :initarg :argument))
      (:report (lambda (condition stream)
                  (format stream "~S is not a number." 
                          (not-a-number-argument condition)))))
    
    (defun assure-number (n)
      (loop (when (numberp n) (return n))
            (cerror "Enter a number."
                    'not-a-number :argument n)
            (format t "~&Type a number: ")
            (setq n (read))
            (fresh-line)))

    (assure-number 'a)
    >>  Correctable error in ASSURE-NUMBER: A is not a number.
    >>  Restart options:
    >>   1: Enter a number.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Type a number: 1/2
    =>  1/2

    (defun assure-large-number (n)
      (loop (when (and (numberp n) (> n 73)) (return n))
            (cerror "Enter a number~:[~; a bit larger than ~D~]."
                    "~*~A is not a large number." 
                    (numberp n) n)
            (format t "~&Type a large number: ")
            (setq n (read))
            (fresh-line)))
    
    (assure-large-number 10000)
    =>  10000

    (assure-large-number 'a)
    >>  Correctable error in ASSURE-LARGE-NUMBER: A is not a large number.
    >>  Restart options:
    >>   1: Enter a number.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Type a large number: 88
    =>  88

    (assure-large-number 37)
    >>  Correctable error in ASSURE-LARGE-NUMBER: 37 is not a large number.
    >>  Restart options:
    >>   1: Enter a number a bit larger than 37.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Type a large number: 259
    =>  259
    
    (define-condition not-a-large-number (error)
      ((argument :reader not-a-large-number-argument :initarg :argument))
      (:report (lambda (condition stream)
                  (format stream "~S is not a large number." 
                          (not-a-large-number-argument condition)))))
    
    (defun assure-large-number (n)
      (loop (when (and (numberp n) (> n 73)) (return n))
            (cerror "Enter a number~3*~:[~; a bit larger than ~*~D~]."
                    'not-a-large-number
                    :argument n 
                    :ignore (numberp n)
                    :ignore n
                    :allow-other-keys t)
            (format t "~&Type a large number: ")
            (setq n (read))
            (fresh-line)))
    

    (assure-large-number 'a)
    >>  Correctable error in ASSURE-LARGE-NUMBER: A is not a large number.
    >>  Restart options:
    >>   1: Enter a number.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Type a large number: 88
    =>  88
    
    (assure-large-number 37)
    >>  Correctable error in ASSURE-LARGE-NUMBER: A is not a large number.
    >>  Restart options:
    >>   1: Enter a number a bit larger than 37.
    >>   2: Top level.
    >>  Debug> :continue 1
    >>  Type a large number: 259
    =>  259
    ```

* 受此影响(Affected By):

        *break-on-signals*.

        已存在的处理者绑定.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        error, format, handler-bind, *break-on-signals*, simple-type-error

* 注意(Notes):

        如果 datum 是一个状况[condition]类型[type]而不是一个字符串[string], format 指令 ~* 在 continue-format-control 中用来忽略初始化参数列表[initialization argument list]中的关键字[keyword]可能特别有用. 比如:

        (cerror "enter a new value to replace ~*~s" 
                'not-a-number
                :argument a)



### <span id="M-CHECK-TYPE">宏 CHECK-TYPE</span>

* 语法(Syntax):

        check-type place typespec [string] => nil

* 参数和值(Arguments and Values):

        place---一个位置[place].
        typespec---一个类型指定符[type specifier].
        string---一个字符串[string]; 求值的.

* 描述(Description):

        如果这个位置 place 的内容不是类型 typespec, 那么 check-type 发出一个类型[type] type-error 的可校正[correctable]错误[error].

        当且仅当 store-value 重启动[restart]被调用, 不管是显式地从一个处理者还是隐式地作为调试器提供的其中一个选项, check-type 可以返回. 如果这个 store-value 重启动[restart]被调用, check-type 存储给这个重启动[restart]调用的参数(或者是通过调试器交互式提示的那个)作为新值到位置 place 并重新开始, 检测这个新值的类型并且如果它仍然不是要求的类型[type]就会发出另一个错误.

        第一次位置 place 被求值时, 它通过正常的求值规则来求值. 如果类型检测失败并且使用了这个 store-value 重启动[restart]那么它接下来被求值为一个位置[place]; 见章节 5.1.1.1 (位置的子表达式形式求值).

        字符串[string]应该是这个类型的一个英语描述, 以一个不定冠词开始 ("a" 或者 "an"). 如果没有提供字符串[string], 它就会自动通过 typespec 来计算. 这个自动生成的信息提及了位置 place, 它的内容和要求的类型. 如果一个具体实现把这个位置 place 识别为一个特定形式, 例如给名为 check-type 的函数的其中一个参数, 这个具体实现可能选择去生成一个措词有点不同的错误信息. 字符串 string 是允许的, 因为 check-type 的一些应用可能需要一个比从 typespec 自动生成的更具体的关于需要什么的描述.

* 示例(Examples):

    ```LISP
    (setq aardvarks '(sam harry fred))
    =>  (SAM HARRY FRED)
    (check-type aardvarks (array * (3)))
    >>  Error: The value of AARDVARKS, (SAM HARRY FRED),
    >>         is not a 3-long array.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> :CONTINUE 1
    >>  Use Value: #(SAM FRED HARRY)
    =>  NIL
    aardvarks
    =>  #<ARRAY-T-3 13571>
    (map 'list #'identity aardvarks)
    =>  (SAM FRED HARRY)
    (setq aardvark-count 'foo)
    =>  FOO
    (check-type aardvark-count (integer 0 *) "A positive integer")
    >>  Error: The value of AARDVARK-COUNT, FOO, is not a positive integer.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Top level.
    >>  Debug> :CONTINUE 2

    (defmacro define-adder (name amount)
      (check-type name (and symbol (not null)) "a name for an adder function")
      (check-type amount integer)
      `(defun ,name (x) (+ x ,amount)))
      
    (macroexpand '(define-adder add3 3))
    =>  (defun add3 (x) (+ x 3))
    
    (macroexpand '(define-adder 7 7))
    >>  Error: The value of NAME, 7, is not a name for an adder function.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Top level.
    >>  Debug> :Continue 1
    >>  Specify a value to use instead.
    >>  Type a form to be evaluated and used instead: 'ADD7
    =>  (defun add7 (x) (+ x 7))
    
    (macroexpand '(define-adder add5 something))
    >>  Error: The value of AMOUNT, SOMETHING, is not an integer.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Top level.
    >>  Debug> :Continue 1
    >>  Type a form to be evaluated and used instead: 5
    =>  (defun add5 (x) (+ x 5))
    ```

        控制被转移到一个处理者.

* 副作用(Side Effects):

        可能进入到调试器中.

* 受此影响(Affected By):

        *break-on-signals*

        这个具体实现.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        章节 9.1 (状况系统的概念)

* 注意(Notes):

    ```LISP
    (check-type place typespec)
    ==  (assert (typep place 'typespec) (place)
                'type-error :datum place :expected-type 'typespec)
    ```

### <span id="CT-SIMPLE-ERROR">状况类型 SIMPLE-ERROR</span>

* 类优先级列表(Class Precedence List):

        simple-error, simple-condition, error, serious-condition, condition, t

* 描述(Description):

        当提供了一个格式化控制字符串[format control]作为 error 或 cerror 的第一个参数时, 类型 simple-error 由通过 error 或 cerror 发出的状况组成. 


### <span id="F-INVALID-METHOD-ERROR">函数 INVALID-METHOD-ERROR</span>

* 语法(Syntax):

        invalid-method-error method format-control &rest args => implementation-dependent

* 参数和值(Arguments and Values):

        method---一个方法[method].
        format-control---一个格式化控制字符串[format control].
        args---格式化控制字符串 format-control 的格式化参数[format argument].

* 描述(Description):

        当这里有一个可应用方法[method]的方法组合类型的限定符[qualifier]不合法时, 使用函数[function] invalid-method-error 来发出一个 error 类型[type]的错误. 错误信息通过使用适用于 format 的格式化控制字符串 format-control 以及给它的任何参数 arg 组成. 由于一个具体实现可能需要给错误信息添加额外的上下文信息, invalid-method-error 应该只在一个方法组合函数的动态范围中被调用.

        当一个方法[method]不满足一个 define-method-combination 表达式形式[form]中的每个限定符[qualifier]模式和断言, 那么函数[function] invalid-method-error 被自动调用. 如果一个强加额外限制的方法组合函数遇到一个它不能接受的方法[method], 那么这个方法组合函数应该显式调用 invalid-method-error.

        invalid-method-error 是返回到它的调用者还是通过 throw 退出是依赖于具体实现的[implementation-dependent].

* 示例(Examples): None.

* 副作用(Side Effects):

        可能会进入调试器中.

* 受此影响(Affected By):

        *break-on-signals*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-method-combination

* 注意(Notes): None. 


### <span id="F-METHOD-COMBINATION-ERROR">函数 METHOD-COMBINATION-ERROR</span>

* 语法(Syntax):

        method-combination-error format-control &rest args => implementation-dependent

* 参数和值(Arguments and Values):

        format-control---一个格式化控制字符串[format control].
        args---格式化控制字符串 format-control 的格式化参数[format argument].

* 描述(Description):

        函数[function] method-combination-error 被用于在方法组合中发出一个错误.

        错误信息通过使用适用于 format 的格式化控制字符串 format-control 以及给它的任何参数 arg 组成. 由于一个具体实现可能需要给错误信息添加额外的上下文信息, method-combination-error 应该只在一个方法组合函数的动态范围中被调用.

        method-combination-error 是返回到它的调用者还是通过 throw 退出是依赖于具体实现的[implementation-dependent].

* 示例(Examples): None.

* 副作用(Side Effects):

        可能进入到调试器中.

* 受此影响(Affected By):

        *break-on-signals*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-method-combination

* 注意(Notes): None. 


### <span id="F-SIGNAL">函数 SIGNAL</span>

* 语法(Syntax):

        signal datum &rest arguments => nil

* 参数和值(Arguments and Values):

        datum, arguments---一个默认类型 simple-condition 的状况[condition]的标识符[designator].

* 描述(Description):

        发出[signal]这个给定的 datum 和 arguments 表示的状况[condition]. 如果这个状况[condition]没有被处理, signal 就返回 nil.

* 示例(Examples):

    ```LISP
    (defun handle-division-conditions (condition)
      (format t "Considering condition for division condition handling~%")
      (when (and (typep condition 'arithmetic-error)
                  (eq '/ (arithmetic-error-operation condition)))
        (invoke-debugger condition)))
    HANDLE-DIVISION-CONDITIONS
    (defun handle-other-arithmetic-errors (condition)
      (format t "Considering condition for arithmetic condition handling~%")
      (when (typep condition 'arithmetic-error)
        (abort)))
    HANDLE-OTHER-ARITHMETIC-ERRORS
    (define-condition a-condition-with-no-handler (condition) ())
    A-CONDITION-WITH-NO-HANDLER
    (signal 'a-condition-with-no-handler)
    NIL
    (handler-bind ((condition #'handle-division-conditions)
                      (condition #'handle-other-arithmetic-errors))
      (signal 'a-condition-with-no-handler))
    Considering condition for division condition handling
    Considering condition for arithmetic condition handling
    NIL
    (handler-bind ((arithmetic-error #'handle-division-conditions)
                      (arithmetic-error #'handle-other-arithmetic-errors))
      (signal 'arithmetic-error :operation '* :operands '(1.2 b)))
    Considering condition for division condition handling
    Considering condition for arithmetic condition handling
    Back to Lisp Toplevel
    ```

* 副作用(Side Effects):

        可能由于 *break-on-signals* 进入到调试器中.

        要被发送的状况的处理者可能转移控制.

* 受此影响(Affected By):

        已存在的处理者绑定.

        *break-on-signals*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        *break-on-signals*, error, simple-condition, 章节 9.1.4 (发送和处理状况)

* 注意(Notes):

        如果 (typep datum *break-on-signals*) 产生[yield] true, 那么在这个发送过程开始前进入调试器. 这个 continue 重启动[restart]可以被用于继续这个发送过程. 对于所有应该, 可能, 或者必须发送[signal]状况[condition]的其他函数[function]和宏[macro], 这个也是对的. 


### <span id="CT-SIMPLE-CONDITION">状况类型 SIMPLE-CONDITION</span>

* 类优先级列表(Class Precedence List):

        simple-condition, condition, t

* 描述(Description):

        每当一个格式化控制字符串 format-control 作为第一个参数提供给 signal 函数时, 类型[type] simple-condition 表示由这个 signal 发出的状况[condition]. 这个格式化控制[format control]和格式化参数[format argument]使用提供给 make-condition 的名为 :format-control 和 :format-arguments 的初始化参数来初始化, 并且可以通过函数[function] simple-condition-format-control 和 simple-condition-format-arguments 来访问. 如果格式化参数没有提供给 make-condition, 那么 nil 就被用作默认值.

* 也见(See Also):

        simple-condition-format-control, simple-condition-format-arguments 


### <span id="F-SCFC-SCFA">函数 SIMPLE-CONDITION-FORMAT-CONTROL, SIMPLE-CONDITION-FORMAT-ARGUMENTS</span>

* 语法(Syntax):

        simple-condition-format-control condition => format-control

        simple-condition-format-arguments condition => format-arguments

* 参数和值(Arguments and Values):

        condition---一个 simple-condition 类型[type]的状况.
        format-control---一个格式化控制字符串[format control].
        format-arguments---一个列表[list].

* 描述(Description):

        simple-condition-format-control 返回处理这个状况 condition 的格式化参数[format argument]所需要的格式化控制字符串[format control].

        simple-condition-format-arguments 返回一个处理这个状况 condition 的格式化控制字符串[format control]所需要的格式化参数[format argument]的列表[list].

* 示例(Examples):

    ```LISP
    (setq foo (make-condition 'simple-condition
                              :format-control "Hi ~S"
                              :format-arguments '(ho)))
    =>  #<SIMPLE-CONDITION 26223553>
    (apply #'format nil (simple-condition-format-control foo)
                        (simple-condition-format-arguments foo))
    =>  "Hi HO"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        simple-condition, 章节 9.1 (状况系统的概念)

* 注意(Notes): None. 


### <span id="F-WARN">函数 WARN</span>

* 语法(Syntax):

        warn datum &rest arguments => nil

* 参数和值(Arguments and Values):

        datum, arguments---一个默认类型 simple-warning 的状况[condition]的标识符[designator].

* 描述(Description):

        发送[signal]一个类型[type] warning 的状况[condition]. 如果这个状况[condition]没有被处理, 报告这个状况[condition]到错误输出[error output].

        警告的精确机制如下:

        这个 warning 状况被发出

            当这个 warning 状况[condition]要被发送时, 这个 muffle-warning 重启动[restart]会为了被一个处理者[handler]使用而被建立. 如果被调用了, 这个重启动[restart]绕开 warn 的进一步动作, 这个反过来导致了 warn 立即返回 nil.

        如果没有找到这个 warning 状况的处理者

            如果没有找到这个 warning 状况的处理者, 或者这些处理者都拒绝了, 那么这个状况[condition]会通过 warn 以一种依赖于具体实现[implementation-dependent]的格式被报告到错误输出[error output].

        返回了 nil

            如果 warn 返回的是 nil 那么这个值就被 warn 返回.

* 示例(Examples):

    ```LISP
      (defun foo (x)
        (let ((result (* x 2)))
          (if (not (typep result 'fixnum))
              (warn "You're using very big numbers."))
          result))
    =>  FOO
    
      (foo 3)
    =>  6
    
      (foo most-positive-fixnum)
    >>  Warning: You're using very big numbers.
    =>  4294967294
    
      (setq *break-on-signals* t)
    =>  T
    
      (foo most-positive-fixnum)
    >>  Break: Caveat emptor.
    >>  To continue, type :CONTINUE followed by an option number.
    >>   1: Return from Break.
    >>   2: Abort to Lisp Toplevel.
    >>  Debug> :continue 1
    >>  Warning: You're using very big numbers.
    =>  4294967294
    ```

* 副作用(Side Effects):

        发出一个警告. 可能进入到调试器中.

* 受此影响(Affected By):

        已存在的处理者绑定.

        *break-on-signals*, *error-output*.

* 异常情况(Exceptional Situations):

        如果 datum 是一个状况[condition]并且这个状况[condition]不是类型[type] warning, 或者参数 arguments 不是 nil [non-nil], 会发出一个类型[type] type-error 的错误.

        如果 datum 是一个状况类型, (apply #'make-condition datum arguments) 的结果必须是类型[type] warning, 否则发出一个类型[type] type-error 的错误.

* 也见(See Also):

        *break-on-signals*, muffle-warning, signal

* 注意(Notes): None. 


### <span id="CT-SIMPLE-WARNING">状况类型 SIMPLE-WARNING</span>

* 类优先级列表(Class Precedence List):

        simple-warning, simple-condition, warning, condition, t

* 描述(Description):

        每当一个格式化控制字符串[format control]作为第一个参数提供给 warn 时, 类型[type] simple-warning 表示 warn 发送的状况[condition]. 


### <span id="F-INVOKE-DEBUGGER">函数 INVOKE-DEBUGGER</span>

* 语法(Syntax):

        invoke-debugger condition =>|

* 参数和值(Arguments and Values):

        condition---一个状况[condition]对象[object].

* 描述(Description):

        invoke-debugger 尝试带状况 condition 进入这个调试器.

        如果 *debugger-hook* 不是 nil, 它应该是一个在进入标准调试器之前要被调用的函数[function] (或者一个函数[name]的名字). 随着 *debugger-hook* 绑定为 nil 这个函数[function]被调用, 并且这个函数[function]必须接受两个参数: 这个状况 condition 和这个 *debugger-hook* 被绑定为 nil 之前的值. 如果这个函数[function]正常返回, 就进入标准调试器.

        标准调试器从不直接返回. 只有通过一个例如使用重启动函数的非局部转移才可能返回.

* 示例(Examples):

    ```LISP
    (ignore-errors ;Normally, this would suppress debugger entry
      (handler-bind ((error #'invoke-debugger)) ;But this forces debugger entry
        (error "Foo.")))
    Debug: Foo.
    To continue, type :CONTINUE followed by an option number:
    1: Return to Lisp Toplevel.
    Debug>
    ```

* 副作用(Side Effects):

        *debugger-hook* 被绑定为 nil, 程序执行不会继续, 然后就进入到调试器中.

* 受此影响(Affected By):

        *debug-io* 和 *debugger-hook*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        error, break

* 注意(Notes): None. 


### <span id="F-BREAK">函数 BREAK</span>

* 语法(Syntax):

        break &optional format-control &rest format-arguments => nil

* 参数和值(Arguments and Values):

        format-control---一个格式化控制字符串[format control]. 默认值是依赖于具体实现的[implementation-dependent].
        format-arguments---这个格式化控制字符串 format-control 的格式化参数[format argument].

* 描述(Description):

        break 格式化[format] format-control 和 format-arguments, 然后在没有任何被程控的错误处理工具拦截可能的情况下直接进入调试器中.

        如果在这个调试器中使用了这个 continue 重启动[restart], break 在没有采取任何不寻常的恢复动作的情况下立即返回 nil.

        break 在尝试进入调试器之前把 *debugger-hook* 绑定为 nil.

* 示例(Examples):

    ```LISP
    (break "You got here with arguments: ~:S." '(FOO 37 A))
    >>  BREAK: You got here with these arguments: FOO, 37, A.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return from BREAK.
    >>   2: Top level.
    >>  Debug> :CONTINUE 1
    >>  Return from BREAK.
    =>  NIL
    ```

* 副作用(Side Effects):

        进入到调试器中.

* 受此影响(Affected By):

        *debug-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        error, invoke-debugger.

* 注意(Notes):

        break 被用作在一个程序中临时插入调试"断点(breakpoints)"的方式, 而不是作为发送错误的方式. 由于这个原因, break 不接受这个 cerror 接受的 continue-format-control 实参[argument]. 这个以及通过状况处理的拦截可能性的缺失是 break 和 cerror 之间仅有的程序可见的区别.

        break 和 cerror 的用户接口方面允许更广泛的变化, 来适应这个具体实现[implementation]的接口需求. 比如, 对于一个 Lisp read-eval-print 循环[Lisp read-eval-print loop]允许通过 break 而不是常规的调试器来进入.

        break 可以通过下面这样定义:

    ```LISP
    (defun break (&optional (format-control "Break") &rest format-arguments)
      (with-simple-restart (continue "Return from BREAK.")
        (let ((*debugger-hook* nil))
          (invoke-debugger
              (make-condition 'simple-condition
                              :format-control format-control
                              :format-arguments format-arguments))))
      nil)
    ```

### <span id="V-DEBUGGER-HOOK">变量 *DEBUGGER-HOOK*</span>

* 值类型(Value Type):

        一个两实参[argument]函数[function] (一个状况[condition]和进入调试器之前 *debugger-hook* 的值[value]) 的标识符[designator], 或者 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        当这个 *debugger-hook* 的值[value]非 nil [non-nil], 它在正常进入调试器前被调用, 不管是由于调用 invoke-debugger 或是从一个带有未处理状况的 error 或 cerror 调用中进入调试器. 这个函数[function]可能处理这个状况[condition] (转移控制) 或者正常返回 (允许标准这个标准调试器来运行). 为了最小化调试期间的递归错误, *debugger-hook* 在调用这个函数[function]前被 invoke-debugger 被绑定为 nil.

* 示例(Examples):

    ```LISP
    (defun one-of (choices &optional (prompt "Choice"))
      (let ((n (length choices)) (i))
        (do ((c choices (cdr c)) (i 1 (+ i 1)))
            ((null c))
          (format t "~&[~D] ~A~%" i (car c)))
        (do () ((typep i `(integer 1 ,n)))
          (format t "~&~A: " prompt)
          (setq i (read))
          (fresh-line))
        (nth (- i 1) choices)))

    (defun my-debugger (condition me-or-my-encapsulation)
      (format t "~&Fooey: ~A" condition)
      (let ((restart (one-of (compute-restarts))))
        (if (not restart) (error "My debugger got an error."))
        (let ((*debugger-hook* me-or-my-encapsulation))
          (invoke-restart-interactively restart))))
    
    (let ((*debugger-hook* #'my-debugger))
      (+ 3 'a))
    >>  Fooey: The argument to +, A, is not a number.
    >>   [1] Supply a replacement for A.
    >>   [2] Return to Cloe Toplevel.
    >>  Choice: 1
    >>   Form to evaluate and use: (+ 5 'b)
    >>   Fooey: The argument to +, B, is not a number.
    >>   [1] Supply a replacement for B.
    >>   [2] Supply a replacement for A.
    >>   [3] Return to Cloe Toplevel.
    >>  Choice: 1
    >>   Form to evaluate and use: 1
    =>  9
    ```

* 受此影响(Affected By):

        invoke-debugger

* 也见(See Also): None.

* 注意(Notes):

        在求值用户交互式输入的代码时, 有时候, 让钩子函数绑定 *debugger-hook* 到它的第二个参数的函数[function]是很有用的, 这样就可以使用相同的交互式工具来处理递归错误了. 


### <span id="V-BREAK-ON-SIGNALS">变量 *BREAK-ON-SIGNALS*</span>
<!--TODO 待校验-->
* 值类型(Value Type):

        一个类型指定符[type specifier].

* 初始值(Initial Value):

        nil.

* 描述(Description):

        当 (typep condition *break-on-signals*) 返回 true 时, 对 signal, 以及其他像 error 这样隐式调用 signal 的操作符[operator]的调用, 在发送这个状况[condition]前进入到调试器中.

        当由于 *break-on-signals* 发生一个中断时, 这个 continue 重启动[condition]可以被用于继续这个正常的发送过程.

* 示例(Examples):

    ```LISP
    *break-on-signals* =>  NIL
    (ignore-errors (error 'simple-error :format-control "Fooey!"))
    =>  NIL, #<SIMPLE-ERROR 32207172>

    (let ((*break-on-signals* 'error))
      (ignore-errors (error 'simple-error :format-control "Fooey!")))
    >>  Break: Fooey!
    >>  BREAK entered because of *BREAK-ON-SIGNALS*.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Continue to signal.
    >>   2: Top level.
    >>  Debug> :CONTINUE 1
    >>  Continue to signal.
    =>  NIL, #<SIMPLE-ERROR 32212257>

    (let ((*break-on-signals* 'error))
      (error 'simple-error :format-control "Fooey!"))
    >>  Break: Fooey!
    >>  BREAK entered because of *BREAK-ON-SIGNALS*.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Continue to signal.
    >>   2: Top level.
    >>  Debug> :CONTINUE 1
    >>  Continue to signal.
    >>  Error: Fooey!
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Top level.
    >>  Debug> :CONTINUE 1
    >>  Top level.
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        break, signal, warn, error, typep, 章节 9.1 (状况系统的概念)

* 注意(Notes):

        *break-on-signals* 主要用于调试执行发送的代码. 当设置了 *break-on-signals* 时, 鼓励用户去选择满足的最限制的说明. 设置 *break-on-signals* 实际上违反了状况[condition]发送的模块化处理. 事实上, 设置 *break-on-signals* 的完整影响在某些方面可能是不可预测的, 因为用户可能没有意识到在附带调用的代码中使用的对 signal 调用的种类和数量.

        *break-on-signals* 允许早期进入调试器, 但是这样的一个进入不会阻止例如 error 和 cerror 操作下额外进入调试器. 


### <span id="M-HANDLER-BIND">宏 HANDLER-BIND</span>

* 语法(Syntax):

        handler-bind ({binding}*) form* => result*

        binding::= (type handler) 

* 参数和值(Arguments and Values):

        type---一个类型指定符[type specifier].
        handler---一个表达式形式[form]; 求值来产生一个处理者函数 handler-function.
        handler-function---一个单实参[argument]函数[function]的标识符[designator].
        forms---一个隐式的 progn [implicit progn].
        results---由这些表达式形式[forms]返回的那些值[value].

* 描述(Description):

        在一个表示的处理者 handler 绑定[binding]生效的动态环境[dynamic environment]中执行那些表达式形式 forms.

        每个处理者 handler 应该被求值为一个处理者函数 handler-function, 它被用于在执行这些表达式形式 forms 期间处理给定类型 type 的状况[condition]. 这个函数[function]应该接收一个单独的参数, 就是要被发送的这个状况[condition].

        如果提供了超过一个处理者 handler 绑定[binding], 从上到下依次搜索这些处理者 handler 绑定[binding]来寻找匹配项 (通过使用 typecase 来做视觉类比). 如果找到一个合适的类型[type], 关联的处理者在一个这些处理者绑定都是可见的动态环境[dynamic environment]中运行 (来避免递归错误). 如果这个处理者[handler]拒绝[decline]了, 就继续搜索另一个处理者[handler].

        如果没有找到合适的处理者[handler], 那么在动态闭合边界中寻找其他处理者[handler]. 如果在外边没有找到处理者[handler], 那么 signal 返回或者 error 进入到调试器中.

* 示例(Examples):

        在以下代码中, 如果在这个主体中被发送一个未绑定的变量错误 (并且没有被一个介入的处理者所处理), 第一个函数会被调用.

    ```LISP
    (handler-bind ((unbound-variable #'(lambda ...))
                    (error #'(lambda ...)))
      ...)
    ```

        如果任何其他种类的错误被发送, 第二个函数会被调用. 不论发生何种情况, 在执行相关函数的代码期间这些处理者都不是活跃的.

    ```LISP
    (defun trap-error-handler (condition)
      (format *error-output* "~&~A~&" condition)
      (throw 'trap-errors nil))

    (defmacro trap-errors (&rest forms)
      `(catch 'trap-errors
          (handler-bind ((error #'trap-error-handler))
            ,@forms)))
    
    (list (trap-errors (signal "Foo.") 1)
          (trap-errors (error  "Bar.") 2)
          (+ 1 2))
    >>  Bar.
    =>  (1 NIL 3)
    ```

        注意, 这个 "Foo." 不会被打印, 因为这个 signal 发送的状况是一个简单转开[simple-condition], 它不是类型[type] error, 所以它不会触发由 trap-errors 设置的 error 的处理者.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        handler-case

* 注意(Notes): None. 


### <span id="M-HANDLER-CASE">宏 HANDLER-CASE</span>

* 语法(Syntax):

        handler-case expression [[{error-clause}* | no-error-clause]] => result*

        clause::= error-clause | no-error-clause 

        error-clause::= (typespec ([var]) declaration* form*) 

        no-error-clause::= (:no-error lambda-list declaration* form*) 

* 参数和值(Arguments and Values):

        expression---一个表达式形式.
        typespec---一个类型特化符.
        var---一个变量名字.
        lambda-list---一个普通 lambda 列表.
        declaration---一个 declare 表达式; 不求值的.
        form---一个表达式形式.
        results---在正常的情况下, 返回的值是那些表达式 expression 求值的结果; 在这个当控制被转移到一个子句 clause 中的异常情况中, 在那个 clause 中的最后一个表达式形式 form 的值会被返回.

* 描述(Description):

        handler-case 在一个各种处理者都活跃的动态环境中执行表达式 expression. 每个 error-clause 指定如果去处理一个匹配那个类型特化符 typespec 的状况. 如果控制正常返回, 那么一个 no-error-clause 允许一个特定动作的规范.<!--TODO 待校验-->

        如果在表达式 expression 求值期间一个有着合适的 error-clause 的状况被发送 (换句话说, (typep condition 'typespec) 返回 true) 并且如果这里没有这个类型状况的介入处理者, 那么控制被装一到这个相关 error-clause 的主体当中. 在这个情况下, 这个动态的状态被解开 (这样这些在表达式 expression 周围建立的处理者不再是活跃的), 并且 var 被绑定为这个要被发送的状况. 如果提供了不止一个情况(case), 这些情况是平行的. 这也就是说, 在下面这个表达式形式中

          (handler-case form
            (typespec1 (var1) form1)
            (typespec2 (var2) form2))

        如果第一个子句 (包括 form1) 已经被选择了, 第二个的处理者不再是可见的 (反之亦然).

        这些子句依次从上倒下被搜索. 如果这里有一个类型在 typespecs 之间重叠, 选择这些子句中更早的那个.

        如果不需要 var, 它可以被省略. 这也就是说, 一个像这样的子句:

          (typespec (var) (declare (ignore var)) form)

        可以被写为 (typespec () form).

        如果在一个选择的子句 clause 中没有表达式形式, 那么这个情况, 那么因此 handler-case, 返回 nil. 如果表达式 expression 的执行正常返回并且不存在 no-error-clause, 表达式 expression 返回的值会被 handler-case 返回. 如果表达式 expression 的执行正常返回并且存在一个 no-error-clause, 返回的值被用作给从 no-error-clause 通过构造 (lambda lambda-list form*) 所描述函数的参数, 并且这个函数调用的值会被 handler-case 返回. 在这个调用时, 在表达式 expression 周围建立的处理者不再是活跃的.<!--TODO 待校验-->

* 示例(Examples):

    ```LISP
    (defun assess-condition (condition)
      (handler-case (signal condition)
        (warning () "Lots of smoke, but no fire.")
        ((or arithmetic-error control-error cell-error stream-error)
            (condition)
          (format nil "~S looks especially bad." condition))
        (serious-condition (condition)
          (format nil "~S looks serious." condition))
        (condition () "Hardly worth mentioning.")))
    =>  ASSESS-CONDITION
    (assess-condition (make-condition 'stream-error :stream *terminal-io*))
    =>  "#<STREAM-ERROR 12352256> looks especially bad."
    (define-condition random-condition (condition) () 
      (:report (lambda (condition stream)
                  (declare (ignore condition))
                  (princ "Yow" stream))))
    =>  RANDOM-CONDITION
    (assess-condition (make-condition 'random-condition))
    =>  "Hardly worth mentioning."
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        handler-bind, ignore-errors, Section 9.1 (Condition System Concepts)

* 注意(Notes):

    ```LISP
    (handler-case form
      (type1 (var1) . body1)
      (type2 (var2) . body2) ...)
    ```

        大约等价于:

    ```LISP
    (block #1=#:g0001
      (let ((#2=#:g0002 nil))
        (tagbody
          (handler-bind ((type1 #'(lambda (temp)
                                          (setq #1# temp)
                                          (go #3=#:g0003)))
                          (type2 #'(lambda (temp)
                                          (setq #2# temp)
                                          (go #4=#:g0004))) ...)
          (return-from #1# form))
            #3# (return-from #1# (let ((var1 #2#)) . body1))
            #4# (return-from #1# (let ((var2 #2#)) . body2)) ...)))
    ```

    ```LISP
    (handler-case form
      (type1 (var1) . body1)
      ...
      (:no-error (varN-1 varN-2 ...) . bodyN))
    ```

        大约等价于:

    ```LISP
    (block #1=#:error-return
      (multiple-value-call #'(lambda (varN-1 varN-2 ...) . bodyN)
        (block #2=#:normal-return
          (return-from #1#
            (handler-case (return-from #2# form)
              (type1 (var1) . body1) ...)))))
    ```

### <span id="M-IGNORE-ERRORS">宏 IGNORE-ERRORS</span>

* 语法(Syntax):

        ignore-errors form* => result*

* 参数和值(Arguments and Values):

        forms---一个隐式的 progn.
        results---在正常情况下, 这些表达式形式 forms 的值被返回; 在异常情况中, 返回两个值: nil 和这个状况.

* 描述(Description):

        ignore-errors 被用于阻止 error 类型的错误去导致进入调试器.

        具体的说, ignore-errors 在一个 error 类型的状况的处理者被建立的动态环境中执行这些表达式形式 forms; 如果被调用了, 它通过从 ignore-errors 表达式形式中返回两个值, nil 和这个被发送的状况, 来处理这样的状况.

        如果发生了一个从这些表达式形式 forms 正常返回, 那么返回的任何值都被 ignore-errors 返回.

* 示例(Examples):

    ```LISP
    (defun load-init-file (program)
      (let ((win nil))
        (ignore-errors ;if this fails, don't enter debugger
          (load (merge-pathnames (make-pathname :name program :type :lisp)
                                  (user-homedir-pathname)))
          (setq win t))
        (unless win (format t "~&Init file failed to load.~%"))
        win))
    
    (load-init-file "no-such-program")
    >>  Init file failed to load.
    NIL
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        handler-case, Section 9.1 (Condition System Concepts)

* 注意(Notes):

        (ignore-errors . forms)

        等价于:

        (handler-case (progn . forms)
          (error (condition) (values nil condition)))

        由于第二个返回值是在异常情况中的一个状况, 通常(但不是必须)在正常情况下第二个返回值被安排为缺失的或者 nil, 这样这两个情况就可以被区分开来. 


### <span id="M-DEFINE-CONDITION">宏 DEFINE-CONDITION</span>

* 语法(Syntax):

        define-condition name (parent-type*) ({slot-spec}*) option*
        => name

        slot-spec::= slot-name | (slot-name slot-option) 

        slot-option::= [[{:reader symbol}* |  
                      {:writer function-name}* |  
                      {:accessor symbol}* |  
                      {:allocation allocation-type} |  
                      {:initarg symbol}* |  
                      {:initform form} |  
                      {:type type-specifier} ]] 

        option::= [[(:default-initargs . initarg-list) |  
                  (:documentation string) |  
                  (:report report-name) ]] 

        function-name::= {symbol | (setf symbol)} 

        allocation-type::= :instance | :class 

        report-name::= string | symbol | lambda expression 

* 参数和值(Arguments and Values):

        name---一个符号.
        parent-type---一个命名一个状况类型的符号. 如果没有提供 parent-types, 这个 parent-types 默认是 (condition).
        default-initargs---一个键值对列表.
        Slot-spec -- 一个槽的名字或者一个由槽名字 slot-name 后面跟着零个或多个槽选项 slot-options 构成的列表.
        Slot-name -- 一个槽名字 (一个符号), 一个槽名字的列表, 或者槽名/槽表达式形式对的列表.

        Option -- 任意下面这些选项:

        :reader

            :reader 对于一个给定的槽可以被提供超过一次但是不能是 nil.

        :writer

            :writer 对于一个给定的槽可以被提供超过一次并且必须命名一个广义函数.

        :accessor

            :accessor 对于一个给定的槽可以被提供超过一次但是不能是 nil.

        :allocation

            :allocation 对于一个给定的槽可以被提供最多一次. 如果 :allocation 没有被提供那么默认值就是 :instance.

        :initarg

            :initarg 对于一个给定的槽可以被提供超过一次.

        :initform

            :initform 对于一个给定的槽可以被提供最多一次.

        :type

            :type 对于一个给定的槽可以被提供最多一次.

        :documentation

            :documentation 对于一个给定的槽可以被提供最多一次.

        :report

            :report 可以被提供最多一次.

* 描述(Description):

        define-condition 定义一个名为 name 的新的状况类型, 它是那个类型的一个子类型或者是名为 parent-type 的子类型. 每个 parent-type 参数为这个新的状况指定一个直接的超类. 这个新的状况从它的每个超类中继承槽和方法, 诸如此类.

        如果提供了一个 槽的名字/槽表达式形式 对, 这个槽表达式形式是一个在没有显式提供值时被 make-condition 求值来提供一个默认值的表达式形式. 如果没有提供槽表达式形式, 这个槽的内容以一种依赖于具体实现的方式来初始化.

        如果要被定义的类型和某个它继承的其他类型有着相同名字的槽, 只会在这个状况中分配一个槽, 但是这个提供的槽表达式形式重写任何从一个 parent-type 中继承而来的槽表达式形式. 如果没有提供槽表达式形式, 继承的槽表达式形式(如果存在的话)始终是可见的.

        访问器根据和 defclass 使用的相同的规则被创建.

        槽选项 slot-options 的一个描述如下:

        :reader

            这个 :reader 槽选项指定了一个要被定义在由给 :reader 的参数命名的这个广义函数上的非限定方法来入去这个给定槽的名字.

        :initform

            这个 :initform 槽选项被用来提供一个在这个槽的初始化中使用的默认初始值表达式形式. 这个表达式形式在每次被用来初始化这个槽的时候被求值. 这个表达式形式求值所在的词法环境是这个 define-condition 表达式形式被求值所在的词法环境. 注意, 这个词法环境同时引用了变量和函数. 对于局部槽, 动态环境是那个 make-condition 被调用时所在的动态环境; 对于共享槽, 动态环境是那个 define-condition 表达式形式被求值时所在的动态环境.

            没有具体实现被允许去扩展 define-condition 的语法来允许 (slot-name form) 作为 (slot-name :initform form) 的一个简写.

        :initarg

            这个 :initarg 槽选项声明了一个由它的符号参数命名的初始化参数并且指定了这个初始化参数初始化给定的槽. 如果这个初始化参数在对 initialize-instance 的调用中有一个值, 那么这个值就被存储在给定槽中, 并且这个槽的 :initform 槽选项如果存在的话, 就不会被求值. 如果为一个给定槽指定的初始化参数都没有值, 这个槽就根据这个 :initform 槽选项来初始化, 如果指定的话.

        :type

            这个 :type 槽选项指定了这个槽的内容总是为给定的类型. 它有效的声明了应用到这个状况类型的对象上的读取器广义函数的结果类型. 尝试去存储一个不满足这个槽的类型的值到这个槽中的后果是未定义的.

        :default-initargs

            这个选项被和 defclass 中一样的方式来对待.

        :documentation

            这个 :documentation 槽选项为这个槽提供了一个文档字符串.

        :report

            状况报告是通过成问题的那个状况类型的 print-object 方法来调节的, 此时 *print-escape* 总是为 nil. 在一个状况类型 C 中指定 (:report report-name) 等价于:

            (defmethod print-object ((x c) stream)
              (if *print-escape* (call-next-method) (report-name x stream)))

            如果通过给 :report (report-name) 的参数提供的值是一个符号或者一个 lambda 表达式, 它对于 function 必须是可接受的. (function report-name) 在当前词法环境中被求值. 它应该返回一个两个参数的函数, 两个参数是一个状况和一个流, 这个函数把这个状况的一个描述打印到这个流中. 每当这个状况被打印并且 *print-escape* 是 nil, 这个函数就会被调用.

            如果 report-name 是一个字符串, 它是下面这个的一个简写

            (lambda (condition stream)
              (declare (ignore condition))
              (write-string report-name stream))

            这个选项在这个新的状况类型已经被创建后被处理, 所以在这个 :report 函数中使用槽访问器是允许的. 如果没有提供这个选项, 关于如何去报告这个类型的状况的信息从 parent-type 继承而来.

        如果尝试去读取一个没有被显式初始化并且没有给定一个默认值的槽, 那么结果是未定义的.

        如果尝试去使用 setf 对这个槽赋值, 那么结果也是未定义的.

        如果一个 define-condition 表达式形式作为一个顶层表达式形式出现, 编译器必须使 name 成为一个可识别的有效类型名字, 并且它必须可以被要被编译的这个文件后面的其他状况类型的 define-condition 表达式形式作为 parent-type 来引用这个状况类型.

* 示例(Examples):

        下面表达式形式定义了类型 peg/hole-mismatch 的状况, 它继承自名为 blocks-world-error 的状况:

    ```LISP
    (define-condition peg/hole-mismatch 
                      (blocks-world-error)
                      ((peg-shape  :initarg :peg-shape
                                  :reader peg/hole-mismatch-peg-shape)
                      (hole-shape :initarg :hole-shape
                                  :reader peg/hole-mismatch-hole-shape))
      (:report (lambda (condition stream)
                (format stream "A ~A peg cannot go in a ~A hole."
                        (peg/hole-mismatch-peg-shape  condition)
                        (peg/hole-mismatch-hole-shape condition)))))
    ```

        这个新类型有着槽 peg-shape 和 hole-shape, 因此 make-condition 接受 :peg-shape 和 :hole-shape 关键字. 读取器 peg/hole-mismatch-peg-shape 和 peg/hole-mismatch-hole-shape 应用于这个类型的对象, 就像在那个 :report 信息中阐述的那样.

        下面这个表达式形式定义了名为 machine-error 的状况类型, 它继承自 error:

    ```LISP
    (define-condition machine-error 
                      (error)
                      ((machine-name :initarg :machine-name
                                    :reader machine-error-machine-name))
      (:report (lambda (condition stream)
                (format stream "There is a problem with ~A."
                        (machine-error-machine-name condition)))))
    ```

        这个定义的基础上, 一个新的错误状况可以被定义, 它是 machine-error 的一个子类型, 在机器不可用时被使用:

    ```LISP
    (define-condition machine-not-available-error (machine-error) ()
      (:report (lambda (condition stream)
                (format stream "The machine ~A is not available."
                        (machine-error-machine-name condition)))))
    ```

        这个定义了一个更具体的状况, 在 machine-not-available-error 的基础上, 它为 machine-name 提供了一个槽初始化表达式形式但是它没有提供任何新的槽或者报告消息. 它只是给 machine-name 槽一个默认初始化:

    ```LISP
    (define-condition my-favorite-machine-not-available-error
                      (machine-not-available-error)
      ((machine-name :initform "mc.lcs.mit.edu")))
    ```

        注意, 由于没有给定 :report 子句, 从 machine-not-available-error 继承来的信息被用来报告这个状况的类型.

    ```LISP
    (define-condition ate-too-much (error) 
        ((person :initarg :person :reader ate-too-much-person)
          (weight :initarg :weight :reader ate-too-much-weight)
          (kind-of-food :initarg :kind-of-food
                        :reader :ate-too-much-kind-of-food)))
    =>  ATE-TOO-MUCH
    (define-condition ate-too-much-ice-cream (ate-too-much)
      ((kind-of-food :initform 'ice-cream)
        (flavor       :initarg :flavor
                      :reader ate-too-much-ice-cream-flavor
                      :initform 'vanilla ))
      (:report (lambda (condition stream)
                  (format stream "~A ate too much ~A ice-cream"
                          (ate-too-much-person condition)
                          (ate-too-much-ice-cream-flavor condition)))))
    =>  ATE-TOO-MUCH-ICE-CREAM
    (make-condition 'ate-too-much-ice-cream
                    :person 'fred
                    :weight 300
                    :flavor 'chocolate)
    =>  #<ATE-TOO-MUCH-ICE-CREAM 32236101>
    (format t "~A" *)
    >>  FRED ate too much CHOCOLATE ice-cream
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        make-condition, defclass, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


### <span id="F-MAKE-CONDITION">函数 MAKE-CONDITION</span>

* 语法(Syntax):

        make-condition type &rest slot-initializations => condition

* 参数和值(Arguments and Values):

        type---一个类型特化符 (对于一个 condition  的子类型).
        slot-initializations---一个初始化参数列表.
        condition---一个状况.

* 描述(Description):

        使用槽初始值的 slot-initializations 来构造并返回一个类型 type 类型的状况. 新创建的状况会被返回.

* 示例(Examples):

    ```LISP
    (defvar *oops-count* 0)

    (setq a (make-condition 'simple-error
                            :format-control "This is your ~:R error."
                            :format-arguments (list (incf *oops-count*))))
    =>  #<SIMPLE-ERROR 32245104>
    
    (format t "~&~A~%" a)
    >>  This is your first error.
    =>  NIL
    
    (error a)
    >>  Error: This is your first error.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return to Lisp Toplevel.
    >>  Debug> 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

The set of defined condition types.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

define-condition, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


### <span id="SC-RESTART">System Class RESTART</span>

* 类优先级列表(Class Precedence List):

        restart, t

* 描述(Description):

        一个类型 restart 的对象表示一个可以被调用来执行一些恢复动作的表达式形式的函数, 通常是到运行的程序中的一个更外部的点的控制转移.

        一个具体实现可以自由地以任何最方便的方式实现重启动; 一个重启动只有和建立它的绑定表达式作用域相关的动态范围. 


### <span id="F-COMPUTE-RESTARTS">函数 COMPUTE-RESTARTS</span>

* 语法(Syntax):

        compute-restarts &optional condition => restarts

* 参数和值(Arguments and Values):

        condition---一个状况对象, 或者 nil.
        restarts---一个重启动列表.

* 描述(Description):

        compute-restarts 使用这个程序的动态状态来计算一个当前是活跃的重启动列表.

        产生的列表是有序的, 因此最内部(较新建立的)的重启动较接近这个列表的头部.

        当状况 condition 不是 nil, 只有那些显式和那个状况 condition 关联的或者没有和任何状况关联的重启动会被考虑; 这也就是说, 排除在外的重启动是那些和一个包含给定状况 condition 的非空状况集合相关联的重启动. 如果状况 condition 是 nil, 所有重启动会被考虑.

        compute-restarts 返回所有可应用的重启动, 包括匿名的那些, 当给定一个符号参数时, 即便它们中的一部分有着和其他的相同的名字并且因此不会被 find-restart 找到.

        在相同的动态环境中时, 具体实现被允许, 但没有被要求从对 compute-restarts 的重复调用中去返回不同的列表. 如果 compute-restarts 返回的列表每次被修改那么结果是未定义的.

* 示例(Examples):

    ```LISP
    ;; One possible way in which an interactive debugger might present
    ;; restarts to the user.
    (defun invoke-a-restart ()
      (let ((restarts (compute-restarts)))
        (do ((i 0 (+ i 1)) (r restarts (cdr r))) ((null r))
          (format t "~&~D: ~A~%" i (car r)))
        (let ((n nil) (k (length restarts)))
          (loop (when (and (typep n 'integer) (>= n 0) (< n k))
                  (return t))
                (format t "~&Option: ")
                (setq n (read))
                (fresh-line))
          (invoke-restart-interactively (nth n restarts)))))

    (restart-case (invoke-a-restart)
      (one () 1)
      (two () 2)
      (nil () :report "Who knows?" 'anonymous)
      (one () 'I)
      (two () 'II))
    >>  0: ONE
    >>  1: TWO
    >>  2: Who knows?
    >>  3: ONE
    >>  4: TWO
    >>  5: Return to Lisp Toplevel.
    >>  Option: 4
    =>  II
    
    ;; Note that in addition to user-defined restart points, COMPUTE-RESTARTS
    ;; also returns information about any system-supplied restarts, such as
    ;; the "Return to Lisp Toplevel" restart offered above.
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        已存在的重启动.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        find-restart, invoke-restart, restart-bind

* 注意(Notes): None. 


### <span id="F-FIND-RESTART">函数 FIND-RESTART</span>

* 语法(Syntax):

        find-restart identifier &optional condition

        restart

* 参数和值(Arguments and Values):

        identifier---一个非 nil 符号, 或者是一个重启动.
        condition---一个状况对象, 或者 nil.
        restart---一个重启动或者 nil.

* 描述(Description):

        find-restart 在当前动态环境中搜索一个特定的重启动.

        当状况 condition 不是 nil 时, 只有那些和那个状况 condition 显式关联或者没有和任何状况关联的重启动会被考虑; 这也就是说, 排除在外的重启动是那些和一个包含给定状况 condition 的非空状况集合相关联的重启动. 如果这个状况 condition 是 nil, 所有重启动都会被考虑.

        如果 identifier 是一个符号, 那么最内部 (most recently established) applicable 带有那个名字的重启动会被返回. 如果没有找到这样的重启动就返回 nil.

        如果 identifier 是一个当前活跃的重启动, 那么它就被返回. 否则, 返回 nil.

* 示例(Examples):

    ```LISP
    (restart-case
        (let ((r (find-restart 'my-restart)))
          (format t "~S is named ~S" r (restart-name r)))
      (my-restart () nil))
    >>  #<RESTART 32307325> is named MY-RESTART
    =>  NIL
    (find-restart 'my-restart)
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        已存在的重启动.

        restart-case, restart-bind, with-condition-restarts.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compute-restarts

* 注意(Notes):

        (find-restart identifier)
        ==  (find identifier (compute-restarts) :key :restart-name)

        虽然匿名重启动由一个 nil 的名字, 如果 nil 被给定用作一个 identifier 那么结果是未指定的. 偶尔, 程序员叹息 nil 不允许用作一个 identifier 参数. 在大部分这样的情况下, compute-restarts 可能被用来模拟预期的效果. 


### <span id="F-INVOKE-RESTART">函数 INVOKE-RESTART</span>

* 语法(Syntax):

        invoke-restart restart &rest arguments => result*

* 参数和值(Arguments and Values):

        restart---一个重启动标识符.
        argument---一个对象.
        results---和重启动 restart 关联的函数返回的值, 如果那个函数返回的话.

* 描述(Description):

        调用和重启动 restart 关联的函数, 传递参数 arguments 给它. 重启动 restart 在当前动态环境中必须是有效的.

* 示例(Examples):

    ```LISP
    (defun add3 (x) (check-type x number) (+ x 3))
    
    (foo 'seven)
    >>  Error: The value SEVEN was not of type NUMBER.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a different value to use.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> (invoke-restart 'store-value 7)
    =>  10
    ```

* 副作用(Side Effects):

        一个非局部控制转移可能被这个重启动 restart 完成.

* 受此影响(Affected By):

        已存在的重启动.

* 异常情况(Exceptional Situations):

        如果重启动 restart 是无效的, 一个类型 control-error 的错误会被发出.

* 也见(See Also):

        find-restart, restart-bind, restart-case, invoke-restart-interactively

* 注意(Notes):

        invoke-restart 的最常见的使用是在一个处理者中. 它可能被显式使用, 或者隐式地通过 invoke-restart-interactively 或一个重启动函数来使用.

        重启动函数调用 invoke-restart, 但反之则不行. 这也就是说, invoke-restart 提供基本功能, 并且重启动函数是非必须的"语法糖".


### <span id="F-INVOKE-RESTART-INTERACTIVELY">函数 INVOKE-RESTART-INTERACTIVELY</span>

* 语法(Syntax):

        invoke-restart-interactively restart => result*

* 参数和值(Arguments and Values):

        restart---一个重启动标识符.
        results---和重启动 restart 关联的函数返回的值, 如果这个函数返回的话.

* 描述(Description):

        invoke-restart-interactively 调用和重启动 restart 关联的函数, 提示任何必要的参数. 如果重启动 restart 是一个名字, 它必须在当前动态环境中是有效的.

        invoke-restart-interactively 通过执行作为提供给 restart-case 的 :interactive 关键字或者给 restart-bind 的 :interactive-function 关键字中的代码来提示参数.

        如果没有在对应 restart-bind 或 restart-case 中提供对应选项, 如果这个重启动 restart 接收必要参数那么结果是未定义的. 如果参数是可选的, 一个 nil 的参数列表会被使用.

        一旦这些参数已经被确定了, invoke-restart-interactively 执行以下代码:

        (apply #'invoke-restart restart arguments)

* 示例(Examples):

    ```LISP
    (defun add3 (x) (check-type x number) (+ x 3))
    
    (add3 'seven)
    >>  Error: The value SEVEN was not of type NUMBER.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a different value to use.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> (invoke-restart-interactively 'store-value)
    >>  Type a form to evaluate and use: 7
    =>  10
    ```

* 副作用(Side Effects):

        如果参数提示是必要的, 可能发生某个打印输出 (在查询 I/O 上).

        一个非局部控制转移可能被这个重启动完成.

* 受此影响(Affected By):

        *query-io*, 活跃的重启动

* 异常情况(Exceptional Situations):

        如果重启动 restart 是无效的, 一个类型 control-error 的错误会被发出.

* 也见(See Also):

        find-restart, invoke-restart, restart-case, restart-bind

* 注意(Notes):

        invoke-restart-interactively 被调试器内部使用并且在实现其他可移植的交互式调试工具时可能也是有用的. 


### <span id="M-RESTART-BIND">宏 RESTART-BIND</span>

* 语法(Syntax):

        restart-bind ({(name function {key-val-pair}*)}) form*
        => result*

        key-val-pair::= :interactive-function interactive-function |  
                        :report-function report-function |  
                        :test-function test-function 

* 参数和值(Arguments and Values):

        name---一个符号; 不求值的.
        function---一个表达式形式; 求值的.
        forms---一个隐式 progn.
        interactive-function---一个表达式形式; evaluated.
        report-function---一个表达式形式; evaluated.
        test-function---一个表达式形式; evaluated.
        results---这些表达式形式 forms 返回的值.

* 描述(Description):

        restart-bind 在动态环境中执行表达式形式 forms 的主体, 在这个环境中给定名字 names 的重启动是生效的.

        如果一个名字 name 是 nil, 它表示一个匿名的重启动; 如果一个名字 name 是一个非 nil 符号, 它表示一个已命名的重启动.

        这个函数 function, 交互式函数 interactive-function, 以及报告函数 report-function 在当前的词法和动态环境中在主体被求值前被无条件求值. 这些表达式形式的每一个都必须求值为一个函数.

        如果 invoke-restart 在这个重启动上被完成了, 那么由求值函数 function 产生的函数会被用传递给 invoke-restart 的参数作为参数来调用, 在那个 invoke-restart 的动态环境中.

        如果这个重启动从调试器中被交互式地调用(使用 invoke-restart-interactively), 这些参数被调用求值 interactive-function 所产生的函数所缺省. 那个函数可以在查询 I/O 上选择性地提示, 然后在调用这个重启动时应该返回一个要被 invoke-restart-interactively 使用的参数列表.

        如果一个重启动被交互式调用但是没有交互式函数 interactive-function 被使用, 那么一个 nil 的参数列表会被使用. 在这个情况下, 函数必须和一个空参数列表兼容.

        如果这个重启动交互式地出现 (比如, 通过这个调试器), 展示是通过调用由求值 report-function 产生的函数来完成的. 这个函数必须是一个单参数的函数, 这个参数为一个流. 它被期望打印重启动采取的操作的描述到那个流中. 当 *print-escape* 是 nil 时, 这个函数每当这个重启动被打印的时候被调用.

        在交互式调用的情况下, 结果依赖于下面这样的 :interactive-function 的值.

        :interactive-function

            值 value 在当前词法环境中被求值并且应该返回一个没有参数的函数, 它构造一个在调用这个重启动时要被 invoke-restart-interactively 使用的参数列表. 如果有必要这个函数可能使用查询 I/O 来交互式地提示.

        :report-function

            值 value 在当前词法环境中被求值并且应该返回一个单参数的函数, 这个参数是一个流, 这个函数打印这个重启动采取的动作的总结到这个流中. 这个函数每当这个重启动被报告的时候就会被调用 (当 *print-escape* 是 nil 时会被打印). 如果没有提供 :report-function 选项, 这个重启动被报告的方式是依赖于具体实现的.

        :test-function

            值 value 在当前词法环境中被求值并且返回一个单参数的函数, 这个参数是一个状况, 如果这个重启动被认为是可见的, 那么这个函数返回 true.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *query-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        restart-case, with-simple-restart

* 注意(Notes):

        restart-bind 主要被用于实现 restart-case, 在实现其他宏的时候可能也是有用的. 程序员对使用 restart-case 还是 restart-bind 不确定的话, 应该更倾向 restart-case 因为它足够强大, 只有在普遍性是非常必要的情况下使用 restart-bind. 


### <span id="M-RESTART-CASE">宏 RESTART-CASE</span>

* 语法(Syntax):

        restart-case restartable-form {clause} => result*

        clause::= (case-name lambda-list  
                  [[:interactive interactive-expression | :report report-expression | :test test-expression]]  
                  declaration* form*) 

* 参数和值(Arguments and Values):

        restartable-form---一个表达式形式.
        case-name---一个符号或 nil.
        lambda-list---一个普通的 lambda 列表.
        interactive-expression---一个符号或一个 lambda 表达式.
        report-expression---一个字符串, 一个符号, 或一个 lambda 表达式.
        test-expression---一个符号或 lambda 表达式.
        declaration---一个 declare 表达式; 不求值的.
        form---一个表达式形式.
        results---这个 restartable-form 求值所产生的值, 或者在一个选定的子句 clause 中最后一个表达式形式返回的值, 或者是 nil.

* 描述(Description):

        restart-case 在一个动态环境中求值 restartable-form, 这个环境中这些子句有着特殊的意义, 可以作为控制转移的点. 如果 restartable-form 完成执行并且返回任何值, 那么所有返回值都会被 restart-case 返回并且这个过程结束. 当 restartable-form 被执行时, 任何代码可以转移控制到这些子句的其中一个 (见 invoke-restart). 如果发生了一个转移, 那个子句的主体中的表达式形式会被求值并且最后一个这样的表达式形式返回的值会被 restart-case 返回. 在这个情况下, 在执行该子句之前, 动态状态被适当地解除(这样一来这些在 restartable-form 周围建立的重启动不再是活跃的).

        如果在选择的子句中没有表达式形式 forms, restart-case 就返回 nil.

        如果 case-name 是一个, 它就命名这个重启动.

        可能有超过一个子句使用相同的 case-name. 在这个情况下, 带有那个名字的第一个子句会被 find-restart 发现. 其他的子句使用 compute-restarts 也是可访问的.

        每个 arglist 是一个在它对应表达式形式执行期间要被绑定的普通 lambda 列表. 这些参数被 restart-case 子句用来从一个对 invoke-restart 的调用中接收任何必要的数据.

        默认情况下, invoke-restart-interactively 不传递参数并且所有参数必须是可选的, 为了适应交互式的重启动. 然而, 如果这个 :interactive 关键字已经被用来告知 invoke-restart-interactively 关于如果计算一个适当的参数列表, 那么这些参数不需要是可选的.

        关键字 keyword 有着以下这些选项.

        :interactive

            通过 :interactive 提供的值 value 必须是一个给 function 的合适的参数. (function value) 在当前词法环境中被求值. 它应该返回一个没有参数的函数, 这个函数被调用时返回要被 invoke-restart-interactively 使用的参数. invoke-restart-interactively 在任何重启动尝试之前在这个可用的动态环境中被调用, 并且为用户交互使用查询 I/O.

            如果一个重启动被交互式调用但是没有提供 :interactive 选项, 那么在这个调用中使用的参数列表是空列表.

        :report

            如果通过这个 :report 提供的值 value 是一个 lambda 表达式或者一个符号, 它对于 function 必须是可接受的. (function value) 在当前词法环境中被求值. 它应该返回一个单参数的函数, 这个参数是一个流, 这个函数应该在这个流上打印这个重启动的一个描述. 当 *print-escape* 是 nil 时, 无论何时这个重启动被打印, 这个函数都会被调用.

            如果值 value 是一个字符串, 它就是下面这个的一个缩写

            (lambda (stream) (write-string value stream))

            如果一个已命名的重启动被请求来报告但是没有提供报告消息, 这个重启动的名字被用来产生默认的报告文本.

            当 *print-escape* 是 nil 时, 打印器就使用一个重启动的报告消息. 例如, 一个调试器可能宣布输入一个"continue"命令的动作通过下面这个:

            (format t "~&~S -- ~A~%" ':continue some-restart)

            它可能显示像这样的内容:

            :CONTINUE -- Return to command level

            如果一个未命名的重启动被指定但是没有提供 :report 选项, 那么结果是未指定的.

        :test

            通过 :test 提供的值 value 必须是一个给 function 的合适的值. (function value) 在当前词法环境中被求值. 它应该返回一个单参数的函数, 这个参数是这个状况, 如果这个重启动要被认为是可见的, 这个函数就返回 true.

            这个选项的默认值等价于 (lambda (c) (declare (ignore c)) t).

        如果这个 restartable-form 是一个是一个列表, 其中 car 部分是 signal, error, cerror, or warn 其中一个符号 (或者是一个宏展开为这样一个列表的宏表达式形式), 那么 with-condition-restarts 会被隐式地用来关联这个表示的重启动和这个要被发送的状况.

* 示例(Examples):

    ```LISP
    (restart-case
        (handler-bind ((error #'(lambda (c)
                                (declare (ignore condition))
                                (invoke-restart 'my-restart 7))))
          (error "Foo."))
      (my-restart (&optional v) v))
    =>  7

    (define-condition food-error (error) ())
    =>  FOOD-ERROR
    (define-condition bad-tasting-sundae (food-error) 
      ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
        (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
        (topping :initarg :topping :reader bad-tasting-sundae-topping))
      (:report (lambda (condition stream)
                  (format stream "Bad tasting sundae with ~S, ~S, and ~S"
                          (bad-tasting-sundae-ice-cream condition)
                          (bad-tasting-sundae-sauce condition)
                          (bad-tasting-sundae-topping condition)))))
    =>  BAD-TASTING-SUNDAE
    (defun all-start-with-same-letter (symbol1 symbol2 symbol3)
      (let ((first-letter (char (symbol-name symbol1) 0)))
        (and (eql first-letter (char (symbol-name symbol2) 0))
              (eql first-letter (char (symbol-name symbol3) 0)))))
    =>  ALL-START-WITH-SAME-LETTER
    (defun read-new-value ()
      (format t "Enter a new value: ")
      (multiple-value-list (eval (read))))
    =>  READ-NEW-VALUE
    (defun verify-or-fix-perfect-sundae (ice-cream sauce topping)
      (do ()
          ((all-start-with-same-letter ice-cream sauce topping))
        (restart-case
          (error 'bad-tasting-sundae
                  :ice-cream ice-cream
                  :sauce sauce
                  :topping topping)
          (use-new-ice-cream (new-ice-cream)
            :report "Use a new ice cream."
            :interactive read-new-value  
            (setq ice-cream new-ice-cream))
          (use-new-sauce (new-sauce)
            :report "Use a new sauce."
            :interactive read-new-value
            (setq sauce new-sauce))
          (use-new-topping (new-topping)
            :report "Use a new topping."
            :interactive read-new-value
            (setq topping new-topping))))
      (values ice-cream sauce topping))
    =>  VERIFY-OR-FIX-PERFECT-SUNDAE
    (verify-or-fix-perfect-sundae 'vanilla 'caramel 'cherry)
    >>  Error: Bad tasting sundae with VANILLA, CARAMEL, and CHERRY.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Use a new ice cream.
    >>   2: Use a new sauce.
    >>   3: Use a new topping.
    >>   4: Return to Lisp Toplevel.
    >>  Debug> :continue 1
    >>  Use a new ice cream.
    >>  Enter a new ice cream: 'chocolate
    =>  CHOCOLATE, CARAMEL, CHERRY
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        restart-bind, with-simple-restart.

* 注意(Notes):

    ```LISP
    (restart-case expression
        (name1 arglist1 ...options1... . body1)
        (name2 arglist2 ...options2... . body2))
    ```

        大约等价于

    ```LISP
    (block #1=#:g0001
      (let ((#2=#:g0002 nil))
            (tagbody
            (restart-bind ((name1 #'(lambda (&rest temp)
                                    (setq #2# temp)
                                    (go #3=#:g0003))
                              ...slightly-transformed-options1...)
                          (name2 #'(lambda (&rest temp)
                                    (setq #2# temp)
                                    (go #4=#:g0004))
                              ...slightly-transformed-options2...))
            (return-from #1# expression))
              #3# (return-from #1#
                      (apply #'(lambda arglist1 . body1) #2#))
              #4# (return-from #1#
                      (apply #'(lambda arglist2 . body2) #2#)))))
    ```

        未命名的重启动通常只有在交互式的情况下有用, 并且一个没有描述的交互式选项没有什么价值. 如果使用了一个未命名的重启动并且在编译时没有提供报告消息的话, 鼓励具体实现去发出警告. 在运行时, 在进入调试器时可能会注意到这个错误. 由于发出一个错误可能会导致递归进入调试器中 (导致另一个递归错误, 等等) , 因此建议调试器在出现这些问题时打印出一些指示, 但实际上并不发出这些错误.

    ```LISP
    (restart-case (signal fred)
      (a ...)
      (b ...))
    == 
    (restart-case
        (with-condition-restarts fred 
                                  (list (find-restart 'a) 
                                        (find-restart 'b))
          (signal fred))
      (a ...)
      (b ...))
    ```

### <span id="F-RESTART-NAME">函数 RESTART-NAME</span>

* 语法(Syntax):

        restart-name restart => name

* 参数和值(Arguments and Values):

        restart---一个重启动.
        name---一个符号.

* 描述(Description):

        返回这个重启动 restart 的名字, 如果这个重启动 restart 没有被命名就返回 nil.

* 示例(Examples):

    ```LISP
    (restart-case 
        (loop for restart in (compute-restarts)
                  collect (restart-name restart))
      (case1 () :report "Return 1." 1)
      (nil   () :report "Return 2." 2)
      (case3 () :report "Return 3." 3)
      (case1 () :report "Return 4." 4))
    =>  (CASE1 NIL CASE3 CASE1 ABORT)
    ;; In the example above the restart named ABORT was not created
    ;; explicitly, but was implicitly supplied by the system.
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compute-restarts find-restart

* 注意(Notes): None. 


### <span id="M-WITH-CONDITION-RESTARTS">宏 WITH-CONDITION-RESTARTS</span>

* 语法(Syntax):

        with-condition-restarts condition-form restarts-form form*
        => result*

* 参数和值(Arguments and Values):

        condition-form---一个表达式形式; 求值来产生一个状况 condition.
        condition---从 condition-form 的求值中产生的一个状况对象.
        restart-form---一个表达式形式; 求值来产生一个重启动列表 restart-list.
        restart-list---从重启动表达式形式 restart-form 的求值中产生的一个重启动对象的列表.
        forms---一个隐式 progn; 求值的.
        results---表达式形式 forms 返回的值.

* 描述(Description):

        首先, 这个 condition-form 和 restarts-form 以正常从左到右的顺序求值; 这些求值产生的主要的值分别调用这个 condition 和 restart-list.

        然后, 表达式形式 forms 在一个动态环境中被求值, 在这个环境中每个在 restart-list 中的重启动都和这个状况 condition 关联. 见章节 9.1.4.2.4 (关联重启动和状况).

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        restart-case

* 注意(Notes):

        通常这个宏不会在代码中被显式调用, 因为 restart-case 以一种语法上更简洁的方式处理大多数常见的情况. 


### <span id="M-WITH-SIMPLE-RESTART">宏 WITH-SIMPLE-RESTART</span>

* 语法(Syntax):

        with-simple-restart (name format-control format-argument*) form*
        => result*

* 参数和值(Arguments and Values):

        name---一个符号.
        format-control---一个格式化控制.
        format-argument---一个对象 (换句话说, 一个格式化参数).
        forms---一个隐式 progn.
        results---在正常情况中, 就是表达式形式 forms 返回的值; 在名为 name 的重启动被调用的异常情况中, 就是两个值---nil 和 t.

* 描述(Description):

        with-simple-restart 建立一个重启动.

        如果这个 name 表示的重启动没有在执行表达式形式 forms 期间被调用, 最后一个表达式形式返回的所有值都会被返回. 如果 name 表示的重启动被调用了, 那么控制会转移到 with-simple-restart, 它返回两个值, nil 和 t.

        如果 name 是 nil, 就会建立一个匿名的重启动.

        这个 format-control 和 format-arguments 被用于报告这个重启动.

* 示例(Examples):

    ```LISP
    (defun read-eval-print-loop (level)
      (with-simple-restart (abort "Exit command level ~D." level)
        (loop
          (with-simple-restart (abort "Return to command level ~D." level)
            (let ((form (prog2 (fresh-line) (read) (fresh-line))))
              (prin1 (eval form)))))))
    =>  READ-EVAL-PRINT-LOOP
    (read-eval-print-loop 1)
    (+ 'a 3)
    >>  Error: The argument, A, to the function + was of the wrong type.
    >>         The function expected a number.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use this time.
    >>   2: Return to command level 1.
    >>   3: Exit command level 1.
    >>   4: Return to Lisp Toplevel.

    (defun compute-fixnum-power-of-2 (x)
      (with-simple-restart (nil "Give up on computing 2^~D." x)
        (let ((result 1))
          (dotimes (i x result)
            (setq result (* 2 result))
            (unless (fixnump result)
              (error "Power of 2 is too large."))))))
    COMPUTE-FIXNUM-POWER-OF-2
    (defun compute-power-of-2 (x)
      (or (compute-fixnum-power-of-2 x) 'something big))
    COMPUTE-POWER-OF-2
    (compute-power-of-2 10)
    1024
    (compute-power-of-2 10000)
    >>  Error: Power of 2 is too large.
    >>  To continue, type :CONTINUE followed by an option number.
    >>   1: Give up on computing 2^10000.
    >>   2: Return to Lisp Toplevel
    >>  Debug> :continue 1
    =>  SOMETHING-BIG
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        restart-case

* 注意(Notes):

        with-simple-restart 是 restart-case 最常见使用中的一个简写.

        with-simple-restart 可以被定义为:

    ```LISP
    (defmacro with-simple-restart ((restart-name format-control
                                                  &rest format-arguments)
                                    &body forms)
      `(restart-case (progn ,@forms)
          (,restart-name ()
              :report (lambda (stream)
                        (format stream ,format-control ,@format-arguments))
            (values nil t))))
    ```

        由于在异常的情况下第二个返回值是 t, 通常 (但不是必须) 安排在正常情况下第二个返回值为缺失的或者是 nil, 这样两种情况就可以被区分. 


### <span id="R-ABORT">重启动 ABORT</span>

* 必要数据参数(Data Arguments Required):

        None.

* 描述(Description):

        这个 abort 重启动的意图是允许返回到最里边的"命令层级(command level)". 鼓励实现者去确保在用户代码周围总是有一个名为 abort 的重启动, 这样用户代码可以在任何时间调用 abort 并且期待发生一些合理的事情; 合理的东西可能会有所不同. 典型地, 在一个交互式的监听器中, abort 的调用返回到 Lisp read-eval-print 循环阶段的 Lisp 读取器, 尽管在某个批处理或多进程情况中, 可能会有这样的情况: 让它杀死运行的过程更合适.

* 也见(See Also):

        Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, abort (function) 


### <span id="R-CONTINUE">重启动 CONTINUE</span>

* 必要数据参数(Data Arguments Required):

        None.

* 描述(Description):
<!--TODO 待校验-->
        这个 continue 重启动通常是有一个单个的"明显"的方式来继续的协议的一部分, 比如在 break 和 cerror 中. 一些用户定义的协议也可能希望将其合并为类似的原因. 一般而言, 然而, 更可靠的方法是设计一个特殊用途的重启动, 它的名称更适合于特定的应用程序.

* 示例(Examples):

    ```LISP
    (let ((x 3))
      (handler-bind ((error #'(lambda (c)
                                (let ((r (find-restart 'continue c)))
                                  (when r (invoke-restart r))))))
        (cond ((not (floatp x))
                (cerror "Try floating it." "~D is not a float." x)
                (float x))
              (t x)))) =>  3.0
    ```

* 也见(See Also):

        Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, continue (function), assert, cerror 


### <span id="R-MUFFLE-WARNING">重启动 MUFFLE-WARNING</span>

* 必要数据参数(Data Arguments Required):

        没有.

* 描述(Description):

        这个重启动[restart]由 warn 建立, 这样一来 warning 状况[condition]的处理者[handler]就有一种途径去通知 warn 一个警告已经被处理并且不需要采取进一步的行动.

* 示例(Examples):

    ```LISP
    (defvar *all-quiet* nil) =>  *ALL-QUIET*
    (defvar *saved-warnings* '()) =>  *SAVED-WARNINGS*
    (defun quiet-warning-handler (c)
      (when *all-quiet*
        (let ((r (find-restart 'muffle-warning c)))
          (when r 
            (push c *saved-warnings*)
            (invoke-restart r)))))
    =>  CUSTOM-WARNING-HANDLER
    (defmacro with-quiet-warnings (&body forms)
      `(let ((*all-quiet* t)
              (*saved-warnings* '()))
          (handler-bind ((warning #'quiet-warning-handler))
            ,@forms
            *saved-warnings*)))
    =>  WITH-QUIET-WARNINGS
    (setq saved
      (with-quiet-warnings
        (warn "Situation #1.")
        (let ((*all-quiet* nil))
          (warn "Situation #2."))
        (warn "Situation #3.")))
    >>  Warning: Situation #2.
    =>  (#<SIMPLE-WARNING 42744421> #<SIMPLE-WARNING 42744365>)
    (dolist (s saved) (format t "~&~A~%" s))
    >>  Situation #3.
    >>  Situation #1.
    =>  NIL
    ```

* 也见(See Also):

        章节 9.1.4.2 (重启动), 章节 9.1.4.2.2 (重启动的接口), invoke-restart, muffle-warning (函数[function]), warn 


### <span id="R-STORE-VALUE">重启动 STORE-VALUE</span>

* 必要数据参数(Data Arguments Required):

        使用的代替值 (在持续的基础上).

* 描述(Description):

        这个 store-value 重启动[restart]通常被处理者[handler]用于尝试从例如 cell-error 或 type-error 这样类型[type]的错误中恢复过来, 它可以希望去提供一个替换的数据来持久地存储.

* 示例(Examples):

    ```LISP
    (defun type-error-auto-coerce (c)
      (when (typep c 'type-error)
        (let ((r (find-restart 'store-value c)))
          (handler-case (let ((v (coerce (type-error-datum c)
                                          (type-error-expected-type c))))
                          (invoke-restart r v))
            (error ()))))) =>  TYPE-ERROR-AUTO-COERCE
    (let ((x 3))
      (handler-bind ((type-error #'type-error-auto-coerce))
        (check-type x float)
        x)) =>  3.0
    ```

* 也见(See Also):

        章节 9.1.4.2 (重启动), 章节 9.1.4.2.2 (重启动的接口), invoke-restart, store-value (函数[function]), ccase, check-type, ctypecase, use-value (函数[function]和[restart]) 


### <span id="R-USE-VALUE">重启动 USE-VALUE</span>

* 必要数据参数(Data Arguments Required):

        使用的代替值 (一次).

* 描述(Description):

        这个 use-value 重启动[restart]通常被处理者[handler]用来尝试从例如 cell-error 这样类型[type]的错误中恢复过来, 在这里这个处理者可能希望去提供一个替代的数据用于单次使用.

* 也见(See Also):

        章节 9.1.4.2 (重启动), 章节 9.1.4.2.2 (重启动的接口), invoke-restart, use-value (函数[function]), store-value (函数[function]和重启动[restart]) 


### <span id="F-ABORT-CONTINUE-MW-SV-UV">函数 ABORT, CONTINUE, MUFFLE-WARNING, STORE-VALUE, USE-VALUE</span>

* 语法(Syntax):

        abort &optional condition =>|

        continue &optional condition => nil

        muffle-warning &optional condition =>|

        store-value value &optional condition => nil

        use-value value &optional condition => nil

* 参数和值(Arguments and Values):

        value---一个对象[object].
        condition---一个状况[condition]对象[object], 或者 nil.

* 描述(Description):

        转移控制到最新建立的有着和这个函数相同名字的可应用重启动[applicable restart]. 这也就是说, 函数[function] abort 搜索一个可应用的[applicable] abort 重启动[restart], 函数[function] continue 搜索一个可应用的[applicable] continue 重启动[restart], 以此类推.

        如果不存在这样的重启动[restart], 函数 continue, store-value, 和 use-value 返回 nil, 并且函数 abort 和 muffle-warning 发出一个 control-error 类型[type]的错误.

        当状况 condition 不是 nil [non-nil], 只有那些和那个状况 condition 显式关联的或者没有和任何状况[condition]关联的重启动[restart]会被考虑; 这也就是说, 没有包含的重启动[restart]是那些和一个不包含给定状况 condition 的非空状况[condition]集合关联的重启动[restart]. 如果状况 condition 是 nil, 那么所有的重启动[restart]都会被考虑.

* 示例(Examples):

    ```LISP
    ;;; Example of the ABORT retart

    (defmacro abort-on-error (&body forms)
      `(handler-bind ((error #'abort))
          ,@forms)) =>  ABORT-ON-ERROR
    (abort-on-error (+ 3 5)) =>  8
    (abort-on-error (error "You lose."))
    >>  Returned to Lisp Top Level.

    ;;; Example of the CONTINUE restart

    (defun real-sqrt (n)
      (when (minusp n)
        (setq n (- n))
        (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
      (sqrt n))

    (real-sqrt 4) =>  2
    (real-sqrt -9)
    >>  Error: Tried to take sqrt(-9).
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Return sqrt(9) instead.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> (continue)
    >>  Return sqrt(9) instead.
    =>  3
    
    (handler-bind ((error #'(lambda (c) (continue))))
      (real-sqrt -9)) =>  3

    ;;; Example of the MUFFLE-WARNING restart

    (defun count-down (x)
      (do ((counter x (1- counter)))
          ((= counter 0) 'done)
        (when (= counter 1)
          (warn "Almost done"))
        (format t "~&~D~%" counter)))
    =>  COUNT-DOWN
    (count-down 3)
    >>  3
    >>  2
    >>  Warning: Almost done
    >>  1
    =>  DONE
    (defun ignore-warnings-while-counting (x)
      (handler-bind ((warning #'ignore-warning))
        (count-down x)))
    =>  IGNORE-WARNINGS-WHILE-COUNTING
    (defun ignore-warning (condition)
      (declare (ignore condition))
      (muffle-warning))
    =>  IGNORE-WARNING
    (ignore-warnings-while-counting 3)
    >>  3
    >>  2
    >>  1
    =>  DONE

    ;;; Example of the STORE-VALUE and USE-VALUE restarts

    (defun careful-symbol-value (symbol)
      (check-type symbol symbol)
      (restart-case (if (boundp symbol)
                        (return-from careful-symbol-value 
                                      (symbol-value symbol))
                        (error 'unbound-variable
                                :name symbol))
        (use-value (value)
          :report "Specify a value to use this time."
          value)
        (store-value (value)
          :report "Specify a value to store and use in the future."
          (setf (symbol-value symbol) value))))
    (setq a 1234) =>  1234
    (careful-symbol-value 'a) =>  1234
    (makunbound 'a) =>  A
    (careful-symbol-value 'a)
    >>  Error: A is not bound.
    >>  To continue, type :CONTINUE followed by an option number.
    >>   1: Specify a value to use this time.
    >>   2: Specify a value to store and use in the future.
    >>   3: Return to Lisp Toplevel.
    >>  Debug> (use-value 12)
    =>  12
    (careful-symbol-value 'a)
    >>  Error: A is not bound.
    >>  To continue, type :CONTINUE followed by an option number.
    >>    1: Specify a value to use this time.
    >>    2: Specify a value to store and use in the future.
    >>    3: Return to Lisp Toplevel.
    >>  Debug> (store-value 24)
    =>  24
    (careful-symbol-value 'a)
    =>  24

    ;;; Example of the USE-VALUE restart

    (defun add-symbols-with-default (default &rest symbols)
      (handler-bind ((sys:unbound-symbol
                        #'(lambda (c)
                            (declare (ignore c)) 
                            (use-value default))))
        (apply #'+ (mapcar #'careful-symbol-value symbols))))
    =>  ADD-SYMBOLS-WITH-DEFAULT
    (setq x 1 y 2) =>  2
    (add-symbols-with-default 3 'x 'y 'z) =>  6
    ```

* 副作用(Side Effects):

        如果一个合适的重启动[restart]是可用的, 那么可能发生一个控制转移, 否则 (在函数[functioni] abort 或者函数[function] muffle-warning 的情况下) 执行可能被停止.

* 受此影响(Affected By):

        这些函数中的每一个可以被一个相同名字的重启动[restart]的出现所影响.

* 异常情况(Exceptional Situations):

        如果对于函数[function] abort 一个合适的 abort 重启动[restart]是不可用的, 或者对于函数[function] muffle-warning 一个合适的 muffle-warning 重启动[restart]是不可用的, 那么就会发出一个类型[type] control-error 的错误.

* 也见(See Also):

        invoke-restart, 章节 9.1.4.2 (重启动), 章节 9.1.4.2.2 (重启动的接口), assert, ccase, cerror, check-type, ctypecase, use-value, warn

* 注意(Notes):

    ```LISP
    (abort condition) ==  (invoke-restart 'abort)
    (muffle-warning)  ==  (invoke-restart 'muffle-warning)
    (continue)        ==  (let ((r (find-restart 'continue))) (if r (invoke-restart r)))
    (use-value x) ==  (let ((r (find-restart 'use-value))) (if r (invoke-restart r x)))
    (store-value x) ==  (let ((r (find-restart 'store-value))) (if r (invoke-restart r x)))
    ```

        这个规范中没有定义需要去提供一个 use-value 重启动[restart]的函数. 