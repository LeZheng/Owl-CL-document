# 9. 状况

> * 9.1 [状况系统的概念](#ConditionSystemConcepts)
> * 9.2 [状况字典](#ConditionsDictionary)

## 9.1 <span id="ConditionSystemConcepts">状况系统的概念</span>

描述 Common Lisp 构造不仅仅是它们打算被使用情况下的行为 (见每个操作符声明的 "描述(Description)" 部分), 还有其他所有情况 (见每个操作符说明的 "异常情况(Exceptional Situations)").

一个情况(situation)是一个表达式在一个特定上下文中的求值. 一个状况(condition)是表示一个被检测到的特定情况的一个对象. 状况是类 condition 的一个普通实例. 在 Common Lisp 定义了一个状况类的层次结构. 一个状况有槽, 可以包含这个状况表示的情况的相关数据.

一个错误(error)是一个没有某个表达式形式的干预(不管是由用户交互还是在程序控制下), 程序就无法正常执行下去的情况. 不是所有错误都被检测到. 当一个错误未被检测到, 它的效果可以是取决于具体实现爱呢(implementation-dependent), 具体实现定义(implementation-defined), 未指定(unspecified), 或者未定义(undefined)的. 见章节 1.4 (Definitions). 所有检测到的错误可以被状况表示, 但不是所有状况都表示错误.

发送(Signaling) 是一个过程, 通过这个过程一个状况可以通过提升可以被处理的状况来修改一个程序中的控制流. 函数 error, cerror, signal, 还有 warn 被用于发出状况信号.

这个发送的过程涉及从一组活跃的处理者(handler)中选择和调用一个处理者. 一个处理者(handler)是一个单参数(这个状况)的被调用来处理一个状况的函数. 每个处理者都和一个状况类型关联, 并且一个处理者只有在这个处理者关联的状况类型上被调用.

活跃的处理者被动态地确定 (见 handler-bind 或 handler-case). 处理者在和这个发送者(signaler)等价的动态环境中被调用, 除了这个活跃的处理者集合只包含在这个要被调用的处理者被建立时活跃的那些. 发出一个状况在这个状况上没有副作用, 并且这里没有动态的状态被包含在一个状况中.

如果一个处理者被调用, 它可以通过以下三种方式来处理这个问题:

Decline

    它可以拒绝去处理这个状况. 它通过简单的返回而不是转移控制来完成这个. 当这个发生的时候, 这个处理者返回的任何值都会被忽略并且下一个最近被建立的处理者会被调用. 如果这里没有这样的处理者并且发送函数是 error 或 cerror, 这个调试器会进入到这个发送者的动态环境中. 如果这里没有这样的处理者并且发送函数是 signal 或 warn, 那么这个发送函数简单地返回 nil.

Handle

    它可以通过执行一个控制的非局部转移来处理这个状况. 这个可以通过简单地使用 go, return, throw 来完成, 或者通过使用例如 abort 或 invoke-restart 函数更抽象地完成.

Defer

    它可以推迟一个关于是否处理(handle)或拒绝(decline)的决定, 通过任何一种动作, 但是最常见的是通过发送另一个状况, 重发相同的状况, 或者强制进入调试器.

> * 9.1.1 [状况类型](#ConditionTypes)
> * 9.1.2 [创建状况](#CreatingConditions)
> * 9.1.3 [打印状况](#PrintingConditions)
> * 9.1.4 [发送和处理状况](#SignalingHandlingConditions)
> * 9.1.5 [断言](#Assertions)
> * 9.1.6 [Notes about the Condition System's Background](#NotesConditionSystemBackground)


### 9.1.1 <span id="ConditionTypes">状况类型</span>

下一段中列出了标准状况类型. 额外的状况类型可以通过使用 define-condition 来定义.

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

所有状况类型都是类型 condition 的子类型. 这也就是说,

```LISP
(typep c 'condition) =>  true
```

当且仅当 c 是一个状况.

具体实现必须定义所有指定的子类型关系. 除非特别注解, 本文档中所示的所有子类型关系都不是相互排斥的. 一个状况继承自它的超类型的结构.

类 condition 的元类是未指定的. 状况类型的名字可能被用于指定 define-condition 中的超类型关系, 但是如果去尝试使用一个状况类型作为一个 defclass 表达式形式中的一个超类, 那么结果是未定义的.

下面这段中展示了定义状况类型和创建状况的操作符.

    define-condition  make-condition    

    Figure 9-2. 定义和创建状况的操作符.

下面这段展示了读取状况中槽的值的操作符.

    arithmetic-error-operands   simple-condition-format-arguments  
    arithmetic-error-operation  simple-condition-format-control    
    cell-error-name             stream-error-stream                
    file-error-pathname         type-error-datum                   
    package-error-package       type-error-expected-type           
    print-not-readable-object   unbound-slot-instance              

    Figure 9-3. 读取状况槽的操作符.

#### 9.1.1.1 严重状况

一个严重状况(serious condition)是一个严重到如果没有处理就需要交互式干涉的状况. 严重状况典型地通过 error 或 cerror 发出; 非严重状况通常用 signal 或 warn 发出. 


### 9.1.2 <span id="CreatingConditions">创建状况</span>

函数 make-condition 可以被用于显式构造一个状况对象. 像 error, cerror, signal, 还有 warn 这样的函数在状况上操作并且可能创建状况对象. 像 ccase, ctypecase, ecase, etypecase, check-type, 还有 assert 这样的宏也可能隐式地创建 (以及发送) 状况.

#### 9.1.2.1 状况指定符

状况系统中的许多函数都采用被标识为状况指定符的参数. 按照惯例, 那些参数被记作

    datum &rest arguments

合起来, 这个 datum 和 arguments 是 "一个 default-type 类型状况的指示符". 表示的状况如何被计算取决于这个 datum 的类型:

* 如果这个 datum 是一个命名状况类型的符号 ...

    表示的状况是下面这个的结果

     (apply #'make-condition datum arguments)

* 如果这个 datum 是一个格式化控制 ...

    表示的状况是下面这个的结果

     (make-condition defaulted-type 
                     :format-control datum
                     :format-arguments arguments)

    其中 defaulted-type 是 default-type 的一个子类型.

* 如果这个 datum 是一个状况 ...

    这个表示的状况就是这个 datum 自身. 在这个情况下, 除非这个讨论中的操作符描述中另有说明, 否则参数 arguments 必须为 null; 这也就是说, 如果提供了任何其他参数那么结果是未定义的.

注意, 这个 default-type 只有在 datum 字符串被提供的情况下才被使用. 在其他情况中, 产生的状况不必是类型 default-type.

这里有一些说明, 关于不同的状况指定符如何表示等价的状况对象:

```LISP
(let ((c (make-condition 'arithmetic-error :operator '/ :operands '(7 0))))
  (error c))
==  (error 'arithmetic-error :operator '/ :operands '(7 0))

(error "Bad luck.")
==  (error 'simple-error :format-control "Bad luck." :format-arguments '())
```

### 9.1.3 <span id="PrintingConditions">打印状况</span>

如果使用了给 define-condition 的 :report 参数, 一个打印函数会被定义, 无论何时当 \*print-escape\* 的值为 false 而且被定义的状况要被打印时, 它会被调用. 这个函数被称为状况汇报者(reporter); 它输出的文本被称为一个报告信息(report message).

当一个状况被打印并且 \*print-escape\* 是 false, 这个状况的状况汇报者会被调用. 状况通过像 invoke-debugger, break, 和 warn 这样的函数被自动打印.

当 *print-escape* 是 true, 这个对象应该根据这个具体实现的风格以一种简短的方式打印 (比如, print-unreadable-object). 一个状况没有必要可以通过读取它的打印表示来重新构造.

没有为直接访问或调用状况汇报者提供函数.

#### 9.1.3.1 状况汇报中的推荐风格

为了在向用户呈现汇报消息时确保正确的美观的结果, 推荐一些风格的惯例.

对于通过状况汇报者输出的消息内容, 有一些风格上的建议, 但是在那些程序上没有正式的需求. 如果一个程序违反了某些信息的建议, 这条信息的显示可能没有遵循指导方针的那样美观, 但是这个程序仍然被认为是复合规范的程序.

在一个调用一个状况汇报者的程序或具体实现上的这个要求更为强烈. 一个符合规范的程序必须被允许假定如果遵循这些样式准则, 将保持适当的美观. 在适当的情况下，关于此类程序的任何具体要求都在下面明确提到.

> * 9.1.3.1.1 [在状况汇报中的大写和标点符号](#CPCR)
> * 9.1.3.1.2 [在状况汇报中领导和尾随的新行](#LTNCR)
> * 9.1.3.1.3 [在状况汇报中内嵌的新行](#ENCR)
> * 9.1.3.1.4 [关于在状况汇报中的 tab 的注意事项](#NTCR)
> * 9.1.3.1.5 [在状况汇报中提及包含函数](#MCFCR)

##### 9.1.3.1.1 <span id="CPCR">在状况汇报中的大写和标点符号</span>

一个汇报信息建议为一个完整的句子, 以适当的大小写并且加标点. 在英语中, 比如, 这个意味着第一个字符应该为大写, 并且这里应该有一个尾部的句号.

```LISP
(error "This is a message")  ; Not recommended
(error "this is a message.") ; Not recommended

(error "This is a message.") ; Recommended instead
```

##### 9.1.3.1.2 <span id="LTNCR">在状况汇报中领导和尾随的新行</span>

一个汇报消息不建议以任何引导文本开始, 像 "Error:" 或 "Warning:" 这样或仅为 freshline 或 newline. 如果对于这个上下文合适, 这样的文本通过调用这个状况汇报者的程序来添加.

一个汇报信息不建议跟着一个尾部的 freshline 或 newline. 如果对于这个上下文合适, 这样的文本通过调用这个状况汇报者的程序来添加.

```LISP
(error "This is a message.~%")   ; Not recommended
(error "~&This is a message.")   ; Not recommended
(error "~&This is a message.~%") ; Not recommended

(error "This is a message.")     ; Recommended instead
```

##### 9.1.3.1.3 <span id="ENCR">在状况汇报中内嵌的新行</span>

如果汇报消息尤其的长, 那么它包含一个或多个内嵌的新行是允许的和适当的.

如果调用程序在消息的第一行中插入了一些额外的前缀(像 "Error:" 或 ";; Error:") , 它也必须确保一个合适的前缀会被添加到这个输出的后续每一行中, 这样这个被状况汇报者输出的信息的左边界会始终是正确对齐的.

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

因为汇报消息的缩进可能会以任意数量转移到右边或左边, 应该对这个不完全标准的字符 <Tab> 采取特别关注(在那些具体实现中支持这样的一个字符). 除非这个具体实现在这个上下文中特别定义了它的行为, 应该避免它的使用. 


##### 9.1.3.1.5 <span id="MCFCR">在状况汇报中提及包含函数</span>

这个包含函数的名字通常不应该在汇报信息中被提及. 假定调试器将使这些信息在必要和适当的情况下可访问. 


### 9.1.4 <span id="SignalingHandlingConditions">发送和处理状况</span>

状况系统的操作取决于活跃可应用的处理者的顺序, 从最新近的到最不新近的.

每个处理者和一个类型指定符相关联, 这个指定符必须指定一个类型 condition 的子类型. 如果一个状况是关联的一个类型指定符指定的类型, 那么就说这个处理者对于这个状况是可应用的.

活跃的处理者通过使用 handler-bind (或者一个基于 handler-bind 的简写, 比如 handler-case 或 ignore-errors) 来建立.

活跃的处理者可以在其他活跃处理者的动态作用域中被建立. 在程序执行期间的任何点, 这里都有一组活跃处理者. 当发出一个状况时, 针对这个状况的最新近的活跃可应用的处理者会从这个组中被选择出来. 给定一个状态, 活跃可应用处理者的最新近顺序通过下面两条规则来定义:

1. 如果在活跃处理者集合 H1 中的处理者被建立时活跃处理者集合 H2 中的处理者是活跃的, 那么 H1 中的每个处理者比 H2 中的更新近.

2. 使 h1 和 h2 是相同表达式形式建立的两个可应用的活跃处理者. 如果在这个建立它们的表达式形式中 h1 被定义在 h2 的左边, 那么 h1 比 h2 更新近.

一旦一个处理者绑定表达式形式(例如 handler-bind 或 handler-case)中的一个处理者被选择, 那个表达式形式形式中的所有处理者变成对于这个发送过程的剩余部分非活跃的. 当这个选择的处理者运行时, 那个表达式形式建立的其他处理者没有是活跃的. 这也就是说, 如果这个处理者拒绝处理, 那个表达式形式建立的其他处理者不会被考虑为可能的调用.

下面这一段中展示了和处理状况相关的操作符.

    handler-bind  handler-case  ignore-errors  

    Figure 9-4. 状况处理相关的操作符.

> * 9.1.4.1 [发送](#Signaling)
> * 9.1.4.2 [重启动](#Restarts)

#### 9.1.4.1 <span id="Signaling">发送</span>

当发送一个状况时, 最新近的活跃可应用的处理者会被调用. 有时候一个处理者会通过没有控制转移的简单返回来拒绝. 在这样的情况下, 下一个最新近的活跃可应用的处理者会被调用.

如果对于一个被发送的状况这里没有可应用的处理者, 或者如果所有可应用的处理者都拒绝了, 那么这个状况就是未处理的.

如果对于函数 cerror 和 error 要被发送的状况没有被处理, 不管它们的类型, 那么函数 cerror 和 error 调用这个交互式的状况处理者 (就是这个调试器) 而不是返回. 相比之下, 对于 signal 如果要被发送的状况没有被处理, 不管它们的类型,  那么 signal 就返回 nil.

变量 \*break-on-signals\* 可以被用于在这个发送过程开始前进入调试器.

下面这段展示了和状况的发送相关的定义的名字The next figure shows defined names relating to the signaling of conditions.

    *break-on-signals*  error   warn  
    cerror              signal        

    Figure 9-5. 状况的发送相关的定义的名字.

##### 9.1.4.1.1 重发一个状况

在一个特定的状况对象的发送过程的动态范围期间, 当且仅当两种情况下表示的情形是喜爱嗯童的时允许再次发送相同的状况对象.

比如, 一个处理者可能合理地发送这个作为它的参数的状况对象来允许更外部的处理者第一时机去处理这个状况. (这样一个处理者有时被称作 "默认处理者(default handler)".) 这个行为是允许的因为第二个发送过程处理的情况确实是相同的情况.

另一方面, 在一个通过用一个对 signal 的调用来打断用户进程进而实现异步键盘事件的具体实现中, 对于两个不同的异步键盘事件在不同情况相同时间下发送相同的状况是不允许的. 


#### 9.1.4.2 <span id="Restarts">重启动</span>

交互式状况处理者只通过非局部控制转移到特别定义的重启动来退出, 这个重启动可以通过系统或用户代码来设置. 转移控制到一个重启动被称为 "调用" 这个重启动. 类似于处理者, 活跃的重启动也动态地确立, 并且只有活跃的重启动可以被调用. 一个活跃的重启动可以被用户从调试器中或者被程序使用 invoke-restart 来调用.

当一个重启动被调用时, 这个重启动包含一个要被调用的函数, 一个被用于查找或调用这个重启动的可选名字, 以及一个用于调试器去使用户手动调用一个重启动的可选交互式信息集合.

一个重启动的名字被 invoke-restart 使用. 只能在调试器中调用的重启动不需要名字.

重启动可以通过使用 restart-bind, restart-case, 和 with-simple-restart 来建立. 一个重启动函数自身可以调用任何其他的在这个函数所属的重启动建立时是活跃的重启动.

通过一个 restart-bind 表达式形式, 一个 restart-case 表达式形式, 或者一个 with-simple-restart 表达式形式建立的重启动有着动态范围, 这个范围延伸到这个表达式形式执行期间.

相同名字的重启动可以根据下面两条规则来从最不新近的到最新近的排序:

1. 如果活跃的重启动集合 R1 中的活跃重启动被建立时集合 R2 中的重启动是活跃的, 那么 R1 中的每个重启都比 R2 中的每个重启更新近.

2. 使 r1 和 r2 为相同表达式形式建立的两个相同名字的活跃重启动. 如果在建立它们的表达式形式中 r1 被定义在 r2 的左边那么 r1 比 r2 更新近.

如果一个重启动被调用但是没有转移控制, 那么这个重启动函数产生的值会被调用这个重启动的函数返回, 不管是 invoke-restart 还是 invoke-restart-interactively.

> * 9.1.4.2.1 [重启动的交互式使用](#InteractiveUseRestarts)
> * 9.1.4.2.2 [重启动的接口](#InterfacesRestarts)
> * 9.1.4.2.3 [重启动测试](#RestartTests)
> * 9.1.4.2.4 [关联重启动和状况](#AssociatingRestartCondition)


##### 9.1.4.2.1 <span id="InteractiveUseRestarts">重启动的交互式使用</span>

关于交互式处理, 一个重启动需要两个信息: 一个汇报函数和一个交互式函数.

这个汇报函数被一个例如调试器的程序使用来呈现这个重启动会采取的动作的描述. 这个汇报函数通过给 restart-bind 的 :report-function 关键字或者给 restart-case 的 :report 关键字来指定和建立.

这个可以使用给 restart-bind 的 :interactive-function 关键字或给 restart-case 的 :interactive 关键字来指定的交互式函数在重启动被交互式调用时, 例如从调试器中, 会被用来产生一个合适的参数列表.

invoke-restart 调用和给 invoke-restart 的第一个参数相同名字的最近建立的重启动. 如果一个重启被调试器交互式调用并且没有转移控制而是返回值, 那么在这些值上的这个调试器的准确动作是由具体实现定义的. 


##### 9.1.4.2.2 <span id="InterfacesRestarts">重启动的接口</span>

一些重启动有着函数的接口, 例如 abort, continue, muffle-warning, store-value, 还有 use-value. 它们是内部使用 find-restart 和 invoke-restart 的普通函数, 有着和它们操纵的重启动相同的名字, 并且简单地出于标记方便而被提供.

下面这段中展示了和重启动相关的定义的名字.

    abort             invoke-restart-interactively  store-value          
    compute-restarts  muffle-warning                use-value            
    continue          restart-bind                  with-simple-restart  
    find-restart      restart-case                                       
    invoke-restart    restart-name                                       

    Figure 9-6. 重启动相关的定义的名字. 


##### 9.1.4.2.3 <span id="RestartTests">重启动测试</span>

每个重启动都有一个关联的测试, 它是一个单参数(一个状况或者 nil)的函数, 如果这个重启动在当前情况下应该是可见的就返回 true. 这个测试通过给 restart-bind 的 :test-function 选项或者给 restart-case 的 :test 选项创建. 


##### 9.1.4.2.4 <span id="AssociatingRestartCondition">关联重启动和状况</span>

一个重启动可以通过 with-condition-restarts 来和一个状况显式关联, 或者通过 restart-case 来隐式关联. 因此一个关联有着动态范围.

一个单个重启动可以同时和多个状况关联. 一个单个状况同时也可以和多个重启动关联.

和一个特定状况关联的活跃重启动可以通过调用例如 find-restart 函数并提供这个状况作为 condition 参数来检测. 没有任何关联状况的活跃重启动也可以通过以没有 condition 参数或者为这个参数提供 nil 值来调用这样一个函数而被检测到.


### 9.1.5 <span id="Assertions">断言</span>

基于键匹配, 表达式形式求值, 以及类型的条件状况发送由断言操作符处理. 下一段中展示了和断言相关的操作符.

    assert  check-type  ecase      
    ccase   ctypecase   etypecase  

    Figure 9-7. 断言相关的操作符. 


### 9.1.6 <span id="NotesConditionSystemBackground">Notes about the Condition System's Background</span>

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

        所有状况类型, 不管是错误或是非错误, 必须继承自这个类型.

        不允许在指定的类型 condition 的子类型中附加子类型关系, 除非在这个文本中明确提及; 但是具体实现允许去引入一些额外类型并且这些类型中的一个可以是类型 condition 任何数量的的子类型的一个子类型.

        一个用户定义的状况类型是否有着可以被 with-slots 访问的槽是取决于具体实现的. 此外, 即便在一个具体实现中用户定义的状况类型有着槽, 但是这个文档中定义的任何状况类型是否有着槽是依赖于具体实现的, 如果它们确实有那么它们的名字也是依赖于具体实现的; 只有这个规范中记录的读取器函数可以被可移植代码所依赖.

        符合规范的代码必须遵守下面这个和状况相关的约束:

        * define-condition, 而不是 defclass, 必须被用于定义新的状况类型.

        * make-condition, 而不是 make-instance, 必须被用于显式创建状况对象.

        * define-condition 的这个 :report 选项, 而不是 defmethod 对于 print-object, 必须被用于定义一个状况汇报者.

        * slot-value, slot-boundp, slot-makunbound, 和 with-slots 一定不能在状况对象上使用. 反而, 应该使用合适的访问器函数 (通过 define-condition 定义). 


### <span id="CT-WARNING">状况类型 WARNING</span>

* 类优先级列表(Class Precedence List):

        warning, condition, t

* 描述(Description):

        类型 warning 包含所有警告的类型.

* 也见(See Also):

        style-warning 


### <span id="CT-STYLE-WARNING">状况类型 STYLE-WARNING</span>

* 类优先级列表(Class Precedence List):

        style-warning, warning, condition, t

* 描述(Description):

        类型 style-warning 包括那些表示代码是符合规范的但是仍然被认为是错误的或者不符合标准的情况的状况.

* 也见(See Also):

        muffle-warning

* 注意(Notes):

        如果一个具体实现遇到使用废弃特性的代码或者不美观的或无效的代码, 它可能发出这样一个状况.

        一个 '没有被使用的变量(unused variable)' 警告必须是类型 style-warning.

        一般而言, 代码是错误的还是不合格的问题是由处理代码的工具做出的一个主观决定. 这样的意图是, 无论何时这样的一个工具想在主观理由上抱怨代码, 它应该使用这个状况类型, 以便用户去重定向或者抑制多余的警告而不用担心他们被重定向或抑制其他更严重的警告. 


### <span id="CT-SERIOUS-CONDITION">状况类型 SERIOUS-CONDITION</span>

* 类优先级列表(Class Precedence List):

serious-condition, condition, t

* 描述(Description):

        所有严重到如果没被处理就需要交互式干预的状况应该继承自类型 serious-condition. 提供这种状况类型主要是为了使它可以被包含作为其他状况类型的超类; 它不打算被直接发送.

* 注意(Notes):

        发送一个严重状况自身不会强制进入调试器. 然而, 除非在程序员可以确保不会因为处理不了严重状况而造成伤害的不寻常情况下, 否则这样一个状况通常使用 error 来发送而不是 signal 来确保这个程序在没有处理这个状况的情况下不会继续下去. (但反之, 使用 signal 而不是 error 去发送不是严重状况的状况是传统做法, 因为正常情况下, 处理非严重情况的失败并不是进入调试器的原因.) 


### <span id="CT-ERROR">状况类型 ERROR</span>

* 类优先级列表(Class Precedence List):

        error, serious-condition, condition, t

* 描述(Description):

        这个类型包括所有表示错误的类型. 


### <span id="CT-CELL-ERROR">状况类型 CELL-ERROR</span>

* 类优先级列表(Class Precedence List):

        cell-error, error, serious-condition, condition, t

* 描述(Description):

        类型 cell-error 由发生在位置访问期间的错误状况组成. 违规的 cell 的名字由 make-condition 的 :name 初始化参数来初始化, 通过函数 cell-error-name 来访问.

* 也见(See Also):

        cell-error-name 


### <span id="F-CELL-ERROR-NAME">函数 CELL-ERROR-NAME</span>

* 语法(Syntax):

        cell-error-name condition => name

* 参数和值(Arguments and Values):

        condition---一个类型 cell-error 的状况.
        name---一个对象.

* 描述(Description):

        返回那个状况 condition 所表示的情况中违规的 cell 的名字.

        结果的性质取决于状况 condition 指定的类型. 比如, 如果这个状况 condition 是类型 unbound-variable, 那么这个结果是那个要被访问的未绑定变量的名字, 如果这个状况 condition 是类型 undefined-function, 那么这个就是那个要被访问的未绑定函数的名字, 而当这个状况 condition 是类型 unbound-slot, 这个就是要被访问的槽的名字.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cell-error, unbound-slot, unbound-variable, undefined-function, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


### <span id="CT-PARSE-ERROR">状况类型 PARSE-ERROR</span>

* 类优先级列表(Class Precedence List):

        parse-error, error, serious-condition, condition, t

* 描述(Description):

        类型 parse-error 由解析相关的错误状况组成.

* 也见(See Also):

        parse-namestring, reader-error 


### <span id="CT-STORAGE-CONDITION">状况类型 STORAGE-CONDITION</span>

* 类优先级列表(Class Precedence List):

        storage-condition, serious-condition, condition, t

* 描述(Description):

        类型 storage-condition 由内存管理问题相关的严重状况组成, 这些状况可能是由于依赖于具体实现的限制而不是符合规范的程序中的语义错误, 并且如果没有被处理通常需要进入到调试器中. 根据具体实现的细节, 这些可能包括堆栈溢出, 内存区域溢出和存储耗尽等问题.

* 注意(Notes):

        尽管由于一些 Common Lisp 操作符被定义用来创建对象, 可能会发出 storage-condition, 但是那些不是被定义用来创建对象的操作符是否会创建它们并且是否也可能发出 storage-condition 是为指定的. 同样的, 求值器自身也可能创建对象并且因此可能发出 storage-condition. (自然的假设可能是, 这样的对象创建自然是低效的, 但即使这样做也是依赖于具体实现的.) 通常, 存储分配如何完成的整个问题都是和具体实现相关的, 因此任何操作符在任何时间都可能发出 storage-condition. 由于这样一个状况是具体实现或镜像的限制的象征而不是一个程序中的错误, 因此类型 storage-condition 的对象不是类型 error. 


### <span id="M-ASSERT">宏 ASSERT</span>

* 语法(Syntax):

        assert test-form [(place*) [datum-form argument-form*]]

=> nil

* 参数和值(Arguments and Values):

        test-form---一个表达式形式; 总是求值的.
        place---一个 place; 如果一个错误被发出就求值.
        datum-form---一个求值为一个 datum 的表达式形式. 每次一个错误要被发出就求值, 如果没有错误发出就一次都不求值.
        argument-form---求值为一个参数 argument 的一个表达式形式. 每次一个错误要被发出就求值, 如果没有错误发出就一次都不求值.
        datum, arguments---一个默认类型 error 的状况的标识符. (这些标识符是求值 datum-form 和每个 argument-form 的结果.)

* 描述(Description):

        assert 确保这个 test-form 求值为 true. 如果 test-form 求值为 false, assert 发出一个可校正的错误 (用 datum 和 arguments 来表示). 从这个错误中使用 continue 重启动来继续使得用户在 assert 再一次求值 test-form 之前修改 places 的值是可能的. 如果这个 test-form 的值不是 nil, assert 返回 nil.

        这些 place 是 test-form 所依赖的普通引用, 它们的值可以通过用户校正这个错误来改变. 每个 place 的子表达式形式只有在一个错误被发出时被求值, 并且在这个错误被再次发出时可能被再次求值 (在没有实际修正这个问题的情况下继续之后). 这些 place 的求值顺序没有被指定; 见章节 5.1.1.1 (Evaluation of Subforms to Places). 如果一个被提供的 place 表达式形式产生了超出看这些存储变量的值, 额外的值会被忽略. 如果提供的这个表达式形式产生的值少于这些存储变量, 那么缺少的值被设置为 nil.

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

        check-type, error, Section 5.1 (Generalized Reference)

* 注意(Notes):

        调试器不需要在错误信息中包含这个 test-form , 并且这些 place 也不应该被包含在这个信息中, 但是它们对于用户的目测应该是可用的. 如果用户给了  "continue" 命令, 这些引用的任何值都能被修改. 这个详情取决于具体实现的用户接口风格. 


### <span id="F-ERROR">函数 ERROR</span>

* 语法(Syntax):

        error datum &rest arguments =>|

* 参数和值(Arguments and Values):

        datum, arguments---一个默认类型 simple-error 的状况标识符.

* 描述(Description):

        error 实际上在表示的状况上调用 signal.

        如果这个状况没有被处理, (invoke-debugger condition) 就会被执行. 作为调用 invoke-debugger 的后果, error 不能直接返回; 从 error 中仅有的退出方式可以通过在一个处理者中非本地转移控制或使用交互式调试命令来实现.

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

        指定状况的那些处理者, 如果存在, 会被调用并且可能有副作用. 程序执行可能停止, 并且可能进入到调试器中.

* 受此影响(Affected By):

        已存在的处理者绑定.

        *break-on-signals*

* 异常情况(Exceptional Situations): 

        如果 datum 和 arguments 不是一个状况类型的标识符, 就发出一个类型 type-error 的错误.

* 也见(See Also):

        cerror, signal, format, ignore-errors, *break-on-signals*, handler-bind, Section 9.1 (Condition System Concepts)

* 注意(Notes):

        某些具体实现可能为从单独的堆栈帧中交互式返回提供调试器命令. 然而, 程序员应该有信心去编写像下面这样的代码:

        (defun wargames:no-win-scenario ()
          (if (error "pushing the button would be stupid."))
          (push-the-button))

        在这种情况下, error 不可能返回，按钮不会被按下.<!-- TODO 这个程序有问题-->

        虽然这个程序的意义是明确的, 并且它可能被正式的定理证明是"安全的", 但是这样的证明并不能保证程序的执行是安全的. 众所周知, 编译器有bug, 计算机有信号故障, 而人类则以不可能预测的方式进行手动干预 . 这些种类的错误, 虽然超出了状况系统对于正式建模的范围, 但它会超出编写代码时应该认真考虑的范围, 而这些代码可能会有本例所暗示的那种广泛的影响. 


### <span id="F-CERROR">函数 CERROR</span>

* 语法(Syntax):

        cerror continue-format-control datum &rest arguments => nil

* 参数和值(Arguments and Values):

        Continue-format-control---一个格式化控制字符串.
        datum, arguments---一个默认类型 simple-error 的状况的标识符.

* 描述(Description):

        cerror 实际上在名为 datum 的状况上调用 error. 就像任何隐式调用 error 的函数一样, 如果这个状况没有被处理, (invoke-debugger condition) 会被执行. 尽管正在发送中, 并且它到达了调试器中, 但是使用 continue 重启动来继续代码的执行(换句话说, 从 cerror 中返回)是可能的.

        如果 datum 是一个状况, 可以提供 arguments, 但是要和 continue-format-control 一起使用.

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

        如果 datum 是一个状况类型而不是一个字符串, 格式化指令 ~* 在 continue-format-control 中用来忽略初始化参数列表中的关键字可能是特别有用的. 比如:

        (cerror "enter a new value to replace ~*~s" 
                'not-a-number
                :argument a)



### <span id="M-CHECK-TYPE">宏 CHECK-TYPE</span>

* 语法(Syntax):

        check-type place typespec [string] => nil

* 参数和值(Arguments and Values):

        place---一个 place.
        typespec---一个类型指定符.
        string---一个字符串; 求值的.

* 描述(Description):
<!--TODO 待校验-->
        如果这个 place 的内容不是类型 typespec, 那么 check-type 发出一个类型 type-error 的可校正错误.

        当且仅当 store-value 重启动被调用, 不管是显式地从一个处理者或者隐式地作为作为调试器提供的其中一个选项, check-type 可以返回. 如果这个 store-value 重启动被调用, check-type 存储这个给重启动调用的 place 的参数(或者是通过调试器交互式提示的那个)新值并重新开始, 检测这个新值的类型并且如果它仍然不是要求的类型就会发出另一个错误.

        第一次 place 被求值时, 它通过正常的求值规则来求值. 如果类型检测失败并且使用了这个 store-value 重启动那么它接下来被被求值为一个 place; 见章节 5.1.1.1 (Evaluation of Subforms to Places).

        字符串 string 应该是这个类型的一个英语描述, 以一个不确定的冠词开始 ("a" 或者 "an"). 如果没有提供字符串 string, 它就会自动通过 typespec 来计算. 这个自动生成的信息提及 place, 它的内容和要求的类型. 如果一个具体实现可能把这个 place 识别为一个特殊表达式形式, 例如给名为 check-type 的函数的其中一个参数, 一个具体实现可能选择去生成一个措词有点不同的错误信息. 字符串 string 是允许的, 因为 check-type 的一些应用可能需要一个比从typespec 自动生成的更具体的关于需要什么的描述.

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

        章节 9.1 (Condition System Concepts)

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

        当提供了一个格式化控制作为 error 或 cerror 的第一个参数时, 类型 simple-error 由通过 error 或 cerror 发出的状况组成. 


### <span id="F-INVALID-METHOD-ERROR">函数 INVALID-METHOD-ERROR</span>

* 语法(Syntax):

        invalid-method-error method format-control &rest args => implementation-dependent

* 参数和值(Arguments and Values):

        method---一个方法.
        format-control---一个格式化控制.
        args---format-control 的格式化参数.

* 描述(Description):

        当这里有一个方法组合类型的限定符不合法的可应用方法时, 函数 invalid-method-error 被用于发出一个 error 类型的错误. 错误信息通过使用适用于 format 的 format-control 以及给它的任何参数组成. 由于一个具体实现可能需要给错误信息添加额外的上下文信息, invalid-method-error 应该只在一个方法组合函数的动态作用域中被调用.

        当一个方法不满足一个 define-method-combination 表达式形式中的每个限定符模式和断言, 那么函数 invalid-method-error 被自动调用. 如果一个强加额外限制的方法组合函数进入到一个它不能接受的方法中, 那么这个方法组合函数应该显式调用 invalid-method-error.

        invalid-method-error 是否返回到它的调用者还是通过 throw 退出是取决于具体实现的.

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

        format-control---一个格式化控制.
        args---format-control 的格式化参数.

* 描述(Description):

        函数 method-combination-error 被用于在方法组合中发出一个错误.

        这个错误信息通过使用一个适用于 format 的 format-control 和给它的参数来构造. 由于一个具体实现可能需要给这个错误信息添加额外的上下文信息, method-combination-error 应该只在一个方法组合函数的动态范围中被调用.

        method-combination-error 是否返回到它的调用者还是通过 throw 退出是取决于具体实现的.

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

        datum, arguments---一个默认类型 simple-condition 的状况的标识符.

* 描述(Description):

        发出这个给定的 datum 和 arguments 表示的状况. 如果这个状况没有被处理, signal 就返回 nil.

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

        *break-on-signals*, error, simple-condition, Section 9.1.4 (发送和处理状况)

* 注意(Notes):

        如果 (typep datum *break-on-signals*) 产生 true, 在这个发送过程开始前进入调试器. 这个 continue 重启动可以被用于继续这个发送过程. 对于所有应该, 可能, 或者必须发送状况的其他函数和宏, 这个也是对的. 


### <span id="CT-SIMPLE-CONDITION">状况类型 SIMPLE-CONDITION</span>

* 类优先级列表(Class Precedence List):

        simple-condition, condition, t

* 描述(Description):

        每当一个 format-control 被提供为 signal 函数的第一个参数, 类型 simple-condition 表示通过这个 signal 发出的状况. 这个格式化控制和格式化参数使用给 make-condition 的名为 :format-control 和 :format-arguments 的初始化参数来初始化, 并且可以通过函数 simple-condition-format-control 和 simple-condition-format-arguments 来访问. 如果格式化参数没有提供给 make-condition, 那么 nil 就被用作默认值.

* 也见(See Also):

        simple-condition-format-control, simple-condition-format-arguments 


### <span id="F-SCFC-SCFA">函数 SIMPLE-CONDITION-FORMAT-CONTROL, SIMPLE-CONDITION-FORMAT-ARGUMENTS</span>

* 语法(Syntax):

        simple-condition-format-control condition => format-control

        simple-condition-format-arguments condition => format-arguments

* 参数和值(Arguments and Values):

        condition---一个 simple-condition 类型的状况.
        format-control---一个格式化控制.
        format-arguments---一个列表.

* 描述(Description):

        simple-condition-format-control 返回处理这个状况 condition 的格式化参数所需要的格式化控制.

        simple-condition-format-arguments 返回一个处理这个状况 condition 的格式化控制所需要的格式化参数的列表.

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

        simple-condition, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


### <span id="F-WARN">函数 WARN</span>

* 语法(Syntax):

        warn datum &rest arguments => nil

* 参数和值(Arguments and Values):

        datum, arguments---一个默认类型 simple-warning 的状况的标识符.

* 描述(Description):

        发送一个类型 warning 的状况. 如果这个状况没有被处理, 报告这个状况到错误输出.

        警告的精确机制如下:

        这个 warning 状况被发出

            当这个 warning 状况要被发送时, 这个 muffle-warning 重启动会为了被一个处理者使用而被建立. 如果被调用了, 这个重启动绕开 warn 的进一步动作, 这个反过来导致了 warn 立即返回 nil.

        如果没有找到这个 warning 状况的处理者

            如果没有找到这个 warning 状况的处理者, 或者这些处理者拒绝了, 那么这个状况会通过 warn 以一种依赖于具体实现的格式被报告到错误输出.

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

        如果 datum 是一个状况并且这个状况不是类型 warning, 或者参数 arguments 不是 nil, 会发出一个类型 type-error 的错误.

        如果 datum 是一个状况类型, (apply #'make-condition datum arguments) 的结果必须是类型 warning, 否则发出一个类型 type-error 的错误.

* 也见(See Also):

        *break-on-signals*, muffle-warning, signal

* 注意(Notes): None. 


### <span id="CT-SIMPLE-WARNING">状况类型 SIMPLE-WARNING</span>

* 类优先级列表(Class Precedence List):

        simple-warning, simple-condition, warning, condition, t

* 描述(Description):

        每当一个格式化控制被提供为这个 warn 的第一个参数, 类型 simple-warning 表示 warn 发送的状况. 


### <span id="F-INVOKE-DEBUGGER">函数 INVOKE-DEBUGGER</span>

* 语法(Syntax):

        invoke-debugger condition =>|

* 参数和值(Arguments and Values):

        condition---一个状况对象.

* 描述(Description):

        invoke-debugger 尝试去和状况 condition 一起进入这个调试器.

        如果 *debugger-hook* 不是 nil, 它应该是一个在进入标准调试器之前要被调用的函数(或者一个函数的名字). 随着 *debugger-hook* 绑定为 nil 这个函数被调用, 并且这个函数必须接受两个参数: 这个状况 condition 和这个 *debugger-hook* 被绑定给 nil 的值. 如果这个函数正常范围, 就进入标准调试器.

        标准调试器从不直接返回. 只有通过一个例如使用 restart 函数的非局部转移才可能返回.

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

        format-control---一个格式化控制. 默认值是依赖于具体实现的.
        format-arguments---这个 format-control 的格式化参数.

* 描述(Description):

        break 格式化 format-control 和 format-arguments, 然后在没有任何被程控的错误处理工具拦截可能的情况下直接进入调试器中.

        如果在这个调试器中, 这个 continue 重启动被使用, break 在没有采取任何不寻常的恢复动作的情况下立即返回 nil.

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

        break 被用作在一个程序中临时插入调试"断点(breakpoints)"的方式, 而不是作为发送错误的方式. 由于这个原因, break 不接受这个 cerror 接受的 continue-format-control 参数. 这个以及通过状况处理的拦截可能性的缺失是 break 和 cerror 之间仅有的程序可见的区别.

        break 和 cerror 的用户接口方面允许更广泛的变化, 为了适应这个具体实现的接口需求. 比如, 对于一个 Lisp read-eval-print 循环允许通过 break 而不是常规的调试器来进入.

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

        一个两参数函数 (a condition and the value of *debugger-hook* at the time the debugger was entered) 的标识符, 或者 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        当这个 *debugger-hook* 的值不是 nil, 它在正常进入调试器前被调用, 不管是由于调用 invoke-debugger 或是从一个带有一个未被处理的状况的 error 或 cerror 调用中进入调试器. 这个函数可能处理这个状况 (转移控制) 或者正常返回 (允许标准这个标准调试器来运行). 为了最小化调试期间的递归错误, *debugger-hook* 在调用这个函数前通过 invoke-debugger 被绑定为 nil.

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

        在求值用户交互式输入的代码时, 有时候, 让钩子函数绑定 *debugger-hook* 到它的第二个参数的函数是很有用的, 这样就可以使用相同的交互式工具来处理递归错误了. 


### <span id="V-BREAK-ON-SIGNALS">变量 *BREAK-ON-SIGNALS*</span>
<!--TODO 待校验-->
* 值类型(Value Type):

        一个类型特化符.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        当 (typep condition *break-on-signals*) 返回 true 时, 调用 signal, 以及调用其他像 error 这样显式调用 signal 的操作符, 在发送这个状况前进入到调试器中.

        当一个 break 由于 *break-on-signals* 发生时, 这个 continue 重启动可以被用于继续这个正常的发送过程.

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

        break, signal, warn, error, typep, Section 9.1 (Condition System Concepts)

* 注意(Notes):

        *break-on-signals* 主要用于调试已经发送的代码. 当设置了 *break-on-signals* 时, 鼓励用户去选择满足的最约束的说明. 设置 *break-on-signals* 实际上违反了状况发送的模块化处理. 事实上, 设置 *break-on-signals* 的完整影响在某些方面可能是不可预测的, 因为用户可能没有意识到在代码中使用的对 signal 调用的种类和数量.

        *break-on-signals* 允许早期进入调试器, 但是这样的一个进入不会阻止例如 error 和 cerror 这样的操作下额外进入调试器. 


### <span id="M-HANDLER-BIND">宏 HANDLER-BIND</span>

* 语法(Syntax):

handler-bind ({binding}*) form* => result*

binding::= (type handler) 

* 参数和值(Arguments and Values):

type---a type specifier.

handler---a form; evaluated to produce a handler-function.

handler-function---a designator for a function of one argument.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

Executes forms in a dynamic environment where the indicated handler bindings are in effect.

Each handler should evaluate to a handler-function, which is used to handle conditions of the given type during execution of the forms. This function should take a single argument, the condition being signaled.

If more than one handler binding is supplied, the handler bindings are searched sequentially from top to bottom in search of a match (by visual analogy with typecase). If an appropriate type is found, the associated handler is run in a dynamic environment where none of these handler bindings are visible (to avoid recursive errors). If the handler declines, the search continues for another handler.

If no appropriate handler is found, other handlers are sought from dynamically enclosing contours. If no handler is found outside, then signal returns or error enters the debugger.

* 示例(Examples):

In the following code, if an unbound variable error is signaled in the body (and not handled by an intervening handler), the first function is called.

 (handler-bind ((unbound-variable #'(lambda ...))
                (error #'(lambda ...)))
   ...)

If any other kind of error is signaled, the second function is called. In either case, neither handler is active while executing the code in the associated function.

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

Note that ``Foo.'' is not printed because the condition made by signal is a simple condition, which is not of type error, so it doesn't trigger the handler for error set up by trap-errors.

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

expression---a form.

typespec---a type specifier.

var---a variable name.

lambda-list---an ordinary lambda list.

declaration---a declare expression; not evaluated.

form---a form.

results---In the normal situation, the values returned are those that result from the evaluation of expression; in the exceptional situation when control is transferred to a clause, the value of the last form in that clause is returned.

* 描述(Description):

handler-case executes expression in a dynamic environment where various handlers are active. Each error-clause specifies how to handle a condition matching the indicated typespec. A no-error-clause allows the specification of a particular action if control returns normally.

If a condition is signaled for which there is an appropriate error-clause during the execution of expression (i.e., one for which (typep condition 'typespec) returns true) and if there is no intervening handler for a condition of that type, then control is transferred to the body of the relevant error-clause. In this case, the dynamic state is unwound appropriately (so that the handlers established around the expression are no longer active), and var is bound to the condition that had been signaled. If more than one case is provided, those cases are made accessible in parallel. That is, in

  (handler-case form
    (typespec1 (var1) form1)
    (typespec2 (var2) form2))

if the first clause (containing form1) has been selected, the handler for the second is no longer visible (or vice versa).

The clauses are searched sequentially from top to bottom. If there is type overlap between typespecs, the earlier of the clauses is selected.

If var is not needed, it can be omitted. That is, a clause such as:

  (typespec (var) (declare (ignore var)) form)

can be written (typespec () form).

If there are no forms in a selected clause, the case, and therefore handler-case, returns nil. If execution of expression returns normally and no no-error-clause exists, the values returned by expression are returned by handler-case. If execution of expression returns normally and a no-error-clause does exist, the values returned are used as arguments to the function described by constructing (lambda lambda-list form*) from the no-error-clause, and the values of that function call are returned by handler-case. The handlers which were established around the expression are no longer active at the time of this call.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

handler-bind, ignore-errors, Section 9.1 (Condition System Concepts)

* 注意(Notes):

 (handler-case form
   (type1 (var1) . body1)
   (type2 (var2) . body2) ...)

is approximately equivalent to:

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

 (handler-case form
   (type1 (var1) . body1)
   ...
   (:no-error (varN-1 varN-2 ...) . bodyN))

is approximately equivalent to:


 (block #1=#:error-return
  (multiple-value-call #'(lambda (varN-1 varN-2 ...) . bodyN)
     (block #2=#:normal-return
       (return-from #1#
         (handler-case (return-from #2# form)
           (type1 (var1) . body1) ...)))))


### <span id="M-IGNORE-ERRORS">宏 IGNORE-ERRORS</span>

* 语法(Syntax):

ignore-errors form* => result*

* 参数和值(Arguments and Values):

forms---an implicit progn.

results---In the normal situation, the values of the forms are returned; in the exceptional situation, two values are returned: nil and the condition.

* 描述(Description):

ignore-errors is used to prevent conditions of type error from causing entry into the debugger.

Specifically, ignore-errors executes forms in a dynamic environment where a handler for conditions of type error has been established; if invoked, it handles such conditions by returning two values, nil and the condition that was signaled, from the ignore-errors form.

If a normal return from the forms occurs, any values returned are returned by ignore-errors.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

handler-case, Section 9.1 (Condition System Concepts)

* 注意(Notes):

 (ignore-errors . forms)

is equivalent to:

 (handler-case (progn . forms)
   (error (condition) (values nil condition)))

Because the second return value is a condition in the exceptional case, it is common (but not required) to arrange for the second return value in the normal case to be missing or nil so that the two situations can be distinguished. 


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

name---a symbol.

parent-type---a symbol naming a condition type. If no parent-types are supplied, the parent-types default to (condition).

default-initargs---a list of keyword/value pairs.

Slot-spec -- the name of a slot or a list consisting of the slot-name followed by zero or more slot-options.

Slot-name -- a slot name (a symbol), the list of a slot name, or the list of slot name/slot form pairs.

Option -- Any of the following:

:reader

    :reader can be supplied more than once for a given slot and cannot be nil.

:writer

    :writer can be supplied more than once for a given slot and must name a generic function.

:accessor

    :accessor can be supplied more than once for a given slot and cannot be nil.

:allocation

    :allocation can be supplied once at most for a given slot. The default if :allocation is not supplied is :instance.

:initarg

    :initarg can be supplied more than once for a given slot.

:initform

    :initform can be supplied once at most for a given slot.

:type

    :type can be supplied once at most for a given slot.

:documentation

    :documentation can be supplied once at most for a given slot.

:report

    :report can be supplied once at most.

* 描述(Description):

define-condition defines a new condition type called name, which is a subtype of the type or types named by parent-type. Each parent-type argument specifies a direct supertype of the new condition. The new condition inherits slots and methods from each of its direct supertypes, and so on.

If a slot name/slot form pair is supplied, the slot form is a form that can be evaluated by make-condition to produce a default value when an explicit value is not provided. If no slot form is supplied, the contents of the slot is initialized in an implementation-dependent way.

If the type being defined and some other type from which it inherits have a slot by the same name, only one slot is allocated in the condition, but the supplied slot form overrides any slot form that might otherwise have been inherited from a parent-type. If no slot form is supplied, the inherited slot form (if any) is still visible.

Accessors are created according to the same rules as used by defclass.

A description of slot-options follows:

:reader

    The :reader slot option specifies that an unqualified method is to be defined on the generic function named by the argument to :reader to read the value of the given slot.

* The :initform slot option is used to provide a default initial value form to be used in the initialization of the slot. This form is evaluated every time it is used to initialize the slot. The lexical environment in which this form is evaluated is the lexical environment in which the define-condition form was evaluated. Note that the lexical environment refers both to variables and to functions. For local slots, the dynamic environment is the dynamic environment in which make-condition was called; for shared slots, the dynamic environment is the dynamic environment in which the define-condition form was evaluated.

    No implementation is permitted to extend the syntax of define-condition to allow (slot-name form) as an abbreviation for (slot-name :initform form).

:initarg

    The :initarg slot option declares an initialization argument named by its symbol argument and specifies that this initialization argument initializes the given slot. If the initialization argument has a value in the call to initialize-instance, the value is stored into the given slot, and the slot's :initform slot option, if any, is not evaluated. If none of the initialization arguments specified for a given slot has a value, the slot is initialized according to the :initform slot option, if specified.

:type

    The :type slot option specifies that the contents of the slot is always of the specified type. It effectively declares the result type of the reader generic function when applied to an object of this condition type. The consequences of attempting to store in a slot a value that does not satisfy the type of the slot is undefined.

:default-initargs

    This option is treated the same as it would be defclass.

:documentation

    The :documentation slot option provides a documentation string for the slot.

:report

    Condition reporting is mediated through the print-object method for the condition type in question, with *print-escape* always being nil. Specifying (:report report-name) in the definition of a condition type C is equivalent to:

     (defmethod print-object ((x c) stream)
       (if *print-escape* (call-next-method) (report-name x stream)))

    If the value supplied by the argument to :report (report-name) is a symbol or a lambda expression, it must be acceptable to function. (function report-name) is evaluated in the current lexical environment. It should return a function of two arguments, a condition and a stream, that prints on the stream a description of the condition. This function is called whenever the condition is printed while *print-escape* is nil.

    If report-name is a string, it is a shorthand for

     (lambda (condition stream)
       (declare (ignore condition))
       (write-string report-name stream))

    This option is processed after the new condition type has been defined, so use of the slot accessors within the :report function is permitted. If this option is not supplied, information about how to report this type of condition is inherited from the parent-type.

The consequences are unspecifed if an attempt is made to read a slot that has not been explicitly initialized and that has not been given a default value.

The consequences are unspecified if an attempt is made to assign the slots by using setf.

If a define-condition form appears as a top level form, the compiler must make name recognizable as a valid type name, and it must be possible to reference the condition type as the parent-type of another condition type in a subsequent define-condition form in the file being compiled.

* 示例(Examples):

The following form defines a condition of type peg/hole-mismatch which inherits from a condition type called blocks-world-error:

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

The new type has slots peg-shape and hole-shape, so make-condition accepts :peg-shape and :hole-shape keywords. The readers peg/hole-mismatch-peg-shape and peg/hole-mismatch-hole-shape apply to objects of this type, as illustrated in the :report information.

The following form defines a condition type named machine-error which inherits from error:

(define-condition machine-error 
                  (error)
                  ((machine-name :initarg :machine-name
                                 :reader machine-error-machine-name))
  (:report (lambda (condition stream)
             (format stream "There is a problem with ~A."
                     (machine-error-machine-name condition)))))

Building on this definition, a new error condition can be defined which is a subtype of machine-error for use when machines are not available:

(define-condition machine-not-available-error (machine-error) ()
  (:report (lambda (condition stream)
             (format stream "The machine ~A is not available."
                     (machine-error-machine-name condition)))))

This defines a still more specific condition, built upon machine-not-available-error, which provides a slot initialization form for machine-name but which does not provide any new slots or report information. It just gives the machine-name slot a default initialization:

(define-condition my-favorite-machine-not-available-error
                  (machine-not-available-error)
  ((machine-name :initform "mc.lcs.mit.edu")))

Note that since no :report clause was given, the information inherited from machine-not-available-error is used to report this type of condition.

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-condition, defclass, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


### <span id="F-MAKE-CONDITION">函数 MAKE-CONDITION</span>

* 语法(Syntax):

make-condition type &rest slot-initializations => condition

* 参数和值(Arguments and Values):

type---a type specifier (for a subtype of condition).

slot-initializations---an initialization argument list.

condition---a condition.

* 描述(Description):

Constructs and returns a condition of type type using slot-initializations for the initial values of the slots. The newly created condition is returned.

* 示例(Examples):

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

An object of type restart represents a function that can be called to perform some form of recovery action, usually a transfer of control to an outer point in the running program.

An implementation is free to implement a restart in whatever manner is most convenient; a restart has only dynamic extent relative to the scope of the binding form which establishes it. 


### <span id="F-COMPUTE-RESTARTS">函数 COMPUTE-RESTARTS</span>

* 语法(Syntax):

compute-restarts &optional condition => restarts

* 参数和值(Arguments and Values):

condition---a condition object, or nil.

restarts---a list of restarts.

* 描述(Description):

compute-restarts uses the dynamic state of the program to compute a list of the restarts which are currently active.

The resulting list is ordered so that the innermost (more-recently established) restarts are nearer the head of the list.

When condition is non-nil, only those restarts are considered that are either explicitly associated with that condition, or not associated with any condition; that is, the excluded restarts are those that are associated with a non-empty set of conditions of which the given condition is not an element. If condition is nil, all restarts are considered.

compute-restarts returns all applicable restarts, including anonymous ones, even if some of them have the same name as others and would therefore not be found by find-restart when given a symbol argument.

Implementations are permitted, but not required, to return distinct lists from repeated calls to compute-restarts while in the same dynamic environment. The consequences are undefined if the list returned by compute-restarts is every modified.

* 示例(Examples):

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
 

* 副作用(Side Effects): None.

* 受此影响(Affected By):

Existing restarts.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

find-restart, invoke-restart, restart-bind

* 注意(Notes): None. 


### <span id="F-FIND-RESTART">函数 FIND-RESTART</span>

* 语法(Syntax):

find-restart identifier &optional condition

restart

* 参数和值(Arguments and Values):

identifier---a non-nil symbol, or a restart.

condition---a condition object, or nil.

restart---a restart or nil.

* 描述(Description):

find-restart searches for a particular restart in the current dynamic environment.

When condition is non-nil, only those restarts are considered that are either explicitly associated with that condition, or not associated with any condition; that is, the excluded restarts are those that are associated with a non-empty set of conditions of which the given condition is not an element. If condition is nil, all restarts are considered.

If identifier is a symbol, then the innermost (most recently established) applicable restart with that name is returned. nil is returned if no such restart is found.

If identifier is a currently active restart, then it is returned. Otherwise, nil is returned.

* 示例(Examples):

 (restart-case
     (let ((r (find-restart 'my-restart)))
       (format t "~S is named ~S" r (restart-name r)))
   (my-restart () nil))
>>  #<RESTART 32307325> is named MY-RESTART
=>  NIL
 (find-restart 'my-restart)
=>  NIL

* 副作用(Side Effects): None.

* 受此影响(Affected By):

Existing restarts.

restart-case, restart-bind, with-condition-restarts.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

compute-restarts

* 注意(Notes):

 (find-restart identifier)
 ==  (find identifier (compute-restarts) :key :restart-name)

Although anonymous restarts have a name of nil, the consequences are unspecified if nil is given as an identifier. Occasionally, programmers lament that nil is not permissible as an identifier argument. In most such cases, compute-restarts can probably be used to simulate the desired effect. 


### <span id="F-INVOKE-RESTART">函数 INVOKE-RESTART</span>

* 语法(Syntax):

invoke-restart restart &rest arguments => result*

* 参数和值(Arguments and Values):

restart---a restart designator.

argument---an object.

results---the values returned by the function associated with restart, if that function returns.

* 描述(Description):

Calls the function associated with restart, passing arguments to it. Restart must be valid in the current dynamic environment.

* 示例(Examples):

 (defun add3 (x) (check-type x number) (+ x 3))
 
 (foo 'seven)
>>  Error: The value SEVEN was not of type NUMBER.
>>  To continue, type :CONTINUE followed by an option number:
>>   1: Specify a different value to use.
>>   2: Return to Lisp Toplevel.
>>  Debug> (invoke-restart 'store-value 7)
=>  10

* 副作用(Side Effects):

A non-local transfer of control might be done by the restart.

* 受此影响(Affected By):

Existing restarts.

* 异常情况(Exceptional Situations):

If restart is not valid, an error of type control-error is signaled.

* 也见(See Also):

find-restart, restart-bind, restart-case, invoke-restart-interactively

* 注意(Notes):

The most common use for invoke-restart is in a handler. It might be used explicitly, or implicitly through invoke-restart-interactively or a restart function.

Restart functions call invoke-restart, not vice versa. That is, invoke-restart provides primitive functionality, and restart functions are non-essential ``syntactic sugar.'' 


### <span id="F-INVOKE-RESTART-INTERACTIVELY">函数 INVOKE-RESTART-INTERACTIVELY</span>

* 语法(Syntax):

invoke-restart-interactively restart => result*

* 参数和值(Arguments and Values):

restart---a restart designator.

results---the values returned by the function associated with restart, if that function returns.

* 描述(Description):

invoke-restart-interactively calls the function associated with restart, prompting for any necessary arguments. If restart is a name, it must be valid in the current dynamic environment.

invoke-restart-interactively prompts for arguments by executing the code provided in the :interactive keyword to restart-case or :interactive-function keyword to restart-bind.

If no such options have been supplied in the corresponding restart-bind or restart-case, then the consequences are undefined if the restart takes required arguments. If the arguments are optional, an argument list of nil is used.

Once the arguments have been determined, invoke-restart-interactively executes the following:

 (apply #'invoke-restart restart arguments)

* 示例(Examples):

 (defun add3 (x) (check-type x number) (+ x 3))
 
 (add3 'seven)
>>  Error: The value SEVEN was not of type NUMBER.
>>  To continue, type :CONTINUE followed by an option number:
>>   1: Specify a different value to use.
>>   2: Return to Lisp Toplevel.
>>  Debug> (invoke-restart-interactively 'store-value)
>>  Type a form to evaluate and use: 7
=>  10

* 副作用(Side Effects):

If prompting for arguments is necesary, some typeout may occur (on query I/O).

A non-local transfer of control might be done by the restart.

* 受此影响(Affected By):

*query-io*, active restarts

* 异常情况(Exceptional Situations):

If restart is not valid, an error of type control-error is signaled.

* 也见(See Also):

find-restart, invoke-restart, restart-case, restart-bind

* 注意(Notes):

invoke-restart-interactively is used internally by the debugger and may also be useful in implementing other portable, interactive debugging tools. 


### <span id="M-RESTART-BIND">宏 RESTART-BIND</span>

* 语法(Syntax):

restart-bind ({(name function {key-val-pair}*)}) form*

=> result*

key-val-pair::= :interactive-function interactive-function |  
                :report-function report-function |  
                :test-function test-function 

* 参数和值(Arguments and Values):

name---a symbol; not evaluated.

function---a form; evaluated.

forms---an implicit progn.

interactive-function---a form; evaluated.

report-function---a form; evaluated.

test-function---a form; evaluated.

results---the values returned by the forms.

* 描述(Description):

restart-bind executes the body of forms in a dynamic environment where restarts with the given names are in effect.

If a name is nil, it indicates an anonymous restart; if a name is a non-nil symbol, it indicates a named restart.

The function, interactive-function, and report-function are unconditionally evaluated in the current lexical and dynamic environment prior to evaluation of the body. Each of these forms must evaluate to a function.

If invoke-restart is done on that restart, the function which resulted from evaluating function is called, in the dynamic environment of the invoke-restart, with the arguments given to invoke-restart. The function may either perform a non-local transfer of control or may return normally.

If the restart is invoked interactively from the debugger (using invoke-restart-interactively), the arguments are defaulted by calling the function which resulted from evaluating interactive-function. That function may optionally prompt interactively on query I/O, and should return a list of arguments to be used by invoke-restart-interactively when invoking the restart.

If a restart is invoked interactively but no interactive-function is used, then an argument list of nil is used. In that case, the function must be compatible with an empty argument list.

If the restart is presented interactively (e.g., by the debugger), the presentation is done by calling the function which resulted from evaluating report-function. This function must be a function of one argument, a stream. It is expected to print a description of the action that the restart takes to that stream. This function is called any time the restart is printed while *print-escape* is nil.

In the case of interactive invocation, the result is dependent on the value of :interactive-function as follows.

:interactive-function

    Value is evaluated in the current lexical environment and should return a function of no arguments which constructs a list of arguments to be used by invoke-restart-interactively when invoking this restart. The function may prompt interactively using query I/O if necessary.

:report-function

    Value is evaluated in the current lexical environment and should return a function of one argument, a stream, which prints on the stream a summary of the action that this restart takes. This function is called whenever the restart is reported (printed while *print-escape* is nil). If no :report-function option is provided, the manner in which the restart is reported is implementation-dependent.

:test-function

    Value is evaluated in the current lexical environment and should return a function of one argument, a condition, which returns true if the restart is to be considered visible.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

*query-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

restart-case, with-simple-restart

* 注意(Notes):

restart-bind is primarily intended to be used to implement restart-case and might be useful in implementing other macros. Programmers who are uncertain about whether to use restart-case or restart-bind should prefer restart-case for the cases where it is powerful enough, using restart-bind only in cases where its full generality is really needed. 


### <span id="M-RESTART-CASE">宏 RESTART-CASE</span>

* 语法(Syntax):

restart-case restartable-form {clause} => result*

clause::= (case-name lambda-list  
           [[:interactive interactive-expression | :report report-expression | :test test-expression]]  
           declaration* form*) 

* 参数和值(Arguments and Values):

restartable-form---a form.

case-name---a symbol or nil.

lambda-list---an ordinary lambda list.

interactive-expression---a symbol or a lambda expression.

report-expression---a string, a symbol, or a lambda expression.

test-expression---a symbol or a lambda expression.

declaration---a declare expression; not evaluated.

form---a form.

results---the values resulting from the evaluation of restartable-form, or the values returned by the last form executed in a chosen clause, or nil.

* 描述(Description):

restart-case evaluates restartable-form in a dynamic environment where the clauses have special meanings as points to which control may be transferred. If restartable-form finishes executing and returns any values, all values returned are returned by restart-case and processing has completed. While restartable-form is executing, any code may transfer control to one of the clauses (see invoke-restart). If a transfer occurs, the forms in the body of that clause is evaluated and any values returned by the last such form are returned by restart-case. In this case, the dynamic state is unwound appropriately (so that the restarts established around the restartable-form are no longer active) prior to execution of the clause.

If there are no forms in a selected clause, restart-case returns nil.

If case-name is a symbol, it names this restart.

It is possible to have more than one clause use the same case-name. In this case, the first clause with that name is found by find-restart. The other clauses are accessible using compute-restarts.

Each arglist is an ordinary lambda list to be bound during the execution of its corresponding forms. These parameters are used by the restart-case clause to receive any necessary data from a call to invoke-restart.

By default, invoke-restart-interactively passes no arguments and all arguments must be optional in order to accomodate interactive restarting. However, the arguments need not be optional if the :interactive keyword has been used to inform invoke-restart-interactively about how to compute a proper argument list.

Keyword options have the following meaning.

:interactive

    The value supplied by :interactive value must be a suitable argument to function. (function value) is evaluated in the current lexical environment. It should return a function of no arguments which returns arguments to be used by invoke-restart-interactively when it is invoked. invoke-restart-interactively is called in the dynamic environment available prior to any restart attempt, and uses query I/O for user interaction.

    If a restart is invoked interactively but no :interactive option was supplied, the argument list used in the invocation is the empty list.

:report

    If the value supplied by :report value is a lambda expression or a symbol, it must be acceptable to function. (function value) is evaluated in the current lexical environment. It should return a function of one argument, a stream, which prints on the stream a description of the restart. This function is called whenever the restart is printed while *print-escape* is nil.

    If value is a string, it is a shorthand for

     (lambda (stream) (write-string value stream))

    If a named restart is asked to report but no report information has been supplied, the name of the restart is used in generating default report text.

    When *print-escape* is nil, the printer uses the report information for a restart. For example, a debugger might announce the action of typing a ``continue'' command by:

     (format t "~&~S -- ~A~%" ':continue some-restart)

    which might then display as something like:

     :CONTINUE -- Return to command level

    The consequences are unspecified if an unnamed restart is specified but no :report option is provided.

:test

    The value supplied by :test value must be a suitable argument to function. (function value) is evaluated in the current lexical environment. It should return a function of one argument, the condition, that returns true if the restart is to be considered visible.

    The default for this option is equivalent to (lambda (c) (declare (ignore c)) t).

If the restartable-form is a list whose car is any of the symbols signal, error, cerror, or warn (or is a macro form which macroexpands into such a list), then with-condition-restarts is used implicitly to associate the indicated restarts with the condition to be signaled.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

restart-bind, with-simple-restart.

* 注意(Notes):

 (restart-case expression
    (name1 arglist1 ...options1... . body1)
    (name2 arglist2 ...options2... . body2))

is essentially equivalent to

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

Unnamed restarts are generally only useful interactively and an interactive option which has no description is of little value. Implementations are encouraged to warn if an unnamed restart is used and no report information is provided at compilation time. At runtime, this error might be noticed when entering the debugger. Since signaling an error would probably cause recursive entry into the debugger (causing yet another recursive error, etc.) it is suggested that the debugger print some indication of such problems when they occur but not actually signal errors.

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


### <span id="F-RESTART-NAME">函数 RESTART-NAME</span>

* 语法(Syntax):

restart-name restart => name

* 参数和值(Arguments and Values):

restart---a restart.

name---a symbol.

* 描述(Description):

Returns the name of the restart, or nil if the restart is not named.

* 示例(Examples):

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

condition-form---a form; evaluated to produce a condition.

condition---a condition object resulting from the evaluation of condition-form.

restart-form---a form; evaluated to produce a restart-list.

restart-list---a list of restart objects resulting from the evaluation of restart-form.

forms---an implicit progn; evaluated.

results---the values returned by forms.

* 描述(Description):

First, the condition-form and restarts-form are evaluated in normal left-to-right order; the primary values yielded by these evaluations are respectively called the condition and the restart-list.

Next, the forms are evaluated in a dynamic environment in which each restart in restart-list is associated with the condition. See Section 9.1.4.2.4 (关联重启动和状况).

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

restart-case

* 注意(Notes):

Usually this macro is not used explicitly in code, since restart-case handles most of the common cases in a way that is syntactically more concise. 


### <span id="M-WITH-SIMPLE-RESTART">宏 WITH-SIMPLE-RESTART</span>

* 语法(Syntax):

with-simple-restart (name format-control format-argument*) form*

=> result*

* 参数和值(Arguments and Values):

name---a symbol.

format-control---a format control.

format-argument---an object (i.e., a format argument).

forms---an implicit progn.

results---in the normal situation, the values returned by the forms; in the exceptional situation where the restart named name is invoked, two values---nil and t.

* 描述(Description):

with-simple-restart establishes a restart.

If the restart designated by name is not invoked while executing forms, all values returned by the last of forms are returned. If the restart designated by name is invoked, control is transferred to with-simple-restart, which returns two values, nil and t.

If name is nil, an anonymous restart is established.

The format-control and format-arguments are used report the restart.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

restart-case

* 注意(Notes):

with-simple-restart is shorthand for one of the most common uses of restart-case.

with-simple-restart could be defined by:

 (defmacro with-simple-restart ((restart-name format-control
                                              &rest format-arguments)
                                &body forms)
   `(restart-case (progn ,@forms)
      (,restart-name ()
          :report (lambda (stream)
                    (format stream ,format-control ,@format-arguments))
         (values nil t))))

Because the second return value is t in the exceptional case, it is common (but not required) to arrange for the second return value in the normal case to be missing or nil so that the two situations can be distinguished. 


### <span id="R-ABORT">重启动 ABORT</span>

* 必要数据参数(Data Arguments Required):

None.

* 描述(Description):

The intent of the abort restart is to allow return to the innermost ``command level.'' Implementors are encouraged to make sure that there is always a restart named abort around any user code so that user code can call abort at any time and expect something reasonable to happen; exactly what the reasonable thing is may vary somewhat. Typically, in an interactive listener, the invocation of abort returns to the Lisp reader phase of the Lisp read-eval-print loop, though in some batch or multi-processing situations there may be situations in which having it kill the running process is more appropriate.

* 也见(See Also):

Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, abort (function) 


### <span id="R-CONTINUE">重启动 CONTINUE</span>

* 必要数据参数(Data Arguments Required):

None.

* 描述(Description):

The continue restart is generally part of protocols where there is a single ``obvious'' way to continue, such as in break and cerror. Some user-defined protocols may also wish to incorporate it for similar reasons. In general, however, it is more reliable to design a special purpose restart with a name that more directly suits the particular application.

* 示例(Examples):

 (let ((x 3))
   (handler-bind ((error #'(lambda (c)
                             (let ((r (find-restart 'continue c)))
                               (when r (invoke-restart r))))))
     (cond ((not (floatp x))
            (cerror "Try floating it." "~D is not a float." x)
            (float x))
           (t x)))) =>  3.0

* 也见(See Also):

Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, continue (function), assert, cerror 


### <span id="R-MUFFLE-WARNING">重启动 MUFFLE-WARNING</span>

* 必要数据参数(Data Arguments Required):

None.

* 描述(Description):

This restart is established by warn so that handlers of warning conditions have a way to tell warn that a warning has already been dealt with and that no further action is warranted.

* 示例(Examples):

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

* 也见(See Also):

Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, muffle-warning (function), warn 


### <span id="R-STORE-VALUE">重启动 STORE-VALUE</span>

* 必要数据参数(Data Arguments Required):

a value to use instead (on an ongoing basis).

* 描述(Description):

The store-value restart is generally used by handlers trying to recover from errors of types such as cell-error or type-error, which may wish to supply a replacement datum to be stored permanently.

* 示例(Examples):

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

* 也见(See Also):

Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, store-value (function), ccase, check-type, ctypecase, use-value (function and restart) 


### <span id="R-USE-VALUE">重启动 USE-VALUE</span>

* 必要数据参数(Data Arguments Required):

a value to use instead (once).

* 描述(Description):

The use-value restart is generally used by handlers trying to recover from errors of types such as cell-error, where the handler may wish to supply a replacement datum for one-time use.

* 也见(See Also):

Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), invoke-restart, use-value (function), store-value (function and restart) 


### <span id="F-ABORT-CONTINUE-MW-SV-UV">函数 ABORT, CONTINUE, MUFFLE-WARNING, STORE-VALUE, USE-VALUE</span>

* 语法(Syntax):

abort &optional condition =>|

continue &optional condition => nil

muffle-warning &optional condition =>|

store-value value &optional condition => nil

use-value value &optional condition => nil

* 参数和值(Arguments and Values):

value---an object.

condition---a condition object, or nil.

* 描述(Description):

Transfers control to the most recently established applicable restart having the same name as the function. That is, the function abort searches for an applicable abort restart, the function continue searches for an applicable continue restart, and so on.

If no such restart exists, the functions continue, store-value, and use-value return nil, and the functions abort and muffle-warning signal an error of type control-error.

When condition is non-nil, only those restarts are considered that are either explicitly associated with that condition, or not associated with any condition; that is, the excluded restarts are those that are associated with a non-empty set of conditions of which the given condition is not an element. If condition is nil, all restarts are considered.

* 示例(Examples):

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


* 副作用(Side Effects):

A transfer of control may occur if an appropriate restart is available, or (in the case of the function abort or the function muffle-warning) execution may be stopped.

* 受此影响(Affected By):

Each of these functions can be affected by the presence of a restart having the same name.

* 异常情况(Exceptional Situations):

If an appropriate abort restart is not available for the function abort, or an appropriate muffle-warning restart is not available for the function muffle-warning, an error of type control-error is signaled.

* 也见(See Also):

invoke-restart, Section 9.1.4.2 (Restarts), Section 9.1.4.2.2 (重启动的接口), assert, ccase, cerror, check-type, ctypecase, use-value, warn

* 注意(Notes):

 (abort condition) ==  (invoke-restart 'abort)
 (muffle-warning)  ==  (invoke-restart 'muffle-warning)
 (continue)        ==  (let ((r (find-restart 'continue))) (if r (invoke-restart r)))
 (use-value x) ==  (let ((r (find-restart 'use-value))) (if r (invoke-restart r x)))
 (store-value x) ==  (let ((r (find-restart 'store-value))) (if r (invoke-restart r x)))

No functions defined in this specification are required to provide a use-value restart. 


