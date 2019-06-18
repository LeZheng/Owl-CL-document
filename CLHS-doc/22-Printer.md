# 22 打印器
<!--TODO 整章待校验-->

> * 22.1 [Lisp 打印器](#TheLispPrinter)
> * 22.2 [Lisp 美观打印器](#TheLispPrettyPrinter)
> * 22.3 [格式化输出](#FormattedOutput)
> * 22.4 [打印器的字典](#ThePrinterDictionary)


## 22.1 <span id="TheLispPrinter">Lisp 打印器</span>

> * 22.1.1 [Lisp 打印器概览](#OverviewTheLispPrinter)
> * 22.1.2 [打印器分派](#PrinterDispatching)
> * 22.1.3 [默认 Print-Object 方法](#DefaultPrintObjectMethods)
> * 22.1.4 [打印器行为的示例](#ExamplesPrinterBehavior)

### 22.1.1 <span id="OverviewTheLispPrinter">Lisp 打印器概览</span>

Common Lisp 以打印文本的形式提供了大多数对象[object]的表示形式, 称为打印表示. 像 print 这样的函数接受一个对象[object]并且把它的打印表示的那些字符发送给一个流[stream]. 做这些的例程集合被称为 (Common Lisp) 打印器.

读取一个打印表示通常产生一个和原始打印对象[object] equal 的对象[object].

#### 22.1.1.1 多种可能的文本表示

大部分对象[object]有着超过一个可能的文本表示. 比如, 一个大小为 27 的正整数[integer]在文本上可以被表示为这些方式的任意一种:

    27    27.    #o33    #x1B    #b11011    #.(* 3 3 3)    81/3

一个包含两个符号 A 和 B 的列表也在文本上被表示为好几种方式:

    (A B)    (a b)    (  a  b )    (\A |B|) 
    (|\A|
      B
    )

通常情况下, 从 Lisp 读取器[Lisp reader]的视角看, 在一个文本表示允许空白字符[whitespace]的地方, 任意数量的空格[space]和换行[newline]可以出现在标准语法[standard syntax]中.

当一个比如 print 这样的函数产生一个打印表示时, 它必须从几个可能的文本表示中选择. 在大部分情况下, 它选择一个程序可读的表示, 但在某些情况下, 它可能使用更紧凑的符号, 而不是程序可读的.

提供了许多被称之为打印器控制变量[printer control variable]的选项变量来允许控制对象[object]的打印表示的各个方面. 下一段中展示了标准[standardized]打印器控制变量[printer control variable]; 这里也可能存在具体实现定义[implementation-defined]的打印器控制变量[printer control variable].

    *print-array*   *print-gensym*       *print-pprint-dispatch*  
    *print-base*    *print-length*       *print-pretty*           
    *print-case*    *print-level*        *print-radix*            
    *print-circle*  *print-lines*        *print-readably*         
    *print-escape*  *print-miser-width*  *print-right-margin*     

    Figure 22-1. 标准打印器控制变量

除了打印器控制变量[printer control variable]之外, 下面附加的已定义名称[defined name]与 Lisp 打印器[Lisp printer]的行为相关或影响着 Lisp 打印器[Lisp printer]的行为:

    *package*                    *read-eval*  readtable-case  
    *read-default-float-format*  *readtable*                  

    Figure 22-2. 对 Lisp 打印器的额外影响.

##### 22.1.1.1.1 打印器转义

变量[variable] \*print-escape* 控制 Lisp 打印器[Lisp printer]是否尝试去产生诸如转义字符和包前缀这类的标记.

当程序可读的输出特别重要时, 变量[variable] \*print-readably* 可以被用于重写由其他打印器控制变量[printer control variable]控制的各个方面.

使 \*print-readably* 的值[value]为 true 的诸多影响中的一个是 Lisp 打印器[Lisp printer]表现地就好像 \*print-escape* 也是 true 一样. 为了表示方便, 我们说如果 \*print-readably* 或 \*print-escape* 任意一个值是 true, 那么打印器转义[printer escaping]就是 "启用的"; 并且我们说如果 \*print-readably* 和 \*print-escape* 的值都是 false, 那么打印器转义[printer escaping]就是 "禁用的". 

### 22.1.2 <span id="PrinterDispatching">打印器分派</span>

Lisp 打印器[Lisp printer]决定如何打印一个对象[object], 如下所示:

如果 \*print-pretty* 的值[value]是 true, 打印由当前美观打印分派表[current pprint dispatch table]控制; 见章节 22.2.1.4 (美观打印分派表).

否则 (如果 \*print-pretty* 的值[value]是 false), 使用对象的 print-object 方法; 见章节 22.1.3 (默认 Print-Object 方法). 

### 22.1.3 <span id="DefaultPrintObjectMethods">默认 Print-Object 方法</span>

这一章节描述了 对于标准[standardized]类型[type]的 print-object 方法的默认行为.

> * 22.1.3.1 [打印数字](#PrintingNumbers)
> * 22.1.3.2 [打印字符](#PrintingCharacters)
> * 22.1.3.3 [打印符号](#PrintingSymbols)
> * 22.1.3.4 [打印字符串](#PrintingStrings)
> * 22.1.3.5 [打印列表和构造(cons)](#PrintingListsConses)
> * 22.1.3.6 [打印位向量](#PrintingBitVectors)
> * 22.1.3.7 [打印其他向量](#PrintingOtherVectors)
> * 22.1.3.8 [打印其他数组](#PrintingOtherArrays)
> * 22.1.3.9 [打印数组的示例](#ExamplesPrintingArrays)
> * 22.1.3.10 [打印随机状态](#PrintingRandomStates)
> * 22.1.3.11 [打印路径名](#PrintingPathnames)
> * 22.1.3.12 [打印结构体](#PrintingStructures)
> * 22.1.3.13 [打印其他对象](#PrintingOtherObjects)

#### 22.1.3.1 <span id="PrintingNumbers">打印数字</span>

> * 22.1.3.1.1 [打印整数](#PrintingIntegers)
> * 22.1.3.1.2 [打印比率](#PrintingRatios)
> * 22.1.3.1.3 [打印浮点数](#PrintingFloats)
> * 22.1.3.1.4 [打印复数](#PrintingComplexes)
> * 22.1.3.1.5 [关于打印数字的注意事项](#NotePrintingNumbers)

##### 22.1.3.1.1 <span id="PrintingIntegers">打印整数</span>

整数[integer]是用当前输出基数[current output base]在位置符号中指定的基数来打印的, 最有效数字优先. 如果合适的话, 可以打印出一个基数说明符; 见 \*print-radix*. 如果一个整数[integer]是负的, 会打印一个负号和那个整数[integer]的绝对值. 整数[integer]零会被表示为单个数字 0 并且不会有符号. 一个小数点可能被打印出来, 取决于 \*print-radix* 的值[value].

关于一个整数[integer]的语法相关信息, 见章节 2.3.2.1.1 (一个整数的语法). 


##### 22.1.3.1.2 <span id="PrintingRatios">打印比率</span>

比率[ratio]按如下打印: 作为一个整数[integer]打印分子的绝对值; 然后是一个 /; 再是分母. 分子和分母都以当前输出基数[current output base]指定的基数打印; 它们就像是通过 numerator 和 denominator 获取到的, 所以比率[ration]以简化形式打印(最低项). 如果合适的话, 可以打印出一个基数说明符; 见 \*print-radix*. 如果这个比率是负的, 在分子前会打印一个负号.

关于一个比率[ratio]的语法相关信息, 见章节 2.3.2.1.2 (一个比率的语法). 


##### 22.1.3.1.3 <span id="PrintingFloats">打印浮点数</span>

如果这个浮点数的大小是零或在 10^-3 (包含的) 和 10^7 (不包含的) 之间, 它会被打印为这个数字的整数部分, 然后一个小数点, 再是这个数字的小数部分; 小数点的每一边都至少有一个数字. 如果这个数字的符号 (由 float-sign 确定) 是负的, 那么会在这个数字之前打印一个负号. 如果这个数字的格式不匹配由 \*read-default-float-format* 指定的格式, 那么也会打印出该格式的指数标记[exponent marker]和数字 0. 例如, 自然对数作为一个短浮点数[short float]可以被打印成 2.71828S0.

对于范围 10^-3 到 10^7 之外的非零大小, 一个浮点数[float]以计算机科学计数法打印的. 这个数字的表示被缩放到 1 (包含的) 和 10 (不包含的) 之间然后打印, 其中小数点前一位数, 小数点后至少有一个位. 接下来是打印这个格式的指数标记[exponent marker], 但是如果数字的格式与 \*read-default-float-format* 指定的格式相匹配, 那么就使用指数标记[exponent marker] E. 最后, 一个 10 的幂被打印成一个十进制整数, 这个小数和这个幂相乘的结果等于原始数字. 例如, 阿伏伽德罗数作为一个短浮点数[short float]会被打印为 6.02S23.

关于一个浮点数[float]的语法相关信息, 见章节 2.3.2.2 (浮点数的语法). 

##### 22.1.3.1.4 <span id="PrintingComplexes">打印复数</span>

一个复数[complex]被打印为一个 #C, 一个开圆括号, 它的实部的打印表示, 一个空格, 它的虚部的打印表示, 以及最后的闭圆括号.

关于一个复数[complex]语法的相关信息, 见章节 2.3.2.3 (复数的语法) 以及章节 2.4.8.11 (井号C(#C)). 


##### 22.1.3.1.5 <span id="NotePrintingNumbers">关于打印数字的注意事项</span>

一个数字的打印表示一定不能包含转义[escape]字符[character]; 见章节 2.3.1.1.1 (转义字符和潜在数字). 


#### 22.1.3.2 <span id="PrintingCharacters">打印字符</span>

当打印器转义[printer escaping]被禁用时, 一个字符[character]被打印为它的自身; 它被直接发送给输出流[stream]. 当打印器转义[printer escaping]被启用时, 那么使用 #\ 语法.

当打印器输出一个字符[character]的名字时, 它使用和 #\ 读取器宏[reader macro]相同的表; 因此输出的任何字符[character]作为输入都是可接受的 (在那个实现[implementation]中). 如果一个非图形化[non-graphic]字符[character]有着一个标准化[standardized]名字[name[5]], 这个名字[name]比非标准的名字更喜欢用 #\ 符号打印. 对于图形化[graphics]标准字符[standard character], 这个字符[character]自身被用于 #\ 标记打印---即便这个字符[character]也有一个名字[name[5]].

关于 #\ 读取器宏[reader macro]的详细信息, 见章节 2.4.8.1 (井号反斜线(#\\)). 

#### 22.1.3.3 <span id="PrintingSymbols">打印符号</span>

当打印器转义[printer escaping]被禁用时, 只有那个符号[symbol]名字[name]的字符会被输出 (但是打印名字[name]中的字符的大小写由 \*print-case* 控制; 见章节 22.1.3.3.2 (Lisp 打印器上读取表大小写的影响)).

章节 22.1.3.3 的其余部分仅在启用打印器转义[printer escaping]时才适用.

当打印一个符号[symbol]时, 打印器插入足够的单转义[single escape] 和/或 多转义[multiple escape]字符 (反斜线[backslash] 和/或 竖线[vertical-bar]) 这样一来如果用相同的 \*readtable* 以及绑定为当前输出基数[current output base]的 \*read-base* 来调用 read, 它会返回相同的符号[symbol] (如果它没有被显式解除捕捉[apparently uninterned]的话) 或者一个带有相同打印名的未捕捉[uninterned symbol]符号[symbol] (否则的话).

比如, 当打印符号 face 时如果 \*print-base* 的值[value]是 16, 它可能会被打印为 \FACE 或 \Face 或 |FACE|, 因为如果 \*read-base* 的值[value]是 16 那么记号 face 会被读取为一个十六进制数字 (数字值 64206).

对于当前读取表[current readtable]中具有非标准语法类型[syntax type]字符的附加限制, 见变量[variable] \*print-readably*

关于 Lisp 读取器[Lisp reader]如何解析符号[symbol]的信息, 见章节 2.3.4 (符号标记) 和章节 2.4.8.5 (井号冒号(#:)).

当 \*print-pretty* 是 true 并且打印器转义[printer escaping]是启用时, nil 可能被打印为 () .

> * 22.1.3.3.1 [符号的包前缀](#PackagePrefixesSymbols)
> * 22.1.3.3.2 [Lisp 打印器上读取表大小写的影响](#EffectReadtableCase)

##### 22.1.3.3.1 <span id="PackagePrefixesSymbols">符号的包前缀</span>

如果必要的话, 包前缀[package prefix]会被打印. 包前缀[package prefix]的规则如下. 当这个符号[symbol]被打印时, 如果它在 KEYWORD 包, 那么它和一个前置的冒号[colon]一起被打印; 否则, 如果它在当前包[current package]中可访问[accessible], 它被打印为不带包前缀[package prefix]的; 否则, 它会被打印为带有一个包前缀[package prefix]的.

如果 \*print-gensym* 是 true 并且打印器转义[printer escaping]是启用的, 那么一个被显式解除绑定[apparently uninterned]的符号[symbol]会以 "#:" 前置的方式被打印; 如果 \*print-gensym* 是 false 或打印器转义[printer escaping]被禁用, 那么符号[symbol]会以不带前缀的方式打印, 就好像它是在当前包[current package]中的.

由于 #: 语法不会捕捉后面的符号, 如果 \*print-circle* 是 true 并且相同的未捕捉的符号在一个要被打印的表达式中出现多次, 有必要去使用循环列表的语法. 比如, 如果 \*print-circle*  是 false, 下面这个的结果

    (let ((x (make-symbol "FOO"))) (list x x))

会被打印为 (#:foo #:foo), 如果 \*print-circle* 是 true, 那么就是 (#1=#:foo #1#).

前面的包前缀规则的总结如下:

foo:bar

    当符号[symbol] bar 在它的 home 包[home package] foo 中是外部的并且在当前包[current package]中不是可访问[accessible]的时候, 打印 foo:bar.

foo::bar

    当 bar 在它的 home 包[home package] foo 中是内部的并且在当前包[current package]中不是可访问[accessible]时, 打印 foo::bar.

:bar

    当 bar 的 home 包是 KEYWORD 包时, 打印 :bar.

#:bar

    当 bar 被显式解除绑定[apparently uninterned]时都会打印为 #:bar, 即使在 bar 没有 home 包[home package]但在当前包[current package]中却是可访问[accessible]的病态情况下. 


##### 22.1.3.3.2 <span id="EffectReadtableCase">Lisp 打印器上读取表大小写的影响</span>

当打印器转义[printer escaping]被禁用, 或正在考虑的字符还没有被单转义[single escape]或多转义[multiple escape]语法引用时, 当前读取表[current readtable]的读取表大小写[readtable case]以以下方式影响 Lisp 打印器[Lisp printer]写入符号[symbol]的方式:

:upcase

    当读取表大小写[readtable case]是 :upcase 时, 大写[uppercase]字符[character]由 *print-case* 指定的大小写来打印, 而小写[lowercase]字符[character]按照它们自身的大小写打印.

:downcase

    当读取表大小写[readtable case]是 :downcase 时, 大写[uppercase]字符[character]按照它们自身的大小写打印, 而小写[lowercase]字符[character]由 *print-case* 指定的大小写来打印.

:preserve

    当读取表大小写[readtable case]是 :preserve 时, 所有字母[alphabetic]字符[character]都按照它们自身的大小写打印.

:invert

    当读取表大小写[readtable case]是 :invert 时, 单一大小写符号名中的所有字母[alphabetic]字符[character]的大小写会被转换. 混合大小写符号名会保留原样打印.

如果启用了打印器转义[printer escaping], 那么符号名中的转义字母[alphabetic]字符[character]的规则受 readtable-case 影响. 字母[alphabetic]字符[character]按如下转义:

:upcase

    当读取表大小写[readtable case]是 :upcase 时, 所有小写[lowercase]字符[character]必须被转义.

:downcase

    当读取表大小写[readtable case]是 :downcase 时, 所有大写[uppercase]字符[character]必须被转义.

:preserve

    当读取表大小写[readtable case]是 :preserve 时, 没有字母[alphabetic]字符[character]需要被转义.

:invert

    当读取表大小写[readtable case]是 :invert 时, 没有字母[alphabetic]字符[character]需要被转义.


###### 22.1.3.3.2.1 Examples of Lisp 打印器上读取表大小写的影响

    ```LISP
    (defun test-readtable-case-printing ()
      (let ((*readtable* (copy-readtable nil))
            (*print-case* *print-case*))
        (format t "READTABLE-CASE *PRINT-CASE*  Symbol-name  Output~
                  ~%--------------------------------------------------~
                  ~%")
        (dolist (readtable-case '(:upcase :downcase :preserve :invert))
          (setf (readtable-case *readtable*) readtable-case)
          (dolist (print-case '(:upcase :downcase :capitalize))
            (dolist (symbol '(|ZEBRA| |Zebra| |zebra|))
              (setq *print-case* print-case)
              (format t "~&:~A~15T:~A~29T~A~42T~A"
                      (string-upcase readtable-case)
                      (string-upcase print-case)
                      (symbol-name symbol)
                      (prin1-to-string symbol)))))))
    ```

来自 (test-readtable-case-printing) 的输出应该如下:

    READTABLE-CASE *PRINT-CASE*  Symbol-name  Output
    --------------------------------------------------
    :UPCASE        :UPCASE       ZEBRA        ZEBRA
    :UPCASE        :UPCASE       Zebra        |Zebra|
    :UPCASE        :UPCASE       zebra        |zebra|
    :UPCASE        :DOWNCASE     ZEBRA        zebra
    :UPCASE        :DOWNCASE     Zebra        |Zebra|
    :UPCASE        :DOWNCASE     zebra        |zebra|
    :UPCASE        :CAPITALIZE   ZEBRA        Zebra
    :UPCASE        :CAPITALIZE   Zebra        |Zebra|
    :UPCASE        :CAPITALIZE   zebra        |zebra|
    :DOWNCASE      :UPCASE       ZEBRA        |ZEBRA|
    :DOWNCASE      :UPCASE       Zebra        |Zebra|
    :DOWNCASE      :UPCASE       zebra        ZEBRA
    :DOWNCASE      :DOWNCASE     ZEBRA        |ZEBRA|
    :DOWNCASE      :DOWNCASE     Zebra        |Zebra|
    :DOWNCASE      :DOWNCASE     zebra        zebra
    :DOWNCASE      :CAPITALIZE   ZEBRA        |ZEBRA|
    :DOWNCASE      :CAPITALIZE   Zebra        |Zebra|
    :DOWNCASE      :CAPITALIZE   zebra        Zebra
    :PRESERVE      :UPCASE       ZEBRA        ZEBRA
    :PRESERVE      :UPCASE       Zebra        Zebra
    :PRESERVE      :UPCASE       zebra        zebra
    :PRESERVE      :DOWNCASE     ZEBRA        ZEBRA
    :PRESERVE      :DOWNCASE     Zebra        Zebra
    :PRESERVE      :DOWNCASE     zebra        zebra
    :PRESERVE      :CAPITALIZE   ZEBRA        ZEBRA
    :PRESERVE      :CAPITALIZE   Zebra        Zebra
    :PRESERVE      :CAPITALIZE   zebra        zebra
    :INVERT        :UPCASE       ZEBRA        zebra
    :INVERT        :UPCASE       Zebra        Zebra
    :INVERT        :UPCASE       zebra        ZEBRA
    :INVERT        :DOWNCASE     ZEBRA        zebra
    :INVERT        :DOWNCASE     Zebra        Zebra
    :INVERT        :DOWNCASE     zebra        ZEBRA
    :INVERT        :CAPITALIZE   ZEBRA        zebra
    :INVERT        :CAPITALIZE   Zebra        Zebra
    :INVERT        :CAPITALIZE   zebra        ZEBRA


#### 22.1.3.4 <span id="PrintingStrings">打印字符串</span>

字符串[string]中的字符会被依次输出. 如果启用了打印器转义[printer escaping], 一个双引号[double-quote]会在之前或之后被输出, 并且所有的双引号[double-quote]和单转义[single escape]前面会有一个反斜线[backslash]. 字符串[string]的打印不受 \*print-array* 的影响. 只有字符串[string]中的有效[active]元素[element]会被打印.

关于 Lisp 读取器[Lisp reader]如何解析字符串[string]的信息, 见章节 2.4.5 (双引号). 

#### 22.1.3.5 <span id="PrintingListsConses">打印列表和构造(cons)</span>

只要有可能, 列表记号优先于点记号. 因此以下运算法则被用于打印一个 cons x:

1. 打印一个左圆括号[left-parenthesis].

2. 打印 x 的 car.

3. 如果 x 的 cdr 自身是一个 cons, 它会成为当前的 cons (换句话说, x 变为那个 cons), 打印一个空格[space], 然后再次进入步骤 2.

4. 如果 x 的 cdr 不为空[null], 打印一个空格[space], 一个点[dot], 一个空格[space], 以及 x 的 cdr.

5. 打印一个右圆括号[right-parenthesis].

事实上, 只有当 \*print-pretty* 是 false 时, 使用以上运算法则. 当 \*print-pretty* 是 true 时(或者当使用 pprint 时), 额外的空格[whitespace[1]]可能替换一个单空格[space]的使用, 并且使用一种更复杂的、具有类似的目标但具有更多的表示灵活性的算法; 见章节 22.1.2 (打印器分派).

虽然以下两个表达式是等价的, 并且读取器接收任意一个并打印相同的 cons, 但是打印器总是用第二种形式来打印这样一个 cons.

    (a . (b . ((c . (d . nil)) . (e . nil))))
    (a b (c d) e)

cons 的打印受 \*print-level*, \*print-length*, 和 \*print-circle* 的影响.

下面是列表的打印表示的示例:

    (a . b)     ;A dotted pair of a and b
    (a.b)       ;A list of one element, the symbol named a.b
    (a. b)      ;A list of two elements a. and b
    (a .b)      ;A list of two elements a and .b
    (a b . c)   ;A dotted list of a and b with c at the end; two conses
    .iot        ;The symbol whose name is .iot
    (. b)       ;Invalid -- an error is signaled if an attempt is made to read 
                ;this syntax.
    (a .)       ;Invalid -- an error is signaled.
    (a .. b)    ;Invalid -- an error is signaled.
    (a . . b)   ;Invalid -- an error is signaled.
    (a b c ...) ;Invalid -- an error is signaled.
    (a \. b)    ;A list of three elements a, ., and b
    (a |.| b)   ;A list of three elements a, ., and b
    (a \... b)  ;A list of three elements a, ..., and b
    (a |...| b) ;A list of three elements a, ..., and b

关于 Lisp 读取器[Lisp reader]如何解析列表[list]和 cons 的信息, 见章节 2.4.1 (左圆括号). 

#### 22.1.3.6 <span id="PrintingBitVectors">打印位向量</span>

一个位向量[bit vector]被打印为 #* 后面依次跟着位向量[bit vector]的各个位. 如果 \*print-array* 是 false, 那么这个位向量[bit vector]以一种简洁但是不可读取的格式 (使用 #<) 来打印. 只有位向量[bit vector]中的有效[active]元素[element]会被打印.

关于 Lisp 读取器[Lisp reader]如何解析位向量[bit vector]的信息, 见章节 2.4.8.4 (井号星号(#*)). 


#### 22.1.3.7 <span id="PrintingOtherVectors">打印其他向量</span>

如果 \*print-array* 是 true 并且 \*print-readably* 是 false, 除了字符串[string]和位向量[bit vector]以外的向量[vector]会用普通向量语法来打印; 这意味着关于特殊向量表示的信息不会出现. 零长度向量[vector]的打印表示是 #(). 非零长度的向量[vector]用 #( 开始打印. 在那个后面, 向量[vector]的第一个元素会被打印. 如果这里有任何其他元素, 它们会依次被打印, 如果 \*print-pretty* 是 false 那么每一个额外的元素前面会有一个空格[space], 如果 \*print-pretty* 是 true 那么就是空格[whitespace[1]]. 最后一个元素后的右圆括号[right-parenthesis]终止这个向量[vector]的打印表示. 向量[vector]的打印受 \*print-level* 和 \*print-length* 的影响. 如果这个向量[vector]有着填充指针[fill pointer], 那么只有那些在填充指针[fill pointer]下的元素会被打印.

如果 \*print-array* 和 \*print-readably* 都是 false, 向量[vector]不会按如上所述被打印, 而是以一种简洁但不可读的格式 (使用 #<) 来打印.

如果 \*print-readably* 是 true, 那么向量[vector]以一种具体实现定义[implementation-defined]的方式来打印; 见变量[variable] \*print-readably*.

关于 Lisp 读取器[Lisp reader]如何解析"其他变量[vector]"的信息, 见章节 2.4.8.3 (井号左括号(#()). 

#### 22.1.3.8 <span id="PrintingOtherArrays">打印其他数组</span>

如果 \*print-array* 是 true 并且 \*print-readably* 是 false, 除了向量[vector]以外的任何数组[array]都用 #nA 格式打印. 这里 n 为这个数组[array]的秩[rank]. 接着打印 #, 再是十进制整数 n, 然后是 A, 然后 n 个开括号. 接下来这些元素[element]按照行优先的顺序被扫描, 在每一个元素[element]上使用 write, 并且元素[element]之间用空格[whitespace[1]]分隔. 数组的维度从左到右被计为 0 到 n-1, 并以最右的索引变化最快的方式枚举. 每次维度 j 的索引递增, 就会采取以下动作:

* 如果 j < n-1, 那么打印一个闭圆括号.

* 如果递增维度 j 的索引导致它和维度 j 相等, 那个索引被重置为零并且维度 j-1 的索引会递增 (因此递归执行这三个步骤), 除非 j=0, 在这个情况下终止整个运算法则. 如果递增维度 j 的索引没有导致它等于维度 j, 那么打印一个空格.

* 如果 j < n-1, 打印一个开圆括号.

这个导致要被打印的内容以一种适合于给 make-array 的 :initial-contents 参数的格式来打印. 这个过程有效打印的列表可能会被 \*print-level* 和 \*print-length* 截断.

如果这个数组[array]是特定类型[type]的, 包含位或者字符, 那么由上述运算法则生成的最内部的列表可以用位向量或字符串的语法来打印, 假定这些最内部的列表不会被 \*print-length* 截断.

如果 \*print-array* 和 \*print-readably* 都是 false, 那么数组[array]以一种简洁但不可读的方式 (使用 #<) 打印.

如果 \*print-readably* 是 true, 这个数组[array]以一种具体实现定义[implementation-defined]的方式打印; 见变量[variable] \*print-readably*. 具体来说, 这对于 0 维数组来说是很重要的.

关于 Lisp 读取器[Lisp reader]如何解析这些"其他数组[array]"的信息, 见章节 2.4.8.12 (井号A(#A)).


#### 22.1.3.9 <span id="ExamplesPrintingArrays">打印数组的示例</span>

    ```LISP
    (let ((a (make-array '(3 3)))
          (*print-pretty* t)
          (*print-array* t))
      (dotimes (i 3) (dotimes (j 3) (setf (aref a i j) (format nil "<~D,~D>" i j))))
      (print a)
      (print (make-array 9 :displaced-to a)))
    >>  #2A(("<0,0>" "<0,1>" "<0,2>") 
    >>      ("<1,0>" "<1,1>" "<1,2>") 
    >>      ("<2,0>" "<2,1>" "<2,2>")) 
    >>  #("<0,0>" "<0,1>" "<0,2>" "<1,0>" "<1,1>" "<1,2>" "<2,0>" "<2,1>" "<2,2>") 
    =>  #<ARRAY 9 indirect 36363476>
    ```

#### 22.1.3.10 <span id="PrintingRandomStates">打印随机状态</span>

没有为打印 random-state 类型[type]的对象[object]指定具体的语法. 然而, 每个具体实现[implementation]必须以这样一种方式来安排打印随机状态[random state]对象[object], 在相同实现中, read 可以从这个打印表示来构造这个随机状态[random state]的一个拷贝, 就好像这个拷贝是通过 make-random-state 创建的一样.

如果这个随机状态[random state]类型实际上是通过defstruct 来有效实现的, 那么常见的结构体语法可以被用于打印随机状态[random state]对象; 一种打印可能看上去像下面这样

    #S(RANDOM-STATE :DATA #(14 49 98436589 786345 8734658324 ... ))

其中的成员是依赖于具体实现的[implementation-dependent]. 


#### 22.1.3.11 <span id="PrintingPathnames">打印路径名</span>

当启用打印器转义[printer escaping]时, 语法 #P"..." 是路径名[pathname]如何被 write 和这里描述的函数打印的语法. 这个 "..." 这个路径名的名称字符串表示.

当禁用打印器转义[printer escaping]时, write 通过写入 (namestring P) 来写入一个路径名[pathname] P.

关于 Lisp 读取器[Lisp reader]如何解析路径名[pathname]的信息, 见章节 2.4.8.14 (井号P(#P)). 


#### 22.1.3.12 <span id="PrintingStructures">打印结构体</span>

默认情况下, 一个类型 S 的结构体[structure]使用 #S 语法打印. 这个行为可以通过给定义 S 的 defstruct 表达式形式[form]指定一个 :print-function 或 :print-object 选项, 或者通过写一个为 S 类型的对象[object]特化[specialized]的 print-object 方法来定制.

不同的结构体可能以不同的方式打印; 结构体的默认表示法是:

    #S(structure-name {slot-key slot-value}*)

其中 #S 表示结构体语法, structure-name 是一个结构体名字[structure name], 每一个 slot-key 是这个结构体[structure]中一个槽[slot]的初始化参数的名字[name], 而每一个对应的 slot-value 是那个槽[slot]中的对象[object]的一个表示.

关于 Lisp 读取器[Lisp reader]如何解析结构体[structure]的信息, 见章节 2.4.8.13 (井号S(#S)). 


#### 22.1.3.13 <span id="PrintingOtherObjects">打印其他对象</span>

其他对象[object]以一种依赖于具体实现[implementation-dependent]的方式被打印. 一个具体实现[implementation]以可读[readably]的方式打印那些对象[object]不是必须的.

比如, 哈系表[hash table], 读取表[readtable], 包[package], 流[stream], 和函数[function]可能不会以可读[readably]的方式打印.

在这个情况中使用的普遍表示是 #<...>. 由于 #< 不是通过 Lisp 读取器[Lisp reader]可读的, 它遵循的文本准确格式是不重要的, 但是要使用的一个普遍格式是由 print-unreadable-object 宏[macro]提供的.

关于 Lisp 读取器[Lisp reader]如何对待这种表示的信息, 见章节 2.4.8.20 (井号小于号(#<)). 关于如果表示不能被可读[readably]地打印的对象[object]的信息, 见章节 2.4.8.6 (井号点(#.)).

### 22.1.4 <span id="ExamplesPrinterBehavior">打印器行为的示例</span>

    ```LISP
    (let ((*print-escape* t)) (fresh-line) (write #\a))
    >>  #\a
    =>  #\a
    (let ((*print-escape* nil) (*print-readably* nil))
      (fresh-line)
      (write #\a))
    >>  a
    =>  #\a
    (progn (fresh-line) (prin1 #\a))
    >>  #\a
    =>  #\a
    (progn (fresh-line) (print #\a))
    >>  
    >>  #\a
    =>  #\a
    (progn (fresh-line) (princ #\a))
    >>  a
    =>  #\a

    (dolist (val '(t nil))
      (let ((*print-escape* val) (*print-readably* val))
        (print '#\a) 
        (prin1 #\a) (write-char #\Space)
        (princ #\a) (write-char #\Space)
        (write #\a)))
    >>  #\a #\a a #\a
    >>  #\a #\a a a
    =>  NIL

    (progn (fresh-line) (write '(let ((a 1) (b 2)) (+ a b))))
    >>  (LET ((A 1) (B 2)) (+ A B))
    =>  (LET ((A 1) (B 2)) (+ A B))

    (progn (fresh-line) (pprint '(let ((a 1) (b 2)) (+ a b))))
    >>  (LET ((A 1)
    >>        (B 2))               
    >>    (+ A B))
    =>  (LET ((A 1) (B 2)) (+ A B))

    (progn (fresh-line) 
            (write '(let ((a 1) (b 2)) (+ a b)) :pretty t))
    >>  (LET ((A 1)
    >>        (B 2))
    >>    (+ A B))                 
    =>  (LET ((A 1) (B 2)) (+ A B))

    (with-output-to-string (s)  
        (write 'write :stream s)
        (prin1 'prin1 s))
    =>  "WRITEPRIN1"
    ```

## 22.2 <span id="TheLispPrettyPrinter">Lisp 美观打印器</span>

> * 22.2.1 [美观打印器的概念](#PrettyPrinterConcepts)
> * 22.2.2 [使用美观打印器的示例](#ExamplesPrettyPrinter)
> * 22.2.3 [美观打印器背景的注意事项](#NotesPrettyPrinterBackground)

### 22.2.1 <span id="PrettyPrinterConcepts">美观打印器的概念</span>

美观打印器[pretty printer]提供的工具允许程序[program]重新定义代码[code]显示的方式, 并且允许美观打印的全部能力应用于复杂的数据结构组合.

任何给定的输出样式实际上是否是"美观"的, 本质上是一个主观的问题. 然而, 由于美观打印器[pretty printer]的效果可以由符合标准的程序[conforming program]进行定制, 所以为单个程序[program]提供了必要的灵活性以达到任意程度的审美控制.

通过直接访问美观打印器内的机制来对布局做出动态决策, 这些 pprint-logical-block, pprint-newline, 和 pprint-indent 宏和函数可以为任何产生输出的函数指定美观打印布局规则. 它们也使得环状和共享的检测以及要被函数支持的基于长度和嵌套深度的缩写变得简单.

美观打印器[pretty printer]完全是根据 \*print-pprint-dispatch* 的值[value]来驱动的. 函数[function] set-pprint-dispatch 使得符合规范的程序[conforming program]去关联美观打印函数和一个类型[type]是可能的.

> * 22.2.1.1 [输出排列的动态控制](#DynamicControlArrangeOutput)
> * 22.2.1.2 [格式化指令接口](#FormatDirectiveInterface)
> * 22.2.1.3 [编译格式化字符串](#CompilingFormatStrings)
> * 22.2.1.4 [美观打印分派表](#PrettyPrintDispatchTables)
> * 22.2.1.5 [美观打印器边距](#PrettyPrinterMargins)

#### 22.2.1.1 <span id="DynamicControlArrangeOutput">输出排列的动态控制</span>
<!--TODO 所有输出到, 但不包括 ??-->
当一个输出[pretty printer]太大而无法容纳到可用空间中时, 可以精确地控制美观打印器的操作. 三个概念是这些操作工作的基础---逻辑块[logical blocks], 条件换行[conditional newlines], 以及节段[sections]. 在继续之前, 定义这些术语是很重要的.

下一段中的第一行展示了输出的示意图片段. 输出的每个字符都用 "-" 表示. 条件换行的位置由数字表示. 这个逻辑块的开始和结束分别由 "<" 和 ">" 表示.

输出作为一个整体是一个逻辑块和最外层节段. 这节段由 Figure 1 的第二行中的 0 表示. 嵌套在这个输出中的逻辑块通过宏 pprint-logical-block 来指定. 条件换行的位置由 pprint-newline 调用来指定. 每个条件换行定义了两个节段 (一个在它之前, 一个在它之后) 并且和第三个关联 (直接包含它的那个节段).

一个条件换行后面的节段由这些组成: 所有输出到, 但不包括, (a) 在同一个逻辑块中直接包含的下一个条件换行; 或者如果 (a) 是不可应用的, (b) 嵌套在逻辑块的更低级别的下一个换行; 或者如果 (b) 是不可应用的, (c) 输出的末尾.

一个条件换行前面的节段由这些组成: 所有的输出都返回到, 但不包括, (a) 被直接包含在相同逻辑块中的前面的条件换行; 或者如果 (a) 不可应用, (b) 直接包含逻辑块的开头. Figure 1 中的最后四行表示四个条件换行前后的节段.

直接包含一个条件换行的节段是包含这个条件换行节段中最短的那个. 在下一段中, 第一个条件换行被直接包含在 0 标记的节段中, 第二个和第三个条件换行被直接包含在第四个条件换行之前的节段中, 并且第四个条件换行被直接包含在第一个条件换行之后的节段中.

    <-1---<--<--2---3->--4-->->
    000000000000000000000000000
    11 111111111111111111111111
              22 222
                  333 3333
            44444444444444 44444

    Figure 22-3. 逻辑块, 条件换行, 和节段的示例

只要有可能, 美观打印器在一行中显示一个节段的完整内容. 然而, 如果该节段太长, 无法容纳在可用空间, 则在该节段内的条件换行位置插入换行符. 


#### 22.2.1.2 <span id="FormatDirectiveInterface">格式化指令接口</span>

动态确定输出排列的操作的主要接口是通过美观打印器的函数和宏提供的. 下一段展示了和美观打印相关的已定义的名字.

    *print-lines*            pprint-dispatch                pprint-pop           
    *print-miser-width*      pprint-exit-if-list-exhausted  pprint-tab           
    *print-pprint-dispatch*  pprint-fill                    pprint-tabular       
    *print-right-margin*     pprint-indent                  set-pprint-dispatch  
    copy-pprint-dispatch     pprint-linear                  write                
    format                   pprint-logical-block                                
    formatter                pprint-newline                                      

    Figure 22-4. 和美观打印相关的已定义的名字.

下一个段标识了一组格式化指令[format directive], 它们以更简洁的形式作为相同的美观打印操作的替代接口.

    ~I   ~W      ~<...~:>  
    ~:T  ~/.../  ~_        

    Figure 22-5. 和美观打印相关的格式化指令


#### 22.2.1.3 <span id="CompilingFormatStrings">编译格式化字符串</span>

格式化字符串[format string]本质上是一个用于执行打印的特殊目的语言的程序, 并且可以通过函数[function] format 来解释. formatter 宏[macro]提供了使用编译后的函数[compiled function]来完成相同的打印的效率, 但是不会丢失格式化字符串[format string]的文本紧凑性.

一个格式化控制[format control]是一个格式化字符串[format string]或一个由 formatter 宏[macro]返回的函数[function]. 


#### 22.2.1.4 <span id="PrettyPrintDispatchTables">美观打印分派表</span>

一个美观打印分派表[pprint dispatch table]是一个从键到值对的映射. 每一个键是一个类型指定符[type specifier]. 和一个键关联的值是一个 "函数" (具体的说, 一个函数标识符[function designator]或 nil) 和一个 "优先级数值" (具体的说, 一个 real). 基本的插入和检索是基于键来进行的, 键的等价性由 equal 测试.

当 \*print-pretty* 是 true 时, 当前美观打印分派表[current pprint dispatch table] (在 \*print-pprint-dispatch* 中) 控制对象[object]如何被打印. 这个表中的信息优先于指定如何打印对象[object]的所有其他机制. 特别是, 它的优先级超过用户定义的 print-object 方法[method], 因为优先考虑这个当前美观打印分派表[current pprint dispatch table].

通过在当前美观打印分派表[current pprint dispatch table]中查找和那个对象[object]匹配的类型指定符[type specifier]相关联的最高优先级的函数来选择这个函数; 如果这里有不止一个这样的函数, 哪一个被使用是依赖于具体实现的[implementation-dependent].

然而, 如果在这个表中没有关于如何美观打印[pretty print]一个特定种类对象[object]的信息, 就会调用一个使用 print-object 来打印这个对象[object]的函数[function]. 当这个函数被调用时, \*print-pretty* 的值始终为 true, 并且对于 print-object 的各个方法仍然可以选择根据 \*print-pretty* 的值[value]产生特殊格式的输出. 


#### 22.2.1.5 <span id="PrettyPrinterMargins">美观打印器边距</span>

美观打印的主要目标是将输出保持在两个边距之间. 输出开始的列取自左边距. 如果当前列不能在输出开始时确定, 这个左边距被假定为零. 右边距由 \*print-right-margin* 控制. 


### 22.2.2 <span id="ExamplesPrettyPrinter">使用美观打印器的示例</span>

作为逻辑块, 条件换行, 和缩进相互作用的一个例子, 细想下面这个函数 simple-pprint-defun. 这个函数用标准的方式打印 car 为 defun 的列表, 假定这些列表的长度都是 4.

```LISP
(defun simple-pprint-defun (*standard-output* list)
  (pprint-logical-block (*standard-output* list :prefix "(" :suffix ")")
    (write (first list))
    (write-char #\Space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (write (second list))
    (write-char #\Space)
    (pprint-newline :fill)
    (write (third list))
    (pprint-indent :block 1)
    (write-char #\Space)
    (pprint-newline :linear)
    (write (fourth list))))
```

假设它按照如下求值:

```LISP
(simple-pprint-defun *standard-output* '(defun prod (x y) (* x y)))
```

如果可用的行宽大于等于 26, 那么所有输出会出现在一行中. 如果可用的行宽减少到 25, 在表达式[expression] (* x y) 之前的线性条件换行符处插入一个换行符, 生成如下所示的输出. 这个 (pprint-indent :block 1) 导致 (* x y) 以逻辑块中相对缩进 1 的位置打印.

```LISP
(DEFUN PROD (X Y) 
  (* X Y))
```

如果可用的行宽是 15, 在参数列表前的填充样式条件换行符处也插入换行符. 这个 (pprint-indent :current 0) 的调用导致参数列表排列在函数名字下.

```LISP
(DEFUN PROD
      (X Y)
  (* X Y))
```

如果 \*print-miser-width* 大于等于 14, 上面输出示例会是如下这样, 因为在最小执行常式(miser)模式下所有缩进的改变会被忽略并且换行符被插入到最小执行常式的条件换行中.

```LISP
(DEFUN
  PROD
  (X Y)
  (* X Y))
```

作为每行前缀的一个例子, 细想行宽为 20 并且 \*print-miser-width* 是 nil 的情况下求值下面这些产生的输出.

```LISP
(pprint-logical-block (*standard-output* nil :per-line-prefix ";;; ")
  (simple-pprint-defun *standard-output* '(defun prod (x y) (* x y))))

;;; (DEFUN PROD
;;;        (X Y)
;;;   (* X Y))
```

作为一个更加复杂 (并且真实) 示例, 细想下面的函数 pprint-let. 这个指定如何去用传统风格打印一个 let 表达式形式[form]. 它比上面的例子更复杂, 因为它不得不去处理嵌套的结构. 同样的, 不像上面的例子, 它包含去可读地打印任何以符号[symbol] let 开始的可能的列表的完整代码. 最外部的 pprint-logical-block 表达式形式[form]处理输入列表作为整体的打印并且指定应该在输出中打印的圆括号. 第二个 pprint-logical-block 表达式形式[form]处理绑定对的列表. 在列表中的每一对自身通过最内部的 pprint-logical-block 打印. (使用 loop 表达式形式[form], 而不是将这个对分解为两个对象[object], 这样一来不管这个对对应的列表有着一个元素, 两个元素, 或(难看的)不止两个元素都会产生可读的输出). 会在除了最后一个之外的每一个对后面放置一个空格和一个填充风格的条件换行. 在最顶层 pprint-logical-block 表达式形式[form]末尾的循环打印在这个 let 表达式形式[form]的主体中被空格和线性风格条件换行分隔的表达式形式.

```LISP
(defun pprint-let (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\Space)
    (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
              (pprint-exit-if-list-exhausted)
              (loop (write (pprint-pop))
                    (pprint-exit-if-list-exhausted)
                    (write-char #\Space)
                    (pprint-newline :linear)))
            (pprint-exit-if-list-exhausted)
            (write-char #\Space)
            (pprint-newline :fill)))
    (pprint-indent :block 1)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space)
          (pprint-newline :linear)
          (write (pprint-pop)))))
```

细想在 \*print-level* 是 4, 并且 \*print-circle* 是 true 的情况下求值下名这个.

```LISP
(pprint-let *standard-output*
            '#1=(let (x (*print-length* (f (g 3))) 
                      (z . 2) (k (car y)))
                  (setq x (sqrt z)) #1#))
```

如果行宽大于等于 77, 所产生的输出出现在一行中. 然而, 如果行宽是 76, 换行符会被插入在分隔这些表达式形式的线性风格条件换行中并且产生下面这样的输出. 注意, 退化绑定对 x 即使不是一个列表也可以打印出来; 一个深度缩写标记会被打印来替换 (g 3); 绑定对 (z . 2) 即便它不是一个 proper 列表也可以打印出来; 并打印缩写的环状标记.

```LISP
#1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) 
      (SETQ X (SQRT Z))
      #1#)
```

如果行宽被减少到 35, 一个换行符会被插入到这些分隔绑定对的填充风格条件换行中的一个.

```LISP
#1=(LET (X (*PRINT-PRETTY* (F #))
          (Z . 2) (K (CAR Y)))
      (SETQ X (SQRT Z))
      #1#)
```

假设这个行宽进一步减少到 22 并且 \*print-length* 被设置为 3. 在这个情况中, 换行符被插入到第一个和第二个绑定对后面. 另外, 第二个绑定对自身被断开为两行. 填充风格条件换行的描述中的子句 (b) (见函数[function] pprint-newline) 避免了绑定对 (z . 2) 被打印在第三行末尾. 注意, 长度缩写从视图上隐藏了环并且因此环标记的打印消失了.

```LISP
(LET (X
      (*PRINT-LENGTH*
        (F #))
      (Z . 2) ...)
  (SETQ X (SQRT Z))
  ...)
```

下面这个函数使用 "#(...)" 标识打印了一个向量.

```LISP
(defun pprint-vector (*standard-output* v)
  (pprint-logical-block (nil nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
        (loop (pprint-pop)
              (write (aref v i))
              (if (= (incf i) end) (return nil))
              (write-char #\Space)
              (pprint-newline :fill))))))
```

在行宽为 15 时求值下面这个的输出如下.

```LISP
(pprint-vector *standard-output* '#(12 34 567 8 9012 34 567 89 0 1 23))

#(12 34 567 8 
  9012 34 567 
  89 0 1 23)
```

作为用格式化字符串[format string]指定美观打印的示例, 细想上面被用作示例的函数 simple-pprint-defun 和 pprint-let 可以按如下简洁地定义. (函数 pprint-vector 不能使用 format 来定义, 因为遍历的数据结构不是一个列表.)

```LISP
(defun simple-pprint-defun (*standard-output* list)
  (format T "~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>" list))

(defun pprint-let (*standard-output* list)
  (format T "~:<~W~^~:<~@{~:<~@{~W~^~_~}~:>~^~:_~}~:>~1I~@{~^~_~W~}~:>" list)) 
```

在下面的示例中, 第一个表达式形式[form]把 \*print-pprint-dispatch* 复原为它的等价初始值. 后面两个表达式形式为美观打印比率设置了一种特殊的方式. 注意, 更具体的类型指定符[type specifier]会和更高的优先级关联.

```LISP
(setq *print-pprint-dispatch* (copy-pprint-dispatch nil))

(set-pprint-dispatch 'ratio
  #'(lambda (s obj)
      (format s "#.(/ ~W ~W)" 
                (numerator obj) (denominator obj))))

(set-pprint-dispatch '(and ratio (satisfies minusp))
  #'(lambda (s obj)
      (format s "#.(- (/ ~W ~W))" 
              (- (numerator obj)) (denominator obj)))
  5)

(pprint '(1/3 -2/3))
(#.(/ 1 3) #.(- (/ 2 3)))
```

下面两个表达式形式[form]阐述了关于代码[code]类型的美观打印函数的定义. 第一个表达式形式[form]阐述了如何为使用单引号[single-quote]引用的对象指定传统方法. 注意, 确保以 quote 开头的数据列表也可以被打印出来. 第二个表达式形式指定了当初始美观打印分派表[pprint dispatch table]生效时以符号 my-let 开始的列表应该和 let 开始的列表打印方式相同.

```LISP
 (set-pprint-dispatch '(cons (member quote)) () 
   #'(lambda (s list)
       (if (and (consp (cdr list)) (null (cddr list)))
          (funcall (formatter "'~W") s (cadr list))
          (pprint-fill s list))))
 
 (set-pprint-dispatch '(cons (member my-let)) 
                      (pprint-dispatch '(let) nil))
```

下一个示例指定一个默认方法, 用于打印与函数调用不对应的列表. 注意, 函数 pprint-linear, pprint-fill, 和 pprint-tabular 都用可选的 colon-p 和 at-sign-p 参数定义, 这样一来它们可以被用作美观打印分派函数(pprint dispatch function), 和 ~/.../ 函数一样.

    ```LISP
    (set-pprint-dispatch '(cons (not (and symbol (satisfies fboundp))))
                          #'pprint-fill -5)
    
    ;; Assume a line length of 9
    (pprint '(0 b c d e f g h i j k))
    (0 b c d
      e f g h
      i j k)
    ```

最后的示例展示了如何为用户定义的数据结构定义一个美观打印函数.

    ```LISP
    (defstruct family mom kids)
    
    (set-pprint-dispatch 'family
      #'(lambda (s f)
          (funcall (formatter "~@<#<~;~W and ~2I~_~/pprint-fill/~;>~:>")
                  s (family-mom f) (family-kids f))))
    ```

结构体 family 的美观打印函数指定了如何去调整输出的布局以至于它可以很好地适应各种行的宽度. 另外, 它遵循打印器控制变量 \*print-level*, \*print-length*, \*print-lines*, \*print-circle* 和 \*print-escape*, 并且可以容忍数据结构中几种不同的多样性. 下面的输出展示了在右边距为 25, \*print-pretty* 是 true, \*print-escape* 是 false 时, 一个多样的 kids 列表会被打印出什么.

    ```LISP
    (write (list 'principal-family
                  (make-family :mom "Lucy"
                              :kids '("Mark" "Bob" . "Dan")))
            :right-margin 25 :pretty T :escape nil :miser-width nil)
    (PRINCIPAL-FAMILY
      #<Lucy and
          Mark Bob . Dan>)
    ```

注意, 一个结构体的美观打印函数和结构体的 print-object 方法[method]是不同的. print-object 方法[method]永久地和一个结构体关联, 美观打印函数是被存储在美观打印分派表[pprint dispatch table]中并且可以迅速地改变来反映不同的打印需求. 如果在当前的美观打印分派表[pprint dispatch table]中没有一个结构体的美观打印函数, 就会使用它的 print-object 方法[method]. 

### 22.2.3 <span id="NotesPrettyPrinterBackground">美观打印器背景的注意事项</span>

有关本节中详细描述的抽象概念的背景引用, 见 XP: A Common Lisp Pretty Printing System. 这篇论文的细节对这份文件没有约束力, 但可能有助于建立理解这种资料的概念基础. 


## 22.3 <span id="">格式化输出</span>

format 用来产生格式化的字符串, 产生美观的信息, 等等是很有用的. format 可以生成和返回一个字符串或输出到目标 destination.

给 format 的控制字符串 control-string 参数是一个格式化控制. 这也就是说, 它可以是一个格式化字符串或一个函数, 比如由 formatter 宏返回的一个函数.

如果它是一个函数, 用合适的输出流作为它的第一个参数并且给 format 的数据参数作为它的剩余参数来调用这个函数. 这个函数应该执行必要的输出, 并返回参数中未使用的尾部 (如果有的话).

由 formatter 执行的编译过程产生一个函数, 它可以像 format 解释器处理这些参数一样来处理这些参数.

这个章节的剩余部分描述了如果 control-string 是一个格式化字符串会发生什么.

控制字符串 control-string 由简单文本 (字符) 和内嵌指令组成.

format 按这样写入简单文本; 每一个内嵌的指令指定了进一步出现在简单文本中对应位置的文本输出. 大部分指令使用参数 args 中的一个或多个来创建它们的输出.

一个指令由一个波浪符号, 一些由逗号分隔的可选前缀参数, 可选冒号和 at-sign 修饰语, 以及表示这个指令种类的单个字符. 在 at-sign 和冒号修饰符之间的顺序是没有必要的. 这个指令字符的大小写会被忽略. 前缀参数被表示为带符号的 (符号是可选的) 十进制数字, 或者一个单引号后面跟着一个字符. 比如, ~5,'0d 可以被用于在五列中以前导零打印一个整数, 或 ~5,'*d 来得到一个前导的星号.

将前缀参数替换为指令, 可以使用 V (或 v). 在这个情况中, format 从 args 中接受一个参数作为给指令的参数. 这个参数应该是一个整数或字符. 如果这个被 V 参数使用的参数 arg 是 nil, 效果就好像这个参数被省略了一样. # 可以被用于替换一个前缀参数; 它表示剩下要被处理的参数 args 的数量. 当在一个递归的 format 中使用时, 在 ~? 或 ~{ 的上下文中, 这个 # 前缀参数表示在这个递归调用中剩余格式化参数的数量.

格式化字符串的示例:

    "~S"        ;This is an S directive with no parameters or modifiers.  
    "~3,-4:@s"  ;This is an S directive with two parameters, 3 and -4,    
                ; and both the colon and at-sign flags.                   
    "~,+4S"     ;Here the first prefix parameter is omitted and takes     
                ; on its default value, while the second parameter is 4.  

    Figure 22-6. 格式化控制字符串的示例

format 发送这个输出到目标 destination. 如果 destination 是 nil, format 创建并返回一个包含来自 control-string 的输出的字符串. 如果 destination 不是 nil, 它必须是一个带有填充指针的字符串, 一个流, 或者符号 t. 如果 destination 是一个带有填充指针的字符串, 输出会被添加到这个字符串的末尾. 如果 destination 是一个流, 输出会被发送到这个流. 如果 destination 是 t, 输出会被发送到标准输出.

在对以下指令的描述中, 术语 arg 通常引用要被处理的 args 集合的下一个条目. 每个描述开头的词或短语是指令的助记符. format 指令不会绑定这些打印控制变量(\*print-...*)的任何一个除了在下面描述中指定的. 具体实现可能为每个格式化指令指定新的, 具体实现指定的打印控制变量绑定, 但是它们可能不绑定在一个标准打印器控制变量的描述中没有指定的任何标准打印器控制变量, 也不能绑定这个描述中指定的任何标准打印器控制变量.

> * 22.3.1 [FORMAT 基本输出](#FORMATBasicOutput)
> * 22.3.2 [FORMAT 基数控制](#FORMATRadixControl)
> * 22.3.3 [FORMAT 浮点打印器](#FORMATFloatingPointPrinters)
> * 22.3.4 [FORMAT 打印器操作](#FORMATPrinterOperations)
> * 22.3.5 [FORMAT 美观打印器操作](#FORMATPrettyPrinterOperations)
> * 22.3.6 [FORMAT 布局控制](#FORMATLayoutControl)
> * 22.3.7 [FORMAT 控制流操作](#FORMATControlFlowOperation)
> * 22.3.8 [FORMAT 杂项操作](#FORMATMiscellaneousOperation)
> * 22.3.9 [FORMAT 杂项伪操作](#FORMATMiscellaneousPseudoOperation)
> * 22.3.10 [关于 FORMAT 的额外信息](#AddInfoFORMATOperations)
> * 22.3.11 [FORMAT 的示例](#ExamplesFORMAT)
> * 22.3.12 [FORMAT 的注意事项](#NotesFORMAT)


### 22.3.1 <span id="FORMATBasicOutput">FORMAT 基本输出</span>

> * 22.3.1.1 [波浪符号 C: 字符](#TildeCCharacter)
> * 22.3.1.2 [波浪符号百分比号%: 换行](#TildePercentNewline)
> * 22.3.1.3 [波浪符号和号&: 换行](#TildeAmpersandFreshLine)
> * 22.3.1.4 [波浪符号竖杠|: 页](#TildeVerticalBarPage)
> * 22.3.1.5 [波浪符号波浪符号~: 波浪符号](#TildeTildeTilde)

#### 22.3.1.1 <span id="TildeCCharacter">波浪符号 C: 字符</span>

下一个参数 arg 应该是一个字符; 它是根据修饰符的标志打印出来的.

如果这个字符是一个简单字符, 那么 ~C 打印这个字符就像是通过使用 write-char 一样. 不是简单的字符没有必要像是通过 write-char 打印, 但是以一种具体实现定义的简洁的格式显示. 比如,

    ```LISP
    (format nil "~C" #\A) =>  "A"
    (format nil "~C" #\Space) =>  " "
    ```

对于打印字符 ~:C 和 ~C 相同, 但是其他字符是 "拼写出来的". 其目的是, 打印字符的"美观"格式. 关于不打印的简单字符, 拼写出来的是字符的名称 (见 char-name). 对于没有打印的非简单字符, 什么被拼写出来是具体实现定义的. 比如,

    ```LISP
    (format nil "~:C" #\A) =>  "A"
    (format nil "~:C" #\Space) =>  "Space"
    ;; This next example assumes an implementation-defined "Control" attribute.
    (format nil "~:C" #\Control-Space)
    =>  "Control-Space"
    OR=>  "c-Space"
    ```

~:@C 打印 ~:C 会打印的东西, 然后如果这个字符需要键盘上的不寻常的 shift 键来输入, 这个事实就会被提到 . 比如,

    ```LISP
    (format nil "~:@C" #\Control-Partial) =>  "Control-<PARTIAL> (Top-F)"  
    ```

这是在提示中用来告诉用户他要输入的密钥的格式. 准确的输出可能不仅取决于实现, 还取决于所使用的特定 I/O 设备.

~@C 以一种 Lisp 读取器可以理解的方式打印这个字符, 使用 #\ 语法.

~@C 绑定 \*print-escape* 为 t. 

#### 22.3.1.2 <span id="TildePercentNewline">波浪符号百分比号%: 换行</span>

这个输出一个 #\Newline 子, 从而终止当前的输出行并开始一个新的输出行. ~n% 输出 n 个新行. 不使用参数 arg. 

#### 22.3.1.3 <span id="TildeAmpersandFreshLine">波浪符号和号&: 换行</span>

除非它可以确定这个输出流已经在一行的开始, 否则这个就会输出一个新行. ~n& 调用 fresh-line 然后输出 n-1 个新行. ~0& 什么都不做.

#### 22.3.1.4 <span id="TildeVerticalBarPage">波浪符号竖杠|: 页</span>

如果可能的话, 这个输出一个分页符号. ~n| 执行 n 次这个. 

#### 22.3.1.5 <span id="TildeTildeTilde">波浪符号波浪符号~: 波浪符号</span>

这个输出一个波浪符号. ~n~ 输出 n 个波浪符号. 

### 22.3.2 <span id="FORMATRadixControl">FORMAT 基数控制</span>

> * 22.3.2.1 [波浪符号 R: 基数](#TildeRRadix)
> * 22.3.2.2 [波浪符号 D: 十进制](#TildeDDecimal)
> * 22.3.2.3 [波浪符号 B: 二进制](#TildeBBinary)
> * 22.3.2.4 [波浪符号 O: 八进制](#TildeOOctal)
> * 22.3.2.5 [波浪符号 X: 十六进制](#TildeXHexadecimal)

#### 22.3.2.1 <span id="TildeRRadix">波浪符号 R: 基数</span>

~nR 用基数 n 来打印参数 arg. 修饰符标识和任何剩余参数被用于 ~D 检测. ~D 和 ~10R 相同. 完整形式是 ~radix,mincol,padchar,commachar,comma-intervalR.

如果没有给 ~R 一个前缀参数, 那就就会给定一个不同的解释. 这个参数应该是一个整数. 比如, 如果参数 arg 是 4:

* ~R 打印参数 arg 作为一个基本的英语数字: four.

* ~:R 打印参数 arg 作为一个英语序数: fourth.

* ~@R 打印参数 arg 作为一个罗马数字: IV.

* ~:@R 打印参数 arg 作为一个旧罗马数字: IIII.

比如:

    ```LISP
    (format nil "~,,' ,4:B" 13) =>  "1101"
    (format nil "~,,' ,4:B" 17) =>  "1 0001"
    (format nil "~19,0,' ,4:B" 3333) =>  "0000 1101 0000 0101"
    (format nil "~3,,,' ,2:R" 17) =>  "1 22"
    (format nil "~,,'|,2:D" #xFFFF) =>   "6|55|35"
    ```

当且仅当第一个参数, n, 被提供, ~R 绑定 \*print-escape* 为 false, \*print-radix* 为 false, \*print-base* 为 n, 并且 \*print-readably* 为 false.

当且仅当没有提供参数时, ~R 绑定 \*print-base* 为 10. 

#### 22.3.2.2 <span id="TildeDDecimal">波浪符号 D: 十进制</span>

一个应该为一个整数的参数 arg 用十进制基数来打印. ~D 不会在这个数字后面放一个小数点.

~mincolD 使用 mincol 的列宽度; 如果这个数的数字和符号需要小于 mincal 列, 空格 space 就会被插入到左边. 如果这个数字不符合 n 列, 则需要额外的列.

~mincol,padcharD 使用 padchar 作为填补字符而不是空格 space.

如果参数 arg 不是一个整数,它会用 ~A 格式和十进制基数来打印.

这个 @ 修饰符导致这个数字的符号总是被打印; 默认当且仅当这个数字是负的才打印. 这个 : 修饰符导致逗号被打印在数字组之间; commachar 可能被用于改变被用作逗号的字符. comma-interval 一定是个整数并且默认为 3. 当这个 : 修饰符被给定为这些指令的任意一个时, 这个 commachar 被打印在 comma-interval 个数字组之间.

因此最广泛的 ~D 形式是 ~mincol,padchar,commachar,comma-intervalD.

~D 绑定 \*print-escape* 为 false, \*print-radix* 为 false, \*print-base* 为 10, 以及 \*print-readably* 为 false. 


#### 22.3.2.3 <span id="TildeBBinary">波浪符号 B: 二进制</span>

这个就像 ~D 但是用二进制基数 (基数 2) 而不是十进制来打印. 因此完整形式为 ~mincol,padchar,commachar,comma-intervalB.

~B 绑定 \*print-escape* 为 false, \*print-radix* 为 false, \*print-base* 为 2, 并且 \*print-readably* 为 false. 


#### 22.3.2.4 <span id="TildeOOctal">波浪符号 O: 八进制</span>

这个就像 ~D 但是用八进制基数 (基数 8) 而不是十进制来打印. 因此完整形式为 ~mincol,padchar,commachar,comma-intervalO.

~O 绑定 \*print-escape* 为 false, \*print-radix* 为 false, \*print-base* 为 8, 并且 \*print-readably* 为 false. 


#### 22.3.2.5 <span id="TildeXHexadecimal">波浪符号 X: 十六进制</span>

这个就像 ~D 但是用十六进制基数 (基数 16) 而不是十进制来打印. 因此完整形式为 ~mincol,padchar,commachar,comma-intervalX.

~X 绑定 \*print-escape* 为 false, \*print-radix* 为 false, \*print-base* 为 16, 并且 \*print-readably* 为 false. 


### 22.3.3 <span id="FORMATFloatingPointPrinters">FORMAT 浮点打印器</span>

> * 22.3.3.1 [波浪符号 F: 固定格式浮点数](#TildeFFixedFormat)
> * 22.3.3.2 [波浪符号 E: 指数浮点数](#TildeEExponential)
> * 22.3.3.3 [波浪符号 G: 一般浮点数](#TildeGGeneral)
> * 22.3.3.4 [波浪符号 美元符号: 货币浮点数](#TildeDollarsignMonetary)


#### 22.3.3.1 <span id="TildeFFixedFormat">波浪符号 F: 固定格式浮点数</span>

下一个参数 arg 被打印为一个浮点数.

完整形式是 ~w,d,k,overflowchar,padcharF. 参数 w 是要被打印的域宽度; d 是在小数点后面要被打印的数字的数量; k 是一个默认为零的伸缩因子.

w 个字符会被输出. 首先, 如果有必要, 打印出字符 padchar (默认为一个空格 space) 的前导副本, 来填充左边的域. 如果这个参数 arg 是负的, 那么会打印一个负号; 如果这个参数 arg 不是负的, 当且仅当提供了 @ 修饰符时会打印一个正号. 然后一个包含单小数点的数字序列会被打印; 这个表示参数 arg 的值的大小乘以 10^k, 舍入到 d 小数数位. 当向上舍入和向下舍入可能产生和参数 arg 的伸缩值等距的打印值时, 那么具体实现可以自由地去选择任意一个. 比如, 使用格式 ~4,2F 打印参数 6.375 可能正确地产生 6.37 或 6.38. 前导零是不允许的, 除了当打印的值小于 1 时一个单个的数字零会被输出到小数点前, 如果 w=d+1 那么这个单个的数字零不会被输出.

如果用请求的格式在一个宽度为 w 的域中打印这个值是不可能的, 那么就会采取这两个动作中的一个. 如果提供了参数 overflowchar, 那么那么个参数的 w 个拷贝会被打印而不是这个参数 arg 的伸缩值. 如果省略了这个 overflowchar 参数, 那么这个伸缩值会使用超过 w 个字符来打印, 和它需要的一样多.

如果省略了这个 w 参数, 那么这个域就是可变宽度的. 实际上, 会以这样一种方式为 w 选择一个值: 没有前导的填充字符需要被打印并且小数点后面准确地跟着 d 个字符. 比如, 指令 ~,2F 会准确打印小数点后两位数而小数点前根据必要尽可能多.

如果省略了参数 d, 那么这里没有约束出现在小数点后面的数字. 会以这样一种方式为 d 选择一个值: 尽可能多的数字可能被打印出来, 取决于参数 w 所施加的宽度限制以及在小数末尾中不出现 0 数字的约束条件, 除非如果这个要被打印的小数是 0, 如果这个宽度约束允许, 那么一个单个零数字应该出现在小数点后面.

如果 w 和 d 都被省略了, 那么效果就是使用普通的自由格式输出来打印这个值; prin1 为任何大小为零或在 10^-3 (包括的) 和 10^7 (不包括的) 之间的数字使用这个格式.

如果省略了 w, 那么如果这个参数 arg 的大小太大 (或者, 如果 d 也被省略, 那么就是太小) 以致于超过 100 个数字不得不被打印, 那么一个实现可以自由地去使用指数符号来打印这个数字, 就像是通过指令 ~E (所有给 ~E 的参数都是默认的, 没有从 ~F 指令中接收任何值).

如果 arg 是一个有理数, 那么它会被强制转换为一个单浮点数并且打印. 或者, 一个具体实现允许去用任何其他有着相同本质行为但是避免由于强制转换导致的精度丢失或溢出的方法来处理一个有理数. 如果没有提供 w 和 d 并且这个数字没有准确的十进制表示, 比如 1/3, 由于只有有限数量的数字可能被打印出来, 所以具体实现必须选择一个精确的截断.

如果参数 arg 是一个复数或者某个非数值对象, 那么它会使用格式化指令 ~wD 来打印, 从而用十进制数字和 w 的最小域宽度来打印它.

~F 绑定 \*print-escape* 为 false 并绑定 \*print-readably* 为 false. 


#### 22.3.3.2 <span id="TildeEExponential">波浪符号 E: 指数浮点数</span>
<!--待校验-->
下一个参数 arg 用指数表示来打印为一个浮点数.

完整形式是 ~w,d,e,k,overflowchar,padchar,exponentcharE. 参数 w 是这个要被打印的域宽度; d 是在小数点后面要被打印的数字的数量; 当打印一个指数时 e 是要被使用的数字数量; k 是一个默认为 1 的伸缩因子 (不是零).

w 个字符会被准确地输出. 首先, 如果有必要, 打印出字符 padchar (默认为一个空格 space) 的前导副本, 来填充左边的域. 如果这个参数 arg 是负的, 那么会打印一个负号; 如果这个参数 arg 不是负的, 当且仅当提供了 @ 修饰符时会打印一个正号. 然后一个包含单小数点的数字序列会被打印. 这个数字序列的形式取决于伸缩因子 k. 如果 k 是 0, 那么在小数点后面打印 d 个数字, 如果总的域宽度允许, 一个单独的数字 0 会出现在小数点前面. 如果 k 是正的, 那么它必须严格小于 d+2; 小数点前打印 k 个有效数字, 而小数点之后打印 d-k+1 个数字. 如果 k 是负的, 那么它必须是严格大于 -d; 如果总的域宽度允许, 一个单独的 0 会出现在小数点前, 而在小数点后先打印 -k 个 0 然后是 d+k 个有效数字. 打印的小数必须正确地四舍五入. 当向上舍入和向下舍入可能产生和参数 arg 的伸缩值等距的打印值时, 那么具体实现可以自由地去选择任意一个. 比如, 使用 ~8,2E 格式来打印 637.5 可能正确地产生 6.37E+2 或 6.38E+2.

在这个数字序列后, 打印指数. 首先打印字符参数 exponentchar; 如果这个参数省略了, 那么这个 prin1 使用的指数标记会被打印, 从这个浮点数的类型和 \*read-default-float-format* 的当前值决定. 接着, 打印一个正号或负号, 后面跟着 e 个数字表示十的幂, 打印的小数必须乘以这个值来适当地表示参数 arg 的舍入值.

如果不可能以要求的格式在域宽度为 w 中打印这个值, 可能是因为 k 太大或太小或者因为这个指数不能被打印在 e 个字符位置中, 那么会采取这两个动作中的一个. 如果提供了参数 overflowchar, 那么那个参数的 w 个宽度副本会被打印而不是打印参数 arg 的伸缩值. 如果这个 overflowchar 参数被省略了, 那么打印的伸缩值使用超过 w 个字符, as many more as may be needed; 如果这个问题是对于提供的 k 这个 d 太小, 或者 e 太小, 那么就会按照需要使用一个更大的 d 或 e 值.

如果省略了这个 w 参数, 那么这个域就是可变宽度的. 事实上, 以一种没有前导填充字符需要被打印的方式来选择一个 w 值.

如果省略了参数 d, 那么在数字的出现数量上没有约束. 以这样的方式为 d 选择一个值: 尽可能多的数字可以打印在参数 w 所施加的宽度限制下, 这个伸缩因子 k 的约束, 以及没有数字零出现在小数的尾部中的约束, 除了如果要被打印的小数是 0 那么单独的数字零会出现在小数点后面.

如果参数 e 被省略, 那么使用这些数字中最小的数字来打印这个指数, 以表示它的值.

如果 w, d, 和 e 都被省略, 那么这个效果是使用普通的自由格式的指数计数法来打印这个值; prin1 为任意非零大小是小于 10^-3 或大于 10^7 的数字使用一个类似的格式. 仅有的区别是这个 ~E 指令总是在指数前打印一个正号或符号, 而如果这个指数是非负的 prin1 省略这个正号.

如果参数 arg 是一个有理数, 那么它会被强制转换为一个单浮点数并且打印. 或者, 一个具体实现允许去用任何其他有着相同本质行为但是避免由于强制转换导致的精度丢失或溢出的方法来处理一个有理数. 如果 w 和 d 没有被提供并且这个数字没有准确的十进制表示, 比如 1/3, 由于只有有限数量的数字可能被打印出来, 所以具体实现必须选择一个精确的截断.

如果 arg 是一个复数或某个非数值对象, 那么它会使用格式化指令 ~wD 来打印, 从而以十进制基数和最小域宽度 w 来打印它.

~E 绑定 \*print-escape* 为 false 并且绑定 \*print-readably* 为 false. 


#### 22.3.3.3 <span id="TildeGGeneral">波浪符号 G: 一般浮点数</span>

下一个参数 arg 是情况用固定格式或指数表示打印一个浮点数.

完整形式是 ~w,d,e,k,overflowchar,padchar,exponentcharG. 用来打印参数 arg 的格式取决于参数 arg 的大小 (绝对值). 让 n 为一个满足 10^n-1 <= |arg| < 10^n 的整数. 让 ee 等于 e+2, 或者如果 e 省略了就是 4. 让 ww 等于 w-ee, 如果 w 省略了就是 nil. 如果 d 省略了, 首先让 q 成为打印 arg 所需的数字的数量, 不丢失任何信息, 没有前导或尾随的零; 然后让 d 等于 (max q (min n 7)). 让 dd 等于 d-n.

如果 0 <= dd <= d, 那么 arg 被打印, 就像是通过格式化指令

    ~ww,dd,,overflowchar,padcharF~ee@T

注意, 伸缩因子 k 不会传递给那个 ~F 指令. 对于 dd 的所有其他值, arg 被打印, 就像是通过格式化指令

    ~w,d,e,k,overflowchar,padchar,exponentcharE

在任何一种情况下, 当且仅当向 ~G 指令提供了一个 @ 修饰符, 那么它将被提供给 ~F 或 ~E 指令.

~G 绑定 \*print-escape* 为 false 并且绑定 \*print-readably* 为 false. 


#### 22.3.3.4 <span id="TildeDollarsignMonetary">波浪符号 美元符号: 货币浮点数</span>

下一个参数 arg 用固定格式表示打印为一个浮点数.

完整形式为 ~d,n,w,padchar$. 参数 d 是小数点后要被打印的数字的数量 (默认值 2); n 是小数点前要打印数字的最小数量 (默认值 1); w 是要被打印的这个域的最小总宽度 (默认值 0).

首先填充和符号会被输出. 如果这个参数 arg 是负的, 那么会输出一个负号; 如果这个参数 arg 不是负的, 当且仅当提供一个 @ 修饰符时打印一个正号. 如果使用了这个 : 修饰符, 这个符号会出现在任何填充之前, 否则就是在填充之后. 如果提供了 w 并且其他要被输出的字符的数量小于 w, 那么 padchar (默认为一个空格 space) 的拷贝会被输出来使域的总宽度等于 w. 然后打印 n 个数字作为参数 arg 的整数部分, 如果必要的话, 带有前导的零; 然后打印一个小数点; 接着是小数的 d 个数字, 适当地舍入.

如果参数 arg 的大小太大以致于超过 m 个数字需要被打印, 其中 m 是 w 和 100 的较大者, 那么一个具体实现可以自由地自行决定使用指数表示来打印这个数字, 就像是通过指令 ~w,q,,,,padcharE, 其中 w 和 padchar 是存在的还是省略的得根据它们在这个 ~$ 指令中是存在的还是省略的来决定, 并且其中 q=d+n-1, 而 d 和 n 是提供给 ~$ 指令的值 (可能是默认的).

如果 arg 是一个有理数, 那么它会被强制转换为一个单浮点数并且打印. 或者, 一个具体实现允许去用任何其他有着相同本质行为但是避免由于强制转换导致的精度丢失或溢出的方法来处理一个有理数.

如果 arg 是一个复数或者某个非数值对象, 那么它使用格式指令 ~wD 来打印, 从而以十进制基数和一个 w 的最小域宽度来打印它.

~$ 绑定 \*print-escape* 为 false 并且绑定 \*print-readably* 为 false. 


### 22.3.4 <span id="FORMATPrinterOperations">FORMAT 打印器操作</span>

#### 22.3.4.1 波浪符号 A: Aesthetic

一个可以是任何对象的参数 arg 不用单转义字符被打印 (就像是通过 princ). 如果 arg 是一个字符串, 它的字符会逐字输出. 如果 arg 是 nil 它会被打印为 nil; 这个冒号修饰符 (~:A) 会倒是一个 nil 的参数 arg 被打印为 (), 但是如果参数 arg 是一个复合结构, 例如一个列表或向量, 任何包含 nil 的出现仍然打印为 nil.

如果必要的话, ~mincolA 在右边插入空格来使这个宽度至少为 mincol 列. 这个 @ 修饰符导致空格被插入到左边而不是右边.

~mincol,colinc,minpad,padcharA 是 ~A 的完整形式, 它允许填充的控制. 这个字符串会用至少 minpad 个 padchar 的拷贝来填充右边 (如果使用了 @ 修饰符就是左边); 填充字符会在一个时间内插入 colinc 字符, 直到总宽度至少是 mincol. 对于 mincol 和 minpad 默认是 0, 对于 colinc 默认是 1, 并且对于 padchar 默认是空格字符.

~A 绑定 \*print-escape* 为 false, 并且绑定 \*print-readably* 为 false. 

#### 22.3.4.2 波浪符号 S: Standard

这个就像是 ~A, 但是参数 arg 用单转义字符打印 (就像是通过 prin1 而不是 princ). 因此这个输出适合作为输入来读取. ~S 接受所有 ~A 接受的参数和修饰符.

~S 绑定 \*print-escape* 为 t. 

#### 22.3.4.3 波浪符号 W: Write

一个可以是任何对象的参数是按每个打印机控制变量打印的 (像是通过 write). 另外, ~W 与深度缩写进行正确的交互, 不将深度计数器重置为 0. ~W 不接受参数. 如果给定了冒号修饰符, ~W 绑定 \*print-pretty* 为 true. 如果给定了 at-sign 修饰符, ~W 绑定 \*print-level* 和 \*print-length* 为 nil.

~W 为环状和共享的检测提供自动支持. 如果 \*print-circle* 的值表示 nil 并且 ~W 应用到了一个是环状(或共享)引用的参数, 一个适当的 #n# 标记会被插入到输出中而不是打印这个参数. 

### 22.3.5 <span id="FORMATPrettyPrinterOperations">FORMAT 美观打印器操作</span>

以下构造提供了对美观打印器的访问:

#### 22.3.5.1 波浪符号 下划线: 条件换行

没有任何修饰符时, ~_ 和 (pprint-newline :linear) 一样. ~@_ 和 (pprint-newline :miser) 一样. ~:_ 和 (pprint-newline :fill) 一样. ~:@_ 和 (pprint-newline :mandatory) 一样. 

#### 22.3.5.2 波浪符号 小于号: 逻辑块

~<...~:>

如果 ~:> 被用于终止 ~<...~>, 这个指令等价于一个对 pprint-logical-block 的调用. 对应于 ~<...~:> 指令的参数和 pprint-logical-block 的列表参数相同方式处理, 因此为非列表参数和环, 共享和深度缩写的检测提供了自动支持. 嵌套在 ~<...~:> 中的 control-string 的一部分指定了 :prefix (or :per-line-prefix), :suffix, 和 pprint-logical-block 的主体.

由 ~<...~:> 围绕的这个 control-string 部分可以通过 ~; 指令被划分为片段 ~<prefix~;body~;suffix~:>. 如果第一部分由 ~@; 终止, 它指定一个行前缀而不是一个简单的前缀. 这个前缀和后缀不能包含格式化指令. 如果前缀或后缀不是一个不变的字符串或者如果这个围绕的部分被拆分为不止三个片段, 那么就会发出一个错误.

如果这个围绕的部分只被划分为两个片段, 后缀默认为空字符串. 如果这个围绕的部分只由一个单独的片段组成, 那么前缀和后缀都默认为空字符串. 如果使用了这个冒号修饰符 (具体来说, 就是 ~:<...~:>), 前缀和后缀默认分别为 "(" 和 ")" 而不是空字符串.

这个主体片段可以是任意格式化字符串. 这个格式化字符串被应用于把 ~<...~:> 作为一个整体对应的数组元素. 使用 pprint-pop 从这个列表中提取元素, 因此为畸形列表以及环, 共享和长度缩写的检测提供自动支持. 在这个主体片段中, ~^ 表现得就像是 pprint-exit-if-list-exhausted.

~<...~:> 支持一个 pprint-logical-block 不支持的特性. 如果 ~:@> 被用于终止指令 (i.e., ~<...~:@>), 那么一个填充风格的条件换行会被自动插入到这个主体中包含的每一个空白组后 (除了一个 <Newline> 指令后面的空白以外). 这使得段落填充的等价物的实现变得简单.

如果这个 at-sign 修饰符和 ~<...~:> 一起使用, 那么这个完整的剩余参数列表会被传递给这个指令, 作为这个指令的参数. 所有这些剩余参数都由 ~@<...~:> 消耗, 即便它们没有全部被嵌套在这个指令中的格式化字符串使用. 除了在它的参数中的区别, ~@<...~:> 和 ~<...~:> 一样, 除了如果 ~@<...~:> 在一个格式化字符串中到达了顶层, 这个环的检测就不会被应用. 这个保证这个环的检测只会应用到数据列表, 不会应用到格式化参数列表.

如果要将环或共享作为一个整体来表示, " . #n#" 将被打印出来.

在很大程度上, 指令 ~<...~> 的基本形式和通过 ~W, ~\_, ~<...~:>, ~I, 和 ~:T 的输出配置的动态控制不兼容. 因此, 如果这些指令中的任何一个嵌套在 ~<...~> 中就会发出一个错误. 除了那个以外, 如果这个 ~<...~> 的 ~<...~:;...~> 形式和 ~W, ~_, ~<...~:>, ~I, 或 ~:T 一起在同一个格式化字符串中使用也会发出一个错误.

也见章节 22.3.6.2 (波浪符号 小于号: 对齐). 


#### 22.3.5.3 波浪符号 I: 缩进

~nI 和 (pprint-indent :block n) 一样.

~n:I 和 (pprint-indent :current n) 一样. 在这两种情况中, 如果 n 被省略的话, 它默认为零. 

#### 22.3.5.4 波浪符号 斜杠: 调用函数

~/name/

可以在一个格式化字符串中通过使用 ~/name/ 指令来调用用户定义的函数. 这个冒号修饰符, 这个 at-sign 修饰符, 以及很多参数可以和 ~/name/ 指令一起被指定. name 可以是不包含 "/" 的任意字符串. 这个 name 中的所有字符都会像它们是大写的情况一样被对待. 如果 name 包含了一个单冒号 (:) 或双冒号 (::), 那么直到但不包括第一个 ":" 或 "::" 的所有字符被认为是命名一个包的字符串. 在第一个 ":" 或 "::" (如果有的话) 的所有字符都被认为是命名一个符号的字符串. 对应于一个 ~/name/ 指令的函数通过在表示的包中查找所表示的符号来获取. 如果 name 不包含 ":" 或 "::", 那么在 COMMON-LISP-USER 包中查找整个 name 字符串.

当与到一个 ~/name/ 指令时, 所表示的函数用四个或更多的参数来调用. 前四个参数是: 输出流, 对应这个指令的格式化参数, 一个如果使用了冒号修饰符就是 true 的广义 boolean, 以及一个如果使用了 at-sign 修饰符就是 true 的广义 boolean. 函数应该适当地打印参数. 这个函数返回的值都会被忽略.

这三个函数 pprint-linear, pprint-fill, 和 pprint-tabular 是特别设计的以致于它们可以通过 ~/.../ 来调用 (换句话说, ~/pprint-linear/, ~/pprint-fill/, 和 ~/pprint-tabular/). 特别地, 它们接受冒号和 at-sign 参数. 


### 22.3.6 <span id="FORMATLayoutControl">FORMAT 布局控制</span>

#### 22.3.6.1 波浪符号 T: 制表符

这个空格到一个给定的列. ~colnum,colincT 会输出足够的空格来使游标移动到列 colnum. 如果这个游标已经在或超过列 colnum, 它会输出空格来移动它到列 colnum+k*colinc, 其中 k 为可能的最小正整数, 除非 colinc 是零, 在这个情况下如果游标已经在或超过列 colnum 不会输出空格. colnum 和 colinc 默认为 1.

如果由于某个原因, 当前完整的列位置不能由直接查询来确定, format 可能推理出当前的列位置 通过标记某些指令 (例如 ~%, 或 ~&, 或带有包含换行的字符串参数的 ~A) 导致这个列位置被重置为零, 并且统计从那个点开始发射的字符数量. 如果这样失败了, format 可能尝试一个类似的推理, 基于"当调用 format 时目标在零列"这个更加危险的设想上. 如果即便这个试探失败了 If even this heuristic fails or is implementationally inconvenient, 最坏情况下这个 ~T 操作符会简单输出两个空格.

~@T 执行相对的制表. ~colrel,colinc@T 输出 colrel 个空格并且接着输出最小非负数量的必要的额外空格来使游标移动到是 colinc 的倍数的一个列. 比如, 指令 ~3,8@T 输出三个空格然后移动游标到一个 "标准的八倍制表停止" if not at one already. 如果不能确定当前的输出列, 那么 colinc 就会被忽略, 而 colrel 个空格恰好是输出.

如果这个冒号修饰符和 ~T 指令一起使用, 这个制表的计算是相对于立即包含指令的部分开始的水平位置, 而不是对于零的水平位置. 这两个数值参数都被解释为以 em 为单位并且默认为 1. ~n,m:T 和 (pprint-tab :section n m) 一样. ~n,m:@T 和 (pprint-tab :section-relative n m) 一样. 


#### 22.3.6.2 波浪符号 小于号: 对齐

~mincol,colinc,minpad,padchar<str~>

这个对齐由在至少 mincol 列宽度的域中处理 str 产生的文本. str 可能使用 ~; 划分片段, 在这个情况中间隔在文本段之间平均分配.

没有修饰符时, 最左边的文本片段在这个域中被左对齐, 并且最右边的文本片段被右对齐. 如果这里只有一个文本元素, 作为一个特殊情况, 它被右对齐. 这个 : 修饰符导致在第一个文本片段之前要被引入的间距; 这个 @ 修饰符导致在最后要被添加的间距. 这个 minpad 参数 (默认 0) 是在每一个片段之间要被输出的填充字符的最小数量. 填充字符由 padchar 提供, 默认为空格字符. 如果需要去满足这些约束的总宽度大于 mincol, 那么使用的宽度是 mincol+k*colinc, 其中 k 是可能的最小非负整数值. colinc 默认为 1, 并且 mincol 默认为 0.

注意, str 可能包含 format 指令. 在 str 中的所有子句依次被处理; 它是被对齐的文本片段.

这个 ~^ 指令可以被用于过早地终止子句处理, 在这个情况下只有那些完整处理的子句会被对齐.

如果 ~< 的第一个子句由 ~:; 终止而不是 ~;, 那么以一种特殊的方式来使用它. 所有这些子句被处理 (当然, 受限于 ~^), 但是第一个不会被用于执行隔开和填充. 当那个填充的结果已经被确定时, 那么如果它符合输出的当前行, 它就被输出, 并且第一个子句的文本会被丢弃. 然而, 如果填充的文本不符合当前行, 那么第一个子句的文本片段会在这个填充文本之前被输出. 第一个子句应该包含一个换行 (例如一个 ~% 指令). 第一个子句总是被处理, 因此它引用的任何参数都会被使用; 这个决策是是否使用生成的文本片段, 而不是是否处理第一个子句. 如果这个 ~:; 有一个前缀参数 n, 那么填充文本必须在当前的行中与 n 个字符位置相匹配以避免输出第一个子句的文本. 比如, 控制字符串

    "~%;; ~{ ~<~%;; ~1:; ~S~>~^ ,~} .~%"

可以用来打印一个由逗号分隔的条目列表, 而不需要在行边界上破坏条目, 其中每一行用 ;; 开始. 在 ~1:; 中的前缀参数 1 说明了对齐条目后面的逗号宽度, 如果这个条目不是列表中的最后一个元素, 如果是最后一个元素的话就是句号的宽度. 如果 ~:; 有一个第二个前缀参数, 那么它被用作这个行的宽度, 因此覆盖了这个输出流的自然行宽度. 为了使前面的例子使用 50 的行宽, 一种方式可以写作

    "~%;; ~{ ~<~%;; ~1,50:; ~S~>~^ ,~}  .~%"

如果没有提供第二个参数, 那么 format 使用 destination 输出流的行宽. 如果不能确定这个 (比如, 当产生一个字符串结果时), 那么 format 使用 72 作为行长度.

也见章节 22.3.5.2 (波浪符号 小于号: 逻辑块). 

#### 22.3.6.3 波浪符号 大于号: 终止对齐

~> 终止一个 ~<. 在其他地方使用它的后果是未定义的. 

#### 22.3.7 <span id="FORMATControlFlowOperation">FORMAT 控制流操作</span>

> * 22.3.7.1 [波浪符号 星号: Go-To](#TildeAsteriskGoTo)
> * 22.3.7.2 [波浪符号 左括号: 条件表达式](#TildeLeftBracketCondExpr)
> * 22.3.7.3 [波浪符号 右括号: 条件表达式的结束](#TildeRightBracketEndCondExpr)
> * 22.3.7.4 [波浪符号 左大括号: 循环](#TildeLeftBraceIteration)
> * 22.3.7.5 [波浪符号 右大括号: 循环的结束](#TildeRightBraceEndIteration)
> * 22.3.7.6 [波浪符号 问号: 递归处理](#TildeQuestionMarkRecursiveProc)

#### 22.3.7.1 <span id="TildeAsteriskGoTo">波浪符号 星号: Go-To</span>

下一个参数 arg 会被忽略. ~n* 忽略接下来的 n 个参数.

~:* 在参数列表中倒退, 这样一来最后处理的参数会被再一次处理. ~n:* 倒退 n 个参数.

当在一个 ~{ 构造 (见下方) 中时, 这个忽略 (不管在哪个方向) 是相对于要被这个循环处理的参数列表.

~n@* 转到第 n 个参数 arg, 这里的 0 表示第一个; n 默认为 0, 因此 ~@* 转移回第一个参数 arg. 在一个 ~n@* 后的指令会接收一个序列中的参数, 这个序列以那个转移到的参数开始. 当在一个 ~{ 构造中时, 这个 "goto" 是相对于要被这个循环处理的参数列表 . 

#### 22.3.7.2 <span id="TildeLeftBracketCondExpr">波浪符号 左括号: 条件表达式</span>

~[str0~;str1~;...~;strn~]

这个是一个控制字符串的集合, 称为子句, 它们的其中一个会被选择并使用. 这些子句由 ~; 分隔并且这个构造由 ~] 终止. 比如,

    "~[Siamese~;Manx~;Persian~] Cat"

这个第 arg 个子句会被选择, 其中第一个子句是数字 0. 如果给定了一个前缀参数 (例如 ~n[), 那么那个参数(parameter)会被使用而不是一个参数(argument). 如果这个参数 arg 在范围之外那么没有子句会被选择并且不会发出错误. 在这个选择的方案被处理后, 这个控制字符串在 ~] 后开始.

~[str0~;str1~;...~;strn~:;default~] has a default case. 如果最后 ~; 用于分隔子句是 ~:;, 那么最后那个子句是一个 else 子句, 没有其他子句被选择时被执行. 比如:

    "~[Siamese~;Manx~;Persian~:;Alley~] Cat"

如果参数 arg 是 false, 那么 ~:[alternative~;consequent~] 选择 alternative 控制字符串, 否则选择 consequent 控制字符串.

~@[consequent~] 检验这个参数. 如果它是 true, 那么这个参数没有被 ~[ 命令用完但是保持为下一个要被处理的参数, 并且那个子句 consequent 被处理. 如果参数 arg 是 false, 那么这个参数会被用完, 并且子句不会被处理. 因此这个子句应该正常使用一个参数, 并且可能期望它为非 nil. 比如:

```LISP
(setq *print-level* nil *print-length* 5)
(format nil
        "~@[ print level = ~D~]~@[ print length = ~D~]"
        *print-level* *print-length*)
=>   " print length = 5"
```

注意

```LISP
(format stream "...~@[str~]..." ...)
==  (format stream "...~:[~;~:*str~]..." ...)
```

这个 ~[ 和 # 的组合是很有用的, 比如, 用于处理打印列表的英语规约:

```LISP
(setq foo "Items:~#[ none~; ~S~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^ ,~}~].")
(format nil foo) =>   "Items: none."
(format nil foo 'foo) =>   "Items: FOO."
(format nil foo 'foo 'bar) =>   "Items: FOO and BAR."
(format nil foo 'foo 'bar 'baz) =>   "Items: FOO, BAR, and BAZ."
(format nil foo 'foo 'bar 'baz 'quux) =>   "Items: FOO, BAR, BAZ, and QUUX."
```

#### 22.3.7.3 <span id="TildeRightBracketEndCondExpr">波浪符号 右括号: 条件表达式的结束</span>

~] 终止一个 ~[. 在其他地方使用它的后果是未定义的. 

#### 22.3.7.4 <span id="TildeLeftBraceIteration">波浪符号 左大括号: 循环</span>

~{str~}

整数一个循环构造. 参数应该是一个列表, 它被用作一个参数集合, 就像是一个对 format 的递归调用. 字符串 str 被重复用作控制字符串. 每个循环都可以吸收列表中它希望的一样多的元素作为参数; 如果 str 本身使用了两个参数, 那么列表中的两个元素将在每次循环时被耗尽. 如果在任何循环步骤之前这个列表就是空的, 那么这个循环会被终止. 同样地, 如果给定了一个前缀参数 n, 那么这里会有至少 n 个 str 的处理的重复. 最后, 这个 ~^ 指令可以被用于提前终止这个循环.

比如:

```LISP
(format nil "The winners are:~{ ~S~}." 
        '(fred harry jill)) 
=>  "The winners are: FRED HARRY JILL."                           
(format nil "Pairs:~{ <~S,~S>~}." 
        '(a 1 b 2 c 3))
=>  "Pairs: <A,1> <B,2> <C,3>."
```

~:{str~} 类似, 但是参数应该是一个子表的列表. 在每一个重复步骤, 一个子表被用作处理 str 的参数的集合; 下一次重复时, 使用一个新的子表, 不管最后的子表是否已经被处理. 例如:

```LISP
(format nil "Pairs:~:{ <~S,~S>~} ." 
                '((a 1) (b 2) (c 3)))
=>  "Pairs: <A,1> <B,2> <C,3>."
```

~@{str~} 类似于 ~{str~}, 但是不使用一个列表参数, 所有的剩余参数被用作这个循环的参数列表. 例如:

```LISP
(format nil "Pairs:~@{ <~S,~S>~} ." 'a 1 'b 2 'c 3)
=>  "Pairs: <A,1> <B,2> <C,3>."
```

如果在所有剩余参数被消费之间循环终止, 那么任何没有被这个循环处理的参数保留给这个循环构造后面的指令处理.

~:@{str~} 组合 ~:{str~} and ~@{str~} 的特性. 所有这些剩余参数都会被使用, 并且每一个一定是一个列表. 在每一个循环中, 下一个参数被用作给 str 的参数列表. 例如:

```LISP
 (format nil "Pairs:~:@{ <~S,~S>~} ." 
              '(a 1) '(b 2) '(c 3)) 
=>  "Pairs: <A,1> <B,2> <C,3>."
```

用 ~:} 而不是 ~} 终止这个重复构造强制对 str 至少处理一次, 即便参数的初始列表是空的. 然而, 这个不会覆盖一个显式的前缀参数零.

如果 str 是空的, 那么一个参数被用作 str. 它必须是一个格式化控制并且先于任何要被这个循环处理的参数之前. 举个例子, 下面这个是等价的:

```LISP
    (apply #'format stream string arguments)
 ==  (format stream "~1{~:}" string arguments)
```

这个会使用 string 作为一个格式化字符串. 这个 ~1{ 说明它最多只会被处理一次, 而这个 ~:} 说明它会被处理至少一次. 因此它只处理一次, 使用 arguments 作为参数. 这个情况可能被 ~? 指令更清晰地处理, 但是这个 ~{ 的一般特性比 ~? 更强大. 


#### 22.3.7.5 <span id="TildeRightBraceEndIteration">波浪符号 右大括号: 循环的结束</span>

~} 终止一个 ~{. 在其他地方使用它的后果是未定义的. 

#### 22.3.7.6 <span id="TildeQuestionMarkRecursiveProc">波浪符号 问号: 递归处理</span>

下一个参数 arg 必须是一个格式化控制, 并且在它后面的是一个列表; 这两个都被 ~? 指令消费. 这两个被处理成一个 control-string, 其中这个列表的元素作为这个参数. 一旦这个递归处理已经完成, 这个包含 ~? 指令的控制字符串的处理就会恢复. 例如:

```LISP
(format nil "~? ~D" "<~A ~D>" '("Foo" 5) 7) =>  "<Foo 5> 7"
(format nil "~? ~D" "<~A ~D>" '("Foo" 5 14) 7) =>  "<Foo 5> 7"
```

注意, 在第二个例子中三个参数被提供给格式化字符串 "<~A ~D>", 但是只有两个被处理并且第三个因此被忽略.

带有这个 @ 修饰符时, 只有一个参数 arg 被直接消费. 这个参数 arg 必须是一个字符串; 它被处理为这个控制字符串的部分, 就好像它已经出现在这个 ~@? 构造中, 并且这个递归处理的字符串中的任何指令可能消费这个包含 ~@? 指令的控制字符串的参数. 例如:

```LISP
(format nil "~@? ~D" "<~A ~D>" "Foo" 5 7) =>  "<Foo 5> 7"
(format nil "~@? ~D" "<~A ~D>" "Foo" 5 14 7) =>  "<Foo 5> 14"
```

### 22.3.8 <span id="FORMATMiscellaneousOperation">FORMAT 杂项操作</span>

#### 22.3.8.1 波浪符号 左圆括号: 大小写转换

~(str~)

包含的控制字符串 str 被处理, 并且产生的受限于大小写转换.

在没有标志的情况下, 每一个大写字符会被转换为对应小写字符.

~:( 用大写字母写所有单词, 就像是通过 string-capitalize.

~@( 只是用大写字母写第一个单词并且强制剩余的为小写.

~:@( 转换每一个小写字符为对应大写字符.

在这个例子中 ~@( 被用于导致由 ~@R 产生的第一个单词用大写字母写:

```LISP
(format nil "~@R ~(~@R~)" 14 14) 
=>  "XIV xiv"
(defun f (n) (format nil "~@(~R~) error~:P detected." n)) =>  F
(f 0) =>  "Zero errors detected."
(f 1) =>  "One error detected."
(f 23) =>  "Twenty-three errors detected."
```

当大小写转换嵌套出现时, 外部的转换来主导, 像下面这个例子中说明的:

```LISP
(format nil "~@(how is ~:(BOB SMITH~)?~)")
=>  "How is bob smith?"
NOT=>  "How is Bob Smith?"
```

#### 22.3.8.2 波浪符号 右圆括号: 大小写转换的终止

~) 终止一个 ~(. 在其他地方使用它的后果是未定义的. 

#### 22.3.8.3 波浪符号 P: Plural

如果 arg 没有和整数 1 是 eql, 打印一个小写的 s; 如果参数 arg 和 1 是 eql 的, 什么都不打印. 如果 arg 是一个浮点数 1.0, 那么打印 s.

~:P 做相同的事, 在执行一个 ~:* 后倒退一个参数; 这也就是说, 如果前面的参数不是 1, 那么它打印一个小写的 s.

如果参数是 1, ~@P 打印 y, 如果不是就是 ies. ~:@P 做相同的事, 但是后退到第一个.

```LISP
(format nil "~D tr~:@P/~D win~:P" 7 1) =>  "7 tries/1 win"
(format nil "~D tr~:@P/~D win~:P" 1 0) =>  "1 try/0 wins"
(format nil "~D tr~:@P/~D win~:P" 1 3) =>  "1 try/3 wins"
```

### 22.3.9 <span id="FORMATMiscellaneousPseudoOperation">FORMAT 杂项伪操作</span>

#### 22.3.9.1 波浪符号 分号: 子句分隔符

这个分隔 ~[ 和 ~< 构造中的子句. 在其他地方是用它的后果是未定义的. 


#### 22.3.9.2 波浪符号 抑扬符: 向上转义

~^

这是一个转义构造. 如果这里没有更多要被处理的参数剩余, 那么这个紧接着闭合的 ~{ 或 ~< 构造会被终止. 如果这里没有这样闭合的构造, 那么整个格式化操作会被终止. 在 ~< 的情况中, 这个格式化会被执行, 但是在执行这个调整之前没有更多的片段会被处理. ~^ 可能出现在一个 ~{ 构造中的任何地方.

```LISP
(setq donestr "Done.~^ ~D warning~:P.~^ ~D error~:P.")
=>  "Done.~^ ~D warning~:P.~^ ~D error~:P."
(format nil donestr) =>  "Done."
(format nil donestr 3) =>  "Done. 3 warnings."
(format nil donestr 1 5) =>  "Done. 1 warning. 5 errors."
```

如果给定了一个前缀参数, 如果这个参数是零那么就会发生终止. (因此 ~^ 等价域 ~#^.) 如果给定了两个参数, 如果它们是相等的那么发生终止. 如果给定了三个参数, 如果第一个小于等于第二个并且第二个小于等于第三个那么就会发生终止. 当然, 如果所有这些前置参数都是常数, 这是没有用的; 它们中至少一个应该是 # 或一个 V 参数.

如果 ~^ 在一个 ~:{ 构造中被使用, 那么它终止当前的循环步骤, 因为在标准情况下它只测试当前步骤的剩余参数; 下一个循环步骤立即开始. ~:^ 被用于终止这个循环过程. ~:^ 可能只有当它会终止的命令是 ~:{ 或 ~:@{ 时才使用. 当且仅当这个为当前循环步骤提供参数的子列表是 ~:{ 中的最后一个子列表时, 或者是 ~:@{ 中的最后一个 format 参数时整个循环过程终止, . ~:^ 不等价于 ~#:^; 当且仅当当前循环步骤没有参数剩余时, 后者会终止整个循环步骤. 比如:

```LISP
(format nil "~:{ ~@?~:^ ...~} " '(("a") ("b"))) =>  "a...b"
```

如果 ~^ 出现在一个 ~? 指令控制下的要被处理的控制字符串中, 但是没有在那个字符串的任何 ~{ 或 ~< 构造中, 那么这个要被处理的字符串会被终止, 因此结束这个 ~? 指令的处理. 然后在包含这个 ~? 指令的字符串中的这个指令后面继续处理.

如果 ~^ 出现在一个 ~[ 或 ~( 构造中, 那么所有直到 ~^ 的命令会被适当地选择或转换大小写, 这个 ~[ 或 ~( 处理会终止, and the outward search continues 并且这个外部搜索继续一个要被终止的 ~{ 或 ~< 构造. 例如:

```LISP
(setq tellstr "~@(~@[~R~]~^ ~A!~)")
=>  "~@(~@[~R~]~^ ~A!~)"
(format nil tellstr 23) =>  "Twenty-three!"
(format nil tellstr nil "losers") =>  " Losers!"
(format nil tellstr 23 "losers") =>  "Twenty-three losers!"
```

下面是在一个 ~< 构造中使用 ~^ 的示例.

```LISP
(format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
=>   "            FOO"
(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
=>   "FOO         BAR"
(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
=>   "FOO   BAR   BAZ"
```

#### 22.3.9.3 波浪符号 换行符: 忽略换行

波浪符号后面立即跟着一个换行符会忽略换行以及后面的非换行空白字符. 带有一个 : 的话, 换行符会被忽略, 但是任何后面的空白字符会留在原处. 带有一个 @ 的话, 换行符会留在原处, 但是任何后面的空白字符会被忽略. 例如:

```LISP
(defun type-clash-error (fn nargs argnum right-type wrong-type)
  (format *error-output*
          "~&~S requires its ~:[~:R~;~*~]~ 
          argument to be of type ~S,~%but it was called ~
          with an argument of type ~S.~%"
          fn (eql nargs 1) argnum right-type wrong-type))
(type-clash-error 'aref nil 2 'integer 'vector)  prints:
AREF requires its second argument to be of type INTEGER,
but it was called with an argument of type VECTOR.
NIL
(type-clash-error 'car 1 1 'list 'short-float)  prints:
CAR requires its argument to be of type LIST,
but it was called with an argument of type SHORT-FLOAT.
NIL
```

注意, 在这个例子中换行符只出现在由 ~& 和 ~% 指令指定的输出中; 这些在这个控制字符串中的实际换行符被抑制, 因为每一个前面都有一个波浪符号. 

### 22.3.10 <span id="AddInfoFORMATOperations">关于 FORMAT 的额外信息</span>

#### 22.3.10.1 FORMAT 操作的嵌套

这个大小写转换, 条件, 循环, 和对齐构造可以通过把其他格式化构造括在一起来包含它们. 这些构造必须和其他每一个正确地嵌套. 例如, 在条件的每个分支中放置一个大小写转换构造的开始是不合法的, 并且在条件之外放置大小写转换结构的结束是不合法的:

```LISP
(format nil "~:[abc~:@(def~;ghi~
:@(jkl~]mno~)" x) ;Invalid!
```

这个表示是非法的因为 ~[...~;...~] 和 ~(...~) 构造是不正确地嵌套.

由 ~? 指令引起的间接处理也是一种嵌套, 以达到这种正确嵌套规则的目的. 在一个 ~? 指令的控制下, 不允许在一个字符串中启动一个括号构造, 并在包含该构造的字符串的 ~? 构造之后结束这个构造, 反之亦然. 例如, 这个情况是非法的:

```LISP
(format nil "~@?ghi~)" "abc~@(def") ;Invalid!
```

这个表示是非法的因为 ~? 和 ~(...~) 构造是不正确地嵌套. 

#### 22.3.10.2 缺失的以及额外的 FORMAT 参数

如果对于一个需要一个参数的指令没有剩余参数, 那么后果是未定义的. 然而, 允许一个或更多剩下的参数没有被一个指令处理; 这样的参数 args 会被我忽略. 

#### 22.3.10.3 额外的 FORMAT 参数

如果一个格式化指令给定了比它在这里描述的可接受参数更多的参数, 那么后果是未定义的. 

#### 22.3.10.4 未定义的 FORMAT 修饰符组合

如果冒号或 at-sign 修饰符以一种没有在这里描述为有意义的组合给一个指令, 那么后果是未定义的. 

### 22.3.11 <span id="">FORMAT 的示例</span>

```LISP
(format nil "foo") =>  "foo"
(setq x 5) =>  5
(format nil "The answer is ~D." x) =>  "The answer is 5."
(format nil "The answer is ~3D." x) =>  "The answer is   5."
(format nil "The answer is ~3,'0D." x) =>  "The answer is 005."
(format nil "The answer is ~:D." (expt 47 x))
=>  "The answer is 229,345,007."
(setq y "elephant") =>  "elephant"
(format nil "Look at the ~A!" y) =>  "Look at the elephant!"
(setq n 3) =>  3
(format nil "~D item~:P found." n) =>  "3 items found."
(format nil "~R dog~:[s are~; is~] here." n (= n 1))
=>  "three dogs are here."
(format nil "~R dog~:*~[s are~; is~:;s are~] here." n)
=>  "three dogs are here."
(format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n)
=>  "Here are three puppies."

(defun foo (x)
  (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
          x x x x x x)) =>  FOO
(foo 3.14159)  =>  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159"
(foo -3.14159) =>  " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159"
(foo 100.0)    =>  "100.00|******|100.00| 100.0|100.00|100.0"
(foo 1234.0)   =>  "1234.00|******|??????|1234.0|1234.00|1234.0"
(foo 0.006)    =>  "  0.01|  0.06|  0.01| 0.006|0.01|0.006"

(defun foo (x)  
  (format nil
          "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
          ~9,3,2,-2,'%@E|~9,2E"
          x x x x))
(foo 3.14159)  =>  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0"
(foo -3.14159) =>  " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0"
(foo 1100.0)   =>  "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3"
(foo 1100.0L0) =>  "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
(foo 1.1E13)   =>  "*********| 11.00$+12|+.001E+16| 1.10E+13"
(foo 1.1L120)  =>  "*********|??????????|%%%%%%%%%|1.10L+120"
(foo 1.1L1200) =>  "*********|??????????|%%%%%%%%%|1.10L+1200"
```

作为一个可变伸缩因子影响的例子, 代码

```LISP
(dotimes (k 13)
  (format t "~%Scale factor ~2D: |~13,6,2,VE|"
          (- k 5) (- k 5) 3.14159))
```

产生以下输出:

    Scale factor -5: | 0.000003E+06|
    Scale factor -4: | 0.000031E+05|
    Scale factor -3: | 0.000314E+04|
    Scale factor -2: | 0.003142E+03|
    Scale factor -1: | 0.031416E+02|
    Scale factor  0: | 0.314159E+01|
    Scale factor  1: | 3.141590E+00|
    Scale factor  2: | 31.41590E-01|
    Scale factor  3: | 314.1590E-02|
    Scale factor  4: | 3141.590E-03|
    Scale factor  5: | 31415.90E-04|
    Scale factor  6: | 314159.0E-05|
    Scale factor  7: | 3141590.E-06|

```LISP
(defun foo (x)
  (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
          x x x x))                                     
(foo 0.0314159) =>  "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2"
(foo 0.314159)  =>  "  0.31   |0.314    |0.314    | 0.31    "
(foo 3.14159)   =>  "   3.1   | 3.14    | 3.14    |  3.1    "
(foo 31.4159)   =>  "   31.   | 31.4    | 31.4    |  31.    "
(foo 314.159)   =>  "  3.14E+2| 314.    | 314.    |  3.14E+2"
(foo 3141.59)   =>  "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3"
(foo 3141.59L0) =>  "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
(foo 3.14E12)   =>  "*********|314.0$+10|0.314E+13| 3.14E+12"
(foo 3.14L120)  =>  "*********|?????????|%%%%%%%%%|3.14L+120"
(foo 3.14L1200) =>  "*********|?????????|%%%%%%%%%|3.14L+1200"

(format nil "~10<foo~;bar~>")   =>  "foo    bar"
(format nil "~10:<foo~;bar~>")  =>  "  foo  bar"
(format nil "~10<foobar~>")     =>  "    foobar"
(format nil "~10:<foobar~>")    =>  "    foobar"
(format nil "~10:@<foo~;bar~>") =>  "  foo bar "
(format nil "~10@<foobar~>")    =>  "foobar    "
(format nil "~10:@<foobar~>")   =>  "  foobar  "

  (FORMAT NIL "Written to ~A." #P"foo.bin")
  =>  "Written to foo.bin."
```

### 22.3.12 <span id="NotesFORMAT">FORMAT 的注意事项</span>

格式化输出不仅仅通过 format 来执行, 也可以通过某些其他的接收一个和 format 使用的相同格式化控制的函数. 例如, 像 cerror 这样的发送错误的函数接收格式化控制.

注意, 给 format 作为目标的 nil 和 t 的意义和那些作为流标识符的 nil 和 t 不同.

这个 ~^ 应该只出现在一个 ~< 子句的开始, 因为它终止这个它出现的完整子句 (所有后面的子句也一样). 

## 22.4 <span id="ThePrinterDictionary">打印器的字典</span>

> * [函数 COPY-PPRINT-DISPATCH](#F-COPY-PPRINT-DISPATCH)
> * [宏 FORMATTER](#M-FORMATTER)
> * [函数 PPRINT-DISPATCH](#F-PPRINT-DISPATCH)
> * [局部宏 PPRINT-EXIT-IF-LIST-EXHAUSTED](#LM-PPRINT-EXIT-IF-LIST-EXHAUSTED)
> * [函数 PPRINT-FILL, PPRINT-LINEAR, PPRINT-TABULAR](#F-PPRINT-FILL-LINEAR-TABULAR)
> * [函数 PPRINT-INDENT](#F-PPRINT-INDENT)
> * [宏 PPRINT-LOGICAL-BLOCK](#M-PPRINT-LOGICAL-BLOCK)
> * [函数 PPRINT-NEWLINE](#F-PPRINT-NEWLINE)
> * [局部宏 PPRINT-POP](#LM-PPRINT-POP)
> * [函数 PPRINT-TAB](#F-PPRINT-TAB)
> * [标准广义函数 PRINT-OBJECT](#SGF-PRINT-OBJECT)
> * [宏 PRINT-UNREADABLE-OBJECT](#M-PRINT-UNREADABLE-OBJECT)
> * [函数 SET-PPRINT-DISPATCH](#F-SET-PPRINT-DISPATCH)
> * [函数 WRITE, PRIN1, PRINT, PPRINT, PRINC](#F-WRITE-PRIN1-PRINT-PPRINT-PRINC)
> * [函数 WRITE-TO-STRING, PRIN1-TO-STRING, PRINC-TO-STRING](#F-WRITE-PRIN1-PRINC-TO-STRING)
> * [变量 *PRINT-ARRAY*](#V-PRINT-ARRAY)
> * [变量 *PRINT-BASE*, *PRINT-RADIX*](#V-PRINT-BASE-RADIX)
> * [变量 *PRINT-CASE*](#V-PRINT-CASE)
> * [变量 *PRINT-CIRCLE*](#V-PRINT-CIRCLE)
> * [变量 *PRINT-ESCAPE*](#V-PRINT-ESCAPE)
> * [变量 *PRINT-GENSYM*](#V-PRINT-GENSYM)
> * [变量 *PRINT-LEVEL*, *PRINT-LENGTH*](#V-PRINT-LEVEL-LENGTH)
> * [变量 *PRINT-LINES*](#V-PRINT-LINES)
> * [变量 *PRINT-MISER-WIDTH*](#V-PRINT-MISER-WIDTH)
> * [变量 *PRINT-PPRINT-DISPATCH*](#V-PRINT-PPRINT-DISPATCH)
> * [变量 *PRINT-PRETTY*](#V-PRINT-PRETTY)
> * [变量 *PRINT-READABLY*](#V-PRINT-READABLY)
> * [变量 *PRINT-RIGHT-MARGIN*](#V-PRINT-RIGHT-MARGIN)
> * [状况类型 PRINT-NOT-READABLE](#CT-PRINT-NOT-READABLE)
> * [函数 PRINT-NOT-READABLE-OBJECT](#F-PRINT-NOT-READABLE-OBJECT)
> * [函数 FORMAT](#F-FORMAT)

### <span id="F-COPY-PPRINT-DISPATCH">函数 COPY-PPRINT-DISPATCH</span>

* 语法(Syntax):

        copy-pprint-dispatch &optional table => new-table

* 参数和值(Arguments and Values):

        table---一个 pprint 分派表, 或 nil.
        new-table---一个新的 pprint 分派表.

* 描述(Description):

        创建并返回指定的表 table 的一个拷贝, 如果没有指定表 table 那么就是 *print-pprint-dispatch* 的值的拷贝, 如果指定了 nil 那么就是 *print-pprint-dispatch* 初始值的拷贝.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 table 不是一个 pprint 分派表那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="M-FORMATTER">宏 FORMATTER</span>

* 语法(Syntax):

        formatter control-string => function

* 参数和值(Arguments and Values):

        control-string---一个格式化字符串; 不求值.
        function---一个函数.

* 描述(Description):

        返回一个表现和下面这个一样的函数:

            #'(lambda (*standard-output* &rest arguments)
                (apply #'format t control-string arguments)
                arguments-tail)

        如果在 control-string 中有更多格式化指令, 其中 arguments-tail 是参数 arguments 的末尾, 它有着下一个要被处理的参数作为它的 car, 如果最近处理的参数后没有更多 arguments 那么就是 nil.

* 示例(Examples):

    ```LISP
    (funcall (formatter "~&~A~A") *standard-output* 'a 'b 'c)
    >>  AB
    =>  (C)

    (format t (formatter "~&~A~A") 'a 'b 'c)
    >>  AB
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这个参数不是一个有效的格式化字符串, 那么可能会发出一个错误(在宏展开时或运行时).

* 也见(See Also):

        format

* 注意(Notes): None. 


### <span id="F-PPRINT-DISPATCH">函数 PPRINT-DISPATCH</span>

* 语法(Syntax):

        pprint-dispatch object &optional table => function, found-p

* 参数和值(Arguments and Values):

        object---一个对象.
        table---一个 pprint 分派表, 或者 nil. 默认是 *print-pprint-dispatch* 的值.
        function---一个函数标识符.
        found-p---一个广义 boolean.

* 描述(Description):

        在表 table 中检索和 object 匹配的类型指定符相关联的最高优先级函数. 这个函数通过在表 table 中查找所有和对象 object 匹配的类型指定符并且挑选和这些类型指定符中的任意一个关联的最高优先级函数来选择的. 如果这里有超过一个最高优先级函数, 会执行任意的选择. 如果没有匹配 object 的类型指定符, 会返回一个使用 print-object 来打印对象 object 的函数.

        第二个值, found-p, 如果在表 table 中找到匹配的类型指定符那么就是 true, 否则就是 false.

        如果 table 是 nil, 检索在最初的 pprint 分派表上进行.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        这个表 table 的状态.

* 异常情况(Exceptional Situations):

        如果 table 既不是 pprint-dispatch-table 也不是 nil 那么就会发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        (let ((*print-pretty* t))
          (write object :stream s))
        ==  (funcall (pprint-dispatch object) s object)

### <span id="LM-PPRINT-EXIT-IF-LIST-EXHAUSTED">局部宏 PPRINT-EXIT-IF-LIST-EXHAUSTED</span>

* 语法(Syntax):

        pprint-exit-if-list-exhausted <no arguments> => nil

* 参数和值(Arguments and Values): None.

* 描述(Description):

        检验传递给词法上当前逻辑块(lexically current logical block)的列表是否已经被耗尽; 见章节 22.2.1.1 (输出排列的动态控制). 如果这个列表已经被归约为 nil, pprint-exit-if-list-exhausted 终止这个词法上当前逻辑块的执行, 除了这个后缀的打印. 否则 pprint-exit-if-list-exhausted 返回 nil.

        pprint-exit-if-list-exhausted 在全局环境中是否为 fbound 的是依赖于具体实现的; 然而, 在这个 pprint-exit-if-list-exhausted 上的重定义和遮蔽的限制和那些在 COMMON-LISP 包中在全局环境中是 fbound 的符号是一样的. 尝试在 pprint-logical-block 外部去使用 pprint-exit-if-list-exhausted 的后果是未定义的.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在任何不是词法上在一个 pprint-logical-block 调用中的地方使用 pprint-exit-if-list-exhausted 就会发出一个错误 (在宏展开时或运行时). 同样, 在 pprint-logical-block 的动态范围之外但是词法上包含时执行 pprint-if-list-exhausted 的后果是未定义的.

* 也见(See Also):

        pprint-logical-block, pprint-pop.

* 注意(Notes): None. 


### <span id="F-PPRINT-FILL-LINEAR-TABULAR">函数 PPRINT-FILL, PPRINT-LINEAR, PPRINT-TABULAR</span>

* 语法(Syntax):

        pprint-fill stream object &optional colon-p at-sign-p => nil

        pprint-linear stream object &optional colon-p at-sign-p => nil

        pprint-tabular stream object &optional colon-p at-sign-p tabsize => nil

* 参数和值(Arguments and Values):

        stream---一个输出流标识符.
        object---一个对象.
        colon-p---一个广义 boolean. 默认是 true.
        at-sign-p---一个广义 boolean. 默认是依赖于具体实现的.
        tabsize---一个非负整数. 默认是 16.

* 描述(Description):

        函数 pprint-fill, pprint-linear, 和 pprint-tabular 指定美观打印一个列表到一个流的特定方式. 当且仅当 colon-p 为 true 时, 每个函数在输出周围打印括号. 每个函数忽略它的 at-sign-p 参数. (这两个参数都包含在内, 即使只需要一个参数, 以便这些功能可以通过 ~/.../ 和 set-pprint-dispatch 函数, 以及直接使用.) 每个函数正确地处理缩写和循环的检测和共享, 并且当它不是一个列表时使用 write 来打印对象 object.<!--TOOD 待校对-->

        如果对象 object 是一个列表并且 *print-pretty* 的值是 false, 这些符号中的每一个都用最少的空格 whitespace 来打印对象, 就像章节 22.1.3.5 (打印列表和构造(cons)) 中描述的. 否则 (如果对象 object 是一个列表并且 *print-pretty* 的值是 true):

            函数 pprint-linear 把一个列表的所有元素打印在一行中, 或者每个元素在一个分隔的行上.

            函数 pprint-fill 把一个列表尽可能多的元素打印到一行上.

            函数 pprint-tabular 和 pprint-fill 一样除了它打印的这些元素按列对齐. 这个 tabsize 以em 单位指定了列间隔, 也就是从一列的前缘到下一列的前缘的总间距.

* 示例(Examples):

        以行长为 25 求值下面这个产生的输出.

    ```LISP
    (progn (princ "Roads ") 
          (pprint-tabular *standard-output* '(elm main maple center) nil nil 8))
    Roads ELM     MAIN
          MAPLE   CENTER
    ```

* 副作用(Side Effects):

        执行输出到指定的流 stream.

* 受此影响(Affected By):

        指定的流 stream 上的游标位置, 如果它可以被确定的话.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        函数 pprint-tabular 可以按如下定义:

    ``LISP
    (defun pprint-tabular (s list &optional (colon-p t) at-sign-p (tabsize nil))
      (declare (ignore at-sign-p))
      (when (null tabsize) (setq tabsize 16))
      (pprint-logical-block (s list :prefix (if colon-p "(" "")
                                    :suffix (if colon-p ")" ""))
        (pprint-exit-if-list-exhausted)
        (loop (write (pprint-pop) :stream s)
              (pprint-exit-if-list-exhausted)
              (write-char #\Space s)
              (pprint-tab :section-relative 0 tabsize s)
              (pprint-newline :fill s))))
    ```

        请注意, 使用 format 指定这个函数会很不方便, 因为需要将 tabsize 参数传递给一个 ~:T 格式指令, 并将其嵌套在列表的迭代中. 


### <span id="F-PPRINT-INDENT">函数 PPRINT-INDENT</span>

* 语法(Syntax):

        pprint-indent relative-to n &optional stream => nil

* 参数和值(Arguments and Values):

        relative-to---:block 或 :current.
        n---一个实数.
        stream---一个输出流标识符. 默认是标准输出.

* 描述(Description):

        pprint-indent 指定了在流 stream 上的一个逻辑块中使用的缩进. 如果流 stream 是一个美观打印流并且 *print-pretty* 的值 true, pprint-indent 设置最内部的动态闭合逻辑块中的缩进; 否则, pprint-indent 没有效果.

        n 用em 单位指定了缩进. 如果 relative-to 是 :block, 这个缩进会被设置为在这个动态当前逻辑块中第一个字符的水平位置加上 n 个em 单位. 如果 relative-to 是 :current, 这个缩进被设置为当前输出位置加上 n 个em 单位. (为了面对可变宽度字体时的健壮性, 可能的情况下建议使用 :current 以及零作为一个 n.)

        N 可以是负的; 然而, 总的缩进不能移动到这行开始的左边或者最右边的每行前缀的末尾的左边---试图超越这些限制一的尝试与试图达到这个极限的尝试是一样的. 由 pprint-indent 导致的缩进的改变不会生效, 直到下一个换行符之后. 另外, 在最小执行常式模式中所有对 pprint-indent 的调用都被忽略, 强制对齐到逻辑块的行, 在块的第一个字符下面对齐.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 relative-to 是任何不是 :block 或 :current 的对象, 就会发出一个错误.

* 也见(See Also):

        章节 22.3.5.3 (Tilde I: Indent)

* 注意(Notes): None. 


### <span id="M-PPRINT-LOGICAL-BLOCK">宏 PPRINT-LOGICAL-BLOCK</span>

* 语法(Syntax):

        pprint-logical-block (stream-symbol object &key prefix per-line-prefix suffix) declaration* form*
        => nil

* 参数和值(Arguments and Values):

        stream-symbol---一个流变量标识符.
        object---一个对象; 求值的.
        :prefix---一个字符串; 求值的. 复杂的默认行为; 见下方.
        :per-line-prefix---一个字符串; 求值的. 复杂的默认行为; 见下方.
        :suffix---一个字符串; 求值的. 默认是空字符串.
        declaration---一个 declare 表达式; 不求值的.
        forms---一个隐式 progn;

* 描述(Description):

        使打印被分组到一个逻辑块中.

        这个逻辑块被打印到一个流, 这个流是由 stream-symbol 表示的变量的值. 在这些表达式形式的执行期间, 那个变量被绑定为一个美观打印流, 它支持关于输出排列的决策, 然后将输出转发到目标流. 所有标准打印函数 (例如, write, princ, 和 terpri) 都可以被用于打印输出到这个美观打印流. 所有的和只有输出到这个美观打印流的输出被视为在逻辑块中.

        这个前缀 prefix 指定了在这个逻辑块开始前要被打印的前缀. 这个行前缀 per-line-prefix 指定了在这个逻辑块以及逻辑块中的每一个新行开始之前要打印的前缀. 这个 :prefix 和 :pre-line-prefix 参数是互斥的. 如果 :prefix 和 :per-line-prefix 都没有指定, 会采取一个空字符串前缀.

        这个后缀 suffix 指定了在这个逻辑块后面打印的后缀.

        这个 object 通常是一个主体表达式形式 forms 负责打印的列表. 如果 object 不是一个列表, 它使用 write 打印. (这使得编写出现错误的参数时是健壮的打印函数变得更加容易). 如果 *print-circle* 不是 nil 并且 object 是一个引用一个 cons 的循环 (或者共享的) 引用, 那么就会打印一个适当的 "#n#" 标记. (这使得编写为循环和共享缩写提供了完整的支持的打印函数变得很容易.) 如果 *print-level* 不是 nil 并且这个逻辑块在这些逻辑块中大于 *print-level* 的动态嵌套深度中, "#" 会被打印. (这使得编写提供了对深度缩写的完全支持的打印函数很容易.)

        如果以上三个情况的任意一个发生了, 指定的输出打印在 stream-symbol 上并且随着 :prefix 和 :suffix 的打印, 主体表达式形式 forms 也被跳过了. (如果主体表达式形式 forms 不负责打印一个列表, 那么上述前两个检验可以通过为 object 参数提供 nil 来关闭.)

        除了 pprint-logical-block 的 object 参数之外, 标准打印函数的参数 (例如 write, print, prin1, 和 pprint, 例如 ~A, ~S, 和 ~W 这样的标准格式化指令的参数也一样) 都会检测循环和共享 (当有必要的时候). 然而, 这样的检测不会应用到函数 write-line, write-string, 和 write-char 的参数上, 也不会应用到由 format 输出的字面文本上. 这样做的结果是, 如果您想要在输出中打印一些字面文本, 而这些文本不支持循环性或共享检测, 那么您必须使用后者的一个函数.

       一个 pprint-logical-block 表达式形式的主体表达式形式 forms 一定不能对周围环境产生副作用; 比如, 没有在它的作用域中被绑定的变量不能被赋值.

        不管 *print-pretty* 的值是什么, 这个 pprint-logical-block 宏都可能被使用.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *print-circle*, *print-level*.

* 异常情况(Exceptional Situations):

        如果提供的 :suffix, :prefix, or :per-line-prefix 的任何一个没有被求值为一个字符串, 那么就会发出一个 type-error 类型的错误.

        如果 :prefix 和 :pre-line-prefix 都使用了就会发出一个错误.

        pprint-logical-block 和它创建的美观打印流有着动态范围. 如果在这个作用域之外尝试去输出到它创建的这个美观打印流, 那么后果是未定义的.

        如果在这个范围内直接发送任何输出到底层的目标流, 会发生什么也是不确定的.

* 也见(See Also):

        pprint-pop, pprint-exit-if-list-exhausted, 章节 22.3.5.2 (波浪符号 小于号: 逻辑块)

* 注意(Notes):

        当 *print-pretty* 的值为 nil 时使用 pprint-logical-block 宏的一个原因是允许它对点列表执行检查, (和 pprint-pop 配合) 检查 *print-level* or *print-length* 溢出也一样.

        美观打印器支持循环和共享的检测, 本质上是执行两次请求的输出. 第一次通过时, 循环和共享被检测而实际输出的字符串被抑制. 在第二次通过时, 适当的 "#n=" 和 "#n#" 标记会被插入并且字符会被输出. 就就是为什么副作用上的约束是必要的. 当遍历一个要被 pprint-logical-block 表达式形式的主体表达式形式 forms 打印的列表时, 使用 pprint-pop 来实现这一限制, 而不是普通的 pop.


### <span id="F-PPRINT-NEWLINE">函数 PPRINT-NEWLINE</span>

* 语法(Syntax):

        pprint-newline kind &optional stream => nil

* 参数和值(Arguments and Values):

        kind---:linear, :fill, :miser, 或 :mandatory 的其中之一.
        stream---一个流标识符. 默认是标准输出.

* 描述(Description):

        如果流 stream 是一个美观打印流并且 *print-pretty* 的值是 true, 当满足以下适当条件时, 一个断行会被插入到输出中; 否则, pprint-newline 没有效果.

        kind 指定了条件换行的样式. 这个参数按如下对待:

        :linear

            这个指定了一个 "线性风格(linear-style)" 条件换行. 当且仅当这个直接包含的片段没有被打印在一行中时会插入一个断行. 这个的效果是断行被插入到一个逻辑块的每一个线性风格的条件换行中或者一个也不插入.

        :miser

            这个指定了一个 "miser-style" 条件换行. 当且仅当这个直接包含的片段没有被打印在一行中并且在这个直接包含的逻辑块中这个 miser style 是生效时会插入一个断行. 这个效果是 miser-style 条件换行表现得像线性风格条件换行一样, 但是只有当 miser style 生效时. 当且仅当一个逻辑块的起始位置距离右边距小于或等于 *print-miser-width* 个 ems 单位时, 这个逻辑块的 miser style 是生效的.

        :fill

            这个指定一个 "填充风格(fill-style)" 条件换行. 当且仅当以下情况满足任意一条时, 会插入一个断行: (a) 后面的片段不会被打印在当前行的结尾, (b) 前面的片段不会被打印在一个单独的行, 或 (c) 这个直接包含的片段不能被打印在一行上并且 miser style 在这个直接包含的逻辑块中是生效的. 如果一个逻辑块被填充风格的条件换行分解成一些子片段, 其基本效果是在每一行上以尽可能多的子片段打印逻辑块. 但是, 如果 miser style 是生效的, fill-style 条件换行表现得像线性风格条件换行一样.

        :mandatory

            这个指定一个 "强制风格(mandatory-style)" 条件换行. 一个断行总是会被插入. 这个意味着这些包含的片段中没有一个可以被打印在一个单独的行并且因此会触发在这些片段中的线性风格条件换行中的断行插入.

        当一个断行通过条件换行的任何类型被插入时, 任何直接位于条件换行之前的空白会从这个输出中被省略并且在下一行的开始引入缩进. 默认情况下, 这个缩进导致下面这行从和直接包含的逻辑块的第一个字符相同的水平位置开始. (缩进不能经由 pprint-indent 被改变.)

        这里有一些把非条件换行引入到输出的方法 (换句话说, 通过 terpri 或通过打印一个包含一个换行字符的字符串). 与强制条件换行一样, 这可以防止任何包含的片段在一行上被打印出来. 通常情况下, 当遇到一个非条件换行时, 它是在没有对前面的空格的压制下打印出来的, 并且没有任何缩进. 然而, 如果指定一个行前缀 (见 pprint-logical-block), 这个前缀总是被打印, 不管一个新行是如何开始的.

* 示例(Examples):

        见章节 22.2.2 (使用美观打印器的示例).

* 副作用(Side Effects):

        输出到流 stream.

* 受此影响(Affected By):

        *print-pretty*, *print-miser*. 包含的逻辑块的出现. 换行符和条件换行符的位置.

* 异常情况(Exceptional Situations):

        如果 kind 表示 :linear, :fill, :miser, 或 :mandatory 的其中之一, 就会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 22.3.5.1 (波浪符号 下划线: 条件换行), 章节 22.2.2 (使用美观打印器的示例)

* 注意(Notes): None. 


### <span id="LM-PPRINT-POP">局部宏 PPRINT-POP</span>

* 语法(Syntax):

        pprint-pop <no arguments> => object

* 参数和值(Arguments and Values):

        object---在当前词法逻辑块中要被打印的列表的一个元素, 或者 nil.

* 描述(Description):

        从在当前词法逻辑块中要被打印的列表中弹出一个元素, 按如下所述遵循 *print-length* 和 *print-circle*.

        每次 pprint-pop 被调用, 它把传递给当前词法逻辑块的列表的下一个值弹出并返回. 但是, 在做这个之前, 它执行三个检验:

            如果剩余的 'list' 不是一个列表, ". " 在剩余的 'list' 后面被打印. (这个使得编写一个面对难看的参数是强健的打印函数变得更容易.)

            如果 *print-length* 不是 nil, 并且 pprint-pop 在直接包含的逻辑块中已经被调用 *print-length* 次, 那么 "..." 会被打印. (这使得编写一个适当处理 *print-length* 的函数变得容易.)

            如果 *print-circle* 不是 nil, 并且剩余列表是一个环状 (或者共享) 引用, 那么 ". " 在一个适当的 "#n#" 标记后打印. (这个捕捉列表中环状和共享的 cdr 实例.)

        如果发生了上面的三个状况中的任意一个, 那么表示的输出会被打印在由直接包含的 pprint-logical-block 创建的美观打印流上并且直接包含的 pprint-logical-block 会终止, 除了后缀的打印.

        如果 pprint-logical-block 被给定一个 nil 的 'list' 参数---因为它没有处理一个列表---pprint-pop 仍然可以被用来获取对 *print-length* 的支持. 在这个情况中, 上面的第一个和第三个测试会被禁用并且 pprint-pop 总是返回 nil. 见章节 22.2.2 (使用美观打印器的示例)---具体来说, 那个 pprint-vector 示例.

        pprint-pop 在全局环境中是否是 fbound 的是依赖于具体实现的; 但是, 在 pprint-pop 的重定义和遮蔽上的约束和 COMMON-LISP 包中在全局环境中被 fbound 的符号相同. 在 pprint-logical-block 外部尝试去使用 pprint-pop 的后果是 未定义的.

* 示例(Examples): None.

* 副作用(Side Effects):

        可能导致输出到和当前词法逻辑块相关联的美观打印流.

* 受此影响(Affected By):

        *print-length*, *print-circle*.

* 异常情况(Exceptional Situations):

        如果一个 pprint-pop 的使用出现在没有词法上包含在 pprint-logical-block 表达式形式的地方, 那么就会发出一个错误 (不管是宏展开时或是运行时).

        如果 pprint-pop 在这个 pprint-logical-block 的动态范围外被执行, 那么后果是未定义的.

* 也见(See Also):

        pprint-exit-if-list-exhausted, pprint-logical-block.

* 注意(Notes):

        在调用 pprint-pop 之前调用 pprint-exit-if-list-exhausted 经常是一个好办法. 


### <span id="F-PPRINT-TAB">函数 PPRINT-TAB</span>

* 语法(Syntax):

        pprint-tab kind colnum colinc &optional stream => nil

* 参数和值(Arguments and Values):

        kind--- :line, :section, :line-relative, 或 :section-relative 其中之一.
        colnum---一个非负整数.
        colinc---一个非负整数.
        stream---一个输出流标识符.

* 描述(Description):

        指定对于流 stream 的制表就像是通过标准 ~T 格式化指令执行的. 如果流 stream 是一个美观打印流并且 *print-pretty* 的值是 true, 那么制表就会被执行; 否则, pprint-tab 没有效果.

        参数 colnum 和 colinc 对应于给 ~T 的两个参数并且根据 ems 单位. 这个 kind 参数指定了 tabbing 的样式. 它必须是 :line (就如 ~T 一样制表), :section (就如 ~:T 一样制表, 但是相对于这个动态闭合部分的开始来水平测量), :line-relative (就如 ~@T 一样制表), 或 :section-relative (就如 ~:@T 一样制表, 但是相对于这个动态闭合部分的开始来水平测量) 其中之一.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 kind 不是 :line, :section, :line-relative, 或 :section-relative 其中之一, 那么就会发出一个错误.

* 也见(See Also):

        pprint-logical-block

* 注意(Notes): None. 


### <span id="SGF-PRINT-OBJECT">标准广义函数 PRINT-OBJECT</span>

* 语法(Syntax):

        print-object object stream => object

* 方法签名(Method Signatures):

        print-object (object standard-object) stream

        print-object (object structure-object) stream

* 参数和值(Arguments and Values):

        object---一个对象.
        stream---一个流.

* 描述(Description):

        广义函数 print-object 把对象 object 的打印表示写入到流 stream. 函数 print-object 是通过 Lisp 打印器调用的; 它不应该被用户调用.

        每个具体实现都需要在类 standard-object 和类 structure-object 上提供一个方法. 另外, 每一个实现必须在足够的其他类上提供方法来确保这里总是由一个可应用的方法. 具体实现可以自由地去给其他类添加方法. 用户如果不希望去继承一个依赖于具体实现的方法, 可以为它们自己的类编写 print-object 方法.

        类 structure-object 上的方法以默认的 #S 表示打印这个对象; 见章节 22.1.3.12 (打印结构体).

        print-object 上的方法有责任去实现它们的这些打印器控制变量的语义部分, 如下:

        *print-readably*

            所有 print-object 的方法必须遵循 *print-readably*. 这个包括用户定义的方法和具体实现定义的方法. 结构体和标准对象的可读打印由它们的 print-object 方法控制, 不是由它们的 make-load-form 方法. 这些对象的相似性是依赖于应用的并且因此被定义为这些方法所做的那样; 见章节 3.2.4.2 (Similarity of Literal Objects).

        *print-escape*

            每个方法必须实现 *print-escape*.

        *print-pretty*

            这个方法可能希望执行某个特殊的断行或在 *print-pretty* 的值上的其他输出条件句. 关于进一步的信息, 见 (例如) 宏 pprint-fill. 也见章节 22.2.1.4 (美观打印分派表) 和章节 22.2.2 (使用美观打印器的示例).

        *print-length*

            产生无限长度输出的方法必须遵循 *print-length*. 关于进一步的信息, 见 (例如) 宏 pprint-logical-block 和 pprint-pop. 也见章节 22.2.1.4 (美观打印分派表) 和章节 22.2.2 (使用美观打印器的示例).

        *print-level*

            打印器自动关注 *print-level*, 假设每个方法处理结构的一个层级而如果这里有更多的结构层级就递归调用 write (或者一个等价的函数). 打印器对于一个对象是否有着成员的决策是依赖于具体实现的 (并且因此当打印深度不小于 *print-level* 时不应该被打印). 在某些实现中它的 print-object 方法不会被调用; 在其他实现中这个方法会被调用, 并且这个对象有着成员的决定是基于它尝试把什么写入到这个流中.

        *print-circle*

            当 *print-circle* 的值是 true 时, 一个用户定义的 print-object 方法可以使用 write, prin1, princ, 或 format 打印对象到提供的流, 并且期望环状会被检测到并使用 #n# 语法打印. 如果一个用户定义的 print-object 方法打印到一个不是提供的那个的流, 那么对于这个流的环状检测重新开始. 见 *print-circle*.

        *print-base*, *print-radix*, *print-case*, *print-gensym*, and *print-array*

            这些打印器控制变量应用于特定对象类型并且由这些对象的方法来处理.

        如果这些规则没有被遵守, 结果就是没有定义的.

        通常情况下, 打印器和 print-object 方法不应该随着它们在结构中递归操作重新绑定这些打印控制变量, 但是这个是依赖于具体实现的.

        在某些实现中传递给一个 print-object 方法的这个 stream 参数不是原始的流, 而是一个实现了这个打印器部分的中间流. 因此方法不应该依赖于这个流的标识.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        pprint-fill, pprint-logical-block, pprint-pop, write, *print-readably*, *print-escape*, *print-pretty*, *print-length*, 章节 22.1.3 (默认 Print-Object 方法), 章节 22.1.3.12 (打印结构体), 章节 22.2.1.4 (美观打印分派表), 章节 22.2.2 (使用美观打印器的示例)

* 注意(Notes): None. 


### <span id="M-PRINT-UNREADABLE-OBJECT">宏 PRINT-UNREADABLE-OBJECT</span>

* 语法(Syntax):

        print-unreadable-object (object stream &key type identity) form* => nil

* 参数和值(Arguments and Values):

        object---一个对象; 求值的.
        stream---一个流标识符; 求值的.
        type---一个广义 boolean; 求值的.
        identity---一个广义 boolean; 求值的.
        forms---一个隐式 progn;

* 描述(Description):

        在流 stream 上输出对象 object 的打印表示, 用 "#<" 开始并且用 ">" 结束. 所有通过主体表达式形式 forms 输出到流 stream 的所有东西被闭合在一个尖括号中. 如果 type 是 true, 来自表达式形式 forms 的输出前有着这个对象 object 类型的简要描述和一个空白字符. 如果 identity 是 true, 那么这个来自表达式形式 forms 的输出后跟着一个空白字符和一个和这个对象身份的一个表示, 通常是一个存储地址.

        如果 type 或 identity 没有被提供, 它的值就是 false. 省略主体表达式形式 forms 是有效的. 如果 type 和 identity 都是 true 并且这里没有主体表达式形式 forms, 只有一个空白字符分隔着类型和身份.

* 示例(Examples):

    ```LISP
    ;; Note that in this example, the precise form of the output ;; is implementation-dependent.

    (defmethod print-object ((obj airplane) stream)
      (print-unreadable-object (obj stream :type t :identity t)
        (princ (tail-number obj) stream)))

    (prin1-to-string my-airplane)
    =>  "#<Airplane NW0773 36000123135>"
    OR=>  "#<FAA:AIRPLANE NW0773 17>"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 *print-readably* 是 true, print-unreadable-object 在不打印任何东西的情况下发出一个 print-not-readable 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-SET-PPRINT-DISPATCH">函数 SET-PPRINT-DISPATCH</span>

* 语法(Syntax):

        set-pprint-dispatch type-specifier function &optional priority table => nil

* 参数和值(Arguments and Values):

        type-specifier---一个类型指定符.
        function---一个函数, 一个函数名, 或 nil.
        priority---一个实数. 默认是 0.
        table---一个 pprint 风派表上. 默认是 *print-pprint-dispatch* 的值.

* 描述(Description):

        安装一个条目到 pprint 分派表 table.

        type-specifier 是这个条目的键. set-pprint-dispatch 的第一个动作是移除任何之前存在的和 type-specifier 关联条目. 这个保证在一个给定的 pprint 分派表中不会有和同一个类型指定符关联的两个条目. 类型指定符的等价性由 equal 确定.

        在一个 pprint 分派表中每一个类型指定符和两个值关联: 一个函数 function 和一个优先级 priority. 这个函数 function 必须接受两个参数: 要被输出的流以及要被打印的对象. 那个函数应该把对象 object 打印到那个流 stream. 这个函数 function 可以假定那个对象 object 满足由 type-specifier 给定的类型. 函数 function 遵循 *print-readably*. 任何由那个函数 function 返回的值会被忽略.

        当一个对象匹配不止一个条目时, priority 是用于解决冲突的优先级.

        允许函数 function 为 nil. 在这个情况中, 在 set-pprint-dispatch 返回后在表 table 中没有 type-specifier 条目.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 priority 不是一个实数, 就会发出一个错误.

* 也见(See Also): None.

* 注意(Notes):

        因为 pprint 分派表经常被哟你关于控制 Lisp 代码的美观打印, 对于 type-specifier 为这个形式的一个表达式是很普遍的.

    ```LISP
    (cons car-type cdr-type)
    ```

        这个表示这个对应对象必须是一个 cons cell, 它的 car 匹配类型指定符 car-type 以及它的 cdr 匹配类型指定符 cdr-type. 这个 cdr-type 可以被省略, 这个情况下它默认为 t. 


### <span id="F-WRITE-PRIN1-PRINT-PPRINT-PRINC">函数 WRITE, PRIN1, PRINT, PPRINT, PRINC</span>

* 语法(Syntax):

        write object &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin stream
        => object

        prin1 object &optional output-stream => object

        princ object &optional output-stream => object

        print object &optional output-stream => object

        pprint object &optional output-stream => <no values>

* 参数和值(Arguments and Values):

        object---一个对象.
        output-stream---一个输出流标识符. 默认是标准输出.
        array---一个广义 boolean.
        base---一个基数.
        case---一个 (member :upcase :downcase :capitalize) 类型的符号.
        circle---一个广义 boolean.
        escape---一个广义 boolean.
        gensym---一个广义 boolean.
        length---一个非负整数, 或 nil.
        level---一个非负整数, 或 nil.
        lines---一个非负整数, 或 nil.
        miser-width---一个非负整数, 或 nil.
        pprint-dispatch---一个 pprint 分派表.
        pretty---一个广义 boolean.
        radix---一个广义 boolean.
        readably---一个广义 boolean.
        right-margin---一个非负整数, 或 nil.
        stream---一个输出流标识符. 默认是标准输出.

* 描述(Description):

        write, prin1, princ, print, 和 pprint 把对象 object 的打印表示写入到输出流 output-stream 中.

        write 是到 Lisp 打印器的一般入口点. 对于显式提供的下一段中命名的每个关键字参数, 对应的打印器控制变量在打印进行时会被动态地绑定为它的值; 对于没有显式提供的下一段中的每个关键字参数, 对应打印器控制变量的值和 write 被调用时一样. 一旦确定合适的绑定, 对象 object 会被 Lisp 打印器输出.

            参数              对应的动态变量  
            array            *print-array*                   
            base             *print-base*                    
            case             *print-case*                    
            circle           *print-circle*                  
            escape           *print-escape*                  
            gensym           *print-gensym*                  
            length           *print-length*                  
            level            *print-level*                   
            lines            *print-lines*                   
            miser-width      *print-miser-width*             
            pprint-dispatch  *print-pprint-dispatch*         
            pretty           *print-pretty*                  
            radix            *print-radix*                   
            readably         *print-readably*                
            right-margin     *print-right-margin*            

            Figure 22-7. 对于 WRITE 函数的参数对应关系.

        prin1, princ, print, 和 pprint 隐式绑定某些打印参数为特定的值. 这些剩余参数值取自 *print-array*, *print-base*, *print-case*, *print-circle*, *print-escape*, *print-gensym*, *print-length*, *print-level*, *print-lines*, *print-miser-width*, *print-pprint-dispatch*, *print-pretty*, *print-radix*, 和 *print-right-margin*.

        prin1 产生适合于 read 的输入的输出. 它绑定 *print-escape* 为 true.

        princ 就像是 prin1 除了输出没有转义字符. 它绑定 *print-escape* 为 false 并且绑定 *print-readably* 为 false. 一般规则是, 来自 princ 的输出意图在于让人更好地查看, 而来自 prin1 的输出意图在于对于 read 是可接受的.

        print 就像是 prin1 除了对象 object 的打印表示前面有一个换行并且后面有空格.

        pprint 就像是 print 除了省略尾部的空格并且对象 object 使用非 nil 的 *print-pretty* 标志来打印, 进而产生美观的输出.

        output-stream 指定输出要被发送到的流.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*, *print-escape*, *print-radix*, *print-base*, *print-circle*, *print-pretty*, *print-level*, *print-length*, *print-case*, *print-gensym*, *print-array*, *read-default-float-format*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        readtable-case, 章节 22.3.4 (FORMAT 打印器操作)

* 注意(Notes):

        函数 prin1 和 print 不会绑定 *print-readably*.

        (prin1 object output-stream)
        ==  (write object :stream output-stream :escape t)

        (princ object output-stream)
        ==  (write object stream output-stream :escape nil :readably nil)

        (print object output-stream)
        ==  (progn (terpri output-stream)
                  (write object :stream output-stream
                                :escape t)
                  (write-char #\space output-stream))

        (pprint object output-stream)
        ==  (write object :stream output-stream :escape t :pretty t)


### <span id="F-WRITE-PRIN1-PRINC-TO-STRING">函数 WRITE-TO-STRING, PRIN1-TO-STRING, PRINC-TO-STRING</span>

* 语法(Syntax):

        write-to-string object &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin
        => string

        prin1-to-string object => string

        princ-to-string object => string

* 参数和值(Arguments and Values):

        object---一个对象.
        array---一个广义 boolean.
        base---一个基数.
        case---一个类型 (member :upcase :downcase :capitalize) 的符号.
        circle---一个广义 boolean.
        escape---一个广义 boolean.
        gensym---一个广义 boolean.
        length---一个非负整数, 或 nil.
        level---一个非负整数, 或 nil.
        lines---一个非负整数, 或 nil.
        miser-width---一个非负整数, 或 nil.
        pprint-dispatch---一个 pprint 分派表.
        pretty---一个广义 boolean.
        radix---一个广义 boolean.
        readably---一个广义 boolean.
        right-margin---一个非负整数, 或 nil.
        string---一个字符串.

* 描述(Description):

        write-to-string, prin1-to-string, 和 princ-to-string 被用于创建一个由对象 object 的打印表示组成的字符串. 对象 object 被有效地打印, 就像分别是通过 write, prin1, 或 princ 一样, 并且输出的字符被构成一个字符串.

        write-to-string 是一个一般的输出函数. 它有着去指定所有可应用于对象 object 打印的参数的能力.

        prin1-to-string 表现地就像是 write-to-string, 其中 :escape 为 t, 这也就是说, 转义字符会在了合适的地方被写入.

        princ-to-string 表现地就像是 write-to-string, 其中 :escape 为 nil 并且 :readably 为 nil. 因此没有转义字符会被写入.

        当 prin1-to-string 或 princ-to-string 被调用时, 所有会被指定给 write-to-string 的其他关键字是默认值.

        给 write-to-string 的关键字参数的意义和默认值与 write 的相同.

* 示例(Examples):

    ```LISP
    (prin1-to-string "abc") =>  "\"abc\""
    (princ-to-string "abc") =>  "abc"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *print-escape*, *print-radix*, *print-base*, *print-circle*, *print-pretty*, *print-level*, *print-length*, *print-case*, *print-gensym*, *print-array*, *read-default-float-format*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        write

* 注意(Notes):

        (write-to-string object {key argument}*)
        ==  (with-output-to-string (#1=#:string-stream) 
            (write object :stream #1# {key argument}*))

        (princ-to-string object)
        ==  (with-output-to-string (string-stream)
            (princ object string-stream))

        (prin1-to-string object)
        ==  (with-output-to-string (string-stream)
            (prin1 object string-stream))


### <span id="V-PRINT-ARRAY">变量 *PRINT-ARRAY*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        依赖于具体实现的.

* 描述(Description):

        控制数组被打印的格式. 如果它是 false, 除了字符串以外的数组内容不会被打印. 反而, 数组以一个使用 #< 的简洁形式来打印, 这个形式为用户提供了足够的信息来辨别这个数组, 但是不会包含整个数组内容. 如果它是 true, 非字符串数组使用 #(...), #*, 或 #nA 语法打印.

* 示例(Examples): None.

* 受此影响(Affected By):

        具体实现.

* 也见(See Also):

        章节 2.4.8.3 (Sharpsign Left-Parenthesis), 章节 2.4.8.20 (Sharpsign Less-Than-Sign)

* 注意(Notes): None. 


### <span id="V-PRINT-BASE-RADIX">变量 *PRINT-BASE*, *PRINT-RADIX*</span>

* 值类型(Value Type):

        *print-base*---a radix. *print-radix*---一个广义 boolean.

* 初始值(Initial Value):

        这个 *print-base* 的初始值是 10. 这个 *print-radix* 的初始值是 false.

* 描述(Description):

        *print-base* 和 *print-radix* 控制有理数的打印. 这个 *print-base* 的值称为当前输出基数.

        这个 *print-base* 的值是打印器打印有理数所用的基数. 对于 10 以上的基数, 字母表中的字母被用于表示 9 以上的数字.

        如果 *print-radix* 的值是 true, 那么打印器会打印一个基数指定符来表示它用于打印有理数所用的基数. 这个基数指定符总是用小写字母来打印. 如果 *print-base* 是 2, 8, 或 16, 那么使用的基数指定负分别是 #b, #o, 或 #x. 对于整数, 基数10是用一个尾部的小数点来表示而不是一个前导的基数指定符; 对于比率, 使用 #10r.

* 示例(Examples):

    ```LISP
    (let ((*print-base* 24.) (*print-radix* t)) 
      (print 23.))
    >>  #24rN
    =>  23
    (setq *print-base* 10) =>  10
    (setq *print-radix* nil) =>  NIL                                          
    (dotimes (i 35)
        (let ((*print-base* (+ i 2)))           ;print the decimal number 40 
          (write 40)                            ;in each base from 2 to 36
          (if (zerop (mod i 10)) (terpri) (format t " "))))
    >>  101000
    >>  1111 220 130 104 55 50 44 40 37 34
    >>  31 2C 2A 28 26 24 22 20 1J 1I
    >>  1H 1G 1F 1E 1D 1C 1B 1A 19 18
    >>  17 16 15 14 
    =>  NIL
    (dolist (pb '(2 3 8 10 16))               
        (let ((*print-radix* t)                 ;print the integer 10 and 
              (*print-base* pb))                ;the ratio 1/10 in bases 2, 
        (format t "~&~S  ~S~%" 10 1/10)))        ;3, 8, 10, 16
    >>  #b1010  #b1/1010
    >>  #3r101  #3r1/101
    >>  #o12  #o1/12
    >>  10.  #10r1/10
    >>  #xA  #x1/A
    =>  NIL
    ```

* 受此影响(Affected By):

        可能被 format, 和 write, write-to-string 绑定.

* 也见(See Also):

        format, write, write-to-string

* 注意(Notes): None. 


### <span id="V-PRINT-CASE">变量 *PRINT-CASE*</span>

* 值类型(Value Type):

        符号 :upcase, :downcase, 或 :capitalize 其中之一.

* 初始值(Initial Value):

        符号 :upcase.

* 描述(Description):

        这个 *print-case* 的值控制当没有使用竖杠语法时符号名字中的任何大写字符要被打印的大小写 (大写, 小写, 或混合的).

        当 *print-escape* 的值是 false 时, *print-case* 在所有时间都有效. 当 *print-escape* 的值是 true 时 *print-case* 也有效, 除非在一个转义上下文中 (换句话说, 除非在竖杠之间或一个斜杠之后).

* 示例(Examples):

    ```LISP
    (defun test-print-case ()
      (dolist (*print-case* '(:upcase :downcase :capitalize))
        (format t "~&~S ~S~%" 'this-and-that '|And-something-elSE|)))
    =>  TEST-PC
    ;; Although the choice of which characters to escape is specified by
    ;; *PRINT-CASE*, the choice of how to escape those characters 
    ;; (i.e., whether single escapes or multiple escapes are used)
    ;; is implementation-dependent.  The examples here show two of the
    ;; many valid ways in which escaping might appear.
    (test-print-case) ;Implementation A
    >>  THIS-AND-THAT |And-something-elSE|
    >>  this-and-that a\n\d-\s\o\m\e\t\h\i\n\g-\e\lse
    >>  This-And-That A\n\d-\s\o\m\e\t\h\i\n\g-\e\lse
    =>  NIL
    (test-print-case) ;Implementation B
    >>  THIS-AND-THAT |And-something-elSE|
    >>  this-and-that a|nd-something-el|se
    >>  This-And-That A|nd-something-el|se
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write

* 注意(Notes):

        read 通常将出现在符号中的小写字符转换为大写字符, 这样一来内容的打印名字通常只包含大写字符.

        如果 *print-escape* 是 true, 一个符号名字中的小写字符总是用小写打印, 并且前面有单转义字符或被多转义字符围绕; 根据 *print-case* 的值, 一个符号名字中的大写字符可能会用大写, 小写或混合的方式来打印, 以便使单词首字母大写. 关于构成一个 "单词(word)" 的惯例和 string-capitalize 一样. 


### <span id="V-PRINT-CIRCLE">变量 *PRINT-CIRCLE*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        false.

* 描述(Description):

        控制尝试在一个要被打印的对象中检测环状和共享.

        如果是 false, 打印过程仅仅是通过递归的下降来进行, 而不需要检测环状和共享.

        如果是 true, 打印器将努力检测出要被打印的结构中的环和共享, 并且使用 #n= 和 #n# 语法来表示环或共享成员.

        如果是 true, 一个用户定义的 print-object 方法可以使用 write, prin1, princ, 或 format 打印对象到提供的流中并且期望使用 #n# 语法检测和打印环和共享. 如果一个用户定义的 print-object 方法打印到一个流而不是提供的那个, 那么环检测从这个流开始.

        注意, 当 Lisp 读取器会自动确保没有它的共享时, 具体实现不应该使用 #n# 标记 (例如, as happens with interned symbols).

* 示例(Examples):

    ```LISP
    (let ((a (list 1 2 3)))
      (setf (cdddr a) a)
      (let ((*print-circle* t))
        (write a)
        :done))
    >>  #1=(1 2 3 . #1#)
    =>  :DONE
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write

* 注意(Notes):

        在 *print-circle* 设置为 nil 的情况下尝试去打印一个环状结构可能导致循环行为并且不能终止. 


### <span id="V-PRINT-ESCAPE">变量 *PRINT-ESCAPE*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        true.

* 描述(Description):

        如果是 false, 在打印一个表达式时, 转义字符和包前缀不会被输出.

        如果是 true, 尝试以一种它可以被再次读取以产生一个 equal 表达式的方式打印一个表达式. (这只是一个指导纲要; 不是必须的. 见 *print-readably*.)

        关于 *print-escape* 的值如何影响特定类型的打印的更具体信息, 见章节 22.1.3 (默认 Print-Object 方法).

* 示例(Examples):

    ```LISP
    (let ((*print-escape* t)) (write #\a))
    >>  #\a
    =>  #\a
    (let ((*print-escape* nil)) (write #\a))
    >>  a
    =>  #\a
    ```

* 受此影响(Affected By):

        princ, prin1, format

* 也见(See Also):

        write, readtable-case

* 注意(Notes):

        princ 实际上绑定 *print-escape* 为 false. prin1 实际上绑定 *print-escape* 为 true. 


### <span id="V-PRINT-GENSYM">变量 *PRINT-GENSYM*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        true.

* 描述(Description):

        控制在明显未捕捉的符号前面是否打印前缀 "#:". 当且仅当这个 *print-gensym* 的值是 true 时, 在这样一个符号之前打印这个前缀.

* 示例(Examples):

    ```LISP
    (let ((*print-gensym* nil))
      (print (gensym)))
    >>  G6040 
    =>  #:G6040
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write, *print-escape*

* 注意(Notes): None. 


### <span id="V-PRINT-LEVEL-LENGTH">变量 *PRINT-LEVEL*, *PRINT-LENGTH*</span>

* 值类型(Value Type):

        一个非负整数, 或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        *print-level* 控制一个嵌套对象打印多少层级深度. 如果它是 false, 那么不会执行控制. 否则, 它就是一个表示要被打印的最大层级的整数. 一个要被打印的对象处于 0 层级; 它的成员 (就像是在一个列表或向量中) 处于 1 层级; 以此类推. 如果一个要被递归打印的对象有着大于等于 *print-level* 的值的层级, 那么这个对象被打印为 "#".

        *print-length* 控制在一个给定的层级下多少个元素会被打印. 如果它是 false, 那么就没有打印成员数量上的限制. 否则, 它就是一个表示要被打印的对象的最大元素数的整数. 如果超过了, 这个打印器会打印 "..." 来替换其他元素. 在点对列表的情况下, 如果这个列表包含了和 *print-length* 的值一样多的元素, 终止的基元会被打印而不是打印 "..."

        *print-level* 和 *print-length* 影响任何用类列表语法打印的对象的打印. 它们不会影响符号, 字符串, 和位向量的打印.

* 示例(Examples):

    ```LISP
    (setq a '(1 (2 (3 (4 (5 (6))))))) =>  (1 (2 (3 (4 (5 (6))))))
    (dotimes (i 8) 
      (let ((*print-level* i)) 
        (format t "~&~D -- ~S~%" i a)))
    >>  0 -- #
    >>  1 -- (1 #)
    >>  2 -- (1 (2 #))
    >>  3 -- (1 (2 (3 #)))
    >>  4 -- (1 (2 (3 (4 #))))
    >>  5 -- (1 (2 (3 (4 (5 #)))))
    >>  6 -- (1 (2 (3 (4 (5 (6))))))
    >>  7 -- (1 (2 (3 (4 (5 (6))))))
    =>  NIL

    (setq a '(1 2 3 4 5 6)) =>  (1 2 3 4 5 6)
    (dotimes (i 7) 
      (let ((*print-length* i)) 
        (format t "~&~D -- ~S~%" i a))) 
    >>  0 -- (...)
    >>  1 -- (1 ...)
    >>  2 -- (1 2 ...)
    >>  3 -- (1 2 3 ...)
    >>  4 -- (1 2 3 4 ...)
    >>  5 -- (1 2 3 4 5 6)
    >>  6 -- (1 2 3 4 5 6)
    =>  NIL

    (dolist (level-length '((0 1) (1 1) (1 2) (1 3) (1 4) 
                          (2 1) (2 2) (2 3) (3 2) (3 3) (3 4)))
    (let ((*print-level*  (first  level-length))
          (*print-length* (second level-length)))
      (format t "~&~D ~D -- ~S~%"
              *print-level* *print-length* 
              '(if (member x y) (+ (car x) 3) '(foo . #(a b c d "Baz"))))))
    >>  0 1 -- #
    >>  1 1 -- (IF ...)
    >>  1 2 -- (IF # ...)
    >>  1 3 -- (IF # # ...)
    >>  1 4 -- (IF # # #)
    >>  2 1 -- (IF ...)
    >>  2 2 -- (IF (MEMBER X ...) ...)
    >>  2 3 -- (IF (MEMBER X Y) (+ # 3) ...)
    >>  3 2 -- (IF (MEMBER X ...) ...)
    >>  3 3 -- (IF (MEMBER X Y) (+ (CAR X) 3) ...)
    >>  3 4 -- (IF (MEMBER X Y) (+ (CAR X) 3) '(FOO . #(A B C D ...)))
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write

* 注意(Notes): None. 


### <span id="V-PRINT-LINES">变量 *PRINT-LINES*</span>

* 值类型(Value Type):

        一个非负整数, 或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        当这个 *print-lines* 的值是除了 nil 以外的值时, 当某个东西要被美观打印时, 它是产生的输出行数的限制. 如果尝试去超出那么多行, ".." 会被打印在最后一行末尾, 后面跟着所有挂起待打印的后缀 (关闭分隔符) that are pending to be printed.

* 示例(Examples):

    ```LISP
    (let ((*print-right-margin* 25) (*print-lines* 3))
      (pprint '(progn (setq a 1 b 2 c 3 d 4))))
    >>  (PROGN (SETQ A 1
    >>               B 2
    >>               C 3 ..))
    =>  <no values>
    ```

* 也见(See Also): None.

* 注意(Notes):

        这个 ".." 标记是有意和用于级别缩写的 "..." 标记不同的, 这样一来这两个不同的情况可以被可见地区分.

        这个符号用于增加 Lisp 读取器如果尝试读取缩略输出时发出错误的可能性. 但是注意这个截断如果发生在一个字符串中, 比如在 "This string has been trunc.." 中, 以后不会检测到问题情况, 也不会出现此类错误. 


### <span id="V-PRINT-MISER-WIDTH">变量 *PRINT-MISER-WIDTH*</span>

* 值类型(Value Type):

        一个非负整数, 或 nil.

* 初始值(Initial Value):

        依赖于具体实现的

* 描述(Description):

        如果它不是 nil, 那么当打印子结构的宽度小于或等于这个许多em 单位时, 这个美观打印器切换到紧凑输出风格(称为 miser 风格).

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes): None.



### <span id="V-PRINT-PPRINT-DISPATCH">变量 *PRINT-PPRINT-DISPATCH*</span>

* 值类型(Value Type):

        一个 pprint 分派表.

* 初始值(Initial Value):

        依赖于具体实现的, 但是这些初始条目都使用了一个特殊的优先级, 它们有着小于所有可以用 set-pprint-dispatch 指定的优先级的属性, 这样一来任何条目的初始内容可以被重写.

* 描述(Description):

        这个 pprint 分派表控制当前美观打印器.

* 示例(Examples): None.

* 也见(See Also):

        *print-pretty*, 章节 22.2.1.4 (美观打印分派表)

* 注意(Notes):

        其目的是, 该变量的初始值应该导致"传统"的代码美观打印. 一般而言, 然而, 你可以在 *print-pprint-dispatch* 放置一个值来使美观打印输出看起来像是非美观打印输出. 设置 *print-pretty* 为 true 只会导致当前 pprint 分派表中的这些函数有着超过普通 print-object 方法的优先级; 没有神奇的方法来强制执行这些函数实际上产生了美观的输出. 关于详细信息, 见章节 22.2.1.4 (美观打印分派表). 


### <span id="V-PRINT-PRETTY">变量 *PRINT-PRETTY*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        依赖于具体实现的.

* 描述(Description):

        控制 Lisp 打印器是否调用美观打印器.

        如果它是 false, 美观打印器不会被使用并且当打印一个表达式时最小数量的空格会被输出.

        如果它是 true, 美观打印器会被使用, 并且 Lisp 打印器会尽可能的在可以使表达式变得更加可读的地方去插入额外的空格 whitespace.

        即便当 *print-escape* 的值是 false 时, *print-pretty* 也是有效的.

* 示例(Examples):

    ```LISP
    (setq *print-pretty* 'nil) =>  NIL
    (progn (write '(let ((a 1) (b 2) (c 3)) (+ a b c))) nil)
    >>  (LET ((A 1) (B 2) (C 3)) (+ A B C))
    =>  NIL
    (let ((*print-pretty* t))
      (progn (write '(let ((a 1) (b 2) (c 3)) (+ a b c))) nil))
    >>  (LET ((A 1)
    >>        (B 2)
    >>        (C 3))
    >>    (+ A B C))
    =>  NIL
    ;; Note that the first two expressions printed by this next form
    ;; differ from the second two only in whether escape characters are printed.
    ;; In all four cases, extra whitespace is inserted by the pretty printer.
    (flet ((test (x)
              (let ((*print-pretty* t))
                (print x)
                (format t "~%~S " x)
                (terpri) (princ x) (princ " ")
                (format t "~%~A " x))))
      (test '#'(lambda () (list "a" #'c #'d))))
    >>  #'(LAMBDA ()
    >>      (LIST "a" #'C #'D))
    >>  #'(LAMBDA ()
    >>      (LIST "a" #'C #'D))
    >>  #'(LAMBDA ()
    >>      (LIST a b 'C #'D)) 
    >>  #'(LAMBDA ()
    >>      (LIST a b 'C #'D))
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write

* 注意(Notes): None. 


### <span id="V-PRINT-READABLY">变量 *PRINT-READABLY*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        false.

* 描述(Description):

        如果 *print-readably* 是 true, 打印对象的一些特殊规则生效. 具体来说, 打印任何对象 O1 会产生一个打印表示, 当标准读取表生效时并且被 Lisp 读取器见到时, 会产生一个和 O1 相似的对象 O2. 产生的打印表示可能和 *print-readably* 是 false 时产生的打印表示一样, 也可能不一样. 如果打印一个可读对象是不可能的, 就会发出一个 print-not-readable 类型的错误而不是使用一个对于相同实现不可读的语法 (比如, "#<" 语法). 如果其他一些打印器控制变量的值是这样的, 那么这些需求就会被违背, 那么这个其他变量的值就会被忽略 If the value of some other printer control variable is such that these requirements would be violated, the value of that other variable is ignored.

        具体的说, 如果 *print-readably* 是 true, 打印就进行, 就好像 *print-escape*, *print-array*, 和 *print-gensym* 也是 true, 以及就好像 *print-length*, *print-level*, 和 *print-lines* 是 false.

        如果 *print-readably* 是 false, 正常的打印规则和其他打印器控制变量的正常解释规则生效.

        单独的 print-object 方法, 包括用户定义的方法, 有责任去实现这些需要.

        如果 *read-eval* 是 false 并且 *print-readably* 是 true, 任何这样会输出对 "#." 读取器宏的引用的方法, 要么输出其他的东西, 要么会发出错误的信号 (就像上面描述的).

* 示例(Examples):

    ```LISP
    (let ((x (list "a" '\a (gensym) '((a (b (c))) d e f g)))
          (*print-escape* nil)
          (*print-gensym* nil)
          (*print-level* 3)
          (*print-length* 3))
      (write x)
      (let ((*print-readably* t))
        (terpri)
        (write x)
        :done))
    >>  (a a G4581 ((A #) D E ...))
    >>  ("a" |a| #:G4581 ((A (B (C))) D E F G))
    =>  :DONE

    ;; This is setup code is shared between the examples
    ;; of three hypothetical implementations which follow.
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32005763> 
    (setf (gethash table 1) 'one) =>  ONE
    (setf (gethash table 2) 'two) =>  TWO

    ;; Implementation A
    (let ((*print-readably* t)) (print table))
    Error: Can't print #<HASH-TABLE EQL 0/120 32005763> readably.

    ;; Implementation B
    ;; No standardized #S notation for hash tables is defined, 
    ;; but there might be an implementation-defined notation.
    (let ((*print-readably* t)) (print table))
    >>  #S(HASH-TABLE :TEST EQL :SIZE 120 :CONTENTS (1 ONE 2 TWO))
    =>  #<HASH-TABLE EQL 0/120 32005763>

    ;; Implementation C
    ;; Note that #. notation can only be used if *READ-EVAL* is true.
    ;; If *READ-EVAL* were false, this same implementation might have to
    ;; signal an error unless it had yet another printing strategy to fall
    ;; back on.
    (let ((*print-readably* t)) (print table))
    >>  #.(LET ((HASH-TABLE (MAKE-HASH-TABLE)))
    >>      (SETF (GETHASH 1 HASH-TABLE) ONE)
    >>      (SETF (GETHASH 2 HASH-TABLE) TWO)
    >>      HASH-TABLE)
    =>  #<HASH-TABLE EQL 0/120 32005763>
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        write, print-unreadable-object

* 注意(Notes):

        "相似性(similarity)" 规则意味着 #A 或 #( 语法不能被用于元素类型除了 t 以外的数组. 一个实现将不得不使用另一种语法或者发出一个 print-not-readable 类型的错误. 


### <span id="V-PRINT-RIGHT-MARGIN">变量 *PRINT-RIGHT-MARGIN*</span>

* 值类型(Value Type):

        一个非负整数, 或 nil.

* 初始值(Initial Value):

        nil.

* 描述(Description):

        如果它不是 nil, 当美观打印器要做布局决定时, 它指定了要使用的右边距 (em 单位的整形数字).

        如果它是 nil, 正确的边距被认为是最大行长度, 这样输出就可以显示, 而不需要包绕或截断. 如果这个没有被确定, 使用一个依赖于具体实现的值.

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes):

        这个度量是用em 单位, 为了与具体实现定义的可变宽度字体兼容, 同时不要求语言提供对字体的支持. 


### <span id="CT-PRINT-NOT-READABLE">状况类型 PRINT-NOT-READABLE</span>

* 类优先级列表(Class Precedence List):

        print-not-readable, error, serious-condition, condition, t

* 描述(Description):

        当 *print-readably* 为 true 时, 类型 print-not-readable 由输出期间发生的错误状况组成, 是尝试去用 Lisp 打印器写入一个不能被 Lisp 读取器读回的打印表示的结果. 这个不能被打印的对象通过给 make-condition 的 :object 初始化参数来初始化, 并且可以通过函数 print-not-readable-object 访问.

* 也见(See Also):

        print-not-readable-object 


### <span id="F-PRINT-NOT-READABLE-OBJECT">函数 PRINT-NOT-READABLE-OBJECT</span>

* 语法(Syntax):

        print-not-readable-object condition => object

* 参数和值(Arguments and Values):

        condition---一个 print-not-readable 类型的状况.
        object---一个对象.

* 描述(Description):

        返回在状况 condition 所表示的情况中不能被可读地打印的对象.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        print-not-readable, 章节 9 (Conditions)

* 注意(Notes): None. 


### <span id="F-FORMAT">函数 FORMAT</span>

* 语法(Syntax):

        format destination control-string &rest args => result

* 参数和值(Arguments and Values):

        destination---nil, t, 一个流, 或者一个带有填充指针的字符串.
        control-string---一个格式化控制.
        args---对于 control-string 的格式化参数.
        result---如果 destination 不是 nil, 那么就是 nil; 否则, 一个字符串.

* 描述(Description):

        format 通过输出 control-string 的字符并观察到一个波浪字符引入了一个指令来生成格式化的输出. 波浪符号后面的字符, 可能前面有前缀参数和和修饰符, 指定了想要的格式化种类. 大部分指令使用 args 中的一个或多个元素来创建它们的输出.

        如果 destination 是一个字符串, 一个流, 或 t, 那么结果是 nil. 否则, 结果是一个包含 'output' 的字符串.

        format 用于产生良好格式化的文本, 产生美观的信息是很有用的, 依次类推. format 可以生成并返回一个字符串或者输出到 destination 中.

        关于 control-string 任何被解释的详细信息, 见章节 22.3 (格式化输出).

* 示例(Examples): None.

* 受此影响(Affected By):

        *standard-output*, *print-escape*, *print-radix*, *print-base*, *print-circle*, *print-pretty*, *print-level*, *print-length*, *print-case*, *print-gensym*, *print-array*.

* 异常情况(Exceptional Situations):

        如果 destination 是一个带有填充指针的字符串, 如果在这个调用的动态范围内直接在这个字符串上执行破坏性的修改, 那么后果是未定义的.

* 也见(See Also):

        write, 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None.