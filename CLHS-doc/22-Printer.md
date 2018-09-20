# 22 打印器

> * 22.1 [Lisp 打印器](#TheLispPrinter)
> * 22.2 [Lisp 美观打印器](#TheLispPrettyPrinter)
> * 22.3 [Formatted Output](#FormattedOutput)
> * 22.4 [The Printer Dictionary](#ThePrinterDictionary)


## 22.1 <span id="TheLispPrinter">Lisp 打印器</span>

> * 22.1.1 [Lisp 打印器概览](#OverviewTheLispPrinter)
> * 22.1.2 [打印器分派](#PrinterDispatching)
> * 22.1.3 [默认 Print-Object 方法](#DefaultPrintObjectMethods)
> * 22.1.4 [打印器行为的示例](#ExamplesPrinterBehavior)

### 22.1.1 <span id="OverviewTheLispPrinter">Lisp 打印器概览</span>

Common Lisp 以打印文本的形式提供了大多数对象的表示形式, 称为打印表示. 像 print 这样的函数接受一个对象并且把它的打印表示的字符发送给一个流. 做这些的例程集合被称为(Common Lisp)打印器.

读取一个打印表示通常产生一个和原始打印对象 equal 的对象.

#### 22.1.1.1 多种可能的文本表示

大部分对象有着超过一个可能的文本表示. 比如, 带有 27 大小的正整数在文本上可以被表示为这些方式的任意一种:

    27    27.    #o33    #x1B    #b11011    #.(* 3 3 3)    81/3

一个包含两个符号 A 和 B 的列表也在文本上被表示为好几种方式:

    (A B)    (a b)    (  a  b )    (\A |B|) 
    (|\A|
      B
    )

通常情况下, 从 Lisp 读取器的视角看, 在一个文本表示允许空白字符的地方, 任意数量的空格和换行可以出现在标准语法中.

当一个比如 print 这样的函数产生一个打印表示时, 它必须从几个可能的文本表示中选择. 在大部分情况下, 它选择一个程序可读的表示, 但在某些情况下, 它可能使用更紧凑的符号, 而不是程序可读的.

提供了许多被称之为打印器控制变量的选项变量来允许控制对象的打印表示的各个方面. 下一段中展示了标准打印器控制变量; 这里也可能存在具体实现定义的打印器控制变量.

    *print-array*   *print-gensym*       *print-pprint-dispatch*  
    *print-base*    *print-length*       *print-pretty*           
    *print-case*    *print-level*        *print-radix*            
    *print-circle*  *print-lines*        *print-readably*         
    *print-escape*  *print-miser-width*  *print-right-margin*     

    Figure 22-1. 标准打印器控制变量

除了打印器控制变量之外, 下面的附加已定义名称与 Lisp 打印器的行为相关或影响:

    *package*                    *read-eval*  readtable-case  
    *read-default-float-format*  *readtable*                  

    Figure 22-2. 对 Lisp 打印器的额外影响.

##### 22.1.1.1.1 打印器转义

变量 \*print-escape* 控制 Lisp 打印器是否尝试去产生诸如转义字符和包前缀这类的标记.

当程序可读的输出特别重要时, 变量 \*print-readably* 可以被用于重写由其他打印器控制变量控制的各个方面.

使 \*print-readably* 的值为 true 的诸多影响中的一个是 Lisp 打印器表现地就好像 \*print-escape* 也是 true 一样. 为了表示方便, 我们说如果 \*print-readably* 或 \*print-escape* 任意一个值是 true, 那么打印器转义就是 "启用的"; 并且我们说如果 \*print-readably* 和 \*print-escape* 的值都是 false, 那么打印器转义就是 "禁用的". 

### 22.1.2 <span id="PrinterDispatching">打印器分派</span>

Lisp 打印器决定如何打印一个对象, 如下所示:

如果 \*print-pretty* 的值是 true, 打印由当前的 pprint 分派表(current pprint dispatch table)控制; 见章节 22.2.1.4 (美观打印分派表).

否则 (如果 \*print-pretty* 的值是 false), 使用对象的 print-object 方法; 见章节 22.1.3 (默认 Print-Object 方法). 

### 22.1.3 <span id="DefaultPrintObjectMethods">默认 Print-Object 方法</span>

这一章节描述了 对于标准类型的 print-object 方法的默认行为.

> * 22.1.3.1 [打印数字](#PrintingNumbers)
> * 22.1.3.2 [打印字符](#PrintingCharacters)
> * 22.1.3.3 [打印符号](#PrintingSymbols)
> * 22.1.3.4 [打印字符串](#PrintingStrings)
> * 22.1.3.5 [打印列表和 cons](#PrintingListsConses)
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

整数是用当前的输出基在位置符号中指定的基数来打印的, 首先是最重要的数字 Integers are printed in the radix specified by the current output base in positional notation, most significant digit first.<!--TODO 待校验--> 如果合适的话, 可以打印出一个基数说明符; 见 \*print-radix*. 如果一个整数是负的, 会打印一个负号还有那个整数的绝对值. 整数零会被表示为单个数字 0 并且不会有符号. 一个小数点可能被打印出来, 取决于 \*print-radix* 的值.

关于一个整数的语法相关信息, 见章节 2.3.2.1.1 (Syntax of an Integer). 


##### 22.1.3.1.2 <span id="PrintingRatios">打印比率</span>

比率按如下打印: 打印分子的绝对值, as for an integer<!--TODO 待翻译-->; 然后是一个 /; 再是分母. 分子和分母都以当前输出基指定的基数打印; 它们就像是通过 numerator 和 denominator 获取到的, 所以比率以简化形式打印(最低项). 如果合适的话, 可以打印出一个基数说明符; 见 \*print-radix*. 如果这个比率是负的, 在分子前会打印一个负号.

关于一个比率的语法相关信息, 见章节 2.3.2.1.2 (Syntax of a Ratio). 


##### 22.1.3.1.3 <span id="PrintingFloats">打印浮点数</span>

如果这个浮点数的大小是零或在 10^-3 (包含的) 和 10^7 (不包含的) 之间, 它会被打印为这个数字的整数部分, 然后一个小数点, 然后是这个数字的小数部分; 小数点的每一边都至少有一个数字. 如果这个数字的符号 (由 float-sign 确定) 是负的, 那么会在这个数字之前打印一个负号. 如果这个数字的格式不匹配由 \*read-default-float-format* 指定的, 那么打印出该格式的指数标记和数字 0. 例如, 自然对数作为一个短的浮点数可以被打印成 2.71828S0.

对于范围 10^-3 到 10^7 之外的非零大小, 一个浮点数以计算机科学计数法打印的. 这个数字的表示被缩放到 1 (包含的) 和 10 (不包含的) 之间然后打印, 其中小数点前一位数, 小数点后至少有一个位. 接下来是打印这个格式的指数标记, 但是如果数字的格式与 \*read-default-float-format* 指定的格式相匹配, 那么就使用 E. 最后, 一个 10 的幂被打印成一个十进制整数, 这个小数和这个幂相乘的结果等于原始数字. 例如, 阿伏伽德罗数作为一个短浮点数会被打印为 6.02S23.

关于一个浮点数的语法相关信息, 见章节 2.3.2.2 (Syntax of a Float). 

##### 22.1.3.1.4 <span id="PrintingComplexes">打印复数</span>

一个复数被打印为一个 #C, 一个开圆括号, 它的实部的打印表示, 一个空格, 它的虚部的打印表示, 以及最后的闭圆括号.

关于一个复数的相关语法, 见章节 2.3.2.3 (Syntax of a Complex) and Section 2.4.8.11 (Sharpsign C). 


##### 22.1.3.1.5 <span id="NotePrintingNumbers">关于打印数字的注意事项</span>

一个数字的打印表示一定不能包含转义字符; 见章节 2.3.1.1.1 (Escape Characters and Potential Numbers). 


#### 22.1.3.2 <span id="PrintingCharacters">打印字符</span>

当打印器转义被禁用时, 一个字符被打印为它的自身; 它被直接发送给输出流. 当打印器转义被启用时, 那么使用 #\ 语法.

当打印器打印输出一个字符的名字时, 它使用和 #\ 读取器宏相同的表; 因此输出的任何字符作为输入都是可接受的 (在那个实现中). 如果一个非图形化字符有着一个标准化名字, 这个名字比非标准的名字更喜欢用 #\ 符号打印. 对于图形化标准字符, 这个字符自身被用于 #\ 标记打印---即便这个字符也有一个名字.

关于 #\ 读取器宏的详细信息, 见章节 2.4.8.1 (Sharpsign Backslash). 

#### 22.1.3.3 <span id="PrintingSymbols">打印符号</span>

当打印器转义被禁用时, 只有那个符号名字的字符会被输出 (但是打印名字中的字符的大小写由 \*print-case* 控制; 见章节 22.1.3.3.2 (Lisp 打印器上读取表大小写的影响)).

章节 22.1.3.3 的其余部分仅在启用打印器转义时才适用.

当打印一个符号时, 打印器插入足够的单转义 和/或 多转义字符 (反斜线 和/或 竖线) 这样一来如果用相同的 \*readtable* 以及绑定到当前输出基数的 \*read-base* 来调用 read, 它会返回相同的符号 (如果它没有被显式解除捕捉的话) 或者一个带有相同打印名的未捕捉符号 (否则的话).

比如, 当打印符号 face 时如果 \*print-base* 的值是 16, 它可能会被打印为 \FACE 或 \Face 或 |FACE|, 因为如果 \*read-base* 的值是 16 那么记号 face 会被读取为一个十六进制数字 (数字值 64206).

对于当前读取表中具有非标准语法类型字符的附加限制, 见变量 \*print-readably*

关于 Lisp 读取器如何解析符号的信息, 见章节 2.3.4 (Symbols as Tokens) 和章节 2.4.8.5 (Sharpsign Colon).

当 \*print-pretty* 是 true 并且打印器转义是启用时, nil 可能被打印为 () .

> * 22.1.3.3.1 [符号的包前缀](#PackagePrefixesSymbols)
> * 22.1.3.3.2 [Lisp](#EffectReadtableCase)

##### 22.1.3.3.1 <span id="PackagePrefixesSymbols">符号的包前缀</span>

如果必要的话, 包前缀会被打印. 包前缀的规则如下. 当这个符号被打印时, 如果它在 KEYWORD 包, 那么它和一个前置的冒号一起被打印; 否则, 如果它在当前包中可访问, 它被打印为不带包前缀的; 否则, 它会被打印为带有一个包前缀的.

如果 \*print-gensym* 是 true 并且打印器转义是启用的, 那么一个被显式解除绑定的符号会以 "#:" 前置的方式被打印; 如果 \*print-gensym* 是 false 或打印器转义被禁用, 那么符号会以不带前缀的方式打印, 就好像它是在当前包中的.

由于 #: 语法不会捕捉后面的符号, 如果 \*print-circle* 是 true 并且相同的未捕捉的符号在一个要被打印的表达式中出现多次, 有必要去使用循环列表的语法. 比如, 如果 \*print-circle*  是 false, 下面这个的结果

    (let ((x (make-symbol "FOO"))) (list x x))

会被打印为 (#:foo #:foo), 如果 \*print-circle* 是 true, 那么就是 (#1=#:foo #1#).

前面的包前缀规则的总结如下:

foo:bar

    当符号 bar 在它的 home 包 foo 中是外部的并且在当前包中不可访问的时, 打印 foo:bar.

foo::bar

    当 bar 在它的 home 包 foo 中是内部的并且在当前包中不可访问时, 打印 foo::bar.

:bar

    当 bar 的 home 包是 KEYWORD 包时, 打印 :bar.

#:bar

    当 bar 被显式解除绑定, 即使在病态的情况下, bar 没有 home 包, 但在当前的包中却可以访问的, 都会打印 #:bar. 


##### 22.1.3.3.2 <span id="EffectReadtableCase">Lisp 打印器上读取表大小写的影响</span>

当打印器转义被禁用, 或正在考虑的字符还没有被单转义或多转义语法引用时, 当前读取表的读取表大小写以以下方式影响 Lisp 打印器写入符号的方式:

:upcase

    当读取表大小写是 :upcase 时, 大写字符由 *print-case* 指定的大小写来打印, 而小写字符按照它们自身的大小写打印.

:downcase

    当读取表大小写是 :downcase 时, 大写字符按照它们自身的大小写打印, 而小写字符由 *print-case* 指定的大小写来打印.

:preserve

    当读取表大小写是 :preserve 时, 所有字母字符都按照它们自身的大小写打印.

:invert

    当读取表大小写是 :invert 时, 单一大小写符号名中的所有字符的大小写会被转换. 混合大小写符号名会保留原样打印.

如果启用了打印器转义, 那么转义符号名中的字母字符的规则受 readtable-case 影响. 字母字符按如下转义:

:upcase

    当读取表大小写是 :upcase 时, 所有小写字符必须被转义.

:downcase

    当读取表大小写是 :downcase 时, 所有大写字符必须被转义.

:preserve

    当读取表大小写是 :preserve 时, 没有字母字符需要被转义.

:invert

    当读取表大小写是 :invert 时, 没有字母字符需要被转义.

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

字符串中的字符会被依次输出. 如果启用了打印器转义, 一个双引号会在之前或之后被输出, 并且所有的双引号和单转义前面会有一个反斜线. 字符串的打印不受 \*print-array* 的影响. 只有字符串中的有效元素会被打印.

关于 Lisp 读取器如何解析字符串的信息, 见章节 2.4.5 (Double-Quote). 

#### 22.1.3.5 <span id="PrintingListsConses">打印列表和 cons</span>

只要有可能, 列表记号优先于点记号 Wherever possible, list notation is preferred over dot notation. 因此以下运算法则被用于打印一个 cons x:

1. 打印一个左圆括号.

2. 打印 x 的 car.

3. 如果 x 的 cdr 自身是一个 cons, 它会成为当前的 cons (换句话说, x 变为那个 cons), 打印一个空格, 然后再次进入步骤 2.

4. 如果 x 的 cdr 不为空, 打印一个空格, 一个点, 一个空格, 以及 x 的 cdr.

5. 打印一个右圆括号.

事实上, 只有当 \*print-pretty* 是 false 时, 使用以上运算法则. 当 \*print-pretty* 是 true 时(或者当使用 pprint 时), 额外的空格可能替换一个单空格的使用, 并且一种更复杂的, 具有类似的目标, 但具有更多的表示灵活性的算法会被使用; 见章节 22.1.2 (打印器分派).

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

关于 Lisp 读取器如何解析列表和 cons 的信息, 见章节 2.4.1 (Left-Parenthesis). 

#### 22.1.3.6 <span id="PrintingBitVectors">打印位向量</span>

一个位向量被打印为 #* 后面依次跟着位向量的各个位. 如果 \*print-array* 是 false, 那么位向量以一种简洁但是不可读取的格式 (使用 #<) 来打印. 只有位向量中的有效元素会被打印.

关于 Lisp 读取器如何解析位向量的信息, 见章节 2.4.8.4 (Sharpsign Asterisk). 


#### 22.1.3.7 <span id="PrintingOtherVectors">打印其他向量</span>

如果 \*print-array* 是 true 并且 \*print-readably* 是 false, 除了字符串和位向量以外的向量会用普通向量语法来打印; 这意味着关于特殊向量表示的信息不会出现. 零长度向量的打印表示是 #(). 非零长度的向量用 #( 开始打印. 在那个后面, 向量的第一个元素会被打印. 如果这里有任何其他元素, 它们会依次被打印, 如果 \*print-pretty* 是 false 那么每一个另外的元素前面会有一个空格 space, 如果 \*print-pretty* 是 true 那么就是空格 whitespace. 最后一个元素后的右圆括号终止这个向量的打印表示. 向量的打印受 \*print-level* 和 \*print-length* 的影响. 如果这个向量有着填充指针, 那么只有那些在填充指针下的元素会被打印.

如果 \*print-array* 和 \*print-readably* 都是 false, 向量不会按如上所述被打印, 而是以一种简洁但不可读的格式 (using #<) 来打印.

如果 \*print-readably* 是 true, 那么向量以一种具体实现定义的方式来打印; 见变量 \*print-readably*.

关于 Lisp 读取器如何解析"其他变量"的信息, 见章节 2.4.8.3 (Sharpsign Left-Parenthesis). 

#### 22.1.3.8 <span id="PrintingOtherArrays">打印其他数组</span>

如果 \*print-array* 是 true 并且 \*print-readably* 是 false, 除了向量以外的任何数组都用 #nA 格式打印. 让 n 为这个数组的维数. 接着打印 #, 再是十进制整数 n, 然后是 A, 然后 n 个开括号. 接下来这些元素按照行优先的顺序被扫描, 在每一个元素上使用 write, 并且元素之间用空格 whitespace 分隔. 数组的维度从左到右被计为 0 到 n-1, and are enumerated with the rightmost index changing fastest<!--TODO 待翻译-->. 每次维度 j 的索引递增, 就会采取以下动作:

* 如果 j < n-1, 那么打印一个闭圆括号.

* 如果递增维度 j 的索引导致它和维度 j 相等, 那个索引被重置为零并且维度 j-1 的索引会递增 (因此递归执行这三个步骤), 除非 j=0, 在这个情况下终止整个运算法则. 如果递增维度 j 的索引没有导致它等于维度 j, 那么打印一个空格 space.<!--TODO 待校验-->

* 如果 j < n-1, 打印一个开圆括号.

这个导致要被打印的内容以一种适合于给 make-array 的 :initial-contents 参数的格式来打印. 这个过程有效打印的列表会被 \*print-level* and \*print-length* 截断.

如果这个数组是特定类型的, 包含位或者字符, 那么由上述运算法则生成的最内部的列表可以用位向量或字符串的语法来打印, 假定这些最内部的列表不会受限于 \*print-length* 的截断.

如果 \*print-array* 和 \*print-readably* 都是 false, 那么数组以一种简洁但不可读的方式 (使用 #<) 打印.

如果 \*print-readably* 是 true, 这个数组以一种具体实现定义的方式打印; 见变量 \*print-readably*. 具体来说, 这对于 0 维数组来说是很重要的.

关于 Lisp 读取器如何解析这些"其他数组"的信息, 见章节 2.4.8.12 (Sharpsign A).

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

没有为打印 random-state 类型的对象指定具体的语法. 然而, 每个具体实现必须以这样一种方式来安排打印随机对象, 在相同实现中, read 可以从这个打印表示来构造这个随机状态的一个拷贝, 就好像这个拷贝是通过 make-random-state 创建的一样.

如果这个随机状态类型被实际上是被 defstruct 来有效实现的, 那么平常的那个结构体语法可以被用于打印随机状态对象; 一个可能看上去像下面这样

    #S(RANDOM-STATE :DATA #(14 49 98436589 786345 8734658324 ... ))

其中的成员是依赖于具体实现的. 

#### 22.1.3.11 <span id="PrintingPathnames">打印路径名</span>

当启用打印器转义时, 语法 #P"..." 是路径名如何被 write 和这里描述的函数打印的语法. 这个 "..." 这个路径名的名称字符串表示.

当禁用打印器转义时, write 通过写入 (namestring P) 来写入一个路径名 P.

关于 Lisp 读取器如何解析路径名的信息, 见章节 2.4.8.14 (Sharpsign P). 


#### 22.1.3.12 <span id="PrintingStructures">打印结构体</span>

默认情况下, 一个类型 S 的结构体使用 #S 语法打印. 这个行为可以通过给定义 S 的 defstruct 表达式形式指定一个 :print-function 或 :print-object 选项, 或者通过写一个为 S 类型的对象特化的 print-object 方法来定制.

不同的结构体可能以不同的方式打印; 结构体的默认表示法是:

    #S(structure-name {slot-key slot-value}*)

其中 #S 表示结构体语法, structure-name 是一个结构体名字, 每一个 slot-key 是这个结构体中一个槽的初始化参数名, 而每一个对应的 slot-value 那个槽中的对象的一个表示.

关于 Lisp 如何解析结构体的信息, 见章节 2.4.8.13 (Sharpsign S). 

#### 22.1.3.13 <span id="PrintingOtherObjects">打印其他对象</span>

其他对象以一种依赖于具体实现的方式被打印. 一个具体实现以可读的方式打印那些对象不是必须的.

比如, 哈系表, 读取表, 包, 流, 和函数可能不会以可读的方式打印.

在这个情况中使用的普遍表示是 #<...>. 由于 #< 不是通过 Lisp 读取器可读的, 它遵循的文本准确格式是不重要的, 但是一个要去使用的普遍格式是由 print-unreadable-object 宏提供的.

关于 Lisp 读取器如何对待这种表示的信息, 见章节 2.4.8.20 (Sharpsign Less-Than-Sign). 关于如果表示不能被可读地打印的对象的信息, 见章节 2.4.8.6 (Sharpsign Dot).

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

美观打印器提供的工具允许程序重新定义代码显示的方式, 并且允许美观打印的全部能力应用于复杂的数据结构组合.

任何给定的输出样式实际上是否是"美观"的, 本质上是一个主观的问题. 然而, 由于美观打印器的效果可以通过符合标准的程序进行定制, 所以为单个程序提供了必要的灵活性以达到任意程度的审美控制.

通过直接访问美观打印器内的机制来对布局做出动态决策, 这些宏以及函数 pprint-logical-block, pprint-newline, 和 pprint-indent 使得为产生输出的任意函数指定美观打印布局规则是可能的. 它们也使得环状和共享的检测以及要被函数支持的基于长度和嵌套深度的缩写变得简单.

美观打印器完全是根据 \*print-pprint-dispatch* 的值来驱动的. 函数 set-pprint-dispatch 使得符合规范的程序去关联美观打印函数和一个类型是可能的.

> * 22.2.1.1 [输出排列的动态控制](#DynamicControlArrangeOutput)
> * 22.2.1.2 [格式指令接口](#FormatDirectiveInterface)
> * 22.2.1.3 [编译格式字符串](#CompilingFormatStrings)
> * 22.2.1.4 [美观打印分派表](#PrettyPrintDispatchTables)
> * 22.2.1.5 [美观打印器边距](#PrettyPrinterMargins)

#### 22.2.1.1 <span id="DynamicControlArrangeOutput">输出排列的动态控制</span>
<!--TODO 待校验-->
当一个输出太大而无法容纳到可用空间时, 美观打印器的操作可以被精确地控制. 三个概念是这些操作工作的基础---逻辑块(logical blocks), 条件换行(conditional newlines), 以及节段(sections). 在继续之前, 定义这些术语是很重要的.

下一段中的第一行展示了输出的示意图片段. 输出的每个字符都用 "-" 表示. 条件换行的位置由数字表示. 这个逻辑块的开始和结束分别由 "<" 和 ">" 表示.

输出作为一个整体是一个逻辑块和最外层节段. 这节段由 Figure 1 的第二行中的 0 表示. 嵌套在这个输出中的逻辑块通过宏 pprint-logical-block 来指定. 条件换行的位置由 pprint-newline 调用来指定. 每个条件换行定义了两个节段 (一个在它之前, 一个在它之后) 并且和一个第三个关联 (直接包含它的那个节段).

一个条件换行后面的节段由这些组成: 所有输出到, 但不包括, (a) 在同一个逻辑块中直接包含的下一个条件换行; 或者如果 (a) 是不可应用的, (b) 嵌套在逻辑块的更低级别的下一个换行; 或者如果 (b) 是不可应用的, (c) 输出的末尾.

一个条件换行前面的节段由这些组成: 所有的输出都返回到, 但不包括, (a) 在同一个逻辑块中直接包含的前面的条件换行; 或者如果 (a) 不可应用, (b) 直接包含逻辑块的开头. Figure 1 中的最后四行表示四个条件换行前后的节段.

直接包含一个条件换行的节段是包含这个条件换行节段中最短的那个. 在下一段中, 第一个条件换行被直接包含在 0 标记的节段中, 第二个和第三个条件换行被直接包含在第四个条件换行之前的节段中, 并且第五个条件换行被直接包含在第一个条件换行之后的节段中.

    <-1---<--<--2---3->--4-->->
    000000000000000000000000000
    11 111111111111111111111111
              22 222
                  333 3333
            44444444444444 44444

    Figure 22-3. 逻辑块, 条件换行, 和节段的示例

只要有可能, 美观打印器在一行中显示一个节段的完整内容. 然而, 如果该节段太长, 无法容纳在可用空间, 则在该节段内的条件换行位置插入换行符. 


#### 22.2.1.2 <span id="FormatDirectiveInterface">格式指令接口</span>

动态确定输出排列的操作的主要接口是通过美观打印器的函数和宏提供的. 下一段展示了和美观打印相关的已定义的名字.

    *print-lines*            pprint-dispatch                pprint-pop           
    *print-miser-width*      pprint-exit-if-list-exhausted  pprint-tab           
    *print-pprint-dispatch*  pprint-fill                    pprint-tabular       
    *print-right-margin*     pprint-indent                  set-pprint-dispatch  
    copy-pprint-dispatch     pprint-linear                  write                
    format                   pprint-logical-block                                
    formatter                pprint-newline                                      

    Figure 22-4. 和美观打印相关的已定义的名字.

下一个段标识了一组格式指令, 它们以更简洁的形式作为相同的美观打印操作的替代接口.

    ~I   ~W      ~<...~:>  
    ~:T  ~/.../  ~_        

    Figure 22-5. 和美观打印相关的格式化指令


#### 22.2.1.3 <span id="CompilingFormatStrings">编译格式字符串</span>

格式化字符串本质上是一个用于执行打印的特殊目的语言的程序, 并且可以通过函数 format 来解释. formatter 宏提供了使用编译后的函数来完成相同的打印的效率, 但是不会丢失格式化字符串的文本紧凑性.

一个格式化控制是一个格式化字符串或一个由 formatter 宏返回的函数. 


#### 22.2.1.4 <span id="PrettyPrintDispatchTables">美观打印分派表</span>

一个美观打印分派表(pprint dispatch table) 是一个从键到值对的映射. 每一个键是一个类型指定符. 和一个键关联的值是一个 "函数" (具体的说, 一个函数标识符或 nil) 和一个 "优先级数值" (具体的说, 一个 real). 基本的插入和检索是基于由 equal 测试的键等价的键之上来完成的.

当 \*print-pretty* 是 true 时, 当前的美观打印分派表 (在 \*print-pprint-dispatch* 中) 控制对象如何被打印. 这个表中的信息优先于指定如何打印对象的所有其他机制. 特别是, 它有着超过用户定义的 print-object 方法的优先级, 因为这个当前美观打印分派表是被优先考虑的.

通过在当前美观打印分派表中查找和那个对象匹配的类型指定符相关联的最高优先级的函数来选择这个函数; 如果这里有不止一个这样的函数, 哪一个被使用是依赖于具体实现的.

然而, 如果在这个表中没有关于如何美观打印一个特定种类对象的信息, 就会调用一个使用 print-object 来打印这个对象的函数. 当这个函数被调用时, \*print-pretty* 的值始终为 true, 并且对于 print-object 的个别方法可能仍然会选择以 \*print-pretty* 的值为条件的特殊格式输出. 


#### 22.2.1.5 <span id="PrettyPrinterMargins">美观打印器边距</span>

美观打印的主要目标是将输出保持在两个边距之间. 输出开始的列取自左边距. 如果当前列不能在输出开始时确定, 这个左边距被假定为零. 右边距由 \*print-right-margin* 控制. 


### 22.2.2 <span id="ExamplesPrettyPrinter">使用美观打印器的示例</span>

作为逻辑块, 条件换行, 和缩进相互作用的一个例子, 细想以下函数 simple-pprint-defun. 这个函数用标准的方式打印 car 为 defun 的列表, 假定这些列表的长度都是 4.

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

假设一个按照如下求值:

    ```LISP
    (simple-pprint-defun *standard-output* '(defun prod (x y) (* x y)))
    ```

如果可用的行宽大于等于 26, 那么所有输出会出现在一行中. 如果可用的行宽减少到 25, 在表达式 (* x y) 之前，在线性风格的条件换行中插入一条换行符, 产生这个输出展示. 这个 (pprint-indent :block 1) 导致 (* x y) 被打印在这个逻辑块中 1 个相对缩进.

    ```LISP
    (DEFUN PROD (X Y) 
      (* X Y))
    ```

如果可用的行宽是 15, 在参数列表之前, 在填充样式的条件换行中插入一个换行符. 这个在 (pprint-indent :current 0) 上的调用导致参数列表排列在函数名字下.

    ```LISP
    (DEFUN PROD
          (X Y)
      (* X Y))
    ```

如果 \*print-miser-width* 大于等于 14, 上面输出示例会是如下这样, 因为在最小执行常式(miser)模式下所有缩进的改变会被忽略并且换行被插入到最小执行常式的条件换行中.

    ```LISP
    (DEFUN
      PROD
      (X Y)
      (* X Y))
    ```

作为每行前缀的一个例子, 细想行宽为 20 并且 \*print-miser-width* 是 nil 的情况下求值以下产生的输出.

    ```LISP
    (pprint-logical-block (*standard-output* nil :per-line-prefix ";;; ")
      (simple-pprint-defun *standard-output* '(defun prod (x y) (* x y))))
    
    ;;; (DEFUN PROD
    ;;;        (X Y)
    ;;;   (* X Y))
    ```

作为一个更加复杂 (并且真实) 示例, 细想下面的函数 pprint-let. 这个指定如何去用传统风格打印一个 let 表达式形式. 它比上面的例子更复杂, 因为它不得不去处理嵌套的结构. 同样的, 不像上面的例子, 它包含去可读地打印任何以符号 let 开始的可能的列表的完整代码. 最外部的 pprint-logical-block 表达式形式处理作为整体的输入列表的打印并且指定应该在输出中打印的圆括号. 第二个 pprint-logical-block 表达式形式处理绑定对的列表. 在列表中的每一对自身通过最内部的 pprint-logical-block 打印. (使用 loop 表达式形式, 而不是将这个对分解为两个对象, 这样一来不管这个对对应的列表有着一个元素, 两个元素, 或(难看的)不止两个元素都会产生可读的输出). 会在每一个对除了最后一个之外的后面放置一个空格和一个填充风格的条件换行. 在最顶层 pprint-logical-block 表达式形式末尾的循环打印在这个 let 表达式形式的主体中被空格和线性风格的条件换行分隔的表达式形式.

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

如果行宽大于等于 77, 这个产生的输出出现在一行中. 然而, 如果行宽是 76, 换行符会被插入在分隔这些表达式形式的线性风格的条件换行中并且产生下面这样的输出. 注意, 退化的结合对 x 是可读的即使它不是一个列表; 一个深度缩写标记会被打印来替换 (g 3); 这个绑定对 (z . 2) 会被可读地打印即便它不是一个 proper 列表; 并打印缩写的环状标记.

    ```LISP
    #1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) 
          (SETQ X (SQRT Z))
          #1#)
    ```

如果行宽被减少到 35, 一个换行符会被插入到这些分隔绑定对的填充风格的条件换行中的一个.

    ```LISP
    #1=(LET (X (*PRINT-PRETTY* (F #))
              (Z . 2) (K (CAR Y)))
          (SETQ X (SQRT Z))
          #1#)
    ```

假设这个行宽进一步减少到 22 并且 \*print-length* 被设置为 3. 在这个情况中, 换行符被插入到第一个和第二个绑定对后面. 另外, 第二个绑定对自身被断开为两行. 填充风格的条件换行的描述中的子句 (b) (见函数 pprint-newline) 避免了绑定对 (z . 2) 被打印在第三行末尾. 注意, 长度缩写从视图上隐藏了环并且因此环标记的打印消失了.

    ```LISP
    (LET (X
          (*PRINT-LENGTH*
            (F #))
          (Z . 2) ...)
      (SETQ X (SQRT Z))
      ...)
    ```

下面这个函数打印了一个使用 "#(...)" 标识的向量.

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

在行宽为 15 时求值下面这个的输出展示.

    ```LISP
    (pprint-vector *standard-output* '#(12 34 567 8 9012 34 567 89 0 1 23))
    
    #(12 34 567 8 
      9012 34 567 
      89 0 1 23)
    ```

作为用格式化字符串指定美观打印的便利的示例, 细想上面被用作示例的函数 simple-pprint-defun 和 pprint-let 可以按如下简洁地定义. (函数 pprint-vector 不能使用 format 来定义, 因为遍历的数据结构不是一个列表.)

    ```LISP
    (defun simple-pprint-defun (*standard-output* list)
      (format T "~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>" list))

    (defun pprint-let (*standard-output* list)
      (format T "~:<~W~^~:<~@{~:<~@{~W~^~_~}~:>~^~:_~}~:>~1I~@{~^~_~W~}~:>" list)) 
    ```

在下面的示例中, 第一个表达式形式把 \*print-pprint-dispatch* 复原为它的等价初始值. 后面两个表达式形式 接下来的两种表达式形式为美观打印比率设置了一种特殊的方式. 注意, 更具体的类型指定符会和更高的优先级关联.

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

下面两个表达式形式阐述了关于代码类型的美观打印函数的定义. 第一个表达式形式阐述了如何为使用单引号引用的对象指定传统方法. 注意, 确保以 quote 开头的数据列表将被可读地打印出来. 第二个表达式形式指定了以符号 my-let 开始的列表应该和初始 pprint 分派表生效时以 let 开始的列表打印方式相同.

```LISP
 (set-pprint-dispatch '(cons (member quote)) () 
   #'(lambda (s list)
       (if (and (consp (cdr list)) (null (cddr list)))
          (funcall (formatter "'~W") s (cadr list))
          (pprint-fill s list))))
 
 (set-pprint-dispatch '(cons (member my-let)) 
                      (pprint-dispatch '(let) nil))
```

下一个示例为打印没有和函数调用对应的列表指定一个默认方法. 注意, 函数 pprint-linear, pprint-fill, 和 pprint-tabular 都用可选的 colon-p 和 at-sign-p 参数定义, 这样一来它们可以被用作 pprint 分派函数, 和 ~/.../ 函数一样.

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

结构体 family 的美观打印函数指定了如何去调整输出的布局以至于它可以很好地适应各种行的宽度. 另外, 它服从打印器控制变量 \*print-level*, \*print-length*, \*print-lines*, \*print-circle* 和 \*print-escape*, 并且可以容忍数据结构中几种不同的多样性. 下面的输出展示了在右边距为 25, \*print-pretty* 是 true, \*print-escape* 是 false, 以及一个多样的 kids 列表时什么会被打印出来.

    ```LISP
    (write (list 'principal-family
                  (make-family :mom "Lucy"
                              :kids '("Mark" "Bob" . "Dan")))
            :right-margin 25 :pretty T :escape nil :miser-width nil)
    (PRINCIPAL-FAMILY
      #<Lucy and
          Mark Bob . Dan>)
    ```

注意, 一个结构体的美观打印函数和结构体的 print-object 方法是不同的. print-object 方法永久地和一个结构体关联, 美观打印函数是被存储在 pprint 分派表中并且可以迅速地改变来反映不同的打印需求. 如果在当前的 pprint 分派表中没有一个结构体的美观打印函数, 就会使用它的 print-object 方法. 

### 22.2.3 <span id="NotesPrettyPrinterBackground">美观打印器背景的注意事项</span>

有关本节中详细描述的抽象概念的背景引用, 见 XP: A Common Lisp Pretty Printing System. 这篇论文的细节对这份文件没有约束力, 但可能有助于建立理解这种资料的概念基础. 


## 22.3 <span id="">Formatted Output</span>

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

#### 22.3.4.1 Tilde A: Aesthetic

An arg, any object, is printed without escape characters (as by princ). If arg is a string, its characters will be output verbatim. If arg is nil it will be printed as nil; the colon modifier (~:A) will cause an arg of nil to be printed as (), but if arg is a composite structure, such as a list or vector, any contained occurrences of nil will still be printed as nil.

~mincolA inserts spaces on the right, if necessary, to make the width at least mincol columns. The @ modifier causes the spaces to be inserted on the left rather than the right.

~mincol,colinc,minpad,padcharA is the full form of ~A, which allows control of the padding. The string is padded on the right (or on the left if the @ modifier is used) with at least minpad copies of padchar; padding characters are then inserted colinc characters at a time until the total width is at least mincol. The defaults are 0 for mincol and minpad, 1 for colinc, and the space character for padchar.

~A binds *print-escape* to false, and *print-readably* to false. 

#### 22.3.4.2 Tilde S: Standard

This is just like ~A, but arg is printed with escape characters (as by prin1 rather than princ). The output is therefore suitable for input to read. ~S accepts all the arguments and modifiers that ~A does.

~S binds *print-escape* to t. 


#### 22.3.4.3 Tilde W: Write

An argument, any object, is printed obeying every printer control variable (as by write). In addition, ~W interacts correctly with depth abbreviation, by not resetting the depth counter to zero. ~W does not accept parameters. If given the colon modifier, ~W binds *print-pretty* to true. If given the at-sign modifier, ~W binds *print-level* and *print-length* to nil.

~W provides automatic support for the detection of circularity and sharing. If the value of *print-circle* is not nil and ~W is applied to an argument that is a circular (or shared) reference, an appropriate #n# marker is inserted in the output instead of printing the argument. 


### 22.3.5 <span id="FORMATPrettyPrinterOperations">FORMAT 美观打印器操作</span>

The following constructs provide access to the pretty printer:

#### 22.3.5.1 Tilde Underscore: Conditional Newline

Without any modifiers, ~_ is the same as (pprint-newline :linear). ~@_ is the same as (pprint-newline :miser). ~:_ is the same as (pprint-newline :fill). ~:@_ is the same as (pprint-newline :mandatory). 


#### 22.3.5.2 Tilde Less-Than-Sign: Logical Block

~<...~:>

If ~:> is used to terminate a ~<...~>, the directive is equivalent to a call to pprint-logical-block. The argument corresponding to the ~<...~:> directive is treated in the same way as the list argument to pprint-logical-block, thereby providing automatic support for non-list arguments and the detection of circularity, sharing, and depth abbreviation. The portion of the control-string nested within the ~<...~:> specifies the :prefix (or :per-line-prefix), :suffix, and body of the pprint-logical-block.

The control-string portion enclosed by ~<...~:> can be divided into segments ~<prefix~;body~;suffix~:> by ~; directives. If the first section is terminated by ~@;, it specifies a per-line prefix rather than a simple prefix. The prefix and suffix cannot contain format directives. An error is signaled if either the prefix or suffix fails to be a constant string or if the enclosed portion is divided into more than three segments.

If the enclosed portion is divided into only two segments, the suffix defaults to the null string. If the enclosed portion consists of only a single segment, both the prefix and the suffix default to the null string. If the colon modifier is used (i.e., ~:<...~:>), the prefix and suffix default to "(" and ")" (respectively) instead of the null string.

The body segment can be any arbitrary format string. This format string is applied to the elements of the list corresponding to the ~<...~:> directive as a whole. Elements are extracted from this list using pprint-pop, thereby providing automatic support for malformed lists, and the detection of circularity, sharing, and length abbreviation. Within the body segment, ~^ acts like pprint-exit-if-list-exhausted.

~<...~:> supports a feature not supported by pprint-logical-block. If ~:@> is used to terminate the directive (i.e., ~<...~:@>), then a fill-style conditional newline is automatically inserted after each group of blanks immediately contained in the body (except for blanks after a <Newline> directive). This makes it easy to achieve the equivalent of paragraph filling.

If the at-sign modifier is used with ~<...~:>, the entire remaining argument list is passed to the directive as its argument. All of the remaining arguments are always consumed by ~@<...~:>, even if they are not all used by the format string nested in the directive. Other than the difference in its argument, ~@<...~:> is exactly the same as ~<...~:> except that circularity detection is not applied if ~@<...~:> is encountered at top level in a format string. This ensures that circularity detection is applied only to data lists, not to format argument lists.

" . #n#" is printed if circularity or sharing has to be indicated for its argument as a whole.

To a considerable extent, the basic form of the directive ~<...~> is incompatible with the dynamic control of the arrangement of output by ~W, ~_, ~<...~:>, ~I, and ~:T. As a result, an error is signaled if any of these directives is nested within ~<...~>. Beyond this, an error is also signaled if the ~<...~:;...~> form of ~<...~> is used in the same format string with ~W, ~_, ~<...~:>, ~I, or ~:T.

See also Section 22.3.6.2 (Tilde Less-Than-Sign: Justification). 


#### 22.3.5.3 Tilde I: Indent

~nI is the same as (pprint-indent :block n).

~n:I is the same as (pprint-indent :current n). In both cases, n defaults to zero, if it is omitted. 


#### 22.3.5.4 Tilde Slash: Call Function

~/name/

User defined functions can be called from within a format string by using the directive ~/name/. The colon modifier, the at-sign modifier, and arbitrarily many parameters can be specified with the ~/name/ directive. name can be any arbitrary string that does not contain a "/". All of the characters in name are treated as if they were upper case. If name contains a single colon (:) or double colon (::), then everything up to but not including the first ":" or "::" is taken to be a string that names a package. Everything after the first ":" or "::" (if any) is taken to be a string that names a symbol. The function corresponding to a ~/name/ directive is obtained by looking up the symbol that has the indicated name in the indicated package. If name does not contain a ":" or "::", then the whole name string is looked up in the COMMON-LISP-USER package.

When a ~/name/ directive is encountered, the indicated function is called with four or more arguments. The first four arguments are: the output stream, the format argument corresponding to the directive, a generalized boolean that is true if the colon modifier was used, and a generalized boolean that is true if the at-sign modifier was used. The remaining arguments consist of any parameters specified with the directive. The function should print the argument appropriately. Any values returned by the function are ignored.

The three functions pprint-linear, pprint-fill, and pprint-tabular are specifically designed so that they can be called by ~/.../ (i.e., ~/pprint-linear/, ~/pprint-fill/, and ~/pprint-tabular/). In particular they take colon and at-sign arguments. 


### 22.3.6 <span id="FORMATLayoutControl">FORMAT 布局控制</span>

#### 22.3.6.1 Tilde T: Tabulate

This spaces over to a given column. ~colnum,colincT will output sufficient spaces to move the cursor to column colnum. If the cursor is already at or beyond column colnum, it will output spaces to move it to column colnum+k*colinc for the smallest positive integer k possible, unless colinc is zero, in which case no spaces are output if the cursor is already at or beyond column colnum. colnum and colinc default to 1.

If for some reason the current absolute column position cannot be determined by direct inquiry, format may be able to deduce the current column position by noting that certain directives (such as ~%, or ~&, or ~A with the argument being a string containing a newline) cause the column position to be reset to zero, and counting the number of characters emitted since that point. If that fails, format may attempt a similar deduction on the riskier assumption that the destination was at column zero when format was invoked. If even this heuristic fails or is implementationally inconvenient, at worst the ~T operation will simply output two spaces.

~@T performs relative tabulation. ~colrel,colinc@T outputs colrel spaces and then outputs the smallest non-negative number of additional spaces necessary to move the cursor to a column that is a multiple of colinc. For example, the directive ~3,8@T outputs three spaces and then moves the cursor to a ``standard multiple-of-eight tab stop'' if not at one already. If the current output column cannot be determined, however, then colinc is ignored, and exactly colrel spaces are output.

If the colon modifier is used with the ~T directive, the tabbing computation is done relative to the horizontal position where the section immediately containing the directive begins, rather than with respect to a horizontal position of zero. The numerical parameters are both interpreted as being in units of ems and both default to 1. ~n,m:T is the same as (pprint-tab :section n m). ~n,m:@T is the same as (pprint-tab :section-relative n m). 


#### 22.3.6.2 Tilde Less-Than-Sign: Justification

~mincol,colinc,minpad,padchar<str~>

This justifies the text produced by processing str within a field at least mincol columns wide. str may be divided up into segments with ~;, in which case the spacing is evenly divided between the text segments.

With no modifiers, the leftmost text segment is left justified in the field, and the rightmost text segment is right justified. If there is only one text element, as a special case, it is right justified. The : modifier causes spacing to be introduced before the first text segment; the @ modifier causes spacing to be added after the last. The minpad parameter (default 0) is the minimum number of padding characters to be output between each segment. The padding character is supplied by padchar, which defaults to the space character. If the total width needed to satisfy these constraints is greater than mincol, then the width used is mincol+k*colinc for the smallest possible non-negative integer value k. colinc defaults to 1, and mincol defaults to 0.

Note that str may include format directives. All the clauses in str are processed in order; it is the resulting pieces of text that are justified.

The ~^ directive may be used to terminate processing of the clauses prematurely, in which case only the completely processed clauses are justified.

If the first clause of a ~< is terminated with ~:; instead of ~;, then it is used in a special way. All of the clauses are processed (subject to ~^, of course), but the first one is not used in performing the spacing and padding. When the padded result has been determined, then if it will fit on the current line of output, it is output, and the text for the first clause is discarded. If, however, the padded text will not fit on the current line, then the text segment for the first clause is output before the padded text. The first clause ought to contain a newline (such as a ~% directive). The first clause is always processed, and so any arguments it refers to will be used; the decision is whether to use the resulting segment of text, not whether to process the first clause. If the ~:; has a prefix parameter n, then the padded text must fit on the current line with n character positions to spare to avoid outputting the first clause's text. For example, the control string

 "~%;; ~{ ~<~%;; ~1:; ~S~>~^ ,~} .~%"

can be used to print a list of items separated by commas without breaking items over line boundaries, beginning each line with ;; . The prefix parameter 1 in ~1:; accounts for the width of the comma that will follow the justified item if it is not the last element in the list, or the period if it is. If ~:; has a second prefix parameter, then it is used as the width of the line, thus overriding the natural line width of the output stream. To make the preceding example use a line width of 50, one would write

 "~%;; ~{ ~<~%;; ~1,50:; ~S~>~^ ,~}  .~%"

If the second argument is not supplied, then format uses the line width of the destination output stream. If this cannot be determined (for example, when producing a string result), then format uses 72 as the line length.

See also Section 22.3.5.2 (Tilde Less-Than-Sign: Logical Block). 


 22.3.6.3 Tilde Greater-Than-Sign: End of Justification

~> terminates a ~<. The consequences of using it elsewhere are undefined. 


#### 22.3.7 <span id="FORMATControlFlowOperation">FORMAT 控制流操作</span>

> * 22.3.7.1 [Tilde Asterisk: Go-To](#TildeAsteriskGoTo)
> * 22.3.7.2 [Tilde Left-Bracket: Conditional Expression](#TildeLeftBracketCondExpr)
> * 22.3.7.3 [Tilde Right-Bracket: End of Conditional Expression](#TildeRightBracketEndCondExpr)
> * 22.3.7.4 [Tilde Left-Brace: Iteration](#TildeLeftBraceIteration)
> * 22.3.7.5 [Tilde Right-Brace: End of Iteration](#TildeRightBraceEndIteration)
> * 22.3.7.6 [Tilde Question-Mark: Recursive Processing](#TildeQuestionMarkRecursiveProc)

#### 22.3.7.1 <span id="TildeAsteriskGoTo">Tilde Asterisk: Go-To</span>

The next arg is ignored. ~n* ignores the next n arguments.

~:* backs up in the list of arguments so that the argument last processed will be processed again. ~n:* backs up n arguments.

When within a ~{ construct (see below), the ignoring (in either direction) is relative to the list of arguments being processed by the iteration.

~n@* goes to the nth arg, where 0 means the first one; n defaults to 0, so ~@* goes back to the first arg. Directives after a ~n@* will take arguments in sequence beginning with the one gone to. When within a ~{ construct, the ``goto'' is relative to the list of arguments being processed by the iteration. 


#### 22.3.7.2 <span id="TildeLeftBracketCondExpr">Tilde Left-Bracket: Conditional Expression</span>

~[str0~;str1~;...~;strn~]

This is a set of control strings, called clauses, one of which is chosen and used. The clauses are separated by ~; and the construct is terminated by ~]. For example,

"~[Siamese~;Manx~;Persian~] Cat"

The argth clause is selected, where the first clause is number 0. If a prefix parameter is given (as ~n[), then the parameter is used instead of an argument. If arg is out of range then no clause is selected and no error is signaled. After the selected alternative has been processed, the control string continues after the ~].

~[str0~;str1~;...~;strn~:;default~] has a default case. If the last ~; used to separate clauses is ~:; instead, then the last clause is an else clause that is performed if no other clause is selected. For example:

"~[Siamese~;Manx~;Persian~:;Alley~] Cat"

~:[alternative~;consequent~] selects the alternative control string if arg is false, and selects the consequent control string otherwise.

~@[consequent~] tests the argument. If it is true, then the argument is not used up by the ~[ command but remains as the next one to be processed, and the one clause consequent is processed. If the arg is false, then the argument is used up, and the clause is not processed. The clause therefore should normally use exactly one argument, and may expect it to be non-nil. For example:

 (setq *print-level* nil *print-length* 5)
 (format nil
        "~@[ print level = ~D~]~@[ print length = ~D~]"
        *print-level* *print-length*)
=>   " print length = 5"

Note also that

 (format stream "...~@[str~]..." ...)
==  (format stream "...~:[~;~:*str~]..." ...)

The combination of ~[ and # is useful, for example, for dealing with English conventions for printing lists:

 (setq foo "Items:~#[ none~; ~S~; ~S and ~S~
           ~:;~@{~#[~; and~] ~S~^ ,~}~].")
 (format nil foo) =>   "Items: none."
 (format nil foo 'foo) =>   "Items: FOO."
 (format nil foo 'foo 'bar) =>   "Items: FOO and BAR."
 (format nil foo 'foo 'bar 'baz) =>   "Items: FOO, BAR, and BAZ."
 (format nil foo 'foo 'bar 'baz 'quux) =>   "Items: FOO, BAR, BAZ, and QUUX."


#### 22.3.7.3 <span id="TildeRightBracketEndCondExpr">Tilde Right-Bracket: End of Conditional Expression</span>

~] terminates a ~[. The consequences of using it elsewhere are undefined. 



#### 22.3.7.4 <span id="TildeLeftBraceIteration">Tilde Left-Brace: Iteration</span>

~{str~}

This is an iteration construct. The argument should be a list, which is used as a set of arguments as if for a recursive call to format. The string str is used repeatedly as the control string. Each iteration can absorb as many elements of the list as it likes as arguments; if str uses up two arguments by itself, then two elements of the list will get used up each time around the loop. If before any iteration step the list is empty, then the iteration is terminated. Also, if a prefix parameter n is given, then there will be at most n repetitions of processing of str. Finally, the ~^ directive can be used to terminate the iteration prematurely.

For example:

 (format nil "The winners are:~{ ~S~}." 
         '(fred harry jill)) 
=>  "The winners are: FRED HARRY JILL."                           
 (format nil "Pairs:~{ <~S,~S>~}." 
         '(a 1 b 2 c 3))
=>  "Pairs: <A,1> <B,2> <C,3>."

~:{str~} is similar, but the argument should be a list of sublists. At each repetition step, one sublist is used as the set of arguments for processing str; on the next repetition, a new sublist is used, whether or not all of the last sublist had been processed. For example:

 (format nil "Pairs:~:{ <~S,~S>~} ." 
                 '((a 1) (b 2) (c 3)))
=>  "Pairs: <A,1> <B,2> <C,3>."

~@{str~} is similar to ~{str~}, but instead of using one argument that is a list, all the remaining arguments are used as the list of arguments for the iteration. Example:

 (format nil "Pairs:~@{ <~S,~S>~} ." 'a 1 'b 2 'c 3)
=>  "Pairs: <A,1> <B,2> <C,3>."

If the iteration is terminated before all the remaining arguments are consumed, then any arguments not processed by the iteration remain to be processed by any directives following the iteration construct.

~:@{str~} combines the features of ~:{str~} and ~@{str~}. All the remaining arguments are used, and each one must be a list. On each iteration, the next argument is used as a list of arguments to str. Example:

 (format nil "Pairs:~:@{ <~S,~S>~} ." 
              '(a 1) '(b 2) '(c 3)) 
=>  "Pairs: <A,1> <B,2> <C,3>."

Terminating the repetition construct with ~:} instead of ~} forces str to be processed at least once, even if the initial list of arguments is null. However, this will not override an explicit prefix parameter of zero.

If str is empty, then an argument is used as str. It must be a format control and precede any arguments processed by the iteration. As an example, the following are equivalent:

    (apply #'format stream string arguments)
 ==  (format stream "~1{~:}" string arguments)

This will use string as a formatting string. The ~1{ says it will be processed at most once, and the ~:} says it will be processed at least once. Therefore it is processed exactly once, using arguments as the arguments. This case may be handled more clearly by the ~? directive, but this general feature of ~{ is more powerful than ~?. 


#### 22.3.7.5 <span id="TildeRightBraceEndIteration">Tilde Right-Brace: End of Iteration</span>

~} terminates a ~{. The consequences of using it elsewhere are undefined. 


#### 22.3.7.6 <span id="TildeQuestionMarkRecursiveProc">Tilde Question-Mark: Recursive Processing</span>

The next arg must be a format control, and the one after it a list; both are consumed by the ~? directive. The two are processed as a control-string, with the elements of the list as the arguments. Once the recursive processing has been finished, the processing of the control string containing the ~? directive is resumed. Example:

 (format nil "~? ~D" "<~A ~D>" '("Foo" 5) 7) =>  "<Foo 5> 7"
 (format nil "~? ~D" "<~A ~D>" '("Foo" 5 14) 7) =>  "<Foo 5> 7"

Note that in the second example three arguments are supplied to the format string "<~A ~D>", but only two are processed and the third is therefore ignored.

With the @ modifier, only one arg is directly consumed. The arg must be a string; it is processed as part of the control string as if it had appeared in place of the ~@? construct, and any directives in the recursively processed control string may consume arguments of the control string containing the ~@? directive. Example:

 (format nil "~@? ~D" "<~A ~D>" "Foo" 5 7) =>  "<Foo 5> 7"
 (format nil "~@? ~D" "<~A ~D>" "Foo" 5 14 7) =>  "<Foo 5> 14"


### 22.3.8 <span id="FORMATMiscellaneousOperation">FORMAT 杂项操作</span>

#### 22.3.8.1 Tilde Left-Paren: Case Conversion

~(str~)

The contained control string str is processed, and what it produces is subject to case conversion.

With no flags, every uppercase character is converted to the corresponding lowercase character.

~:( capitalizes all words, as if by string-capitalize.

~@( capitalizes just the first word and forces the rest to lower case.

~:@( converts every lowercase character to the corresponding uppercase character.

In this example ~@( is used to cause the first word produced by ~@R to be capitalized:

 (format nil "~@R ~(~@R~)" 14 14) 
=>  "XIV xiv"
 (defun f (n) (format nil "~@(~R~) error~:P detected." n)) =>  F
 (f 0) =>  "Zero errors detected."
 (f 1) =>  "One error detected."
 (f 23) =>  "Twenty-three errors detected."

When case conversions appear nested, the outer conversion dominates, as illustrated in the following example:

 (format nil "~@(how is ~:(BOB SMITH~)?~)")
 =>  "How is bob smith?"
 NOT=>  "How is Bob Smith?"


#### 22.3.8.2 Tilde Right-Paren: End of Case Conversion

~) terminates a ~(. The consequences of using it elsewhere are undefined. 

#### 22.3.8.3 Tilde P: Plural

If arg is not eql to the integer 1, a lowercase s is printed; if arg is eql to 1, nothing is printed. If arg is a floating-point 1.0, the s is printed.

~:P does the same thing, after doing a ~:* to back up one argument; that is, it prints a lowercase s if the previous argument was not 1.

~@P prints y if the argument is 1, or ies if it is not. ~:@P does the same thing, but backs up first.

 (format nil "~D tr~:@P/~D win~:P" 7 1) =>  "7 tries/1 win"
 (format nil "~D tr~:@P/~D win~:P" 1 0) =>  "1 try/0 wins"
 (format nil "~D tr~:@P/~D win~:P" 1 3) =>  "1 try/3 wins"


### 22.3.9 <span id="FORMATMiscellaneousPseudoOperation">FORMAT 杂项伪操作</span>

#### 22.3.9.1 Tilde Semicolon: Clause Separator

This separates clauses in ~[ and ~< constructs. The consequences of using it elsewhere are undefined. 


#### 22.3.9.2 Tilde Circumflex: Escape Upward

~^

This is an escape construct. If there are no more arguments remaining to be processed, then the immediately enclosing ~{ or ~< construct is terminated. If there is no such enclosing construct, then the entire formatting operation is terminated. In the ~< case, the formatting is performed, but no more segments are processed before doing the justification. ~^ may appear anywhere in a ~{ construct.

 (setq donestr "Done.~^ ~D warning~:P.~^ ~D error~:P.")
=>  "Done.~^ ~D warning~:P.~^ ~D error~:P."
 (format nil donestr) =>  "Done."
 (format nil donestr 3) =>  "Done. 3 warnings."
 (format nil donestr 1 5) =>  "Done. 1 warning. 5 errors."

If a prefix parameter is given, then termination occurs if the parameter is zero. (Hence ~^ is equivalent to ~#^.) If two parameters are given, termination occurs if they are equal. If three parameters are given, termination occurs if the first is less than or equal to the second and the second is less than or equal to the third. Of course, this is useless if all the prefix parameters are constants; at least one of them should be a # or a V parameter.

If ~^ is used within a ~:{ construct, then it terminates the current iteration step because in the standard case it tests for remaining arguments of the current step only; the next iteration step commences immediately. ~:^ is used to terminate the iteration process. ~:^ may be used only if the command it would terminate is ~:{ or ~:@{. The entire iteration process is terminated if and only if the sublist that is supplying the arguments for the current iteration step is the last sublist in the case of ~:{, or the last format argument in the case of ~:@{. ~:^ is not equivalent to ~#:^; the latter terminates the entire iteration if and only if no arguments remain for the current iteration step. For example:

 (format nil "~:{ ~@?~:^ ...~} " '(("a") ("b"))) =>  "a...b"

If ~^ appears within a control string being processed under the control of a ~? directive, but not within any ~{ or ~< construct within that string, then the string being processed will be terminated, thereby ending processing of the ~? directive. Processing then continues within the string containing the ~? directive at the point following that directive.

If ~^ appears within a ~[ or ~( construct, then all the commands up to the ~^ are properly selected or case-converted, the ~[ or ~( processing is terminated, and the outward search continues for a ~{ or ~< construct to be terminated. For example:

 (setq tellstr "~@(~@[~R~]~^ ~A!~)")
=>  "~@(~@[~R~]~^ ~A!~)"
 (format nil tellstr 23) =>  "Twenty-three!"
 (format nil tellstr nil "losers") =>  " Losers!"
 (format nil tellstr 23 "losers") =>  "Twenty-three losers!"

Following are examples of the use of ~^ within a ~< construct.

 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
=>   "            FOO"
 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
=>   "FOO         BAR"
 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
=>   "FOO   BAR   BAZ"


#### 22.3.9.3 Tilde Newline: Ignored Newline

Tilde immediately followed by a newline ignores the newline and any following non-newline whitespace[1] characters. With a :, the newline is ignored, but any following whitespace[1] is left in place. With an @, the newline is left in place, but any following whitespace[1] is ignored. For example:

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

Note that in this example newlines appear in the output only as specified by the ~& and ~% directives; the actual newline characters in the control string are suppressed because each is preceded by a tilde. 


### 22.3.10 <span id="AddInfoFORMATOperations">关于 FORMAT 的额外信息</span>

#### 22.3.10.1 Nesting of FORMAT Operations

The case-conversion, conditional, iteration, and justification constructs can contain other formatting constructs by bracketing them. These constructs must nest properly with respect to each other. For example, it is not legitimate to put the start of a case-conversion construct in each arm of a conditional and the end of the case-conversion construct outside the conditional:

 (format nil "~:[abc~:@(def~;ghi~
:@(jkl~]mno~)" x) ;Invalid!

This notation is invalid because the ~[...~;...~] and ~(...~) constructs are not properly nested.

The processing indirection caused by the ~? directive is also a kind of nesting for the purposes of this rule of proper nesting. It is not permitted to start a bracketing construct within a string processed under control of a ~? directive and end the construct at some point after the ~? construct in the string containing that construct, or vice versa. For example, this situation is invalid:

 (format nil "~@?ghi~)" "abc~@(def") ;Invalid!

This notation is invalid because the ~? and ~(...~) constructs are not properly nested. 


#### 22.3.10.2 Missing and Additional FORMAT Arguments

The consequences are undefined if no arg remains for a directive requiring an argument. However, it is permissible for one or more args to remain unprocessed by a directive; such args are ignored. 


#### 22.3.10.3 Additional FORMAT Parameters

The consequences are undefined if a format directive is given more parameters than it is described here as accepting. 


#### 22.3.10.4 Undefined FORMAT Modifier Combinations

The consequences are undefined if colon or at-sign modifiers are given to a directive in a combination not specifically described here as being meaningful. 


### 22.3.11 <span id="">FORMAT 的示例</span>

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

As an example of the effects of varying the scale factor, the code

 (dotimes (k 13)
   (format t "~%Scale factor ~2D: |~13,6,2,VE|"
           (- k 5) (- k 5) 3.14159))

produces the following output:

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


### 22.3.12 <span id="NotesFORMAT">FORMAT 的注意事项</span>

Formatted output is performed not only by format, but by certain other functions that accept a format control the way format does. For example, error-signaling functions such as cerror accept format controls.

Note that the meaning of nil and t as destinations to format are different than those of nil and t as stream designators.

The ~^ should appear only at the beginning of a ~< clause, because it aborts the entire clause in which it appears (as well as all following clauses). 


## 22.4 <span id="ThePrinterDictionary">The Printer Dictionary</span>

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

        如果对象 object 是一个列表并且 *print-pretty* 的值是 false, 这些符号中的每一个都用最少的空格 whitespace 来打印对象, 就像章节 22.1.3.5 (打印列表和 cons) 中描述的. 否则 (如果对象 object 是一个列表并且 *print-pretty* 的值是 true):

            函数 pprint-linear 把一个列表的所有元素打印在一行中, 或者每个元素在一个分隔的行上.

            函数 pprint-fill 把一个列表尽可能多的元素打印到一行上.

            函数 pprint-tabular 和 pprint-fill 一样除了它打印的这些元素按列对齐. 这个 tabsize 以西文排版行长单位指定了列间隔, 也就是从一列的前缘到下一列的前缘的总间距.

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

        n 用西文排版行长单位指定了缩进. 如果 relative-to 是 :block, 这个缩进会被设置为在这个动态当前逻辑块中第一个字符的水平位置加上 n 个西文排版行长单位. 如果 relative-to 是 :current, 这个缩进被设置为当前输出位置加上 n 个西文排版行长单位. (为了面对可变宽度字体时的健壮性, 可能的情况下建议使用 :current 以及零作为一个 n.)

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

        pprint-pop, pprint-exit-if-list-exhausted, 章节 22.3.5.2 (Tilde Less-Than-Sign: Logical Block)

* 注意(Notes):

        当 *print-pretty* 的值为 nil 时使用 pprint-logical-block 宏的一个原因是允许它对点列表执行检查, (和 pprint-pop 配合) 检查 *print-level* or *print-length* 溢出也一样.

        美观打印器支持循环和共享的检测, 本质上是执行两次请求的输出. 第一次通过时, 循环和共享被检测而实际输出的字符串被抑制. 在第二次通过时, 适当的 "#n=" 和 "#n#" 标记会被插入并且字符会被输出. 就就是为什么副作用上的约束是必要的. 当遍历一个要被 pprint-logical-block 表达式形式的主体表达式形式 forms 打印的列表时, 使用 pprint-pop 来实现这一限制, 而不是普通的 pop.


### <span id="F-PPRINT-NEWLINE">函数 PPRINT-NEWLINE</span>

* 语法(Syntax):

pprint-newline kind &optional stream => nil

* 参数和值(Arguments and Values):

kind---one of :linear, :fill, :miser, or :mandatory.

stream---a stream designator. The default is standard output.

* 描述(Description):

If stream is a pretty printing stream and the value of *print-pretty* is true, a line break is inserted in the output when the appropriate condition below is satisfied; otherwise, pprint-newline has no effect.

Kind specifies the style of conditional newline. This parameter is treated as follows:

:linear

    This specifies a ``linear-style'' conditional newline. A line break is inserted if and only if the immediately containing section cannot be printed on one line. The effect of this is that line breaks are either inserted at every linear-style conditional newline in a logical block or at none of them.

:miser

    This specifies a ``miser-style'' conditional newline. A line break is inserted if and only if the immediately containing section cannot be printed on one line and miser style is in effect in the immediately containing logical block. The effect of this is that miser-style conditional newlines act like linear-style conditional newlines, but only when miser style is in effect. Miser style is in effect for a logical block if and only if the starting position of the logical block is less than or equal to *print-miser-width* ems from the right margin.

:fill

    This specifies a ``fill-style'' conditional newline. A line break is inserted if and only if either (a) the following section cannot be printed on the end of the current line, (b) the preceding section was not printed on a single line, or (c) the immediately containing section cannot be printed on one line and miser style is in effect in the immediately containing logical block. If a logical block is broken up into a number of subsections by fill-style conditional newlines, the basic effect is that the logical block is printed with as many subsections as possible on each line. However, if miser style is in effect, fill-style conditional newlines act like linear-style conditional newlines.

:mandatory

    This specifies a ``mandatory-style'' conditional newline. A line break is always inserted. This implies that none of the containing sections can be printed on a single line and will therefore trigger the insertion of line breaks at linear-style conditional newlines in these sections.

When a line break is inserted by any type of conditional newline, any blanks that immediately precede the conditional newline are omitted from the output and indentation is introduced at the beginning of the next line. By default, the indentation causes the following line to begin in the same horizontal position as the first character in the immediately containing logical block. (The indentation can be changed via pprint-indent.)

There are a variety of ways unconditional newlines can be introduced into the output (i.e., via terpri or by printing a string containing a newline character). As with mandatory conditional newlines, this prevents any of the containing sections from being printed on one line. In general, when an unconditional newline is encountered, it is printed out without suppression of the preceding blanks and without any indentation following it. However, if a per-line prefix has been specified (see pprint-logical-block), this prefix will always be printed no matter how a newline originates.

* 示例(Examples):

See Section 22.2.2 (使用美观打印器的示例).

* 副作用(Side Effects):

Output to stream.

* 受此影响(Affected By):

*print-pretty*, *print-miser*. The presence of containing logical blocks. The placement of newlines and conditional newlines.

* 异常情况(Exceptional Situations):

An error of type type-error is signaled if kind is not one of :linear, :fill, :miser, or :mandatory.

* 也见(See Also):

Section 22.3.5.1 (Tilde Underscore: Conditional Newline), Section 22.2.2 (使用美观打印器的示例)

* 注意(Notes): None. 


### <span id="LM-PPRINT-POP">局部宏 PPRINT-POP</span>

* 语法(Syntax):

pprint-pop <no arguments> => object

* 参数和值(Arguments and Values):

object---an element of the list being printed in the lexically current logical block, or nil.

* 描述(Description):

Pops one element from the list being printed in the lexically current logical block, obeying *print-length* and *print-circle* as described below.

Each time pprint-pop is called, it pops the next value off the list passed to the lexically current logical block and returns it. However, before doing this, it performs three tests:

    If the remaining `list' is not a list, ``. '' is printed followed by the remaining `list.' (This makes it easier to write printing functions that are robust in the face of malformed arguments.)

    If *print-length* is non-nil, and pprint-pop has already been called *print-length* times within the immediately containing logical block, ``...'' is printed. (This makes it easy to write printing functions that properly handle *print-length*.)

    If *print-circle* is non-nil, and the remaining list is a circular (or shared) reference, then ``. '' is printed followed by an appropriate ``#n#'' marker. (This catches instances of cdr circularity and sharing in lists.)

If either of the three conditions above occurs, the indicated output is printed on the pretty printing stream created by the immediately containing pprint-logical-block and the execution of the immediately containing pprint-logical-block is terminated except for the printing of the suffix.

If pprint-logical-block is given a `list' argument of nil---because it is not processing a list---pprint-pop can still be used to obtain support for *print-length*. In this situation, the first and third tests above are disabled and pprint-pop always returns nil. See Section 22.2.2 (使用美观打印器的示例)---specifically, the pprint-vector example.

Whether or not pprint-pop is fbound in the global environment is implementation-dependent; however, the restrictions on redefinition and shadowing of pprint-pop are the same as for symbols in the COMMON-LISP package which are fbound in the global environment. The consequences of attempting to use pprint-pop outside of pprint-logical-block are undefined.

* 示例(Examples): None.

* 副作用(Side Effects):

Might cause output to the pretty printing stream associated with the lexically current logical block.

* 受此影响(Affected By):

*print-length*, *print-circle*.

* 异常情况(Exceptional Situations):

An error is signaled (either at macro expansion time or at run time) if a usage of pprint-pop occurs where there is no lexically containing pprint-logical-block form.

The consequences are undefined if pprint-pop is executed outside of the dynamic extent of this pprint-logical-block.

* 也见(See Also):

pprint-exit-if-list-exhausted, pprint-logical-block.

* 注意(Notes):

It is frequently a good idea to call pprint-exit-if-list-exhausted before calling pprint-pop. 


### <span id="F-PPRINT-TAB">函数 PPRINT-TAB</span>

* 语法(Syntax):

pprint-tab kind colnum colinc &optional stream => nil

* 参数和值(Arguments and Values):

kind---one of :line, :section, :line-relative, or :section-relative.

colnum---一个非负整数.

colinc---一个非负整数.

stream---an output stream designator.

* 描述(Description):

Specifies tabbing to stream as performed by the standard ~T format directive. If stream is a pretty printing stream and the value of *print-pretty* is true, tabbing is performed; otherwise, pprint-tab has no effect.

The arguments colnum and colinc correspond to the two parameters to ~T and are in terms of ems. The kind argument specifies the style of tabbing. It must be one of :line (tab as by ~T), :section (tab as by ~:T, but measuring horizontal positions relative to the start of the dynamically enclosing section), :line-relative (tab as by ~@T), or :section-relative (tab as by ~:@T, but measuring horizontal positions relative to the start of the dynamically enclosing section).

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error is signaled if kind is not one of :line, :section, :line-relative, or :section-relative.

* 也见(See Also):

pprint-logical-block

* 注意(Notes): None. 


### <span id="SGF-PRINT-OBJECT">标准广义函数 PRINT-OBJECT</span>

* 语法(Syntax):

print-object object stream => object

Method Signatures:

print-object (object standard-object) stream

print-object (object structure-object) stream

* 参数和值(Arguments and Values):

object---一个对象.

stream---a stream.

* 描述(Description):

The generic function print-object writes the printed representation of object to stream. The function print-object is called by the Lisp printer; it should not be called by the user.

Each implementation is required to provide a method on the class standard-object and on the class structure-object. In addition, each implementation must provide methods on enough other classes so as to ensure that there is always an applicable method. Implementations are free to add methods for other classes. Users may write methods for print-object for their own classes if they do not wish to inherit an implementation-dependent method.

The method on the class structure-object prints the object in the default #S notation; see Section 22.1.3.12 (打印结构体).

Methods on print-object are responsible for implementing their part of the semantics of the printer control variables, as follows:

*print-readably*

    All methods for print-object must obey *print-readably*. This includes both user-defined methods and implementation-defined methods. Readable printing of structures and standard objects is controlled by their print-object method, not by their make-load-form method. Similarity for these objects is application dependent and hence is defined to be whatever these methods do; see Section 3.2.4.2 (Similarity of Literal Objects).

*print-escape*

    Each method must implement *print-escape*.

*print-pretty*

    The method may wish to perform specialized line breaking or other output conditional on the value of *print-pretty*. For further information, see (for example) the macro pprint-fill. See also Section 22.2.1.4 (美观打印分派表) and Section 22.2.2 (使用美观打印器的示例).

*print-length*

    Methods that produce output of indefinite length must obey *print-length*. For further information, see (for example) the macros pprint-logical-block and pprint-pop. See also Section 22.2.1.4 (美观打印分派表) and Section 22.2.2 (使用美观打印器的示例).

*print-level*

    The printer takes care of *print-level* automatically, provided that each method handles exactly one level of structure and calls write (or an equivalent function) recursively if there are more structural levels. The printer's decision of whether an object has components (and therefore should not be printed when the printing depth is not less than *print-level*) is implementation-dependent. In some implementations its print-object method is not called; in others the method is called, and the determination that the object has components is based on what it tries to write to the stream.

*print-circle*

    When the value of *print-circle* is true, a user-defined print-object method can print objects to the supplied stream using write, prin1, princ, or format and expect circularities to be detected and printed using the #n# syntax. If a user-defined print-object method prints to a stream other than the one that was supplied, then circularity detection starts over for that stream. See *print-circle*.

*print-base*, *print-radix*, *print-case*, *print-gensym*, and *print-array*

    These printer control variables apply to specific types of objects and are handled by the methods for those objects.

If these rules are not obeyed, the results are undefined.

In general, the printer and the print-object methods should not rebind the print control variables as they operate recursively through the structure, but this is implementation-dependent.

In some implementations the stream argument passed to a print-object method is not the original stream, but is an intermediate stream that implements part of the printer. methods should therefore not depend on the identity of this stream.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

pprint-fill, pprint-logical-block, pprint-pop, write, *print-readably*, *print-escape*, *print-pretty*, *print-length*, Section 22.1.3 (默认 Print-Object 方法), Section 22.1.3.12 (打印结构体), Section 22.2.1.4 (美观打印分派表), Section 22.2.2 (使用美观打印器的示例)

* 注意(Notes): None. 


### <span id="M-PRINT-UNREADABLE-OBJECT">宏 PRINT-UNREADABLE-OBJECT</span>

* 语法(Syntax):

print-unreadable-object (object stream &key type identity) form* => nil

* 参数和值(Arguments and Values):

object---一个对象; evaluated.

stream---a stream designator; evaluated.

type---a generalized boolean; evaluated.

identity---a generalized boolean; evaluated.

forms---一个隐式 progn;

* 描述(Description):

Outputs a printed representation of object on stream, beginning with ``#<'' and ending with ``>''. Everything output to stream by the body forms is enclosed in the the angle brackets. If type is true, the output from forms is preceded by a brief description of the object's type and a space character. If identity is true, the output from forms is followed by a space character and a representation of the object's identity, typically a storage address.

If either type or identity is not supplied, its value is false. It is valid to omit the body forms. If type and identity are both true and there are no body forms, only one space character separates the type and the identity.

* 示例(Examples):

;; Note that in this example, the precise form of the output ;; is implementation-dependent.

 (defmethod print-object ((obj airplane) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (princ (tail-number obj) stream)))

 (prin1-to-string my-airplane)
=>  "#<Airplane NW0773 36000123135>"
OR=>  "#<FAA:AIRPLANE NW0773 17>"

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If *print-readably* is true, print-unreadable-object signals an error of type print-not-readable without printing anything.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-SET-PPRINT-DISPATCH">函数 SET-PPRINT-DISPATCH</span>

* 语法(Syntax):

set-pprint-dispatch type-specifier function &optional priority table => nil

* 参数和值(Arguments and Values):

type-specifier---a type specifier.

function---a function, a function name, or nil.

priority---a real. The default is 0.

table---a pprint dispatch table. The default is the value of *print-pprint-dispatch*.

* 描述(Description):

Installs an entry into the pprint dispatch table which is table.

Type-specifier is the key of the entry. The first action of set-pprint-dispatch is to remove any pre-existing entry associated with type-specifier. This guarantees that there will never be two entries associated with the same type specifier in a given pprint dispatch table. Equality of type specifiers is tested by equal.

Two values are associated with each type specifier in a pprint dispatch table: a function and a priority. The function must accept two arguments: the stream to which output is sent and the object to be printed. The function should pretty print the object to the stream. The function can assume that object satisfies the type given by type-specifier. The function must obey *print-readably*. Any values returned by the function are ignored.

Priority is a priority to resolve conflicts when an object matches more than one entry.

It is permissible for function to be nil. In this situation, there will be no type-specifier entry in table after set-pprint-dispatch returns.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error is signaled if priority is not a real.

* 也见(See Also): None.

* 注意(Notes):

Since pprint dispatch tables are often used to control the pretty printing of Lisp code, it is common for the type-specifier to be an expression of the form

 (cons car-type cdr-type)

This signifies that the corresponding object must be a cons cell whose car matches the type specifier car-type and whose cdr matches the type specifier cdr-type. The cdr-type can be omitted in which case it defaults to t. 


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

output-stream---an output stream designator. The default is standard output.

array---一个广义 boolean.

base---a radix.

case---a symbol of type (member :upcase :downcase :capitalize).

circle---一个广义 boolean.

escape---一个广义 boolean.

gensym---一个广义 boolean.

length---一个非负整数, 或 nil.

level---一个非负整数, 或 nil.

lines---一个非负整数, 或 nil.

miser-width---一个非负整数, 或 nil.

pprint-dispatch---a pprint dispatch table.

pretty---一个广义 boolean.

radix---一个广义 boolean.

readably---一个广义 boolean.

right-margin---一个非负整数, 或 nil.

stream---an output stream designator. The default is standard output.

* 描述(Description):

write, prin1, princ, print, and pprint write the printed representation of object to output-stream.

write is the general entry point to the Lisp printer. For each explicitly supplied keyword parameter named in the next figure, the corresponding printer control variable is dynamically bound to its value while printing goes on; for each keyword parameter in the next figure that is not explicitly supplied, the value of the corresponding printer control variable is the same as it was at the time write was invoked. Once the appropriate bindings are established, the object is output by the Lisp printer.

Parameter        Corresponding Dynamic Variable  
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

Figure 22-7. Argument correspondences for the WRITE function.

prin1, princ, print, and pprint implicitly bind certain print parameters to particular values. The remaining parameter values are taken from *print-array*, *print-base*, *print-case*, *print-circle*, *print-escape*, *print-gensym*, *print-length*, *print-level*, *print-lines*, *print-miser-width*, *print-pprint-dispatch*, *print-pretty*, *print-radix*, and *print-right-margin*.

prin1 produces output suitable for input to read. It binds *print-escape* to true.

princ is just like prin1 except that the output has no escape characters. It binds *print-escape* to false and *print-readably* to false. The general rule is that output from princ is intended to look good to people, while output from prin1 is intended to be acceptable to read.

print is just like prin1 except that the printed representation of object is preceded by a newline and followed by a space.

pprint is just like print except that the trailing space is omitted and object is printed with the *print-pretty* flag non-nil to produce pretty output.

Output-stream specifies the stream to which output is to be sent.

* 受此影响(Affected By):

*standard-output*, *terminal-io*, *print-escape*, *print-radix*, *print-base*, *print-circle*, *print-pretty*, *print-level*, *print-length*, *print-case*, *print-gensym*, *print-array*, *read-default-float-format*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

readtable-case, Section 22.3.4 (FORMAT 打印器操作)

* 注意(Notes):

The functions prin1 and print do not bind *print-readably*.

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

base---a radix.

case---a symbol of type (member :upcase :downcase :capitalize).

circle---一个广义 boolean.

escape---一个广义 boolean.

gensym---一个广义 boolean.

length---一个非负整数, 或 nil.

level---一个非负整数, 或 nil.

lines---一个非负整数, 或 nil.

miser-width---一个非负整数, 或 nil.

pprint-dispatch---a pprint dispatch table.

pretty---一个广义 boolean.

radix---一个广义 boolean.

readably---一个广义 boolean.

right-margin---一个非负整数, 或 nil.

string---a string.

* 描述(Description):

write-to-string, prin1-to-string, and princ-to-string are used to create a string consisting of the printed representation of object. Object is effectively printed as if by write, prin1, or princ, respectively, and the characters that would be output are made into a string.

write-to-string is the general output function. It has the ability to specify all the parameters applicable to the printing of object.

prin1-to-string acts like write-to-string with :escape t, that is, escape characters are written where appropriate.

princ-to-string acts like write-to-string with :escape nil :readably nil. Thus no escape characters are written.

All other keywords that would be specified to write-to-string are default values when prin1-to-string or princ-to-string is invoked.

The meanings and defaults for the keyword arguments to write-to-string are the same as those for write.

* 示例(Examples):

 (prin1-to-string "abc") =>  "\"abc\""
 (princ-to-string "abc") =>  "abc"

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

a generalized boolean.

* 初始值(Initial Value):

implementation-dependent.

* 描述(Description):

Controls the format in which arrays are printed. If it is false, the contents of arrays other than strings are never printed. Instead, arrays are printed in a concise form using #< that gives enough information for the user to be able to identify the array, but does not include the entire array contents. If it is true, non-string arrays are printed using #(...), #*, or #nA syntax.

* 示例(Examples): None.

* 受此影响(Affected By):

The implementation.

* 也见(See Also):

Section 2.4.8.3 (Sharpsign Left-Parenthesis), Section 2.4.8.20 (Sharpsign Less-Than-Sign)

* 注意(Notes): None. 


### <span id="V-PRINT-BASE-RADIX">变量 *PRINT-BASE*, *PRINT-RADIX*</span>

* 值类型(Value Type):

*print-base*---a radix. *print-radix*---一个广义 boolean.

* 初始值(Initial Value):

The initial value of *print-base* is 10. The initial value of *print-radix* is false.

* 描述(Description):

*print-base* and *print-radix* control the printing of rationals. The value of *print-base* is called the current output base.

The value of *print-base* is the radix in which the printer will print rationals. For radices above 10, letters of the alphabet are used to represent digits above 9.

If the value of *print-radix* is true, the printer will print a radix specifier to indicate the radix in which it is printing a rational number. The radix specifier is always printed using lowercase letters. If *print-base* is 2, 8, or 16, then the radix specifier used is #b, #o, or #x, respectively. For integers, base ten is indicated by a trailing decimal point instead of a leading radix specifier; for ratios, #10r is used.

* 示例(Examples):

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

* 受此影响(Affected By):

Might be bound by format, and write, write-to-string.

* 也见(See Also):

format, write, write-to-string

* 注意(Notes): None. 


### <span id="V-PRINT-CASE">变量 *PRINT-CASE*</span>

* 值类型(Value Type):

One of the symbols :upcase, :downcase, or :capitalize.

* 初始值(Initial Value):

The symbol :upcase.

* 描述(Description):

The value of *print-case* controls the case (upper, lower, or mixed) in which to print any uppercase characters in the names of symbols when vertical-bar syntax is not used.

*print-case* has an effect at all times when the value of *print-escape* is false. *print-case* also has an effect when the value of *print-escape* is true unless inside an escape context (i.e., unless between vertical-bars or after a slash).

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 也见(See Also):

write

* 注意(Notes):

read normally converts lowercase characters appearing in symbols to corresponding uppercase characters, so that internally print names normally contain only uppercase characters.

If *print-escape* is true, lowercase characters in the name of a symbol are always printed in lowercase, and are preceded by a single escape character or enclosed by multiple escape characters; uppercase characters in the name of a symbol are printed in upper case, in lower case, or in mixed case so as to capitalize words, according to the value of *print-case*. The convention for what constitutes a ``word'' is the same as for string-capitalize. 


### <span id="V-PRINT-CIRCLE">变量 *PRINT-CIRCLE*</span>

* 值类型(Value Type):

a generalized boolean.

* 初始值(Initial Value):

false.

* 描述(Description):

Controls the attempt to detect circularity and sharing in an object being printed.

If false, the printing process merely proceeds by recursive descent without attempting to detect circularity and sharing.

If true, the printer will endeavor to detect cycles and sharing in the structure to be printed, and to use #n= and #n# syntax to indicate the circularities or shared components.

If true, a user-defined print-object method can print objects to the supplied stream using write, prin1, princ, or format and expect circularities and sharing to be detected and printed using the #n# syntax. If a user-defined print-object method prints to a stream other than the one that was supplied, then circularity detection starts over for that stream.

Note that implementations should not use #n# notation when the Lisp reader would automatically assure sharing without it (e.g., as happens with interned symbols).

* 示例(Examples):

 (let ((a (list 1 2 3)))
   (setf (cdddr a) a)
   (let ((*print-circle* t))
     (write a)
     :done))
>>  #1=(1 2 3 . #1#)
=>  :DONE

* 受此影响(Affected By): None.

* 也见(See Also):

write

* 注意(Notes):

An attempt to print a circular structure with *print-circle* set to nil may lead to looping behavior and failure to terminate. 


### <span id="V-PRINT-ESCAPE">变量 *PRINT-ESCAPE*</span>

* 值类型(Value Type):

a generalized boolean.

* 初始值(Initial Value):

true.

* 描述(Description):

If false, escape characters and package prefixes are not output when an expression is printed.

If true, an attempt is made to print an expression in such a way that it can be read again to produce an equal expression. (This is only a guideline; not a requirement. See *print-readably*.)

For more specific details of how the value of *print-escape* affects the printing of certain types, see Section 22.1.3 (默认 Print-Object 方法).

* 示例(Examples):

 (let ((*print-escape* t)) (write #\a))
>>  #\a
=>  #\a
 (let ((*print-escape* nil)) (write #\a))
>>  a
=>  #\a

* 受此影响(Affected By):

princ, prin1, format

* 也见(See Also):

write, readtable-case

* 注意(Notes):

princ effectively binds *print-escape* to false. prin1 effectively binds *print-escape* to true. 


### <span id="V-PRINT-GENSYM">变量 *PRINT-GENSYM*</span>

* 值类型(Value Type):

a generalized boolean.

* 初始值(Initial Value):

true.

* 描述(Description):

Controls whether the prefix ``#:'' is printed before apparently uninterned symbols. The prefix is printed before such symbols if and only if the value of *print-gensym* is true.

* 示例(Examples):

 (let ((*print-gensym* nil))
   (print (gensym)))
>>  G6040 
=>  #:G6040

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

        如果它不是 nil, 那么当打印子结构的宽度小于或等于这个许多西文排版行长单位时, 这个美观打印器切换到紧凑输出风格(称为 miser 风格).

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

        如果它不是 nil, 当美观打印器要做布局决定时, 它指定了要使用的右边距 (西文排版行长单位的整形数字).

        如果它是 nil, 正确的边距被认为是最大行长度, 这样输出就可以显示, 而不需要包绕或截断. 如果这个没有被确定, 使用一个依赖于具体实现的值.

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes):

        这个度量是用西文排版行长单位, 为了与具体实现定义的可变宽度字体兼容, 同时不要求语言提供对字体的支持. 


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

        关于 control-string 任何被解释的详细信息, 见章节 22.3 (Formatted Output).

* 示例(Examples): None.

* 受此影响(Affected By):

        *standard-output*, *print-escape*, *print-radix*, *print-base*, *print-circle*, *print-pretty*, *print-level*, *print-length*, *print-case*, *print-gensym*, *print-array*.

* 异常情况(Exceptional Situations):

        如果 destination 是一个带有填充指针的字符串, 如果在这个调用的动态范围内直接在这个字符串上执行破坏性的修改, 那么后果是未定义的.

* 也见(See Also):

        write, 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None.