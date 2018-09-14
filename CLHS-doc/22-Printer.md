# 22 打印器

> * 22.1 [Lisp 打印器](#TheLispPrinter)
> * 22.2 [The Lisp Pretty Printer](#TheLispPrettyPrinter)
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

如果 \*print-pretty* 的值是 true, 打印由当前的 pprint 分派表(current pprint dispatch table)控制; 见章节 22.2.1.4 (Pretty Print Dispatch Tables).

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

## 22.2 <span id="TheLispPrettyPrinter">The Lisp Pretty Printer</span>

> * 22.2.1 [Pretty Printer Concepts](#PrettyPrinterConcepts)
> * 22.2.2 [Examples of using the Pretty Printer](#ExamplesPrettyPrinter)
> * 22.2.3 [Notes about the Pretty Printer's Background](#NotesPrettyPrinterBackground)

### 22.2.1 <span id="PrettyPrinterConcepts">Pretty Printer Concepts</span>

The facilities provided by the pretty printer permit programs to redefine the way in which code is displayed, and allow the full power of pretty printing to be applied to complex combinations of data structures.

Whether any given style of output is in fact ``pretty'' is inherently a somewhat subjective issue. However, since the effect of the pretty printer can be customized by conforming programs, the necessary flexibility is provided for individual programs to achieve an arbitrary degree of aesthetic control.

By providing direct access to the mechanisms within the pretty printer that make dynamic decisions about layout, the macros and functions pprint-logical-block, pprint-newline, and pprint-indent make it possible to specify pretty printing layout rules as a part of any function that produces output. They also make it very easy for the detection of circularity and sharing, and abbreviation based on length and nesting depth to be supported by the function.

The pretty printer is driven entirely by dispatch based on the value of *print-pprint-dispatch*. The function set-pprint-dispatch makes it possible for conforming programs to associate new pretty printing functions with a type.

> * 22.2.1.1 [Dynamic Control of the Arrangement of Output](#DynamicControlArrangeOutput)
> * 22.2.1.2 [Format Directive Interface](#FormatDirectiveInterface)
> * 22.2.1.3 [Compiling Format Strings](#CompilingFormatStrings)
> * 22.2.1.4 [Pretty Print Dispatch Tables](#PrettyPrintDispatchTables)
> * 22.2.1.5 [Pretty Printer Margins](#PrettyPrinterMargins)

#### 22.2.1.1 <span id="DynamicControlArrangeOutput">Dynamic Control of the Arrangement of Output</span>

The actions of the pretty printer when a piece of output is too large to fit in the space available can be precisely controlled. Three concepts underlie the way these operations work---logical blocks, conditional newlines, and sections. Before proceeding further, it is important to define these terms.

The first line of the next figure shows a schematic piece of output. Each of the characters in the output is represented by ``-''. The positions of conditional newlines are indicated by digits. The beginnings and ends of logical blocks are indicated by ``<'' and ``>'' respectively.

The output as a whole is a logical block and the outermost section. This section is indicated by the 0's on the second line of Figure 1. Logical blocks nested within the output are specified by the macro pprint-logical-block. Conditional newline positions are specified by calls to pprint-newline. Each conditional newline defines two sections (one before it and one after it) and is associated with a third (the section immediately containing it).

The section after a conditional newline consists of: all the output up to, but not including, (a) the next conditional newline immediately contained in the same logical block; or if (a) is not applicable, (b) the next newline that is at a lesser level of nesting in logical blocks; or if (b) is not applicable, (c) the end of the output.

The section before a conditional newline consists of: all the output back to, but not including, (a) the previous conditional newline that is immediately contained in the same logical block; or if (a) is not applicable, (b) the beginning of the immediately containing logical block. The last four lines in Figure 1 indicate the sections before and after the four conditional newlines.

The section immediately containing a conditional newline is the shortest section that contains the conditional newline in question. In the next figure, the first conditional newline is immediately contained in the section marked with 0's, the second and third conditional newlines are immediately contained in the section before the fourth conditional newline, and the fourth conditional newline is immediately contained in the section after the first conditional newline.

 <-1---<--<--2---3->--4-->->
 000000000000000000000000000
 11 111111111111111111111111
           22 222
              333 3333
        44444444444444 44444

Figure 22-3. Example of Logical Blocks, Conditional Newlines, and Sections

Whenever possible, the pretty printer displays the entire contents of a section on a single line. However, if the section is too long to fit in the space available, line breaks are inserted at conditional newline positions within the section. 


#### 22.2.1.2 <span id="FormatDirectiveInterface">Format Directive Interface</span>

The primary interface to operations for dynamically determining the arrangement of output is provided through the functions and macros of the pretty printer. The next figure shows the defined names related to pretty printing.

*print-lines*            pprint-dispatch                pprint-pop           
*print-miser-width*      pprint-exit-if-list-exhausted  pprint-tab           
*print-pprint-dispatch*  pprint-fill                    pprint-tabular       
*print-right-margin*     pprint-indent                  set-pprint-dispatch  
copy-pprint-dispatch     pprint-linear                  write                
format                   pprint-logical-block                                
formatter                pprint-newline                                      

Figure 22-4. Defined names related to pretty printing.

The next figure identifies a set of format directives which serve as an alternate interface to the same pretty printing operations in a more textually compact form.

~I   ~W      ~<...~:>  
~:T  ~/.../  ~_        

Figure 22-5. Format directives related to Pretty Printing 


#### 22.2.1.3 <span id="CompilingFormatStrings">Compiling Format Strings</span>

A format string is essentially a program in a special-purpose language that performs printing, and that is interpreted by the function format. The formatter macro provides the efficiency of using a compiled function to do that same printing but without losing the textual compactness of format strings.

A format control is either a format string or a function that was returned by the the formatter macro. 


#### 22.2.1.4 <span id="PrettyPrintDispatchTables">Pretty Print Dispatch Tables</span>

A pprint dispatch table is a mapping from keys to pairs of values. Each key is a type specifier. The values associated with a key are a ``function'' (specifically, a function designator or nil) and a ``numerical priority'' (specifically, a real). Basic insertion and retrieval is done based on the keys with the equality of keys being tested by equal.

When *print-pretty* is true, the current pprint dispatch table (in *print-pprint-dispatch*) controls how objects are printed. The information in this table takes precedence over all other mechanisms for specifying how to print objects. In particular, it has priority over user-defined print-object methods because the current pprint dispatch table is consulted first.

The function is chosen from the current pprint dispatch table by finding the highest priority function that is associated with a type specifier that matches the object; if there is more than one such function, it is implementation-dependent which is used.

However, if there is no information in the table about how to pretty print a particular kind of object, a function is invoked which uses print-object to print the object. The value of *print-pretty* is still true when this function is called, and individual methods for print-object might still elect to produce output in a special format conditional on the value of *print-pretty*. 


#### 22.2.1.5 <span id="PrettyPrinterMargins">Pretty Printer Margins</span>

A primary goal of pretty printing is to keep the output between a pair of margins. The column where the output begins is taken as the left margin. If the current column cannot be determined at the time output begins, the left margin is assumed to be zero. The right margin is controlled by *print-right-margin*. 


### 22.2.2 <span id="ExamplesPrettyPrinter">Examples of using the Pretty Printer</span>

As an example of the interaction of logical blocks, conditional newlines, and indentation, consider the function simple-pprint-defun below. This function prints out lists whose cars are defun in the standard way assuming that the list has exactly length 4.

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

Suppose that one evaluates the following:

(simple-pprint-defun *standard-output* '(defun prod (x y) (* x y)))

If the line width available is greater than or equal to 26, then all of the output appears on one line. If the line width available is reduced to 25, a line break is inserted at the linear-style conditional newline before the expression (* x y), producing the output shown. The (pprint-indent :block 1) causes (* x y) to be printed at a relative indentation of 1 in the logical block.

 (DEFUN PROD (X Y) 
   (* X Y))

If the line width available is 15, a line break is also inserted at the fill style conditional newline before the argument list. The call on (pprint-indent :current 0) causes the argument list to line up under the function name.

(DEFUN PROD
       (X Y)
  (* X Y))

If *print-miser-width* were greater than or equal to 14, the example output above would have been as follows, because all indentation changes are ignored in miser mode and line breaks are inserted at miser-style conditional newlines.

 (DEFUN
  PROD
  (X Y)
  (* X Y))

As an example of a per-line prefix, consider that evaluating the following produces the output shown with a line width of 20 and *print-miser-width* of nil.

 (pprint-logical-block (*standard-output* nil :per-line-prefix ";;; ")
   (simple-pprint-defun *standard-output* '(defun prod (x y) (* x y))))
 
 ;;; (DEFUN PROD
 ;;;        (X Y)
 ;;;   (* X Y))

As a more complex (and realistic) example, consider the function pprint-let below. This specifies how to print a let form in the traditional style. It is more complex than the example above, because it has to deal with nested structure. Also, unlike the example above it contains complete code to readably print any possible list that begins with the symbol let. The outermost pprint-logical-block form handles the printing of the input list as a whole and specifies that parentheses should be printed in the output. The second pprint-logical-block form handles the list of binding pairs. Each pair in the list is itself printed by the innermost pprint-logical-block. (A loop form is used instead of merely decomposing the pair into two objects so that readable output will be produced no matter whether the list corresponding to the pair has one element, two elements, or (being malformed) has more than two elements.) A space and a fill-style conditional newline are placed after each pair except the last. The loop at the end of the topmost pprint-logical-block form prints out the forms in the body of the let form separated by spaces and linear-style conditional newlines.

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

Suppose that one evaluates the following with *print-level* being 4, and *print-circle* being true.

 (pprint-let *standard-output*
             '#1=(let (x (*print-length* (f (g 3))) 
                       (z . 2) (k (car y)))
                   (setq x (sqrt z)) #1#))

If the line length is greater than or equal to 77, the output produced appears on one line. However, if the line length is 76, line breaks are inserted at the linear-style conditional newlines separating the forms in the body and the output below is produced. Note that, the degenerate binding pair x is printed readably even though it fails to be a list; a depth abbreviation marker is printed in place of (g 3); the binding pair (z . 2) is printed readably even though it is not a proper list; and appropriate circularity markers are printed.

 #1=(LET (X (*PRINT-LENGTH* (F #)) (Z . 2) (K (CAR Y))) 
      (SETQ X (SQRT Z))
      #1#)

If the line length is reduced to 35, a line break is inserted at one of the fill-style conditional newlines separating the binding pairs.

 #1=(LET (X (*PRINT-PRETTY* (F #))
          (Z . 2) (K (CAR Y)))
      (SETQ X (SQRT Z))
      #1#)

Suppose that the line length is further reduced to 22 and *print-length* is set to 3. In this situation, line breaks are inserted after both the first and second binding pairs. In addition, the second binding pair is itself broken across two lines. Clause (b) of the description of fill-style conditional newlines (see the function pprint-newline) prevents the binding pair (z . 2) from being printed at the end of the third line. Note that the length abbreviation hides the circularity from view and therefore the printing of circularity markers disappears.

 (LET (X
       (*PRINT-LENGTH*
        (F #))
       (Z . 2) ...)
   (SETQ X (SQRT Z))
   ...)

The next function prints a vector using ``#(...)'' notation.

(defun pprint-vector (*standard-output* v)
  (pprint-logical-block (nil nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
        (loop (pprint-pop)
              (write (aref v i))
              (if (= (incf i) end) (return nil))
              (write-char #\Space)
              (pprint-newline :fill))))))

Evaluating the following with a line length of 15 produces the output shown.

 (pprint-vector *standard-output* '#(12 34 567 8 9012 34 567 89 0 1 23))
 
 #(12 34 567 8 
   9012 34 567 
   89 0 1 23)

As examples of the convenience of specifying pretty printing with format strings, consider that the functions simple-pprint-defun and pprint-let used as examples above can be compactly defined as follows. (The function pprint-vector cannot be defined using format because the data structure it traverses is not a list.)

(defun simple-pprint-defun (*standard-output* list)
  (format T "~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>" list))

(defun pprint-let (*standard-output* list)
  (format T "~:<~W~^~:<~@{~:<~@{~W~^~_~}~:>~^~:_~}~:>~1I~@{~^~_~W~}~:>" list)) 

In the following example, the first form restores *print-pprint-dispatch* to the equivalent of its initial value. The next two forms then set up a special way to pretty print ratios. Note that the more specific type specifier has to be associated with a higher priority.

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

The following two forms illustrate the definition of pretty printing functions for types of code. The first form illustrates how to specify the traditional method for printing quoted objects using single-quote. Note the care taken to ensure that data lists that happen to begin with quote will be printed readably. The second form specifies that lists beginning with the symbol my-let should print the same way that lists beginning with let print when the initial pprint dispatch table is in effect.

 (set-pprint-dispatch '(cons (member quote)) () 
   #'(lambda (s list)
       (if (and (consp (cdr list)) (null (cddr list)))
          (funcall (formatter "'~W") s (cadr list))
          (pprint-fill s list))))
 
 (set-pprint-dispatch '(cons (member my-let)) 
                      (pprint-dispatch '(let) nil))

The next example specifies a default method for printing lists that do not correspond to function calls. Note that the functions pprint-linear, pprint-fill, and pprint-tabular are all defined with optional colon-p and at-sign-p arguments so that they can be used as pprint dispatch functions as well as ~/.../ functions.

 (set-pprint-dispatch '(cons (not (and symbol (satisfies fboundp))))
                      #'pprint-fill -5)
 
 ;; Assume a line length of 9
 (pprint '(0 b c d e f g h i j k))
 (0 b c d
  e f g h
  i j k)

This final example shows how to define a pretty printing function for a user defined data structure.

 (defstruct family mom kids)
 
 (set-pprint-dispatch 'family
   #'(lambda (s f)
       (funcall (formatter "~@<#<~;~W and ~2I~_~/pprint-fill/~;>~:>")
               s (family-mom f) (family-kids f))))

The pretty printing function for the structure family specifies how to adjust the layout of the output so that it can fit aesthetically into a variety of line widths. In addition, it obeys the printer control variables *print-level*, *print-length*, *print-lines*, *print-circle* and *print-escape*, and can tolerate several different kinds of malformity in the data structure. The output below shows what is printed out with a right margin of 25, *print-pretty* being true, *print-escape* being false, and a malformed kids list.

 (write (list 'principal-family
              (make-family :mom "Lucy"
                           :kids '("Mark" "Bob" . "Dan")))
        :right-margin 25 :pretty T :escape nil :miser-width nil)
 (PRINCIPAL-FAMILY
  #<Lucy and
      Mark Bob . Dan>)

Note that a pretty printing function for a structure is different from the structure's print-object method. While print-object methods are permanently associated with a structure, pretty printing functions are stored in pprint dispatch tables and can be rapidly changed to reflect different printing needs. If there is no pretty printing function for a structure in the current pprint dispatch table, its print-object method is used instead. 

### 22.2.3 <span id="NotesPrettyPrinterBackground">Notes about the Pretty Printer's Background</span>

For a background reference to the abstract concepts detailed in this section, see XP: A Common Lisp Pretty Printing System. The details of that paper are not binding on this document, but may be helpful in establishing a conceptual basis for understanding this material. 


## 22.3 <span id="">Formatted Output</span>

format is useful for producing nicely formatted text, producing good-looking messages, and so on. format can generate and return a string or output to destination.

The control-string argument to format is actually a format control. That is, it can be either a format string or a function, for example a function returned by the formatter macro.

If it is a function, the function is called with the appropriate output stream as its first argument and the data arguments to format as its remaining arguments. The function should perform whatever output is necessary and return the unused tail of the arguments (if any).

The compilation process performed by formatter produces a function that would do with its arguments as the format interpreter would do with those arguments.

The remainder of this section describes what happens if the control-string is a format string.

Control-string is composed of simple text (characters) and embedded directives.

format writes the simple text as is; each embedded directive specifies further text output that is to appear at the corresponding point within the simple text. Most directives use one or more elements of args to create their output.

A directive consists of a tilde, optional prefix parameters separated by commas, optional colon and at-sign modifiers, and a single character indicating what kind of directive this is. There is no required ordering between the at-sign and colon modifier. The case of the directive character is ignored. Prefix parameters are notated as signed (sign is optional) decimal numbers, or as a single-quote followed by a character. For example, ~5,'0d can be used to print an integer in decimal radix in five columns with leading zeros, or ~5,'*d to get leading asterisks.

In place of a prefix parameter to a directive, V (or v) can be used. In this case, format takes an argument from args as a parameter to the directive. The argument should be an integer or character. If the arg used by a V parameter is nil, the effect is as if the parameter had been omitted. # can be used in place of a prefix parameter; it represents the number of args remaining to be processed. When used within a recursive format, in the context of ~? or ~{, the # prefix parameter represents the number of format arguments remaining within the recursive call.

Examples of format strings:

"~S"        ;This is an S directive with no parameters or modifiers.  
"~3,-4:@s"  ;This is an S directive with two parameters, 3 and -4,    
            ; and both the colon and at-sign flags.                   
"~,+4S"     ;Here the first prefix parameter is omitted and takes     
            ; on its default value, while the second parameter is 4.  

Figure 22-6. Examples of format control strings

format sends the output to destination. If destination is nil, format creates and returns a string containing the output from control-string. If destination is non-nil, it must be a string with a fill pointer, a stream, or the symbol t. If destination is a string with a fill pointer, the output is added to the end of the string. If destination is a stream, the output is sent to that stream. If destination is t, the output is sent to standard output.

In the description of the directives that follows, the term arg in general refers to the next item of the set of args to be processed. The word or phrase at the beginning of each description is a mnemonic for the directive. format directives do not bind any of the printer control variables (*print-...*) except as specified in the following descriptions. Implementations may specify the binding of new, implementation-specific printer control variables for each format directive, but they may neither bind any standard printer control variables not specified in description of a format directive nor fail to bind any standard printer control variables as specified in the description.

> * 22.3.1 [FORMAT Basic Output](#FORMATBasicOutput)
> * 22.3.2 [FORMAT Radix Control](#FORMATRadixControl)
> * 22.3.3 [FORMAT Floating-Point Printers](#FORMATFloatingPointPrinters)
> * 22.3.4 [FORMAT Printer Operations](#FORMATPrinterOperations)
> * 22.3.5 [FORMAT Pretty Printer Operations](#FORMATPrettyPrinterOperations)
> * 22.3.6 [FORMAT Layout Control](#FORMATLayoutControl)
> * 22.3.7 [FORMAT Control-Flow Operations](#FORMATControlFlowOperation)
> * 22.3.8 [FORMAT Miscellaneous Operations](#FORMATMiscellaneousOperation)
> * 22.3.9 [FORMAT Miscellaneous Pseudo-Operations](#FORMATMiscellaneousPseudoOperation)
> * 22.3.10 [Additional Information about FORMAT Operations](#AddInfoFORMATOperations)
> * 22.3.11 [Examples of FORMAT](#ExamplesFORMAT)
> * 22.3.12 [Notes about FORMAT](#NotesFORMAT)


### 22.3.1 <span id="FORMATBasicOutput">FORMAT Basic Output</span>

> * 22.3.1.1 [Tilde C: Character](#TildeCCharacter)
> * 22.3.1.2 [Tilde Percent: Newline](#TildePercentNewline)
> * 22.3.1.3 [Tilde Ampersand: Fresh-Line](#TildeAmpersandFreshLine)
> * 22.3.1.4 [Tilde Vertical-Bar: Page](#TildeVerticalBarPage)
> * 22.3.1.5 [Tilde Tilde: Tilde](#TildeTildeTilde)

#### 22.3.1.1 <span id="TildeCCharacter">Tilde C: Character</span>

The next arg should be a character; it is printed according to the modifier flags.

~C prints the character as if by using write-char if it is a simple character. Characters that are not simple are not necessarily printed as if by write-char, but are displayed in an implementation-defined, abbreviated format. For example,

 (format nil "~C" #\A) =>  "A"
 (format nil "~C" #\Space) =>  " "

~:C is the same as ~C for printing characters, but other characters are ``spelled out.'' The intent is that this is a ``pretty'' format for printing characters. For simple characters that are not printing, what is spelled out is the name of the character (see char-name). For characters that are not simple and not printing, what is spelled out is implementation-defined. For example,

 (format nil "~:C" #\A) =>  "A"
 (format nil "~:C" #\Space) =>  "Space"
;; This next example assumes an implementation-defined "Control" attribute.
 (format nil "~:C" #\Control-Space)
=>  "Control-Space"
OR=>  "c-Space"

~:@C prints what ~:C would, and then if the character requires unusual shift keys on the keyboard to type it, this fact is mentioned. For example,

 (format nil "~:@C" #\Control-Partial) =>  "Control-<PARTIAL> (Top-F)"  

This is the format used for telling the user about a key he is expected to type, in prompts, for instance. The precise output may depend not only on the implementation, but on the particular I/O devices in use.

~@C prints the character in a way that the Lisp reader can understand, using #\ syntax.

~@C binds *print-escape* to t. 

#### 22.3.1.2 <span id="TildePercentNewline">Tilde Percent: Newline</span>

This outputs a #\Newline character, thereby terminating the current output line and beginning a new one. ~n% outputs n newlines. No arg is used. 


#### 22.3.1.3 <span id="TildeAmpersandFreshLine">Tilde Ampersand: Fresh-Line</span>

Unless it can be determined that the output stream is already at the beginning of a line, this outputs a newline. ~n& calls fresh-line and then outputs n-1 newlines. ~0& does nothing.


#### 22.3.1.4 <span id="TildeVerticalBarPage">Tilde Vertical-Bar: Page</span>

This outputs a page separator character, if possible. ~n| does this n times. 


#### 22.3.1.5 <span id="TildeTildeTilde">Tilde Tilde: Tilde</span>

This outputs a tilde. ~n~ outputs n tildes. 


### 22.3.2 <span id="FORMATRadixControl">FORMAT Radix Control</span>

> * 22.3.2.1 [Tilde R: Radix](#TildeRRadix)
> * 22.3.2.2 [Tilde D: Decimal](#TildeDDecimal)
> * 22.3.2.3 [Tilde B: Binary](#TildeBBinary)
> * 22.3.2.4 [Tilde O: Octal](#TildeOOctal)
> * 22.3.2.5 [Tilde X: Hexadecimal](#TildeXHexadecimal)

#### 22.3.2.1 <span id="TildeRRadix">Tilde R: Radix</span>

~nR prints arg in radix n. The modifier flags and any remaining parameters are used as for the ~D directive. ~D is the same as ~10R. The full form is ~radix,mincol,padchar,commachar,comma-intervalR.

If no prefix parameters are given to ~R, then a different interpretation is given. The argument should be an integer. For example, if arg is 4:

* ~R prints arg as a cardinal English number: four.

* ~:R prints arg as an ordinal English number: fourth.

* ~@R prints arg as a Roman numeral: IV.

* ~:@R prints arg as an old Roman numeral: IIII.

For example:

 (format nil "~,,' ,4:B" 13) =>  "1101"
 (format nil "~,,' ,4:B" 17) =>  "1 0001"
 (format nil "~19,0,' ,4:B" 3333) =>  "0000 1101 0000 0101"
 (format nil "~3,,,' ,2:R" 17) =>  "1 22"
 (format nil "~,,'|,2:D" #xFFFF) =>   "6|55|35"

If and only if the first parameter, n, is supplied, ~R binds *print-escape* to false, *print-radix* to false, *print-base* to n, and *print-readably* to false.

If and only if no parameters are supplied, ~R binds *print-base* to 10. 

#### 22.3.2.2 <span id="TildeDDecimal">Tilde D: Decimal</span>

An arg, which should be an integer, is printed in decimal radix. ~D will never put a decimal point after the number.

~mincolD uses a column width of mincol; spaces are inserted on the left if the number requires fewer than mincol columns for its digits and sign. If the number doesn't fit in mincol columns, additional columns are used as needed.

~mincol,padcharD uses padchar as the pad character instead of space.

If arg is not an integer, it is printed in ~A format and decimal base.

The @ modifier causes the number's sign to be printed always; the default is to print it only if the number is negative. The : modifier causes commas to be printed between groups of digits; commachar may be used to change the character used as the comma. comma-interval must be an integer and defaults to 3. When the : modifier is given to any of these directives, the commachar is printed between groups of comma-interval digits.

Thus the most general form of ~D is ~mincol,padchar,commachar,comma-intervalD.

~D binds *print-escape* to false, *print-radix* to false, *print-base* to 10, and *print-readably* to false. 


#### 22.3.2.3 <span id="TildeBBinary">Tilde B: Binary</span>

This is just like ~D but prints in binary radix (radix 2) instead of decimal. The full form is therefore ~mincol,padchar,commachar,comma-intervalB.

~B binds *print-escape* to false, *print-radix* to false, *print-base* to 2, and *print-readably* to false. 


#### 22.3.2.4 <span id="TildeOOctal">Tilde O: Octal</span>

This is just like ~D but prints in octal radix (radix 8) instead of decimal. The full form is therefore ~mincol,padchar,commachar,comma-intervalO.

~O binds *print-escape* to false, *print-radix* to false, *print-base* to 8, and *print-readably* to false. 


#### 22.3.2.5 <span id="TildeXHexadecimal">Tilde X: Hexadecimal</span>

This is just like ~D but prints in hexadecimal radix (radix 16) instead of decimal. The full form is therefore ~mincol,padchar,commachar,comma-intervalX.

~X binds *print-escape* to false, *print-radix* to false, *print-base* to 16, and *print-readably* to false. 


### 22.3.3 <span id="FORMATFloatingPointPrinters">FORMAT Floating-Point Printers</span>

> * 22.3.3.1 [Tilde F: Fixed-Format Floating-Point](#TildeFFixedFormat)
> * 22.3.3.2 [Tilde E: Exponential Floating-Point](#TildeEExponential)
> * 22.3.3.3 [Tilde G: General Floating-Point](#TildeGGeneral)
> * 22.3.3.4 [Tilde Dollarsign: Monetary Floating-Point](#TildeDollarsignMonetary)


#### 22.3.3.1 <span id="TildeFFixedFormat">Tilde F: Fixed-Format Floating-Point</span>

The next arg is printed as a float.

The full form is ~w,d,k,overflowchar,padcharF. The parameter w is the width of the field to be printed; d is the number of digits to print after the decimal point; k is a scale factor that defaults to zero.

Exactly w characters will be output. First, leading copies of the character padchar (which defaults to a space) are printed, if necessary, to pad the field on the left. If the arg is negative, then a minus sign is printed; if the arg is not negative, then a plus sign is printed if and only if the @ modifier was supplied. Then a sequence of digits, containing a single embedded decimal point, is printed; this represents the magnitude of the value of arg times 10^k, rounded to d fractional digits. When rounding up and rounding down would produce printed values equidistant from the scaled value of arg, then the implementation is free to use either one. For example, printing the argument 6.375 using the format ~4,2F may correctly produce either 6.37 or 6.38. Leading zeros are not permitted, except that a single zero digit is output before the decimal point if the printed value is less than one, and this single zero digit is not output at all if w=d+1.

If it is impossible to print the value in the required format in a field of width w, then one of two actions is taken. If the parameter overflowchar is supplied, then w copies of that parameter are printed instead of the scaled value of arg. If the overflowchar parameter is omitted, then the scaled value is printed using more than w characters, as many more as may be needed.

If the w parameter is omitted, then the field is of variable width. In effect, a value is chosen for w in such a way that no leading pad characters need to be printed and exactly d characters will follow the decimal point. For example, the directive ~,2F will print exactly two digits after the decimal point and as many as necessary before the decimal point.

If the parameter d is omitted, then there is no constraint on the number of digits to appear after the decimal point. A value is chosen for d in such a way that as many digits as possible may be printed subject to the width constraint imposed by the parameter w and the constraint that no trailing zero digits may appear in the fraction, except that if the fraction to be printed is zero, then a single zero digit should appear after the decimal point if permitted by the width constraint.

If both w and d are omitted, then the effect is to print the value using ordinary free-format output; prin1 uses this format for any number whose magnitude is either zero or between 10^-3 (inclusive) and 10^7 (exclusive).

If w is omitted, then if the magnitude of arg is so large (or, if d is also omitted, so small) that more than 100 digits would have to be printed, then an implementation is free, at its discretion, to print the number using exponential notation instead, as if by the directive ~E (with all parameters to ~E defaulted, not taking their values from the ~F directive).

If arg is a rational number, then it is coerced to be a single float and then printed. Alternatively, an implementation is permitted to process a rational number by any other method that has essentially the same behavior but avoids loss of precision or overflow because of the coercion. If w and d are not supplied and the number has no exact decimal representation, for example 1/3, some precision cutoff must be chosen by the implementation since only a finite number of digits may be printed.

If arg is a complex number or some non-numeric object, then it is printed using the format directive ~wD, thereby printing it in decimal radix and a minimum field width of w.

~F binds *print-escape* to false and *print-readably* to false. 


#### 22.3.3.2 <span id="TildeEExponential">Tilde E: Exponential Floating-Point</span>

The next arg is printed as a float in exponential notation.

The full form is ~w,d,e,k,overflowchar,padchar,exponentcharE. The parameter w is the width of the field to be printed; d is the number of digits to print after the decimal point; e is the number of digits to use when printing the exponent; k is a scale factor that defaults to one (not zero).

Exactly w characters will be output. First, leading copies of the character padchar (which defaults to a space) are printed, if necessary, to pad the field on the left. If the arg is negative, then a minus sign is printed; if the arg is not negative, then a plus sign is printed if and only if the @ modifier was supplied. Then a sequence of digits containing a single embedded decimal point is printed. The form of this sequence of digits depends on the scale factor k. If k is zero, then d digits are printed after the decimal point, and a single zero digit appears before the decimal point if the total field width will permit it. If k is positive, then it must be strictly less than d+2; k significant digits are printed before the decimal point, and d-k+1 digits are printed after the decimal point. If k is negative, then it must be strictly greater than -d; a single zero digit appears before the decimal point if the total field width will permit it, and after the decimal point are printed first -k zeros and then d+k significant digits. The printed fraction must be properly rounded. When rounding up and rounding down would produce printed values equidistant from the scaled value of arg, then the implementation is free to use either one. For example, printing the argument 637.5 using the format ~8,2E may correctly produce either 6.37E+2 or 6.38E+2.

Following the digit sequence, the exponent is printed. First the character parameter exponentchar is printed; if this parameter is omitted, then the exponent marker that prin1 would use is printed, as determined from the type of the float and the current value of *read-default-float-format*. Next, either a plus sign or a minus sign is printed, followed by e digits representing the power of ten by which the printed fraction must be multiplied to properly represent the rounded value of arg.

If it is impossible to print the value in the required format in a field of width w, possibly because k is too large or too small or because the exponent cannot be printed in e character positions, then one of two actions is taken. If the parameter overflowchar is supplied, then w copies of that parameter are printed instead of the scaled value of arg. If the overflowchar parameter is omitted, then the scaled value is printed using more than w characters, as many more as may be needed; if the problem is that d is too small for the supplied k or that e is too small, then a larger value is used for d or e as may be needed.

If the w parameter is omitted, then the field is of variable width. In effect a value is chosen for w in such a way that no leading pad characters need to be printed.

If the parameter d is omitted, then there is no constraint on the number of digits to appear. A value is chosen for d in such a way that as many digits as possible may be printed subject to the width constraint imposed by the parameter w, the constraint of the scale factor k, and the constraint that no trailing zero digits may appear in the fraction, except that if the fraction to be printed is zero then a single zero digit should appear after the decimal point.

If the parameter e is omitted, then the exponent is printed using the smallest number of digits necessary to represent its value.

If all of w, d, and e are omitted, then the effect is to print the value using ordinary free-format exponential-notation output; prin1 uses a similar format for any non-zero number whose magnitude is less than 10^-3 or greater than or equal to 10^7. The only difference is that the ~E directive always prints a plus or minus sign in front of the exponent, while prin1 omits the plus sign if the exponent is non-negative.

If arg is a rational number, then it is coerced to be a single float and then printed. Alternatively, an implementation is permitted to process a rational number by any other method that has essentially the same behavior but avoids loss of precision or overflow because of the coercion. If w and d are unsupplied and the number has no exact decimal representation, for example 1/3, some precision cutoff must be chosen by the implementation since only a finite number of digits may be printed.

If arg is a complex number or some non-numeric object, then it is printed using the format directive ~wD, thereby printing it in decimal radix and a minimum field width of w.

~E binds *print-escape* to false and *print-readably* to false. 


#### 22.3.3.3 <span id="TildeGGeneral">Tilde G: General Floating-Point</span>

The next arg is printed as a float in either fixed-format or exponential notation as appropriate.

The full form is ~w,d,e,k,overflowchar,padchar,exponentcharG. The format in which to print arg depends on the magnitude (absolute value) of the arg. Let n be an integer such that 10^n-1 <= |arg| < 10^n. Let ee equal e+2, or 4 if e is omitted. Let ww equal w-ee, or nil if w is omitted. If d is omitted, first let q be the number of digits needed to print arg with no loss of information and without leading or trailing zeros; then let d equal (max q (min n 7)). Let dd equal d-n.

If 0 <= dd <= d, then arg is printed as if by the format directives

~ww,dd,,overflowchar,padcharF~ee@T

Note that the scale factor k is not passed to the ~F directive. For all other values of dd, arg is printed as if by the format directive

~w,d,e,k,overflowchar,padchar,exponentcharE

In either case, an @ modifier is supplied to the ~F or ~E directive if and only if one was supplied to the ~G directive.

~G binds *print-escape* to false and *print-readably* to false. 


#### 22.3.3.4 <span id="TildeDollarsignMonetary">Tilde Dollarsign: Monetary Floating-Point</span>

The next arg is printed as a float in fixed-format notation.

The full form is ~d,n,w,padchar$. The parameter d is the number of digits to print after the decimal point (default value 2); n is the minimum number of digits to print before the decimal point (default value 1); w is the minimum total width of the field to be printed (default value 0).

First padding and the sign are output. If the arg is negative, then a minus sign is printed; if the arg is not negative, then a plus sign is printed if and only if the @ modifier was supplied. If the : modifier is used, the sign appears before any padding, and otherwise after the padding. If w is supplied and the number of other characters to be output is less than w, then copies of padchar (which defaults to a space) are output to make the total field width equal w. Then n digits are printed for the integer part of arg, with leading zeros if necessary; then a decimal point; then d digits of fraction, properly rounded.

If the magnitude of arg is so large that more than m digits would have to be printed, where m is the larger of w and 100, then an implementation is free, at its discretion, to print the number using exponential notation instead, as if by the directive ~w,q,,,,padcharE, where w and padchar are present or omitted according to whether they were present or omitted in the ~$ directive, and where q=d+n-1, where d and n are the (possibly default) values given to the ~$ directive.

If arg is a rational number, then it is coerced to be a single float and then printed. Alternatively, an implementation is permitted to process a rational number by any other method that has essentially the same behavior but avoids loss of precision or overflow because of the coercion.

If arg is a complex number or some non-numeric object, then it is printed using the format directive ~wD, thereby printing it in decimal radix and a minimum field width of w.

~$ binds *print-escape* to false and *print-readably* to false. 


### 22.3.4 <span id="FORMATPrinterOperations">FORMAT Printer Operations</span>

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


### 22.3.5 <span id="FORMATPrettyPrinterOperations">FORMAT Pretty Printer Operations</span>

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


### 22.3.6 <span id="FORMATLayoutControl">FORMAT Layout Control</span>

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


#### 22.3.7 <span id="FORMATControlFlowOperation">FORMAT Control-Flow Operations</span>

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


### 22.3.8 <span id="FORMATMiscellaneousOperation">FORMAT Miscellaneous Operations</span>

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


### 22.3.9 <span id="FORMATMiscellaneousPseudoOperation">FORMAT Miscellaneous Pseudo-Operations</span>

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


### 22.3.10 <span id="AddInfoFORMATOperations">Additional Information about FORMAT Operations</span>

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


### 22.3.11 <span id="">Examples of FORMAT</span>

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


### 22.3.12 <span id="NotesFORMAT">Notes about FORMAT</span>

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

        检验传递给词法上当前逻辑块(lexically current logical block)的列表是否已经被耗尽; 见章节 22.2.1.1 (Dynamic Control of the Arrangement of Output). 如果这个列表已经被归约为 nil, pprint-exit-if-list-exhausted 终止这个词法上当前逻辑块的执行, 除了这个后缀的打印. 否则 pprint-exit-if-list-exhausted 返回 nil.

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

relative-to---either :block or :current.

n---a real.

stream---an output stream designator. The default is standard output.

* 描述(Description):

pprint-indent specifies the indentation to use in a logical block on stream. If stream is a pretty printing stream and the value of *print-pretty* is true, pprint-indent sets the indentation in the innermost dynamically enclosing logical block; otherwise, pprint-indent has no effect.

N specifies the indentation in ems. If relative-to is :block, the indentation is set to the horizontal position of the first character in the dynamically current logical block plus n ems. If relative-to is :current, the indentation is set to the current output position plus n ems. (For robustness in the face of variable-width fonts, it is advisable to use :current with an n of zero whenever possible.)

N can be negative; however, the total indentation cannot be moved left of the beginning of the line or left of the end of the rightmost per-line prefix---an attempt to move beyond one of these limits is treated the same as an attempt to move to that limit. Changes in indentation caused by pprint-indent do not take effect until after the next line break. In addition, in miser mode all calls to pprint-indent are ignored, forcing the lines corresponding to the logical block to line up under the first character in the block.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error is signaled if relative-to is any object other than :block or :current.

* 也见(See Also):

Section 22.3.5.3 (Tilde I: Indent)

* 注意(Notes): None. 


### <span id="M-PPRINT-LOGICAL-BLOCK">宏 PPRINT-LOGICAL-BLOCK</span>

* 语法(Syntax):

pprint-logical-block (stream-symbol object &key prefix per-line-prefix suffix) declaration* form*

=> nil

* 参数和值(Arguments and Values):

stream-symbol---a stream variable designator.

object---an object; evaluated.

:prefix---a string; evaluated. Complicated defaulting behavior; see below.

:per-line-prefix---a string; evaluated. Complicated defaulting behavior; see below.

:suffix---a string; evaluated. The default is the null string.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

* 描述(Description):

Causes printing to be grouped into a logical block.

The logical block is printed to the stream that is the value of the variable denoted by stream-symbol. During the execution of the forms, that variable is bound to a pretty printing stream that supports decisions about the arrangement of output and then forwards the output to the destination stream. All the standard printing functions (e.g., write, princ, and terpri) can be used to print output to the pretty printing stream. All and only the output sent to this pretty printing stream is treated as being in the logical block.

The prefix specifies a prefix to be printed before the beginning of the logical block. The per-line-prefix specifies a prefix that is printed before the block and at the beginning of each new line in the block. The :prefix and :pre-line-prefix arguments are mutually exclusive. If neither :prefix nor :per-line-prefix is specified, a prefix of the null string is assumed.

The suffix specifies a suffix that is printed just after the logical block.

The object is normally a list that the body forms are responsible for printing. If object is not a list, it is printed using write. (This makes it easier to write printing functions that are robust in the face of malformed arguments.) If *print-circle* is non-nil and object is a circular (or shared) reference to a cons, then an appropriate ``#n#'' marker is printed. (This makes it easy to write printing functions that provide full support for circularity and sharing abbreviation.) If *print-level* is not nil and the logical block is at a dynamic nesting depth of greater than *print-level* in logical blocks, ``#'' is printed. (This makes easy to write printing functions that provide full support for depth abbreviation.)

If either of the three conditions above occurs, the indicated output is printed on stream-symbol and the body forms are skipped along with the printing of the :prefix and :suffix. (If the body forms are not to be responsible for printing a list, then the first two tests above can be turned off by supplying nil for the object argument.)

In addition to the object argument of pprint-logical-block, the arguments of the standard printing functions (such as write, print, prin1, and pprint, as well as the arguments of the standard format directives such as ~A, ~S, (and ~W) are all checked (when necessary) for circularity and sharing. However, such checking is not applied to the arguments of the functions write-line, write-string, and write-char or to the literal text output by format. A consequence of this is that you must use one of the latter functions if you want to print some literal text in the output that is not supposed to be checked for circularity or sharing.

The body forms of a pprint-logical-block form must not perform any side-effects on the surrounding environment; for example, no variables must be assigned which have not been bound within its scope.

The pprint-logical-block macro may be used regardless of the value of *print-pretty*.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

*print-circle*, *print-level*.

* 异常情况(Exceptional Situations):

An error of type type-error is signaled if any of the :suffix, :prefix, or :per-line-prefix is supplied but does not evaluate to a string.

An error is signaled if :prefix and :pre-line-prefix are both used.

pprint-logical-block and the pretty printing stream it creates have dynamic extent. The consequences are undefined if, outside of this extent, output is attempted to the pretty printing stream it creates.

It is also unspecified what happens if, within this extent, any output is sent directly to the underlying destination stream.

* 也见(See Also):

pprint-pop, pprint-exit-if-list-exhausted, Section 22.3.5.2 (Tilde Less-Than-Sign: Logical Block)

* 注意(Notes):

One reason for using the pprint-logical-block macro when the value of *print-pretty* is nil would be to allow it to perform checking for dotted lists, as well as (in conjunction with pprint-pop) checking for *print-level* or *print-length* being exceeded.

Detection of circularity and sharing is supported by the pretty printer by in essence performing requested output twice. On the first pass, circularities and sharing are detected and the actual outputting of characters is suppressed. On the second pass, the appropriate ``#n='' and ``#n#'' markers are inserted and characters are output. This is why the restriction on side-effects is necessary. Obeying this restriction is facilitated by using pprint-pop, instead of an ordinary pop when traversing a list being printed by the body forms of the pprint-logical-block form.) 


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

See Section 22.2.2 (Examples of using the Pretty Printer).

* 副作用(Side Effects):

Output to stream.

* 受此影响(Affected By):

*print-pretty*, *print-miser*. The presence of containing logical blocks. The placement of newlines and conditional newlines.

* 异常情况(Exceptional Situations):

An error of type type-error is signaled if kind is not one of :linear, :fill, :miser, or :mandatory.

* 也见(See Also):

Section 22.3.5.1 (Tilde Underscore: Conditional Newline), Section 22.2.2 (Examples of using the Pretty Printer)

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

If pprint-logical-block is given a `list' argument of nil---because it is not processing a list---pprint-pop can still be used to obtain support for *print-length*. In this situation, the first and third tests above are disabled and pprint-pop always returns nil. See Section 22.2.2 (Examples of using the Pretty Printer)---specifically, the pprint-vector example.

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

    The method may wish to perform specialized line breaking or other output conditional on the value of *print-pretty*. For further information, see (for example) the macro pprint-fill. See also Section 22.2.1.4 (Pretty Print Dispatch Tables) and Section 22.2.2 (Examples of using the Pretty Printer).

*print-length*

    Methods that produce output of indefinite length must obey *print-length*. For further information, see (for example) the macros pprint-logical-block and pprint-pop. See also Section 22.2.1.4 (Pretty Print Dispatch Tables) and Section 22.2.2 (Examples of using the Pretty Printer).

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

pprint-fill, pprint-logical-block, pprint-pop, write, *print-readably*, *print-escape*, *print-pretty*, *print-length*, Section 22.1.3 (默认 Print-Object 方法), Section 22.1.3.12 (打印结构体), Section 22.2.1.4 (Pretty Print Dispatch Tables), Section 22.2.2 (Examples of using the Pretty Printer)

* 注意(Notes): None. 


### <span id="M-PRINT-UNREADABLE-OBJECT">宏 PRINT-UNREADABLE-OBJECT</span>

* 语法(Syntax):

print-unreadable-object (object stream &key type identity) form* => nil

* 参数和值(Arguments and Values):

object---an object; evaluated.

stream---a stream designator; evaluated.

type---a generalized boolean; evaluated.

identity---a generalized boolean; evaluated.

forms---an implicit progn.

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

length---a non-negative integer, or nil.

level---a non-negative integer, or nil.

lines---a non-negative integer, or nil.

miser-width---a non-negative integer, or nil.

pprint-dispatch---a pprint dispatch table.

pretty---一个广义 boolean.

radix---一个广义 boolean.

readably---一个广义 boolean.

right-margin---a non-negative integer, or nil.

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

readtable-case, Section 22.3.4 (FORMAT Printer Operations)

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

length---a non-negative integer, or nil.

level---a non-negative integer, or nil.

lines---a non-negative integer, or nil.

miser-width---a non-negative integer, or nil.

pprint-dispatch---a pprint dispatch table.

pretty---一个广义 boolean.

radix---一个广义 boolean.

readably---一个广义 boolean.

right-margin---a non-negative integer, or nil.

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

a non-negative integer, or nil.

* 初始值(Initial Value):

nil.

* 描述(Description):

*print-level* controls how many levels deep a nested object will print. If it is false, then no control is exercised. Otherwise, it is an integer indicating the maximum level to be printed. An object to be printed is at level 0; its components (as of a list or vector) are at level 1; and so on. If an object to be recursively printed has components and is at a level equal to or greater than the value of *print-level*, then the object is printed as ``#''.

*print-length* controls how many elements at a given level are printed. If it is false, there is no limit to the number of components printed. Otherwise, it is an integer indicating the maximum number of elements of an object to be printed. If exceeded, the printer will print ``...'' in place of the other elements. In the case of a dotted list, if the list contains exactly as many elements as the value of *print-length*, the terminating atom is printed rather than printing ``...''

*print-level* and *print-length* affect the printing of an any object printed with a list-like syntax. They do not affect the printing of symbols, strings, and bit vectors.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 也见(See Also):

write

* 注意(Notes): None. 


### <span id="V-PRINT-LINES">变量 *PRINT-LINES*</span>

* 值类型(Value Type):

a non-negative integer, or nil.

* 初始值(Initial Value):

nil.

* 描述(Description):

When the value of *print-lines* is other than nil, it is a limit on the number of output lines produced when something is pretty printed. If an attempt is made to go beyond that many lines, ``..'' is printed at the end of the last line followed by all of the suffixes (closing delimiters) that are pending to be printed.

* 示例(Examples):

 (let ((*print-right-margin* 25) (*print-lines* 3))
   (pprint '(progn (setq a 1 b 2 c 3 d 4))))
>>  (PROGN (SETQ A 1
>>               B 2
>>               C 3 ..))
=>  <no values>

* 也见(See Also): None.

* 注意(Notes):

The ``..'' notation is intentionally different than the ``...'' notation used for level abbreviation, so that the two different situations can be visually distinguished.

This notation is used to increase the likelihood that the Lisp reader will signal an error if an attempt is later made to read the abbreviated output. Note however that if the truncation occurs in a string, as in "This string has been trunc..", the problem situation cannot be detected later and no such error will be signaled. 


### <span id="V-PRINT-MISER-WIDTH">变量 *PRINT-MISER-WIDTH*</span>

* 值类型(Value Type):

a non-negative integer, or nil.

* 初始值(Initial Value):

implementation-dependent

* 描述(Description):

If it is not nil, the pretty printer switches to a compact style of output (called miser style) whenever the width available for printing a substructure is less than or equal to this many ems.

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes): None.



### <span id="V-PRINT-PPRINT-DISPATCH">变量 *PRINT-PPRINT-DISPATCH*</span>

* 值类型(Value Type):

a pprint dispatch table.

* 初始值(Initial Value):

implementation-dependent, but the initial entries all use a special class of priorities that have the property that they are less than every priority that can be specified using set-pprint-dispatch, so that the initial contents of any entry can be overridden.

* 描述(Description):

The pprint dispatch table which currently controls the pretty printer.

* 示例(Examples): None.

* 也见(See Also):

*print-pretty*, Section 22.2.1.4 (Pretty Print Dispatch Tables)

* 注意(Notes):

The intent is that the initial value of this variable should cause `traditional' pretty printing of code. In general, however, you can put a value in *print-pprint-dispatch* that makes pretty-printed output look exactly like non-pretty-printed output. Setting *print-pretty* to true just causes the functions contained in the current pprint dispatch table to have priority over normal print-object methods; it has no magic way of enforcing that those functions actually produce pretty output. For details, see Section 22.2.1.4 (Pretty Print Dispatch Tables). 


### <span id="V-PRINT-PRETTY">变量 *PRINT-PRETTY*</span>

* 值类型(Value Type):

a generalized boolean.

* 初始值(Initial Value):

implementation-dependent.

* 描述(Description):

Controls whether the Lisp printer calls the pretty printer.

If it is false, the pretty printer is not used and a minimum of whitespace[1] is output when printing an expression.

If it is true, the pretty printer is used, and the Lisp printer will endeavor to insert extra whitespace[1] where appropriate to make expressions more readable.

*print-pretty* has an effect even when the value of *print-escape* is false.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 也见(See Also):

write

* 注意(Notes): None. 


### <span id="V-PRINT-READABLY">变量 *PRINT-READABLY*</span>

* 值类型(Value Type):

a generalized boolean.

* 初始值(Initial Value):

false.

* 描述(Description):

If *print-readably* is true, some special rules for printing objects go into effect. Specifically, printing any object O1 produces a printed representation that, when seen by the Lisp reader while the standard readtable is in effect, will produce an object O2 that is similar to O1. The printed representation produced might or might not be the same as the printed representation produced when *print-readably* is false. If printing an object readably is not possible, an error of type print-not-readable is signaled rather than using a syntax (e.g., the ``#<'' syntax) that would not be readable by the same implementation. If the value of some other printer control variable is such that these requirements would be violated, the value of that other variable is ignored.

Specifically, if *print-readably* is true, printing proceeds as if *print-escape*, *print-array*, and *print-gensym* were also true, and as if *print-length*, *print-level*, and *print-lines* were false.

If *print-readably* is false, the normal rules for printing and the normal interpretations of other printer control variables are in effect.

Individual methods for print-object, including user-defined methods, are responsible for implementing these requirements.

If *read-eval* is false and *print-readably* is true, any such method that would output a reference to the ``#.'' reader macro will either output something else or will signal an error (as described above).

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 也见(See Also):

write, print-unreadable-object

* 注意(Notes):

The rules for ``similarity'' imply that #A or #( syntax cannot be used for arrays of element type other than t. An implementation will have to use another syntax or signal an error of type print-not-readable. 


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