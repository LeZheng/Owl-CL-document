# 10. 符号

> * 10.1 [符号概念](#SymbolConcepts)
> * 10.2 [符号字典](#TheSymbolsDictionary)

## 10.1 <span id="SymbolConcepts">符号概念</span>

下面这段列出了一些可应用于符号属性列表的已定义的名字.

    get  remprop  symbol-plist  

    Figure 10-1. 属性列表已定义的名字

下一段列出了一些可应用于创建和查询符号的已定义名字.

    copy-symbol  keywordp     symbol-package  
    gensym       make-symbol  symbol-value    
    gentemp      symbol-name                  

    Figure 10-2. 符号创建和查询的已定义名字


## 10.2 <span id="TheSymbolsDictionary">符号字典</span>

> * [系统类 SYMBOL](#SC-SYMBOL)
> * [类型 KEYWORD](#T-KEYWORD)
> * [函数 SYMBOLP](#F-SYMBOLP)
> * [函数 KEYWORDP](#F-KEYWORDP)
> * [函数 MAKE-SYMBOL](#F-MAKE-SYMBOL)
> * [函数 COPY-SYMBOL](#F-COPY-SYMBOL)
> * [函数 GENSYM](#F-GENSYM)
> * [变量 *GENSYM-COUNTER*](#V-GENSYM-COUNTER)
> * [函数 GENTEMP](#F-GENTEMP)
> * [访问器 SYMBOL-FUNCTION](#A-SYMBOL-FUNCTION)
> * [函数 SYMBOL-NAME](#F-SYMBOL-NAME)
> * [函数 SYMBOL-PACKAGE](#F-SYMBOL-PACKAGE)
> * [访问器 SYMBOL-PLIST](#A-SYMBOL-PLIST)
> * [访问器 SYMBOL-VALUE](#A-SYMBOL-VALUE)
> * [访问器 GET](#A-GET)
> * [函数 REMPROP](#F-REMPROP)
> * [函数 BOUNDP](#F-BOUNDP)
> * [函数 MAKUNBOUND](#F-MAKUNBOUND)
> * [函数 SET](#F-SET)
> * [状况类型 UNBOUND-VARIABLE](#CT-UNBOUND-VARIABLE)


### <span id="SC-SYMBOL">系统类 SYMBOL</span>

* 类优先级列表(Class Precedence List):

        symbol, t

* 描述(Description):

        符号被用于它们的对象标识来命名 Common Lisp 中不同的实体, 包括 (但不限于) 像变量和函数这样的语言实体.

        符号可以被一起收集到包中. 如果一个符号在一个包中是可访问的就是那个符号被捕捉到那个包中; 相同符号可以被捕捉到不止一个包中. 如果一个符号没有被捕捉到任何包中, 就说它是未捕捉的.

        一个被捕捉的符号在它是可访问的任何包中通过它的名称是唯一可识别的.

        符号有着以下属性. 出于历史原因, 它们有时作为 cell 被引用, 虽然符号和它们的属性的实际内部表示是依赖于具体实现的.

        Name

            一个符号的名字是用于标识这个符号的一个字符串. 每个符号有一个名字, 如果那个名字被修改那么结果是未定义的. 这个名字被用作这个符号的外部的, 打印的表示部分; 见章节 2.1 (Character Syntax). 函数 symbol-name 返回一个给定符号的名字. 一个符号的名字中可能有任何字符.

        Package

            在这个 cell 的这个对象被称为这个符号的 home 包. 如果这个 home 包是 nil, 那么这个符号有时就说是没有 home 包.

            当一个符号被首次创建, 它没有 home 包. 当它被第一次捕捉时, 最初被捕捉所在包称为它的 home 包. 一个符号的 home 包可以通过函数 symbol-package 来访问.

            如果一个符号从它的 home 包中被解除捕捉, 它的 home 包会被设置为 nil. 这个符号可能或可能不是一个未被捕捉的符号取决于这里是否存在另一个包捕捉了这个符号. 一个没有 home 包的符号显然因此被称为是未被捕捉的.

            如果尝试去修改在 COMMON-LISP 包或者 KEYWORD 包中的一个符号的 home 包那么结果是未定义的.

        Property list

            一个符号的属性列表为关联已命名属性和那个符号提供了一个机制. 关于添加和移除的操作对于这个属性列表是破坏性的. Common Lisp 为直接操作属性列表对象 (比如, see getf, remf, 和 symbol-plist) 以及通过对这个符号的引用来隐式操作一个符号的属性列表 (比如, 见 get 和 remprop) 都提供了操作符. 和一个新的符号关联的属性列表被初始化为 null.

        Value

            如果一个符号有一个值属性, 它就被称为是绑定的, 并且这个事实可以通过函数 boundp 来被检测. 被包含在一个绑定的符号的值 cell 中的对象是那个符号命名的全局变量的值, 并且可以通过函数 symbol-value 来访问. 一个符号可以通过函数 makunbound 变得是未绑定的.

            如果尝试去修改一个命名一个常变量的符号的值或者使这样一个符号变为未绑定的, 那么后果是未定义的.

        Function

            如果一个符号有一个函数属性, 它就被称为是 fbound 的, 并且这个事实可以通过函数 fboundp 来检测. 如果这个符号是一个全局环境中的函数的名字, 这个函数 cell 就包含了那个函数, 并且可以通过函数 symbol-function 来访问. 如果这个符号是全局环境中的一个宏 (见 macro-function) 或者一个特殊操作符 (见 special-operator-p) 的名字, 那么这个符号就是 fbound 的, 并且可以通过函数 symbol-function 来访问, 但是那个函数 cell 所包含的对象的类型和目的是依赖于具体实现. 一个符号可以通过函数 fmakunbound 来变为 funbound 的.

            如果尝试去修改命名一个特殊表达式形式的符号的函数值, 那么后果是未定义的.

        在一个符号的值 cell 和函数 cell 上的操作有时被描述为对符号自身的影响, 但是用户应该注意在那些 cell 的内容和全局变量或全局函数定义之间分别存在一个紧密的关系.

        符号被用作词法变量和词法函数定义的标识, 但是在那个角色中<!--TODO but in that role ??-->, 只有它们的对象标识是有意义的. Common Lisp 没有在一个符号上提供在一个词法变量或一个词法函数定义上有任何效果的操作符.

* 也见(See Also):

        Section 2.3.4 (Symbols as Tokens), Section 2.3.1.1 (Potential Numbers as Tokens), Section 22.1.3.3 (Printing Symbols)


### <span id="T-KEYWORD">类型 KEYWORD</span>

* 超类型(Supertypes):

        keyword, symbol, t

* 描述(Description):

        类型 keyword 包括所有被捕捉在 KEYWORD 包的符号.

        捕捉一个符号到 KEYWORD 包中有三个必然的影响:

        1. 它导致这个符号被绑定到它自身.
        2. 它导致这个符号变为这个 KEYWORD 包的外部符号.
        3. 它导致这个符号变为一个常变量.

* 也见(See Also):

        keywordp


### <span id="F-SYMBOLP">函数 SYMBOLP</span>

* 语法(Syntax):

        symbolp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果对象 object 是 symbol 类型的就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (symbolp 'elephant) =>  true
    (symbolp 12) =>  false
    (symbolp nil) =>  true
    (symbolp '()) =>  true
    (symbolp :test) =>  true
    (symbolp "hello") =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

        keywordp, symbol, typep

* 注意(Notes):

        (symbolp object) ==  (typep object 'symbol)


 ### <span id="F-KEYWORDP">函数 KEYWORDP</span>

 * 语法(Syntax):

        keywordp object => generalized-boolean

 * 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

 * 描述(Description):

        如果对象 object 是一个关键字就返回 true; 否则, 返回 false.

 * 示例(Examples):

    ```LISP
      (keywordp 'elephant) =>  false
      (keywordp 12) =>  false
      (keywordp :test) =>  true
      (keywordp ':test) =>  true
      (keywordp nil) =>  false
      (keywordp :nil) =>  true
      (keywordp '(:test)) =>  false
      (keywordp "hello") =>  false
      (keywordp ":hello") =>  false
      (keywordp '&optional) =>  false
    ```

 * 副作用(Side Effects): None.

 * 受此影响(Affected By): None.

 * 异常情况(Exceptional Situations):  None.

 * 也见(See Also):

        constantp, keyword, symbolp, symbol-package

 * 注意(Notes): None.


### <span id="F-MAKE-SYMBOL">函数 MAKE-SYMBOL</span>

* 语法(Syntax):

        make-symbol name => new-symbol

* 参数和值(Arguments and Values):

        name---一个字符串.
        new-symbol---一个新的, 未被捕捉的符号.

* 描述(Description):

        make-symbol 创建并返回一个新的, 未捕捉的名为给定名字 name 的符号. 这个新的符号 new-symbol 既没有被绑定也没有被 fbound 并且有一个 null 属性列表.

        成为这个新符号 new-symbol 的名字的字符串是那个给定的名字 name 还是它的拷贝是依赖于具体实现的. 一旦一个字符串已经被给定作为给 make-symbol 的 name 参数, 如果后面去修改这个字符串那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (setq temp-string "temp") =>  "temp"
    (setq temp-symbol (make-symbol temp-string)) =>  #:|temp|
    (symbol-name temp-symbol) =>  "temp"
    (eq (symbol-name temp-symbol) temp-string) =>  implementation-dependent
    (find-symbol "temp") =>  NIL, NIL
    (eq (make-symbol temp-string) (make-symbol temp-string)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果名字 name 不是一个字符串那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        copy-symbol

* 注意(Notes):

        make-symbol 不会尝试去转换这个名字的大小写为大写的. 对于符号发生的仅有的大小写转换是通过 Lisp 读取器完成的. 对于符号创建的编程接口保留大小写, 并且对于捕捉符号的编程接口是大小写敏感的.


### <span id="F-COPY-SYMBOL">函数 COPY-SYMBOL</span>

* 语法(Syntax):

        copy-symbol symbol &optional copy-properties => new-symbol

* 参数和值(Arguments and Values):

        symbol---一个符号.
        copy-properties---一个广义的 boolean. 默认是 false.
        new-symbol---一个新的, 未捕捉的符号.

* 描述(Description):

        copy-symbol 返回一个新的, 未被捕捉的符号, 它的名字和给定符号的名字是 string= 的或者可能是一样的.

        如果 copy-properties 是 false, 这个新符号 new-symbol 即没有被绑定也没有被 fbound 并且有一个 null 属性列表. 如果 copy-properties 是 true, 那么这个新符号 new-symbol 的初始值是那个符号 symbol 的值, 这个新符号的初始函数定义是那个符号 symbol 的函数值, 并且那个新符号 new-symbol 的属性列表是符号 symbol 的属性列表的一个拷贝.

* 示例(Examples):

    ```LISP
    (setq fred 'fred-smith) =>  FRED-SMITH
    (setf (symbol-value fred) 3) =>  3
    (setq fred-clone-1a (copy-symbol fred nil)) =>  #:FRED-SMITH
    (setq fred-clone-1b (copy-symbol fred nil)) =>  #:FRED-SMITH
    (setq fred-clone-2a (copy-symbol fred t))   =>  #:FRED-SMITH
    (setq fred-clone-2b (copy-symbol fred t))   =>  #:FRED-SMITH
    (eq fred fred-clone-1a) =>  false
    (eq fred-clone-1a fred-clone-1b) =>  false
    (eq fred-clone-2a fred-clone-2b) =>  false
    (eq fred-clone-1a fred-clone-2a) =>  false
    (symbol-value fred) =>  3
    (boundp fred-clone-1a) =>  false
    (symbol-value fred-clone-2a) =>  3
    (setf (symbol-value fred-clone-2a) 4) =>  4
    (symbol-value fred) =>  3
    (symbol-value fred-clone-2a) =>  4
    (symbol-value fred-clone-2b) =>  3
    (boundp fred-clone-1a) =>  false
    (setf (symbol-function fred) #'(lambda (x) x)) =>  #<FUNCTION anonymous>
    (fboundp fred) =>  true
    (fboundp fred-clone-1a) =>  false
    (fboundp fred-clone-2a) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        make-symbol

* 注意(Notes):

        鼓励实现者不要去拷贝那个没有必要是那个符号的名字的字符串. 除非这里有一个好的理由去做这个, 对于这个新符号 new-symbol 的名字的正常的具体实现策略是和给定符号 symbol 的名字是相同的.


### <span id="F-GENSYM">函数 GENSYM</span>

* 语法(Syntax):

gensym &optional x => new-symbol

* 参数和值(Arguments and Values):

x---a string or a non-negative integer. Complicated defaulting behavior; see below.

new-symbol---a fresh, uninterned symbol.

* 描述(Description):

Creates and returns a fresh, uninterned symbol, as if by calling make-symbol. (The only difference between gensym and make-symbol is in how the new-symbol's name is determined.)

The name of the new-symbol is the concatenation of a prefix, which defaults to "G", and a suffix, which is the decimal representation of a number that defaults to the value of *gensym-counter*.

If x is supplied, and is a string, then that string is used as a prefix instead of "G" for this call to gensym only.

If x is supplied, and is an integer, then that integer, instead of the value of *gensym-counter*, is used as the suffix for this call to gensym only.

If and only if no explicit suffix is supplied, *gensym-counter* is incremented after it is used.

* 示例(Examples):

 (setq sym1 (gensym)) =>  #:G3142
 (symbol-package sym1) =>  NIL
 (setq sym2 (gensym 100)) =>  #:G100
 (setq sym3 (gensym 100)) =>  #:G100
 (eq sym2 sym3) =>  false
 (find-symbol "G100") =>  NIL, NIL
 (gensym "T") =>  #:T3143
 (gensym) =>  #:G3144

* 副作用(Side Effects):

Might increment *gensym-counter*.

* 受此影响(Affected By):

*gensym-counter*

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if x is not a string or a non-negative integer.

* 也见(See Also):

gentemp, *gensym-counter*

* 注意(Notes):

The ability to pass a numeric argument to gensym has been deprecated; explicitly binding *gensym-counter* is now stylistically preferred. (The somewhat baroque conventions for the optional argument are historical in nature, and supported primarily for compatibility with older dialects of Lisp. In modern code, it is recommended that the only kind of argument used be a string prefix. In general, though, to obtain more flexible control of the new-symbol's name, consider using make-symbol instead.)


### <span id="V-GENSYM-COUNTER">变量 *GENSYM-COUNTER*</span>

Value Type:

a non-negative integer.

Initial Value:

implementation-dependent.

* 描述(Description):

A number which will be used in constructing the name of the next symbol generated by the function gensym.

*gensym-counter* can be either assigned or bound at any time, but its value must always be a non-negative integer.

* 示例(Examples): None.

* 受此影响(Affected By):

gensym.

* 也见(See Also):

gensym

* 注意(Notes):

The ability to pass a numeric argument to gensym has been deprecated; explicitly binding *gensym-counter* is now stylistically preferred.


### <span id="F-GENTEMP">函数 GENTEMP</span>

* 语法(Syntax):

gentemp &optional prefix package => new-symbol

* 参数和值(Arguments and Values):

prefix---a string. The default is "T".

package---a package designator. The default is the current package.

new-symbol---a fresh, interned symbol.

* 描述(Description):

gentemp creates and returns a fresh symbol, interned in the indicated package. The symbol is guaranteed to be one that was not previously accessible in package. It is neither bound nor fbound, and has a null property list.

The name of the new-symbol is the concatenation of the prefix and a suffix, which is taken from an internal counter used only by gentemp. (If a symbol by that name is already accessible in package, the counter is incremented as many times as is necessary to produce a name that is not already the name of a symbol accessible in package.)

* 示例(Examples):

 (gentemp) =>  T1298
 (gentemp "FOO") =>  FOO1299
 (find-symbol "FOO1300") =>  NIL, NIL
 (gentemp "FOO") =>  FOO1300
 (find-symbol "FOO1300") =>  FOO1300, :INTERNAL
 (intern "FOO1301") =>  FOO1301, :INTERNAL
 (gentemp "FOO") =>  FOO1302
 (gentemp) =>  T1303

* 副作用(Side Effects):

Its internal counter is incremented one or more times.

Interns the new-symbol in package.

* 受此影响(Affected By):

The current state of its internal counter, and the current state of the package.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if prefix is not a string. Should signal an error of type type-error if package is not a package designator.

* 也见(See Also):

gensym

* 注意(Notes):

The function gentemp is deprecated.

If package is the KEYWORD package, the result is an external symbol of package. Otherwise, the result is an internal symbol of package.

The gentemp internal counter is independent of *gensym-counter*, the counter used by gensym. There is no provision for accessing the gentemp internal counter.

Just because gentemp creates a symbol which did not previously exist does not mean that such a symbol might not be seen in the future (e.g., in a data file---perhaps even created by the same program in another session). As such, this symbol is not truly unique in the same sense as a gensym would be. In particular, programs which do automatic code generation should be careful not to attach global attributes to such generated symbols (e.g., special declarations) and then write them into a file because such global attributes might, in a different session, end up applying to other symbols that were automatically generated on another day for some other purpose.


### <span id="A-SYMBOL-FUNCTION">访问器 SYMBOL-FUNCTION</span>

* 语法(Syntax):

symbol-function symbol => contents

(setf (symbol-function symbol) new-contents)

* 参数和值(Arguments and Values):

symbol---a symbol.

contents--- If the symbol is globally defined as a macro or a special operator, an object of implementation-dependent nature and identity is returned. If the symbol is not globally defined as either a macro or a special operator, and if the symbol is fbound, a function object is returned.

new-contents---a function.

* 描述(Description):

Accesses the symbol's function cell.

* 示例(Examples):

 (symbol-function 'car) =>  #<FUNCTION CAR>
 (symbol-function 'twice) is an error   ;because TWICE isn't defined.
 (defun twice (n) (* n 2)) =>  TWICE
 (symbol-function 'twice) =>  #<FUNCTION TWICE>
 (list (twice 3)
       (funcall (function twice) 3)
       (funcall (symbol-function 'twice) 3))
=>  (6 6 6)
 (flet ((twice (x) (list x x)))
   (list (twice 3)
         (funcall (function twice) 3)
         (funcall (symbol-function 'twice) 3)))
=>  ((3 3) (3 3) 6)   
 (setf (symbol-function 'twice) #'(lambda (x) (list x x)))
=>  #<FUNCTION anonymous>
 (list (twice 3)
       (funcall (function twice) 3)
       (funcall (symbol-function 'twice) 3))
=>  ((3 3) (3 3) (3 3))
 (fboundp 'defun) =>  true
 (symbol-function 'defun)
=>  implementation-dependent
 (functionp (symbol-function 'defun))
=>  implementation-dependent
 (defun symbol-function-or-nil (symbol)
   (if (and (fboundp symbol)
            (not (macro-function symbol))
            (not (special-operator-p symbol)))
       (symbol-function symbol)
       nil)) =>  SYMBOL-FUNCTION-OR-NIL
 (symbol-function-or-nil 'car) =>  #<FUNCTION CAR>
 (symbol-function-or-nil 'defun) =>  NIL

* 副作用(Side Effects): None.

* 受此影响(Affected By):

defun

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

Should signal undefined-function if symbol is not fbound and an attempt is made to read its definition. (No such error is signaled on an attempt to write its definition.)

* 也见(See Also):

fboundp, fmakunbound, macro-function, special-operator-p

* 注意(Notes):

symbol-function cannot access the value of a lexical function name produced by flet or labels; it can access only the global function value.

setf may be used with symbol-function to replace a global function definition when the symbol's function definition does not represent a special operator.

(symbol-function symbol) ==  (fdefinition symbol)

However, fdefinition accepts arguments other than just symbols.


### <span id="F-SYMBOL-NAME">函数 SYMBOL-NAME</span>

* 语法(Syntax):

symbol-name symbol => name

* 参数和值(Arguments and Values):

symbol---a symbol.

name---a string.

* 描述(Description):

symbol-name returns the name of symbol. The consequences are undefined if name is ever modified.

* 示例(Examples):

 (symbol-name 'temp) =>  "TEMP"
 (symbol-name :start) =>  "START"
 (symbol-name (gensym)) =>  "G1234" ;for example

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-SYMBOL-PACKAGE">函数 SYMBOL-PACKAGE</span>

* 语法(Syntax):

symbol-package symbol => contents

* 参数和值(Arguments and Values):

symbol---a symbol.

contents---a package object or nil.

* 描述(Description):

Returns the home package of symbol.

* 示例(Examples):

 (in-package "CL-USER") =>  #<PACKAGE "COMMON-LISP-USER">
 (symbol-package 'car) =>  #<PACKAGE "COMMON-LISP">
 (symbol-package 'bus) =>  #<PACKAGE "COMMON-LISP-USER">
 (symbol-package :optional) =>  #<PACKAGE "KEYWORD">
 ;; Gensyms are uninterned, so have no home package.
 (symbol-package (gensym)) =>  NIL
 (make-package 'pk1) =>  #<PACKAGE "PK1">
 (intern "SAMPLE1" "PK1") =>  PK1::SAMPLE1, NIL
 (export (find-symbol "SAMPLE1" "PK1") "PK1") =>  T
 (make-package 'pk2 :use '(pk1)) =>  #<PACKAGE "PK2">
 (find-symbol "SAMPLE1" "PK2") =>  PK1:SAMPLE1, :INHERITED
 (symbol-package 'pk1::sample1) =>  #<PACKAGE "PK1">
 (symbol-package 'pk2::sample1) =>  #<PACKAGE "PK1">
 (symbol-package 'pk1::sample2) =>  #<PACKAGE "PK1">
 (symbol-package 'pk2::sample2) =>  #<PACKAGE "PK2">
 ;; The next several forms create a scenario in which a symbol
 ;; is not really uninterned, but is "apparently uninterned",
 ;; and so SYMBOL-PACKAGE still returns NIL.
 (setq s3 'pk1::sample3) =>  PK1::SAMPLE3
 (import s3 'pk2) =>  T
 (unintern s3 'pk1) =>  T
 (symbol-package s3) =>  NIL
 (eq s3 'pk2::sample3) =>  T

* 副作用(Side Effects): None.

* 受此影响(Affected By):

import, intern, unintern

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

intern

* 注意(Notes): None.


### <span id="A-SYMBOL-PLIST">访问器 SYMBOL-PLIST</span>

* 语法(Syntax):

symbol-plist symbol => plist

(setf (symbol-plist symbol) new-plist)

* 参数和值(Arguments and Values):

symbol---a symbol.

plist, new-plist---a property list.

* 描述(Description):

Accesses the property list of symbol.

* 示例(Examples):

 (setq sym (gensym)) =>  #:G9723
 (symbol-plist sym) =>  ()
 (setf (get sym 'prop1) 'val1) =>  VAL1
 (symbol-plist sym) =>  (PROP1 VAL1)
 (setf (get sym 'prop2) 'val2) =>  VAL2
 (symbol-plist sym) =>  (PROP2 VAL2 PROP1 VAL1)
 (setf (symbol-plist sym) (list 'prop3 'val3)) =>  (PROP3 VAL3)
 (symbol-plist sym) =>  (PROP3 VAL3)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

get, remprop

* 注意(Notes):

The use of setf should be avoided, since a symbol's property list is a global resource that can contain information established and depended upon by unrelated programs in the same Lisp image.


### <span id="A-SYMBOL-VALUE">访问器 SYMBOL-VALUE</span>

* 语法(Syntax):

symbol-value symbol => value

(setf (symbol-value symbol) new-value)

* 参数和值(Arguments and Values):

symbol---a symbol that must have a value.

value, new-value---an object.

* 描述(Description):

Accesses the symbol's value cell.

* 示例(Examples):

 (setf (symbol-value 'a) 1) =>  1
 (symbol-value 'a) =>  1
 ;; SYMBOL-VALUE cannot see lexical variables.
 (let ((a 2)) (symbol-value 'a)) =>  1
 (let ((a 2)) (setq a 3) (symbol-value 'a)) =>  1
 ;; SYMBOL-VALUE can see dynamic variables.
 (let ((a 2))
   (declare (special a))
   (symbol-value 'a)) =>  2
 (let ((a 2))
   (declare (special a))
   (setq a 3)
   (symbol-value 'a)) =>  3
 (let ((a 2))
   (setf (symbol-value 'a) 3)
   a) =>  2
 a =>  3
 (symbol-value 'a) =>  3
 (let ((a 4))
   (declare (special a))
   (let ((b (symbol-value 'a)))
     (setf (symbol-value 'a) 5)
     (values a b))) =>  5, 4
 a =>  3
 (symbol-value :any-keyword) =>  :ANY-KEYWORD
 (symbol-value 'nil) =>  NIL
 (symbol-value '()) =>  NIL
 ;; The precision of this next one is implementation-dependent.
 (symbol-value 'pi) =>  3.141592653589793d0  

* 副作用(Side Effects): None.

* 受此影响(Affected By):

makunbound, set, setq

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

Should signal unbound-variable if symbol is unbound and an attempt is made to read its value. (No such error is signaled on an attempt to write its value.)

* 也见(See Also):

boundp, makunbound, set, setq

* 注意(Notes):

symbol-value can be used to get the value of a constant variable. symbol-value cannot access the value of a lexical variable.


### <span id="A-GET">访问器 GET</span>

* 语法(Syntax):

get symbol indicator &optional default => value

(setf (get symbol indicator &optional default) new-value)

* 参数和值(Arguments and Values):

symbol---a symbol.

indicator---an object.

default---an object. The default is nil.

value---if the indicated property exists, the object that is its value; otherwise, the specified default.

new-value---an object.

* 描述(Description):

get finds a property on the property list[2] of symbol whose property indicator is identical to indicator, and returns its corresponding property value. If there are multiple properties[1] with that property indicator, get uses the first such property. If there is no property with that property indicator, default is returned.

setf of get may be used to associate a new object with an existing indicator already on the symbol's property list, or to create a new assocation if none exists. If there are multiple properties[1] with that property indicator, setf of get associates the new-value with the first such property. When a get form is used as a setf place, any default which is supplied is evaluated according to normal left-to-right evaluation rules, but its value is ignored.

* 示例(Examples):

 (defun make-person (first-name last-name)
   (let ((person (gensym "PERSON")))
     (setf (get person 'first-name) first-name)
     (setf (get person 'last-name) last-name)
     person)) =>  MAKE-PERSON
 (defvar *john* (make-person "John" "Dow")) =>  *JOHN*
 *john* =>  #:PERSON4603
 (defvar *sally* (make-person "Sally" "Jones")) =>  *SALLY*
 (get *john* 'first-name) =>  "John"
 (get *sally* 'last-name) =>  "Jones"
 (defun marry (man woman married-name)
   (setf (get man 'wife) woman)
   (setf (get woman 'husband) man)
   (setf (get man 'last-name) married-name)
   (setf (get woman 'last-name) married-name)
   married-name) =>  MARRY
 (marry *john* *sally* "Dow-Jones") =>  "Dow-Jones"
 (get *john* 'last-name) =>  "Dow-Jones"
 (get (get *john* 'wife) 'first-name) =>  "Sally"
 (symbol-plist *john*)
=>  (WIFE #:PERSON4604 LAST-NAME "Dow-Jones" FIRST-NAME "John")
 (defmacro age (person &optional (default ''thirty-something))
   `(get ,person 'age ,default)) =>  AGE
 (age *john*) =>  THIRTY-SOMETHING
 (age *john* 20) =>  20
 (setf (age *john*) 25) =>  25
 (age *john*) =>  25
 (age *john* 20) =>  25

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

getf, symbol-plist, remprop

* 注意(Notes):

 (get x y) ==  (getf (symbol-plist x) y)

Numbers and characters are not recommended for use as indicators in portable code since get tests with eq rather than eql, and consequently the effect of using such indicators is implementation-dependent.

There is no way using get to distinguish an absent property from one whose value is default. However, see get-properties.


### <span id="F-REMPROP">函数 REMPROP</span>

* 语法(Syntax):

remprop symbol indicator => generalized-boolean

* 参数和值(Arguments and Values):

symbol---a symbol.

indicator---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

remprop removes from the property list[2] of symbol a property[1] with a property indicator identical to indicator. If there are multiple properties[1] with the identical key, remprop only removes the first such property. remprop returns false if no such property was found, or true if a property was found.

The property indicator and the corresponding property value are removed in an undefined order by destructively splicing the property list. The permissible side-effects correspond to those permitted for remf, such that:

 (remprop x y) ==  (remf (symbol-plist x) y)

* 示例(Examples):

 (setq test (make-symbol "PSEUDO-PI")) =>  #:PSEUDO-PI
 (symbol-plist test) =>  ()
 (setf (get test 'constant) t) =>  T
 (setf (get test 'approximation) 3.14) =>  3.14
 (setf (get test 'error-range) 'noticeable) =>  NOTICEABLE
 (symbol-plist test)
=>  (ERROR-RANGE NOTICEABLE APPROXIMATION 3.14 CONSTANT T)
 (setf (get test 'approximation) nil) =>  NIL
 (symbol-plist test)
=>  (ERROR-RANGE NOTICEABLE APPROXIMATION NIL CONSTANT T)
 (get test 'approximation) =>  NIL
 (remprop test 'approximation) =>  true
 (get test 'approximation) =>  NIL
 (symbol-plist test)
=>  (ERROR-RANGE NOTICEABLE CONSTANT T)
 (remprop test 'approximation) =>  NIL
 (symbol-plist test)
=>  (ERROR-RANGE NOTICEABLE CONSTANT T)
 (remprop test 'error-range) =>  true
 (setf (get test 'approximation) 3) =>  3
 (symbol-plist test)
=>  (APPROXIMATION 3 CONSTANT T)

* 副作用(Side Effects):

The property list of symbol is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

remf, symbol-plist

* 注意(Notes):

Numbers and characters are not recommended for use as indicators in portable code since remprop tests with eq rather than eql, and consequently the effect of using such indicators is implementation-dependent. Of course, if you've gotten as far as needing to remove such a property, you don't have much choice---the time to have been thinking about this was when you used setf of get to establish the property.


### <span id="F-BOUNDP">函数 BOUNDP</span>

* 语法(Syntax):

boundp symbol => generalized-boolean

* 参数和值(Arguments and Values):

symbol---a symbol.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if symbol is bound; otherwise, returns false.

* 示例(Examples):

 (setq x 1) =>  1
 (boundp 'x) =>  true
 (makunbound 'x) =>  X
 (boundp 'x) =>  false
 (let ((x 2)) (boundp 'x)) =>  false
 (let ((x 2)) (declare (special x)) (boundp 'x)) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

set, setq, symbol-value, makunbound

* 注意(Notes):

The function bound determines only whether a symbol has a value in the global environment; any lexical bindings are ignored.


### <span id="F-MAKUNBOUND">函数 MAKUNBOUND</span>

* 语法(Syntax):

makunbound symbol => symbol

* 参数和值(Arguments and Values):

symbol---a symbol

* 描述(Description):

Makes the symbol be unbound, regardless of whether it was previously bound.

* 示例(Examples):

 (setf (symbol-value 'a) 1)
 (boundp 'a) =>  true
 a =>  1
 (makunbound 'a) =>  A
 (boundp 'a) =>  false

* 副作用(Side Effects):

The value cell of symbol is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

Should signal an error of type type-error if symbol is not a symbol.

* 也见(See Also):

boundp, fmakunbound

* 注意(Notes): None.


### <span id="F-SET">函数 SET</span>

* 语法(Syntax):

set symbol value => value

* 参数和值(Arguments and Values):

symbol---a symbol.

value---an object.

* 描述(Description):

set changes the contents of the value cell of symbol to the given value.

(set symbol value) ==  (setf (symbol-value symbol) value)

* 示例(Examples):

 (setf (symbol-value 'n) 1) =>  1
 (set 'n 2) =>  2
 (symbol-value 'n) =>  2
 (let ((n 3))
   (declare (special n))
   (setq n (+ n 1))
   (setf (symbol-value 'n) (* n 10))
   (set 'n (+ (symbol-value 'n) n))
   n) =>  80
 n =>  2
 (let ((n 3))
   (setq n (+ n 1))
   (setf (symbol-value 'n) (* n 10))
   (set 'n (+ (symbol-value 'n) n))
   n) =>  4
 n =>  44
 (defvar *n* 2)
 (let ((*n* 3))
   (setq *n* (+ *n* 1))
   (setf (symbol-value '*n*) (* *n* 10))
   (set '*n* (+ (symbol-value '*n*) *n*))
   *n*) =>  80
  *n* =>  2
 (defvar *even-count* 0) =>  *EVEN-COUNT*
 (defvar *odd-count* 0) =>  *ODD-COUNT*
 (defun tally-list (list)
   (dolist (element list)
     (set (if (evenp element) '*even-count* '*odd-count*)
          (+ element (if (evenp element) *even-count* *odd-count*)))))
 (tally-list '(1 9 4 3 2 7)) =>  NIL
 *even-count* =>  6
 *odd-count* =>  20

* 副作用(Side Effects):

The value of symbol is changed.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 也见(See Also):

setq, progv, symbol-value

* 注意(Notes):

The function set is deprecated.

set cannot change the value of a lexical variable.


### <span id="CT-UNBOUND-VARIABLE">状况类型 UNBOUND-VARIABLE</span>

* 类优先级列表(Class Precedence List):

unbound-variable, cell-error, error, serious-condition, condition, t

* 描述(Description):

The type unbound-variable consists of error conditions that represent attempts to read the value of an unbound variable.

The name of the cell (see cell-error) is the name of the variable that was unbound.

* 也见(See Also):

cell-error-name
