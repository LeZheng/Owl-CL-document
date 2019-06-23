# 10. 符号

> * 10.1 [符号概念](#SymbolConcepts)
> * 10.2 [符号字典](#TheSymbolsDictionary)

## 10.1 <span id="SymbolConcepts">符号概念</span>

下面这段列出了一些可应用于符号[symbol]的属性列表[property list]的已定义名字[defined name].

    get  remprop  symbol-plist  

    Figure 10-1. 属性列表已定义的名字

下一段列出了一些可应用于创建和查询符号[symbol]的已定义名字[defined name].

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

        符号[symbol]用于它们的对象[object]标识来命名 Common Lisp 中不同的实体, 包括 (但不限于) 像变量[variable]和函数[function]这样的语言实体.

        符号[symbol]可以被一起收集到包[package]中. 如果一个符号[symbol]在一个包[package]中是可访问的[accessible]就说那个符号[symbol]被捕捉[interned]到那个包[package]中; 相同符号[symbol]可以被捕捉[interned]到不止一个包[package]中. 如果一个符号[symbol]没有被捕捉[interned]到任何包[package]中, 就说它是未捕捉的[uninterned].

        一个被捕捉的[interned]符号[symbol]在它可访问的[accessible]任何包[package]中根据它的名称[name]是唯一可识别的.

        符号[symbol]有着以下属性. 出于历史原因, 它们有时作为存储格[cell]被引用, 虽然符号[symbol]和它们的属性的实际内部表示是依赖于具体实现的[implementation-dependent].

        名字(Name)

            一个符号[symbol]的名字[name]是用于标识这个符号[symbol]的一个字符串[string]. 每个符号[symbol]有一个名字[name], 如果那个名字[name]被修改那么结果是未定义的. 这个名字[name]被用作这个符号[symbol]的外部打印表示的一部分; 见章节 2.1 (字符语法). 函数[function] symbol-name 返回一个给定符号[symbol]的名字[name]. 一个符号[symbol]的名字[name]中可能有任何字符[character].

        包(Package)

            在这个存储格[cell]的这个对象[object]被称为这个符号[symbol]的 home 包[home package]. 如果这个 home 包[home package]是 nil, 那么有时就说这个符号[symbol]没有 home 包[home package].

            当一个符号[symbol]被首次创建, 它没有 home 包[home package]. 当它第一次被捕捉[interned]时, 最初被捕捉[interned]时所在的包[package]成为它的 home 包[home package]. 一个符号[symbol]的 home 包[home package]可以通过函数[function] symbol-package 来访问.

            如果一个符号[symbol]从它的 home 包[home package]中被解除捕捉[uninterned], 它的 home 包[home package]会被设置为 nil. 这个符号[symbol]是否为一个未被捕捉[uninterned]的符号[symbol]取决于这个符号[symbol]是否被捕捉[interned]到另一个包[package]中. 一个没有 home 包[home package]的符号[symbol]因此被称为是显然未捕捉的[apparently uninterned].

            如果尝试去修改在 COMMON-LISP 包或者 KEYWORD 包中的一个外部符号[symbol]的 home 包[home package], 那么结果是未定义的.

        属性列表(Property list)

            一个符号[symbol]的属性列表[property list]为关联已命名属性和那个符号[symbol]提供了一个机制. 关于添加和移除的操作对于这个属性列表[property list]是破坏性的[destructive]. Common Lisp 提供了操作符[operator]来直接操作属性列表[property list]对象[object] (比如, 见 getf, remf, 和 symbol-plist) 以及通过引用一个符号[symbol]来隐式操作这个符号[symbol]的属性列表[property list] (比如, 见 get 和 remprop). 和一个新[fresh]符号[symbol]关联的属性列表[property list]被初始化为空[null].

        值(Value)

            如果一个符号有一个值属性, 它就被称为是绑定的[bound], 并且这个事实可以通过函数[function] boundp 来检测. 被包含在一个绑定的[bound]符号[symbol]的值存储格[value cell]中的对象[object]是那个符号[symbol]命名的全局变量[global variable]的值[value], 并且可以通过函数[function] symbol-value 来访问. 一个符号[symbol]可以通过函数[function] makunbound 变为未绑定的[unbound].

            如果尝试去修改一个命名常变量[constant variable]的符号[symbol]的值[value]或者使这样一个符号[symbol]变为未绑定的[unbound], 那么后果是未定义的.

        函数(Function)

            如果一个符号有一个函数属性, 它就被称为是 fbound 的, 并且这个事实可以通过函数[function] fboundp 来检测. 如果这个符号[symbol]是一个全局环境[global environment]中的函数[function]的名字[name], 这个函数存储格[function cell]就包含了那个函数[function], 并且可以通过函数[function] symbol-function 来访问. 如果这个符号[symbol]是全局环境[global environment]中的一个宏[macro] (见 macro-function) 或者一个特殊操作符[special operator] (见 special-operator-p) 的名字[name], 那么这个符号[symbol]就是 fbound 的, 并且可以通过函数[function] symbol-function 来访问, 但是那个函数存储格[function cell]所包含的对象的类型[type]和目的依赖于具体实现[implementation-dependent]. 一个符号[symbol]可以通过函数[function] fmakunbound 来变为 funbound 的.

            如果尝试去修改命名一个特殊表达式形式[special form]的符号[symbol]的函数值[functional value], 那么后果是未定义的.

        在一个符号[symbol]的值存储格[value cell]和函数存储格[function cell]上的操作有时被描述为对符号[symbol]自身的影响, 但是用户应该注意在那些存储格[cell]的内容分别和全局变量[global variable]或全局函数[function]定义之间存在一个紧密的关系.

        符号[symbol]被用作词法变量[lexical variable]和词法函数[function]定义的标识, 但是在那个角色中, 只有它们的对象标识才是重要的. Common Lisp 没有在一个符号[symbol]上提供在一个词法变量[lexical variable]或一个词法函数[function]定义上有任何效果的操作符.

* 参见(See Also):

        章节 2.3.4 (符号标记), 章节 2.3.1.1 (潜在数字作为标记), 章节 22.1.3.3 (打印符号)


### <span id="T-KEYWORD">类型 KEYWORD</span>

* 超类型(Supertypes):

        keyword, symbol, t

* 描述(Description):

        类型[type] keyword 包括所有被捕捉[interned]在 KEYWORD 包中的符号[symbol].

        捕捉一个符号[symbol]到 KEYWORD 包中有三个必然的影响:

        1. 它导致这个符号[symbol]被绑定到它自身.
        2. 它导致这个符号[symbol]变为这个 KEYWORD 包的外部符号[external symbol].
        3. 它导致这个符号[symbol]变为一个常变量[constant variable].

* 参见(See Also):

        keywordp


### <span id="F-SYMBOLP">函数 SYMBOLP</span>

* 语法(Syntax):

        symbolp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 symbol 类型[type]的就返回 true; 否则, 返回 false.

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

* 参见(See Also):

        keywordp, symbol, typep

* 注意(Notes):

        (symbolp object) ==  (typep object 'symbol)


 ### <span id="F-KEYWORDP">函数 KEYWORDP</span>

 * 语法(Syntax):

        keywordp object => generalized-boolean

 * 参数和值(Arguments and Values):

        object---一个对象[symbol].
        generalized-boolean---一个广义 boolean [generalized boolean].

 * 描述(Description):

        如果对象 object 是一个关键字[keyword[1]]就返回 true; 否则, 返回 false.

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

 * 参见(See Also):

        constantp, keyword, symbolp, symbol-package

 * 注意(Notes): None.


### <span id="F-MAKE-SYMBOL">函数 MAKE-SYMBOL</span>

* 语法(Syntax):

        make-symbol name => new-symbol

* 参数和值(Arguments and Values):

        name---一个字符串[string].
        new-symbol---一个新的[fresh], 未捕捉的[uninterned]符号[symbol].

* 描述(Description):

        make-symbol 创建并返回一个新的[fresh], 未捕捉的[uninterned]符号[symbol], 它的名字[name]为给定的名字 name. 这个新符号 new-symbol 既没有被绑定[bound]也没有被 fbound 并且有一个空[null]属性列表[property list].

        成为这个新符号 new-symbol 的名字[name]的字符串[string]是那个给定的名字 name 还是它的拷贝依赖于具体实现[implementation-dependent]. 一旦一个字符串[string]已经被给定作为给 make-symbol 的 name 实参[argument], 如果后面去修改这个字符串[string]那么后果是未定义的.

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

        如果名字 name 不是一个字符串[string]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        copy-symbol

* 注意(Notes):

        make-symbol 不会尝试去转换这个名字[name]的大小写为大写的. 对于符号[symbol]发生的仅有的大小写转换是通过 Lisp 读取器[Lisp reader]完成的. 对于符号[symbol]创建的编程接口保留大小写, 并且对于捕捉符号的编程接口是大小写敏感的.


### <span id="F-COPY-SYMBOL">函数 COPY-SYMBOL</span>

* 语法(Syntax):

        copy-symbol symbol &optional copy-properties => new-symbol

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        copy-properties---一个广义的 boolean [generalized boolean]. 默认是 false.
        new-symbol---一个新的[fresh], 未捕捉的[uninterned]符号[symbol].

* 描述(Description):

        copy-symbol 返回一个新的[fresh], 未被捕捉的[uninterned]符号[symbol], 它的名字[name]和给定符号 symbol 的名字[name]是 string= 的或者可能是一样的[same].

        如果 copy-properties 是 false, 这个新符号 new-symbol 即没有被绑定[bound]也没有被 fbound 并且有一个空[null]属性列表[property list]. 如果 copy-properties 是 true, 那么这个新符号 new-symbol 的初始值[value]是那个符号 symbol 的值[value], 这个新符号的初始函数[function]定义是那个符号 symbol 的函数值[functional value], 并且那个新符号 new-symbol 的属性列表[property list]是符号 symbol 的属性列表[property list]的一个拷贝[copy[2]].

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

        如果 symbol 不是一个符号[symbol], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-symbol

* 注意(Notes):

        鼓励实现者不要去不必要地复制那个是符号[symbol]名字[name]的字符串[string]. 除非这里有一个好的理由去做这个, 对于这个新符号 new-symbol 的名字[name]的正常的实现策略是和给定符号 symbol 的名字[name]是相同的[identical].


### <span id="F-GENSYM">函数 GENSYM</span>

* 语法(Syntax):

        gensym &optional x => new-symbol

* 参数和值(Arguments and Values):

        x---一个字符串[string]或者一个非负整数[integer]. 复杂的默认行为; 见下文.
        new-symbol---一个新的[fresh], 未捕捉的[uninterned]符号[symbol].

* 描述(Description):

        创建并返回一个新的[fresh], 未捕捉的[uninterned]符号[symbol], 就像是通过调用 make-symbol 的一样. (gensym 和 make-symbol 仅有的区别在于如何决定这个新符号 new-symbol 的名字[name].)

        这个新符号 new-symbol 的名字[name]由一个默认为 "G" 的前缀和一个默认为 *gensym-counter* 值[value]的十进制表示的后缀拼接而成.

        如果提供了 x, 并且是一个字符串[string], 那么这个字符串[string]仅在这个对 gensym 的调用中替换 "G" 作为前缀.

        如果提供了 x, 并且是一个整数[ingeter], 那么这个整数[ingeter]仅在这个对 gensym 的调用中替换 *gensym-counter* 的值[value]被用作后缀.

        当且仅当没有提供显式后缀时, *gensym-counter* 在它被使用后递增.

* 示例(Examples):

    ```LISP
    (setq sym1 (gensym)) =>  #:G3142
    (symbol-package sym1) =>  NIL
    (setq sym2 (gensym 100)) =>  #:G100
    (setq sym3 (gensym 100)) =>  #:G100
    (eq sym2 sym3) =>  false
    (find-symbol "G100") =>  NIL, NIL
    (gensym "T") =>  #:T3143
    (gensym) =>  #:G3144
    ```

* 副作用(Side Effects):

        可能递增 *gensym-counter*.

* 受此影响(Affected By):

        *gensym-counter*

* 异常情况(Exceptional Situations): 

        如果 x 不是一个字符串[string]也不是一个非负整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        gentemp, *gensym-counter*

* 注意(Notes):

        传递一个数字参数给 gensym 的能力已经被废弃了; 显示绑定 *gensym-counter* 目前语法上是首选的. (关于可选参数的一些巴洛克式的惯例本质上是历史性的, 并且主要是为了支持较老版本的 Lisp 方言的兼容性. 在现代代码中, 建议仅有的参数种类是一个字符串前缀. 通常, 为了获取这个新符号 new-symbol 的名字的更灵活的控制, 考虑使用 make-symbol.)


### <span id="V-GENSYM-COUNTER">变量 *GENSYM-COUNTER*</span>

* 值类型(Value Type):

        一个非负整数[integer].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        用来构造由函数[function] gensym 生成的下一个符号[symbol]名字[name]的一个数字.

        *gensym-counter* 可以在任何时间被赋值或绑定[bound], 但是它的值必须总是为一个非负整数[integer].

* 示例(Examples): None.

* 受此影响(Affected By):

        gensym.

* 参见(See Also):

        gensym

* 注意(Notes):

        传递一个数字参数给 gensym 的能力已经被废弃了; 显式绑定[binding] *gensym-counter* 目前是语法上首选的.


### <span id="F-GENTEMP">函数 GENTEMP</span>

* 语法(Syntax):

        gentemp &optional prefix package => new-symbol

* 参数和值(Arguments and Values):

        prefix---一个字符串[string]. 默认为 "T".
        package---一个包标识符[package designator]. 默认是当前包[current package].
        new-symbol---一个新的[fresh], 被捕捉的[interned]符号[symbol].

* 描述(Description):

        gentemp 创建并返回一个新的[fresh]符号[symbol], 被捕捉[interned]在指定的包 package 中. 这个符号[symbol]保证不是一个之前在包 package 中可访问的[accessible]符号. 它既没有被绑定[bound]也没有被 fbound, 并且有一个空[null]属性列表[property list].

        这个新符号 new-symbol 的名字[name]是一个前缀 prefix 和后缀拼接而成, 这个后缀取自一个只有 gentemp 使用的内部计数器. (如果这个名字的一个符号[symbol]在包 package 中已经是可访问的[accessible], 这个计数器递增必要的次数来产生一个之前在这个包 package 中不是一个可访问[accessible]符号[symbol]的名字.)

* 示例(Examples):

    ```LISP
    (gentemp) =>  T1298
    (gentemp "FOO") =>  FOO1299
    (find-symbol "FOO1300") =>  NIL, NIL
    (gentemp "FOO") =>  FOO1300
    (find-symbol "FOO1300") =>  FOO1300, :INTERNAL
    (intern "FOO1301") =>  FOO1301, :INTERNAL
    (gentemp "FOO") =>  FOO1302
    (gentemp) =>  T1303
    ```

* 副作用(Side Effects):

        它的内部计数器被递增一次或多次.
        在包 package 中捕捉[intern]这个新符号 new-symbol.

* 受此影响(Affected By):

        它的内部计数器的当前状态, 以及这个包 package 的当前状态.

* 异常情况(Exceptional Situations): 

        如果前缀 prefix 不是一个字符串[string]那么应该发出一个 type-error 类型[type]的错误. 如果 package 不是一个包标识符[package designator]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        gensym

* 注意(Notes):

        函数 gentemp 被废弃了.

        如果包 package 是 KEYWORD 包, 那么结果是包 package 的一个外部符号[external symbol]. 否则, 结果就是一个包 package 的一个内部符号[internal symbol].

        这个 gentemp 内部计数器独立于 gensym 使用的计数器 *gensym-counter*. 这里没有用于访问这个 gentemp 内部计数器的措施.

        只是因为 gentemp 创建一个之前不存在的符号[symbol]并不意味着这样一个符号[symbol]在未来可能不会出现 (比如, 在一个数据文件中---可能由另一个会话中的相同程序创建). 因此, 这个符号并不像 gensym 一样是唯一的. 特别地, 自动代码生成的程序应该注意不要将全局属性附加到这些生成的符号[symbol]上 (比如, special 声明[declaration]) 并且把它们写到一个文件中, 因为这样的全局属性可能在不同的会话中, 最终会应用到其他符号[symbol]上, 这些符号[symbol]是在另一天自动生成用于其他目的.


### <span id="A-SYMBOL-FUNCTION">访问器 SYMBOL-FUNCTION</span>

* 语法(Syntax):

        symbol-function symbol => contents

        (setf (symbol-function symbol) new-contents)

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].

        contents--- 如果这个符号 symbol 是一个全局定义的宏[macro]或者特殊操作符[special operator], 那么就返回一个性质和标识依赖于具体实现[implementation-dependent]的对象[object]. 如果这个符号 symbol 没有被全局定义为一个宏[macro]或一个特殊操作符[special operator], 并且这个符号 symbol 被 fbound 了, 返回一个函数[function]对象[object].

        new-contents---一个函数.

* 描述(Description):

        访问[access]这个符号[symbol]的函数存储格[function cell].

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        defun

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

        如果 symbol 没有被 fbound 并且尝试去读取[read]它的定义那么应该发出一个 undefined-function 类型的错误. (在尝试去写入[write]它的定义时不会发出这样的错误.)

* 参见(See Also):

        fboundp, fmakunbound, macro-function, special-operator-p

* 注意(Notes):

        symbol-function 不能访问[access]由 flet 或 labels 产生的词法函数名的值; 它只能访问[access]全局函数值.

        当这个符号[symbol]的函数定义不表示一个特殊操作符[special operator]时, setf 可以和 symbol-function 一起使用来替换一个全局函数定义.

        (symbol-function symbol) ==  (fdefinition symbol)

        然而, fdefinition 接受的参数不仅仅是符号[symbol].


### <span id="F-SYMBOL-NAME">函数 SYMBOL-NAME</span>

* 语法(Syntax):

        symbol-name symbol => name

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        name---一个字符串[string].

* 描述(Description):

        symbol-name 返回符号 symbol 的名字[name]. 如果名字 name 被修改那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (symbol-name 'temp) =>  "TEMP"
    (symbol-name :start) =>  "START"
    (symbol-name (gensym)) =>  "G1234" ;for example
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes): None.


### <span id="F-SYMBOL-PACKAGE">函数 SYMBOL-PACKAGE</span>

* 语法(Syntax):

        symbol-package symbol => contents

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        contents---一个包[package]对象[object]或者 nil.

* 描述(Description):

        返回这个符号 symbol 的 home 包[home package].

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        import, intern, unintern

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        intern

* 注意(Notes): None.


### <span id="A-SYMBOL-PLIST">访问器 SYMBOL-PLIST</span>

* 语法(Syntax):

        symbol-plist symbol => plist

        (setf (symbol-plist symbol) new-plist)

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        plist, new-plist---一个属性列表[property list].

* 描述(Description):

        访问[access]符号 symbol 的属性列表[property list].

* 示例(Examples):

    ```LISP
    (setq sym (gensym)) =>  #:G9723
    (symbol-plist sym) =>  ()
    (setf (get sym 'prop1) 'val1) =>  VAL1
    (symbol-plist sym) =>  (PROP1 VAL1)
    (setf (get sym 'prop2) 'val2) =>  VAL2
    (symbol-plist sym) =>  (PROP2 VAL2 PROP1 VAL1)
    (setf (symbol-plist sym) (list 'prop3 'val3)) =>  (PROP3 VAL3)
    (symbol-plist sym) =>  (PROP3 VAL3)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        get, remprop

* 注意(Notes):

        应该避免使用 setf, 因为一个符号[symbol]的属性列表[property list]是一个全局资源, 它可以包含相同 Lisp 镜像[Lisp image]中由不相关的程序建立和依赖的信息.


### <span id="A-SYMBOL-VALUE">访问器 SYMBOL-VALUE</span>

* 语法(Syntax):

        symbol-value symbol => value

        (setf (symbol-value symbol) new-value)

* 参数和值(Arguments and Values):

        symbol---必须有一个值[value]的符号[symbol].
        value, new-value---一个对象[object].

* 描述(Description):

        访问[access]这个符号[symbol]的值存储格[value cell].

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        makunbound, set, setq

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

        如果符号 symbol 被解绑[unbound]并且尝试去读取[read]它的值[value], 那么应该发出 unbound-variable 类型的错误. (在尝试去写入[write]它的值[value]时不会发出这样的错误.)

* 参见(See Also):

        boundp, makunbound, set, setq

* 注意(Notes):

        symbol-value 可以被用于获取一个常变量[constant variable]的值. symbol-value 不能访问一个词法变量[lexical variable]的值[value].


### <span id="A-GET">访问器 GET</span>

* 语法(Syntax):

        get symbol indicator &optional default => value

        (setf (get symbol indicator &optional default) new-value)

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        indicator---一个对象[object].
        default---一个对象[object]. 默认为 nil.
        value---如果表示的属性存在, 就是它的值[value]对象[object]; 否则, 就是指定的默认值 default.
        new-value---一个对象[object].

* 描述(Description):

        get 在符号 symbol 的属性列表[property list[2]]中找到一个属性指示符[property indicator]和指示符 indicator 一样[identical]的属性, 并且返回它对应的属性值[property value]. 如果这里有多个属性[property[1]]带有那个属性指示符[property indicator], get 使用第一个这样的属性[property]. 如果没有带有那个属性指示符[property indicator]的属性[property], 返回默认值 default.

        get 的 setf 可以被用于关联一个新对象[object]和一个存在于那个符号 symbol 的属性列表[property list]中的指示符, 如果不存在就创建一个新的关联. 如果这里有多个属性[property[1]]带有那个属性指示符[property indicator], get 的 setf 关联那个新值 new-value 到第一个这样的属性[property[1]]. 当一个 get 表达式形式[form]被用作一个 setf 的位置 place 时, 任何提供的默认值 default 都根据正常从左到右的求值规则被求值, 但是它的值会被忽略.

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[type]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        getf, symbol-plist, remprop

* 注意(Notes):

        (get x y) ==  (getf (symbol-plist x) y)

        在可移植的代码中不推荐数字[number]和字符[character]被用作指示符 indicator, 因为 get 使用 eq 来测试而不是使用 eql, 因此使用这样的指示符的影响是依赖于具体实现的[implementation-dependent].

        使用 get 没法去区分一个缺省的属性和值为默认值 default 的属性. 但是, 见 get-properties.


### <span id="F-REMPROP">函数 REMPROP</span>

* 语法(Syntax):

        remprop symbol indicator => generalized-boolean

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        indicator---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        remprop 从符号 symbol 的属性列表[property list[2]]中移除一个带有和指示符 indicator 一样[identical]属性指示符[property indicator]的属性[property[1]]. 如果这里有多个带有相同[identical]键的属性[property[1]], remprop 只移除第一个这样的属性[property[1]]. 如果没有找到这样的属性[property]那么 remprop 返回 false, 如果找到一个属性就返回 true.

        这个属性指示符[property indicator]和对应属性值[property value]通过破坏性地拼接这个属性列表以一种未定义的顺序被移除. 可允许的副作用相当于 remf 允许的那些, 如此这般:

        (remprop x y) ==  (remf (symbol-plist x) y)

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects):

        符号 symbol 的属性列表[property list]被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        remf, symbol-plist

* 注意(Notes):

        在可移植的代码中不推荐数字[number]和字符[character]被用作指示符 indicator 因为 remprop 使用 eq 来测试而不是 eql, 因此使用这样的指示符的影响是依赖于具体实现的[implementation-dependent]. 当然, 如果你想要移除这样的属性[property], 你没有太多选择---考虑这个问题的时间就是当你使用 get 的 setf 来建立这个属性[property]的时候.


### <span id="F-BOUNDP">函数 BOUNDP</span>

* 语法(Syntax):

        boundp symbol => generalized-boolean

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果符号 symbol 被绑定[bound]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (setq x 1) =>  1
    (boundp 'x) =>  true
    (makunbound 'x) =>  X
    (boundp 'x) =>  false
    (let ((x 2)) (boundp 'x)) =>  false
    (let ((x 2)) (declare (special x)) (boundp 'x)) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        set, setq, symbol-value, makunbound

* 注意(Notes):

        函数[function] bound 只决定一个符号[symbol]在全局环境[global environment]中是否有一个值; 任何词法绑定[lexical binding]都被忽略.


### <span id="F-MAKUNBOUND">函数 MAKUNBOUND</span>

* 语法(Syntax):

        makunbound symbol => symbol

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol]

* 描述(Description):

        使这个符号 symbol 变为未绑定的[unbound], 不管之前它是否被绑定[bound].

* 示例(Examples):

    ```LISP
    (setf (symbol-value 'a) 1)
    (boundp 'a) =>  true
    a =>  1
    (makunbound 'a) =>  A
    (boundp 'a) =>  false
    ```

* 副作用(Side Effects):

        符号 symbol 的值存储格[value cell]会被求值.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): 

        如果 symbol 不是一个符号[symbol]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        boundp, fmakunbound

* 注意(Notes): None.


### <span id="F-SET">函数 SET</span>

* 语法(Syntax):

        set symbol value => value

* 参数和值(Arguments and Values):

        symbol---一个符号[symbol].
        value---一个对象[object].

* 描述(Description):

        set 改变符号[symbol]的值存储格[value cell]的内容为给定的值[value].

        (set symbol value) ==  (setf (symbol-value symbol) value)

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects):

        符号 symbol 的值[value]被改变.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):  None.

* 参见(See Also):

        setq, progv, symbol-value

* 注意(Notes):

        函数 set 已经被废弃.

        set 不能修改一个词法变量[lexical variable]的值.


### <span id="CT-UNBOUND-VARIABLE">状况类型 UNBOUND-VARIABLE</span>

* 类优先级列表(Class Precedence List):

        unbound-variable, cell-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] unbound-variable 由表示尝试去读取[read]一个未绑定变量[unbound variable]的值[value]的错误[error]状况[condition]组成.

        这个存储格 (见 cell-error) 的名字是这个未绑定[unbound]变量[variable]的名字[name].

* 参见(See Also):

        cell-error-name
