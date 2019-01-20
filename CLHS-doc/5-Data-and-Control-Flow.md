# 5. 数据和控制流

> * 5.1 [广义引用](#GeneralizedReference)
> * 5.2 [退出点的控制转移](#TCEP)
> * 5.3 [数据和控制流字典](#DCFDictionary)

## 5.1 <span id="GeneralizedReference">广义引用</span>

> * 5.1.1 [位置和广义引用的概述](#OverviewPlacesGeneralizedReference)
> * 5.1.2 [位置的种类](#KindsOfPlaces)
> * 5.1.3 [基于 SETF 的其他宏的处理](#TreatmentMacrosSETF)

### 5.1.1 <span id="OverviewPlacesGeneralizedReference">位置和广义引用的概述</span>

一个广义引用[generalized reference]是一个表达式形式[form]的使用, 这个表达式形式[form]有时也称作一个位置[place], 就好像它是一个可以被读写的变量[variable]. 一个位置[place]的值[value]就是这个位置[place]表达式形式[form]求值后的对象[object]. 一个位置[place]的值[value]可以通过使用 setf 来改变. 绑定一个位置[place]的概念没有在 Common Lisp 中定义, 但是一个具体实现[implementation]允许通过定义它的概念来扩展这个语言.

下面这段包含了 setf 使用的示例. 注意, 求值第二列的表达式形式[form]返回的值没有必要和求值第三列中的表达式形式[form]获取到的值一样. 总之, setf 表达式形式[form]准确的宏展开[macro expansion]是不保证的, 甚至是依赖于具体实现的[implementation-dependent]; 可以保证的是, 这个展开式是一个针对特定实现[implementation]的更新表达式形式, 对子表达式形式[subform]的从左到右求值是保留的, 而求值 setf 的最终结果是存储的值或多值.

|访问函数(Access function) |  更新函数(Update Function) |  使用 setf 更新   |
| -              | -                | -                               |
|x               |  (setq x datum)  |  (setf x datum)                 |
|(car x)         |  (rplaca x datum)|  (setf (car x) datum)           |
|(symbol-value x)|  (set x datum)   |  (setf (symbol-value x) datum)  |

Figure 5-1. setf 的示例

下面这段展示了和位置[place]以及广义引用[generalized reference]相关的操作符[operator].

    assert                defsetf             push
    ccase                 get-setf-expansion  remf
    ctypecase             getf                rotatef
    decf                  incf                setf
    define-modify-macro   pop                 shiftf
    define-setf-expander  psetf

Figure 5-2. 位置[place]以及广义引用[generalized reference]相关的操作符.

上面的一些操作符[operator]操作位置[place], 而一些操作 setf 展开器[setf expander]. 一个 setf 展开器[setf expander]可以来自于任何位置[place]. 可以通过使用 defsetf 和 define-setf-expander 来定义新的 setf 展开器[setf expander].

> * 5.1.1.1 [位置的子表达式形式求值](#EvaluationSubformsPlaces)
> * 5.1.1.2 [Setf 展开式](#SetfExpansions)

#### 5.1.1.1 <span id="EvaluationSubformsPlaces">位置的子表达式形式求值</span>

以下规则应用于位置[place]的子表达式形式[subform]的求值[evaluation]:

1. 一个位置[place]中的子表达式形式[subform]的求值顺序由 get-setf-expansion 返回的第二个值指定的顺序所决定. 对于这个标准定义的所有位置[place] (比如, getf, ldb, ...), 求值的顺序是从左到右的. 当一个位置[place]从一个宏展开式中得到, 这个规则将在宏展开后应用, 以找到合适的位置[place].

    通过使用 defmacro 或 define-setf-expander 定义的位置[place]使用这些定义所定义的求值顺序. 比如, 思考下面这个:

    ```LISP
    (defmacro wrong-order (x y) `(getf ,y ,x))
    ```

    下面这个表达式形式[form]先求值 place2 然后再是 place1 因为这个是它们在宏展开式中的求值顺序:

    ```LISP
    (push value (wrong-order place1 place2))
    ```

2. 对于操纵位置[place]的宏[macro] (push, pushnew, remf, incf, decf, shiftf, rotatef, psetf, setf, pop, 还有那些 define-modify-macro 定义的宏), 宏调用的子表达式形式[subform]按从左到右的顺序求值一次, 其中这些位置[place]的子表达式形式[subform]按照 (1) 中指定的顺序求值.

    push, pushnew, remf, incf, decf, shiftf, rotatef, psetf, pop 在修改任意位置[place]之前求值所有子表达式形式[subform]. setf (当 setf 有超过两个参数的情况下) 在序列中的每一对上执行它的操作. 比如, 在下面这个表达式中

    ```LISP
    (setf place1 value1 place2 value2 ...)
    ```

    子表达式形式[subform] place1 和 value1 被求值, 修改 place1 指定的位置来包含 value1 返回的值, 然后 setf 表达式形式的剩余部分按照类似的方式处理.

3. 对于 check-type, ctypecase, 和 ccase, 位置[place]的子表达式形式[subform]像 (1) 中那样被求值一次, 但是如果 check-type 中类型检测失败了或者 ctypecase 和 ccase 中没有情况(case)被处理可能会再次求值.

4. 对于 assert, 广义引用[generalized reference]的求值顺序没有被指定.

规则 2, 3 和 4 覆盖所有操作位置[place]的标准化[standardized]宏[macro].

##### 5.1.1.1.1 对位置的子表达式形式求值的示例

```LISP
(let ((ref2 (list '())))
  (push (progn (princ "1") 'ref-1)
        (car (progn (princ "2") ref2))))
>>  12
=>  (REF-1)

(let (x)
  (push (setq x (list 'a))
        (car (setq x (list 'b))))
    x)
=>  (((A) . B))
```

push 先求值 (setq x (list 'a)) => (a), 然后求值 (setq x (list 'b)) => (b), 再修改最后的值的 car 部分为 ((a) . b).

#### 5.1.1.2 <span id="SetfExpansions">Setf 展开式</span>

有时避免多次求值一个位置[place]的子表达式形式[subform]或以错误的顺序求值是可能的. 对于一个给定的表达式形式, 一个 setf 展开式[setf expansion]可以被表达为五个对象[object]的一个有序集合:

临时对象的列表

    一个命名临时变量的符号列表, 这些符号被依次绑定到由值表达式形式返回的那些值, 就像是通过 let* 一样.

值表达式形式列表

    一个表达式形式列表(典型地, 这个位置[place]的子表达式形式[subform]), 当求值时, 会产生相应的临时变量应该被绑定的值.

存储变量的列表

    一个用来命名临时存储变量的符号列表, 用来保存将被分配给这个位置[place]的新值.

存储表达式形式

    一个可以同时引用临时变量和存储变量的表达式形式, 它会改变这个位置[place]的值[value], 并保证返回存储变量的值作为它的值, 这是 setf 返回的正确值.

访问表达式形式

    一个可以引用临时变量并且返回这个位置[place]的值[value]的表达式形式[form].

访问表达式形式返回的值受到存储表达式形式执行的影响, 但是这些表达式形式中的任何一种都可能被多次求值.

可以通过 psetf, shiftf 和 rotatef 并行执行多个 setf. 由于这个, setf 展开器[setf expander]必须每次产生新的临时和存储变量名字. 关于如何去做这个的示例, 见 gensym.

对于每一个标准化[standardized]的访问器函数 F, 除非它被显式地记载, 否则使用一个 F 表达式形式[form]作为一个 setf 位置[place]的能力被实现为使用一个 setf 展开器[setf expander]还是一个 setf 函数[setf function], 是依赖于具体实现的[implementation-dependent]. 同样, 由此可以得出, 名字 (setf F) 是否被 fbound 是依赖于具体实现的[implementation-dependent].

##### 5.1.1.2.1 Setf 展开式的示例

下面是 setf 展开式[setf expansion]组成成分内容的示例.

对于一个变量 x:

```LISP
()              ;list of temporary variables
()              ;list of value forms
(g0001)         ;list of store variables
(setq x g0001)  ;storing form
x               ;accessing form
```

Figure 5-3. 一个变量的简单 setf 展开式

对于 (car exp):

```LISP
(g0002)                             ;list of temporary variables
(exp)                               ;list of value forms
(g0003)                             ;list of store variables
(progn (rplaca g0002 g0003) g0003)  ;storing form
(car g0002)                         ;accessing form
```

Figure 5-4. CAR 表达式形式的简单 setf 展开式

对于 (subseq seq s e):

```LISP
(g0004 g0005 g0006)         ;list of temporary variables
(seq s e)                   ;list of value forms
(g0007)                     ;list of store variables
(progn (replace g0004 g0007 :start1 g0005 :end1 g0006) g0007)
                            ;storing form
(subseq g0004 g0005 g0006)  ; accessing form
```

Figure 5-5. SUBSEQ 表达式形式的简单 setf 展开式

在一些情况下, 如果一个位置[place]的一个子表达式形式[subform]自身也是一个位置[place], 为了计算外面的位置[place]展开式的一些值, 展开子表达式形式[subform]是有必要的. 对于 (ldb bs (car exp)):

```LISP
(g0001 g0002)            ;list of temporary variables
(bs exp)                 ;list of value forms
(g0003)                  ;list of store variables
(progn (rplaca g0002 (dpb g0003 g0001 (car g0002))) g0003)
                         ;storing form
(ldb g0001 (car g0002))  ; accessing form
```

Figure 5-6. LDB 表达式形式的简单 setf 展开式

### 5.1.2 <span id="KindsOfPlaces">位置的种类</span>

Common Lisp 定义了多个位置[place]的种类; 这个章节会列举它们. 这个集合可以被具体实现[implementation]和程序员的代码[programmer code]所扩展.

> * 5.1.2.1 [变量名作为位置](#VariableNamesPlaces)
> * 5.1.2.2 [函数调用表达式形式作为位置](#FunctionCallFormsPlaces)
> * 5.1.2.3 [VALUES 表达式形式作为位置](#VALUESFormsPlaces)
> * 5.1.2.4 [THE 表达式形式作为位置](#THEFormsPlaces)
> * 5.1.2.5 [APPLY 表达式形式作为位置](#APPLYFormsPlaces)
> * 5.1.2.6 [Setf 展开式和位置](#SetfExpansionsPlaces)
> * 5.1.2.7 [宏表达式形式作为位置](#MacroFormsPlaces)
> * 5.1.2.8 [符号宏作为位置](#SymbolMacrosPlaces)
> * 5.1.2.9 [其他复合表达式形式作为位置](#OtherCompoundFormsPlaces)

#### 5.1.2.1 <span id="VariableNamesPlaces">变量名作为位置</span>

一个词法变量[lexical variable]或动态变量[dynamic variable]的名字可以被用作一个位置[place].

#### 5.1.2.2 <span id="FunctionCallFormsPlaces">函数调用表达式形式作为位置</span>
<!--TODO 待理解-->
如果一个函数表达式形式[function form]属于下列类别之一, 它被用作一个位置[place]:

  * 第一个元素是下面这段中任何一个函数名的函数调用表达式形式.

        aref    cdadr                    get
        bit     cdar                     gethash
        caaaar  cddaar                   logical-pathname-translations
        caaadr  cddadr                   macro-function
        caaar   cddar                    ninth
        caadar  cdddar                   nth
        caaddr  cddddr                   readtable-case
        caadr   cdddr                    rest
        caar    cddr                     row-major-aref
        cadaar  cdr                      sbit
        cadadr  char                     schar
        cadar   class-name               second
        caddar  compiler-macro-function  seventh
        cadddr  documentation            sixth
        caddr   eighth                   slot-value
        cadr    elt                      subseq
        car     fdefinition              svref
        cdaaar  fifth                    symbol-function
        cdaadr  fill-pointer             symbol-plist
        cdaar   find-class               symbol-value
        cdadar  first                    tenth
        cdaddr  fourth                   third

        Figure 5-7. 可以和 setf 一起使用的函数---1

    在 subseq 的情况中, 替换的值必须是一个序列[sequence], 其元素可能被 subseq 的序列参数所包含, 但它不一定是与指定子序列的序列[sequence]相同类型[type]的序列[sequence]. 如果替换的值的长度和要被替换的子序列长度不一样, 那么更短的那个长度决定要被存储的元素的数量, 如 replace.

* 第一个元素是 defstruct 构造的选择器函数的名字的一个函数调用表达式形式. 这个函数名字必须引用全局函数定义, 而不是一个局部定义的函数[function].

* 第一个元素是下面这段中任何一个函数名的函数调用表达式形式, 前提是给那个函数的提供的参数为一个位置[place]表达式形式; 在本例中, 新位置[place]存储回了调用提供的"更新(update)"函数的结果中.

    |函数名      |    是位置(place)的参数  | 使用的更新(update)函数 |
    | --        | --                      | --                   |
    |ldb        |   second                |     dpb              |
    |mask-field |   second                |     deposit-field    |
    |getf       |   first                 |     依赖于具体实现 |

    Figure 5-8. 可以和 setf 一起使用的函数---2 
    
    在这些表达式形式[form]的 setf 展开期间, 需要调用 get-setf-expansion 来以了解内部的, 嵌套的广义变量必须如何处理.

    来自 get-setf-expansion 的信息按如下使用.

    * ldb

        在像这样的表达式形式中:

        ```LISP
        (setf (ldb byte-spec place-form) value-form)
        ```
        
        被 place-form 引用的位置必须总是既可读[read]又可写[write]的; 注意这个更新是对 place-form 指定的广义变量, 不是任何 integer 类型[type]的对象.

        因此, setf 应该生成代码来执行以下操作:

        1. 求值 byte-spec (并且把它绑定到一个临时变量).
        2. 为 place-form 绑定临时变量.
        3. 求值 value-form (并绑定它的值或多值到存储变量中).
        4. 执行从 place-form 中读取[read].
        5. 使用来自步骤 3 的值替换步骤 4 中获取到的整数[integer]的给定位来写[write]入 place-form.

        如果步骤 3 中的 value-form 求值修改了 place-form 的内容, 就像设置整数[integer]的不同的位, 那么由 byte-spec 表示的位改变为修改后的整数[integer], 因为步骤 4 在 value-form 求值后被执行. 不过, 绑定[binding]临时变量所需的求值在步骤 1 和步骤 2 中完成, 因此可以看到预期的从左到右的求值顺序. 比如:

        ```LISP
        (setq integer #x69) =>  #x69
        (rotatef (ldb (byte 4 4) integer)
                (ldb (byte 4 0) integer))
        integer =>  #x96
        ;;; This example is trying to swap two independent bit fields
        ;;; in an integer.  Note that the generalized variable of
        ;;; interest here is just the (possibly local) program variable
        ;;; integer.
        ```

    * mask-field

        这个情况和 ldb 在所有重要方面都是一样的.

    * getf

        在像这样的表达式形式中:

        ```LISP
        (setf (getf place-form ind-form) value-form)
        ```
        
        place-form 引用的位置必须总是可读[read]可写[write]的; 注意这个更新是对 place-form 指定的广义变量, 而不一定是讨论中的特定属性列表.

        因此, setf 应该生成代码来执行以下操作:

        1. 绑定 place-form 的临时变量.
        2. 求值 ind-form (并且把它绑定到临时变量).
        3. 求值 value-form (并且绑定它的值或多值到存储变量中).
        4. 执行从 place-form 中读取[read].
        5. 用通过组合步骤2, 3 和 4 中的值获取的可能是新的属性列表写[write]到 place-form 中. (注意这个措辞 "可能的新属性列表(possibly-new property list)" 可能意味着之前的属性列表以某种方式破坏性的再次使用, 或者可能意味着它的部分或完全的复制. 因为不管是复制还是破坏性地再使用都可以发生, 因此必须继续处理可能的新属性列表的结果值, 就好像它是需要存储回广义变量中的不同副本一样.)

        如果步骤 3 中的 value-form 求值修改了 place-form 中的内容, 例如在列表中设置一个不同的已命名属性, 那么 ind-form 表示的属性的修改是对那个修改后的列表的, 因为步骤 4 在 value-form 求值后执行. 不过, 绑定临时变量所需的求值在步骤 1 和步骤 2 中完成, 因此可以看到预期的从左到右的求值顺序.

        比如:

        ```LISP
        (setq s (setq r (list (list 'a 1 'b 2 'c 3)))) =>  ((a 1 b 2 c 3))
        (setf (getf (car r) 'b)
              (progn (setq r nil) 6)) =>  6
        r =>  NIL
        s =>  ((A 1 B 6 C 3))
        ;;; Note that the (setq r nil) does not affect the actions of
        ;;; the SETF because the value of R had already been saved in
        ;;; a temporary variable as part of the step 1. Only the CAR
        ;;; of this value will be retrieved, and subsequently modified
        ;;; after the value computation.
        ```

#### 5.1.2.3 <span id="VALUESFormsPlaces">VALUES 表达式形式作为位置</span>

一个 values 表达式形式[form]可以被用作一个位置[place], 前提是它的每一个子表达式形式[subform]也是一个位置[place]表达式形式[form].

一个像这样的表达式形式

```LISP
(setf (values place-1 ...place-n) values-form)
```

执行了以下动作:

1. 每一个嵌套位置 place 的子表达式形式[subform]都按照从左到右的顺序求值.
2. 这个 values-form 被求值, 并且来自每一个位置 place 的第一个存储变量被绑定给它的返回值, 就像是通过 multiple-value-bind 一样.
3. 如果任何位置 place 的 setf 展开式[setf expansion]涉及超过一个存储变量, 那么额外的存储变量会绑定为 nil.
4. 每个位置 place 的存储表达式形式都按照从左到右的顺序计算.

values 的 setf 展开式[setf expansion]中的存储表达式形式以多值[multiple values[2]]的形式返回步骤 2 中存储变量的值. 这也就是说, 返回的值的数量和位置[place]表达式形式数量一样. 这个可能比 values-form 所产生的值更多或更少.

#### 5.1.2.4 <span id="THEFormsPlaces">THE 表达式形式作为位置</span>

一个 the 表达式形式[form]可以被用作一个位置[place], 在这个情况下这个声明被转到那个新值表达式形式 newvalue 中, 然后对产生的 setf 进行分析. 比如,

```LISP
(setf (the integer (cadr x)) (+ y 3))
```

就像下面这样被处理

```LISP
(setf (cadr x) (the integer (+ y 3)))
```

#### 5.1.2.5 <span id="APPLYFormsPlaces">APPLY 表达式形式作为位置</span>

以下涉及 apply 的 setf 情况必须被支持:

* (setf (apply #'aref array subscript* more-subscripts) new-element)
* (setf (apply #'bit array subscript* more-subscripts) new-element)
* (setf (apply #'sbit array subscript* more-subscripts) new-element)

在所有这三种情况中, 由 subscripts 和 more-subscripts 连接所指定的数组 array 的元素[element] (换句话说, 同样的元素, 如果不是 setf 表达式形式[form]的一部分, 它将通过调用 apply 来读取[read]) 被改变为 new-element 给定的值[value]. 对于这些用法, 函数名 (aref, bit, 或 sbit) 必须引用全局函数定义, 而不是局部定义函数[function].

没有其他标准化[standardized]函数[function]需要被支持, 但是一个具体实现[implementation]可以定义这样的支持. 一个具体实现[implementation]也可以为具体实现[implementation-defined]所定义的操作符定义支持.

如果一个用户定义的函数[function]被用于这个上下文, 那么以下等价是成立的, 除了要注意保留对参数子表达式形式[subform]的正确的从左到右的求值:

```LISP
(setf (apply #'name arg*) val)
==  (apply #'(setf name) val arg*)
```

#### 5.1.2.6 <span id="SetfExpansionsPlaces">Setf 展开式和位置</span>

任何有着已定义的 setf 展开器[setf expander]的操作符[operator]的复合表达式形式[compound form]可以被用作一个位置[place]. 这个操作符[operator]必须引用一个全局函数定义, 而不是一个局部定义的函数[function]或宏[macro].

#### 5.1.2.7 <span id="MacroFormsPlaces">宏表达式形式作为位置</span>

一个宏表达式形式[macro form]可以被用作一个位置[place], 在这个情况下 Common Lisp 展开那个宏表达式形式[macro form]就像是通过 macroexpand-1 然后用这个宏展开式[macro expansion]替换原始的位置[place]. 这样的宏展开式[macro expansion]只在耗尽所有其他可能性之后才尝试, 而不是在展开到对一个名为 (setf reader) 的函数调用之后.

#### 5.1.2.8 <span id="SymbolMacrosPlaces">符号宏作为位置</span>

对一个已经建立[establish]为符号宏[symbol macro]的符号[symbol]的引用可以被用作一个位置[place]. 在这个情况下, setf 展开这个引用并且分析产生的表达式形式[form].

#### 5.1.2.9 <span id="OtherCompoundFormsPlaces">其他复合表达式形式作为位置</span>

对于其他任何复合表达式形式[compound form], 假定其中操作符[operator]为符号[symbol] f, 这个 setf 表达式形式[form]展开为一个对名为 (setf f) 的函数[function]调用. 这个新构建的函数表达式形式[function form]的第一个实参[argument]是 newvalue 并且剩下的实参[argument]是位置 place 的剩余元素[element]. 不管 f 或者 (setf f) 被局部还是全局地定义为函数, 或者都不是, 这个展开都会发生. 比如,

```LISP
(setf (f arg1 arg2 ...) new-value)
```

展开为一个和下面这个有着相同效果和值的表达式形式

```LISP
(let ((#:temp-1 arg1)          ;force correct order of evaluation
      (#:temp-2 arg2)
      ...
      (#:temp-0 new-value))
  (funcall (function (setf f)) #:temp-0 #:temp-1 #:temp-2...))
```

一个名为 (setf f) 的函数[function]必须返回它的第一个参数作为它唯一的值, 以便保留 setf 的语义.

### 5.1.3 <span id="TreatmentMacrosSETF">基于 SETF 的其他宏的处理</span>

对于下一段中的每个 "读-修改-写(read-modify-write)" 操作符[operator], 还有对于任何程序员[programmer]使用 define-modify-macro 定义的额外的宏[macro], 对参数从左到右求值的常规规则导致一个例外. 对参数[argument]表达式形式[form]的求值以从左到右的顺序发生, 除了位置 place 参数[argument]例外, 那个 place 的"旧值"的实际读取[read]发生在所有的参数[argument]表达式形式[form]求值[evaluation]之后, 并且在一个"新值"被计算并重新写入到该 place 之前.

具体地说, 这些操作符[operator]中的每一个都可以被看作是带有以下通用语法的表达式形式[form]:

```LISP
(operator preceding-form* place following-form*)
```

对每个这样的表达式形式[form]的求值都是这样处理的:

1. 按从左到右的顺序求值[evaluate]每一个 preceding-forms.
2. 求值[evaluate]这个位置 place 的子表达式形式[subform], 按照这个位置 place 的 setf 展开式[setf expansion]的第二个值指定的顺序.
3. 按从左到右的顺序求值[evaluate]每一个 following-forms.
4. 从位置 place 中读取[read]旧的值.
5. 计算新的值.
6. 存储新的值到这个位置 place.

        decf  pop   pushnew
        incf  push  remf

        Figure 5-9. 读-修改-写(read-modify-write) 宏

## 5.2 <span id="TCEP">退出点的控制转移</span>

当控制转移由 go, return-from, 或 throw 发起时, 为了完成控制权的转移, 发生以下事件. 注意, 对于 go, 退出点[exit point]是 go 执行时要被执行的 tagbody 里的表达式形式[form]; 对于 return-from, 退出点[exit point]是对应的 block 表达式形式[form]; 对于 throw, 退出点[exit point]是对应的 catch 表达式形式[form].

1. 中间的退出点[exit point]被"抛弃"了 (换句话说, 它们的范围[extent]结束了, 试图通过它们来转移控制已经不再有效了).
2. 对任何中间 unwind-protect 子句的清理子句进行求值.
3. 中间的 special 变量, 捕捉标签[catch tag], 状况处理者[condition handler], 还有重启器[restart]的动态绑定[binding]被消除.
4. 被调用的退出点[exit point]的范围[extent]结束, 控制被传递给目标.

由于被跳过而"抛弃"的退出的范围在发生控制转移时就结束了. 这也就是说, 事件 1 发生在控制转移的开始. 如果尝试去转移控制到一个动态范围[dynamic extent]已经结束的退出点[exit point], 那么结果是未定义的.

事件 2 和 3 实际上是交替进行的, 顺序与它们建立的逆序相对应. 这样做的效果是一个 unwind-protect 的清理子句看到进入 unwind-protect 时变量和捕捉标签[catch tag]的相同动态绑定[binding].

事件 4 发生在控制转移结束的时候.

## 5.3 <span id="DCFDictionary">数据和控制流字典</span>

> * [函数 APPLY](#FAPPLY)
> * [宏 DEFUN](#MDEFUN)
> * [访问器 FDEFINITION](#AFDEFINITION)
> * [函数 FBOUNDP](#FFBOUNDP)
> * [函数 FMAKUNBOUND](#FFMAKUNBOUND)
> * [特殊操作符 FLET, LABELS, MACROLET](#SOFLETLABELSMACROLET)
> * [函数 FUNCALL](#FFUNCALL)
> * [特殊操作符 FUNCTION](#SOFUNCTION)
> * [函数 FUNCTION-LAMBDA-EXPRESSION](#FFUNCTION-LAMBDA-EXPRESSION)
> * [函数 FUNCTIONP](#FFUNCTIONP)
> * [函数 COMPILED-FUNCTION-P](#FCOMPILED-FUNCTION-P)
> * [常量 CALL-ARGUMENTS-LIMIT](#CCALL-ARGUMENTS-LIMIT)
> * [常量 LAMBDA-LIST-KEYWORDS](#CLAMBDA-LIST-KEYWORDS)
> * [常量 LAMBDA-PARAMETERS-LIMIT](#CLAMBDA-PARAMETERS-LIMIT)
> * [宏 DEFCONSTANT](#MDEFCONSTANT)
> * [宏 DEFPARAMETER, DEFVAR](#MDEFPARAMETERDEFVAR)
> * [宏 DESTRUCTURING-BIND](#MDESTRUCTURING-BIND)
> * [特殊操作符 LET, LET*](#SOLETLET*)
> * [特殊操作符 PROGV](#SOPROGV)
> * [特殊表达式 SETQ](#SOSETQ)
> * [宏 PSETQ](#MPSETQ)
> * [特殊操作符 BLOCK](#SOBLOCK)
> * [特殊操作符 CATCH](#SOCATCH)
> * [特殊操作符 GO](#SOGO)
> * [特殊操作符 RETURN-FROM](#SORETURN-FROM)
> * [宏 RETURN](#MRETURN)
> * [特殊操作符 TAGBODY](#SOTAGBODY)
> * [特殊操作符 THROW](#SOTHROW)
> * [特殊操作符 UNWIND-PROTECT](#SOUNWIND-PROTECT)
> * [常量 NIL](#CNIL)
> * [函数 NOT](#FNOT)
> * [常量 T](#CT)
> * [函数 EQ](#FEQ)
> * [函数 EQL](#FEQL)
> * [函数 EQUAL](#FEQUAL)
> * [函数 EQUALP](#FEQUALP)
> * [函数 IDENTITY](#FIDENTITY)
> * [函数 COMPLEMENT](#F)
> * [函数 CONSTANTLY](#COMPLEMENT)
> * [函数 EVERY, SOME, NOTEVERY, NOTANY](#FEVERYSOMENOTEVERYNOTANY)
> * [宏 AND](#MAND)
> * [宏 COND](#MCOND)
> * [特殊操作符 IF](#SOIF)
> * [宏 OR](#MOR)
> * [宏 WHEN, UNLESS](#MWHENUNLESS)
> * [宏 CASE, CCASE, ECASE](#MCASECCASEECASE)
> * [宏 TYPECASE, CTYPECASE, ETYPECASE](#MTYPECASECTYPECASEETYPECASE)
> * [宏 MULTIPLE-VALUE-BIND](#MMULTIPLE-VALUE-BIND)
> * [特殊操作符 MULTIPLE-VALUE-CALL](#SOMULTIPLE-VALUE-CALL)
> * [宏 MULTIPLE-VALUE-LIST](#MMULTIPLE-VALUE-LIST)
> * [特殊操作符 MULTIPLE-VALUE-PROG1](#SOMULTIPLE-VALUE-PROG1)
> * [宏 MULTIPLE-VALUE-SETQ](#MMULTIPLE-VALUE-SETQ)
> * [访问器 VALUES](#AVALUES)
> * [函数 VALUES-LIST](#FVALUES-LIST)
> * [常量 MULTIPLE-VALUES-LIMIT](#CMULTIPLE-VALUES-LIMIT)
> * [宏 NTH-VALUE](#MNTH-VALUE)
> * [宏 PROG, PROG*](#MPROGPROG*)
> * [宏 PROG1, PROG2](#MPROG1PROG2)
> * [特殊操作符 PROGN](#SOPROGN)
> * [宏 DEFINE-MODIFY-MACRO](#MDEFINE-MODIFY-MACRO)
> * [宏 DEFSETF](#MDEFSETF)
> * [宏 DEFINE-SETF-EXPANDER](#MDEFINE-SETF-EXPANDER)
> * [函数 GET-SETF-EXPANSION](#FGET-SETF-EXPANSION)
> * [宏 SETF, PSETF](#MSETFPSETF)
> * [宏 SHIFTF](#MSHIFTF)
> * [宏 ROTATEF](#MROTATEF)
> * [状况类型 CONTROL-ERROR](#CTCONTROL-ERROR)
> * [状况类型 PROGRAM-ERROR](#CTPROGRAM-ERROR)
> * [状况类型 UNDEFINED-FUNCTION](#CTUNDEFINED-FUNCTION)



### <span id="FAPPLY">函数 APPLY</span>

* 语法(Syntax):

        apply function &rest args+ => result*

* 参数和值(Arguments and Values):

        function---一个函数标识符[function designator].
        args---一个可扩展参数列表标识符[spreadable argument list designator].
        results---函数 function 返回的值[value].

* 描述(Description):

        应用[apply]函数 function 到这些参数 args.

        当这个函数 function 通过 &rest 接受到它的参数时, 允许(但不是必须)一个实现[implementation]去绑定[bind]剩余参数[rest parameter]到一个和 apply 的最后一个参数共享结构的对象[object]. 因为一个函数既不能检测它是否通过 apply 被调用, (如果是这样的话)也不能检测到给 apply 的最后一个参数是否是一个常量[constant], 因此符合规范的程序[conforming program]既不能依赖于剩余列表[rest list]的列表[list]结构是新创建的, 也不能修改这个列表[list]结构.

        在某些情况下, setf 可以和 apply 一起使用; 见章节 5.1.2.5 (APPLY 表达式形式作为位置).

* 示例(Examples):

    ```LISP
    (setq f '+) =>  +
    (apply f '(1 2)) =>  3
    (setq f #'-) =>  #<FUNCTION ->
    (apply f '(1 2)) =>  -1
    (apply #'max 3 5 '(2 7 3)) =>  7
    (apply 'cons '((+ 2 3) 4)) =>  ((+ 2 3) . 4)
    (apply #'+ '()) =>  0

    (defparameter *some-list* '(a b c))
    (defun strange-test (&rest x) (eq x *some-list*))
    (apply #'strange-test *some-list*) =>  implementation-dependent

    (defun bad-boy (&rest x) (rplacd x 'y))
    (bad-boy 'a 'b 'c) has undefined consequences.
    (apply #'bad-boy *some-list*) has undefined consequences.

    (defun foo (size &rest keys &key double &allow-other-keys)
      (let ((v (apply #'make-array size :allow-other-keys t keys)))
        (if double (concatenate (type-of v) v v) v)))
    (foo 4 :initial-contents '(a b c d) :double t)
        =>  #(A B C D A B C D)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        funcall, fdefinition, function, 章节 3.1 (求值), 章节 5.1.2.5 (APPLY 表达式形式作为位置)

* 注意(Notes): None.


### <span id="MDEFUN">宏 DEFUN</span>

* 语法(Syntax):

        defun function-name lambda-list [[declaration* | documentation]] form*
        => function-name

* 参数和值(Arguments and Values):

        function-name---一个函数名[function name].
        lambda-list---一个普通 lambda 列表[ordinary lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        forms---一个隐式的 progn [implicit progn].
        block-name---这个 function-name 的函数块名字[function block name].

* 描述(Description):

        在全局环境[global environment]中定义一个名为 function-name 的新函数[function]. 由 defun 定义的函数[function]的主体部分由这些表达式形式 forms 组成; 当函数[function]被调用时它们会作为一个隐式的 progn [implicit progn]被执行. defun 可以被用于定义一个新的函数[function], 去设置一个错误定义的修正版本, 去重定义一个已经定义的函数[function], 或者把一个宏[macro]重定义为函数[function].

        defun 隐式地把一个名为 block-name 的块[block]放置在这个函数[function]定义的主体表达式形式 forms 周围 (而不是 lambda-list 中的那些表达式形式[form]).

        documentation 作为一个文档字符串附加到 name (作为 function 种类) 和函数[function]对象[object]上.

        求值 defun 导致 function-name 成为 defun 被执行的词法环境[lexical environment]中的 lambda 表达式[lambda expression]

    ```LISP
    (lambda lambda-list
      [[declaration* | documentation]]
      (block block-name form*))
    ```

        所指定的函数[function]的全局名字.

        (参数中没有在宏展开时被求值.)

        defun 不需要去产生任何编译时副作用. 具体来说, defun 不会使这个函数[function]定义在编译时可用. 一个实现[implementation]可能选择去存储关于这个函数的信息用于编译时错误检测的目的 (比如检测一个调用的参数数量), 或者去使函数[function]被内联展开.

* 示例(Examples):

    ```LISP
    (defun recur (x)
      (when (> x 0)
        (recur (1- x)))) =>  RECUR
    (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
        (list a b c d keys test start)) =>  EX
    (ex 1 2) =>  (1 2 NIL 66 NIL NIL 0)
    (ex 1 2 3 4 :test 'equal :start 50)
    =>  (1 2 3 4 (:TEST EQUAL :START 50) EQUAL 50)
    (ex :test 1 :start 2) =>  (:TEST 1 :START 2 NIL NIL 0)

    ;; This function assumes its callers have checked the types of the
    ;; arguments, and authorizes the compiler to build in that assumption.
    (defun discriminant (a b c)
      (declare (number a b c))
      "Compute the discriminant for a quadratic equation."
      (- (* b b) (* 4 a c))) =>  DISCRIMINANT
    (discriminant 1 2/3 -2) =>  76/9

    ;; This function assumes its callers have not checked the types of the
    ;; arguments, and performs explicit type checks before making any assumptions.
    (defun careful-discriminant (a b c)
      "Compute the discriminant for a quadratic equation."
      (check-type a number)
      (check-type b number)
      (check-type c number)
      (locally (declare (number a b c))
        (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
    (careful-discriminant 1 2/3 -2) =>  76/9
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        flet, labels, block, return-from, declare, documentation, 章节 3.1 (求值), 章节 3.4.1 (普通 Lambda 列表), 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        return-from 可以用于从 defun 定义的函数[function]中提前返回.

        当关于这个函数定义的额外信息(通常是调试信息)被记录时, 可能发生额外的副作用.

### <span id="AFDEFINITION">访问器 FDEFINITION</span>

* 语法(Syntax):

        fdefinition function-name => definition

        (setf (fdefinition function-name) new-definition)

* 参数和值(Arguments and Values):

        function-name---一个函数名[function]. 在非 setf 的情况下, 这个名字[name]在全局环境[global environment]中必须是被 fbound 的.
        definition---由 function-name 命名的当前全局函数定义.
        new-definition---一个函数[function].

* 描述(Description):

        fdefinition 访问[access]由 function-name 命名的当前全局函数定义. 这个定义可能是一个函数[function]或表示一个特殊表达式形式[special form]或宏[macro]的对象[object]. 当 fboundp 返回 true 但是这个函数名 function-name 表示一个宏[macro]或者特殊表达式形式[special form]时, fdefinition 返回的值不是明确定义的, 但是 fdefinition 不发出一个错误.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 function-name 不是一个函数名字[function name], 那么应该发出一个 type-error 类型[type]的错误.

        在非 setf 的情况下如果 function-name 没有被 fbound, 那么就会发出 undefined-function 类型[type]的错误.

* 也见(See Also):

        fboundp, fmakunbound, macro-function, special-operator-p, symbol-function

* 注意(Notes):

        fdefinition 不能访问[access]由 flet 或 labels 产生的词法函数名的值; 它只能访问[access]全局函数的值.

        当这个函数名 function-name 的函数定义不表示一个特殊表达式形式[special form]时, setf 可以和 fdefinition 一起使用去替换全局函数定义. fdefinition 的 setf 需要一个函数[function]作为新的值. 把 function-name 的 fdefinition 设置为一个符号[symbol], 一个列表[list], 或者是一个在一个宏[macro]或特殊表达式形式[special form]上调用 fdefinition 返回的值都是错误的.


### <span id="FFBOUNDP">函数 FBOUNDP</span>

* 语法(Syntax):

        fboundp name => generalized-boolean

* 发音(Pronunciation):

        [,ef'bandpee]

* 参数和值(Arguments and Values):

        name---一个函数名[function].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果 name 被 fbound 就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (fboundp 'car) =>  true
    (fboundp 'nth-value) =>  false
    (fboundp 'with-open-file) =>  true
    (fboundp 'unwind-protect) =>  true
    (defun my-function (x) x) =>  MY-FUNCTION
    (fboundp 'my-function) =>  true
    (let ((saved-definition (symbol-function 'my-function)))
      (unwind-protect (progn (fmakunbound 'my-function)
                            (fboundp 'my-function))
        (setf (symbol-function 'my-function) saved-definition)))
    =>  false
    (fboundp 'my-function) =>  true
    (defmacro my-macro (x) `',x) =>  MY-MACRO
    (fboundp 'my-macro) =>  true
    (fmakunbound 'my-function) =>  MY-FUNCTION
    (fboundp 'my-function) =>  false
    (flet ((my-function (x) x))
      (fboundp 'my-function)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 name 不是一个函数名[function name], 应该会发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        symbol-function, fmakunbound, fdefinition

* 注意(Notes):

        允许在任何被 fbound 的符号[symbol]上调用 symbol-function.

        fboundp 有时候被用于 "保护" 一个对函数存储格[function cell]的访问, 就像:

    ```LISP
    (if (fboundp x) (symbol-function x))
    ```

        定义一个 setf 展开器[setf expander] F 不会导致 setf 函数[setf function] (setf F) 被定义.


### <span id="FFMAKUNBOUND">函数 FMAKUNBOUND</span>

* 语法(Syntax):

        fmakunbound name => name

* 发音(Pronunciation):

        [,ef'makuhn,band] 或 [,ef'maykuhn,band]

* 参数和值(Arguments and Values):

        name---一个函数名字[function name].

* 描述(Description):

        在全局环境[global environment]中移除这个名字 name 的函数[function]或宏[macro]定义, 如果有的话.

* 示例(Examples):

    ```LISP
    (defun add-some (x) (+ x 19)) =>  ADD-SOME
    (fboundp 'add-some) =>  true
    (flet ((add-some (x) (+ x 37)))
        (fmakunbound 'add-some)
        (add-some 1)) =>  38
    (fboundp 'add-some) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 name 不是一个函数名[function], 应该发出一个 type-error 类型[type]的错误.

        如果 name 是一个特殊操作符[special operator]那么后果是未定义的.

* 也见(See Also):

        fboundp, makunbound

* 注意(Notes): None.

### <span id="SOFLETLABELSMACROLET">特殊操作符 FLET, LABELS, MACROLET</span>

* 语法(Syntax):

        flet ((function-name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*
        => result*

        labels ((function-name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*
        => result*

        macrolet ((name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*
        => result*

* 参数和值(Arguments and Values):

        function-name---一个函数名[function name].
        name---一个符号[symbol].
        lambda-list---一个 lambda 列表[lambda list]; 对于 flet 和 labels, 它是一个普通 lambda 列表[ordinary lambda list]; 对于 macrolet, 它是一个宏 lambda 列表[macro lambda list].
        local-declaration---一个 declare 表达式[expression]; 不求值.
        declaration---一个 declare 表达式[expression]; 不求值.
        local-documentation---一个字符串[string]; 不求值.
        local-forms, forms---一个隐式的 progn [implicit progn].
        results---表达式形式 forms 的值[value].

* 描述(Description):

        flet, labels, 和 macrolet 定义局部函数[function]和宏[macro], 并且使用这些局部定义执行表达式形式 forms. 这些表达式形式 forms 以出现的顺序被执行.

        每一个由 flet 和 labels 创建的函数[function]和每一个由 macrolet 创建的宏[macro]的主体表达式形式 (而不是 lambda 列表[lambda list]) 被封闭在一个隐式块[implicit block]中, 它的名字为 function-name 或 name 的函数块名字[function block name].

        在局部函数/宏定义的列表和 flet 或 labels 中的主体表达式形式 forms 的声明 declarations 的作用域不包括局部定义的函数[function]的主体, 除了对于 labels, 任何引用局部定义函数的 inline, notinline, 或 ftype 声明可以应用于局部函数的主体. 这就是说, 它们的作用域[scope]和它们影响的函数名一样. 这些声明 declarations 的作用域不包括由 macrolet 定义的这个宏展开函数的主体.

    * flet

            flet 局部定义已命名函数[function]并且用这些定义绑定[binding]执行一连串的表达式形式 forms. 可以定义任意数量的局部函数[function].

            这个名称绑定[binding]的作用域[scope]仅包含主体部分. 在 flet 的主体中, 与 flet 定义的那些匹配的 function-names 引用的是局部定义的函数[function], 而不是同名的全局函数定义. 此外, 在 flet 的作用域内, 由 flet 定义的函数名 function-name 的全局 setf 展开器[setf expander]定义不适用. 注意这个适用于 (defsetf f ...), 不是 (defmethod (setf f) ...).

            flet 定义的函数[function]的名字是在这个词法环境[lexical environment]中的; 它们仅在 flet 的主体中保持他们的局部定义. 这个函数定义绑定仅在 flet 的主体中可见, 在定义自身中不可见. 在这个函数定义中, 匹配那些要被定义的局部函数名字引用的是那些在 flet 外面定义的函数[function]或宏[macro]. flet 可以局部遮蔽[shadow]一个全局函数的名字, 并且这个新的定义可以引用全局定义.

            任何局部文档 local-documentation 都作为一个文档字符串[documentation string]被附加到相应的局部函数 function 中(如果实际创建的话).

    * labels

            labels 等价于 flet 除了 labels 定义的函数名的范围包含函数定义本身以及主体.

    * macrolet

            macrolet 建立一个局部宏[macro]定义, 使用和 defmacro 相同的格式.

            在 macrolet 的主体中, 由 macrolet 定义的名字 names 对应的全局 setf 展开器[setf expander]定义是不适用的; 相反, setf 展开宏表达式形式[macro form]并递归地处理生成的表达式形式[form].

            由 macrolet 定义的宏展开函数是定义在 macrolet 表达式形式出现的词法环境[lexical environment]中的. 声明和 macrolet 和 symbol-macrolet 定义影响一个 macrolet 中的局部宏定义, 但是, 如果局部宏定义引用在该词法环境[lexical environment]中可见的任何局部变量[variable]或函数[function]绑定[binding], 那么后果是没有定义的.

            任何局部文档 local-documentation 都作为一个文档字符串[documentation string]被附加到相应的局部宏函数中.

* 示例(Examples):

    ```LISP
    (defun foo (x flag)
      (macrolet ((fudge (z)
                    ;The parameters x and flag are not accessible
                    ; at this point; a reference to flag would be to
                    ; the global variable of that name.
                    ` (if flag (* ,z ,z) ,z)))
      ;The parameters x and flag are accessible here.
        (+ x
          (fudge x)
          (fudge (+ x 1)))))
    ==
    (defun foo (x flag)
      (+ x
        (if flag (* x x) x)
        (if flag (* (+ x 1) (+ x 1)) (+ x 1))))
    ```

        在宏展开之后. x 和 flag 的出现合理地引用了函数 foo 的参数, 因为这些参数在这个产生展开式的宏调用的位置是可见的.

    ```LISP
    (flet ((flet1 (n) (+ n n)))
        (flet ((flet1 (n) (+ 2 (flet1 n))))
          (flet1 2))) =>  6

    (defun dummy-function () 'top-level) =>  DUMMY-FUNCTION
    (funcall #'dummy-function) =>  TOP-LEVEL
    (flet ((dummy-function () 'shadow))
          (funcall #'dummy-function)) =>  SHADOW
    (eq (funcall #'dummy-function) (funcall 'dummy-function))
    =>  true
    (flet ((dummy-function () 'shadow))
      (eq (funcall #'dummy-function)
          (funcall 'dummy-function)))
    =>  false

    (defun recursive-times (k n)
      (labels ((temp (n)
                  (if (zerop n) 0 (+ k (temp (1- n))))))
        (temp n))) =>  RECURSIVE-TIMES
    (recursive-times 2 3) =>  6

    (defmacro mlets (x &environment env)
        (let ((form `(babbit ,x)))
          (macroexpand form env))) =>  MLETS
    (macrolet ((babbit (z) `(+ ,z ,z))) (mlets 5)) =>  10

    (flet ((safesqrt (x) (sqrt (abs x))))
      ;; The safesqrt function is used in two places.
      (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6)))))
    =>  3.291173

    (defun integer-power (n k)
      (declare (integer n))
      (declare (type (integer 0 *) k))
      (labels ((expt0 (x k a)
                  (declare (integer x a) (type (integer 0 *) k))
                  (cond ((zerop k) a)
                        ((evenp k) (expt1 (* x x) (floor k 2) a))
                        (t (expt0 (* x x) (floor k 2) (* x a)))))
                (expt1 (x k a)
                  (declare (integer x a) (type (integer 0 *) k))
                  (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
                        (t (expt0 (* x x) (floor k 2) (* x a))))))
        (expt0 n k 1))) =>  INTEGER-POWER

    (defun example (y l)
      (flet ((attach (x)
                (setq l (append l (list x)))))
        (declare (inline attach))
        (dolist (x y)
          (unless (null (cdr x))
            (attach x)))
        l))

    (example '((a apple apricot) (b banana) (c cherry) (d) (e))
              '((1) (2) (3) (4 2) (5) (6 3 2)))
    =>  ((1) (2) (3) (4 2) (5) (6 3 2) (A APPLE APRICOT) (B BANANA) (C CHERRY))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declare, defmacro, defun, documentation, let, 章节 3.1 (求值), 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        用 flet 去定义递归的函数[function]是可以的. labels 可以被用于定义相互递归函数[function].

        如果一个 macrolet 表达式形式[form]是一个顶层表达式形式[top level form], 主体表达式形式 forms 也被当作顶层表达式形式[top level form]处理. 见章节 3.2.3 (文件编译).


### <span id="FFUNCALL">函数 FUNCALL</span>

* 语法(Syntax):

        funcall function &rest args => result*

* 参数和值(Arguments and Values):

        function---一个函数指定符[function designator].
        args---给这个函数 function 的实参[argument].
        results---这个函数 function 返回的值[value].

* 描述(Description):

        funcall 对 args 应用函数 function. 如果 function 是一个符号[symbol], 它会被强制转为一个函数[function], 就好像是通过在全局环境[global environment]中找它的函数性值[functional value]一样.

* 示例(Examples):

    ```LISP
    (funcall #'+ 1 2 3) =>  6
    (funcall 'car '(1 2 3)) =>  1
    (funcall 'position 1 '(1 2 3 2 1) :start 1) =>  4
    (cons 1 2) =>  (1 . 2)
    (flet ((cons (x y) `(kons ,x ,y)))
      (let ((cons (symbol-function '+)))
        (funcall #'cons
                (funcall 'cons 1 2)
                (funcall cons 1 2))))
    =>  (KONS (1 . 2) 3)
    ```LISP

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 function 是一个没有作为函数[function]的全局定义或者有着作为宏[macro]或特殊操作符[special operator]的全局定义的符号[symbol], 那么应该会发出一个 undefined-function 类型[type]的错误.

* 也见(See Also):

        apply, function, 章节 3.1 (求值)

* 注意(Notes):

    ```LISP
    (funcall function arg1 arg2 ...)
    ==  (apply function arg1 arg2 ... nil)
    ==  (apply function (list arg1 arg2 ...))
    ```

        funcall 和一个普通函数调用的区别在于, 在前一种情况下, 函数是通过对一种表达式形式[form]的普通求值[evaluation]得到的, 在后者的情况下, 它是由正常发生的函数位置的特殊解释得到的.


### <span id="SOFUNCTION">特殊操作符 FUNCTION</span>

* 语法(Syntax):

        function name => function

* 参数和值(Arguments and Values):

        name---一个函数名[function name]或者一个 lambda 表达式[lambda expression].
        function---一个函数[function]对象[object].

* 描述(Description):

        这个 function 的值[value]是当前词法环境[lexical environment]中 name 的函数性值[functional value].

        如果 name 是一个函数名[function name], 这个名称的函数定义是由最内部词法上闭合的 flet, labels, 或 macrolet 表达式形式[form]建立的那个, 如果有的话. 否则会返回这个函数名[function name]的全局函数定义会被返回.

        如果 name 是一个 lambda 表达式[lambda expression], 那么返回一个词法闭包[lexical closure]. 如果在可能不止依次产生同一组绑定[binding]上的闭包[closure]的情况中, 那么产生的各种闭包可能也可能不是 eq 的.

        在一个 function 表达式形式出现的词法环境中, 在一个不表示函数[function]的函数名[function name]上用 function 是错误的. 具体来说, 在一个表示宏[macro]或者特殊表达式形式[special form]的符号[symbol]上使用 function 是错误的. 一个具体实现可能出于性能原因不去发出这个错误, 但是具体实现禁止去把这个发送错误的失败定义为一个有用的行为. 

* 示例(Examples):

    ```LISP
    (defun adder (x) (function (lambda (y) (+ x y))))
    ```

        这个 (adder 3) 的结果是一个把 3 加给参数的函数:

    ```LISP
    (setq add3 (adder 3))
    (funcall add3 5) =>  8
    ```

        这个可以正常工作是因为 function 创建了一个 lambda 表达式[lambda expression]的闭包[closure], 这个闭包引用了变量 x 的值[value] 3, 即便控制流已经中函数 adder 中返回了.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defun, fdefinition, flet, labels, symbol-function, 章节 3.1.2.1.1 (符号表达式形式), 章节 2.4.8.2 (井号单引号(#')), 章节 22.1.3.13 (打印其他对象)

* 注意(Notes):

        标记 #'name 可能被用于 (function name) 的缩写.


### <span id="FFUNCTION-LAMBDA-EXPRESSION">函数 FUNCTION-LAMBDA-EXPRESSION</span>

* 语法(Syntax):

        function-lambda-expression function
        => lambda-expression, closure-p, name

* 参数和值(Arguments and Values):

        function---一个函数[function].
        lambda-expression---一个 lambda 表达式[lambda expression]或 nil.
        closure-p---一个广义的 boolean [generalized boolean].
        name---一个对象[object].

* 描述(Description):

        按如下返回关于函数 function 的信息:

        这个主值[primary value], lambda-expression, 是函数 function 的定义 lambda 表达式[lambda expression], 如果这个信息不可用那么就是 nil. 这个 lambda 表达式[lambda expression]可能已经在某些方面进行了预处理, 但它仍然应该是 compile 或 function 的一个合适的参数. 任何具体实现[implementation]可以合理地返回 nil 作为任何函数 function 的 lambda-expression.

        第二个值[secondary value], closure-p, 如果函数 function 的定义是空词法环境[null lexical environment]中封闭的那么就是 nil, 如果是非空词法环境[non-null lexical environment]封闭的那么就不是 nil [non-nil]. 任何具体实现[implementation]可以合理地返回 true 作为任何 function 的 closure-p.

        第三个值[tertiary value], name, 是函数 function 的 "名字(name)". 该名称仅用于调试, 例如, 没有必要是在 defun 或 function 中可以有效用作名称的一个名称. 按照惯例, nil 被用于表示这个函数 function 没有名字. 任何具体实现[implementation]可以合理地返回 nil 作为任何 function 的 name.

* 示例(Examples):

        下面的例子说明了一些可能的返回值, 但是并不是详尽的:

    ```LISP
    (function-lambda-expression #'(lambda (x) x))
    =>  NIL, false, NIL
    OR=>  NIL, true, NIL
    OR=>  (LAMBDA (X) X), true, NIL
    OR=>  (LAMBDA (X) X), false, NIL

    (function-lambda-expression
        (funcall #'(lambda () #'(lambda (x) x))))
    =>  NIL, false, NIL
    OR=>  NIL, true, NIL
    OR=>  (LAMBDA (X) X), true, NIL
    OR=>  (LAMBDA (X) X), false, NIL

    (function-lambda-expression
        (funcall #'(lambda (x) #'(lambda () x)) nil))
    =>  NIL, true, NIL
    OR=>  (LAMBDA () X), true, NIL
    NOT=>  NIL, false, NIL
    NOT=>  (LAMBDA () X), false, NIL

    (flet ((foo (x) x))
      (setf (symbol-function 'bar) #'foo)
      (function-lambda-expression #'bar))
    =>  NIL, false, NIL
    OR=>  NIL, true, NIL
    OR=>  (LAMBDA (X) (BLOCK FOO X)), true, NIL
    OR=>  (LAMBDA (X) (BLOCK FOO X)), false, FOO
    OR=>  (SI::BLOCK-LAMBDA FOO (X) X), false, FOO

    (defun foo ()
      (flet ((bar (x) x))
        #'bar))
    (function-lambda-expression (foo))
    =>  NIL, false, NIL
    OR=>  NIL, true, NIL
    OR=>  (LAMBDA (X) (BLOCK BAR X)), true, NIL
    OR=>  (LAMBDA (X) (BLOCK BAR X)), true, (:INTERNAL FOO 0 BAR)
    OR=>  (LAMBDA (X) (BLOCK BAR X)), false, "BAR in FOO"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        虽然具体实现[implementation]可以在所有情况下返回 "nil, true, nil", 但是还是鼓励这些实现在参数是对 compile 或 eval 的调用所创建的情况下返回一个 lambda 表达式[lambda expression]作为主值[primary value] (与通过加载已编译文件[compiled file]所创建的相反).


### <span id="FFUNCTIONP">函数 FUNCTIONP</span>

* 语法(Syntax):

        functionp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 function 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (functionp 'append) =>  false
    (functionp #'append) =>  true
    (functionp (symbol-function 'append)) =>  true
    (flet ((f () 1)) (functionp #'f)) =>  true
    (functionp (compile nil '(lambda () 259))) =>  true
    (functionp nil) =>  false
    (functionp 12) =>  false
    (functionp '(lambda (x) (* x x))) =>  false
    (functionp #'(lambda (x) (* x x))) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (functionp object) ==  (typep object 'function)


### <span id="FCOMPILED-FUNCTION-P">函数 COMPILED-FUNCTION-P</span>

* 语法(Syntax):

        compiled-function-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 compiled-function 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (defun f (x) x) =>  F
    (compiled-function-p #'f)
    =>  false
    OR=>  true
    (compiled-function-p 'f) =>  false
    (compile 'f) =>  F
    (compiled-function-p #'f) =>  true
    (compiled-function-p 'f) =>  false
    (compiled-function-p (compile nil '(lambda (x) x)))
    =>  true
    (compiled-function-p #'(lambda (x) x))
    =>  false
    OR=>  true
    (compiled-function-p '(lambda (x) x)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        compile, compile-file, compiled-function

* 注意(Notes):

        (compiled-function-p object) ==  (typep object 'compiled-function)


### <span id="CCALL-ARGUMENTS-LIMIT">常量 CALL-ARGUMENTS-LIMIT</span>

* 常量值(Constant Value):

        一个不小于 50 的整数, 至少和 lambda-parameters-limit 的值[value]一样大, 它的精确大小是依赖于具体实现的[implementation-dependent].

* 描述(Description):

        传递给函数[function]的实参[arugment]数量的上限.

* 示例(Examples): None.

* 也见(See Also):

        lambda-parameters-limit, multiple-values-limit

* 注意(Notes): None.


### <span id="CLAMBDA-LIST-KEYWORDS">常量 LAMBDA-LIST-KEYWORDS</span>

* 常量值(Constant Value):

        一个列表[list], 其中的元素[element]是依赖于具体实现的[implementation-dependent], 但是其中至少需要包含符号[symbol] &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, 和 &whole.

* 描述(Description):

        在这个具体实现[implementation]中使用的所有 lambda 列表关键字[lambda list keywords]的列表[list], 包括仅被宏[macro]定义表达式形式[form]所使用的其他关键词.

* 示例(Examples): None.

* 也见(See Also):

        defun, flet, defmacro, macrolet, 章节 3.1.2 (求值模型)

* 注意(Notes): None.

### <span id="CLAMBDA-PARAMETERS-LIMIT">常量 LAMBDA-PARAMETERS-LIMIT</span>

* 常量值(Constant Value):

        依赖于具体实现[implementation-dependent], 但是不小于 50.

* 描述(Description):

        一个正整数[integer], 表示可以出现在一个单独的 lambda 列表[lambda list]中的形参[parameter]名字[name]数量的上限.

* 示例(Examples): None.

* 也见(See Also):

        call-arguments-limit

* 注意(Notes):

        鼓励实现者去使 lambda-parameters-limit 的值[value]尽可能的大.

### <span id="MDEFCONSTANT">宏 DEFCONSTANT</span>

* 语法(Syntax):

        defconstant name initial-value [documentation] => name

* 参数和值(Arguments and Values):

        name---一个符号[symbol]; 不求值.
        initial-value---一个表达式形式[form]; 求值.
        documentation---一个字符串[string]; 不求值.

* 描述(Description):

        defconstant 导致由 name 命名的全局变量被赋予 initial-value 求值后的结果值.

        一个由 defconstant 定义的常量可以被 defconstant 重定义. 然而, 如果尝试去使用其他操作符去给这个符号[symbol]赋一个值[value]或者使用后面的 defconstant 将其赋给不同[different]的值[value], 那么后果是未定义的.

        如果提供了 documentation, 它会作为一个 variable 种类的文档字符串[documentation string]关联到 name 上.

        defconstant 通常以顶层表达式形式[top level form]出现, 但它作为非顶层表达式形式[non-top-level form]出现也是有意义的. 然而, 下面描述的编译时副作用只发生在 defconstant 以顶层表达式形式[top level form]出现的情况中.

        如果在 defconstant 被执行时存在任何由 name 命名的变量的绑定[binding]或者其中的值和 initial-value 不是 eql 的, 那么结果是未定义的.

        当常量符号被重绑定为词法或动态变量时, 其结果是未定义的. 换句话说, 指向 defconstant 声明的符号[symbol]的一个引用总是指向它的全局的值.

        执行 defconstant 的副作用必须与以下代码执行的副作用相等:

    ```LISP
    (setf (symbol-value 'name) initial-value)
    (setf (documentation 'name 'variable) 'documentation)
    ```

        如果一个 defconstant 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须识别出 name 是一个常变量[constant variable]. 一个具体实现可能选择在编译时或加载时或两种情况下都去求值值表达式形式(value-form). 因此, 用户必须确保 initial-value 可以在编译时被求值 (不管对这个名字 name 的引用是否出现在文件中) 并且总是求值为同一个值.

* 示例(Examples):

    ```LISP
    (defconstant this-is-a-constant 'never-changing "for a test") =>  THIS-IS-A-CONSTANT
    this-is-a-constant =>  NEVER-CHANGING
    (documentation 'this-is-a-constant 'variable) =>  "for a test"
    (constantp 'this-is-a-constant) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declaim, defparameter, defvar, documentation, proclaim, 章节 3.1.2.1.1.3 (常变量), 章节 3.2 (编译)

* 注意(Notes): None.


### <span id="MDEFPARAMETERDEFVAR">宏 DEFPARAMETER, DEFVAR</span>

* 语法(Syntax):

        defparameter name initial-value [documentation] => name
        defvar name [initial-value [documentation]] => name

* 参数和值(Arguments and Values):

        name---一个符号[symbol]; 不求值.
        initial-value---一个表达式形式[form]; 对于 defparameter, 它总是被求值, 但是对于 defvar 只有在 name 还没有被绑定的情况下求值.
        documentation---一个字符串; 不求值.

* 描述(Description):

        defparameter 和 defvar 把名字 name 建立[establish]为一个动态变量[dynamic variable].

        defparameter 无条件地把 initial-value 赋值[assign]给 name 命名的动态变量[dynamic variable]. defvar, 相比之下, 只有在 name 还没有被绑定[bound]时把 initial-value (如果提供的话) 赋值[assign]给 name 命名的动态变量[dynamic variable].

        如果没有提供 initial-value, defvar 让名为 name 的动态变量[dynamic variable]的值存储格[value cell]保持不变; 如果 name 以前绑定[bound]过的话, 它的旧值[value]就会一直存在, 如果它之前未绑定[unbound], 那么它仍然是未绑定[unbound].

        如果提供了 documentation, 它会作为 variable 类型的文档字符串[documentation string]关联给 name.

        defparameter 和 defvar 通常作为顶层表达式形式[top level form]出现, 但是当它们作为非顶层表达式形式[non-top-level form]出现时也是有意义的. 然而, 下面描述的编译时副作用仅发生在它们作为顶层表达式形式[top level form]的时候.

* 示例(Examples):

    ```LISP
    (defparameter *p* 1) =>  *P*
    *p* =>  1
    (constantp '*p*) =>  false
    (setq *p* 2) =>  2
    (defparameter *p* 3) =>  *P*
    *p* =>  3

    (defvar *v* 1) =>  *V*
    *v* =>  1
    (constantp '*v*) =>  false
    (setq *v* 2) =>  2
    (defvar *v* 3) =>  *V*
    *v* =>  2

    (defun foo ()
      (let ((*p* 'p) (*v* 'v))
        (bar))) =>  FOO
    (defun bar () (list *p* *v*)) =>  BAR
    (foo) =>  (P V)
    ```

        defparameter 和 defvar 之间的主要操作上区别是, defparameter 对 name 做出了一个无条件的赋值, 而 defvar 则是一个有条件的赋值. 在实践中, 这就意味着在加载或重新加载定义会想要获得变量的新值时 defparameter 是很有用的, 而当文件被加载或重新加载时想要保持旧值时 defvar 是比较有用的. 比如, 可以创建一个文件, 其中包含:

    ```LISP
    (defvar *the-interesting-numbers* '())
    (defmacro define-interesting-number (name n)
      `(progn (defvar ,name ,n)
              (pushnew ,name *the-interesting-numbers*)
              ',name))
    (define-interesting-number *my-height* 168) ;cm
    (define-interesting-number *my-weight* 13)  ;stones
    ```

        这里的变量 *the-interesting-numbers* 的初始值, (), 只是一个种子, 一旦从中扩展出什么时我们不会想去重置它为某个其他东西. 像这样, 我们使用 defvar 来避免文件第二次加载时 *the-interesting-numbers* 信息被重置. 确实, 这里的两个对 define-interesting-number 调用会被重新处理, 但是如果在另一个文件中由其他调用, 它们不会被重新处理并且信息会丢失. 另一方面, 思考下面代码:

    ```LISP
    (defparameter *default-beep-count* 3)
    (defun beep (&optional (n *default-beep-count*))
      (dotimes (i n) (si:%beep 1000. 100000.) (sleep 0.1)))
    ```
    
        这里我们可以简单地想象编辑代码去改变 *default-beep-count* 的初始值, 然后重新载入文件去取出新的值. 为了使值更新更简单, 我们使用 defparameter.

        另一方面, 在这种情况下使用 defvar 是有潜在价值的. 比如, 假设某人为 *default-beep-count* 预先定义了一个替代值, 或者已经加载这个文件然后手动修改这个值. 在这两种情况下, 如果我们已经使用 defvar 而不是 defparameter, 那些用户设置不会因加载(或重新加载) 而被重写.

        使用 defparameter 还是 defvar 的选择对程序有明显的影响, 但通常是出于主观原因.

* 副作用(Side Effects):

        如果一个 defvar 或 defparameter 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须识别这个已经被公告为 special 的 name. 然而, 在编译时它既不能求值[evaluate] initial-value 表达式形式[form], 也不能对名为 name 的动态变量[dynamic variable]赋值[assign].

        这里可能由额外的编译期或运行期副作用 (依赖于具体实现的[implementation-defined]), 只要这些副作用不影响符合规范的程序[conforming program]的正常操作.

* 受此影响(Affected By):

        defvar 受 name 是否已经被绑定[bound]的影响.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declaim, defconstant, documentation, 章节 3.2 (编译)

* 注意(Notes):

        通常, 在名称的开头和结尾用星号[asterisk]来命名动态变量[dynamic variable]. 比如, *foo* 对于一个动态变量[dynamic variable]是一个好名字, 但是对于一个词法变量[lexical variable]则不是好名字; foo 对于词法变量[lexical variable]是一个好名字, 但是对于动态变量[[dynamic variable]]不是一个好名字. Common Lisp 中所有已定义的名字[defined name]都遵守这种命名约定; 然而, 符合规范的程序[conforming program]和符合规范的实现[conforming implementation]都不是必须去遵守这个约定.

        允许额外的副作用的目的是去允许具体实现[implementation]伴随着定义做正常的"记录(bookkeeping)". 比如, 一个 defvar 或 defparameter 表达式形式[form]的宏展开式[macro expansion]可能包含安排记录这个定义发生的源文件名字的代码.

        defparameter 和 defvar 可能按如下定义:

    ```LISP
    (defmacro defparameter (name initial-value 
                            &optional (documentation nil documentation-p))
      `(progn (declaim (special ,name))
              (setf (symbol-value ',name) ,initial-value)
              ,(when documentation-p
                  `(setf (documentation ',name 'variable) ',documentation))
              ',name))
    (defmacro defvar (name &optional
                            (initial-value nil initial-value-p)
                            (documentation nil documentation-p))
      `(progn (declaim (special ,name))
              ,(when initial-value-p
                  `(unless (boundp ',name)
                    (setf (symbol-value ',name) ,initial-value)))
              ,(when documentation-p
                  `(setf (documentation ',name 'variable) ',documentation))
              ',name))
    ```

### <span id="MDESTRUCTURING-BIND">宏 DESTRUCTURING-BIND</span>

* 语法(Syntax):

        destructuring-bind lambda-list expression declaration* form*
        => result*

* 参数和值(Arguments and Values):

        lambda-list---一个解构 lambda 列表[destructuring lambda list].
        expression---一个表达式形式[from].
        declaration---一个 declare 表达式[expression]; 不求值.
        forms---一个隐式的 progn [implicit progn].
        results---这些表达式形式[forms]返回的值[value].

* 描述(Description):

        destructuring-bind 绑定 lambda-list 中指定的变量为 expression 求值结果的树结构的对应值中; 然后 destructuring-bind 求值表达式形式 forms.

        这个 lambda-list 按照章节 3.4.5 (解构 lambda 列表) 中描述的那样支持解构.

* 示例(Examples):

    ```LISP
    (defun iota (n) (loop for i from 1 to n collect i))       ;helper
    (destructuring-bind ((a &optional (b 'bee)) one two three)
        `((alpha) ,@(iota 3))
      (list a b three two one)) =>  (ALPHA BEE 3 2 1)
    ```
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果求值 expression 的结果不匹配这个解构模式, 应该会发出一个 error 类型[type]的错误.

* 也见(See Also):

        macrolet, defmacro

* 注意(Notes): None.


### <span id="SOLETLET*">特殊操作符 LET, LET*</span>

* 语法(Syntax):

        let ({var | (var [init-form])}*) declaration* form* => result*
        let* ({var | (var [init-form])}*) declaration* form* => result*

* 参数和值(Arguments and Values):

        var---一个符号[symbol].
        init-form---一个表达式形式[form].
        declaration---一个 declare 表达式[expression]; 不求值.
        form---一个表达式形式[form].
        results---这些表达式形式[form]返回的值[value].

* 描述(Description):

        let 和 let* 创建新的变量绑定[binding]并且使用这些绑定[binding]执行一系列的表达式形式 forms. let 并行地执行这些绑定[binding]而 let* 顺序地执行.

        下面这个表达式形式

    ```LISP
    (let ((var1 init-form-1)
          (var2 init-form-2)
          ...
          (varm init-form-m))
      declaration1
      declaration2
      ...
      declarationp
      form1
      form2
      ...
      formn)
    ```

        首先求值表达式 init-form-1, init-form-2, 以此类推, 按照这个顺序, 保存结果值. 然后所有变量 varj 绑定为对应的值; 每一个绑定[binding]都是词法的除非相反有一个 special 声明. 然后表达式 formk 依次被求值; 除了最后一个, 所有的值都被丢弃 (这也就是说, 一个 let 的主体是一个隐式的 progn [implicit progn]).

        let* 类似于 let, 但是变量的绑定[binging]被顺序执行而非并行执行. 一个 var 的 init-form 的表达式可以引用前面 let* 绑定的 vars.

        下面这个表达式形式

    ```LISP
    (let* ((var1 init-form-1)
           (var2 init-form-2)
           ...
           (varm init-form-m))
      declaration1
      declaration2
      ...
      declarationp
      form1
      form2
      ...
      formn)
    ```

        首先求值 init-form-1, 然后绑定变量 var1 为那个值; 然后求值 init-form-2 并绑定 var2, 以此类推. 然后表达式 formj 按顺序求值; 除了最后一个, 所有的值都被丢弃 (这也就是说, 一个 let* 的主体是一个隐式的 progn).

        对于 let 和 let*, 如果这里没有和一个 var 关联的一个 init-form, var 被初始化为 nil.

        特殊表达式形式 let 有一个属性, 就是名称绑定的作用域[scope]不包括任何初始值表达式形式. 对于 let*, 一个变量的作用域[scope]包括了剩下的后续变量绑定的剩余初始化值表达式.

* 示例(Examples):

    ```LISP
    (setq a 'top) =>  TOP
    (defun dummy-function () a) =>  DUMMY-FUNCTION
    (let ((a 'inside) (b a))
       (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP TOP"
    (let* ((a 'inside) (b a))
       (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE INSIDE TOP"
    (let ((a 'inside) (b a))
       (declare (special a))
       (format nil "~S ~S ~S" a b (dummy-function))) =>  "INSIDE TOP INSIDE"
    ```

        代码

    ```LISP
    (let (x)
      (declare (integer x))
      (setq x (gcd y z))
      ...)
    ```

        是错误的; 虽然 x 事实上在它被使用前设置, 并且被设置为一个声明类型整型[integer]的值, 然而, x 一开始在值为 nil 时违反了类型声明.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        progv

* 注意(Notes): None.


### <span id="SOPROGV">特殊操作符 PROGV</span>

* 语法(Syntax):

        progv symbols values form* => result*

* 参数和值(Arguments and Values):

        symbols---一个符号[symbol]列表[list]; 求值.
        values---一个对象[object]列表[list]; 求值.
        forms---一个隐式的 progn [implicit progn].
        results---这些表达式形式[form]返回的值.

* 描述(Description):

        progv 创建新的动态变量绑定[binding]并且使用这些绑定[binding]执行每个表达式形式 form. 每个表达式形式 form 按顺序求值.

        progv 允许去绑定[binding]一个或多个动态变量, 这些变量的名字可能在运行时确定. 每一个表达式形式 form 依次被求值, 其中名字在这些符号 symbols 中的这些动态变量被绑定为对应的值. 如果提供的值 values 太少, 剩余的符号[symbol]就会被绑定, 并且没有值. 如果提供了太多的值 values, 多余的值会被忽略. 这些动态变量的绑定在 progv 退出时被取消了.

* 示例(Examples):

    ```LISP
    (setq *x* 1) =>  1
    (progv '(*x*) '(2) *x*) =>  2
    *x* =>  1
    ```

        假设 *x* 不是全局特殊的,

    ```LISP
    (let ((*x* 3))
        (progv '(*x*) '(4)
          (list *x* (symbol-value '*x*)))) =>  (3 4)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        let, 章节 3.1 (求值)

* 注意(Notes):

        除了别的之外, 为 Lisp 中的嵌入式语言写解释器时 progv 是很有用的; 它为绑定[binding]动态变量[dynamic variable]的机制提供了一个途径.


### <span id="SOSETQ">特殊表达式 SETQ</span>

* 语法(Syntax):

        setq {pair}* => result

        pair::= var form

* 发音(Pronunciation):

        ['set,kyoo]

* 参数和值(Arguments and Values):

        var---一个符号[symbol], 命名一个变量[variable], 而不是一个常量[constant variable].
        form---一个表达式形式[form].
        result---最后一个表达式形式 form 返回的主值[primary value], 如果没有提供 pair 就返回 nil.

* 描述(Description):

        赋值给那些变量[variable].

        (setq var1 form1 var2 form2 ...) 是 Lisp 简单的变量赋值语句. 首先 form1 被求值并且结果被存储在变量 var1 中, 然后 form2 被求值并且结果存在 var2 中, 等等. setq 可以被用于词法变量或动态变量的赋值.

        如果任何 var 引用了 symbol-macrolet 产生的一个绑定[binding], 那么这个 var 就好像是被 setf (不是 setq) 处理一样.

* 示例(Examples):

    ```LISP
    ;; A simple use of SETQ to establish values for variables.
    (setq a 1 b 2 c 3) =>  3
    a =>  1
    b =>  2
    c =>  3

    ;; Use of SETQ to update values by sequential assignment.
    (setq a (1+ b) b (1+ a) c (+ a b)) =>  7
    a =>  3
    b =>  4
    c =>  7

    ;; This illustrates the use of SETQ on a symbol macro.
    (let ((x (list 10 20 30)))
      (symbol-macrolet ((y (car x)) (z (cadr x)))
        (setq y (1+ z) z (1+ y))
        (list x y z)))
    =>  ((21 22 30) 21 22)
    ```

* 副作用(Side Effects):

        每一个表达式形式 form 的主值[primary value]都被赋给对应的 var.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        psetq, set, setf

* 注意(Notes): None.


### <span id="MPSETQ">宏 PSETQ</span>

* 语法(Syntax):

        psetq {pair}* => nil

        pair::= var form

* 发音(Pronunciation):

        psetq: [;pee'set,kyoo]

* 参数和值(Arguments and Values):

        var---一个符号[symbol], 命名一个变量[variable], 而不是一个常量[constant variable].
        form---一个表达式形式[form].

* 描述(Description):

        赋值给那些变量[variable].

        这个就像是 setq, 除了赋值是并行发生的之外. 这也就是说, 首先所有的表达式形式都被求值, 只有这样之后变量才会被设置成对应结果值. 通过这种方式, 对一个变量的赋值不会影响另一个变量的值计算, 这个会发生在 setq 的顺序赋值中.

        如果任何 var 引用了 symbol-macrolet 创建的绑定[binding], 那么这个 var 就好像是被 psetf (不是 psetq) 处理一样.

* 示例(Examples):

    ```LISP
    ;; A simple use of PSETQ to establish values for variables.
    ;; As a matter of style, many programmers would prefer SETQ
    ;; in a simple situation like this where parallel assignment
    ;; is not needed, but the two have equivalent effect.
    (psetq a 1 b 2 c 3) =>  NIL
    a =>  1
    b =>  2
    c =>  3

    ;; Use of PSETQ to update values by parallel assignment.
    ;; The effect here is very different than if SETQ had been used.
    (psetq a (1+ b) b (1+ a) c (+ a b)) =>  NIL
    a =>  3
    b =>  2
    c =>  3

    ;; Use of PSETQ on a symbol macro.
    (let ((x (list 10 20 30)))
       (symbol-macrolet ((y (car x)) (z (cadr x)))
        (psetq y (1+ z) z (1+ y))
        (list x y z)))
    =>  ((21 11 30) 21 11)
 
    ;; Use of parallel assignment to swap values of A and B.
    (let ((a 1) (b 2))
      (psetq a b  b a)
      (values a b))
    =>  2, 1
    ```

* 副作用(Side Effects):

        这些表达式形式 forms 的值会赋给那些变量 vars.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        psetf, setq

* 注意(Notes): None.


### <span id="SOBLOCK">特殊操作符 BLOCK</span>

* 语法(Syntax):

        block name form* => result*

* 参数和值(Arguments and Values):

        name---一个符号[symbol].
        form---一个表达式形式[form].
        results---如果发生正常返回[normal return]那么就是那些表达式形式[form]的那些值[value], 否则, 如果发生显式返回[explicit return], 就返回传递的那些值[value].

* 描述(Description):

        block 建立[establish]一个名为 name 的语句块[block]然后就好像一个隐式 progn 来求值多个 form.

        特殊操作符[special operator] block 和 return-from 一起使用来提供一个结构化的, 词法的, 非局部的退出机制. 词法上包含在这些表达式形式中的任何点, return-from 可以和给定的 name 一起使用来从 blook 表达式形式[form]中返回控制和值, 除了当内部有一个相同名字的块[block]被建立时, 在这种情况下较外部的块[block]会被更内部的那个所遮蔽.

        名为 name 的这个块有着词法作用域[lexical scope]和动态范围[dynamic extent].

        一旦建立, 一个块[block]可能只退出一次, 不管是正常退出[normal return]还是显式退出[explicit return].

* 示例(Examples):

    ```LISP
    (block empty) =>  NIL
    (block whocares (values 1 2) (values 3 4)) =>  3, 4
    (let ((x 1))
      (block stop (setq x 2) (return-from stop) (setq x 3))
      x) =>  2
    (block early (return-from early (values 1 2)) (values 3 4)) =>  1, 2
    (block outer (block inner (return-from outer 1)) 2) =>  1
    (block twin (block twin (return-from twin 1)) 2) =>  2
    ;; Contrast behavior of this example with corresponding example of CATCH.
    (block b
      (flet ((b1 () (return-from b 1)))
        (block b (b1) (print 'unreachable))
        2)) =>  1
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        return, return-from, 章节 3.1 (求值)

* 注意(Notes):

### <span id="SOCATCH">特殊操作符 CATCH</span>

* 语法(Syntax):

        catch tag form* => result*

* 参数和值(Arguments and Values):

        tag---一个捕捉标签[catch tag]; 求值的.
        forms---一个隐式的 progn [implicit progn].
        results---如果这些表达式形式 forms 正常退出, 那么就是这些表达式形式 forms 返回的那些值[value]; 如果发生一个对 tag 的抛出, 那么就是那些被抛出来的值[value].

* 描述(Description):

        catch 被用作 throw 的非局部控制转移的终点. 这些 tags 被用于查找 throw 控制转移到的 catch. (catch 'foo form) 捕捉一个 (throw 'foo form) 而不是 (throw 'bar form).

        catch 执行顺序如下:

        1. tag 被求值. 它是 catch 的名称.

        2. 然后这些表达式形式 forms 作为隐式 progn [implicit progn]来求值, 除非发生一个 throw, 否则返回最后一个表达式形式 form 的结果.

        3. 如果在执行这些表达式形式 forms 的其中一个时发生一个 throw, 控制被转移到一个 catch 表达式形式[form], 这个 catch 表达式形式的 tag 和 throw 的 tag 参数是 eq 的并且它是这个 tag 最近建立的 catch. 不再对这些表达式形式 forms 进行进一步的求值.

        4. catch 建立的这个 tag 在结果返回前就会被消除.

        如果在这些表达式形式 forms 的其中一个执行期间, 一个 throw 被执行而它的 tag 和 catch 的 tag 是 eq 的, 那么 throw 指定的值作为动态最新建立的这个 tag 的 catch 表达式的结果返回.

        即便 throw 不在 catch 的词法作用域里, catch 和 throw 机制还是会起作用. throw 必须发生在对应 tag 的 catch 的主体求值[evaluation]的动态范围[dynamic extent]内.

* 示例(Examples):

    ```LISP
    (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4) =>  3
    (catch 'dummy-tag 1 2 3 4) =>  4
    (defun throw-back (tag) (throw tag t)) =>  THROW-BACK
    (catch 'dummy-tag (throw-back 'dummy-tag) 2) =>  T

    ;; Contrast behavior of this example with corresponding example of BLOCK.
    (catch 'c
      (flet ((c1 () (throw 'c 1)))
        (catch 'c (c1) (print 'unreachable))
        2)) =>  2
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在没有合适的 catch tag 时 throw 被执行, 会发出一个 control-error 类型[type]的错误.

* 也见(See Also):

        throw, 章节 3.1 (求值)

* 注意(Notes):

        符号[symbol]被用作 tags 是惯例, 但是任何对象[object]也是允许的. 然而, 数字不应该被使用, 因为这个比较是使用 eq 来完成的.

        catch 和 block 的区别在于 catch 标签有着动态作用域[scope]而 block 名字有词法作用域[lexical scope].

### <span id="SOGO">特殊操作符 GO</span>

* 语法(Syntax):

        go tag =>|

* 参数和值(Arguments and Values):

        tag---一个 go 标签[go tag].

* 描述(Description):

        go 将控制转移到一个由和 tag 是 eql 的标签所标记的闭合 tagbody 表达式形式的主体上的一个点. 如果在这个主体内没有这样一个 tag, 那么词法上包含 tagbody 表达式形式[form]的那些主体 (如果有的话) 也会被检查. 如果有好几个标签和 tag 是 eql 的, 控制会被转移到包含这个 go 的最内部的包含这个匹配 tag 的 tagbody 表达式形式上. 如果在这个 go 的点上没有词法上可见的匹配的 tag 那么结果是未定义的.

        由 go 发起的控制转移是按照章节 5.2 (退出点的控制转移) 所描述的进行的.

* 示例(Examples):

    ```LISP
    (tagbody
      (setq val 2)
      (go lp)
      (incf val 3)
      lp (incf val 4)) =>  NIL
    val =>  6
    ```

        下面这个是错误的因为在 go 被执行前这里有一个 tagbody 正常的退出.

    ```LISP
    (let ((a nil))
      (tagbody t (setq a #'(lambda () (go t))))
      (funcall a))
    ```

        下面这个是错误的因为在 go 表达式形式[form]被执行前这个 tagbody 被跳过了.

    ```LISP
    (funcall (block nil
               (tagbody a (return #'(lambda () (go a))))))
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        tagbody

* 注意(Notes): None.


### <span id="SORETURN-FROM">特殊操作符 RETURN-FROM</span>

* 语法(Syntax):

        return-from name [result] =>|

* 参数和值(Arguments and Values):

        name---一个块标签[block tag]; 不求值.
        result---一个表达式形式[form]; 求值. 默认是 nil.

* 描述(Description):

        从一个词法上闭合的块[block]中返回控制和多值[multiple values[2]].

        一个名为 name 的 block 表达式形式[form]必须词法上包含 return-from 的出现; 结果 result 的求值[evaluation]所产生的任何值[value]都将立即从最内层的词法上闭合的块[block]中返回.

        由 return-from 发起的控制转移是按照章节 5.2 (退出点的控制转移) 所所描述的进行的.

* 示例(Examples):

    ```LISP
    (block alpha (return-from alpha) 1) =>  NIL
    (block alpha (return-from alpha 1) 2) =>  1
    (block alpha (return-from alpha (values 1 2)) 3) =>  1, 2
    (let ((a 0))
       (dotimes (i 10) (incf a) (when (oddp i) (return)))
       a) =>  2
    (defun temp (x)
       (if x (return-from temp 'dummy))
       44) =>  TEMP
    (temp nil) =>  44
    (temp t) =>  DUMMY
    (block out
      (flet ((exit (n) (return-from out n)))
        (block out (exit 1)))
      2) =>  1
    (block nil
      (unwind-protect (return-from nil 1)
        (return-from nil 2)))
    =>  2
    (dolist (flag '(nil t))
      (block nil
        (let ((x 5))
          (declare (special x))
          (unwind-protect (return-from nil)
            (print x))))
      (print 'here))
    >>  5
    >>  HERE
    >>  5
    >>  HERE
    =>  NIL
    (dolist (flag '(nil t))
      (block nil
        (let ((x 5))
          (declare (special x))
          (unwind-protect
              (if flag (return-from nil))
            (print x))))
      (print 'here))
    >>  5
    >>  HERE
    >>  5
    >>  HERE
    =>  NIL
    ```

        下面这个结果也是未定义的因为在 return-from 表达式形式[form]被尝试之前这个 block 表达式形式[form]就正常退出了.

    ```LISP
    (funcall (block nil #'(lambda () (return-from nil)))) 
    ```

        是一个错误.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        block, return, 章节 3.1 (求值)

* 注意(Notes): None.


### <span id="MRETURN">宏 RETURN</span>

* 语法(Syntax):

        return [result] =>|

* 参数和值(Arguments and Values):

        result---一个表达式形式[form]; 求值. 默认是 nil.

* 描述(Description):

        从一个名为 nil 的块[block]中返回, 就像是通过 return-from 的一样.

* 示例(Examples):

    ```LISP
    (block nil (return) 1) =>  NIL
    (block nil (return 1) 2) =>  1
    (block nil (return (values 1 2)) 3) =>  1, 2
    (block nil (block alpha (return 1) 2)) =>  1
    (block alpha (block nil (return 1)) 2) =>  2
    (block nil (block nil (return 1) 2)) =>  1
    ```

* 受此影响(Affected By): None.

* 条件(Conditions): None.

* 也见(See Also):

        block, return-from, 章节 3.1 (求值)

* 注意(Notes):

    ```LISP
    (return) ==  (return-from nil)
    (return form) ==  (return-from nil form)
    ```

        被类似于 do 这样的宏建立的这些隐式块[implicit block]经常名为 nil, 因此 return 可以被用于从这样的表达式形式[form]中返回.


### <span id="SOTAGBODY">特殊操作符 TAGBODY</span>

* 语法(Syntax):

        tagbody {tag | statement}* => nil

* 参数和值(Arguments and Values):

        tag---一个 go 标签[go tag]; 不求值.
        statement---一个复合表达式形式[compound form]; 按照以下描述求值.

* 描述(Description):

        在一个提供了对这些 tag 所指示的标签的控制转移的词法环境[lexical environment]中执行零个或多个语句 statement.

        在一个 tagbody 中的语句 statements 按照从左到右的顺序被求值, 并且它们的值[value]会被丢弃. 如果在任何没有剩余语句 statement 的时候, 那么 tagbody 返回 nil. 然而, 如果 (go tag) 被求值, 控制会跳转到这个 tag 所表示的标签主体部分. (这些标签用 eql 来比较.)

        一个通过 tagbody 所建立的 tag 有着词法作用域[lexical scope]和动态范围[dynamic extent]. 一旦 tagbody 退出, 通过 go 到一个它的主体中的 tag 就不再合法. 允许 go 去跳转到一个不是最内部的包含这个 go 的 tagbody; 由 tagbody 所建立的这些 tag 只会遮蔽其他名字一样的 tag.

        在任何这个主体的元素的宏展开[macro expansion]之前, 确定主体的哪些元素是标签 tag, 哪些是语句 statement. 如果一个语句 statement 是一个宏表达式形式[macro form]并且它的宏展开式[macro expansion]是一个原子[atom], 那么这个原子[atom]被当作是一个语句而不是一个标签.

* 示例(Examples):

    ```LISP
    (let (val)
        (tagbody
          (setq val 1)
          (go point-a)
          (incf val 16)
        point-c
          (incf val 04)
          (go point-b)
          (incf val 32)
        point-a
          (incf val 02)
          (go point-c)
          (incf val 64)
        point-b
          (incf val 08))
        val)
    =>  15
    (defun f1 (flag)
      (let ((n 1))
        (tagbody 
          (setq n (f2 flag #'(lambda () (go out))))
          out
          (prin1 n))))
    =>  F1
    (defun f2 (flag escape)
      (if flag (funcall escape) 2))
    =>  F2
    (f1 nil)
    >>  2
    =>  NIL
    (f1 t)
    >>  1
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        go

* 注意(Notes):

        下一段的这些宏[macro]中有着隐式的 tagbody [implicit tagbody].

        do              do-external-symbols  dotimes
        do*             do-symbols           prog
        do-all-symbols  dolist               prog*

        Figure 5-10. 有着隐式的 tagbody 的宏.


### <span id="SOTHROW">特殊操作符 THROW</span>

* 语法(Syntax):

        throw tag result-form =>|

* 参数和值(Arguments and Values):

        tag---一个捕捉标签[catch tag]; 求值.
        result-form---一个表达式形式[form]; 按以下方式求值.

* 描述(Description):

        throw 导致一个局部控制转移, 转移到一个标签和 tag 是 eq 的 catch 中.

        标签 tag 首先被求值来产生一个称之为抛出标签[throw tag]的对象[object]; 然后结果表达式形式 result-form 被求值, 并且它的结果被保存下来. 如果这个结果表达式形式 result-form 产生多个值, 那么所有的值会被保存. 其中的 tag 和这个抛出标签是 eq 的最新的未完成的 catch 会退出; 保存的结果会被作为这个 catch 的值或多值返回.

        throw 发起的控制转移会像章节 5.2 (退出点的控制转移) 中描述的那样被执行.

* 示例(Examples):

    ```LISP
    (catch 'result
       (setq i 0 j 0)
       (loop (incf j 3) (incf i)
             (if (= i 3) (throw 'result (values i j))))) =>  3, 9

    (catch nil
      (unwind-protect (throw nil 1)
        (throw nil 2))) =>  2
    ```

        下面这个的结果是未定义的因为 b 的 catch 被第一个 throw 跳过了, 因此, 可移植程序必须假定其动态范围[dynamic extent]是终止的. 捕获标签[catch tag]的绑定[binding]还没有被消除, 因此它是第二个 throw 的目标.

    ```LISP
    (catch 'a
      (catch 'b
        (unwind-protect (throw 'a 1)
          (throw 'b 2))))
    ```

        下面的打印 "The inner catch returns :SECOND-THROW" 然后返回 :outer-catch.

    ```LISP
    (catch 'foo
            (format t "The inner catch returns ~s.~%"
                    (catch 'foo
                        (unwind-protect (throw 'foo :first-throw)
                            (throw 'foo :second-throw))))
            :outer-catch)
    >>  The inner catch returns :SECOND-THROW
    =>  :OUTER-CATCH
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这里没有匹配抛出标签的未完成的捕捉标签[catch tag], 那么不会执行栈的解除(unwinding), 并且会发出一个 control-error 类型[type]的错误. 当这个错误被发出时, 动态环境[dynamic environment]是在 throw 点处生效的那个.

* 也见(See Also):

        block, catch, return-from, unwind-protect, 章节 3.1 (求值)

* 注意(Notes):

        通常当退出点[exit point]必须有动态作用域[dynamic scope]时使用 catch 和 throw (比如, 这个 throw 词法上没有被 catch 封闭), 而当词法作用域[lexical scope]足够时使用 block 和 return.


### <span id="SOUNWIND-PROTECT">特殊操作符 UNWIND-PROTECT</span>

* 语法(Syntax):

        unwind-protect protected-form cleanup-form* => result*

* 参数和值(Arguments and Values):

        protected-form---一个表达式形式[form].
        cleanup-form---一个表达式形式[form].
        results---这个 protected-form 的值[value].

* 描述(Description):

        unwind-protect 求值 protected-form 并且保证这个 cleanup-form 在 unwind-protect 退出前被执行, 不管它正常终止或被某种控制转移所跳过. unwind-protect 的目的是确保求值 protected-form 之后会发生某些副作用.

        如果在执行 cleanup-form 的时候发生一个非局部退出[non-local exit], 不会采取特殊动作. 这个 unwind-protect 的 cleanup-forms 不受 unwind-protect 保护.

        unwind-protect 防止所有企图退出 protected-form 的尝试, 包括 go, handler-case, ignore-errors, restart-case, return-from, throw, 还有 with-simple-restart.

        在退出时取消处理者[handler]和重启动[restart]绑定[binding], 与取消动态变量[dynamic variable]和 catch 标签绑定是并行的, 和它们建立的顺序是相反的. 这样做的效果是 cleanup-form 可以看到和进入 unwind-protect 时见到的一样的处理者[handler]和重启动[restart]绑定[binding], 以及动态变量[dynamic variable]绑定和 catch 标签.

* 示例(Examples):

    ```LISP
    (tagbody
      (let ((x 3))
        (unwind-protect
          (if (numberp x) (go out))
          (print x)))
     out
      ...)
    ```

        当 go 被执行时, 对 print 的调用首先被执行, 然后完成控制转移到标签 out.

    ```LISP
    (defun dummy-function (x)
       (setq state 'running)
       (unless (numberp x) (throw 'abort 'not-a-number))
       (setq state (1+ x))) =>  DUMMY-FUNCTION
    (catch 'abort (dummy-function 1)) =>  2
    state =>  2
    (catch 'abort (dummy-function 'trash)) =>  NOT-A-NUMBER
    state =>  RUNNING
    (catch 'abort (unwind-protect (dummy-function 'trash)
                     (setq state 'aborted))) =>  NOT-A-NUMBER
    state =>  ABORTED
    ```

        下面的代码是不对的:

    ```LISP
    (unwind-protect
      (progn (incf *access-count*)
             (perform-access))
      (decf *access-count*))
    ```
    
        如果在 incf 完成之前发生了一个退出, 这个 decf 表达式形式[form]无论如何都会被执行, 导致 *access-count* 的一个不正确的值. 正确的代码如下:

    ```LISP
    (let ((old-count *access-count*))
      (unwind-protect
        (progn (incf *access-count*)
               (perform-access))
        (setq *access-count* old-count)))

    ;;; The following returns 2.
    (block nil
      (unwind-protect (return 1)
        (return 2)))

    ;;; The following has undefined consequences.
    (block a
      (block b
        (unwind-protect (return-from a 1)
          (return-from b 2))))

    ;;; The following returns 2.
    (catch nil
      (unwind-protect (throw nil 1)
        (throw nil 2)))

    ;;; The following has undefined consequences because the catch of B is
    ;;; passed over by the first THROW, hence portable programs must assume
    ;;; its dynamic extent is terminated.  The binding of the catch tag is not
    ;;; yet disestablished and therefore it is the target of the second throw.
    (catch 'a
      (catch 'b
        (unwind-protect (throw 'a 1)
          (throw 'b 2))))

    ;;; The following prints "The inner catch returns :SECOND-THROW"
    ;;; and then returns :OUTER-CATCH.
    (catch 'foo
            (format t "The inner catch returns ~s.~%"
                    (catch 'foo
                        (unwind-protect (throw 'foo :first-throw)
                            (throw 'foo :second-throw))))
            :outer-catch)

    ;;; The following returns 10. The inner CATCH of A is passed over, but
    ;;; because that CATCH is disestablished before the THROW to A is executed,
    ;;; it isn't seen.
    (catch 'a
      (catch 'b
        (unwind-protect (1+ (catch 'a (throw 'b 1)))
          (throw 'a 10))))

    ;;; The following has undefined consequences because the extent of
    ;;; the (CATCH 'BAR ...) exit ends when the (THROW 'FOO ...)
    ;;; commences.
    (catch 'foo
      (catch 'bar
          (unwind-protect (throw 'foo 3)
            (throw 'bar 4)
            (print 'xxx))))

    ;;; The following returns 4; XXX is not printed.
    ;;; The (THROW 'FOO ...) has no effect on the scope of the BAR
    ;;; catch tag or the extent of the (CATCH 'BAR ...) exit.
    (catch 'bar
      (catch 'foo
          (unwind-protect (throw 'foo 3)
            (throw 'bar 4)
            (print 'xxx))))

    ;;; The following prints 5.
    (block nil
      (let ((x 5))
        (declare (special x))
        (unwind-protect (return)
          (print x))))
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        catch, go, handler-case, restart-case, return, return-from, throw, 章节 3.1 (求值)

* 注意(Notes): None.


### <span id="CNIL">常量 NIL</span>

* 常量值(Constant Value):

        nil.

* 描述(Description):

        nil 表示布尔值[boolean] (还有广义的 boolean [generalized boolean]) false 还有空列表[empty list].

* 示例(Examples):

    ```LISP
    nil =>  NIL
    ```

* 也见(See Also):

        t

* 注意(Notes): None.


### <span id="FNOT">函数 NOT</span>

* 语法(Syntax):

        not x => boolean

* 参数和值(Arguments and Values):

        x---一个广义的 boolean [generalized boolean] (换句话说, 任何对象[object]).

        boolean---一个布尔值[boolean].

* 描述(Description):

        如果 x 是 false 就返回 t; 否则, 返回 nil.

* 示例(Examples):

    ```LISP
    (not nil) =>  T
    (not '()) =>  T
    (not (integerp 'sss)) =>  T
    (not (integerp 1)) =>  NIL
    (not 3.7) =>  NIL
    (not 'apple) =>  NIL
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        null

* 注意(Notes):

        not 的目的是用来反转布尔值[boolean] (或者是广义的 boolean [generalized boolean]) 的"真实值(truth value)"  而 null 用于测试空列表[empty list]. 操作上, not 和 null 的计算结果是一样的; 使用哪个是一个风格问题.


### <span id="CT">常量 T</span>

* 常量值(Constant Value):

        t.

* 描述(Description):

        表示 true 的布尔值[boolean], 并且是表示 true 的标准的广义 boolean [generalized boolean]. 虽然除了 nil 以外的任何对象[object]都被当成 true, t 通常用于没有特殊理由偏爱其中一个对象而不喜欢另一个对象的情况.<!--TODO 最后半句 ？-->

        符号[symbol] t 有时也被用作其他目的. 比如, 作为一个类[class]的名字[name], 作为一个标识符[designator] (比如, 一个流标识符[stream designator]) 或者由于某些语法原因作为一个特殊符号 (比如, 在 case 和 typecase 中去表示 otherwise-clause 子句).

* 示例(Examples):

    ```LISP
    t =>  T
    (eq t 't) =>  true
    (find-class 't) =>  #<CLASS T 610703333>
    (case 'a (a 1) (t 2)) =>  1
    (case 'b (a 1) (t 2)) =>  2
    (prin1 'hello t)
    >>  HELLO
    =>  HELLO
    ```
    
* 也见(See Also):

        nil

* 注意(Notes): None.


### <span id="FEQ">函数 EQ</span>

* 语法(Syntax):

        eq x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象[object].
        y---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果它的那两个实参[argument]是一样的, 完全相同的对象[object]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (eq 'a 'b) =>  false
    (eq 'a 'a) =>  true
    (eq 3 3)
    =>  true
    OR=>  false
    (eq 3 3.0) =>  false
    (eq 3.0 3.0)
    =>  true
    OR=>  false
    (eq #c(3 -4) #c(3 -4))
    =>  true
    OR=>  false
    (eq #c(3 -4.0) #c(3 -4)) =>  false
    (eq (cons 'a 'b) (cons 'a 'c)) =>  false
    (eq (cons 'a 'b) (cons 'a 'b)) =>  false
    (eq '(a . b) '(a . b))
    =>  true
    OR=>  false
    (progn (setq x (cons 'a 'b)) (eq x x)) =>  true
    (progn (setq x '(a . b)) (eq x x)) =>  true
    (eq #\A #\A)
    =>  true
    OR=>  false
    (let ((x "Foo")) (eq x x)) =>  true
    (eq "Foo" "Foo")
    =>  true
    OR=>  false
    (eq "Foo" (copy-seq "Foo")) =>  false
    (eq "FOO" "foo") =>  false
    (eq "string-seq" (copy-seq "string-seq")) =>  false
    (let ((x 5)) (eq x x))
    =>  true
    OR=>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        eql, equal, equalp, =, 章节 3.2 (编译)

* 注意(Notes):

        在打印时显示相同的对象[object]不一定是相互之间 eq 的. 由于 intern 函数的使用, 在打印时相同的符号[symbol]通常相互之间是 eq 的. 然而, 值相同的数字[number]不需要是 eq 的, 并且两个相似的列表[list]通常也不是一样的[identical].

        一个具体实现允许在任何时间去 "拷贝" 字符[character]和数字[number]. 其结果是, 如果参数是字符[character]或数字[number], 即使它们是"相同的", Common Lisp 无法保证 eq 是 true 的.

        大部分 Common Lisp 操作符[operator]使用 eql 而不是 eq 来比较对象, 或者它们会默认用 eql, 只在特定要求下使用 eq. 然而, 下面的操作符[operator]被定义为使用 eq 而不是 eql , 它不能被使用它们的代码所覆盖:

        catch           getf     throw
        get             remf
        get-properties  remprop

        Figure 5-11. 总是使用 EQ 而不是 EQL 的操作符


### <span id="FEQL">函数 EQL</span>

* 语法(Syntax):

        eql x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象[object].
        y---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        两个对象, x 和 y, 在下面情况下 eql 的值是 true:

        1. 如果 x 和 y 是 eq 的.
        2. 如果 x 和 y 都是相同类型[type]的数字[number]并且值相同.
        3. 如果它们都是表示相同字符的字符[character].

        否则 eql 的值就是 false.

        如果一个具体实现支持正负零是不同[distinct]的值, 那么 (eql 0.0 -0.0) 返回 false. 否则, 当语法 -0.0 被读取时会被解释为值 0.0, 所以 (eql 0.0 -0.0) 返回 true.

* 示例(Examples):

    ```LISP
    (eql 'a 'b) =>  false
    (eql 'a 'a) =>  true
    (eql 3 3) =>  true
    (eql 3 3.0) =>  false
    (eql 3.0 3.0) =>  true
    (eql #c(3 -4) #c(3 -4)) =>  true
    (eql #c(3 -4.0) #c(3 -4)) =>  false
    (eql (cons 'a 'b) (cons 'a 'c)) =>  false
    (eql (cons 'a 'b) (cons 'a 'b)) =>  false
    (eql '(a . b) '(a . b))
    =>  true
    OR=>  false
    (progn (setq x (cons 'a 'b)) (eql x x)) =>  true
    (progn (setq x '(a . b)) (eql x x)) =>  true
    (eql #\A #\A) =>  true
    (eql "Foo" "Foo")
    =>  true
    OR=>  false
    (eql "Foo" (copy-seq "Foo")) =>  false
    (eql "FOO" "foo") =>  false
    ```
    
        通常 (eql 1.0s0 1.0d0) 是 false, 在 1.0s0 和 1.0d0 是不同的数据类型的设定下. 然而, 不提供 4 种不同浮点格式的具体实现允许将这四种格式"折叠"成更小的数字格式; 在这样一个实现中 (eql 1.0s0 1.0d0) 可能是 true.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        eq, equal, equalp, =, char=

* 注意(Notes):

        eql 和 eq 是一样的, 除了当参数是字符[character]或相同类型的数字[number]时它们的值会被比较. 因此 eql 讲述两个对象[object]是否在概念上相等, 而 eq 讲述两个对象[object]是否在实现上相等. 这也就是 eql, 而不是 eq, 是序列[sequence]作为参数的运算符[operator]的默认比较谓词的原因.

        eql 对于两个表示相同值的浮点数[float]可能不是 true. = 被用于比较算数值.

        两个复数[complex]如果它们的实部和虚部都是 eql 的就认为它们是 eql 的. 比如, (eql #C(4 5) #C(4 5)) 是 true 而 (eql #C(4 5) #C(4.0 5.0)) 是 false. 注意 (eql #C(5.0 0.0) 5.0) 是 false, 而 (eql #C(5 0) 5) 是 true. 在 (eql #C(5.0 0.0) 5.0) 情况下两个参数是不同类型, 因此不能满足 eql. 在 (eql #C(5 0) 5) 情况下, #C(5 0) 不是一个复数[complex], 但是被自动归约为整数[integer] 5.


### <span id="FEQUAL">函数 EQUAL</span>

* 语法(Syntax):

        equal x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象[object].
        y---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果 x 和 y 是结构上类似(同构)的对象[object], 则返回 true. 这些对象[object]按照以下类别被 equal 处理.

    * 符号[symbol], 数字[number], 和字符[character]

            两个对象[object]如果它们是相互 eq 的符号[symbol], 或者是相互 eql 的数字[number], 或者相互 eql 的字符[character], 那么它们的 equal 就是 true.

    * 构造[cons]

            对于 cons, equal 按照递归定义, 如果两个 car 是 equal 的并且两个 cdr 也是 equal 的那么就是 equal 的.

    * 数组[array]

            两个数组[array]只有当它们是 eq 的情况下才会 equal, 除了一个例外: 字符串[string]和位向量[bit vector]按照元素对元素一一比较 (使用 eql). 如果 x 或 y 有一个填充指针[fill pointer], 这个填充指针[fill pointer]限制被 equal 检查元素的数量. 字符串[string]中的大写字母和小写字母被 equal 认为是不同的.

    * 路径名[pathname]

            两个路径名[pathname]当且仅当所有对应的成分 (host, device, 等等) 相等的时候才是 equal 的. 大写和小写字母在组件的字符串[string]中是否被认为是等价的是依赖于具体实现的[implementation-dependent]. 多个 equal 的路径名[pathname]应该在功能上是等同的.

    * 其他 (结构体, 哈希表, 实例, ...)

            只有当两个其他对象[object]是 eq 的情况下才会 equal.

        equal 不会下降(descend)任何对象[object], 除了上面显式指定的那些. 下面这段总结了前面的列表中的信息. 另外, 这段中指明了 equal 行为的优先级, 上面的条目优先于下面的条目.

    |类型       |    行为    |
    |--        | --         |
    |数字[number]    |    使用 eql|
    |字符[character] |    使用 eql|
    |构造[cons]      |    下降(descend)|
    |位向量[bit vector]|    下降(descend)|
    |字符串[string]    |    下降(descend)|
    |路径名[pathname]  |    "功能上等价(functionally equivalent)"|
    |结构体[structure] |    使用 eq|
    |其他数组[array]|   使用 eq|
    |哈希表[hash table] |   使用 eq|
    |其他对象[object]|  使用 eq|

    Figure 5-12. equal 行为的概要和优先级

        任何两个 eql 的对象[object]也是 equal 的.

        如果 x 和 y 是环状的 equal 可能不会终止.

* 示例(Examples):

    ```LISP
    (equal 'a 'b) =>  false
    (equal 'a 'a) =>  true
    (equal 3 3) =>  true
    (equal 3 3.0) =>  false
    (equal 3.0 3.0) =>  true
    (equal #c(3 -4) #c(3 -4)) =>  true
    (equal #c(3 -4.0) #c(3 -4)) =>  false
    (equal (cons 'a 'b) (cons 'a 'c)) =>  false
    (equal (cons 'a 'b) (cons 'a 'b)) =>  true
    (equal #\A #\A) =>  true
    (equal #\A #\a) =>  false
    (equal "Foo" "Foo") =>  true
    (equal "Foo" (copy-seq "Foo")) =>  true
    (equal "FOO" "foo") =>  false
    (equal "This-string" "This-string") =>  true
    (equal "This-string" "this-string") =>  false
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        eq, eql, equalp, =, string=, string-equal, char=, char-equal, tree-equal

* 注意(Notes):

        对象[object]相等性不是一个只有唯一确定的正确算法的概念. 等价谓词的适当性只能在某些特定程序的需求上下文中进行判断. 虽然这些函数接受任何类型的参数并且它们的名字听起来很通用, 但是 equal 和 equalp 不适用于每一个应用.

        一个粗略的经验法则是两个对象当且仅当它们的打印表示是相同的时候是 equal 的.


### <span id="FEQUALP">函数 EQUALP</span>

* 语法(Syntax):

        equalp x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象[object].
        y---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

    如果 x 和 y 是 equal 的就返回 true, 或者如果它们有着相同类型[type]的组件并且那些组件也是 equalp 的就返回 true; 具体来说, equalp 在以下情况返回 true:

    * 字符[character]

            如果两个字符[character]是 char-equal 的.

    * 数字[number]

            如果两个数字[number]在 = 下是相同的[same].

    * 构造[cons]

            如果这两个 cons 中的 car 是 equalp 的并且 cdr 也是 equalp 的.

    * 数组[array]

            如果两个数组[array]有着相同的维度, 维度是匹配的, 并且对应有效[active]元素[element]是 equalp 的. 数组[array]特化[specialized]的类型[type]不需要匹配; 比如, 一个字符串[string]和一个包含相同字符[character]的普通的数组[array]是 equalp 的. 由于 equalp 执行字符串[string]元素对元素的比较并且忽略字符[character]的大小写[case], 当使用 equalp 比较字符串[string]时大小写[case]差别是被忽略的.

    * 结构体[structure]

            如果两个结构体[structure] S1 和 S2 有着相同的类[class]并且 S1 中每一个槽[slot]的值和 S2 中对应槽[slot]的值在 equalp 下是相同的[same].

    * 哈希表[hash table]

            equalp 首先通过判断元素的数量和这个 :test 函数来递减(descend) hash-tables; 如果这些是一样的, 它用 :test 函数来比较这个哈希表的 key 然后匹配的 key 对应的 value 也递归地使用 equalp.

    equalp 不会下降(descend)任何对象[object], 除了上面显式指定的那些. 下一段中总结了上面列表中给定的信息. 另外, 这段中指明了 equalp 行为的优先级, 上面的条目优先于下面的条目.

    |类型       |    行为 |
    | -- | -- |
    |数字[number]     |   使用 =|
    |字符[character]  |   使用 char-equal|
    |构造[cons]       |   descends|
    |位向量[bit vector] |   descends|
    |字符串[string]     |   descends|
    |路径名[pathname]   |   same as equal|
    |结构体[structure]  |   descends, as described above|
    |其他数组[array]|   descends|
    |哈希表[hash table] |   descends, as described above|
    |其他对象[object]|  使用 eq|

    Figure 5-13. equalp行为的优先级和总结

* 示例(Examples):

    ```LISP
    (equalp 'a 'b) =>  false
    (equalp 'a 'a) =>  true
    (equalp 3 3) =>  true
    (equalp 3 3.0) =>  true
    (equalp 3.0 3.0) =>  true
    (equalp #c(3 -4) #c(3 -4)) =>  true
    (equalp #c(3 -4.0) #c(3 -4)) =>  true
    (equalp (cons 'a 'b) (cons 'a 'c)) =>  false
    (equalp (cons 'a 'b) (cons 'a 'b)) =>  true
    (equalp #\A #\A) =>  true
    (equalp #\A #\a) =>  true
    (equalp "Foo" "Foo") =>  true
    (equalp "Foo" (copy-seq "Foo")) =>  true
    (equalp "FOO" "foo") =>  true
  
    (setq array1 (make-array 6 :element-type 'integer
                               :initial-contents '(1 1 1 3 5 7)))
    =>  #(1 1 1 3 5 7)
    (setq array2 (make-array 8 :element-type 'integer
                               :initial-contents '(1 1 1 3 5 7 2 6)
                               :fill-pointer 6))
    =>  #(1 1 1 3 5 7)
    (equalp array1 array2) =>  true
    (setq vector1 (vector 1 1 1 3 5 7)) =>  #(1 1 1 3 5 7)
    (equalp array1 vector1) =>  true
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        eq, eql, equal, =, string=, string-equal, char=, char-equal

* 注意(Notes):

        对象[object]相等性不是一个只有唯一确定的正确算法的概念. 等价谓词的适当性只能在某些特定程序的需求上下文中进行判断. 虽然这些函数接受任何类型的参数并且它们的名字听起来很通用, 但是 equal 和 equalp 不适用于每一个应用.


### <span id="FIDENTITY">函数 IDENTITY</span>

* 语法(Syntax):

        identity object => object

* 参数和值(Arguments and Values):

        object---一个对象[object].

* 描述(Description):

        返回它的参数对象 object.

* 示例(Examples):

    ```LISP
    (identity 101) =>  101
    (mapcan #'identity (list (list 1 2 3) '(4 5 6))) =>  (1 2 3 4 5 6)
    ```
 
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        identity 被用于需要一个函数作为参数的函数.

        (eql x (identity x)) 对于所有 x 的可能的值返回 true, 但是 (eq x (identity x)) 当 x 是一个数字[number]或者字符[character]时可能返回 false.

        identity 可以通过以下定义

        (defun identity (x) x)


### <span id="FCOMPLEMENT">函数 COMPLEMENT</span>

* 语法(Syntax):

        complement function => complement-function

* 参数和值(Arguments and Values):

        function---一个函数[function].
        complement-function---一个函数[function].

* 描述(Description):

        返回一个和函数 function 接受相同实参[argument]并且和 function 有着相同副作用但是只返回单个值的函数[function], 返回的单个值为一个广义的 boolean [generalized boolean], 表示 function 返回的主值[primary value]的相反值. 这也就是说, 当这个 function 会返回 true 作为主值[primary value]时 complement-function 返回 false, 而当这个 function 返回 false 作为主值[primary value]时, complement-function 返回 true.

* 示例(Examples):

    ```LISP
    (funcall (complement #'zerop) 1) =>  true
    (funcall (complement #'characterp) #\A) =>  false
    (funcall (complement #'member) 'a '(a b c)) =>  false
    (funcall (complement #'member) 'd '(a b c)) =>  true
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        not

* 注意(Notes):

        (complement x) ==  #'(lambda (&rest arguments) (not (apply x arguments)))

        在 Common Lisp 中, 带有像 "xxx-if-not" 名字的函数和有着类似 "xxx-if" 名字的函数之间的关系如下

        (xxx-if-not f . arguments) ==  (xxx-if (complement f) . arguments)

        比如,

        (find-if-not #'zerop '(0 0 3)) ==
        (find-if (complement #'zerop) '(0 0 3)) =>  3

        注意由于这些 "xxx-if-not" 函数[function]和 :test-not 参数已经被废弃, 首选是和 complement 一起使用 "xxx-if" 函数[function]或  :test 参数.


### <span id="FCONSTANTLY">函数 CONSTANTLY</span>

* 语法(Syntax):

        constantly value => function

* 参数和值(Arguments and Values):

        value---一个对象[object].
        function---一个函数[function].

* 描述(Description):

        constantly 返回一个接受任何数量参数的函数[function], 没有副作用, 并且总是返回 value.

* 示例(Examples):

    ```LISP
    (mapcar (constantly 3) '(a b c d)) =>  (3 3 3 3)
    (defmacro with-vars (vars &body forms)
      `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars)))
    =>  WITH-VARS
    (macroexpand '(with-vars (a b) (setq a 3 b (* a a)) (list a b)))
    =>  ((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL), true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        identity

* 注意(Notes):

        constantly 可以通过以下方式来定义:

    ```LISP
    (defun constantly (object)
      #'(lambda (&rest arguments) object))
    ```


### <span id="FEVERYSOMENOTEVERYNOTANY">函数 EVERY, SOME, NOTEVERY, NOTANY</span>

* 语法(Syntax):

        every predicate &rest sequences+ => generalized-boolean
        some predicate &rest sequences+ => result
        notevery predicate &rest sequences+ => generalized-boolean
        notany predicate &rest sequences+ => generalized-boolean

* 参数和值(Arguments and Values):

        predicate---一个实参[argument]数量多达这些序列 sequences 数量的函数[function]标识符[designator].
        sequence---一个序列[sequence].
        result---一个对象[object].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        every, some, notevery, 还有 notany 测试这些序列 sequences 的元素[element]对于这个给定的断言 predicate 的满足情况. 给 predicate 的第一个参数是第一个序列 sequence 的元素[element]; 每一个后续的参数都是后续序列 sequence 的元素[element].

        这个断言 predicate 首先应用到这些序列 sequences 的每一个索引为 0 的元素, 然后可能是索引为 1 的, 以此类推, 直到遇到一个终止准则或者到达最短序列的末尾.

        当任何一个 predicate 的调用返回 false 的时候 every 返回 false. 如果到达一个 sequence 的末尾, every 返回 true. 因此, every 当且仅当所有 predicate 返回 true 的时候返回 true.

        some 返回由一个 predicate 调用返回的第一个非 nil [non-nil]的值. 如果到达一个 sequence 的末尾还没有任何 predicate 的调用返回 true, some 返回 false. 因此, some 当且仅当某个 predicate 调用返回 true 的时候返回 true.

        当任何一个 predicate 调用返回 true 时 notany 返回 false. 如果到达一个 sequence 的末尾, notany 返回 true. 因此, notany 当且仅当任何 predicate 调用都不返回 true 的时候返回 true.

        当任何一个 predicate 调用返回 false 时 notevery 返回 true. 如果到达一个 sequence 的末尾, notevery 返回 false. 因此, notevery 当且仅当不是每一个 predicate 都返回 true 时返回 true.

* 示例(Examples):

    ```LISP
    (every #'characterp "abc") =>  true
    (some #'= '(1 2 3 4 5) '(5 4 3 2 1)) =>  true
    (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  false
    (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的第一个参数既不是符号[symbol]也不是一个函数[function]或者如果任何后续的参数都不是一个正常序列[proper sequence], 那么应该发出一个 type-error 类型的错误.

        其他异常情况也是有可能的, 取决于 predicate 的性质.

* 也见(See Also):

        and, or, 章节 3.6 (遍历规则和副作用)

* 注意(Notes):

        (notany predicate sequence*) ==  (not (some predicate sequence*))
        (notevery predicate sequence*) ==  (not (every predicate sequence*))


### <span id="MAND">宏 AND</span>

* 语法(Syntax):

        and form* => result*

* 参数和值(Arguments and Values):

        form---一个表达式形式[form].
        results---最后一个表达式形式 form 求值产生的值[value], 或者符号 nil 或 t.

* 描述(Description):

        宏 and 对每一个表达式形式 form 从左到右的顺序求值一次. 当任何表达式形式 form 求值为 nil 时, and 在不求值剩余表达式形式 forms 的情况下返回 nil. 如果除了最后一个以外的所有表达式形式 forms 都被求值为 true, 那么 and 就返回最后一个表达式形式 form 求值产生的结果.

        如果没有提供这些表达式形式 forms, (and) 就返回 t.

        and 传递回来自最后一个子表达式形式[subform]的多值, 但是不会传递来自除了最后一个以外的其他的子表达式形式[subform]的多值.

* 示例(Examples):

    ```LISP
    (if (and (>= n 0)
             (< n (length a-simple-vector))
             (eq (elt a-simple-vector n) 'foo))
        (princ "Foo!"))
    ```
    
        假如 n 事实上是 a-simple-vector 的一个合法的索引, 如果 a-simple-vector 中的元素 n 是符号 foo, 上面表达式就打印 Foo!. 由于 and 保证从左到右测试它的各个部分, 如果 n 超出范围那么 elt 不会被调用.

    ```LISP
    (setq temp1 1 temp2 1 temp3 1) =>  1
    (and (incf temp1) (incf temp2) (incf temp3)) =>  2
    (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) =>  true
    (decf temp3) =>  1
    (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3)) =>  NIL
    (and (eql temp1 temp2) (eql temp2 temp3)) =>  true
    (and) =>  T
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cond, every, if, or, when

* 注意(Notes):

        (and form) ==  (let () form)
        (and form1 form2 ...) ==  (when form1 (and form2 ...))

### <span id="MCOND">宏 COND</span>

* 语法(Syntax):

        cond {clause}* => result*

        clause::= (test-form form*)

* 参数和值(Arguments and Values):

        test-form---一个表达式形式[form].
        forms---一个隐式的 progn [implicit progn].
        results---第一个产生[yield] true 的 test-form 的子句 clause 的值[value], 如果在这样的子句 clause 中没有表达式形式 form 那么就是 test-form 的主值[primary value], 如果没有 test-form 产生[yield] true 就返回 nil.

* 描述(Description):

        cond 允许这些表达式形式 forms 的执行依赖于 test-form.

        test-form 按它们出现在参数列表中的顺序一次求值一个, 直到有一个 test-form 求值为 true.

        如果在这样一个子句 clause 中没有表达式形式[form], 那么 test-form 的主值[primary value]被 cond 表达式形式[form]返回. 否则, 和这个 test-form 关联的那些表达式形式 forms 按顺序求值, 从左到右, 像一个隐式的 progn [implicit progn], 并且最后一个表达式形式 form 返回的值[value]被 cond 表达式形式[form]返回.

        一旦一个 test-form 已经产生 true, 不会有其他的 test-forms 被求值. 如果没有 test-form 产生[yield] true, 那么就返回 nil.

* 示例(Examples):

    ```LISP
    (defun select-options ()
      (cond ((= a 1) (setq a 2))
            ((= a 2) (setq a 3))
            ((and (= a 3) (floor a 2)))
            (t (floor a 3)))) =>  SELECT-OPTIONS
    (setq a 1) =>  1
    (select-options) =>  2
    a =>  2
    (select-options) =>  3
    a =>  3
    (select-options) =>  1
    (setq a 5) =>  5
    (select-options) =>  1, 2
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        if, case.

* 注意(Notes): None.


### <span id="SOIF">特殊操作符 IF</span>

* 语法(Syntax):

        if test-form then-form [else-form] => result*

* 参数和值(Arguments and Values):

        Test-form---一个表达式形式[form].
        Then-form---一个表达式形式[form].
        Else-form---一个表达式形式[form]. 默认是 nil.
        results---如果这个 test-form 产生[yield] true, 那么就是 then-form 返回的值[value]; 否则, 就是 else-form 返回的值[value].

* 描述(Description):

        if 允许一个表达式形式[form]的执行取决于一个单独的 test-form.

        首先 test-form 被求值. 如果结果是 true, 那么 then-form 被选择; 否则 else-form 被选择. 不管哪个 form 被选择, 都会在接下来被求值.

* 示例(Examples):

    ```LISP
    (if t 1) =>  1
    (if nil 1 2) =>  2
    (defun test ()
      (dolist (truth-value '(t nil 1 (a b c)))
        (if truth-value (print 'true) (print 'false))
        (prin1 truth-value))) =>  TEST
    (test)
    >>  TRUE T
    >>  FALSE NIL
    >>  TRUE 1
    >>  TRUE (A B C)
    =>  NIL
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cond, unless, when

* 注意(Notes):

        (if test-form then-form else-form)
        ==  (cond (test-form then-form) (t else-form))


### <span id="MOR">宏 OR</span>

* 语法(Syntax):

        or form* => results*

* 参数和值(Arguments and Values):

        form---一个表达式形式[form].

        results---最后一个被执行的表达式形式 form 的求值所产生的值[value]或主值[primary value] (见下方) 或者是 nil.

* 描述(Description):

        or 求值每个表达式形式 form, 一次一个, 从左到右. 当一个表达式形式 form 求值为 true (换句话说, 某个不是 nil 的结果) 则所有表达式形式 forms 的求值结束.

        如果除了最后一个以外的任何表达式形式 form 的求值[evaluation]都返回 true 的主值[primary value], 那么 or 在不求值剩下那些表达式形式 forms 的情况下立即返回这个值[value] (但是没有其他额外的值[value]). 如果除了最后一个以外每个表达式形式 form 都返回 false 作为主值[primary value], or 返回最后一个表达式形式 form 的所有值[value]. 如果没有提供表达式形式 forms, or 返回 nil.

* 示例(Examples):

    ```LISP
    (or) =>  NIL
    (setq temp0 nil temp1 10 temp2 20 temp3 30) =>  30
    (or temp0 temp1 (setq temp2 37)) =>  10
    temp2 =>  20
    (or (incf temp1) (incf temp2) (incf temp3)) =>  11
    temp1 =>  11
    temp2 =>  20
    temp3 =>  30
    (or (values) temp1) =>  11
    (or (values temp1 temp2) temp3) =>  11
    (or temp0 (values temp1 temp2)) =>  11, 20
    (or (values temp0 temp1) (values temp2 temp3)) =>  20, 30
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        and, some, unless

* 注意(Notes): None.


### <span id="MWHENUNLESS">宏 WHEN, UNLESS</span>

* 语法(Syntax):

        when test-form form* => result*
        unless test-form form* => result*

* 参数和值(Arguments and Values):

        test-form---一个表达式形式[form].
        forms---一个隐式的 progn [implicit progn].
        results---如果 when 表达式形式[form]中的 test-form 产生[yield] true 那么就是一个 when 表达式形式[form]中的那些表达式形式 forms 的值[value], 如果 unless 表达式形式[form]中的 test-form 产生[yield] false 那么就是 unless 表达式形式[form]中的那些表达式形式 forms 的值[value]; 否则就是 nil.

* 描述(Description):

        when 和 unless 允许这些表达式形式 forms 的求值取决于单独的 test-form.

        在一个 when 表达式形式[form]中, 如果这个 test-form 产生[yield] true, 这些表达式形式 forms 按从左到右的顺序求值并从 when 表达式形式[form]中返回这些表达式形式 forms 所返回的值[value]. 否则, 如果 test-form 产生 false, 这些表达式形式 forms 不会被求值, 然后这个 when 表达式形式[form]返回 nil.

        在一个 unless 表达式形式[form]中, 如果这个 test-form 产生[yield] false, 这些表达式形式 forms 按从左到右的顺序求值并从 unless 表达式形式[form]中返回 forms 所返回的值[value]. 否则, 如果这个 test-form 产生[yield] false, 这些表达式形式 forms 不会被求值, 并且这个 unless 表达式形式[form]返回 nil.

* 示例(Examples):

    ```LISP
    (when t 'hello) =>  HELLO
    (unless t 'hello) =>  NIL
    (when nil 'hello) =>  NIL
    (unless nil 'hello) =>  HELLO
    (when t) =>  NIL
    (unless nil) =>  NIL
    (when t (prin1 1) (prin1 2) (prin1 3))
    >>  123
    =>  3
    (unless t (prin1 1) (prin1 2) (prin1 3)) =>  NIL
    (when nil (prin1 1) (prin1 2) (prin1 3)) =>  NIL
    (unless nil (prin1 1) (prin1 2) (prin1 3))
    >>  123
    =>  3
    (let ((x 3))
      (list (when (oddp x) (incf x) (list x))
            (when (oddp x) (incf x) (list x))
            (unless (oddp x) (incf x) (list x))
            (unless (oddp x) (incf x) (list x))
            (if (oddp x) (incf x) (list x))
            (if (oddp x) (incf x) (list x))
            (if (not (oddp x)) (incf x) (list x))
            (if (not (oddp x)) (incf x) (list x))))
    =>  ((4) NIL (5) NIL 6 (6) 7 (7))
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        and, cond, if, or

* 注意(Notes):

    ```LISP
    (when test {form}+) ==  (and test (progn {form}+))
    (when test {form}+) ==  (cond (test {form}+))
    (when test {form}+) ==  (if test (progn {form}+) nil)
    (when test {form}+) ==  (unless (not test) {form}+)
    (unless test {form}+) ==  (cond ((not test) {form}+))
    (unless test {form}+) ==  (if test nil (progn {form}+))
    (unless test {form}+) ==  (when (not test) {form}+)
    ```

### <span id="MCASECCASEECASE">宏 CASE, CCASE, ECASE</span>

* 语法(Syntax):

        case keyform {normal-clause}* [otherwise-clause] => result*
        ccase keyplace {normal-clause}* => result*
        ecase keyform {normal-clause}* => result*
        normal-clause::= (keys form*)
        otherwise-clause::= ({otherwise | t} form*)
        clause::= normal-clause | otherwise-clause

* 参数和值(Arguments and Values):

        keyform---一个表达式形式[form]; 求值后产生一个 test-key.
        keyplace---一个表达式形式[form]; 求值后产生一个 test-key. 如果没有 key 匹配那么可能被用作一个位置[place].
        test-key---通过求值 keyform 或 keyplace 所产生的一个对象.
        keys---一个对象[object]列表[list]的标识符[designator]. 在 case 的情况下, 符号[symbol] t 和 otherwise 可能不被用作 keys 标识符[designator]. 为了将这些符号[symbol]自身作为 keys 来引用, 必须分别使用标识符[designator] (t) 和 (otherwise), .
        forms---一个隐式的 progn [implicit progn].
        results---匹配的子句 clause 中的表达式形式 form 所返回的值[value].

* 描述(Description):

        这些宏[macro]允许一个子句 clause 中的那些表达式形式 forms 主体被条件执行, 这个子句通过在它的标识上匹配 test-key 来选择.

        这个 keyform 或者 keyplace 被求值用于产生 test-key.

        每一个 normal-clause 依次被考虑. 如果 test-key 和任何子句 clause 中的键[key]是相同的[same], 那么这个子句 clause 中的那些表达式形式 forms 作为一个隐式的 progn [implicit progn]来求值, 然后它返回的值[value]会被 case, ccase, 或 ecase 表达式形式[form]作为结果返回.

        这些宏[macro]只在没有匹配到 normal-clause 时, 会有不同的行为; 具体来说:

    * case

            如果没有匹配到 normal-clause, 并且这里有一个 otherwise-clause, 那么这个 otherwise-clause 自动匹配; 那个子句 clause 中的那些表达式形式 forms 会作为一个隐式的 progn [implicit progn]被求值, 并且它返回的值[value]作为 case 的结果被返回.

            如果这里没有 otherwise-clause, case 返回 nil.

    * ccase

            如果没有 normal-clause 匹配, 一个 type-error 类型[type]的可校正[correctable]的错误[error]会被发出. 这个违背的基准是 test-key 和期望的类型是类型等价[type equivalent]于 (member key1 key2 ...). 这个 store-value 重启动[restart]可以被用于校正这个错误.

            如果这个 store-value 重启动[restart]被调用, 它的实参[argument]会成为新的 test-key, 并且被存储在 keyplace 就像是通过 (setf keyplace test-key) 一样. 然后 ccase 重新开始, 再考虑每一个子句 clause.

            如果没有情况支持, 这个 keyplace 的那些子表达式形式可能被再次求值.

    * ecase

            如果没有 normal-clause 匹配, 一个 type-error 类型[type]的不可校正[non-correctable]的错误[error]会被发出. 违背的基准是 test-key 和期望的类型是类型等价[type equivalent]于 (member key1 key2 ...).

            注意, 与 ccase 相比, 对 ecase 的调用可能依赖于如果没有 normal-clause 匹配的情况下那么 ecase 不返回的事实.

* 示例(Examples):

    ```LISP
    (dolist (k '(1 2 3 :four #\v () t 'other))
       (format t "~S "
          (case k ((1 2) 'clause1)
                  (3 'clause2)
                  (nil 'no-keys-so-never-seen)
                  ((nil) 'nilslot)
                  ((:four #\v) 'clause4)
                  ((t) 'tslot)
                  (otherwise 'others))))
    >>  CLAUSE1 CLAUSE1 CLAUSE2 CLAUSE4 CLAUSE4 NILSLOT TSLOT OTHERS
    =>  NIL
    (defun add-em (x) (apply #'+ (mapcar #'decode x)))
    =>  ADD-EM
    (defun decode (x)
      (ccase x
        ((i uno) 1)
        ((ii dos) 2)
        ((iii tres) 3)
        ((iv cuatro) 4)))
    =>  DECODE
    (add-em '(uno iii)) =>  4
    (add-em '(uno iiii))
    >>  Error: The value of X, IIII, is not I, UNO, II, DOS, III,
    >>         TRES, IV, or CUATRO.
    >>   1: Supply a value to use instead.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> :CONTINUE 1
    >>  Value to evaluate and use for X: 'IV
    =>  5
    ```

* 副作用(Side Effects):

        可能进入调试器(debugger). 如果调用了 store-value 重启动[restart], 这个 keyplace 的值[value]可能被改变.

* 受此影响(Affected By):

        ccase 和 ecase, 因为它们可能发出一个错误, 所以受存在的那些处理者 handlers 和 *debug-io* 的潜在影响.

* 异常情况(Exceptional Situations):

        如果没有匹配到 normal-clause 那么 ccase 和 ecase 发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        cond, typecase, setf, 章节 5.1 (广义引用)

* 注意(Notes):

        (case test-key
          {((key*) form*)}*)
        ==
        (let ((#1=#:g0001 test-key))
          (cond {((member #1# '(key*)) form*)}*))

        ecase 和 ccase 所使用的具体错误信息在不同的具体实现之间可以是不同的. 在错误消息的特定措辞的控制是很重要的情况下, 最好使用 case 与 otherwise-clause 来显式地用适当的消息发出错误消息.


### <span id="MTYPECASECTYPECASEETYPECASE">宏 TYPECASE, CTYPECASE, ETYPECASE</span>

* 语法(Syntax):

        typecase keyform {normal-clause}* [otherwise-clause] => result*
        ctypecase keyplace {normal-clause}* => result*
        etypecase keyform {normal-clause}* => result*
        normal-clause::= (type form*)
        otherwise-clause::= ({otherwise | t} form*)
        clause::= normal-clause | otherwise-clause

* 参数和值(Arguments and Values):

        keyform---一个表达式形式[form]; 求值后产生一个 test-key.
        keyplace---一个表达式形式[form]; 求值后产生一个 test-key. 如果没有类型匹配的时候也可能被用作一个位置[place].
        test-key---求值 keyform 或 keyplace 产生的对象.
        type---一个类型指定符[type specifier].
        forms---一个隐式的 progn [implicit progn].
        results---匹配的子句 clause 中的那些表达式形式 forms 所返回的值[value].

* 描述(Description):

        这些宏[macro]允许有条件地执行一个子句 clause 中的主体表达式形式 forms, 这个子句 clause 是通过在 test-key 上匹配它的类型[type]来选择的.

        这里的 keyform 或 keyplace 求值后产生 test-key.

        每一个 normal-clauses 都会被依次考虑. 如果 test-key 是这些子句 clauses 的类型所给定的类型[type], 那么这个子句 clause 中的这些表达式形式 forms 作为一个隐式的 progn [implicit progn]被执行, 并且它返回的值作为 typecase, ctypecase, 或 etypecase 表达式形式[form]的值被返回.

        这些宏[macro]的行为只有在没有匹配的 normal-clause 情况下有所区别; 具体来说:

    * typecase

            如果没有 normal-clause 匹配, 并且这里有一个 otherwise-clause, 那么这个 otherwise-clause 自动匹配; 这个子句 clause 中的那些表达式形式 forms 作为一个隐式的 progn [implicit progn]被执行, 并且它返回的值[value]作为 typecase 的值被返回.

            如果这里没有 otherwise-clause, typecase 返回 nil.

    * ctypecase

            如果没有 normal-clause 匹配, 一个 type-error 类型[type]的可校正[correctable]的错误[error]会被发出. 违背的基准是 test-key 和期望的类型是和 (or type1 type2 ...) 类型等价[type equivalent]. 这个 store-value 重启动[restart]可以被用于校正这个错误.

            如果调用了 store-value 重启动[restart], 它的实参[argument]会成为新的 test-key, 然后被存储在 keyplace 就好像是通过 (setf keyplace test-key) 一样. 然后 ctypecase 重新启动, 再次考虑每一个 clause.

            如果这个 store-value 重启动[restart]被交互式地调用了, 那么提示用户去使用一个新的 test-key.

            如果没有情况支持, 这个 keyplace 的子表达式形式可能被再次求值.

    * etypecase

            如果没有匹配的 normal-clause, 一个不可校正[non-correctable]的 type-error 类型[type]的错误[error]会被发出. 违背的基准是 test-key 以及期望的类型和 (or type1 type2 ...) 类型等价[type equivalent].

            注意, 相比于 ctypecase, 对 etypecase 的调用可能依赖于如果没有匹配的 normal-clause 那么 etypecase 不返回的事实.

    在所有这三种情况下, 允许超过一个子句 clause 去指定匹配类型[type], 特别是在一个已经是另一个的子类型[subtype]的时候; 最早可应用的 clause 会被选择.

* 示例(Examples):

    ```LISP
    ;;; (Note that the parts of this example which use TYPE-OF
    ;;;  are implementation-dependent.)
    (defun what-is-it (x)
      (format t "~&~S is ~A.~%"
              x (typecase x
                  (float "a float")
                  (null "a symbol, boolean false, or the empty list")
                  (list "a list")
                  (t (format nil "a(n) ~(~A~)" (type-of x))))))
    =>  WHAT-IS-IT
    (map 'nil #'what-is-it '(nil (a b) 7.0 7 box))
    >>  NIL is a symbol, boolean false, or the empty list.
    >>  (A B) is a list.
    >>  7.0 is a float.
    >>  7 is a(n) integer.
    >>  BOX is a(n) symbol.
    =>  NIL
    (setq x 1/3)
    =>  1/3
    (ctypecase x
        (integer (* x 4))
        (symbol  (symbol-value x)))
    >>  Error: The value of X, 1/3, is neither an integer nor a symbol.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> :CONTINUE 1
    >>  Use value: 3.7
    >>  Error: The value of X, 3.7, is neither an integer nor a symbol.
    >>  To continue, type :CONTINUE followed by an option number:
    >>   1: Specify a value to use instead.
    >>   2: Return to Lisp Toplevel.
    >>  Debug> :CONTINUE 1
    >>  Use value: 12
    =>  48
    x =>  12
    ```

* 受此影响(Affected By):

        ctypecase 和 etypecase, 因为它们可能发出一个错误, 所以被存在的那些处理者 handlers 和 *debug-io* 潜在地影响.

* 异常情况(Exceptional Situations):

        如果没有 normal-clause 匹配那么 ctypecase 和 etypecase 发出一个 type-error 类型[type]的错误.

        如果一个 clause 被更早子句所遮蔽导致不会被选择到, 编译器[compiler]可能会选择去发出一个 style-warning 类型[type]的警告.

* 也见(See Also):

        case, cond, setf, 章节 5.1 (广义引用)

* 注意(Notes):

        (typecase test-key
          {(type form*)}*)
        ==
        (let ((#1=#:g0001 test-key))
          (cond {((typep #1# 'type) form*)}*))

        etypecase 和 ctypecase 所使用的具体错误信息在不同的具体实现之间可以是不同的. 在错误消息的具体措辞的控制很重要的情况下, 最好使用 typecase 与 otherwise-clause 来显式地用适当的消息发出错误消息.


### <span id="MMULTIPLE-VALUE-BIND">宏 MULTIPLE-VALUE-BIND</span>

* 语法(Syntax):

        multiple-value-bind (var*) values-form declaration* form*
        => result*

* 参数和值(Arguments and Values):

        var---一个命名一个变量的符号[symbol]; 不求值.
        values-form---一个表达式形式[form]; 求值.
        declaration---一个 declare 表达式[expression]; 不求值.
        forms---一个隐式的 progn [implicit progn].
        results---这些表达式形式 forms 返回的值[value].

* 描述(Description):

        为 vars 创建新的变量绑定[binding]并且使用这些绑定[binding]执行一系列表达式形式 forms.

        创建的这个变量绑定[binding]是词法的, 除非指定了 special 声明.

        值表达式形式 values-form 被求值, 并且这些变量 vars 中的每一个都分别被绑定为那个表达式形式 form 返回的值. 如果这里的 vars 数量超过返回的值的数量, 额外的 nil 值被赋予剩余的 vars. 如果值的数量超过 vars, 多出来的值会被丢弃. vars 在这些表达式形式 forms 执行时被绑定为这些值, 这些表达式形式 forms 组成了隐式的 progn [implicit progn]. 如果一个 var 被指定了类型声明 declaration, 而绑定到这个 var 的值不符合这个类型声明 declaration, 那么结果是不确定的.

        这些名称绑定和声明 declarations 的作用域[scope]不包括 values-form.

* 示例(Examples):
    
    ```LISP
    (multiple-value-bind (f r)
        (floor 130 11)
      (list f r)) =>  (11 9)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        let, multiple-value-call

* 注意(Notes):

        (multiple-value-bind (var*) values-form form*)
        ==  (multiple-value-call #'(lambda (&optional var* &rest #1=#:ignore)
                                    (declare (ignore #1#))
                                    form*)
                                values-form)


### <span id="SOMULTIPLE-VALUE-CALL">特殊操作符 MULTIPLE-VALUE-CALL</span>

* 语法(Syntax):

        multiple-value-call function-form form* => result*

* 参数和值(Arguments and Values):

        function-form---一个表达式形式[form]; 求值后产生函数 function.
        function---通过求值 function-form 得到的一个函数标识符[function designator].
        form---一个表达式形式[form].
        results---函数 function 返回的值[value].

* 描述(Description):

        应用函数 function 到从多值[multiple values[2]]组中收集而来的这些对象[object]的一个列表[list]上.

        multiple-value-call 首先求值 function-form 来获取函数 function, 然后求值每一个表达式形式 form. 每个 form 的所有的值都被一起收集 (不只是每个表达式形式 form 的一个值) 然后作为参数传递给 function.

* 示例(Examples):

    ```LISP
    (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
    =>  (1 / 2 3 / / 2 0.5)
    (+ (floor 5 3) (floor 19 4)) ==  (+ 1 4)
    =>  5
    (multiple-value-call #'+ (floor 5 3) (floor 19 4)) ==  (+ 1 2 4 3)
    =>  10
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        multiple-value-list, multiple-value-bind

* 注意(Notes): None.


### <span id="MMULTIPLE-VALUE-LIST">宏 MULTIPLE-VALUE-LIST</span>

* 语法(Syntax):

        multiple-value-list form => list

* 参数和值(Arguments and Values):

        form---一个表达式形式[form]; 按如下所述求值.
        list---form 返回的那些值[value]的一个列表[list].

* 描述(Description):

        multiple-value-list 求值表达式形式 form 并且创建它返回的那些多值[multiple values[2]]的一个列表[list].

* 示例(Examples):
    
    ```LISP
    (multiple-value-list (floor -3 4)) =>  (-1 1)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        values-list, multiple-value-call

* 注意(Notes):

        multiple-value-list 和 values-list 是互逆的.

        (multiple-value-list form) ==  (multiple-value-call #'list form)


### <span id="SOMULTIPLE-VALUE-PROG1">特殊操作符 MULTIPLE-VALUE-PROG1</span>

* 语法(Syntax):

        multiple-value-prog1 first-form form* => first-form-results

* 参数和值(Arguments and Values):

        first-form---一个表达式形式[form]; 如下所述求值.
        form---一个表达式形式[form]; 如下所述求值.
        first-form-results---这个 first-form 的求值[evaluation]得到的值[value].

* 描述(Description):

        multiple-value-prog1 求值 first-form 然后把这个表达式形式[form]产生的所有值保存起来. 然后它从左到右求值每个表达式形式 form, 丢弃它们的值.

* 示例(Examples):

    ```LISP
    (setq temp '(1 2 3)) =>  (1 2 3)
    (multiple-value-prog1
       (values-list temp)
       (setq temp nil)
       (values-list temp)) =>  1, 2, 3
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        prog1

* 注意(Notes): None.


### <span id="MMULTIPLE-VALUE-SETQ">宏 MULTIPLE-VALUE-SETQ</span>

* 语法(Syntax):

        multiple-value-setq vars form => result

* 参数和值(Arguments and Values):

        vars---一个符号[symbol]列表[list], 这些符号是变量[variable]名字[name]或符号宏[symbol macro]的名字[name].
        form---一个表达式形式[form].
        result---这个表达式形式 form 返回的主值[primary value].

* 描述(Description):

        multiple-value-setq 给这些变量 vars 赋值.

        这个表达式形式 form 被求值, 然后每个变量 var 被赋予这个表达式形式 form 返回的对应的值[value]. 如果这里的这些变量 vars 的数量超过返回值[value]的数量, nil 会被赋值给多余的变量 vars. 如果返回值[value]的数量多于变量 vars, 多余的值[value]会被丢弃.

        如果任何变量 var 是一个符号宏[symbol macro]的名字[name], 那么它就会像是通过 setf 的一样被赋值. 具体的说, 为了使求值顺序和副作用的规则与 setf 使用的规则一致

        (multiple-value-setq (symbol1 ... symboln) value-producing-form)

        被定义为像下面方式一样运作

        (values (setf (values symbol1 ... symboln) value-producing-form))

        见章节 5.1.2.3 (VALUES 表达式形式作为位置).

* 示例(Examples):

    ```LISP
    (multiple-value-setq (quotient remainder) (truncate 3.2 2)) =>  1
    quotient =>  1
    remainder =>  1.2
    (multiple-value-setq (a b c) (values 1 2)) =>  1
    a =>  1
    b =>  2
    c =>  NIL
    (multiple-value-setq (a b) (values 4 5 6)) =>  4
    a =>  4
    b =>  5
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        setq, symbol-macrolet

* 注意(Notes): None.


### <span id="AVALUES">访问器 VALUES</span>

* 语法(Syntax):

        values &rest object => object*
        (setf (values &rest place) new-values)

* 参数和值(Arguments and Values):

        object---一个对象[object]
        place---一个位置[place].
        new-value---一个对象[object].

* 描述(Description):

        values 以多值[multiple values[2]]的形式返回这些对象 objects.

        values 的 setf 被用于存储这些多值[multiple values[2]] new-values 到这些位置 places. 见章节 5.1.2.3 (VALUES 表达式形式作为位置).

* 示例(Examples):

    ```LISP
    (values) =>  <no values>
    (values 1) =>  1
    (values 1 2) =>  1, 2
    (values 1 2 3) =>  1, 2, 3
    (values (values 1 2 3) 4 5) =>  1, 4, 5
    (defun polar (x y)
      (values (sqrt (+ (* x x) (* y y))) (atan y x))) =>  POLAR
    (multiple-value-bind (r theta) (polar 3.0 4.0)
      (vector r theta))
    =>  #(5.0 0.927295)
    ```

        有时我们需要显式指出函数只返回一个值. 比如, 函数
    
    ```LISP
    (defun foo (x y)
      (floor (+ x y) y)) =>  FOO
    ```
    
        由于 floor 返回两个值因此它也返回两个值. 第二个值可能是没有意义的, 或者出于效率考虑, 我们不希望计算第二个值. values 是表示只返回一个值的标准惯用语:

    ```LISP
    (defun foo (x y)
      (values (floor (+ x y) y))) =>  FOO
    ```

        这个正常工作是因为 values 只返回参数 args 中的每一个的一个值; 对于任何函数调用, 如果任何参数产生不止一个值, 除了第一个以外其他都被丢弃.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        values-list, multiple-value-bind, multiple-values-limit, 章节 3.1 (求值)

* 注意(Notes):

        由于 values 是一个函数[function], 不是一个宏[macro]或者特殊表达式形式[special form], 它只接收它的实参[argument]表达式形式[form]的主值[primary value]作为它的实参[argument].


### <span id="FVALUES-LIST">函数 VALUES-LIST</span>

* 语法(Syntax):

        values-list list => element*

* 参数和值(Arguments and Values):

        list---一个列表[list].
        elements---这个列表 list 的元素[element].

* 描述(Description):

        以多值[multiple values[2]]的形式返回这个列表 list 的元素[element].

* 示例(Examples):

    ```LISP
    (values-list nil) =>  <no values>
    (values-list '(1)) =>  1
    (values-list '(1 2)) =>  1, 2
    (values-list '(1 2 3)) =>  1, 2, 3
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果参数不是一个正规列表[proper list], 应该会发出一个 type-error 的错误.

* 也见(See Also):

        multiple-value-bind, multiple-value-list, multiple-values-limit, values

* 注意(Notes):

        (values-list list) ==  (apply #'values list)

        (equal x (multiple-value-list (values-list x))) 对于所有的列表[list] x 返回 true.


### <span id="CMULTIPLE-VALUES-LIMIT">常量 MULTIPLE-VALUES-LIMIT</span>

* 常量值(Constant Value):

        一个不小于 20 的整数[integer], 准确的大小是依赖于具体实现的[implementation-dependent].

* 描述(Description):

        可以被一个函数[function]返回, 被 multiple-value-bind 或 multiple-value-setq 绑定或赋值还有作为传递给 nth-value 的第一个参数的值[value]数量的上限. (如果这些单独的限制不同, 则使用最小值.)

* 示例(Examples): None.

* 也见(See Also):

        lambda-parameters-limit, call-arguments-limit

* 注意(Notes):

        鼓励实现者去使这个限制尽可能的大.


### <span id="MNTH-VALUE">宏 NTH-VALUE</span>

* 语法(Syntax):

        nth-value n form => object

* 参数和值(Arguments and Values):

        n---一个非负整数[integer]; 求值的.
        form---一个表达式形式[form]; 按如下所述求值.
        object---一个对象[object].

* 描述(Description):

        求值 n 和表达式形式 form, 表达式形式 form 产生的第 n 个值作为唯一的值返回, 如果 n 大于等于表达式形式 form 返回的值[value]的数量那么就是 nil. (返回的第一个值编号为 0.)

* 示例(Examples):
    
    ```LISP
    (nth-value 0 (values 'a 'b)) =>  A
    (nth-value 1 (values 'a 'b)) =>  B
    (nth-value 2 (values 'a 'b)) =>  NIL
    (let* ((x 83927472397238947423879243432432432)
           (y 32423489732)
           (a (nth-value 1 (floor x y)))
           (b (mod x y)))
      (values a b (= a b)))
    =>  3332987528, 3332987528, true
    ```
    
* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        multiple-value-list, nth

* 注意(Notes):

        操作上, 下面的关系是 true 的, 尽管 nth-value 在某些实现中可能由于某些因素而更高效, 比如, 可以避免一些 cons.

        (nth-value n form) ==  (nth n (multiple-value-list form))


### <span id="MPROGPROG*">宏 PROG, PROG*</span>

* 语法(Syntax):

        prog ({var | (var [init-form])}*) declaration* {tag | statement}*
        => result*

        prog* ({var | (var [init-form])}*) declaration* {tag | statement}*
        => result*

* 参数和值(Arguments and Values):

        var---变量名字.
        init-form---一个表达式形式[form].
        declaration---一个 declare 表达式[expression]; 不求值.
        tag---一个 go 标签[go tag]; 不求值.
        statement---一个复合表达式形式[compound form]; 按如下所述求值.
        results---如果发生正常返回[normal return]就是 nil, 否则, 如果发生一个显式返回[explicit return], 就是转移的那些值[value].

* 描述(Description):

        由 prog 和 prog* 执行三个不同的操作: 它们绑定局部变量, 它们允许 return 语句的使用, 并且它们允许 go 语句的使用. 一个典型的 prog 看上去像这样:

        (prog (var1 var2 (var3 init-form-3) var4 (var5 init-form-5))
              declaration*
              statement1
         tag1
              statement2
              statement3
              statement4
         tag2
              statement5
              ...
              )

        对于 prog, 这些 init-forms 首先被求值, 按照它们提供的顺序. 这些变量 vars 并行地绑定为对应的值. 如果没有为一个给定的变量 var 提供 init-form, 那个这个变量 var 绑定为 nil.

        这个 prog 的主体部分好像它是一个 tagbody 表达式形式[form]一样被执行; 这个 go 语句可以被用于转移控制到一个 tag. 这些 tags 标注这些语句 statements.

        prog 在整个 prog 表达式形式[form]周围隐式建立一个名为 nil 的 block, 这样 return 可以在任何想要退出 prog 表达式形式[form]的时候被使用.

        prog* 和 prog 之间的不同在于 prog* 顺序执行这些变量 vars 的绑定[binding]和初始化, 因此每一个的 init-form 都可以使用前一个的值.

* 示例(Examples):
    
    ```LISP
    (prog* ((y z) (x (car y)))
          (return x))
    ```
    
        返回 z 的 car 部分.

    ```LISP
    (setq a 1) =>  1
    (prog ((a 2) (b a)) (return (if (= a b) '= '/=))) =>  /=
    (prog* ((a 2) (b a)) (return (if (= a b) '= '/=))) =>  =
    (prog () 'no-return-value) =>  NIL

    (defun king-of-confusion (w)
      "Take a cons of two lists and make a list of conses.
       Think of this function as being like a zipper."
      (prog (x y z)          ;Initialize x, y, z to NIL
           (setq y (car w) z (cdr w))
       loop
           (cond ((null y) (return x))
                 ((null z) (go err)))
       rejoin
           (setq x (cons (cons (car y) (car z)) x))
           (setq y (cdr y) z (cdr z))
           (go loop)
       err
           (cerror "Will self-pair extraneous items"
                   "Mismatch - gleep!  ~S" y)
           (setq z y)
           (go rejoin))) =>  KING-OF-CONFUSION
    ```
    
        按照下面这样这可以更详细地完成:

    ```LISP
    (defun prince-of-clarity (w)
      "Take a cons of two lists and make a list of conses.
       Think of this function as being like a zipper."
      (do ((y (car w) (cdr y))
           (z (cdr w) (cdr z))
           (x '() (cons (cons (car y) (car z)) x)))
          ((null y) x)
        (when (null z)
          (cerror "Will self-pair extraneous items"
                 "Mismatch - gleep!  ~S" y)
          (setq z y)))) =>  PRINCE-OF-CLARITY
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        block, let, tagbody, go, return, 章节 3.1 (求值)

* 注意(Notes):

        prog 可以被解释为 block, let, 和 tagbody, 如下所述:

        (prog variable-list declaration . body)
           ==  (block nil (let variable-list declaration (tagbody . body)))


### <span id="MPROG1PROG2">宏 PROG1, PROG2</span>

* 语法(Syntax):

        prog1 first-form form* => result-1
        prog2 first-form second-form form* => result-2

* 参数和值(Arguments and Values):

        first-form---一个表达式形式[form]; 按如下所述求值.
        second-form---一个表达式形式[form]; 按如下所述求值.
        forms---一个隐式的 progn [implicit progn]; 按如下所述求值.
        result-1---这个 first-form 的求值[evaluation]所产生的主值[primary value].
        result-2---这个 second-form 的求值[evaluation]所产生的主值[primary value].

* 描述(Description):

        prog1 求值[evaluate] first-form 然后再是其他表达式形式 forms, first-form 产生的主值[primary value]作为唯一的返回值[value].

        prog2 求值[evaluate] first-form, 然后 second-form, 然后再是这些表达式形式 forms, 第二个表达式形式 second-form 产生的主值[primary value]作为唯一的返回值[value].

* 示例(Examples):

    ```LISP
    (setq temp 1) =>  1
    (prog1 temp (print temp) (incf temp) (print temp))
    >>  1
    >>  2
    =>  1
    (prog1 temp (setq temp nil)) =>  2
    temp =>  NIL
    (prog1 (values 1 2 3) 4) =>  1
    (setq temp (list 'a 'b 'c))
    (prog1 (car temp) (setf (car temp) 'alpha)) =>  A
    temp =>  (ALPHA B C)
    (flet ((swap-symbol-values (x y)
             (setf (symbol-value x)
                   (prog1 (symbol-value y)
                          (setf (symbol-value y) (symbol-value x))))))
      (let ((*foo* 1) (*bar* 2))
        (declare (special *foo* *bar*))
        (swap-symbol-values '*foo* '*bar*)
        (values *foo* *bar*)))
    =>  2, 1
    (setq temp 1) =>  1
    (prog2 (incf temp) (incf temp) (incf temp)) =>  3
    temp =>  4
    (prog2 1 (values 2 3 4) 5) =>  2
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        multiple-value-prog1, progn

* 注意(Notes):

        prog1 和 prog2 通常被用于求值[evaluate]一个或多个带有副作用的表达式形式[form]并且返回一个在一些或所有副作用发生之前计算的值[value].

        (prog1 form*) ==  (values (multiple-value-prog1 form*))
        (prog2 form1 form*) ==  (let () form1 (prog1 form*))


### <span id="SOPROGN">特殊操作符 PROGN</span>

* 语法(Syntax):

        progn form* => result*

* 参数和值(Arguments and Values):

        forms---一个隐式的 progn [implicit progn].
        results---这些表达式形式[form]的值[value].

* 描述(Description):

        progn 求值这些表达式形式 forms, 按它们被提供的顺序.

        除了最后一个以外的所有表达式形式 form 的值都会被丢弃.

        如果 progn 作为顶层表达式形式[top level form]出现, 那么出现在这个 progn 中的所有表达式形式[form]都会被当作顶层表达式形式[top level form]处理.

* 示例(Examples):
    
    ```LISP
    (progn) =>  NIL
    (progn 1 2 3) =>  3
    (progn (values 1 2 3)) =>  1, 2, 3
    (setq a 1) =>  1
    (if a
         (progn (setq a nil) 'here)
         (progn (setq a t) 'there)) =>  HERE
    a =>  NIL
    ```name
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        prog1, prog2, 章节 3.1 (求值)

* 注意(Notes):
name
        很多 Common Lisp 中的位置涉及使用隐式 progn [implicit progn]的语法. 这也就是说, 其语法的一部分允许编写的多个表达式形式[form]按顺序进行求值, 丢弃除了最后一个以外的表达式形式[form]的值并且返回最后一个表达式形式[form]的结果. 这些位置包括, 但不限于以下这些: 一个 lambda 表达式[lambda expression]的主体部分; 多种控制和条件表达式形式[form]的主体部分 (比如, case, catch, progn, 和 when).


### <span id="MDEFINE-MODIFY-MACRO">宏 DEFINE-MODIFY-MACRO</span>

* 语法(Syntax):

        define-modify-macro name lambda-list function [documentation] => name

* 参数和值(Arguments and Values):

        name---一个符号[symbol].
        lambda-list---一个 define-modify-macro lambda 列表[define-modify-macro lambda list].
        function---一个符号[symbol].
        documentation---一个字符串[string]; 不求值.

* 描述(Description):

        define-modify-macro 定义一个名为 name 的宏[macro]去读取[read]和写入[write]一个位置[place].

        给这个新的宏[macro]的这些参数是一个位置[place], 后面跟着 lambda-list 中提供的那些参数. 使用 define-modify-macro 定义的宏[macro]正确地传递环境参数[environment parameter]给 get-setf-expansion.

        当这个宏[macro]被调用, 函数 function 被应用到这个位置[place]的旧的内容和 lambda-list 的参数上来获取新的值, 然后这个位置[place]被更新为包含这个结果.

        除了避免多重求值的问题 (见下方), 一个 define-modify-macro 的展开式等价于下面这个:

    ```LISP
    (defmacro name (reference . lambda-list)
      documentation
      `(setf ,reference
              (function ,reference ,arg1 ,arg2 ...)))
    ```

        其中 arg1, arg2, ..., 是出现在 lambda-list 中的形参; 为剩余参数[rest parameter]制定适当的规定.

        通过 define-modify-macro 定义的这个宏调用的子表达式形式[subform]按照章节 5.1.1.1 (位置的子表达式形式求值) 中描述的求值.

        documentation 作为一个文档字符串[documentation string]关联给 name (作为 function) 和这个宏函数[macro function].

        如果一个 define-modify-macro 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须在编译时存储这个宏[macro]定义, 以便出现在这个文件中后面的宏可以被正确地展开.

* 示例(Examples):

    ```LISP
    (define-modify-macro appendf (&rest args)
        append "Append onto list") =>  APPENDF
    (setq x '(a b c) y x) =>  (A B C)
    (appendf x '(d e f) '(1 2 3)) =>  (A B C D E F 1 2 3)
    x =>  (A B C D E F 1 2 3)
    y =>  (A B C)
    (define-modify-macro new-incf (&optional (delta 1)) +)
    (define-modify-macro unionf (other-set &rest keywords) union)
    ```

* 副作用(Side Effects):

        一个宏定义被赋值给 name.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defsetf, define-setf-expander, documentation, 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes): None.

### <span id="MDEFSETF">宏 DEFSETF</span>

* 语法(Syntax):

        "短表达式形式(short form)":

        defsetf access-fn update-fn [documentation]
        => access-fn

        "长表达式形式(long form)":

        defsetf access-fn lambda-list (store-variable*) [[declaration* | documentation]] form*
        => access-fn

* 参数和值(Arguments and Values):

        access-fn---命名一个函数[function]或一个宏[macro]的一个符号[symbol].
        update-fn---命名一个函数[function]或一个宏[macro]的一个符号[symbol].
        lambda-list---一个 defsetf lambda 列表[defsetf lambda list].
        store-variable---一个符号[symbol] (一个变量[variable]名字[name]).
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        form---一个表达式形式[form].

* 描述(Description):

        对于相对简单的情况 defsetf 定义了如何去 setf 这个表达式形式 (access-fn ...) 的一个位置[place]. (参阅 define-setf-expander 以获得更多对这个机制的使用方式.) 必须是由 access-fn 命名的函数[function]或宏[macro]计算了其所有参数.

        defsetf 可能接受两个表达式形式的其中之一, 它们称之为 "短表达式形式(short form)" 和  "长表达式形式(long form)", 它们是由第二个实参[argument]的类型[type]来区分的.

        当使用短表达式形式时, update-fn 必须命名一个函数[function] (或者宏[macro]), 它比 access-fn 多接收一个参数. 当 setf 被给予一个 access-fn 调用的位置[place], 它展开为一个 update-fn 的调用并且给定所有给 access-fn 的参数, 还有作为它的最后一个参数, 那个新的值(这个值必须被 update-fn 作为它的值返回).

        这个长表达式形式的 defsetf 类似于 defmacro. 其中 lambda-list 描述了 access-fn 的参数. 这些 store-variables 描述了存储到这个位置[place]的值或多值. 这个主体 body 必须计算一个 access-fn 上调用的 setf 的展开式. 这个展开函数定义在 defsetf 表达式形式[form]出现的相同词法环境[lexical environment]内.

        在这些表达式形式 forms 的求值期间, 在 lambda-list 和 store-variables 中的那些变量绑定为临时变量的名字, 就像是通过 gensym 或 gentemp 生成的一样, 这会通过对这些子表达式形式[subform]的值的 setf 的展开式来绑定. 这个绑定允许在不考虑求值顺序问题的情况下编写这些表达式形式 forms. 在可能的情况下, defsetf 会安排临时变量, 以优化最终结果.

        这个 defsetf 中的主体代码隐式地闭合在一个名为 access-fn 的块[block]内.

        defsetf 确保这个位置[place]的子表达式形式[subform]只被求值一次.

        documentation 作为 setf 种类的文档字符串[documentation string]关联到 access-fn.

        如果一个 defsetf 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须使这个 setf 展开器[setf expander]可用, 以便它可以被用于展开这个文件[file]中后面的 setf 调用. 如果在同一文件[file]中后面的一个位置[place]中使用 access-fn, 那么如果有的话, 用户必须确保这些表达式形式 forms 能够在编译时进行求值. 编译器[compiler]必须使这些 setf 展开器[setf expander]对 get-setf-expansion 的编译期调用是可用的, 当它的 environment 参数是一个作为一个宏[macro]的环境参数[environment parameter]接收到的值时.

* 示例(Examples):

        下面这个表达式

    ```LISP
    (defsetf symbol-value set)
    ```
    
        的效果被构建到 Common Lisp 系统中. 这个导致表达式形式 (setf (symbol-value foo) fu) 展开为 (set foo fu).

        注意

    ```LISP
    (defsetf car rplaca)
    ```

        会是不正确的因为 rplaca 不会返回它的最后一个参数.

    ```LISP
    (defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x)) =>  MIDDLEGUY
    (defun set-middleguy (x v)
        (unless (null x)
          (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
        v) =>  SET-MIDDLEGUY
    (defsetf middleguy set-middleguy) =>  MIDDLEGUY
    (setq a (list 'a 'b 'c 'd)
          b (list 'x)
          c (list 1 2 3 (list 4 5 6) 7 8 9)) =>  (1 2 3 (4 5 6) 7 8 9)
    (setf (middleguy a) 3) =>  3
    (setf (middleguy b) 7) =>  7
    (setf (middleguy (middleguy c)) 'middleguy-symbol) =>  MIDDLEGUY-SYMBOL
    a =>  (A 3 C D)
    b =>  (7)
    c =>  (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9)
    ```

        一个 defsetf 的长表达式的使用的示例是:

    ```LISP
    (defsetf subseq (sequence start &optional end) (new-sequence)
      `(progn (replace ,sequence ,new-sequence
                        :start1 ,start :end1 ,end)
              ,new-sequence)) =>  SUBSEQ

    (defvar *xy* (make-array '(10 10)))
    (defun xy (&key ((x x) 0) ((y y) 0)) (aref *xy* x y)) =>  XY
    (defun set-xy (new-value &key ((x x) 0) ((y y) 0))
      (setf (aref *xy* x y) new-value)) =>  SET-XY
    (defsetf xy (&key ((x x) 0) ((y y) 0)) (store)
      `(set-xy ,store 'x ,x 'y ,y)) =>  XY
    (get-setf-expansion '(xy a b))
    =>  (#:t0 #:t1),
      (a b),
      (#:store),
      ((lambda (&key ((x #:x)) ((y #:y)))
          (set-xy #:store 'x #:x 'y #:y))
        #:t0 #:t1),
      (xy #:t0 #:t1)
    (xy 'x 1) =>  NIL
    (setf (xy 'x 1) 1) =>  1
    (xy 'x 1) =>  1
    (let ((a 'x) (b 'y))
      (setf (xy a 1 b 2) 3)
      (setf (xy b 5 a 9) 14))
    =>  14
    (xy 'y 0 'x 1) =>  1
    (xy 'x 1 'y 2) =>  3
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        documentation, setf, define-setf-expander, get-setf-expansion, Section 5.1 (Generalized Reference), Section 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        这些表达式形式 forms 必须包含返回正确值的规定 (这个 store-variable 的值或多值). 这个由这些表达式形式 forms 处理而不是 defsetf 因为在很多情况下这个值不需要额外成本就可以被返回, 通过调用一个同时存储到那个位置[place]并返回正确值的函数.

        一个在 access-fn 上调用的 setf 也求值所有 access-fn 的参数; 它不能特别地对待任何一个. 这就意味着 defsetf 不能被用于描述如何存储到一个广义引用[generalized reference]为一个字节, 例如 (ldb field reference). define-setf-expander 用于处理不符合 defsetf 所施加的限制的情况, 并为用户提供额外的控制.

### <span id="MDEFINE-SETF-EXPANDER">宏 DEFINE-SETF-EXPANDER</span>

* 语法(Syntax):

        define-setf-expander access-fn lambda-list [[declaration* | documentation]] form*
        => access-fn

* 参数和值(Arguments and Values):

        access-fn---命名[name]一个宏[macro]或函数[function]的一个符号[symbol].
        lambda-list -- 宏 lambda 列表[macro lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        forms---一个隐式的 progn [implicit progn].

* 描述(Description):

        define-setf-expander 指定了 setf 更新一个被 access-fn 引用的位置[place]的方法.

        当 setf 被给定一个根据 access-fn 指定的位置[place]和这个位置的一个新值时, 它被展开为一个执行适当更新的表达式形式.

        这个 lambda-list 支持解构. 见章节 3.4.4 (宏 lambda 列表).

        documentation 作为一个 setf 类型的文档字符串[documentation string]关联给 access-fn.

        这些表达式形式 forms 组成了 setf 展开器[setf expander]定义的主体, 并且必须计算一个借助于给定 access-fn 来引用这个位置[place]的 setf 上的调用的 setf 展开式[setf expansion]. 这个 setf 展开器[setf expander]函数被定义在 define-setf-expander 表达式形式[form]出现的同一个词法环境[lexical environment]里. 当这些表达式形式 forms 要被执行时, lambda-list 中的变量被绑定为位置[place]表达式形式[form]的各个部分. 一个 define-setf-expander 表达式形式[form]的主体表达式形式 forms (不包括 lambda-list) 被隐式地闭合在一个名为 access-fn 的块[block]中.

        这些表达式形式 forms 的求值必须产生章节 5.1.1.2 (Setf 展开式) 中描述的 5 个值.

        如果一个 define-setf-expander 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须使这个 setf 展开器[setf expander]可用, 这样它就可以被用于展开这个文件[file]中后面对 setf 的调用. 如果这个 access-fn 被用于同一个文件[file]后面的一个位置[place], 程序员[programmer]必须确保这些表达式形式 forms 可以在编译期被求值. 编译器[compiler]必须使这些 setf 展开器[setf expander]对 get-setf-expansion 的编译期调用是可用的, 当它的 environment 参数是一个作为一个宏[macro]的环境参数[environment parameter]接收到的值时.

* 示例(Examples):

    ```LISP
    (defun lastguy (x) (car (last x))) =>  LASTGUY
    (define-setf-expander lastguy (x &environment env)
      "Set the last element in a list to the given value."
      (multiple-value-bind (dummies vals newval setter getter)
          (get-setf-expansion x env)
        (let ((store (gensym)))
          (values dummies
                  vals
                  `(,store)
                  `(progn (rplaca (last ,getter) ,store) ,store)
                  `(lastguy ,getter))))) =>  LASTGUY
    (setq a (list 'a 'b 'c 'd)
          b (list 'x)
          c (list 1 2 3 (list 4 5 6))) =>  (1 2 3 (4 5 6))
    (setf (lastguy a) 3) =>  3
    (setf (lastguy b) 7) =>  7
    (setf (lastguy (lastguy c)) 'lastguy-symbol) =>  LASTGUY-SYMBOL
    a =>  (A B C 3)
    b =>  (7)
    c =>  (1 2 3 (4 5 LASTGUY-SYMBOL))

    ;;; Setf expander for the form (LDB bytespec int).
    ;;; Recall that the int form must itself be suitable for SETF.
    (define-setf-expander ldb (bytespec int &environment env)
      (multiple-value-bind (temps vals stores
                              store-form access-form)
          (get-setf-expansion int env);Get setf expansion for int.
        (let ((btemp (gensym))     ;Temp var for byte specifier.
              (store (gensym))     ;Temp var for byte to store.
              (stemp (first stores))) ;Temp var for int to store.
          (if (cdr stores) (error "Can't expand this."))
    ;;; Return the setf expansion for LDB as five values.
          (values (cons btemp temps)       ;Temporary variables.
                  (cons bytespec vals)     ;Value forms.
                  (list store)             ;Store variables.
                  `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                      ,store-form
                      ,store)               ;Storing form.
                  `(ldb ,btemp ,access-form) ;Accessing form.
                  ))))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        setf, defsetf, documentation, get-setf-expansion, 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes):

        define-setf-expander 与长表达式形式的 defsetf 不同在于, 在执行主体的过程中, lambda-list 中的那些变量[variable]被绑定为位置[place]表达式形式[form]的各个部分, 而不是绑定到这些部分的值的临时变量. 另外, define-setf-expander 没有 defsetf 的 access-fn 必须是一个函数[function]或类函数宏[macro]的限制; 一个任意 defmacro 解构模式允许出现在 lambda-list 中.


### <span id="FGET-SETF-EXPANSION">函数 GET-SETF-EXPANSION</span>

* 语法(Syntax):

        get-setf-expansion place &optional environment
        => vars, vals, store-vars, writer-form, reader-form

* 参数和值(Arguments and Values):

        place---一个位置[place].
        environment---一个环境对象[environment object].
        vars, vals, store-vars, writer-form, reader-form---一个 setf 展开式[setf expansion].

* 描述(Description):

        确定在环境 environment 中组成这个 place 的 setf 展开式[setf expansion]的 5 个值; 见章节 5.1.1.2 (Setf 展开式).

        如果 environment 没有被提供或者是 nil, 那么环境就是那个空词法环境[null lexical environment].

* 示例(Examples):

    ```LISP
    (get-setf-expansion 'x)
    =>  NIL, NIL, (#:G0001), (SETQ X #:G0001), X

    ;;; This macro is like POP

    (defmacro xpop (place &environment env)
      (multiple-value-bind (dummies vals new setter getter)
                            (get-setf-expansion place env)
          `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
            (if (cdr new) (error "Can't expand this."))
            (prog1 (car ,(car new))
                    (setq ,(car new) (cdr ,(car new)))
                    ,setter))))

    (defsetf frob (x) (value)
        `(setf (car ,x) ,value)) =>  FROB
    ;;; The following is an error; an error might be signaled at macro expansion time
    (flet ((frob (x) (cdr x)))  ;Invalid
      (xpop (frob z)))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

    defsetf, define-setf-expander, setf

* 注意(Notes):

        任何复合表达式形式[compound form]都是一个合法的位置[place], 因为对于任何操作符[operator] f 没有 setf 展开器[setf expander]的复合表达式形式[compound form]都被展开为一个对 (setf f) 的调用.


### <span id="MSETFPSETF">宏 SETF, PSETF</span>

* 语法(Syntax):

        setf {pair}* => result*
        psetf {pair}* => nil
        pair::= place newvalue

* 参数和值(Arguments and Values):

        place---一个位置[place].
        newvalue---一个表达式形式[form].
        results---最后一个位置 place 的存储表达式形式返回的多值[multiple values[2]], 如果没有这些对 pairs 的话就是 nil.

* 描述(Description):

        setf 改变 place 的值[value]为 newvalue.

        (setf place newvalue) 展开为一个更新表达式形式, 它存储 newvalue 求值的结果到这个位置 place 引用的位置. 有些 place 表达式形式涉及接受可选参数的访问器(accessor)的使用. 这些可选参数是否被 setf 允许, 或者它们的用途是什么, 取决于 setf 展开器函数, 不在 setf 的控制之下. 对于任何接受 &optional, &rest, 或 ..... key 参数并且与 setf 一起使用的声明的函数[function]的文档都必须指明这些参数是如何处理的.

        如果提供了不止一个对(pair) pair, 这些对(pair)会按照顺序处理; 这也就是说,

        (setf place-1 newvalue-1
              place-2 newvalue-2
              ...
              place-N newvalue-N)

        正号等价于

        (progn (setf place-1 newvalue-1)
                (setf place-2 newvalue-2)
                ...
                (setf place-N newvalue-N))

        对于 psetf, 如果提供了不止一个对(pair) pair, 那么赋新的值到这些位置的操作被并行执行. 更准确地说, 要求值的所有子表达式形式[subform] (不管是 place 还是 newvalue 表达式形式[form]中) 都是从左到右进行求值的; 在所有求值被执行后, 所有的赋值都以不可预计的顺序执行.

        关于对待 setf 和 psetf 的展开式的详细信息, 见章节 5.1.2 (位置的种类).

* 示例(Examples):

    ```LISP
    (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3)
    (setf (car x) 'x (cadr y) (car x) (cdr x) y) =>  (1 X 3)
    x =>  (X 1 X 3)
    y =>  (1 X 3)
    (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3)
    (psetf (car x) 'x (cadr y) (car x) (cdr x) y) =>  NIL
    x =>  (X 1 A 3)
    y =>  (1 A 3)
    ```

* 受此影响(Affected By):

        define-setf-expander, defsetf, *macroexpand-hook*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-setf-expander, defsetf, macroexpand-1, rotatef, shiftf, 章节 5.1 (广义引用)

* 注意(Notes): None.


### <span id="MSHIFTF">宏 SHIFTF</span>

* 语法(Syntax):

        shiftf place+ newvalue => old-value-1

* 参数和值(Arguments and Values):

        place---一个位置[place].
        newvalue---一个表达式形式[form]; 求值.
        old-value-1---一个对象[object] (第一个 place 的旧值[value]).

* 描述(Description):

        shiftf 通过存储 newvalue 到最后一个 place 并将第二个到最后一个 place 的值移动到到其余 place 中来修改每一个 place 的值, .

        如果 newvalue 产生了超过这里的存储变量数量的值, 多余的值会被忽略. 如果 newvalue 产生的值少于存储变量的数量, 缺少的值会被设置为 nil.

        在表达式形式 (shiftf place1 place2 ... placen newvalue) 中, 从 place1 到 placen 的值被读取[read]和保存, 然后 newvalue 被求值, 总共 n+1 个值. 从 2 到 n+1 的值被分别存储到 place1 到 placen. 就好像所有这些 place 都形成了一个移位寄存器; newvalue 从右边移过来, 所有的值都转移到左边一个位置, 然后返回的值从 place1 中移出.

        关于这些位置 places 的子表达式形式[subform]的求值[evaluation]的更多信息, 见章节 5.1.1.1 (位置的子表达式形式求值).

* 示例(Examples):

    ```LISP
    (setq x (list 1 2 3) y 'trash) =>  TRASH
    (shiftf y x (cdr x) '(hi there)) =>  TRASH
    x =>  (2 3)
    y =>  (1 HI THERE)

    (setq x (list 'a 'b 'c)) =>  (A B C)
    (shiftf (cadr x) 'z) =>  B
    x =>  (A Z C)
    (shiftf (cadr x) (cddr x) 'q) =>  Z
    x =>  (A (C) . Q)
    (setq n 0) =>  0
    (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
    (shiftf (nth (setq n (+ n 1)) x) 'z) =>  B
    x =>  (A Z C D)
    ```

* 受此影响(Affected By):

        define-setf-expander, defsetf, *macroexpand-hook*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        setf, rotatef, 章节 5.1 (广义引用)

* 注意(Notes):

        这个 (shiftf place1 place2 ... placen newvalue) 的影响差不多等价于

        (let ((var1 place1)
              (var2 place2)
              ...
              (varn placen)
              (var0 newvalue))
          (setf place1 var2)
          (setf place2 var3)
          ...
          (setf placen var0)
          var1)

        除了后者会对每个 place 的任何子表达式形式[subform]进行两次求值, 而 shiftf 计算一次. 比如,

        (setq n 0) =>  0
        (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
        (prog1 (nth (setq n (+ n 1)) x)
                (setf (nth (setq n (+ n 1)) x) 'z)) =>  B
        x =>  (A B Z D)


### <span id="MROTATEF">宏 ROTATEF</span>

* 语法(Syntax):

        rotatef place* => nil

* 参数和值(Arguments and Values):

        place---一个位置[place].

* 描述(Description):

        rotatef 通过将值从一个 place 旋转到另一个 place 来修改每个 place 的值.

        如果一个 place 产生了超过这里的存储变量数量的值, 多余的值会被忽略. 如果一个 place 产生的值少于存储变量的数量, 缺少的值会被设置为 nil.

        在表达式形式 (rotatef place1 place2 ... placen) 中, 从 place1 到 placen 的值被读取[read]和写入[write]. 第 2 到 n 个值和值 1 接下来被存储到 place1 到 placen. 就好像所有这些位置都形成了一个末端的移位寄存器, 它将一个位置旋转到左边, 而 place1 的值被移动到 placen.

        关于这些位置 places 的子表达式形式[subform]的求值[evaluation]的更多信息, 见章节 5.1.1.1 (位置的子表达式形式求值).

* 示例(Examples):

    ```LISP
    (let ((n 0)
            (x (list 'a 'b 'c 'd 'e 'f 'g)))
        (rotatef (nth (incf n) x)
                (nth (incf n) x)
                (nth (incf n) x))
        x) =>  (A C D B E F G)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-setf-expander, defsetf, setf, shiftf, *macroexpand-hook*, 章节 5.1 (广义引用)

* 注意(Notes):

        这个 (rotatef place1 place2 ... placen) 的效果粗略等价于

        (psetf place1 place2
                place2 place3
                ...
                placen place1)

        除了后者会对每个 place 的任何子表达式形式[subform]进行两次求值, 而 rotatef 求值一次.

### <span id="CTCONTROL-ERROR">状况类型 CONTROL-ERROR</span>

* 类优先级列表(Class Precedence List):

        control-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] control-error 由程序中非法的动态控制转移所引起的错误状况组成. throw 一个不活跃的 tag 或者 go 或 return-from 一个不再是动态可用的 tag 导致的错误就是 control-error 类型[type]的.


### <span id="CTPROGRAM-ERROR">状况类型 PROGRAM-ERROR</span>

* 类优先级列表(Class Precedence List):

        program-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] program-error 由不正确的程序语法相关的错误状况组成. 命名一个不是词法上可见的 go 标签[go tag]或 block 标签[block tag]所导致的错误就是类型[type] program-error 的.


### <span id="CTUNDEFINED-FUNCTION">状况类型 UNDEFINED-FUNCTION</span>

* 类优先级列表(Class Precedence List):

        undefined-function, cell-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] undefined-function 由表示尝试去读取[read]一个未定义函数[undefined function]的定义的错误[error]状况[condition]组成.

        这个存储格(cell) (见 cell-error) 的名字是一个 funbound 的函数名[function name].

* 也见(See Also):

        cell-error-name
