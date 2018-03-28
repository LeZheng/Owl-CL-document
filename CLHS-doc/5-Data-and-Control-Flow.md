# 5. 数据和控制流
<!-- TODO place  Generalized Reference ?? -->
> * 5.1 [Generalized Reference](#GeneralizedReference)
> * 5.2 [退出点的控制转移](#TCEP)
> * 5.3 [数据和控制流字典](#DCFDictionary)

## 5.1 <span id="GeneralizedReference">Generalized Reference</span>

> * 5.1.1 [Places 和 Generalized Reference 的概述](#OverviewPlacesGeneralizedReference)
> * 5.1.2 [Places 的种类](#KindsOfPlaces)
> * 5.1.3 [基于 SETF 的其他宏的处理](#TreatmentMacrosSETF)

### 5.1.1 <span id="OverviewPlacesGeneralizedReference">Places 和 Generalized Reference 的概述</span>

一个 generalized reference 是一个表达式形式的使用, 有时也称作一个 place , 就好像它是一个可以被读写的变量. 一个 place 的值就是这个 place 表达式求值后的对象. 一个 place 的值可以通过使用 setf 来改变. 绑定一个 place 的概念没有在 Common Lisp 中定义, 但是一个实现允许通过定义它的概念来扩展这个语言.

下面这段包含了 setf 使用的示例. 注意, 求值第二列的表达式形式返回的值没有必要和求值第三列中的表达式形式获取到的值一样. 总之, setf 表达式形式准确的宏展开是不保证的, 甚至是依赖于具体实现的; 可以保证的是, 展开后是一个针对特定实现的更新表达式形式, 对子表达式的从左到右求值是保留的, 而求值 setf 的最终结果是存储的值或多值.

|Access function |  Update Function |  Update using setf              |
| -              | -                | -                               |
|x               |  (setq x datum)  |  (setf x datum)                 |
|(car x)         |  (rplaca x datum)|  (setf (car x) datum)           |
|(symbol-value x)|  (set x datum)   |  (setf (symbol-value x) datum)  |

Figure 5-1. setf 的实例

下面这段展示了关于 places 和 generalized reference 的操作符.

    assert                defsetf             push
    ccase                 get-setf-expansion  remf
    ctypecase             getf                rotatef
    decf                  incf                setf
    define-modify-macro   pop                 shiftf
    define-setf-expander  psetf

Figure 5-2. places 和 generalized reference 相关的操作符.

上面的操作符中一些操作 places, 一些操作 setf 的展开. 一个 setf 展开可以来自于任何 place. 可以通过使用 defsetf 和 define-setf-expander 来定义新的 setf 展开.

> * 5.1.1.1 [对 Place 的字表达式求值](#EvaluationSubformsPlaces)
> * 5.1.1.2 [Setf 展开](#SetfExpansions)

#### 5.1.1.1 <span id="EvaluationSubformsPlaces">对 Place 的字表达式求值</span>

以下规则应用于 place 的字表达式的求值:

1. 一个 place 中的字表达式的求值顺序由 get-setf-expansion 返回的第二个值指定的顺序所决定. 对于所有这个标准定义的 place (比如, getf, ldb, ...), 求值的顺序是从左到右的. 当一个 place 从一个宏展开中得到, 这个规则将在宏展开后应用, 以找到合适的 place.

    Places defined 通过使用 defmacro 或 define-setf-expander 定义的 place 使用这些定义所定义的求值顺序. 比如, 思考下面这个:

    ```LISP
    (defmacro wrong-order (x y) `(getf ,y ,x))
    ```

    下面这个表达式形式先求值 place2 然后再是 place1 因为这个是它们在宏展开中的求值顺序:

    ```LISP
    (push value (wrong-order place1 place2))
    ```

2. 对于操纵 places (push, pushnew, remf, incf, decf, shiftf, rotatef, psetf, setf, pop, 还有那些 define-modify-macro 定义的宏) 的宏, 宏调用的子表达式按从左到右的顺序求值一次, 其中这些 place 的子表达式按照 (1) 中指定的顺序求值.

    push, pushnew, remf, incf, decf, shiftf, rotatef, psetf, pop 在修改任意 place 之前求值所有子表达式. setf (当 setf 有超过两个参数的情况下) 在序列的每一对中执行它的操作. 比如, 在下面这个表达式中

    ```LISP
    (setf place1 value1 place2 value2 ...)
    ```

    子表达式 place1 和 value1 被求值, place1 指定的位置被修改去包含 value1 返回的值, 然后 setf 表达式的剩余部分按照类似的方式处理.

3. 对于 check-type, ctypecase, 和 ccase, place 的子表达式像 (1) 中那样被求值一次, 但是如果 check-type 中类型检测失败了或者 ctypecase 和 ccase 中没有 case 被处理可能会再次求值.

4. 对于 assert, 这个 generalized references 求值顺序没有被指定.

规则 2, 3 和 4 覆盖所有操作 place 的标准化宏.

##### 5.1.1.1.1 对 Places 的子表达式求值的示例

```LISP
(let ((ref2 (list '())))
  (push (progn (princ "1") 'ref-1)
        (car (progn (princ "2") ref2))))
>>  12
=>  (REF1)

(let (x)
  (push (setq x (list 'a))
        (car (setq x (list 'b))))
    x)
=>  (((A) . B))
```

push 先求值 (setq x (list 'a)) => (a), 然后求值 (setq x (list 'b)) => (b), 再修改最后的值的 car 部分为 ((a) . b).

#### 5.1.1.2 <span id="SetfExpansions">Setf 展开</span>

有时避免多次求值一个 place 的子表达式或以错误的顺序求值是可能的. 对于一个给定的表达式形式, 一个 setf 展开可以被表达为一个五个对象的有序集合:

临时对象的列表

    一个命名临时变量的符号列表, 这些符号被顺序绑定到由值表达式形式返回的值, 就像是被 let*.

值表达式形式列表

    当求值时, 会产生相应的临时变量应该被绑定的值的表达式列表 (典型地, place 的子表达式).

存储变量的列表

    一个用来命名临时存储变量的符号列表, 用来保存将被分配给 place 的新值.

存储表达式形式

    一种可以同时引用临时变量和存储变量的表达式形式, 它会改变 place 的值, 并保证返回值, 因为它的值是存储变量的值, 这是 setf 返回的正确值.<!-- TODO 待校验 -->

访问表达式形式

    一个可以引用临时变量并且返回这个 place 的值的表达式形式.

访问表达式返回的值受到存储表达式的执行的影响, 但是这些表达式中的任何一种都可能被多次求值.

可以通过 psetf, shiftf 和 rotatef 并行执行多个 setf. 由于这个, setf 展开式必须每次产生新的临时和存储变量. 关于如果去做这个的例子, 见 gensym.

对于每一个标准化的访问器函数 F, 除非它被显式地记载, 否则使用一个 F 表达式形式作为一个 setf place 的能力被实现为通过一个 setf 展开式还是一个 setf 函数, 这是依赖于具体实现的. 同样, 名字 (setf F) 是否被 fbound 是依赖于具体实现的.

##### 5.1.1.2.1 Setf 展开式的示例

下面是 setf 展开式成分内容的示例.

对于一个变量 x:

```LISP
()              ;list of temporary variables
()              ;list of value forms
(g0001)         ;list of store variables
(setq x g0001)  ;storing form
x               ;accessing form
```

Figure 5-3. 一个变量的简单 Setf 展开式

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

在一些情况下, 如果一个 place 的一个子表达式自身也是一个 place, 为了计算外面的 place 展开式的一些值, 展开子表达式是必要的. 对于 (ldb bs (car exp)):

```LISP
(g0001 g0002)            ;list of temporary variables
(bs exp)                 ;list of value forms
(g0003)                  ;list of store variables
(progn (rplaca g0002 (dpb g0003 g0001 (car g0002))) g0003)
                         ;storing form
(ldb g0001 (car g0002))  ; accessing form
```

Figure 5-6. LDB 表达式形式的简单 setf 展开式

### 5.1.2 <span id="KindsOfPlaces">Places 的种类</span>

Common Lisp 定义了多个 place 的种类; 这个章节会列举它们. 这个集合可以被具体实现和程序员的代码所扩展.

> * 5.1.2.1 [变量名作为 Places](#VariableNamesPlaces)
> * 5.1.2.2 [函数调用表达式作为 Places](#FunctionCallFormsPlaces)
> * 5.1.2.3 [VALUES 表达式形式作为 Places](#VALUESFormsPlaces)
> * 5.1.2.4 [THE 表达式形式作为 Places](#THEFormsPlaces)
> * 5.1.2.5 [APPLY 表达式形式作为 Places](#APPLYFormsPlaces)
> * 5.1.2.6 [Setf展开和Places](#SetfExpansionsPlaces)
> * 5.1.2.7 [Macro 表达式形式作为 Places](#MacroFormsPlaces)
> * 5.1.2.8 [符号宏作为 Places](#SymbolMacrosPlaces)
> * 5.1.2.9 [其他复合表达式形式作为 Places](#OtherCompoundFormsPlaces)

#### 5.1.2.1 <span id="VariableNamesPlaces">变量名作为 Places</span>

一个词法变量或动态变量的名字可以被用作一个 place.

#### 5.1.2.2 <span id="FunctionCallFormsPlaces">函数调用表达式作为 Places</span>

如果一个函数表达式属于下列类别之一, 它被用作 place:

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

    subseq 的情况, 替换的值必须是一个序列, 其元素可能被 subseq 的序列参数所包含, 但它不一定是与指定子序列的类型相同的序列. 如果替换的值的长度和要被替换的子序列长度不一样, 那么更短的那个长度决定要被存储的元素的数量, 如 replace.

* 第一个元素是 defstruct 构造的选择器函数的名字的一个函数调用表达式形式. 这个函数名字必须引用全局函数定义, 而不是一个局部定义的函数.

* 第一个元素是下面这段中任何一个函数名的函数调用表达式形式, 只要给那个函数的提供的参数依次为一个 place 表达式; 在本例中, 新 place 存储回了调用提供的"update"函数的结果中.

    函数名          是 place 的参数            使用的 update 函数
    ldb            second                    dpb
    mask-field     second                    deposit-field
    getf           first                     implementation-dependent

    Figure 5-8. 可以和 setf 一起使用的函数---2 
    
    在这些表达式的 setf 展开期间, 需要调用 get-setf-expansion, 以了解内部的, 嵌套的广义变量必须如何处理.

    来自 get-setf-expansion 的信息用于以下内容.

    * ldb

        在像这样的表达式中:

        ```LISP
        (setf (ldb byte-spec place-form) value-form)
        ```
        
        place-form 涉及的 place 必须总是可读写的; 注意这个更新是对 place-form 指定的广义变量, 不是任何 integer 类型的对象.

        因此, setf 应该生成代码来执行以下操作:

        1. 求值 byte-spec (并且把它绑定到一个临时变量).
        2. 为 place-form 绑定临时变量.
        3. 求值 value-form (并绑定它的值或多值到存储变量中).
        4. 执行从 place-form 中读取.
        5. 用第4步中获取的整数的给定位来写 place-form, 替换为步骤3中的值.

        如果步骤3中的 value-form 求值修改了 place-form 的内容, 就像设置 integer 的不同的位, 那么 byte-spec 表示的改变的位是修改后的 integer, 因为步骤4在 value-form 求值后被执行. 不过, 绑定临时变量所需的求值在步骤1和步骤2中完成, 因此可以看到预期的从左到右的求值顺序. 比如:

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

        这个情况和 ldb 在所有重要情况下都是一样的.

    * getf

        在像这样的表达式中:

        ```LISP
        (setf (getf place-form ind-form) value-form)
        ```
        
        place-form 涉及的 place 必须总是可读写的; 注意这个更新是对 place-form 指定的广义变量, 而不一定是特定列表中讨论中的属性列表. <!--TODO in question ??-->

        因此, setf 应该生成代码来执行以下操作:

        1. 为 place-form 绑定临时变量.
        2. 求值 ind-form (并且把它绑定到临时变量).
        3. 求值 value-form (并且绑定它的值或多值到存储变量中).
        4. 执行从 place-form 中读取.
        5. 用通过组合步骤2, 3 和 4 中的值获取的可能是新的属性列表写到 place-form 中. (注意这个措辞 "可能的新属性列表(possibly-new property list)" 可能意味着之前的属性列表以某种方式破坏性的再次使用, 或者可能意味着它的部分或完全的复制. 因为不管是复制还是破坏性地再使用都可以发生, 对于可能的新属性列表的结果值的处理必须进行处理, 就好像它是一个不同的副本, 需要将其存储回广义变量中.)

        如果步骤3中的 value-form 求值修改了 place-form 中的内容, 就像在列表设置一个不同的已命名属性, 那么 ind-form 表示的属性的修改是那个修改后的列表, 因为步骤 4 在 value-form 求值后执行. 不过, 绑定临时变量所需的求值在步骤1和步骤2中完成, 因此可以看到预期的从左到右的求值顺序.

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

#### 5.1.2.3 <span id="VALUESFormsPlaces">VALUES 表达式形式作为 Places</span>

一个 values 表达式形式可以被用作一个 place, 只要它的每一个子表达式也是一个 place 表达式形式.

一个像这样的表达式形式

```LISP
(setf (values place-1 ...place-n) values-form)
```

执行了以下动作:

1. 每一个嵌套 place 的子表达式都按照从左到右的顺序求值.
2. 这个 values-form 被求值, 并且每一个 place 的 第一个存储变量被绑定给它的返回值就像是通过 multiple-value-bind 的一样.
3. 如果这个 setf 展开式对于任何 place 涉及超过一个存储变量, 那么额外的存储变量会绑定为 nil.
4. 每个 place 的存储表达式形式都按照从左到右的顺序计算.

values 的 setf 展开中的存储形式作为多个值返回第2步中存储变量的值. 这也就是说, 返回的值的数量和 place 表达式数量一样. 这个可能比 values-form 产生的值更多或更少.

#### 5.1.2.4 <span id="THEFormsPlaces">THE 表达式形式作为 Places</span>

一个 the 表达式形式可以被用作一个 place, 在这个情况下这个声明被转到那个新值表达式形式中, 然后对结果的 setf 进行分析. 比如,

```LISP
(setf (the integer (cadr x)) (+ y 3))
```

被处理就好像它是这样的

```LISP
(setf (cadr x) (the integer (+ y 3)))
```

#### 5.1.2.5 <span id="APPLYFormsPlaces">APPLY 表达式形式作为 Places</span>

以下 apply 的 setf 情况必须被支持:

* (setf (apply #'aref array subscript* more-subscripts) new-element)
* (setf (apply #'bit array subscript* more-subscripts) new-element)
* (setf (apply #'sbit array subscript* more-subscripts) new-element)

在所有这三种情况中, 由 subscripts 和 more-subscripts 连接所指定的 array 的数组元素 (换句话说, 同样的元素, 如果不是 setf 表达式的一部分, 它将通过调用 apply 来读取) 被改变为 new-element 给定的值. 对于这些用法, 函数名 (aref, bit, or sbit) 必须引用全局函数定义, 而不是局部函数定义.

没有其他标准化的函数需要被支持, 但是一个具体实现可以定义这样的支持. 一个具体实现可能也可以为具体实现所定义的操作符定义支持.

如果一个用户定义的函数被用于这个上下文, 那么以下等价是成立的, 除了要注意保留对参数子表达式的正确的从左到右的求值:

```LISP
(setf (apply #'name arg*) val)
==  (apply #'(setf name) val arg*)
```

#### 5.1.2.6 <span id="SetfExpansionsPlaces">Setf展开和Places</span>

任何有着 setf 展开器定义的操作符的复合表达式形式可以被用作一个 place. 这个操作符必须引用一个全局函数定义, 而不是一个局部定义的函数或宏.

#### 5.1.2.7 <span id="MacroFormsPlaces">Macro 表达式形式作为 Places</span>

一个宏表达式形式可以被用作 place, 在这个情况下 Common Lisp 展开那个宏表达式形式就像是通过  macroexpand-1 然后用这个宏展开式替换原始的 place. 这样的宏展开只在耗尽所有其他可能性之后才尝试, 而不是展开到调用一个名为 (setf reader) 的函数.

#### 5.1.2.8 <span id="SymbolMacrosPlaces">符号宏作为 Places</span>

对一个已经确定是符号宏引用的符号可以被用作一个 place. 在这个情况下, setf 展开这个引用并且分析结果表达式.

#### 5.1.2.9 <span id="OtherCompoundFormsPlaces">其他复合表达式形式作为 Places</span>

对于其他任何复合表达式形式, 其中操作符为符号 f, 这个 setf 表达式形式展开为一个对名为 (setf f) 的函数调用. 这个新构建的函数的第一个参数是 newvalue 并且剩下的参数是 place 的剩余元素. 不管 f 或者 (setf f) 是局部函数还是全局函数或者都不是, 这个展开都会发生. 比如,

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

一个名为 (setf f) 的函数必须返回它的第一个参数作为它唯一的值, 以便保留 setf 的语义.

### 5.1.3 <span id="TreatmentMacrosSETF">基于 SETF 的其他宏的处理</span>

对于下一段中的每个 "读-修改-写(read-modify-write)" 操作符, 还有对于任何程序员使用 define-modify-macro 定义的额外的宏, 一个异常是由对参数的左至右求值的正常规则所造成的. 对参数表达式形式的求值以从左到右的顺序发生, 而对于 place 参数来说, 从那个 place 的"旧值"的实际读取发生在所有的参数表达式形式求值之后, 并且在一个"新值"被计算并重新写入到该 place 之前.

具体地说, 每个操作符都可以被看作是包含以下通用语法的表达式形式:

```LISP
(operator preceding-form* place following-form*)
```

对每个这样的表达式形式的求值都是这样的:

1. 按从左到右的顺序求值每一个 preceding-forms.
2. 求值 place 的子表达式, 按照这个 place 的 setf 展开式的第二个值指定的顺序.
3. 按从左到右的顺序求值每一个 following-forms.
4. 从 place 中读取旧的值.
5. 计算新的值.
6. 存储新的值到 place.

    decf  pop   pushnew
    incf  push  remf

    Figure 5-9. 读-修改-写(read-modify-write) 宏

## 5.2 <span id="TCEP">退出点的控制转移</span>

当控制转移由 go, return-from, 或 throw 发起时, 为了完成控制权的转移, 发生以下事件. 注意, 对于 go, 退出点是 go 执行时要被执行的 tagbody 里的表达式形式; 对于 return-from, 退出点是对应的 block 表达式形式; 对于 throw, 退出点是对应的 catch 表达式形式.

1. 中间的退出点被"抛弃"了 (换句话说, 它们的范围结束了, 试图通过它们来转移控制已经不再有效了).

2. 对任何中间 unwind-protect 子句的清理子句进行了求值.

3. 其中 special 变量, 捕捉标签, 状况处理者, 还有重启器的动态绑定被消除.

4. 被调用的退出点的范围结束, 控制被传递给目标.

退出的范围被"抛弃", 因为一旦控制转移, 它就会被传递到结束 The extent of an exit being "abandoned" because it is being passed over ends as soon as the transfer of control is initiated. 这也就是说, 事件 1 发生在控制转移的开始. 如果尝试去转移控制到一个动态范围已经结束的退出点, 那么结果是未定义的.

事件 2 和 3 实际上是交替进行的, 顺序与它们建立的逆序相对应. 这样做的效果是一个 unwind-protect 的清理子句看到进入 unwind-protect 时变量和捕捉标签(catch tags)的相同童待绑定.

事件 4 发生在控制转移结束的时候.

## 5.3 <span id="DCFDictionary">数据和控制流字典</span>

> * [函数 APPLY](#)
> * [宏 DEFUN](#)
> * [访问器 FDEFINITION](#)
> * [函数 FBOUNDP](#)
> * [函数 FMAKUNBOUND](#)
> * [特殊操作符 FLET, LABELS, MACROLET](#)
> * [函数 FUNCALL](#)
> * [特殊操作符 FUNCTION](#)
> * [函数 FUNCTION-LAMBDA-EXPRESSION](#)
> * [函数 FUNCTIONP](#)
> * [函数 COMPILED-FUNCTION-P](#)
> * [常量 CALL-ARGUMENTS-LIMIT](#)
> * [常量 LAMBDA-LIST-KEYWORDS](#)
> * [常量 LAMBDA-PARAMETERS-LIMIT](#)
> * [宏 DEFCONSTANT](#)
> * [宏 DEFPARAMETER, DEFVAR](#)
> * [宏 DESTRUCTURING-BIND](#)
> * [特殊操作符 LET, LET*](#)
> * [特殊操作符 PROGV](#)
> * [特殊表达式 SETQ](#)
> * [宏 PSETQ](#)
> * [特殊操作符 BLOCK](#)
> * [特殊操作符 CATCH](#)
> * [特殊操作符 GO](#)
> * [特殊操作符 RETURN-FROM](#)
> * [宏 RETURN](#)
> * [特殊操作符 TAGBODY](#)
> * [特殊操作符 THROW](#)
> * [特殊操作符 UNWIND-PROTECT](#)
> * [常量 NIL](#)
> * [函数 NOT](#)
> * [常量 T](#)
> * [函数 EQ](#)
> * [函数 EQL](#)
> * [函数 EQUAL](#)
> * [函数 EQUALP](#)
> * [函数 IDENTITY](#)
> * [函数 COMPLEMENT](#)
> * [函数 CONSTANTLY](#)
> * [函数 EVERY, SOME, NOTEVERY, NOTANY](#)
> * [宏 AND](#)
> * [宏 COND](#)
> * [特殊操作符 IF](#)
> * [宏 OR](#)
> * [宏 WHEN, UNLESS](#)
> * [宏 CASE, CCASE, ECASE](#)
> * [宏 TYPECASE, CTYPECASE, ETYPECASE](#)
> * [宏 MULTIPLE-VALUE-BIND](#)
> * [特殊操作符 MULTIPLE-VALUE-CALL](#)
> * [宏 MULTIPLE-VALUE-LIST](#)
> * [特殊操作符 MULTIPLE-VALUE-PROG1](#)
> * [宏 MULTIPLE-VALUE-SETQ](#)
> * [访问器 VALUES](#)
> * [函数 VALUES-LIST](#)
> * [常量 MULTIPLE-VALUES-LIMIT](#)
> * [宏 NTH-VALUE](#)
> * [宏 PROG, PROG*](#)
> * [宏 PROG1, PROG2](#)
> * [特殊操作符 PROGN](#)
> * [宏 DEFINE-MODIFY-MACRO](#)
> * [宏 DEFSETF](#)
> * [宏 DEFINE-SETF-EXPANDER](#)
> * [函数 GET-SETF-EXPANSION](#)
> * [宏 SETF, PSETF](#)
> * [宏 SHIFTF](#)
> * [宏 ROTATEF](#)
> * [状况类型 CONTROL-ERROR](#)
> * [状况类型 PROGRAM-ERROR](#)
> * [状况类型 UNDEFINED-FUNCTION](#)



### <span id="">函数 APPLY</span>

* 语法(Syntax):

        apply function &rest args+ => result*

* 参数和值(Arguments and Values):

        function---一个函数指定符.
        args---一个可扩展的参数列表指示符.
        results---function 的返回值.

* 描述(Description):

        将 args 应用给 function.

        当这个 function 通过 &rest 接受到它的参数时, 对于一个实现, 允许 (但不是必须) 去绑定剩余参数到一个和 apply 的最后一个参数共享结构的对象. 因为一个函数既不能检测它是否通过 apply 调用, 也不能检测到 apply 的最后一个参数是否是一个常量, 符合规范的程序必须既不能依赖于rest列表的列表结构, 也不能修改这个列表结构.

        在某些情况下, setf可以用于 apply; 见章节 5.1.2.5 (APPLY Forms as Places).

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

        funcall, fdefinition, function, Section 3.1 (Evaluation), Section 5.1.2.5 (APPLY Forms as Places)

* 注意(Notes): None.


### <span id="">宏 DEFUN</span>

* 语法(Syntax):

        defun function-name lambda-list [[declaration* | documentation]] form*

        => function-name

* 参数和值(Arguments and Values):

        function-name---一个函数名.
        lambda-list---一个普通 lambda 列表.
        declaration---一个 declare 表达式; 不求值.
        documentation---一个字符串; 不求值.
        forms---一个隐式的 progn.
        block-name---这个 function-name 的函数块名字.

* 描述(Description):

        在全局环境中定义一个名为 function-name 的函数. 由 defun 定义的函数的 body 部分由 表达式形式组成; 当函数被调用时它们会作为一个隐式的 progn 被执行. defun 可以被用于定义一个新的函数, 去安装一个错误定义的修正版本, 去重定义一个已经定义的函数, 或者把一个宏重定义为函数.

        defun 隐式地把一个名为 block-name 的 block 放在定义的函数的 body 表达式周围 (但是不是 lambda-list 中的表达式形式).

        documentation 作为一个文档字符串附加到 name(作为函数) 和函数对象.

        求值 defun 导致 function-name 成为 defun 被执行的词法环境中的 lambda 表达式

        (lambda lambda-list
          [[declaration* | documentation]]
          (block block-name form*))

        所表示函数的全局名字的函数.

        (参数中没有在宏展开时被求值的.)

        defun 不需要去产生任何编译时副作用. 具体来说, defun 不会使函数定义在编译时可用. 一个实现可能选择去存储关于这个函数的信息用于编译时错误检测的目的 (比如检测一个调用的参数数量), 或者去使函数被内联展开.

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

        flet, labels, block, return-from, declare, documentation, Section 3.1 (Evaluation), Section 3.4.1 (Ordinary Lambda Lists), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

        return-from 可以用于从 defun 定义的函数中提前返回.

        当关于这个函数的额外信息(通常是调试信息)被记录时, 可能发生额外的副作用.


### <span id="">访问器 FDEFINITION</span>

* 语法(Syntax):

        fdefinition function-name => definition

        (setf (fdefinition function-name) new-definition)

* 参数和值(Arguments and Values):

        function-name---一个函数名. 在不是 setf 情况下, 这个名字在全局环境中必须被 fbound.

        definition---function-name 命名的当前全局函数定义.

        new-definition---一个函数.

* 描述(Description):

fdefinition 访问 function-name 命名的当前全局函数定义. 这个定义可能是一个函数或表示一个特殊表达式或宏的对象. 当 fboundp 返回 true 但是这个 function-name 表示一个宏或者特殊表达式时, fdefinition 返回的值是没有定义好的, 但是 fdefinition 不发出一个错误.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 function-name 不是一个函数名字, 那么应该发出一个 type-error 类型的错误.

        在不是 setf 的情况下如果 function-name 没有被 fbound, 那么就会发出 undefined-function 类型的错误.

* 也见(See Also):

        fboundp, fmakunbound, macro-function, special-operator-p, symbol-function

* 注意(Notes):

        fdefinition 不能访问由 flet 或 labels 产生的词法函数名的值; 它只能访问全局函数的值.

        当一个函数名对应的函数定义不表示一个特殊表达式时, setf 可以和 fdefinition 一起使用去替换全局函数定义. fdefinition 的 setf 需要一个函数作为新的值. 把 function-name 的 fdefinition 设置为一个符号, 一个列表, 或者在一个宏或特殊表达式上调用 fdefinition 返回的值是错误的.


### <span id="">函数 FBOUNDP</span>

* 语法(Syntax):

        fboundp name => generalized-boolean

* 发音(Pronunciation):

        [,ef'bandpee]

* 参数和值(Arguments and Values):

        name---一个函数名字.
        generalized-boolean---一个广义的 boolean.

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

        如果 name 不是一个函数名字, 应该会发出一个 type-error 类型的错误.

* 也见(See Also):

        symbol-function, fmakunbound, fdefinition

* 注意(Notes):

允许在任何被 fbound 的符号上调用 symbol-function.

fboundp 有时候被用于 "保护" 对函数 cell 的访问, 就像:

```LISP
(if (fboundp x) (symbol-function x))
```

定义一个 setf 展开器 F 不会导致 setf 函数 (setf F) 被定义.


### <span id="">函数 FMAKUNBOUND</span>

* 语法(Syntax):

        fmakunbound name => name

* 发音(Pronunciation):

        [,ef'makuhn,band] 或 [,ef'maykuhn,band]

* 参数和值(Arguments and Values):

        name---一个函数名字.

* 描述(Description):

        在全局环境中删除函数或宏定义, 如果有的话.

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

        如果 name 不是一个函数名字, 应该发出一个 type-error 类型的错误.

        如果 name 是一个特殊操作符那么结果是未定义的.

* 也见(See Also):

        fboundp, makunbound

* 注意(Notes): None.

### <span id="">特殊操作符 FLET, LABELS, MACROLET</span>

* 语法(Syntax):

        flet ((function-name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*

        => result*

        labels ((function-name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*

        => result*

        macrolet ((name lambda-list [[local-declaration* | local-documentation]] local-form*)*) declaration* form*

        => result*

* 参数和值(Arguments and Values):

        function-name---一个函数名字.
        name---一个符号.
        lambda-list---一个 lambda 列表; 对于 flet 和 labels, 它是一个普通 lambda 列表; 对于 macrolet, 它是一个宏 lambda 列表.
        local-declaration---一个 declare 表达式; 不求值.
        declaration---一个 declare 表达式; 不求值.
        local-documentation---一个字符串; 不求值.
        local-forms, forms---一个隐式的 progn.
        results---forms 的值.

* 描述(Description):

        flet, labels, 和 macrolet 定义局部函数和宏, 并且使用局部定义执行 forms. Forms 以出现的顺序被执行.

        每一个被 flet 和 labels 创建的函数和每一个被 macrolet 创建的宏的主体表达式 (但是不是 lambda 列表) 被封闭在一个隐式的 block 中, 它的名字为 function-name 或 name 的函数块名字.

        在局部函数/宏定义和 flet 或 labels 之间的声明的作用域不包括局部定义的函数的主体, 除了对于 labels , 任何引用局部定义函数的 inline, notinline, 或 ftype 声明可以应用于局部定义函数的主体. 这就是说, 它们的作用于和它们影响的函数名一样. 这个声明的作用于不包括 macrolet 定义的这个宏展开函数的主体.

        flet

            flet 定义局部命名函数并且用这些定义绑定执行一连串的表达式形式. 可以定义任意数量的局部函数.

            名称绑定的作用域仅包含主体(body)部分. 在 flet 的主体中, 与 flet 定义的 function-names 对应的是局部定义的函数, 而不是同名的全局函数定义. 此外, 在 flet 的作用域内, 由 flet 定义的函数名的全局 setf 展开器定义不适用. 注意这个适用于 (defsetf f ...), 不是 (defmethod (setf f) ...).

            flet 定义的函数的名字是在词法环境中的; 它们仅在 flet 的主体中保持他们的局部定义. 这个函数定义绑定仅在 flet 的主体 (body) 中可见, 在定义自身中不可见. 在这个函数定义中, 匹配那些要被定义的局部函数名字引用的是那些在 flet 外面定义的局部函数或宏. flet 可以在局部遮蔽一个全局的名字, 并且这个新的定义可以引用全局定义.

            任何局部文档都作为一个文档字符串被附加到相应的局部函数中(如果实际创建的话).

        labels

            labels 等价于 flet 除了 labels 定义的函数名的范围包含函数定义本身以及主体.

        macrolet

            macrolet 建立一个局部宏定义, 使用和 defmacro 相同的格式.

            在 macrolet 的主体中, macrolet 定义的名字对应的全局 setf 展开器定义是不适用的; 相反, setf 展开宏表达式并递归地处理生成的表达式.

            macrolet 定义的宏展开函数是定义在 macrolet 表达式出现的词法作用域中的. 声明和  macrolet 和 symbol-macrolet 定义影响一个 macrolet 中的局部宏定义, 但是, 如果局部宏定义引用在该词法环境中可见的任何局部变量或函数绑定, 那么后果是没有定义的.

            任何局部文档都作为一个文档字符串被附加到相应的局部宏函数中.

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

        declare, defmacro, defun, documentation, let, Section 3.1 (Evaluation), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

        用 flet 去定义递归的函数是可以的. labels 可以被用于定义相互递归函数.

        如果一个 macrolet 表达式形式是一个顶层表达式, 主体 forms 也被当作顶层表达式处理. 见章节 3.2.3 (File Compilation).


### <span id="">函数 FUNCALL</span>

* 语法(Syntax):

        funcall function &rest args => result*

* 参数和值(Arguments and Values):

        function---一个函数指定符.
        args---给这个 function 的参数.
        results---这个 function 返回的值.

* 描述(Description):

        funcall 对 args 应用 function. 如果 function 是一个符号, 它会被强制转为一个函数就好像在全局环境中找它的函数函数性值.

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

        如果 function 是一个没有作为函数的全局定义或者全局定义是宏或特殊操作符的符号, 应该会发出一个 undefined-function 类型的错误.

* 也见(See Also):

        apply, function, Section 3.1 (Evaluation)

* 注意(Notes):

    ```LISP
    (funcall function arg1 arg2 ...)
    ==  (apply function arg1 arg2 ... nil)
    ==  (apply function (list arg1 arg2 ...))
    ```

        funcall 和一个普通函数调用的区别在于, 在前一种情况下, 函数是通过对一种表达式形式的普通求值得到的, 在后者的情况下, 它是由正常发生的函数位置的特殊解释得到的.


### <span id="">特殊操作符 FUNCTION</span>

* 语法(Syntax):

        function name => function

* 参数和值(Arguments and Values):

        name---一个函数名字或者一个 lambda 表达式.
        function---一个函数对象.

* 描述(Description):

        function 的值是当前词汇环境中 name 的函数值.

        如果 name 是一个函数名, 这个名称的函数定义是由最内部的词法闭包的 flet, labels, 或macrolet 表达式形式建立的, 如果有的话. 否则会返回这个函数名的全局函数定义会被返回.

        如果 name 是一个 lambda 表达式, 那么返回一个词法闭包. 如果在同一组绑定上的闭包可能产生不止一次的情况, 那么各种结果的闭包可能也可能不是 eq 的.

        在一个 function 表达式形式出现的词法环境中在一个不表示函数的函数名上用 function 是错误的. 具体来说, 在一个表示宏或者特殊表达式的符号上使用 function 是错误的. 一个具体实现可能出于性能原因不去发出这个错误, 但是具体实现禁止去定义这个发送错误的失败为一个有用的行为. <!-- TODO the failure to signal an error ?? -->

* 示例(Examples):

    ```LISP
    (defun adder (x) (function (lambda (y) (+ x y))))
    ```

        这个 (adder 3) 的结果是一个把 3 加给参数的函数:

    ```LISP
    (setq add3 (adder 3))
    (funcall add3 5) =>  8
    ```

        这个可以正常工作是因为 function 创建了一个 lambda 表达式的闭包, 这个闭包引用了变量 x 的值 3, 即便控制流已经中函数 adder 中返回了.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defun, fdefinition, flet, labels, symbol-function, Section 3.1.2.1.1 (Symbols as Forms), Section 2.4.8.2 (Sharpsign Single-Quote), Section 22.1.3.13 (Printing Other Objects)

* 注意(Notes):

        标记 #'name 可能被用于 (function name) 的缩写.


### <span id="">函数 FUNCTION-LAMBDA-EXPRESSION</span>

* 语法(Syntax):

        function-lambda-expression function
        => lambda-expression, closure-p, name

* 参数和值(Arguments and Values):

        function---一个函数.
        lambda-expression---一个 lambda 表达式或 nil.
        closure-p---一个广义的 boolean.
        name---一个对象.

* 描述(Description):

        返回关于函数的信息:

        这个主要的值, lambda-expression, 是函数的定义 lambda 表达式, 如果这个信息不可用就是 nil. 这个 lambda 表达式可能已经在某些方面进行了预处理，但它仍然应该是 compile 或function 的一个合适的参数. 任何具体实现可能会合理地返回 nil 作为 任何 function 的 lambda-expression.

        第二个值, closure-p, 如果函数的定义是 null 词法作用域中的闭包那么就是 nil, 如果是非 null 词法作用域那么就不是 nil. 任何具体实现可能合理地返回 true 作为任何 function 的 closure-p.

        第三个值, name, 是 function 的 "名字(name)". 该名称仅用于调试, 并不一定是在 defun 或 function 中用作名称, 例如. 按照惯例, nil 被用于表示这个 function 没有名字. 任何具体实现可能合理地返回 nil 作为任何 function 的名字.

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

        虽然具体实现可以在所有情况下返回 "nil, true, nil", 但是还是鼓励在参数是对 compile 或 eval 的调用所创建的情况下返回一个 lambda 表达式作为主要的值 (与通过加载已编译的文件而创建的相反).


### <span id="">函数 FUNCTIONP</span>

* 语法(Syntax):

        functionp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果 object 是 function 类型返回 true; 否则, 返回 false.

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


### <span id="">函数 COMPILED-FUNCTION-P</span>

* 语法(Syntax):

        compiled-function-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果对象是 compiled-function 类型的对象就返回 true; 否则, 返回 false.

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


### <span id="">常量 CALL-ARGUMENTS-LIMIT</span>

* 常量值(Constant Value):

        一个不小于 50 的整数, 至少和 lambda-parameters-limit 的值一样大, 它的精确大小是与具体实现相关的.

* 描述(Description):

        传递给函数的参数数量的上限.

* 示例(Examples): None.

* 也见(See Also):

        lambda-parameters-limit, multiple-values-limit

* 注意(Notes): None.


### <span id="">常量 LAMBDA-LIST-KEYWORDS</span>

* 常量值(Constant Value):

        一个列表, 其中的元素是依赖于具体实现的, 但是其中至少需要包含 符号 &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, 和 &whole.

* 描述(Description):

        在具体实现中使用的所有 lambda 列表关键字的列表, 包括仅由宏定义表达式使用的其他关键词.

* 示例(Examples): None.

* 也见(See Also):

        defun, flet, defmacro, macrolet, Section 3.1.2 (The Evaluation Model)

* 注意(Notes): None.

### <span id="">常量 LAMBDA-PARAMETERS-LIMIT</span>

* 常量值(Constant Value):

        依赖于具体实现, 但是不小于 50.

* 描述(Description):

        可以出现在单个 lambda 列表中的参数名字数量的上限.

* 示例(Examples): None.

* 也见(See Also):

        call-arguments-limit

* 注意(Notes):

        鼓励实现者去使 lambda-parameters-limit 这个参数的值尽可能的大.

### <span id="">宏 DEFCONSTANT</span>

* 语法(Syntax):

        defconstant name initial-value [documentation] => name

* 参数和值(Arguments and Values):

        name---一个符号; 不求值.
        initial-value---一个表达式形式; 求值.
        documentation---一个字符串; 不求值.

* 描述(Description):

        defconstant 导致 name 命名的全局变量赋予 initial-value 求值结果的值.

        一个 defconstant 定义的常量可以被 defconstant 重定义. 然而, 如果尝试去使用其他操作符去给这个符号赋值或者使用后面的 defconstant 将其赋给不同的值, 那么结果是未定义的.<!-- TODO ?? -->

        如果提供了 documentation, 它会作为一个 variable 种类的文档字符串关联到 name 上.

        defconstant 通常以顶层表达式出现, 但它作为非顶层表达式出现也是有意义的. 然而, 下面描述的编译时副作用只发生在 defconstant 以顶层表达式出现的情况.

        如果在 defconstant 被执行时存在任何 name 命名变量的绑定或者其中的值和 initial-value 不是 eql 的, 那么结果是不可预料的.

        当常量符号作为词法或动态变量重绑定时, 其结果是不确定的. 换句话说, 一个指向 defconstant 声明的符号的引用总是指向它的全局的值.

        执行 defconstant 的副作用必须与以下代码执行的副作用相等:


    ```LISP
    (setf (symbol-value 'name) initial-value)
    (setf (documentation 'name 'variable) 'documentation)
    ```

        如果 defconstant 作为顶层表达式形式出现, 编译器必须识别出 name 是一个常量变量. 一个具体实现可能选择在编译时或加载时或都去求值 value-form. 因此, 用户必须确保 initial-value 可以在编译时被求值 (不管是否在文件中出现了引用 name) 并且总是求值为同一个值.

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

        declaim, defparameter, defvar, documentation, proclaim, Section 3.1.2.1.1.3 (Constant Variables), Section 3.2 (Compilation)

* 注意(Notes): None.


### <span id="">宏 DEFPARAMETER, DEFVAR</span>

* 语法(Syntax):

        defparameter name initial-value [documentation] => name
        defvar name [initial-value [documentation]] => name

* 参数和值(Arguments and Values):

        name---一个符号; 不求值.
        initial-value---一个表达式形式; 对于 defparameter, 它总是被求值, 但是对于 defvar 只有在 name 还没有被绑定的情况下求值.
        documentation---一个字符串; 不求值.

* 描述(Description):

        defparameter 和 defvar 确立 name 为一个动态变量.

        defparameter 无条件地赋 initial-value 给 name 命名的动态变量. defvar, 相比之下, 只有在 name 还没有被绑定时赋 initial-value (如果提供的话) 给 name 命名的动态变量.

        如果没有提供 initial-value, defvar 离开名为 name 的动态变量的值单元; 如果 name 以前绑定过的话, 它的旧值就会一直存在, 如果它之前没有绑定, 那么它仍然是未绑定的.

        如果提供了 documentation, 它会作为变量类型的文档字符串关联给 name.

        defparameter 和 defvar 通常作为顶层表达式形式出现, 但是当它们作为非顶层表达式出现时也是有意义的. 然而, 下面描述的编译时副作用仅发生在它们作为顶层表达式的时候.

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

        defparameter 和 defvar 之间的主要操作区别是, defparameter 做出了一个无条件的赋值, 而 defvar 则是一个有条件的赋值. 在实践中, 这就意味着在加载或重新加载定义想要获得变量的新值时 defparameter 是很有用的, 而当文件被加载或重新加载时想要保持旧值时 defvar 是比较有用的. 比如, 可以创建一个文件, 其中包含:

    ```LISP
    (defvar *the-interesting-numbers* '())
    (defmacro define-interesting-number (name n)
    `(progn (defvar ,name ,n)
            (pushnew ,name *the-interesting-numbers*)
            ',name))
    (define-interesting-number *my-height* 168) ;cm
    (define-interesting-number *my-weight* 13)  ;stones
    ```

        这里的初始值, (), 对于变量 *the-interesting-numbers* 只是一个种子, 一旦从中生长出什么时我们不会想去重置它. 像这样, 我们使用 defvar 来避免文件第二次加载时 *the-interesting-numbers* 信息被重置. 确实, 这里的两个对 define-interesting-number 调用会被重新处理, 但是如果在另一个文件中由其他调用, 它们不会被重新处理并且信息会丢失. 另一方面, 思考下面代码:

    ```LISP
    (defparameter *default-beep-count* 3)
    (defun beep (&optional (n *default-beep-count*))
    (dotimes (i n) (si:%beep 1000. 100000.) (sleep 0.1)))
    ```
    
        这里我们可以简单地想象编辑代码去改变 *default-beep-count* 的初始值, 然后重新载入文件去取出新的值. 为了使值更新更简单, 我们使用 defparameter.

        另一方面, 在这种情况下使用 defvar 是有潜在价值的. 比如, 假设某人为 *default-beep-count* 预先定义了一个替代值, 或者已经加载这个文件然后手动修改这个值. 在这两种情况下, 如果我们已经使用 defvar 而不是 defparameter, 那些用户设置不会因加载(或重新加载) 而被重写.

        使用 defparameter 还是 defvar 的选择对程序有明显的影响, 但通常是出于主观原因.

* 副作用(Side Effects):

        如果一个 defvar 或 defparameter 表达式形式作为顶层表达式出现, 编译器必须识别这个已经被全局声明为 special 的 name. 然而, 它既不能求值初始表达式, 也不能在编译时分配名为 name 的动态变量.

        这里可能由额外的编译期或运行期副作用 (依赖于具体实现), 只要这些副作用不影响符合规范的程序的正常操作.

* 受此影响(Affected By):

        defvar 受 name 是否已经被绑定的影响.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        declaim, defconstant, documentation, Section 3.2 (Compilation)

* 注意(Notes):

        通常, 在名称的开头和结尾用星号来命名动态变量. 比如, *foo* 对于一个动态变量是一个好名字, 但是对于一个词法变量则不是好名字; foo 对于词法变量是一个好名字, 但是对于动态变量不是一个好名字. 这种命名约定适用于 Common Lisp 中所有已定义的名称; 然而, 符合规范的程序和符合规范的具体实现都不是必须去遵守这个约定.

        允许额外的副作用的目的是让具体实现伴随着定义做正常的"记录". 比如, 一个 defvar or defparameter 的宏展开可能包含安排记录这个定义发生的源文件名的代码.

        defparameter 和 defvar 可能定义如下:

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

### <span id="">宏 DESTRUCTURING-BIND</span>

* 语法(Syntax):

        destructuring-bind lambda-list expression declaration* form*
        => result*

* 参数和值(Arguments and Values):

        lambda-list---一个解构 lambda 列表.
        expression---一个表达式形式.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式的 progn.
        results---forms 返回的结果.

* 描述(Description):

        destructuring-bind 绑定 lambda-list 中指定的变量到 expression 求值结果的树结构的对应值中; 然后 destructuring-bind 求值forms.

        支持解构的 lambda-list 在章节 3.4.5 (Destructuring Lambda Lists) 中有描述.

* 示例(Examples):

    ```LISP
    (defun iota (n) (loop for i from 1 to n collect i))       ;helper
    (destructuring-bind ((a &optional (b 'bee)) one two three)
        `((alpha) ,@(iota 3))
      (list a b three two one)) =>  (ALPHA BEE 3 2 1)
    ```
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果求值 expression 的结果不匹配这个解构模式, 应该会发出一个 error 类型的错误.

* 也见(See Also):

        macrolet, defmacro

* 注意(Notes): None.


### <span id="">特殊操作符 LET, LET*</span>

* 语法(Syntax):

        let ({var | (var [init-form])}*) declaration* form* => result*
        let* ({var | (var [init-form])}*) declaration* form* => result*

* 参数和值(Arguments and Values):

        var---一个符号.
        init-form---一个表达式形式.
        declaration---一个 declare 表达式; 不求值.
        form---一个表达式形式.
        results---forms 返回的结果.

* 描述(Description):

        let 和 let* 创建新的变量绑定并且使用这些绑定执行一系列的表达式形式. let 并行地执行这些绑定而 let* 顺序地执行.

        表达式形式

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

        首先求值表达式 init-form-1, init-form-2, 等等, 按照这个顺序, 保存结果值. 然后所有变量 varj 绑定为对应的值; 每一个绑定都是词法的除非有一个 special 声明. 表达式 formk 依次被求值; 除了最后一个, 所有的值都被丢弃 (这也就是说, 一个 let 的主体是一个隐式的 progn).

        let* 类似于 let, 但是变量的绑定被顺序执行而非并行执行. 一个 var 的 init-form 表达式可以引用前面 let* 绑定的 vars.

        表达式形式

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

        首先求值 init-form-1, 然后绑定变量 var1 为那个值; 然后求值 init-form-2 并绑定 var2, 等等. 然后表达式 formj 按顺序求值; 除了最后一个, 所有的值都被丢弃 (这也就是说, 一个 let* 的主体是一个隐式的 progn).

        对于 let 和 let*, 如果这里没有一个 init-form 关联 var, var 被初始化为 nil.

        特殊表达式 let 有一个属性, 名称绑定的范围不包括任何初始话值表达式. 对于 let*, 一个变量的作用域包括了剩下的后面变量绑定的初始化值表达式.

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

        是错误的; 虽然 x 事实上在它被使用前设置, 并且设置为一个声明类型整型的值, 然而, x 一开始在值为 nil 时违反了类型声明.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        progv

* 注意(Notes): None.


### <span id="">特殊操作符 PROGV</span>

* 语法(Syntax):

        progv symbols values form* => result*

* 参数和值(Arguments and Values):

        symbols---一个符号列表; 求值.
        values---一个对象列表; 求值.
        forms---一个隐式的 progn.
        results---forms 返回的值.

* 描述(Description):

        progv 创建新的动态变量绑定并且使用这些绑定执行每个 form. 每个 form 按顺序求值.

        progv 允许去绑定一个或多个动态变量, 这些变量的名字可能在运行时确定定. 每一种 form 都是根据动态变量来进行求值的, 这些变量的名称都与相应的值绑定在一起. 如果提供的值太少, 剩余的符号就会被绑定, 然后提出 have no value. 如果提供了太多的值, 多余的值会被忽略. 这些动态变量的绑定在 progv 退出时被取消了.

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

        let, Section 3.1 (Evaluation)

* 注意(Notes):

        除了别的之外, 为 Lisp 中的嵌入式语言写解释器时 progv 是很有用的; 它为绑定动态变量的机制提供了一个途径.


### <span id="">特殊表达式 SETQ</span>

* 语法(Syntax):

        setq {pair}* => result

        pair::= var form

* 发音(Pronunciation):

        ['set,kyoo]

* 参数和值(Arguments and Values):

        var---一个符号, 表示一个变量, 而不是一个常量.
        form---一个表达式形式.
        result---最后一个表达式形式返回的主要的值, 如果没有提供 pair 就返回 nil.

* 描述(Description):

        赋值给变量.

        (setq var1 form1 var2 form2 ...) 是 Lisp 的简单变量赋值语句. 首先 form1 被求值并且结果被存储在变量 var1 中, 然后 form2 被求值并且结果存在 var2 中, 等等. setq 可能被用于词法变量或动态变量的赋值.

        如果任何 var 引用了 symbol-macrolet 产生的一个绑定, 那么这个 var 被认为就好像是被 setf (不是 setq).

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

        每一个 form 的主要的值都被赋给对应的 var.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        psetq, set, setf

* 注意(Notes): None.


### <span id="">宏 PSETQ</span>

* 语法(Syntax):

        psetq {pair}* => nil

        pair::= var form

* 发音(Pronunciation):

        psetq: [;pee'set,kyoo]

* 参数和值(Arguments and Values):

        var---一个符号, 表示一个变量, 而不是一个常量.
        form---一个表达式形式.

* 描述(Description):

        赋值给变量.

        这个就像是 setq, 除了赋值是并行发生的之外. 这也就是说, 首先所有的表达式形式都被求值, 只有这样, 变量才会被设置成对应结果值. 通过这种方式, 对一个变量的赋值不会影响另一个变量的值计算, 这与 setq 的顺序赋值不同.

        如果任何 var 引用了 symbol-macrolet 创建的绑定, 那么这个 var 被认为就好像是被 psetf (不是 psetq).

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

        forms 的值会赋给 vars.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        psetf, setq

* 注意(Notes): None.


### <span id="">特殊操作符 BLOCK</span>

* 语法(Syntax):

        block name form* => result*

* 参数和值(Arguments and Values):

        name---一个符号.
        form---一个表达式形式.
        results---如果正常返回就是 forms 的值, 否则, 如果显式地返回, 就返回传递的值.

* 描述(Description):

        block 建立一个名为 name 的语句块然后就好像一个隐式 progn 来求值多个 form.

        特殊操作符 block 和 return-from 一起使用来提供一个结构化的, 词法的, 非局部的退出机制. 在 forms 中词法包含的任何点, return-from 可以和给定的 name 一起使用来从 blook 表达式形式中返回控制和值, 除了当内部有一个相同名字的块被建立时, 在这种情况下较外部的块会被更内部的块所遮蔽.

        名为 name 的块有词法作用于和动态范围.

        一旦建立, 一个块可能只退出一次, 不管是正常退出还是显式退出.

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

        return, return-from, Section 3.1 (Evaluation)

* 注意(Notes):

### <span id="">特殊操作符 CATCH</span>

* 语法(Syntax):

        catch tag form* => result*

* 参数和值(Arguments and Values):

        tag---一个捕捉标签; 求值的.
        forms---一个隐式的 progn.
        results---如果 forms 正常退出, 就是 forms 返回的值; 如果发生一个对 tag 的抛出, 值就会被抛出来.

* 描述(Description):

        catch 被用作 throw 的非局部控制转移的终点. Tags 用于查找 throw 控制转移的 catch. (catch 'foo form) 捕捉一个 (throw 'foo form) 而不是 (throw 'bar form).

        catch 执行顺序如下:

        1. Tag 被求值. 它是 catch 的名称.

        2. 然后 forms 作为隐式 progn 来求值, 除非一个 throw 发生, 否则返回最后一个表达式形式的结果.

        3. 如果在执行 forms 的其中一个时发生一个 throw, 控制被转移到一个 catch 表达式形式, 这个 catch 表达式形式的 tag 和 throw 的 tag 参数是 eq 的并且它是这个 tag 最近建立的 catch. 不再对表达式形式进行进一步的求值.

        4. catch 建立的这个 tag 在结果返回前就会被消除.

        如果在执行 forms 的其中一个的时候, 一个 throw 被执行而它的 tag 和 catch 的 tag 是 eq 的, 那么 throw 指定的值作为动态最新建立的这个 tag 的 catch 表达式的结果返回.

        即便 throw 不在 catch 的词法作用域里, catch 和 throw 机制还是会起作用. throw 必须发生在对应 tag 的 catch 的主体求值的动态范围内.

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

        如果在没有合适的 catch tag 时 throw 被执行, 会发出一个 control-error 类型的错误.

* 也见(See Also):

throw, Section 3.1 (Evaluation)

* 注意(Notes):

        符号被用作 tags 是惯例, 但是任何对象也是允许的. 然而, 数字不应该被使用因为比较是使用 eq 来做的.

        catch 和 block 的区别在于 catch 标签有着动态作用域而 block 名字有词法作用域.

### <span id="">特殊操作符 GO</span>

* 语法(Syntax):

        go tag =>|

* 参数和值(Arguments and Values):

        tag---一个 go 标签.

* 描述(Description):

        go 将控制转移到一个闭合的由和 tag eql 的标签所标记的 tagbody 表达式的主体上. 如果在这个主体内没有这样一个 tag, tagbody 的词法包含的主体 (如果有的话) 也会被检查. 如果有好几个标签和 tag 是 eql 的, 控制会被转移到包含这个 go 的最内部匹配 tag 的 tagbody 表达式上. 如果在这个 go 的点上没有词法可见的匹配的 tag 那么结果是未定义的.

        由 go 发起的控制转移是按照章节 5.2 (Transfer of Control to an Exit Point)所描述的进行的.

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

下面这个是错误的因为在 go 表达式被执行前这个 tagbody 被跳过了.

    ```LISP
    (funcall (block nil
               (tagbody a (return #'(lambda () (go a))))))
    ```
    
* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        tagbody

* 注意(Notes): None.


### <span id="">特殊操作符 RETURN-FROM</span>

* 语法(Syntax):

        return-from name [result] =>|

* 参数和值(Arguments and Values):

        name---一个 block 标签; 不求值.
        result---一个表达式形式; 求值. 默认是 nil.

* 描述(Description):

        从一个词法上闭合的 block 中返回控制和多值.

        一个名为 name 的 block 表达式形式必须词法上包含 return-from 的发生; 结果求值所产生的任何值都将立即从最内层的词法闭合 block 中返回.

        由 return-from 发起的控制转移是按照章节 5.2 (Transfer of Control to an Exit Point) 所所描述的进行的.

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

        下面这个结果也是未定义的因为在 return-from 被尝试之前这个 block 表达式就正常退出了.

    ```LISP
    (funcall (block nil #'(lambda () (return-from nil)))) 
    ```

        是一个错误.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        block, return, Section 3.1 (Evaluation)

* 注意(Notes): None.


### <span id="">宏 RETURN</span>

* 语法(Syntax):

        return [result] =>|

* 参数和值(Arguments and Values):

        result---一个表达式形式; 求值. 默认是 nil.

* 描述(Description):

        从一个名为 nil 的 block 中返回, 就像是通过 return-from 的一样.

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

        block, return-from, Section 3.1 (Evaluation)

* 注意(Notes):

    ```LISP
    (return) ==  (return-from nil)
    (return form) ==  (return-from nil form)
    ```

        这个被类似于 do 的宏建立的隐式的块经常名为 nil, 因此 return 可以被用于从这样的块中返回.


### <span id="">特殊操作符 TAGBODY</span>

* 语法(Syntax):

        tagbody {tag | statement}* => nil

* 参数和值(Arguments and Values):

        tag---一个 go 标签; 不求值.
        statement---一个复合表达式形式; 按照以下描述求值.

* 描述(Description):

        在词法环境中执行零个或多个语句, 以提供对 tag 所指示的标签的控制转移.

        一个 tagbody 中的语句按照从左到右的顺序被求值, 并且它们的值会被丢弃. 如果在任何时候都没有剩余的语句, tagbody 返回 nil. 然而, 如果 (go tag) 被求值, 控制会跳转到这个 tag 所表示的标签主体部分. (Tags 用 eql 来比较.)

        一个通过 tagbody 建立的 tag 有着词法作用域和动态范围. 一旦 tagbody 退出, 去 go 到一个它的主体中的 tag 不再合法. 用 go 去跳转到一个不是包含这个 go 的最内部的 tagbody 是允许的; tagbody 建立的 tags 只会遮蔽其他名字一样的 tags.

        在该元素的任何宏展开之前, 确定主体的哪些元素是标签, 哪些是语句. 如果一个语句是一个宏表达式并且它的宏展开是一个 atom, 那么这个 atom 被当作是一个语句而不是一个标签.

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

        下一段的宏中有着隐式的 tagbody.

        do              do-external-symbols  dotimes
        do*             do-symbols           prog
        do-all-symbols  dolist               prog*

        Figure 5-10. 有着隐式的 tagbody 的宏.


### <span id="">特殊操作符 THROW</span>

* 语法(Syntax):

        throw tag result-form =>|

* 参数和值(Arguments and Values):

        tag---一个捕捉标签; 求值.
        result-form---一个表达式形式; 按以下方式求值.

* 描述(Description):

        throw 导致一个局部控制转移, 转移到一个标签和 tag eq 的 catch 中.

        Tag 首先被求值来产生一个称之为 throw 标签的对象; 然后 result-form 被求值, 并且它的结果被保存下来. 如果这个 result-form 产生多个值, 那么所有的值会被保存. 最新的未完成的其中 tag 和 throw 的 tag 是 eq 的 catch 会退出; 保存的结果会作为 catch 的值或多值被返回.

        throw 发起的控制转移会像章节 5.2 (Transfer of Control to an Exit Point) 中描述的那样被执行.

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

        下面这个的结果是未定义的因为 b 的 catch 被第一个 throw 跳过了, 因此, 可移植程序必须假定其动态范围是终止的. 捕获标签的绑定还没有被消除, 因此它是第二个 throw 的目标.

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

        如果这里没有未完成的捕捉标签和抛出的匹配, 没有执行栈的解除(unwinding), 那么会发出一个 control-error 类型的错误. 当这个错误被发出时, 动态环境是在 throw 的作用下产生的the dynamic environment is that which was in force at the point of the throw.<!-- TODO 待校验 -->

* 也见(See Also):

        block, catch, return-from, unwind-protect, Section 3.1 (Evaluation)

* 注意(Notes):

        通常当退出点必须有动态作用域时 catch 和 throw 被使用 (比如, 这个 throw 不是被 catch 词法围绕的), 而 block 和 return 被用于词法作用域能满足的情况.


### <span id="">特殊操作符 UNWIND-PROTECT</span>

* 语法(Syntax):

        unwind-protect protected-form cleanup-form* => result*

* 参数和值(Arguments and Values):

        protected-form---一个表达式形式.
        cleanup-form---一个表达式形式.
        results---这个 protected-form 的值.

* 描述(Description):

        unwind-protect 求值 protected-form 并且保证这个 cleanup-form 在 unwind-protect 退出前被执行, 不管它正常终止或被某种控制转移所跳过. unwind-protect 的目的是确保求值 protected-form 之后会发生某些副作用.

        如果在执行 cleanup-form 的时候发生一个非局部(non-local)退出, 不会采取特殊动作. 这个 unwind-protect 的 cleanup-forms 不受 unwind-protect 保护.

        unwind-protect 防止所有企图退出 protected-form 的尝试, 包括 go, handler-case, ignore-errors, restart-case, return-from, throw, 还有 with-simple-restart.

        在退出时取消 handler 和 restart 绑定, 与取消动态变量和 catch 标签绑定是并行的, 和它们建立的顺序是相反的. 这样做的效果是 cleanup-form 看到相同的 handler 和 restart 绑定, 以及动态变量绑定和 catch 标签, 和进入 unwind-protect 时见到的一样.

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

        当 go 被执行时, 对 print 的调用首先被执行, 然后控制转移到标签 out.

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
    
        如果在 incf 完成之前发生了一个退出, 这个 decf 表达式形式无论如何都会被执行, 导致 *access-count* 的一个不正确的值. 正确的代码如下:

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

        catch, go, handler-case, restart-case, return, return-from, throw, Section 3.1 (Evaluation)

* 注意(Notes): None.


### <span id="">常量 NIL</span>

* 常量值(Constant Value):

        nil.

* 描述(Description):

        nil 表示 boolean (还有广义的 boolean) false 还有空列表.

* 示例(Examples):

    ```LISP
    nil =>  NIL
    ```

* 也见(See Also):

        t

* 注意(Notes): None.


### <span id="">函数 NOT</span>

* 语法(Syntax):

        not x => boolean

* 参数和值(Arguments and Values):

        x---一个广义的 boolean (换句话说, 任何对象).

        boolean---一个 boolean.

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

        not 的目的是用来反转 boolean 的"真值(truth value)" (或者是广义的 boolean) 然而 null 用于测试空列表. 操作上, not 和 null 的计算结果是一样的; 使用哪个是一个风格问题.


### <span id="">常量 T</span>

* 常量值(Constant Value):

        t.

* 描述(Description):

        这个 boolean 表示 true, 并且这个标准的广义 boolean 表示 true. 虽然任何不是 nil 的对象都被当成 true, t 在没有特殊原因的情况下被广泛使用.

        符号 t 有时也被用作其他目的. 比如, 作为一个类的名字, 作为一个标志符 (比如, 一个 stream 标志符) 或者由于某些语法原因作为一个特殊符号 (比如, 在 case 和 typecase 中去表示 otherwise-clause).

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


### <span id="">函数 EQ</span>

* 语法(Syntax):

        eq x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象.
        y---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果它的参数是一样的, 完全相同的对象就返回 true; 否则, 返回 false.

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

        eql, equal, equalp, =, Section 3.2 (Compilation)

* 注意(Notes):

        在打印时出现相同的对象不一定是相互之间 EQ 的. 由于 intern 函数的使用, 在打印时相同的符号通常相互之间是 EQ 的. 然而, 值相同的数字不需要是 EQ 的, 并且两个相似的列表通常也不是一样的.

        一个具体实现允许在任何时间去 "拷贝" 字符和数字. 其结果是, 如果参数是字符或数字, 即使它们是"相同的", Common Lisp 无法保证 eq 是 true 的.

        大部分 Common Lisp 操作符使用 eql 而不是 eq 来比较对象, 或者它们会默认用 EQL, 只在特定要求下使用 EQ. 然而, 下面的操作符被定义为使用 EQ 而不是 EQL , 它不能被使用它们的代码所覆盖:

        catch           getf     throw
        get             remf
        get-properties  remprop

        Figure 5-11. 总是使用 EQ 而不是 EQL 的操作符


### <span id="">函数 EQL</span>

* 语法(Syntax):

        eql x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象.
        y---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        两个对象, x 和 y, 在下面情况下 eql 的值是 true:

        1. 如果 x 和 y 是 eq 的.
        2. 如果 x 和 y 都是相同类型的数字并且值相同.
        3. 如果它们都是表示相同字符的字符.

        否则 eql 的值就是 false.

        如果一个具体实现支持正负零是不同的值, 那么 (eql 0.0 -0.0) 返回 false. 否则, 当语法 -0.0 被读取时会被解释为值 0.0, 所以 (eql 0.0 -0.0) 返回 true.

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
    
        通常 (eql 1.0s0 1.0d0) 是 false, 在 1.0s0 和 1.0d0 是不同的数据类型的设定下. 然而, 不提供4种不同的 floating-point 格式的具体实现允许将这四种格式"折叠"成更小的数字格式.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        eq, equal, equalp, =, char=

* 注意(Notes):

        eql 和 eq 是一样的, 除了当参数是字符或相同类型的数字时它们的值会被比较. 因此 eql 讲述两个对象是否在概念上相等, 而 eq 讲述两个对象是否在实现上相等. 这也就是 eql, 而不是eq, 作为参数的运算符的默认比较谓词的原因.

        eql 对于两个表示相同值的浮点数可能不是 true. = 被用于比较算数值.

        两个复数如果它们的实部和虚部都是 eql 的就认为它们是 eql 的. 比如, (eql #C(4 5) #C(4 5)) 是 true 而 (eql #C(4 5) #C(4.0 5.0)) 是 false. 注意 (eql #C(5.0 0.0) 5.0) 是 false, 而 (eql #C(5 0) 5) 是 true. 在 (eql #C(5.0 0.0) 5.0) 情况下两个参数是不同类型, 因此不能满足 eql. 在 (eql #C(5 0) 5) 情况下, #C(5 0) 不是一个复数, 但是被自动简化为 integer 5.


### <span id="">函数 EQUAL</span>

* 语法(Syntax):

        equal x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象.
        y---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果 x 和 y 是结构上类似(同构)的对象, 则返回true. 对象按照以下类别被 equal 处理.

        Symbols, Numbers, 和 Characters

            两个对象如果它们是相互 eq 的符号, 或者是相互 eql 的数字, 或者相互 eql 的字符, 那么 equal 就是 true.

        Conses

            对于 cons, equal 按照递归定义, 如果两个 car 是 equal 的并且两个 cdr 也是 equal 的那么就是 equal 的.

        Arrays

            两个数组只有当它们是 eq 的情况下才会 equal, 除了一个例外: string 和 bit vector 被一个一个元素比较 (使用 eql). 如果 x 或 y 有一个填充指针, 这个填充指针限制检查被 equal 检查元素的数量. string 中的大写字母和小写字母被 equal 认为是不同的.

        Pathnames

            两个 pathname 当且仅当所有对应的部分 (host, device, 等等) 相等的时候才是 equal 的. 大写和小写字母在组件的字符串中是否被认为是等价的是依赖于具体实现的. equal 的多个 pathname 应该在功能上是等同的.

        Other (Structures, hash-tables, instances, ...)

            只有当两个其他对象是 eq 的情况下才会 equal.

        equal does not descend any objects other than the ones explicitly specified above. 下面这段总结了前面的列表中的信息. 另外, 这段中指明了 equal 行为的优先级, 上面的条目优先于下面的条目.

        类型           行为
        number        uses eql
        character     uses eql
        cons          descends
        bit vector    descends
        string        descends
        pathname      ``functionally equivalent''
        structure     uses eq
        Other array   uses eq
        hash table    uses eq
        Other object  uses eq

        Figure 5-12. equal行为的概要和优先级

        任何两个 eql 的对象也是 equal 的.

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

        对象相等并不是一个概念, 它有唯一确定的正确算法. 等价谓词的适当性只能在某些特定程序的需求上下文中进行判断. 虽然这些函数接受任何类型的参数并且它们的名字听起来很通用, 但是 equal 和 equalp 不适用于每一个应用.

        一个粗略的经验法则是两个对象当且仅当它们的打印表示是相同的那么就是 equal 的.


### <span id="">函数 EQUALP</span>

* 语法(Syntax):

        equalp x y => generalized-boolean

* 参数和值(Arguments and Values):

        x---一个对象.
        y---一个对象.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果 x 和 y 是 equal 的就返回 true, 或者如果它们有着相同类型的组件并且那些组建也是 equalp 的就返回 true; 特别指出, equalp 在以下情况返回 true:

        Characters

            如果两个字符是 char-equal 的.

        Numbers

            如果两个数字在 = 下是一样的.

        Conses

            如果这两个 cons 中的 car 是 equalp 的并且 cdr 也是 equalp 的.

        Arrays

            如果两个数组有着相同的维度, 维度是匹配的, 并且对应可用的元素是 equalp 的. 数组指定的类型不需要匹配; 比如, 一个 string 和 一个包含相同字符的普通的数组是 equalp 的. 由于 equalp 执行字符串挨个元素的比较并且忽略字符的大小写, 当使用 equalp 比较字符串时大小写区分是被忽略的.

        Structures

            如果两个结构 S1 和 S2 有着相同的类并且 S1 中每一个槽的值和 S2 中对应槽的值是 equalp 的.

        Hash Tables

            equalp 首先通过判断元素的数量和这个 :test 函数来 descend hash-tables; 如果这些是一样的, 它用 :test 函数来比较这个哈希表的 key 然后匹配的 key 对应的 value 也递归地使用 equalp.<!-- TODO descend -->

        equalp 除了上述明确指定外不会去 descend 任何对象. 下一段中总结了上面列表中给定的信息. 另外, 这段中指明了 equalp 行为的优先级, 上面的条目优先于下面的条目.

        类型           行为
        number        uses =
        character     uses char-equal
        cons          descends
        bit vector    descends
        string        descends
        pathname      same as equal
        structure     descends, as described above
        Other array   descends
        hash table    descends, as described above
        Other object  uses eq

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

        对象相等并不是一个概念, 它有唯一确定的正确算法. 等价谓词的适当性只能在某些特定程序的需求上下文中进行判断. 虽然这些函数接受任何类型的参数并且它们的名字听起来很通用, 但是 equal 和 equalp 不适用于每一个应用.


### <span id="">函数 IDENTITY</span>

* 语法(Syntax):

        identity object => object

* 参数和值(Arguments and Values):

        object---一个对象.

* 描述(Description):

        返回它的参数对象.

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

        (eql x (identity x)) 对于所有 x 的可能的值返回 true, 但是 (eq x (identity x)) 当 x 是一个数字或者字符时可能返回 false.

        identity 可以通过以下定义

        (defun identity (x) x)


### <span id="">函数 COMPLEMENT</span>

* 语法(Syntax):

        complement function => complement-function

* 参数和值(Arguments and Values):

        function---一个函数.
        complement-function---一个函数.

* 描述(Description):

        返回一个和 function 接受相同参数并且和 function 有着相同副作用的函数, 但是只返回单个值: 一个广义的 boolean , 表示 function 返回的主要的值的相反值. 这也就是说, 当这个 function 会返回 true 作为主要的值时 complement-function 返回 false, 而当这个 function 返回 false 作为主要的值时, complement-function 返回 true.

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

        在 Common Lisp 中, 有着像名字 ``xxx-if-not'' 的函数和有着类似 ``xxx-if'' 名字的函数有关

        (xxx-if-not f . arguments) ==  (xxx-if (complement f) . arguments)

        比如,

        (find-if-not #'zerop '(0 0 3)) ==
        (find-if (complement #'zerop) '(0 0 3)) =>  3

        注意由于 "xxx-if-not" 函数和 :test-not 参数已经被废弃, 首选和 complement 一起使用 "xxx-if" 函数或  :test 参数.


### <span id="">函数 CONSTANTLY</span>

* 语法(Syntax):

        constantly value => function

* 参数和值(Arguments and Values):

        value---一个对象.

        function---一个函数.

* 描述(Description):

        constantly 返回一个接受任何数量的参数的函数, 没有副作用, 并且总是返回 value.

* 示例(Examples):

    ```LISP
    (mapcar (constantly 3) '(a b c d)) =>  (3 3 3 3)
    (defmacro with-vars (vars &body forms)
      `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars)))
    =>  WITH-VARS
    (macroexpand '(with-vars (a b) (setq a 3 b (* a a)) (list a b)))
    =>  ((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL), true

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


### <span id="">函数 EVERY, SOME, NOTEVERY, NOTANY</span>

* 语法(Syntax):

every predicate &rest sequences+ => generalized-boolean

some predicate &rest sequences+ => result

notevery predicate &rest sequences+ => generalized-boolean

notany predicate &rest sequences+ => generalized-boolean

* 参数和值(Arguments and Values):

predicate---a designator for a function of as many arguments as there are sequences.

sequence---a sequence.

result---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

every, some, notevery, and notany test elements of sequences for satisfaction of a given predicate. The first argument to predicate is an element of the first sequence; each succeeding argument is an element of a succeeding sequence.

Predicate is first applied to the elements with index 0 in each of the sequences, and possibly then to the elements with index 1, and so on, until a termination criterion is met or the end of the shortest of the sequences is reached.

every returns false as soon as any invocation of predicate returns false. If the end of a sequence is reached, every returns true. Thus, every returns true if and only if every invocation of predicate returns true.

some returns the first non-nil value which is returned by an invocation of predicate. If the end of a sequence is reached without any invocation of the predicate returning true, some returns false. Thus, some returns true if and only if some invocation of predicate returns true.

notany returns false as soon as any invocation of predicate returns true. If the end of a sequence is reached, notany returns true. Thus, notany returns true if and only if it is not the case that any invocation of predicate returns true.

notevery returns true as soon as any invocation of predicate returns false. If the end of a sequence is reached, notevery returns false. Thus, notevery returns true if and only if it is not the case that every invocation of predicate returns true.

* 示例(Examples):

 (every #'characterp "abc") =>  true
 (some #'= '(1 2 3 4 5) '(5 4 3 2 1)) =>  true
 (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  false
 (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if its first argument is neither a symbol nor a function or if any subsequent argument is not a proper sequence.

Other exceptional situations are possible, depending on the nature of the predicate.

* 也见(See Also):

and, or, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

 (notany predicate sequence*) ==  (not (some predicate sequence*))
 (notevery predicate sequence*) ==  (not (every predicate sequence*))


### <span id="">宏 AND</span>

* 语法(Syntax):

and form* => result*

* 参数和值(Arguments and Values):

form---a form.

results---the values resulting from the evaluation of the last form, or the symbols nil or t.

* 描述(Description):

The macro and evaluates each form one at a time from left to right. As soon as any form evaluates to nil, and returns nil without evaluating the remaining forms. If all forms but the last evaluate to true values, and returns the results produced by evaluating the last form.

If no forms are supplied, (and) returns t.

and passes back multiple values from the last subform but not from subforms other than the last.

* 示例(Examples):

 (if (and (>= n 0)
          (< n (length a-simple-vector))
          (eq (elt a-simple-vector n) 'foo))
     (princ "Foo!"))

The above expression prints Foo! if element n of a-simple-vector is the symbol foo, provided also that n is indeed a valid index for a-simple-vector. Because and guarantees left-to-right testing of its parts, elt is not called if n is out of range.

 (setq temp1 1 temp2 1 temp3 1) =>  1
 (and (incf temp1) (incf temp2) (incf temp3)) =>  2
 (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3)) =>  true
 (decf temp3) =>  1
 (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3)) =>  NIL
 (and (eql temp1 temp2) (eql temp2 temp3)) =>  true
 (and) =>  T

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

cond, every, if, or, when

* 注意(Notes):

 (and form) ==  (let () form)
 (and form1 form2 ...) ==  (when form1 (and form2 ...))


### <span id="">宏 COND</span>

* 语法(Syntax):

cond {clause}* => result*

clause::= (test-form form*)

* 参数和值(Arguments and Values):

test-form---a form.

forms---an implicit progn.

results---the values of the forms in the first clause whose test-form yields true, or the primary value of the test-form if there are no forms in that clause, or else nil if no test-form yields true.

* 描述(Description):

cond allows the execution of forms to be dependent on test-form.

Test-forms are evaluated one at a time in the order in which they are given in the argument list until a test-form is found that evaluates to true.

If there are no forms in that clause, the primary value of the test-form is returned by the cond form. Otherwise, the forms associated with this test-form are evaluated in order, left to right, as an implicit progn, and the values returned by the last form are returned by the cond form.

Once one test-form has yielded true, no additional test-forms are evaluated. If no test-form yields true, nil is returned.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

if, case.

* 注意(Notes): None.


### <span id="">特殊操作符 IF</span>

* 语法(Syntax):

if test-form then-form [else-form] => result*

* 参数和值(Arguments and Values):

Test-form---a form.

Then-form---a form.

Else-form---a form. The default is nil.

results---if the test-form yielded true, the values returned by the then-form; otherwise, the values returned by the else-form.

* 描述(Description):

if allows the execution of a form to be dependent on a single test-form.

First test-form is evaluated. If the result is true, then then-form is selected; otherwise else-form is selected. Whichever form is selected is then evaluated.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

cond, unless, when

* 注意(Notes):

 (if test-form then-form else-form)
 ==  (cond (test-form then-form) (t else-form))


### <span id="">宏 OR</span>

* 语法(Syntax):

or form* => results*

* 参数和值(Arguments and Values):

form---a form.

results---the values or primary value (see below) resulting from the evaluation of the last form executed or nil.

* 描述(Description):

or evaluates each form, one at a time, from left to right. The evaluation of all forms terminates when a form evaluates to true (i.e., something other than nil).

If the evaluation of any form other than the last returns a primary value that is true, or immediately returns that value (but no additional values) without evaluating the remaining forms. If every form but the last returns false as its primary value, or returns all values returned by the last form. If no forms are supplied, or returns nil.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

and, some, unless

* 注意(Notes): None.


### <span id="">宏 WHEN, UNLESS</span>

* 语法(Syntax):

when test-form form* => result*

unless test-form form* => result*

* 参数和值(Arguments and Values):

test-form---a form.

forms---an implicit progn.

results---the values of the forms in a when form if the test-form yields true or in an unless form if the test-form yields false; otherwise nil.

* 描述(Description):

when and unless allow the execution of forms to be dependent on a single test-form.

In a when form, if the test-form yields true, the forms are evaluated in order from left to right and the values returned by the forms are returned from the when form. Otherwise, if the test-form yields false, the forms are not evaluated, and the when form returns nil.

In an unless form, if the test-form yields false, the forms are evaluated in order from left to right and the values returned by the forms are returned from the unless form. Otherwise, if the test-form yields false, the forms are not evaluated, and the unless form returns nil.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

and, cond, if, or

* 注意(Notes):

 (when test {form}+) ==  (and test (progn {form}+))
 (when test {form}+) ==  (cond (test {form}+))
 (when test {form}+) ==  (if test (progn {form}+) nil)
 (when test {form}+) ==  (unless (not test) {form}+)
 (unless test {form}+) ==  (cond ((not test) {form}+))
 (unless test {form}+) ==  (if test nil (progn {form}+))
 (unless test {form}+) ==  (when (not test) {form}+)


### <span id="">宏 CASE, CCASE, ECASE</span>

* 语法(Syntax):

case keyform {normal-clause}* [otherwise-clause] => result*

ccase keyplace {normal-clause}* => result*

ecase keyform {normal-clause}* => result*

normal-clause::= (keys form*)

otherwise-clause::= ({otherwise | t} form*)

clause::= normal-clause | otherwise-clause

* 参数和值(Arguments and Values):

keyform---a form; evaluated to produce a test-key.

keyplace---a form; evaluated initially to produce a test-key. Possibly also used later as a place if no keys match.

test-key---an object produced by evaluating keyform or keyplace.

keys---a designator for a list of objects. In the case of case, the symbols t and otherwise may not be used as the keys designator. To refer to these symbols by themselves as keys, the designators (t) and (otherwise), respectively, must be used instead.

forms---an implicit progn.

results---the values returned by the forms in the matching clause.

* 描述(Description):

These macros allow the conditional execution of a body of forms in a clause that is selected by matching the test-key on the basis of its identity.

The keyform or keyplace is evaluated to produce the test-key.

Each of the normal-clauses is then considered in turn. If the test-key is the same as any key for that clause, the forms in that clause are evaluated as an implicit progn, and the values it returns are returned as the value of the case, ccase, or ecase form.

These macros differ only in their behavior when no normal-clause matches; specifically:

case

    If no normal-clause matches, and there is an otherwise-clause, then that otherwise-clause automatically matches; the forms in that clause are evaluated as an implicit progn, and the values it returns are returned as the value of the case.

    If there is no otherwise-clause, case returns nil.

ccase

    If no normal-clause matches, a correctable error of type type-error is signaled. The offending datum is the test-key and the expected type is type equivalent to (member key1 key2 ...). The store-value restart can be used to correct the error.

    If the store-value restart is invoked, its argument becomes the new test-key, and is stored in keyplace as if by (setf keyplace test-key). Then ccase starts over, considering each clause anew.

    The subforms of keyplace might be evaluated again if none of the cases holds.

ecase

    If no normal-clause matches, a non-correctable error of type type-error is signaled. The offending datum is the test-key and the expected type is type equivalent to (member key1 key2 ...).

    Note that in contrast with ccase, the caller of ecase may rely on the fact that ecase does not return if a normal-clause does not match.

* 示例(Examples):

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

* 副作用(Side Effects):

The debugger might be entered. If the store-value restart is invoked, the value of keyplace might be changed.

* 受此影响(Affected By):

ccase and ecase, since they might signal an error, are potentially affected by existing handlers and *debug-io*.

* 异常情况(Exceptional Situations):

ccase and ecase signal an error of type type-error if no normal-clause matches.

* 也见(See Also):

cond, typecase, setf, Section 5.1 (Generalized Reference)

* 注意(Notes):

(case test-key
  {((key*) form*)}*)
==
(let ((#1=#:g0001 test-key))
  (cond {((member #1# '(key*)) form*)}*))

The specific error message used by ecase and ccase can vary between implementations. In situations where control of the specific wording of the error message is important, it is better to use case with an otherwise-clause that explicitly signals an error with an appropriate message.


### <span id="">宏 TYPECASE, CTYPECASE, ETYPECASE</span>

* 语法(Syntax):

typecase keyform {normal-clause}* [otherwise-clause] => result*

ctypecase keyplace {normal-clause}* => result*

etypecase keyform {normal-clause}* => result*

normal-clause::= (type form*)

otherwise-clause::= ({otherwise | t} form*)

clause::= normal-clause | otherwise-clause

* 参数和值(Arguments and Values):

keyform---a form; evaluated to produce a test-key.

keyplace---a form; evaluated initially to produce a test-key. Possibly also used later as a place if no types match.

test-key---an object produced by evaluating keyform or keyplace.

type---a type specifier.

forms---an implicit progn.

results---the values returned by the forms in the matching clause.

* 描述(Description):

These macros allow the conditional execution of a body of forms in a clause that is selected by matching the test-key on the basis of its type.

The keyform or keyplace is evaluated to produce the test-key.

Each of the normal-clauses is then considered in turn. If the test-key is of the type given by the clauses's type, the forms in that clause are evaluated as an implicit progn, and the values it returns are returned as the value of the typecase, ctypecase, or etypecase form.

These macros differ only in their behavior when no normal-clause matches; specifically:

typecase

    If no normal-clause matches, and there is an otherwise-clause, then that otherwise-clause automatically matches; the forms in that clause are evaluated as an implicit progn, and the values it returns are returned as the value of the typecase.

    If there is no otherwise-clause, typecase returns nil.

ctypecase

    If no normal-clause matches, a correctable error of type type-error is signaled. The offending datum is the test-key and the expected type is type equivalent to (or type1 type2 ...). The store-value restart can be used to correct the error.

    If the store-value restart is invoked, its argument becomes the new test-key, and is stored in keyplace as if by (setf keyplace test-key). Then ctypecase starts over, considering each clause anew.

    If the store-value restart is invoked interactively, the user is prompted for a new test-key to use.

    The subforms of keyplace might be evaluated again if none of the cases holds.

etypecase

    If no normal-clause matches, a non-correctable error of type type-error is signaled. The offending datum is the test-key and the expected type is type equivalent to (or type1 type2 ...).

    Note that in contrast with ctypecase, the caller of etypecase may rely on the fact that etypecase does not return if a normal-clause does not match.

In all three cases, is permissible for more than one clause to specify a matching type, particularly if one is a subtype of another; the earliest applicable clause is chosen.

* 示例(Examples):

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

* 受此影响(Affected By):

ctypecase and etypecase, since they might signal an error, are potentially affected by existing handlers and *debug-io*.

* 异常情况(Exceptional Situations):

ctypecase and etypecase signal an error of type type-error if no normal-clause matches.

The compiler may choose to issue a warning of type style-warning if a clause will never be selected because it is completely shadowed by earlier clauses.

* 也见(See Also):

case, cond, setf, Section 5.1 (Generalized Reference)

* 注意(Notes):

(typecase test-key
  {(type form*)}*)
==
(let ((#1=#:g0001 test-key))
  (cond {((typep #1# 'type) form*)}*))

The specific error message used by etypecase and ctypecase can vary between implementations. In situations where control of the specific wording of the error message is important, it is better to use typecase with an otherwise-clause that explicitly signals an error with an appropriate message.


### <span id="">宏 MULTIPLE-VALUE-BIND</span>

* 语法(Syntax):

multiple-value-bind (var*) values-form declaration* form*

=> result*

* 参数和值(Arguments and Values):

var---a symbol naming a variable; not evaluated.

values-form---a form; evaluated.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

Creates new variable bindings for the vars and executes a series of forms that use these bindings.

The variable bindings created are lexical unless special declarations are specified.

Values-form is evaluated, and each of the vars is bound to the respective value returned by that form. If there are more vars than values returned, extra values of nil are given to the remaining vars. If there are more values than vars, the excess values are discarded. The vars are bound to the values over the execution of the forms, which make up an implicit progn. The consequences are unspecified if a type declaration is specified for a var, but the value to which that var is bound is not consistent with the type declaration.

The scopes of the name binding and declarations do not include the values-form.

* 示例(Examples):

 (multiple-value-bind (f r)
     (floor 130 11)
   (list f r)) =>  (11 9)

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


### <span id="">特殊操作符 MULTIPLE-VALUE-CALL</span>

* 语法(Syntax):

multiple-value-call function-form form* => result*

* 参数和值(Arguments and Values):

function-form---a form; evaluated to produce function.

function---a function designator resulting from the evaluation of function-form.

form---a form.

results---the values returned by the function.

* 描述(Description):

Applies function to a list of the objects collected from groups of multiple values[2].

multiple-value-call first evaluates the function-form to obtain function, and then evaluates each form. All the values of each form are gathered together (not just one value from each) and given as arguments to the function.

* 示例(Examples):

 (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
=>  (1 / 2 3 / / 2 0.5)
 (+ (floor 5 3) (floor 19 4)) ==  (+ 1 4)
=>  5
 (multiple-value-call #'+ (floor 5 3) (floor 19 4)) ==  (+ 1 2 4 3)
=>  10

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

multiple-value-list, multiple-value-bind

* 注意(Notes): None.


### <span id="">宏 MULTIPLE-VALUE-LIST</span>

* 语法(Syntax):

multiple-value-list form => list

* 参数和值(Arguments and Values):

form---a form; evaluated as described below.

list---a list of the values returned by form.

* 描述(Description):

multiple-value-list evaluates form and creates a list of the multiple values[2] it returns.

* 示例(Examples):

 (multiple-value-list (floor -3 4)) =>  (-1 1)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

values-list, multiple-value-call

* 注意(Notes):

multiple-value-list and values-list are inverses of each other.

 (multiple-value-list form) ==  (multiple-value-call #'list form)


### <span id="">特殊操作符 MULTIPLE-VALUE-PROG1</span>

* 语法(Syntax):

multiple-value-prog1 first-form form* => first-form-results

* 参数和值(Arguments and Values):

first-form---a form; evaluated as described below.

form---a form; evaluated as described below.

first-form-results---the values resulting from the evaluation of first-form.

* 描述(Description):

multiple-value-prog1 evaluates first-form and saves all the values produced by that form. It then evaluates each form from left to right, discarding their values.

* 示例(Examples):

 (setq temp '(1 2 3)) =>  (1 2 3)
 (multiple-value-prog1
    (values-list temp)
    (setq temp nil)
    (values-list temp)) =>  1, 2, 3

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

prog1

* 注意(Notes): None.


### <span id="">宏 MULTIPLE-VALUE-SETQ</span>

* 语法(Syntax):

multiple-value-setq vars form => result

* 参数和值(Arguments and Values):

vars---a list of symbols that are either variable names or names of symbol macros.

form---a form.

result---The primary value returned by the form.

* 描述(Description):

multiple-value-setq assigns values to vars.

The form is evaluated, and each var is assigned to the corresponding value returned by that form. If there are more vars than values returned, nil is assigned to the extra vars. If there are more values than vars, the extra values are discarded.

If any var is the name of a symbol macro, then it is assigned as if by setf. Specifically,

 (multiple-value-setq (symbol1 ... symboln) value-producing-form)

is defined to always behave in the same way as

 (values (setf (values symbol1 ... symboln) value-producing-form))

in order that the rules for order of evaluation and side-effects be consistent with those used by setf. See Section 5.1.2.3 (VALUES Forms as Places).

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

setq, symbol-macrolet

* 注意(Notes): None.


### <span id="">访问器 VALUES</span>

* 语法(Syntax):

values &rest object => object*

(setf (values &rest place) new-values)

* 参数和值(Arguments and Values):

object---an object.

place---a place.

new-value---an object.

* 描述(Description):

values returns the objects as multiple values[2].

setf of values is used to store the multiple values[2] new-values into the places. See Section 5.1.2.3 (VALUES Forms as Places).

* 示例(Examples):

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

Sometimes it is desirable to indicate explicitly that a function returns exactly one value. For example, the function

 (defun foo (x y)
   (floor (+ x y) y)) =>  FOO

returns two values because floor returns two values. It may be that the second value makes no sense, or that for efficiency reasons it is desired not to compute the second value. values is the standard idiom for indicating that only one value is to be returned:

 (defun foo (x y)
   (values (floor (+ x y) y))) =>  FOO

This works because values returns exactly one value for each of args; as for any function call, if any of args produces more than one value, all but the first are discarded.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

values-list, multiple-value-bind, multiple-values-limit, Section 3.1 (Evaluation)

* 注意(Notes):

Since values is a function, not a macro or special form, it receives as arguments only the primary values of its argument forms.


### <span id="">函数 VALUES-LIST</span>

* 语法(Syntax):

values-list list => element*

* 参数和值(Arguments and Values):

list---a list.

elements---the elements of the list.

* 描述(Description):

Returns the elements of the list as multiple values[2].

* 示例(Examples):

 (values-list nil) =>  <no values>
 (values-list '(1)) =>  1
 (values-list '(1 2)) =>  1, 2
 (values-list '(1 2 3)) =>  1, 2, 3

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if its argument is not a proper list.

* 也见(See Also):

multiple-value-bind, multiple-value-list, multiple-values-limit, values

* 注意(Notes):

 (values-list list) ==  (apply #'values list)

(equal x (multiple-value-list (values-list x))) returns true for all lists x.


### <span id="">常量 MULTIPLE-VALUES-LIMIT</span>

* 常量值(Constant Value):

An integer not smaller than 20, the exact magnitude of which is implementation-dependent.

* 描述(Description):

The upper exclusive bound on the number of values that may be returned from a function, bound or assigned by multiple-value-bind or multiple-value-setq, or passed as a first argument to nth-value. (If these individual limits might differ, the minimum value is used.)

* 示例(Examples): None.

* 也见(See Also):

lambda-parameters-limit, call-arguments-limit

* 注意(Notes):

Implementors are encouraged to make this limit as large as possible.


### <span id="">宏 NTH-VALUE</span>

* 语法(Syntax):

nth-value n form => object

* 参数和值(Arguments and Values):

n---a non-negative integer; evaluated.

form---a form; evaluated as described below.

object---an object.

* 描述(Description):

Evaluates n and then form, returning as its only value the nth value yielded by form, or nil if n is greater than or equal to the number of values returned by form. (The first returned value is numbered 0.)

* 示例(Examples):

 (nth-value 0 (values 'a 'b)) =>  A
 (nth-value 1 (values 'a 'b)) =>  B
 (nth-value 2 (values 'a 'b)) =>  NIL
 (let* ((x 83927472397238947423879243432432432)
        (y 32423489732)
        (a (nth-value 1 (floor x y)))
        (b (mod x y)))
   (values a b (= a b)))
=>  3332987528, 3332987528, true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

multiple-value-list, nth

* 注意(Notes):

Operationally, the following relationship is true, although nth-value might be more efficient in some implementations because, for example, some consing might be avoided.

 (nth-value n form) ==  (nth n (multiple-value-list form))


### <span id="">宏 PROG, PROG*</span>

* 语法(Syntax):

prog ({var | (var [init-form])}*) declaration* {tag | statement}*

=> result*

prog* ({var | (var [init-form])}*) declaration* {tag | statement}*

=> result*

* 参数和值(Arguments and Values):

var---variable name.

init-form---a form.

declaration---a declare expression; not evaluated.

tag---a go tag; not evaluated.

statement---a compound form; evaluated as described below.

results---nil if a normal return occurs, or else, if an explicit return occurs, the values that were transferred.

* 描述(Description):

Three distinct operations are performed by prog and prog*: they bind local variables, they permit use of the return statement, and they permit use of the go statement. A typical prog looks like this:

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

For prog, init-forms are evaluated first, in the order in which they are supplied. The vars are then bound to the corresponding values in parallel. If no init-form is supplied for a given var, that var is bound to nil.

The body of prog is executed as if it were a tagbody form; the go statement can be used to transfer control to a tag. Tags label statements.

prog implicitly establishes a block named nil around the entire prog form, so that return can be used at any time to exit from the prog form.

The difference between prog* and prog is that in prog* the binding and initialization of the vars is done sequentially, so that the init-form for each one can use the values of previous ones.

* 示例(Examples):

(prog* ((y z) (x (car y)))
       (return x))

returns the car of the value of z.

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

This can be accomplished more perspicuously as follows:

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

block, let, tagbody, go, return, Section 3.1 (Evaluation)

* 注意(Notes):

prog can be explained in terms of block, let, and tagbody as follows:

 (prog variable-list declaration . body)
    ==  (block nil (let variable-list declaration (tagbody . body)))


### <span id="">宏 PROG1, PROG2</span>

* 语法(Syntax):

prog1 first-form form* => result-1

prog2 first-form second-form form* => result-2

* 参数和值(Arguments and Values):

first-form---a form; evaluated as described below.

second-form---a form; evaluated as described below.

forms---an implicit progn; evaluated as described below.

result-1---the primary value resulting from the evaluation of first-form.

result-2---the primary value resulting from the evaluation of second-form.

* 描述(Description):

prog1 evaluates first-form and then forms, yielding as its only value the primary value yielded by first-form.

prog2 evaluates first-form, then second-form, and then forms, yielding as its only value the primary value yielded by first-form.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

multiple-value-prog1, progn

* 注意(Notes):

prog1 and prog2 are typically used to evaluate one or more forms with side effects and return a value that must be computed before some or all of the side effects happen.

 (prog1 form*) ==  (values (multiple-value-prog1 form*))
 (prog2 form1 form*) ==  (let () form1 (prog1 form*))


### <span id="">特殊操作符 PROGN</span>

* 语法(Syntax):

progn form* => result*

* 参数和值(Arguments and Values):

forms---an implicit progn.

results---the values of the forms.

* 描述(Description):

progn evaluates forms, in the order in which they are given.

The values of each form but the last are discarded.

If progn appears as a top level form, then all forms within that progn are considered by the compiler to be top level forms.

* 示例(Examples):

 (progn) =>  NIL
 (progn 1 2 3) =>  3
 (progn (values 1 2 3)) =>  1, 2, 3
 (setq a 1) =>  1
 (if a
      (progn (setq a nil) 'here)
      (progn (setq a t) 'there)) =>  HERE
 a =>  NIL

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

prog1, prog2, Section 3.1 (Evaluation)

* 注意(Notes):

Many places in Common Lisp involve syntax that uses implicit progns. That is, part of their syntax allows many forms to be written that are to be evaluated sequentially, discarding the results of all forms but the last and returning the results of the last form. Such places include, but are not limited to, the following: the body of a lambda expression; the bodies of various control and conditional forms (e.g., case, catch, progn, and when).


### <span id="">宏 DEFINE-MODIFY-MACRO</span>

* 语法(Syntax):

define-modify-macro name lambda-list function [documentation] => name

* 参数和值(Arguments and Values):

name---a symbol.

lambda-list---a define-modify-macro lambda list

function---a symbol.

documentation---a string; not evaluated.

* 描述(Description):

define-modify-macro defines a macro named name to read and write a place.

The arguments to the new macro are a place, followed by the arguments that are supplied in lambda-list. Macros defined with define-modify-macro correctly pass the environment parameter to get-setf-expansion.

When the macro is invoked, function is applied to the old contents of the place and the lambda-list arguments to obtain the new value, and the place is updated to contain the result.

Except for the issue of avoiding multiple evaluation (see below), the expansion of a define-modify-macro is equivalent to the following:

 (defmacro name (reference . lambda-list)
   documentation
   `(setf ,reference
          (function ,reference ,arg1 ,arg2 ...)))

where arg1, arg2, ..., are the parameters appearing in lambda-list; appropriate provision is made for a rest parameter.

The subforms of the macro calls defined by define-modify-macro are evaluated as specified in Section 5.1.1.1 (Evaluation of Subforms to Places).

Documentation is attached as a documentation string to name (as kind function) and to the macro function.

If a define-modify-macro form appears as a top level form, the compiler must store the macro definition at compile time, so that occurrences of the macro later on in the file can be expanded correctly.

* 示例(Examples):

 (define-modify-macro appendf (&rest args)
    append "Append onto list") =>  APPENDF
 (setq x '(a b c) y x) =>  (A B C)
 (appendf x '(d e f) '(1 2 3)) =>  (A B C D E F 1 2 3)
 x =>  (A B C D E F 1 2 3)
 y =>  (A B C)
 (define-modify-macro new-incf (&optional (delta 1)) +)
 (define-modify-macro unionf (other-set &rest keywords) union)

* 副作用(Side Effects):

A macro definition is assigned to name.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

defsetf, define-setf-expander, documentation, Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes): None.


### <span id="">宏 DEFSETF</span>

* 语法(Syntax):

The ``short form'':

defsetf access-fn update-fn [documentation]

=> access-fn

The ``long form'':

defsetf access-fn lambda-list (store-variable*) [[declaration* | documentation]] form*

=> access-fn

* 参数和值(Arguments and Values):

access-fn---a symbol which names a function or a macro.

update-fn---a symbol naming a function or macro.

lambda-list---a defsetf lambda list.

store-variable---a symbol (a variable name).

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

* 描述(Description):

defsetf defines how to setf a place of the form (access-fn ...) for relatively simple cases. (See define-setf-expander for more general access to this facility.) It must be the case that the function or macro named by access-fn evaluates all of its arguments.

defsetf may take one of two forms, called the ``short form'' and the ``long form,'' which are distinguished by the type of the second argument.

When the short form is used, update-fn must name a function (or macro) that takes one more argument than access-fn takes. When setf is given a place that is a call on access-fn, it expands into a call on update-fn that is given all the arguments to access-fn and also, as its last argument, the new value (which must be returned by update-fn as its value).

The long form defsetf resembles defmacro. The lambda-list describes the arguments of access-fn. The store-variables describe the value or values to be stored into the place. The body must compute the expansion of a setf of a call on access-fn. The expansion function is defined in the same lexical environment in which the defsetf form appears.

During the evaluation of the forms, the variables in the lambda-list and the store-variables are bound to names of temporary variables, generated as if by gensym or gentemp, that will be bound by the expansion of setf to the values of those subforms. This binding permits the forms to be written without regard for order-of-evaluation issues. defsetf arranges for the temporary variables to be optimized out of the final result in cases where that is possible.

The body code in defsetf is implicitly enclosed in a block whose name is access-fn

defsetf ensures that subforms of the place are evaluated exactly once.

Documentation is attached to access-fn as a documentation string of kind setf.

If a defsetf form appears as a top level form, the compiler must make the setf expander available so that it may be used to expand calls to setf later on in the file. Users must ensure that the forms, if any, can be evaluated at compile time if the access-fn is used in a place later in the same file. The compiler must make these setf expanders available to compile-time calls to get-setf-expansion when its environment argument is a value received as the environment parameter of a macro.

* 示例(Examples):

The effect of

 (defsetf symbol-value set)

is built into the Common Lisp system. This causes the form (setf (symbol-value foo) fu) to expand into (set foo fu).

Note that

 (defsetf car rplaca)

would be incorrect because rplaca does not return its last argument.

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

An example of the use of the long form of defsetf:

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

documentation, setf, define-setf-expander, get-setf-expansion, Section 5.1 (Generalized Reference), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

forms must include provision for returning the correct value (the value or values of store-variable). This is handled by forms rather than by defsetf because in many cases this value can be returned at no extra cost, by calling a function that simultaneously stores into the place and returns the correct value.

A setf of a call on access-fn also evaluates all of access-fn's arguments; it cannot treat any of them specially. This means that defsetf cannot be used to describe how to store into a generalized reference to a byte, such as (ldb field reference). define-setf-expander is used to handle situations that do not fit the restrictions imposed by defsetf and gives the user additional control.


### <span id="">宏 DEFINE-SETF-EXPANDER</span>

* 语法(Syntax):

define-setf-expander access-fn lambda-list [[declaration* | documentation]] form*

=> access-fn

* 参数和值(Arguments and Values):

access-fn---a symbol that names a function or macro.

lambda-list -- macro lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

forms---an implicit progn.

* 描述(Description):

define-setf-expander specifies the means by which setf updates a place that is referenced by access-fn.

When setf is given a place that is specified in terms of access-fn and a new value for the place, it is expanded into a form that performs the appropriate update.

The lambda-list supports destructuring. See Section 3.4.4 (Macro Lambda Lists).

Documentation is attached to access-fn as a documentation string of kind setf.

Forms constitute the body of the setf expander definition and must compute the setf expansion for a call on setf that references the place by means of the given access-fn. The setf expander function is defined in the same lexical environment in which the define-setf-expander form appears. While forms are being executed, the variables in lambda-list are bound to parts of the place form. The body forms (but not the lambda-list) in a define-setf-expander form are implicitly enclosed in a block whose name is access-fn.

The evaluation of forms must result in the five values described in Section 5.1.1.2 (Setf Expansions).

If a define-setf-expander form appears as a top level form, the compiler must make the setf expander available so that it may be used to expand calls to setf later on in the file. Programmers must ensure that the forms can be evaluated at compile time if the access-fn is used in a place later in the same file. The compiler must make these setf expanders available to compile-time calls to get-setf-expansion when its environment argument is a value received as the environment parameter of a macro.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

setf, defsetf, documentation, get-setf-expansion, Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

define-setf-expander differs from the long form of defsetf in that while the body is being executed the variables in lambda-list are bound to parts of the place form, not to temporary variables that will be bound to the values of such parts. In addition, define-setf-expander does not have defsetf's restriction that access-fn must be a function or a function-like macro; an arbitrary defmacro destructuring pattern is permitted in lambda-list.


### <span id="">函数 GET-SETF-EXPANSION</span>

* 语法(Syntax):

get-setf-expansion place &optional environment

=> vars, vals, store-vars, writer-form, reader-form

* 参数和值(Arguments and Values):

place---a place.

environment---an environment object.

vars, vals, store-vars, writer-form, reader-form---a setf expansion.

* 描述(Description):

Determines five values constituting the setf expansion for place in environment; see Section 5.1.1.2 (Setf Expansions).

If environment is not supplied or nil, the environment is the null lexical environment.

* 示例(Examples):

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


* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

defsetf, define-setf-expander, setf

* 注意(Notes):

Any compound form is a valid place, since any compound form whose operator f has no setf expander are expanded into a call to (setf f).


### <span id="">宏 SETF, PSETF</span>

* 语法(Syntax):

setf {pair}* => result*

psetf {pair}* => nil

pair::= place newvalue

* 参数和值(Arguments and Values):

place---a place.

newvalue---a form.

results---the multiple values[2] returned by the storing form for the last place, or nil if there are no pairs.

* 描述(Description):

setf changes the value of place to be newvalue.

(setf place newvalue) expands into an update form that stores the result of evaluating newvalue into the location referred to by place. Some place forms involve uses of accessors that take optional arguments. Whether those optional arguments are permitted by setf, or what their use is, is up to the setf expander function and is not under the control of setf. The documentation for any function that accepts &optional, &rest, or ..... key arguments and that claims to be usable with setf must specify how those arguments are treated.

If more than one pair is supplied, the pairs are processed sequentially; that is,

 (setf place-1 newvalue-1
       place-2 newvalue-2
       ...
       place-N newvalue-N)

is precisely equivalent to

 (progn (setf place-1 newvalue-1)
        (setf place-2 newvalue-2)
        ...
        (setf place-N newvalue-N))

For psetf, if more than one pair is supplied then the assignments of new values to places are done in parallel. More precisely, all subforms (in both the place and newvalue forms) that are to be evaluated are evaluated from left to right; after all evaluations have been performed, all of the assignments are performed in an unpredictable order.

For detailed treatment of the expansion of setf and psetf, see Section 5.1.2 (Kinds of Places).

* 示例(Examples):

 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3)
 (setf (car x) 'x (cadr y) (car x) (cdr x) y) =>  (1 X 3)
 x =>  (X 1 X 3)
 y =>  (1 X 3)
 (setq x (cons 'a 'b) y (list 1 2 3)) =>  (1 2 3)
 (psetf (car x) 'x (cadr y) (car x) (cdr x) y) =>  NIL
 x =>  (X 1 A 3)
 y =>  (1 A 3)

* 受此影响(Affected By):

define-setf-expander, defsetf, *macroexpand-hook*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

define-setf-expander, defsetf, macroexpand-1, rotatef, shiftf, Section 5.1 (Generalized Reference)

* 注意(Notes): None.


### <span id="">宏 SHIFTF</span>

* 语法(Syntax):

shiftf place+ newvalue => old-value-1

* 参数和值(Arguments and Values):

place---a place.

newvalue---a form; evaluated.

old-value-1---an object (the old value of the first place).

* 描述(Description):

shiftf modifies the values of each place by storing newvalue into the last place, and shifting the values of the second through the last place into the remaining places.

If newvalue produces more values than there are store variables, the extra values are ignored. If newvalue produces fewer values than there are store variables, the missing values are set to nil.

In the form (shiftf place1 place2 ... placen newvalue), the values in place1 through placen are read and saved, and newvalue is evaluated, for a total of n+1 values in all. Values 2 through n+1 are then stored into place1 through placen, respectively. It is as if all the places form a shift register; the newvalue is shifted in from the right, all values shift over to the left one place, and the value shifted out of place1 is returned.

For information about the evaluation of subforms of places, see Section 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

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

* 受此影响(Affected By):

define-setf-expander, defsetf, *macroexpand-hook*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

setf, rotatef, Section 5.1 (Generalized Reference)

* 注意(Notes):

The effect of (shiftf place1 place2 ... placen newvalue) is roughly equivalent to

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

except that the latter would evaluate any subforms of each place twice, whereas shiftf evaluates them once. For example,

 (setq n 0) =>  0
 (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
 (prog1 (nth (setq n (+ n 1)) x)
        (setf (nth (setq n (+ n 1)) x) 'z)) =>  B
 x =>  (A B Z D)


### <span id="">宏 ROTATEF</span>

* 语法(Syntax):

rotatef place* => nil

* 参数和值(Arguments and Values):

place---a place.

* 描述(Description):

rotatef modifies the values of each place by rotating values from one place into another.

If a place produces more values than there are store variables, the extra values are ignored. If a place produces fewer values than there are store variables, the missing values are set to nil.

In the form (rotatef place1 place2 ... placen), the values in place1 through placen are read and written. Values 2 through n and value 1 are then stored into place1 through placen. It is as if all the places form an end-around shift register that is rotated one place to the left, with the value of place1 being shifted around the end to placen.

For information about the evaluation of subforms of places, see Section 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

 (let ((n 0)
        (x (list 'a 'b 'c 'd 'e 'f 'g)))
    (rotatef (nth (incf n) x)
             (nth (incf n) x)
             (nth (incf n) x))
    x) =>  (A C D B E F G)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

define-setf-expander, defsetf, setf, shiftf, *macroexpand-hook*, Section 5.1 (Generalized Reference)

* 注意(Notes):

The effect of (rotatef place1 place2 ... placen) is roughly equivalent to

 (psetf place1 place2
        place2 place3
        ...
        placen place1)

except that the latter would evaluate any subforms of each place twice, whereas rotatef evaluates them once.


### <span id="">状况类型 CONTROL-ERROR</span>

Class Precedence List:

control-error, error, serious-condition, condition, t

* 描述(Description):

The type control-error consists of error conditions that result from invalid dynamic transfers of control in a program. The errors that result from giving throw a tag that is not active or from giving go or return-from a tag that is no longer dynamically available are of type control-error.


### <span id="">状况类型 PROGRAM-ERROR</span>

Class Precedence List:

program-error, error, serious-condition, condition, t

* 描述(Description):

The type program-error consists of error conditions related to incorrect program syntax. The errors that result from naming a go tag or a block tag that is not lexically apparent are of type program-error.


### <span id="">状况类型 UNDEFINED-FUNCTION</span>

Class Precedence List:

undefined-function, cell-error, error, serious-condition, condition, t

* 描述(Description):

The type undefined-function consists of error conditions that represent attempts to read the definition of an undefined function.

The name of the cell (see cell-error) is the function name which was funbound.

* 也见(See Also):

cell-error-name
