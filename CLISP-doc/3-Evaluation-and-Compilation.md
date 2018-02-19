## 3.1 求值（Evaluation） [sec_3-1]

### 3.1.1 环境介绍（Introduction to Environments） [sec_3-1-1]

宏 EXT:THE-ENVIRONMENT. 就像 Scheme 中的, 这个宏 (EXT:THE-ENVIRONMENT) 返回当前的 词法环境. 这个只能用于解释的代码，不能编译!

函数 (EXT:EVAL-ENV form &OPTIONAL environment). 在给定的词法环境中对一个结构求值, 就像这个结构是环境 environment 所在程序的一部分.

### 3.1.2 符号作为结构（Symbols as Forms） [sec_3-1-2-1-1]

#### 3.1.2.1 宏 DEFINE-SYMBOL-MACRO

宏 DEFINE-SYMBOL-MACRO 定义全局作用域的 SYMBOL-MACRO (与 SYMBOL-MACROLET 定义的局部作用域的 SYMBOL-MACRO 对立).

函数 EXT:SYMBOL-MACRO-EXPAND 测试一个 SYMBOL-MACRO: 如果 symbol 定义为一个全局环境中的 SYMBOL-MACRO , (EXT:SYMBOL-MACRO-EXPAND symbol) 返回两个值, T 以及它的展开形式; 否则返回 NIL.

EXT:SYMBOL-MACRO-EXPAND 是 MACROEXPAND-1 的一个特例. MACROEXPAND-1 也可以测试一个 SYMBOL-MACRO 是定义在词法环境还是全局环境.

#### 3.1.2.2 动态变量（Dynamic Variables） [sec_3-1-2-1-1-2]

“Undefined variables”, 换句话说引用外部任何词法绑定的没有声明 SPECIAL 的同名变量都, 都被当作是全局环境中的动态变量. 编译器会在遇上未定义变量时发出一个 WARNING 警告.

### 3.1.3 序对作为结构（Conses as Forms） [sec_3-1-2-1-2]

((SETF symbol) ...) 表达式的列表也被当作函数表达式形式. 这使得语法 (function-name arguments ...) 和语法 (FUNCALL #'function-name arguments ...) 保持一致. 它实现了 [ANSI CL standard] 的条款7 FUNCTION-NAME:LARGE 问题以及函数表达式形式的定义的定义, 并且和 Common Lisp 其他地方的函数名的使用不冲突.

### 3.1.4 特殊结构（Special Forms） [sec_3-1-2-1-2-1]

#### 3.1.4.1 特殊操作符 EVAL-WHEN

EVAL-WHEN 也接受 (NOT EVAL) 和 (NOT COMPILE).

    警告

    这里的 EVAL, LOAD 和 COMPILE 情况是 [ANSI CL standard] 反对的, 并且他们和新的标准 :EXECUTE, :LOAD-TOPLEVEL and :COMPILE-TOPLEVEL 不等价，后者忽略顶层表达式和非顶层表达式的差异.

#### 3.1.4.2 特殊操作符 THE

这个特殊的结构 (THE value-type form) 类似于 CHECK-TYPE 但是只在解释的代码中做类型检查 (编译的代码不会进行类型检查 - 但是见 EXT:ETHE 宏) 并且不允许用户的交互式错误修正行为.

### 3.1.5 函数表达式形式 [sec_3-1-2-1-2-3]

不变的 LAMBDA-LIST-KEYWORDS. (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE &ENVIRONMENT)

#### 3.1.5.1 函数 SYMBOL-FUNCTION

(SETF (SYMBOL-FUNCTION symbol) object) 需要 object 是一个 FUNCTION, 一个 SYMBOL-FUNCTION 返回值, 或者一个 lambda expression. 这个 lambda expression 直接转换为 FUNCTION.

### 3.1.6 宏 DEFUN & DEFMACRO

DEFUN 和 DEFMACRO 允许出现在非顶层位置. 比如, 细想以前旧的 ([CLtL1]) GENSYM 定义:

```LISP
(let ((gensym-prefix "G")
      (gensym-count 1))
  (defun gensym (&optional (x nil s))
    (when s
      (cond ((stringp x) (setq gensym-prefix x))
            ((integerp x)
             (if (minusp x)
               (error "~S: index ~S is negative" 'gensym x)
               (setq gensym-count x)))
            (t (error "~S: argument ~S of wrong type" 'gensym x))))
    (prog1
      (make-symbol
        (concatenate 'string
          gensym-prefix
          (write-to-string gensym-count :base 10 :radix nil)))
      (incf gensym-count))))
```

也见 Section 3.2.2.2, “Minimal Compilation ”.

函数 EXT:ARGLIST. 函数 (EXT:ARGLIST name) 返回 name 指定的函数或宏的 lambda list 并且在 name 没有被 FBOUNDP 时发出一个 ERROR. 当这个宏 lambda list 由于编译器的最优化设置不可用时也会发出一个 ERROR  (见 Section 3.3.6, “Declaration SPACE”).

变量 CUSTOM:\*SUPPRESS-CHECK-REDEFINITION\*. 当 CUSTOM:\*SUPPRESS-CHECK-REDEFINITION\* 是 NIL,当一个函数 (宏, 变量, 类, 等等)除了最初的定义在其他文件中被重复定义时， CLISP 发出一个 WARNING . 把这个变量设置为 T 不是一个好主意.

变量 CUSTOM:\*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST\*. 当 CUSTOM:\*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST\* 不是 NIL, DEFUN 接受特定的lambda列表(specialized lambda lists), 把类型参数关联转换为类型声明:

```LISP
(defun f ((x list) (y integer)) ...)
```

等价于

```LISP
(defun f (x y) (declare (type list x) (type integer y)) ...)
```

这个扩展被 -ansi 和设置 CUSTOM:\*ANSI\* 为 T禁用, 但是可以通过设置 CUSTOM:\*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST\* 再次启用.
 
## 3.2 编译

### 3.2.1 编译器术语（Compiler Terminology）

CLISP 编译为平台相关的字节码 bytecode.

#### 3.2.1.1 即时本地编译（Just-In-Time Native Compilation）

平台依赖: 仅限带GNU lightning 构建的 CLISP

编译为带有优化级别

```LISP
(OR (>= 0 SPACE) (<= 1 SPEED))
```

(被 COMPILE, COMPILE-FILE, 或 (COMPILE) 编译) 字节码的代码会被 GNU lightning 即时 (换句话说, 在第一次执行时) 编译为本地代码.

### 3.2.2 编译语义（Compilation Semantics）

#### 3.2.2.1 编译器宏（Compiler Macros）

编译器的宏只在编译的代码中展开, 会被解释器所忽略.

#### 3.2.2.2 最小编译（Minimal Compilation）

当一个 DEFUN 结构被求值了, 在这里的宏会被展开, 所以它们必须是已经定义了的, 并且它们的(重)定义不会影响到已经定义的函数.

这个意味着解释的代码是在 CLISP 中最小编译的.

#### 3.2.2.3 语义限制（Semantic Constraints）

不遵守以下规则

    “动态变量的特殊公告(proclamation)必须在编译环境中”

的代码可能产生无法预料的结果, 比如, 编译的和解释的代码中可观测的区别:

```LISP
(defun adder-c (value) (declare (COMPILE)) (lambda (x) (+ x value)))
⇒ ADDER-C    ; compiled function; value is lexical
(defun adder-i (value) (lambda (x) (+ x value)))
⇒ ADDER-I    ; interpreted function; value is lexical
(defparameter add-c-10 (adder-c 10))
⇒ ADD-C-10    ; compiled function
(defparameter add-i-10 (adder-i 10))
⇒ ADD-I-10    ; interpreted function
(funcall add-c-10 32)
⇒ 42    ; as expected
(funcall add-i-10 32)
⇒ 42    ; as expected
(defvar value 12)
⇒ VALUE    ; affects ADDER-I and ADD-I-10 but not ADDER-C and ADD-C-10
(funcall add-c-10 32)
⇒ 42    ; as before
(funcall add-i-10 32)
⇒ 44    ; value is now dynamic!
```

非一致性. 这个代码展示了在执行环境中变量 value (在最后两个 FUNCALL 前)有一个 SPECIAL 声明 (通过 DEFVAR) 但是在编译环境中没有: 这个时候 ADDER-I 函数是定义两, value 不会被认为是 SPECIAL 变量. 因此这个代码是不一致的.
原理

函数 ADD-C-10 在 value 被声明为 SPECIAL 之前被编译了，所以符号 value 从代码里被消除并且这个 SPECIAL 声明不会影响到返回值 (换句话说, (funcall add-c-10 32) 总是返回32).

相反的, 函数 ADDER-I 没有被编译, 所以 ADD-I-10 被解释. 无论何时 ADD-I-10 被执行, 它的定义会再从头解释一遍. 在 DEFVAR 之前， value 作为一个词法变量求值 (因为还没有被声明为 SPECIAL ), 但是在 DEFVAR 之后, 我们看到一个全局的 SPECIAL 符号 value ，它只能有一个全局的 SYMBOL-VALUE (不是一个本地绑定), 所以我们不得不把它求值为12.

这个行为是故意实现为这样以便交互式开发, 因为通常上述 ADDER-I 会跟随着 (forgotten) DEFVAR.

当一个用户编译了一个程序, 编译器被允许记住一个变量是否为 SPECIAL , 因为这样可以让编译器去生成更有效的代码, 但是在解释的代码里, 当用户去改变这个变量的状态, 他不想为使用这个变量再重新求值 DEFUN.

[ANSI CL standard] 关于解释的代码求值给了实现的自由, 它想去记住或缓存多少, 还有如果它改变了，根据当前的环境它想去重复求值多少. CLISP 为变量实现了 ad-hoc look-up (但是没有为宏实现, 见 Section 3.2.2.2, “Minimal Compilation ”).

### 3.2.3 定义的相似性（Definition of Similarity）

哈希表是可以序列化的对象.

### 3.2.4 编译器中的例外的位置（Situations in the Compiler）

不管是 COMPILE 还是 EVAL 都可能发出来自于 PROGRAM-ERROR 的、包含额外槽和访问器函数的  EXT:SOURCE-PROGRAM-ERROR状态

EXT:SOURCE-PROGRAM-ERROR-FORM

    返回报出错误的整个结构

EXT:SOURCE-PROGRAM-ERROR-DETAIL

    返回触发错误的具体（通常是短小的）部分

## 3.3 声明（Declarations）

声明 (TYPE type variable ...), (FTYPE type function ...), 都会被解释器和编译器所忽略.

### 3.3.1 声明 SPECIAL

声明 EXT:NOTSPECIAL. 声明 (PROCLAIM '(SPECIAL variable)) 和 DEFCONSTANT 可以被 (PROCLAIM '(EXT:NOTSPECIAL variable)) 声明撤销. 这个声明只在全局的 PROCLAIM 和 DECLAIM 结构总可以被使用, 而不是在局部的 DECLARE 结构.

    警告

    你不能期望奇迹: 在 EXT:NOTSPECIAL 声明之前编译的函数会出问题，会一直认为 variable 是特殊变量甚至在 EXT:NOTSPECIAL 声明后. 也见 Section 3.2.2.3, “Semantic Constraints ”.

函数 EXT:SPECIAL-VARIABLE-P. 你可以使用函数 (EXT:SPECIAL-VARIABLE-P symbol &OPTIONAL environment) 去检查 symbol 是否为一个自由变量. environment 是 NIL 或者没有提供意味着试用全局的environment. 你可以通过宏 EXT:THE-ENVIRONMENT (仅限解释的代码) 来或者当前的词法环境. 对于全局特殊变量和常量，这个函数总是会返回 T .

### 3.3.2 声明 EXT:CONSTANT-NOTINLINE

常量通过 DEFCONSTANT 定义，但是通过 EXT:CONSTANT-NOTINLINE 公告的会被编译器内联. 这个有利于保存常量在单个进程中，但是可能在进程间或机器间发生变化的 (例如字节顺序和字大小) 的变量 所以它们应该被作为符号而不是值写到 #P".fas" 中.

### 3.3.3 函数 CONSTANTP

函数 CONSTANTP 完全遵从 [ANSI CL standard]. 另外, 一些有意义的结构也被认为是常量, 例如, (CONSTANTP '(+ 1 2 3)) 返回 T.

    警告

    因为 DEFCONSTANT 初始化值结构不是在编译时求值,对于null的词法环境, 在相同的编译单元中 CONSTANTP 不会报告 T of their name . 这是不矛盾的并且可以使用 (IF (CONSTANTP form) (EVAL form)) 来匹配有疑问的代码. 如果你需要在编译时识别并取值，应该使用 EVAL-WHEN . 也见 Section 31.11.5, “Macro EXT:COMPILE-TIME-VALUE”.

### 3.3.4 声明 SAFETY

声明 (OPTIMIZE (SAFETY 3)) 发生在 “安全” 的编译的代码中: 函数调用从来不会消除. 这个确保在 [sec_3-5] 中描述的语义。

### 3.3.5 声明 (COMPILE)

这个 (COMPILE) 声明导致当前的结构在执行前被编译. 例如:

```LISP
(LOCALLY (DECLARE (compile)) form)
```

执行 form 的编译版本.

```LISP
(LET ((x 0))
  (FLET ((inc () (DECLARE (compile)) (INCF x))
         (dec () (DECF x)))
    (VALUES #'inc #'dec)))
```

返回两个函数. 第一个是编译的，增加 x, 第二个是解释的 (更慢一些) 减少同样的 x.

这个声明也可以用于命名编译后的闭包:

```LISP
(LAMBDA (x) (DECLARE (compile ident)) x)
⇒ #<COMPILED-FUNCTION IDENT>
(FUNCTION-LAMBDA-EXPRESSION *)
⇒ NIL    ; source is not preserved
⇒ T
⇒ IDENT
(FBOUNDP 'ident)
⇒ NIL    ; sic!
```

    注意

    这个声明 (COMPILE) 被一下特殊操作符忽略:
    LABELS
    FLET
    MACROLET

### 3.3.6 声明 SPACE

这个声明决定了哪些元数据保存在函数对象中:

SPACE >= 2

    文档字符串被丢弃

SPACE >= 3

    起初的lambda也被丢弃 (大部分信息还是可用, 见 DESCRIBE, 但是参数的名字不可用).

## 3.4 Lambda Lists

### 3.4.1 Boa Lambda Lists [sec_3-4-6]

在一个boa lambda列表中的一个 &AUX 变量初始值是对应槽的初始结构的值.