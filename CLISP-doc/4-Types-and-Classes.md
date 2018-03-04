# 4 类型和类

## 4.1 类型 [sec_4-2]

### 4.1.1 类型说明符 [sec_4-2-3]

这个 COMPLEX 类型声明的常用结构是 (COMPLEX type-of-real-part type-of-imaginary-part). 这个 (COMPLEX type) 类型声明等价于 (COMPLEX type type).

DEFTYPE lambda列表可以解构 (嵌套的lambda列表是允许的, 就像 DEFMACRO) 并且可能包含一个 &WHOLE 标识, 但是不是一个 &ENVIRONMENT 标识.

函数 (EXT:TYPE-EXPAND type &OPTIONAL once-p). 如果 type 是用户定义的类型声明这个会递归地展开它直到它不再是一个用户定义的类型 (除非提供了 once-p 并且不是 NIL). 返回2个值 - 这个展开式还有一个指示 (T 或者 NIL) 最初的 type 是否为用户定义的类型声明.

TYPE-OF 结果

    CONS
    SYMBOL, NULL, BOOLEAN, KEYWORD
    BIT, (INTEGER 0 #.MOST-POSITIVE-FIXNUM), (INTEGER #.MOST-NEGATIVE-FIXNUM (0)), (INTEGER (#.MOST-POSITIVE-FIXNUM)), (INTEGER * (#.MOST-NEGATIVE-FIXNUM))
    RATIONAL, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT, COMPLEX
    CHARACTER, BASE-CHAR, STANDARD-CHAR
    (ARRAY element-type dimensions), (SIMPLE-ARRAY element-type dimensions)
    (VECTOR T size), (SIMPLE-VECTOR size)
    (STRING size), (SIMPLE-STRING size)
    (BASE-STRING size), (SIMPLE-BASE-STRING size)
    (BIT-VECTOR size), (SIMPLE-BIT-VECTOR size)
    FUNCTION, COMPILED-FUNCTION, STANDARD-GENERIC-FUNCTION
    STREAM, FILE-STREAM, SYNONYM-STREAM, BROADCAST-STREAM, CONCATENATED-STREAM, TWO-WAY-STREAM, ECHO-STREAM, STRING-STREAM
    PACKAGE, HASH-TABLE, READTABLE, PATHNAME, LOGICAL-PATHNAME, RANDOM-STATE, BYTE
    SPECIAL-OPERATOR, LOAD-TIME-EVAL, SYMBOL-MACRO, GLOBAL-SYMBOL-MACRO, EXT:ENCODING, FFI:FOREIGN-POINTER, FFI:FOREIGN-ADDRESS, FFI:FOREIGN-VARIABLE, FFI:FOREIGN-FUNCTION
    EXT:WEAK-POINTER, EXT:WEAK-LIST, EXT:WEAK-AND-RELATION, EXT:WEAK-OR-RELATION, EXT:WEAK-MAPPING, EXT:WEAK-AND-MAPPING, EXT:WEAK-OR-MAPPING, EXT:WEAK-ALIST, READ-LABEL, FRAME-POINTER, SYSTEM-INTERNAL
    ADDRESS (不应该发生的)
    任何其他的 SYMBOL (结构类型或者 CLOS 类)
    一个类的对象 (没有正当名字的 CLOS 类)

函数 COERCE. FIXNUM 在 [ANSI CL standard] 中不是一个 character designator , 虽然 CODE-CHAR 提供了一个提供了一个显而易见的方式去强制转化 FIXNUM 为 CHARACTER. 当 CUSTOM:\*COERCE-FIXNUM-CHAR-ANSI\* 是 NIL, CLISP 强制通过CODE-CHAR 把 FIXNUM 转为 CHARACTER . 当 CUSTOM:\*COERCE-FIXNUM-CHAR-ANSI\* 不是 NIL, FIXNUM 不能 COERCE 强制转为 CHARACTER.

 
## 4.2 类 [sec_4-3]

这些 CLOS 符号从包 “CLOS” 中导出. “COMMON-LISP” 使用 (as in USE-PACKAGE) “CLOS” 并且 EXT:RE-EXPORT 这个 [ANSI CL standard] 标准导出的符号 (这个 CLISP 扩展, 例如, 那些描述在 Chapter 29, Meta-Object Protocol, 不是 EXT:RE-EXPORT 的). 因为默认给 MAKE-PACKAGE 的 :USE 参数是 “COMMON-LISP”, 这些标准的 CLOS 符号正常情况下对于所有用户定义的包都是可见的. 如果你不想要它们 (比如说, 如果你想使用 PCL 的 CLOS 实现而不是这里本地的), 执行以下操作:

```LISP
(DEFPACKAGE "CL-NO-CLOS" (:use "CL"))
(DO-EXTERNAL-SYMBOLS (symbol “COMMON-LISP”)
  (SHADOW symbol "CL-NO-CLOS"))
(DO-SYMBOLS (symbol "CL-NO-CLOS")
  (EXPORT symbol "CL-NO-CLOS"))
(IN-PACKAGE "CL-NO-CLOS")
(LOAD "pcl")    ; or whatever
(DEFPACKAGE "MY-USER" (:use "CL-NO-CLOS"))
(IN-PACKAGE "MY-USER")
;; your code which uses PCL goes here
```
 
## 4.3 背弃 [ANSI CL standard]

DEFCLASS 支持选项 :METACLASS STRUCTURE-CLASS. 这个选项在使用  DEFCLASS 而不是  DEFSTRUCT 来定义一个 DEFSTRUCT 定义出来的结构类型的子类是很有效的.

当 CALL-NEXT-METHOD 被调用时带了参数, 在解释代码时, 可应用的方法必须和最初的参数一致这个规则, 这是被 clisp 这个实现强制要求的.

CLOS:GENERIC-FLET 和 CLOS:GENERIC-LABELS 被实现为宏, 而不是特殊操作符 (就像在 [sec_3-1-2-1-2-2] 中允许的). 它们没有被导入包 “COMMON-LISP-USER” 和 “COMMON-LISP” 因为 [ANSI CL standard] 问题 GENERIC-FLET-POORLY-DESIGNED:DELETE.

PRINT-OBJECT 只会调用在 STANDARD-OBJECT 和 STRUCTURE-OBJECT 类型的对象上. 考虑到性能问题, 它不会调用在其他类似 CONS 和 NUMBER 对象上 .

 
## 4.4 标准的元类 [sec_4-3-1-1]

列在 Figure 4-8 的那些类中, 只有以下这些是 BUILT-IN-CLASS 的实例:

    T
    CHARACTER
    NUMBER, COMPLEX, REAL, FLOAT, RATIONAL, RATIO, INTEGER
    SEQUENCE
    ARRAY, VECTOR, BIT-VECTOR, STRING
    LIST, CONS
    SYMBOL, NULL
    FUNCTION
    HASH-TABLE
    PACKAGE
    PATHNAME, LOGICAL-PATHNAME
    RANDOM-STATE
    READTABLE
    STREAM, BROADCAST-STREAM, CONCATENATED-STREAM, ECHO-STREAM, STRING-STREAM, FILE-STREAM, SYNONYM-STREAM, TWO-WAY-STREAM

 
## 4.5 定义类 [sec_4-3-2]

DEFCLASS 支持 :METACLASS 选项. 可能的值是 STANDARD-CLASS (默认的), STRUCTURE-CLASS (创建结构类, 就像 DEFSTRUCT 所做的), 还有用户定义的元类 (见 Section 29.3.6.7, “Generic Function CLOS:VALIDATE-SUPERCLASS”).

在已经被求值的类的 DEFCLASS 结构之前定义它的超类不是必须的. 使用 Meta-Object Protocol 的普通函数 CLOS:CLASS-FINALIZED-P 去检测这个类是否已定然后就可以创建他的实例, 并且 CLOS:FINALIZE-INHERITANCE 会强制类的定稿.

也见 Section 29.3.1, “Macro DEFCLASS”.

 
## 4.6 重定义类 [sec_4-3-6]

平常的改变, 比如说, 那些重复加载同样的代码不需要去更新实例情况. 就是那些不改变实例中本地访问的槽的修改, 比如说, 对槽选项 :INITFORM, :DOCUMENTATION 的改变, 还有对类选项 :DEFAULT-INITARGS, :DOCUMENTATION 的改变.

不是在类被重定义或者  MAKE-INSTANCES-OBSOLETE 被调用时候, 而是实例被第一次访问时会更新. 自从实例被最后一次访问后, 类被重复定义好多次, UPDATE-INSTANCE-FOR-REDEFINED-CLASS 只调用一次.