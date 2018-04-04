## 5.1 广义的引用（Generalized Reference）

        &KEY 标识允许出现在 DEFSETF lambda列表中, 但是对应的关键字也必须出想在程序文本中.

        任何试图去修改只读数据都会发出一个 ERROR. 从文件里载入的程序文本还有引用常量都被认为是只读数据. 这个检测只为字符串执行, 不包括cons, 其他类型的数组, 还有用户定义的数据类型.

        也见 Section 31.11.2, “Macros EXT:LETF & EXT:LETF*”.
        
## 5.2 setf的展开（Setf Expansions）

        (GET-SETF-EXPANSION form &OPTIONAL environment), (EXT:GET-SETF-METHOD form &OPTIONAL environment), 还有 (EXT:GET-SETF-METHOD-MULTIPLE-VALUE form &OPTIONAL environment) 都接受一个可选的参数 environment ，这里的environment在宏展开时是很有必要的. 在 DEFINE-SETF-EXPANDER 和 EXT:DEFINE-SETF-METHOD lambda 列表 中, 可以指定一个 &ENVIRONMENT 和一个变量, 这个变量会绑定给 environment. 这个 environment 应该被传递给 所有的 GET-SETF-EXPANSION, EXT:GET-SETF-METHOD 和 EXT:GET-SETF-METHOD-MULTIPLE-VALUE 调用. 如果这个被执行了, 甚至本地的宏会被解释为适当的 place.

## 5.3 place的种类（Kinds of Places）

        另外 places:

        FUNCALL
            (SETF (FUNCALL #'symbol ...) object) 和 (SETF (FUNCALL 'symbol ...) object) 等价于 (SETF (symbol ...) object).
            
        PROGN
            (SETF (PROGN form ... place) object)

        LOCALLY
            (SETF (LOCALLY declaration ... form ... place) object)

        IF
            (SETF (IF condition place1 place2) object)

        GET-DISPATCH-MACRO-CHARACTER
            (SETF (GET-DISPATCH-MACRO-CHARACTER ...) ...) 调用 SET-DISPATCH-MACRO-CHARACTER.

        EXT:LONG-FLOAT-DIGITS:
            (SETF (EXT:LONG-FLOAT-DIGITS) digits) 设置默认的 LONG-FLOAT 尾数长度为 digits 位.

        VALUES-LIST
            (SETF (VALUES-LIST list) form) 等价于 (VALUES-LIST (SETF list (MULTIPLE-VALUE-LIST form))).
            注意

            注意这个 place 是受限的: 它只能用于 SETF, EXT:LETF, EXT:LETF*, 不能用于其他的.

## 5.4 Miscellaneous

        函数 FUNCTION-LAMBDA-EXPRESSION
            一个 FFI:FOREIGN-FUNCTION 的名字是一个字符串 (下层 C 函数的名字), 不是一个lisp function name.

        宏 DESTRUCTURING-BIND
            这个宏不会去执行全部的错误检查.

        宏 PROG1, PROG2, AND, OR, PSETQ, WHEN, UNLESS, COND, CASE, MULTIPLE-VALUE-LIST, MULTIPLE-VALUE-BIND, MULTIPLE-VALUE-SETQ
            这些宏被实现为特殊操作符 (被 [sec_3-1-2-1-2-2] 许可的) , 所以它们的性能很高.
            
## 5.5 宏 DEFCONSTANT

        这个初始值不会在编译时求值, 就像 DEFVAR 和 DEFPARAMETER. 如果你在编译的时候需要这个值就使用 EVAL-WHEN .

        constant variables 可能不会被动态或者词法绑定.

        也见 Section 3.3.2, “Declaration EXT:CONSTANT-NOTINLINE”.

        如果你需要去撤销一个 DEFCONSTANT 结构的影响, 声明这个符号为 SPECIAL (去把一个常量转化为动态变量), 然后声明它为 EXT:NOTSPECIAL (把这个动态变量转化为词法变量).

            注意

            如果你遵守通常的变量命名规则 ( DEFVAR 和 DEFPARAMETER 为*FOO* ,DEFCONSTANT 为 +BAR+ , LET/LET* 为 ZOT)， 你会为自己减少很多麻烦. 也见 Q: A.4.14.
            
## 5.5.1. 变量 CUSTOM:\*SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING\*

        如果被 DEFCONSTANT 定义的变量已经绑定一个值，它不和新的值 EQL , 会发出一个 WARNING . 然而，如果新值看上去和旧值相似 (打印为同一个字符串, 就像重复加载文件那样), 可以通过设置 CUSTOM:*SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* 为一个非NIL的值来抑制这个警告.

        CUSTOM:*SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* 的初始值是 NIL.
        
## 5.6 宏 EXT:FCASE

        这个宏允许为 CASE 指定测试的值，比如,

```LISP
(fcase string= (subseq foo 0 (position #\Space foo))
  ("first" 1)
  (("second" "two") 2)
  (("true" "yes") t)
  (otherwise nil))
```

        和以下结构一样

```LISP
(let ((var (subseq foo 0 (position #\Space foo))))
  (cond ((string= var "first") 1)
        ((or (string= var "second") (string= var "two")) 2)
        ((or (string= var "true") (string= var "yes")) t)
        (t nil)))
```

        如果你使用内置的 HASH-TABLE 测试方式 (见 Section 18.4, “Function HASH-TABLE-TEST”) 作为这个测试值 (比如, EQUAL 而不是上述 STRING= , 但不是用  EXT:DEFINE-HASH-TABLE-TEST 定义的测试方式), 编译器就可能会优化这个 EXT:FCASE 结构，优于对应的 COND 结构.
        
## 5.7 函数 EXT:XOR

        这个函数检查它的参数中是否一个为非NIL, 如果是的话, 以多值返回它的值和在参数列表中的索引否则返回 NIL.

## 5.8 函数 EQ

        EQ 比较 CHARACTER 和 FIXNUM 就像 EQL 所做的. 没有不必要的 CHARACTER 和 NUMBER 的拷贝. 然而, 还是应该使用 EQL 因为它跨 Common Lisp 实现时具有更好的移植性.

        (LET ((x y)) (EQ x x)) 总是返回 T 对于任何的Lisp对象 y. 也见 Equality of foreign values.

## 5.9 特殊操作符 FUNCTION

        (FUNCTION symbol) 如果由 FLET or LABELS 建立的局部的函数定义存在就返回这个, 否则返回全局函数定义.

        (SPECIAL-OPERATOR-P symbol) 返回 NIL 或者 T. 如果它返回 T, 那么 (SYMBOL-FUNCTION symbol) 返回这个特殊操作符的处理者 (无用的) .