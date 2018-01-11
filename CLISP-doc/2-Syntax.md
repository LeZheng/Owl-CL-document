# 2.1 标准字符（Standard Characters） [sec_2-1-3]

标准字符就是 #\Newline 以及用32到126之间的 CODE-CHAR 表示的字符.

 
# 2.2 读取算法（Reader Algorithm） [sec_2-2]

步骤4的需要“reader macro function 可能会返回0个值或者一个值” 是强制的. 你可以使用函数 VALUES 来控制返回值的个数.

 
#2.3 符号作为token（Symbols as Tokens） [sec_2-3-4]

一个 reserved token , 换句话说, 一个包含潜在数字语法但是不能被解释为一个 NUMBER 的 token , 在被读入时解释为 SYMBOL .
 
# 2.4 Token的合法模式（Valid Patterns for Tokens） [sec_2-3-5]

当一个带有包标记的token被读入时，并且没有检查这个 SYMBOL-PACKAGE 部分和 SYMBOL-NAME 部分没有数字语法. (这个检查的目的会是什么呢?) 所以我们把 tokens 类似 USER:: 或者 :1 或者 LISP::4711 或者 21:3 作为符号.

 
# 2.5 反引号（Backquote） [sec_2-4-6]

这里嵌套的反引号在读取宏的时候也正常工作. 示例:

```LISP
(EVAL ``(,#'(LAMBDA () ',a) ,#'(LAMBDA () ',b)))
 ≡ (EVAL `(list #'(LAMBDA () ',a) #'(LAMBDA () ',b)))
 ≡ (EVAL (list 'list (list 'function (list 'lambda nil (list 'quote a)))
                     (list 'function (list 'lambda nil (list 'quote b)))))
```
 
# 2.6 Sharpsign [sec_2-4-8]

读取宏也用以下方式定义:

额外的读取宏

\#,

    加载时的求值, 被保留了，虽然存在 [ANSI CL standard] 问题 SHARP-COMMA-CONFUSION:REMOVE.

\#Y

    编译后的 FUNCTION 对象还有输入 STREAM的 EXT:ENCODINGs

\#""

    PATHNAME: #"test.lisp" 是 (PATHNAME "test.lisp" 的值


## 2.6.1 Sharpsign 反斜线字符 [sec_2-4-8-1]

\#\Code 允许输入任意代码: 比如, #\Code231 读取到 (CODE-CHAR 231).

## 2.6.2 Sharpsign Less-Than-Sign [sec_2-4-8-20]

这个是额外表示不能被读入的对象列表:

不可读对象

    #<type ...>
        所有缺少关键字构造器的 STRUCTURE-OBJECT.

    #<ARRAY type dimensions>
        所有的 ARRAY 除了 STRING, 在 *PRINT-ARRAY* 是 NIL 的情况下

    #<SYSTEM-FUNCTION name>
        用C写的内置的函数

    #<ADD-ON-SYSTEM-FUNCTION name>
        用C写的模块函数

    #<SPECIAL-OPERATOR name>
        特殊操作符

    #<COMPILED-FUNCTION name>
        编译后的函数, 如果 CUSTOM:*PRINT-CLOSURE* 是 NIL

    #<FUNCTION name ...>
        解释的函数 ,如果 CUSTOM:*PRINT-CLOSURE* 是 NIL

    #<FRAME-POINTER #x...>
        指向栈边界的指针

    #<DISABLED POINTER>
        从对应的  BLOCK 或者 TAGBODY 退出就变为无效的边界指针

    #<...STREAM...>
        STREAM

    #<PACKAGE name>
        PACKAGE

    #<HASH-TABLE #x...>
        HASH-TABLE, 如果 *PRINT-ARRAY* 是 NIL 的话

    #<READTABLE #x...>
        READTABLE

    #<SYMBOL-MACRO form>
        SYMBOL-MACRO

    #<MACRO function>
        macro expander (被 DEFMACRO 和friends定义的)

    #<FFI:FOREIGN-POINTER #x...>
        外部指针 (平台依赖: 仅限 UNIX, Win32.)

    #<FFI:FOREIGN-ADDRESS #x...>
        外部地址 (平台依赖: 仅限 UNIX, Win32.)

    #<FFI:FOREIGN-VARIABLE name #x...>
        外部变量 (平台依赖: 仅限 UNIX, Win32.)

    #<FFI:FOREIGN-FUNCTION name #x...>
        外部函数 (平台依赖: 仅限 UNIX, Win32.)

    #<UNBOUND>
        一个没绑定符号或没提供的选项或关键字参数的“value”

    #<SPECIAL REFERENCE>
        用 SPECIAL 声明的环境标识

    #<DOT>
        内部的 “.”读取结果

    #<END OF FILE>
        当到达 end-of-stream  时，这是内部的读取结果

    #<READ-LABEL ...>
        #n# 的中间读取结果

    #<ADDRESS #x...>
        不应该出现的机器地址

    #<SYSTEM-POINTER #x...>
        不应该出现的