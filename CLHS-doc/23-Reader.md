# 23 读取器

> * 23.1 [读取器的概念](#ReaderConcepts)
> * 23.2 [读取器的字典](#TheReaderDictionary)

## 23.1 <span id="ReaderConcepts">读取器的概念</span>

> * 23.1.1 [Lisp 读取器的动态控制](#DynamicControlLispReader)
> * 23.1.2 [Lisp 读取器上的读取表大小写的影响](#EffectReadtableLispReader)
> * 23.1.3 [一些读取器函数的参数转换](#ArgConventSomeReaderFun)


### 23.1.1 <span id="DynamicControlLispReader">Lisp 读取器的动态控制</span>

Lisp 读取器的各个方面可以被动态控制. 见章节 2.1.1 (Readtables) 和章节 2.1.2 (Variables that affect the Lisp Reader). 

### 23.1.2 <span id="EffectReadtableLispReader">Lisp 读取器上的读取表大小写的影响</span>

当前读取表的读取表大小写以以下方式影响 Lisp 读取器:

:upcase

    当读取表大小写是 :upcase 时, 未转义的成分字符会被转换为大写, 如章节 2.2 (Reader Algorithm) 中所指定的那样.

:downcase

    当读取表大小写是 :downcase 时, 未转义的成分字符会被转换为小写.

:preserve

    当读取表大小写是 :preserve 时, 所有字符的大小写保持不变.

:invert

    当读取表大小写是 :invert 时, 那么如果所有这些未转义字母在扩展 token 中是相同类型的, 这些 (未转义的) 字母会被转换为相反的类型.

#### 23.1.2.1 Lisp 读取器上的读取表大小写的影响的示例

```LISP
(defun test-readtable-case-reading ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  Input   Symbol-name~
              ~%-----------------------------------~
              ~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (setf (readtable-case *readtable*) readtable-case)
      (dolist (input '("ZEBRA" "Zebra" "zebra"))
        (format t "~&:~A~16T~A~24T~A"
                (string-upcase readtable-case)
                input
                (symbol-name (read-from-string input)))))))
```

来自 (test-readtable-case-reading) 的输出 应该如下:

| READTABLE-CASE  |   Input | Symbol-name |
| --              |--       |--           |
|  :UPCASE        | ZEBRA   | ZEBRA       |
|  :UPCASE        | Zebra   | ZEBRA       |
|  :UPCASE        | zebra   | ZEBRA       |
|  :DOWNCASE      | ZEBRA   | zebra       |
|  :DOWNCASE      | Zebra   | zebra       |
|  :DOWNCASE      | zebra   | zebra       |
|  :PRESERVE      | ZEBRA   | ZEBRA       |
|  :PRESERVE      | Zebra   | Zebra       |
|  :PRESERVE      | zebra   | zebra       |
|  :INVERT        | ZEBRA   | zebra       |
|  :INVERT        | Zebra   | Zebra       |
|  :INVERT        | zebra   | ZEBRA       |


### 23.1.3 <span id="ArgConventSomeReaderFun">一些读取器函数的参数转换</span>

#### 23.1.3.1 EOF-ERROR-P 参数

在输入函数调用中的 eof-error-p 控制如果输入来自于一个文件(或者任何其他有着有限结尾的输入源)并且到达了文件的末尾会发生什么. 如果 eof-error-p 是 true (默认的), 就会在文件的结尾发出一个 end-of-file 类型的错误. 如果它是 false, 那么没有错误会发生, 并且这个函数返回 eof-value.

像 read 这样读取一个对象的打印表示而不是单个字符的函数, 如果文件结束在一个对象打印表示的中间, 不管 eof-error-p, 都会发出一个错误. 比如, 如果一个文件确实不包含足够的右括号来平衡它里面的右括号, read 就会发出一个错误. 如果一个文件以一个符号或一个数字后面直接跟着 end-of-file 终止. 类似地, 函数 read-line 成功地读取一个文件的最后一行, 即便那行用 end-of-file 终止而不是用一个换行字符. 可忽略的文本, 例如只包含空格或注释, 不会被认为开始一个对象; 如果 read 开始去读取一个表达式但是只看到这样的可忽略文本, 它不会认为这个文件终止在一个对象的中间. 因此一个 eof-error-p 参数控制当文件在对象之间结束时会发生什么. 

#### 23.1.3.2 RECURSIVE-P 参数

如果 recursive-p 被提供并且不是 nil, 它指明这个函数调用不是一个对 read 的最外部的调用而是一个内嵌的调用, 通常来自于一个读取器宏函数. 区分这样的递归调用是很重要的, 原因有三.

1. 一个最外部的调用建立上下文, 这个 #n= 和 #n# 语法在这个上下文中被审视. 细想, 例如, 表达式

    ```LISP
    (cons '#3=(p q r) '(x y . #3#))
    ```

    如果这个单引号读取器宏用这种方式定义:

    ```LISP
    (set-macro-character #\'       ;incorrect
      #'(lambda (stream char)
            (declare (ignore char))
            (list 'quote (read stream))))
    ```

    那么每一个对这个单引号读取器宏函数的调用会为 read 信息的作用域建立一个独立的上下文, 包括在像 "#3=" 和 "#3#" 这样的标记之间的识别作用域. 但是, 对于这个表达式, 这个作用域显然是由更外面的一组括号来决定的, 所以这样一个定义是不正确的. 正确的方式去定义这个单引号读取器宏是使用 recursive-p:

    ```LISP
    (set-macro-character #\'       ;correct
      #'(lambda (stream char)
            (declare (ignore char))
            (list 'quote (read stream t nil t))))
    ```

2. 一个递归调用不会改变这个读取过程是否保留空格 (由这个最外面的调用是否为 read 或 read-preserving-whitespace 决定). 再一次假设这个单引号被定义为上面不正确的定义所展示的那样. 然后一个读取表达式 'foo<Space> 的对 read-preserving-whitespace 的调用会无法保留符号 foo 后的空白字符, 因为这个单引号读取器宏函数调用 read 而不是 read-preserving-whitespace 去读取后面的表达式 (在这个情况 foo). 传递给 read 的 recursive-p 值为 true 的正确定义允许最外部的调用去决定是否保留空格.

3. 当遇到 end-of-file 并且 eof-error-p 参数不是 nil 时, 发送的错误的种类可能取决于 recursive-p 的值. 如果 recursive-p 是 true, 那么这个 end-of-file 被认为已经出现在一个打印表示的中间; 如果 recursive-p 是 false, 那么这个 end-of-file 可能被认为已经出现在对象之间而不是在一个的中间. 

## 23.2 <span id="TheReaderDictionary">读取器的字典</span>

> * [系统类 READTABLE](#SC-READTABLE)
> * [函数 COPY-READTABLE](#F-COPY-READTABLE)
> * [函数 MAKE-DISPATCH-MACRO-CHARACTER](#F-MDMC)
> * [函数 READ, READ-PRESERVING-WHITESPACE](#F-READ-AND-RPW)
> * [函数 READ-DELIMITED-LIST](#F-READ-DELIMITED-LIST)
> * [函数 READ-FROM-STRING](#F-READ-FROM-STRING)
> * [访问器 READTABLE-CASE](#A-READTABLE-CASE)
> * [函数 READTABLEP](#F-READTABLEP)
> * [函数 SET-DISPATCH-MACRO-CHARACTER, GET-DISPATCH-MACRO-CHARACTER](#F-SET-AND-GET-DMC)
> * [函数 SET-MACRO-CHARACTER, GET-MACRO-CHARACTER](#F-SET-AND-GET-MC)
> * [函数 SET-SYNTAX-FROM-CHAR](#F-SET-SYNTAX-FROM-CHAR)
> * [宏 WITH-STANDARD-IO-SYNTAX](#M-WITH-STANDARD-IO-SYNTAX)
> * [变量 *READ-BASE*](#V-READ-BASE)
> * [变量 *READ-DEFAULT-FLOAT-FORMAT*](#V-READ-DEFAULT-FLOAT-FORMAT)
> * [变量 *READ-EVAL*](#V-READ-EVAL)
> * [变量 *READ-SUPPRESS*](#V-READ-SUPPRESS)
> * [变量 *READTABLE*](#V-READTABLE)
> * [状况类型 READER-ERROR](#CT-READER-ERROR)


### <span id="SC-READTABLE">系统类 READTABLE</span>

* 类优先级列表(Class Precedence List):

        readtable, t

* 描述(Description):

        一个读取表映射字符到 Lisp 读取器的语法类型; 见章节 2 (Syntax). 一个读取表也包含宏字符和它们的读取器宏函数之间的关联, 并且记录当解析符号时, 关于 Lisp 读取器使用的大小写转换规则的信息.

        每一个简单字符在这个读取表中必须是可以表示的. 非简单字符是否在读取表中是否可以有语法描述是具体实现定义的.

* 也见(See Also):

        章节 2.1.1 (Readtables), 章节 22.1.3.13 (Printing Other Objects) 


### <span id="F-COPY-READTABLE">函数 COPY-READTABLE</span>

* 语法(Syntax):

        copy-readtable &optional from-readtable to-readtable => readtable

* 参数和值(Arguments and Values):

        from-readtable---一个读取表标识符. 默认是当前读取表.
        to-readtable---一个读取表或 nil. 默认是 nil.
        readtable---如果它不是 nil 那么就是 to-readtable, 否则就是一个新的读取表.

* 描述(Description):

        copy-readtable 复制 from-readtable.

        如果 to-readtable 是 nil, 一个新的读取表会被创建并返回. 否则由 to-readtable 指定的读取表会被修改并返回.

        copy-readtable 复制 readtable-case 的设置.

* 示例(Examples):

    ```LISP
    (setq zvar 123) =>  123
    (set-syntax-from-char #\z #\' (setq table2 (copy-readtable))) =>  T
    zvar =>  123
    (copy-readtable table2 *readtable*) =>  #<READTABLE 614000277>
    zvar =>  VAR
    (setq *readtable* (copy-readtable)) =>  #<READTABLE 46210223>
    zvar =>  VAR
    (setq *readtable* (copy-readtable nil)) =>  #<READTABLE 46302670>
    zvar =>  123
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        readtable, *readtable*

* 注意(Notes):

        (setq *readtable* (copy-readtable nil))

        恢复输入语法未标准的 Common Lisp 语法, 即便那个初始的读取表已经被重写 (假定它没有被恶劣的重写以至于你不能输入上述表达式).

        另一方面,

        (setq *readtable* (copy-readtable))

        用当前表自身的一个拷贝替换当前表. 如果你想去保存一个读取表的拷贝用于后面的使用, 同时又不需要修改, 那么这是非常有用的. 如果你想局部绑定这个读取表为它自身的一个拷贝, 这也是很有用的, 像下面这样:

        (let ((*readtable* (copy-readtable))) ...)


### <span id="F-MDMC">函数 MAKE-DISPATCH-MACRO-CHARACTER</span>

* 语法(Syntax):

        make-dispatch-macro-character char &optional non-terminating-p readtable => t

* 参数和值(Arguments and Values):

        char---一个字符.
        non-terminating-p---一个广义 boolean. 默认是 false.
        readtable---一个读取表. 默认是当前读取表.

* 描述(Description):

        make-dispatch-macro-character 使 char 成为表 readtable 中的一个分派宏字符.

        首先, 在这个分派表中和 char 关联的每一个字符有一个发出 reader-error 类型的错误的关联函数.

        如果 non-terminating-p 是 true, 这个分派宏字符会是一个非终止宏字符; 如果 non-terminating-p 是 false, 这个分派宏字符会是一个终止宏字符.

* 示例(Examples):

    ```LISP
    (get-macro-character #\{) =>  NIL, false
    (make-dispatch-macro-character #\{) =>  T
    (not (get-macro-character #\{)) =>  false
    ```

* 副作用(Side Effects): None.

        这个读取表 readtable 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        *readtable*, set-dispatch-macro-character

* 注意(Notes): None. 


### <span id="F-READ-AND-RPW">函数 READ, READ-PRESERVING-WHITESPACE</span>

* 语法(Syntax):

        read &optional input-stream eof-error-p eof-value recursive-p => object

        read-preserving-whitespace &optional input-stream eof-error-p eof-value recursive-p
        => object

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        object---一个对象 (由 Lisp 读取器解析的) 或者 eof-value.

* 描述(Description):

        read 从输入流 input-stream 解析一个对象的打印表示并且构建这样一个对象.

        read-preserving-whitespace 类似于 read 但是保留任何分隔这个对象的打印表示的空格字符. 当给 read-preserving-whitespace 的 recursive-p 参数是 true 时 read-preserving-whitespace 和 read 一样.

        当 *read-suppress* 是 false 时, 如果是空白字符, read 会丢弃某些打印表示所需要的分隔字符; 但是如果它是语法上有意义的, 那么 read 会保留这个字符 (使用 unread-char), 因为它可以是下一个表达式的开始.

        如果一个文件终止于一个符号或一个数字后直接跟着文件的末尾, read 成功地读取这个符号或数字; 当再一次调用时, 它见到文件的末尾并且根据 eof-error-p 行动. 如果一个文件在末尾包含了可忽略的文本, 例如空白行和注释, read 不会把这个当作终止于一个对象的中间.

        如果 recursive-p 是 true, 这个对 read 的调用应该是由某个函数内部生成的, 而这个函数本身是由 read 或类似的输入函数调用的, 而不是从顶层调用的.

        两个函数都返回从 input-stream 中读取到的函数. 如果 eof-error-p 是 false 并且在开始一个对象前到达了文件的末尾, 那么返回 eof-value.

* 示例(Examples):

    ```LISP
    (read)
    >>  'a
    =>  (QUOTE A)
    (with-input-from-string (is " ") (read is nil 'the-end)) =>  THE-END
    (defun skip-then-read-char (s c n)
        (if (char= c #\{) (read s t nil t) (read-preserving-whitespace s))
        (read-char-no-hang s)) =>  SKIP-THEN-READ-CHAR
    (let ((*readtable* (copy-readtable nil)))
        (set-dispatch-macro-character #\# #\{ #'skip-then-read-char)
        (set-dispatch-macro-character #\# #\} #'skip-then-read-char)
        (with-input-from-string (is "#{123 x #}123 y")
          (format t "~S ~S" (read is) (read is)))) =>  #\x, #\Space, NIL
    ```

        作为一个示例, 思考这个读取器宏定义:

    ```LISP
    (defun slash-reader (stream char)
      (declare (ignore char))
      `(path . ,(loop for dir = (read-preserving-whitespace stream t nil t)
                      then (progn (read-char stream t nil t)
                                  (read-preserving-whitespace stream t nil t))
                      collect dir
                      while (eql (peek-char nil stream nil nil t) #\/))))
    (set-macro-character #\/ #'slash-reader)
    ```

        思考现在在这个表达式上调用 read:

    ```LISP
    (zyedh /usr/games/zork /usr/games/boggle)
    ```

        这个 / 读取器宏读取由多个 / 字符分隔的对象; 因此 /usr/games/zork 打算被读取为 (path usr games zork). 这整个示例表达式应该因此被读取为

    ```LISP
    (zyedh (path usr games zork) (path usr games boggle))
    ```

        然而, 如果 read 已经被使用而不是 read-preserving-whitespace, 那么在符号 zork 的读取后, 后面的空格会被丢弃; 下一个对 peek-char 的调用会见到后面的 /, 并且这个循环会继续, 产生这个解释:

    ```LISP
    (zyedh (path usr games zork usr games boggle))
    ```

        有时候空白应该被丢弃. 入股一个命令解释器接受单字符命令, 但是偶尔读取一个对象, 那么如果一个符号后的空格没有被丢弃, 它有时可能在读取这个符号后被解释为一个命令.

* 受此影响(Affected By):

        *standard-input*, *terminal-io*, *readtable*, *read-default-float-format*, *read-base*, *read-suppress*, *package*, *read-eval*.

* 异常情况(Exceptional Situations):

        如果文件终止于一个对象表示的中间, 那么不管 eof-error-p 的值, read 发出一个 end-of-file 类型的错误. 例如, 如果一个文件没有包含足够的右括号来平衡它里面的左括号, read 就会发出一个错误. 当 read 或 read-preserving-whitespace 被调用时 recursive-p 和 eof-error-p 不是 nil, 并且在一个对象开始前到达了文件的末尾, 这个会被检测到.

        如果 eof-error-p 是 true, 在文件的末尾发出一个 end-of-file 类型的错误.

* 也见(See Also):

        peek-char, read-char, unread-char, read-from-string, read-delimited-list, parse-integer, 章节 2 (Syntax), 章节 23.1 (Reader Concepts)

* 注意(Notes): None. 


### <span id="F-READ-DELIMITED-LIST">函数 READ-DELIMITED-LIST</span>

* 语法(Syntax):

        read-delimited-list char &optional input-stream recursive-p => list

* 参数和值(Arguments and Values):

        char---一个字符.
        input-stream---一个输入流标识符. 默认是标准输入.
        recursive-p---一个广义 boolean. 默认是 false.
        list---一个读取到的对象列表.

* 描述(Description):

        read-delimited-list 从 input-stream 中读取对象直到在一个对象表示后面的字符下一个字符(忽略空格字符和注释)是 char.

        read-delimited-list 向前查看每一步, 以查找下一个非空格字符, 并以 peek-char 的方式观察它. 如果它是 char, 那么这个字符被消耗并且返回对象列表. 如果它是一个 constituent 或转义字符, 那么 read 被用于读取一个对象, 它会被添加到这个对象的末尾. 如果它是一个宏字符, 那么就会调用它的读取器宏函数; 如果这个函数返回一个值, 那么这个值会被添加给列表. 然后重复窥视的过程.

        如果 recursive-p 是 true, 这个调用预计被嵌入在一个更高层的对 read 或一个相似函数的调用中.

        在 read-delimited-list 操作期间到达文件末尾是一个错误.

        如果 char 有一个当前读取表中的空格的语法类型, 那么后果是未定义的.

* 示例(Examples):

    ```LISP
    (read-delimited-list #\]) 1 2 3 4 5 6 ]
    =>  (1 2 3 4 5 6)
    ```

        假设你想 #{a b c ... z} 被读取未元素 a, b, c, ..., z 的所有序对的列表, 例如.

            #{p q z a}  读取为  ((p q) (p z) (p a) (q z) (q a) (z a))

        这个可以通过为 #{ 指定一个宏字符定义来完成, 这个宏字符定义完成两件事: 读入直到 } 的所有项, 并且构造这些序对. read-delimited-list 执行第一个任务.

    ```LISP
    (defun |#{-reader| (stream char arg)
      (declare (ignore char arg))
      (mapcon #'(lambda (x)
                  (mapcar #'(lambda (y) (list (car x) y)) (cdr x)))
              (read-delimited-list #\} stream t))) =>  |#{-reader|

    (set-dispatch-macro-character #\# #\{ #'|#{-reader|) =>  T 
    (set-macro-character #\} (get-macro-character #\) nil))
    ```

        注意提供给 recursive-p 参数的 true.

        这里有必要对字符 } 给出一个定义, 并防止其成为一个 constituent. 如果上面显示的这行

    ```LISP
    (set-macro-character #\} (get-macro-character #\) nil))
    ```

        没有包括在内, 那么在

    ```LISP
    #{ p q z a}
    ```

        中的 } 会被认为是一个 constituent 字符, 成为名为 a} 的符号的一部分. 这个可以通过在 } 之前放置一个空格来纠正, 但是去调用 set-macro-character 更好.

        给 } 和字符 ) 的标准定义相同的定义有着双重好处: 使它和 read-delimited-list 一起使用时终止标记并且也使它在其他上下文中使用是无效的. 尝试去读取一个偏离的 } 会发出一个错误.

* 受此影响(Affected By):

        *standard-input*, *readtable*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        read, peek-char, read-char, unread-char.

* 注意(Notes):

        read-delimited-list 意图在于被用来实现读取器宏. 通常, char 应该是一个终止的宏字符, 以便可以使用它来分隔记号; 但是, read-delimited-list 不会尝试去修改有当前读取器表指定的 char 的语法. 调用者必须对读取表语法做出显式的必要改变. 


### <span id="F-READ-FROM-STRING">函数 READ-FROM-STRING</span>

* 语法(Syntax):

        read-from-string string &optional eof-error-p eof-value &key start end preserve-whitespace
        => object, position

* 参数和值(Arguments and Values):

        string---一个字符串.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        start, end---字符串 string 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        preserve-whitespace---一个广义 boolean. 默认是 false.
        object---一个对象 (由 Lisp 读取器解析的) 或者 eof-value
        position---一个大于等于零并且小于等于字符串 string 的长度减一的整数.

* 描述(Description):

        从字符串 string 中由 string 和 end 限定的子序列中解析一个对象的打印表示, 就好像 read 在一个包含相同的这些字符的输入流上被调用一样.

        如果 preserve-whitespace 是 true, 这个操作会保留空格, 就好像 read-preserving-whitespace 做的一样.

        如果一个对象被成功解析, 这个主要的值, object, 就是那个解析的对象. 如果 eof-error-p 是 false 并且到达了这个子字符串的末尾, 就会返回 eof-value.

        第二个值, position, 是限定的字符串 string 中第一个没有被读取的字符的索引. 这个 position 可能依赖 preserve-whitespace 的值. 如果这个完整的字符串被读取, 这个返回的 position 是这个字符串 string 的长度或者比这个字符串 string 长度大一的数.

* 示例(Examples):

    ```LISP
    (read-from-string " 1 3 5" t nil :start 2) =>  3, 5
    (read-from-string "(a b c)") =>  (A B C), 7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在一个对象可以被读取之前到达这个提供的子字符串的末尾, 如果 eof-error-p 是 true 就会发出一个错误. 如果这个子字符串的末尾出现在一个未完成的对象中间, 那么就会发出一个错误.

* 也见(See Also):

        read, read-preserving-whitespace

* 注意(Notes):

        这个 position 被允许超过这个字符串 string 的长度的原因是为了允许具体实现去通过模拟这些限定字符串 string 末尾的一个尾部分隔符的效果来工作. 当 preserve-whitespace 是 true 时, 这个 position 可能统计了模拟分隔符. 


### <span id="A-READTABLE-CASE">访问器 READTABLE-CASE</span>

* 语法(Syntax):

        readtable-case readtable => mode

        (setf (readtable-case readtable) mode)

* 参数和值(Arguments and Values):

        readtable---一个读取表.
        mode---一个大小写敏感模式.

* 描述(Description):

        访问 readtable 的读取表大小写, 它影响 Lisp 读取器读取符号的方式以及 Lisp 打印器写入符号的方式.

* 示例(Examples):

        见示例 23.1.2.1 (Examples of Lisp 读取器上的读取表大小写的影响) 以及章节 22.1.3.3.2.1 (Examples of Effect of Readtable Case on the Lisp Printer).

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 readtable 不是一个读取表, 那么应该发出一个 type-error 类型的错误. 如果 mode 不是一个大小写敏感模式, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        *readtable*, *print-escape*, 章节 2.2 (Reader Algorithm), 章节 23.1.2 (Lisp 读取器上的读取表大小写的影响), 章节 22.1.3.3.2 (Effect of Readtable Case on the Lisp Printer)

* 注意(Notes):

        copy-readtable 拷贝这个 readtable 的读取表大小写. 

### <span id="F-READTABLEP">函数 READTABLEP</span>

* 语法(Syntax):

        readtablep object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 readtable 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (readtablep *readtable*) =>  true
    (readtablep (copy-readtable)) =>  true
    (readtablep '*readtable*) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (readtablep object) ==  (typep object 'readtable) 


### <span id="F-SET-AND-GET-DMC">函数 SET-DISPATCH-MACRO-CHARACTER, GET-DISPATCH-MACRO-CHARACTER</span>

* 语法(Syntax):

        get-dispatch-macro-character disp-char sub-char &optional readtable => function

        set-dispatch-macro-character disp-char sub-char new-function &optional readtable => t

* 参数和值(Arguments and Values):

        disp-char---一个字符.
        sub-char---一个字符.
        readtable---一个读取表标识符. 默认是当前读取表.
        function---一个函数标识符或 nil.
        new-function---一个函数标识符.

* 描述(Description):

        set-dispatch-macro-character 导致 new-function 在读取到 disp-char 后面跟着 sub-char 时被调用. 如果 sub-char 是一个小写字母, 它会被转换为它的大写等价. 如果 sub-char 是一个十进制数字中的一个, 那么就是一个错误.

        set-dispatch-macro-character 安装一个 new-function, 当一个特定分派宏字符对被读取到时 new-function 会被调用. new-function 被安装为当 readtable 正在被使用并且 disp-char 后跟着 sub-char 时要被调用的分派函数.

        关于这个 new-function 被调用的更多信息, 见章节 2.1.4.4 (Macro Characters).

        get-dispatch-macro-character 检索表 readtable 中和 disp-char 以及 sub-char 关联的分派函数.

        get-dispatch-macro-character 返回这个 sub-char 从属 disp-char 的宏字符函数, 如果这里没有和 sub-char 关联的函数那么就是 nil. 如果 sub-char 是一个十进制数字, get-dispatch-macro-character 返回 nil.

* 示例(Examples):

    ```LISP
    (get-dispatch-macro-character #\# #\{) =>  NIL
    (set-dispatch-macro-character #\# #\{        ;dispatch on #{
        #'(lambda(s c n)
            (let ((list (read s nil (values) t)))  ;list is object after #n{
              (when (consp list)                   ;return nth element of list
                (unless (and n (< 0 n (length list))) (setq n 0))
                (setq list (nth n list)))
            list))) =>  T
    #{(1 2 3 4) =>  1
    #3{(0 1 2 3) =>  3
    #{123 =>  123
    ```

        如果期望 #$foo : 就像 (dollars foo) 一样.

    ```LISP
    (defun |#$-reader| (stream subchar arg)
      (declare (ignore subchar arg))
      (list 'dollars (read stream t nil t))) =>  |#$-reader|
    (set-dispatch-macro-character #\# #\$ #'|#$-reader|) =>  T
    ```

* 也见(See Also):

        章节 2.1.4.4 (Macro Characters)

* 副作用(Side Effects):

        这个读取表 readtable 会被修改.

* 受此影响(Affected By):

        *readtable*.

* 异常情况(Exceptional Situations):

        对于任何一个函数, 如果 disp-char 不是一个 readtable 中的分派宏字符, 就会发出一个错误.

* 也见(See Also):

        *readtable*

* 注意(Notes):

        有必要在指定这个分派字符的子字符之前使用 make-dispatch-macro-character 去设置这个分派字符. 


### <span id="F-SET-AND-GET-MC">函数 SET-MACRO-CHARACTER, GET-MACRO-CHARACTER</span>

* 语法(Syntax):

        get-macro-character char &optional readtable => function, non-terminating-p

        set-macro-character char new-function &optional non-terminating-p readtable => t

* 参数和值(Arguments and Values):

        char---一个字符.
        non-terminating-p---一个广义 boolean. 默认是 false.
        readtable---一个读取表标识符. 默认是当前读取表.
        function---nil, 或者一个两参数函数的标识符.
        new-function---一个函数标识符.

* 描述(Description):

        get-macro-character 返回这个在读取表 readtable 中和 char 相关联的读取器宏函数作为它的主要的值 (如果有的话), 或者如果 char 不是一个读取表 readtable 中的一个宏字符, 那么就是 nil. 第二个值, non-terminating-p, 如果 char 是一个非终止宏字符, 那么就是 true; 否则, 它就是 false.

        set-macro-character 导致 char 成为读取表 readtable 中和读取器宏函数 new-function (或者 new-function 的标识符) 相关联的宏字符. 如果 non-terminating-p 是 true, char 成为非终止宏字符; 否则它成为一个终止宏字符.

* 示例(Examples):

    ```LISP
    (get-macro-character #\{) =>  NIL, false
    (not (get-macro-character #\;)) =>  false
    ```

        以下是一个对于标准语法中的单引号读取器宏的可能的定义:

    ```LISP
    (defun single-quote-reader (stream char)
      (declare (ignore char))
      (list 'quote (read stream t nil t))) =>  SINGLE-QUOTE-READER
    (set-macro-character #\' #'single-quote-reader) =>  T
    ```

        这里 single-quote-reader 读取一个跟在单引号后面的对象并且返回一个 quote 和那个对象的列表. 这个 char 参数被忽略.

        下面是一个对于标准语法中的分号读取器宏的可能的定义:

    ```LISP
    (defun semicolon-reader (stream char)
      (declare (ignore char))
      ;; First swallow the rest of the current input line.
      ;; End-of-file is acceptable for terminating the comment.
      (do () ((char= (read-char stream nil #\Newline t) #\Newline)))
      ;; Return zero values.
      (values)) =>  SEMICOLON-READER
    (set-macro-character #\; #'semicolon-reader) =>  T
    ```

* 副作用(Side Effects):

        这个读取表 readtable 被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        *readtable*

* 注意(Notes): None. 


### <span id="F-SET-SYNTAX-FROM-CHAR">函数 SET-SYNTAX-FROM-CHAR</span>

* 语法(Syntax):

        set-syntax-from-char to-char from-char &optional to-readtable from-readtable => t

* 参数和值(Arguments and Values):

        to-char---一个字符.
        from-char---一个字符.
        to-readtable---一个读取表. 默认是当前读取表.
        from-readtable---一个读取表标识符. 默认是标准读取表.

* 描述(Description):

        set-syntax-from-char 使得 to-readtable 中的 to-char 语法和 from-readtable 中的 from-char 一样.

        set-syntax-from-char 拷贝 from-char 的语法类型. 如果 from-char 是一个宏字符, 它的读取器宏函数也被拷贝. 如果这个字符是一个分派宏字符, 它的整个读取器宏函数的分派表被拷贝. 这个 from-char 的 constituent 特质不会被拷贝.

        一个来自一个字符的宏定义可以被拷贝到另一个字符, 例如 "; 这个 " 的标准定义查找另一个和调用它的字符相同的字符. 这个 ( 的定义不能被有意义地拷贝到 {, 另一方面. 结果是那个列表为 {a b c) 形式, 而不是 {a b c}, 因为这个定义总是查找一个闭合的圆括号, 不是一个闭合的大括号.

* 示例(Examples):

    ```LISP
    (set-syntax-from-char #\7 #\;) =>  T
    123579 =>  1235
    ```
    
* 副作用(Side Effects):

        这个 to-readtable 被修改.

* 受此影响(Affected By):

        在 from-readtable 中已存在的值.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        set-macro-character, make-dispatch-macro-character, 章节 2.1.4 (Character Syntax Types)

* 注意(Notes):

        一个字符的 constituent 特质被 "硬连接(hard wired)" 到扩展标记的解析器中. 例如, 如果 S 的定义被拷贝到 *, 那么 * 会成为一个字母 constituent 但是不能被用作一个短浮点指数标记. 关于进一步信息, 见章节 2.1.4.2 (Constituent Traits). 


### <span id="M-WITH-STANDARD-IO-SYNTAX">宏 WITH-STANDARD-IO-SYNTAX</span>

* 语法(Syntax):

        with-standard-io-syntax form* => result*

* 参数和值(Arguments and Values):

        forms---一个隐式 progn.
        results---由表达式形式 forms 返回的值.

* 描述(Description):

        在这个 forms 主体的动态范围内, 所有读取器/打印器控制变量, 包括任何这个标准没有指定但是具体实现定义的, 被绑定为产生标准读取/打印行为的值. 这个标准指定的变量的值列在下一段中.

        变量                          值                               
        *package*                    The CL-USER package                 
        *print-array*                t                                   
        *print-base*                 10                                  
        *print-case*                 :upcase                             
        *print-circle*               nil                                 
        *print-escape*               t                                   
        *print-gensym*               t                                   
        *print-length*               nil                                 
        *print-level*                nil                                 
        *print-lines*                nil                                 
        *print-miser-width*          nil                                 
        *print-pprint-dispatch*      The standard pprint dispatch table  
        *print-pretty*               nil                                 
        *print-radix*                nil                                 
        *print-readably*             t                                   
        *print-right-margin*         nil                                 
        *read-base*                  10                                  
        *read-default-float-format*  single-float                        
        *read-eval*                  t                                   
        *read-suppress*              nil                                 
        *readtable*                  The standard readtable              

        Figure 23-1. 标准控制变量的值

* 示例(Examples):

    ```LISP
    (with-open-file (file pathname :direction :output)
      (with-standard-io-syntax
        (print data file)))

    ;;; ... Later, in another Lisp:

    (with-open-file (file pathname :direction :input)
      (with-standard-io-syntax
        (setq data (read file))))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="M-WITH-STANDARD-IO-SYNTAX">变量 *READ-BASE*</span>

* 值类型(Value Type):

        一个基数.

* 初始值(Initial Value):

        10.

* 描述(Description):

        控制被读取为整数或比数的标记的解释.

        这个 *read-base* 的值, 称为当前的输入基数, 是整数和比数要被 Lisp 读取器读取的基数. 其他数值类型(例如, 浮点数)的解析不被这个选项所影响.

        *read-base* 在任何特定的有理数的读取上的效果可以通过显式使用 #O, #X, #B, 或 #nR 语法或通过一个尾部的小数点覆盖.

* 示例(Examples):

    ```LISP
    (dotimes (i 6)
      (let ((*read-base* (+ 10. i)))
        (let ((object (read-from-string "(\\DAD DAD |BEE| BEE 123. 123)")))
          (print (list *read-base* object)))))
    >>  (10 (DAD DAD BEE BEE 123 123))
    >>  (11 (DAD DAD BEE BEE 123 146))
    >>  (12 (DAD DAD BEE BEE 123 171))
    >>  (13 (DAD DAD BEE BEE 123 198))
    >>  (14 (DAD 2701 BEE BEE 123 227))
    >>  (15 (DAD 3088 BEE 2699 123 258))
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 也见(See Also): None.

* 注意(Notes):

        在读取特殊格式的数据文件时, 修改输入基数是很有用的.

### <span id="V-READ-DEFAULT-FLOAT-FORMAT">变量 *READ-DEFAULT-FLOAT-FORMAT*</span>

* 值类型(Value Type):

        原子类型指定符 short-float, single-float, double-float, 或 long-float 其中之一, 或者某个具体实现定义的其他类型指定符也是可接受的.

* 初始值(Initial Value):

       符号 single-float.

* 描述(Description):

        控制当读取一个没有指数标记或者有着 e 或 E 的指数标记的浮点数时要被使用的浮点格式. 其他指数标记显式规定了要被使用的浮点数格式.

        当打印一个浮点数时, 打印器使用 *read-default-float-format* 来引导指数标记的选择.

* 示例(Examples):

    ```LISP
    (let ((*read-default-float-format* 'double-float))
      (read-from-string "(1.0 1.0e0 1.0s0 1.0f0 1.0d0 1.0L0)"))
    =>  (1.0   1.0   1.0   1.0 1.0   1.0)   ;Implementation has float format F.
    =>  (1.0   1.0   1.0s0 1.0 1.0   1.0)   ;Implementation has float formats S and F.
    =>  (1.0d0 1.0d0 1.0   1.0 1.0d0 1.0d0) ;Implementation has float formats F and D.
    =>  (1.0d0 1.0d0 1.0s0 1.0 1.0d0 1.0d0) ;Implementation has float formats S, F, D.
    =>  (1.0d0 1.0d0 1.0   1.0 1.0d0 1.0L0) ;Implementation has float formats F, D, L.
    =>  (1.0d0 1.0d0 1.0s0 1.0 1.0d0 1.0L0) ;Implementation has formats S, F, D, L.
    ```

* 受此影响(Affected By): None.

* 也见(See Also): None.

* 注意(Notes): None.

### <span id="V-READ-EVAL">变量 *READ-EVAL*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        true.

* 描述(Description):

        如果它是 true, 那么这个 #. 读取器后果有着它的正常效果. 否则, 这个读取器宏发出一个 reader-error 类型的错误.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 也见(See Also):

        *print-readably*

* 注意(Notes):

        如果 *read-eval* 是 false 并且 *print-readably* 是 true, 那么会输出一个对 #. 读取器宏的引用的 print-object 的任何方法会输出一个不同的东西或者发出一个 print-not-readable 类型的错误.

### <span id="V-READ-SUPPRESS">变量 *READ-SUPPRESS*</span>

* 值类型(Value Type):

        一个广义 boolean.

* 初始值(Initial Value):

        false.

* 描述(Description):

        这个变量主要用于支持读取时的条件标记 #+ 和 #- 的操作. 重要的是读取器宏实现这些符号能够跳过一个表达式的打印表示尽管可能跳过的语法表达可能不是完全有效的当前实现, 因为 #+ 和 #- 的存在为了允许同一程序在多个 Lisp 语言实现 (包括除了 Common Lisp 以外的方言) 之间共享, 尽管有小的不兼容的语法.

        如果它是 false, 那么这个 Lisp 读取器正常操作.

        如果这个 *read-suppress* 的值是 true, 那么 read, read-preserving-whitespace, read-delimited-list, 和 read-from-string 都在它们成功完成时返回一个 nil 的主要值; 但是, 它们继续用正常的方式去解析一个对象的打印表示, 来跳过这个对象, 并且继续用正常方式去表示文件的末尾. 除了下面记录的, 任何定义去读取一个后面对象或标记的标准读取器宏都会这样做, 但是如果这个读取到的对象不是一个合适的类型或语法, 那么不会发出一个错误. 这个标准语法和它关联的读取器宏不会构造任何新对象 (比如, 当读取一个符号的打印表示时, 没有符号会被构造或捕捉).

        扩展标记

            所有扩展标记都完全不解释. 由于检测到无效的潜在数字, 包标记的无效模式和点字符的无效使用, 这些错误可能会被抑制.

        分派宏字符 (包括井号)

            分派宏字符继续去解析一个中缀数值参数, 并且调用分派函数. 标准的井号读取器宏不会在这个数值参数的出现和值上实施任何约束.

        #=

            这个 #= 标记整个忽略. 它不会读取一个后面的对象. 它不产生对象, 但是会被像空格一样对待.

        ##

            这个 ## 标记总是产生 nil.

        不管 *read-suppress* 的值是什么, 圆括号仍然继续去分隔和构造列表; 这个 #( 标记继续去分隔向量; 并且注释, 字符串, 以及单引号和反引号继续去被正常解释. 像 '), #<, #), 和 #<Space> 这样的情况继续去发出错误.

* 示例(Examples):

    ```LISP
    (let ((*read-suppress* t))
      (mapcar #'read-from-string
              '("#(foo bar baz)" "#P(:type :lisp)" "#c1.2"
                "#.(PRINT 'FOO)" "#3AHELLO" "#S(INTEGER)"
                "#*ABC" "#\GARBAGE" "#RALPHA" "#3R444")))
    =>  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    ```

* 受此影响(Affected By): None.

* 也见(See Also):

        read, 章节 2 (Syntax)

* 注意(Notes):

        强烈鼓励定义额外宏字符的程序员和具体实现去使它们遵守 *read-suppress*, 就像标准化宏字符那样. 这也就是说, 当 *read-suppress* 的值是 true 时, 它们应该在读取一个后续的对象时忽略类型错误, 并且实现分派宏字符的函数应该接受 nil 作为它们的中缀参数值, 即便通常要求为一个数值.

### <span id="V-READTABLE">变量 *READTABLE*</span>

* 值类型(Value Type):

        一个读取表.

* 初始值(Initial Value):

        一个符合章节 2 (Syntax) Common Lisp 语法描述的读取表.

* 描述(Description):

        *readtable* 的值被称为当前的读取表. 它控制 Lisp 读取器的解析行为, 并且也可以影响 Lisp 打印器 (比如, 见函数 readtable-case).

* 示例(Examples):

    ```LISP
    (readtablep *readtable*) =>  true
    (setq zvar 123) =>  123
    (set-syntax-from-char #\z #\' (setq table2 (copy-readtable))) =>  T
    zvar =>  123
    (setq *readtable* table2) =>  #<READTABLE>
    zvar =>  VAR
    (setq *readtable* (copy-readtable nil)) =>  #<READTABLE>
    zvar =>  123
    ```

* 受此影响(Affected By):

        compile-file, load

* 也见(See Also):

        compile-file, load, readtable, 章节 2.1.1.1 (The Current Readtable)

* 注意(Notes): None. 


### <span id="CT-READER-ERROR">状况类型 READER-ERROR</span>

* 类优先级列表(Class Precedence List):

        reader-error, parse-error, stream-error, error, serious-condition, condition, t

* 描述(Description):

        类型 reader-error 由 Lisp 读取器执行的标记化和解析的错误状况组成.

* 也见(See Also):

        read, stream-error-stream, 章节 23.1 (Reader Concepts) 

