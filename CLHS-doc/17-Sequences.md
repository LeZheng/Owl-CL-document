# 17 Sequences

> * 17.1 [序列的概念](#SequenceConcepts)
> * 17.2 [关于测试函数的规则](#RulesTestFunctions)
> * 17.3 [序列的字典](#TheSequencesDictionary)


## 17.1 <span id="SequenceConcepts">序列的概念</span>

一个序列是多个元素的一个有序集合, 实现为一个向量或一个列表.

序列可以由函数 make-sequence 创建, 其他创建 sequence 的子类型的对象的函数也可以 (比如, list, make-list, mapcar, 和 vector).

一个序列函数是一个由这个规范定义的或者由具体实现作为一个扩展添加的一个在一个或多个序列上操作的函数. 在一个序列函数必须构造并返回一个新的向量时, 它总是返回一个简单向量. 类似地, 任何构造的字符串都会是简单字符串.

    concatenate        length              remove             
    copy-seq           map                 remove-duplicates  
    count              map-into            remove-if          
    count-if           merge               remove-if-not      
    count-if-not       mismatch            replace            
    delete             notany              reverse            
    delete-duplicates  notevery            search             
    delete-if          nreverse            some               
    delete-if-not      nsubstitute         sort               
    elt                nsubstitute-if      stable-sort        
    every              nsubstitute-if-not  subseq             
    fill               position            substitute         
    find               position-if         substitute-if      
    find-if            position-if-not     substitute-if-not  
    find-if-not        reduce                                 

    Figure 17-1. 标准序列函数

### 17.1.1 在必须为序列的参数上的普遍约束

通常, 被当作序列的列表 (包括关联列表和属性列表) 必须是 proper 列表. 


## 17.2 <span id="RulesTestFunctions">关于测试函数的规则</span>

> * 17.2.1 [满足一个两个参数的测试](#SatisfyingTwoArgumentTest)
> * 17.2.2 [满足一个单参数的测试](#SatisfyingOneArgumentTest)

### 17.2.1 <span id="SatisfyingTwoArgumentTest">满足一个两个参数的测试</span>

当一个对象 O 要被迭代地和一个序列 S 的每一个元素 Ei 通过下面这段列出的操作符 F 考虑时, 有时候, 控制 O 在 S 中由 F 测试的方式是很有用的. 这个控制在一个被 :test 或 :test-not 参数标识的函数的基础上.

    adjoin           nset-exclusive-or  search            
    assoc            nsublis            set-difference    
    count            nsubst             set-exclusive-or  
    delete           nsubstitute        sublis            
    find             nunion             subsetp           
    intersection     position           subst             
    member           pushnew            substitute        
    mismatch         rassoc             tree-equal        
    nintersection    remove             union             
    nset-difference  remove-duplicates                    

    Figure 17-2. 有两个参数的测试需要满足的操作符

这个对象 O 可能不会直接和 Ei 比较. 如果提供了一个 :key 参数, 它就是要被调用的单参数的函数的标识符, 而每一个 Ei 作为一个参数, 并且产生一个对象 Zi 被用作这个比较. (如果这里没有 :key 参数, Zi 就是 Ei.)

这个由 :key 参数标识的函数从不在 O 自身上调用. 然而, 如果这个函数在多个序列上操作 (比如, 就像发生在 set-difference), O 会是在另一个序列上调用这个函数的结果.

如果提供给 F 的一个 :test 参数, 那么它就是一个两参数函数的标识符, 参数为 O 和 Zi. 如果这个 :test 函数返回表示 true 的广义 boolean 那么就说一个 Ei (或者, 有时候, 就说一个 O 和一个 Ei) 满足这个测试条件.

如果提供 F 一个 :test-not 参数, 那么它就是一个两个参数函数的标识符, 参数是 O 和 Zi. 如果这个 :test-not 函数返回一个表示 false 的广义 boolean, 那么就说 Ei (或者, 有时候, 就说一个 O 和一个 Ei) 满足这个测试条件.

如果 :test 和 :test-not 参数都没有提供, 那么就好像提供了一个 #'eql 的 :test 参数.

如果在对 F 的同一个调用中 :test 和 :test-not 参数都提供了, 那么后果是未指定的.

#### 17.2.1.1 满足一个两个参数的测试的示例

```LISP
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equal)
=>  (foo bar "BAR" "foo" "bar")
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equalp)
=>  (foo bar "BAR" "bar")
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string-equal)
=>  (bar "BAR" "bar")
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string=)
=>  (BAR "BAR" "foo" "bar")

 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'eql)
=>  (1)
 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'=)
=>  (1 1.0 #C(1.0 0.0))
 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test (complement #'=))
=>  (1 1.0 #C(1.0 0.0))

 (count 1 '((one 1) (uno 1) (two 2) (dos 2)) :key #'cadr) =>  2

 (count 2.0 '(1 2 3) :test #'eql :key #'float) =>  1

 (count "FOO" (list (make-pathname :name "FOO" :type "X")  
                    (make-pathname :name "FOO" :type "Y"))
        :key #'pathname-name
        :test #'equal)
=>  2
```

### 17.2.2 <span id="SatisfyingOneArgumentTest">满足一个单参数的测试</span>

当使用下面这段中的函数的其中一个时, 序列 S 的元素 E 被过滤不是根据两个参数的断言下的对象 O 的存在或缺失的基础上, 如章节 17.2.1 (Satisfying a Two-Argument Test) 中描述的函数, 而是在单参数断言的基础上.

    assoc-if       member-if           rassoc-if          
    assoc-if-not   member-if-not       rassoc-if-not      
    count-if       nsubst-if           remove-if          
    count-if-not   nsubst-if-not       remove-if-not      
    delete-if      nsubstitute-if      subst-if           
    delete-if-not  nsubstitute-if-not  subst-if-not       
    find-if        position-if         substitute-if      
    find-if-not    position-if-not     substitute-if-not  

    Figure 17-3. 有单参数的测试需要满足的操作符

元素 Ei 可能不会被直接考虑. 如果提供了一个 :key 参数, 那么它就是一个单参数函数的标识符, 用每一个 Ei 作为参数来调用, 并且产生一个要被用来比较的对象 Zi. (如果没有 :key 参数, Zi 就是 Ei.)

在这个规范中定义的并且有着一个以 "-if" 结尾的名字的函数接受一个单参数 Zi 的函数的标识符作为第一个参数. 如果这个 :test 函数返回一个表示  true 的广义 boolean, 那么就说一个 Ei 满足这个测试.

在这个规范中定义的并且有着一个以 "-if-not" 结尾的名字的函数接受一个单参数 Zi 的函数的标识符作为第一个参数. 如果这个 :test 函数返回一个表示 false 的广义 boolean, 那么就说一个 Ei 满足这个测试条件.


#### 17.2.2.1 Examples of Satisfying a One-Argument Test

```LISP
 (count-if #'zerop '(1 #C(0.0 0.0) 0 0.0d0 0.0s0 3)) =>  4

 (remove-if-not #'symbolp '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
=>  (A B C D E F)
 (remove-if (complement #'symbolp) '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
=>  (A B C D E F)

 (count-if #'zerop '("foo" "" "bar" "" "" "baz" "quux") :key #'length)
=>  3
```

## 17.3 <span id="TheSequencesDictionary">序列的字典</span>

> * [系统类 SEQUENCE](#SC-SEQUENCE)
> * [函数 COPY-SEQ](#F-COPY-SEQ)
> * [访问器 ELT](#A-ELT)
> * [函数 FILL](#F-FILL)
> * [函数 MAKE-SEQUENCE](#F-MAKE-SEQUENCE)
> * [访问器 SUBSEQ](#A-SUBSEQ)
> * [函数 MAP](#F-MAP)
> * [函数 MAP-INTO](#F-MAP-INTO)
> * [函数 REDUCE](#F-REDUCE)
> * [函数 COUNT, COUNT-IF, COUNT-IF-NOT](#F-COUNT-ALL)
> * [函数 LENGTH](#F-LENGTH)
> * [函数 REVERSE, NREVERSE](#F-REVERSE-ALL)
> * [函数 SORT, STABLE-SORT](#F-SORT-ALL)
> * [函数 FIND, FIND-IF, FIND-IF-NOT](#F-FIND-ALL)
> * [函数 POSITION, POSITION-IF, POSITION-IF-NOT](#F-POSITION-ALL)
> * [函数 SEARCH](#F-SEARCH)
> * [函数 MISMATCH](#F-MISMATCH)
> * [函数 REPLACE](#F-REPLACE)
> * [函数 SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT, NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT](#F-SUBSTITUTE-ALL)
> * [函数 CONCATENATE](#F-CONCATENATE)
> * [函数 MERGE](#F-MERGE)
> * [函数 REMOVE, REMOVE-IF, REMOVE-IF-NOT, DELETE, DELETE-IF, DELETE-IF-NOT](#F-REMOVE-ALL)
> * [函数 REMOVE-DUPLICATES, DELETE-DUPLICATES](#F-DUPLICATES-ALL)


### <span id="SC-SEQUENCE">系统类 SEQUENCE</span>

* 类优先级列表(Class Precedence List):

        sequence, t

* 描述(Description):

        序列是对象的有序集合, 这些对象称为序列的元素.

        类型 vector 类型 list 是类型 sequence 的互斥的子类型, 但没有必要是序列的一个详尽分区.

        当把一个向量视作一个序列时, 只有这个向量的有效元素被当作这个序列的元素; 这也就是说, 当给定的序列被表示为向量时, 序列操作遵守填充指针. 


### <span id="F-COPY-SEQ">函数 COPY-SEQ</span>

* 语法(Syntax):

        copy-seq sequence => copied-sequence

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        copied-sequence---一个 proper 序列.

* 描述(Description):

        创建一个序列 sequence 的拷贝. 这个新的序列中的元素和给定序列 sequence 中的对应元素是相同的.

        如果序列 sequence 是一个向量, 那么结果就是维数为一的新的简单数组, 并且有着和序列 sequence 相同的实际数组元素类型. 如果序列 sequence 是一个列表, 那么结果就是一个新的列表.

* 示例(Examples):

    ```LISP
    (setq str "a string") =>  "a string"
    (equalp str (copy-seq str)) =>  true
    (eql str (copy-seq str)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备发出一个 type-error 类型的错误.

* 也见(See Also):

        copy-list

* 注意(Notes):

        从功能的角度来看,

        (copy-seq x) ==  (subseq x 0)

        然而, 在这两种情况下, 程序员的意图通常是非常不同的. 


### <span id="A-ELT">访问器 ELT</span>

* 语法(Syntax):

        elt sequence index => object

        (setf (elt sequence index) new-object)

* 参数和值(Arguments and Values):

        sequence---一个proper 序列.
        index---对于序列 sequence 的一个有效序列索引.
        object---一个对象.
        new-object---一个对象.

* 描述(Description):

        访问由索引 index 指定的序列 sequence 中的元素.

* 示例(Examples):

    ```LISP
    (setq str (copy-seq "0123456789")) =>  "0123456789"
    (elt str 6) =>  #\6
    (setf (elt str 0) #\#) =>  #\#
    str =>  "#123456789"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 sequence 不是一个 proper 序列, 那么应该准备发出一个 type-error 类型的错误. 如果 index 对于序列 sequence 的不是一个有效序列索引, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        aref, nth, 章节 3.2.1 (Compiler Terminology)

* 注意(Notes):

        aref 可能被用于访问超出这个向量填充指针的向量元素. 

### <span id="F-FILL">函数 FILL</span>

* 语法(Syntax):

        fill sequence item &key start end => sequence

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        item---一个序列.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别是 0 和 nil.

* 描述(Description):

        用 item 替换由 start and end 限制的序列 sequence 中的元素.

* 示例(Examples):

    ```LISP
    (fill (list 0 1 2 3 4 5) '(444)) =>  ((444) (444) (444) (444) (444) (444))
    (fill (copy-seq "01234") #\e :start 3) =>  "012ee"
    (setq x (vector 'a 'b 'c 'd 'e)) =>  #(A B C D E)
    (fill x 'z :start 1 :end 3) =>  #(A Z Z D E)
    x =>  #(A Z Z D E)
    (fill x 'p) =>  #(P P P P P)
    x =>  #(P P P P P)
    ```

* 副作用(Side Effects):

        序列 sequence 被破坏性地修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误. 如果 start 不是一个非负整数, 那么应该发出一个 type-error 类型的错误. 如果 end 不是一个非负整数或者 nil, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        replace, nsubstitute

* 注意(Notes):

        (fill sequence item) == (nsubstitute-if item (constantly t) sequence) 


### <span id="F-MAKE-SEQUENCE">函数 MAKE-SEQUENCE</span>

* 语法(Syntax):

        make-sequence result-type size &key initial-element => sequence

* 参数和值(Arguments and Values):

        result-type---一个序列类型指定符.
        size---一个非负整数.
        initial-element---一个对象. 默认值是依赖于具体实现的.
        sequence---一个 proper 序列.

* 描述(Description):

        返回一个 result-type 类型并且长度为 size 的序列, 其中的每一个元素都被初始化为 initial-element.

        如果 result-type 是 list 的一个子类型, 那么结果会是一个列表 list.

        如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

    ```LISP
    (make-sequence 'list 0) =>  ()
    (make-sequence 'string 26 :initial-element #\.) 
    =>  ".........................."
    (make-sequence '(vector double-float) 2
                    :initial-element 1d0)
    =>  #(1.0d0 1.0d0)

    (make-sequence '(vector * 2) 3) should signal an error
    (make-sequence '(vector * 4) 3) should signal an error
    ```

* 受此影响(Affected By):

        这个具体实现.

* 异常情况(Exceptional Situations):

        如果 initial-element 不是一个可以被存储在产生的序列中的对象, 那么后果是未指定的.

        如果 result-type 既不是一个 list 的可识别子类型, 也不是一个 vector 的可识别子类型, 那么就会发出一个 type-error 类型的错误.

        如果 result-type 指定的元素的数量并且 size 和那个数量不同, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        make-array, make-list

* 注意(Notes):

        (make-sequence 'string 5) ==  (make-string 5)               

### <span id="A-SUBSEQ">Accessor SUBSEQ</span>

* 语法(Syntax):

        subseq sequence start &optional end => subsequence

        (setf (subseq sequence start &optional end) new-subsequence)

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        start, end---序列 sequence 边界索引标识符. 对于 end 默认是 nil.
        subsequence---一个 proper 序列.
        new-subsequence---一个 proper 序列.

* 描述(Description):

        subseq 创建一个序列, 它是由 start 和 end 限定的 sequence 的子序列的一个拷贝.

        start 指定了原始序列 sequence 中的一个偏移位并且标记这个子序列的起始位置. end 标记这个子序列的最后一个元素的后面的位置.

        subseq 总是为一个结果分配一个新的序列; 它从不与旧的序列共享存储. 这个产生的子序列总是和 sequence 相同类型.

        如果序列 sequence 是一个向量, 那么结果是一个新的一维的有着和 sequence 相同实际数组元素类型的简单数组. 如果序列 sequence 是一个列表, 那么结果是一个新的列表.

        setf 可以和 subseq 一起使用来破坏性地用一个新值的序列中的元素替换一个子序列的元素. 如果这个子序列和那个新的序列不是相同长度, 更短长度的那个确定了要被替换的元素的数量. 在较长序列中末尾的剩余元素在这个操作中不会被修改.

* 示例(Examples):

    ```LISP
    (setq str "012345") =>  "012345"
    (subseq str 2) =>  "2345"
    (subseq str 3 5) =>  "34"
    (setf (subseq str 4) "abc") =>  "abc"
    str =>  "0123ab"
    (setf (subseq str 0 2) "A") =>  "A"
    str =>  "A123ab"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误. 如果序列 new-subsequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        replace

* 注意(Notes): None. 


### <span id="F-MAP">函数 MAP</span>

* 语法(Syntax):

        map result-type function &rest sequences+ => result

* 参数和值(Arguments and Values):

        result-type -- 一个 sequence 类型指定符, 或者 nil.
        function---一个函数标识符. function 必须接受和 sequences 数量相同的参数.
        sequence---一个 proper 序列.
        result---如果 result-type 是一个类型指定符而不是 nil, 那么就是那个类型表示的一个序列; 否则 (如果 result-type 是 nil), 就是 nil.

* 描述(Description):

        应用函数 function 到参数的连续集合上, 这里的每一个参数从每个序列 sequence 中获取. 这个函数 function 首先在所有索引为 0 的元素上调用, 然后在所有索引为 1 的元素上, 以此类推. 这个 result-type 指定了产生的序列的类型.

        如果 result-type 是 nil, 那么 map 返回 nil. 否则, map 返回一个序列, 其中第 j 个元素是应用函数到每个序列的第 j 个元素的结果. 结果序列和这些序列 sequences 中最短的一个一样长. 如果应用函数到那些序列的连续元素的结果不能包含在一个给定的 result-type 类型的序列中, 那么后果是未指定的.

        如果这个 result-type 是 list 的一个子类型, 这个结果就是一个列表.

        如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

    ```LISP
    (map 'string #'(lambda (x y)
                      (char "01234567890ABCDEF" (mod (+ x y) 16)))
          '(1 2 3 4)
          '(10 9 8 7)) =>  "AAAA"
    (setq seq '("lower" "UPPER" "" "123")) =>  ("lower" "UPPER" "" "123")
    (map nil #'nstring-upcase seq) =>  NIL
    seq =>  ("LOWER" "UPPER" "" "123")
    (map 'list #'- '(1 2 3 4)) =>  (-1 -2 -3 -4)
    (map 'string
          #'(lambda (x) (if (oddp x) #\1 #\0))
          '(1 2 3 4)) =>  "1010"

    (map '(vector * 4) #'cons "abc" "de") should signal an error
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 result-type 不是 list 的一个可识别的子类型, 不是一个 vector 的可识别子类型, 并且不是 nil, 那么就会发出一个 type-error 的错误.

        如果任何一个 sequence 不是一个 proper 序列, 那么应该准备发出一个 type-error 类型的错误.

        如果 result-type 指定了元素的数量而这些序列的最小长度和这个数量不同, 那么就会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-MAP-INTO">函数 MAP-INTO</span>

* 语法(Syntax):

        map-into result-sequence function &rest sequences => result-sequence

* 参数和值(Arguments and Values):

        result-sequence---一个 proper 序列.
        function---一个和序列 sequences 的数量相同参数的函数标识符.
        sequence---一个 proper 序列.

* 描述(Description):

        破坏性地修改 result-sequence 来包含依次应用函数 function 到参数序列 sequences 的结果.

        result-sequence 和序列 sequences 的每个元素可以是一个列表或者一个向量. 如果 result-sequence 和 sequences 的每一个元素都不是相同长度, 当最短的序列(这些 sequences 中的任何一个或者 result-sequence)耗尽时迭代终止. 如果 result-sequence 是一个带有填充指针的向量, 在决定要执行多少迭代时, 这个填充指针会被忽略, 然后这个填充指针会被设置到函数 function 被应用的次数数量上. 如果 result-sequence 比这些序列 sequences 中最短的一个长, 在 result-sequence 末尾的额外元素保持不变. 如果 result-sequence 是 nil, map-into 立即返回 nil, 因为 nil 是长度为 0 的序列.

        如果函数 function 有着副作用, 它可以首先在所有索引为 0 的元素上调用, 然后在所有索引为 1 上, 以此类推.

* 示例(Examples):

    ```LISP
    (setq a (list 1 2 3 4) b (list 10 10 10 10)) =>  (10 10 10 10)
    (map-into a #'+ a b) =>  (11 12 13 14)
    a =>  (11 12 13 14)
    b =>  (10 10 10 10)
    (setq k '(one two three)) =>  (ONE TWO THREE)
    (map-into a #'cons k a) =>  ((ONE . 11) (TWO . 12) (THREE . 13) 14)
    (map-into a #'gensym) =>  (#:G9090 #:G9091 #:G9092 #:G9093)
    a =>  (#:G9090 #:G9091 #:G9092 #:G9093)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 result-sequence 不是一个 proper, 那么应该发出一个 type-error 类型的错误. 如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        map-into 和 map 的区别在于它修改一个已存在的序列而不是创建一个新的. 另外, map-into 只能用两个参数来调用, 而 map 需要至少三个参数.

        map-into 可以被定义为:

        (defun map-into (result-sequence function &rest sequences)
          (loop for index below (apply #'min 
                                        (length result-sequence)
                                        (mapcar #'length sequences))
                do (setf (elt result-sequence index)
                          (apply function
                                (mapcar #'(lambda (seq) (elt seq index))
                                        sequences))))
          result-sequence)


### <span id="F-REDUCE">函数 REDUCE</span>

* 语法(Syntax):

        reduce function sequence &key key from-end start end initial-value => result

* 参数和值(Arguments and Values):

        function---一个可以用零个或两个参数来调用的函数的标识符.
        sequence---一个 proper 序列.
        key---一个单参数函数的标识符, 或者 nil.
        from-end---一个广义 boolean. 默认是 false.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 或 nil.
        initial-value---一个对象.
        result---一个对象.

* 描述(Description):

        reduce 使用一个二元操作符, function, 来组合由 start 和 end 限制的 sequence 的元素.

        这个函数 function 必须接受序列 sequence 的两个元素作为参数或者 must accept as arguments two elements of sequence or the results from combining those elements. 这个函数 function 必须也可以接受没有参数的情况.<!--TODO 待翻译-->

        如果提供了 key, 它被用于去提取给 reduce 的值. 这个 key 函数以那个归约的顺序暗示的顺序只应用一次到序列 sequence 的每个元素上, 除了 initial-value 的值意外, 如果提供的话. 这个 key 函数通常返回序列 sequence 的元素的一部分. 如果 key 没有被提供或者是 nil, 那么就使用这个序列的元素自身.

        这个归约时左结合的(left-associative), 除非 from-end 是 true 时, 在这个情况下是右结合的(right-associative).

        如果提供了 initial-value, 它会在这个子序列之前 (如果 from-end 是 true 就是在它之后) 被逻辑上替换并且包含在这个归约操作中.

        在正常的情况中, 这个 reduce 的结果是函数 function 被应用到序列 sequence 的连续元素对的组合结果. 如果这个子序列只包含了一个元素并且没有给定 initial-value, 那么返回那个元素而函数 function 不会被调用. 如果这个子序列时空的并且给定了一个 initial-value, 那么返回这个 initial-value 而函数 function 不会被调用. 如果这个子序列是空并且没有给定 initial-value, 那么这个函数 function 会用零个参数被调用, 然后 reduce 返回函数 function 的结果. 这是仅有的函数 function 用两个参数以外被调用的情况.

* 示例(Examples):

    ```LISP
    (reduce #'* '(1 2 3 4 5)) =>  120
    (reduce #'append '((1) (2)) :initial-value '(i n i t)) =>  (I N I T 1 2)
    (reduce #'append '((1) (2)) :from-end t                  
                                :initial-value '(i n i t)) =>  (1 2 I N I T) 
    (reduce #'- '(1 2 3 4)) ==  (- (- (- 1 2) 3) 4) =>  -8
    (reduce #'- '(1 2 3 4) :from-end t)    ;Alternating sum.
    ==  (- 1 (- 2 (- 3 4))) =>  -2
    (reduce #'+ '()) =>  0
    (reduce #'+ '(3)) =>  3
    (reduce #'+ '(foo)) =>  FOO
    (reduce #'list '(1 2 3 4)) =>  (((1 2) 3) 4)
    (reduce #'list '(1 2 3 4) :from-end t) =>  (1 (2 (3 4)))
    (reduce #'list '(1 2 3 4) :initial-value 'foo) =>  ((((foo 1) 2) 3) 4)
    (reduce #'list '(1 2 3 4)
            :from-end t :initial-value 'foo) =>  (1 (2 (3 (4 foo))))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-COUNT-ALL">函数 COUNT, COUNT-IF, COUNT-IF-NOT</span>

* 语法(Syntax):

        count item sequence &key from-end start end key test test-not => n

        count-if predicate sequence &key from-end start end key => n

        count-if-not predicate sequence &key from-end start end key => n

* 参数和值(Arguments and Values):

        item---一个对象.
        sequence---一个 proper 序列.
        predicate---一个返回一个广义 boolean 的单参数函数的标识符.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回一个广义 boolean 的两参数函数的标识符.
        test-not---一个返回一个广义 boolean 的两参数函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        key---一个单参数函数的标识符, 或 nil.
        n---一个小于等于序列 sequence 的长度的非负整数.

* 描述(Description):

        count, count-if, 和 count-if-not 计算并返回在序列 sequence 中由 start 和 end 限定并满足这个测试条件 test 的元素的数量.

        这个 from-end 对结果没有直接影响. 然而, 如果 from-end 是 true, 这个序列 sequence 的元素会以逆序提供给 test, test-not, 和 key 作为参数, 它可能改变这些函数的副作用, 如果有的话.

* 示例(Examples):

    ```LISP
    (count #\a "how many A's are there in here?") =>  2
    (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) =>  2
    (count-if #'upper-case-p "The Crying of Lot 49" :start 4) =>  2 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 17.2 (Rules about Test Functions), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数已经被废弃.

        函数 count-if-not 已经被废弃. 


### <span id="F-LENGTH">函数 LENGTH</span>

* 语法(Syntax):

        length sequence => n

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        n---一个非负整数.

* 描述(Description):

        返回在序列 sequence 中的元素数量.

        如果序列 sequence 是一个带有填充指针的向量, 返回由填充指针指定的有效长度.

* 示例(Examples):

    ```LISP
    (length "abc") =>  3
    (setq str (make-array '(3) :element-type 'character 
                                :initial-contents "abc"
                                :fill-pointer t)) =>  "abc"
    (length str) =>  3
    (setf (fill-pointer str) 2) =>  2
    (length str) =>  2
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        list-length, sequence

* 注意(Notes): None. 


### <span id="F-REVERSE-ALL">函数 REVERSE, NREVERSE</span>

* 语法(Syntax):

        reverse sequence => reversed-sequence

        nreverse sequence => reversed-sequence

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        reversed-sequence---一个序列.

* 描述(Description):

        reverse 和 nreverse 返回一个和序列 sequence 相同种类的新的序列, 包含相同的元素, 但是是以逆序的形式.

        reverse 和 nreverse 的区别在于 reverse 总是创建并返回一个新的序列, 反之 nreverse 可能修改并返回给定的序列 sequence. reverse 从不修改那个给定的序列 sequence.

        对于 reverse, 如果序列 sequence 是一个向量, 那么那个结果是一个新的有着和 sequence 相同的实际数组元素类型的一维简单数组. 如果序列 sequence 是一个列表, 那么那个结果就是一个新的列表.

        对于 nreverse, 如果序列 sequence 是一个向量, 那么那个结果也是一个有着和 sequence 相同实际数组元素的向量. 如果序列 sequence 是一个列表, 结果就是一个列表.

        对于 nreverse, 序列 sequence 可能被破坏并且重新使用来产生那个结果. 结果可能和序列 sequence 一样, 也可能不一样. 具体来说, 当序列 sequence 是一个列表时, nreverse 允许去 setf 序列 sequence 的列表结构部分的 cons 的任何部分, car 或 cdr. 当序列 sequence 是一个向量时, nreverse 允许去重排序列 sequence 的元素来产生结果序列.

* 示例(Examples):

    ```LISP
    (setq str "abc") =>  "abc"
    (reverse str) =>  "cba"
    str =>  "abc"
    (setq str (copy-seq str)) =>  "abc"
    (nreverse str) =>  "cba"
    str =>  implementation-dependent
    (setq l (list 1 2 3)) =>  (1 2 3)
    (nreverse l) =>  (3 2 1)
    l =>  implementation-dependent
    ```

* 副作用(Side Effects):

        nreverse 可能创建一个新序列, 或修改参数序列 sequence, 或两者都执行. (reverse 不会修改序列 sequence.)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-SORT-ALL">函数 SORT, STABLE-SORT</span>

* 语法(Syntax):

        sort sequence predicate &key key => sorted-sequence

        stable-sort sequence predicate &key key => sorted-sequence

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        predicate---一个返回一个广义 boolean 的两参数函数的标识符.
        key---一个单参数函数的标识符, 或 nil.
        sorted-sequence---一个序列.

* 描述(Description):

        sort 和 stable-sort 根据断言 predicate 函数确定的顺序破坏性地对序列 sequence 排序.

        如果序列 sequence 是一个向量, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

        sort 由给到断言 predicate 的元素中提取的键来确定两个元素之间的关系. 给这个断言 predicate 函数 的第一个参数是通过 key 函数(如果提供的话)提取的序列 sequence 的一个元素的一部分; 第二个参数是通过 key 函数(如果提供的话)提取的序列 sequence 的另一个元素的一部分. 如果第一个参数严格小于第二个参数(在某个适当的意义下), 那么断言 predicate 应该返回 true. 如果第一个参数大于或等于第二个参数(在某个适当的意义下), 那么这个断言 predicate 应该返回 false.

        给这个 key 函数的参数是这个序列 sequence 元素. 这个 key 函数的返回值称为给断言 predicate 的一个参数. 如果 key 没有被提供或者是 nil, 那么使用这个序列 sequence 的元素自身. 这里不保证这个 key 函数会被调用的次数.

        如果这个 key 和 predicate 总是返回, 那么这个排序操作总是会终止, 产生一个和 sequence 包含相同元素的序列 (这也就是说, 结果是序列 sequence 的一个变换). 这是有保证的即便这个断言 predicate 并不始终表示一个总顺序 (在这个情况中这些元素总是以一种不可预知的方式被搅乱, 但是没有元素会被丢失). 如果这个 key 函数始终返回有意义的键, 并且这个断言 predicate 确实反映了在这些键上的某个总排序准则, 那么在 sorted-sequence 的这些元素会根据那个顺序被正确排序.

        这个由 sort 执行的排序操作不保证稳定. 由断言 predicate 认为是相同的元素可能或可能不会以它们的原始顺序. 如果 (funcall predicate x y) 和 (funcall predicate y x) 都是 false, 那么这个断言 predicate 认为两个元素 x 和 y 是相等的. stable-sort 保证稳定性.

        这个排序操作在所有情况下都可以是破坏性的. 在一个向量参数的情况下, 这是通过对元素进行适当的处理来实现的. 在一个列表的情况下, 这个列表会被破坏性地重排, 以和 nreverse 相同的规矩.

* 示例(Examples):

    ```LISP
    (setq tester (copy-seq "lkjashd")) =>  "lkjashd"
    (sort tester #'char-lessp) =>  "adhjkls"
    (setq tester (list '(1 2 3) '(4 5 6) '(7 8 9))) =>  ((1 2 3) (4 5 6) (7 8 9))
    (sort tester #'> :key #'car)  =>  ((7 8 9) (4 5 6) (1 2 3)) 
    (setq tester (list 1 2 3 4 5 6 7 8 9 0)) =>  (1 2 3 4 5 6 7 8 9 0)
    (stable-sort tester #'(lambda (x y) (and (oddp x) (evenp y))))
    =>  (1 3 5 7 9 2 4 6 8 0)
    (sort (setq committee-data
                (vector (list (list "JonL" "White") "Iteration")
                        (list (list "Dick" "Waters") "Iteration")
                        (list (list "Dick" "Gabriel") "Objects")
                        (list (list "Kent" "Pitman") "Conditions")
                        (list (list "Gregor" "Kiczales") "Objects")
                        (list (list "David" "Moon") "Objects")
                        (list (list "Kathy" "Chapman") "Editorial")
                        (list (list "Larry" "Masinter") "Cleanup")
                        (list (list "Sandra" "Loosemore") "Compiler")))
          #'string-lessp :key #'cadar)
    =>  #((("Kathy" "Chapman") "Editorial")
        (("Dick" "Gabriel") "Objects")
        (("Gregor" "Kiczales") "Objects")
        (("Sandra" "Loosemore") "Compiler")
        (("Larry" "Masinter") "Cleanup")
        (("David" "Moon") "Objects")
        (("Kent" "Pitman") "Conditions")
        (("Dick" "Waters") "Iteration")
        (("JonL" "White") "Iteration"))
    ;; Note that individual alphabetical order within `committees'
    ;; is preserved.
    (setq committee-data 
          (stable-sort committee-data #'string-lessp :key #'cadr))
    =>  #((("Larry" "Masinter") "Cleanup")
        (("Sandra" "Loosemore") "Compiler")
        (("Kent" "Pitman") "Conditions")
        (("Kathy" "Chapman") "Editorial")
        (("Dick" "Waters") "Iteration")
        (("JonL" "White") "Iteration")
        (("Dick" "Gabriel") "Objects")
        (("Gregor" "Kiczales") "Objects")
        (("David" "Moon") "Objects"))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        merge, 章节 3.2.1 (Compiler Terminology), 章节 3.6 (Traversal Rules and Side Effects), 章节 3.7 (Destructive Operations)

* 注意(Notes):

        如果序列 sequence 是一个向量, 那么这个结果可能或可能不是一个简单的, 并且可能或可能不会和序列 sequence 相同. 


### <span id="F-FIND-ALL">函数 FIND, FIND-IF, FIND-IF-NOT</span>

* 语法(Syntax):

        find item sequence &key from-end test test-not start end key => element

        find-if predicate sequence &key from-end start end key => element

        find-if-not predicate sequence &key from-end start end key => element

* 参数和值(Arguments and Values):

        item---一个对象.
        sequence---一个 proper 序列.
        predicate---一个返回一个广义 boolean 的单参数函数的标识符.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回一个广义 boolean 的两个参数的函数的标识符.
        test-not---一个返回一个广义 boolean 的两个参数的函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        key---一个单参数函数的标识符, 或者 nil.
        element---序列 sequence 的一个元素, 或者 nil.

* 描述(Description):

        find, find-if, 和 find-if-not 每一个都搜索序列 sequence 中由 start 和 end 限定, 满足断言 predicate 或满足测试条件 test 或 test-not, 视情况而定.

        如果 from-end 是 true, 那么结果是满足这个测试条件 test 的最右边的元素.

        如果这个序列 sequence 包含了一个满足这个测试条件 test 的元素, 那么就会返回最左边或最右边的序列元素, 取决于 from-end; 否则返回 nil.

* 示例(Examples):

    ```LISP
    (find #\d "here are some letters that can be looked at" :test #'char>)
    =>  #\Space 
    (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t) =>  3
    (find-if-not #'complexp                                    
                '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
                :start 2) =>  NIL 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        position, 章节 17.2 (Rules about Test Functions), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数已经被废弃.

        这个 find-if-not 函数已经被废弃. 


### <span id="F-POSITION-ALL">函数 POSITION, POSITION-IF, POSITION-IF-NOT</span>

* 语法(Syntax):

        position item sequence &key from-end test test-not start end key => position

        position-if predicate sequence &key from-end start end key => position

        position-if-not predicate sequence &key from-end start end key => position

* 参数和值(Arguments and Values):

        item---一个对象.
        sequence---一个 proper 序列.
        predicate---一个返回广义 boolean 的单参数函数的标识符.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别是 0 和 nil.
        key---一个单参数函数的标识符, 或者 nil.
        position---序列 sequence 的一个边界索引, 或者 nil.

* 描述(Description):

        position, position-if, 和 position-if-not 每一个都搜索序列 sequence 来查找一个满足测试条件 test 的元素.

        返回的 position 是在序列 sequence 中满足测试条件 test 的最左边 (如果 from-end 是 true) 或者最右边 (如果 from-end 是 false) 的元素. 返回的索引是相对于整个序列 sequence 的左端, 不管那个 start, end, 或是 from-end 的值.

* 示例(Examples):

    ```LISP
    (position #\a "baobab" :from-end t) =>  4
    (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) =>  2
    (position 595 '()) =>  NIL
    (position-if-not #'integerp '(1 2 3 4 5.0)) =>  4 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        find, 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数已经被废弃.

        函数 position-if-not 已经被废弃. 


### <span id="F-SEARCH">函数 SEARCH</span>

* 语法(Syntax):

        search sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2
        => position

* 参数和值(Arguments and Values):

        Sequence-1---一个序列.
        Sequence-2---一个序列.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        key---一个单参数函数的标识符, 或 nil.
        start1, end1---序列 sequence-1 的边界索引标识符. 对于 start1 和 end1 默认分别是 0 和 nil.
        start2, end2---序列 sequence-2 的边界索引标识符. 对于 start2 和 end2 默认分别是 0 和 nil.
        position---序列 sequence-2 的边界索引, 或 nil.

* 描述(Description):

        搜索序列 sequence-2 来查找一个匹配序列 sequence-1 的子序列.

        具体实现可以选择以任何顺序搜索序列 sequence-2; there is no guarantee on the number of times the test is made. 比如, 当 start-end 是 true, 这个序列 sequence 事实上可能从左到右被搜索而不是从右到左 (但是不管在那种情况下都会返回最右边的匹配子序列). 如果这个搜索成功了, search 返回那个最左边或最右边的匹配子序列的第一个元素在序列 sequence-2 中的偏移位, 取决于 from-end; 否则 search 返回 nil.

        如果 from-end 是 true, 那么那个最右边的匹配子序列的最左边的元素的索引会被返回.

* 示例(Examples):

    ```LISP
    (search "dog" "it's a dog's life") =>  7
    (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) =>  2
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数已经被废弃. 


### <span id="F-MISMATCH">函数 MISMATCH</span>

* 语法(Syntax):

        mismatch sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2
        => position

* 参数和值(Arguments and Values):

        Sequence-1---一个序列.
        Sequence-2---一个序列.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        start1, end1---序列 sequence-1 的边界索引标识符. 对于 start1 和 end1 默认分别是 0 和 nil.
        start2, end2---序列 sequence-2 的边界索引标识符. 对于 start2 和 end2 默认分别是 0 和 nil.
        key---一个单参数函数的标识符, 或者 nil.
        position---序列 sequence-1 的边界索引, 或者 nil.

* 描述(Description):

        这个序列 sequence-1 和序列 sequence-2 的指定子序列按元素比较.

        这个 key 参数同时被用于序列 sequence-1 和序列 sequence-2.

        如果序列 sequence-1 和序列 sequence-2 是相同长度并且每个元素都匹配, 那么结果就是 false. 否则, 结果是一个非负整数, 表示那个两个子序列不匹配的位置在序列 sequence-1 中最左边或最右边索引, 取决于 from-end. 如果一个子序列短于另一个或者是另一个的匹配前缀, 那么结果就是和序列 sequence-1 相关的超出最后一个测试的位置的索引.

        如果 from-end 是 true, 那么返回一加上这些序列 sequences 不同的最右边位置的索引. 实际上, 这些子序列在它们的右边对齐; 那么, 最后一个元素会被比较, 然后是倒数第二个元素, 以此类推. 返回的那个索引是和序列 sequence-1 相关的索引.

* 示例(Examples):

    ```LISP
    (mismatch "abcd" "ABCDE" :test #'char-equal) =>  4
    (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) =>  3
    (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp) =>  NIL
    (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4) =>  NIL 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数已经被废弃. 


### <span id="F-REPLACE">函数 REPLACE</span>

* 语法(Syntax):

        replace sequence-1 sequence-2 &key start1 end1 start2 end2 => sequence-1

* 参数和值(Arguments and Values):

        sequence-1---一个序列.
        sequence-2---一个序列.
        start1, end1---序列 sequence-1 的边界索引标识符. 对于 start1 和 end1 默认分别是 0 和 nil.
        start2, end2---序列 sequence-2 的边界索引标识符. 对于 start2 和 end2 默认分别是 0 和 nil.

* 描述(Description):

        通过用由 start2 和 end2 限定的子序列 subsequence-2 的元素来替换子序列 subsequence-1 中由 start1 和 end1 限定的元素来破坏性地修改序列 sequence-1.

        通过从序列 sequence-2 拷贝连续的元素到序列 sequence-1, 序列 sequence-1 被破坏性地修改. 序列 sequence-2 中由 start2 和 end2 限定的子序列的元素会被拷贝到序列 sequence-1 中由 start1 和 end1 限定的子序列中. 如果这些序列不是相同长度, 那么较短的长度决定了要拷贝多少个元素; 在较长的子序列末尾处的额外元素不会被涉及到这个操作. 被拷贝的数量可以表达为:

        (min (- end1 start1) (- end2 start2))

        如果序列 sequence-1 和序列 sequence-2 是相同的对象并且被修改的区域与被复制的区域重叠, 那么就好像整个源区域被拷贝到另一个位置然后再拷贝回目标区域. 然而, 如果 sequence-1 和 sequence-2 是不同的, 但是被修改的区域与被复制的区域重叠 (或许是由于共享列表结构或者转移数组), 那么在这个替换操作后, 要被修改的序列 sequence-1 的子序列会有不可预测的内容. 如果序列 sequence-2 的元素不是一个可以被存储到序列 sequence-1 中的类型, 那么就是一个错误.

* 示例(Examples):

    ```LISP
    (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4) 
    =>  "abcd456hij"
    (setq lst "012345678") =>  "012345678"
    (replace lst lst :start1 2 :start2 0) =>  "010123456"
    lst =>  "010123456"
    ```

* 副作用(Side Effects):

        这个序列 sequence-1 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        fill

* 注意(Notes): None. 


### <span id="F-SUBSTITUTE-ALL">函数 SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT, NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT</span>

* 语法(Syntax):

        substitute newitem olditem sequence &key from-end test test-not start end count key
        => result-sequence

        substitute-if newitem predicate sequence &key from-end start end count key
        => result-sequence

        substitute-if-not newitem predicate sequence &key from-end start end count key
        => result-sequence

        nsubstitute newitem olditem sequence &key from-end test test-not start end count key
        => sequence

        nsubstitute-if newitem predicate sequence &key from-end start end count key
        => sequence

        nsubstitute-if-not newitem predicate sequence &key from-end start end count key
        => sequence

* 参数和值(Arguments and Values):

        newitem---一个对象.
        olditem---一个对象.
        sequence---一个 proper 序列.
        predicate---一个返回广义 boolean 的单参数函数的标识符.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        count---一个整数或 nil. 默认是 nil.
        key---一个单参数函数的标识符, 或者 nil.
        result-sequence---一个序列.

* 描述(Description):

        substitute, substitute-if, 和 substitute-if-not 返回序列 sequence 的一个拷贝, 其中每个满足测试条件 test 的元素都被替换为 newitem.

        nsubstitute, nsubstitute-if, 和 nsubstitute-if-not 分别类似于 substitute, substitute-if, 和 substitute-if-not, 除了它们可能修改序列 sequence.

        如果序列 sequence 是一个向量, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

        如果提供了 count, 限制要被修改的元素的数量; 如果超过 count 个元素满足测试条件 test, 那么那些元素中只有最左边或最右边的和 count 指定的一样多的元素会被替换, 取决于 from-end. 如果提供了 count 并且是负的, 那么这个行为就好像提供了零一样. 如果 count 是 nil, 所有匹配的项都会被影响.

        只有在那个 count 被提供时 (and non-nil), 提供一个 true 的 from-end 是有关系的; 在这个情况中, 只有最右边 count 个满足测试条件 test 的元素会被移除 (而不是最左边).

        predicate, test, 和 test-not 对于每个序列元素可以被调用不止一次, 而它们的副作用可能以任意顺序发生.

        所有这些函数的结果是一个和 sequence 有着相同类型的序列, 它们有着相同的元素除了那些由 start 和 end 限定并且满足这个测试条件 test 的被替换为 newitem.

        substitute, substitute-if, 和 substitute-if-not 返回一个可能和序列 sequence 共享结构或者如果没有元素需要被改变那么就是和输入序列 sequence 相同的序列.

        nsubstitute 和 nsubstitute-if 需要去 setf 任何需要被替换为 newitem 的序列 sequence 的任何 car (如果序列 sequence 是一个列表) 或 aref (如果 sequence 是一个向量). 如果序列 sequence 是一个列表, 这些顶层列表的 cdr 都不会被修改.

* 示例(Examples):

    ```LISP
    (substitute #\. #\SPACE "0 2 4 6") =>  "0.2.4.6"
    (substitute 9 4 '(1 2 4 1 3 4 5)) =>  (1 2 9 1 3 9 5)
    (substitute 9 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 9 1 3 4 5)
    (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
    =>  (1 2 4 1 3 9 5)
    (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>) =>  (9 9 4 9 3 4 5)

    (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
    =>  ((1) (2) (3) 0)
    (substitute-if 9 #'oddp '(1 2 4 1 3 4 5)) =>  (9 2 4 9 9 4 9)
    (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
    =>  (1 2 4 1 3 9 5)

    (setq some-things (list 'a 'car 'b 'cdr 'c)) =>  (A CAR B CDR C)
    (nsubstitute-if "function was here" #'fboundp some-things
                    :count 1 :from-end t) =>  (A CAR B "function was here" C)
    some-things =>  (A CAR B "function was here" C)
    (setq alpha-tester (copy-seq "ab ")) =>  "ab "
    (nsubstitute-if-not #\z #'alpha-char-p alpha-tester) =>  "abz"
    alpha-tester =>  "abz"
    ```

* 副作用(Side Effects):

        nsubstitute, nsubstitute-if, 和 nsubstitute-if-not 序列 sequence.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        subst, nsubst, 章节 3.2.1 (Compiler Terminology), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        如果序列 sequence 是一个向量, 那么结果可能或可能不是简单的, 并且可能或可能不会和序列 sequence 相同.

        这个 :test-not 参数已经被废弃.

        函数 substitute-if-not 和 nsubstitute-if-not 已经被废弃.

        nsubstitute 和 nsubstitute-if 可以被用于代码中只为了副作用的位置.

        因为副作用变体(比如, nsubstitute)可能会改变被遍历的路径, 它们出现在共享或循环结构的副作用在和它们的无副作用替代比较时可能以令人惊讶的方式表现地不同. 为了观察到这个, 细想以下副作用行为, 它可能被某些实现展现出来:

        (defun test-it (fn)
          (let ((x (cons 'b nil)))
            (rplacd x x)
            (funcall fn 'a 'b x :count 1)))
        (test-it #'substitute) =>  (A . #1=(B . #1#))
        (test-it #'nsubstitute) =>  (A . #1#)


### <span id="F-CONCATENATE">函数 CONCATENATE</span>

* 语法(Syntax):

concatenate result-type &rest sequences => result-sequence

* 参数和值(Arguments and Values):

result-type---a sequence type specifier.

sequences---一个序列.

result-sequence---a proper sequence of type result-type.

* 描述(Description):

concatenate returns a sequence that contains all the individual elements of all the sequences in the order that they are supplied. The sequence is of type result-type, which must be a subtype of type sequence.

All of the sequences are copied from; the result does not share any structure with any of the sequences. Therefore, if only one sequence is provided and it is of type result-type, concatenate is required to copy sequence rather than simply returning it.

It is an error if any element of the sequences cannot be an element of the sequence result. If the result-type is a subtype of list, the result will be a list.

如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

(concatenate 'string "all" " " "together" " " "now") =>  "all together now"
(concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
=>  (#\A #\B #\C D E F 1 2 3 1 0 1 1)
(concatenate 'list) =>  NIL

  (concatenate '(vector * 2) "a" "bc") should signal an error

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error is signaled if the result-type is neither a recognizable subtype of list, nor a recognizable subtype of vector.

An error of type type-error should be signaled if result-type specifies the number of elements and the sum of sequences is different from that number.

* 也见(See Also):

append

* 注意(Notes): None. 


### <span id="F-MERGE">函数 MERGE</span>

* 语法(Syntax):

merge result-type sequence-1 sequence-2 predicate &key key => result-sequence

* 参数和值(Arguments and Values):

result-type---a sequence type specifier.

sequence-1---一个序列.

sequence-2---一个序列.

predicate---一个返回广义 boolean 的两个参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

result-sequence---a proper sequence of type result-type.

* 描述(Description):

Destructively merges sequence-1 with sequence-2 according to an order determined by the predicate. merge determines the relationship between two elements by giving keys extracted from the sequence elements to the predicate.

The first argument to the predicate function is an element of sequence-1 as returned by the key (if supplied); the second argument is an element of sequence-2 as returned by the key (if supplied). Predicate should return true if and only if its first argument is strictly less than the second (in some appropriate sense). If the first argument is greater than or equal to the second (in the appropriate sense), then predicate should return false. merge considers two elements x and y to be equal if (funcall predicate x y) and (funcall predicate y x) both yield false.

The argument to the key is the sequence element. Typically, the return value of the key becomes the argument to predicate. If key is not supplied or nil, the sequence element itself is used. The key may be executed more than once for each sequence element, and its side effects may occur in any order.

If key and predicate return, then the merging operation will terminate. The result of merging two sequences x and y is a new sequence of type result-type z, such that the length of z is the sum of the lengths of x and y, and z contains all the elements of x and y. If x1 and x2 are two elements of x, and x1 precedes x2 in x, then x1 precedes x2 in z, and similarly for elements of y. In short, z is an interleaving of x and y.

If x and y were correctly sorted according to the predicate, then z will also be correctly sorted. If x or y is not so sorted, then z will not be sorted, but will nevertheless be an interleaving of x and y.

The merging operation is guaranteed stable; if two or more elements are considered equal by the predicate, then the elements from sequence-1 will precede those from sequence-2 in the result.

sequence-1 and/or sequence-2 may be destroyed.

If the result-type is a subtype of list, the result will be a list.

如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

 (setq test1 (list 1 3 4 6 7))
 (setq test2 (list 2 5 8))
 (merge 'list test1 test2 #'<) =>  (1 2 3 4 5 6 7 8)
 (setq test1 (copy-seq "BOY"))
 (setq test2 (copy-seq :nosy"))
 (merge 'string test1 test2 #'char-lessp) =>  "BnOosYy"
 (setq test1 (vector ((red . 1) (blue . 4))))
 (setq test2 (vector ((yellow . 2) (green . 7))))
 (merge 'vector test1 test2 #'< :key #'cdr) 
=>  #((RED . 1) (YELLOW . 2) (BLUE . 4) (GREEN . 7)) 

 (merge '(vector * 4) '(1 5) '(2 4 6) #'<) should signal an error

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error must be signaled if the result-type is neither a recognizable subtype of list, nor a recognizable subtype of vector.

An error of type type-error should be signaled if result-type specifies the number of elements and the sum of the lengths of sequence-1 and sequence-2 is different from that number.

* 也见(See Also):

sort, stable-sort, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-REMOVE-ALL">函数 REMOVE, REMOVE-IF, REMOVE-IF-NOT, DELETE, DELETE-IF, DELETE-IF-NOT</span>

* 语法(Syntax):

remove item sequence &key from-end test test-not start end count key => result-sequence

remove-if test sequence &key from-end start end count key => result-sequence

remove-if-not test sequence &key from-end start end count key => result-sequence

delete item sequence &key from-end test test-not start end count key => result-sequence

delete-if test sequence &key from-end start end count key => result-sequence

delete-if-not test sequence &key from-end start end count key => result-sequence

* 参数和值(Arguments and Values):

item---一个对象.

sequence---一个 proper 序列.

test---一个返回广义 boolean 的单参数函数的标识符.

from-end---一个广义 boolean. The default is false.

test---一个返回广义 boolean 的两个参数函数的标识符.

test-not---一个返回广义 boolean 的两个参数函数的标识符.

start, end---序列 sequence 的边界索引标识符. The defaults for start and end are 0 and nil, respectively.

count---an integer or nil. The default is nil.

key---一个单参数函数的标识符, 或者 nil.

result-sequence---一个序列.

* 描述(Description):

remove, remove-if, and remove-if-not return a sequence from which the elements that satisfy the test have been removed.

delete, delete-if, and delete-if-not are like remove, remove-if, and remove-if-not respectively, but they may modify sequence.

If sequence is a vector, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

Supplying a from-end of true matters only when the count is provided; in that case only the rightmost count elements satisfying the test are deleted.

Count, if supplied, limits the number of elements removed or deleted; if more than count elements satisfy the test, then of these elements only the leftmost or rightmost, depending on from-end, are deleted or removed, as many as specified by count. If count is supplied and negative, the behavior is as if zero had been supplied instead. If count is nil, all matching items are affected.

For all these functions, elements not removed or deleted occur in the same order in the result as they did in sequence.

remove, remove-if, remove-if-not return a sequence of the same type as sequence that has the same elements except that those in the subsequence bounded by start and end and satisfying the test have been removed. This is a non-destructive operation. If any elements need to be removed, the result will be a copy. The result of remove may share with sequence; the result may be identical to the input sequence if no elements need to be removed.

delete, delete-if, and delete-if-not return a sequence of the same type as sequence that has the same elements except that those in the subsequence bounded by start and end and satisfying the test have been deleted. Sequence may be destroyed and used to construct the result; however, the result might or might not be identical to sequence.

delete, when sequence is a list, is permitted to setf any part, car or cdr, of the top-level list structure in that sequence. When sequence is a vector, delete is permitted to change the dimensions of the vector and to slide its elements into new positions without permuting them to produce the resulting vector.

delete-if is constrained to behave exactly as follows:

 (delete nil sequence
             :test #'(lambda (ignore item) (funcall test item))
             ...)

* 示例(Examples):

 (remove 4 '(1 3 4 5 9)) =>  (1 3 5 9)
 (remove 4 '(1 2 4 1 3 4 5)) =>  (1 2 1 3 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 1 3 4 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
 (remove 3 '(1 2 4 1 3 4 5) :test #'>) =>  (4 3 4 5)
 (setq lst '(list of four elements)) =>  (LIST OF FOUR ELEMENTS)
 (setq lst2 (copy-seq lst)) =>  (LIST OF FOUR ELEMENTS)
 (setq lst3 (delete 'four lst)) =>  (LIST OF ELEMENTS)
 (equal lst lst2) =>  false
 (remove-if #'oddp '(1 2 4 1 3 4 5)) =>  (2 4 4)
 (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) 
=>  (1 2 4 1 3 5)
 (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
=>  (1 2 3 4 5 6 8)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete 4 tester) =>  (1 2 1 3 5)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete 4 tester :count 1) =>  (1 2 1 3 4 5)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete 4 tester :count 1 :from-end t) =>  (1 2 4 1 3 5)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete 3 tester :test #'>) =>  (4 3 4 5)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete-if #'oddp tester) =>  (2 4 4)
 (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
 (delete-if #'evenp tester :count 1 :from-end t) =>  (1 2 4 1 3 5)    
 (setq tester (list 1 2 3 4 5 6)) =>  (1 2 3 4 5 6) 
 (delete-if #'evenp tester) =>  (1 3 5) 
 tester =>  implementation-dependent

 (setq foo (list 'a 'b 'c)) =>  (A B C)
 (setq bar (cdr foo)) =>  (B C)
 (setq foo (delete 'b foo)) =>  (A C)
 bar =>  ((C)) or ...
 (eq (cdr foo) (car bar)) =>  T or ...

* 副作用(Side Effects):

For delete, delete-if, and delete-if-not, sequence may be destroyed and used to construct the result.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

If sequence is a vector, the result might or might not be simple, and might or might not be identical to sequence.

这个 :test-not 参数已经被废弃.

The functions delete-if-not and remove-if-not are deprecated. 

### <span id="F-DUPLICATES-ALL">函数 REMOVE-DUPLICATES, DELETE-DUPLICATES</span>

* 语法(Syntax):

remove-duplicates sequence &key from-end test test-not start end key

=> result-sequence

delete-duplicates sequence &key from-end test test-not start end key

=> result-sequence

* 参数和值(Arguments and Values):

sequence---一个 proper 序列.

from-end---一个广义 boolean. The default is false.

test---一个返回广义 boolean 的两个参数函数的标识符.

test-not---一个返回广义 boolean 的两个参数函数的标识符.

start, end---序列 sequence 的边界索引标识符. The defaults for start and end are 0 and nil, respectively.

key---一个单参数函数的标识符, 或者 nil.

result-sequence---一个序列.

* 描述(Description):

remove-duplicates returns a modified copy of sequence from which any element that matches another element occurring in sequence has been removed.

If sequence is a vector, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

delete-duplicates is like remove-duplicates, but delete-duplicates may modify sequence.

The elements of sequence are compared pairwise, and if any two match, then the one occurring earlier in sequence is discarded, unless from-end is true, in which case the one later in sequence is discarded.

remove-duplicates and delete-duplicates return a sequence of the same type as sequence with enough elements removed so that no two of the remaining elements match. The order of the elements remaining in the result is the same as the order in which they appear in sequence.

remove-duplicates returns a sequence that may share with sequence or may be identical to sequence if no elements need to be removed.

delete-duplicates, when sequence is a list, is permitted to setf any part, car or cdr, of the top-level list structure in that sequence. When sequence is a vector, delete-duplicates is permitted to change the dimensions of the vector and to slide its elements into new positions without permuting them to produce the resulting vector.

* 示例(Examples):

 (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t) =>  "aBcD"
 (remove-duplicates '(a b c b d d e)) =>  (A C B D E)
 (remove-duplicates '(a b c b d d e) :from-end t) =>  (A B C D E)
 (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
     :test #'char-equal :key #'cadr) =>  ((BAR #\%) (BAZ #\A))
 (remove-duplicates '((foo #\a) (bar #\%) (baz #\A)) 
     :test #'char-equal :key #'cadr :from-end t) =>  ((FOO #\a) (BAR #\%))
 (setq tester (list 0 1 2 3 4 5 6))
 (delete-duplicates tester :key #'oddp :start 1 :end 6) =>  (0 4 5 6)

* 副作用(Side Effects):

delete-duplicates might destructively modify sequence.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if sequence is not a proper sequence.

* 也见(See Also):

Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

If sequence is a vector, the result might or might not be simple, and might or might not be identical to sequence.

这个 :test-not 参数已经被废弃.

These functions are useful for converting sequence into a canonical form suitable for representing a set. 



