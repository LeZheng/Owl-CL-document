# 17. 序列

> * 17.1 [序列的概念](#SequenceConcepts)
> * 17.2 [关于测试函数的规则](#RulesTestFunctions)
> * 17.3 [序列的字典](#TheSequencesDictionary)


## 17.1 <span id="SequenceConcepts">序列的概念</span>

一个序列[sequence]是多个元素[element]的一个有序集合, 实现为一个向量[vector]或一个列表[list].

序列[sequence]可以由函数[function] make-sequence 创建, 其他创建 sequence 的子类型[subtype]的对象[object]的函数[function]也可以 (比如, list, make-list, mapcar, 和 vector).

一个序列函数[sequence function]是一个由这个规范定义的或者由具体实现[implementation]作为一个扩展所添加的一个操作一个或多个序列[sequence]上的函数[function]. 在一个序列函数[sequence function]必须构造并返回一个新的向量[vector]时, 它总是返回一个简单向量[simple vector]. 类似地, 任何构造的字符串[string]都会是简单字符串[simple string].

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

通常, 被当作序列[sequence]的列表[list] (包括关联列表[association list]和属性列表[property list]) 必须是正规列表[proper list]. 


## 17.2 <span id="RulesTestFunctions">关于测试函数的规则</span>

> * 17.2.1 [满足一个两个参数的测试](#SatisfyingTwoArgumentTest)
> * 17.2.2 [满足一个单参数的测试](#SatisfyingOneArgumentTest)

### 17.2.1 <span id="SatisfyingTwoArgumentTest">满足一个两个参数的测试</span>
<!--TODO consider 考虑 ？？-->
当通过下面这段列出的操作符[operator] F 在一个序列[sequence] S 的每一个元素[element] Ei 上迭代地考虑对象[object] O 时, 有时控制操作符 F 测试 O 在 S 中的存在性的方式是有用的. 基于一个被 :test 或 :test-not 实参[argument]标识的函数[function]上来提供这个控制.

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

这个对象 O 可能不会直接和 Ei 比较. 如果提供了一个 :key 实参[argument], 它就是要被调用的单参数[argument]的函数[function]的标识符[designator], 而每一个 Ei 作为一个实参[argument], 并且产生一个对象[object] Zi 被用作这个比较. (如果这里没有 :key 实参[argument], Zi 就是 Ei.)

这个由 :key 参数[argument]标识的函数[function]从不在 O 自身上调用. 然而, 如果这个函数在多个序列上操作 (比如, 就像发生在 set-difference), O 会是在其他序列的元素[element]上调用这个函数的结果.

如果提供给 F 的一个 :test 参数[argument], 那么它就是一个两参数[argument]函数[function]的标识符[designator], 参数为 O 和 Zi. 如果这个 :test 函数[function]返回表示 true 的广义 boolean [generalized boolean], 那么就说一个 Ei (或者, 有时候, 就说一个 O 和一个 Ei) 满足这个测试条件[satisfy the test].

如果提供 F 一个 :test-not 参数[argument], 那么它就是一个两参数[argument]函数[function]的标识符[designator], 参数是 O 和 Zi. 如果这个 :test-not 函数[function]返回一个表示 false 的广义 boolean [generalized boolean], 那么就说 Ei (或者, 有时候, 就说一个 O 和一个 Ei) 满足这个测试条件[satisfy the test].

如果 :test 和 :test-not 参数[argument]都没有提供, 那么处理方式就跟提供了一个 #'eql 的 :test 参数一样.

如果在对 F 的同一个调用[call]中 :test 和 :test-not 参数[argument]都提供了, 那么后果是未指定的.

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

当使用下面这段中的函数[function]的其中一个时, 序列[sequence] S 的元素 E 不是基于章节 17.2.1 (满足一个两个参数的测试) 中描述的两参数[argument]断言[predicate]下对象 O 的存在或缺失来过滤, 而是基于单参数[argument]断言[predicate]来过滤.

    assoc-if       member-if           rassoc-if          
    assoc-if-not   member-if-not       rassoc-if-not      
    count-if       nsubst-if           remove-if          
    count-if-not   nsubst-if-not       remove-if-not      
    delete-if      nsubstitute-if      subst-if           
    delete-if-not  nsubstitute-if-not  subst-if-not       
    find-if        position-if         substitute-if      
    find-if-not    position-if-not     substitute-if-not  

    Figure 17-3. 有单参数的测试需要满足的操作符
<!--TODO consider 考虑 ？？-->
元素 Ei 可能不会被直接考虑. 如果提供了一个 :key 参数[argument], 那么它就是一个单参数[argument]函数[function]的标识符[designator], 用每一个 Ei 作为一个实参[argument]来调用, 并且产生一个要被用来比较的对象[argument] Zi. (如果没有 :key 参数[argument], Zi 就是 Ei.)

在这个规范中定义的并且有着一个以 "-if" 结尾的名字的函数[function]接受一个单参数[argument] Zi 的函数[function]的标识符[designator]作为第一个参数[argument]. 如果这个 :test 函数[function]返回一个表示  true 的广义 boolean [generalized boolean], 那么就说一个 Ei 满足这个测试条件[satisfy the test].

在这个规范中定义的并且有着一个以 "-if-not" 结尾的名字的函数[function]接受一个单参数[argument] Zi 的函数[function]的标识符[designator]作为第一个参数[argument]. 如果这个 :test 函数[function]返回一个表示 false 的广义 boolean [generalized boolean], 那么就说一个 Ei 满足这个测试条件[satisfy the test].


#### 17.2.2.1 满足一个单参数的测试的示例

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

        序列[sequence]是对象[object]的有序集合, 这些对象称为序列[sequence]的元素[element].

        类型[type] vector 和类型[type] list 是类型[type] sequence 的互斥的[disjoint]子类型[subtype], 但没有必要是序列[sequence]的一个详尽分区[exhaustive partition].

        当把一个向量[vector]视作一个序列[sequence]时, 只有这个向量[vector]的有效[active]元素[element]被当作这个序列[sequence]的元素[element]; 这也就是说, 当给定的序列[sequence]被表示为向量[vector]时, 序列[sequence]操作遵守填充指针[fill pointer]. 


### <span id="F-COPY-SEQ">函数 COPY-SEQ</span>

* 语法(Syntax):

        copy-seq sequence => copied-sequence

* 参数和值(Arguments and Values):

        sequence---一个正规序列[proper sequence].
        copied-sequence---一个正规序列[proper sequence].

* 描述(Description):

        创建一个序列 sequence 的拷贝. 这个新的序列[sequence]中的元素[element]和给定序列 sequence 中的对应元素[element]是相同的[same].

        如果序列 sequence 是一个向量[vector], 那么结果就是秩[rank]为一的新的[fresh]简单数组[simple array], 并且有着和序列 sequence 相同的实际数组元素类型[actual array element type]. 如果序列 sequence 是一个列表[list], 那么结果就是一个新的[fresh]列表[list].

* 示例(Examples):

    ```LISP
    (setq str "a string") =>  "a string"
    (equalp str (copy-seq str)) =>  true
    (eql str (copy-seq str)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个正规序列[proper sequence], 那么应该准备发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        copy-list

* 注意(Notes):

        从功能的角度来看,

        (copy-seq x) ==  (subseq x 0)

        然而, 在这两种情况下, 程序员的意图通常是不同的. 


### <span id="A-ELT">访问器 ELT</span>

* 语法(Syntax):

        elt sequence index => object

        (setf (elt sequence index) new-object)

* 参数和值(Arguments and Values):

        sequence---一个正规序列[proper sequence].
        index---对于序列 sequence 的一个有效序列索引[valid sequence index].
        object---一个对象[object].
        new-object---一个对象[object].

* 描述(Description):

        访问[access]由索引 index 指定的序列 sequence 中的元素[element].

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

        如果 sequence 不是一个正规序列[proper sequence], 那么应该准备发出一个 type-error 类型[type]的错误. 如果 index 对于序列 sequence 的不是一个有效序列索引[valid sequence index], 那么应该发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        aref, nth, 章节 3.2.1 (编译器术语)

* 注意(Notes):

        aref 可能被用于访问[access]超出这个向量[vector]填充指针[fill pointer]的向量[vector]元素[element]. 

### <span id="F-FILL">函数 FILL</span>

* 语法(Syntax):

        fill sequence item &key start end => sequence

* 参数和值(Arguments and Values):

        sequence---一个正规序列[proper sequence].
        item---一个序列[sequence].
        start, end---序列 sequence 的边界索引标识符[bounding index designator]. 对于 start 和 end 默认分别是 0 和 nil.

* 描述(Description):

        用 item 替换由 start 和 end 限定[bounded]的序列 sequence 中的元素[element].

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

        如果序列 sequence 不是一个正规序列[proper sequence], 那么应该准备去发出一个 type-error 类型[type]的错误. 如果 start 不是一个非负整数[integer], 那么应该发出一个 type-error 类型[type]的错误. 如果 end 不是一个非负整数[integer]或者 nil, 那么应该发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        replace, nsubstitute

* 注意(Notes):

        (fill sequence item) == (nsubstitute-if item (constantly t) sequence) 


### <span id="F-MAKE-SEQUENCE">函数 MAKE-SEQUENCE</span>

* 语法(Syntax):

        make-sequence result-type size &key initial-element => sequence

* 参数和值(Arguments and Values):

        result-type---一个 sequence 类型指定符[type specifier].
        size---一个非负整数[integer].
        initial-element---一个对象[object]. 默认值是依赖于具体实现的[implementation-dependent].
        sequence---一个正规序列[proper sequence].

* 描述(Description):

        返回一个 result-type 类型并且长度为 size 的序列[sequence], 其中的每一个元素[element]都被初始化为 initial-element.

        如果 result-type 是 list 的一个子类型[subtype], 那么结果会是一个列表[list].

        如果 result-type 是 vector 的一个子类型[subtype], 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升[upgrade]的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

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

        这个具体实现[implementation].

* 异常情况(Exceptional Situations):

        如果 initial-element 不是一个可以被存储在产生的序列[sequence]中的对象[object], 那么后果是未指定的.

        如果 result-type 既不是一个 list 的可识别子类型[recognizable subtype], 也不是一个 vector 的可识别子类型[recognizable subtype], 那么就会发出一个 type-error 类型[type]的错误.

        如果 result-type 指定的元素的数量并且 size 和那个数量不同, 那么应该发出一个 type-error 类型[type]的错误.

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

        这个函数 function 必须接受作为序列 sequence 的两个元素的参数, 或者将这些元素组合起来的结果. 这个函数 function 必须也可以接受没有参数的情况.

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

        merge, 章节 3.2.1 (编译器术语), 章节 3.6 (Traversal Rules and Side Effects), 章节 3.7 (Destructive Operations)

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

        subst, nsubst, 章节 3.2.1 (编译器术语), 章节 3.6 (Traversal Rules and Side Effects)

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

        result-type---一个 sequence 类型指定符.
        sequences---一个序列.
        result-sequence---一个 result-type 类型的 proper 序列.

* 描述(Description):

        concatenate 返回一个包含了所有这些序列 sequences 的单独元素的序列, 以它们被提供的顺序. 这个序列是 result-type 类型的, 这个类型必须是类型 sequence 的一个子类型.

        所有这些序列 sequences 都会被拷贝; 这个结果不会和这些序列 sequences 的任何一个共享结构. 因此, 如果只提供了一个序列 sequence 并且它是 result-type 类型的, concatenate 需要去拷贝这个序列 sequence 而不是简单地返回它.

        如果这些元素 sequences 的任何一个元素不能是这些结果序列的一个元素, 那么就是一个错误. 如果这个 result-type 是 list 的一个子类型, 结果会是一个列表.

        如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

    ```LISP
    (concatenate 'string "all" " " "together" " " "now") =>  "all together now"
    (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
    =>  (#\A #\B #\C D E F 1 2 3 1 0 1 1)
    (concatenate 'list) =>  NIL

      (concatenate '(vector * 2) "a" "bc") should signal an error
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 result-type 既不是 list 的一个可识别子类型, 也不是 vector 的一个可识别子类型, 那么就会发出一个错误.

        如果 result-type 指定的元素的数量和这些序列 sequences 的总数在数量上不同, 那么就会发出一个 type-error 类型的错误.

* 也见(See Also):

        append

* 注意(Notes): None. 


### <span id="F-MERGE">函数 MERGE</span>

* 语法(Syntax):

        merge result-type sequence-1 sequence-2 predicate &key key => result-sequence

* 参数和值(Arguments and Values):

        result-type---一个 sequence 类型指定符.
        sequence-1---一个序列.
        sequence-2---一个序列.
        predicate---一个返回广义 boolean 的两个参数函数的标识符.
        key---一个单参数函数的标识符, 或者 nil.
        result-sequence---一个 result-type 类型的 proper 序列.

* 描述(Description):

        根据断言 predicate 决定的顺序破坏性地合并 sequence-1 和 sequence-2. merge 通过给定从这些序列元素中的键到这个断言 predicate 来决定两个元素之间的关系.

        给这个 predicate 函数的第一个参数是由 key 返回的序列 sequence-1 的一个元素 (如果提供的话); 第二个元素是由 key 返回的序列 sequence-2 的元素 (如果提供的话). 当且仅当这个断言 predicate 的第一个参数严格小于第二个参数时(在一些适当的意义上), 它应该返回 true. 如果第一个参数大于或等于第二个参数 (在一些适当的意义上), 那么断言 predicate 应该返回 false. 如果 (funcall predicate x y) 和 (funcall predicate y x) 都产生 false, 那么 merge 把这两个元素 x 和 y 认为是相等的.

        给 key 的参数是这个序列 sequence 元素. 通常地, 这个 key 的返回值成为给断言 predicate 的参数. 如果没有提供 key 或者是 nil, 那么就使用这个序列元素自身. 这个 key 对于每个序列元素可能被执行不止一次, 那么它的副作用可能以任意顺序发生.

        如果 key 和 predicate 返回了, 那么这个合并操作就会终止. 合并两个序列 x 和 y 的结果是一个类型 result-type 的新序列 z, 这样一来 z 的长度是 x 和 y 的总和, 并且 z 包含了 x 和 y 的所有元素. 如果 x1 和 x2 是 x 的两个元素, 并且在 x 中 x1 在 x2 前面, 那么在 z 中 x1 也在 x2 前面, 而对于 y 的元素也类型. 总之, z 是 x 和 y 的交错.

        如果 x 和 y 根据断言 predicate 被正确地排序, 那么 z 也会被正确排序. 如果 x 或 y 没有被这样排序, 那么 z 不会被排序, 但它仍然是 x 和 y 的交错.

        这个合并操作保证稳定的; 如果根据这个断言 predicate, 两个或更多元素被认为是相等的, 那么在结果中来自于序列 sequence-1 的元素会在来自于序列 sequence-2 的元素之前.

        sequence-1 和/或 sequence-2 可能会被破坏.

        如果这个 result-type 是 list 的一个子类型, 结果可能是一个列表.

        如果 result-type 是 vector 的一个子类型, 那么如果实现可以确定 result-type 指定的元素类型, 那么产生的数组的元素类型就是那个元素类型的提升的结果; 或者, 如果实现可以确定那个元素类型是未指定的 (或 *), 产生的元素类型就是 t; 否则, 发出一个错误.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 result-type 既不是 list 的一个可识别子类型, 也不是 vector 的一个可识别子类型, 那么就会发出一个错误.

        如果 result-type 指定的元素的数量和这些序列 sequence-1 和 sequence-2 的长度在数量上不同, 那么就会发出一个 type-error 类型的错误.

* 也见(See Also):

        sort, stable-sort, 章节 3.2.1 (编译器术语), 章节 3.6 (Traversal Rules and Side Effects)

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
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        count---一个整数 nil. 默认是 nil.
        key---一个单参数函数的标识符, 或者 nil.
        result-sequence---一个序列.

* 描述(Description):

        remove, remove-if, 和 remove-if-not 返回一个序列, 这个序列中满足测试条件 test 的元素已经被移出.

        delete, delete-if, 和 delete-if-not 分别类似于 remove, remove-if, 和 remove-if-not, 但是它们修改序列 sequence.

        如果序列 sequence 是一个 vector, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

        提供一个为 true 的 from-end 只有在这个 count 被提供时起作用; 在这个情况下只有最右边满足测试条件 test 的 count 的元素会被删除.

        count, 如果提供的话, 限制要被移除或删除的元素的数量; 如果超过 count 个元素满足这个测试条件 test, 那么这些元素只有最右边或最左边由 count 指定的数量会被删除, 取决于 from-end. 如果 count 被提供并且是负的, 那么这个行为就跟提供了零一样. 如果 count 是 nil, 那么所有匹配的项都会被影响.

        对于所有这些函数, 元素在结果中被移除的顺序和它们在序列 sequence 中的一样.

        remove, remove-if, remove-if-not 返回一个和序列 sequence 相同类型的序列, 并且有着相同的元素除了那些在 start 和 end 限定的子序列中哦个满足这个测试条件 test 的会被移除. 这是一个非破坏性的操作. 如果没有元素需要被移除, 那么结果就是一个拷贝. 这个 remove 的结果可能和序列 sequence 共享结构; 如果没有元素需要被移除, 这个结果可能和输入的序列 sequence 是相同的.

        delete, delete-if, 和 delete-if-not 返回一个和序列 sequence 相同类型的序列, 并且有着相同的元素除了那些在 start 和 end 限定的子序列中哦个满足这个测试条件 test 的会被删除. 序列 sequence 可能被破坏并且被用于构造这个结果; 然而, 这个结果可能也可能不会和序列 sequence 相同.

        delete, 当序列 sequence 是一个列表时, 允许去 setf 这个序列 sequence 中的顶层列表结构的任何部分, car 或 cdr. 当序列 sequence 是一个向量时, delete 允许去改变这个向量的规模去滑动它的元素到新的位置, 在没有交换它们的情况下产生这个结果向量.

        delete-if 被约束为表现得像下面这样:

        (delete nil sequence
                    :test #'(lambda (ignore item) (funcall test item))
                    ...)

* 示例(Examples):

    ```LISP
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
    ```

* 副作用(Side Effects):

        对于 delete, delete-if, 和 delete-if-not, sequence 可能被破坏并被用于构造这个结果.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该准备去发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (编译器术语), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        如果序列 sequence 是一个向量, 那么结果可能也可能不是简单的, 并且可能也可能不会和序列 sequence 相同.

        这个 :test-not 参数已经被废弃.

        函数 delete-if-not 和 remove-if-not 已经被废弃. 

### <span id="F-DUPLICATES-ALL">函数 REMOVE-DUPLICATES, DELETE-DUPLICATES</span>

* 语法(Syntax):

        remove-duplicates sequence &key from-end test test-not start end key
        => result-sequence

        delete-duplicates sequence &key from-end test test-not start end key
        => result-sequence

* 参数和值(Arguments and Values):

        sequence---一个 proper 序列.
        from-end---一个广义 boolean. 默认是 false.
        test---一个返回广义 boolean 的两个参数函数的标识符.
        test-not---一个返回广义 boolean 的两个参数函数的标识符.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别是 0 和 nil.
        key---一个单参数函数的标识符, 或者 nil.
        result-sequence---一个序列.

* 描述(Description):

        remove-duplicates 返回序列 sequence 的一个被修改的拷贝, 其中任何匹配出现在序列 sequence 中的另一个元素的元素已经被移除.

        如果 sequence 是一个向量, 那么结果是一个和 sequence 有着相同实际数组元素类型的向量. 如果序列 sequence 是一个列表, 那么结果也是有一个列表.

        delete-duplicates 类似于 remove-duplicates, 但是 delete-duplicates 可能修改序列 sequence.

        序列 sequence 的元素是成对的比较, 如果任意两个匹配, 那么更早出现在序列 sequence 中的那个会被丢弃, 除非 from-end 是 true, 在这个情况下在序列 sequence 中较晚的那个会被丢弃.

        remove-duplicates 和 delete-duplicates 返回一个和序列 sequence 相同类型的序列, 其中足够的元素被移除以致于没有两个剩余元素是匹配的. 在这个结果中剩余元素的顺序和它们出现在序列 sequence 中的顺序是一样的.

        remove-duplicates 返回一个可能和序列 sequence 共享的序列, 如果没有元素需要被移除, 那么可能返回一个和序列 sequence 相同的序列.

        delete-duplicates, 当序列 sequence 是一个列表, 允许去 setf 这个序列 sequence 中的顶层列表结构的任何部分, car 或 cdr. 当序列 sequence 是一个向量时, delete 允许去改变这个向量的规模去滑动它的元素到新的位置, 在没有交换它们的情况下产生这个结果向量.

* 示例(Examples):

    ```LISP
    (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t) =>  "aBcD"
    (remove-duplicates '(a b c b d d e)) =>  (A C B D E)
    (remove-duplicates '(a b c b d d e) :from-end t) =>  (A B C D E)
    (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
        :test #'char-equal :key #'cadr) =>  ((BAR #\%) (BAZ #\A))
    (remove-duplicates '((foo #\a) (bar #\%) (baz #\A)) 
        :test #'char-equal :key #'cadr :from-end t) =>  ((FOO #\a) (BAR #\%))
    (setq tester (list 0 1 2 3 4 5 6))
    (delete-duplicates tester :key #'oddp :start 1 :end 6) =>  (0 4 5 6)
    ```

* 副作用(Side Effects):

        delete-duplicates 可能破坏性地修改序列 sequence.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (编译器术语), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        如果序列 sequence 是一个序列, 那么结果可能也可能不是简单的, 并且可能也可能不会和序列 sequence 相同.

        这个 :test-not 参数已经被废弃.

        这些函数对于转换序列 sequence 为适合表示集合的规范形式是非常有用. 



