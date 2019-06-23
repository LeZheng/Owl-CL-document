# 18. 哈希表

> * 18.1 [哈希表的概念](#HashTableConcepts)
> * 18.2 [哈希表的字典](#TheHashTablesDictionary)

## 18.1 <span id="HashTableConcepts">哈希表的概念</span>

> * 18.1.1 [哈希表操作](#HashTableOperations)
> * 18.1.2 [修改哈希表的键](#ModifyingHashTableKeys)


### 18.1.1 <span id="HashTableOperations">哈希表操作</span>

下面这段列出了一些可以应用于哈希表[hash table]的已定义的名字[defined name]. 下面规则应用于哈希表[hash table].

-- 一个哈希表[hash table]只能关联一个值和一个给定的键. 如果尝试去添加一个给定的键的第二个值, 第二个值会替换第一个. 因此, 添加一个值到哈希表[hash table]是一个破坏性操作; 这个哈希表[hash table]会被修改.

-- 这里有四种哈希表[hash table]: 键是使用 eq 来比较的那些, 键是使用 eql 来比较的那些, 键是使用 equal 来比较的那些, 以及键是使用 equalp 来比较的那些.

-- 哈希表[hash table]由 make-hash-table 创建. gethash 被用于搜索一个键以及找到那个关联的值. 新的条目会使用 setf 和 gethash 来添加到哈希表[hash table]中. remhash 被用于移除一个条目. 比如:

    ```LISP
    (setq a (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32536573>
    (setf (gethash 'color a) 'brown) =>  BROWN
    (setf (gethash 'name a) 'fred) =>  FRED
    (gethash 'color a) =>  BROWN, true
    (gethash 'name a) =>  FRED, true
    (gethash 'pointy a) =>  NIL, false
    ```

    在这个例子中, 符号 color 和 name 被用作键, 而符号 brown 和 fred 被用作关联的值. 这个哈希表[hash table]中有两个值, 一个关联是从 color 到 brown, 而另一个关联是从 name 到 fred.

-- 一个键和一个值可以是任何对象[object].

-- 一个条目在哈希表[hash table]中的存在性可以由 gethash 返回的第二个值[secondary value]来决定.

    clrhash           hash-table-p     remhash  
    gethash           make-hash-table  sxhash   
    hash-table-count  maphash                   

    Figure 18-1. Hash-table 已定义的名字 

### 18.1.2 <span id="ModifyingHashTableKeys">修改哈希表的键</span>

提供给 make-hash-table 的 :test 参数的函数指定了它创建的哈希表[hash table]的 '等价性测试(equivalence test)'.

如果存在一组对象[object] (或潜在对象[object]), 在修改之前, 和一个对象[object]是等价的, 但在随后的情况下不再是等价的, 那么相对于等价性测试那个对象[object]就是被'可见地修改'的.

如果一个对象[object] O1 被用作哈希表[hash table] H 中的一个键并且接下来相对于 H 的等价性测试被可见地修改, 那么如果 O1 或任何在这个等价性测试下等价于 O1 的对象[object] O2 (不管在这个修改之前或之后) 在后面对 H 的操作中被用作一个键, 后果是未指定的. 即便 O1 被可见地修改并且接着被再次修改来撤销这个可见的修改, 那么使用 O1 作为一个 key 的后果也是未指定的.

下面是哈希表[hash table]必须支持的相对于等价性测试可见的修改的规范. 这些修改是用成员的修改来描述的, 并且是递归地定义的. 这个对象[object]的成员的可见修改是这个对象[object]的可见修改.

> * 18.1.2.1 [关于 EQ 和 EQL 的对象可见修改](#VM-EQ-EQL)
> * 18.1.2.2 [关于 EQUAL 的对象可见修改](#VM-EQUAL)
> * 18.1.2.3 [关于 EQUALP 的对象可见修改](#VM-EQUALP)
> * 18.1.2.4 [通过语言扩展的可见修改](#VM-LanguageExtensions)


#### 18.1.2.1 <span id="VM-EQ-EQL">关于 EQ 和 EQL 的对象可见修改</span>

没有提供标准化[standardized]函数[function]能够对于 eq 或 eql 可见地修改一个对象[object]. 

#### 18.1.2.2 <span id="VM-EQUAL">关于 EQUAL 的对象可见修改</span>

作为 equal 的行为的结果, 对于没有在这个章节中被显式提及的对象[object]的可见修改的规则衍生自那些在章节 18.1.2.1 (关于 EQ 和 EQL 的对象可见修改) 中提及的.

##### 18.1.2.2.1 关于 EQUAL 的 cons 可见修改

对一个 cons 的 car 或 cdr 的任何可见修改都被当作是对于 equal 可见的修改. 


##### 18.1.2.2.2 关于 EQUAL 的字符串和位向量的可见修改

对于一个 bit-vector 类型[type]或 string 类型[type]的向量[vector], 对这个向量[vector]的一个有效[active]元素[element]或者对这个向量[vector]的长度[length] (如果它实际上是可调整的[actually adjustable]或者有着一个填充指针[fill pointer])的任何可见修改都被认为是对于 equal 的可见修改. 


#### 18.1.2.3 <span id="VM-EQUALP">关于 EQUALP 的对象可见修改</span>

作为 equalp 的行为的结果, 对于没有在这个章节中被显式提及的对象[object]的可见修改的规则衍生自那些在章节 18.1.2.2 (关于 EQUAL 的对象可见修改) 中提及的.

##### 18.1.2.3.1 关于 EQUALP 的结构体可见修改

对一个结构体[structure]的槽[slot]的任何可见修改都被当作是对于 equalp 可见的修改. 

##### 18.1.2.3.2 关于 EQUALP 的数组可见修改

在一个数组[array]中, 对一个有效[active]元素[element], 填充指针[fill pointer] (如果这个数组[array]可以并且确实有一个的话), 或者维度大小[dimension] (如果这个数组[array]实际上是可调整的[actually adjustable])的任何可见修改都会被当作相对于 equalp 可见的修改. 

##### 18.1.2.3.3 关于 EQUALP 的哈希表可见修改

在一个哈希表[hash table]中, 对这个哈希表[hash table]中的条目数量, 键, 或者和那些键关联的值的任何可见修改都会被当作相对于 equalp 可见的修改.

注意, 对于这些键修改的可见性取决于这个哈希表[hash table]的等价性测试, 而不是 equalp 的说明. 

#### 18.1.2.4 <span id="VM-LanguageExtensions">通过语言扩展的可见修改</span>

通过提供额外的更改函数(或者对于已存在的更改函数提供额外行为)来扩展这个语言的具体实现[implementation]必须记录这些扩展的使用如何与等价性测试和哈希表[hash table]搜索相互作用.

通过为哈希表[hash table]定义额外的可接受的等价性测试 (给 make-hash-table 的 :test 参数提供额外的值)来扩展这个语言的具体实现[implementation]必须记录这些测试的可见部分. 

## 18.2 <span id="TheHashTablesDictionary">哈希表的字典</span>

> * [系统类 HASH-TABLE](#SC-HASH-TABLE)
> * [函数 MAKE-HASH-TABLE](#F-MAKE-HASH-TABLE)
> * [函数 HASH-TABLE-P](#F-HASH-TABLE-P)
> * [函数 HASH-TABLE-COUNT](#F-HASH-TABLE-COUNT)
> * [函数 HASH-TABLE-REHASH-SIZE](#F-HASH-TABLE-REHASH-SIZE)
> * [函数 HASH-TABLE-REHASH-THRESHOLD](#F-HASH-TABLE-REHASH-THRESHOLD)
> * [函数 HASH-TABLE-SIZE](#F-HASH-TABLE-SIZE)
> * [函数 HASH-TABLE-TEST](#F-HASH-TABLE-TEST)
> * [访问器 GETHASH](#A-GETHASH)
> * [函数 REMHASH](#F-REMHASH)
> * [函数 MAPHASH](#F-MAPHASH)
> * [宏 WITH-HASH-TABLE-ITERATOR](#M-WITH-HASH-TABLE-ITERATOR)
> * [函数 CLRHASH](#F-CLRHASH)
> * [函数 SXHASH](#F-SXHASH)


### <span id="SC-HASH-TABLE">系统类 HASH-TABLE</span>

* 类优先级列表(Class Precedence List):

        hash-table, t

* 描述(Description):

        哈希表[hash table]提供了一个方式来映射任何对象[object] (一个键[key])到一个关联的对象[object] (一个值[value]).

* 参见(See Also):

        章节 18.1 (哈希表的概念), 章节 22.1.3.13 (打印其他对象)

* 注意(Notes):

        其目的是通过一个哈希散列机制实现这个映射, 就像 The Art of Computer Programming, Volume 3 (pp506-549) 的章节 6.4 "Hashing" 中描述的那样. 尽管有这样的意图, 符合规范的实现[conforming implementation]不需要去使用任何特定的技巧来实现这个映射. 


### <span id="F-MAKE-HASH-TABLE">函数 MAKE-HASH-TABLE</span>

* 语法(Syntax):

        make-hash-table &key test size rehash-size rehash-threshold => hash-table

* 参数和值(Arguments and Values):

        test---函数[function] eq, eql, equal, 或 equalp 其中之一的标识符[designator]. 默认是 eql.
        size---一个非负整数[integer]. 默认是依赖于具体实现的[implementation-dependent].
        rehash-size---一个 (or (integer 1 *) (float (1.0) *)) 类型[type]的实数[real]. 默认是依赖于具体实现的[implementation-dependent].
        rehash-threshold---一个 (real 0 1) 类型[type]的实数[real]. 默认是依赖于具体实现的[implementation-dependent].
        hash-table---一个哈希表[hash table].

* 描述(Description):

        创建并返回一个新的哈希表[hash table].

        test 决定了如何比较这些键[key]. 如果一个对象[object]在这个测试条件 test 下和这个哈希表 hash-table 中的某个条目的键[key]是相同的[same], 就说这个对象[object]出现在这个哈希表 hash-table 中.

        size 是一个对于具体实现[implementation]的暗示, 表示关于在这个哈希表 hash-table 中分配的初始空间的大小. 这个信息和 rehash-threshold 相联系来控制这个表增长之前应该可以插入的条目的合适数量. 实际的大小可能在 size 到下一个'好的'大小; 比如, 一些具体实现[implementation]可能会舍入到下一个质数.

        rehash-size 指定了当 hash-table 变得足够多以致于需要再散列时需要增长的最小数量; 见下面的 rehash-theshold. 如果 rehash-size 是一个整数[integer], 这个表的预期增长率是加法的并且这个整数[integer]就是要增加的条目数; 如果它是一个浮点数[float], 这个表的预期增长率是乘法的并且这个浮点数[float]就是新的大小和旧的大小的比值. 和 size 一样, 这个增长的实际大小可能会被舍入.

        rehash-threshold 指定了在 hash-table 必须增长前它可以的达到的完整大小. 它指定了最大期望的 hash-table 占用级别(occupancy level).

        这个 rehash-size 和 rehash-threshold 的值[value]不会强迫具体实现[implementation]使用任何特定的方法来计算哈希表 hash-table 何时要被扩展以及扩展的大小. 这样的决定是依赖于具体实现的[implementation-dependent], 这些值[value]只是程序员[programmer]对实现[implementation]的提示, 并且允许具体实现[implementation]去忽略它们.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 46142754>
    (setf (gethash "one" table) 1) =>  1
    (gethash "one" table) =>  NIL, false
    (setq table (make-hash-table :test 'equal)) =>  #<HASH-TABLE EQUAL 0/139 46145547>
    (setf (gethash "one" table) 1) =>  1
    (gethash "one" table) =>  1, T
    (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7) 
    =>  #<HASH-TABLE EQL 0/120 46156620>
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        gethash, hash-table

* 注意(Notes): None. 


### <span id="F-HASH-TABLE-P">函数 HASH-TABLE-P</span>

* 语法(Syntax):

        hash-table-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 hash-table 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32511220>
    (hash-table-p table) =>  true
    (hash-table-p 37) =>  false
    (hash-table-p '((a . 1) (b . 2))) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (hash-table-p object) ==  (typep object 'hash-table)


### <span id="F-HASH-TABLE-COUNT">函数 HASH-TABLE-COUNT</span>

* 语法(Syntax):

        hash-table-count hash-table => count

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].
        count---一个非负整数[integer].

* 描述(Description):

        返回在这个哈希表 hash-table 中的条目的数量. 如果 hash-table 刚刚被创建或者重新被清理 (见 clrhash) 那么条目数量就是 0.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115135>
    (hash-table-count table) =>  0
    (setf (gethash 57 table) "fifty-seven") =>  "fifty-seven"
    (hash-table-count table) =>  1
    (dotimes (i 100) (setf (gethash i table) i)) =>  NIL
    (hash-table-count table) =>  100
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        clrhash, remhash, gethash 的 setf

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        hash-table-size

* 注意(Notes):

        下面的关系在功能上是正确的, 尽管在实践中使用 hash-table-count 可能要快得多:

        (hash-table-count table) == 
        (loop for value being the hash-values of table count t) == 
        (let ((total 0))
          (maphash #'(lambda (key value)
                        (declare (ignore key value))
                        (incf total))
                    table)
          total)


### <span id="F-HASH-TABLE-REHASH-SIZE">函数 HASH-TABLE-REHASH-SIZE</span>

* 语法(Syntax):

        hash-table-rehash-size hash-table => rehash-size

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].
        rehash-size---一个 (or (integer 1 *) (float (1.0) *)) 类型[type]的实数[real].

* 描述(Description):

        返回哈希表 hash-table 的当前再散列大小, 适合用于 make-hash-table 的调用中来产生一个带有和 hash-table 当前状态对应的状态的哈希表[hash table].

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table :size 100 :rehash-size 1.4))
    =>  #<HASH-TABLE EQL 0/100 2556371>
    (hash-table-rehash-size table) =>  1.4
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 hash-table 不是一个哈希表[hash table]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-hash-table, hash-table-rehash-threshold

* 注意(Notes):

        如果这个哈希表被创建时带有一个表示再散列大小的整数[integer], 表示再散列时这个 hash-table 的增长率是加法的; 否则, 结果是一个浮点数[float], 表示再散列时这个 hash-table 的增长率是乘法的. 然而, 这个值只是给具体实现[implementation]的建议; 再散列时这个 hash-table 增长的实际数量是依赖于具体实现的[implementation-dependent]. 


### <span id="F-HASH-TABLE-REHASH-THRESHOLD">函数 HASH-TABLE-REHASH-THRESHOLD</span>

* 语法(Syntax):

        hash-table-rehash-threshold hash-table => rehash-threshold

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].
        rehash-threshold---一个 (real 0 1) 类型[type]的实数[real].

* 描述(Description):

        返回这个 hash-table 的当前再散列阈值, 它适合用于 make-hash-table 的调用中来产生一个带有和 hash-table 当前状态对应的状态的哈希表[hash table].

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table :size 100 :rehash-threshold 0.5))
    =>  #<HASH-TABLE EQL 0/100 2562446>
    (hash-table-rehash-threshold table) =>  0.5
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 hash-table 不是一个哈希表[hash table]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-hash-table, hash-table-rehash-size

* 注意(Notes): None. 


### <span id="F-HASH-TABLE-SIZE">函数 HASH-TABLE-SIZE</span>

* 语法(Syntax):

        hash-table-size hash-table => size

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].
        size---一个非负整数[integer].

* 描述(Description):

        返回这个 hash-table 的当前大小, 适合用于 make-hash-table 的调用中来产生一个带有和 hash-table 当前状态对应的状态的哈希表[hash table].

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 hash-table 不是一个哈希表[hash table]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        hash-table-count, make-hash-table

* 注意(Notes): None.


### <span id="F-HASH-TABLE-TEST">函数 HASH-TABLE-TEST</span>

* 语法(Syntax):

        hash-table-test hash-table => test

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].
        test---一个函数标识符[function designator]. 对于这四个标准化[standardlized]的哈希表[hash table]测试函数[function] (见 make-hash-table), 返回的这个 test 值总是为一个符号[symbol]. 如果一个具体实现[implementation]允许额外的测试条件, 那么这些测试条件是否作为函数[function]对象[object]或函数名字[function name]返回是依赖于具体实现的[implementation-dependent].

* 描述(Description):

        返回在这个 hash-table 中用来比较键[key]的测试条件.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 hash-table 不是一个哈希表[hash table]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-hash-table

* 注意(Notes): None. 


### <span id="A-GETHASH">访问器 GETHASH</span>

* 语法(Syntax):

        gethash key hash-table &optional default => value, present-p

        (setf (gethash key hash-table &optional default) new-value)

* 参数和值(Arguments and Values):

        key---一个对象[object].
        hash-table---一个哈希表[hash table].
        default---一个对象[object]. 默认是 nil.
        value---一个对象[object].
        present-p---一个广义 boolean [generalized boolean].

* 描述(Description):

        value 是一个在 hash-table 中键[key]在这个 hash-table 的等价性测试条件下和 key 相同[same]的对象[object]. 如果这里没有这样的条目, value 就是那个默认值 default.

        如果找到一个条目, 那么 present-p 就是 true; 否则, 它就是 false.

        setf 可以和 gethash 一起使用来修改和给定的键[key]关联的一个值[value], 或者去添加一个新的条目. 当一个 gethash 表达式形式[form]被用作一个 setf 位置时, 任何提供的默认值 default 都根据正常的从左到右的求值规则被求值, 但是它的值[value]会被忽略.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32206334>
    (gethash 1 table) =>  NIL, false
    (gethash 1 table 2) =>  2, false
    (setf (gethash 1 table) "one") =>  "one"
    (setf (gethash 2 table "two") "two") =>  "two"
    (gethash 1 table) =>  "one", true
    (gethash 2 table) =>  "two", true
    (gethash nil table) =>  NIL, false
    (setf (gethash nil table) nil) =>  NIL 
    (gethash nil table) =>  NIL, true
    (defvar *counters* (make-hash-table)) =>  *COUNTERS*
    (gethash 'foo *counters*) =>  NIL, false
    (gethash 'foo *counters* 0) =>  0, false
    (defmacro how-many (obj) `(values (gethash ,obj *counters* 0))) =>  HOW-MANY
    (defun count-it (obj) (incf (how-many obj))) =>  COUNT-IT
    (dolist (x '(bar foo foo bar bar baz)) (count-it x))
    (how-many 'foo) =>  2
    (how-many 'bar) =>  3
    (how-many 'quux) =>  0
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        remhash

* 注意(Notes):

        第二个值[secondary value], present-p, 可以被用于区分一个缺失的条目和一个有着默认值 default 的已存在条目. 


### <span id="F-REMHASH">函数 REMHASH</span>

* 语法(Syntax):

        remhash key hash-table => generalized-boolean

* 参数和值(Arguments and Values):

        key---一个对象[object].
        hash-table---一个哈希表[hash table].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        移除 hash-table 中 key 表示的条目, 如果存在的话. 如果这里有这样一个条目就返回 true, 否则返回 false.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115666>
    (setf (gethash 100 table) "C") =>  "C"
    (gethash 100 table) =>  "C", true
    (remhash 100 table) =>  true
    (gethash 100 table) =>  NIL, false
    (remhash 100 table) =>  false
    ```

* 副作用(Side Effects):

        这个 hash-table 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAPHASH">函数 MAPHASH</span>

* 语法(Syntax):

        maphash function hash-table => nil

* 参数和值(Arguments and Values):

        function---一个两参数[argument]函数[function]的标识符[designator], 这两个参数为那个键[key]和值[value].
        hash-table---一个哈希表[hash table].

* 描述(Description):

        在这个哈希表 hash-table 的所有条目上迭代. 对于每一个条目, 用两个参数[argument]调用这个函数 function--就是那个条目的键[key]和值[value].

        如果在一个 maphash 正在进行时尝试从 hash-table 移除或添加一个条目, 那么后果是未指定的, 其中有两个例外: 这个函数 function 可以使用 gethash 的 setf 来改变当前正在被处理的条目的值[value]部分, 或者它可以使用 remhash 来移除这个条目.

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32304110>
    (dotimes (i 10) (setf (gethash i table) i)) =>  NIL
    (let ((sum-of-squares 0))
        (maphash #'(lambda (key val) 
                    (let ((square (* val val)))
                      (incf sum-of-squares square)
                      (setf (gethash key table) square)))
                table)
        sum-of-squares) =>  285
    (hash-table-count table) =>  10
    (maphash #'(lambda (key val)
                  (when (oddp val) (remhash key table)))
              table) =>  NIL
    (hash-table-count table) =>  5
    (maphash #'(lambda (k v) (print (list k v))) table)
    (0 0) 
    (8 64) 
    (2 4) 
    (6 36) 
    (4 16) 
    =>  NIL
    ```

* 副作用(Side Effects):

        没有, 除了任何可能由函数 function 实现的副作用.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        loop, with-hash-table-iterator, 章节 3.6 (遍历规则和副作用)

* 注意(Notes): None. 


### <span id="M-WITH-HASH-TABLE-ITERATOR">宏 WITH-HASH-TABLE-ITERATOR</span>

* 语法(Syntax):

        with-hash-table-iterator (name hash-table) declaration* form* => result*

* 参数和值(Arguments and Values):

        name---一个适合用作给 macrolet 的第一个参数的名字.
        hash-table---一个表达式形式[form], 求值一次, 它应该产生一个哈希表[hash table].
        declaration---一个 declare 表达式[expression]; 不求值.
        forms---一个隐式的 progn [implicit progn].
        results---由表达式 forms 返回的值[value].

* 描述(Description):

        在这个主体的词法作用域中, name 通过 macrolet 被定义, 这样一来连续的对 (name) 的调用会一个接一个返回这个哈希表[hash table]中的项, 这个哈希表通过只求值一次 hash-table 来获取.

        一个 (name) 返回以下三个值:

        1. 一个广义 boolean [generalized boolean], 如果返回一个条目就是 true.
        2. 这个 hash-table 条目的键.
        3. 这个 hash-table 条目的值.

        在所有条目已经通过连续调用 (name) 返回后, 只有一个值会被返回, 也就是 nil.

        如果一个迭代的任何隐式的内部状态被返回到这个 with-hash-table-iterator 表达式形式[form]的动态范围以外, 比如通过从这个调用表达式形式[form]返回某个闭包[closure].

        with-hash-table-iterator 的任意数量的调用可以是嵌套的, 并且最里边的那个的主体可以调用所有这些局部建立的宏[macro], 假定所有这些宏[macro]都有着不同[distinct]的名字.

* 示例(Examples):

        以下函数应该在任何哈希表[hash table]上返回 t, 如果 with-hash-table-iterator 的使用和对应 maphash 的使用不一致, 那么应该发出一个错误.

    ```LISP
    (defun test-hash-table-iterator (hash-table)
      (let ((all-entries '())
            (generated-entries '())
            (unique (list nil)))
        (maphash #'(lambda (key value) (push (list key value) all-entries))
                  hash-table)
        (with-hash-table-iterator (generator-fn hash-table)
          (loop     
            (multiple-value-bind (more? key value) (generator-fn)
              (unless more? (return))
              (unless (eql value (gethash key hash-table unique))
                (error "Key ~S not found for value ~S" key value))
              (push (list key value) generated-entries))))
        (unless (= (length all-entries)
                    (length generated-entries)
                    (length (union all-entries generated-entries
                                  :key #'car :test (hash-table-test hash-table))))
          (error "Generated entries and Maphash entries don't correspond"))
        t))
    ```

        下面这个可以是 maphash 通过 with-hash-table-iterator 实现的可接受定义.

    ```LISP
    (defun maphash (function hash-table)
      (with-hash-table-iterator (next-entry hash-table)
        (loop (multiple-value-bind (more key value) (next-entry)
                (unless more (return nil))
                (funcall function key value)))))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这个由 with-hash-table-iterator 建立的名为 name 的局部函数在它已经返回 false 作为它的主值[primary value]之后被调用, 后果是未定义的.

* 参见(See Also):

        章节 3.6 (遍历规则和副作用)

* 注意(Notes): None. 


### <span id="F-CLRHASH">函数 CLRHASH</span>

* 语法(Syntax):

        clrhash hash-table => hash-table

* 参数和值(Arguments and Values):

        hash-table---一个哈希表[hash table].

* 描述(Description):

        从 hash-table 中移除所有条目, 然后返回一个空的哈希表[hash table].

* 示例(Examples):

    ```LISP
    (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32004073>
    (dotimes (i 100) (setf (gethash i table) (format nil "~R" i))) =>  NIL
    (hash-table-count table) =>  100
    (gethash 57 table) =>  "fifty-seven", true
    (clrhash table) =>  #<HASH-TABLE EQL 0/120 32004073>
    (hash-table-count table) =>  0
    (gethash 57 table) =>  NIL, false
    ```

* 副作用(Side Effects):

        这个 hash-table 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="F-SXHASH">函数 SXHASH</span>

* 语法(Syntax):

        sxhash object => hash-code

* 参数和值(Arguments and Values):

        object---一个对象[object].
        hash-code---一个非负 fixnum.

* 描述(Description):

        sxhash 返回对象 object 的一个哈希值.

        这个哈希值计算的方式是依赖于具体实现的[implementation-dependent], 但是受限于某些约束条件:

        1. (equal x y) 意味着 (= (sxhash x) (sxhash y)).

        2. 对于任意两个对象[object], x 和 y, 它们两个都是位向量[bit vector], 字符[character], cons, 数字[number], 路径名[pathname], 字符串[string], 或符号[symbol], 并且都是相似的[similar], 那么 (sxhash x) 和 (sxhash y) 产生[yield]相同的数学值, 即便 x 和 y 只存在于同一实现[implementation]的 Lisp 镜像[Lisp image]. 见章节 3.2.4 (编译后文件中的字面化对象).

        3. 对于一个对象[object]的 hash-code 在单个会话[session]中总是相同的[same], 假定对于等价性测试条件 equal 这个对象[object]没有被可见修改. 见章节 18.1.2 (修改哈希表的键).

        4. 这个 hash-code 用于散列. 这对符合标准的实现[conforming implementation]没有任何可验证的约束, 但其目的是一个实现应该作出善意的努力, 以生成在非负 fixnum 范围内分布良好的 hash-code.

        5. 这个 hash-code 必须终止, 即便这个对象 object 包含了环.

* 示例(Examples):

    ```LISP
    (= (sxhash (list 'list "ab")) (sxhash (list 'list "ab"))) =>  true
    (= (sxhash "a") (sxhash (make-string 1 :initial-element #\a))) =>  true
    (let ((r (make-random-state)))
      (= (sxhash r) (sxhash (make-random-state r))))
    =>  implementation-dependent
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        这个具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        很多常见的散列需要都是通过 make-hash-table 和在哈希表[hash table]上相关的函数满足的. sxhash 的目的是在预定义的抽象不充分的地方使用. 它的主要目的是让用户能够方便地实现比哈希表[hash table]提供的更复杂的散列范例.

        sxhash 返回的哈希值不需要和任何其他 Common Lisp 中的函数[function]使用的散列策略相关.

        对于类型[type] equal 的对象[object]用 eq 比较, 条目 3 要求这个 hash-code 基于这个对象标识的某个不变的特性. 另一个合法的实现技术是让 sxhash 为这些对象[object]分配(和缓存)一个随机的哈希值, 因为这里没有要求那个相似[similar]但是不是 eq 的对象有着相同的哈希值.

        虽然依据符号[symbol]的名字[name]和符号[symbol]可访问[accessible]的包[package]都为符号[symbol]定义了相似性[similarity], 条目 3 不允许使用包[package]信息来计算哈希值, 因为对一个符号的包状态的改变对于 equal 是不可见的. 

