
# 18 Hash Tables

> * 18.1 [哈希表的概念](#HashTableConcepts)
> * 18.2 [哈希表的字典](#TheHashTablesDictionary)

## 18.1 <span id="HashTableConcepts">哈希表的概念</span>

> * 18.1.1 [哈希表操作](#HashTableOperations)
> * 18.1.2 [修改哈希表的键](#ModifyingHashTableKeys)


### 18.1.1 <span id="HashTableOperations">哈希表操作</span>

下面这段列出了一些可以应用于哈希表的已定义的名字. 下面规则应用于哈希表.

-- 一个哈希表只能关联一个值和一个给定的键. 对于一个给定的键如果尝试去添加第二个值, 第二个值会替换第一个. 因此, 添加一个值到哈希表是一个破坏性操作; 这个哈希表会被修改.

-- 这里有四种哈希表: 键使用 eq 来比较的那些, 键使用 eql 来比较的那些, 键使用 equal 来比较的那些, 以及键使用 equalp 来比较的那些.

-- 哈希表由 make-hash-table 创建. gethash 被用于搜索一个键以及找到那个关联的值. 新的条目会使用 setf 和 gethash 来添加到哈希表中. remhash 被用于移除一个条目. 比如:

    ```LISP
    (setq a (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32536573>
    (setf (gethash 'color a) 'brown) =>  BROWN
    (setf (gethash 'name a) 'fred) =>  FRED
    (gethash 'color a) =>  BROWN, true
    (gethash 'name a) =>  FRED, true
    (gethash 'pointy a) =>  NIL, false
    ```

    在这个例子中, 符号 color 和 name 被用作键, 而符号 brown 和 fred 被用作关联的值. 这个哈希表中有两个值, 一个关联是从 color 到 brown, 而另一个关联是从 name 到 fred.

-- 一个键和一个值可以是任何对象.

-- 一个条目在哈希表中的存在性可以由 gethash 返回的第二个值来决定.

clrhash           hash-table-p     remhash  
gethash           make-hash-table  sxhash   
hash-table-count  maphash                   

Figure 18-1. Hash-table 已定义的名字 

### 18.1.2 <span id="ModifyingHashTableKeys">修改哈希表的键</span>

提供给 make-hash-table 的 :test 参数的函数指定了它创建的哈希表的 '等价性测试条件(equivalence test)'.

如果存在一组对象(或潜在对象), 在修改之前, 对象是等价的, 但在随后的情况下不再是等价的, 那么对象就会被一个等价性测试条件当作被'可见地修改'.

如果一个对象 O1 被用作哈希表 H 中的一个键并且被 H 的等价性测试条件当作被可见地修改, 那么如果 O1 或任何在这个等价性测试条件下等价于 O1 的对象 O2 (不管在这个修改之前或之后) 在后面对 H 的操作中被用作一个键, 后果是未指定的. 即便 O1 被可见地修改并且接着被再次修改来撤销这个可见的修改, 那么使用 O1 作为一个 key 的后果也是未指定的.

下面是哈希表必须支持的对于等价性测试条件可见的修改的规范. 这些修改是用成员的修改来描述的, 并且是递归地定义的. 这个对象的成员的可见修改是这个对象的可见修改.

> * 18.1.2.1 [关于 EQ 和 EQL 的对象可见修改](#VM-EQ-EQL)
> * 18.1.2.2 [关于 EQUAL 的对象可见修改](#VM-EQUAL)
> * 18.1.2.3 [关于 EQUALP 的对象可见修改](#VM-EQUALP)
> * 18.1.2.4 [通过语言扩展的可见修改](#VM-LanguageExtensions)


#### 18.1.2.1 <span id="VM-EQ-EQL">关于 EQ 和 EQL 的对象可见修改</span>

没有提供标准化的函数能够对于 eq 或 eql 可见地修改一个对象. 

#### 18.1.2.2 <span id="VM-EQUAL">关于 EQUAL 的对象可见修改</span>

As a consequence of the behavior for equal,<!--TODO 待翻译--> 对于对象的可见修改没有在这个章节中被显式提及的规则衍生自那些在章节 18.1.2.1 (Visible Modification of Objects with respect to EQ and EQL) 中提及的.

##### 18.1.2.2.1 关于 EQUAL 的 cons 可见修改

对一个 cons 的 car 或 cdr 的任何可见修改都被当作是对于 equal 可见的修改. 


##### 18.1.2.2.2 关于 EQUAL 的字符串和位向量的可见修改

对于一个 bit-vector 类型或 string 类型的向量, 对这个向量的一个有效元素或者对这个向量的长度(如果它实际上是可调整的或者有着一个填充指针)的任何可见修改都被认为是对于 equal 的可见修改. 


#### 18.1.2.3 <span id="VM-EQUALP">关于 EQUALP 的对象可见修改</span>

As a consequence of the behavior for equalp,<!--TODO 待翻译--> 对于对象的可见修改没有在这个章节中被显式提及的规则衍生自那些在章节 18.1.2.2 (Visible Modification of Objects with respect to EQUAL) 中提及的.

##### 18.1.2.3.1 关于 EQUALP 的结构体可见修改

对一个结构体的槽的任何可见修改都被当作是对于 equalp 可见的修改. 


##### 18.1.2.3.2 关于 EQUALP 的数组可见修改

在一个数组中, 对一个有效元素, 填充指针(如果这个数组可以或确实有一个的话), 或者规模(如果这个数组实际上是可调整的话)的任何可见修改都会被当作对于 equalp 可见的修改. 

##### 18.1.2.3.3 关于 EQUALP 的哈希表可见修改

在一个哈希表中, 对这个哈希表中的条目数量, 键, 或者和那些键关联的值的任何可见修改都会被当作对于 equalp 可见的修改.

注意, 对于这些键修改的可见性取决于这个哈希表的等价性测试条件, 而不是 equalp 的说明. 

#### 18.1.2.4 <span id="VM-LanguageExtensions">通过语言扩展的可见修改</span>

通过提供额外的设值方法函数(或者对于已存在的设值方法函数提供额外行为)来扩展这个语言的具体实现必须记录这些扩展的使用如何于等价性测试和哈希表搜索相互作用.

通过为哈希表定义额外的可接受的等价性测试条件(给 make-hash-table 的 :test 参数提供额外的值)来扩展这个语言的具体实现必须记录这些测试条件的可见部分. 

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

        哈希表提供了一个方式来映射任何对象(一个键)到一个关联的对象(一个值).

* 也见(See Also):

        章节 18.1 (Hash Table Concepts), 章节 22.1.3.13 (Printing Other Objects)

* 注意(Notes):

        其目的是通过一个哈希散列机制实现这个映射, 比如 The Art of Computer Programming, Volume 3 (pp506-549) 的章节 6.4 "Hashing" 中描述的. 尽管有这样的意图, 符合规范的实现不需要去使用任何特定的技巧来实现这个映射. 


### <span id="F-MAKE-HASH-TABLE">函数 MAKE-HASH-TABLE</span>

* 语法(Syntax):

make-hash-table &key test size rehash-size rehash-threshold => hash-table

* 参数和值(Arguments and Values):

test---a designator for one of the functions eq, eql, equal, or equalp. The default is eql.

size---a non-negative integer. The default is implementation-dependent.

rehash-size---a real of type (or (integer 1 *) (float (1.0) *)). The default is implementation-dependent.

rehash-threshold---a real of type (real 0 1). The default is implementation-dependent.

hash-table---a hash table.

* 描述(Description):

Creates and returns a new hash table.

test determines how keys are compared. An object is said to be present in the hash-table if that object is the same under the test as the key for some entry in the hash-table.

size is a hint to the implementation about how much initial space to allocate in the hash-table. This information, taken together with the rehash-threshold, controls the approximate number of entries which it should be possible to insert before the table has to grow. The actual size might be rounded up from size to the next `good' size; for example, some implementations might round to the next prime number.

rehash-size specifies a minimum amount to increase the size of the hash-table when it becomes full enough to require rehashing; see rehash-theshold below. If rehash-size is an integer, the expected growth rate for the table is additive and the integer is the number of entries to add; if it is a float, the expected growth rate for the table is multiplicative and the float is the ratio of the new size to the old size. As with size, the actual size of the increase might be rounded up.

rehash-threshold specifies how full the hash-table can get before it must grow. It specifies the maximum desired hash-table occupancy level.

The values of rehash-size and rehash-threshold do not constrain the implementation to use any particular method for computing when and by how much the size of hash-table should be enlarged. Such decisions are implementation-dependent, and these values only hints from the programmer to the implementation, and the implementation is permitted to ignore them.

* 示例(Examples):

 (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 46142754>
 (setf (gethash "one" table) 1) =>  1
 (gethash "one" table) =>  NIL, false
 (setq table (make-hash-table :test 'equal)) =>  #<HASH-TABLE EQUAL 0/139 46145547>
 (setf (gethash "one" table) 1) =>  1
 (gethash "one" table) =>  1, T
 (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7) 
=>  #<HASH-TABLE EQL 0/120 46156620>

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

gethash, hash-table

* 注意(Notes): None. 


### <span id="F-HASH-TABLE-P">函数 HASH-TABLE-P</span>

* 语法(Syntax):

hash-table-p object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type hash-table; otherwise, returns false.

* 示例(Examples):

 (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32511220>
 (hash-table-p table) =>  true
 (hash-table-p 37) =>  false
 (hash-table-p '((a . 1) (b . 2))) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

 (hash-table-p object) ==  (typep object 'hash-table)


### <span id="F-HASH-TABLE-COUNT">函数 HASH-TABLE-COUNT</span>

* 语法(Syntax):

hash-table-count hash-table => count

* 参数和值(Arguments and Values):

hash-table---a hash table.

count---a non-negative integer.

* 描述(Description):

Returns the number of entries in the hash-table. If hash-table has just been created or newly cleared (see clrhash) the entry count is 0.

* 示例(Examples):

 (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115135>
 (hash-table-count table) =>  0
 (setf (gethash 57 table) "fifty-seven") =>  "fifty-seven"
 (hash-table-count table) =>  1
 (dotimes (i 100) (setf (gethash i table) i)) =>  NIL
 (hash-table-count table) =>  100

* 副作用(Side Effects): None.

* 受此影响(Affected By):

clrhash, remhash, setf of gethash

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

hash-table-size

* 注意(Notes):

The following relationships are functionally correct, although in practice using hash-table-count is probably much faster:

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

hash-table---a hash table.

rehash-size---a real of type (or (integer 1 *) (float (1.0) *)).

* 描述(Description):

Returns the current rehash size of hash-table, suitable for use in a call to make-hash-table in order to produce a hash table with state corresponding to the current state of the hash-table.

* 示例(Examples):

 (setq table (make-hash-table :size 100 :rehash-size 1.4))
=>  #<HASH-TABLE EQL 0/100 2556371>
 (hash-table-rehash-size table) =>  1.4

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if hash-table is not a hash table.

* 也见(See Also):

make-hash-table, hash-table-rehash-threshold

* 注意(Notes):

If the hash table was created with an integer rehash size, the result is an integer, indicating that the rate of growth of the hash-table when rehashed is intended to be additive; otherwise, the result is a float, indicating that the rate of growth of the hash-table when rehashed is intended to be multiplicative. However, this value is only advice to the implementation; the actual amount by which the hash-table will grow upon rehash is implementation-dependent. 

### <span id="F-HASH-TABLE-REHASH-THRESHOLD">函数 HASH-TABLE-REHASH-THRESHOLD</span>

* 语法(Syntax):

hash-table-rehash-threshold hash-table => rehash-threshold

* 参数和值(Arguments and Values):

hash-table---a hash table.

rehash-threshold---a real of type (real 0 1).

* 描述(Description):

Returns the current rehash threshold of hash-table, which is suitable for use in a call to make-hash-table in order to produce a hash table with state corresponding to the current state of the hash-table.

* 示例(Examples):

 (setq table (make-hash-table :size 100 :rehash-threshold 0.5))
=>  #<HASH-TABLE EQL 0/100 2562446>
 (hash-table-rehash-threshold table) =>  0.5

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if hash-table is not a hash table.

* 也见(See Also):

make-hash-table, hash-table-rehash-size

* 注意(Notes): None. 

### <span id="F-HASH-TABLE-SIZE">函数 HASH-TABLE-SIZE</span>

* 语法(Syntax):

hash-table-size hash-table => size

* 参数和值(Arguments and Values):

hash-table---a hash table.

size---a non-negative integer.

* 描述(Description):

Returns the current size of hash-table, which is suitable for use in a call to make-hash-table in order to produce a hash table with state corresponding to the current state of the hash-table.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if hash-table is not a hash table.

* 也见(See Also):

hash-table-count, make-hash-table

* 注意(Notes): None.

### <span id="F-HASH-TABLE-TEST">函数 HASH-TABLE-TEST</span>

* 语法(Syntax):

hash-table-test hash-table => test

* 参数和值(Arguments and Values):

hash-table---a hash table.

test---a function designator. For the four standardized hash table test functions (see make-hash-table), the test value returned is always a symbol. If an implementation permits additional tests, it is implementation-dependent whether such tests are returned as function objects or function names.

* 描述(Description):

Returns the test used for comparing keys in hash-table.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if hash-table is not a hash table.

* 也见(See Also):

make-hash-table

* 注意(Notes): None. 


### <span id="A-GETHASH">访问器 GETHASH</span>

* 语法(Syntax):

gethash key hash-table &optional default => value, present-p

(setf (gethash key hash-table &optional default) new-value)

* 参数和值(Arguments and Values):

key---an object.

hash-table---a hash table.

default---an object. The default is nil.

value---an object.

present-p---a generalized boolean.

* 描述(Description):

Value is the object in hash-table whose key is the same as key under the hash-table's equivalence test. If there is no such entry, value is the default.

Present-p is true if an entry is found; otherwise, it is false.

setf may be used with gethash to modify the value associated with a given key, or to add a new entry. When a gethash form is used as a setf place, any default which is supplied is evaluated according to normal left-to-right evaluation rules, but its value is ignored.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

remhash

* 注意(Notes):

The secondary value, present-p, can be used to distinguish the absence of an entry from the presence of an entry that has a value of default. 


### <span id="F-REMHASH">函数 REMHASH</span>

* 语法(Syntax):

remhash key hash-table => generalized-boolean

* 参数和值(Arguments and Values):

key---an object.

hash-table---a hash table.

generalized-boolean---a generalized boolean.

* 描述(Description):

Removes the entry for key in hash-table, if any. Returns true if there was such an entry, or false otherwise.

* 示例(Examples):

 (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115666>
 (setf (gethash 100 table) "C") =>  "C"
 (gethash 100 table) =>  "C", true
 (remhash 100 table) =>  true
 (gethash 100 table) =>  NIL, false
 (remhash 100 table) =>  false

* 副作用(Side Effects):

The hash-table is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAPHASH">函数 MAPHASH</span>

* 语法(Syntax):

maphash function hash-table => nil

* 参数和值(Arguments and Values):

function---a designator for a function of two arguments, the key and the value.

hash-table---a hash table.

* 描述(Description):

Iterates over all entries in the hash-table. For each entry, the function is called with two arguments--the key and the value of that entry.

The consequences are unspecified if any attempt is made to add or remove an entry from the hash-table while a maphash is in progress, with two exceptions: the function can use can use setf of gethash to change the value part of the entry currently being processed, or it can use remhash to remove that entry.

* 示例(Examples):

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

* 副作用(Side Effects):

None, other than any which might be done by the function.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

loop, with-hash-table-iterator, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="M-WITH-HASH-TABLE-ITERATOR">宏 WITH-HASH-TABLE-ITERATOR</span>

* 语法(Syntax):

with-hash-table-iterator (name hash-table) declaration* form* => result*

* 参数和值(Arguments and Values):

name---a name suitable for the first argument to macrolet.

hash-table---a form, evaluated once, that should produce a hash table.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by forms.

* 描述(Description):

Within the lexical scope of the body, name is defined via macrolet such that successive invocations of (name) return the items, one by one, from the hash table that is obtained by evaluating hash-table only once.

An invocation (name) returns three values as follows:

1. A generalized boolean that is true if an entry is returned.
2. The key from the hash-table entry.
3. The value from the hash-table entry.

After all entries have been returned by successive invocations of (name), then only one value is returned, namely nil.

It is unspecified what happens if any of the implicit interior state of an iteration is returned outside the dynamic extent of the with-hash-table-iterator form such as by returning some closure over the invocation form.

Any number of invocations of with-hash-table-iterator can be nested, and the body of the innermost one can invoke all of the locally established macros, provided all of those macros have distinct names.

* 示例(Examples):

The following function should return t on any hash table, and signal an error if the usage of with-hash-table-iterator does not agree with the corresponding usage of maphash.

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

The following could be an acceptable definition of maphash, implemented by with-hash-table-iterator.

 (defun maphash (function hash-table)
   (with-hash-table-iterator (next-entry hash-table)
     (loop (multiple-value-bind (more key value) (next-entry)
             (unless more (return nil))
             (funcall function key value)))))

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The consequences are undefined if the local function named name established by with-hash-table-iterator is called after it has returned false as its primary value.

* 也见(See Also):

Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-CLRHASH">函数 CLRHASH</span>

* 语法(Syntax):

clrhash hash-table => hash-table

* 参数和值(Arguments and Values):

hash-table---a hash table.

* 描述(Description):

Removes all entries from hash-table, and then returns that empty hash table.

* 示例(Examples):

 (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32004073>
 (dotimes (i 100) (setf (gethash i table) (format nil "~R" i))) =>  NIL
 (hash-table-count table) =>  100
 (gethash 57 table) =>  "fifty-seven", true
 (clrhash table) =>  #<HASH-TABLE EQL 0/120 32004073>
 (hash-table-count table) =>  0
 (gethash 57 table) =>  NIL, false

* 副作用(Side Effects):

The hash-table is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-SXHASH">函数 SXHASH</span>

* 语法(Syntax):

sxhash object => hash-code

* 参数和值(Arguments and Values):

object---an object.

hash-code---a non-negative fixnum.

* 描述(Description):

sxhash returns a hash code for object.

The manner in which the hash code is computed is implementation-dependent, but subject to certain constraints:

1. (equal x y) implies (= (sxhash x) (sxhash y)).

2. For any two objects, x and y, both of which are bit vectors, characters, conses, numbers, pathnames, strings, or symbols, and which are similar, (sxhash x) and (sxhash y) yield the same mathematical value even if x and y exist in different Lisp images of the same implementation. See Section 3.2.4 (Literal Objects in Compiled Files).

3. The hash-code for an object is always the same within a single session provided that the object is not visibly modified with regard to the equivalence test equal. See Section 18.1.2 (Modifying Hash Table Keys).

4. The hash-code is intended for hashing. This places no verifiable constraint on a conforming implementation, but the intent is that an implementation should make a good-faith effort to produce hash-codes that are well distributed within the range of non-negative fixnums.

5. Computation of the hash-code must terminate, even if the object contains circularities.

* 示例(Examples):

 (= (sxhash (list 'list "ab")) (sxhash (list 'list "ab"))) =>  true
 (= (sxhash "a") (sxhash (make-string 1 :initial-element #\a))) =>  true
 (let ((r (make-random-state)))
   (= (sxhash r) (sxhash (make-random-state r))))
=>  implementation-dependent

* 副作用(Side Effects): None.

* 受此影响(Affected By):

The implementation.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

Many common hashing needs are satisfied by make-hash-table and the related functions on hash tables. sxhash is intended for use where the pre-defined abstractions are insufficient. Its main intent is to allow the user a convenient means of implementing more complicated hashing paradigms than are provided through hash tables.

The hash codes returned by sxhash are not necessarily related to any hashing strategy used by any other function in Common Lisp.

For objects of types that equal compares with eq, item 3 requires that the hash-code be based on some immutable quality of the identity of the object. Another legitimate implementation technique would be to have sxhash assign (and cache) a random hash code for these objects, since there is no requirement that similar but non-eq objects have the same hash code.

Although similarity is defined for symbols in terms of both the symbol's name and the packages in which the symbol is accessible, item 3 disallows using package information to compute the hash code, since changes to the package status of a symbol are not visible to equal. 


