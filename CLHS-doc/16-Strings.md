# 16. 字符串

> * 16.1 [字符串的概念](#StringConcepts)
> * 16.2 [字符串的字典](#TheStringsDictionary)

## 16.1 <span id="StringConcepts">字符串的概念</span>

### 16.1.1 字符串作为数组的含义

因为所有字符串[string]是数组[array], 所有可应用于数组[array]的规则也可以应用于字符串[string]. 见章节 15.1 (数组的概念).

比如, 字符串[string]可以有填充指针[fill pointer], 并且字符串[string]也服从应用于数组的元素类型[element type]提升[upgrade]的规则. 

### 16.1.2 STRING 的子类型

所有在字符串[string]上操作的函数也可以在字符串[string]的子类型[subtype]上操作.

然而, 如果一个字符[character]被插入到一个字符串[string]中而这个字符串[string]的元素类型[element type]不包括那个字符[character], 那么后果是未定义的. 

### 16.2 <span id="TheStringsDictionary">字符串的字典</span>

> * [系统类 STRING](#SC-STRING)
> * [类型 BASE-STRING](#T-BASE-STRING)
> * [类型 SIMPLE-STRING](#T-SIMPLE-STRING)
> * [类型 SIMPLE-BASE-STRING](#T-SIMPLE-BASE-STRING)
> * [函数 SIMPLE-STRING-P](#F-SIMPLE-STRING-P)
> * [访问器 CHAR, SCHAR](#A-CHAR-SCHAR)
> * [函数 STRING](#F-STRING)
> * [函数 STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE, NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE](#F-STRING-CASE)
> * [函数 STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM](#F-STRING-TRIM)
> * [函数 STRING=, STRING/=, STRING<, STRING>, STRING<=, STRING>=, STRING-EQUAL, STRING-NOT-EQUAL, STRING-LESSP, STRING-GREATERP, STRING-NOT-GREATERP, STRING-NOT-LESSP](#F-STRING-COMPARE)
> * [函数 STRINGP](#F-STRINGP)
> * [函数 MAKE-STRING](#F-MAKE-STRING)


### <span id="SC-STRING">系统类 STRING</span>

* 类优先级列表(Class Precedence List):

        string, vector, array, sequence, t

* 描述(Description):

        一个字符串[string]是一个元素[element]为类型[type] character 或 character 的一个子类型[subtype]的一个特化[specialized]向量[vector]. 当被用作对象创建时的类型指定符[type specifier]时, string 意味着 (vector character).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        对于所有 character 的子类型[subtype] c, 这个表示所有 (array c (size)) 类型[type]的并集; 这也就是说, 大小为 size 的字符串[string]的集合.

* 也见(See Also):

        章节 16.1 (字符串的概念), 章节 2.4.5 (双引号), 章节 22.1.3.4 (打印字符串) 


### <span id="T-BASE-STRING">类型 BASE-STRING</span>

* 超类型(Supertypes):

        base-string, string, vector, array, sequence, t

* 描述(Description):

        类型[type] base-string 等价于 (vector base-char). 这个基本字符串[base string]表示是可以持有一个标准字符[standard character]的任意序列的最有效字符串[string]表示.

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        base-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个等价于类型 (vector base-char size); 换言之, 就是大小为 size 的基本字符串[base string的集合. 


### <span id="T-SIMPLE-STRING">类型 SIMPLE-STRING</span>

* 超类型(Supertypes):

        simple-string, string, vector, simple-array, array, sequence, t

* 描述(Description):

        一个简单字符串[simple string]是元素为 character 类型[type]或 character 的子类型[subtype]的一个特化的一维简单数组[simple array]. 当被用作对象创建时的类型指定符[type specifier]时, simple-string 意味着 (simple-array character (size)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        对于 character 的子类型[subtype] c, 这个表示所有 (simple-array c (size)) 类型[type]的并集; 换言之, 大小为 size 的简单字符串[simple string]的集合.


### <span id="T-SIMPLE-BASE-STRING">类型 SIMPLE-BASE-STRING</span>

* 超类型(Supertypes):

        simple-base-string, base-string, simple-string, string, vector, simple-array, array, sequence, t

* 描述(Description):

        类型[type] simple-base-string 等价于 (simple-array base-char (*)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-base-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个等价于类型 (simple-array base-char (size)); 换言之, 大小为 size 的简单[simple]基本字符串[base string]的集合. 


### <span id="F-SIMPLE-STRING-P">函数 SIMPLE-STRING-P</span>

* 语法(Syntax):

        simple-string-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 simple-string 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (simple-string-p "aaaaaa") =>  true
    (simple-string-p (make-array 6 
                                  :element-type 'character 
                                  :fill-pointer t)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (simple-string-p object) ==  (typep object 'simple-string)


### <span id="A-CHAR-SCHAR">访问器 CHAR, SCHAR</span>

* 语法(Syntax):

        char string index => character

        schar string index => character

        (setf (char string index) new-character)

        (setf (schar string index) new-character)

* 参数和值(Arguments and Values):

        string---对于 char, 一个字符串[string]; 对于 schar, 一个简单字符串[simple string].
        index---字符串 string 的一个有效数组索引[valid array index].
        character, new-character---一个字符[character].

* 描述(Description):

        char 和 schar 访问[access]由索引 index 指定的字符串 string 中的元素[element].

        当访问[access]元素[element]时, char 忽略填充指针[fill pointer].

* 示例(Examples):

    ```LISP
    (setq my-simple-string (make-string 6 :initial-element #\A)) =>  "AAAAAA"
    (schar my-simple-string 4) =>  #\A
    (setf (schar my-simple-string 4) #\B) =>  #\B
    my-simple-string =>  "AAAABA"
    (setq my-filled-string
          (make-array 6 :element-type 'character
                        :fill-pointer 5
                        :initial-contents my-simple-string))
    =>  "AAAAB"
    (char my-filled-string 4) =>  #\B
    (char my-filled-string 5) =>  #\A
    (setf (char my-filled-string 3) #\C) =>  #\C
    (setf (char my-filled-string 5) #\D) =>  #\D
    (setf (fill-pointer my-filled-string) 6) =>  6
    my-filled-string =>  "AAACBD"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        aref, elt, 章节 3.2.1 (编译器术语)

* 注意(Notes):

        (char s j) ==  (aref (the string s) j)


### <span id="F-STRING">函数 STRING</span>

* 语法(Syntax):

        string x => string

* 参数和值(Arguments and Values):

        x---一个字符串[string], 一个符号[symbol], 或者一个字符[character].
        string---一个字符串[string].

* 描述(Description):

        返回一个由 x 描述的字符串[string]; 具体来说:

            如果 x 是一个字符串[string], 就返回它.
            如果 x 是一个符号[symbol], 返回它的名字[name].
            如果 x 是一个字符[character], 那么返回包含这样一个字符[character]的字符串[string].
            string 可能执行额外的, 具体实现定义的[implementation-defined]转换.

* 示例(Examples):

    ```LISP
    (string "already a string") =>  "already a string"
    (string 'elm) =>  "ELM"
    (string #\c) =>  "c"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        在一个转换既没有被这个规范也没有被具体实现[implementation]所定义的情况下, 就会发出一个 type-error 类型[type]的错误.

* 也见(See Also):

        coerce, string (类型).

* 注意(Notes):

        coerce 可以被用于去转换一个字符[character]的序列[sequence]为一个字符串[string].

        prin1-to-string, princ-to-string, write-to-string, 或 format (带有一个 nil 的第一个参数) 可以被用于获取一个数字[number]或其他任何对象[object]的字符串[string]表示. 


### <span id="F-STRING-CASE">函数 STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE, NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE</span>

* 语法(Syntax):

        string-upcase string &key start end => cased-string

        string-downcase string &key start end => cased-string

        string-capitalize string &key start end => cased-string

        nstring-upcase string &key start end => string

        nstring-downcase string &key start end => string

        nstring-capitalize string &key start end => string

* 参数和值(Arguments and Values):

        string---一个字符串标识符[string designator]. 对于 nstring-upcase, nstring-downcase, 和 nstring-capitalize, 这个字符串标识符[string designator]必须是一个字符串[string].
        start, end---字符串 string 的边界索引标识符[bounding index designator]. 对于 start 和 end 默认分别是 0 和 nil.
        cased-string---一个字符串[string].

* 描述(Description):

        string-upcase, string-downcase, string-capitalize, nstring-upcase, nstring-downcase, nstring-capitalize 按照如下改变字符串 string 由 start 和 end 限定[bounded]的子序列的大小写:

        string-upcase

            string-upcase 返回一个和字符串 string 相似的字符串[string], 其中所有小写字符被对应大写字符替换. 更确切地说, 结果字符串[string]中的每个字符都通过应用 char-upcase 函数[function]到字符串 string 的对应字符上产生的.

        string-downcase

            string-downcase 类似于 string-upcase 除了所有大写字符被对应小写字符替换(使用 char-downcase).

        string-capitalize

            string-capitalize 产生一个字符串 string 的一个拷贝, 这样一来, 对于这个拷贝中的每一个词, 这个 "词" 的第一个字符[character], 如果有大小写[case], 就变为大写[uppercase]并且在这个词中的其他任何带有大小写[case]的字符[character]变为小写[lowercase]. 对于 string-capitalize 的目的, 一个"单词"被定义为一个连续的子序列, 由字母数字[alphanumeric]字符[character]组成, 每一端都用非字母数字[alphanumeric]字符[character]或字符串[string]的结尾分隔.

        nstring-upcase, nstring-downcase, nstring-capitalize

            nstring-upcase, nstring-downcase, 和 nstring-capitalize 分别等价于 string-upcase, string-downcase, 和 string-capitalize 除了它们修改字符串 string.

        对于 string-upcase, string-downcase, 和 string-capitalize, 字符串 string 不会被修改. 然而, 如果在字符串 string 中没有字符需要转换, 那么结果可能就是那个字符串 string 或者它的一个拷贝, 由实现判定.

* 示例(Examples):

    ```LISP
    (string-upcase "abcde") =>  "ABCDE"
    (string-upcase "Dr. Livingston, I presume?")
    =>  "DR. LIVINGSTON, I PRESUME?"
    (string-upcase "Dr. Livingston, I presume?" :start 6 :end 10)
    =>  "Dr. LiVINGston, I presume?"
    (string-downcase "Dr. Livingston, I presume?")
    =>  "dr. livingston, i presume?"

    (string-capitalize "elm 13c arthur;fig don't") =>  "Elm 13c Arthur;Fig Don'T"
    (string-capitalize " hello ") =>  " Hello "
    (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
    =>   "Occluded Casements Forestall Inadvertent Defenestration"
    (string-capitalize 'kludgy-hash-search) =>  "Kludgy-Hash-Search"
    (string-capitalize "DON'T!") =>  "Don'T!"    ;not "Don't!"
    (string-capitalize "pipe 13a, foo16c") =>  "Pipe 13a, Foo16c"

    (setq str (copy-seq "0123ABCD890a")) =>  "0123ABCD890a"
    (nstring-downcase str :start 5 :end 7) =>  "0123AbcD890a"
    str =>  "0123AbcD890a"
    ```

* 副作用(Side Effects):

        nstring-upcase, nstring-downcase, 和 nstring-capitalize 适当地修改字符串 string 而不是构建一个新的字符串[string].

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        char-upcase, char-downcase

* 注意(Notes):

        结果总是和字符串 string 有着相同长度. 


### <span id="F-STRING-TRIM">函数 STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM</span>

* 语法(Syntax):

        string-trim character-bag string => trimmed-string

        string-left-trim character-bag string => trimmed-string

        string-right-trim character-bag string => trimmed-string

* 参数和值(Arguments and Values):

        character-bag---一个包含字符[character]的序列[sequence].
        string---一个字符串标识符[string designator].
        trimmed-string---一个字符串[string].

* 描述(Description):

        string-trim 返回字符串 string 的一个子字符串, 带有在 character-bag 中的所有字符, 除去开始和结尾. string-left-trim 也相似, 除了只脱去开头的字符; string-right-trim 只脱去结尾的字符.

        如果没有字符[character]需要从字符串 string 中被削减, 那么返回字符串 string 自身或者它的一个拷贝, 由这个实现来判定.

        所有这些函数[function]都会注意到这个填充指针[fill pointer].

* 示例(Examples):

    ```LISP
    (string-trim "abc" "abcaakaaakabcaaa") =>  "kaaak"
    (string-trim '(#\Space #\Tab #\Newline) " garbanzo beans
            ") =>  "garbanzo beans"
    (string-trim " (*)" " ( *three (silly) words* ) ")
    =>  "three (silly) words"

    (string-left-trim "abc" "labcabcabc") =>  "labcabcabc"
    (string-left-trim " (*)" " ( *three (silly) words* ) ")
    =>  "three (silly) words* ) "

    (string-right-trim " (*)" " ( *three (silly) words* ) ") 
    =>  " ( *three (silly) words"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        这个具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-STRING-COMPARE">函数 STRING=, STRING/=, STRING<, STRING>, STRING<=, STRING>=, STRING-EQUAL, STRING-NOT-EQUAL, STRING-LESSP, STRING-GREATERP, STRING-NOT-GREATERP, STRING-NOT-LESSP</span>

* 语法(Syntax):

        string= string1 string2 &key start1 end1 start2 end2 => generalized-boolean

        string/= string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string< string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string> string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string<= string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string>= string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string-equal string1 string2 &key start1 end1 start2 end2 => generalized-boolean

        string-not-equal string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string-lessp string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string-greaterp string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string-not-greaterp string1 string2 &key start1 end1 start2 end2 => mismatch-index

        string-not-lessp string1 string2 &key start1 end1 start2 end2 => mismatch-index

* 参数和值(Arguments and Values):

        string1---一个字符串标识符[string designator].
        string2---一个字符串标识符[string designator].
        start1, end1---字符串 string1 的边界索引标识符[bounding index designator]. 对于 start 和 end 默认分别为 0 和 nil.
        start2, end2---字符串 string2 的边界索引标识符[bounding index designator]. 对于 start 和 end 默认分别为 0 和 nil.
        generalized-boolean---一个广义 boolean [generalized boolean].
        mismatch-index---字符串 string1 的一个边界索引[bounding index], 或者 nil.

* 描述(Description):

        这些函数 functions 在字符串 string1 和 string2 上执行字典式的比较. string= 和 string-equal 被称为等价性函数; 其他的称为不等性函数. 这些函数[function]执行的比较操作被限定为在字符串 string1 由 start1 和 end1 限制的子序列以及字符串 string2 由 start2 和 end2 限制的子序列.

        如果一个字符串 a 和一个字符串 b 有着相同数量的字符, 并且对应字符在 char= 或 char-equal 下是相同的[same], 那么就说这两个字符串是相等的.

        如果在一个字符串 a 和一个字符串 b 有区别的第一个位置中, 根据 char< 或 char-lessp, a 的字符小于 b 中的对应字符, 或者如果字符串 a 是字符串 b 的一个前缀(更短的长度并且匹配 a 中的所有字符), 那么字符串 a 小于字符串 b.

        如果这些字符串是相等的, 那么这些相等函数就返回一个 true 的广义 boolean, 否则就是 false.

        如果这些字符串不相等, 那么这些不相等的函数返回一个为 true 的不匹配索引 mismatch-index, 否则就是 false. 当这个不匹配索引 mismatch-index 是 true, 它是一个表示在两个子字符串中第一个不同的字符位置的整数[integer], 按照从 string1 的起始点开始的偏移位.

        这个比较有着以下这些结果的其中之一:

        string=

            如果提供的子字符串是相同长度并且在对应位置的字符是相同的[same], 那么 string= 返回 true; 否则它就是 false.

        string/=

            如果提供的子字符串是不同的, 那么 string/= 就是 true; 否则它就是 false.

        string-equal

            string-equal 类似于 string= 除了在大小写上的区别会被忽略; 如果 char-equal 对于两个字符是 true, 那么它们就被认为是相同的.

        string<

            如果 substring1 小于 substring2, 那么 string< 就是 true; 否则它就是 false.

        string>

            如果 substring1 大于 substring2, 那么 string> 就是 true; 否则它就是 false.

        string-lessp, string-greaterp

            string-lessp 和 string-greaterp 分别类似于 string< 和 string>, 除了忽略大小写字母之间的区别. 就好像是使用 char-lessp 而不是 char< 来比较字符.

        string<=

            如果 substring1 小于等于 substring2, 那么 string<= 就是 true; 否则它就是 false.

        string>=

            如果 substring1 大于等于 substring2, 那么 string>= 就是 true; 否则它就是 false.

        string-not-greaterp, string-not-lessp

            string-not-greaterp 和 string-not-lessp 分别类似于 string<= 和 string>=, 除了忽略字母大小写之间的区别. 就好像是使用 char-lessp 而不是 char< 来比较字符.

* 示例(Examples):

    ```LISP
    (string= "foo" "foo") =>  true
    (string= "foo" "Foo") =>  false
    (string= "foo" "bar") =>  false
    (string= "together" "frog" :start1 1 :end1 3 :start2 2) =>  true
    (string-equal "foo" "Foo") =>  true
    (string= "abcd" "01234abcd9012" :start2 5 :end2 9) =>  true
    (string< "aaaa" "aaab") =>  3
    (string>= "aaaaa" "aaaa") =>  4
    (string-not-greaterp "Abcde" "abcdE") =>  5
    (string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7
                                          :start2 2 :end2 6) =>  6
    (string-not-equal "AAAA" "aaaA") =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        char=

* 注意(Notes):

        如果 equal 应用于两个字符串[string], 那么 equal 调用 string=. 


### <span id="F-STRINGP">函数 STRINGP</span>

* 语法(Syntax):

        stringp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 string 类型[type]就返回 true; 发在, 返回 false.

* 示例(Examples):

    ```LISP
    (stringp "aaaaaa") =>  true
    (stringp #\a) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        typep, string (类型[type])

* 注意(Notes):

        (stringp object) ==  (typep object 'string)


### <span id="F-MAKE-STRING">函数 MAKE-STRING</span>

* 语法(Syntax):

        make-string size &key initial-element element-type => string

* 参数和值(Arguments and Values):

        size---一个有效数组维数[valid array dimension].
        initial-element---一个字符[character]. 默认是依赖于具体实现的[implementation-dependent].
        element-type---一个类型指定符[type specifier]. 默认是 character.
        string---一个简单字符串[simple string].

* 描述(Description):

        make-string 返回一个长度为 size 并且元素被初始化为 initial-element 的简单字符串[simple string].

        这个元素类型 element-type 命名这个字符串[string]中元素[element]的类型[type]; 一个字符串[string]由可以容纳给定类型[type]元素[element]的最具体[specialized]类型[type]构成.

* 示例(Examples):

    ```LISP
    (make-string 10 :initial-element #\5) =>  "5555555555"
    (length (make-string 10)) =>  10
    ```

* 受此影响(Affected By):

        这个具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 

