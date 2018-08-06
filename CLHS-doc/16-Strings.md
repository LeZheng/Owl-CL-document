# 16 字符串

> * 16.1 [字符串的概念](#StringConcepts)
> * 16.2 [字符串的字典](#TheStringsDictionary)

## 16.1 <span id="StringConcepts">字符串的概念</span>

### 16.1.1 字符串数组的含义

因为所有字符串是数组, 所有可应用于数组的规则也可以应用于字符串. 见章节 15.1 (Array Concepts).

比如, 字符串可以有填充指针, 并且字符串也服从应用于数组的元素类型提升的规则. 

### 16.1.2 STRING 的子类型

所有在字符串上操作的函数也可以在字符串的子类型上操作.

然而, 如果一个字符被插入到一个字符串中而这个字符串的元素类型不包括那个字符, 那么后果是未定义的. 

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

        一个字符串是一个元素为类型 character 或 character 的一个子类型的一个特化向量. 当被用作对象创建时的类型指定符时, string 意味着 (vector character).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号 *.

* 复合类型指定符描述(Compound Type Specifier Description):

        对于所有 character 的子类型 c, 这个表示所有 (array c (size)) 类型的并集; 这也就是说, 大小为 size 的字符串的集合.

* 也见(See Also):

        章节 16.1 (String Concepts), 章节 2.4.5 (Double-Quote), 章节 22.1.3.4 (Printing Strings) 


### <span id="T-BASE-STRING">类型 BASE-STRING</span>

* 超类型(Supertypes):

        base-string, string, vector, array, sequence, t

* 描述(Description):

        类型 base-string 等价于 (vector base-char). 这个基本字符串表示是可以持有一个标准字符的任意序列的最有效字符串表示.

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        base-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号 *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个等价于类型 (vector base-char size); 这也就是说, 大小为 size 的基本字符串的集合. 


### <span id="T-SIMPLE-STRING">类型 SIMPLE-STRING</span>

* 超类型(Supertypes):

        simple-string, string, vector, simple-array, array, sequence, t

* 描述(Description):

        一个简单字符串是一个特化的元素为 character 类型或 character 的子类型的一维简单数组. 当被用作对象创建时的类型指定符时, simple-string 意味着 (simple-array character (size)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号 *.

* 复合类型指定符描述(Compound Type Specifier Description):

        对于 character 的子类型 c, 这个表示所有 (simple-array c (size)) 类型的并集; 这也就是说, 大小为 size 的简单字符串的集合.


### <span id="T-SIMPLE-BASE-STRING">类型 SIMPLE-BASE-STRING</span>

* 超类型(Supertypes):

        simple-base-string, base-string, simple-string, string, vector, simple-array, array, sequence, t

* 描述(Description):

        类型 simple-base-string 等价于 (simple-array base-char (*)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-base-string [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号 *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个等价于类型 (simple-array base-char (size)); 这也就是说, 大小为 size 的简单基本字符串的集合. 

### <span id="F-SIMPLE-STRING-P">函数 SIMPLE-STRING-P</span>

* 语法(Syntax):

        simple-string-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 simple-string 类型就返回 true; 发在, 返回 false.

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

        string---对于 char, 一个字符串; 对于 schar, 一个简单字符串.
        index---对于字符串 string 的一个有效数组索引.
        character, new-character---一个字符.

* 描述(Description):

        char 和 schar 访问由索引 index 指定的字符串 string 中的元素.

        当访问元素时 char 忽略填充指针.

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

        aref, elt, 章节 3.2.1 (Compiler Terminology)

* 注意(Notes):

        (char s j) ==  (aref (the string s) j)


### <span id="F-STRING">函数 STRING</span>

* 语法(Syntax):

        string x => string

* 参数和值(Arguments and Values):

        x---一个字符串, 一个符号, 或者一个字符.
        string---一个字符串.

* 描述(Description):

        返回一个由 x 描述的字符串; 具体来说:

            如果 x 是一个字符串, 就返回它.
            如果 x 是一个符号, 返回它的名字.
            如果 x 是一个字符, 那么包含这样一个字符的字符串会被返回.
            string 可能执行额外的, 具体实现定义的转换.

* 示例(Examples):

    ```LISP
    (string "already a string") =>  "already a string"
    (string 'elm) =>  "ELM"
    (string #\c) =>  "c"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        在一个转换既没有被这个规范也没有被具体实现所定义的情况下, 就会发出一个 type-error 的错误.

* 也见(See Also):

        coerce, string (type).

* 注意(Notes):

        coerce 可以被用于去转换一个字符的序列为一个字符串.

        prin1-to-string, princ-to-string, write-to-string, 或 format (带有一个 nil 的第一个参数) 可以被用于获取一个数字或其他任何对象的字符串表示. 


### <span id="F-STRING-CASE">函数 STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE, NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE</span>

* 语法(Syntax):

string-upcase string &key start end => cased-string

string-downcase string &key start end => cased-string

string-capitalize string &key start end => cased-string

nstring-upcase string &key start end => string

nstring-downcase string &key start end => string

nstring-capitalize string &key start end => string

* 参数和值(Arguments and Values):

string---a string designator. For nstring-upcase, nstring-downcase, and nstring-capitalize, the string designator must be a string.

start, end---bounding index designators of string. The defaults for start and end are 0 and nil, respectively.

cased-string---一个字符串.

* 描述(Description):

string-upcase, string-downcase, string-capitalize, nstring-upcase, nstring-downcase, nstring-capitalize change the case of the subsequence of string bounded by start and end as follows:

string-upcase

    string-upcase returns a string just like string with all lowercase characters replaced by the corresponding uppercase characters. More precisely, each character of the result string is produced by applying the function char-upcase to the corresponding character of string.

string-downcase

    string-downcase is like string-upcase except that all uppercase characters are replaced by the corresponding lowercase characters (using char-downcase).

string-capitalize

    string-capitalize produces a copy of string such that, for every word in the copy, the first character of the ``word,'' if it has case, is uppercase and any other characters with case in the word are lowercase. For the purposes of string-capitalize, a ``word'' is defined to be a consecutive subsequence consisting of alphanumeric characters, delimited at each end either by a non-alphanumeric character or by an end of the string.

nstring-upcase, nstring-downcase, nstring-capitalize

    nstring-upcase, nstring-downcase, and nstring-capitalize are identical to string-upcase, string-downcase, and string-capitalize respectively except that they modify string.

For string-upcase, string-downcase, and string-capitalize, string is not modified. However, if no characters in string require conversion, the result may be either string or a copy of it, at the implementation's discretion.

* 示例(Examples):

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

* 副作用(Side Effects):

nstring-upcase, nstring-downcase, and nstring-capitalize modify string as appropriate rather than constructing a new string.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

char-upcase, char-downcase

* 注意(Notes):

The result is always of the same length as string. 


### <span id="F-STRING-TRIM">函数 STRING-TRIM, STRING-LEFT-TRIM, STRING-RIGHT-TRIM</span>

* 语法(Syntax):

string-trim character-bag string => trimmed-string

string-left-trim character-bag string => trimmed-string

string-right-trim character-bag string => trimmed-string

* 参数和值(Arguments and Values):

character-bag---a sequence containing characters.

string---a string designator.

trimmed-string---一个字符串.

* 描述(Description):

string-trim returns a substring of string, with all characters in character-bag stripped off the beginning and end. string-left-trim is similar but strips characters off only the beginning; string-right-trim strips off only the end.

If no characters need to be trimmed from the string, then either string itself or a copy of it may be returned, at the discretion of the implementation.

All of these functions observe the fill pointer.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By):

The implementation.

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

string1---a string designator.

string2---a string designator.

start1, end1---bounding index designators of string1. The defaults for start and end are 0 and nil, respectively.

start2, end2---bounding index designators of string2. The defaults for start and end are 0 and nil, respectively.

generalized-boolean---一个广义 boolean.

mismatch-index---a bounding index of string1, or nil.

* 描述(Description):

These functions perform lexicographic comparisons on string1 and string2. string= and string-equal are called equality functions; the others are called inequality functions. The comparison operations these functions perform are restricted to the subsequence of string1 bounded by start1 and end1 and to the subsequence of string2 bounded by start2 and end2.

A string a is equal to a string b if it contains the same number of characters, and the corresponding characters are the same under char= or char-equal, as appropriate.

A string a is less than a string b if in the first position in which they differ the character of a is less than the corresponding character of b according to char< or char-lessp as appropriate, or if string a is a proper prefix of string b (of shorter length and matching in all the characters of a).

The equality functions return a generalized boolean that is true if the strings are equal, or false otherwise.

The inequality functions return a mismatch-index that is true if the strings are not equal, or false otherwise. When the mismatch-index is true, it is an integer representing the first character position at which the two substrings differ, as an offset from the beginning of string1.

The comparison has one of the following results:

string=

    string= is true if the supplied substrings are of the same length and contain the same characters in corresponding positions; otherwise it is false.

string/=

    string/= is true if the supplied substrings are different; otherwise it is false.

string-equal

    string-equal is just like string= except that differences in case are ignored; two characters are considered to be the same if char-equal is true of them.

string<

    string< is true if substring1 is less than substring2; otherwise it is false.

string>

    string> is true if substring1 is greater than substring2; otherwise it is false.

string-lessp, string-greaterp

    string-lessp and string-greaterp are exactly like string< and string>, respectively, except that distinctions between uppercase and lowercase letters are ignored. It is as if char-lessp were used instead of char< for comparing characters.

string<=

    string<= is true if substring1 is less than or equal to substring2; otherwise it is false.

string>=

    string>= is true if substring1 is greater than or equal to substring2; otherwise it is false.

string-not-greaterp, string-not-lessp

    string-not-greaterp and string-not-lessp are exactly like string<= and string>=, respectively, except that distinctions between uppercase and lowercase letters are ignored. It is as if char-lessp were used instead of char< for comparing characters.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

char=

* 注意(Notes):

equal calls string= if applied to two strings. 


### <span id="F-STRINGP">函数 STRINGP</span>

* 语法(Syntax):

stringp object => generalized-boolean

* 参数和值(Arguments and Values):

object---一个对象.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if object is of type string; otherwise, returns false.

* 示例(Examples):

 (stringp "aaaaaa") =>  true
 (stringp #\a) =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

typep, string (type)

* 注意(Notes):

 (stringp object) ==  (typep object 'string)


### <span id="F-MAKE-STRING">函数 MAKE-STRING</span>

* 语法(Syntax):

make-string size &key initial-element element-type => string

* 参数和值(Arguments and Values):

size---a valid array dimension.

initial-element---a character. The default is implementation-dependent.

element-type---a type specifier. The default is character.

string---a simple string.

* 描述(Description):

make-string returns a simple string of length size whose elements have been initialized to initial-element.

The element-type names the type of the elements of the string; a string is constructed of the most specialized type that can accommodate elements of the given type.

* 示例(Examples):

 (make-string 10 :initial-element #\5) =>  "5555555555"
 (length (make-string 10)) =>  10

* 受此影响(Affected By):

The implementation.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 

