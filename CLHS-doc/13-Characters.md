# 13 字符

> * 13.1 [字符概念](#CharacterConcepts)
> * 13.2 [字符字典](#TheCharactersDictionary)

## 13.1 <span id="CharacterConcepts">字符概念</span>

> * 13.1.1 [字符介绍](#IntroductionToCharacters)
> * 13.1.2 [Introduction to Scripts and Repertoires](#IntroductionScriptsRepertoires)
> * 13.1.3 [字符属性](#CharacterAttributes)
> * 13.1.4 [字符类别](#CharacterCategories)
> * 13.1.5 [Identity of Characters](#IdentityCharacters)
> * 13.1.6 [Ordering of Characters](#OrderingCharacters)
> * 13.1.7 [Character Names](#CharacterNames)
> * 13.1.8 [Treatment of Newline during Input and Output](#TreatmentNewlineInputOutput)
> * 13.1.9 [Character Encodings](#CharacterEncodings)
> * 13.1.10 [Documentation of Implementation-Defined Scripts](#DocImplDefinedScripts)

### 13.1.1 <span id="IntroductionToCharacters">字符介绍</span>

一个字符是一个在一个文本的总量中 (比如, 一个字符串或一个文本流) 表示单一记号 (比如, 一个字符, 一个特殊符号, 或者一个 "控制字符(control character)") 的对象.

Common Lisp 允许一个具体实现去为国际语言字符和专门领域中(比如, 数学上)使用的字符提供支持.

下面这段包含了可应用于字符的已定义的名字.

下面这段列出了一些和字符属性和字符断言相关的已定义的名字.

    alpha-char-p     char-not-equal     char>            
    alphanumericp    char-not-greaterp  char>=           
    both-case-p      char-not-lessp     digit-char-p     
    char-code-limit  char/=             graphic-char-p   
    char-equal       char<              lower-case-p     
    char-greaterp    char<=             standard-char-p  
    char-lessp       char=              upper-case-p     

    Figure 13-1. 字符相关的已定义的名字 -- 1

下面这段列出了一些字符构造和转换的已定义的名字.

    char-code      char-name    code-char   
    char-downcase  char-upcase  digit-char  
    char-int       character    name-char   

    Figure 13-2. 字符相关的已定义的名字 -- 2 


### 13.1.2 <span id="IntroductionScriptsRepertoires">Introduction to Scripts and Repertoires</span>

#### 13.1.2.1 Character Scripts
<!-- TODO 待翻译 -->
A script is one of possibly several sets that form an exhaustive partition of the type character.

The number of such sets and boundaries between them is implementation-defined. Common Lisp does not require these sets to be types, but an implementation is permitted to define such types as an extension. Since no character from one script can ever be a member of another script, it is generally more useful to speak about character repertoires.

虽然术语 "script" 为了和 ISO 术语定义上兼容而被选择, 但是没有符合规范的具体实现需要去使用 ISO 或者任何其他标准化组织标准化的特定 scripts.

Whether and how the script or scripts used by any given implementation are named is implementation-dependent. 


#### 13.1.2.2 Character Repertoires

一个 repertoire 是一个 character 类型的子类型的类型指定符. 这个术语通常在描述一个独立于编码的字符集合时被使用. repertoires 中的字符只能通过名字, 字符的可见表示, 或者通过字符描述来确认.

一个 repertoire 可以包含来自多个 scripts 的字符, 并且一个字符可以出现在不止一个 repertoire 中.

关于 repertoires 的一些例子, 见字符编码标准 ISO 8859/1, ISO 8859/2, 和 ISO 6937/2. 注意, 虽然术语 "repertoire" 为了和 ISO 术语定义上兼容而被选择, 但是没有符合规范的具体实现需要去使用 ISO 或者任何其他标准化组织标准化的 repertoires. 


### 13.1.3 <span id="CharacterAttributes">字符属性</span>

字符只有一个标准化的属性: 一个码值. 一个字符的码值是一个非负整数. 这个码值是由一个字符 script 和一个字符标签以一种依赖于具体实现的方式组成. 见函数 char-code 和 code-char.

另外, 具体实现定义的字符的属性是允许的, 这样一来, 比如, 两个带有相同码值的字符可以以一个具体实现定义的方式来区分.

对于任何具体实现定义的属性这里有一个突出的值称之为这个属性的 null 值. 一个每个具体实现定义的属性都有这个属性的 null 值的字符被称为一个简单字符. 如果这个具体实现没有具体实现定义的属性, 那么所有字符都是简单字符. 


### 13.1.4 <span id="CharacterCategories">字符类别</span>

有几个(重叠的)类别的字符没有正式关联的类型但这对名称很有用. 它们包括图形字符(graphic characters), 字母字符(alphabetic characters), 大小写字符 (大写和小写字符), 数字字符(numeric characters), 字母数字字符(alphanumeric characters), 还有数字 (以一个给定的基数).

对于一个字符的每个具体实现定义的属性, 那个具体实现的文档必须指明只能在那个属性上区分的字符是否允许和上述类别之一的成员之间存在差异 the documentation for that implementation must specify whether characters that differ only in that attribute are permitted to differ in whether are not they are members of one of the aforementioned categories.<!--TODO 待校验-->

注意, 这些定义的术语独立于任何当前读取表中启用的特殊语法.

> * 13.1.4.1 [图形字符](#GraphicCharacters)
> * 13.1.4.2 [字母字符](#AlphabeticCharacters)
> * 13.1.4.3 [大小写字符](#CharactersWithCase)
> * 13.1.4.4 [数字字符](#NumericCharacters)
> * 13.1.4.5 [字母数字字符](#AlphanumericCharacters)
> * 13.1.4.6 [一个给定基数的数字](#DigitsRadix)

#### 13.1.4.1 <span id="GraphicCharacters">图形字符</span>

每个被分类为图像或可显示的字符都和一个该字符的可见表示的字形相关联.

一个图形字符是一个有着作为一个单个字形的标准文本表示的字符, 就像 A 或 * 或 =. 实际上有着空白字形的空格(space)被定义为是一个图形字符.

在标准字符中, 换行(newline)是非图形的而所有其他的是图形的; 见章节 2.1.3 (Standard Characters).

不是图形的字符被称为非图形字符. 非图形字符有时也被通俗地称为 "格式化字符(formatting characters)" 或 "控制字符(control characters)".

如果 #\Backspace, #\Tab, #\Rubout, #\Linefeed, #\Return, 和 #\Page 被具体实现支持, 那么就是非图形的. 


#### 13.1.4.2 <span id="AlphabeticCharacters">字母字符</span>

字母字符是图形字符的一个子集. 标准字符中, 只有这些是字母字符:

    A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

    a b c d e f g h i j k l m n o p q r s t u v w x y z

任何具体实现定义的有着大小写的字符都是字母的. 对于每个具体实现定义的没有大小写的图形字符, 它是否为字母的是具体实现定义的. 


#### 13.1.4.3 <span id="AlphabeticCharacters">大小写字符</span>

The characters with case are a subset of the alphabetic[1] characters. A character with case has the property of being either uppercase or lowercase. Every character with case is in one-to-one correspondence with some other character with the opposite case.

> * 13.1.4.3.1 [Uppercase Characters](#UppercaseCharacters)
> * 13.1.4.3.2 [Lowercase Characters](#LowercaseCharacters)
> * 13.1.4.3.3 [Corresponding Characters in the Other Case](#CorrespondingCharactersOtherCase)
> * 13.1.4.3.4 [Case of Implementation-Defined Characters](#CaseImplDefCharacters)


##### 13.1.4.3.1 <span id="UppercaseCharacters">Uppercase Characters</span>

An uppercase character is one that has a corresponding lowercase character that is different (and can be obtained using char-downcase).

Of the standard characters, only these are uppercase characters:

A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 


##### 13.1.4.3.2 <span id="LowercaseCharacters">Lowercase Characters</span>

A lowercase character is one that has a corresponding uppercase character that is different (and can be obtained using char-upcase).

Of the standard characters, only these are lowercase characters:

a b c d e f g h i j k l m n o p q r s t u v w x y z 

##### 13.1.4.3.3 <span id="CorrespondingCharactersOtherCase">Corresponding Characters in the Other Case</span>

The uppercase standard characters A through Z mentioned above respectively correspond to the lowercase standard characters a through z mentioned above. For example, the uppercase character E corresponds to the lowercase character e, and vice versa. 

##### 13.1.4.3.4 <span id="CaseImplDefCharacters">Case of Implementation-Defined Characters</span>

An implementation may define that other implementation-defined graphic characters have case. Such definitions must always be done in pairs---one uppercase character in one-to-one correspondence with one lowercase character. 


#### 13.1.4.4 <span id="NumericCharacters">数字字符</span>

The numeric characters are a subset of the graphic characters. Of the standard characters, only these are numeric characters:

0 1 2 3 4 5 6 7 8 9

For each implementation-defined graphic character that has no case, the implementation must define whether or not it is a numeric character. 


#### 13.1.4.5 <span id="AlphanumericCharacters">字母数字字符</span>

The set of alphanumeric characters is the union of the set of alphabetic[1] characters and the set of numeric characters. 

#### 13.1.4.6 <span id="DigitsRadix">一个给定基数的数字</span>

What qualifies as a digit depends on the radix (an integer between 2 and 36, inclusive). The potential digits are:

0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

Their respective weights are 0, 1, 2, ... 35. In any given radix n, only the first n potential digits are considered to be digits. For example, the digits in radix 2 are 0 and 1, the digits in radix 10 are 0 through 9, and the digits in radix 16 are 0 through F.

Case is not significant in digits; for example, in radix 16, both F and f are digits with weight 15. 


### 13.1.5 <span id="IdentityCharacters">Identity of Characters</span>

Two characters that are eql, char=, or char-equal are not necessarily eq. 


### 13.1.6 <span id="OrderingCharacters">Ordering of Characters</span>

The total ordering on characters is guaranteed to have the following properties:

* If two characters have the same implementation-defined attributes, then their ordering by char< is consistent with the numerical ordering by the predicate < on their code attributes.

* If two characters differ in any attribute, then they are not char=.

* The total ordering is not necessarily the same as the total ordering on the integers produced by applying char-int to the characters.

* While alphabetic[1] standard characters of a given case must obey a partial ordering, they need not be contiguous; it is permissible for uppercase and lowercase characters to be interleaved. Thus (char<= #\a x #\z) is not a valid way of determining whether or not x is a lowercase character.

Of the standard characters, those which are alphanumeric obey the following partial ordering:

 A<B<C<D<E<F<G<H<I<J<K<L<M<N<O<P<Q<R<S<T<U<V<W<X<Y<Z
 a<b<c<d<e<f<g<h<i<j<k<l<m<n<o<p<q<r<s<t<u<v<w<x<y<z
 0<1<2<3<4<5<6<7<8<9
 either 9<A or Z<0
 either 9<a or z<0                                                      

This implies that, for standard characters, alphabetic[1] ordering holds within each case (uppercase and lowercase), and that the numeric characters as a group are not interleaved with alphabetic characters. However, the ordering or possible interleaving of uppercase characters and lowercase characters is implementation-defined. 

### 13.1.7 <span id="CharacterNames">Character Names</span>

The following character names must be present in all conforming implementations:

Newline

    The character that represents the division between lines. An implementation must translate between #\Newline, a single-character representation, and whatever external representation(s) may be used.

Space

    The space or blank character.

The following names are semi-standard; if an implementation supports them, they should be used for the described characters and no others.

Rubout

    The rubout or delete character.

Page

    The form-feed or page-separator character.

Tab

    The tabulate character.

Backspace

    The backspace character.

Return

    The carriage return character.

Linefeed

    The line-feed character.

In some implementations, one or more of these character names might denote a standard character; for example, #\Linefeed and #\Newline might be the same character in some implementations. 


### 13.1.8 <span id="TreatmentNewlineInputOutput">Treatment of Newline during Input and Output</span>

When the character #\Newline is written to an output file, the implementation must take the appropriate action to produce a line division. This might involve writing out a record or translating #\Newline to a CR/LF sequence. When reading, a corresponding reverse transformation must take place. 


### 13.1.9 <span id="CharacterEncodings">Character Encodings</span>

A character is sometimes represented merely by its code, and sometimes by another integer value which is composed from the code and all implementation-defined attributes (in an implementation-defined way that might vary between Lisp images even in the same implementation). This integer, returned by the function char-int, is called the character's ``encoding.'' There is no corresponding function from a character's encoding back to the character, since its primary intended uses include things like hashing where an inverse operation is not really called for. 

### 13.1.10 <span id="DocImplDefinedScripts">Documentation of Implementation-Defined Scripts</span>

An implementation must document the character scripts it supports. For each character script supported, the documentation must describe at least the following:

* Character labels, glyphs, and descriptions. Character labels must be uniquely named using only Latin capital letters A--Z, hyphen (-), and digits 0--9.
* Reader canonicalization. Any mechanisms by which read treats different characters as equivalent must be documented.
* The impact on char-upcase, char-downcase, and the case-sensitive format directives. In particular, for each character with case, whether it is uppercase or lowercase, and which character is its equivalent in the opposite case.
* The behavior of the case-insensitive functions char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, and char-not-lessp.
* The behavior of any character predicates; in particular, the effects of alpha-char-p, lower-case-p, upper-case-p, both-case-p, graphic-char-p, and alphanumericp.
* The interaction with file I/O, in particular, the supported coded character sets (for example, ISO8859/1-1987) and external encoding schemes supported are documented. 


## 13.2 <span id="TheCharactersDictionary">字符字典</span>

> * [系统类 CHARACTER](#SC-CHARACTER)
> * [类型 BASE-CHAR](#T-BASE-CHAR)
> * [类型 STANDARD-CHAR](#T-STANDARD-CHAR)
> * [类型 EXTENDED-CHAR](#T-EXTENDED-CHAR)
> * [函数 CHAR=, CHAR/=, CHAR<, CHAR>, CHAR<=, CHAR>=, CHAR-EQUAL, CHAR-NOT-EQUAL, CHAR-LESSP, CHAR-GREATERP, CHAR-NOT-GREATERP, CHAR-NOT-LESSP](#F-CCCCCCCCCCCC)
> * [函数 CHARACTER](#F-CHARACTER)
> * [函数 CHARACTERP](#F-CHARACTERP)
> * [函数 ALPHA-CHAR-P](#F-ALPHA-CHAR-P)
> * [函数 ALPHANUMERICP](#F-ALPHANUMERICP)
> * [函数 DIGIT-CHAR](#F-DIGIT-CHAR)
> * [函数 DIGIT-CHAR-P](#F-DIGIT-CHAR-P)
> * [函数 GRAPHIC-CHAR-P](#F-GRAPHIC-CHAR-P)
> * [函数 STANDARD-CHAR-P](#F-STANDARD-CHAR-P)
> * [函数 CHAR-UPCASE, CHAR-DOWNCASE](#F-CHAR-UPCASE-DOWNCASE)
> * [函数 UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P](#F-CASE-P)
> * [函数 CHAR-CODE](#F-CHAR-CODE)
> * [函数 CHAR-INT](#F-CHAR-INT)
> * [函数 CODE-CHAR](#F-CODE-CHAR)
> * [常量 CHAR-CODE-LIMIT](#CV-CHAR-CODE-LIMIT)
> * [函数 CHAR-NAME](#F-CHAR-NAME)
> * [函数 NAME-CHAR](#F-NAME-CHAR)

### <span id="SC-CHARACTER">系统类 CHARACTER</span>

* 类优先级列表(Class Precedence List):

character, t

* 描述(Description):

A character is an object that represents a unitary token in an aggregate quantity of text; see Section 13.1 (Character Concepts).

The types base-char and extended-char form an exhaustive partition of the type character.

* 也见(See Also):

Section 13.1 (Character Concepts), Section 2.4.8.1 (Sharpsign Backslash), Section 22.1.3.2 (Printing Characters) 


### <span id="T-BASE-CHAR">类型 BASE-CHAR</span>

* 超类型(Supertypes):

base-char, character, t

* 描述(Description):

The type base-char is defined as the upgraded array element type of standard-char. An implementation can support additional subtypes of type character (besides the ones listed in this standard) that might or might not be supertypes of type base-char. In addition, an implementation can define base-char to be the same type as character.

Base characters are distinguished in the following respects:

1. The type standard-char is a subrepertoire of the type base-char.
2. The selection of base characters that are not standard characters is implementation defined.
3. Only objects of the type base-char can be elements of a base string.
4. No upper bound is specified for the number of characters in the base-char repertoire; the size of that repertoire is implementation-defined. The lower bound is 96, the number of standard characters.

Whether a character is a base character depends on the way that an implementation represents strings, and not any other properties of the implementation or the host operating system. For example, one implementation might encode all strings as characters having 16-bit encodings, and another might have two kinds of strings: those with characters having 8-bit encodings and those with characters having 16-bit encodings. In the first implementation, the type base-char is equivalent to the type character: there is only one kind of string. In the second implementation, the base characters might be those characters that could be stored in a string of characters having 8-bit encodings. In such an implementation, the type base-char is a proper subtype of the type character.

The type standard-char is a subtype of type base-char. 


### <span id="T-STANDARD-CHAR">类型 STANDARD-CHAR</span>

* 超类型(Supertypes):

standard-char, base-char, character, t

* 描述(Description):

A fixed set of 96 characters required to be present in all conforming implementations. Standard characters are defined in Section 2.1.3 (Standard Characters).

Any character that is not simple is not a standard character.

* 也见(See Also):

Section 2.1.3 (Standard Characters) 

### </span id="T-EXTENDED-CHAR">类型 EXTENDED-CHAR</span>

* 超类型(Supertypes):

extended-char, character, t

* 描述(Description):

The type extended-char is equivalent to the type (and character (not base-char)).

* 注意(Notes):

The type extended-char might have no elements[4] in implementations in which all characters are of type base-char. 

### <span id="F-CCCCCCCCCCCC">函数 CHAR=, CHAR/=, CHAR<, CHAR>, CHAR<=, CHAR>=, CHAR-EQUAL, CHAR-NOT-EQUAL, CHAR-LESSP, CHAR-GREATERP, CHAR-NOT-GREATERP, CHAR-NOT-LESSP</span>

* 语法(Syntax):

char= &rest characters+ => generalized-boolean

char/= &rest characters+ => generalized-boolean

char< &rest characters+ => generalized-boolean

char> &rest characters+ => generalized-boolean

char<= &rest characters+ => generalized-boolean

char>= &rest characters+ => generalized-boolean

char-equal &rest characters+ => generalized-boolean

char-not-equal &rest characters+ => generalized-boolean

char-lessp &rest characters+ => generalized-boolean

char-greaterp &rest characters+ => generalized-boolean

char-not-greaterp &rest characters+ => generalized-boolean

char-not-lessp &rest characters+ => generalized-boolean

* 参数和值(Arguments and Values):

character---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

These predicates compare characters.

char= returns true if all characters are the same; otherwise, it returns false. If two characters differ in any implementation-defined attributes, then they are not char=.

char/= returns true if all characters are different; otherwise, it returns false.

char< returns true if the characters are monotonically increasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char< is consistent with the numerical ordering by the predicate < on their codes.

char> returns true if the characters are monotonically decreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char> is consistent with the numerical ordering by the predicate > on their codes.

char<= returns true if the characters are monotonically nondecreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char<= is consistent with the numerical ordering by the predicate <= on their codes.

char>= returns true if the characters are monotonically nonincreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char>= is consistent with the numerical ordering by the predicate >= on their codes.

char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, and char-not-lessp are similar to char=, char/=, char<, char>, char<=, char>=, respectively, except that they ignore differences in case and might have an implementation-defined behavior for non-simple characters. For example, an implementation might define that char-equal, etc. ignore certain implementation-defined attributes. The effect, if any, of each implementation-defined attribute upon these functions must be specified as part of the definition of that attribute.

* 示例(Examples):

 (char= #\d #\d) =>  true
 (char= #\A #\a) =>  false
 (char= #\d #\x) =>  false
 (char= #\d #\D) =>  false
 (char/= #\d #\d) =>  false
 (char/= #\d #\x) =>  true
 (char/= #\d #\D) =>  true
 (char= #\d #\d #\d #\d) =>  true
 (char/= #\d #\d #\d #\d) =>  false
 (char= #\d #\d #\x #\d) =>  false
 (char/= #\d #\d #\x #\d) =>  false
 (char= #\d #\y #\x #\c) =>  false
 (char/= #\d #\y #\x #\c) =>  true
 (char= #\d #\c #\d) =>  false
 (char/= #\d #\c #\d) =>  false
 (char< #\d #\x) =>  true
 (char<= #\d #\x) =>  true
 (char< #\d #\d) =>  false
 (char<= #\d #\d) =>  true
 (char< #\a #\e #\y #\z) =>  true
 (char<= #\a #\e #\y #\z) =>  true
 (char< #\a #\e #\e #\y) =>  false
 (char<= #\a #\e #\e #\y) =>  true
 (char> #\e #\d) =>  true
 (char>= #\e #\d) =>  true
 (char> #\d #\c #\b #\a) =>  true
 (char>= #\d #\c #\b #\a) =>  true
 (char> #\d #\d #\c #\a) =>  false
 (char>= #\d #\d #\c #\a) =>  true
 (char> #\e #\d #\b #\c #\a) =>  false
 (char>= #\e #\d #\b #\c #\a) =>  false
 (char> #\z #\A) =>  implementation-dependent
 (char> #\Z #\a) =>  implementation-dependent
 (char-equal #\A #\a) =>  true
 (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
=>  (#\A #\a #\b #\B #\c #\C)
 (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char<)
=>  (#\A #\B #\C #\a #\b #\c) ;Implementation A
=>  (#\a #\b #\c #\A #\B #\C) ;Implementation B
=>  (#\a #\A #\b #\B #\c #\C) ;Implementation C
=>  (#\A #\a #\B #\b #\C #\c) ;Implementation D
=>  (#\A #\B #\a #\b #\C #\c) ;Implementation E

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type program-error if at least one character is not supplied.

* 也见(See Also):

Section 2.1 (Character Syntax), Section 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes):

If characters differ in their code attribute or any implementation-defined attribute, they are considered to be different by char=.

There is no requirement that (eq c1 c2) be true merely because (char= c1 c2) is true. While eq can distinguish two characters that char= does not, it is distinguishing them not as characters, but in some sense on the basis of a lower level implementation characteristic. If (eq c1 c2) is true, then (char= c1 c2) is also true. eql and equal compare characters in the same way that char= does.

The manner in which case is used by char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, and char-not-lessp implies an ordering for standard characters such that A=a, B=b, and so on, up to Z=z, and furthermore either 9<A or Z<0. 


### <span id="F-CHARACTER">函数 CHARACTER</span>

* 语法(Syntax):

character character => denoted-character

* 参数和值(Arguments and Values):

character---a character designator.

denoted-character---a character.

* 描述(Description):

Returns the character denoted by the character designator.

* 示例(Examples):

 (character #\a) =>  #\a
 (character "a") =>  #\a
 (character 'a) =>  #\A
 (character '\a) =>  #\a
 (character 65.) is an error.
 (character 'apple) is an error.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if object is not a character designator.

* 也见(See Also):

coerce

* 注意(Notes):

 (character object) ==  (coerce object 'character)


### <span id="F-CHARACTERP">函数 CHARACTERP</span>

* 语法(Syntax):

characterp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type character; otherwise, returns false.

* 示例(Examples):

 (characterp #\a) =>  true
 (characterp 'a) =>  false
 (characterp "a") =>  false
 (characterp 65.) =>  false
 (characterp #\Newline) =>  true
 ;; This next example presupposes an implementation 
 ;; in which #\Rubout is an implementation-defined character.
 (characterp #\Rubout) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

character (type and function), typep

* 注意(Notes):

 (characterp object) ==  (typep object 'character)


### <span id="F-ALPHA-CHAR-P">函数 ALPHA-CHAR-P</span>

* 语法(Syntax):

alpha-char-p character => generalized-boolean

* 参数和值(Arguments and Values):

character---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if character is an alphabetic[1] character; otherwise, returns false.

* 示例(Examples):

 (alpha-char-p #\a) =>  true
 (alpha-char-p #\5) =>  false
 (alpha-char-p #\Newline) =>  false
 ;; This next example presupposes an implementation
 ;; in which #\<ALPHA> is a defined character.
 (alpha-char-p #\<ALPHA>) =>  implementation-dependent

* 受此影响(Affected By):

None. (In particular, the results of this predicate are independent of any special syntax which might have been enabled in the current readtable.)

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

alphanumericp, Section 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-ALPHANUMERICP">函数 ALPHANUMERICP</span>

* 语法(Syntax):

alphanumericp character => generalized-boolean

* 参数和值(Arguments and Values):

character---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if character is an alphabetic[1] character or a numeric character; otherwise, returns false.

* 示例(Examples):

 (alphanumericp #\Z) =>  true
 (alphanumericp #\9) =>  true
 (alphanumericp #\Newline) =>  false
 (alphanumericp #\#) =>  false

* 受此影响(Affected By):

None. (In particular, the results of this predicate are independent of any special syntax which might have been enabled in the current readtable.)

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

alpha-char-p, graphic-char-p, digit-char-p

* 注意(Notes):

Alphanumeric characters are graphic as defined by graphic-char-p. The alphanumeric characters are a subset of the graphic characters. The standard characters A through Z, a through z, and 0 through 9 are alphanumeric characters.

 (alphanumericp x)
   ==  (or (alpha-char-p x) (not (null (digit-char-p x))))


### <span id="F-DIGIT-CHAR">函数 DIGIT-CHAR</span>

* 语法(Syntax):

digit-char weight &optional radix => char

* 参数和值(Arguments and Values):

weight---a non-negative integer.

radix---a radix. The default is 10.

char---a character or false.

* 描述(Description):

If weight is less than radix, digit-char returns a character which has that weight when considered as a digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will be an uppercase character.

If weight is greater than or equal to radix, digit-char returns false.

* 示例(Examples):

 (digit-char 0) =>  #\0
 (digit-char 10 11) =>  #\A
 (digit-char 10 10) =>  false
 (digit-char 7) =>  #\7
 (digit-char 12) =>  false
 (digit-char 12 16) =>  #\C  ;not #\c
 (digit-char 6 2) =>  false
 (digit-char 1 2) =>  #\1

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

digit-char-p, graphic-char-p, Section 2.1 (Character Syntax)

* 注意(Notes):

### <span id="F-DIGIT-CHAR-P">函数 DIGIT-CHAR-P</span>

* 语法(Syntax):

digit-char-p char &optional radix => weight

* 参数和值(Arguments and Values):

char---a character.

radix---a radix. The default is 10.

weight---either a non-negative integer less than radix, or false.

* 描述(Description):

Tests whether char is a digit in the specified radix (i.e., with a weight less than radix). If it is a digit in that radix, its weight is returned as an integer; otherwise nil is returned.

* 示例(Examples):

 (digit-char-p #\5)    =>  5
 (digit-char-p #\5 2)  =>  false
 (digit-char-p #\A)    =>  false
 (digit-char-p #\a)    =>  false
 (digit-char-p #\A 11) =>  10
 (digit-char-p #\a 11) =>  10
 (mapcar #'(lambda (radix) 
             (map 'list #'(lambda (x) (digit-char-p x radix)) 
                  "059AaFGZ"))
         '(2 8 10 16 36))
 =>  ((0 NIL NIL NIL NIL NIL NIL NIL)
     (0 5 NIL NIL NIL NIL NIL NIL)
     (0 5 9 NIL NIL NIL NIL NIL)
     (0 5 9 10 10 15 NIL NIL)
     (0 5 9 10 10 15 16 35))

* 受此影响(Affected By):

None. (In particular, the results of this predicate are independent of any special syntax which might have been enabled in the current readtable.)

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

alphanumericp

* 注意(Notes):

Digits are graphic characters. 

### <span id="F-GRAPHIC-CHAR-P">函数 GRAPHIC-CHAR-P</span>

* 语法(Syntax):

graphic-char-p char => generalized-boolean

* 参数和值(Arguments and Values):

char---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if character is a graphic character; otherwise, returns false.

* 示例(Examples):

 (graphic-char-p #\G) =>  true
 (graphic-char-p #\#) =>  true
 (graphic-char-p #\Space) =>  true
 (graphic-char-p #\Newline) =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

read, Section 2.1 (Character Syntax), Section 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-STANDARD-CHAR-P">函数 STANDARD-CHAR-P</span>

* 语法(Syntax):

standard-char-p character => generalized-boolean

* 参数和值(Arguments and Values):

character---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if character is of type standard-char; otherwise, returns false.

* 示例(Examples):

 (standard-char-p #\Space) =>  true
 (standard-char-p #\~) =>  true
 ;; This next example presupposes an implementation
 ;; in which #\Bell is a defined character.
 (standard-char-p #\Bell) =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also): None.

* 注意(Notes): None. 

### <span id="F-CHAR-UPCASE-DOWNCASE">函数 CHAR-UPCASE, CHAR-DOWNCASE</span>

* 语法(Syntax):

char-upcase character => corresponding-character

char-downcase character => corresponding-character

* 参数和值(Arguments and Values):

character, corresponding-character---a character.

* 描述(Description):

If character is a lowercase character, char-upcase returns the corresponding uppercase character. Otherwise, char-upcase just returns the given character.

If character is an uppercase character, char-downcase returns the corresponding lowercase character. Otherwise, char-downcase just returns the given character.

The result only ever differs from character in its code attribute; all implementation-defined attributes are preserved.

* 示例(Examples):

 (char-upcase #\a) =>  #\A
 (char-upcase #\A) =>  #\A
 (char-downcase #\a) =>  #\a
 (char-downcase #\A) =>  #\a
 (char-upcase #\9) =>  #\9
 (char-downcase #\9) =>  #\9
 (char-upcase #\@) =>  #\@
 (char-downcase #\@) =>  #\@
 ;; Note that this next example might run for a very long time in 
 ;; some implementations if CHAR-CODE-LIMIT happens to be very large
 ;; for that implementation.
 (dotimes (code char-code-limit)
   (let ((char (code-char code)))
     (when char
       (unless (cond ((upper-case-p char) (char= (char-upcase (char-downcase char)) char))
                     ((lower-case-p char) (char= (char-downcase (char-upcase char)) char))
                     (t (and (char= (char-upcase (char-downcase char)) char)
                             (char= (char-downcase (char-upcase char)) char))))
         (return char)))))
=>  NIL

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

upper-case-p, alpha-char-p, Section 13.1.4.3 (大小写字符), Section 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes):

If the corresponding-char is different than character, then both the character and the corresponding-char have case.

Since char-equal ignores the case of the characters it compares, the corresponding-character is always the same as character under char-equal. 


### <span id="F-CASE-P">函数 UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P</span>

* 语法(Syntax):

upper-case-p character => generalized-boolean

lower-case-p character => generalized-boolean

both-case-p character => generalized-boolean

* 参数和值(Arguments and Values):

character---a character.

generalized-boolean---a generalized boolean.

* 描述(Description):

These functions test the case of a given character.

upper-case-p returns true if character is an uppercase character; otherwise, returns false.

lower-case-p returns true if character is a lowercase character; otherwise, returns false.

both-case-p returns true if character is a character with case; otherwise, returns false.

* 示例(Examples):

 (upper-case-p #\A) =>  true
 (upper-case-p #\a) =>  false
 (both-case-p #\a) =>  true
 (both-case-p #\5) =>  false
 (lower-case-p #\5) =>  false
 (upper-case-p #\5) =>  false
 ;; This next example presupposes an implementation 
 ;; in which #\Bell is an implementation-defined character.
 (lower-case-p #\Bell) =>  false

Side Effects: None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

char-upcase, char-downcase, Section 13.1.4.3 (大小写字符), Section 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-CHAR-CODE">函数 CHAR-CODE</span>

* 语法(Syntax):

char-code character => code

* 参数和值(Arguments and Values):

character---a character.

code---a character code.

* 描述(Description):

char-code returns the code attribute of character.

* 示例(Examples):

;; An implementation using ASCII character encoding 
;; might return these values:
(char-code #\$) =>  36
(char-code #\a) =>  97

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

char-code-limit

* 注意(Notes): None. 

### <span id="F-CHAR-INT">函数 CHAR-INT</span>

* 语法(Syntax):

char-int character => integer

* 参数和值(Arguments and Values):

character---a character.

integer---a non-negative integer.

* 描述(Description):

Returns a non-negative integer encoding the character object. The manner in which the integer is computed is implementation-dependent. In contrast to sxhash, the result is not guaranteed to be independent of the particular Lisp image.

If character has no implementation-defined attributes, the results of char-int and char-code are the same.

 (char= c1 c2) ==  (= (char-int c1) (char-int c2))

for characters c1 and c2.

* 示例(Examples):

 (char-int #\A) =>  65       ; implementation A
 (char-int #\A) =>  577      ; implementation B
 (char-int #\A) =>  262145   ; implementation C

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

char-code

* 注意(Notes): None. 

### <span id="F-CODE-CHAR">函数 CODE-CHAR</span>

* 语法(Syntax):

code-char code => char-p

* 参数和值(Arguments and Values):

code---a character code.

char-p---a character or nil.

* 描述(Description):

Returns a character with the code attribute given by code. If no such character exists and one cannot be created, nil is returned.

* 示例(Examples):

(code-char 65.) =>  #\A  ;in an implementation using ASCII codes
(code-char (char-code #\Space)) =>  #\Space  ;in any implementation

* 受此影响(Affected By):

The implementation's character encoding.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

char-code

* 注意(Notes):


### <span id="CV-CHAR-CODE-LIMIT">常量 CHAR-CODE-LIMIT</span>

Constant Value:

A non-negative integer, the exact magnitude of which is implementation-dependent, but which is not less than 96 (the number of standard characters).

* 描述(Description):

The upper exclusive bound on the value returned by the function char-code.

* 也见(See Also):

char-code

* 注意(Notes):

The value of char-code-limit might be larger than the actual number of characters supported by the implementation. 


### <span id="F-CHAR-NAME">函数 CHAR-NAME</span>

* 语法(Syntax):

char-name character => name

* 参数和值(Arguments and Values):

character---a character.

name---a string or nil.

* 描述(Description):

Returns a string that is the name of the character, or nil if the character has no name.

All non-graphic characters are required to have names unless they have some implementation-defined attribute which is not null. Whether or not other characters have names is implementation-dependent.

The standard characters <Newline> and <Space> have the respective names "Newline" and "Space". The semi-standard characters <Tab>, <Page>, <Rubout>, <Linefeed>, <Return>, and <Backspace> (if they are supported by the implementation) have the respective names "Tab", "Page", "Rubout", "Linefeed", "Return", and "Backspace" (in the indicated case, even though name lookup by ``#\'' and by the function name-char is not case sensitive).

* 示例(Examples):

 (char-name #\ ) =>  "Space"
 (char-name #\Space) =>  "Space"
 (char-name #\Page) =>  "Page"

 (char-name #\a)
=>  NIL
OR=>  "LOWERCASE-a"
OR=>  "Small-A"
OR=>  "LA01"

 (char-name #\A)
=>  NIL
OR=>  "UPPERCASE-A"
OR=>  "Capital-A"
OR=>  "LA02"

 ;; Even though its CHAR-NAME can vary, #\A prints as #\A
 (prin1-to-string (read-from-string (format nil "#\\~A" (or (char-name #\A) "A"))))
=>  "#\\A"

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if character is not a character.

* 也见(See Also):

name-char, Section 22.1.3.2 (Printing Characters)

* 注意(Notes):

Non-graphic characters having names are written by the Lisp printer as ``#\'' followed by the their name; see Section 22.1.3.2 (Printing Characters). 

### <span id="F-NAME-CHAR">函数 NAME-CHAR</span>

* 语法(Syntax):

name-char name => char-p

* 参数和值(Arguments and Values):

name---a string designator.

char-p---a character or nil.

* 描述(Description):

Returns the character object whose name is name (as determined by string-equal---i.e., lookup is not case sensitive). If such a character does not exist, nil is returned.

* 示例(Examples):

(name-char 'space) =>  #\Space
(name-char "space") =>  #\Space
(name-char "Space") =>  #\Space
(let ((x (char-name #\a)))
  (or (not x) (eql (name-char x) #\a))) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if name is not a string designator.

* 也见(See Also):

char-name

* 注意(Notes): None. 

