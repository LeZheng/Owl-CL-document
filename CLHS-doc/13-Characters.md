# 13 字符

> * 13.1 [字符概念](#CharacterConcepts)
> * 13.2 [字符字典](#TheCharactersDictionary)

## 13.1 <span id="CharacterConcepts">字符概念</span>

> * 13.1.1 [字符介绍](#IntroductionToCharacters)
> * 13.1.2 [Introduction to Scripts and Repertoires](#IntroductionScriptsRepertoires)
> * 13.1.3 [字符属性](#CharacterAttributes)
> * 13.1.4 [字符类别](#CharacterCategories)
> * 13.1.5 [字符的等价](#IdentityCharacters)
> * 13.1.6 [字符的顺序](#OrderingCharacters)
> * 13.1.7 [字符的名字](#CharacterNames)
> * 13.1.8 [在输入和输出中的 Newline 的处理](#TreatmentNewlineInputOutput)
> * 13.1.9 [字符编码](#CharacterEncodings)
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
<!-- TODO 待翻译 Scripts and Repertoires ？？-->
#### 13.1.2.1 字符 Scripts

一个 script 是组成这个 character 类型的详尽分区可能的几个集合中的一个.

这样的集合的数量以及它们之间的边界是具体实现定义的. Common Lisp 不需要这些集合称为类型, 但是一个具体实现允许开去定义这样的类型作为一个扩展. 因为一个 script 的字符不能是另一个 script 的成员, 所以它在谈及字符 repertoires 时是很有用的.

虽然术语 "script" 为了和 ISO 术语定义上兼容而被选择, 但是没有符合规范的具体实现需要去使用 ISO 或者任何其他标准化组织标准化的特定 scripts.

script 或 scripts 是否被任何给定的已命名的实现使用, 如何使用, 是依赖于具体实现的. 


#### 13.1.2.2 字符 Repertoires

一个 repertoire 是一个 character 类型的子类型的类型指定符. 这个术语通常在描述一个独立于编码的字符集合时被使用. repertoires 中的字符只能通过名字, 字符的可见表示, 或者通过字符描述来确认.

一个 repertoire 可以包含来自多个 scripts 的字符, 并且一个字符可以出现在不止一个 repertoire 中.

关于 repertoires 的一些例子, 见字符编码标准 ISO 8859/1, ISO 8859/2, 和 ISO 6937/2. 注意, 虽然术语 "repertoire" 为了和 ISO 术语定义上兼容而被选择, 但是没有符合规范的具体实现需要去使用 ISO 或者任何其他标准化组织标准化的 repertoires. 


### 13.1.3 <span id="CharacterAttributes">字符属性</span>

字符只有一个标准化的属性: 一个码值. 一个字符的码值是一个非负整数. 这个码值是由一个字符 script 和一个字符标签以一种依赖于具体实现的方式组成. 见函数 char-code 和 code-char.

另外, 具体实现定义的字符的属性是允许的, 这样一来, 比如, 两个带有相同码值的字符可以以一个具体实现定义的方式来区分.

对于任何具体实现定义的属性这里有一个突出的值称之为这个属性的 null 值. 一个每个具体实现定义的属性都有这个属性的 null 值的字符被称为一个简单字符. 如果这个具体实现没有具体实现定义的属性, 那么所有字符都是简单字符. 


### 13.1.4 <span id="CharacterCategories">字符类别</span>

有几个(重叠的)类别的字符没有正式关联的类型但这对名称很有用. 它们包括图形字符(graphic characters), 字母字符(alphabetic characters), 大小写字符 (大写和小写字符), 数字字符(numeric characters), 字母数字字符(alphanumeric characters), 还有数字 (以一个给定的基数).

对于一个字符的每个具体实现定义的属性, 那个具体实现的文档必须指明只能在那个属性上区分的字符是否允许和上述类别之一的成员之间存在差异.

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

带有大小写的字符是字母字符的一个子集. 一个带有大小写的字符有着可以是大写或小写的属性. 每个带有大小写的字符都和另一个带有相反大小写的字符一对一对应.

> * 13.1.4.3.1 [大写字符](#UppercaseCharacters)
> * 13.1.4.3.2 [小写字符](#LowercaseCharacters)
> * 13.1.4.3.3 [另一个大小写的对应字符](#CorrespondingCharactersOtherCase)
> * 13.1.4.3.4 [具体实现定义的字符的大小写](#CaseImplDefCharacters)


##### 13.1.4.3.1 <span id="UppercaseCharacters">大写字符</span>

一个大写字符是一个有着对应不同的小写字符(可以使用 char-downcase 来获取)的字符.

标准字符中, 只有这些是大写字符:

    A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 


##### 13.1.4.3.2 <span id="LowercaseCharacters">小写字符</span>

一个小写字符是一个有着对应不同的大写字符(可以使用 char-upcase 来获取)的字符.

标准字符中, 只有这些是小写写字符:

    a b c d e f g h i j k l m n o p q r s t u v w x y z 

##### 13.1.4.3.3 <span id="CorrespondingCharactersOtherCase">另一个大小写的对应字符</span>

上面提及的标准大写字符 A 到 Z 分别对应上面提及的标准小写字符 a 到 z. 比如, 大写字符 E 对应小写字符 e, 反之亦然. 

##### 13.1.4.3.4 <span id="CaseImplDefCharacters">具体实现定义的字符的大小写</span>

一个具体实现可能定义其他具体实现定义的图形字符有着大小写. 这样的定义必须总是成对的---一个大写字符和一个小写字符一对一对应. 

#### 13.1.4.4 <span id="NumericCharacters">数字字符</span>

数字字符是图形字符的一个子集. 在标准字符中, 只有这些是数字字符:

    0 1 2 3 4 5 6 7 8 9

对于每个具体实现定义的没有大小写的图形字符, 具体实现必须定义它是否为一个数字字符. 


#### 13.1.4.5 <span id="AlphanumericCharacters">字母数字字符</span>

数字字母字符的集合是字母字符集合和数字字符集合的并集. 

#### 13.1.4.6 <span id="DigitsRadix">一个给定基数的数字</span>

什么限制一个数位取决于基数 (一个在 2 到 36 之间的整数, 包含 36). 这些可能的数位是:

0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

它们的权重分别是 0, 1, 2, ... 35. 在任何给定的基数 n, 只有前面 n 个可能的数位会被当作数位. 比如, 在基数 2 的数位是 0 和 1, 在基数 10 的数位是 0 到 9, 而在基数 16 的数位是 0 到 F.

在数位中大小写是无效的; 比如, 在基数 16 中, 不管是 F 还是 f 都是权重为 15 的数位. 


### 13.1.5 <span id="IdentityCharacters">字符的等价</span>

字符的等价表示两个字符是 eql, char=, 或 char-equal 但没有必要是 eq. 


### 13.1.6 <span id="OrderingCharacters">字符的顺序</span>

字符的总顺序保证有着以下特性:

* 如果两个字符有着相同的具体实现定义的属性, 那么它们根据 char< 的顺序和根据它们的码值上的 < 断言的数值顺序是一致的.

* 如果两个字符在任何一个属性上有区别, 那么它们就不是 char=.

* 这个总的顺序没有必要和给这些字符应用 char-int 所产生的整数的总顺序一致.

* 虽然一个给定大小写的标准字母字符必须遵循一个局部顺序, 它们不需要是连续的; 允许大写字母和小写字母是交错的. 因此 (char<= #\a x #\z) 不是一个确定 x 是否为一个小写字符的有效方法.

在标准字符中, 那些数字字母字符遵循下面的局部顺序:

    A<B<C<D<E<F<G<H<I<J<K<L<M<N<O<P<Q<R<S<T<U<V<W<X<Y<Z
    a<b<c<d<e<f<g<h<i<j<k<l<m<n<o<p<q<r<s<t<u<v<w<x<y<z
    0<1<2<3<4<5<6<7<8<9
    9<A 或 Z<0
    9<a 或 z<0                                                      

这个意味着, 对于标准字符, 在每个大小写中(大写和小写)字母顺序保持不变, 并且数字字符作为一个组不会和字母字符交错. 但是, 大写字符和小写字符的顺序或交错可能性是具体实现定义的. 

### 13.1.7 <span id="CharacterNames">字符的名字</span>

下面这些字符的名字必须出现在所有符合规范的具体实现中:

Newline

    这个字符表示行之间的分割. 一个具体实现必须在 #\Newline, 一个单字符表示, 和可能被使用的其他外部表示之间转换.

Space

    空格或空白字符.

下面的名字是不完全标准的; 如果一个具体实现支持它们, 它们应该用于这些描述的字符, 而不是其他字符.

Rubout

    擦去或删除字符.

Page

    格式符或页分割字符.

Tab

    制表符.

Backspace

    退格符.

Return

    回车字符.

Linefeed

    换行符字符.

在一些具体实现中, 这些字符名字中的一个或多个可能表示一个标准字符; 比如, #\Linefeed 和 #\Newline 在某些具体实现中可能是相同字符. 


### 13.1.8 <span id="TreatmentNewlineInputOutput">在输入和输出中的 Newline 的处理</span>

当这个 #\Newline 字符被写入到一个输出文件中, 具体实现必须采取适当的动作来产生一个行分割符. 这个可能涉及输出一个标记或转换 #\Newline 为一个 CR/LF 序列. 在读取时, 会发生一个对应的反向转换. 


### 13.1.9 <span id="CharacterEncodings">字符编码</span>

一个字符有时仅仅由它的码值表示, 而有时候通过另一个由码值和所有具体实现定义的属性组成的整数来表示 (以一种即便在相同具体实现中的 Lisp 镜像之间也可能改变的具体实现定义的方式). 这个被函数 char-int 返回的整数, 称为这个字符的 "编码(encoding)". 这里没有对应的函数从一个字符的编码回退到这个字符, 因为它的主要用途包括像哈希这样的东西, 在这里不需要进行逆运算. 

### 13.1.10 <span id="DocImplDefinedScripts">Documentation of Implementation-Defined Scripts</span>

一个具体实现必须记录它支持的字符 scripts. 对于每个支持的字符 script, 这个文档必须描述以下这些:

* 字符标签, 图形, 和描述. 字符标签必须使用拉丁大写字母 A--Z, 连字符 (-), 和数字 0--9 来唯一命名.
* 读取器规范化. 任何 read 用来把不同的字符当作等价的机制必须被记录.
* 对 char-upcase, char-downcase, 还有大小写敏感的格式化指令的影响. 特别地, 对于每个带有大小写的字符, 它是大写还是小写, 以及哪个字符是它在相反大小写的等价物.
* 大小写敏感的函数 char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, 和 char-not-lessp 的行为.
* 任意字符断言的行为; 尤其, alpha-char-p, lower-case-p, upper-case-p, both-case-p, graphic-char-p, 和 alphanumericp 的效果.
* 与文件 I/O 的交互, 特别地, 记录支持的编码字符集 (比如, ISO8859/1-1987) 和支持的外部编码方案. 


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

      一个 character 是一个表示一个在文本总量中表示一个单一标记的对象; 见章节 13.1 (Character Concepts).

      类型 base-char 和 extended-char 组成一个类型 character 的详尽分区.

* 也见(See Also):

        章节 13.1 (Character Concepts), 章节 2.4.8.1 (Sharpsign Backslash), 章节 22.1.3.2 (Printing Characters) 


### <span id="T-BASE-CHAR">类型 BASE-CHAR</span>

* 超类型(Supertypes):

        base-char, character, t

* 描述(Description):

        类型 base-char 被定义为 standard-char 的提升<!--TODO 提升 ？？-->数组元素类型. 一个具体实现可以支持额外的 character 类型的子类型 (除了在这个标准中列出的那些), 它们可能或可能不是 base-char 类型的子类型. 另外, 一个具体实现可以定义 base-char 为 charactor 的相同类型.

        基本字符在以下方面有区别:

        1. 类型 standard-char 是一个 base-char 的一个 subrepertoire.
        2. 不是标准字符的基本字符的选择是具体实现定义的.
        3. 只有 base-char 的类型的对象可以是一个 base-string 类型的元素.
        4. 在这个 base-char 的 repertoire 中没有指定字符的数量上边界; 那个 repertoire 的大小是具体实现定义的. 下边界是 96, 就是标准字符的数量.

        一个字符是否为一个基本字符依赖于一个具体实现表示字符串的方式, 而不是任何其他具体实现和主机操作系统的特性. 比如, 一个实现可能把所有字符串编码为有着16位编码的字符, 而另一种可能有着两种字符串: 那些有着8位编码的字符和有着16位编码的字符. 在第一个实现中, 类型 base-char 等价于 character 类型: 这里只有一种字符串. 在第二个实现中, 基本字符是那些可以被存储在一个有着8位编码字符的字符串的字符. 在这样一个实现中, 类型 base-char 是一个 charactor 类型的适当子类型.

        类型 standard-char 是 type base-char 的子类型. 


### <span id="T-STANDARD-CHAR">类型 STANDARD-CHAR</span>

* 超类型(Supertypes):

        standard-char, base-char, character, t

* 描述(Description):

        在所有符合规范的实现中, 需要有一个96个字符的固定集合. 标准字符被定义在章节 2.1.3 (Standard Characters).

        任何不是简单字符的字符不是标准字符.

* 也见(See Also):

        章节 2.1.3 (Standard Characters) 

### </span id="T-EXTENDED-CHAR">类型 EXTENDED-CHAR</span>

* 超类型(Supertypes):

        extended-char, character, t

* 描述(Description):

        类型 extended-char 等价于类型 (and character (not base-char)).

* 注意(Notes):

        类型 extended-char 在所有字符都是 base-char 类型的实现中可能没有元素. 

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

        character---一个字符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        这些断言用来比较字符串.

        如果所有字符 characters 都是相同的那么 char= 返回 true; 否则, 它返回 false. 如果两个字符在任何一个具体实现定义的属性上不相同, 那么它们就不是 char= 的.

        如果所有字符 characters 都是不同的那么 char/= 返回 true; 否则, 它返回 false.

        如果所有字符 characters 都是单调递增那么 char< 返回 true; 否则, 它返回 false. 如果两个字符有着相同的具体实现定义的属性, 那么它们根据 char< 得到的顺序和在它们的码值上根据断言 < 得到的顺序一致.

        如果所有字符 characters 都是单调递减那么 char> 返回 true; 否则, 它返回 false. 如果两个字符有着相同的具体实现定义的属性, 那么它们根据 char> 得到的顺序和在它们的码值上根据断言 > 得到的顺序一致.

        如果所有字符 characters 都是非单调递减的那么 char<= 返回 true; 否则, 它返回 false. 如果两个字符有着相同的具体实现定义的属性, 那么它们根据 char<= 得到的顺序和在它们的码值上根据断言 <= 得到的顺序一致.

        如果所有字符 characters 都是非单调递增的那么 char>= 返回 true; 否则, 它返回 false. 如果两个字符有着相同的具体实现定义的属性, 那么它们根据 char>= 得到的顺序和在它们的码值上根据断言 >= 得到的顺序一致.

        char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, 和 char-not-lessp 分别类似于 char=, char/=, char<, char>, char<=, char>=, 除了它们忽略大小写差异并且可能对非简单字符有着具体实现定义的行为. 比如, 一个具体实现可能定义 char-equal, 及其他, 忽略某些具体实现定义的属性. 在这些函数上的每个实现定义的属性的效果如果有的话必须被指定为该属性定义的一部分.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果一个参数都没有提供那么应该发出一个 program-error 类型的错误.

* 也见(See Also):

        章节 2.1 (Character Syntax), 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes):

        如果字符在它们的码值属性或其他具体实现定义的属性上有区别, 它们被 char= 当作是不同的.

        没有必要仅仅因为 (char= c1 c2) 为 true, (eq c1 c2) 就要为 true. 尽管 eq 可以区分两个 char= 区分不了的字符时, 它不是作为字符来区分它们, 而是某种意义上基于较低层次的具体实现的特性. 如果 (eq c1 c2) 是 true, 那么 (char= c1 c2) 也是 true. eql 和 equal 比较字符的方式和 char= 相同.

        char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, 和 char-not-lessp 所使用的大小写的惯例意味着标准字符的顺序 A=a, B=b, 诸如此类, 直到 Z=z, 而且 9<A 或 Z<0. 


### <span id="F-CHARACTER">函数 CHARACTER</span>

* 语法(Syntax):

        character character => denoted-character

* 参数和值(Arguments and Values):

        character---一个字符标识符.
        denoted-character---一个字符.

* 描述(Description):

        返回字符标识符 character 表示的字符.

* 示例(Examples):

    ```LISP
    (character #\a) =>  #\a
    (character "a") =>  #\a
    (character 'a) =>  #\A
    (character '\a) =>  #\a
    (character 65.) is an error.
    (character 'apple) is an error.
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果对象 object 不是一个字符标识符, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        coerce

* 注意(Notes):

        (character object) ==  (coerce object 'character)


### <span id="F-CHARACTERP">函数 CHARACTERP</span>

* 语法(Syntax):

        characterp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 character 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (characterp #\a) =>  true
    (characterp 'a) =>  false
    (characterp "a") =>  false
    (characterp 65.) =>  false
    (characterp #\Newline) =>  true
    ;; This next example presupposes an implementation 
    ;; in which #\Rubout is an implementation-defined character.
    (characterp #\Rubout) =>  true
    ```

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

        character---一个字符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果字符 character 是一个字母字符就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (alpha-char-p #\a) =>  true
    (alpha-char-p #\5) =>  false
    (alpha-char-p #\Newline) =>  false
    ;; This next example presupposes an implementation
    ;; in which #\<ALPHA> is a defined character.
    (alpha-char-p #\<ALPHA>) =>  implementation-dependent
    ```

* 受此影响(Affected By):

        无. (特别地, 这个断言的结果独立于任何在当前读取表中被启用的特殊语法.)

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        alphanumericp, 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-ALPHANUMERICP">函数 ALPHANUMERICP</span>

* 语法(Syntax):

        alphanumericp character => generalized-boolean

* 参数和值(Arguments and Values):

        character---一个字符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果 character 是一个字母字符或一个数字字符就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (alphanumericp #\Z) =>  true
    (alphanumericp #\9) =>  true
    (alphanumericp #\Newline) =>  false
    (alphanumericp #\#) =>  false
    ```

* 受此影响(Affected By):

        无. (特别地, 这个断言的结果独立于任何在当前读取表中被启用的特殊语法.)

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        alpha-char-p, graphic-char-p, digit-char-p

* 注意(Notes):

        字母数字字符是图形的就像 graphic-char-p 定义的那样. 字母数字字符是图形字符的一个子集. 标准字符 A 到 Z, a 到 z, 还有 0 到 9 是字母数字字符.

        (alphanumericp x)
          ==  (or (alpha-char-p x) (not (null (digit-char-p x))))


### <span id="F-DIGIT-CHAR">函数 DIGIT-CHAR</span>

* 语法(Syntax):

        digit-char weight &optional radix => char

* 参数和值(Arguments and Values):

        weight---一个非负整数.
        radix---一个基数. 默认是 10.
        char---一个字符或或 false.

* 描述(Description):

        如果权重 weight 小于基数 radix, 那么 digit-char 返回一个字符, 这个字符被当作指定基数下的数字时有着那个权重 weight. 如果产生的字符是一个字母字符, 它会是一个大写的字符.

        如果权重 weight 大于等于基数 radix, digit-char 返回 false.

* 示例(Examples):

    ```LISP
    (digit-char 0) =>  #\0
    (digit-char 10 11) =>  #\A
    (digit-char 10 10) =>  false
    (digit-char 7) =>  #\7
    (digit-char 12) =>  false
    (digit-char 12 16) =>  #\C  ;not #\c
    (digit-char 6 2) =>  false
    (digit-char 1 2) =>  #\1
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        digit-char-p, graphic-char-p, 章节 2.1 (Character Syntax)

* 注意(Notes):

### <span id="F-DIGIT-CHAR-P">函数 DIGIT-CHAR-P</span>

* 语法(Syntax):

        digit-char-p char &optional radix => weight

* 参数和值(Arguments and Values):

        char---一个字符.
        radix---一个基数 radix. 默认为 10.
        weight---一个小于基数 radix 的非负整数, 或者 false.

* 描述(Description):

        测试字符 char 是否为给定基数下的一个数字 (换句话说, 它带有小于基数 radix 的权重). 如果它是那个基数下的一个数字, 它的权重会作为整数返回; 否则返回 nil.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By):

        无. (特别地, 这个断言的结果独立于任何在当前读取表中被启用的特殊语法.)

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        alphanumericp

* 注意(Notes):

        数字是一个图形字符. 

### <span id="F-GRAPHIC-CHAR-P">函数 GRAPHIC-CHAR-P</span>

* 语法(Syntax):

        graphic-char-p char => generalized-boolean

* 参数和值(Arguments and Values):

        char---一个字符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果字符 character 是一个图形字符就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (graphic-char-p #\G) =>  true
    (graphic-char-p #\#) =>  true
    (graphic-char-p #\Space) =>  true
    (graphic-char-p #\Newline) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        read, 章节 2.1 (Character Syntax), 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-STANDARD-CHAR-P">函数 STANDARD-CHAR-P</span>

* 语法(Syntax):

        standard-char-p character => generalized-boolean

* 参数和值(Arguments and Values):

        character---一个字符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果字符 character 是 standard-char 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (standard-char-p #\Space) =>  true
    (standard-char-p #\~) =>  true
    ;; This next example presupposes an implementation
    ;; in which #\Bell is a defined character.
    (standard-char-p #\Bell) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 

### <span id="F-CHAR-UPCASE-DOWNCASE">函数 CHAR-UPCASE, CHAR-DOWNCASE</span>

* 语法(Syntax):

        char-upcase character => corresponding-character

        char-downcase character => corresponding-character

* 参数和值(Arguments and Values):

        character, corresponding-character---一个字符.

* 描述(Description):

        如果字符 character 是一个小写字符, char-upcase 返回对应大写字符. 否则, char-upcase 只是返回给定字符.

        如果 character 是一个大写字符, char-downcase 返回对应小写字符. 否则, char-downcase 只是返回给定字符.

        结果只在码值属性上和字符 character 有区别; 所有具体实现定义的属性都会被保留.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        upper-case-p, alpha-char-p, 章节 13.1.4.3 (大小写字符), 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes):

        如果这个 corresponding-char 和 character 不同, 那么字符 character 和 corresponding-char 都有大小写.

        由于 char-equal 忽略它比较的字符的大小写, 因此这个 corresponding-character 在 char-equal 下总是和字符 character 相同. 


### <span id="F-CASE-P">函数 UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P</span>

* 语法(Syntax):

        upper-case-p character => generalized-boolean

        lower-case-p character => generalized-boolean

        both-case-p character => generalized-boolean

* 参数和值(Arguments and Values):

        character---一个符号.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        这些函数测试给定字符的大小写.

        如果字符 character 是一个大写字符那么 upper-case-p 返回 true; 否则, 返回 false.

        如果字符 character 是一个小写字符那么 lower-case-p 返回 true; 否则, 返回 false.

        如果字符 character 是一个带有大小写的字符那么 both-case-p 返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (upper-case-p #\A) =>  true
    (upper-case-p #\a) =>  false
    (both-case-p #\a) =>  true
    (both-case-p #\5) =>  false
    (lower-case-p #\5) =>  false
    (upper-case-p #\5) =>  false
    ;; This next example presupposes an implementation 
    ;; in which #\Bell is an implementation-defined character.
    (lower-case-p #\Bell) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        char-upcase, char-downcase, 章节 13.1.4.3 (大小写字符), 章节 13.1.10 (Documentation of Implementation-Defined Scripts)

* 注意(Notes): None. 

### <span id="F-CHAR-CODE">函数 CHAR-CODE</span>

* 语法(Syntax):

        char-code character => code

* 参数和值(Arguments and Values):

        character---一个字符.
        code---一个字符的码值.

* 描述(Description):

        char-code 返回字符 character 的码值属性.

* 示例(Examples):

```LISP
;; An implementation using ASCII character encoding 
;; might return these values:
(char-code #\$) =>  36
(char-code #\a) =>  97
```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        char-code-limit

* 注意(Notes): None. 

### <span id="F-CHAR-INT">函数 CHAR-INT</span>

* 语法(Syntax):

        char-int character => integer

* 参数和值(Arguments and Values):

        character---一个字符.
        integer---一个非负整数.

* 描述(Description):

        返回一个编码这个字符对象的非负整数 integer. 这个整数 integer 计算的惯例是依赖于具体实现的. 与 sxhash 相比, 结果不保证独立于特定的 Lisp 镜像.

        如果字符 character 没有具体实现定义的属性, 那么对于字符 c1 和 c2, 结果和 char-int 还有 char-code 是相同的.

        (char= c1 c2) ==  (= (char-int c1) (char-int c2))

* 示例(Examples):

    ```LISP
    (char-int #\A) =>  65       ; implementation A
    (char-int #\A) =>  577      ; implementation B
    (char-int #\A) =>  262145   ; implementation C
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        char-code

* 注意(Notes): None. 

### <span id="F-CODE-CHAR">函数 CODE-CHAR</span>

* 语法(Syntax):

        code-char code => char-p

* 参数和值(Arguments and Values):

        code---一个字符码值.
        char-p---一个字符或 nil.

* 描述(Description):

        返回一个带有给定码值 code 的码值属性的字符. 如果不存在这样的字符以及不能创建一个, 那么就返回 nil.

* 示例(Examples):

    ```LISP
    (code-char 65.) =>  #\A  ;in an implementation using ASCII codes
    (code-char (char-code #\Space)) =>  #\Space  ;in any implementation
    ```

* 受此影响(Affected By):

        具体实现的字符编码.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        char-code

* 注意(Notes):


### <span id="CV-CHAR-CODE-LIMIT">常量 CHAR-CODE-LIMIT</span>

* 常量值(Constant Value):

        一个非负整数, 它的准确大小是依赖于具体实现的, 但是不小于 96 (标准字符的数量).

* 描述(Description):

        函数 char-code 返回的值的上边界.

* 也见(See Also):

        char-code

* 注意(Notes):

        char-code-limit 的值可能大于这个具体实现所支持的字符数. 


### <span id="F-CHAR-NAME">函数 CHAR-NAME</span>

* 语法(Syntax):

        char-name character => name

* 参数和值(Arguments and Values):

        character---一个字符.
        name---一个字符串或 nil.

* 描述(Description):

        返回字符 character 名称字符串, 如果字符 character 没有名称就是 nil.

        所有非图形字符需要有名字, 除非它们有着一些具体实现定义的不是 null 的属性. 其他字符是否有名称是依赖于具体实现的.

        标准字符 <Newline> 和 <Space> 有着各自的名称 "Newline" 和 "Space". 不完全标准字符 <Tab>, <Page>, <Rubout>, <Linefeed>, <Return>, 和 <Backspace> (如果这个具体实现支持它们的话) 有着各自的名称 "Tab", "Page", "Rubout", "Linefeed", "Return", 和 "Backspace" (在这个指示的情况下, 即便名字是通过 "#\" 和函数 name-char 查找的也不是大小写敏感的).

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 character 不是一个字符就应该发出一个 type-error 类型的错误.

* 也见(See Also):

        name-char, 章节 22.1.3.2 (Printing Characters)

* 注意(Notes):

        非图形字符的名字被 Lisp 打印器写做 "#\" 后面跟着这个名字; 见章节 22.1.3.2 (Printing Characters). 

### <span id="F-NAME-CHAR">函数 NAME-CHAR</span>

* 语法(Syntax):

        name-char name => char-p

* 参数和值(Arguments and Values):

        name---一个字符串标识符.
        char-p---一个字符或 nil.

* 描述(Description):

        返回名为 name 的字符对象 (由 string-equal 决定---换句话说, 查找是大小写敏感的). 如果这样一个字符不存在, 返回 nil.

* 示例(Examples):

    ```LISP
    (name-char 'space) =>  #\Space
    (name-char "space") =>  #\Space
    (name-char "Space") =>  #\Space
    (let ((x (char-name #\a)))
      (or (not x) (eql (name-char x) #\a))) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 name 不是一个字符串标识符, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        char-name

* 注意(Notes): None. 

