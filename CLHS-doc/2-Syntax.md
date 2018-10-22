# 2. 语法

> * 2.1 [字符语法](#CharacterSyntax)
> * 2.2 [读取器算法](#ReaderAlgorithm)
> * 2.3 [token的解释](#InterpretationOfTokens)
> * 2.4 [标准宏字符](#StandardMacroCharacters)

## 2.1 <span id = "CharacterSyntax">字符语法</span>

Lisp 读取器[Lisp reader]从一个流[stream]中读取字符[character], 然后将其解释为一个对象[object]的打印表示, 构建这个对象[object]并且返回它.

这个章节描述的语法称之为标准语法[standard syntax]. Common Lisp 提供了对应操作, 因此可以在程序的控制下修改读取表[readtable]所表示的语法信息的各个方面; 见章节 23 (读取器). 除了明确说明的以外, 这个文档中使用的就是标准语法[standard syntax].

> * 2.1.1 [读取表](#Readtables)
> * 2.1.2 [影响 Lisp 读取器的变量](#VariablesAffectReader)
> * 2.1.3 [标准字符](#StandardCharacters)
> * 2.1.4 [字符语法类型](#CharacterSyntaxTypes)

### 2.1.1 <span id = "Readtables">读取表</span>

Lisp 读取器[Lisp reader]使用的语法信息体现在一个称之为读取表[readtable]的对象[object]中. 此外，这个读取表[readtable]还包含字符[character]和语法类型[syntax type]之间的关联.

下一块列出了一些适用于读取表的定义的名字.

    *readtable*                   readtable-case                
    copy-readtable                 readtablep                    
    get-dispatch-macro-character   set-dispatch-macro-character  
    get-macro-character            set-macro-character           
    make-dispatch-macro-character  set-syntax-from-char          

    Figure 2-1. 读取表已定义的名字

> * 2.1.1.1 [当前的读取表](#CurrentReadtable)
> * 2.1.1.2 [标准读取表](#StandardReadtable)
> * 2.1.1.3 [初始读取表](#InitialReadtable)

#### 2.1.1.1 <span id = "CurrentReadtable">当前的读取表</span>

可以存在一些描述不同语法的读取表[readtable]，但是在任何给定的时间内都只存在一个影响着 Lisp 读取器[Lisp reader]把表达式[expression]解析为对象[object]的方式, 称之为当前读取表[current readtable]. 在一个给定的动态环境[dynamic environment]中的当前读取表[current readtable]是这个环境[environment]中的 \*readtable* 的值[value]. 为了使一个不同的读取表[readtable]成为当前的读取表[current readtable], \*readtable* 可以被赋值或绑定[bound]. 

#### 2.1.1.2 <span id = "StandardReadtable">标准读取表</span>

这个标准读取表[standard readtable]符合标准语法[standard syntax]. 如果尝试去修改标准读取表[standard readtable], 后果是未定义的. 为了实现修改或者扩展标准语法[standard syntax]的效果, 可以去创建一个标准读取表[standard readtable]的副本; 见函数[function] copy-readtable.

标准读取表[standard readtable]的读取表大小写模式[readtable case]是 :upcase. 

#### 2.1.1.3 <span id = "InitialReadtable">初始读取表</span>

初始读取表[initial readtable]是这个 Lisp 镜像[Lisp image]开始时的当前读取表[current readtable]. 在那个时候, 它符合标准语法[standard syntax]. 初始读取表[initial readtable]不同[distinct]于标准读取表[standard readtable]. 一个符合规范的程序[conforming program]去修改初始读取表[initial readtable]是允许的. 

### 2.1.2 <span id = "VariablesAffectReader">影响 Lisp 读取器的变量</span>

Lisp 读取器[Lisp reader]不止受当前读取表[current readtable]所影响, 也被很多动态变量[dynamic variable]所影响. 下面这段就列出了这些影响Lisp 读取器[Lisp reader]行为的变量[variable].

    *package*    *read-default-float-format*  *readtable*  
    *read-base*  *read-suppress*                           

    Figure 2-2. 影响 Lisp 读取器的变量. 

### 2.1.3 <span id = "StandardCharacters">标准字符</span>

所有实现[implementation]必须支持的一个称之为 standard-char 的字符[character]字元库[repertoire]; 这个字元库[repertoire]中的成员字符[character]称之为标准字符[standard character].

这个 standard-char 字元库[repertoire]由非图形化[non-graphic]字符[character]换行[newline], 图形化[graphic]字符[character]空格[space], 还有以下 94 个图形化[graphic]字符[character]或者它们的等价体构成:

  | 图形ID       | 字形    | 描述         | 图形ID      | 字形    | 描述  |
  | :------:   | :------:| :------:  | :------:  | :------:| :------:|
  |LA01        |a      |小写 a      |LN01        |n      |小写 n      |
  |LA02        |A      |大写 A    |LN02        |N      |大写 N    |
  |LB01        |b      |小写 b      |LO01        |o      |小写 o      |
  |LB02        |B      |大写 B    |LO02        |O      |大写 O    |
  |LC01        |c      |小写 c      |LP01        |p      |小写 p      |
  |LC02        |C      |大写 C    |LP02        |P      |大写 P    |
  |LD01        |d      |小写 d      |LQ01        |q      |小写 q      |
  |LD02        |D      |大写 D    |LQ02        |Q      |大写 Q    |
  |LE01        |e      |小写 e      |LR01        |r      |小写 r      |
  |LE02        |E      |大写 E    |LR02        |R      |大写 R    |
  |LF01        |f      |小写 f      |LS01        |s      |小写 s      |
  |LF02        |F      |大写 F    |LS02        |S      |大写 S    |
  |LG01        |g      |小写 g      |LT01        |t      |小写 t      |
  |LG02        |G      |大写 G    |LT02        |T      |大写 T    |
  |LH01        |h      |小写 h      |LU01        |u      |小写 u      |
  |LH02        |H      |大写 H    |LU02        |U      |大写 U    |
  |LI01        |i      |小写 i      |LV01        |v      |小写 v      |
  |LI02        |I      |大写 I    |LV02        |V      |大写 V    |
  |LJ01        |j      |小写 j      |LW01        |w      |小写 w      |
  |LJ02        |J      |大写 J    |LW02        |W      |大写 W    |
  |LK01        |k      |小写 k      |LX01        |x      |小写 x      |
  |LK02        |K      |大写 K    |LX02        |X      |大写 X    |
  |LL01        |l      |小写 l      |LY01        |y      |小写 y      |
  |LL02        |L      |大写 L    |LY02        |Y      |大写 Y    |
  |LM01        |m      |小写 m      |LZ01        |z      |小写 z      |
  |LM02        |M      |大写 M    |LZ02        |Z      |大写 Z    |

Figure 2-3. 标准字符子表 (Part 1 of 3: 拉丁字母)

  |图形ID       |字形   |描述          |图形ID       |字形   |描述  |
  | :------:   | :------:| :------:  | :------:  | :------:| :------:|
  |ND01        |1      |digit 1      |ND06        |6      |digit 6      |
  |ND02        |2      |digit 2      |ND07        |7      |digit 7      |
  |ND03        |3      |digit 3      |ND08        |8      |digit 8      |
  |ND04        |4      |digit 4      |ND09        |9      |digit 9      |
  |ND05        |5      |digit 5      |ND10        |0      |digit 0      |

Figure 2-4. 标准字符子表 (Part 2 of 3: 数字字符)

  |图形ID       |字形   |描述                              |
  | :------:   | :------:| :------:  |
  |SP02        |!      |感叹号                         |
  |SC03        |$      |美元符号                              |
  |SP04        |"      |引号, 或双引号          |
  |SP05        |'      |撇号, 或 [单] 引号            |
  |SP06        |(      |左圆括号, 或开圆括号    |
  |SP07        |)      |右圆括号, 或闭圆括号  |
  |SP08        |,      |逗号                                    |
  |SP09        |_      |下划线, 或底部线                  |
  |SP10        |-      |连字符, 或减 [号]                  |
  |SP11        |.      |休止符, 句号, 或点                |
  |SP12        |/      |斜线, 或斜杠                        |
  |SP13        |:      |冒号                                    |
  |SP14        |;      |分号                                |
  |SP15        |?      |问号                            |
  |SA01        |+      |加 [号]                              |
  |SA03        |<      |小于 [号]                         |
  |SA04        |=      |等于 [号]                            |
  |SA05        |>      |大于 [号]                      |
  |SM01        |#      |数字符号, 或井[号]              |
  |SM02        |%      |百分 [号]                           |
  |SM03        |&      |和号                                |
  |SM04        |*      |星号, 或星                        |
  |SM05        |@      |单价记号, 或 at-号                |
  |SM06        |[      |左 [方] 括号                    |
  |SM07        |\      |反斜线号, 或反斜杠            |
  |SM08        |]      |右 [方] 括号                   |
  |SM11        |{      |左卷括号, 或左大括号        |
  |SM13        ||      |竖杠                             |
  |SM14        |}      |右卷括号, 或右大括号      |
  |SD13        |`      |沉音符, 或反引号               |
  |SD15        |^      |抑扬音符号                        |
  |SD19        |~      |波浪符号                                    |

Figure 2-5. 标准字符子表 (Part 3 of 3: 特殊字符)

这个图形ID(图形ID)在 Common Lisp 中不可用, 但是为了和 ISO 6937/2 交叉引用的目的而提供. 注意图形ID(图形ID)的第一个字母把字符分成以下几类: L---Latin, N---Numeric, S---Special.

### 2.1.4 <span id = "CharacterSyntaxTypes">字符语法类型</span>

Lisp读取器通过根据语法类型解释输入文本的每一个字符来构建一个对象. Lisp读取器不能接受Lisp打印器所生成的所有内容, 并且Lisp读取器包含了不能被Lisp打印器所使用的特性. Lisp读取器可以用作更通用的用户编写的解析器的词法分析器.

当Lisp读取器被调用, 它从输入流中读取单个字符并且根据这个字符的语法类型分派它. 每个出现在输入流中的字符都是Figure 2-6中出现的语法类型中的一个.

    constituent  macro character  single escape  
    invalid      multiple escape  whitespace[2]  

Figure 2-6. 可能的字符语法类型

一个字符在读取表中的语法类型决定了当这个读取表是当前读取表时Lisp读取器如何解释这个字符. 在任何给定的时间, 每个字符都有一个确定的语法类型.

Figure 2-7 列出了每个字符在标准语法中的语法类型.

    character  syntax type                 character  syntax type             
    Backspace  constituent                 0--9       constituent             
    Tab        whitespace[2]               :          constituent             
    Newline    whitespace[2]               ;          terminating macro char  
    Linefeed   whitespace[2]               <          constituent             
    Page       whitespace[2]               =          constituent             
    Return     whitespace[2]               >          constituent             
    Space      whitespace[2]               ?          constituent*            
    !          constituent*                @          constituent             
    "          terminating macro char      A--Z       constituent             
    \#          non-terminating macro char  [          constituent*            
    $          constituent                 \          single escape           
    %          constituent                 ]          constituent*            
    &          constituent                 ^          constituent             
    '          terminating macro char      _          constituent             
    (          terminating macro char      `          terminating macro char  
    )          terminating macro char      a--z       constituent             
    \*          constituent                 {          constituent*            
    \+          constituent                 |          multiple escape         
    ,          terminating macro char      }          constituent*            
    \-          constituent                 ~          constituent             
    .          constituent                 Rubout     constituent             
    /          constituent                 

Figure 2-7. 标准语法中的字符语法类型

用星号(*)标记的字符是最初的构成成分, 但是它们不被用于任何标准的Common Lisp标记中. 这些字符被显式地保留给程序员. ~ 不被用于 Common Lisp, 保留给实现者. $ 和 % 是字母字符, 但是不被用于任何标准Common Lisp定义的名字.

空白字符充当分隔符的作用但是被忽略. 组成和转义字符被累计起来，以使其成为一个被解释为一个数字或一个符号的token. 宏字符触发对函数的调用(可能是用户提供的)，可以执行任意的解析操作. 宏字符被分为2种, 终止和非终止的, 取决于它们是否会终结一个token. 以下是每一种语法类型的描述.

> * 2.1.4.1 [组成成分字符](#ConstituentCharacters)
> * 2.1.4.2 [组成成分特性](#ConstituentTraits)
> * 2.1.4.3 [非法字符](#InvalidCharacters)
> * 2.1.4.4 [宏字符](#MacroCharacters)
> * 2.1.4.5 [多重转义字符](#MultipleEscapeCharacters)
> * 2.1.4.6 [单转义字符](#SingleEscapeCharacter)
> * 2.1.4.7 [空白字符](#WhitespaceCharacters)

#### 2.1.4.1 <span id = "ConstituentCharacters">组成成分字符</spans>

组成成分字符被用于token中. 一个token表示为一个数字或符号. 字母和数字就是组成成分字符的示例.

当符号名被读取时, 其中的字母有时会被转换成大小写相反的字母; 见章节 23.1.2 (Effect of Readtable Case on the Lisp Reader). 大小写转换可以通过使用单个或多个转义符抑制.

#### 2.1.4.2 <span id = "ConstituentTraits">组成成分特性</span>

每个字符都有一个或多个构成特性定义了当这个字符是组成成分字符时如何被Lisp读取器解释. 这些构成特性是 alphabetic[2], digit, package marker, plus sign, minus sign, dot, decimal point, ratio marker, exponent marker, 还有 invalid. Figure 2-8 展示了标准的和不完全标准字符的构成特性; 不提供改变字符的构成特征的机制. 如果当前输入基数大于该字符的数字值, 那么任何具有数字(alphadigit)组成特征的字符都是一个数字, 否则字符是字母的(alphabetic). 任何字符被单个转义符引用, 不管它正常的语法, 都被当作 alphabetic[2] 组成成分.
                                                                                    
    constituent  traits          constituent  traits                                    
    character                    character    
    ----------
                                                                                    
    Backspace    invalid         {            alphabetic[2]                             
    Tab          invalid*        }            alphabetic[2]                             
    Newline      invalid*        +            alphabetic[2], plus sign                  
    Linefeed     invalid*        -            alphabetic[2], minus sign                 
    Page         invalid*        .            alphabetic[2], dot, decimal point         
    Return       invalid*        /            alphabetic[2], ratio marker               
    Space        invalid*        A, a         alphadigit                                
    !            alphabetic[2]   B, b         alphadigit                                
    "            alphabetic[2]*  C, c         alphadigit                                
    #            alphabetic[2]*  D, d         alphadigit, double-float exponent marker  
    $            alphabetic[2]   E, e         alphadigit, float exponent marker         
    %            alphabetic[2]   F, f         alphadigit, single-float exponent marker  
    &            alphabetic[2]   G, g         alphadigit                                
    '            alphabetic[2]*  H, h         alphadigit                                
    (            alphabetic[2]*  I, i         alphadigit                                
    )            alphabetic[2]*  J, j         alphadigit                                
    *            alphabetic[2]   K, k         alphadigit                                
    ,            alphabetic[2]*  L, l         alphadigit, long-float exponent marker    
    0-9          alphadigit      M, m         alphadigit                                
    :            package marker  N, n         alphadigit                                
    ;            alphabetic[2]*  O, o         alphadigit                                
    <            alphabetic[2]   P, p         alphadigit                                
    =            alphabetic[2]   Q, q         alphadigit                                
    >            alphabetic[2]   R, r         alphadigit                                
    ?            alphabetic[2]   S, s         alphadigit, short-float exponent marker   
    @            alphabetic[2]   T, t         alphadigit                                
    [            alphabetic[2]   U, u         alphadigit                                
    \            alphabetic[2]*  V, v         alphadigit                                
    ]            alphabetic[2]   W, w         alphadigit                                
    ^            alphabetic[2]   X, x         alphadigit                                
    _            alphabetic[2]   Y, y         alphadigit                                
    `            alphabetic[2]*  Z, z         alphadigit                                
    |            alphabetic[2]*  Rubout       invalid                                   
    ~            alphabetic[2]   
                                                                                    
Figure 2-8. 标准字符和不完全标准字符的构成成分特性

这个表中的解释方式只应用于语法类型为constituent的字符. 标记了星号 (*) 的条目正常是被屏蔽的因为这些字符是 空格(whitespace), 宏字符(macro character), 单转义(single escape), 或者 多重转义(multiple escape) 语法类型; 如果它们的语法类型改变为组成成分(constituent)这些组成特性才适用于它们. 

#### 2.1.4.3 <span id = "InvalidCharacters">非法字符</span>

具有组成成分无效(invalid)的字符不能出现在token里, 除非在单转义字符的控制下. 当读取时遇到一个非法的字符, 会发出一个 reader-error 类型的错误. 如果一个非法字符前有一个转义符, 它会被当作 alphabetic[2] constituent. 

#### 2.1.4.4 <span id = "MacroCharacters">宏字符</span>

当Lisp读取器从输入流中读入一个宏字符, 将对输入流中的后续字符进行特殊解析.

一个宏字符有一个关联的函数称之为读取器宏函数实现了它专门的解析行为. 一个这样的关联可以在一个规范的程序中通过使用函数 set-macro-character 和 set-dispatch-macro-character 被确定和修改.

遇到一个宏字符时, 这个Lisp读取器会调用它的读取器宏函数, 由它从输入流中解析一个经过特殊格式化的对象. 这个函数也返回解析后的对象, 或者它不返回表示函数扫描的字符被忽略了 (比如, 在注释的情况下). 宏字符的示例是反引号(backquote), 单引号(single-quote), 左小括号(left-parenthesis), 还有右小括号(right-parenthesis).

宏字符要么是终止，要么是非终止. 终止和非终止宏字符的区别在于当这些字符出现在token中间时，会发生什么. 如果一个非终止的宏字符出现在token中, 这个非终止宏字符关联的函数不会被调用, 并且这个非终止的宏字符不终结这个token的名字; 它就像是一个真的组成成分字符(constituent character) 一样成为这个名字的一部分. 一个终止宏字符会终结任何token, 并且它关联的读取宏函数会被调用, 无论这个字符出现在哪里. 在标准语法中唯一个非终结宏字符是#号(sharpsign).

如果一个字符是一个调度宏字符 C1, 它的读取器宏函数是具体实现提供的函数. 这个函数读取十进制数字字符直到读取到一个非数字的C2. 如果读取到任何数字, 它们转化为一个对应的整数中缀的参数数 P; 否则, 这个中缀参数 P 就是 nil. 这个终止的非数字 C2 是一个与调度宏字符 C1 相关联的调度表中查找到的字符 (有时被称为"子字符"，以强调其在调度中的从属角色) . 与子字符 C2 关联的这个读取器宏函数调用需要三个参数: 流, 子字符 C2, 还有中缀参数 P. 关于调度字符的更多信息, 见函数 set-dispatch-macro-character.

关于标准语法中可用的宏字符的信息, 见章节 2.4 (Standard Macro Characters). 

#### 2.1.4.5 <span id = "MultipleEscapeCharacters">多重转义字符</span>

一对多重转义字符用于指明一个可能包含宏字符和空白字符的闭合字符序列当作保持大小写的字母字符. 在序列中出现的任何单转义字符和多重转义字符都必须有一个转义字符.

垂直条(Vertical-bar)是标准语法中的一个多重转义字符.

##### 2.1.4.5.1 多重转义字符的示例

```LISP
 ;; The following examples assume the readtable case of *readtable* 
 ;; and *print-case* are both :upcase.
 (eq 'abc 'ABC) =>  true
 (eq 'abc '|ABC|) =>  true
 (eq 'abc 'a|B|c) =>  true
 (eq 'abc '|abc|) =>  false
```

#### 2.1.4.6 <span id = "SingleEscapeCharacter">单转义字符</span>

一个单转义字符被用于表明下一个字符被当作字母字符处理, 保留大小写, 无论它是什么字符或者它由什么组成成分特性.

反斜杠(backslash) 是标准语法中一个单转义字符.

##### 2.1.4.6.1 单转义字符示例

```LISP
 ;; The following examples assume the readtable case of *readtable* 
 ;; and *print-case* are both :upcase.
 (eq 'abc '\A\B\C) =>  true
 (eq 'abc 'a\Bc) =>  true
 (eq 'abc '\ABC) =>  true
 (eq 'abc '\abc) =>  false
```

#### 2.1.4.7 <span id = "WhitespaceCharacters">空白字符</span>

空白字符被用于分割token.

空格(Space) 和 新行(newline) 是标准语法中的空白字符.

##### 2.1.4.7.1 空白字符的示例

```LISP
 (length '(this-that)) =>  1
 (length '(this - that)) =>  3
 (length '(a
           b)) =>  2
 (+ 34) =>  34
 (+ 3 4) =>  7
```

## 2.2 <span id = "ReaderAlgorithm">读取器算法</span>

这个章节描述了Lisp读取器用来解析输入字符流中对象的算法规则, 包括Lisp读取器如何处理宏字符.

当处理token时, 读取器的基本功能是区分符号和数字的表示. 当累积到一个token时, 如果它满足 Figure 2-9 中列出的数字语法就被假定为数字. 如果它不是表示一个数字, 但它满足潜在数字的语法规则就会被当作潜在数字. 如果一个有效token既不是数字表示也不是一个潜在数字, 它表示一个符号.

Lisp读取器使用的算法规则如下:

1. 如果到了文件末尾, end-of-file 处理将按照读取时指定方法执行. 否则, 将从输入流中读取到一个字符, x, 并且根据它的语法类型去执行步骤2到7中的一个.

2. 如果 x 是一个无效字符, 发出一个 reader-error 类型的错误. 

3. 如果 x 是一个空白字符, 它会被丢弃并且会再次进入步骤1.

4. 如果 x 是一个终止或非终止的宏字符, 那么它的关联的读取器宏函数会被调用, 并且传入2个参数, 输入流 和 x.

    这个读取器宏函数可能从输入流中读取字符; 如果它这么做了, 会看到跟在那个宏字符后的字符. Lisp读取器可能会被这个读取器宏函数递归调用.

    除了对这个输入流以外, 这个读取器宏函数必须没有任何副作用; 由于对这个读取操作的跟踪和重启, Lisp读取器的前端 (比如, \``editors'' 和 ``rubout handlers'') 可能导致在x只出现一个的单个表达式的读取期间这个读取器宏函数被重复调用.

    这个读取器宏函数可能返回0个值或者一个值. 如果一个值被返回, 这个返回的值就是这个读取操作的结果; 这个读取算法规则结束. 如果没有值返回, 然后会再次进入步骤1.

5. 如果 x 是单转义字符, 那么下一个字符 y 会被读取或者在文件末尾时发出一个 end-of-file 类型的错误. y 被当作一个组成成分(constituent), 它的唯一的组成成分特性是字母的(alphabetic). y 被用于开始一个token, 并且进入步骤8.

6. 如果x是一个多重转义字符, 那么一个token(最初不包含字符)就会开始并且进入步骤9.

7. 如果 x 是一个组成成分字符(constituent character), 它会开始一个token. 在这个token被读取后, 它会被解释为一个Lisp对象或者一个无效的语法. 如果这个token表示一个对象, 就会返回这个对象作为这个读取操作的结果. 如果这个token是一个无效的字符, 就会发出一个错误. 如果x是一个小写的字符，它可能会被替换为相反情况的对应字符, 取决于当前读取表的情况, 就像章节 23.1.2 (Effect of Readtable Case on the Lisp Reader)阐述的一样. X 被用于开始一个token, 并且进入步骤8.

8. 此时已经累积到一个token, 并且遇到多个多重转义字符. 如果到了文件的末尾, 就进入步骤10. 否则, 一个字符, y, 被读入, 并且根据它的语法类型以下操作会被执行:

    * 如果 y 是一个组成成分(constituent) 或者一个非终止宏字符:

        -- 如果 y 是一个小写的字符，它可能会被替换为相反情况的对应字符, 取决于当前读取表的情况, 就像章节 23.1.2 (Effect of Readtable Case on the Lisp Reader)阐述的一样.
        -- Y 被追加到构建的对应token中.
        -- 重复步骤8.

    * 如果 y 是一个单转义字符, 那么下一个字符 z 会被读取或者在文件末尾时发出一个 end-of-file 类型的错误. z 被当作一个组成成分(constituent), 它的唯一的组成成分特性是字母的(alphabetic). Z 会被追加到构建的token中, 并且重复步骤8.

    * 如果 y 是一个多重转义字符, 就会进入步骤9.

    * 如果 y 是一个非法的字符, 一个 reader-error 类型的错误就会被发出.

    * 如果 y 是一个终止宏字符, 它会终止这个token. 首先这个字符 y 会被撤销读取, 然后进入步骤10.

    * 如果 y 是一个空白字符, 它会终止这个token. 首先如果合适的话这个字符 y 会被撤销读取(见 read-preserving-whitespace), 然后进入步骤10.

9. 此时，将会累积一个token，并且会遇到奇数个多重转义字符. 如果到了文件的末尾就会发出一个 end-of-file 类型的错误. 否则, 一个字符, y, 会被读取, 根据它的类型下面的其中一个动作会被执行:

    * 如果 y 是一个组成成分(constituent), 宏, 或者空格字符, y 被认为是一个组成成分(constituent), 并且它唯一的组成特性是字母(alphabetic). Y 被追加到构建的token中, 然后重复步骤9.

    * 如果 y 是一个单转义字符, 那么下一个字符 z 会被读取或者在文件末尾时发出一个 end-of-file 类型的错误. z 被当作一个组成成分(constituent), 它的唯一的组成成分特性是字母的(alphabetic). Z 会被追加到构建的token中, 并且重复步骤9.

    * 如果 y 是一个多重转义字符, 那么进入步骤8.

    * 如果 y 是个非法字符, 会发出一个类型为 reader-error 的错误.

10. 已经遇到一个完整的token. 这个token表示的对象作为这个读取操作的结果返回, 如果这个token是无效的就发出一个 reader-error 类型的错误. 

## 2.3 <span id = "InterpretationOfTokens">token的解释</span>

> * 2.3.1 [数字token](#NumbersAsTokens)
> * 2.3.2 [从token构建数字](#ConstructingNumbersFromTokens)
> * 2.3.3 [点对](#TheConsingDot)
> * 2.3.4 [符号token](#SymbolsAsTokens)
> * 2.3.5 [token的合法模式](#ValidPatternsForTokens)
> * 2.3.6 [包系统一致性规则](#PackageSystemConsistencyRules)

### 2.3.1 <span id = "NumbersAsTokens">数字token</span>

当一个token被读取, 它被解释为一个数字或符号. 如果这个token满足下面这段指定的数字语法, 它就被解释为数字.

```
numeric-token  ::=  integer |
				   ratio   |
				   float       
integer        ::=  [sign]
				   decimal-digit+
				   decimal-point |
				   [sign]
				   digit+      
ratio          ::=  [sign]
				   {digit}+
				   slash
				   {digit}+    
float          ::=  [sign]
				   {decimal-digit}*
				   decimal-point
				   {decimal-digit}+
				   [exponent]  
                    | 
				   [sign]
				   {decimal-digit}+
				   [decimal-point
					   {decimal-digit}*]
				   exponent    
exponent       ::=  exponent-marker
				   [sign]
				   {digit}+ 
```   
                                       
sign--- 一个正负号.                         
slash--- 一个斜杠                       
decimal-point--- 一个点.                        
exponent-marker--- 一个阶码标记符.                        
decimal-digit--- 一个基数10的数.                        
digit--- 一位当前输入基数的数.                        

Figure 2-9. 数字token的语法

#### 2.3.1.1 潜在的数字token

为了允许实现者和未来 Common Lisp 标准去扩展数字的语法, 定义了一个比数字语法更通用的潜在数字语法. 如果一个token满足下面的所有需求它就是一个潜在数字:

1. 这个token完全由数字, 正负号, 比标记, 小数点 (.), 扩展字符 (^ or _), 还有数字标记构成. 一个数字标记是一个字. 一个字目是否会被当作数字标记取决于上下文, 但是与其他字母相邻的字母不能被视为数字标记. 指数标记也是数字标记Exponent markers are number markers.

2. 这个token包含了至少一个数字. 根据当前的输入基数，字母可能被认为是数字，但只在没有小数点的token中.

3. 这个token以一个整数, 正负号, 小数点或扩展字符开始, 但是没有包标记. 潜在数字遵循的包含一个开始的包标记的语法还没有定义完善. 在一个阅读表达式的地方使用例如:1, :1/2, 还有 :2^3 的结果是未知的..

4. 这个token不能以一个正负号结尾.

如果一个潜在数字有数字语法, 这个数字在一个实现中是可被表示的, 会创建一个合适类型的数字并且返回. 如果一个数字超出了由具体实现相关的常量所设定的界限它在实现中是不能被表示的. 比如, 为浮点数指定太大或太小的指数可能会使该实现中的数字无法表示. 一个带有分母为0的比数 (比如 -35/000) 在任何实现中都是不能表示的. 当一个带有数字语法的token不能被转化为一个内置的数字, 会发出一个reader-error类型的错误. 为一个浮点数指定太多有效数字也肯定会发出一个错误; 应该生成一个截断的或者四舍五入的值.

如果出现字母是否应该被当作数字或数字标记的歧义, 那么字母就被当作数字来对待.

##### 2.3.1.1.1 转义字符和潜在数字

一个潜在数字不能包含任何转义字符. 一个转义字符剥夺了后面字符的所有句法特性，迫使其严格转成字母，因此不适合在潜在的数字中使用. 比如, 以下表示会被解释成符号, 而不是数字:

     \256   25\64   1.0\E6   |100|   3\.14159   |3/4|   3\/4   5||

在每一个示例中, 移除转义字符会导致token被转成潜在数字. 

##### 2.3.1.1.2 潜在数字示例

作为示例, 下面这一块的token是潜在数字, 但是它们实际上不是数字, 所以是保留的token; 一个合格的实现允许但不是必须去定义它们的意义.

    1b5000                       777777q                1.7J  -3/4+6.7J  12/25/83  
    27^19                        3^4/5                  6//7  3.1.2.6    ^-43^     
    3.141_592_653_589_793_238_4  -3.7+2.6i-6.17j+19.6k  

Figure 2-10. 保留token的示例

下面这段里的token不是潜在数字; 它们总是被当作符号:

    /     /5     +  1+  1-   
    foo+  ab.cd  _  ^   ^/-  

Figure 2-11. 符号的示例

如果当前的输入进制是16, 那么下面这段里的token就是潜在数字, 如果当前输入进制是10, 那么它们总是被当作符号.

    bad-face  25-dec-83  a/b  fad_cafe  f^  

Figure 2-12. 符号或潜在数字的示例

### 2.3.2 <span id = "ConstructingNumbersFromTokens">从token构建数字</span>

一个实数是由对应数字token直接构造的; 见 Figure 2-9.

一个复数表示为一个 #C (或 #c) 跟着两个实数的列表; 见章节 2.4.8.11 (Sharpsign C).

读取器宏 #B, #O, #X, 和 #R 对于控制解析的有理数的输入基数可能也是有用的; 见章节 2.4.8.7 (Sharpsign B), 章节 2.4.8.8 (Sharpsign O), 章节 2.4.8.9 (Sharpsign X), 还有章节 2.4.8.10 (Sharpsign R).

本节概述了数字的完整语法.

> * 2.3.2.1 [有理数语法](#SyntaxRational)
> * 2.3.2.2 [浮点数的语法](#SyntaxFloat)
> * 2.3.2.3 [复数的语法](#SyntaxComplex)

#### 2.3.2.1 <span id = "SyntaxRational">有理数语法</span>

##### 2.3.2.1.1 一个整数的语法

整数可以写成数字序列, 前面可以有一个符号, 然后还可以有一个小数点; 见 Figure 2-9. 当使用了一个小数点, 这个数字的基数为 10; 当没有使用小数点, 这个数字的基数就是当前的输入基数.

关于如何打印整数的信息, 见章节 22.1.3.1.1 (Printing Integers). 

##### 2.3.2.1.2 比数的语法

比率可以写成两个非空的数字序列, 由斜杠分隔, 前面可以有一个可选的正负号; 见 Figure 2-9. 第二个序列可能不是完全由0组成的. 下一段是比率的示例.

```LISP
2/3                 ;This is in canonical form                  
4/6                 ;A non-canonical form for 2/3               
-17/23              ;A ratio preceded by a sign                 
-30517578125/32768  ;This is (-5/2)^15                          
10/5                ;The canonical form for this is 2           
#o-101/75           ;Octal notation for -65/61                  
#3r120/21           ;Ternary notation for 15/7                  
#Xbc/ad             ;Hexadecimal notation for 188/173           
#xFADED/FACADE      ;Hexadecimal notation for 1027565/16435934  
```

Figure 2-13. 比率的示例

关于如何打印比率的信息, 见章节 22.1.3.1.2 (Printing Ratios). 

#### 2.3.2.2 <span id = "SyntaxFloat">浮点数的语法</span>

浮点数可以用小数或计算机科学符号来表示: 一个可选的正负号, 然后是一个带有内嵌小数点的非空序列, 然后是一个可选的十进制指数说明符. 如果没有指数说明符, 那么就需要一个小数点, 并且在它后面要有个数字. 这个指数说明符由一个指数标记, 一个可选的正负号, 还有一个非空的数字序列. 如果没有指数说明符, 或者指数说明符用的是 e (或 E), 使用 \*read-default-float-format* 指定的格式. 见 Figure 2-9.

一个具体实现可能提供一种或多种浮点数来共同构建浮点类型. 字母 s, f, d, 和 l (或者它们的大写等价体) 分别明确指定 short-float, single-float, double-float, 和 long-float 类型.

用于扩展表示的内部格式仅依赖于指数标记, 而不依赖于扩展表示的小数位数.

下面这块包含了浮点数的标记示例:

```LISP
0.0       ;Floating-point zero in default format                          
0E0       ;As input, this is also floating-point zero in default format.  
          ;As output, this would appear as 0.0.                           
0e0       ;As input, this is also floating-point zero in default format.  
          ;As output, this would appear as 0.0.                           
-.0       ;As input, this might be a zero or a minus zero,                
          ; depending on whether the implementation supports              
          ; a distinct minus zero.                                        
          ;As output, 0.0 is zero and -0.0 is minus zero.                 
0.        ;On input, the integer zero---not a floating-point number!      
          ;Whether this appears as 0 or 0. on output depends              
          ;on the value of *print-radix*.                                 
0.0s0     ;A floating-point zero in short format                          
0s0       ;As input, this is a floating-point zero in short format.       
          ;As output, such a zero would appear as 0.0s0                   
          ; (or as 0.0 if short-float was the default format).            
6.02E+23  ;Avogadro's number, in default format                           
602E+21   ;Also Avogadro's number, in default format                      
```

Figure 2-14. 浮点数示例

关于浮点数如何打印的信息, 见章节 22.1.3.1.3 (Printing Floats). 

#### 2.3.2.3 <span id = "SyntaxComplex">复数的语法</span>

一个复数有一个笛卡尔结构, 带有一个实数部分和一个虚数部分, 其中每一个部分都是实数表示. 复数的各个部分没有必要一定是浮点数但是每个部分必须是一样的类型: 都是有理数, 或者都是浮点数的子类型. 当构建一个复数时, 如果指定的部分不是相同的类型, 这些部分会被转换为相同的内部类型 (换句化说, 有理数部分被转化为一个浮点数). 一个复数类型的对象在内部被转换, 并且在它的虚部是一个值为0的整数时表示为一个有理数.

关于进一步的信息, 见章节 2.4.8.11 (Sharpsign C) 还有章节 22.1.3.1.4 (Printing Complexes). 

### 2.3.3 <span id = "TheConsingDot">点对</span>

如果一个token仅由点组成 (没有转义字符), 会发出一个 reader-error 类型的错误, 除了在一种情况下: 如果token是一个单点, 并且出现在点对点符号允许一个点这样的情况中, 那么它就被作为这种语法的一部分接受了, 并且没有发出错误信号. 见章节 2.4.1 (Left-Parenthesis). 

### 2.3.4 <span id = "SymbolsAsTokens">符号token</span>

任何不是潜在数字, 不包含包标记符, 也不是完全由点构成的token总是被解释为一个符号. 任何是一个潜在数字但是不符合数字语法的token作为保留token并且有着实现相关的解释. 在所有其他情况下，token被解释为一个符号的名称.

下一段中有符号的打印表示. 为了表示的简单性, 这些示例假定当前的读取表是 :upcase.

```LISP
FROBBOZ         The symbol whose name is FROBBOZ.                
frobboz         Another way to notate the same symbol.           
fRObBoz         Yet another way to notate it.                    
unwind-protect  A symbol with a hyphen in its name.              
+$              The symbol named +$.                             
1+              The symbol named 1+.                             
+1              This is the integer 1, not a symbol.             
pascal_style    This symbol has an underscore in its name.       
file.rel.43     This symbol has periods in its name.             
\(              The symbol whose name is (.                      
\+1             The symbol whose name is +1.                     
+\1             Also the symbol whose name is +1.                
\frobboz        The symbol whose name is fROBBOZ.                
3.14159265\s0   The symbol whose name is 3.14159265s0.           
3.14159265\S0   A different symbol, whose name is 3.14159265S0.  
3.14159265s0    A possible short float approximation to <PI>.    
```

Figure 2-15. 符号的打印表示示例 (Part 1 of 2)

```LISP
APL\\360            The symbol whose name is APL\360.       
apl\\360            Also the symbol whose name is APL\360.  
\(b^2\)\-\4*a*c     The name is (B^2) - 4*A*C.              
                    Parentheses and two spaces in it.       
\(\b^2\)\-\4*\a*\c  The name is (b^2) - 4*a*c.              
                    Letters explicitly lowercase.           
|"|                 The same as writing \".                 
|(b^2) - 4*a*c|     The name is (b^2) - 4*a*c.              
|frobboz|           The name is frobboz, not FROBBOZ.       
|APL\360|           The name is APL360.                     
|APL\\360|          The name is APL\360.                    
|apl\\360|          The name is apl\360.                    
|\|\||              Same as \|\| ---the name is ||.         
|(B^2) - 4*A*C|     The name is (B^2) - 4*A*C.              
                    Parentheses and two spaces in it.       
|(b^2) - 4*a*c|     The name is (b^2) - 4*a*c.              
```

Figure 2-16. 符号的打印表示示例 (Part 2 of 2)

在解析一个符号的过程中, 它是依赖于实现的, 由实现定义的属性从字符中删除, 形成一个表示符号的token.

在解析这个符号的语法时, Lisp读取器会在当前包中查阅这个符号的名字. 这个查找可能牵涉到查找其他的包, 这些包的外部符号是由当前的包继承的. 如果找到这个名字, 返回对应的符号. 如果没有找到这个名字 (这就表示当前包中没有这个名字的符号), 会创建一个新的符号并作为内部符号放到这个包中. 当前包成为这个符号的拥有者 (home package), 并且这个符号成为当前包中内置的. 如果这个名字后面被读取到时当前包是同一个, 这个相同的符号会被找到并返回. 

### 2.3.5 <span id = "ValidPatternsForTokens">token的合法模式</span>

token的合法模式总结在下面这段.

    nnnnn              a number                                           
    xxxxx              a symbol in the current package                    
    :xxxxx             a symbol in the the KEYWORD package                
    ppppp:xxxxx        an external symbol in the ppppp package            
    ppppp::xxxxx       a (possibly internal) symbol in the ppppp package  
    :nnnnn             undefined                                          
    ppppp:nnnnn        undefined                                          
    ppppp::nnnnn       undefined                                          
    ::aaaaa            undefined                                          
    aaaaa:             undefined                                          
    aaaaa:aaaaa:aaaaa  undefined                                          

Figure 2-17. token的合法模式

注意, nnnnn有数字语法, xxxxx和ppppp都有数字语法, aaaaa有任何语法.

后面有关于包标记的规则总结. 在每一个情况后面, 都会提供一个例子阐明这个情况; 为了表达的简单性, 示例假定当前的读取表是 :upcase.

1. 如果这里有一个单个的包标记, 并且它出想在这个token的开始, 那么这个token被解释为包KEYWORD中的符号. 它还将新创建的符号的符号值设置为相同的符号, 以使符号可以自求值.

    比如, :bar, 读取的时候, 把 BAR 作为 KEYWORD 包的扩展符号.

2. 如果这里有一个包标记, 但不是在token的开头, 这个token会被拆成2个部分. 第一个部分指定一个包; 第二个部分就成为这个包中的扩展符号的名字.

    比如, foo:bar, 读取的时候, 在名为 FOO 的包中查找 外部符号 BAR.

3. 如果这里由两个相邻的包标记, 并且不是在token的开始, 那么它们会被分为两个部分. 第一部分指定一个包; 第二个部分就成为这个包中的扩展符号的名字 (可能是一个内部的符号).

    比如, foo::bar, 读取的时候, 把 BAR 放到名为 FOO 的包中.

4. 如果这个token没有包含包标记, 并且没有潜在的数字语法, 那么这个整个token就是这个符号的名字. 从当前包中查找这个符号.

    比如, bar, 读取的时候, 把 BAR 放到当前包中.

5. 如果其他包标记的模式被使用那么结果是不确定的. 所有其他使用在符号名中的包标记没有被这个标准所定义但是保留给具体实现.

比如, 假定当前的读取表的大小写情况是 :upcase, editor:buffer 引用包名为 editor 中的名为 BUFFER 的符号, 不管当前包中是否有一个名为 BUFFER 的符号. 如果没有名为 editor 的包, 或者包 editor 中没有名为 BUFFER 的符号, 或者 BUFFER 没有被 editor 包导出, 读取器就会发出一个可矫正的错误. 如果见到 editor::buffer, 其效果与使用 EDITOR 包作为当前包的读取缓冲区完全相同. 

### 2.3.6 <span id = "PackageSystemConsistencyRules">包系统一致性规则</span>

只要 \*package* 的值没有变化那么以下规则就适用于包系统:

Read-read consistency

    读取相同的符号名称总是会产生相同的符号.

Print-read consistency

    一个被扣押的符号总是打印为一个字符序列, 当读取回来时也是相同的符号.

    关于Lisp打印器如何对待符号, 见章节 22.1.3.3 (Printing Symbols).

Print-print consistency

    如果两个不同的被扣押的符号, 那么它们的打印表示会是不同的字符序列.

不管任何隐式扣押, 这些规则都是对的. 只要当前包没有改变, 不管加载文件的顺序或在什么时候输入符号的确切历史, 结果都是可复写的. 如果 \*package* 的值被改变了并且改变回一个之前的值, 还是会保持一致性. 可以通过修改 \*package* 的值, 通过从一个错误中 continue 强制改变符号或者包或者都修改, 或者调用以下函数中的一个来违反这个规则: unintern, unexport, shadow, shadowing-import, 或者 unuse-package.

只有当两个被命名的符号之间的限制被违反时, 不一致性才适用. shadow, unexport, unintern, 还有 shadowing-import 只影响作为参数传递的名字相同(用 string= 判断)的符号. 

## 2.4 <span id = "StandardMacroCharacters">标准宏字符</span>

如果读取器遇到一个宏字符, 那么它的关联读取器宏函数会被调用并且可能产生一个返回的对象. 这个函数可能读取流中宏字符后面任何语法的字符并且返回这个语法对应的对象.

任何字符都可以作为宏字符. 一个合格的实现定义的宏字符包括以下这些:

> * 2.4.1 [左括号](#LeftParenthesis)
> * 2.4.2 [右括号](#RightParenthesis)
> * 2.4.3 [单引号](#SingleQuote)
> * 2.4.4 [分号](#Semicolon)
> * 2.4.5 [双引号](#DoubleQuote)
> * 2.4.6 [反引号](#Backquote)
> * 2.4.7 [逗号](#Comma)
> * 2.4.8 [井号](#Sharpsign)
> * 2.4.9 [重复读取缩写的表达式](#ReReadingAbbreviatedExpressions)
 
### 2.4.1 <span id = "LeftParenthesis">左括号</span>

左括号开始一个列表的读取. read 会被递归调用去读取连续的对象直到在流中找到一个右括号. 返回一个读取的对象列表. 因此

```LISP
 (a b c)
```

被读取为一个三个对象的列表 (符号 a, b, 还有 c). 右括号不需要紧跟着最后一个对象的打印形式后面; 空格字符和注释可能在它之前.

如果在右括号前没有对象, 它就读取一个0对象的列表 (一个空列表).

如果一个token只是一个点，而不是在一个转义字符之前就被读取，那么在某个对象之后再读取一个对象，那么就会有另一个对象紧跟这个点，然后可能是在空白字符或注释后面，后面跟着右括号:

```LISP
 (a b c . d)
```

这就意味着这个list最后一个cons的cdr部分不是nil, 而是点之后表示的对象. 上面的例子可能是下面表达式的结果

```LISP
 (cons 'a (cons 'b (cons 'c 'd)))
```

类似的,

```LISP
 (cons 'this-one 'that-one) =>  (this-one . that-one)
```

跟在点后面的对象也允许是一个列表:

```LISP
 (a b c d . (e f . (g))) ==  (a b c d e f g)
```

关于Lisp打印器如何打印列表和cons的信息, 见章节 22.1.3.5 (Printing Lists and Conses). 

### 2.4.2 <span id = "RightParenthesis">右括号</span>

除了和一个左括号字符结合, 右括号是非法. 关于更多的信息, 见章节 2.2 (Reader Algorithm). 

### 2.4.3 <span id = "SingleQuote">单引号</span>

语法: '<\<exp>>

一个单引号表示一个表达式被 ``quoted''. 单引号后面跟着一个表达式 exp 会被Lisp读取器当作一个缩写并且并被解析为同等表达式的缩写 (quote exp). 见特殊操作符 quote.

#### 2.4.3.1 单引号的示例

```LISP
 'foo =>  FOO
 ''foo =>  (QUOTE FOO)
 (car ''foo) =>  QUOTE
```

### 2.4.4 <span id = "Semicolon">分号</span>

语法: ;<<text>>

一个分号表示字符需要被忽略, 就像注释. 分号和所有的字符, 包括下一个换行符或文件的结尾都被忽略了.

#### 2.4.4.1 分号的示例

```LISP
 (+ 3 ; three
    4)
=>  7  
```  

#### 2.4.4.2 关于分号样式的注释

一些文本编辑器根据开始注释的分号数量来对期望的缩进做出假设. 下面的样式惯例是较通用的，尽管不是普遍的.

> * 2.4.4.2.1 [单分号的使用](#UseOfSingleSemicolon)
> * 2.4.4.2.2 [两个分号的使用](#UseOfDoubleSemicolon)
> * 2.4.4.2.3 [三个分号的使用](#UseOfTripleSemicolon)
> * 2.4.4.2.4 [四个分号的使用](#UseOfQuadrupleSemicolon)
> * 2.4.4.2.5 [分号风格的示例](#ExamplesOfStyleForSemicolon)

##### 2.4.4.2.1 <span id = "UseOfSingleSemicolon">单分号的使用</span>

以一个分号开始的注释都对齐到右边的同一列上 (有时也被称为 ``comment column''). 这样一个注释的文本通常值应用于它出现的行. 偶尔会有两个或三个一起包含单个句子的; 这有时被用来表示除了第一个带了额外的空格 (在分号后) 其他都是缩进. 

##### 2.4.4.2.2 <span id = "UseOfDoubleSemicolon">两个分号的使用</span>

以双分号开头的注释都对齐到相同的缩进水平，就像表单在代码中处于相同的位置. 这样的注释的文本通常用来描述注释出现点的程序的状态, 或者这个注释后的代码, 或者都描述了. 

##### 2.4.4.2.3 <span id = "UseOfTripleSemicolon">三个分号的使用</span>

以三个分号开头的注释都对齐到左边框. 通常它们是在定义或定义集之前使用的, 而不是定义在定义中. 

##### 2.4.4.2.4 <span id = "UseOfQuadrupleSemicolon">四个分号的使用</span>

以四个个分号开头的注释都对齐到左边框, 并且通常包含一小段文本作为后面跟着的代码的标题, 并且可能被用于这个程序的页眉或页脚, 作为代码的一个硬拷贝文档. 

##### 2.4.4.2.5 <span id = "ExamplesOfStyleForSemicolon">分号风格的示例</span>

```LISP
;;;; Math Utilities

;;; FIB computes the the Fibonacci function in the traditional
;;; recursive way.

(defun fib (n)
  (check-type n integer)
  ;; At this point we're sure we have an integer argument.
  ;; Now we can get down to some serious computation.
  (cond ((< n 0)
         ;; Hey, this is just supposed to be a simple example.
         ;; Did you really expect me to handle the general case?
         (error "FIB got ~D as an argument." n))
        ((< n 2) n)             ;fib[0]=0 and fib[1]=1
        ;; The cheap cases didn't work.
        ;; Nothing more to do but recurse.
        (t (+ (fib (- n 1))     ;The traditional formula
              (fib (- n 2)))))) ; is fib[n-1]+fib[n-2].
```

### 2.4.5 <span id = "DoubleQuote">双引号</span>

语法: "<<text>>"

双引号被用于开始或结束一个字符串. 当遇到一个双引号, 从输入流中读取到的字符会累积直到遇到另一个双引号. 如果见到一个单转义字符, 这个单转义字符会被丢弃, 累积下一个字符并继续. 直到匹配的双引号为止的字符会被转成一个简单字符串并返回. 累积字符的哪些属性会在这个操作中被移除是取决于具体实现的.

下一段中有双引号字符的示例.

```LISP
"Foo"                      ;A string with three characters in it  
""                         ;An empty string                       
"\"APL\\360?\" he cried."  ;A string with twenty characters       
"|x| = |-x|"               ;A ten-character string                
```

Figure 2-18. 双引号字符的示例

注意, 要将单个转义字符或双引号放入字符串中, 这样的字符必须先有一个转义字符. 还要注意, 多重转义字符不需要被字符串中的单转义字符引用.

关于Lisp打印器如何打印字符串的信息, 见章节 22.1.3.4 (Printing Strings). 

### 2.4.6 <span id = "Backquote">反引号</span>

反引号表示一个要创建的数据结构的模板. 比如, 写下

```LISP
`(cond ((numberp ,x) ,@y) (t (print ,x) ,@y))
```

粗略等价于写下

```LISP
(list 'cond 
    (cons (list 'numberp x) y) 
    (list* 't (list 'print x) y))
```

这个模板中出现逗号的地方, 这个逗号后面的表达式会被求值并产生一个对象插入到这个地方. 假定 b 的值是3, 比如求值 `(a b ,b ,(+ b 1) b) 表示的结构会产生结果 (a b 3 4 b).

如果一个逗号紧跟着 @, 这个 @ 符号后面的表达式形式会被求值并产生一个对象列表. 这些对象会被拼接进这个模板. 比如, 如果 x 的值为 (a b c), 那么

```LISP
`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
=>  (x (a b c) a b c foo b bar (b c) baz b c)
```

这个反引号语法可以被总结为如下.

* \`basic 等同于 'basic, 也就表示, (quote basic), 对于上面任何表达式 basic 都不是一个列表或序列.

* `,form 等同于 form, 对于任何 form, 提供没有以 @ 或者 点 开始的 form 的表示. (对于一个逗号后面的所有出现的情况, 也有类似的警告.)

* `,@form 有着不可预计的结果.

* `(x1 x2 x3 ... xn . atom) 可能被解释成

     (append [ x1] [ x2] [ x3] ... [ xn] (quote atom))

    其中方括号被用于表示如下的一个 xj 的转化:

    -- [form] 被解释为 (list `form), 包含一个反引号的 form 必须继续被解释.

    -- [,form] 被解释为 (list form).

    -- [,@form] 被解释为 form.

* \`(x1 x2 x3 ... xn) 可能被解释为相同的反引号表达式 \`(x1 x2 x3 ... xn . nil), 因此将其减少到前一种情况.

* `(x1 x2 x3 ... xn . ,form) 可能被解释为

     (append [ x1] [ x2] [ x3] ... [ xn] form)

    其中的方括号表示如上所述的 xj 的转换.

* \`(x1 x2 x3 ... xn . ,@form) 有着不可预计的结果.

* \`#(x1 x2 x3 ... xn) 可能被解释为 (apply #'vector `(x1 x2 x3 ... xn)).

任何可能使用 \``,@'' 的地方, 可能使用 \``,.'' 来表示允许在 \``,.'' 后面的表达式形式的列表结构中对列表结构进行破坏性的操作 (实际上, 去使用 nconc 而不是 append).

如果反引号语法是嵌套的，那么最内层的反引号表达式形式应该首先展开. 这个也意味着如果有多个逗号出现在一行中, 最左边的一个是属于最里面的反引号.

一个具体实现可以自由地解释一个反引号表达式形式 F1 为任何表达式形式 F2, 在求值后, 会产生一个适用于上面定义的结果, 提供的副作用行为替代形式F2也符合上述描述. 模板的构造拷贝可能和模板自身共享列表的结构. 就像例子中, 上面的定义意味着

```LISP
`((,a b) ,c ,@d)
```

会被解释为就像

```LISP
(append (list (append (list a) (list 'b) 'nil)) (list c) d 'nil)
```

但是它也可能被合理地解释为以下任何一种:

```LISP
(append (list (append (list a) (list 'b))) (list c) d)
(append (list (append (list a) '(b))) (list c) d)
(list* (cons a '(b)) c d)
(list* (cons a (list 'b)) c d)
(append (list (cons a '(b))) (list c) d)
(list* (cons a '(b)) c (copy-list d))
```

#### 2.4.6.1 关于反引号的注意事项

因为Lisp读取器解析一个包含反引号读取宏的表达式的确切方式没有指定, 一个具体实现可以自由地选择任何保留描述的语法的表示形式.

通常, 实现会选择一种便于打印表达式的表示形式, 这样 (pprint \`(a ,b)) 会显示 \`(a ,b) 而不是比如 (list 'a b). 然而, 这不是必须的.

没有特定理由做出选择的实现者可能希望引用IEEE标准来实现Scheme编程语言, Scheme确定了一种流行的表示方式, 它可以为一些用户社区提供有用的兼容性. 没有任何要求, 然而, 任何符合规范的实现都使用这个特定的表示. 此信息仅用于交叉引用目的.

### 2.4.7 <span id = "Comma">逗号</span>

逗号是反引号语法的一部分; 见章节 2.4.6 (Backquote). 如果逗号被用于非上面所述的反引号表达式语法中是非法的. 

### 2.4.8 <span id = "Sharpsign">井号</span>

井号是一个非终止调度宏字符. 它读取一个可选的数字序列和至少一个字符, 然后使用这个字符去选择一个函数作为读取器宏函数来运行.

标准语法包括由 # 字符引入的结构. 这些结构的语法如下: 标识结构类型的字符后面跟着一些表达式形式的参数. 如果这个字符是一个字母, 它的大小写是不重要的;比如 #O 和 #o 被认为是等价的.

某些 # 构造允许在 # 和字符之间出现一个无符号的十进制数.

和调度宏字符 # 关联的读取器宏在这个章节的后面部分有描述, 并且总结在下面这段.

    dispatch char  purpose                  dispatch char  purpose                
    Backspace      signals error            {              undefined*             
    Tab            signals error            }              undefined*             
    Newline        signals error            +              read-time conditional  
    Linefeed       signals error            -              read-time conditional  
    Page           signals error            .              read-time evaluation   
    Return         signals error            /              undefined              
    Space          signals error            A, a           array                  
    !              undefined*               B, b           binary rational        
    "              undefined                C, c           complex number         
    #              reference to = label     D, d           undefined              
    $              undefined                E, e           undefined              
    %              undefined                F, f           undefined              
    &              undefined                G, g           undefined              
    '              function abbreviation    H, h           undefined              
    (              simple vector            I, i           undefined              
    )              signals error            J, j           undefined              
    *              bit vector               K, k           undefined              
    ,              undefined                L, l           undefined              
    :              uninterned symbol        M, m           undefined              
    ;              undefined                N, n           undefined              
    <              signals error            O, o           octal rational         
    =              labels following object  P, p           pathname               
    >              undefined                Q, q           undefined              
    ?              undefined*               R, r           radix-n rational       
    @              undefined                S, s           structure              
    [              undefined*               T, t           undefined              
    \              character object         U, u           undefined              
    ]              undefined*               V, v           undefined              
    ^              undefined                W, w           undefined              
    _              undefined                X, x           hexadecimal rational   
    `              undefined                Y, y           undefined              
    |              balanced comment         Z, z           undefined              
    ~              undefined                Rubout         undefined              

Figure 2-19. 标准 # 调度宏字符语法

由星号(\*)标记的组合被显式地保留给用户. 没有符合规范的实现定义它们.

注意数字也没有出现在之前的表中. 这是因为标记 #0, #1, ..., #9 保留给另一个占相同句法空间的目的. 当一个数字跟着一个井号, 它不会被认为是调度字符. 取而代之的是, 一个无符号整型的参数被累计起来, 并作为参数传递给数字后面字符的读取器宏. 比如, #2A((1 2) (3 4)) 就是一个参数为 2 的 #A 的使用.

> * 2.4.8.1 [井号反斜线(#\\)](#SharpsignBackslash)
> * 2.4.8.2 [井号单引号(#')](#SharpsignSingleQuote)
> * 2.4.8.3 [井号左括号(#()](#SharpsignLeftParenthesis)
> * 2.4.8.4 [井号星号(#*)](#SharpsignAsterisk)
> * 2.4.8.5 [井号冒号(#:)](#SharpsignColon)
> * 2.4.8.6 [井号点(#.)](#SharpsignDot)
> * 2.4.8.7 [井号B(#B)](#SharpsignB)
> * 2.4.8.8 [井号O(#O)](#SharpsignO)
> * 2.4.8.9 [井号X(#X)](#SharpsignX)
> * 2.4.8.10 [井号R(#R)](#SharpsignR)
> * 2.4.8.11 [井号C(#C)](#SharpsignC)
> * 2.4.8.12 [井号A(#A)](#SharpsignA)
> * 2.4.8.13 [井号S(#S)](#SharpsignS)
> * 2.4.8.14 [井号P(#P)](#SharpsignP)
> * 2.4.8.15 [井号等号(#=)](#SharpsignEqualSign)
> * 2.4.8.16 [井号井号(##)](#SharpsignSharpsign)
> * 2.4.8.17 [井号加号(#+)](#SharpsignPlus)
> * 2.4.8.18 [井号减号(#-)](#SharpsignMinus)
> * 2.4.8.19 [井号竖线(#|)](#SharpsignVerticalBar)
> * 2.4.8.20 [井号小于号(#<)](#SharpsignLessThanSign)
> * 2.4.8.21 [井号空格(# )](#SharpsignWhitespace)
> * 2.4.8.22 [井号右括号(#))](#SharpsignRightParenthesis)

#### 2.4.8.1 <span id = "SharpsignBackslash">井号反斜线(#\\)</span>

语法: #\\<\<x>>

当一个 token x 是单个字符长时, 这个会被解析为字面字符 char. 在 #\ 后面大小写字母是区分开来的; #\A 和 #\a 表示不同的字符对象. 任何在 #\ 后面的单个字符都会正常工作, 甚至那些读取时非常特殊的, 例如左括号和右括号.

在单字符情况下, x 后面必须跟着一个非构成字符. 在 #\ 被读取之后, 读取器退到斜线前然后开始读取一个token, 把最初的斜线作为单转义字符 (不管它是否真的在当前的读取表里).

当这个 token 不止一个字符长度时, 这个 x 必须有着符号的语法, 并且其中没有包标记符. 在这种情况下, 这个井号反斜线标记被解析为名为 (string-upcase x) 的字符; 见章节 13.1.7 (Character Names).

关于Lisp打印器如何打印字符对象的信息, 见章节 22.1.3.2 (Printing Characters). 

#### 2.4.8.2 <span id = "SharpsignSingleQuote">井号单引号(#')</span>

任何前面有 #' (井号后面是单引号) 的表达式, 就像 #'expression, 被Lisp读取器当作是一个缩写并且解释为表达式 (function expression). 见 function. 比如,

```LISP
(apply #'+ l) ==  (apply (function +) l)
```

#### 2.4.8.3 <span id = "SharpsignLeftParenthesis">井号左括号(#()</span>

\#( and ) 被用于表示一个简单序列.

如果一个无符号十进制整数出现在 # 和 ( 中间, 它明确指明这个序列的长度. 如果在结束的 ) 之前的对象数超过那个无符号十进制整数, 结果是无法预料的. 在结束的 ) 之前提供的对象数量如果小于那个无符号十进制整数大于0那么最后一个对象被用于填充这个序列的剩余部分. 如果这个无符号十进制整数是非空的但是在结束的 ) 之前提供的对象数是0那么结果是未定义的. 比如,

```LISP
#(a b c c c c)
#6(a b c c c c)
#6(a b c)
#6(a b c c)
```

都意味着同样的东西: 一个长度为6的序列, 其中有a, b, 和4个 c 作为其中的元素. 其他例子如下:

```LISP
#(a b c)               ;A vector of length 3
#(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
                    ;A vector containing the primes below 50
#()                    ;An empty vector
```

标记 #() 表示一个空序列, 就像 #0().

关于Lisp打印器如何打印序列的信息, 见章节 22.1.3.4 (Printing Strings), 章节 22.1.3.6 (Printing Bit Vectors), 或章节 22.1.3.7 (Printing Other Vectors). 

#### 2.4.8.4 <span id = "SharpsignAsterisk">井号星号(#*)</span>

语法: #*<\<bits>>

一个简单的包含指定的位 (0's 和 1's) 的位序列会被构建, 其中最左边的位索引为 0 并且后面的位有增长的索引.

语法: #<\<n>>*<\<bits>>

带有一个参数 n, 这个要创建的序列的长度是 n. 如果位数少于 n 但是大于 0, 那么最后一位会被用于填充这个位序列的剩余部分.

标记 #* 和 #0* 每个都表示一个空的位序列.

不管是否提供那个可选的参数 n,星号后面的token会被正常的token分隔符来分割. 然而, (除非 \*read-suppress* 的值是 true) 如果这个token不是完全由 0 和 1 组成, 或者如果提供了 n 但是位数却大于 n 位, 或者 n 大于等于 1, 但是没有提供元素, 都会发出一个 reader-error 类型的错误. 单转义字符和多重转义字符都不允许出现在这个token里.

关于Lisp打印器如何打印位字符的信息, 见章节 22.1.3.6 (Printing Bit Vectors).

##### 2.4.8.4.1 井号星号的示例

比如,

```LISP
#*101111
#6*101111
#6*101
#6*1011
```

都意味着相同的东西: 一个长度6的序列, 带有元素 1, 0, 1, 1, 1, 和 1.

比如:

```LISP
 #*         ;An empty bit-vector
```

#### 2.4.8.5 <span id = "SharpsignColon">井号冒号(#:)</span>

语法: #:<\<symbol-name>>

\#: 引入一个名字为 symbol-name 的未拘留的符号. 每次遇到这个语法, 会创建一个不同的未拘留的符号. 这个 symbol-name 必须有符号的语法并且没有包标记符.

关于Lisp读取器如何打印未拘留的符号, 见章节 22.1.3.3 (Printing Symbols). 

#### 2.4.8.6 <span id = "SharpsignDot">井号点(#.)</span>

\#.foo 被读取为 foo 表示的对象求值的结果. 当读取到 #. 标记, 求值会在读取过程中完成. 因此这个 #. 语法执行了一个 foo 的读取时求值.

当 \*read-eval* 是 false 时, 这个 #. 正常的影响会被抑制. 在这个情况下, 会发出一个 reader-error 类型的错误.

对于一个没有适当的打印表示的对象, 计算对象的表达式形式可以使用符号 #. 表示. 

#### 2.4.8.7 <span id = "SharpsignB">井号B(#B)</span>

\#Brational 读取二进制有理数. 比如,

```LISP
#B1101 ==  13 ;11012
#b101/11 ==  5/3
```

如果紧跟这个 #B 的 token没有二进制的语法, 那么结果是未定义的. 

#### 2.4.8.8 <span id = "SharpsignO">井号O(#O)</span>

\#Orational 读取八进制有理数. 比如,

```LISP
#o37/15 ==  31/13
#o777 ==  511
#o105 ==  69 ;1058
```

如果紧跟在 #O 后的token没有八进制有理数的语法, 结果是未定义的. 

#### 2.4.8.9 <span id = "SharpsignX">井号X(#X)</span>

\#Xrational 读取十六进制有理数. 在 9 之上的数字是字母 A 到 F (小写字母 a 到 f 也是可接受的). 比如,

```LISP
#xF00 ==  3840             
#x105 ==  261 ;10516
```

如果紧跟着 #X 的token没有十六进制有理数的语法, 那么结果是未定义的. 

#### 2.4.8.10 <span id = "SharpsignR">井号R(#R)</span>

\#nR

\#radixRrational 根据指定的进制来读取有理数. 进制必须是十进制有理数的整数; 它的值必须在 2 和 36 (包括)之间. 只有指定的进制中合法的数字可以被使用.

比如, #3r102 是另一种写 11(十进制) 的方式, 并且 #11R32 是写 35(十进制) 的另一种方式. 对于进制数大于 10, 字母表中的字母按顺序被用于 9 之后的进制. 对于十进制数没有替代的 # 表示法.

下一段包括了 #B, #O, #X, 和 #R 的使用.

```LISP
#2r11010101  ;Another way of writing 213 decimal  
#b11010101   ;Ditto                               
#b+11010101  ;Ditto                               
#o325        ;Ditto, in octal radix               
#xD5         ;Ditto, in hexadecimal radix         
#16r+D5      ;Ditto                               
#o-300       ;Decimal -192, written in base 8     
#3r-21010    ;Same thing in base 3                
#25R-7H      ;Same thing in base 25               
#xACCEDED    ;181202413, in hexadecimal radix     
```

Figure 2-20. 进制指示器的例子

如果跟在 #nR 后的token不满足 n 进制有理数的语法, 结果是未定义的. 

#### 2.4.8.11 <span id = "SharpsignC">井号C(#C)</span>

\#C 读取一个跟在后面的对象, 这个对象必须是一个长度为 2 且其中的元素都是实数的列表. 这两个实数分别表示一个复数的实部和虚部. 如果这两部分不是相同的数据类型, 那么它们会根据章节 12.1.1.2 (Contagion in Numeric Operations) 描述的浮点数转换的规则被转换.

\#C(real imag) 等价于 #.(complex (quote real) (quote imag)), 除了这个 #C 不会被 \*read-eval* 影响. See the function complex.

下一段中包含了 #C 的使用示例.

```LISP
#C(3.0s1 2.0s-1)  ;A complex with small float parts.                
#C(5 -3)          ;A ``Gaussian integer''                           
#C(5/3 7.0)       ;Will be converted internally to #C(1.66666 7.0)  
#C(0 1)           ;The imaginary unit; that is, i.                  
```

Figure 2-21. 复数示例

关于更多信息, 见章节 22.1.3.1.4 (Printing Complexes) 和章节 2.3.2.3 (Syntax of a Complex). 

#### 2.4.8.12 <span id = "SharpsignA">井号A(#A)</span>

\#nA

\#nAobject 构建一个n维数组, 使用 object 作为 :initial-contents 参数的值调用 make-array.

比如, #2A((0 1 5) (foo 2 (hot dog))) 表示一个 2-by-3 矩阵:

```
0       1       5
foo     2       (hot dog)
```

相对的, #1A((0 1 5) (foo 2 (hot dog))) 表示一个长度 2 的序列, 其中的元素是列表:

```LISP
(0 1 5) (foo 2 (hot dog))
```

\#0A((0 1 5) (foo 2 (hot dog))) 表示一个0维数组, 其中其中仅有的元素是一个列表:

```LISP
((0 1 5) (foo 2 (hot dog)))
```

\#0A foo 表示一个0维数组, 其中仅有的元素是 foo. 这个 #1A foo 标记是非法的因为 foo 不是一个序列.

如果一些数组的维度表示被解析为0, 所有的维度 (也就是说, 更高的维度) 也被认为是 0.

关于Lisp打印器如何打印数组的信息, 见章节 22.1.3.4 (Printing Strings), Section 22.1.3.6 (Printing Bit Vectors), Section 22.1.3.7 (Printing Other Vectors), 或者 Section 22.1.3.8 (Printing Other Arrays). 

#### 2.4.8.13 <span id = "SharpsignS">井号S(#S)</span>

\#s(name slot1 value1 slot2 value2 ...) 表示一个结构. 如果 name 是一个 defstruct 已经定义的结构的名字 并且如果这个结构的类型有标准构造函数, 这也是有效的. 让 cm 成为这个构造器函数的 name; 那么这个语法等价于

```LISP
#.(cm keyword1 'value1 keyword2 'value2 ...)
```

这里每一个 keywordj 都是计算下面表达式的结果

```LISP
(intern (string slotj) (find-package 'keyword))
```

其净效果是构造器函数被调用, 其中指定的槽具有指定的值. (这个强制特性被弃用; 在未来, keyword 的名字将会在他们被读入的包中被取出, 因此如果这是需要的, 那么实际上在 KEYWORD 包中的符号应该被使用.)

无论构造器函数返回的是什么对象都会通过 #S 语法.

关于Lisp打印器如何打印结构信息, 见章节 22.1.3.12 (Printing Structures). 

#### 2.4.8.14 <span id = "SharpsignP">井号P(#P)</span>

\#P 读取后面的对象, 它必须是一个字符串.

\#P<\<expression>> 等价于 #.(parse-namestring '<\<expression>>), 除了 #P 不受 \*read-eval* 影响.

关于Lisp打印器如何打印一个路径名的信息, 见章节 22.1.3.11 (Printing Pathnames). 

#### 2.4.8.15 <span id = "SharpsignEqualSign">井号等号(#=)</span>

\#n=

\#n=object 读取这个对象无论它是否是他的打印表示. 但是, 该对象是由n所标记的, 这是一个必需的无符号小数, 可以通过语法 #n# 进行引用. 这个标记的作用域是被读取的最外面调用的表达式; 这个表达式里, 相同的标记可能不会出像第二次. 

#### 2.4.8.16 <span id = "SharpsignSharpsign">井号井号(##)</span>

\#n#

\#n#, 其中的 n 是一个必须的无符号整数, 提供一个对 #n= 标记的对象的引用; 这就是说, #n# 表示一个指向 #n= 标记的对象相同 (eq) 对象的指针. 比如, 变量 y 的结构会被这个代码创建出来:

```LISP
(setq x (list 'p 'q))
(setq y (list (list 'a 'b) x 'foo x))
(rplacd (last y) (cdr y))
```

可以通过以下方式表示:

```LISP
((a b) . #1=(#2=(p q) foo #2# . #1#))
```

在这种表示下,如果 \*print-length* 设置为 10 并且 \*print-circle* 设置为 nil, 结构会被以下面方式打印:

```LISP
((a b) (p q) foo (p q) (p q) foo (p q) (p q) foo (p q) ...)
```

一个 #n# 引用可能只出现在一个 #n= 标记后; 超前的引用是不允许的. 这个引用可能不会出现在这个对象自身的标记中 (就是说, #n=#n#) 因为这个 #n= 标记的对象在这个情况下还没有被定义好. 

#### 2.4.8.17 <span id = "SharpsignPlus">井号加号(#+)</span>

\#+ 提供一个读取时条件化机制; 语法是 #+test expression. 如果这个特性 expression 测试成功, 那么这个文本标记表述一个打印表示是一个表达式的对象. 如果这个特性 expression 测试失败, 那么这个文本的标记就当作空白字符对待; 这就是说, 就好象 ``#+ test expression'' 不会出现并且只有一个空格出现在它的位置.

关于这个特性表达式测试的成功与否的详细描述, 见章节 24.1.2.1 (Feature Expressions).

#+ 通过第一次读取这个特性表达式时操作并且如果测性表达式测试失败就会跳回这个表达式. 读取这个 test 时, 当前包是 KEYWORD 包. 跳过这个表达式形式是通过将 \*read-suppress* 绑定到 true 然后调用 read 来完成的.

关于示例, 见章节 24.1.2.1.1 (Examples of Feature Expressions). 

#### 2.4.8.18 <span id = "SharpsignMinus">井号减号(#-)</span>

\#- 就像 #+ 除了它会在那个测试正确是跳过这个表达式; 也就是说,

```LISP
#-test expression ==  #+(not test) expression
```

关于示例, 见章节 24.1.2.1.1 (Examples of Feature Expressions). 

#### 2.4.8.19 <span id = "SharpsignVerticalBar">井号竖线(#|)</span>

\#|...|# 被读取器当作是一个注释. 它必须与 #| 和 |# 的出现保持平衡, 但是除此之外, 它可能包含任何字符.

##### 2.4.8.19.1 竖线的示例

The following are some examples that exploit the #|...|# notation:

```LISP
;;; In this example, some debugging code is commented out with #|...|#
;;; Note that this kind of comment can occur in the middle of a line
;;; (because a delimiter marks where the end of the comment occurs)
;;; where a semicolon comment can only occur at the end of a line 
;;; (because it comments out the rest of the line).
 (defun add3 (n) #|(format t "~&Adding 3 to ~D." n)|# (+ n 3))

;;; The examples that follow show issues related to #| ... |# nesting.

;;; In this first example, #| and |# always occur properly paired,
;;; so nesting works naturally.
 (defun mention-fun-fact-1a ()
   (format t "CL uses ; and #|...|# in comments."))
=>  MENTION-FUN-FACT-1A
 (mention-fun-fact-1a)
>>  CL uses ; and #|...|# in comments.
=>  NIL
 #| (defun mention-fun-fact-1b ()
      (format t "CL uses ; and #|...|# in comments.")) |#
 (fboundp 'mention-fun-fact-1b) =>  NIL

;;; In this example, vertical-bar followed by sharpsign needed to appear
;;; in a string without any matching sharpsign followed by vertical-bar
;;; having preceded this.  To compensate, the programmer has included a
;;; slash separating the two characters.  In case 2a, the slash is 
;;; unnecessary but harmless, but in case 2b, the slash is critical to
;;; allowing the outer #| ... |# pair match.  If the slash were not present,
;;; the outer comment would terminate prematurely.
 (defun mention-fun-fact-2a ()
   (format t "Don't use |\# unmatched or you'll get in trouble!"))
=>  MENTION-FUN-FACT-2A
 (mention-fun-fact-2a)
>>  Don't use |# unmatched or you'll get in trouble!
=>  NIL
 #| (defun mention-fun-fact-2b ()
      (format t "Don't use |\# unmatched or you'll get in trouble!") |#
 (fboundp 'mention-fun-fact-2b) =>  NIL

;;; In this example, the programmer attacks the mismatch problem in a
;;; different way.  The sharpsign vertical bar in the comment is not needed
;;; for the correct parsing of the program normally (as in case 3a), but 
;;; becomes important to avoid premature termination of a comment when such 
;;; a program is commented out (as in case 3b).
 (defun mention-fun-fact-3a () ; #|
   (format t "Don't use |# unmatched or you'll get in trouble!"))
=>  MENTION-FUN-FACT-3A
 (mention-fun-fact-3a)
>>  Don't use |# unmatched or you'll get in trouble!
=>  NIL
 #|
 (defun mention-fun-fact-3b () ; #|
   (format t "Don't use |# unmatched or you'll get in trouble!"))
 |#
 (fboundp 'mention-fun-fact-3b) =>  NIL
```

##### 2.4.8.19.2 井号竖线风格的注意事项

一些声称理解 Lisp 语法的文本编辑器把任何 |...| 作为不能嵌套的对称对 (就像它们只是对使用在标注符号的多重转义字符对做了对称). 为了弥补这一缺陷, 一些程序员使用 #||...#||...||#...||# 而不是 #|...#|...|#...|#. 注意这个替代方式并不是一个不同的读取器宏; 它只是利用了额外的垂直条在注释中出现的事实, 这一方法可以让特定的文本编辑器以更好地支持嵌套的注释. 像这样, 一个可能的代码:

```LISP
#|| (+ #|| 3 ||# 4 5) ||# 
```

这样的代码等价于:

```LISP
#| (+ #| 3 |# 4 5) |#
```

#### 2.4.8.20 <span id = "SharpsignLessThanSign">井号小于号(#<)</span>

\#< 不是一个合法的读取器语法. Lisp读取器在遇到 #< 时会发出一个 reader-error 类型的错误. 这个语法通常被用于不能被读回的对象的打印表示. 

#### 2.4.8.21 <span id = "SharpsignWhitespace">井号空格(# )</span>

\# 后面紧跟空格键不是一个合法的读取器宏. 如果Lisp读取器遇到 #<Newline> 或 #<Space> 会发出一个 reader-error 类型的错误. 

#### 2.4.8.22 <span id = "SharpsignRightParenthesis">井号右括号(#))</span>

这不是一个合法的读取器语法.

如果Lisp读取器遇到一个 #) 会发出一个 reader-error 类型的错误. 

### 2.4.9 <span id = "ReReadingAbbreviatedExpressions">重复读取缩写的表达式</span>

注意, 当读取一个由于 \``..'', \``...'', \``#'' 后面跟着空格和 \``#)'' 长度或级别 (见 \*print-level\*, \*print-length\*, 和 \*print-lines\*) 限制而被简化的表达式时，Lisp读取器通常会发出一个 reader-error 类型的错误. 



