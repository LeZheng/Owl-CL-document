# 2. 语法

> * 2.1 [字符语法](#CharacterSyntax)
> * 2.2 [读取器算法](#ReaderAlgorithm)
> * 2.3 [token 标记的解释](#InterpretationOfTokens)
> * 2.4 [标准宏字符](#StandardMacroCharacters)

## 2.1 <span id = "CharacterSyntax">字符语法</span>

Lisp 读取器[Lisp reader]从一个流[stream]中读取字符[character], 然后将其解释为一个对象[object]的打印表示, 构建这个对象[object]并且返回它.

这个章节描述的语法称之为标准语法[standard syntax]. Common Lisp 提供了对应操作, 因此可以在程序的控制下修改读取表[readtable]所表示的语法信息的各个方面; 见章节 23 (读取器). 除了明确说明的以外, 这个文档中使用的就是标准语法[standard syntax].

> * 2.1.1 [读取表](#Readtables)
> * 2.1.2 [影响 Lisp 读取器的变量](#VariablesAffectReader)
> * 2.1.3 [标准字符](#StandardCharacters)
> * 2.1.4 [字符语法类型](#CharacterSyntaxTypes)

### 2.1.1 <span id = "Readtables">读取表</span>

Lisp 读取器[Lisp reader]使用的语法信息体现在一个称之为读取表[readtable]的对象[object]中. 此外, 这个读取表[readtable]还包含字符[character]和语法类型[syntax type]之间的关联.

下一块列出了一些适用于读取表的已定义的名字[defined name].

    *readtable*                    readtable-case                
    copy-readtable                 readtablep                    
    get-dispatch-macro-character   set-dispatch-macro-character  
    get-macro-character            set-macro-character           
    make-dispatch-macro-character  set-syntax-from-char          

    Figure 2-1. 读取表已定义的名字

> * 2.1.1.1 [当前的读取表](#CurrentReadtable)
> * 2.1.1.2 [标准读取表](#StandardReadtable)
> * 2.1.1.3 [初始读取表](#InitialReadtable)

#### 2.1.1.1 <span id = "CurrentReadtable">当前的读取表</span>

可以存在一些描述不同语法的读取表[readtable], 但是在任何给定的时间内都只存在一个读取表影响着 Lisp 读取器[Lisp reader]把表达式[expression]解析为对象[object]的方式, 称之为当前读取表[current readtable]. 在一个给定的动态环境[dynamic environment]中的当前读取表[current readtable]是这个环境[environment]中的 \*readtable* 的值[value]. 为了使一个不同的读取表[readtable]成为当前的读取表[current readtable], \*readtable* 可以被赋值或绑定[bound]. 

#### 2.1.1.2 <span id = "StandardReadtable">标准读取表</span>

这个标准读取表[standard readtable]符合标准语法[standard syntax]. 如果尝试去修改标准读取表[standard readtable], 后果是未定义的. 为了实现修改或者扩展标准语法[standard syntax]的效果, 可以去创建一个标准读取表[standard readtable]的副本; 见函数[function] copy-readtable.

标准读取表[standard readtable]的读取表大小写模式[readtable case]是 :upcase. 

#### 2.1.1.3 <span id = "InitialReadtable">初始读取表</span>

初始读取表[initial readtable]是这个 Lisp 镜像[Lisp image]开始时的当前读取表[current readtable]. 在那个时候, 它符合标准语法[standard syntax]. 初始读取表[initial readtable]不同[distinct]于标准读取表[standard readtable]. 一个符合规范的程序[conforming program]去修改初始读取表[initial readtable]是允许的. 

### 2.1.2 <span id = "VariablesAffectReader">影响 Lisp 读取器的变量</span>

Lisp 读取器[Lisp reader]不止受当前读取表[current readtable]所影响, 也被很多动态变量[dynamic variable]所影响. 下面这段就列出了这些影响 Lisp 读取器[Lisp reader]行为的变量[variable].

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

Lisp 读取器[Lisp reader]通过根据语法类型[syntax type]解释输入文本的每一个字符[character]来构建一个对象[object]. Lisp 读取器[Lisp reader]不能接受 Lisp 打印器[Lisp printer]所生成的所有内容, 并且 Lisp 读取器[Lisp reader]包含了不能被 Lisp 打印器[Lisp printer]所使用的特性. Lisp 读取器[Lisp reader]可以用作更通用的用户编写的解析器的词法分析器.

当 Lisp 读取器[Lisp reader]被调用, 它从输入[input]流[stream]中读取单个字符并且根据这个字符[character]的语法类型[syntax type]分派它. 每个出现在输入[input]流[stream]中的字符[character]都是 Figure 2-6 中出现的语法类型[syntax type]中的一个.

    constituent  macro character  single escape  
    invalid      multiple escape  whitespace[2]  

    Figure 2-6. 可能的字符语法类型

一个字符[character]在读取表[readtable]中的语法类型[syntax type]决定了当这个读取表[readtable]是当前读取表[current readtable]时 Lisp 读取器[[Lisp reader]]如何解释这个字符. 在任何给定的时间, 每个字符都有一个确定的语法类型.

Figure 2-7 列出了每个字符[character]在标准语法[standard syntax]中的语法类型[syntax type].

  字符       | 语法类型                    | 字符       | 语法类型  
  | :------:| :------:                   | :------:  | :------:  |           
  Backspace | constituent                | 0--9      | constituent             
  Tab       | whitespace[2]              | :         | constituent             
  Newline   | whitespace[2]              | ;         | terminating macro char  
  Linefeed  | whitespace[2]              | <         | constituent             
  Page      | whitespace[2]              | =         | constituent             
  Return    | whitespace[2]              | >         | constituent             
  Space     | whitespace[2]              | ?         | constituent*            
  !         | constituent*               | @         | constituent             
  "         | terminating macro char     | A--Z      | constituent             
  \#         | non-terminating macro char | [         | constituent*            
  $         | constituent                | \         | single escape           
  %         | constituent                | ]         | constituent*            
  &         | constituent                | ^         | constituent             
  '         | terminating macro char     | _         | constituent             
  (         | terminating macro char     | `         | terminating macro char  
  )         | terminating macro char     | a--z      | constituent             
  \*         | constituent                | {         | constituent*            
  \+         | constituent                | |         | multiple escape         
  ,         | terminating macro char     | }         | constituent*            
  \-         | constituent                | ~         | constituent             
  .         | constituent                | Rubout    | constituent             
  /         | constituent                |           |

    Figure 2-7. 标准语法中的字符语法类型

用星号(*)标记的字符是最初的标记成分[constituent], 但是它们不被用于任何标准的 Common Lisp 标记中. 这些字符被明确地地保留给程序员[programmer]. ~ 不被用于 Common Lisp, 保留给实现者. $ 和 % 是字母[alphabetic[2]]字符[character], 但是不被用于任何标准 Common Lisp 已定义名字[defined name]的名字.

空白[Whitespace[2]]字符充当分隔符, 但在其他情况下被忽略. 标记成分[constituent]和转义[escape]字符[character]被累计起来, 以使其成为一个被解释为一个数字[number]或一个符号[symbol]的标记[token]. 宏字符[macro character]触发对函数[function]的调用(可能是用户提供的), 可以执行任意的解析操作. 宏字符[macro character]被分为2种, 终止[terminating]和非终止[non-terminating]的, 取决于它们是否会终结一个标记[token]. 以下是每一种语法类型[syntax type]的描述.

> * 2.1.4.1 [标记成分字符](#ConstituentCharacters)
> * 2.1.4.2 [标记成分特质](#ConstituentTraits)
> * 2.1.4.3 [非法字符](#InvalidCharacters)
> * 2.1.4.4 [宏字符](#MacroCharacters)
> * 2.1.4.5 [多转义字符](#MultipleEscapeCharacters)
> * 2.1.4.6 [单转义字符](#SingleEscapeCharacter)
> * 2.1.4.7 [空白字符](#WhitespaceCharacters)

#### 2.1.4.1 <span id = "ConstituentCharacters">标记成分字符</spans>

标记成分[constituent]字符[character]被用于标记[token]中. 一个标记[token]是一个数字[number]或符号[symbol]的表示. 字母和数字就是标记成分[constituent]字符[character]的示例.

当符号名被读取时, 其中的字母有时会被转换成大小写[case]相反的字母; 见章节 23.1.2 (Lisp 读取器上的读取表大小写的影响). 大小写[case]转换可以通过使用单转义[single escape]或多转义[multiple escape]字符抑制.

#### 2.1.4.2 <span id = "ConstituentTraits">标记成分特质</span>

每个字符[character]都有一个或多个标记成分特质[constituent trait], 它们定义了当这个字符[character]是标记成分[constituent]字符[character]时如何被 Lisp 读取器[Lisp reader]解释. 这些标记成分特质[constituent trait]是字母[alphabetic[2]], 数字, 包标记[package marker], 加号, 减号, 点, 小数点, 比率标记[ratio marker], 指数标记[exponent marker], 还有非法[invalid]. Figure 2-8 展示了标准字符[standard characters]和不完全标准[semi-standard]字符[character]的标记成分特质[constituent trait]; 没有为改变字符[character]的标记成分特质[constituent trait]提供机制. 如果当前输入基数[current input base]大于一个字符的数字值, 那么任何具有字母数字(alphadigit)标记成分特质[constituent trait]的字符[character]都是一个数字, 否则字符[character]是字母[alphabetic[2]]. 任何被单转义符[single escape]引用的字符, 不管它正常的语法, 都被当作字母[alphabetic[2]]标记成分.
                                                                                    
  标记成分字符 | 特质                    | 标记成分字符 | 特质  
  | :------:| :------:                   | :------:  | :------:  |  
  Backspace |   invalid                | {           |  alphabetic[2]
  Tab       |   invalid*               | }           |  alphabetic[2] 
  Newline   |   invalid*               | +           |  alphabetic[2], plus sign   
  Linefeed  |   invalid*               | -           |  alphabetic[2], minus sign
  Page      |   invalid*               | .           |  alphabetic[2], dot, decimal point  
  Return    |   invalid*               | /           |  alphabetic[2], ratio marker  
  Space     |   invalid*               | A, a        |  alphadigit 
  !         |   alphabetic[2]          | B, b        |  alphadigit  
  "         |   alphabetic[2]*         | C, c        |  alphadigit    
  \#         |   alphabetic[2]*         | D, d        |  alphadigit, double-float exponent marker
  $         |   alphabetic[2]          | E, e        |  alphadigit, float exponent marker 
  %         |   alphabetic[2]          | F, f        |  alphadigit, single-float exponent marker
  &         |   alphabetic[2]          | G, g        |  alphadigit  
  '         |   alphabetic[2]*         | H, h        |  alphadigit  
  (         |   alphabetic[2]*         | I, i        |  alphadigit   
  )         |   alphabetic[2]*         | J, j        |  alphadigit    
  \*         |   alphabetic[2]          | K, k        |  alphadigit 
  ,         |   alphabetic[2]*         | L, l        |  alphadigit, long-float exponent marker 
  0-9       |   alphadigit             | M, m        |  alphadigit 
  :         |   package marker         | N, n        |  alphadigit 
  ;         |   alphabetic[2]*         | O, o        |  alphadigit   
  <         |   alphabetic[2]          | P, p        |  alphadigit 
  =         |   alphabetic[2]          | Q, q        |  alphadigit   
  \>         |   alphabetic[2]          | R, r        |  alphadigit   
  ?         |   alphabetic[2]          | S, s        |  alphadigit, short-float exponent marker
  @         |   alphabetic[2]          | T, t        |  alphadigit    
  [         |   alphabetic[2]          | U, u        |  alphadigit   
  \         |   alphabetic[2]*         | V, v        |  alphadigit  
  ]         |   alphabetic[2]          | W, w        |  alphadigit  
  ^         |   alphabetic[2]          | X, x        |  alphadigit 
  _         |   alphabetic[2]          | Y, y        |  alphadigit    
  `         |   alphabetic[2]*         | Z, z        |  alphadigit   
  |         |   alphabetic[2]*         | Rubout      |  invalid    
  ~         |   alphabetic[2]          |             |
                                                                                    
Figure 2-8. 标准字符和不完全标准字符的构成成分特性

这个表中的解释方式只应用于语法类型[syntax type]为标记成分[constituent]的字符[character]. 标记了星号 (*) 的条目正常是被屏蔽[shadow[2]]的, 因为这些表示的字符[character]是空白[whitespace], 宏字符[macro character], 单转义[single escape], 或者多转义[multiple escape]语法类型[syntax type]; 只有当它们的语法类型被改变为标记成分[constituent]时, 这些标记成分特质[constituent trait]才适用于它们. 

#### 2.1.4.3 <span id = "InvalidCharacters">非法字符</span>

带有标记成分特质[constituent trait]无效[invalid]的字符[character]不能出现在标记[token]里, 除非在单转义[single escape]字符[character]的控制下. 如果一个对象[object]被读取时遇到一个无效[invalid]字符[character], 会发出一个 reader-error 类型[type]的错误. 如果一个无效[invalid]字符[character]前有一个单转义[single escape]字符[character], 它会被当作字母[alphabetic[2]]标记成分[constituent]. 

#### 2.1.4.4 <span id = "MacroCharacters">宏字符</span>

当 Lisp 读取器[Lisp reader]从输入[input]流[stream]中读入一个宏字符[macro character]时, 将对输入[input]流[stream]中的后续字符[character]进行特殊解析.

一个宏字符[macro character]有一个关联的函数[function]称之为读取器宏函数[reader macro function]实现了它专门的解析行为. 一个这样的关联可以在一个符合规范的程序[conforming program]的控制下通过使用函数[function] set-macro-character 和 set-dispatch-macro-character 来建立和修改.

遇到一个宏字符[macro character]时, 这个 Lisp 读取器[Lisp reader]会调用它的读取器宏函数[reader macro function], 它从输入[input]流[stream]中解析一个经过特殊格式化的对象. 这个函数[function]也返回解析后的对象[object], 或者它不返回值[value]表示函数[function]扫描的这些字符被忽略了 (比如, 在注释的情况下). 宏字符[macro character]的示例是反引号[backquote], 单引号[single-quote], 左圆括号[left-parenthesis], 还有右圆括号[right-parenthesis].

宏字符[macro character]要么是终止的[terminating], 要么是非终止的[non-terminating]. 终止的[terminating]和非终止的[non-terminating]宏字符[macro character]的区别在于当这些字符出现在标记[token]中间时, 会发生什么. 如果一个非终止的[non-terminating]宏字符[macro character]出现在标记[token]中, 这个非终止的[non-terminating]宏字符[macro character]关联的函数[function]不会被调用, 并且这个非终止的[non-terminating]宏字符[macro character]不终结这个标记[token]的名字; 它就好像这个宏字符[macro character]真的是一个标记成分字符一样成为这个名字的一部分. 一个终止的[terminating]宏字符[macro character]会终结任何标记[token], 并且它关联的读取器宏函数[reader macro function]会被调用, 无论这个字符[character]出现在哪里. 在标准语法[standard syntax]中唯一个非终止的[non-terminating]宏字符[macro character]是#号[sharpsign].

如果一个字符[character]是一个分派宏字符[dispatching macro character] C1, 它的读取器宏函数[reader macro function]是一个具体实现[implementation]提供的函数[function]. 这个函数[function]读取十进制数字[digit]字符[character]直到读取到一个非数字的 C2. 如果读取到任何数字[digit], 它们转化为一个对应的整数[integer]中缀的参数 P; 否则, 这个中缀参数 P 就是 nil. 这个终止的非数字 C2 是一个与分派宏字符[dispatching macro character] C1 相关联的分派表中查找到的字符 (有时被称为"子字符(sub-character)", 以强调其在分派中的从属角色) . 与子字符 C2 关联的这个读取器宏函数[[reader macro function]]调用需要三个参数: 流[stream], 子字符 C2, 还有中缀参数 P. 关于分派字符的更多信息, 见函数[function] set-dispatch-macro-character.

关于标准语法[standard syntax]中可用的宏字符[macro character]的信息, 见章节 2.4 (标准宏字符). 

#### 2.1.4.5 <span id = "MultipleEscapeCharacters">多转义字符</span>

一对多转义[multiple escape]字符[character]用于指明一个可能包含宏字符[macro character]和空白[whitespace[2]]字符[character]的闭合字符序列被认为是保持大小写[case]的字母[alphabetic[2]]字符[character]. 在序列中出现的任何单转义字符和多转义字符都必须有一个转义字符.

竖杠[vertical-bar]是标准语法[standard syntax]中的一个多转义[multiple escape]字符[character].

##### 2.1.4.5.1 多转义字符的示例

```LISP
 ;; The following examples assume the readtable case of *readtable* 
 ;; and *print-case* are both :upcase.
 (eq 'abc 'ABC) =>  true
 (eq 'abc '|ABC|) =>  true
 (eq 'abc 'a|B|c) =>  true
 (eq 'abc '|abc|) =>  false
```

#### 2.1.4.6 <span id = "SingleEscapeCharacter">单转义字符</span>

一个单转义符[single escape]被用于表明下一个字符[character]被当作字母[alphabetic[2]]字符[character]处理, 保留大小写[case], 无论它是什么字符[character]或者它由什么标记成分特质[constituent trait].

反斜杠[backslash]是标准语法[standard syntax]中一个单转义[single escape]字符[character].

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

空白[whitespace[2]]字符[character]被用于分隔多个标记[token].

空格[space]和换行[newline]是标准语法[standard syntax]中的空白[whitespace[2]]字符[character].

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

这个章节描述了 Lisp 读取器[Lisp reader]用来从输入[input]字符[character]流[stream]中解析对象[object]的算法规则, 包括 Lisp 读取器[Lisp reader]如何处理宏字符[macro character].

当处理标记[token]时, 读取器的基本功能是区分符号[symbol]和数字[number]的表示. 当累积到一个标记[token]时, 如果它满足 Figure 2-9 中列出的数字语法就被假定为数字[number]. 如果它不是表示一个数字[number], 但它满足潜在数字[potential number]的语法规则就会被当作潜在数字[potential number]. 如果一个有效标记[token]既不是数字[number]表示也不是一个潜在数字[potential number], 它表示一个符号[symbol].

Lisp 读取器[Lisp reader]使用的算法规则如下:

1. 如果到了文件末尾, end-of-file 处理将按照 read 中所指定的执行. 否则, 将从输入[input]流[stream]中读取到一个字符[character], x, 并且根据 x 的语法类型[syntax type]被分派到 2 至 7 中的一个步骤.

2. 如果 x 是一个无效[invalid]字符[character], 发出一个 reader-error 类型[type]的错误. 

3. 如果 x 是一个空白[whitespace[2]]字符[character], 它会被丢弃并且会再次进入步骤 1.

4. 如果 x 是一个终止的[terminating]或非终止的[non-terminating]宏字符[macro character], 那么它的关联的读取器宏函数[reader macro function]会用两个实参[argument], 输入[input]流[stream] 和 x, 来调用.

    这个读取器宏函数[reader macro function]可能从输入[input]流[stream]中读取字符[character]; 如果它这么做了, 会看到跟在那个宏字符[macro character]后的那些字符[character]. Lisp 读取器[Lisp reader]可能会从这个读取器宏函数[reader macro function]被递归调用.

    除了在这个输入[input]流[stream]以外, 这个读取器宏函数[reader macro function]一定没有任何副作用; 由于对这个 read 操作的回溯和重启, Lisp 读取器[Lisp reader]的前端 (比如, "编辑器(editor)" 和 "抹掉处理程序(rubout handler)") 可能导致在 x 只出现一次的单个表达式[expression]的读取期间这个读取器宏函数[reader macro function]被重复调用.

    这个读取器宏函数[reader macro function]可能返回零个或者一个值. 如果返回一个值, 那么这个返回的值就是这个读取操作的结果; 这个读取算法规则结束. 如果没有返回值, 然后会再次进入步骤 1.

5. 如果 x 是一个单转义[single escape]字符[character], 那么下一个字符[character] y 会被读取或者在文件末尾时发出一个 end-of-file 类型[type]的错误. y 被当作一个标记成分[constituent], 它的唯一的标记成分特质[constituent trait]是字母[alphabetic[2]]. y 被用于开始一个标记[token], 并且进入步骤 8.

6. 如果 x 是一个多转义[multiple escape]字符[character], 那么就会开始一个标记[token] (初始不包含字符[character])并且进入步骤 9.

7. 如果 x 是一个标记成分[constituent]字符[character], 它就会开始一个标记[token]. 在这个标记[token]被读取后, 它会被解释为一个 Lisp 对象[object]或者一个无效的语法. 如果这个标记[token]表示一个对象[object], 就会返回这个对象[object]作为这个读取操作的结果. 如果这个标记[token]是一个无效的语法, 就会发出一个错误. 如果 x 是一个带有大小写[case]的字符[character], 它可能会被替换为相反大小写[case]的对应字符[character], 取决于当前读取表[current readtable]的读取表[readtable case], 就像章节 23.1.2 (Lisp 读取器上的读取表大小写的影响) 阐述的一样. x 被用于开始一个标记[token], 并且进入步骤 8.

8. 此时正在累积一个标记[token], 并且已经遇到偶数个多转义[multiple escape]字符[character]. 如果到了文件的末尾, 就进入步骤 10. 否则, 一个字符[character], y, 被读入, 并且根据它的语法类型[syntax type]以下操作之一会被执行:

    * 如果 y 是一个标记成分[constituent] 或者一个非终止[non-terminating]宏字符[macro character]:

        -- 如果 y 是一个带有大小写[case]的字符[character], 它可能会被替换为相反大小写[case]的对应字符[character], 取决于当前读取表[current readtable]的读取表[readtable case], 就像章节 23.1.2 (Lisp 读取器上的读取表大小写的影响)阐述的一样.
        -- Y 被追加到正在构建的对应标记[token]中.
        -- 重复步骤 8.

    * 如果 y 是一个单转义[single escape]字符[character], 那么下一个字符[character] z 会被读取或者在文件末尾时发出一个 end-of-file 类型[type]的错误. z 被认为是一个标记成分[constituent], 它的唯一的标记成分特质[constituent trait]是字母[alphabetic[2]]. z 会被追加到正在被构建的标记[token]中, 并且重复步骤 8.

    * 如果 y 是一个多转义[multiple escape]字符[character], 就会进入步骤 9.

    * 如果 y 是一个非法[invalid]字符[character], 一个 reader-error 类型[type]的错误就会被发出.

    * 如果 y 是一个终止的[terminating]宏字符[macro character], 它会终止这个标记[token]. 首先这个字符[character] y 会被撤销读取(见 unread-char), 然后进入步骤 10.

    * 如果 y 是一个空白[whitespace[2]]字符[character], 它会终止这个标记[token]. 首先如果合适的话这个字符 y 会被撤销读取(见 read-preserving-whitespace), 然后进入步骤10.

9. 此时正在累积一个标记[token], 并且已经遇到奇数个多转义[multiple escape]字符[character]. 如果到了文件的末尾就会发出一个 end-of-file 类型[type]的错误. 否则, 一个字符[character], y, 会被读取, 并且根据它的语法类型[syntax type]以下操作之一会被执行:

    * 如果 y 是一个标记成分[constituent], 宏, 或者空白[whitespace[2]]字符[character], y 被认为是一个标记成分[constituent], 并且它唯一的标记成分特质[constituent trait]是字母[alphabetic[2]]. y 被追加到正在被构建的标记[token]中, 然后重复步骤 9.

    * 如果 y 是一个单转义[single escape]字符[character], 那么下一个字符[character] z 会被读取或者在文件末尾时发出一个 end-of-file 类型[type]的错误. z 被当作一个标记成分[constituent], 它的唯一的标记成分特质[constituent trait]是字母[alphabetic[2]]. z 被追加到正在被构建的标记[token], 并且重复步骤 9.

    * 如果 y 是一个多转义[multiple escape]字符[character], 那么进入步骤 8.

    * 如果 y 是个非法[invalid]字符[character], 会发出一个类型[type]为 reader-error 的错误.

10. 已经累积到一个完整的标记[token]. 这个标记[token]表示的对象[object]作为这个读取操作的结果返回, 如果这个标记[token]不是有效语法的就发出一个 reader-error 类型[type]的错误. 

## 2.3 <span id = "InterpretationOfTokens">token 标记的解释</span>

> * 2.3.1 [数字标记](#NumbersAsTokens)
> * 2.3.2 [从标记构建数字](#ConstructingNumbersFromTokens)
> * 2.3.3 [点对](#TheConsingDot)
> * 2.3.4 [符号标记](#SymbolsAsTokens)
> * 2.3.5 [标记的合法模式](#ValidPatternsForTokens)
> * 2.3.6 [包系统一致性规则](#PackageSystemConsistencyRules)

### 2.3.1 <span id = "NumbersAsTokens">数字标记</span>

当一个标记[token]被读取, 它被解释为一个数字[number]或符号[symbol]. 如果这个标记[token]满足下面这段指定的数字语法, 它就被解释为数字[number].

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
                                          
    sign--- 一个正负号[sign].                         
    slash--- 一个斜杠[slash]
    decimal-point--- 一个点[dot].                        
    exponent-marker--- 一个指数标记[exponent marker].                        
    decimal-digit--- 一个基数[radix] 10 的数[digit].                        
    digit--- 一位当前输入基数的数[digit].                        

Figure 2-9. 数字标记的语法

#### 2.3.1.1 潜在数字作为标记

为了允许实现者和未来 Common Lisp 标准去扩展数字的语法, 定义了一个比数字语法更通用的潜在数字语法. 如果一个标记[token]满足下面的所有需求它就是一个潜在数字[potential number]:

1. 这个标记[token]完全由数字[digit], 正负号[sign], 比率标记[ratio marker], 小数点(.), 扩展字符(^ or _), 还有数字标记构成. 一个数字标记是一个字母. 一个字母是否会被当作数字标记取决于上下文, 但是与其他字母相邻的字母不能被视为数字标记. 指数标记[exponent marker]也是数字标记.

2. 这个标记[token]包含了至少一个数字. 根据当前输入基数[current input base], 字母可能被认为是数字, 但只在没有小数点的标记[token]中.

3. 这个标记[token]以一个数字[digit], 正负号[sign], 小数点或扩展字符开始, 但不是包标记[package marker]. 涉及一个前导包标记[package marker]后面跟着潜在数字[potential number]的语法还没有定义完善. 在一个期望适合于 read 的表达式的地方使用例如:1, :1/2, 还有 :2^3 的后果是未知的.

4. 这个标记[token]不能以一个正负号结尾.

如果一个潜在数字[potential number]有数字语法, 这个数字[number]在一个实现中是可被表示的, 会创建一个适当类型的数字[number]并且返回. 如果一个数字[number]超出了由依赖于具体实现的[implementation-dependent]数字[number]常量所设定的界限, 那么它在实现中是不能被表示的. 比如, 为浮点数[float]指定太大或太小的指数可能会使该实现中的那个数字[number]无法表示. 一个带有分母为零的比率[ratio] (比如 -35/000) 在任何实现中都是不能表示的. 当一个带有数字语法的标记[token]不能被转化为一个内置的数字[number]时, 会发出一个 reader-error 类型[type]的错误. 为一个浮点数指定太多有效数字一定不会发出一个错误; 应该生成一个截断的或者舍入的值.

如果出现字母应该被当作一个数字还是数字标记的歧义, 那么字母就被当作数字来对待.

##### 2.3.1.1.1 转义字符和潜在数字

一个潜在数字[potential number]不能包含任何转义[escape]字符[character]. 一个转义[escape]字符[character]剥夺了后面字符的所有语法特性, 迫使其严格转成字母[alphabetic[2]], 因此不适用于潜在数字[potential number]. 比如, 以下所有表示会被解释成符号[symbol], 而不是数字[number]:

     \256   25\64   1.0\E6   |100|   3\.14159   |3/4|   3\/4   5||

在每一个示例中, 移除单个转义[escape]字符[character] (或多个) 会导致标记被转成潜在数字[potential number]. 

##### 2.3.1.1.2 潜在数字示例

作为示例, 下面这一段的这些标记[token]是潜在数字[potential number], 但是它们实际上不是数字, 所以是保留的标记[token]; 一个符合规范的实现[conforming implementation]允许但不是必须去定义它们的意义.

    1b5000                       777777q                1.7J  -3/4+6.7J  12/25/83  
    27^19                        3^4/5                  6//7  3.1.2.6    ^-43^     
    3.141_592_653_589_793_238_4  -3.7+2.6i-6.17j+19.6k  

Figure 2-10. 保留标记的示例

下面这段里的这些标记[token]不是潜在数字[potential number]; 它们总是被当作符号[symbol]:

    /     /5     +  1+  1-   
    foo+  ab.cd  _  ^   ^/-  

Figure 2-11. 符号的示例

如果当前输入基数[current input base]是 16, 那么下面这段里的这些标记[token]就是潜在数字[potential number], 如果当前输入基数[current input base]是 10, 那么它们总是被当作符号[symbol].

    bad-face  25-dec-83  a/b  fad_cafe  f^  

Figure 2-12. 符号或潜在数字的示例

### 2.3.2 <span id = "ConstructingNumbersFromTokens">从标记构建数字</span>

一个实数[real]是由对应数字标记[token]直接构造的; 见 Figure 2-9.

一个复数[complex]被表示为一个 #C (或 #c) 跟着两个实数的列表[list]; 见章节 2.4.8.11 (井号C(#C)).

读取器宏[reader macro] #B, #O, #X, 和 #R 对于控制解析有理数[rational]所用的输入基数[radix]可能也是有用的; 见章节 2.4.8.7 (井号B(#B)), 章节 2.4.8.8 (井号O(#O)), 章节 2.4.8.9 (井号X(#X)), 还有章节 2.4.8.10 (井号R(#R)).

本节概述了数字[number]的完整语法.

> * 2.3.2.1 [有理数语法](#SyntaxRational)
> * 2.3.2.2 [浮点数的语法](#SyntaxFloat)
> * 2.3.2.3 [复数的语法](#SyntaxComplex)

#### 2.3.2.1 <span id = "SyntaxRational">有理数语法</span>

##### 2.3.2.1.1 一个整数的语法

整数[integer]可以写成多个数字[digit]的序列, 前面可以选择性地有一个正负号[sign], 然后还可以选择性地有一个小数点; 见 Figure 2-9. 当使用了一个小数点时, 这些数字[digit]采用的基数[radix]为 10; 当没有使用小数点时, 这个数字采取的基数就是当前输入基数[current input base].

关于整数[integer]如何被打印的信息, 见章节 22.1.3.1.1 (打印整数). 

##### 2.3.2.1.2 一个比率的语法

比率[ratio]可以写成一个可选的正负号[sign]后面跟着两个由斜杠[slash]分隔的非空数字[digit]序列; 见 Figure 2-9. 第二个序列可能不是完全由0组成的. 下一段是比率[ratio]的示例.

    2/3                 ;This is in canonical form                  
    4/6                 ;A non-canonical form for 2/3               
    -17/23              ;A ratio preceded by a sign                 
    -30517578125/32768  ;This is (-5/2)^15                          
    10/5                ;The canonical form for this is 2           
    #o-101/75           ;Octal notation for -65/61                  
    #3r120/21           ;Ternary notation for 15/7                  
    #Xbc/ad             ;Hexadecimal notation for 188/173           
    #xFADED/FACADE      ;Hexadecimal notation for 1027565/16435934  

Figure 2-13. 比率的示例

关于比率[ratio]如何被打印的信息, 见章节 22.1.3.1.2 (打印比率). 

#### 2.3.2.2 <span id = "SyntaxFloat">浮点数的语法</span>

浮点数[float]可以用十进制小数或计算机化科学记法来表示: 一个可选的正负号, 然后是一个带有内嵌小数点的非空序列, 然后是一个可选的十进制指数说明符. 如果没有指数说明符, 那么这个小数点是必须的, 并且在它后面要有个数字. 这个指数说明符由一个指数标记[exponent marker], 一个可选的正负号, 还有一个非空的数字序列组成. 如果没有指数说明符, 或者指数标记[exponent marker]用的是 e (或 E), 使用 \*read-default-float-format* 指定的格式. 见 Figure 2-9.

一个具体实现可能提供一种或多种浮点数[float]来共同构建 float 类型[type]. 字母 s, f, d, 和 l (或者它们的大写等价体) 分别显式指定 short-float, single-float, double-float, 和 long-float 类型[type]的使用.

用于扩展表示的内部格式仅依赖于指数标记[exponent marker], 而不依赖于扩展表示中的十进制数字的数量.

下面这块包含了浮点数的标记示例:

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

Figure 2-14. 浮点数示例

关于浮点数[float]如何打印的信息, 见章节 22.1.3.1.3 (打印浮点数). 

#### 2.3.2.3 <span id = "SyntaxComplex">复数的语法</span>

一个复数[complex]有一个笛卡尔结构, 带有一个实数部分和一个虚数部分, 其中每一个部分都是实数[real]表示. 一个复数[complex]的各个部分没有必要一定是浮点数[float]但是每个部分必须是一样的类型[type]: 都是有理数[rational], 或者都是相同的浮点数[float]子类型[subtype]. 当构建一个复数[complex]时, 如果指定的部分不是相同的类型[type], 这些部分在内部会被转换为相同的类型[type] (换句化说, 有理数[rational]部分被转化为一个浮点数[float]). 如果一个 (complex rational) 类型的对象[object]的虚部是一个值为 0 的整数[integer], 那么它在内部被转换, 并且此后被表示为一个有理数[rational].

关于进一步的信息, 见章节 2.4.8.11 (井号C(#C)) 还有章节 22.1.3.1.4 (打印复数). 

### 2.3.3 <span id = "TheConsingDot">点对</span>

如果一个标记[token]仅由点组成 (没有转义字符), 会发出一个 reader-error 类型[type]的错误, 除了在一种情况下: 如果标记[token]是一个单独的点[dot], 并且出现在点状对[dotted pair]标记允许一个点[dot]的情况中, 那么它就被接受作为这种语法的一部分, 并且不发出错误. 见章节 2.4.1 (左圆括号). 

### 2.3.4 <span id = "SymbolsAsTokens">符号标记</span>

任何不是潜在数字[potential number], 不包含包标记符[package marker], 也不是完全由点构成的标记[token]总是被解释为一个符号[symbol]. 任何是一个潜在数字[potential number]但是不符合数字语法的标记[token]作为保留标记[token]并且有着依赖于具体实现的[implementation-dependent]解释. 在所有其他情况下, 标记[token]被解释为一个符号[symbol]的名称.

下一段中有着符号[symbol]的打印表示. 为了表示的简单性, 这些示例假定当前读取表[current readtable]的读取表大小写[readtable case]是 :upcase.

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

Figure 2-15. 符号的打印表示示例 (Part 1 of 2)

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

Figure 2-16. 符号的打印表示示例 (Part 2 of 2)

在解析一个符号[symbol]的过程中, 哪些具体实现定义[implementation-defined]的属性[attribute]会从组成一个表示符号[symbol]的标记[token]的这些字符[character]中被移除, 是依赖于具体实现的[implementation-dependent].

在解析一个符号[symbol]的语法时, Lisp 读取器[Lisp reader]会在当前包[current package]中查找这个符号[symbol]的名字[name]. 这个查找可能牵涉到查找外部符号[external symbol]被当前包[current package]继承的其他包[package]. 如果找到这个名字, 返回对应的符号[symbol]. 如果没有找到这个名字 (这就表示当前包[current package]中没有这个名字的可访问[accessible]符号[symbol]), 会创建一个新的符号[symbol]并作为内部符号[internal symbol]放到当前包[current package]中. 当前包[current package]成为这个符号[symbol]的拥有者 (home 包[home package]), 并且这个符号[symbol]被捕捉到当前包[current package]. 如果这个名字后面被再一次读取到而当前的包[package]是同一个, 会找到并返回相同的符号[symbol]. 

### 2.3.5 <span id = "ValidPatternsForTokens">标记的合法模式</span>

标记的合法模式总结在下面这段.

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

Figure 2-17. 标记的合法模式

注意, nnnnn 有数字语法, xxxxx 和 ppppp 都没有数字语法, aaaaa 有任何语法.

后面有关于包标记[package marker]的规则总结. 在每一个情况中, 都会提供一个例子阐明这个情况; 为了表达的简单性, 示例假定当前读取表[current readtable]的读取表大小写[readtable case]是 :upcase.

1. 如果这里有一个单个的包标记[package marker], 并且它出想在这个标记[token]的开始, 那么这个标记[token]被解释为包 KEYWORD 中的一个符号[symbol]. 它还将新创建的符号的 symbol-value 设置为相同的符号[symbol], 以使这个符号[symbol]可以自求值.

    比如, :bar, 读取的时候, 把 BAR 作为 KEYWORD 包的外部符号[external symbol].

2. 如果这里有一个单个的包标记[package marker], 但不是在标记[token]的开头, 这个标记[token]会被拆成2个部分. 第一个部分指定一个包[package]; 第二个部分就成为这个包中有效的外部符号[external symbol]的名字.

    比如, foo:bar, 读取的时候, 在名为 FOO 的包的外部符号[external symbol]中查找 BAR.

3. 如果这里由两个相邻的包标记[package marker], 并且不是在标记[token]的开始或结尾, 那么它们会被分为两个部分. 第一部分指定一个包[package]; 第二个部分就成为这个包[package]中的符号[symbol]的名字 (可能是一个内部符号[internal symbol]).

    比如, foo::bar, 读取的时候, 把 BAR 捕捉到名为 FOO 的包[package]中.

4. 如果这个标记[token]没有包含包标记[package marker], 并且没有潜在数字[potential number]语法, 那么这个整个标记[token]就是这个符号[symbol]的名字. 从当前包[current package]中查找这个符号[symbol].

    比如, bar, 读取的时候, 把 BAR 放到当前包[current package]中.

5. 如果在一个标记[token]中使用任何其他包标记[package marker]的模式, 那么后果是不确定的. 在符号[symbol]名中没有被这个标准所定义的所有其他包标记[package marker]的使用保留给依赖于具体实现[implementation-dependent]的使用.

比如, 假定当前读取表[current readtable]的读取表大小写[readtable case]是 :upcase, editor:buffer 引用名为 editor 的包[package]中的名为 BUFFER 的外部符号[external symbol], 不管当前包[current package]中是否有一个名为 BUFFER 的符号[symbol]. 如果没有名为 editor 的包[package], 或者包 editor 中没有名为 BUFFER 的符号[symbol], 或者 BUFFER 没有被 editor 包导出, 读取器就会发出一个可矫正的错误. 如果见到 editor::buffer, 其效果与使用 EDITOR 包作为当前包[current package]的情况下读取 buffer 完全相同. 

### 2.3.6 <span id = "PackageSystemConsistencyRules">包系统一致性规则</span>

只要 \*package* 的值[value]没有变化那么以下规则就适用于包系统:

读取-读取 一致性

    读取相同的符号[symbol]名称[name]总是会产生相同的[same]符号[symbol].

打印-读取 一致性

    一个被捕捉的[interned]符号[symbol]总是打印为一个字符序列, 当读取回来时也是相同的符号[symbol].

    关于 Lisp 打印器[Lisp printer]如何对待符号, 见章节 22.1.3.3 (打印符号).

打印-打印 一致性

    如果两个被捕捉的符号[symbol]是不同的[same], 那么它们的打印表示会是不同的字符序列.

不管任何隐式捕捉, 这些规则都是对的. 只要当前包[current package]没有改变, 不管加载文件的顺序或在什么时候输入什么符号的确切历史, 结果都是可复写的. 如果 \*package* 的值[value]被改变了然后改变回这个之前的值, 还是会保持一致性. 可以通过修改 \*package* 的值, 通过从一个错误中继续来强制改变符号[symbol]或者包[package]或者都修改, 或者调用以下函数[function]中的一个来违反这个规则: unintern, unexport, shadow, shadowing-import, 或者 unuse-package.

只有当两个已命名的符号[symbol]之间的限制被违反时, 不一致性才适用. shadow, unexport, unintern, 还有 shadowing-import 只影响作为参数传递的名字[name]相同(用 string= 判断)的符号[symbol]. 

## 2.4 <span id = "StandardMacroCharacters">标准宏字符</span>

如果读取器遇到一个宏字符[macro character], 那么它的关联读取器宏函数[reader macro function]会被调用并且可能产生一个要被返回的对象[object]. 这个函数[function]可能以任何语法读取流中宏字符[macro character]后面的那些字符[character]并且返回这个语法所表示的对象[object].

任何字符[character]都可以作为宏字符[macro character]. 一个符合规范的实现[conforming implementation]中最初定义的宏字符[macro character]包括以下这些:

> * 2.4.1 [左圆括号](#LeftParenthesis)
> * 2.4.2 [右圆括号](#RightParenthesis)
> * 2.4.3 [单引号](#SingleQuote)
> * 2.4.4 [分号](#Semicolon)
> * 2.4.5 [双引号](#DoubleQuote)
> * 2.4.6 [反引号](#Backquote)
> * 2.4.7 [逗号](#Comma)
> * 2.4.8 [井号](#Sharpsign)
> * 2.4.9 [重复读取缩写的表达式](#ReReadingAbbreviatedExpressions)
 
### 2.4.1 <span id = "LeftParenthesis">左圆括号</span>

左圆括号[left-parenthesis]开始一个列表[list]的读取. read 会被递归调用去读取后续的对象直到在这个输入流[stream]中找到一个右圆括号. 返回一个读取到的对象[object]的列表[list]. 因此

```LISP
 (a b c)
```

被读取为一个三个对象[object]的列表[list] (符号[symbol] a, b, 还有 c). 右圆括号不需要紧跟着最后一个对象[object]的打印形式后面; 空白[whitespace[2]]和注释可能在它之前.

如果在右圆括号前没有对象[object], 它就读取到一个零对象[object]的列表[list] (一个空列表[empty list]).

如果一个标记[token]只是一个点, 而前面不是紧挨着一个在某个对象[object]后被读取的转义字符, 那么这个这个点之后一定跟着又一个对象, 然后可能前后有空白[whitespace[2]]或注释, 后面跟着右圆括号:

```LISP
 (a b c . d)
```

这就意味着这个列表[list]最后一个 cons 的 cdr 部分不是 nil, 而是点之后表示的对象[object]. 上面的例子可能是下面表达式的求值结果

```LISP
 (cons 'a (cons 'b (cons 'c 'd)))
```

类似的,

```LISP
 (cons 'this-one 'that-one) =>  (this-one . that-one)
```

跟在点后面的对象也允许是一个列表[list]:

```LISP
 (a b c d . (e f . (g))) ==  (a b c d e f g)
```

关于 Lisp 打印器[Lisp printer]如何打印列表[list]和 cons 的信息, 见章节 22.1.3.5 (打印列表和 cons). 

### 2.4.2 <span id = "RightParenthesis">右圆括号</span>

除了和一个左圆括号字符结合以外, 右圆括号[right-parenthesis]是非法. 关于更多的信息, 见章节 2.2 (读取器算法). 

### 2.4.3 <span id = "SingleQuote">单引号</span>

语法: '<\<exp>>

一个单引号[single-quote]引入一个要被被"引用(quoted)"的表达式[expression]. 单引号[single-quote]后面跟着一个表达式[expression] exp 会被 Lisp 读取器[Lisp reader]当作一个缩写并且和表达式(quote exp)同等方式被解析. 见特殊操作符[special operator] quote.

#### 2.4.3.1 单引号的示例

```LISP
 'foo =>  FOO
 ''foo =>  (QUOTE FOO)
 (car ''foo) =>  QUOTE
```

### 2.4.4 <span id = "Semicolon">分号</span>

语法: ;<\<text>>

一个分号[semicolon]引入需要被忽略的字符[character], 就像注释. 分号[semicolon]和所有直到并包括下一个换行符或到文件结尾的字符都被忽略.

#### 2.4.4.1 分号的示例

```LISP
 (+ 3 ; three
    4)
=>  7  
```  

#### 2.4.4.2 关于分号样式的注释

一些文本编辑器根据开始注释的分号[semicolon]数量来对期望的缩进做出假设. 下面的样式规约是较普遍的, 尽管不是通用的.

> * 2.4.4.2.1 [单分号的使用](#UseOfSingleSemicolon)
> * 2.4.4.2.2 [两个分号的使用](#UseOfDoubleSemicolon)
> * 2.4.4.2.3 [三个分号的使用](#UseOfTripleSemicolon)
> * 2.4.4.2.4 [四个分号的使用](#UseOfQuadrupleSemicolon)
> * 2.4.4.2.5 [分号风格的示例](#ExamplesOfStyleForSemicolon)

##### 2.4.4.2.1 <span id = "UseOfSingleSemicolon">单分号的使用</span>

以一个分号[semicolon]开始的注释都对齐到右边的同一列上 (有时也被称为 "注释列(comment column)"). 这样一个注释的文本通常只应用于它出现的行. 偶尔会有两个或三个一起包含一个单句; 这有时通过用一个额外的空格 (在分号[semicolon]后) 缩进除了第一个以外的所有来表示. 

##### 2.4.4.2.2 <span id = "UseOfDoubleSemicolon">两个分号的使用</span>

以双分号[semicolon]开头的注释都和在代码[code]中处于相同位置的表达式形式[form]对齐到相同的缩进级别. 这样的注释的文本通常用来描述注释出现点的程序[program]的状态, 或者这个注释后的代码[code], 或者两者都描述了. 

##### 2.4.4.2.3 <span id = "UseOfTripleSemicolon">三个分号的使用</span>

以三个分号[semicolon]开头的注释都对齐到左边框. 通常它们是在一个定义或定义集合之前使用的, 而不是在定义中. 

##### 2.4.4.2.4 <span id = "UseOfQuadrupleSemicolon">四个分号的使用</span>

以四个个分号[semicolon]开头的注释都对齐到左边框, 并且通常包含一小段文本作为后面跟着的代码的标题, 并且可能被用于这个程序的页眉或页脚中, 该程序准备代码以作为一个硬拷贝文档呈现. 

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

语法: "<\<text>>"

双引号[double-quote]被用于开始和结束一个字符串. 当遇到一个双引号[double-quote]时, 从输入[input]流[stream]中读取到的字符[character]会累积直到遇到另一个双引号[double-quote]. 如果见到一个单转义[single escape]字符[character], 这个单转义[single escape]字符[character]会被丢弃, 累积下一个字符并继续. 直到匹配的双引号为止但不包括双引号[double-quote]的那些字符[character]会被转成一个简单字符串[simple string]并返回. 这些累积字符的哪些属性[attribute]会在这个操作中被移除是依赖于具体实现的[implementation-dependent].

下一段中有双引号[double-quote]字符的示例.

```LISP
"Foo"                      ;A string with three characters in it  
""                         ;An empty string                       
"\"APL\\360?\" he cried."  ;A string with twenty characters       
"|x| = |-x|"               ;A ten-character string                
```

Figure 2-18. 双引号字符的示例

注意, 要将单转义字符或双引号[double-quote]放入字符串中, 这样的字符必须先有一个单转义字符. 还要注意, 多转义字符不需要被字符串中的单转义字符引用.

关于 Lisp 打印器[Lisp printer]如何打印字符串[string]的信息, 见章节 22.1.3.4 (打印字符串). 

### 2.4.6 <span id = "Backquote">反引号</span>

反引号[backquote]引入一个要创建的数据结构的模板. 比如, 写下

```LISP
`(cond ((numberp ,x) ,@y) (t (print ,x) ,@y))
```

粗略等价于写下

```LISP
(list 'cond 
    (cons (list 'numberp x) y) 
    (list* 't (list 'print x) y))
```

这个模板中出现逗号的地方, 这个逗号后面的表达式[expression]会被求值并产生一个对象插入到这个地方. 假定 b 的值是 3, 比如求值 `(a b ,b ,(+ b 1) b) 表示的表达式形式[form]会产生结果 (a b 3 4 b).

如果一个逗号紧跟着 @ 符号[at-sign], 这个 @ 符号[at-sign]后面的表达式形式[form]会被求值并产生一个对象[object]列表[list]. 这些对象[object]会被"接合(splice)"进这个模板. 比如, 如果 x 的值为 (a b c), 那么

```LISP
`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
=>  (x (a b c) a b c foo b bar (b c) baz b c)
```

这个反引号语法可以被总结为如下.

* 对于任何不是一个列表[list]或一个普通向量[vector]的表达式[expression] basic, `basic 等同于 'basic, 也就表示, (quote basic).

* 对于任何不是以 @ 符号[at-sign]或者点[dot]开始的表达式形式 form, `,form 等同于 form. (对于一个逗号[comma]后面的一个表达式形式的所有出现情况, 也有类似的警告.) <!--TODO 待校验-->

* `,@form 有着未定义的后果.

* `(x1 x2 x3 ... xn . atom) 可能被解释成

     (append [ x1] [ x2] [ x3] ... [ xn] (quote atom))

    其中方括号被用于表示一个如下的一个 xj 的转化:

    -- [form] 被解释为 (list `form), 包含一个接下来必须被进一步解释的反引号表达式形式.

    -- [,form] 被解释为 (list form).

    -- [,@form] 被解释为 form.

* \`(x1 x2 x3 ... xn) 可能被解释为相同的反引号表达式 `(x1 x2 x3 ... xn . nil), 由此将其规约到前一种情况.

* `(x1 x2 x3 ... xn . ,form) 可能被解释为

     (append [ x1] [ x2] [ x3] ... [ xn] form)

    其中的方括号表示一个如上所述的一个 xj 的转换.

* `(x1 x2 x3 ... xn . ,@form) 有着未定义的后果.

* \`#(x1 x2 x3 ... xn) 可能被解释为 (apply #'vector `(x1 x2 x3 ... xn)).

任何可能使用 ",@" 的地方, 可能使用 ",." 来表示允许在 ",." 后面的表达式形式产生的列表结构[list structure]上进行破坏性的操作 (实际上, 使用 nconc 而不是 append).

如果反引号语法是嵌套的, 那么最内层的反引号表达式形式应该首先展开. 这个也意味着如果有多个逗号出现在一行中, 最左边的一个是属于最里面的反引号[backquote].

一个具体实现[implementation]可以自由地解释一个反引号表达式形式[form] F1 为任何表达式形式[form] F2, 在求值时, 会产生一个和上面定义暗示的结果在 equal 下是相同[same]的结果, 假定替代的表达式形式[form] F2 的副作用行为也和上述给定的描述一致. 模板的构造拷贝可能也可能不和模板自身共享列表[list]的结构. 例如, 上面的定义意味着

```LISP
`((,a b) ,c ,@d)
```

会被解释为

```LISP
(append (list (append (list a) (list 'b) 'nil)) (list c) d 'nil)
```

但是它也可能被合法地解释为以下任何一种:

```LISP
(append (list (append (list a) (list 'b))) (list c) d)
(append (list (append (list a) '(b))) (list c) d)
(list* (cons a '(b)) c d)
(list* (cons a (list 'b)) c d)
(append (list (cons a '(b))) (list c) d)
(list* (cons a '(b)) c (copy-list d))
```

#### 2.4.6.1 关于反引号的注意事项

由于 Lisp 读取器[Lisp reader]解析一个涉及反引号[backquote]读取器宏[reader macro]的表达式[expressioni]的确切方式没有指定, 一个具体实现[implementation]可以自由地选择任何保留描述的语义的表示形式.

通常, 实现会选择一种便于美观打印表达式的表示形式, 这样一来 ``(pprint `(a ,b))`` 会显示 ``\`(a ,b)`` 而不是例如 ``(list 'a b)``. 然而, 这不是必须的.

没有特定理由做出一个选择或另一个的实现者可能希望引用《IEEE Standard for the Scheme Programming Language》, 它为这样的表达式确定了一种流行的表示方式, 可以为一些用户社区提供有用的兼容性. 然而这里没有任何必要条件, 要求任何符合规范的实现[conforming implementation]使用这个特定的表示. 此信息仅用于交叉引用目的.

### 2.4.7 <span id = "Comma">逗号</span>

逗号[comma]是反引号语法的一部分; 见章节 2.4.6 (反引号). 如果在上述的反引号表达式[expression]的主体之外的地方使用逗号[comma], 那么它的非法的. 

### 2.4.8 <span id = "Sharpsign">井号</span>

井号[sharpsign]是一个非终止[non-terminating]分派宏字符[dispatching macro character]. 它读取一个可选的数字序列然后再读取一个字符, 然后使用这个字符去选择一个函数[function]作为读取器宏函数[reader macro function]来运行.

标准语法[standard syntax]包括由 # 字符引入的结构. 这些结构的语法如下: 标识结构类型的字符后面跟着一些某个形式的参数. 如果这个字符是一个字母, 它的大小写[case]是不重要的; 比如 #O 和 #o 被认为是等价的.

某些 # 结构允许在 # 和字符之间出现一个无符号的十进制数.

和分派宏字符[dispatching macro character] # 关联的读取器宏[reader macro]在这个章节的后面部分有描述, 并且总结在下面这段.

  分派字符        | 目的                    | 分派字符      | 目的      
  | :------:| :------:                   | :------:  | :------:  | 
  Backspace      | signals error          | {            | undefined*             
  Tab            | signals error          | }            | undefined*             
  Newline        | signals error          | +            | read-time conditional  
  Linefeed       | signals error          | -            | read-time conditional  
  Page           | signals error          | .            | read-time evaluation   
  Return         | signals error          | /            | undefined              
  Space          | signals error          | A, a         | array                  
  !              | undefined*             | B, b         | binary rational        
  "              | undefined              | C, c         | complex number         
  #              | reference to = label   | D, d         | undefined              
  $              | undefined              | E, e         | undefined              
  %              | undefined              | F, f         | undefined              
  &              | undefined              | G, g         | undefined              
  '              | function abbreviation  | H, h         | undefined              
  (              | simple vector          | I, i         | undefined              
  )              | signals error          | J, j         | undefined              
  *              | bit vector             | K, k         | undefined              
  ,              | undefined              | L, l         | undefined              
  :              | uninterned symbol      | M, m         | undefined              
  ;              | undefined              | N, n         | undefined              
  <              | signals error          | O, o         | octal rational         
  =              | labels following object| P, p         | pathname               
  >              | undefined              | Q, q         | undefined              
  ?              | undefined*             | R, r         | radix-n rational       
  @              | undefined              | S, s         | structure              
  [              | undefined*             | T, t         | undefined              
  \              | character object       | U, u         | undefined              
  ]              | undefined*             | V, v         | undefined              
  ^              | undefined              | W, w         | undefined              
  _              | undefined              | X, x         | hexadecimal rational   
  `              | undefined              | Y, y         | undefined              
  |              | balanced comment       | Z, z         | undefined              
  ~              | undefined              | Rubout       | undefined              

Figure 2-19. 标准 # 分派宏字符语法

由星号(\*)标记的组合被显式地保留给用户. 符合规范的实现[conforming implementation]不会定义它们.

注意数字[digit]也没有出现在前面的表中. 这是因为标记 #0, #1, ..., #9 保留给另一个占相同语法空间的目的. 当一个数字[digit]跟在一个井号[sharpsign]后面, 它不会被认为是分派字符. 取而代之的是, 一个无符号整型的参数被累计起来, 并作为实参[argument]传递给数字后面字符[character]的读取器宏[reader macro]. 比如, #2A((1 2) (3 4)) 就是一个参数为 2 的 #A 的使用.

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

当这个标记[token] x 是单个字符[character]长时, 这个会被解析为字面字符[character] char. 在 #\ 后面大写[uppercase]字母和小写[lowercase]字母是区分开来的; #\A 和 #\a 表示不同的字符[character]对象[object]. 任何在 #\ 后面的单个字符[character]都会正常工作, 甚至那些对于 read 通常是非常特殊的字符, 例如左圆括号[left-parenthesis]和右圆括号[right-parenthesis].

在单字符[character]情况下, x 后面必须跟着一个非成分字符[character]. 在 #\ 被读取之后, 读取器备份<!--TODO back up over ??-->斜线[slash]然后开始读取一个标记[token], 把最初的斜线[slash]作为单转义[single escape]字符[character] (不管它在当前读取表[current readtable]里是否为单转义字符).

当这个标记[token]不止一个字符[character]长度时, 这个 x 必须有着符号[symbol]的语法, 并且其中没有内嵌的包标记[package marker]. 在这种情况下, 这个井号[sharpsign]反斜线[backslash]标记被解析为名为 (string-upcase x) 的字符[character]; 见章节 13.1.7 (字符的名字).

关于 Lisp 打印器[Lisp printer]如何打印字符[character]对象[object]的信息, 见章节 22.1.3.2 (打印字符). 

#### 2.4.8.2 <span id = "SharpsignSingleQuote">井号单引号(#')</span>

任何前面有 #' (井号[sharpsign]后面是单引号[single-quote]) 的表达式, 就像 #'expression, 被 Lisp 读取器[Lisp reader]当作是一个缩写并且解释为表达式[expression] (function expression). 见 function. 比如,

```LISP
(apply #'+ l) ==  (apply (function +) l)
```

#### 2.4.8.3 <span id = "SharpsignLeftParenthesis">井号左括号(#()</span>

#( 和 ) 被用于表示一个简单向量[simple vector].

如果一个无符号十进制整数出现在 # 和 ( 中间, 它明确指明这个向量[vector]的长度. 如果在结束的 ) 之前的对象[object]数超过那个无符号十进制整数, 后果是未定义的. 在结束的 ) 之前提供的对象[object]数量如果小于那个无符号十进制整数大于 0 那么最后一个对象[object]被用于填充这个向量[vector]的剩余部分. 如果这个无符号十进制整数是非零的但是在结束的 ) 之前提供的对象[object]数是 0 那么结果是未定义的. 比如,

```LISP
#(a b c c c c)
#6(a b c c c c)
#6(a b c)
#6(a b c c)
```

都意味着同样的东西: 一个长度为 6 的向量, 带有 a, b, 和 4 个 c 元素[element]. 其他例子如下:

```LISP
#(a b c)               ;A vector of length 3
#(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
                    ;A vector containing the primes below 50
#()                    ;An empty vector
```

标记 #() 表示一个空向量, 就像 #0().

关于 Lisp 打印器[Lisp printer]如何打印向量的信息, 见章节 22.1.3.4 (打印字符串), 章节 22.1.3.6 (打印位向量), 或章节 22.1.3.7 (打印其他向量). 

#### 2.4.8.4 <span id = "SharpsignAsterisk">井号星号(#*)</span>

语法: #*<\<bits>>

构造一个包含指定的位 (0's 和 1's) 的简单位向量[simple bit vector], 其中最左边的位索引为 0 并且后面的位有着递增的索引.

语法: #<\<n>>*<\<bits>>

带有一个参数 n, 那么这个要创建的向量的长度是 n. 如果位的数量少于 n 但是大于 0, 那么最后一位会被用于填充这个位向量[bit vector]的剩余部分.

标记 #* 和 #0* 每个都表示一个空的位向量[bit vector].

不管是否提供那个可选的参数 n, 星号[asterisk]后面的标记[token]会被正常的标记[token]分隔符来分割. 然而, 如果这个标记[token]不是完全由 0 和 1 组成, 或者如果提供了 n 但是位的数量却大于 n 位, 或者 n 大于 1, 但是没有提供位元素, 都会发出一个 reader-error 类型[type]的错误 (除非 \*read-suppress* 的值[value]是 true). 单转义字符[single escape]和多转义字符[multiple escape]都不允许出现在这个标记[token]里.

关于 Lisp 打印器[Lisp printer]如何打印位向量[bit vector]的信息, 见章节 22.1.3.6 (打印位向量).

##### 2.4.8.4.1 井号星号的示例

比如,

```LISP
#*101111
#6*101111
#6*101
#6*1011
```

都意味着相同的东西: 一个长度 6 的向量, 带有元素[element] 1, 0, 1, 1, 1, 和 1.

比如:

```LISP
 #*         ;An empty bit-vector
```

#### 2.4.8.5 <span id = "SharpsignColon">井号冒号(#:)</span>

语法: #:<\<symbol-name>>

\#: 引入一个名字[name]为 symbol-name 的未捕捉[uninterned]的符号[symbol]. 每次遇到这个语法, 会创建一个不同[distinct]的未拘留[uninterned]符号[symbol]. 这个 symbol-name 必须有符号[symbol]的语法并且没有包前缀[package prefix].

关于 Lisp 读取器[Lisp reader]如何打印未拘留[uninterned]符号[symbol], 见章节 22.1.3.3 (打印符号). 

#### 2.4.8.6 <span id = "SharpsignDot">井号点(#.)</span>

\#.foo 被读取为 foo 表示的对象[object]求值产生的对象[object]. 当读取到这个 #. 标记时, 求值会在 read 过程中完成. 因此这个 #. 语法执行了一个 foo 的读取时求值.

当 \*read-eval* 的值[value]是 false 时, 这个 #. 正常的影响会被抑制. 在这个情况下, 会发出一个 reader-error 类型[type]的错误.

对于一个没有适当的打印表示的对象[object], 一个计算这个对象[object]的表达式形式[form]可以使用符号 #. 来给定. 

#### 2.4.8.7 <span id = "SharpsignB">井号B(#B)</span>

\#Brational 用二进制(基数 2)读取有理数 rational. 比如,

```LISP
#B1101 ==  13 ;11012
#b101/11 ==  5/3
```

如果紧跟这个 #B 的标记没有二进制(换句话说, 基数 2)有理数[rational]的语法, 那么结果是未定义的. 

#### 2.4.8.8 <span id = "SharpsignO">井号O(#O)</span>

\#Orational 用八进制(基数 8)读取有理数 rational. 比如,

```LISP
#o37/15 ==  31/13
#o777 ==  511
#o105 ==  69 ;1058
```

如果紧跟在 #O 后的标记没有八进制(换句话说, 基数 8)有理数[rational]的语法, 结果是未定义的. 

#### 2.4.8.9 <span id = "SharpsignX">井号X(#X)</span>

\#Xrational 用十六进制(基数 16)读取有理数. 在 9 之上的数字是字母 A 到 F (小写字母 a 到 f 也是可接受的). 比如,

```LISP
#xF00 ==  3840             
#x105 ==  261 ;10516
```

如果紧跟着 #X 的标记没有十六进制(换句话说, 基数 16)有理数[rational]的语法, 那么结果是未定义的. 

#### 2.4.8.10 <span id = "SharpsignR">井号R(#R)</span>

\#nR

\#radixRrational 根据指定的进制 radix 来读取有理数 rational. 进制 radix 必须是由解释为十进制整数[integer]的数字组成; 它的值必须在 2 和 36 (包括)之间. 只有指定的进制中合法的数字可以被使用.

比如, #3r102 是另一种编写 11(十进制) 的方式, 并且 #11R32 是写 35(十进制) 的另一种方式. 对于大于 10 的进制数, 字母表中的字母依次被用于 9 之后的进制. 由于一个小数点就足够了, 不存在替代十进制数的 # 表示法.

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

Figure 2-20. 进制指示符的示例

如果跟在 #nR 后的标记不满足 n 进制有理数[rational]的语法, 结果是未定义的. 

#### 2.4.8.11 <span id = "SharpsignC">井号C(#C)</span>

\#C 读取一个跟在后面的对象[object], 这个对象必须是一个长度为 2 且其中的元素[element]都是实数的列表[list]. 这两个实数分别表示一个复数[complex]的实部和虚部. 如果这两部分不是相同的数据类型, 那么它们会根据章节 12.1.1.2 (数值运算的传递性) 描述的浮点数传递的规则被转换.

\#C(real imag) 等价于 #.(complex (quote real) (quote imag)), 除了这个 #C 不会被 \*read-eval* 影响. 见函数[function] complex.

下一段中包含了 #C 的使用示例.

```LISP
#C(3.0s1 2.0s-1)  ;A complex with small float parts.                
#C(5 -3)          ;A ``Gaussian integer''                           
#C(5/3 7.0)       ;Will be converted internally to #C(1.66666 7.0)  
#C(0 1)           ;The imaginary unit; that is, i.                  
```

Figure 2-21. 复数示例

关于更多信息, 见章节 22.1.3.1.4 (打印复数) 和章节 2.3.2.3 (复数的语法). 

#### 2.4.8.12 <span id = "SharpsignA">井号A(#A)</span>

\#nA

\#nAobject 构建一个 n 维数组[array], 使用 object 作为 :initial-contents 参数的值调用 make-array.

比如, #2A((0 1 5) (foo 2 (hot dog))) 表示一个 2×3 矩阵:

    0       1       5
    foo     2       (hot dog)

相对的, #1A((0 1 5) (foo 2 (hot dog))) 表示一个长度[length] 2 的向量[vector], 其中的元素[element]是列表[list]:

    (0 1 5) (foo 2 (hot dog))

\#0A((0 1 5) (foo 2 (hot dog))) 表示一个 0 维数组[array], 其中其中仅有的元素是一个列表[list]:

    ((0 1 5) (foo 2 (hot dog)))

\#0A foo 表示一个 0 维数组[array], 其中仅有的元素是符号[symbol] foo. 这个 #1A foo 标记是非法的因为 foo 不是一个序列[sequence].

如果这个表示要被解析的数组[array]的某个容积[dimension]被发现是 0, 所有右边的容积[dimension] (也就是说, 更高的容积[dimension]) 也被认为是 0.

关于 Lisp 打印器[Lisp printer]如何打印数组[array]的信息, 见章节 22.1.3.4 (打印字符串), 章节 22.1.3.6 (打印位向量), 章节 22.1.3.7 (打印其他向量), 或者 章节 22.1.3.8 (打印其他数组). 

#### 2.4.8.13 <span id = "SharpsignS">井号S(#S)</span>

\#s(name slot1 value1 slot2 value2 ...) 表示一个结构体[structure]. 只有当 name 是一个 defstruct 已经定义的结构体[structure]类型[type]的名字并且这个结构体[structure]类型[type]有标准构造函数, 这个才是有效的. 让 cm 成为这个构造器函数的 name; 那么这个语法等价于

```LISP
#.(cm keyword1 'value1 keyword2 'value2 ...)
```

这里每一个 keywordj 都是计算下面表达式的结果

```LISP
(intern (string slotj) (find-package 'keyword))
```

其净效果是用带有指定的值的指定的槽调用构造器函数. (这个强制特性被弃用; 在未来, 关键字的名字将会取自它们被读入的包, 因此如果希望如此, 那么实际上在 KEYWORD 包中的符号[symbol]应该被使用.)

无论构造器函数返回的是什么对象[object]都会被 #S 语法返回.

关于 Lisp 打印器[Lisp printer]如何打印结构体[structure]信息, 见章节 22.1.3.12 (打印结构体). 

#### 2.4.8.14 <span id = "SharpsignP">井号P(#P)</span>

\#P 读取后面的对象[object], 它必须是一个字符串[string].

\#P<\<expression>> 等价于 #.(parse-namestring '<\<expression>>), 除了 #P 不受 \*read-eval* 影响.

关于 Lisp 打印器[Lisp printer]如何打印一个路径名[pathname]的信息, 见章节 22.1.3.11 (打印路径名). 

#### 2.4.8.15 <span id = "SharpsignEqualSign">井号等号(#=)</span>

\#n=

\#n=object 就像读取有着对象 object 作为打印表示的对象. 但是, 该对象[object]是由一个必要的无符号十进制整数 n 所标记的, 为了可以通过语法 #n# 进行引用. 这个标记的作用域是被最外面对 read 的调用所读取的表达式[expression]; 这个表达式[expression]里, 相同的标记可能不会出像第二次. 

#### 2.4.8.16 <span id = "SharpsignSharpsign">井号井号(##)</span>

\#n#

\#n#, 其中的 n 是一个必要的无符号十进制整数[integer], 提供一个对 #n= 标记的某个对象[object]的引用; 这就是说, #n# 表示一个指向和 #n= 标记的对象相同 (eq) 对象的指针. 比如, 被下面这个代码创建出来的变量 y 中的结构:

```LISP
(setq x (list 'p 'q))
(setq y (list (list 'a 'b) x 'foo x))
(rplacd (last y) (cdr y))
```

可以通过以下方式表示:

    ((a b) . #1=(#2=(p q) foo #2# . #1#))

不用这个标记, 如果 \*print-length* 设置为 10 并且 \*print-circle* 设置为 nil, 结构会被以下面方式打印:

    ((a b) (p q) foo (p q) (p q) foo (p q) (p q) foo (p q) ...)

一个 #n# 引用可能只出现在一个 #n= 标记后; 超前的引用是不允许的. 这个引用可能不会作为被标记的对象自身出现 (就是说, #n=#n#) 可能不会被写入因为这个 #n= 标记的对象[object]在这个情况下还没有被定义好. <!--TODO 断句有疑问-->

#### 2.4.8.17 <span id = "SharpsignPlus">井号加号(#+)</span>

\#+ 提供一个读取时条件化机制; 语法是 #+test expression. 如果这个特性表达式[feature expression] test 成功, 那么这个文本标记表示一个打印表示是表达式 expression 的对象[object]. 如果这个特性表达式[feature expression] test 失败, 那么这个文本的标记就当作空白[whitespace[2]]对待; 这就是说, 就好象 "#+ test expression" 没有出现而只有一个空格出现在它的位置.

关于这个特性表达式[feature expression]的成功与否的详细描述, 见章节 24.1.2.1 (特性表达式).

\#+ 通过首先读取这个特性表达式[feature expression]然后如果测性表达式[feature expression]失败就会跳过这个表达式形式来操作的. 读取这个 test 时, 当前包[current package]是 KEYWORD 包. 跳过这个表达式形式是通过将 \*read-suppress* 绑定[binding]到真[true]然后调用 read 来完成的.

关于示例, 见章节 24.1.2.1.1 (特性表达式的示例). 

#### 2.4.8.18 <span id = "SharpsignMinus">井号减号(#-)</span>

\#- 就像 #+ 除了它会在那个 test 正确时跳过这个表达式 expression; 也就是说,

    #-test expression ==  #+(not test) expression

关于示例, 见章节 24.1.2.1.1 (特性表达式的示例). 

#### 2.4.8.19 <span id = "SharpsignVerticalBar">井号竖线(#|)</span>

\#|...|# 被读取器当作是一个注释. 它必须与 #| 和 |# 的其他出现保持平衡, 但是除此之外, 它可能包含任何字符.

##### 2.4.8.19.1 井号竖线的示例

下面是利用 #|...|# 标记的一些示例:

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

一些声称理解 Lisp 语法的文本编辑器把任何 |...| 作为不能嵌套的对称对 (就像它们只是被用于标注特定符号的多转义字符的平衡对). 为了弥补这一缺陷, 一些程序员使用 #||...#||...||#...||# 而不是 #|...#|...|#...|#. 注意这个替代方式并不是一个不同的读取器宏[reader macro]; 它只是利用了额外的竖线以一种欺骗特定文本编辑器来更好地支持嵌套注释的方式出现在注释中的事实. 像这样, 一个可能的代码:

```LISP
#|| (+ #|| 3 ||# 4 5) ||# 
```

这样的代码等价于:

```LISP
#| (+ #| 3 |# 4 5) |#
```

#### 2.4.8.20 <span id = "SharpsignLessThanSign">井号小于号(#<)</span>

\#< 不是一个合法的读取器语法. Lisp 读取器[Lisp reader]在遇到 #< 时会发出一个 reader-error 类型[type]的错误. 这个语法通常被用于不能被读回的对象[object]的打印表示. 

#### 2.4.8.21 <span id = "SharpsignWhitespace">井号空格(# )</span>

\# 后面紧跟空白[whitespace[1]]不是一个合法的读取器宏. 如果 Lisp 读取器[Lisp reader]遇到 #<Newline> 或 #<Space> 读取器宏标记会发出一个 reader-error 类型[type]的错误. 

#### 2.4.8.22 <span id = "SharpsignRightParenthesis">井号右括号(#))</span>

这不是一个合法的读取器语法.

如果 Lisp 读取器[Lisp reader]遇到一个 #) 会发出一个 reader-error 类型[type]的错误. 

### 2.4.9 <span id = "ReReadingAbbreviatedExpressions">重复读取缩写的表达式</span>

注意, 当读取一个由于 "..", "...", "#" 后面跟着空白[whitespace[1]]以及 "#)" 的约束而长度或层级限制(见 \*print-level\*, \*print-length\*, 和 \*print-lines\*)而被简化的表达式[expression[2]]时, Lisp 读取器[Lisp reader]通常会发出一个 reader-error 类型[type]的错误. 



