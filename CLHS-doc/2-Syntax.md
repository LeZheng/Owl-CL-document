# 2. 语法

> * 2.1 [字符语法](#CharacterSyntax)
> * 2.2 [读取器算法](#ReaderAlgorithm)
> * 2.3 [token的解释](#InterpretationOfTokens)
> * 2.4 [Standard Macro Characters](#StandardMacroCharacters)

## 2.1 <span id = "CharacterSyntax">字符语法</span>

Lisp 读取器从一个流中读取字符, 然后将其解释为一个对象的打印表示, 构建这个对象并且返回它.

这个章节描述的语法称之为标准语法. Common Lisp提供了对应操作, 因此可以在程序的控制下修改读取表所表示的语法信息的各个方面; 见章节 23 (Reader). 除了明确说明的以外, 这个文档中使用的就是标准语法.

> * 2.1.1 [读取表](#Readtables)
> * 2.1.2 [影响Lisp读取器的变量](#VariablesAffectReader)
> * 2.1.3 [标准字符](#StandardCharacters)
> * 2.1.4 [字符语法类型](#CharacterSyntaxTypes)

### 2.1.1 <span id = "Readtables">读取表</span>

Lisp读取器使用的语法信息包含在一个称之为读取表(readtable)的对象中. 此外，readtable还包含字符和语法类型之间的关联.

下一块列出了一些适用于读取表的定义的名字.

\*readtable*                   readtable-case                
copy-readtable                 readtablep                    
get-dispatch-macro-character   set-dispatch-macro-character  
get-macro-character            set-macro-character           
make-dispatch-macro-character  set-syntax-from-char          

Figure 2-1. 读取表定义的名字

> * 2.1.1.1 [当前的读取表](#CurrentReadtable)
> * 2.1.1.2 [标准读取表](#StandardReadtable)
> * 2.1.1.3 [最初的读取表](#InitialReadtable)

#### 2.1.1.1 <span id = "CurrentReadtable">当前的读取表</span>

可以存在一些描述不同语法的读取表，但是在任何给定的时间内都只存在一个, 称之为当前读取表, 影响着Lisp读取器把表达式解析为对象的方式. 当前的读取表在一个给定的动态环境中是这个环境中的 \*readtable* 的值. 为了使一个不同的读取表成为当前的读取表, \*readtable* 可以被赋值或绑定. 

#### 2.1.1.2 <span id = "StandardReadtable">标准读取表</span>

这个标准读取表符合标准语法. 如果尝试去修改标准的读取表, 结果是不可预料的. 为了实现修改或者扩展语法的效果, 可以去创建一个标准读取表的副本; 见函数 copy-readtable.

:upcase就是标准读取表的案例. 

#### 2.1.1.3 <span id = "InitialReadtable">最初的读取表</span>

最初的读取表是这个Lisp镜像开始时的当前读取表. 在那个时候, 它符合标准语法. 最初的读取表不同于标准读取表. 一个符合规范的程序去修改最初的读取表是允许的. 

### 2.1.2 <span id = "VariablesAffectReader">影响Lisp读取器的变量</span>

Lisp读取器不止受当前读取表所影响, 也被很多动态变量所影响. 下面这段就列出了这些影响Lisp读取器行为的变量.

\*package*    \*read-default-float-format*  \*readtable*  
\*read-base*  \*read-suppress*                           

Figure 2-2. 影响Lisp读取器的变量. 

### 2.1.3 <span id = "StandardCharacters">标准字符</span>

所有实现必须支持的一个字符集合称之为标准字符集合; 这个字符集合中的成员称之为标准字符.

这个标准字符集合由非图形化的字符newline, 图形化字符space, 还有以下94个图形化字符或者它们的等价物构成:

    Graphic ID  Glyph  Description  Graphic ID  Glyph  Description  
    LA01        a      small a      LN01        n      small n      
    LA02        A      capital A    LN02        N      capital N    
    LB01        b      small b      LO01        o      small o      
    LB02        B      capital B    LO02        O      capital O    
    LC01        c      small c      LP01        p      small p      
    LC02        C      capital C    LP02        P      capital P    
    LD01        d      small d      LQ01        q      small q      
    LD02        D      capital D    LQ02        Q      capital Q    
    LE01        e      small e      LR01        r      small r      
    LE02        E      capital E    LR02        R      capital R    
    LF01        f      small f      LS01        s      small s      
    LF02        F      capital F    LS02        S      capital S    
    LG01        g      small g      LT01        t      small t      
    LG02        G      capital G    LT02        T      capital T    
    LH01        h      small h      LU01        u      small u      
    LH02        H      capital H    LU02        U      capital U    
    LI01        i      small i      LV01        v      small v      
    LI02        I      capital I    LV02        V      capital V    
    LJ01        j      small j      LW01        w      small w      
    LJ02        J      capital J    LW02        W      capital W    
    LK01        k      small k      LX01        x      small x      
    LK02        K      capital K    LX02        X      capital X    
    LL01        l      small l      LY01        y      small y      
    LL02        L      capital L    LY02        Y      capital Y    
    LM01        m      small m      LZ01        z      small z      
    LM02        M      capital M    LZ02        Z      capital Z    

Figure 2-3. 标准字符子表 (Part 1 of 3: 拉丁字母)

    Graphic ID  Glyph  Description  Graphic ID  Glyph  Description  
    ND01        1      digit 1      ND06        6      digit 6      
    ND02        2      digit 2      ND07        7      digit 7      
    ND03        3      digit 3      ND08        8      digit 8      
    ND04        4      digit 4      ND09        9      digit 9      
    ND05        5      digit 5      ND10        0      digit 0      

Figure 2-4. 标准字符子表 (Part 2 of 3: 数字字符)

    Graphic ID  Glyph  Description                              
    SP02        !      exclamation mark                         
    SC03        $      dollar sign                              
    SP04        "      quotation mark, or double quote          
    SP05        '      apostrophe, or [single] quote            
    SP06        (      left parenthesis, or open parenthesis    
    SP07        )      right parenthesis, or close parenthesis  
    SP08        ,      comma                                    
    SP09        _      low line, or underscore                  
    SP10        -      hyphen, or minus [sign]                  
    SP11        .      full stop, period, or dot                
    SP12        /      solidus, or slash                        
    SP13        :      colon                                    
    SP14        ;      semicolon                                
    SP15        ?      question mark                            
    SA01        +      plus [sign]                              
    SA03        <      less-than [sign]                         
    SA04        =      equals [sign]                            
    SA05        >      greater-than [sign]                      
    SM01        #      number sign, or sharp[sign]              
    SM02        %      percent [sign]                           
    SM03        &      ampersand                                
    SM04        *      asterisk, or star                        
    SM05        @      commercial at, or at-sign                
    SM06        [      left [square] bracket                    
    SM07        \      reverse solidus, or backslash            
    SM08        ]      right [square] bracket                   
    SM11        {      left curly bracket, or left brace        
    SM13        |      vertical bar                             
    SM14        }      right curly bracket, or right brace      
    SD13        `      grave accent, or backquote               
    SD15        ^      circumflex accent                        
    SD19        ~      tilde                                    

Figure 2-5. 标准字符子表 (Part 3 of 3: 特殊字符)

这个图形化ID (graphic ID) 在Common Lisp中不可用, 但是提供了和ISO 6937/2交叉引用的目的. 注意图形化ID的第一个字母把字符分成以下几类: L---Latin, N---Numeric, S---Special. <!-- TODO 待校验 -->

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
> * 2.3.3 [The Consing Dot](#TheConsingDot)
> * 2.3.4 [Symbols as Tokens](#SymbolsAsTokens)
> * 2.3.5 [Valid Patterns for Tokens](#ValidPatternsForTokens)
> * 2.3.6 [Package System Consistency Rules](#PackageSystemConsistencyRules)

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

如果当前的输入奇数是16, 那么下面这段里的token就是潜在数字, 如果当前输入基数是10, 那么它们总是被当作符号.

    bad-face  25-dec-83  a/b  fad_cafe  f^  

Figure 2-12. 符号或潜在数字的示例

### 2.3.2 <span id = "ConstructingNumbersFromTokens">从token构建数字</span>

一个实数是由对应数字token直接构造的; 见 Figure 2-9.

一个复数表示为一个 #C (或 #c) 跟着两个实数的列表; 见章节 2.4.8.11 (Sharpsign C).

读取器宏 #B, #O, #X, 和 #R 对于控制解析的有理数的输入基数可能也是有用的; 见章节 2.4.8.7 (Sharpsign B), 章节 2.4.8.8 (Sharpsign O), 章节 2.4.8.9 (Sharpsign X), 还有章节 2.4.8.10 (Sharpsign R).

本节概述了数字的完整语法.

> * 2.3.2.1 [有理数语法](#SyntaxRational)
> * 2.3.2.2 [Syntax of a Float](#SyntaxFloat)
> * 2.3.2.3 [Syntax of a Complex](#SyntaxComplex)

#### 2.3.2.1 <span id = "SyntaxRational">有理数语法</span>

##### 2.3.2.1.1 一个整数的语法

整数可以写成数字序列, 前面可以有一个符号, 然后还可以有一个小数点; 见 Figure 2-9. 当使用了一个小数点, 这个数字的基数为 10; 当没有使用小数点, 这个数字的基数就是当前的输入基数.

关于如何打印整数的信息, 见章节 22.1.3.1.1 (Printing Integers). 

##### 2.3.2.1.2 Syntax of a Ratio

比率可以写成两个非空的数字序列, 由斜杠分隔, 前面可以有一个可选的正负号; 见 Figure 2-9. 第二个序列可能不是完全由0组成的. 下一段是比率的示例.

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

关于如何打印比率的信息, 见章节 22.1.3.1.2 (Printing Ratios). 

#### 2.3.2.2 <span id = "SyntaxFloat">Syntax of a Float</span>

Floats can be written in either decimal fraction or computerized scientific notation: an optional sign, then a non-empty sequence of digits with an embedded decimal point, then an optional decimal exponent specification. If there is no exponent specifier, then the decimal point is required, and there must be digits after it. The exponent specifier consists of an exponent marker, an optional sign, and a non-empty sequence of digits. If no exponent specifier is present, or if the exponent marker e (or E) is used, then the format specified by *read-default-float-format* is used. See Figure 2-9.

An implementation may provide one or more kinds of float that collectively make up the type float. The letters s, f, d, and l (or their respective uppercase equivalents) explicitly specify the use of the types short-float, single-float, double-float, and long-float, respectively.

The internal format used for an external representation depends only on the exponent marker, and not on the number of decimal digits in the external representation.

The next figure contains examples of notations for floats:

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

Figure 2-14. Examples of Floating-point numbers

For information on how floats are printed, see Section 22.1.3.1.3 (Printing Floats). 

#### 2.3.2.3 <span id = "SyntaxComplex">Syntax of a Complex</span>

A complex has a Cartesian structure, with a real part and an imaginary part each of which is a real. The parts of a complex are not necessarily floats but both parts must be of the same type: either both are rationals, or both are of the same float subtype. When constructing a complex, if the specified parts are not the same type, the parts are converted to be the same type internally (i.e., the rational part is converted to a float). An object of type (complex rational) is converted internally and represented thereafter as a rational if its imaginary part is an integer whose value is 0.

For further information, see Section 2.4.8.11 (Sharpsign C) and Section 22.1.3.1.4 (Printing Complexes). 

### 2.3.3 <span id = "TheConsingDot">The Consing Dot</span>

If a token consists solely of dots (with no escape characters), then an error of type reader-error is signaled, except in one circumstance: if the token is a single dot and appears in a situation where dotted pair notation permits a dot, then it is accepted as part of such syntax and no error is signaled. See Section 2.4.1 (Left-Parenthesis). 

### 2.3.4 <span id = "SymbolsAsTokens">Symbols as Tokens</span>

Any token that is not a potential number, does not contain a package marker, and does not consist entirely of dots will always be interpreted as a symbol. Any token that is a potential number but does not fit the number syntax is a reserved token and has an implementation-dependent interpretation. In all other cases, the token is construed to be the name of a symbol.

Examples of the printed representation of symbols are in the next figure. For presentational simplicity, these examples assume that the readtable case of the current readtable is :upcase.

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

Figure 2-15. Examples of the printed representation of symbols (Part 1 of 2)

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

Figure 2-16. Examples of the printed representation of symbols (Part 2 of 2)

In the process of parsing a symbol, it is implementation-dependent which implementation-defined attributes are removed from the characters forming a token that represents a symbol.

When parsing the syntax for a symbol, the Lisp reader looks up the name of that symbol in the current package. This lookup may involve looking in other packages whose external symbols are inherited by the current package. If the name is found, the corresponding symbol is returned. If the name is not found (that is, there is no symbol of that name accessible in the current package), a new symbol is created and is placed in the current package as an internal symbol. The current package becomes the owner (home package) of the symbol, and the symbol becomes interned in the current package. If the name is later read again while this same package is current, the same symbol will be found and returned. 

### 2.3.5 <span id = "ValidPatternsForTokens">Valid Patterns for Tokens</span>

The valid patterns for tokens are summarized in the next figure.

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

Figure 2-17. Valid patterns for tokens

Note that nnnnn has number syntax, neither xxxxx nor ppppp has number syntax, and aaaaa has any syntax.

A summary of rules concerning package markers follows. In each case, examples are offered to illustrate the case; for presentational simplicity, the examples assume that the readtable case of the current readtable is :upcase.

1. If there is a single package marker, and it occurs at the beginning of the token, then the token is interpreted as a symbol in the KEYWORD package. It also sets the symbol-value of the newly-created symbol to that same symbol so that the symbol will self-evaluate.

    For example, :bar, when read, interns BAR as an external symbol in the KEYWORD package.

2. If there is a single package marker not at the beginning or end of the token, then it divides the token into two parts. The first part specifies a package; the second part is the name of an external symbol available in that package.

    For example, foo:bar, when read, looks up BAR among the external symbols of the package named FOO.

3. If there are two adjacent package markers not at the beginning or end of the token, then they divide the token into two parts. The first part specifies a package; the second part is the name of a symbol within that package (possibly an internal symbol).

    For example, foo::bar, when read, interns BAR in the package named FOO.

4. If the token contains no package markers, and does not have potential number syntax, then the entire token is the name of the symbol. The symbol is looked up in the current package.

    For example, bar, when read, interns BAR in the current package.

5. The consequences are unspecified if any other pattern of package markers in a token is used. All other uses of package markers within names of symbols are not defined by this standard but are reserved for implementation-dependent use.

For example, assuming the readtable case of the current readtable is :upcase, editor:buffer refers to the external symbol named BUFFER present in the package named editor, regardless of whether there is a symbol named BUFFER in the current package. If there is no package named editor, or if no symbol named BUFFER is present in editor, or if BUFFER is not exported by editor, the reader signals a correctable error. If editor::buffer is seen, the effect is exactly the same as reading buffer with the EDITOR package being the current package. 

### 2.3.6 <span id = "PackageSystemConsistencyRules">Package System Consistency Rules</span>

The following rules apply to the package system as long as the value of *package* is not changed:

Read-read consistency

    Reading the same symbol name always results in the same symbol.

Print-read consistency

    An interned symbol always prints as a sequence of characters that, when read back in, yields the same symbol.

    For information about how the Lisp printer treats symbols, see Section 22.1.3.3 (Printing Symbols).

Print-print consistency

    If two interned symbols are not the same, then their printed representations will be different sequences of characters.

These rules are true regardless of any implicit interning. As long as the current package is not changed, results are reproducible regardless of the order of loading files or the exact history of what symbols were typed in when. If the value of *package* is changed and then changed back to the previous value, consistency is maintained. The rules can be violated by changing the value of *package*, forcing a change to symbols or to packages or to both by continuing from an error, or calling one of the following functions: unintern, unexport, shadow, shadowing-import, or unuse-package.

An inconsistency only applies if one of the restrictions is violated between two of the named symbols. shadow, unexport, unintern, and shadowing-import can only affect the consistency of symbols with the same names (under string=) as the ones supplied as arguments. 

## 2.4 <span id = "StandardMacroCharacters">Standard Macro Characters</span>

If the reader encounters a macro character, then its associated reader macro function is invoked and may produce an object to be returned. This function may read the characters following the macro character in the stream in any syntax and return the object represented by that syntax.

Any character can be made to be a macro character. The macro characters defined initially in a conforming implementation include the following:

> * 2.4.1 [Left-Parenthesis](#LeftParenthesis)
> * 2.4.2 [Right-Parenthesis](#RightParenthesis)
> * 2.4.3 [Single-Quote](#SingleQuote)
> * 2.4.4 [Semicolon](#Semicolon)
> * 2.4.5 [Double-Quote](#DoubleQuote)
> * 2.4.6 [Backquote](#Backquote)
> * 2.4.7 [Comma](#Comma)
> * 2.4.8 [Sharpsign](#Sharpsign)
> * 2.4.9 [Re-Reading Abbreviated Expressions](#ReReadingAbbreviatedExpressions)
 
### 2.4.1 <span id = "LeftParenthesis">Left-Parenthesis</span>

The left-parenthesis initiates reading of a list. read is called recursively to read successive objects until a right parenthesis is found in the input stream. A list of the objects read is returned. Thus

 (a b c)

is read as a list of three objects (the symbols a, b, and c). The right parenthesis need not immediately follow the printed representation of the last object; whitespace[2] characters and comments may precede it.

If no objects precede the right parenthesis, it reads as a list of zero objects (the empty list).

If a token that is just a dot not immediately preceded by an escape character is read after some object then exactly one more object must follow the dot, possibly preceded or followed by whitespace[2] or a comment, followed by the right parenthesis:

 (a b c . d)

This means that the cdr of the last cons in the list is not nil, but rather the object whose representation followed the dot. The above example might have been the result of evaluating

 (cons 'a (cons 'b (cons 'c 'd)))

Similarly,

 (cons 'this-one 'that-one) =>  (this-one . that-one)

It is permissible for the object following the dot to be a list:

 (a b c d . (e f . (g))) ==  (a b c d e f g)

For information on how the Lisp printer prints lists and conses, see Section 22.1.3.5 (Printing Lists and Conses). 

### 2.4.2 <span id = "RightParenthesis">Right-Parenthesis</span>

The right-parenthesis is invalid except when used in conjunction with the left parenthesis character. For more information, see Section 2.2 (Reader Algorithm). 

### 2.4.3 <span id = "SingleQuote">Single-Quote</span>

Syntax: '<<exp>>

A single-quote introduces an expression to be ``quoted.'' Single-quote followed by an expression exp is treated by the Lisp reader as an abbreviation for and is parsed identically to the expression (quote exp). See the special operator quote.

#### 2.4.3.1 Examples of Single-Quote

 'foo =>  FOO
 ''foo =>  (QUOTE FOO)
 (car ''foo) =>  QUOTE

### 2.4.4 <span id = "Semicolon">Semicolon</span>

Syntax: ;<<text>>

A semicolon introduces characters that are to be ignored, such as comments. The semicolon and all characters up to and including the next newline or end of file are ignored.

#### 2.4.4.1 Examples of Semicolon

 (+ 3 ; three
    4)
=>  7    

#### 2.4.4.2 Notes about Style for Semicolon

Some text editors make assumptions about desired indentation based on the number of semicolons that begin a comment. The following style conventions are common, although not by any means universal.

> * 2.4.4.2.1 [Use of Single Semicolon](#UseOfSingleSemicolon)
> * 2.4.4.2.2 [Use of Double Semicolon](#UseOfDoubleSemicolon)
> * 2.4.4.2.3 [Use of Triple Semicolon](#UseOfTripleSemicolon)
> * 2.4.4.2.4 [Use of Quadruple Semicolon](#UseOfQuadrupleSemicolon)
> * 2.4.4.2.5 [Examples of Style for Semicolon](#ExamplesOfStyleForSemicolon)

##### 2.4.4.2.1 <span id = "UseOfSingleSemicolon">Use of Single Semicolon</span>

Comments that begin with a single semicolon are all aligned to the same column at the right (sometimes called the ``comment column''). The text of such a comment generally applies only to the line on which it appears. Occasionally two or three contain a single sentence together; this is sometimes indicated by indenting all but the first with an additional space (after the semicolon). 

##### 2.4.4.2.2 <span id = "UseOfDoubleSemicolon">Use of Double Semicolon</span>

Comments that begin with a double semicolon are all aligned to the same level of indentation as a form would be at that same position in the code. The text of such a comment usually describes the state of the program at the point where the comment occurs, the code which follows the comment, or both. 

##### 2.4.4.2.3 <span id = "UseOfTripleSemicolon">Use of Triple Semicolon</span>

Comments that begin with a triple semicolon are all aligned to the left margin. Usually they are used prior to a definition or set of definitions, rather than within a definition. 

##### 2.4.4.2.4 <span id = "UseOfQuadrupleSemicolon">Use of Quadruple Semicolon</span>

Comments that begin with a quadruple semicolon are all aligned to the left margin, and generally contain only a short piece of text that serve as a title for the code which follows, and might be used in the header or footer of a program that prepares code for presentation as a hardcopy document. 

##### 2.4.4.2.5 <span id = "ExamplesOfStyleForSemicolon">Examples of Style for Semicolon</span>

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

### 2.4.5 <span id = "DoubleQuote">Double-Quote</span>

Syntax: "<<text>>"

The double-quote is used to begin and end a string. When a double-quote is encountered, characters are read from the input stream and accumulated until another double-quote is encountered. If a single escape character is seen, the single escape character is discarded, the next character is accumulated, and accumulation continues. The accumulated characters up to but not including the matching double-quote are made into a simple string and returned. It is implementation-dependent which attributes of the accumulated characters are removed in this process.

Examples of the use of the double-quote character are in the next figure.

"Foo"                      ;A string with three characters in it  
""                         ;An empty string                       
"\"APL\\360?\" he cried."  ;A string with twenty characters       
"|x| = |-x|"               ;A ten-character string                

Figure 2-18. Examples of the use of double-quote

Note that to place a single escape character or a double-quote into a string, such a character must be preceded by a single escape character. Note, too, that a multiple escape character need not be quoted by a single escape character within a string.

For information on how the Lisp printer prints strings, see Section 22.1.3.4 (Printing Strings). 

### 2.4.6 <span id = "Backquote">Backquote</span>

The backquote introduces a template of a data structure to be built. For example, writing

 `(cond ((numberp ,x) ,@y) (t (print ,x) ,@y))

is roughly equivalent to writing

 (list 'cond 
       (cons (list 'numberp x) y) 
       (list* 't (list 'print x) y))

Where a comma occurs in the template, the expression following the comma is to be evaluated to produce an object to be inserted at that point. Assume b has the value 3, for example, then evaluating the form denoted by `(a b ,b ,(+ b 1) b) produces the result (a b 3 4 b).

If a comma is immediately followed by an at-sign, then the form following the at-sign is evaluated to produce a list of objects. These objects are then ``spliced'' into place in the template. For example, if x has the value (a b c), then

 `(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))
=>  (x (a b c) a b c foo b bar (b c) baz b c)

The backquote syntax can be summarized formally as follows.

* `basic is the same as 'basic, that is, (quote basic), for any expression basic that is not a list or a general vector.

* `,form is the same as form, for any form, provided that the representation of form does not begin with at-sign or dot. (A similar caveat holds for all occurrences of a form after a comma.)

* `,@form has undefined consequences.

* `(x1 x2 x3 ... xn . atom) may be interpreted to mean

     (append [ x1] [ x2] [ x3] ... [ xn] (quote atom))

    where the brackets are used to indicate a transformation of an xj as follows:

    -- [form] is interpreted as (list `form), which contains a backquoted form that must then be further interpreted.

    -- [,form] is interpreted as (list form).

    -- [,@form] is interpreted as form.

* `(x1 x2 x3 ... xn) may be interpreted to mean the same as the backquoted form `(x1 x2 x3 ... xn . nil), thereby reducing it to the previous case.

* `(x1 x2 x3 ... xn . ,form) may be interpreted to mean

     (append [ x1] [ x2] [ x3] ... [ xn] form)

    where the brackets indicate a transformation of an xj as described above.

* `(x1 x2 x3 ... xn . ,@form) has undefined consequences.

* `#(x1 x2 x3 ... xn) may be interpreted to mean (apply #'vector `(x1 x2 x3 ... xn)).

Anywhere ``,@'' may be used, the syntax ``,.'' may be used instead to indicate that it is permissible to operate destructively on the list structure produced by the form following the ``,.'' (in effect, to use nconc instead of append).

If the backquote syntax is nested, the innermost backquoted form should be expanded first. This means that if several commas occur in a row, the leftmost one belongs to the innermost backquote.

An implementation is free to interpret a backquoted form F1 as any form F2 that, when evaluated, will produce a result that is the same under equal as the result implied by the above definition, provided that the side-effect behavior of the substitute form F2 is also consistent with the description given above. The constructed copy of the template might or might not share list structure with the template itself. As an example, the above definition implies that

 `((,a b) ,c ,@d)

will be interpreted as if it were

 (append (list (append (list a) (list 'b) 'nil)) (list c) d 'nil)

but it could also be legitimately interpreted to mean any of the following:

 (append (list (append (list a) (list 'b))) (list c) d)
 (append (list (append (list a) '(b))) (list c) d)
 (list* (cons a '(b)) c d)
 (list* (cons a (list 'b)) c d)
 (append (list (cons a '(b))) (list c) d)
 (list* (cons a '(b)) c (copy-list d))

#### 2.4.6.1 Notes about Backquote

Since the exact manner in which the Lisp reader will parse an expression involving the backquote reader macro is not specified, an implementation is free to choose any representation that preserves the semantics described.

Often an implementation will choose a representation that facilitates pretty printing of the expression, so that (pprint `(a ,b)) will display `(a ,b) and not, for example, (list 'a b). However, this is not a requirement.

Implementors who have no particular reason to make one choice or another might wish to refer to IEEE Standard for the Scheme Programming Language, which identifies a popular choice of representation for such expressions that might provide useful to be useful compatibility for some user communities. There is no requirement, however, that any conforming implementation use this particular representation. This information is provided merely for cross-reference purposes. 

### 2.4.7 <span id = "Comma">Comma</span>

The comma is part of the backquote syntax; see Section 2.4.6 (Backquote). Comma is invalid if used other than inside the body of a backquote expression as described above. 

### 2.4.8 <span id = "Sharpsign">Sharpsign</span>

Sharpsign is a non-terminating dispatching macro character. It reads an optional sequence of digits and then one more character, and uses that character to select a function to run as a reader macro function.

The standard syntax includes constructs introduced by the # character. The syntax of these constructs is as follows: a character that identifies the type of construct is followed by arguments in some form. If the character is a letter, its case is not important; #O and #o are considered to be equivalent, for example.

Certain # constructs allow an unsigned decimal number to appear between the # and the character.

The reader macros associated with the dispatching macro character # are described later in this section and summarized in the next figure.

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

Figure 2-19. Standard #Dispatching Macro Character Syntax

The combinations marked by an asterisk (*) are explicitly reserved to the user. No conforming implementation defines them.

Note also that digits do not appear in the preceding table. This is because the notations #0, #1, ..., #9 are reserved for another purpose which occupies the same syntactic space. When a digit follows a sharpsign, it is not treated as a dispatch character. Instead, an unsigned integer argument is accumulated and passed as an argument to the reader macro for the character that follows the digits. For example, #2A((1 2) (3 4)) is a use of #A with an argument of 2.

> * 2.4.8.1 [Sharpsign Backslash](#SharpsignBackslash)
> * 2.4.8.2 [Sharpsign Single-Quote](#SharpsignSingleQuote)
> * 2.4.8.3 [Sharpsign Left-Parenthesis](#SharpsignLeftParenthesis)
> * 2.4.8.4 [Sharpsign Asterisk](#SharpsignAsterisk)
> * 2.4.8.5 [Sharpsign Colon](#SharpsignColon)
> * 2.4.8.6 [Sharpsign Dot](#SharpsignDot)
> * 2.4.8.7 [Sharpsign B](#SharpsignB)
> * 2.4.8.8 [Sharpsign O](#SharpsignO)
> * 2.4.8.9 [Sharpsign X](#SharpsignX)
> * 2.4.8.10 [Sharpsign R](#SharpsignR)
> * 2.4.8.11 [Sharpsign C](#SharpsignC)
> * 2.4.8.12 [Sharpsign A](#SharpsignA)
> * 2.4.8.13 [Sharpsign S](#SharpsignS)
> * 2.4.8.14 [Sharpsign P](#SharpsignP)
> * 2.4.8.15 [Sharpsign Equal-Sign](#SharpsignEqualSign)
> * 2.4.8.16 [Sharpsign Sharpsign](#SharpsignSharpsign)
> * 2.4.8.17 [Sharpsign Plus](#SharpsignPlus)
> * 2.4.8.18 [Sharpsign Minus](#SharpsignMinus)
> * 2.4.8.19 [Sharpsign Vertical-Bar](#SharpsignVerticalBar)
> * 2.4.8.20 [Sharpsign Less-Than-Sign](#SharpsignLessThanSign)
> * 2.4.8.21 [Sharpsign Whitespace](#SharpsignWhitespace)
> * 2.4.8.22 [Sharpsign Right-Parenthesis](#SharpsignRightParenthesis)

#### 2.4.8.1 <span id = "SharpsignBackslash">Sharpsign Backslash</span>

Syntax: #\<<x>>

When the token x is a single character long, this parses as the literal character char. Uppercase and lowercase letters are distinguished after #\; #\A and #\a denote different character objects. Any single character works after #\, even those that are normally special to read, such as left-parenthesis and right-parenthesis.

In the single character case, the x must be followed by a non-constituent character. After #\ is read, the reader backs up over the slash and then reads a token, treating the initial slash as a single escape character (whether it really is or not in the current readtable).

When the token x is more than one character long, the x must have the syntax of a symbol with no embedded package markers. In this case, the sharpsign backslash notation parses as the character whose name is (string-upcase x); see Section 13.1.7 (Character Names).

For information about how the Lisp printer prints character objects, see Section 22.1.3.2 (Printing Characters). 

#### 2.4.8.2 <span id = "SharpsignSingleQuote">Sharpsign Single-Quote</span>

Any expression preceded by #' (sharpsign followed by single-quote), as in #'expression, is treated by the Lisp reader as an abbreviation for and parsed identically to the expression (function expression). See function. For example,

(apply #'+ l) ==  (apply (function +) l)

#### 2.4.8.3 <span id = "SharpsignLeftParenthesis">Sharpsign Left-Parenthesis</span>

#( and ) are used to notate a simple vector.

If an unsigned decimal integer appears between the # and (, it specifies explicitly the length of the vector. The consequences are undefined if the number of objects specified before the closing ) exceeds the unsigned decimal integer. If the number of objects supplied before the closing ) is less than the unsigned decimal integer but greater than zero, the last object is used to fill all remaining elements of the vector. The consequences are undefined if the unsigned decimal integer is non-zero and number of objects supplied before the closing ) is zero. For example,

 #(a b c c c c)
 #6(a b c c c c)
 #6(a b c)
 #6(a b c c)

all mean the same thing: a vector of length 6 with elements a, b, and four occurrences of c. Other examples follow:

 #(a b c)               ;A vector of length 3
 #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
                        ;A vector containing the primes below 50
 #()                    ;An empty vector

The notation #() denotes an empty vector, as does #0().

For information on how the Lisp printer prints vectors, see Section 22.1.3.4 (Printing Strings), Section 22.1.3.6 (Printing Bit Vectors), or Section 22.1.3.7 (Printing Other Vectors). 

#### 2.4.8.4 <span id = "SharpsignAsterisk">Sharpsign Asterisk</span>

Syntax: #*<<bits>>

A simple bit vector is constructed containing the indicated bits (0's and 1's), where the leftmost bit has index zero and the subsequent bits have increasing indices.

Syntax: #<<n>>*<<bits>>

With an argument n, the vector to be created is of length n. If the number of bits is less than n but greater than zero, the last bit is used to fill all remaining bits of the bit vector.

The notations #* and #0* each denote an empty bit vector.

Regardless of whether the optional numeric argument n is provided, the token that follows the asterisk is delimited by a normal token delimiter. However, (unless the value of *read-suppress* is true) an error of type reader-error is signaled if that token is not composed entirely of 0's and 1's, or if n was supplied and the token is composed of more than n bits, or if n is greater than one, but no bits were specified. Neither a single escape nor a multiple escape is permitted in this token.

For information on how the Lisp printer prints bit vectors, see Section 22.1.3.6 (Printing Bit Vectors).

##### 2.4.8.4.1 Examples of Sharpsign Asterisk

For example,

  #*101111
 #6*101111
 #6*101
 #6*1011

all mean the same thing: a vector of length 6 with elements 1, 0, 1, 1, 1, and 1.

For example:

 #*         ;An empty bit-vector

#### 2.4.8.5 <span id = "SharpsignColon">Sharpsign Colon</span>

Syntax: #:<<symbol-name>>

#: introduces an uninterned symbol whose name is symbol-name. Every time this syntax is encountered, a distinct uninterned symbol is created. The symbol-name must have the syntax of a symbol with no package prefix.

For information on how the Lisp reader prints uninterned symbols, see Section 22.1.3.3 (Printing Symbols). 

#### 2.4.8.6 <span id = "SharpsignDot">Sharpsign Dot</span>

#.foo is read as the object resulting from the evaluation of the object represented by foo. The evaluation is done during the read process, when the #. notation is encountered. The #. syntax therefore performs a read-time evaluation of foo.

The normal effect of #. is inhibited when the value of *read-eval* is false. In that situation, an error of type reader-error is signaled.

For an object that does not have a convenient printed representation, a form that computes the object can be given using the #. notation. 

#### 2.4.8.7 <span id = "SharpsignB">Sharpsign B</span>

#Brational reads rational in binary (radix 2). For example,

 #B1101 ==  13 ;11012
 #b101/11 ==  5/3

The consequences are undefined if the token immediately following the #B does not have the syntax of a binary (i.e., radix 2) rational. 

#### 2.4.8.8 <span id = "SharpsignO">Sharpsign O</span>

#Orational reads rational in octal (radix 8). For example,

 #o37/15 ==  31/13
 #o777 ==  511
 #o105 ==  69 ;1058

The consequences are undefined if the token immediately following the #O does not have the syntax of an octal (i.e., radix 8) rational. 

#### 2.4.8.9 <span id = "SharpsignX">Sharpsign X</span>

#Xrational reads rational in hexadecimal (radix 16). The digits above 9 are the letters A through F (the lowercase letters a through f are also acceptable). For example,

 #xF00 ==  3840             
 #x105 ==  261 ;10516

The consequences are undefined if the token immediately following the #X does not have the syntax of a hexadecimal (i.e., radix 16) rational. 

#### 2.4.8.10 <span id = "SharpsignR">Sharpsign R</span>

#nR

#radixRrational reads rational in radix radix. radix must consist of only digits that are interpreted as an integer in decimal radix; its value must be between 2 and 36 (inclusive). Only valid digits for the specified radix may be used.

For example, #3r102 is another way of writing 11 (decimal), and #11R32 is another way of writing 35 (decimal). For radices larger than 10, letters of the alphabet are used in order for the digits after 9. No alternate # notation exists for the decimal radix since a decimal point suffices.

The next figure contains examples of the use of #B, #O, #X, and #R.

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

Figure 2-20. Radix Indicator Example

The consequences are undefined if the token immediately following the #nR does not have the syntax of a rational in radix n. 

#### 2.4.8.11 <span id = "SharpsignC">Sharpsign C</span>

#C reads a following object, which must be a list of length two whose elements are both reals. These reals denote, respectively, the real and imaginary parts of a complex number. If the two parts as notated are not of the same data type, then they are converted according to the rules of floating-point contagion described in Section 12.1.1.2 (Contagion in Numeric Operations).

#C(real imag) is equivalent to #.(complex (quote real) (quote imag)), except that #C is not affected by *read-eval*. See the function complex.

The next figure contains examples of the use of #C.

#C(3.0s1 2.0s-1)  ;A complex with small float parts.                
#C(5 -3)          ;A ``Gaussian integer''                           
#C(5/3 7.0)       ;Will be converted internally to #C(1.66666 7.0)  
#C(0 1)           ;The imaginary unit; that is, i.                  

Figure 2-21. Complex Number Example

For further information, see Section 22.1.3.1.4 (Printing Complexes) and Section 2.3.2.3 (Syntax of a Complex). 

#### 2.4.8.12 <span id = "SharpsignA">Sharpsign A</span>

#nA

#nAobject constructs an n-dimensional array, using object as the value of the :initial-contents argument to make-array.

For example, #2A((0 1 5) (foo 2 (hot dog))) represents a 2-by-3 matrix:

 0       1       5
 foo     2       (hot dog)

In contrast, #1A((0 1 5) (foo 2 (hot dog))) represents a vector of length 2 whose elements are lists:

 (0 1 5) (foo 2 (hot dog))

#0A((0 1 5) (foo 2 (hot dog))) represents a zero-dimensional array whose sole element is a list:

 ((0 1 5) (foo 2 (hot dog)))

#0A foo represents a zero-dimensional array whose sole element is the symbol foo. The notation #1A foo is not valid because foo is not a sequence.

If some dimension of the array whose representation is being parsed is found to be 0, all dimensions to the right (i.e., the higher numbered dimensions) are also considered to be 0.

For information on how the Lisp printer prints arrays, see Section 22.1.3.4 (Printing Strings), Section 22.1.3.6 (Printing Bit Vectors), Section 22.1.3.7 (Printing Other Vectors), or Section 22.1.3.8 (Printing Other Arrays). 

#### 2.4.8.13 <span id = "SharpsignS">Sharpsign S</span>

#s(name slot1 value1 slot2 value2 ...) denotes a structure. This is valid only if name is the name of a structure type already defined by defstruct and if the structure type has a standard constructor function. Let cm stand for the name of this constructor function; then this syntax is equivalent to

 #.(cm keyword1 'value1 keyword2 'value2 ...)

where each keywordj is the result of computing

 (intern (string slotj) (find-package 'keyword))

The net effect is that the constructor function is called with the specified slots having the specified values. (This coercion feature is deprecated; in the future, keyword names will be taken in the package they are read in, so symbols that are actually in the KEYWORD package should be used if that is what is desired.)

Whatever object the constructor function returns is returned by the #S syntax.

For information on how the Lisp printer prints structures, see Section 22.1.3.12 (Printing Structures). 

#### 2.4.8.14 <span id = "SharpsignP">Sharpsign P</span>

#P reads a following object, which must be a string.

#P<<expression>> is equivalent to #.(parse-namestring '<<expression>>), except that #P is not affected by *read-eval*.

For information on how the Lisp printer prints pathnames, see Section 22.1.3.11 (Printing Pathnames). 

#### 2.4.8.15 <span id = "SharpsignEqualSign">Sharpsign Equal-Sign</span>

#n=

#n=object reads as whatever object has object as its printed representation. However, that object is labeled by n, a required unsigned decimal integer, for possible reference by the syntax #n#. The scope of the label is the expression being read by the outermost call to read; within this expression, the same label may not appear twice. 

#### 2.4.8.16 <span id = "SharpsignSharpsign">Sharpsign Sharpsign</span>

#n#

#n#, where n is a required unsigned decimal integer, provides a reference to some object labeled by #n=; that is, #n# represents a pointer to the same (eq) object labeled by #n=. For example, a structure created in the variable y by this code:

 (setq x (list 'p 'q))
 (setq y (list (list 'a 'b) x 'foo x))
 (rplacd (last y) (cdr y))

could be represented in this way:

 ((a b) . #1=(#2=(p q) foo #2# . #1#))

Without this notation, but with *print-length* set to 10 and *print-circle* set to nil, the structure would print in this way:

 ((a b) (p q) foo (p q) (p q) foo (p q) (p q) foo (p q) ...)

A reference #n# may only occur after a label #n=; forward references are not permitted. The reference may not appear as the labeled object itself (that is, #n=#n#) may not be written because the object labeled by #n= is not well defined in this case. 

#### 2.4.8.17 <span id = "SharpsignPlus">Sharpsign Plus</span>

#+ provides a read-time conditionalization facility; the syntax is #+test expression. If the feature expression test succeeds, then this textual notation represents an object whose printed representation is expression. If the feature expression test fails, then this textual notation is treated as whitespace[2]; that is, it is as if the ``#+ test expression'' did not appear and only a space appeared in its place.

For a detailed description of success and failure in feature expressions, see Section 24.1.2.1 (Feature Expressions).

#+ operates by first reading the feature expression and then skipping over the form if the feature expression fails. While reading the test, the current package is the KEYWORD package. Skipping over the form is accomplished by binding *read-suppress* to true and then calling read.

For examples, see Section 24.1.2.1.1 (Examples of Feature Expressions). 

#### 2.4.8.18 <span id = "SharpsignMinus">Sharpsign Minus</span>

#- is like #+ except that it skips the expression if the test succeeds; that is,

#-test expression ==  #+(not test) expression

For examples, see Section 24.1.2.1.1 (Examples of Feature Expressions). 

#### 2.4.8.19 <span id = "SharpsignVerticalBar">Sharpsign Vertical-Bar</span>

#|...|# is treated as a comment by the reader. It must be balanced with respect to other occurrences of #| and |#, but otherwise may contain any characters whatsoever.

##### 2.4.8.19.1 Examples of Sharpsign Vertical-Bar

The following are some examples that exploit the #|...|# notation:

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


##### 2.4.8.19.2 Notes about Style for Sharpsign Vertical-Bar

Some text editors that purport to understand Lisp syntax treat any |...| as balanced pairs that cannot nest (as if they were just balanced pairs of the multiple escapes used in notating certain symbols). To compensate for this deficiency, some programmers use the notation #||...#||...||#...||# instead of #|...#|...|#...|#. Note that this alternate usage is not a different reader macro; it merely exploits the fact that the additional vertical-bars occur within the comment in a way that tricks certain text editor into better supporting nested comments. As such, one might sometimes see code like:

 #|| (+ #|| 3 ||# 4 5) ||# 

Such code is equivalent to:

 #| (+ #| 3 |# 4 5) |#

#### 2.4.8.20 <span id = "SharpsignLessThanSign">Sharpsign Less-Than-Sign</span>

#< is not valid reader syntax. The Lisp reader will signal an error of type reader-error on encountering #<. This syntax is typically used in the printed representation of objects that cannot be read back in. 

#### 2.4.8.21 <span id = "SharpsignWhitespace">Sharpsign Whitespace</span>

# followed immediately by whitespace[1] is not valid reader syntax. The Lisp reader will signal an error of type reader-error if it encounters the reader macro notation #<Newline> or #<Space>. 

#### 2.4.8.22 <span id = "SharpsignRightParenthesis">Sharpsign Right-Parenthesis</span>

This is not valid reader syntax.

The Lisp reader will signal an error of type reader-error upon encountering #). 

### 2.4.9 <span id = "ReReadingAbbreviatedExpressions">Re-Reading Abbreviated Expressions</span>

Note that the Lisp reader will generally signal an error of type reader-error when reading an expression[2] that has been abbreviated because of length or level limits (see *print-level*, *print-length*, and *print-lines*) due to restrictions on ``..'', ``...'', ``#'' followed by whitespace[1], and ``#)''. 



