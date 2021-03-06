# 26. 术语

## 26.1 术语

### 术语表示

这个术语表中的每个条目都有以下部分:

* 要定义的术语, 用粗线表示.

* 可选的发音, 用方括号括起来并且用黑体字表示, 就像后面的例子中那样: ['a,list]. 这个发音的调遵循 Third New International Dictionary the English Language, Unabridged, 除了这个 "uh" 被用于表示非中央元音 (颠倒的 "e") 字符, "ee" 被用于表示一个 hard "e" (一个带有上划线的 "e"), "oh" 被用于表示一个 hard "o" (一个带有上划线的 "o"), 并且 "ay" 被用于表示一个 hard "a" (一个带有上划线的 "a")..
<!--TODO hard ？？-->
* 词类的部分或多个部分, 用斜体表示. 如果一个术语可以被用作多个词类部分, 那么每一部分的词类都有一个单独的定义.

* 不止一个定义的话, 按如下组织:

    如果这里有多个定义, 就会出现一个可选的数字. 小写字母也可以用于需要编号定义的子定义的情况.

    一个可选的词类部分, 用斜体表示, 如果这个术语是词类的多个部分之一就会出现.

    如果术语有重复的标准定义, 就会出现一个用斜体表示的可选的学科. 比如, "*Math*".

    如果这个定义只有在可选的上下文中有意义就会出现一个可选的上下文. 例如, "一个符号(of a symbol)".

    这个术语的定义.

    一个可选的示例句子. 比如, "This is an example of an example."

    可选的交叉引用.

另外, 一些术语在 Common Lisp 社区有着惯用的用法, 但是不被其他社区所共享, 或者不是学术上正确的. 标记了 "*习语*(*Idiom*)." 的定义表示这样的本地用法; 有时这些定义后面跟着一个注释.

这个字体的单词是这个术语表中有条目的单词. 示例句子中的单词不遵循这个约定.

当出现歧义时, 最长匹配的子字符串具有优先级. 例如, "复浮点数[complex float]" 引用单独术语条目 "复浮点数[complex float]" 而不是术语 "复数[complex]" 和 "浮点数[float]" 的组合意义.

下标符号 , 就像在 "something[n]" 中的意味着打算用 "something" 的第 n 个定义. 这种表示法只在上下文可能不足以消除歧义的情况下使用.

以下是术语表中使用的缩写:

    缩写          意义                                     
    adj.          形容词                                   
    adv.          副词                                      
    ANSI          兼容一个或多个 ANSI 标准  
    Comp.         计算机                                   
    Idiom.        习语                                   
    IEEE          兼容一个或多个 IEEE 标准  
    ISO           兼容一个或多个 ISO 标准   
    Math.         数学                                 
    Trad.         传统                                 
    n.            名词                                        
    v.            动词                                        
    v.t.          及物动词                             


### 非字母的

**()** ['nil], n. 编写符号 nil 的一个替代表示, 用于强调 nil 作为空列表[empty list]的使用. 

### A

**absolute** adj. 1. (指时间[time]) 表示一个特定的时间点. 2. (指路径名[pathname]) 表示目录层次结构中的特定位置. 见相对[relative].

**access** n., v.t. 1. v.t. (一个位置[place], 或数组[array]) 去读取[read[1]]或写入[write[1]]这个[place]的值[value]或这个数组[array]的元素[element]. 2. n. (指位置[place]) 尝试去访问[access[1]]这个位置[place]的值[valuea].

**accessibility** n. 可访问[accessible]的状态.

**accessible** adj. 1. (指对象[object]) 能够被引用. 2. (指一个类[class]的实例[instance]中共享槽[shared slot]或局部槽[local slot]) 已经被这个实例[instance]的类[class]定义或从这个类[class]的超类[superclass]中继承. 3. (指一个包[package]中的一个符号[symbol]) 能够在那个包[package]是当前包时不使用包前缀[package prefix]引用, 不管那个符号[symbol]出现[present]在那个包[package]中或是继承的.

**accessor** n. 执行一个访问[access]的一个操作符[operator]. 见读取函数[reader]和写入函数[writer].

**active** adj. 1. (指处理者[handler], 重启动[restart], 或捕获标记[catch tag]) 已经被建立但是还没有狼狈废除. 2. (指一个数组[array]的元素[element]) 有着大于等于零, 但是小于填充指针[fill pointer] (如果有的话) 的索引. 对于一个没有填充指针[fill pointer]的数组[array], 所有元素[element]都被认为是活跃的[active].

**actual adjustability** n. (指数组[array]) 和这个数组[array]关联的一个广义 boolean [generalized boolean], 表示这个数组[array]实际上可调整的[actually adjustable]. 参见明确可调整性[expressed adjustability]和 adjustable-array-p.

**actual argument** n. Trad. 一个实参[argument].

**actual array element type** n. (指数组[array]) 这个数组[array]实际特化的类型[type], 它是这个数组[array]的表达数组元素类型[expressed array element type]的提升数组元素类型[upgraded array element type]. 见函数[function] array-element-type.

**actual complex part type** n. (指复数[complex]) 实际用来表示这个复数[complex]的实部和虚部的类型[type], 它是这个复数[complex]的表达复数部分类型[expressed complex part type]的提升复数部分类型[upgraded complex part type].

**actual parameter** n. Trad. 一个实参[argument].

**actually adjustable** adj. (指数组[array]) 这样的 adjust-array 可以通过直接修改来直接调整它的性质. 只有在已知该数组[array]是明确可调整的[expressly adjustable], 或者该数组[array]已经被 adjustable-array-p 显式地测试过的情况下, 一个符合规范的程序[conforming program]可以认为这个数组[array]实际上可调整的[actually adjustable].

**adjustability** n. (指数组[array]) 1. 明确可调整性[expressed adjustability]. 2. 实际可调整性[actual adjustability].

**adjustable** adj. (指数组[array]) 1. 明确可调整的[expressly adjustable]. 2. 实际可调整的[actually adjustable].

**after method** n. 一个有着 :after 限定符[qualifier]的方法[method].

**alist** ['ay,list], n. 一个关联列表[association list].

**alphabetic** n., adj. 1. adj. (一个字符[character]的) 是标准字符[standard character] A 到 Z 或 a 到 z 的一个, 或者是任何具体实现定义的[implementation-defined]有着大小写[case]的字符, 或者是某个其他的由具体实现[implementation]定义为字母的[alphabetic[1]]的图形[graphic]字符[character]. 2. a. n. 一个字符的几种标记成分特质[constituent trait]中的一个. 关于详情, 见章节 2.1.4.1 (标记成分字符) 和章节 2.2 (读取器算法). b. adj. (一个字符[character]的) 是一个在当前读取表中有着标记成分[constituent]语法类型[syntax type]并且有着标记成分特质[constituent trait]字母[alphabetic[2a]]的字符. 见 Figure 2-8.

**alphanumeric** adj. (一个字符[character]的) 是一个字母[alphabetic[1]]字符[character]或一个数值[numeric]字符.

**ampersand** n. 被称为 "ampersand" (&) 的标准字符[standard character]. 见 Figure 2-5.

**anonymous** adj. 1. (指一个类[class]或函数[function]) 没有名字[name] 2. (指重启动[restart]) 有一个为 nil 的名字[name].

**apparently uninterned** adj. 有着一个为 nil 的 home 包[home package]. (一个明确解除捕捉[apparently uninterned]的符号[symbol]可能是也可能不是一个未捕捉的[uninterned]符号[symbol]. 未捕捉的[uninterned]符号[symbol]有着一个为 nil 的 home 包[home package], 但是那些已经从 home 包[home package]中被解除捕捉[uninterned]的符号[symbol]也有着一个为 nil 的 home 包[home package], 即便它们可能仍然被捕获[interned]在某个其他包[package]中.)

**applicable** adj. 1. (指处理者[handler]) 是一个可应用处理者[applicable handler]. 2. (指方法[method]) 是一个可应用[applicable method]. 3. (指重启动[restart]) 是一个可应用[applicable restart].

**applicable handler** n. (指一个要被发送的状况[condition]) 一个关联类型包含这个状况[condition]的活跃[active]处理者[handler].

**applicable method** n. (指使用实参[argument]调用的广义函数[generic function]) 该广义函数[generic function]的一个方法[method], 其中这些参数[argument]满足这个方法[method]的参数特化符[parameter specializer]. 见章节 7.6.6.1.1 (选择可应用的方法).

**applicable restart** n. 1. (指状态[condition]) 当给定这个状况[condition]作为参数时, 关联的测试条件返回 true 的一个活跃[active]处理者[handler]. 2. (指非特定状况[condition]) 当给定 nil 作为参数时, 关联的测试条件返回 true 的一个活跃[active]处理者[handler].

**apply** v.t. (一个函数[function]到一个[list]) 使用这个列表[list]的元素[element]作为参数来调用这个函数[function]. "应用函数 + 到整数列表会返回这个列表的元素的总和(Applying the function + to a list of integers returns the sum of the elements of that list)".

**argument** n. 1. (一个函数[function]的) 当这个函数[function]被调用时, 作为数据提供给它的对象[object]. 2. (一个格式化控制[format control]的) 一个格式化参数[format argument].

**argument evaluation order** n. 一个函数调用中实参[argument]的求值顺序. "Common Lisp 的参数求值顺序是从左到右的". 见章节 3.1 (求值).

**argument precedence order** n. 将可应用方法[applicable method]按照优先级顺序排序时, 考虑给广义函数[generic function]的参数[argument]的顺序.

**around method** n. 有着 :around 限定符[qualifier]的方法[method].

**array** n. 一个 array 类型[type]的对象[object], 作为被安排在一个笛卡儿坐标系统(Cartesian coordinate system)的其他对象[object]的容器.

**array element type** n. (指数组[array]) 1. 一个和数组[array]关联的类型[type], 并且这个数组[array]的所有元素[element]都被约束为它的一个成员. 2. 这个数组的实际数组元素类型[actual array element type]. 3. 这个数组[array]的表达数组元素类型[expressed array element type].

**array total size** n. 一个数组[array]中的元素[element]的总数量, 由这个数组[array]的维数[dimension]的乘积计算得来. (因此一个零维数组[array]的大小为一.)

**assign** v.t. (一个变量[variable]) 去改变一个已经被建立的绑定[binding]的变量[variable]的值[value]. 见特殊操作符[special operator] setq.

**association list** n. 一个表示键[key]和值[value]关联的 cons 的列表[list], 其中每一个 cons 的car 为键[key]而 cdr 为那个键[key]关联的值[value].

**asterisk** n. 标准字符[standard character], 可被称为 "asterisk" 或 "star" (*). 见 Figure 2-5.

**at-sign** n. 标准字符[standard character], 可被称为 "commercial at" 或 "at sign" (@). 见 Figure 2-5.

**atom** n. 任何不是一个 cons 的对象[object]. "一个向量是一个原子(A vector is an atom)".

**atomic** adj. 是一个原子[atom]. "数字 3, 符号 foo, 还有 nil 都是原子的(The number 3, the symbol foo, and nil are atomic)".

**atomic type specifier** n. 一个原子的[atomic]类型指定符[type specifier]. 对于每一个原子类型指定符[atomic type specifier], x, 这里有一个没有提供参数的等价符合类型指定符[compound type specifier], (x).

**attribute** n. (指字符[character]) 这个字符[character]的程序可见的方面. 一个字符[character]的仅有标准化[standardized]属性[attribute]是它的码值[code[2]], 但是允许具体实现[implementation]去持有额外的具体实现定义的[implementation-defined]属性[attribute]. 见章节 13.1.3 (字符属性). "支持字体的实现可能使字体信息成为字符的属性, 而其他实现可能分别表示字体信息和字符".

**aux variable** n. 出现在 lambda 列表[lambda list]中由 &aux 引入的部分的一个变量[variable]. 不想 lambda-list 引入的所有其他变量[variable], 辅助变量[aux variable]不是参数[parameter].

**auxiliary method** n. 两组方法[method] (主方法[primary method]集合是另一组)中的一组方法[method]的成员, 它们在方法[method]的广义函数[generic function]上形成方法[method]集合的穷举分区. 如何确定这些集合取决于方法组合[method combination]类型; 见章节 7.6.2 (方法的介绍). 

### B

**backquote** n. 标准字符[standard character], 被称为 "抑音符" 或 "反引号" (`). 见 Figure 2-5.

**backslash** n. 标准字符[standard character], 被称为 "反斜线" 或 "反斜杠" (\\). See Figure 2-5.

**base character** n. 一个 base-char 类型[type]的字符[character].

**base string** n. 一个 base-string 类型[type]的字符串[string].

**before method** n. 一个修饰符[qualifier]为 :before 的方法.

**bidirectional** adj. (指流[stream]) 同时为一个输入[input]流[stream]和一个输出[output]流[stream].

binary adj. 1. (指流[stream]) 元素类型[element type]为 integer 类型[type]的子类型[subtype]的一个流[stream]. 在一个二进制[binary]输入[input]流[stream]上最基本的操作是 read-byte 而在一个二进制[binary]输出[output]流[stream]上最基本的操作是 write-byte. 见字符[character]. 2. (指文件[file]) 已经通过打开一个二进制[binary]流[stream]来创建. (这是否为一个文件[file]的可见测方面, 或者任何给定的字符[character]文件[file]是否被当作一个二进制[binary]文件[file]处理是依赖于具体实现的[implementation-dependent].)

**bind** v.t. (一个变量[variable]) 去建立这个变量[variable]的一个绑定[binding].

**binding** n. 一个名字[name]和那个名字[name]所表示的之间的一个关联. "一个词法绑定是一个名字和它的值之间的一个词法关联". 当这个术语绑定[binding]被一个命名空间[namespace]的名字限定时, 例如 "变量(variable)" 或 "函数(function)", 它约束这个绑定[binding]到指定的命名空间[namespace], 就像在: "let 建立变量绑定." 或 "let 建立多个变量的绑定" 中

**bit** n. 一个 bit 类型[type]的对象[object]; 也就是说, 是整数[integer] 0 或者整数[integer] 1.

**bit array** n. 一个 (array bit) 类型[type]的特化数组[array], 并且它的元素是 bit 类型[type].

**bit vector** n. 一个 bit-vector 类型[type]的特化向量[vector], 并且它的元素是 bit 类型[type].

**bit-wise logical operation specifier** n. 一个命名了可以被 boole 函数执行的十六个可能的位逻辑操作符之一的对象[object], 并且它是常变量[constant variable] boole-clr, boole-set, boole-1, boole-2, boole-c1, boole-c2, boole-and, boole-ior, boole-xor, boole-eqv, boole-nand, boole-nor, boole-andc1, boole-andc2, boole-orc1, 或 boole-orc2 之一的值[value].

**block** n. 一个已命名的词法退出点[exit point], 由 block 显式建立或者由 loop, do 和 prog 之类的操作符隐式建立, 通过使用带有这个语句块[block]的名字的 return-from 表达式形式, 可以将控制和值转移到该退出点.

**block tag** n. 在一个 block 表达式形式[form]内命名由这个 block 表达式形式[form]建立的语句块[block]的符号[symbol]. 见 return 或 return-from.

**boa lambda list** n. 一个语法上像普通 lambda 列表[ordinary lambda list]但是是"按参数顺序"处理的风格的 lambda 列表[lambda list]. 见章节 3.4.6 (Boa Lambda 列表).

**body parameter** n. 特定 lambda 列表[lambda list]中的一个可用的形参[parameter], 从符合规范的程序[conforming program]的视角看, 各种方面它就像是剩余参数[rest parameter], 除了它是由 &body 引入而不是 &rest 引入. (允许具体实现[implementation]提供区分主体参数[body parameter]和剩余参数[rest parameter]的扩展---比如, 使用主体参数[body parameter]定义的操作符[operator]的表达式形式[form]打印出来可能和使用剩余参数[rest parameter]定义的操作符[operator]的表达式形式[form]稍有不同.)

**boolean** n. 一个 boolean 类型[type]的对象[object]; 这也就是说, 下面这些对象[object]之一: 符号 t (表示 true), 或者符号 nil (表示 false). 见广义 boolean [generalized boolean].

**boolean equivalent** n. (指对象[object] O1) 当 O1 和 O2 都被视为广义 boolean [generalized boolean]时, 与 O1 有相同真实值的任何对象[object] O2.

**bound** adj., v.t. 1. adj. 在一个绑定[binding]中有一个关联的表示. "这些由一个 let 命名的变量在它的主体中是被绑定的(The variables named by a let are bound within its body)". 见未绑定的[unbound]. 2. adj. 有着一个遮蔽[shadows[2]]另一个的局部绑定[binding]. "在 princ 函数中时, 变量 \*print-escape* 是被绑定的(The variable \*print-escape* is bound while in the princ function)". 3. v.t. 绑定[bind]的过去时态.

**bound declaration** n. 一种声明[declaration], 它引用一个变量[variable]或函数[function]或者与它们关联, 并且出现在建立这个变量[variable]或函数[function]的特殊表达式形式[special form]中, 但是在这个特殊表达式形式[special form]的主体之前 (具体来说, 就是在那个表达式形式[form]主体的头部). (如果一个绑定声明[bound declaration]引用一个函数[function]绑定[binding]或者一个词法变量[lexical variable]绑定[binding], 这个声明[declaration]的作用域[scope]就是那个绑定[binding]的作用域[scope]. 如果这个声明[declaration]引用了动态变量[dynamic variable]绑定[binding], 那么这个声明[declaration]的作用域[scope]和这个绑定[binding]是词法而非动态时的作用域[scope]相同.)

**bounded** adj. (指序列[sequence] S, 由一对有序的绑定索引[bounding index] istart 和 iend 表示) 限定为 S 的元素[element]的子范围, 其中包括以 istart 索引的元素[element]开始(并包括), 直到(但不包括) iend 索引的元素[element]为止的每个元素[element].

**bounding index** n. (指一个长度[length]为 n 的序列[sequence]) 一个概念上的整数[integer]对 istart 和 iend, 分别称为 "上边界索引" 和 "下边界索引", 使 0 <=istart <=iend <=n, 因此它划分了一个由 istart 和 iend 限定[bounded]的序列[sequence]的子范围.

**bounding index designator** (指序列[sequence]) 作为有序对放在一起的两个对象[object]中的一个, 表现为序列[sequence]的边界索引[bounding index]的标识符[designator]; 这也就是说, 它们表示这个序列[sequence]的边界索引[bounding index], 并且可以是一个整数[integer] (表示它自身)和 nil (表示这个序列[sequence]的长度[length]), 或者两个整数[integer] (每一个都表示它自身).

**break loop** n. 正常 Lisp read-eval-print 循环[Lisp read-eval-print loop]的一个递归进入的变体, 通常因为调试的目的而暂停了正在进行的其他表达式形式[form]的求值[evaluation]. 通常, 一个中断循环[break loop]提供以某种方式退出的能力来继续中断的计算. 见函数[function] break.

**broadcast stream** n. 一个 broadcast-stream 类型[type]的输出[output]流[stream].

**built-in class** n. 一个类[class], 它是类[class] built-in-class 的一个广义实例[generalized instance].

**built-in type** n. Figure 4-2 中的类型[type]之一.

**byte** n. 1. 一个整数[integer]中的相邻位. (在程序中, 具体的比特数可以随点而变化; 见函数[function] byte.) 2. 在一个特定范围内的整数[integer]. (在程序中, 具体的范围可以随点而变化; 见函数[function] open 和 write-byte.) <!--TODO 待校验-->

**byte specifier** n. 一个有着依赖于具体实现[implementation-dependent]的性质的对象[object], 它由函数[function] byte 返回并且指定了一个整数[integer]中要被诸如 ldb 这样的函数[function]用作字节[byte]的位的范围. 

### C

**cadr** ['ka,duhr], n. (指对象[object]) 那个对象[object]的 cdr 的 car.

**call** v.t., n. 1. v.t. (带实参[argument]的函数[function]) 使该函数[function]所表示的代码[code]在基于这个实参[argument]为其形参[parameter]的值[value]建立绑定[binding]的环境[environment]中执行. "使用参数 5 和 1 来调用函数 + 会产生一个 6 的值 (calling the function + with the arguments 5 and 1 yields a value of 6)." 2. n. 函数[function]被调用的一种情况[situation].

**captured initialization form** n. 一个初始化表达式形式[initialization form]以及定义这个初始化表达式形式[initialization form]的表达式形式[form]被求值时所在的词法环境[lexical environment]. "每个新添加的共享槽都被设置为求值新类的 defclass 表达式形式中指定的槽的捕获初始化表达式形式的结果 (Each newly added shared slot is set to the result of evaluating the captured initialization form for the slot that was specified in the defclass form for the new class)."

**car** n. 1. a. (指 cons) 对应于给 cons 的第一个参数[argument]的 cons 的成员. "函数 rplaca 修改一个 cons 的 car (The function rplaca modifies the car of a cons)." b. (指列表[list]) 这个列表[list]的第一个元素[element], 如果这个列表[list]是空列表[empty list]那么就是 nil. 2. 被保存在 car[1]中的那个对象[object]. "函数 car 返回一个 cons 的 car."

**case** n. (一个字符[character]) 大写或小写的属性. 不是所有字符[character]都有大小写[case]. "The characters #\A and #\a have case, but the character #\$ has no case". 见章节 13.1.4.3 (大小写字符) 以及函数[function] both-case-p.

**case sensitivity mode** n. 符号[symbol] :upcase, :downcase, :preserve, 或 :invert 的其中之一.

**catch** n. 由一个 catch 表达式形式[form]建立在它主体的动态作用域[dynamic scope]中的一个退出点[exit point], 它由一个捕捉标签[catch tag]命名, 并且控制和值[value]可以 throw 给它.

**catch tag** n. 一个命名一个活跃[active]捕捉[catch]的对象[object]. (如果不止一个带有相同捕捉标签[catch tag]的捕捉[catch]是活跃的[active], 那么只可能 throw 到最内部的那个捕捉[catch], 因为外面那个被遮蔽[shadowed[2]]了.)

**cddr** ['kduh,duhr] 或 ['kuh,dduhr], n. (指对象[object]) 那么对象[object]的 cdr 的 cdr.

**cdr** ['k,duhr], n. 1. a. (指 cons) 对应于给 cons 的第二个实参[argument]的 cons 的成员; 另一个成员就是 car. "函数 rplacd 修改一个 cons 的 cdr (The function rplacd modifies the cdr of a cons)." b. (指一个列表[list] L1) 要么是包含  L1 的第一个之后的元素的列表[list] L2, 如果 L1 是空列表[empty list]那么就是 nil. 2. 保存在 cdr[1] 中的那个对象[object]. "函数 cdr 返回一个 cons 中的 cdr (The function cdr returns the cdr of a cons)."

**cell** n. Trad. (指对象[object]) 那个对象[object]的一个概念槽[slot]. 一个符号[symbol]的动态变量[dynamic variable]和全局函数[function]绑定[binding]有时分别被引用为它的值存储格[value cell]和函数存储格[function cell].

**character** n., adj. 1. n. 一个 character 类型[type]的对象[object]; 这也就是说, 一个表示一个文本的聚合总量中的单一标记; 见章节 13.1 (字符概念). 2. adj. a. (一个流[stream]的) 有着一个是 character 类型[type]的子类型[subtype]的元素类型[element type]. 在一个字符[character]输入[input]流[stream]上最基本的操作是 read-char 而在一个字符[character]输出[output]流[stream]上是 write-char. 见二进制[binary]. b. (一个文件[file]的) 已经通过打开一个字符[character]流[stream]来创建了. (这个是否是这个文件[file]的可检查方面, 或者任何给定的二进制[binary]文件[file]是否可以被当作一个字符[character]文件[file]是依赖于具体实现的[implementation-dependent].)

**character code** n. 1. 一个字符[character]的多个可能的属性[attribute]之一. 2. 一个小于 char-code-limit 的值的非负整数[integer], 它适用于字符码[character code[1]].

**character designator** n. 一个字符[character]的标识符[designator]; 这也就是说, 一个对象[object], 表示一个字符[character]并且为以下之一: 长度 1 的字符串[string]的一个标识符[designator] (表示它仅有的那个字符[character]元素[element]), 或者一个字符[character] (表示它自身).

**circular** adj. 1. (指列表[list]) 一个环状列表[circular list]. 2. (指一个任意对象[object]) 有着一个成员, 元素[element], 标记成分[constituent[2]], 或子表达式[subexpression] (视情况而定) 为这个对象[object]自身.

**circular list** n. 一个没有终止的 cons 链, 因为这个链中的某个 cons 是后面的一个 cons 的 cdr.

**class** n. 1. 一个对象[object],  它决定了那些被称为它的直接实例[direct instance]的其他对象[object]集合的结构和行为, 给那些被称为非直接实例[indirect instance]的其他对象[object]集合提供结构和行为, 并且充当一组被称为它的广义实例[generalized instance]的对象[object]的类型指定符[type specifier]. "类 integer 是类 number 的子类 (The class integer is a subclass of the class number)." (注意, 措辞 "类 foo" 经常指代更精确的术语 "名为 foo 的类[class]" --- 在这两种情况中, 表示的都是一个类[class]对象[object] (不是一个符号[symbol]).) 2. (指一个对象[object]) 对象[object]是其直接实例[direct instance]的唯一确定类[class]. 见函数[function] class-of. "由 gensym 返回的对象的类是 symbol (The class of the object returned by gensym is symbol)." (注意, 在这种用法中, "它的类[class]是 foo"之类的短语经常被替换为更精确的短语"它的类[class]是名为 foo 的类[class]"---在这两种情况中, 表示的都是一个类[class]对象[object] (不是一个符号[symbol]).)

**class designator** n. 一个类[class]的标识符[designator]; 这也就是说, 一个对象, 它表示一个类[class]并且它是以下之一: 一个符号[symbol] (表示由这个符号[symbol]命名的这个类[class]; 见函数[function] find-class) 或一个类 (表示它自身).

**class precedence list** n. 类[class]及其超类[superclass]上的唯一总排序, 与类[class]及其超类[superclass]的局部优先级[local precedence order]顺序一致. 关于详细信息, 见章节 4.3.5 (确定类的优先级列表).

**close** v.t. (一个流[stream]) 终止流[stream]作为数据的源或接收器的使用, 允许具体实现[implementation]去收回它的内部数据结构, 并且去释放任何在这个流[stream]被打开时锁定的外部资源.

**closed** adj. (指一个流[stream]) 已经被关闭 (见 close). 一些 (但不是全部) 在打开的[open]流[stream]上是合法的操作在关闭的[closed]流[stream]上是不合法的. 见章节 21.1.1.1.2 (打开和关闭的流).

**closure** n. 一个词法闭包[lexical closure].

**coalesce** v.t. (类似的[similar]字面化[literal]对象[object]) 来巩固这些对象[object]的标识, 使它们成为相同的[same]对象[object]. 见章节 3.2.1 (编译器术语).

**code** n. 1. Trad. 要执行的操作的任何表示形式, 无论是概念上的还是作为实际对象[object], 例如表达式形式[form], lambda 表达式[lambda expression], 函数[function]类型[type]的对象[object], 源码文件[source file]中的文本, 或者一个编译后文件[compiled file]中的指令序列. 这是一个通用术语; 表示的具体性质取决于它的上下文. 2. (指字符[character]) 一个字符码[character code].

**coerce** v.t. (一个对象[object]为一个类型[type]) 通过遵循一组强制转换规则从给定对象[object]生成一个对象[object], 而不修改该对象[object], 这些强制转换规则必须针对使用该术语的任何上下文进行特定说明. 得到的对象[object]必须是指定的类型[type], 除非该类型是类型[type] complex 的子类型[subtype]; 在这个情况中, 如果产生一个虚部为零的复有理数[complex rational], 则结果是一个有理数[rational]而不是一个复数[complex]---见章节 12.1.5.3 (复数的正规表示规则).

**colon** n. 标准字符[standard character], 它被称为 "冒号" (:). 见 Figure 2-5.

**comma** n. 标准字符[standard character], 它被称为 "逗号" (,). 见 Figure 2-5.

**compilation** n. 编译器[compiler]编译代码[code]的过程.

**compilation environment** n. 1. 一个环境[environment], 表示编译器[compiler]所知的关于一个要被编译的表达式形式[form]的信息. 见章节 3.2.1 (编译器术语). 2. 一个对象[object], 表示编译环境[compilation environment[1]]并且被用作给一个宏函数[macro function]的第二个参数 (它为这个宏函数[macro function]定义中的任何 &environment 形参[parameter]提供一个值[value]).

**compilation unit** n. 发生单个编译单元的时间间隔. 见宏[macro] with-compilation-unit.

**compile** v.t. 1. (代码[code]) 执行代码[code]的语义预处理, 通常优化代码的一个或多个质量, 如运行时执行速度或运行时存储使用情况. 编译的最小语义要求是它必须移除所有宏调用并且并安排在运行时之前解析所有加载时的值[ load time value]. 2. (一个函数[function]) 来产生一个 compiled-function 类型[type]的新对象[object], 它表示编译这个函数[function]表示的代码[code]的结果. 见函数[function] compile. 3. (一个源码文件[source file]) 来产生一个来自于一个源码文件[source file]的编译后文件[compiled file]. 见函数[function] compile-file.

**compile time** n. 编译器[compiler]处理源代码[source code]的持续时间.

**compile-time definition** n. 编译环境[compilation environment]中的一个定义.

**compiled code** n. 1. 编译后的函数[compiled function]. 2. 表示编译后的函数[compiled function]的代码[code], 例如一个编译后文件[compiled file]的内容.

**compiled file** n. 一个文件[file], 表示编译那些出现在对应源文件[source file]中的表达式形式[form]的结果, 并且可以被加载. 见函数[function] compile-file.

**compiled function** n. 一个 compiled-function 类型[type]的对象[object], 它是一个已经被编译的函数[function], 不包含必须在运行时被展开的宏[macro]的引用, 并且不包含对加载期值[load time value]的无法解决的引用.

**compiler** n. 一个工具, 它是 Lisp 的一部分, 将代码[code]转换为一个依赖于具体实现的[implementation-dependent]可以被高效地表示和执行的形式. 函数 compile 和 compile-file 允许程序去调用这个编译器[compiler].

**compiler macro** n. 全局定义的函数[function]或宏[macro]的辅助宏定义, 它可能被任何给定的符合规范的实现[conforming implementation]调用, 也可能不被调用, 它必须保留全局定义的函数[function]或宏[macro]的语义, 但可能执行一些额外的优化. (不像一个宏[macro], 一个编译器宏[compiler macro]不会扩展 Common Lisp 的语法; 相反, 它为一些现有的语法或功能提供了一种替代的实现策略.)

**compiler macro expansion** n. 1. 通过一个编译器宏[compiler macro]将一个表达式形式[form]转换为另一个表达式形式[form]的过程. 2. 这个过程产生的表达式形式[form].

**compiler macro form** n. 一个操作符[operator]被定义为编译器宏[compiler macro]的函数表达式形式[function form]或宏表达式形式[macro form], 或者一个 funcall 表达式形式[form], 它的第一个实参[argument]是一个 function 表达式形式[form], 这个表达式形式的实参[function]是一个被定义为编译器宏[compiler macro]的函数[function]的名字[name].

**compiler macro function** n. 一个两个参数的函数[function], 两个参数是一个表达式形式[form]和一个环境[environment], 它通过生成一个表达式形式[form]来实现编译器宏展开[compiler macro expansion], 该表达式形式[form]可以用来替代原始参数表达式形式[form], 也可以是 nil, 表示不应该替换原始表达式形式[form]. 见章节 3.2.2.1 (编译器宏).

**complex** n. 一个 complex 类型[type]的对象[object].

**complex float** n. 一个 complex 类型[type]的对象[object], 它的复数部分类型[complex part type]是 float 的一个子类型[subtype]. 一个复浮点数[complex float]是一个复数[complex], 但不是一个浮点数[float].

**complex part type** n. (指复数[complex]) 1. 被用于表示复数[complex]的实部和虚部的类型[type]. 2. 复数[complex]的实际复数部分类型[actual complex part type]. 3. 复数[complex]的表达复数部分类型[expressed complex part type].

**complex rational** n. 一个 complex 类型[type]的对象[object], 它的复数部分类型[complex part type]是 rational 的子类型[subtype]. 一个复有理数[complex rational]是一个复数[complex], 但不是一个有理数[rational]. 没有虚部为零的复有理数[complex rational], 因为这样一个数字总是被 Common Lisp 表示为一个 rational 类型[type]的对象[object]; 见章节 12.1.5.3 (复数的正规表示规则).

**complex single float** n. 一个 complex 类型[type]的对象[object], 它的复数部分类型[complex part type]是 single-float 的子类型[subtype]. 一个复单精度浮点数[complex single float]是一个复数[complex], 但不是一个单精度浮点数[single float].

**composite stream** n. 一个流[stream], 它由一个或多个流[stream]组成. "make-synonym-stream 创建一个复合流 (make-synonym-stream creates a composite stream)."

**compound form** n. 一个非空[non-empty]列表[list], 它是一个表达式形式[form]: 一个特殊表达式形式[special form], 一个 lambda 表达式形式[lambda form], 一个宏表达式形式[macro form], 或者一个函数表达式形式[function form].

**compound type specifier** n. 一个类型指定符[type specifier], 它是一个 cons; 也就是说, 一个不是原子类型指定符[atomic type specifier]的类型指定符[type specifier]. "(vector single-float) 是一个符合类型指定符 ((vector single-float) is a compound type specifier)."

**concatenated stream** n. 一个 concatenated-stream 类型[type]的输入[input]流[stream].

**condition** n. 1. 表示一种情况[situation]的对象[object]---通常, 但不一定是在发送期间. 2. 一个 condition 类型[type]的对象[object].

**condition designator** n. 一个或多个对象[object], 它们组合在一起表示现有的状况[condition]对象[object]或要隐式创建的状况[condition]对象[object]. 关于详细信息, 见章节 9.1.2.1 (状况标识符).

**condition handler** n. 一个函数[function], 它可能被发送的动作所调用, 它接收要被发送的状况[condition]作为它仅有的参数, 并且允许它去处理[handle]这个状况[condition]或拒绝[decline]. 见章节 9.1.4.1 (发送).

**condition reporter** n. 一个函数[function], 它描述了当 Lisp 打印器[Lisp printer]被调用而 \*print-escape* 是 false 时一个状况[condition]如何被打印. 见章节 9.1.3 (打印状况).

**conditional newline** n. 输出中的一个点, 可以由美观打印器[pretty printer]任意插入新行[newline]. 这里有四种条件换行[conditional newline], 称为 "线性风格(linear-style)", "填充风格(fill-style)", "吝啬风格(miser-style)", 以及 "强制风格(mandatory-style)". 见函数[function] pprint-newline 和章节 22.2.1.1 (输出排列的动态控制).

**conformance** n. 通过适当和完全遵守本规范的要求而达到的状态. 见章节 1.5 (规范性).

**conforming code** n. 符合规范的程序[conforming program]中的所有部分的代码[code].

**conforming implementation** n. 一个具体实现[implementation], 用来强调对所有规范性标准的完全和正确的遵守. 一个符合规范的实现[conforming implementation]能够接受符合规范的程序[conforming program]作为输入, 准备执行该程序[program], 并按照本规范执行所准备的程序[program]. 如果没有扩展干扰任何符合规范的程序[conforming program]的正确功能, 已扩展的具体实现[implementation]仍然可以是符合规范的实现[conforming implementation].

**conforming processor** n. ANSI 一个符合规范的实现[conforming implementation].

**conforming program** n. 一个程序[program], 用于强调程序的正确性仅依赖于 Common Lisp 的文档化方面, 因此可以期望在任何符合规范的实现[conforming implementation]中正确运行.

**congruent** n. 符合 lambda 列表[lambda list]一致性的规则, 就像章节 7.6.4 (广义函数的所有方法的一致 Lambda-list) 中详述的那样.

**cons** n.v. 1. n. 一个有着两个成员的复合数据对象[object], 这两个成员称为 car 和 cdr. 2. v. 去创建这样一个对象[object]. 3. v. Idiom. 去创建任何对象[object], 或者去分配存储.

**constant** n. 1. 一个常量表达式形式[constant form]. 2. 一个常变量[constant variable]. 3. 一个常量对象[constant object]. 4. 一个自求值[self-evaluating object].

**constant form** n. 求值[evaluation]总是产生[yield]相同值[value]的任何表达式形式[form], 既不影响它被求值时所在的环境, 也不受该环境影响 (除非允许引用环境[environment]中定义的常变量[constant variable]的名称), 并且不影响任何对象[object]的状态, 也不受任何对象[object]的状态的影响, 除非这些对象是由表达式形式[form]本身创建的对象[object]的其他不可访问部分[otherwise inaccessible part]. "参数为一个 quote 表达式形式的一个 car 表达式形式是一个常量表达式形式(A car form in which the argument is a quote form is a constant form)."

**constant object** n. 一个对象[object], 它被约束为不可变的[immutable] (例如, 根据它在程序[program]中的上下文或它的来源). "一个被 compile-file 处理的字面化对象是一个常量对象(A literal object that has been processed by compile-file is a constant object)."

**constant variable** n. 一个变量[variable], 它的值[value]从不改变; 也就是说, 一个关键字[keyword[1]] 或者一个已命名常量[named constant]. "符号 t, nil, :direction, 和 most-positive-fixnum 都是常变量(The symbols t, nil, :direction, and most-positive-fixnum are constant variables)."

**constituent** n., adj. 1. a. n. 作为标记[token]的一部分的字符[character]的语法类型[syntax type]. 关于详情, 见章节 2.1.4.1 (标记成分字符). b. adj. (一个字符[character]) 有着标记成分[constituent[1a]] 语法类型[syntax type[2]]. c. n. 一个标记成分[constituent[1b]]字符[character]. 2. n. (一个复合流[composite stream]) 组成该流[stream]的源或汇聚的几个对象[object]之一.

**constituent trait** n. (一个字符[character]) 一个读取表[readtable]中的一个标记成分[constituent[1b]]字符[character]的几个分类中的一个. 见章节 2.1.4.1 (标记成分字符).

**constructed stream** n. 一个源或接收器是一个 Lisp 对象[object]的流[stream]. 注意, 由于一个流[stream]也是一个 Lisp 对象[object], 因此复合流[composite stream]也被认为是构造流[constructed stream]. "一个字符串流是一个构造流 (A string stream is a constructed stream)."

**contagion** n. 一个过程, 其中不同类型[type]的对象[object]上的操作 (例如, 对混合类型[type]的数字[number]进行算术操作) 产生一个类型[type]由一个实参[argument]类型[type]对其他实参[argument]类型[type]的支配性控制的结果. 见章节 12.1.1.2 (数值运算的传递性).

**continuable** n. (指一个错误[error]) 一个通过 continue 重启动是可校正的[correctable]的错误[error].

**control form** n. 1. 一个表达式形式[form], 它建立一个或多个控制可以转移到的位置. 2. 一个转移控制的表达式形式[form].

**copy** n. 1. (指一个 cons C) 一个带有和 C 相同[same]的 car 和 cdr 的新的[fresh] cons. 2. (指一个列表[list] L) 一个带有和 L 相同[same]元素[element]的新[fresh]列表[list]. (只有列表结构[list structure]是新的[fresh]; 其中的元素[element]都相同[same].) 见函数[function] copy-list. 3. (指一个带有多个元素[element] Ai 的关联列表[association list] A) 一个带有多个元素[element] Bi 的新[fresh]列表[list], 如果 Ai 是 nil 那么对应的每一个都是 nil, 否则就是 cons Ai 的一个拷贝[copy]. 见函数[function] copy-alist. 4. (指一个树[tree] T) 一个带有和 T 相同[same]叶节点[leave]的新[fresh]树[tree]. 见函数[function] copy-tree. 5. (指一个随机状态[random state] R) 一个新[fresh]随机状态[random state], 如果被用作函数[function] random 的参数会产生和 R 产生的相同的 "随机" 值序列. 6. (指一个结构体[structure] S) 一个和 S 相同类型[type]的新[fresh]结构体[structure], 并且每一个槽的值都合 S 的对应槽的值相同[same]. (注意, 由于一个 cons, 一个列表[list], 和一个树之间的区别在于 "视图" 或 "意图", 没有一种通用函数[function]仅根据对象[object]的类型[type]就可以确定这些不同含义中的哪些含义. 区别完全取决于本文档内的文本说明. 比如, 像 "给定列表[list]的一个拷贝[copy] (a copy of the given list)" 或 "列表[list] x 的拷贝" 这样的短语意味着第二个定义.)

**correctable** adj. (指一个错误[error]) 1. (通过一个和该错误[error]关联的重启动[restart]而不是 abort) 能够通过调用该重启动[restart]进行纠正. "函数 cerror 发出一个错误, 它是可以通过 continue 重启动[restart]来校正的 (The function cerror signals an error that is correctable by the continue restart)." (注意, 这个可校正性不是一个错误[error]对象[object]的属性, 而是这个错误[error]发出时生效的动态环境[dynamic environment]的属性. 具体来说, 重启动[restart]与错误[error]状况[condition]对象[object]"关联". 见章节 9.1.4.2.4 (关联重启动和状况).) 2. (当没有特定重启动[restart]被提及时) 至少通过一个重启动[restart]即可纠正[correctable[1]]. "如果导入的任何符号具有与包中已经可访问的某些不同符号相同的名称, 则 import 发出 package-error 类型[type]的可纠正错误 (import signals a correctable error of type package-error if any of the imported symbols has the same name as some distinct symbol already accessible in the package)."

**current input base** n. (在一个动态环境[dynamic environment]中) 这个基数[radix]是这个环境[environment]中的 \*read-base* 的值[value], 并且它是 Lisp 读取器[Lisp reader]和它的相关函数[function]采用的默认基数[radix].

**current logical block** n. 最内部词法闭合的 pprint-logical-block 的上下文.

**current output base** n. (在一个动态环境[dynamic environment]中) 这个基数[radix]是这个环境[environment]中的 \*read-base* 的值[value], 并且它是 Lisp 打印器[Lisp printer]和它的相关函数[function]采用的默认基数[radix].

**current package** n. (在一个动态环境[dynamic environment]中) 这个包[package]是这个环境[environment]中 \*package* 的值[value], 并且它是 Lisp 读取器[Lisp reader]和 Lisp 打印器[Lisp printer]采用的默认包[package].

**current pprint dispatch table** n. (在一个动态环境[dynamic environment]中) 这个美观打印分派表[pprint dispatch table]是这个环境[environment]中 \*print-pprint-dispatch* 的值[value], 并且它是美观打印器[pretty printer]采用的默认美观打印分派表[pprint dispatch table].

**current random state** n. (在一个动态环境[dynamic environment]中) 这个随机状况[random state]是这个环境[environment]中 \*random-state* 的值[value], 并且它是 random 采用的默认随机状态[random state].

**current readtable** n. (在一个动态环境[dynamic environment]中) 那个环境中作为 \*readtable* 的值的读取表, 并且影响着表达式[expressions[2]]被 Lisp 读取器[Lisp reader]解析为对象[object]的方式. 


### D

**data type** n. Trad. 一个类型[type].

**debug I/O** n. 一个双向[bidirectional]流[stream], 它是变量[variable] \*debug-io* 的值[value].

**debugger** n. 一个工具, 它允许用户[user]去交互式地处理一个状况[condition]. 例如, 这个调试器[debugger]可以允许从活跃的[active]的重启动[restart]中交互式地选择一个重启动[restart], 并且它可能执行额外的具体实现定义的[implementation-defined]用于调试的服务.

**declaration** n. 一个全局声明[global declaration]或局部声明[local declaration].

**declaration identifier** n. 符号[symbol] declaration, dynamic-extent, ftype, function, ignore, inline, notinline, optimize, special, 或 type 之一; 或者是一个类型[type]名字[name]的符号[symbol]; 或者是一个已经通过 declaration 声明[declaration]被声明为一个声明标识符[declaration identifier]的符号[symbol].

**declaration specifier** n. 一个表达式[expression], 它可以出现在 declare 表达式或 declaim 表达式形式的顶层, 或作为给 proclaim 的参数, 并且它的 car 为一个声明标识符[declaration identifier], 而它的 cdr 是根据这个声明标识符[declaration identifier]特化的规则来解释的数据.

**declare** v. 去建立[establish]一个声明[declaration]. 见 declare, declaim, 或 proclaim.

**decline** v. (指一个处理者[handler]) 在没有处理这个被发出的状况[condition]的情况下正常返回, 允许发送过程继续, 就好像处理者[handler]不存在一样.

**decoded time** n. 绝对[absolute]时间[time], 表示为九个对象[object]的一个有序集, 合起来, 组成日期时间中一个点的描述, 精确到最近的秒 (除了闰秒[leap second]会被忽略). 见章节 25.1.4.1 (解码时间).

**default method** n. 一个方法[method], 它没有除了类[class] t 以外的参数特化符[parameter specializer]. 这样一个方法[method]总是为一个可应用方法[applicable method], 但是可能被一个更具体的方法[method]所遮蔽[shadow[2]].

**defaulted initialization argument list** n. 初始化参数名称[name]和值[value]的交替列表[list], 其中未提供的初始化参数被缺省了, 用于初始化和重新初始化类[class]的实例[instance].

**define-method-combination arguments lambda list** n. 一个被用于 define-method-combination 的 :argument 选项的 lambda 列表[lambda list]. 见章节 3.4.10 (Define-method-combination 参数 Lambda 列表).

**define-modify-macro lambda list** n. 一个被 define-modify-macro 使用的 lambda 列表[lambda list]. 见章节 3.4.9 (Define-modify-macro Lambda 列表).

**defined name** n. 一个意义已经由 Common Lisp 定义了的符号.

**defining form** n. 一个表达式形式[form], 它有着建立一个定义的副作用. "defun 和 defparameter 是定义表达式形式."

**defsetf lambda list** n. 一个 lambda 列表[lambda list], 它就像是一个普通 lambda 列表[ordinary lambda list], 除了它不允许 &aux 而允许 &environment 的使用. 见章节 3.4.7 (Defsetf Lambda 列表).

**deftype lambda list** n. 一个 lambda 列表[lambda list], 它就像一个宏 lambda 列表[macro lambda list], 除了未提供的可选参数[optional parameter]和关键字参数[keyword parameter]的默认值是符号[symbol] * (而不是 nil). 见章节 3.4.8 (Deftype Lambda 列表).

**denormalized** adj., ANSI, IEEE (指一个浮点数[float]) 符合 IEEE Standard for Binary Floating-Point Arithmetic 所描述的 "非规格化". 例如, 在一个最小可能的指数是 -7 但 0.001 是一个有效尾数的具体实现[implementation]中, 数字 1.0e-10 可能被内部表示为 0.001e-7, 即便标准化[normalized]表示需要它被表示为 1.0e-10 或 0.1e-9. 从本质上讲, 非规范化[denormalized]浮点数[float]的精度通常低于规范化[normalized]浮点数[float].

**derived type** n. 一种类型指定符[type specifier], 它是根据展开为另一个类型指定符[type specifier]来定义的. deftype 定义衍生类型[derived type], 可能还有其他具体实现定义的[implementation-defined]操作符[operator]也这样做.

**derived type specifier** n. 一个衍生类型[derived type]的类型指定符[type specifier].

**designator** n. 一个对象[object], 它表示另一个对象[object]. 在一个操作符[operator]的字典条目中, 如果一个形参被描述为一个类型[type]的标识符[designator], 那么所编写的该操作符[operator]的描述假设已经发生了对该类型[type]的适当强制转换; 也就是说, 那个形参[parameter]已经是表示的类型[type]了. 关于详细信息, 见章节 1.4.1.5 (标识符).

**destructive** adj. (指一个操作符[operator]) 能够修改一个或多个对象[object]的某些程序可见的方面, 这些对象[object]要么是操作符[operator]的显式参数[argument], 要么可以由操作符[operator]直接或间接地从全局环境[global environment]中获得.

**destructuring lambda list** n. 一个扩展 lambda 列表[extended lambda list], 它被用于 destructuring-bind 以及内嵌在宏 lambda 列表[macro lambda list]中. 见章节 3.4.5 (解构 lambda 列表).

**different** adj. 不相同[same] "字符串 'FOO' 和 'foo' 在 equal 下是不同的但是在 equalp 下是相同的 (The strings 'FOO' and 'foo' are different under equal but not under equalp)."

**digit** n. (在一个基数[radix]下) 在可能的数字(0 到 9、a 到 Z 和 a 到 Z)之间的一种字符[character], 它被定义为具有一个相关的数值权重, 作为基数[radix]中的一个数字. 见章节 13.1.4.6 (一个给定基数的数字).

**dimension** n. 1. 一个表示一个数组[array]可以在一个坐标轴中持有的对象[object]数量的非负整数[integer]. 如果这个数组[array]是一个带有填充指针[fill pointer]的向量[vector], 这个填充指针[fill pointer]会被忽略. "那个数组的第二维度大小是 7 (The second dimension of that array is 7)." 2. 一个数组[array]的一个坐标轴. "这个数组有六个维度 (This array has six dimensions)."

**direct instance** n. (指一个类[class] C) 一个类[class]是 C 自身的对象[object], 而不是 C 的某个子类[subclass]. "函数 make-instance 总是返回它的第一个参数表示 (或者由它命名) 的类的直接实例 (The function make-instance always returns a direct instance of the class which is (or is named by) its first argument)."

**direct subclass** n. (指一个类[class] C1) 一个类[class] C2, 使得 C1 是 C2 的一个直接超类[direct superclass].

**direct superclass** n. (指一个类[class] C1) 一个类[class] C2, 它在 C1 的定义中被显式的标识为 C1 的一个超类[superclass].

**disestablish** v.t. 去撤销一个对象[object], 一个绑定[binding], 一个退出点[exit point], 一个标签[tag], 一个处理者[handler], 一个重启动[restart], 或者一个环境[environment]的建立.

**disjoint** n. (指类型[type]) 没有公共的元素[element].

**dispatching macro character** n. 一个宏字符[macro character], 它有一个关联的表指定了跟在这个分派宏字符[dispatching macro character]后的每个字符调用的函数[function]. 见函数[function] make-dispatch-macro-character.

**displaced array** n. 一个数组[array], 它自身没有存储, 而是以指定的偏移位定向到另一个数组[array]的存储, 称为它的目标[target], 以这种方式, 任何访问存储转移数组[displaced array]的尝试都会隐式引用目标数组[target array].

**distinct** adj. 不相同[identical].

**documentation string** n. (在一个定义表达式形式[form]) 一个字面化[literal]字符串[string]由于它出现的上下文 (而不是由于该字符串[string]的某些本质上可观察的方面) 而被视为文档. 在某些情况下, 文档字符串[documentation string]以这样一种方式保存, 以便以后可以通过向函数[function] documentation 提供对象[object]或名称[name]和"种类"来获得它. "在一个 defmacro 表达式形式的代码主体前可以有一个 function 种类的文档字符串 (The body of code in a defmacro form can be preceded by a documentation string of kind function)."

**dot** n. 标准字符[standard character], 可以被称为 "句点(full stop)", "句号(period)", 或 "点(dot)" (.). 见 Figure 2-5.

**dotted list** n. 一个终止原子[atom]不是 nil 的列表[list]. (然而, 一个原子[atom]自身不是一个点列表[dotted list].)

**dotted pair** n. 1. 一个 cdr 为非列表[non-list]的 cons. 2. 任何 cons, 用于强调一个 cons 作为对称数据对的使用.

**double float** n. 一个 double-float 类型[type]的对象[object].

**double-quote** n. 标准字符[standard character], 可称为 "引号(quotation mark)" 或 "双引号(double quote)" ("). 见 Figure 2-5.

**dynamic binding** n. 在一个动态环境[dynamic environment]中的绑定[binding].

**dynamic environment** n. 一个环境[environment]的这个部分包含了带有动态范围[dynamic extent]的绑定[binding]. 一个动态环境[dynamic environment]包括: 由 unwind-protect 建立的退出点[exit point], 以及动态变量[dynamic variable]的绑定[binding], 由 catch 建立的退出点[exit point], 状况处理者[condition handler], 以及重启动[restart].

**dynamic extent** n. 一种范围[extent], 其持续时间受某一特定表达式形式[form]执行过程中的建立点和废除点的限制. 见不确定范围[indefinite extent]. "动态变量绑定有着动态范围 (Dynamic variable bindings have dynamic extent)".

**dynamic scope** n. 带有动态范围[dynamic extent]的不确定作用域[indefinite scope].

**dynamic variable** n. 一个变量[variable], 它的绑定[binding]是在动态环境[dynamic environment]中的. 见 special. 


### E

**echo stream** n. 一个 echo-stream 类型[type]的流[stream].

**effective method** n. 可应用方法[applicable method]的组合, 它在使用一个特定参数[argument]序列调用一个广义函数[generic function]时被执行.

**element** n. 1. (指列表[list]) 一个对象[object], 它是组成这个列表[list]的其中一个 cons 的 car. 2. (指数组[array]) 一个存储在数组[array]中的一个对象[object]. 3. (指序列[sequence]) 一个对象[object], 它是这个序列[sequence]所表示的列表[list]或数组[array]的一个元素[element]. 4. (指类型[type]) 一个对象[object], 它是这个类型[type]所表示的对象集中的一个成员[member]. 5. (指一个输入[input]流[stream]) 一个字符[character]或数字[number] (适用于流[stream]的元素类型[element type]) 它是可从流[stream]中读取的有序对象[object]序列中的一个 (使用 read-char 或 read-byte, 适用于该流[stream]). 6. (指一个输出[output]流[stream]) 一个字符[character]或数字[number] (适用于流[stream]的元素类型[element type]) 它是已经被写入到该流[stream]或者会被写入到该流[stream]的有序对象[object]序列中的一个 (使用 write-char 或 write-byte, 适用于该流[stream]). 7. (指类[class]) 该类[class]的一个广义实例[generalized instance].

**element type** n. 1. (指数组[array]) 数组[array]的数组元素类型[array element type]. 2. (指一个流[stream]) 流[steam]的流元素类型[stream element type].

**em** n. Trad. 一种与上下文相关的度量单位, 通常用于排版, 等于当前字体中字母"M"的显示宽度. (传统上选择字母"M"是因为它通常由字体中最宽的字形[glyph]表示, 而其他字符的宽度通常是 em 的一部分. 在提供比"M"字符更宽的非罗马字符的实现中, 允许另一个字符作为该度量的具体实现定义的[implementation-defined]引用字符, 而"M"只是 em 宽度的一小部分.) 在一个固定宽度字体中, 一个带有 n 个字符的行是 n 个 em 的宽度; 在一个可变宽度字体中, n 个 em 是这样一行的宽度的期望上限.

**empty list** n. 不包含元素[element]的列表[list]. 见 ().

**empty type** n. 这个类型[type]不包含元素[element], 并且它是所有类型[type]的子类型[subtype] (包括它自身). 见 nil.

**end of file** n. 1. 一个输入[input]流[stream]中一个一旦越过就没有进一步数据的点. 在一个交互式流[interactive stream]中是否有这样的点是由具体实现定义的[implementation-defined]. 2. 试图从位于文件末尾[end of file[1]]的输入[input]流[stream]中获取数据时发生的一种情况[situation].

**environment** n. 1. 一个绑定[binding]集合. 见章节 3.1.1 (环境的介绍). 2. 一个环境对象[environment object]. "macroexpand 接受一个可选的环境参数 (macroexpand takes an optional environment argument)".

**environment object** n. 表示词法绑定[lexical binding]集合的一个对象[object], 用于表达式形式[form]的处理中为该表达式形式[form]中的名称[name]提供意义. "macroexpand 接受一个可选环境参数 (macroexpand takes an optional environment argument)". (对象[object] nil 在用作一个环境对象[environment object]时表示空词法环境[null lexical environment]; 传递给宏函数[macro function]的环境参数[environment parameter]的值[value]是带有依赖于具体实现的[implementation-dependent]性质的对象[object], 它表示对应宏表达式形式[macro form]被展开时的环境[environment[1]].) 见章节 3.1.1.4 (环境对象).

**environment parameter** n. 定义表达式形式[defining form] f 中的一个形参[parameter], 它没有对应的实参[argument]; 而是, 该形参[parameter]接受一个环境[environment]对象[object]作为其值, 对应定义表达式形式[defining form] f 出现所在的词法环境[lexical environment].

**error** n. 1. (尽在措辞 "是一个错误 (is an error)" 中) 未指定程序语义, 且结果未定义的一种情况[situation]. 2. 表示一个错误[error]情况[situation]的一个状况[condition]. 见章节 1.4.2 (错误术语). 3. 一个 error 类型[type]的对象[object].

**error output** n. 动态变量[dynamic variable] \*error-output* 的值[value]所指代的输出[output]流[stream].

**escape** n., adj. 1. n. 一个单转义字符[single escape]或多转义字符[multiple escape]. 2. adj. 单转义[single escape]或多转义[multiple escape].

**establish** v.t. 去建立或形成一个绑定[binding], 一个声明[declaration], 一个退出点[exit point], 一个标签[tag], 一个处理者[handler], 一个重启动[restart], 或者一个环境[environment]. "let 建立词法绑定 (let establishes lexical bindings)".

**evaluate** v.t. (一个表达式形式[form]或一个隐式 progn [implicit progn]) 通过应用求值[evaluation]规则来执行这个表达式形式[form] (或者由多个表达式形式[form]组成的隐式 progn [implicit progn]) 所表示的代码[code], 返回零个或多个值.

**evaluation** n. 执行表达式形式[form]的模型, 返回零个或多个值. 这样的执行过程可能被解释器直接实现为一步或者通过先编译这个表达式形式[form]然后执行编译后的代码[code]来实现为两步; 这个选择依赖于上下文和具体实现[implementation]的性质, 但在任何情况下都不会被任何程序检测到. 求值模型的设计方式是, 符合规范的实现[conforming implementation]可能合法地只有一个编译器而没有解释器, 反之亦然. 见章节 3.1.2 (求值模型).

**evaluation environment** n. 一个运行时环境[run-time environment], 宏展开器和 eval-when 指定要被求值的代码在这里被求值. 编译器[compiler]发起的求值都发生在求值环境[evaluation environment]中.

**execute** v.t. Trad. (代码[code]) 去执行由这个代码[code]表示的命令操作.

**execution time** n. 已编译代码[compiled code]正在被执行的时期.

**exhaustive partition** n. (指一个类型[type]) 一组成对[pairwise]不相交[disjoint]的类型[type], 构成一个详尽的并集[exhaustive union].

**exhaustive union** n. (指一个类型[type]) 这个类型[type]的子类型[subtype]的集合, 它们的并集包含了这个类型[type]的所有元素[element].

**exit point** n. 控制表达式形式[control form]中的一个点, 从该点 (例如, block), 通过该点(例如, unwind-protect), 或到该点(例如, tagbody), 可以通过使用另一种控制表达式形式[control form]主动地转移控制和可能的值[value], 也可以通过求值[evaluation]的正常控制和数据流被动地传递值. "catch 和 block 为退出点建立绑定, throw 和 return-from 分别可以转移控制和值到该退出点; tagbody 建立为一个退出点建立一个带有词法范围的绑定, go 可以转移控制到这个绑定; 以及 unwind-protect 建立一个退出点, 控制可能被诸如 throw, return-from, 和 go 这样的操作符转移过它 (catch and block establish bindings for exit points to which throw and return-from, respectively, can transfer control and values; tagbody establishes a binding for an exit point with lexical extent to which go can transfer control; and unwind-protect establishes an exit point through which control might be transferred by operators such as throw, return-from, and go)".

**explicit return** n. 通过使用 return-from (或 return) 将控制 (以及可能的值[value]) 转移到一个语句块[block]的操作.

**explicit use** n. (指表达式形式[form] F 中的一个变量[variable] V) 一个对 V 的引用直接出现在 F 的正常语义中; 换句话说, 这不会暴露表达式形式[form]本身的宏展开[macro expansion]的任何未记录的细节. 然而, 由 F 的展开子表达式形式[subform]暴露的对 V 的引用被认为是对 V 的显式使用[explicit use].

**exponent marker** n. 一个字符, 被用于一个浮点数[float]的文本表示, 来分隔尾数和指数. 下一段中展示了标准读取表[standard readtable]中被定义为指数标记符[exponent marker]的那些字符. 关于更多信息, 见章节 2.1 (字符语法). "在 '3.0d7' 中的指数标记符 'd' 表示这个数字被表示为一个双精度浮点数 (The exponent marker 'd' in '3.0d7' indicates that this number is to be represented as a double float)".

    标记     意义                                  
    D 或 d  double-float                             
    E 或 e  float (见 *read-default-float-format*)  
    F 或 f  single-float                             
    L 或 l  long-float                               
    S 或 s  short-float                              

    Figure 26-1. Exponent Markers

**export** v.t. (一个包[package]中的一个符号[symbol]) 去把这个符号[symbol]添加到这个包[package]的外部符号[external symbol]列表中.

**exported** adj. (指一个包[package]中的一个符号[symbol]) 是这个包[package]的外部符号[external symbol].

**expressed adjustability** n. (指数组[array]) 概念上 (但实际上不一定) 与这个数组[array]相关联的一个广义 boolean [generalized boolean], 表示这个数组[array]是否为明确指定可调整的[expressly adjustable][expressly adjustable]. 参见实际可调整性[actual adjustability].

**expressed array element type** n. (指数组[array]) 这个类型[type]是这个数组[array]的类型声明[type declaration]所指示的数组元素类型[array element type], 或是在其创建时所请求的数组元素类型[array element type], 先于提升后的数组元素类型[upgraded array element type]的选择. (Common Lisp 没有提供一种在运行时直接检测这个类型的方式, 但是允许一个具体实现[implementation]在代码分析期间注意到该类型[type]时去对这个数组[array]的内容和可能在这个数组[array]上执行的操作做出假设, 即使这些假设通常不适用于明确指定数组元素类型[expressed array element type]的提升后的数组元素类型[upgraded array element type].)

**expressed complex part type** n. (指复数[complex]) 这个类型[type]是这个复数[complex]的类型声明[type declaration]所指示的复数部分类型[complex part type], 或是在其创建时所请求的复数部分类型[complex part type], 先于提升后的复数部分类型[upgraded complex part type]的选择. (Common Lisp 没有提供一种在运行时直接检测这个类型的方式, 但是允许一个具体实现[implementation]在代码分析期间注意到该类型[type]时去对这个数组[array]的内容和可能在这个数组[array]上执行的操作做出假设, 即使这些假设通常不适用于明确指定复数部分类型[expressed complex part type]的提升后的复数部分类型[upgraded complex part type].)

**expression** n. 1. 一个对象[object], 经常用于强调这个对象[object]的使用以一种专门的格式来编码或表示信息, 例如程序文本. "在一个 let 表达式形式中的第二个表达式是一个绑定列表(The second expression in a let form is a list of bindings)". 2. 用于表示一个源文件中的一个对象[object]的文本记号. "表达式 'sample 等价于 (quote sample)".

**expressly adjustable** adj. (指数组[array]) 由于在其创建时对该特性的显式请求而实际可调整的[actually adjustable]. 所有明确指定可调整的[expressly adjustable]数组[array]都是实际可调整的[actually adjustable], 但反过来未必如此.

**extended character** n. 一个 extended-char 类型[type]的字符[character]: 一个非基本字符[base character]的字符.

**extended function designator** n. 一个函数[function]的标识符[designator]; 也就是说, 一个表示一个函数[function]的对象[object], 它是以下之一: 一个函数名[function name] (表示在全局环境[global environment]中它命名的函数[function]), 或者一个函数[function] (表示它自身). 如果一个函数名[function name]被用作一个扩展函数标识符[extended function designator]但是它没有作为函数[function]的全局定义, 或者它是一个有着作为宏[macro]或特殊表达式形式[special form]作为全局定义的符号[symbol], 那么后果是未定义的.

**extended lambda list** n. 一个列表, 在形式和目的上类似一个普通 lambda 列表[ordinary lambda list], 但是提供额外的在普通 lambda 列表[ordinary lambda list]中不可用的语法或功能. "defmacro 使用扩展 lambda 列表 (defmacro uses extended lambda lists)".

**extension** n. 在一个 Common Lisp 的具体实现[implementatioin]的一个没有被这个标准所指定的工具.

**extent** n. 定义一个对一个对象, 一个绑定[binding], 一个退出点[exit point], 一个标签[tag], 一个处理者[handler], 一个重启动[restart], 或者一个环境[environment]的引用的时间间隔.

**external file format** n. 一个具有依赖于具体实现[implementation-dependent]的性质的对象[object], 它确定字符[character]在字符[character]文件[file]中外部编码的可能的几种依赖于具体实现的[implementation-dependent]方式中的一个.

**external file format designator** n. 一个外部文件格式[external file format]的标识符[designator]; 也就是说, 一个表示外部文件格式[external file format]的对象[object], 它是以下之一: 符号[symbol] :default (表示一个依赖于具体实现[implementation-dependent]的默认外部文件格式[external file format], 它至少可以容纳基本字符[base character]), 某个被具体实现[implementation]定义为一个外部文件格式标识符[external file format designator]的其他对象[object] (表示一个具体实现定义的[implementation-defined]外部文件格式[external file format]), 或者某个由具体实现定义的外部文件格式[external file format] (表示它自身).

**external symbol** n. (指包[package]) 一个符号[symbol], 它是这个包[package]的 '外部接口(external interface)' 的一部分, 并且被任何其他使用[use]这个包[package]的包[package]所继承[inherited[3]]. 在使用 Lisp 读取器[Lisp reader]时, 如果使用了一个包前缀[package prefix], 则外部符号[external symbol]的名称[name]与包[package]名称[name]之间用单个包标记[package marker]分隔, 而内部符号[internal symbol]的名[name]称与包[package]名称[name]之间用双包标记[package marker]分隔; 见章节 2.3.4 (符号标记).

**externalizable object** n. 一个对象[object], 它在要被文件编译器[file compiler]处理的代码[code]中用作一个字面化[literal]对象[object]. 


### F

**false** n. 符号[symbol] nil, 被用于表示一个断言[predicate]检测的失败.

**fbound** ['ef,band] adj. (指一个函数名[function name]) 被绑定[bound]在函数[function]命名空间[namespace]. (宏[macro]和特殊操作符[special operator]的名称是 fbound 的, 但是它们的值[value]对象[object]的类型[type]和性质是依赖于具体实现的[implementation-dependent]. 更进一步, 定义一个 setf 展开器[setf expander] F 不会导致 setf 函数[setf function] (setf F) 变为已定义的; 同样的, 如果这里这样一个 setf 展开器[setf expander]的定义, 那么当且仅当一个 (setf F) 的函数绑定被独立建立时, 不管是故意的或是巧合, 函数[function] (setf F) 可以是 fbound 的.) 见函数[function] fboundp 和 symbol-function.

**feature** n. 1. Common Lisp, 具体实现[implementation], 或环境[environment]的一个方面或属性. 2.命名一个特性[feature[1]]的一个符号[symbol]. 见章节 24.1.2 (特性). ":ansi-cl 特性出现在所有符合规范的实现中 (The :ansi-cl feature is present in all conforming implementations)".

**feature expression** n. #+ 和 #- 读取器宏[reader macro]使用的特性的一种 boolean 组合, 用于直到 Lisp 读取器[Lisp reader]条件读取表达式[expression]. 见章节 24.1.2.1 (特性表达式).

**features list** n. \*features* 的值[value]所表示的列表[list].

**file** n. 在一个文件系统[file system]中的一个已命名的条目, 有着具体实现定义的[implementation-defined]性质.

**file compiler** n. 编译[compile]文件[file]中包含的源代码[source code], 生成已编译文件[compiled file]作为输出的任何编译器[compiler]. 这个 compile-file 函数是 Common Lisp 提供的这样一个编译器[compiler]的仅有的接口, 但是这里可能有其他具体实现定义的[implementation-defined]机制来调用这个文件编译器[file compiler].

**file position** n. (在一个流[stream]中) 表示这个流[stream]中的位置的非负整数[integer]. 并不是所有的流[stream]都能够表示文件位置[file position]的概念; 在任何操纵文件位置[file position]的操作符的描述中, 没有此概念的流[stream]的行为必须显式声明. 对于二进制[binary]流[stream], 文件位置[file position]表示流[stream]中前面的字节[byte]数. 对于字符[character]流[stream], 约束更加宽松: 文件位置[file position]必须单调递增, 与流[stream]中任意两个连续字符对应的文件位置[file position]之间的增量依赖于具体实现[implementation-dependent].

**file position designator** n. (在一个流[stream]中) 在这个流[stream]中一个文件位置[file position]的标识符[designator]; 也就是说, 符号[symbol] :start (表示 0, 在这个流[stream]中的第一个文件位置[file position]), 符号[symbol] :end (在这个流[stream]中的最后一个文件位置[file position]; 也就是说, 这个流[stream]中最后一个元素[element]后面的位置), 或者一个文件位置[file position] (表示它自身).

**file stream** n. 一个 file-stream 类型[type]的对象[object].

**file system** n. 一种工具, 它允许将数据聚合存储在 Lisp 镜像[Lisp image]外部的某些介质上的命名文件[file]中, 从而在不同的会话[session]之间持久保存.

**filename** n. 一个句柄, 不一定直接表示为对象[object], 它可以被用于引用一个文件系统[file system]中的一个文件[file]. 路径名[pathname]和名称字符串[namestring]是 Common Lisp 中两种可以替代文件名[filename]的对象[object].

**fill pointer** n. (指一个向量[vector]) 和一个向量[vector]关联的一个整数[integer], 它表示上面没有有效[active]元素[element]的索引. (一个填充指针[fill pointer]是一个不大于这个向量[vector]中元素[element]数量的非负整数[integer]. 不是所有向量[vector]都有填充指针[fill pointer].)

**finite** adj. (指一个类型[type]) 有着有限数量的元素[element]. "类型指定符 (integer 0 5) 表示一个有限类型, 但类型指定符 integer 和 (integer 0) 不是 (The type specifier (integer 0 5) denotes a finite type, but the type specifiers integer and (integer 0) do not)".

**fixnum** n. 一个 fixnum 类型[type]的整数[integer].

**float** n. 一个 float 类型[type]的对象[object].

**for-value** adj. (指对一个绑定[binding]的引用[reference]) 作为读取[read[2]]绑定[binding]值[value]的一个引用[reference].

**form** n. 1. 任何要被求值的对象[object]. 2. 一个符号[symbol], 一个复合表达式形式[compound form], 或者一个子求值对象[self-evaluating object]. 3. (对于一个操作符[operator], 例如在 "<<operator>> 表达式形式" 中) 第一个元素[element]为该操作符[operator]的一个复合表达式形式[compound form]. "一个 quote 表达式形式是一个常量表达式形式 (A quote form is a constant form)".

**formal argument** n. Trad. 一个参数[parameter].

**formal parameter** n. Trad. 一个参数[parameter].

**format** v.t. (一个格式化控制[format control]和格式化参数[format argument]) 就像通过 format 一样使用格式化字符串[format string]和格式化参数[format argument]去执行输出.

**format argument** n. 一个对象[object], 它被例如 format 这样解释格式化控制[format control]的函数[function]用作数据.

**format control** n. 一个格式化字符串[format string], 或者一个函数[function], 它遵循由 formatter 宏[macro]返回的函数[function]的参数[argument]规约. 见章节 22.2.1.3 (编译格式化字符串).

**format directive** n. 1. 在一个格式化字符串[format string]中通过波浪线[tilde]引入的一个字符[character]序列, 它由处理格式化字符串[format string]的代码[code]进行特殊解释, 表示应该执行某些特殊操作, 可能涉及格式化字符串[format string]附带的格式化参数[format argument]提供的数据. 见函数[function] format. "在 '~D base 10 = ~8R' 中, 字符序列 '~D' 和 '~8R' 是格式化指令 (In '~D base 10 = ~8R', the character sequences '~D' and '~8R' are format directives)". 2. 使用相同分派字符的所有格式化指令[format directives[1]]的概念类别. "'~3d' 和 '~3,'0D' 都是 '~D' 格式化指令的有效使用 (Both '~3d' and '~3,'0D' are valid uses of the '~D' format directive)".

**format string** n. 一个字符串[string], 它可以包含普通文本和格式化指令[format directive], 并且它和格式化参数[format argument]结合来描述文本输出应该如何被特定函数 (例如, format) 格式化.

**free declaration** n. 一个声明[declaration], 它不是一个绑定声明[bound declaration]. 见 declare.

**fresh** adj. 1. (指一个函数[function]产生的一个对象[object]) 由该函数[function]新分配的. (一个返回一个新[fresh]对象[object]的函数[function]的调用者可以自由地修改那个对象[object], 不需要担心这样的修改会破坏那个函数[function]未来的正确行为.) 2. (指一个名称[name]的绑定[binding]) 新分配的; 不和该名称[name]的其他绑定[binding]共享.

**freshline** n. 流[stream]上的一个概念操作, 由函数[function] fresh-line 和格式化指令[format directive] ~& 实现, 它将显示位置提前到下一行的开头 (就好像输入一个换行[newline]或者调用函数[function] terpri 一样), 除非该流[stream]已经知道位于一行的开头. 不像换行符[newline], 新行[freshline]不是一个字符[character].

**funbound** ['efunband] n. (指一个函数名[function name]) 没有被 fbound.

**function** n. 1. 一个表示代码的对象[object], 它可以使用零个或多个参数[argument]来调用, 并且它产生零个或多个值[value]. 2. 一个 function 类型[type]的对象[object].

**function block name** n. (指一个函数名[function name]) 被用作包围在该函数名[function name]表示的函数[function]主体外的隐式语句块[implicit block]的名字的符号[symbol]. 如果这个函数名[function name]是一个符号[symbol], 它的函数语句块名称[function block name]就是那个函数名[function name]自身. 如果那个函数名[function name]是一个 car 为 setf 且 cadr 为一个符号[symbol]的列表[list], 它的函数语句块名称[function block name]就是那个函数名[function name]的 cadr 的符号[symbol]. 一个支持其他种类的函数名[function name]的具体实现[implementation]必须为每种函数名指定如何计算函数语句块名[function block name].

**function cell** n. Trad. (指一个符号[symbol]) 持有由这个符号[symbol]命名的全局函数[function]绑定[binding] (如果存在的话) 的定义的位置[place], 它可以通过 symbol-function 来访问. 见存储格[cell].

**function designator** n. 一个函数[function]的标识符[designator]; 也就是说, 一个表示函数[function]的对象[object], 并且它是以下之一: 一个符号[symbol] (表示全局环境[global environment]中由这个符号[symbol]命名的函数[function]), 或者一个函数[function] (表示它自身). 如果一个符号[symbol]被用作一个函数标识符[function designator]但是它没有作为一个函数[function]的全局定义, 或者它是一个宏[macro]或特殊表达式形式[special form]的全局定义, 那么后果是未定义的. 参见扩展函数标识符[extended function designator].

**function form** n. 一个表达式形式[form], 它是一个列表[list]并且它的第一个元素[element]是要被调用的函数[function]的名称[name], 而实参[argument]是求值该函数表达式形式[function form]的后续元素的结果.

**function name** n. 1. (在一个环境[environment]中) 一个符号[symbol]或一个列表[list] (setf symbol), 它是那个环境[environment]中一个函数[function]的名字[name]. 2. 一个符号[symbol]或一个列表[list] (setf symbol).

**functional evaluation** n. 从一个函数名[function name]或一个 lambda 表达式[lambda expression]中提取一个函数值[functional value]的过程. 求值器在遇到一个复合表达式形式[compound form]的 car 中的一个函数名[function name]或 lambda 表达式[lambda expression]时隐式执行函数求值[functional evaluation], 在遇上一个 function 特殊表达式形式[special form]时被显式求值. 一个符号[symbol]作为函数标识符[function designator]的使用, 以及函数[function] symbol-function 去提取一个符号[symbol]的函数值[functional value]都不被认为是一个函数求值[functional evaluation].

**functional value** n. 1. (指一个环境[environment] E 中一个函数名[function name] N) 在环境[environment] E 的函数[function]命名空间[namespace]中名为 N 的绑定[binding]的值[value]; 也就是说, 在环境[environment] E 中名为 N 的函数存储格[function cell]的内容. 2. (指一个 fbound 的符号[symbol] S) 这个符号[symbol]的函数存储格[function cell]的内容; 也就是说, 全局环境[global environment]函数[function]命名空间[namespace]中的名为 S 的绑定[binding]的值[value]. (一个在全局环境[global environment]中是一个宏名称[macro name]或者是一个特殊操作符[special operator]的名称[name]可能是也可能不是 fbound 的. 但是如果 S 是这样一个名称[name]并且是 fbound 的, 它的函数值[functional value]的具体性质是依赖于具体实现的[implementation-dependent]; 它可能是一个函数[function], 也可能不是.)

**further compilation** n. 在最小编译[minimal compilation]之外的依赖于具体实现[implementation-dependent]的编译. 进一步编译允许发生在运行时[run time'. "语句块编译和特定于机器的指令的生成是进一步编译的示例 (Block compilation and generation of machine-specific instructions are examples of further compilation)".


### G

**general** adj. (指数组[array]) 有着元素类型[element type] t, 因此能够将任何对象[object]作为元素[element].

**generalized boolean** n. 被用作一个真值的对象[object], 其中符号[symbol] nil 表示 false 并且所有其他对象[object]表示 true. 见 boolean.

**generalized instance** n. (指一个类[class]) 一个对象[object], 它的类[class]是该类[class]自身或者该类[class]的某个子类[subclass]. (因为类型[type]和类[class]之间的对应关系, 术语 "X 的广义实例 (generalized instance of X)" 意味着 "类型[type] X 的对象[object] (object of type X)" 并且在 X 为一个类[class] (或类[class]名称[name]) 的情况下反过来说也是对的. 前者强调 X 作为类[class]的观点, 而后者强调 X 作为类型指定符[type specifier]的观点.)

**generalized reference** n. 对存储对象[object]的位置的引用, 就像对变量[variable]的引用一样. (这样的引用可以读取[read]或写入[write]这个位置.) 见章节 5.1 (广义引用). 参见位置 [place].

**generalized synonym stream** n. (使用同义流符号[synonym stream symbol]) 1. (对一个流[stream]) 指向该流[stream]的一个同义流[synonym stream], 或者以指向该流[stream]的同义流[synonym stream]为目标的一个复合流[composite stream]. 2. (对一个符号[symbol]) 指向该符号[symbol]的一个同义流[synonym stream], 或者以指向该符号[symbol]的同义流[synonym stream]为目标的一个复合流[composite stream].

**generic function** n. 一个函数[function], 其行为取决于提供给它的参数的类[class]或标识, 其部分包括一组方法[method]、lambda 列表[lambda list]和方法组合[method combination]类型等.

**generic function lambda list** n. 被用于描述到一个广义函数[generic function]的数据流的 lambda 列表[lambda list]. 见章节 3.4.2 (广义函数 Lambda 列表).

**gensym** n. Trad. 一个未捕获[uninterned]的符号[symbol]. 见函数[function] gensym.

**global declaration** n. 使有关代码的特定种类的信息全局可用的一种表达式形式[form]; 也就是说, 一个 proclaim 表达式形式[form]或 declaim 表达式形式[form].

**global environment** n. 一个环境[environment]中包含带有非限定作用域[indefinite scope]和非限定范围[indefinite extent]的绑定[binding]的部分.

**global variable** n. 一个动态变量[dynamic variable]或者一个常变量[constant variable].

**glyph** n. 一种可见表示. "图形字符具有相关的字形 (Graphic characters have associated glyphs)".

**go** v. 把控制转移到一个 go 点[go point]. 见特殊操作符[special operator] go.

**go point** 由 tagbody (或者其他抽象, 例如 prog, 它由 tagbody 构造而来) 建立的多个可能的退出点[exit point]中的一个.

**go tag** n. 在一个 tagbody 表达式形式[form]的词法作用域[lexical scope]内的这个符号[symbol]或整数[integer]命名了该 tagbody 表达式形式[form]建立的一个退出点[exit point].

**graphic** adj. (指一个字符[character]) 一种 "打印" 或 "可显示" 的字符[character], 具有作为单个字形[glyph]的标准视觉表示, 例如 A 或 * 或 =. 空格[space]被定义为是图形的[graphic]. 标准字符[standard character]中, 除了换行符[newline]以外所有字符都是图形的[graphic]. 见非图形[non-graphic]. 


### H

**handle** v. (指正在被发送的一个状况[condition]) 去执行一个控制的非局部转移, 终止正在进行的状况[condition]发送.

**handler** n. 一个状况处理者[condition handler].

**hash table** n. 一个 hash-table 类型[type]的对象[object], 它提供了一个从键[key]到值[value]的映射.

**home package** n. (指一个符号[symbol]) 这个包[package], 如果存在, 它就是这个符号[symbol]的包存储格[package cell]的内容, 并且它表示当前包[current package]中这个符号[symbol]不是可访问[accessible]时, Lisp 打印器[Lisp printer]如何打印该符号[symbol]. (包存储格[package cell]中的值为 nil 的符号[symbol]被说成是没有 home 包[home package]的, 并且也是明显未捕获的[apparently uninterned].) 


### I

**I/O customization variable** n. 下一段中的那些流变量[stream variable]中的一个, 或者被具体实现[implementation]定义为 I/O 定制变量[I/O customization variable]的某个其他 (具体实现定义的[implementation-defined]) 流变量[stream variable].

    *debug-io*        *error-io*         query-io*       
    *standard-input*  *standard-output*  *trace-output*  

    Figure 26-2. 标准化的 I/O 定制变量

**identical** adj. 在 eq 下相同[same].

**identifier** n. 1. 一个被用于标识或区分名称[name]的符号[symbol]. 2. 被用于相同途径的字符串[string].

**immutable** adj. 不受更改的约束, 这要么是因为没有提供能够执行此类更改的操作符[operator], 要么是因为存在一些约束, 禁止使用本可以执行此类更改的操作符[operator]. 除非显式指出, 否则不要求具体实现[implementation]去检测那些修改不可变[immutable]对象[object]或存储格[cell]的尝试; 尝试去作出这样修改的后果是未定义的. "数字是不可变的 (Numbers are immutable)".

**implementation** n. 一个实现了 Common Lisp 的语义的系统, 机制, 或代码体.

**implementation limit** n. 由具体实现[implementation]强加的限制.

**implementation-defined** adj. 依赖于具体实现的[implementation-dependent], 但是本规范要求每个符合规范的实现[conforming implementation]都要定义的, 并且由对应实现者记录.

**implementation-dependent** adj. 描述 Common Lisp 的一个行为或方面, 这个行为或方面被故意保留为未指定的, 可能在一些符合规范的实现[conforming implementation]中定义, 而在其他实现中没有定义, 其细节可能在不同的实现[implementation]中有所不同. 鼓励 (但不要求) 一个符合规范的实现[conforming implementation]去记录其对本规范中标记为依赖于具体实现[implementation-dependent]的每一项的处理, 虽然在这些情况中这样的文档记录可能简单地标识这个项为 "未定义的".

**implementation-independent** adj. 用于标识或强调 Common Lisp 的一个行为或方面, 该行为或方面在符合规范的实现[conforming implementation]之间没有变化.

**implicit block** n. 一个由宏表达式形式[macro form]而不是显式 block 表达式形式[form]引入的语句块[block].

**implicit compilation** n. 在求值[evaluation]期间执行的编译[compilation].

**implicit progn** n. 在另一个表达式形式[form]中出现的相邻表达式形式[form]的有序集合, 并由该表达式形式[form]中的上下文定义为就像在一个 progn 中执行一样.

**implicit tagbody** n. 在另一个表达式形式[form]中的相邻表达式形式[form] 和/或 标签[tag]的有序集合, 并由该表达式形式[form]中的上下文定义为就像在一个 tagbody 中执行一样.

**import** v.t. (一个包[package]中的一个符号[symbol]) 使这个符号[symbol]出现[present]在该包[package]中.

**improper list** n. 一个列表[list], 它不是一个正规列表[proper list]: 一个循环列表[circular list]或者一个点对列表[dotted list].

**inaccessible** adj. 不是可访问的[accessible].

**indefinite extent** n. 时长是无限的一个范围[extent]. "大部分 Common Lisp 对象有着无限范围 (Most Common Lisp objects have indefinite extent)".

**indefinite scope** n. 没有限制的作用域[scope].

**indicator** n. 一个属性指示器[property indicator].

**indirect instance** n. (指一个类[class] C1) 类[class] C2 的一个对象, 其中 C2 是 C1 的一个子类[subclass]. "一个整数是类 number 的一个简洁实例 (An integer is an indirect instance of the class number)".

**inherit** v.t. 1. 接受或获得一种品质、特质或特征; 访问在其他地方定义的特性. 2. (一个类[class]) 获取超类[superclass]定义的结构和行为. 3. (一个包[package]) 通过使用 use-package 来使被另一个包[package]导出[export]是可访问的[accessible].

**initial pprint dispatch table** n. Lisp 镜像[Lisp image]开始时 \*print-pprint-dispatch* 的值[value].

**initial readtable** n. Lisp 镜像[Lisp image]开始时 \*readtable* 的值[value].

**initialization argument list** n. 用于初始化和重新初始化类[class]实例[instance]的协议中初始化参数名称[name]和值[value]的属性列表[property list]. 见章节 7.1 (对象创建和初始化).

**initialization form** n. 被用于给一个槽[slot]或变量[variable]提供初始值[value]的表达式形式[form]. "一个 defclass 表达式形式中的一个槽的初始化表达式形式由关键字 :initform 引入 (The initialization form for a slot in a defclass form is introduced by the keyword :initform)".

**input** adj. (指一个流[stream]) 支持输入操作 (换句话说, 是一个 "数据源"). 一个输入[input]流[stream]也可能是一个输出[output]流[stream], 在这种情况下它有时被称为双向[bidirectional]流[stream]. 见函数[function] input-stream-p.

**instance** n. 1. 一个直接实例[direct instance]. 2. 一个广义实例[generalized instance]. 3. 一个间接实例[indirect instance].

**integer** n. 一个 integer 类型[type]的对象[object], 表示一个数学上的整数.

**interactive stream** n. 一个流[stream], 可以在其上执行交互式查询. 见章节 21.1.1.1.3 (交互式流).

**intern** v.t. 1. (一个包[package]的字符串[string]) 在这个包[package]中查找这个字符串[string], 返回这个包[package]中已经可访问[accessible]的带有该名称[name]的符号[symbol], 或者返回这个包[package]中新创建的带有这个名称[name]的内部符号[internal symbol]. 2. Idiom. 通常, 观察一种协议, 在该协议定义的某个谓词下具有等价或具有等价名称的对象被映射到单个规范对象.

**internal symbol** n. (指一个包[package]) 一个符号[symbol], 它在该包[package]中是可访问[accessible]的, 但是不是这个包[package]的一个外部符号[external symbol].

**internal time** n. 时间[time], 被表示为内部时间单元[internal time unit]的一个整数[integer]数字. 绝对[absolute]内部时间[internal time]是作为从任意选择的、依赖于具体实现[implementation-dependent]的基础上的偏移量来度量的. 见章节 25.1.4.3 (内部时间).

**internal time unit** n. 等价于一秒的 1/n 的时间单元, 其中 n 是某个具体实现定义的[implementation-defined]整数[integer]值. 见变量[variable] internal-time-units-per-second.

**interned** adj. Trad. 1. (指一个符号[symbol]) 在任何包[package]中可访问的[accessible[3]]. 2. (指一个特定包[package]中的一个符号[symbol]) 出现[present]在该包[package]中.

**interpreted function** n. 一个函数[function], 它不是一个编译后的函数[compiled function]. (这里可能存在一个符合规范的实现[conforming implementation]没有解释型函数[interpreted function], 但是一个符合规范的程序[conforming program]一定不能假设所有的函数[function]都是编译后的函数[compiled function].)

**interpreted implementation** n. 一个具体实现[implementation], 它对解释型函数[interpreted function]使用的执行策略不涉及一次性的语义分析预传递, 而是在执行过程中遇到表达式形式[form]时使用的"惰性"(有时是重复的)语义分析.

**interval designator** n. (指类型[type] T) 一种有序的对象[object]对, 通过在实数行上分隔一个区间来描述 T 的子类型[subtype]. 见章节 12.1.6 (区间标识符).

**invalid** n., adj. 1. n. 字符[character]的一个可能的标记成分特质[constituent trait], 如果它存在就表示这个字符[character]不能出现在一个标记[token]中, 除了在单转义[single escape]字符[character]的控制下. 关于详情, 见章节 2.1.4.1 (标记成分字符). 2. adj. (指一个字符[character]) 是一个字符[character], 它在当前读取表[current readtable]中有着语法类型[syntax type]的标记成分[constituent], 并且它有着非法[invalid[1]]标记成分特质[constituent trait]. 见 Figure 2-8.

**iteration form** n. 操作符[operator]在下一段中命名的一个复合表达式形式[compound form], 或者是一个有着具体实现定义的[implementation-defined]操作符[operator]并且被该实现定义为循环表达式形式[iteration form]的复合表达式形式[compound form].

    do              do-external-symbols  dotimes  
    do*             do-symbols           loop     
    do-all-symbols  dolist                        

    Figure 26-3. 标准循环表达式形式

**iteration variable** n. 一个变量[variable] V, 它的绑定[binding]由一个循环表达式形式[iteration form]中 V 的显式使用[explicit use]所创建. 


### K

**key** n. 在检索过程中用于选择的对象[object]. 见关联列表[association list], 属性列表[property list], 还有散列表[hash table]. 另外, 见章节 17.1 (序列的概念).

**keyword** n. 1. 一个 home 包[home package]是 KEYWORD 包的符号[symbol]. 2. 任何在传递的关键字风格参数中被用作一个标识记号的符号[symbol], 通常在 KEYWORD 包中, 但不一定在 KEYWORD 包中. 见 lambda. 3. Idiom. 一个 lambda 列表关键字[lambda list keyword].

**keyword parameter** n. 一个形参[parameter], 对应的关键字实参[argument]是可选的. (There is no such thing as a required keyword argument.) 如果没有提供这样一个实参[argument], 那么就使用一个默认值. 参见 supplied-p 参数[supplied-p parameter].

**keyword/value pair** n. 一个属性列表[property list]的两个连续元素[element] (分别是一个关键字[keyword]和一个值[value]). 


### L

**lambda combination** n. Trad. 一个 lambda 表达式形式[lambda form].

**lambda expression** n. 一个列表[list], 通过直接描述其行为而非间接引用一个已建立函数[function]的名字, 可以在特定上下文中被用于取代一个函数名[function]来表示一个函数[function]; 它的名称源于它的第一个元素是符号[symbol] lambda. 见 lambda.

**lambda form** n. 一个表达式形式[form], 它是一个列表[list], 并且它的第一个元素[element]是一个要被调用的函数[function]的 lambda 表达式[lambda expression], 这个函数使用求值这个 lambda 表达式形式[lambda form]的后续元素的结果作为实参[argument]来调用.

**lambda list** n. 一个列表[list], 它指定了形参[parameter]集合 (有时被称为 lambda 变量[lambda variables]) 以及一个协议来接收这些形参[parameter]的值[value]; 也就是说, 一个普通 lambda 列表[ordinary lambda list], 一个扩展 lambda 列表[extended lambda list], 或者一个修改后的 lambda 列表 [modified lambda list].

**lambda list keyword** n. 一种符号[symbol], 其名称[name]以与号[ampersand]开头, 在 lambda 列表[lambda list]中被特别识别. 注意, 在 KEYWORD 包中没有标准[standardized] lambda 列表关键字[lambda list keyword].

**lambda variable** n. 一个正规形参[formal parameter], 用于强调该变量[variable]与建立它的 lambda 列表[lambda list]的关系.

**leaf** n. 1. 一个树[tree[1]]中的一个原子. 2. 一个树[tree[2]]的终端节点.

**leap seconds** n. 额外的一秒间隔, 偶尔被官方计时员插入到真实日历中, 作为类似"闰年"的修正. 所有 Common Lisp 时间[time]表示都忽略闰秒[leap second]; 假设每天正好是86400秒长.

**left-parenthesis** n. 标准字符[standard character] "(", 也就是所谓的 "左圆括号(left parenthesis)" 或 "开圆括号(open parenthesis)". 见 Figure 2-5.

**length** n. (指一个序列[sequence]) 这个序列[sequence]中元素[element]的数量. (注意, 如果这个序列[sequence]是一个带有填充指针[fill pointer]的向量[vector], 那么它的长度[length]和填充指针[fill pointer]相同, 即便这个向量[vector]总分配的大小可能更大.)

**lexical binding** n. 一个词法环境[lexical environment]中的绑定[binding].

**lexical closure** n. 一个函数[function], 当在实参[argument]上被调用时, 在这个词法闭包[lexical closure]被创建时所捕获的词法环境[lexical environment]中执行一个 lambda 表达式[lambda expression]的主体, 通过这个函数[function]的形参[parameter]到对应实参[argument]的绑定[binding]来扩增.

**lexical environment** n. 环境[environment]的这个部分包含了名称具有词法作用域[lexical scope]的绑定[binding]. 一个词法环境[lexical environment]还包括其他内容: 变量[variable]名称[name]到值[value]的普通绑定[binding], 词法上建立的函数名[function name]到函数[function]的绑定[binding], 宏[macro], 符号宏[symbol macro], 语句块[block], 标签[tag], 以及局部声明[local declaration] (见 declare).

**lexical scope** n. 限定于建立表达式形式[form]内的空间或文本区域的作用域[scope]. "函数的参数名通常在词法上限定作用域 (The names of parameters to a function normally are lexically scoped)".

**lexical variable** n. 一个变量[variable], 它的绑定[binding]是在词法环境[lexical environment]中.

**Lisp image** n. 一个 Common Lisp 实现[implementation]的运行实例. 一个 Lisp 映像[Lisp image]的特征是一个单独的地址空间, 其中任何对象[object]可以直接引用符合此规范的任何其他对象, 以及一个单一的、公共的全局环境[global environment]. (外部操作系统有时将其称为 "核心映像(core image)", "fork", "实体(incarnation)", "作业(job)", 或者 "进程(process)". 但是请注意, 在这样的操作系统中, "进程"的问题在技术上与这里定义的 Lisp 映像[Lisp image]的问题是正交的. 根据操作系统的不同, 一个"进程"可能有多个 Lisp 映像[Lisp image], 多个"进程"可能驻留在一个 Lisp 映像[Lisp image]中. 因此, 在所有对象[object]之间直接引用一个完全共享的地址空间的概念是定义特征. 还要注意, 如果两个"进程"有一个通信区域, 允许共享一些但不是所有对象[object], 那么这两个"进程"被认为是不同的 Lisp 映像[Lisp image].)

**Lisp printer** n. Trad. 打印一个对象[object]的字符表示到一个流[stream]的程序. (这个程序由函数[function] write 实现.)

**Lisp read-eval-print loop** n. Trad. 一个无尽循环, 它读取[read[2]]一个表达式形式[form], 求值[evaluate]该表达式形式, 然后打印 (也就是, 写入[writes[2]]) 结果. 在许多实现[implementation]中, 在程序开发期间与 Common Lisp 交互的默认模式就是通过这样一个循环.

**Lisp reader** n. Trad. 从一个流[stream]中解析对象[object]的字符表示并产生对象[object]的程序. (这个程序由函数[function] read 实现.)

**list** n. 1. 一个 cons 链, 其中每一个 cons 的 car 是这个列表[list]的元素[element], 而每一个 cons 的 cdr 是这个链中的下一个链接或一个终止的原子[atom]. 参见正规列表[proper list], 点对列表[dotted list], 或循环列表[circular list]. 2. null 和 cons 的并集所表示的类型[type].

**list designator** n. 一个对象[object]列表[list]的标识符[designator]; 也就是说, 一个表示一个列表[list]的对象[object]并且它是以下之一: 一个非 nil [non-nil]原子[atom] (表示一个单元素[singleton]列表[list], 它的元素[element]是这个非 nil [non-nil]原子[atom]) 或一个正规列表[proper list] (表示它自身[itself]).

**list structure** n. (指一个列表[list]) 构成这个列表[list]的 cons 集合. 注意, 当每一个这样的 cons 的 car 组件是这个列表结构[list structure]的一部分时, 作为列表[list]元素[element]的对象[object]本身 (也就是说, 这个列表[list]中每一个 cons 的 car 所表示的这些对象[object]) 不是列表结构的一部分, 即便它们是 cons, 除了这个列表[list]实际上包含了它的一个尾部[tail]作为元素[element] (循环[circular[2]]) 的情况下. (一个列表[list]的列表结构[list structure]有时被冗余地称为"顶级列表结构", 以强调不涉及列表[list]元素[element]的任何 cons.)

**literal** adj. (指一个对象[object]) 在一个程序中直接引用而不是被这个程序计算; 这也就是说, 作为一个 quote 表达式形式[form]中的数据出现, 或者, 如果这个对象[object]是一个子求值对象[self-evaluating object], 则作为非 quote 数据出现. "在表达式形式 (cons "one" '("two")) 中, 表达式 "one", ("two"), 和 "two" 是字面化对象 (In the form (cons "one" '("two")), the expressions "one", ("two"), and "two" are literal objects)".

**load** v.t. (一个文件[file]) 使这个文件[file]中包含的代码[code]被执行. 见函数[function] load.

**load time** n. 加载器加载编译后代码[compiled code]的时期.

**load time value** n. 由 load-time-value 表达式形式[form]在代码[code]中引用的对象[object]. 这样一个表达式形式[form]的值[value]是某个只能在运行时环境[environment]中被计算的特殊对象[object]. 在文件[file]编译[compilation]的情况中, 该值[value]作为加载已编译文件[compiled file]过程的一部分计算一次, 不会再一次计算. 见特殊操作符[special operator] load-time-value.

**loader** n. 一个工具, 它是 Lisp 的一部分, 并且它加载[load]一个文件[file]. 见函数[function] load.

**local declaration** n. 一个表达式[expression], 它只能出现在特定表达式形式[form]的特定位置, 并且它提供有关包含表达式形式[form]中包含的代码的信息; 也就是说, 一个 declare 表达式[expression].

**local precedence order** n. (指一个类[class]) 由类[class]及其直接超类[direct superclass]按照类[class]定义表达式形式[form]中提到的顺序组成的列表[list].

**local slot** n. (指一个类[class]) 只有在一个实例[instance]可访问[accessible]的槽[slot], 即分配这个槽[slot]的实例[instance].

**logical block** n. 美观打印器[pretty printer]使用的相关输出的概念分组. 见宏[macro] pprint-logical-block 以及章节 22.2.1.1 (输出排列的动态控制).

**logical host** n. 一个依赖于具体实现[implementation-dependent]的对象[object], 它被用作一个逻辑路径名[logical pathname]中的 "主机(host)" 的表示, 并且它有一个转换规则的关联集合, 用于将属于该主机的逻辑路径名[logical pathname]转换为物理路径名[physical pathname]. 见章节 19.3 (逻辑路径名).

**logical host designator** n. 一个逻辑主机[logical host]的标识符[designator]; 也就是说, 一个表示一个逻辑主机[logical host]的对象[object]并且它是以下之一: 一个字符串[string] (表示它命名的逻辑主机[logical host]), 或者一个逻辑主机[logical host] (表示它自身). (注意, 由于一个逻辑主机[logical host]的表示是依赖于具体实现的[implementation-dependent], 一个具体实现[implementation]把一个逻辑主机[logical host]表示为一个命名它的字符串[string]是可能的.)

**logical pathname** n. 一个 logical-pathname 类型[type]的对象[object].

**long float** n. 一个 long-float 类型[type]的对象[object].

**loop keyword** n. Trad. 一个符号[symbol], 它是一个扩展 loop 表达式形式[form]的语法中特殊可识别的部分. 这样的符号[symbol]通过它们的名称[name]来识别 (使用 string=), 而不是通过它们的标识; 因此, 它们可能在任何包[package]中. 一个 loop 关键字 [loop keyword]不是一个关键字[keyword].

**lowercase** adj. (指一个字符[character]) 位于与小写字母 a 到 z 对应的标准字符[standard character]之中, 或由具体实现[implementation]定义为小写[lowercase]的其他具体实现定义的[implementation-defined]字符[character]. 见章节 13.1.4.3 (大小写字符). 


### M

**macro** n. 1. 一个宏表达式形式[macro form] 2. 一个宏函数[macro function]. 3. 一个宏名称[macro name].

**macro character** n. 一个字符[character], 当被 Lisp 读取器[Lisp reader]在它的主要分派循环中遇到时, 引入一个读取器宏[reader macro[1]]. (宏字符[macro character]与宏[macro]无关.)

**macro expansion** n. 1. 把一个宏表达式形式[macro form]转换为另一个表达式形式[form]打过程. 2. 这个过程产生的表达式形式[form].

**macro form** n. 代表另一个表达式形式[form]的表达式形式[form] (例如, 出于抽象、信息隐藏或语法方便的目的); 也就是说, 要么是第一个元素是宏名称[macro name]的复合表达式形式[compound form], 要么是为符号宏[symbol macro]命名的符号[symbol]表达式形式[form].

**macro function** n. 一个两参数的函数[function], 两个参数是一个表达式形式[form]和一个环境[environment], 它通过产生一个要被求值的表达式形式[form]去替换原始的参数表达式形式[form]来实现宏展开[macro expansion].

**macro lambda list** n. 被用于建立[establish]宏[macro]定义的表达式形式[form] (例如 defmacro 和 macrolet) 中的扩展 lambda 列表[extended lambda list]. 见章节 3.4.4 (宏 Lambda 列表).

**macro name** n. 一个名称[name], 对于它 macro-function 返回 true, 并且当被用作一个复合表达式形式[compound form]的第一个元素时标识这个表达式形式[form]为一个宏表达式形式[macro form].

**macroexpand hook** n. \*macroexpand-hook* 的值[value]所指的函数[function].

**mapping** n. 1. 一种迭代类型, 其中一个函数[function]被依次应用到取自集合 (例如序列[sequence]或散列表[hash table]) 中对应条目的对象[object]上. 2. Math. 两个集合之间的一种关系, 其中第一个集合的每个元素("域")都分配给第二个集合的一个元素("范围").

**metaclass** n. 1. 一个类[class], 它的实例[instance]也是类[class]. 2. (指一个对象[object]) 该对象[object]的类[class]的类[class].

**Metaobject Protocol** n. 关于一个符合规范的实现[conforming implementation]可能如何实现对象系统的多个方面的可能的描述之一. 这个描述超出了文档的范围, 并且没有要求符合规范的实现[conforming implementation]去遵守它, 除了它在这个规范中显式标注的例外. 然而, 它的存在有助于建立规范的做法, 并鼓励没有理由背离它的实现者考虑在可能的情况下使他们的实现[implementation]遵守它. 这个在 The Art of the Metaobject Protocol 中详细描述.

**method** n. 一个对象[object], 它是一个广义函数[generic function]的一部分, 并且它提供了关于实参[argument]是特定类[class]的对象[object]或特定标识时该广义函数[generic function]应该如何表现的信息.

**method combination** n. 1. 通常, 一组方法[method]的组合, 来产生一个广义函数[generic function]的生效方法[effective method]. 2. 一个 method-combination 类型[type]的对象[object], 它表示一个或多个特定广义函数[generic function]的方法组合如果被执行的详细情况.

**method-defining form** n. 一个表达式形式[form], 它定义了一个广义函数[generic function]的一个方法[method], 不管是显式的还是隐式的. 见章节 7.6.1 (广义函数的介绍).

**method-defining operator** n. 对应于一个方法定义表达式形式[method-defining form]的操作符[operator]. 见 Figure 7-1.

**minimal compilation** n. 编译器[compiler]必须要在编译期采取的动作. 见章节 3.2.2 (编译语义).

**modified lambda list** n. 一个列表, 它在形式和目的上类似于普通 lambda 列表[ordinary lambda list], 但在语法或功能上偏离了普通 lambda 列表[ordinary lambda list]的定义. 见普通 lambda 列表[ordinary lambda list]. "deftype 使用一个改进 lambda 列表 (deftype uses a modified lambda list)".

**most recent** adj. 最里面的; 也就是说, 比其他同类的建立得更晚 (而且尚未解除建立).

**multiple escape** n., adj. 1. n. 一个被成对使用的字符[character]的语法类型[syntax type], 来表示括起来的那些字符要被当作字母[alphabetic[2]]字符[character], 保留它们的大小写[case]. 关于详情, 见章节 2.1.4.5 (多转义字符). 2. adj. (一个字符[character]的) 有着多转义[multiple escape]语法类型[syntax type]. 3. n. 一个多转义[multiple escape[2]]字符[character]. (在标准读取表[standard readtable]中, 竖线[vertical-bar]是一个多转义[multiple escape]字符[character].)

**multiple values** n. 1. 不止一个值[value]. "函数 truncate 返回多个值 (The function truncate returns multiple values)". 2. 一个可变数量的值[value], 可能包含零个或一个. "函数 values 返回多值 (The function values returns multiple values)". 3. 除 1 以外的固定数量的值[value]. "宏 multiple-value-bind 是 Common Lisp 中为数不多的可以检测和操纵多值的操作符 (The macro multiple-value-bind is among the few operators in Common Lisp which can detect and manipulate multiple values)".


### N

**name** n., v.t. 1. n. 一个标识符[identifier],  一个绑定[binding]关联通过它来引用一个对象[object]、一个绑定[binding]或一个退出点[exit point]. 2. v.t. 去给定一个名称[name]. 3. n. (指具有一个名称组件的对象[object]) 就是该组件的对象[object]. "一个符号的名称字符串由 symbol-name 返回 (The string which is a symbol's name is returned by symbol-name)". 4. n. (指一个路径名[pathname]) a. 由 pathname-name 返回的名称组件. b. 由 namestring 返回的完整路径名字符串. 5. n. (指一个字符[character]) 一个命名该字符[character]的字符串[string], 并且它有着超过一的长度[length]. (所有非图形[non-graphic]字符[character]都需要有名称[name], 除非它们具有某个具体实现定义的[implementation-defined]不为空[null]的属性[attribute]. 其他字符[character]是否具有名称[name]是依赖于具体实现的[implementation-dependent].)

**named constant** n. 一个变量[variable], 它由 Common Lisp、具体实现[implementation]或用户代码 (见宏[macro] defconstant) 定义, 在求值时总是产生[yield]相同的值[value]. "一个已命名常量的值不能通过赋值或绑定更改".

**namespace** n. 1. 表示受限于一个特定种类的绑定[binding]. "名称到标签的绑定是标签命名空间 (The bindings of names to tags is the tag namespace)". 2.域是一组名称[name]的任何映射[mapping]. "一个包定义一个命名空间 (A package defines a namespace)".

**namestring** n. 一个字符串[string], 它使用章节 19.3.1 (逻辑路径名名称字符串的语法)所述的命名逻辑路径名[logical pathname]的标准[standardized]表示法或者某个具体实现定义的[implementation-defined]命名一个物理路径名[physical pathname]的表示法来表示了一个文件名[filename].

**newline** n. 标准字符[standard character] <Newline>, 为 Lisp 读取器[Lisp reader]表示为 #\Newline.

**next method** n. 针对特定参数或参数类[class]的集合的给定方法[method]调用的下一个方法[method]. 见章节 7.6.6.1.3 (对排序后的可应用方法应用方法组合).

**nickname** n. (指一个包[package]) 可以被用于引用该包[package]的几个可能的名字[name]中的一个, 但不是这个包[package]的主要名称[name].

**nil** n. 这个对象[object], 即 COMMON-LISP 包中名为 "NIL" 的符号[symbol]、空列表[empty list]、表示 false 的 boolean (或广义 boolean [generalized boolean])以及空类型[empty type]的名称[name].

**non-atomic** adj. 不是一个原子[atom]; 换句话说, 就是一个 cons.

**non-constant variable** n. 一个变量[variable], 它不是一个常变量[constant variable].

**non-correctable** adj. (指一个错误[error]) 不能故意校正的[correctable]. (由于重启动[restart]的动态特性, 完全禁止错误[error]的可校正性[correctable]既不可能, 通常也没有用处. 使用这个术语是为了表达一种意图, 即不应该通过发出错误[error]信号的代码[code]来做出特殊的努力来使这个错误[error]成为可校正的[correctable]; 然而, 这个术语对符合标准的程序[conforming program]或符合标准的实现[conforming implementation]并没有实际的要求.)

**non-empty** adj. 至少有一个元素[element].

**non-generic function** n. 一个函数[function], 它不是一个广义函数[generic function].

**non-graphic** adj. (指一个字符[character]) 不是图形的[graphic]. 见章节 13.1.4.1 (图形字符).

**non-list** n., adj. 除了列表[list]以外; 换句话说, 一个非空[non-nil]原子[atom].

**non-local exit** n. 将控制(有时时值[value])转移到一个退出点[exit point], 而不是出于正常返回的原因. "操作符 go、throw 以及 return-from 导致一个非局部退出 (The operators go, throw, and return-from cause a non-local exit)".

**non-nil** n., adj. 不是 nil. 学术上, 任何不是 nil 的对象[object]都可以被引用为 true, 但是这将倾向于将对象[object]作为一个广义 boolean [generalized boolean]的独特视图. 引用这样的一个对象[object]为非 nil [non-nil]避免了这种暗示.

**non-null lexical environment** n. 一个词法环境[lexical environment], 它具有没有出现在全局环境[global environment]中的额外信息, 例如一个或多个绑定[binding].

**non-simple** adj. 不简单[simple].

**non-terminating** adj. (一个宏字符[macro character]的) 是这样的, 当它出现在一个扩展标记中时, 它会被当作一个标记成分字符[character]. 见章节 2.2 (读取器算法).

**non-top-level form** n. 一个表达式形式[form], 它是另一个表达式形式[form]的子表达式形式[subform], 一个顶层表达式形式[top level form]. 见章节 3.2.3.1 (顶层表达式形式的处理).

**normal return** n. 控制和值[value]的自然转移, 发生在表达式形式[form]的完整执行之后.

**normalized** adj., ANSI, IEEE (指一个浮点数[float]) 符合 IEEE Standard for Binary Floating-Point Arithmetic 所述的 "标准化 (normalized)" 的描述. 见非标准化[denormalized].

**null** adj., n. 1. adj. a. (指一个列表[list]) 没有元素[element]: 空的. 见空列表[empty list]. b. (指一个字符串[string]) 具有一个为 0 的长度[length]. (无论是在本文档中, 还是在观察到的口语行为中, 通过明显的明确引用来引用空字符串都是常见的, 就像在 "空字符串" 中一样, 即使没有尝试去捕获[intern[2]]空字符串. 措辞 "一个空字符串 (a null string)" 在技术上更正确, 但是大多数 Lisp 程序员通常认为它很笨拙. 因此, 除了回指引用外, 术语 "空字符串 (the null string)" 在所有情况下都应视为不确定引用.) c. (指一个字符[character]的一个具体实现定义的[implementation-defined]属性[attribute]) 如果没有请求特定值, 则该属性[attribute]的值默认为该对象[object]的值. 2. n. 一个 null 类型[type]的对象[object] (仅有的这样的对象[object]是 nil).

**null lexical environment** n. 没有绑定[binding]的词法环境[lexical environment].

**number** n. 一个 number 类型[type]的对象[object].

**numeric** adj. (指一个字符[character]) 是标准字符[standard character] 0 到 9 中的一个, 或者被具体实现[implementation]定义为数值[numeric]的某个其他的图形[graphic]字符[character]. 


### O

**object** n. 1. 任何 Lisp 数据. "函数 cons 创建一个引用其他两个对象的对象(The function cons creates an object which refers to two other objects)". 2. (直接跟在一个类型[type]的名字后面) 一个那个类型[type]的对象[object], 被用于强调那个对象[object]不止是那个类型[type]的一个对象的名字[name], 并且在那个类型[type](例如 function 或 class)的对象[object]普遍通过名字[name]来引用的情况下是那个类型[type]的一个元素[element]. "函数 symbol-function 接收一个函数名并返回一个函数对象(The function symbol-function takes a function name and returns a function object)".

**object-traversing** adj. 连续地对对象[object]的组件进行操作. "操作符 mapcar、maphash、with-package-iterator 和 count 执行对象穿越操作 (The operators mapcar, maphash, with-package-iterator and count perform object-traversing operations)".

**open** adj., v.t. (一个文件[file]) 1. v.t. 去创建并返回一个到该文件[file]的流[stream]. 2. adj. (指一个流[stream]) 已经被打开[open[1]], 但还没有被关闭[closed].

**operator** n. 1. 一个函数[function], 宏[macro], 或[special operator]. 2. 命名这样一个函数[function], 宏[macro], 或特殊操作符[special operator]的符号[symbol]. 3. (在一个 function 特殊表达式形式[special form]中) 这个 function 特殊表达式形式[special form] 的 cadr, 它可能是一个操作符[operator[2]] 或者一个 lambda 表达式[lambda expression]. 4. (一个复合表达式形式[compound form]的) 这个复合表达式形式[compound form]的 car, 它可能是一个操作符[operator[2]] 或者一个 lambda 表达式[lambda expression], 并且它从来不是 (setf symbol).

**optimize quality** n. 一个程序中可以被特定编译器优化的多个方面中的一个. 由于优化一个这样的质量可能与优化另一个冲突, 相关质量的优先级可以再一个 optimize 声明[declaration]中建立. 标准[standardized]优化质量[optimize quality]是 compilation-speed (编译过程的速度)、 debug (调试的容易程度)、 safety (运行时错误检测), space (包括代码大小和运行时空间)、 以及 speed (指对象代码). 具体实现[implementation]可以定义额外的优化质量[optimize quality].

**optional parameter** n. 一个形参[parameter], 其对应位置的实参[argument]是可选的. 如果这个实参[argument]没有被提供, 就使用一个默认值. 参见 supplied-p 参数[supplied-p parameter].

**ordinary function** n. 一个函数[function], 它不是一个广义函数[generic function].

**ordinary lambda list** n. 被 lambda 所使用的 lambda 列表[lambda list]的种类. 见改进 lambda 列表[modified lambda list]和扩展 lambda 列表[extended lambda list]. "defun 使用一个普通 lambda 列表 (defun uses an ordinary lambda list)".

**otherwise inaccessible part** n. (指一个对象[object], O1) 一个对象[object] O2, 如果 O1 不可访问[inaccessible], 则该对象 O2 将不可访问[inaccessible]. (每个对象[object]都是它自身的其他不可访问部分[otherwise inaccessible part].)

**output** adj. (指一个流[stream]) 支持输出操作 (换句话说, 就是一个 "数据接收器 (data sink)"). 一个输出[output]流[stream]可能也是一个输入[input]流[stream], 在这个情况下它有时被称为双向[bidirectional]流[stream]. 见函数[function] output-stream-p. 


### P

**package** n. 一个 package 类型[type]的对象[object].

**package cell** n. Trad. (指一个符号[symbol]) 一个符号[symbol]中的一个位置[place], 它持有了该符号[symbol]被捕获[interned]的多个包[package]中的一个, 称为 home 包[home package], 或者如果不存在这样的包[package]或不可知则持有 nil. 见函数[function] symbol-package.

**package designator** n. 一个包[package]的标识符[designator]; 也就是说, 表示一个包[package]的对象[object]并且是以下之一: 一个字符串标识符[string designator] (表示名称[name]或别名[nickname]之一为该字符串[string]的包[package]), 或者一个包[package] (表示它自身).

**package marker** n. 一个字符, 它被用于一个符号中去分割包名和符号名, 它是标准读取表[standard readtable]中的冒号[colon]. 见章节 2.1 (字符语法).

**package prefix** n. 在 Lisp 读取器[Lisp reader]处理的文本中的一个符号[symbol]名字[name]前的标记, 它使用一个包[package]名称[name]后面跟着一个或多个包标记[package marker], 并且它表示这个符号[symbol]在表示的包[package]中查找.

**package registry** n. 一个名称[name]到包[package]对象[object]的映射. 有可能有一个包[package]对象[object]不在这个映射中; 这样一个包[package]被称为未注册的包[unregistered package]. 例如 find-package 这样的操作符[operator]查阅这个映射, 来从它的名称[name]中找到一个包[package]. 例如 do-all-symbols、find-all-symbols 和 list-all-packages 这样的操作符[operator]只在出现在包注册表[package registry]中的包[package]上操作.

**pairwise** adv. (指集合上的形容词)单独地应用于集合中所有可能的元素对. "如果 A 和 B 互斥, B 和 C 互斥并且 A 和 C 也互斥, 那么类型 A、B 和 C 时成对互斥的 (The types A, B, and C are pairwise disjoint if A and B are disjoint, B and C are disjoint, and A and C are disjoint)".

**parallel** adj. Trad. (指绑定[binding]或赋值[assignment]) 以 psetq、let 或 do 的风格完成; 也就是说, 首先求值所有产生值[value]的表达式形式[form], 然后才赋值或绑定[binding]这些变量[variable] (或者位置[place]). 注意, 这并不意味着传统的计算"并行性", 因为生成值[value]的表达式形式[form]是顺序[sequential]计算的. 见顺序的[sequential].

**parameter** n. 1. (指一个函数[function]) 在一个函数[function]的定义中的一个变量[variable], 当这个函数被调用时取自对应实参[argument]的指[value] (或者对应实参的一个列表[list]), 或者在某些情况下由于没有对应的实参[argument]就给定一个默认值. 2. (指一个格式化指令[format directive]) 由格式化指令[format directive]接收的作为数据流的对象[object], 由于格式化字符串[format string]中的前缀表示法位于格式化指令[format directive]的使用点. 见章节 22.3 (格式化输出). "在 "~3,'0D" 中, 数字 3 和字符 #\0 是给 ~D 格式化指令的参数 (In "~3,'0D", the number 3 and the character #\0 are parameters to the ~D format directive)".

**parameter specializer** n. 1. (指一个方法[method]) 一种表达式[expression], 它限制该方法[method]仅适用于对应实参[argument]匹配参数特化符[parameter specializer]的实参[argument]序列. 2. 一个类[class], 或一个列表[list] (eql object).

**parameter specializer name** n. 1. (指一个方法[method]定义) 一个表达式[expression], 它被用于代码中来命名一个参数特化符[parameter specializer]. 见章节 7.6.2 (方法的介绍). 2. 一个类[class], 命名一个类[class]的符号[symbol], 或一个列表[list] (eql form).

**pathname** n. 一个 pathname 类型[type]的对象[object], 它是一个文件[file]名字的结构化表示. 一个路径名[pathname]有六个成员: 一个 "主机(host)", 一个"设备(device)", 一个 "目录(directory)", 一个 "名称(name)",一个 "类型(type)" 以及一个版本 "版本(version)".

**pathname designator** n. 一个路径名[pathname]的标识符[designator]; 也就是说, 一个表示路径名[pathname]的对象[object]并且它是以下之一: 一个路径名[pathname]名称字符串[namestring] (表示对应路径名[pathname]), 和一个文件[file]关联的流[stream] (表示被用于打开该文件[file]的路径名[pathname]; 这可能但不一定是该文件[file]的实际名称), 或者一个路径名[pathname] (表示它自身). 见章节 21.1.1.1.2 (打开和关闭流).

**physical pathname** n. 一个路径名[pathname], 它不是一个逻辑路径名[pathname].

**place** n. 1. 一个表达式形式[form], 它适用于一个广义引用[generalized reference]. 2. 被这样一个位置[place[1]]引用的概念位置.

**plist** ['pee,list] n. 一个属性列表[property list].

**portable** adj. (指代码[code]) 需要在所有符合规范的实现[conforming implementation]中产生等价的结果和可观测的副作用.

**potential copy** n. (指一个受限于约束的对象[object] O1) 一个对象[object] O2, 如果指定的约束由 O1 满足而没有进行修改, 那么它可能与 O1 相同[identical], 也可能与 O1 不同, 或者它必须是一个新的[fresh]对象[object], 类似于 O1 的副本[copy], 除非它已经根据需要进行了修改以满足约束. <!--TODO 待校对-->

**potential number** n. 一个文本标记, 可能在某个符合规范的实现[conforming implementation]中被 Lisp 读取器[Lisp reader]解析为一个数字[number], 但不是必须解析为一个数字[number]. 没有对象[object]是一个潜在数字[potential number]---一个对象[object]要么是一个数字[number]要么不是. 见章节 2.3.1.1 (潜在数字作为标记).

**pprint dispatch table** n. 一个对象[object], 它可以是 \*print-pprint-dispatch* 的值[value]并且因此可以控制当 \*print-pretty* 为 true 时对象[object]如何被打印. 见章节 22.2.1.4 (美观打印分派表).

**predicate** n. 一个函数[function], 它返回一个广义 boolean [generalized boolean]作为它的第一个值.

**present** n. 1. (指一个 Lisp 映像[Lisp image]中的一个特性[feature]) 当且仅当命名特性[feature]的符号[symbol]是特性列表[feature list]的一个元素[element]时才有效的存在状态. 2. (指一个包[package]中的一个符号[symbol]) 在那个包[package]中是直接可访问的, 而不是从另一个包[package]中继承而来.

**pretty print** v.t. (一个对象[object]) 在这个对象[object]上去调用美观打印器[pretty printer].

**pretty printer** n. 当 \*print-pretty* 的值[value]是 true 时打印一个对象[object]的字符表示到一个流[stream]上的程序, 它使用了布局技术(例如缩进), 这种技术往往突出对象[object]的结构, 使人类读者更容易直观地解析. 见变量[variable] \*print-pprint-dispatch* 和章节 22.2 (Lisp 美观打印器).

**pretty printing stream** n. 执行美观打印的一个流[stream]. 这样的流由函数[function] pprint-logical-block 创建, 作为该输出流和逻辑语句块之间的链接.

**primary method** n. 两个方法[method]集合中的一个的成员 (另一个是辅助方法[auxiliary method]), 这两个集合组成了该方法[method]的广义函数[generic function]上的方法[method]集合的详尽分区. 这些集合如果决定取决于方法组合[method combination]类型; 见章节 7.6.2 (方法的介绍).

**primary value** n. (指一个表达式形式[form]的求值[evaluation]所产生的那些值[value]) 如果存在就是第一个值[value], 如果这里没有值[value]那么就是 nil. "由 truncate 返回的主值是一个整数商, 朝着零截断 (The primary value returned by truncate is an integer quotient, truncated toward zero)".

**principal** adj. (一个由 Common Lisp 函数[function]返回的值, 该函数实现在复域中定义的数学上的无理数或超越函数) 可能有许多 (有时是无穷多个) 数学函数的正确值, 即定义相应的 Common Lisp 函数[function]返回的特定值[value].<!--TODO 待校对-->

**print name** n. Trad. (通常指一个符号[symbol]) 一个名称[name[3]].

**printer control variable** n. 一个变量[variable], 它的具体目的是去控制 Lisp 打印器[Lisp printer]的一些行为; 也就是说, Figure 22-1 中的变量[variable]之一, 或者某个被具体实现[implementation]定义为一个打印器控制变量[variable]的具体实现定义的[implementation-defined]变量[variable].

**printer escaping** n. 打印器控制变量[printer control variable] \*print-escape* 和 \*print-readably* 的组合状态. 如果 \*print-readably* 或 \*print-escape* 的值[value]是 true, 那么打印器转义[printer escaping]是 "启用的"; 否则 (如果 \*print-readably* 和 \*print-escape* 的值[value]都是 false), 那么打印器转义[printer escaping]是 "禁用的".

**printing** adj. (指一个字符[character]) 是一个图形[graphic]字符[character]而不是空格[space].

**process** v.t. (一个表达式形式[form]被编译器[compiler]) 去执行最小编译[minimal compilation], 决定一个表达式形式[form]求值的时间, 并且可能求值那个表达式形式[form] (如果必要的话).

**processor** n., ANSI 一个具体实现[implementation].

**proclaim** v.t. (一个公告[proclamation]) 去建立[establish]该公告[proclamation].

**proclamation** n. 一个全局声明[global declaration].

**prog tag** n. Trad. 一个 go 标签[go tag].

**program** n. Trad. Common Lisp 代码[code].

**programmer** n. 编写一个程序[program]的一个活跃的实体, 通常是一个人, 他可能是也可能不是该程序[program]的用户[user].

**programmer code** n. 程序员提供的代码[code]; 也就是说, 不是系统代码[system code]的代码[code].

**proper list** n. 由空列表[empty list]终止的一个列表[list]. (空列表[empty list]是一个正规列表[proper list].) 见非正规列表[improper list].

**proper name** n. (指一个类[class]) 一个符号[symbol], 它命名[name]了名称[name]为该符号[symbol]的类[class]. 见函数[function] class-name 和 find-class.

**proper sequence** n. 一个不是非正规列表[imporper list]的序列[sequence]; 也就是说, 一个向量[vector]或一个正规列表[proper list].

**proper subtype** n. (指一个类型[type]) 与该类型[type]不同的该类型[type]的子类型[subtype] (换句话说, 它的元素[element]时该类型[type]的一个"正规子集[proper subset]").

**property** n. (指一个属性列表[property list]) 1. 属性指示符[property indicator]及其在属性列表[property list]上的关联属性值[property value]的概念配对. 2. 一个属性值[property value].

**property indicator** n. (指一个属性列表[property list]) 一个属性[property]的名称[name]部分, 在一个属性列表[property list]上查找一个属性值[property value]时用作一个键[key].

**property list** n. 1. 一个包含偶数个元素[element]的列表[list], 它们是交替的[name] (有时称为指示器[indicator]或键[key]) 和值[value] (有时称为属性[property]). 当属性列表[property list]中有多个具有相同[identical]名称[name]的名称[name]和值[value]对时, 第一个这样的对确定属性[property]. 2. (指一个符号[symbol]) 包含一个属性列表[property list]的该符号[symbol]的成员.

**property value** n. (指一个属性列表[property list]上的属性指示器[property indicator]) 在该属性列表[property list]上与该属性指示器[property indicator]关联的对象[object].

**purports to conform** v. 对一致性做出善意的声明, 这个术语表达了一致性的意图, 不管这个意图的目标是否在实践中实现. 例如, 众所周知, 语言实现存在 bug, 尽管带有 bug 的此规范的实现[implementation]可能不是符合规范的实现[conforming implementation], 但它仍然可以声称符合规范. 在某些特定的情况下, 这是一个重要的区别; 例如, 见变量[variable] \*features*. 


### Q

**qualified method** n. 一个方法[method], 它有一个或多个限定符[qualifier].

**qualifier** n. (指一个广义函数[generic function]的一个方法[method]) 以一种确定其在方法组合[method combination]中的角色的方式对方法[method]进行注释的几个对象[object]之一. 方法组合[method combination]类型[type]决定了每一个方法[method]允许多少修饰符[qualifier], 允许哪些修饰符[qualifier], 以及这些修饰符[qualifier]的语义.

**query I/O** n. 这个双向[bidirectional]流[stream]它时变量[variable] \*query-io* 的值[value].

**quoted object** n. 一个对象[object], 它是一个 quote 表达式形式[form]的第二个元素. 


### R

**radix** n. 一个在 2 和 36 之间的整数[integer] (包括 2 和 36), 它可以用来指定执行哪一种特定类型的数字输入或输出的基数. (对于任何给定的基数[radix] n 这里有 n 个有效数字字符, 这些数字分别是序列 0, 1, ..., 9, A, B, ..., Z 中的前 n 个数字, 分别有着权值 0, 1, ..., 9, 10, 11, ..., 35. 在解析基数大于 10 的数字时大小写是无效的, 因此 "9b8a" 和 "9B8A" 都表示相同的基数[radix] 16.)

**random state** n. 一个 random-state 类型[type]的对象[object].

**rank** n. 一个表示数组[array]的维度[dimension]数量的非负整数[integer].

**ratio** n. 一个 ratio 类型[type]的对象[object].

**ratio marker** n. 在一个比率[ratio]的文本标记中被用于分隔分子和分母的字符, 它在标准读取表[standard readtable]中是一个斜杠[slash]. 见章节 2.1 (字符语法).

**rational** n. 一个 rational 类型[type]的对象[object].

**read** v.t. 1. (一个绑定[binding]或槽[slot]或成员) 去获取该绑定[binding]或槽[slot]的值[value]. 2. (来自一个流[stream]的对象[object]) 从这个流[stream]上的该对象的表示去解析一个对象[object].

**readably** adv. (指打印一个对象[object] O1 的方式) 在这种方式下允许 Lisp 读取器[Lisp reader]稍后把这个打印输出解析为一个对象[object] O2, 它与 O1 相似[similar].

**reader** n. 1. 一个函数[function], 它读取[read[1]]一个变量[variable]或槽[slot]. 2. Lisp 读取器[Lisp reader].

**reader macro** n. 1. 一种文本符号, 由一个或两个字符[character]上的分派引入, 它定义了供 Lisp 读取器[Lisp reader]使用的特殊语法, 并由读取器宏函数实现[reader macro function]. 见章节 2.2 (读取器算法). 2. 引入一个读取器宏[reader macro[1]]的字符[character]或多个字符[character]; 也就是说, 一个宏字符[macro character]或一个分派宏字符[dispatching macro character]和跟在它后面的字符[character]的概念对. (一个读取器宏[reader macro]不是一个宏[macro]的种类.)

**reader macro function** n. 一个函数标识符[function designator], 它表示实现了一个读取器宏[reader macro[2]]的函数[function]. 见函数[function] set-macro-character 和 set-dispatch-macro-character.

**readtable** n. 一个 readtable 类型[type]的对象[object].

**readtable case** n. 一个读取表[readtable]的一个属性, 它的值是一个大小写敏感模式[case sensitivity mode], 并且它选择了一个符号[symbol]的名字[name]中的字符[character]会被 Lisp 读取器[Lisp reader]和 Lisp 打印器[Lisp printer]对待的方式. 见章节 23.1.2 (Lisp 读取器上的读取表大小写的影响) 和章节 22.1.3.3.2 (Lisp 打印器上读取表大小写的影响).

**readtable designator** n. 一个读取表[readtable]的标识符[designator]; 也就是说, 一个表示一个读取表的对象[object]并且它是以下之一: nil (表示标准读取表[standard readtable]), 或者一个读取表[readtable] (表示它自身).

**recognizable subtype** n. (指一个类型[type]) 可以被具体实现[implementation]可靠检测到的该类型[type]的一个子类型[subtype]. 见函数[function] subtypep.

**reference** n., v.t. 1. n. 引用一个对象[object], 一个绑定[binding], 一个退出点[exit point], 一个标签[tag], 或者一个环境[environment]的行为或出现. 2. v.t. 去引用一个对象[object], 一个绑定[binding], 一个退出点[exit point], 一个标签[tag], 或者一个环境[environment], 通常是通过名称[name].

**registered package** n. 一个包[package]对象[object], 它被安装到包注册表[package registry]中. (每个已注册包[registered package]有着一个字符串[string]名称[string], 以及零个或多个字符串[string]别名. All packages that are initially specified 由 Common Lisp 最初指定的或由 make-package 或 defpackage 创建的所有包[package]都是已注册包[registered package]. 已注册包[registered package]可以通过 delete-package 变成未注册包[unregistered package].)

**relative** adj. 1. (指时间[time]) 表示相对于一个绝对[absolute]时间[time]的偏移量, 使用与该时间相适应的单位表示. 例如, 一个相对[relative]内部时间[internal time] 是两个绝对[absolute]内部时间[internal time]只差, 用内部时间单位[internal time unit]来衡量. 2. (指一个路径名[pathname]) 通过从根目录以外的位置移动来表示目录层次结构中的位置, 因此根目录的位置可能不同. "如果主机文件系统是 Unix, 那么标记 #P"../foo.text" 表示一个相对路径名 (The notation #P"../foo.text" denotes a relative pathname if the host file system is Unix)". 见绝对[absolute].

**repertoire** n., ISO 一个 character 的子类型[subtype]. 见章节 13.1.2.2 (字符字元库).

**report** n. (指一个状况[condition]) 在一个 \*print-escape* 的值[value]是 false 的环境[environment]中在该状况[condition]上去调用函数[function] print-object.

**report message** n. 由状况汇报器[condition reporter]输出的文本.

**required parameter** n. 调用函数[function]时必须为其提供相应位置实参[argument]的形参[parameter].

**rest list** n. (指一个具有剩余参数[rest parameter]的函数[function]) 在对函数[function]的某个特定调用[call]上剩余参数[rest parameter]被绑定[bound]到的那个列表[list].

**rest parameter** n. 由 &rest 引入的一个形参[parameter].

**restart** n. 一个 restart 类型[type]的对象[object].

**restart designator** n. 一个重启动[restart]的标识符[designator]; 也就是说, 一个表示重启动[restart]的对象[object], 并且是以下之一: 一个非 nil [non-nil]符号[symbol] (表示名字[name]为该符号[symbol]的最近建立的活跃[active]重启动[restart]), 或者一个重启动[restart] (表示它自身).

**restart function** n. 一个调用一个重启动[restart]的函数[function], 就像是通过 invoke-restart 一样. 一个重启动函数[restart function]的主要目的是去提供一个替代的接口. 按照惯例, 重启动函数[restart function]通常与它调用的重启动[restart]具有相同的名称[name]. 下一段展示了标准化[standardized]重启动函数[restart function]的列表.

abort     muffle-warning  use-value  
continue  store-value                

Figure 26-4. 标准化重启动函数

**return** v.t. (指值[value]) 1. (从一个语句块[block]) 从这个语句块[block]转移控制和值[value]; 也就说说, 去使这个语句块[block]在没有对主体中的表达式形式[form]做任何进一步求值的情况下立即产生[yield]这些值[value]. 2. (从一个表达式形式[form]) 来产生[yield]这些值[value].

**return value** n. Trad. 一个值[value[1]]

**right-parenthesis** n. 标准字符[standard character] ")", 这被称为 "右圆括号" 或 "闭圆括号". 见 Figure 2-5.

**run time** n. 1. 加载时间[load time] 2. 执行时间[execution time]

**run-time compiler** n. 引用 compile 函数或隐式编译[implicit compilation], 编译和运行时环境[environment]在相同的 Lisp 映像[Lisp image]中维护.

**run-time definition** n. 在运行时环境[run-time environment]中的一个定义.

**run-time environment** n. 一个程序被执行时所在的环境[environment]. 


S

**safe** adj. 1. (指代码[code]) 在最高安全(safety)等级 (3) 生效的词法环境[lexical environment]中处理的. 见 optimize. 2. (指一个调用[call]) 一个安全调用[safe call].

**safe call** n. 一种调用[call], 其中该调用[call], 被调用的函数[function], 以及函数求值[functional evaluation]的点都是安全[safe[1]]代码[code]. 关于更多详细信息, 见章节 3.5.1.1 (安全和非安全調用).

**same** adj. 1. (指多个对象[object]在指定断言[predicate]) 通过该断言[predicate]是不可区分的. "符号 car, 字符串 "car", 以及字符串 "CAR" 在 string-equal 下时相同的(The symbol car, the string "car", and the string "CAR" are the same under string-equal)". 2. (指多个对象[object], 如果上下文没有暗示断言) 通过 eql 是不可区分的. 注意, eq 可能可以区分 eql 不能区分的一些数字[number]和字符[character], 但是这样的性质如果存在的话是依赖于具体实现的[implementation-dependent]. 由于 eq 在这个规范中很少使用, 当没有显式提及时, eql 就是默认的断言. "两个对 cons 的连续调用返回的 cons 从不相同 (The conses returned by two successive calls to cons are never the same)". 3. (指类型[type]) 具有相同的元素[element]集合; 也就是说, 每个类型[type]是另一个的子类型[subtype]. "由 (integer 0 1), (unsigned-byte 1), 和 bit 指定的类型是相同的 (The types specified by (integer 0 1), (unsigned-byte 1), and bit are the same)".

**satisfy the test** v. (指一个正在被序列函数[sequence function]考虑的对象[object]) 1. (对于一个单实参[argument]测试) 当给定一个实参[argument] (该实参是在被考虑对象[object]上调用序列函数[sequence function]的 key 参数[argument]的结果)时, 作为序列函数[sequence function] predicate 参数[argument]的函数[function]返回 true. 见章节 17.2.2 (满足一个单参数的测试). 2. (对于一个两实参[argument]测试) 处于这样一种状态: 当被考虑的对象[object]作为第一个参数[argument]并且这个序列函数[sequence function]的 sequence 参数[argument]的元素[element]上调用该序列函数[sequence function]的 key 参数[argument]的结果作为第二个参数[argument]时, 该序列函数[sequence function]的 test 参数[argument]的二元的断言[predicate]检测为等价的; 或者处于 test-not 函数[function]在给定相同参数[argument]的情况下返回 false 的状态. 见章节 17.2.1 (满足一个两个参数的测试).

**scope** n. 可以引用一个对象[object], 一个绑定[binding], 一个退出点[exit point], 一个标签[tag], 或一个环境[environment] (通常是通过名称[name]) 的代码中的结构或文本区域.

**script** n. ISO 组成 character 类型的详尽分区[exhaustive partition]的多个可能的集合中的一个. 见章节 13.1.2.1 (字符文字).

**secondary value** n. (指一个表达式形式[form]的求值[evaluation]所产生的多个值[value]) 如果存在就是第二个值[value], 如果这里少于两个值[value]那么就是 nil. "由 truncate 返回的第二个值是个余数 (The secondary value returned by truncate is a remainder)".

**section** n. 在美观打印流[pretty printing stream]上用条件换行[conditional newline]对输出进行的分区. 见章节 22.2.1.1 (输出排列的动态控制).

**self-evaluating object** n. 一个对象[object], 它既不是一个符号[symbol]也不是一个 cons. 如果一个自求值对象[self-evaluating object]被求值, 它产生[yield]它自身作为仅有的值[value]. "字符串是自求值对象 (Strings are self-evaluating objects)".

**semi-standard** adj. (指一个语言特性) 没有要求被任何符合规范的实现[conforming implementation]所实现, 但是在一个实现[implementation]确实计划支持此类特性的情况下, 推荐使用规范方法. 该语言中存在半标准[semi-standard]方面的目的是减少可移植性问题, 并降低实现[implementation]之间不必要的差异的风险, 这些差异可能会阻碍未来的标准化.

**semicolon** n. 标准字符[standard character], 它被称为 "分号(semicolon)" (;). 见 Figure 2-5.

**sequence** n. 1. 元素的一个有序集合 2. 一个向量[vector]或一个列表[list].

**sequence function** n. 在 Figure 17-1 中的那些函数[function]之一, 或者一个具体实现定义的[implementation-defined]操作一个或多个序列[sequence]的函数[function], 它也被具体实现[implementation]定义为序列函数[sequence function].

**sequential** adj. Trad. (指绑定[binding]或赋值) 以 setq、let* 或 do* 的风格完成; 也就是说, 将生成值[value]的表达式形式[form]的计算与变量[variable] (或位置[place])的赋值或绑定[binding]交错起来. 见并行[parallel].

**sequentially** adv. 以一种顺序的[sequential]方式.

**serious condition** n. 一个 serious-condition 类型[type]的状况[condition], 它表示一种情况[situation], 这种情况通常非常严重, 如果该状况[condition]被发出但没有被处理, 则应该预期会进入调试器[debugger].

**session** n. 在一个 Lisp 映像[List image]中从开始到结束的事件的概念聚合.

**set** v.t. Trad. (任何变量[variable]或一个符号[symbol], 它是一个动态变量[dynamic variable]的名称[name]) 去对这个变量[variable]赋值[dynamic variable].

**setf expander** n. 被 setf 用来计算一个位置[place]的 setf 展开式[setf expansion]的函数[function].

**setf expansion** n. 一个五表达式[expressions[1]]的集合, 一起描述了如何存储到一个位置[place]以及求值与该位置关联的宏调用的哪些子表达式形式[subform]. 见章节 5.1.1.2 (Setf 展开式).

**setf function** n. 一个名字[name]为 (setf symbol) 的函数[function].

**setf function name** n. (指一个符号[symbol] S) 列表[list] (setf S).

**shadow** v.t. 1. 去重写...的意义. "X 的绑定遮蔽了较外面的那个 (That binding of X shadows an outer one)". 2. 去隐藏...的存在. "这个 F 的 macrolet 遮蔽了较外面的 F 的 flet". 3. 去替换. "这个包用它自己的符号 car 遮蔽了符号 cl:car (That package shadows the symbol cl:car with its own symbol car)".

**shadowing symbol** n. (在一个包[package]中) 该包[package]的遮蔽符号列表[shadowing symbols list]的一个元素[element].

**shadowing symbols list** n. (指一个包[package]) 和该包[package]关联的一个符号[symbol]列表[list], 这些符号在包被使用时将免于"符号冲突错误"检测. 见函数[function] package-shadowing-symbols.

**shared slot** n. (指一个类[class]) 一个槽[slot]在一个类[class]的不止一个实例[instance]中是可访问的[accessible]; 具体来说, 这样一个槽[slot]在该类[class]的所有直接实例[direct instance]中都是可访问的[accessible], 并且在类[class]没有遮蔽[shadow[1]]该槽[slot]的间接实例[indirect instance]中也是可访问的.

**sharpsign** n. 标准字符[standard character], 被称为 "数字符号"、 "sharp" 或 "井号" (#). 见 Figure 2-5.

**short float** n. 一个 short-float 类型[type]的对象[object].

**sign** n. 标准字符[standard character] "+" 或 "-".

**signal** v. 使用标准协议宣布已经检测到由状况[condition]表示的特定情况. 见章节 9.1 (状况系统的概念).

**signature** n. (指一个方法[method]) 该方法的形参[parameter]和参数特化符[parameter specializer]的描述, 决定了该方法[method]对于一个给定的必要实参[argument]集合的可应用性, 并且也描述了它的其他非必要实参[argument]的参数[argument]规约.

**similar** adj. (指两个对象[object]) 在相似性[similarity]关系下被定义为等价的.

**similarity** n. 一个二元概念等价性断言, 它独立于 Lisp 映像[Lisp image], 这样一来在不同 Lisp 映像[Lisp image]中的两个对象[object]可以在这个断言下被理解为等价的. 见章节 3.2.4 (编译后文件中的字面化对象).

**simple** adj. 1. (指数组[array]) 是 simple-array 类型[type]. 2. (指一个字符[character]) 没有具体实现定义的[implementation-defined]属性[attribute], 或者是有着具体实现定义的[implementation-defined]属性[attribute]但是每一个属性[attribute]都有着空[null]值.

**simple array** n. 一个 simple-array 类型[type]的数组[array].

**simple bit array** n. 一个位数组[bit array], 它是一个简单数组[simple array]; 也就是说, 一个 (simple-array bit) 类型[type]的对象[object].

**simple bit vector** n. 一个 simple-bit-vector 类型[type]的位向量[bit vector].

**simple condition** n. 一个 simple-condition 类型[type]的状况[condition].

**simple general vector** n. 一个简单向量[simple vector].

**simple string** n. 一个 simple-string 类型[type]的字符串[string].

**simple vector** n. 一个 simple-vector 类型[type]的向量[vector], 有时被称为一个 "简单普通向量[simple general vector]". 不是所有的简单的[simple]向量[vector]都是简单向量[simple array]---只有那些元素类型[element type]为 t 的.

**single escape** n., adj. 1. n. 一个字符[character]的语法类型[syntax type], 表示下一个字符[character]被当作是字母[alphabetic[2]]字符[character], 保留它的大小写[case]. 关于详情, 见章节 2.1.4.6 (单转义字符). 2. adj. (一个字符[character]的) 有着单转义[single escape]语法类型[syntax type]. 3. n. 一个单转义[single escape[2]]字符[character]. (在标准读取表[standard readtable]中, 斜线[slash]是仅有的单转义[single escape].)

**single float** n. 一个 single-float 类型[type]的对象[object].

**single-quote** n. 标准字符[standard character], 它也被称为 "撇号(apostrophe)","重音号(acute accent)","引号(quote)",或 "单引号(single quote)" ('). 见 Figure 2-5.

**singleton** adj. (指一个序列[sequence]) 只有一个元素[element]. "(list 'hello) 返回一个单元素列表 ((list 'hello) returns a singleton list)".

**situation** n. 一个表达式形式[form]在一个特定环境[environment]中的求值[evaluation].

**slash** n. 标准字符[standard character], 它也被称为 "斜线(solidus)" 或 "斜杠(slash)" (/). 见 Figure 2-5.

**slot** n. 可以存储一个值[value]的一个对象[object]的组件.

**slot specifier** n. 一个槽[slot]的表示, 它包括槽[slot]的名称[name]以及零个或多个槽[slot]选项. 一个槽[slot]选项只能从属于一个单独的槽[slot].

**source code** n. 表示适合求值[evaluation]的对象[object]的代码[code] (例如, objects created 由 read, 宏展开式[macro expansion], 或者编译期宏展开式[compiler macro expansion]创建的对象[object]).

**source file** n. 一个包含源代码[source code]的文本表示的文件[file], 它可以被编辑、加载或编译.

**space** n. 标准字符[standard character] <Space>, 为 Lisp 读取器[Lisp reader]标记为 #\Space.

**special form** n. 一个列表[list], 而不是一个宏表达式形式[macro form], 它是一个具有特殊语法或特殊求值[evaluation]规则或者两者兼有的表达式形式[form], 它可能操纵求值[evaluation]环境[environment]或控制流或两者都操纵. 一个特殊表达式形式[special form]的第一个元素是一个特殊操作符[special operator].

**special operator** n. 枚举在 Figure 3-2 中的一组固定符号[symbol]中的一个, 它可能出现在一个表达式形式[form]的 car 中来标识这个表达式形式[form]为一个特殊表达式形式[special form].

**special variable** n. Trad. 一个动态变量[dynamic variable].

**specialize** v.t. (一个广义函数[generic function]) 去为这个广义函数[generic function]定义一个方法[method], 或者换句话说, 通过给它一组类[class]或实参[argument]的特定意义来精炼这个广义函数[generic function]的行为.

**specialized** adj. 1. (指一个广义函数[generic function]) 有着特化[specialize]这个广义函数[generic function]的方法[method]. 2. (指数组[array]) 实际数组元素类型[actual array element type]是类型[type] t 的合适子类型[proper subtype]; 见章节 15.1.1 (数组元素). "(make-array 5 :element-type 'bit) 创建一个长度 5 的数组, 并且它特化为 bit ((make-array 5 :element-type 'bit) makes an array of length five that is specialized for bits)".

**specialized lambda list** n. 一个扩展 lambda 列表[extended lambda list], 被用于建立[establish]方法[method]定义的表达式形式[form]中, 例如 defmethod. 见章节 3.4.3 (特化的 lambda 列表).

**spreadable argument list designator** n. 一个对象[object]列表[list]的标识符[designator]; 也就是说, 一个对象[object], 它表示一个列表[list]并且它是一个长度为 n 的非空列表[list] L1, 其中最后一个元素是长度为 m 的列表[list] L2 (表示一个长度为 m+n-1 的列表 L3, 其中元素是 L1i (i < n-1) 后面跟着 L2i (j < m)). "列表 (1 2 (3 4 5)) 是列表 (1 2 3 4 5) 的一个可扩展参数列表标识符 (The list (1 2 (3 4 5)) is a spreadable argument list designator for the list (1 2 3 4 5))".

**stack allocate** v.t. Trad. 用一种非持久方式, 例如在一个栈上. 栈上分配是一种优化技术在某些实现[implementation]中用于分配具有动态范围[dynamic extent]的某些类型的对象[object]. 这样的对象[object]会被分配在栈上而不是在堆上, 这样一来它们的存储就可以作为栈解除的一部分被释放而不是在下一次垃圾回收之前都占用着堆中的空间. 什么类型 (如果有的话) 可以有动态范围[dynamic extent]在实现[implementation]和实现[implementation]之间有所不同. 没有要求实现[implementation]去执行栈上分配.

**stack-allocated** adj. Trad. 已经被栈上分配.

**standard character** n. 一个 standard-char 类型[type]的字符[character], 它是所有符合规范的实现[conforming implementation]中需要的 96 个固定字符[character]中的一个. 见章节 2.1.3 (标准字符).

**standard class** n. 一个类[class], 它是类[class] standard-class 的一个广义实例[generalized instance].

**standard generic function** 一个 standard-generic-function 类型[type]的函数[function].

**standard input** n. 这个输入[input]流[stream], 它是动态变量[dynamic variable] \*standard-input* 的值[value].

**standard method combination** n. 名为 standard 的方法组合[method combination].

**standard object** n. 一个对象[object], 它是类[class] standard-object 的一个广义实例[generalized instance].

**standard output** n. 这个输出[output]流[stream], 它是动态变量[dynamic variable] \*standard-output* 的值[value].

**standard pprint dispatch table** n. 一个美观打印分派表[pprint dispatch table], 它和初始美观打印分派表[initial pprint dispatch table]不同, 它按照这个规范中所描述的实现了美观打印, 并且它不像其他美观打印分派表[pprint dispatch table], 一定不能被任何程序修改. (虽然在本文档中通常使用了明确的引用 "标准美观打印分派表", 但是到底是一个单独的对象[object]还是多个这样的对象来充当标准美观打印分派表[standard pprint dispatch table]的角色, 实际上是依赖于具体实现的[implementation-dependent], 任何一种都可以被用于需要"标准美观打印分派表"的特定场合. 因此, 除照应前项引用外, 这句话在所有情况下都应视为不确定的引用.)

**standard readtable** n. 一个区别[different]于初始读取表[initial readtable]的读取表[readtable], 它实现了这个规范中定义的表达式[expression]语法, 并且它不像其他读取表[readtable], 一定不能被任何程序所修改. (虽然明确的引用 "标准读取表[standard readtable]" 被普遍用于这个文档中, 事实上, 是否是一个单独的对象[object]充当这个标准读取表[standard readtable]的角色, 还是这里存在多个这样的对象, 它们中的任意一个都可以被用于需要 "标准读取表[standard readtable]" 的场合, 这个是依赖于具体实现的[implementation-dependent]. 因此, 这个短语在所有情况下都应该被看作是不确定的引用, 除了照应前项引用(anaphoric reference).)

**standard syntax** n. 由标准读取表[standard readtable]表示的语法, 并且被用作贯穿这个文档的参考语法. 见章节 2.1 (字符语法).

**standardized** adj. (一个名字[name], 对象[object], 或定义) 已经被 Common Lisp 定义. "所有需要持有双向流的标准化变量的名字中都具有 '-io*' (All standardized variables that are required to hold bidirectional streams have \"-io*\" in their name)".

**startup environment** n. 调用编译器的运行的 Lisp 映像[Lisp image]的全局环境[global environment].

**step** v.t., n. 1. v.t. (一个循环变量[variable]) 在一个循环的结束去给这个变量[variable]赋予[assign]一个新值[value], 为新的循环做准备. 2. n. 标识一个循环中的下一个值如何被计算的代码[code]. 3. v.t. (代码[code]) 去特殊地执行这个代码[code], 每隔一段时间停下来让用户确认或干预, 通常用于调试.

**stream** n. 可以被一个输入或输出函数用来标识这个操作的一个字符[character]或字节[byte]源或目标的对象[object].

**stream associated with a file** n. 一个文件流[file stream], 或者一个同义流[synonym stream], 它的目标是一个和文件关联的流[stream associated with a file]. 这样一个流[stream]不能用 make-two-way-stream, make-echo-stream, make-broadcast-stream, make-concatenated-stream, make-string-input-stream, 或 make-string-output-stream 创建.

**stream designator** n. 一个流[stream]的标识符[designator]; 也就是说, 一个对象[object], 它是一个流[stream]并且它是以下之一: t (表示 \*terminal-io* 的值[value]), nil (对于输入[input]流标识符[stream designator]表示 \*standard-input* 的值[value]或者对于输出[output]流标识符[stream designator]表示 \*standard-output* 的值[value]), 或者一个流[stream] (表示它自身).

**stream element type** n. (指一个流[stream]) 这个流[stream]被特化的数据类型[type].

**stream variable** n. 一个值[value]必须为一个流[stream]的变量[variable].

**stream variable designator** n. 一个流变量[stream variable]的标识符[designator]; 这也就是说, 一个表示流变量[stream variable]的符号[symbol]并且它是以下之一: t (表示 \*terminal-io*), nil (对于输入[input]流变量标识符[stream variable designator]表示 \*standard-input* 或者对于输出[output]流变量标识符[stream variable designator]表示 \*standard-output*), 或者某个其他符号[symbol] (表示它自身).

**string** n. 一个 string 类型[type]的特化向量[vector], 并且它的元素是 character 类型[type]或者 character 类型[type]的子类型[subtype].

**string designator** n. 一个字符串[string]的标识符[designator]; 这也就是说, 一个表示字符串[string]的对象[object]并且它是其中之一: 一个字符[character] (表示一个单独的字符串[string], 持有该字符[character]作为它仅有的元素[element]), 一个符号[symbol] (表示它的名称字符串[string]), 或者一个字符串[string] (表示它自身). 其目的是使这个术语与 string 的行为保持一致; 扩展 string 的具体实现[implementation]必须以一种兼容的方式扩展这个术语的意义.

**string equal** adj. 在 string-equal 下是相同的[same].

**string stream** n. 一个 string-stream 类型[type]的流[stream].

**structure** n. 一个 structure-object 类型[type]的对象[object].

**structure class** n. 一个类[class], 它是类[class] structure-class 的广义实例[generalized instance].

**structure name** n. 使用 defstruct 定义的名称[name]. 通常, 这样一个类型[type]也是一个结构体类[structure class], 但是如果使用了给 defstruct 的 :type 选项, 可能会出现依赖于具体实现[implementation-dependent]的情况, 而不是这样.

**style warning** n. 一个 style-warning 类型[type]的状况[condition].

**subclass** n. 一个类[class], 它继承[inherit]自另一个类[class], 称为超类[superclass]. (没有类[class]是它自身的子类[subclass].)

**subexpression** n. (指一个表达式[expression]) 一个表达式[expression], 它被包含在该表达式[expression]中. (事实上, 成为一个子表达式[subexpression]的状态不是该子表达式[subexpression]的一个属性, 但实际上是包含表达式[expression]的属性, 因为相同[same]对象[object]可以同时是一个上下文中的子表达式[subexpression], 而不是另一个上下文中的子表达式.)

**subform** n. (指一个表达式形式[form]) 一个表达式[expression], 它是这个表达式形式[form]的子表达式[subexpression], 由于它在这个表达式形式[form]中的位置, 它也是一个表达式形式[form]. "(f x) 和 x, 除了 exit, 都是 (return-from exit (f x)) 的子表达式形式[subform] ((f x) and x, but not exit, are subforms of (return-from exit (f x)))".

**subrepertoire** n. 一个字元库[repertoire]的子集.

**subtype** n. 一个类型[type], 它的成员关系和另一个称为超类型[supertype]的类型[type]的成员关系相同或者是一个适当子集. (每个类型[type]都是它自身的一个子类型[subtype].)

**superclass** n. 一个类[class], 另一个类[class] (称为一个子类[subclass])继承[inherit]自它. (没有类[class]是它自身的一个超类[superclass].) 见子类[subclass].

**supertype** n. 一个类型[type], 它的成员关系与另一个称为子类型[subtype]的类型[type]的成员关系相同或是一个适当超集. (每个类型[type]都是它自身的超类型[supertype].) 见子类型[subtype].

**supplied-p parameter** n. 一个形参[parameter], 由于与另一个形参[parameter] (例如一个可选参数[optional parameter]或一个剩余参数[rest parameter])对应的实参[argument]的存在与否, 它隐式地接收它的广义 boolean [generalized boolean]值. 见章节 3.4.1 (普通 Lambda 列表).

**symbol** n. 一个 symbol 类型[type]的对象[object].

**symbol macro** n. 一个符号[symbol], 它代表另一个表达式形式[form]. 见宏[macro] symbol-macrolet.

**synonym stream** n. 1. 一个 synonym-stream 类型[type]的流[stream], 因此它是一个流[stream], 这个流是另一个流[stream]的别名, 它是一个动态变量[dynamic variable]的值[value], 该变量的名称[name]是同义流[synonym stream]的同义流符号[synonym stream symbol]. 见函数[function] make-synonym-stream. 2. (对一个流[stream]) 一个同义流[synonym stream], 它的同义流符号[synonym stream symbol]的值[value]就是该流[stream]. 3. (对一个符号[symbol]) 一个同义流[synonym stream], 它的同义流符号[synonym stream symbol]即为该符号[symbol].

**synonym stream symbol** n. (指一个同义流[synonym stream]) 这个符号[symbol]命名一个动态变量[dynamic variable], 它的值为另一个流, 这个同义流[synonym stream]是该流的一个别名.

**syntax type** n. (一个字符[character]的) 几个在解析期间被 Lisp 读取器[Lisp reader]用来分派的类型中的一个, 枚举在 Figure 2-6 中. 见章节 2.1.4 (字符语法类型).

**system class** n. 一个类[class], 在一个符合规范的实现[conforming implementation]中它可能是 built-in-class 类型[type] 并且因此不能被符合规范的程序[conforming program]定义的类[class]所继承.

**system code** n. 由具体实现[implementation]提供来实现这个规范 (例如, mapcar 的定义) 或自动生成支持此规范的代码[code] (例如, 在方法组合期间); 也就是说, 不是编程者代码[programmer code]的代码[code]. 


### T

**t** n. 1. a. 表示 true 的 boolean 值. b. 表示 true 的标准广义 boolean [generalized boolean]. (虽然除了 nil 以外的任何对象[object]都被认为是 true, 作为一个广义 boolean [generalized boolean], t 通常用于没有特殊原因更偏向其中一个对象[object]时.) 2. 所有对象[object]所属类型[type]的名字[name]---所有类型[type]的超类型[supertype] (包括它自身). 3. 除了它自身以外的所有类[class]的超类[superclass]的名字[name].

**tag** n. 1. 一个捕获标签[catch tag]. 2. 一个 go 标签[go tag].

**tail** n. (指一个列表[list]) 一个对象[object], 它和构成这个列表[list]的某个 cons 相同[same]或者和终止这个列表[list]的原子[atom]相同[same] (如果存在的话). "空列表是每个正规列表的末尾 (The empty list is a tail of every proper list)".

**target** n. 1. (指一个构造流[constructed stream]) 这个构造流[constructed stream]的一个标记成分[constituent]. "一个同义流的目标是它的同义流符号的值 (The target of a synonym stream is the value of its synonym stream symbol)". 2. (指一个存储被转移的数组[displaced array]) 这个存储被转移的数组[displaced array]被转移到的数组[array]. (对于一个构造流[constructed stream]或存储被转移的数组[displaced array]的链, 非限定术语 "目标[target]" 总是指链中第一项的直接目标[target], 而不是最后一项的直接目标.)

**terminal I/O** n. 这个双向流[bidirectional stream]是变量[variable] \*terminal-io* 的值[value].

**terminating** n. (一个宏字符[macro character]的) 是这样的, 如果它在解析一个标记时出现, 那么它终止那个标记. 见章节 2.2 (读取器算法).

**tertiary value** n. (指从一个表达式形式[form]的求值[evaluation]所产生的哪些值[value]) 第三个值[value] (如果存在的话), 或者如果这里少于三个值[value]那么就是 nil.

**throw** v. 去转移控制和值[value]到一个捕获点[catch]. 见特殊操作符[special operator] throw.

**tilde** n. 标准字符[standard character], 它被称为 "波浪符(tilde)" (~). 见 Figure 2-5.

**time** 一个时间线上的时间点 (绝对[absolute]时间[time]) 或者时间间隔 (相对[relative]时间[time]) 的表示. 见解码时间[decoded time], 内部时间[internal time], 和通用时间[universal time].

**time zone** n. 一个 1/3600 的有理[rational]倍数, 在-24(包含)和24(包含)之间, 表示一个时区与格林尼治平均时间偏移的小时数. 时区值随着向西移动而增加, 美国马萨诸塞州位于时区 5, 美国加利福尼亚州位于时区 8, 俄罗斯莫斯科位于时区 3. (当 "夏令时" 单独表示为一个实参[argument]或返回值[return value]时, 它所对应的时区并不取决于夏令时是否有效.)

**token** n. 一个数字[number]或符号[symbol]的文本表示. 见章节 2.3 (Interpretation of Tokens).

**top level form** n. 一个表达式形式[form], 出于启用该表达式形式[form]的编译期[compile time]求值[evaluation]的目的, 它被 compile-file 特殊处理. 顶层表达式形式[Top level form]包括那些不是任何其他表达式形式[form]的子表达式形式[subform]的表达式形式[form], 以及某些其他情况. 见章节 3.2.3.1 (顶层表达式形式的处理).

**trace output** n. 这个输出[output]流[stream], 它是动态变量[dynamic variable] \*trace-output* 的值[value].

**tree** n. 1. 一个由 cons 和原子[atom]组成的二元递归数据结构: 这些 cons 自身也是树[tree] (有时称为 "子树(subtree)" 或 "分支(branch)"), 而原子[atom]是终端节点 (有时也称为叶[leaf]). 通常, 这些叶[leaf]表示数据而这些分支建立数据之间的某个联系. 2. 多数情况下, 任何具有某种"分支"和"叶[leaf]"概念的递归数据结构.

**tree structure** n. (指一个树[tree[1]]) 组成这个树[true]的 cons 的集合. 注意, 当每一个这样的 cons 的 car[1b] 组件是这个树结构[tree structure]的一部分时, 这个树[tree]中每一个 cons 的 car[2] 中的这些对象[object]本身不是树结构[tree structure]的一部分, 除非它们也是 cons.

**true** n. 任何不是 false 的对象[object], 被用于表示一个断言[predicate]测试的成功. 见 t[1].

**truename** n. 1. 在文件系统[file system]中的一个文件[file]的正规文件名[filename]. 见章节 20.1.3 (真实名字). 2. 表示一个真实名字[truename[1]]的路径名[pathname].

**two-way stream** n. 一个 two-way-stream 类型[type]的流[stream], 它是一个双向[bidirectional]复合流[composite stream], 它从一个关联的输入[input]流[stream]接收它的输入并把它的输出发送到一个关联的输出[output]流[stream].

**type** n. 1. 一个对象(object)的集合, 通常带有共同的结构, 行为, 和目的. (注意, 如果 Sa 是 Sb 的一个子类型, 语句 "X 是 Sa 类型" 自然意味着 "X 是 Sb 类型".) 2. (紧跟在类型[type]名称之后) 那个类型[type]的一个子类型[subtype]. "类型 vector 是一个数组类型 (The type vector is an array type)."

**type declaration** n. 一个声明[declaration], 断言声明[declaration]的范围内对指定绑定[binding]的每次引用结果都是指定类型[type]的某个对象[object].

**type equivalent** adj. (指两个类型[type] X 和 Y) 相同的元素[element]; 也就是说, X 是 Y 的子类型[subtype] 并且 Y 是 X 的子类型[subtype].

**type expand** n. 去完全展开为一个类型指定符[type specifier], 移除对衍生类型[derived type]的任何引用. (Common Lisp 没有提供编程接口来使这个发生, 但是 Common Lisp 的语义是这样的, 每个实现[implementation]都必须能够在内部完成这一操作, 而且一些涉及类型指定符[type specifier]的情况最容易用完全展开的类型指定符[type specifier]来描述.)

**type specifier** n. 表示一个类型[type]的表达式[expression]. "符号 random-state, 列表 (integer 3 5), 列表 (and list (not null)), 以及名为 standard-class 的类都是类型指定符 (The symbol random-state, the list (integer 3 5), the list (and list (not null)), and the class named standard-class are type specifiers)". 


### U

**unbound** adj. 在一个绑定[binding]中没有一个关联的表示. 见绑定[bound].

**unbound variable** n. 一个名称[name], 它在语法结构上似乎是一个变量[variable]的名字, 但是它在变量[variable]命名空间[namespace]中没有被绑定[bound].

**undefined function** n. 一个名称[name], 它在语法结构上似乎是一个函数[function]的名字, 但是它在函数[function]命名空间[namespace]中没有被绑定[bound].
符号符号符号
**unintern** v.t. (一个包[package]中的一个符号[symbol]) 去使这个符号[symbol]没有出现[present]在包[package]中. (这个符号[symbol]可能通过继承可继续访问.)

**uninterned** adj. (指一个符号[symbol]) 在任何包[package]中都是不可访问的[accessible]; 换句话说, 不是捕获的[interned[1]].

**universal time** n. 时间[time], 表示为秒的一个非负整数[integer]数量. 绝对[absolute]通用时间[universal time]使用一个从 1900 年开始的偏移来测量 (忽略闰秒[leap second]). 见章节 25.1.4.2 (通用时间).

**unqualified method** n. 一个没有限定符[qualifier]的方法[method].

**unregistered package** n. 一个没有出现在包注册表[package registry]中的包[package]对象[object]. 一个未注册的包[unregistered package]没有名字[name]; 换句话说, 它的名称[name]是 nil. 见函数[function] delete-package.

**unsafe** adj. (指代码[code]) 不是安全的[safe]. (注意, 除非显式地另外指定, 否则如果只保证在安全[safe]上下文中进行特定类型的错误检查, 那么如果是不安全的[unsafe], 则相同的检查可能会也可能不会在该上下文中发生; 把一个上下文描述为不安全的[unsafe]意味着特定种类的错误检查不能确定启用, 但不能保证绝对禁用错误检查.)

**unsafe call** n. 一个不是安全调用[safe call]的调用[call]. 关于更多详细的信息, 见章节 3.5.1.1 (安全和非安全调用).

**upgrade** v.t. (一个声明的类型[type]到一个实际的类型[type]) 1. (在创建一个数组时[array]) 当选择一个适当特化[specialized]数组[array]表示时将实际数组元素类型[actual array element type]替换为表达数组元素类型[expressed array element type]. 见函数[function] upgraded-array-element-type. 2. (当创建一个复数时[complex]) 当选择一个适当特化[specialized]复数[complex]表示时将实际复数部分类型[actual complex part type]替换为一个表达复数部分类型[expressed complex part type]. 见函数[function] upgraded-complex-part-type.

**upgraded array element type** n. (指一个类型[type]) 一个类型[type], 它是该类型[type]的一个超类型[supertype], 当该类型[type]用作数组元素类型[array element type]的对象创建或类型区分时, 它被用于替换该类型[type]. 见章节 15.1.2.1 (数组提升).

**upgraded complex part type** n. (指一个类型[type]) 一个类型[type], 它是该类型[type]的一个超类型[supertype], 当该类型[type]用作复数部分类型[complex part type]的对象创建或类型区分时, 它被用于替换该类型[type]. 见函数[function] upgraded-complex-part-type.

**uppercase** adj. (指一个字符[character]) 在标准字符[standard character]中对应大写字母 A 到 Z, 或者是某个被具体实现[implementation]定义为大写[uppercase]的具体实现定义的[implementation-defined]字符[character]. 见章节 13.1.4.3 (大小写字符).

**use** v.t. (一个包[package] P1) 去继承[inherit] P1 的外部符号[external symbol]. (如果包[package] P2 使用 P1, 那么 P1 的外部符号[external symbol]成为 P2 的内部符号[internal symbol], 除非它们被显式导出[export].) "包 CL-USER 使用包 CL (The package CL-USER uses the package CL)".

**use list** n. (指一个包[package]) 一个和每个包[package]关联的 (可能为空的) 列表[list], 它决定了其他哪些包[package]当前正在被该包[package]所使用.

**user** n. 在运行时调用程序[package]或与程序[package]交互的活动实体(通常是人), 但不一定是程序员[programmer]. 


### V

**valid array dimension** n. 适合用于一个数组[array]维度[dimension]的一个 fixnum. 这样一个 fixnum 一定大于等于零, 并且小于 array-dimension-limit 的值[value]. 当多个数组[array]维度[dimension]一起使用来指定一个多维数组[array]时, 还有一个隐含的约束, 即所有维度的乘积都小于 array-total-size-limit 的值[value].

**valid array index** n. (指数组[array]) 一种适合用作根据多维笛卡尔坐标系命名数组[array]元素[element]所需的几个索引之一的 fixnum. 这样一个 fixnum 一定大于等于零, 并且小于该数组[array]的对应维度[dimension[1]]. (除非另有明确规定, 术语 "一个有效数组索引列表" 进一步暗示了这个列表[list]的长度[length]和这个数组[array]的秩[rank]相同.) "对于一个2×3的数组, 第一个维度的有效数组索引是0和1, 第二个维度的有效数组索引是0、1和2 (For a 2 by 3 array, valid array indices for the first dimension are 0 and 1, and valid array indices for the second dimension are 0, 1 and 2)".

**valid array row-major index** n. (指一个数组[array], 它可能有任意数量的维度[dimensions[2]]) 适用于命名该数组[array]的任何元素[element]的一个单独的 fixnum, 通过将该数组的存储视为以行优先的顺序排列的一系列线性元素. 这样一个 fixnum 一定大于等于零, 并且小于该数组[array]的数组总大小[array total size].

**valid fill pointer** n. (指数组[array]) 一个适用于该数组[array]的填充指针[fill pointer]的 fixnum. 这样一个 fixnum 必须大于等于零, 并且小于等于该数组[array]的数组总大小[array total size].

**valid logical pathname host** n. 一个字符串[string], 它已经被定义为一个逻辑主机[logical host]的名称. 见函数[function] load-logical-pathname-translations.

**valid pathname device** n. 一个字符串[string], nil, :unspecific, 或某个被具体实现[implementation]定义为有效路径名设备[valid pathname device]的其他对象[object].

**valid pathname directory** n. 一个字符串[string], 一个字符串[string]列表[list], nil, :wild, :unspecific, 或某个被具体实现[implementation]定义为有效路径名成员的其他对象[object].

**valid pathname host** n. 一个有效物理路径名主机[valid physical pathname host]或一个有效逻辑路径名主机[valid logical pathname host].

**valid pathname name** n. 一个字符串[string], nil, :wild, :unspecific, 或某个被具体实现[implementation]定义为有效路径名名称[valid pathname name]的其他对象[object].

**valid pathname type** n. 一个字符串[string], nil, :wild, :unspecific.

**valid pathname version** n. 一个非负整数[integer], 或 :wild、 :newest、:unspecific 或 nil 其中之一. 符号 :oldest, :previous, 和 :installed 是不完全标准[semi-standard]特殊版本符号.

**valid physical pathname host** n. 一个字符串[string]、一个字符串[string]列表[list]或符号 :unspecific 中的任意一个, 它被具体实现识别为一个主机的名称.

**valid sequence index** n. (指一个序列[sequence]) 适用于命名该序列[sequence]的一个元素[element]的整数[integer]. 这样一个整数[integer]一定大于等于零, 并且已经小于这个序列[sequence]的长度[length]. (如果这个序列[sequence]是一个数组[array], 那么这个有效序列索引[valid sequence index]被进一步认为是一个 fixnum.)

**value** n. 1. a. 可能是求值[evaluation]结果的几个对象[object]之一. b. (在一个表达式形式[form]的求值[evaluation]只需要一个值[value]的情况下) 由这个表达式形式[form]返回的主值[primary value]. c. (指一个隐式 progn [implicit progn]中的表达式形式[form]) 最后一个表达式形式[form]的求值[evaluation]可能产生的几个对象[object]之一, 或者如果没有表达式形式[form]那么就是 nil. 2. 和一个绑定[binding]中的名字[name]关联的对象[object]. 3. (指一个符号[symbol]) 由这个符号命名的动态变量[dynamic variable]的值[value]. 4. 和一个关联列表[association list]、一个属性列表[property list]或一个哈希表[hash table]中的一个键[key]关联的一个对象[object].

**value cell** n. Trad. (指一个符号[symbol]) 持有这个符号[symbol]命名的动态变量[dynamic variable]的值[value] (如果有的话)的位置, 并且它可以通过 symbol-value 来访问. 见存储格[cell].

**variable** n. 在 "变量(variable)" 命名空间[namespace]的一个绑定[binding]. 见章节 3.1.2.1.1 (符号表达式形式).

**vector** n. 一个一维数组[array].

**vertical-bar** n. 标准字符[standard character], 它被称为 "竖杠(vertical bar)" (|). 见 Figure 2-5. 


### W

**whitespace** n. 1. 一个或多个是图形[graphic]字符[character] #\Space 或是例如 #\Newline 这样只移动打印位置的非图形[non-graphic]字符的字符[character]. 2. a. n. 一个是标记[token]分隔符的字符[character]的语法类型[syntax type]. 关于详情, 见章节 2.1.4.7 (空白字符). b. adj. (一个字符[character]的) 有着空白[whitespace[2a]]语法类型[syntax type[2]]. c. n. 一个空白[whitespace[2b]]字符[character].

**wild** adj. 1. (指一个名称字符串[namestring]) 使用一个具体实现定义的[implementation-defined]用于命名文件的语法, 它可能 "匹配" 几个可能的文件名[filename]中的任意一个, 因此它可以用来引用由这些文件名[filename]命名的文件[file]的聚合体. 2. (指一个路径名[pathname]) 一个名称的结构化表示, 它可能 "匹配" 几个可能的路径名[pathname]中的任何一个, 因此它可以用来引用由这些路径名[pathname]命名的文件[file]的聚合体. 这个通配符[wild]路径名[pathname]的集合包括单不限于: 具有成员为 :wild 的路径名[pathname], 或者具有包含 :wild or :wild-inferors 的目录成员的路径名[pathname]. 见函数[function] wild-pathname-p.

**write** v.t. 1. (一个绑定[binding]或槽[slot]或成员) 去修改这个绑定[binding]或槽[slot]的值[value]. 2. (到一个流[stream]的一个对象[object]) 把这个对象[object]的表示输出到这个流[stream].

**writer** n. 一个函数[function], 它写入[writes[1]]一个变量[variable]或槽[slot]. 


### Y

**yield** v.t. (值[value]) 产生值[value]作为求值[evaluation]的结果. "表达式形式 (+ 2 3) 产生 5 (The form (+ 2 3) yields 5)".


