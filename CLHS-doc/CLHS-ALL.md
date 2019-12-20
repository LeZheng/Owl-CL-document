# 1. 引言

> * 1.1 [适用范围, 目的, 和历史](#ScopePurposeAndHistory)
> * 1.2 [文档的组织结构](#OrganizationOfTheDocument)
> * 1.3 [参考的出版物](#ReferencedPublications)
> * 1.4 [定义](#Definitions)
> * 1.5 [规范性](#Conformance)
> * 1.6 [语言的扩展](#LanguageExtensions)
> * 1.7 [语言的子集](#LanguageSubsets)
> * 1.8 [弃用的语言特性](#DeprecatedLanguageFeatures)
> * 1.9 [COMMON-LISP 包中的符号](#SymbolsInTheCOMMON-LISPPackage)

## 1.1 <span id = "ScopePurposeAndHistory">适用范围, 目的, 和历史</span>

### 1.1.1 适用范围和目的

这个文档中提出的规范旨在促进 Common Lisp 程序在各种数据处理系统之间的移植性. 这是一份面向实现者和博学的程序员的语言说明书. 它既不是教程也不是一个实现指南.

### 1.1.2 历史

Lisp 是一个有悠久历史的语言家族. 早期的 Lisp 中的关键思想是 John McCarthy 在 1956 Dartmouth 人工智能夏季研究项目中开发出来的. McCarthy 的动机是去开发一个用于人工智能工作的代数列表处理语言. 早期 Lisp 方言的实现着手于 IBM 704, IBM 7090, Digital Equipment Corporation (DEC) PDP-1,  DEC PDP-6, 以及 PDP-10. 在 1960 到 1965 主要的 Lisp 方言是 Lisp 1.5. 直到20世纪70年代早期出现了两个得益于早先努力占主导地位的 Lisp 方言: MacLisp 和 Interlisp. 关于早期Lisp方言的更多信息, 见 《The Anatomy of Lisp》 或者 《Lisp 1.5 Programmer's Manual》.

MacLisp 对 Lisp 1.5 特殊变量和错误处理进行改进. MacLisp 也引入了可以接受可变数量参数的函数, 宏, 数组, 非局部动态退出, 快速计算, 第一个好的 Lisp 编译器, 还有突出的运行速度. 直到20世纪70年代末, MacLisp 被用于超过 50 个网站. 关于 Maclisp 的进一步信息, 见《Maclisp Reference Manual, Revision 0》或《The Revised Maclisp Manual》.

Interlisp 提出了很多关于 Lisp 编程环境和方法论的理念. 其中一个影响了 Common Lisp 的理念就是 Warren Teitelman 实现的循环结构, 这个启发了后面 Lisp Machines 和 MacLisp, 以及现在的 Common Lisp 的 loop 宏. 关于 Interlisp 的进一步信息, 见《Interlisp Reference Manual》.

虽然第一个 Lisp 实现是在 IBM 704 和 IBM 7090 上, 后来的工作聚焦于 DEC PDP-6 以及后来的 PDP-10 计算机, 再后来从20世纪60年代中期到70年代的大部分时间成为 Lisp 和人工智能工作的支柱的是像 Massachusetts Institute of Technology (MIT), Stanford University, 还有 Carnegie Mellon University (CMU) 这样的地方. 这个 PDP-10 计算机和它的前辈 PDP-6 计算机被设计为特别适合于Lisp因为它们有36位的字以及18位的地址. 这个结构允许一个 cons 序对存储在一个字里; 单个指令可以解出 car 和 cdr 部分. 这个 PDP-6 和 PDP-10 拥有快速强大的栈指令可以快速的进行函数调用. 但是到1973年 PDP-10 的局限也是明显的: 它支持很少数量的研究者使用Lisp, 并且这个小的 18 位地址空间 (2^18 = 262,144 字) 限制了单个程序大小. 对于地址空间问题的一个解决方案是 Lisp Machine, 一种特殊目的计算机被设计用于运行Lisp程序. 另外一种回答就是使用带有更大地址空间的通用目的计算机, 就像 DEC VAX 还有S-1 Mark IIA. 关于 S-1 Common Lisp 的更多信息, 见 "S-1 Common Lisp Implementation".

这个 Lisp machine 概念发展于20世纪60年代末期. 在20世纪70年代早期, 和 Daniel Bobrow 一起工作的 Peter Deutsch 在个人迷你计算机 Alto 上实现了一个Lisp, 使用微代码来解释一个字节码实现语言. 在那以后不久,在 MIT 的 Richard Greenblatt 开始着手一个不同的硬件和指令集的设计. 虽然作为 Lisp machine 来说 Alto 并不是完全的成功, 但是在 Xerox 开发的 D-series 机器上一个 Interlisp 的方言 Interlisp-D 变得可用. 在早期的 MIT Lisp Machines 上一个称为 Lisp Machine Lisp 的向上兼容的 MacLisp 延伸变得可用. 到了1981年, 来自于 Xerox, Lisp Machines (LMI), 和 Symbolics 的商用 Lisp Machine 出现在市场上. 关于 Lisp Machine Lisp 的更多信息, 见《Lisp Machine Manual》.

在20世纪70年代末期, Lisp Machine Lisp 开始朝向一个更完整的语言发展. 久经考验的 lambda 列表, setf, 多值, 以及像 Common Lisp 中的结构体是 Lisp Machine 组的早期编程风格试验的结果. Jonl White 还有其他人将这些特点移植到 MacLisp. 大约 1980, Scott Fahlman 和在 CMU 的其他人开始致力于一个运行在 Scientific Personal Integrated Computing Environment (SPICE) 工作站的 Lisp. 这个工程的其中一个目标就是去设计一个比 Lisp Machine Lisp 更简单的方言.

MIT 的 Macsyma 小组在20世纪70年代末期开始了一个项目, 称为 New Implementation of Lisp (NIL) for the VAX, 由 White 领导. 这个 NIL 工程的其中一个定期目标是在保留和 MacLisp 的兼容性的同时去修复一些 Lisp 的历史遗留问题. 同时, 一个 Stanford University 的研究小组以及 Richard P. Gabriel 领导的劳伦斯利物莫国家实验室开始了运行于 S-1 Mark IIA 超级计算机之上的 Lisp. S-1 Lisp 的设计, 它具有不完全的功能, 是为 Lisp 的具体实现适配先进的编译器技术的测试点. 最终这个 S-1 和 NIL 小组合作. 关于 NIL 工程的更多信息, 见 "NIL---A Perspective".

第一个朝向 Lisp 标准化的尝试是在1969年, 那时 Anthony Hearn 和 University of Utah 的 Martin Griss 定义了标准 Lisp ---是一个 Lisp 1.5 和其他方言的子集---来迁移 REDUCE, 一个符号化的代数系统. 在20世纪70年代, 这个 Utah 小组首先为标准 Lisp 实现了一个可重定向编译的优化编译器, 并且还有一个被认为是Portable Standard Lisp (PSL) 的扩展实现. 直到20世纪80年代中期, PSL 运行在十多种计算机上. 关于标准 Lisp 的更多信息, 见 "Standard LISP Report".

PSL 和 Franz Lisp ---是 Unix 机器上的类 MacLisp 方言---是第一个在多种硬件平台广泛可用的 Lisp 方言的例子.

Lisp 的其中一个最重要的发展是在20世纪70年代后半期: Scheme. Scheme, 是 Gerald J. Sussman 和 Guy L. Steele Jr 设计, 是一个简洁的 Lisp 方言, 从20世纪60年代以来这里面的设计带给 Lisp 很多编程语言语义上的理念. Sussman 是20世纪60年代末到70年代很多 Lisp 技术其他进展背后主要创新点之一. Scheme 主要的贡献是词法作用域, 词法闭包, 一阶连续性(first-class continuations), 还有简单的语法 (没有区分值 cell 和 函数 cell). 这些贡献中很多对Common Lisp的设计产生了很大影响. 关于Scheme的更多信息, 见《IEEE Standard for the Scheme Programming Language》或者 "Revised^3 Report on the Algorithmic Language Scheme".

在20世纪70年代末期,面向对象编程概念对 Lisp 产生了很大影响. 在 MIT, 来自于 Smalltalk 的思想径直进入很多广泛使用的编程系统中. Flavors, 一个带有多继承的面向对象系统, 为了 Lisp machine 社区在 MIT 被 Howard Cannon 和其他人开发出来. 在 Xerox, Smalltalk 的经验和 Knowledge Representation Language (KRL) 引导了 Lisp Object Oriented Programming System (LOOPS) 还有后来的 Common LOOPS 的发展. 关于 Smalltalk 的进一步的信息, 见《Smalltalk-80: The Language and its Implementation》. 关于 Flavors 更多信息, 见《Flavors: A Non-Hierarchical Approach to Object-Oriented Programming》.

这些系统影响了 Common Lisp Object System (CLOS) 的设计. CLOS 是为了这个标准化工作特地开发的, 并且详细记载在了 "Common Lisp Object System Specification". 然而, 在公布之后它的设计中小部分细节有了轻微的改变, 并且就像这个文档中所说, 这个文章不应该被当作一份这个对象系统语义上的官方说明.

在 1980 年 Symbolics 和 LMI 开发 Lisp Machine Lisp; stock-hardware 实现小组开发 NIL, Franz Lisp, 还有 PSL; Xerox 开发 Interlisp; 还有 CMU 的 SPICE 项目开发称之为 SpiceLisp 的类 MacLisp 方言.

在1981年四月, 在一个 DARPA 赞助的会议后, 分裂的 Lisp 社区, Symbolics, SPICE 项目, NIL 项目, 还有 S-1 Lisp 项目一起加入来定义 Common Lisp. 开始由 White 和 Gabriel 带领, 这个基层工作背后的驱动力由 Fahlman, Daniel Weinreb, David Moon, Steele, 和 Gabriel 提供. Common Lisp 被设计为一个家族语言的描述. 对于 Common Lisp 主要的影响是 Lisp Machine Lisp, MacLisp, NIL, S-1 Lisp, Spice Lisp, 还有 Scheme. 《Common Lisp: The Language》是这个设计的描述. 部分地方它的语义是有意未指定的, 因为那里感觉严格规范会限制 Common Lisp 的研究和使用.

在1986年 X3J13 被组织为一个技术小组去起草 ANSI Common Lisp 标准. 由于 Common Lisp 的接纳, 这个小组的目标与最初的设计者不同. 这些目标包括更严格的移植性标准化, 一个面向对象编程系统, 一个状态系统, 循环机制, 以及一种处理大型字符集的方式. 为了促成这个目标, 一个新的语言说明(这个文档)被开发出来.

## 1.2 <span id = "OrganizationOfTheDocument">文档的组织结构</span>

这是一份参考文档, 不是一份教程文档. 为了尽可能方便, 这个报告的顺序是原始主题在前, 构建于它们之上的在后; 然而, 线性的可读性并不是值得优先考虑的.

这个文档根据主题来拆分章节. 任何给定章节可能包含概念性内容或字典条目, 或者都有.

一个章节中的字典部分把主题相关的已定义的名字[defined name]分组后放得接近一些. 很多这样的分组可能并且不应该从分组中推断出的深层意义, 都是表面意思. 为了看到以字母顺序排列分组的已定义的名字[defined name], 可以查阅索引. 关于已定义名字[defined name]的完整列表, 见章节 1.9 (COMMON-LISP 包中的符号).

为了补偿这份文档的无序部分, 提供一份词汇表; 见章节 26 (术语). 这个词汇表通过快速访问术语的定义来提供连通性,并且一些情况下提供示例或者和另外概念的交叉引用.

关于这个文档中符号规约的内容, 见章节 1.4 (定义).

关于规范性的内容, 见章节 1.5 (规范性).

关于扩展和子集的内容, 见章节 1.6 (语言的扩展) 和章节 1.7 (语言的子集).

关于这个语言的程序[program]如何被 Lisp 读取器[Lisp reader]解析, 见章节 2 (语法).

关于这个语言的程序[program]如何被编译和执行, 见章节 3 (编译和求值).

关于数据类型的内容, 见章节 4 (类型和类). 不是所有的类型[type]和类(class)都被定义在这个章节; 很多定义在它们主题对应的章节--比如, 数值类型定义在了章节 12 (数字). 关于标准化[standardized]类型[type]的完整列表, 见 Figure 4-2.

关于常用目的控制和数据流, 见章节 5 (数据和控制流) 或章节 6 (循环).

## 1.3 <span id = "ReferencedPublications">参考的出版物</span>

* The Anatomy of Lisp, John Allen, McGraw-Hill, Inc., 1978.

* The Art of Computer Programming, Volume 3, Donald E. Knuth, Addison-Wesley Company (Reading, MA), 1973.

* The Art of the Metaobject Protocol, Kiczales et al., MIT Press (Cambridge, MA), 1991.

* "Common Lisp Object System Specification", D. Bobrow, L. DiMichiel, R.P. Gabriel, S. Keene, G. Kiczales, D. Moon, SIGPLAN Notices V23, September, 1988.

* Common Lisp: The Language, Guy L. Steele Jr., Digital Press (Burlington, MA), 1984.

* Common Lisp: The Language, Second Edition, Guy L. Steele Jr., Digital Press (Bedford, MA), 1990.

* Exceptional Situations in Lisp, Kent M. Pitman, Proceedings of the First European Conference on the Practical Application of LISP (EUROPAL '90), Churchill College, Cambridge, England, March 27-29, 1990.

* Flavors: A Non-Hierarchical Approach to Object-Oriented Programming, Howard I. Cannon, 1982.

* IEEE Standard for Binary Floating-Point Arithmetic, ANSI/IEEE Std 754-1985, Institute of Electrical and Electronics Engineers, Inc. (New York), 1985.

* IEEE Standard for the Scheme Programming Language, IEEE Std 1178-1990, Institute of Electrical and Electronic Engineers, Inc. (New York), 1991.

* Interlisp Reference Manual, Third Revision, Teitelman, Warren, et al, Xerox Palo Alto Research Center (Palo Alto, CA), 1978.

* ISO 6937/2, Information processing---Coded character sets for text communication---Part 2: Latin alphabetic and non-alphabetic graphic characters, ISO, 1983.

* Lisp 1.5 Programmer's Manual, John McCarthy, MIT Press (Cambridge, MA), August, 1962.

* Lisp Machine Manual, D.L. Weinreb and D.A. Moon, Artificial Intelligence Laboratory, MIT (Cambridge, MA), July, 1981.

* Maclisp Reference Manual, Revision 0, David A. Moon, Project MAC (Laboratory for Computer Science), MIT (Cambridge, MA), March, 1974.

* "NIL---A Perspective", JonL White, Macsyma User's Conference, 1979.

* Performance and Evaluation of Lisp Programs, Richard P. Gabriel, MIT Press (Cambridge, MA), 1985.

* "Principal Values and Branch Cuts in Complex APL", Paul Penfield Jr., APL 81 Conference Proceedings, ACM SIGAPL (San Francisco, September 1981), 248-256. Proceedings published as APL Quote Quad 12, 1 (September 1981).

* The Revised Maclisp Manual, Kent M. Pitman, Technical Report 295, Laboratory for Computer Science, MIT (Cambridge, MA), May 1983.

* "Revised^3 Report on the Algorithmic Language Scheme", Jonathan Rees and William Clinger (editors), SIGPLAN Notices V21, #12, December, 1986.

* "S-1 Common Lisp Implementation", R.A. Brooks, R.P. Gabriel, and G.L. Steele, Conference Record of the 1982 ACM Symposium on Lisp and Functional Programming, 108-113, 1982.

* Smalltalk-80: The Language and its Implementation, A. Goldberg and D. Robson, Addison-Wesley Company, 1983.

* "Standard LISP Report", J.B. Marti, A.C. Hearn, M.L. Griss, and C. Griss, SIGPLAN Notices V14, #10, October, 1979.

* Webster's Third New International Dictionary the English Language, Unabridged, Merriam Webster (Springfield, MA), 1986.

* XP: A Common Lisp Pretty Printing System, R.C. Waters, Memo 1102a, Artificial Intelligence Laboratory, MIT (Cambridge, MA), September 1989.

## 1.4 <span id = "Definitions">定义</span>

这个章节中包含了这个手册里的符号规约和术语的定义.

> * 1.4.1 [符号规约](#NotationalConventions)
> * 1.4.2 [错误术语](#ErrorTerminology)
> * 1.4.3 [本标准未正式规定的部分](#SectionsNotFormallyPartOfThisStandard)
> * 1.4.4 [解释字典条目](#InterpretingDictionaryEntries)

### 1.4.1 <span id = "NotationalConventions">符号规约</span>

下面的符号规约适用于整个文档.

> * 1.4.1.1 [字体的线索](#FontKey)
> * 1.4.1.2 [修改后的 BNF 语法](#ModifiedBNFSyntax)
> * 1.4.1.3 [特殊符号](#SpecialSymbols)
> * 1.4.1.4 [带有多种表示的对象](#ObjectsWithMultipleNotations)
> * 1.4.1.5 [标识符](#Designators)
> * 1.4.1.6 [无意义的单词](#NonsenseWords)

#### 1.4.1.1 <span id = "FontKey">字体的线索</span>(注意：这个字体在翻译中未体现出来)

在这个文档中使用字体来传递信息.

<u>*name*</u>

    表示定义在术语表中的正式的术语. 如果使用了这个字体, 那么这个术语表中定义优先于正常的英语用法.

    有时候这个术语表的术语会有下标, 就像 "whitespace[2]" (译者注: 这里的单词应该有斜体下划线). 这样的定义从多个术语表定义选择其中一个, 这里是第二个. 这个下标标记法在术语表里通常被使用于那种上下文不足以消除歧义的定义.

<u>*name*</u>

    表示在局部对当前文本引入正式术语. 这里一直有一个对应的术语表条目, 并且通常等价于使用 "name" (译者注: 这里的单词应该有斜体下划线), 但是这使得这个使用更加明显, 可以在一些情况避免读者查阅词汇表.

**name**

    表示一个在 COMMON-LISP 包里的符号. 关于大小写[case]规约的更多内容, 见章节 1.4.1.4.1 (符号案例).

name

    表示一个程序员可能用 Common Lisp 写的简单的名字[name]或代码[code]片段.

    这个字体也被用于某些标准化[standardized]的不是 COMMON-LISP 包的外部符号[external symbol]的名字, 比如关键字[keyword][1], 包[package]名[name], 还有循环关键字[loop keyword].

*name*

    表示一个形参[parameter]或者值[value]的名字.

    有些情况下也使用标记 "<<name>>" (换句话说, 同样的字体, 但是带有 "尖括号" 围绕) 来为包在里面的多个字符提供更好的目视间距. 这些 "尖括号" 是元语法, 并且实际上从不出现在程序的输入输出中.

#### 1.4.1.2 <span id = "ModifiedBNFSyntax">修改后的 BNF 语法</span>

这个规范使用了一个扩展 BNF 范式来描述 Common Lisp 宏表达式形式[macro form]和特殊表达式形式[special form]的语法. 这个章节讨论 BNF 范式的语法.

> * 1.4.1.2.1 [修改后BNF语法的拼接](#SplicingInModifiedBNFSyntax)
> * 1.4.1.2.2 [修改后BNF语法的间接引用](#IndirectionInModifiedBNFSyntax)
> * 1.4.1.2.3 [修改后BNF语法中间接定义的额外使用](#AdditionalUsesForIndirectDef)

##### 1.4.1.2.1 <span id = "SplicingInModifiedBNFSyntax">修改后BNF语法的拼接</span>

使用的主要扩展如下:

```
[[O]]
```

无论何时当一个多元素列表被拼接到一个更大的结构并且元素可以以任何顺序出现的情况下, 就会出现这个形式的表达式. 这个符号 O 表示要被拼接的多个语法元素的语法描述; 这个描述必须以这种形式

```
O1 | ... | Ol
```

其中每一个 Oi 可以是 S 形式或者 S* 形式或者 {S}1 的 形式 . 这个表达式 [[O]] 意味着这样形式的列表

```
(Oi1...Oij) 1<=j
```

被拼接到一个闭合的表达式中, 这样的情况下如果 n /=m 并且 1<=n,m<=j, 就存在 Oin/=Oim 或 Oin = Oim = Qk, 其中 1<=k <=n, Ok 是形式 Qk*. 此外, 对于每一个在结构 {Qk}1 中的 Oin, 这个元素需要出现在拼接后列表的某处.

比如说, 这个表达式

```
(x [[A | B* | C]] y)
```

意味着最多一个 A, 任意数量的 B, 以及最多一个 C 可以以任何顺序出现. 它是以下任意形式的描述:

```Lisp
(x y)
(x B A C y)
(x A B B B B B C y)
(x C B A B B B y)
```

但不是下面这些:

```Lisp
(x B B A A C C y)
(x C B C y)
```

在第一种情况下, 不管 A 还是 C 都出现的太频繁, 而第二个例子中 C 出现得太频繁.

标记法 [[O1 | O2 | ...]]+ 添加额外的限制, 表示至少有一个选项必须被使用. 比如说:

```
(x [[A | B* | C]]+ y)
```

意味着最多一个 A, 任意数量的 B, 并且最多一个 C 以任意顺序出现, 但是在任何情况下至少它们之中的一个选项要满足. 它是以下这些的描述:

```Lisp
(x B y)
(x B A C y)
(x A B B B B B C y)
(x C B A B B B y)
```

但是不是以下这些:

```Lisp
(x y)
(x B B A A C C y)
(x C B C y)
```

在第一个例子中, 没有任何条件满足; 在第二个例子中, A 和 C 都出现得太频繁; 还有在第三个示例中 C 出现得太频繁.

并且, 这个表达式:

```BNF
(x [[{A}1 | {B}1 | C]] y)
```

可以全部表示为这些并且没有其他可能 :

```Lisp
(x A B C y)
(x A C B y)
(x A B y)
(x B A C y)
(x B C A y)
(x B A y)
(x C A B y)
(x C B A y)
```

##### 1.4.1.2.2 <span id = "IndirectionInModifiedBNFSyntax">修改后BNF语法的间接引用</span>

引入一个间接引用的扩展是为了使这种语法可读性更高:

```BNF
O
```

如果 O 不是一个终止符, 它的定义的右边部分会完整地替换表达式 O. 比如, 下面的这个 BNF 表达式等价于上面那个例子:

```BNF
(x [[O]] y)

O::= A | B* | C
```

##### 1.4.1.2.3 <span id = "AdditionalUsesForIndirectDef">修改后BNF语法中间接定义的额外使用</span>

在一些情况下, 一个 BNF 辅助定义可能不会出现在这个 BNF 表达式里, 但是在其他地方是有用的. 比如说, 思考下面的定义:

```BNF
case keyform {normal-clause}* [otherwise-clause] => result*

ccase keyplace {normal-clause}* => result*

ecase keyform {normal-clause}* => result*

normal-clause::= (keys form*)

otherwise-clause::= ({otherwise | t} form*)

clause::= normal-clause | otherwise-clause
```

这里的术语 "clause" 似乎是 "废弃的(dead)" 因为它没有被使用于上面的 BNF 表达式里. 然而, 这个 BNF 的目的并不只是引导解析, 但是也定义有用的术语为了给在后面的描述性文本作参考. 像这个样子, 术语 "clause" 可能出现在后面跟着的文本中, 作为 "normal-clause 或 otherwise-clause" 的速记法.


#### 1.4.1.3 <span id = "SpecialSymbols">特殊符号</span>

这里描述的特殊符号在这个文档中为了概念上的便利, 既不是 Common Lisp 语言也不是它的运行环境的一部分.

=>

    这个表示求值. 比如说:

     (+ 4 5) =>  9

    这个意味着对表达式形式[form] (+ 4 5) 求值的结果是 9.

    如果一个表达式形式[form]返回多值[multiple values], 这些值可能通过空格, 换行或者逗号区分. 比如说:

     (truncate 7 5)
    =>  1 2
     (truncate 7 5)
    =>  1
       2
     (truncate 7 5)
    =>  1, 2

    上面三个表达式的每一个都是等价的, 表示那个 (truncate 7 5) 返回两个值: 1 和 2.

    一些符合规范的实现[conforming implementation]事实上在显示返回值之前打印箭头 (或者一些其他的指示方式), 有一些则没有.

OR=>

    记号 "OR=> " 被用于表示几个可能值的其中一个. 比如

     (char-name #\a)
    =>  NIL
    OR=>  "LOWERCASE-a"
    OR=>  "Small-A"
    OR=>  "LA01"

    表示 nil, "LOWERCASE-a", "Small-A", "LA01" 都是 (char-name #\a) 可能的结果---每一个都有相等的可能性. 除非明确指定, 它不应被认为可能值的集合已经是完整的了. 正常情况下, 上面的例子等价于

     (char-name #\a) =>  implementation-dependent

    但是它旨在提供额外的信息去说明允许实现去发散的一些方式.

NOT=>

    标记 "NOT=> " 用于表示不可能的结果. 这个可能被用于, 比如, 为了强调一种情况, 一些可预见的误解可能引导读者错误地相信这个结果是可能出现的. 比如,

    (function-lambda-expression
        (funcall #'(lambda (x) #'(lambda () x)) nil))
    =>  NIL, true, NIL
    OR=>  (LAMBDA () X), true, NIL
    NOT=>  NIL, false, NIL
    NOT=>  (LAMBDA () X), false, NIL

==

    这个表示代码等价. 比如说:

    (gcd x (gcd y z)) ==  (gcd (gcd x y) z)

    这个意味着对于任意的 x, y, z, 求值表达式 (gcd x (gcd y z)) 的结果和副作用与求值 (gcd (gcd x y) z) 的是一样的.

\>>

    Common Lisp 指定了输入和输出为非交互的流模型. 这个指定了交互式的输入输出流如何映射到非交互的模型的具体详情是具体实现定义的[implementation-defined].

    比如, 允许符合规范的实现[conforming implementation]去区分交互式输入如何中止的问题. 比如, 当最后的定界符被输入到非交互的流中, read 函数[function]中止. 在一些具体实现中, 一个交互式的对 read 的调用在最后的定界符一输入就返回, 甚至那个定界符不是换行符[newline]. 在其他实现[implementation]中, 总是需要一个最后的换行符[newline]. 在其他实现中, 可能会有一个命令去 "激活" 一个满的输入缓冲区, 而这个命令本身在程序输入流中不可见.

    这个文档的例子中, 标记 ">> " 先于交互式输入输出前. 这种方案下, 这个标记表示用户输入.

    比如, 下面的标记

    (+ 1 (print (+ (sqrt (read)) (sqrt (read)))))
    >>  9 16
    >>  7
    =>  8

    展示一个交互, 其中要被求值的是表达式形式[form] "(+ 1 (print (+ (sqrt (read)) (sqrt (read)))))", "9 16 " 是交互是输入, "7" 是交互的输出, 然后 "8" 是求值(evaluation)后产生的值[value].

    这个标记的使用是为了掩盖各个实现[implementation]之间交互式输入输出行为的微小区别.

    有时, 非交互的流模型需要换行符[newline]. 换行符[newline]如何被交互式输入是用户接口的实现定义的[implementation-defined]细节, 但是在那个例子中, 可能会被使用的不是 "<Newline>" 就是 "<NEWLINE>".

     (progn (format t "~&Who? ") (read-line))
    >>  Who? Fred, Mary, and Sally<NEWLINE>
    =>  "Fred, Mary, and Sally", false

#### 1.4.1.4 <span id = "ObjectsWithMultipleNotations">带有多种表示的对象</span>

Common Lisp 中的一些对象[object]不止一种表示的方法. 这种情况下, 选择使用哪一种是随意的, 但是表示一个 "视角(point of view)" 或 "意向感(sense of intent)" 的规约是存在的.

> * 1.4.1.4.1 [符号大小写](#CaseInSymbols)
> * 1.4.1.4.2 [数字](#Numbers)
> * 1.4.1.4.3 [点字符的使用](#DotCharacterUse)
> * 1.4.1.4.4 [NIL](#NIL)

##### 1.4.1.4.1 <span id = "CaseInSymbols">符号大小写</span>

虽然捕捉一个符号[symbol]的过程中大小写[case]是很重要的, 但是 Lisp 读取器[Lisp reader]默认在捕捉前会尝试去规范符号[symbol]; 见章节 23.1.2 (Lisp 读取器上的读取表大小写的影响). 既然如此, 符号的大小写默认是不重要的. 这个文档自始自终, 除了明确指定外, 符号[symbol]的大小写是不重要的; 也就是说, HELLO, Hello, hElLo, 还有 hello 都是表示 "HELLO" 符号的等价方法.

反斜线符号[backslash]和竖线符号[vertical-bar]被用于显式引用大小写[case]还有其他解析相关的字符方面. 即便如此, 标记法 |hello| 和 \h\e\l\l\o 是表示 "hello" 符号的等价方式, 并且明显不同[distinct]于符号 "HELLO".

符号[symbol]对应的 Common Lisp 已定义的名字[defined name]已经大写化[uppercase]了, 即便它们的名字通常以小写[lowercase]的方式出现在文档里.

##### 1.4.1.4.2 <span id = "Numbers">数字</span>

虽然 Common Lisp 为程序提供了很多方式去操纵有理数的输入和输出基数, 但是这个文档中的所有数字是十进制表示的, 除非明确指定外.

##### 1.4.1.4.3 <span id = "DotCharacterUse">点字符的使用</span>

圆点字符在表达式[expression]中单独出现, 例如

    (item1 item2 . tail)

表示那个 tail 对应一个列表结尾的对象[object]列表[list]. 比如,

    (A B C . (D E F))

表示上等价于:

    (A B C D E F)

虽然圆点[dot]是符号的一个合法成分字符, 但是没有标准化的[standardized]符号包含了圆点[dot]字符, 所以这个文档中对句子末尾符号[symbol]的引用后面的句号总是解释为一个句号而不是符号[symbol]名[name]的一部分. 比如, 这个文档中, 就像这样 "This sample sentence refers to the symbol car." 的一个句子引用了一个名字为 "CAR" 的符号(带有三个字母), 不表示4个字母的 "CAR."

##### 1.4.1.4.4 <span id = "NIL">NIL</span>

nil 有很多意思. 它是 COMMON-LISP 包中名字[name]为 "NIL" 的符号[symbol], 也是布尔值[boolean] (而且是广义布尔[generalized boolean]) false, 它是空列表[empty list], 并且它是空类型[empty type]的名字[name] (所有类型[type]的子类型[subtype]).

在 Common Lisp 中, nil 可以表示为 NIL 或者 (). 按照惯例, 这个表示的选择提供了线索去判断它扮演的角色.

| 是否求值? | 表示法 | 通常表示的作用 |
|----------|-------|-------------|
|Yes       |nil    |作为 boolean. |
|Yes       |'nil   |作为符号.      |
|Yes       |'()    |表示空列表     |
|No        |nil    |作为符号或布尔值.|
|No        |()     |作为符号.       |

Figure 1-1. NIL 的表示

仅在这个文档中, nil 有时也用 false 表示来强调它作为布尔值[boolean]的角色.

比如:

```Lisp
(print ())                          ;废止的
(defun three nil 3)                 ;废止的
'(nil nil)                          ;两个符号的列表
'(() ())                            ;空列表的列表
(defun three () 3)                  ;强调空参数列表
(append '() '()) =>  ()             ;强调空列表的使用
(not nil) =>  true                  ;强调作为布尔值 false 的使用
(get 'nil 'color)                   ;强调作为一个符号的使用
```

一个函数[function]在一些情况下被说成 "是 false" 或者 "是 true". 因为看作布尔值[boolean]时没有函数[function]对象等同于 nil 并且所有函数[function]对象[object]都表示true, 所以去说一个函数[function]是 false 是无意义的, 去说它是 true 是无聊的. 这些只是表示函数[function] "返回 false" 或着 "返回 true", 的传统方式.

#### 1.4.1.5 <span id = "Designators">标识符</span>

一个标识符[designator]是一个表示另一个对象[object]的对象[object].

在一个操作符[operator]的形参[parameter]被描述为标识符[designator]的地方, 这个操作符[operator]的描述以假定这个形参[parameter]的值是表示的对象[object]的方式编写; 这就表示, 这个形参[parameter]是表示的类型[type]. (由一个 "<\<type>> 表示符" 或者 "对于 <\<type>> 的一个标识符" 指定的这个对象[object]的具体性质可以在术语表中 "<\<type>> designator" 条目找到.)

比如, "nil" 和 "the value of \*standard-output*" 作为流标识符[stream designator]在操作上是难以区分的. 类似的, 作为字符串标识符[string designator], 符号 foo 和字符串 "FOO" 在操作上也是难以区分的.

除了额外的提示, 在这个指定的对象[object]被多次使用的情况下, 这个对象[object]是只求值一次还是被使用时每次都求值, 依赖于具体实现[implementation-dependent].

比如, mapcar 接受一个函数标识符[function designator]作为参数, 并且它的描述中写的就像它只是个函数. 事实上, 这个函数标识符[function designator]是被马上求值还是在表达式形式的内部在每次被需要的时候求值是依赖于具体实现[implementation-dependent]. 大部分情况下, 符合规范的程序[conforming program]不能检测到其中的区别, 但是也有一些不正常的情况 (尤其是那些包含自身重定义或者相互重定义的函数) 确实符合并且可以检测这个区别. 下面的程序就是一个符合规范的程序[conforming program], 但是可能有或者没有明显正确的结果, 取决于它的正确性是否依赖一个或其他的结果:

```Lisp
 (defun add-some (x)
   (defun add-some (x) (+ x 2))
   (+ x 1)) =>  ADD-SOME
 (mapcar 'add-some '(1 2 3 4))
=>  (2 3 4 5)
OR=>  (2 4 5 6)
```

在一些罕见的情况下, 这里可能有个需要在字典条目中去引用一个参数的最初标识符的对象[object]. 因为对一个参数取名会应用表示的对象, 短语 "the <\<parameter-name>> designator" 可以被用于引用那个标识符, 从它可以计算得到 <\<parameter-name>> 的值.

#### 1.4.1.6 <span id = "NonsenseWords">无意义的单词</span>

当需要一个没有前置关联语义的单词时 (比如, 在一个示例中), 在 Lisp 社区中使用 "foo", "bar", "baz", 和 "quux" 的其中一种是普遍的. 比如

```Lisp
 (defun foo (x) (+ x 1))
```

名字 foo 的使用表示 "请用你喜欢的名字替换这里" 的速记方式.

这些无意义的单词有如此的使用率, 社区的新人开始去思考这里是否有他们忽略的已绑定的语义---当然这里是没有的.

### 1.4.2 <span id = "ErrorTerminology">错误术语</span>

在这个标准中已经描述了错误可能或应该或必然出现的情况. 用于描述这种情况的词旨在有确切的含义. 以下列表是这些意义的词汇表.

安全代码(Safe code)

    这个代码[code]用设置为最高(3) 的 safety 优化来处理. safety 是代码的一个词法属性. 短语 "函数 F 应该发出一个错误(the function F should signal an error)" 意味着如果 F 在代码被最高级 safety 优化的情况下被调用, 会发出一个错误. 是 F 还是调用的代码来发出这个错误是依赖于具体实现的[implementation-dependent].

不安全代码(Unsafe code)

    这个代码是被最低安全级别处理的.

    不安全代码可能会做错误检测. 具体实现允许一直把所有代码视作安全代码.

一个错误被发出(An error is signaled)

    这个意味着在安全的和不安全的代码中都会发出一个错误. 符合规范的代码[conforming code]可以依赖安全和非安全代码中都会发出错误这一事实. 不管是安全的还是非安全的代码, 每一个具体实现需要去检测这个错误. 比如, "如果给定 unexport 一个在当前包不能访问的符号, 那么一个错误就被发出(an error is signaled if unexport is given a symbol not accessible in the current package)".

    如果没有指定明确的错误类型, 默认是 error.

一个错误应该被发出(An error should be signaled)

    这个意味着一个错误在安全的代码中发出, 并且一个错误可能在不安全的代码中发出. 符合规范的代码[conforming code]可以依赖错误会在安全代码中发出这一事实. 每一个实现至少需要在安全的代码中去检测这个错误. 当这个错误没有发出, 那么 "后果是未定义的(consequences are undefined)" (见下方). 比如, "如果任何参数都不是 number 类型, 那么 + 应该发出一个 type-error 类型的错误(+ should signal an error of type type-error if any argument is not of type number)".

应该准备去发出一个错误(Should be prepared to signal an error)

    这个类似于 "应该被发出", 但它并不意味着如果操作符[operator]的正常操作只能通过'惰性'检查成功地执行, 那么操作符[operator]就必须采取"额外的努力"来发现错误的情况. 一个实现[implementation]总是允许去发出一个错误, 但是即使在安全[safe]代码[code]里, 只在没有发出可能导致不正确的结果时需要发出这个错误. 在不安全[unsafe]代码[code]中, 后果是未定义的.

    比如, 定义 "如果 find 的第二个参数不是一个正常列表, 那么它应该准备去发出一个 type-error 类型的错误(find should be prepared to signal an error of type type-error if its second argument is not a proper list)" 不表示一定会发出一个错误. 这个

    (find 'a '(a b . c))

    表达式形式[form]必须在安全[safe]代码[code]中发出一个 type-error 类型[type]的错误, 或者返回 A. 在不安全[unsafe]代码[code]中, 后果是未定义的. 相比之下,

    (find 'd '(a b . c))

    必须在安全[safe]代码[code]中发出一个 type-error 类型的错误. 在不安全[unsafe]代码[code]中, 后果是未定义的. 同样的

    (find 'd '#1=(a b . #1#))

    在安全[safe]代码[code]中可能返回 nil (作为一个具体实现定义[implementation-defined]的扩展), 可能从来不返回, 或者发出一个 type-error 类型[type]的错误. 安全[safe]代码[code]中, 后果是未定义的.

    通常, 这个 "should be prepared to signal" 术语被用于类型检测的情况, 这个时候存在效率考虑, 使得检测与操作符[operator]的正确操作无关的错误是不切实际的.

结果是未指定的(The consequences are unspecified)

    这个意味着结果是不可预测但是无害的. 具体实现允许指定这种情况的结果. 符合规范的代码[conforming code]不能依赖这种情况的结果或影响, 并且所有的符合规范的代码[conforming code]需要把这种情况的结果和影响认为是不可预测的但是无害的. 比如, "如果给 shared-initialize 的第二个参数指定一个不对应于对象中的任何一个可访问槽的名字, 那么结果是未指定的(if the second argument to shared-initialize specifies a name that does not correspond to any slots accessible in the object, the results are unspecified)".

后果是未定义的(The consequences are undefined)

    这个意味着结果是不可预测的. 结果可能是无害的或者致命的. 没有符合规范的代码[conforming code]可能依赖这个结果或影响. 符合规范的代码[conforming code]必须把这个结果当作不可预测的. 在 "must", "must not", 或者 "may not" 词语被使用的地方, 如果没有看到规定的需求或者没有明确规定特定的结果, 就会有 "后果是未定义的".

    比如: "一旦一个名字已经被 defconstant 声明为常量, 任何进一步赋值或绑定那个变量有着未定义的后果(Once a name has been declared by defconstant to be constant, any further assignment or binding of that variable has undefined consequences)".

一个错误可能被发出(An error might be signaled)

    这个意味有着存在未定义的结果; 然而, 如果一个错误被发出, 它就是指定的类型[type]. 比如, "open 可能发出一个 file-error 类型的错误(open might signal an error of type file-error)".

返回值是未指定的(The return values are unspecified)

    这个意味着一个表达式形式[form]的返回值数量和性质没有指定. 然而, 副作用和控制转移的发生与否是指定好的.

    一个程序可以是指定好的, 即便它使用了没有指定返回值的函数. 比如, 即便函数 F 的返回值没有指定, 就像 (length (list (F))) 表达式也是指定好的因为它不依赖任何 F 返回值的特定方面.

具体实现可能去扩展来覆盖这个情况(Implementations may be extended to cover this situation)

    这个意味着这种情况[situation]存在未定义的结果; 然而, 一个符合规范的实现[conforming implementation]可以用一种更加具体的方式去对待这种情况. 比如, 一个具体实现[implementation]可能发出一个错误, 或者应该发出一个错误, 或者甚至出现一个定义好的非错误行为.

    没有符合规范的代码[conforming code]可能依赖这个情况[situation]的结果; 所有符合规范的代码[conforming code]必须把这种情况的结果当作是未定义的. 具体实现需要用文档记录如何对待这种情况.

    比如, "具体实现可以扩展来定义其他类型指定符来持有一个对应类(implementations may be extended to define other type specifiers to have a corresponding class)".

具体实现可以自由地去扩展这个语法(Implementations are free to extend the syntax)

    这意味着在这种情况下, 实现允许去为被描述的表达式形式[form]的语法定义清楚的扩展. 没有符合规范的代码[conforming code]可以依赖这个扩展. 所有具体实现[implementation]需要去用文档记录每一个这样的扩展. 所有符合规范的代码[conforming code]需要去把这个语法当作是无意义的. 标准可能禁止一些扩展而允许其他的. 比如, "没有具体实现可以自由地去扩展 defclass 的语法".

一个警告可能被发出(A warning might be issued)

    这意味着在适当上下文中 (比如, 编译的时候), 鼓励具体实现[implementation]去发出一个警告的. 然而, 一个符合规范的实现[conforming implementation]不需要发出一个警告.


### 1.4.3 <span id = "SectionsNotFormallyPartOfThisStandard">本标准未正式规定的部分</span>

这个标准的前页和书后的附属资料, 就像 "Table of Contents", "Index", "Figures", "Credits", 和 "Appendix" 不是这个标准的正规考虑的部分, 所以我们保留更新这些部分所需要的灵活性, 以便于即使在最后一刻也不需要担心修改文档的这些部分需要正式的投票. 这些条目很短并且很有用, 然而, 不推荐在这个文档的删减版本里把它们删除.

在概念性的章节里, 提供的名字以单词 "注意(Note)" 或 "注意(Notes)" 或 "例如(Example)" 或 "例如(Examples)" 开头的部分只用于说明目的, 并且不被认为是标准的一部分.

已经试着把这些章节放在父章节的末尾, 这样它们就可以被移除而不需要修改相邻章节的序号来减少文档的大小.

同样的, 字典条目中的这个 "示例(Examples)" 和 "注意(Notes)" 部分也不被认为是标准的一部分, 如有必要可以移除.

不过, 这些示例对剩下的部分提供了重要的说明和规范性检查, 这样的删减是不推荐的, 除非完全无法避免.

### 1.4.4 <span id = "InterpretingDictionaryEntries">解释字典条目</span>

每个已定义的名字[defined name]的字典条目被划分为几个部分. 除非明确表示, 每个部分由确定这个部分的标签引入. 该部分的遗漏表示这个章节是不可应用的或者没有提供什么有意思的信息.

这个章节定义了每个可能出现在字典章节的条目的意义.

> * 1.4.4.1 ["受此影响(Affected By)" 字典条目部分](#AffectedBySDE)
> * 1.4.4.2 ["参数(Arguments)" 字典条目部分](#ArgumentsSDE)
> * 1.4.4.3 ["参数和值(Arguments and Values)" 字典条目部分](#ArgumentsAndValuesSDE)
> * 1.4.4.4 ["绑定类型的影响(Binding Types Affected)" 字典条目部分](#BindingTypesAffectedSDE)
> * 1.4.4.5 ["类优先级列表(Class Precedence List)" 字典条目部分](#ClassPrecedenceListSDE)
> * 1.4.4.6 [类型标识符的字典条目](#DictionaryEntriesForTS)
> * 1.4.4.7 ["常量值(Constant Value)" 字典条目部分](#ConstantValue)
> * 1.4.4.8 ["描述(Description)" 字典条目部分](#DescriptionSDE)
> * 1.4.4.9 ["示例(Examples)" 字典条目部分](#ExamplesSDE)
> * 1.4.4.10 ["异常情况(Exceptional Situations)" 字典条目部分](#ExceptionalSituationsSED)
> * 1.4.4.11 ["初始值(Initial Value)" 字典条目部分](#InitialValueSDE)
> * 1.4.4.12 ["参数优先级顺序(Argument Precedence Order)" 字典条目部分](#ArgumentPrecedenceOrderSDE)
> * 1.4.4.13 ["方法签名(Method Signature)" 字典条目部分](#MethodSignatureSDE)
> * 1.4.4.14 ["名称(Name)" 字典条目部分](#NameSDE)
> * 1.4.4.15 ["注意(Notes)" 字典条目部分](#NotesSDE)
> * 1.4.4.16 ["发音(Pronunciation)" 字典条目部分](#PronunciationSDE)
> * 1.4.4.17 ["参见(See Also)" 字典条目部分](#SeeAlsoSDE)
> * 1.4.4.18 ["副作用(Side Effects)" 字典条目部分](#SideEffectsSDE)
> * 1.4.4.19 ["超类型(Supertypes)" 字典条目部分](#SupertypesSDE)
> * 1.4.4.20 ["语法(Syntax)" 字典条目部分](#SyntaxSDE)
> * 1.4.4.21 ["合法上下文(Valid Context)" 字典条目部分](#ValidContextSDE)
> * 1.4.4.22 ["值类型(Value Type)" 字典条目部分](#ValueTypeSDE)

#### 1.4.4.1 <span id = "AffectedBySDE">"受此影响(Affected By)" 字典条目部分</span>

对于一个操作符[operator], 就是任何可以影响这个操作符[operator]的副作用或者返回的值[value]的事物.

对于一个变量[variable], 就是任何可以影响这个变量[variable]的值[value]的事物, 这个值包括绑定或赋值给这个变量的函数[function].

#### 1.4.4.2 <span id = "ArgumentsSDE">"参数(Arguments)" 字典条目部分</span>

这个信息描述那些声明[declaration]还有不求值为表达式形式[form]并且求值也不返回值[value]的特殊表达式[expression]条目的语法信息.

#### 1.4.4.3 <span id = "ArgumentsAndValuesSDE">"参数和值(Arguments and Values)" 字典条目部分</span>

这是对操作符[operator]接收实参[argument]还有返回的值[value]的英语描述(译者注: 这里的英语对应到本译文是中文), 包括可遗漏实参[argument]的默认形参[parameter]的信息 (例如可选参数[optional parameter]和关键字参数[keyword parameter]). 对于特殊操作符[special operator]和宏[macro], 它们的实参[argument]不会被求值, 除非在它们的描述中就明确指定它们被求值.

除了显式指明以外, 如果这些类型约束被违反了, 那么结果将是未定义的.

#### 1.4.4.4 <span id = "BindingTypesAffectedSDE">"绑定类型的影响(Binding Types Affected)" 字典条目部分</span>

这个信息警告读者这个类型的绑定[binding]可能被一个声明所影响. 事实上任何这样的特定绑定[binding]是否被影响取决于其他因素. 详情见 "描述(Description)" 部分中问题的声明.

#### 1.4.4.5 <span id = "ClassPrecedenceListSDE">"类优先级列表(Class Precedence List)" 字典条目部分</span>

这个出现在类[class]的字典条目, 并且包含由 Common Lisp 定义的类[class]的一个有序列表, 这个列表一定在这个类[class]的优先级列表[class precedence list]中.

允许其他类[class] (具体实现定义的[implementation-defined]) 出现在具体实现的这个类[class]的优先级列表[class precedence list]中.

允许标准对象[standard-object]或者结构体对象[structure-object]出现在具体实现[implementation]的类优先级列表[class precedence list]中; 详情见章节 4.2.2 (类型关系).

除非在这个说明书里明确说明, 没有标准化[standardized]的类[class]出现在具体实现的类优先级列表[class precedence list]中.

根据类和类型之间的关系定义, 这部分列出来的类[class]也是这个类[class]表示的类型[type]的超类型[supertype].

#### 1.4.4.6 <span id = "DictionaryEntriesForTS">类型标识符的字典条目</span>

这个原子类型指定符[atomic type specifier]是那些 Figure 4-2 列出的已定义的名字[defined name]. 这些字典条目是 "类(Class)", "状况类型(Condition Type)", "系统类(System Class)", 或者 "类型(Type)" 种类的. 如何去把命名这些类型[type]或类[class]的符号[symbol]解释为原子类型指定符[atomic type specifier]的描述在每个字典条目的 "描述(Description)" 部分.

复合类型指定符[compound type specifiers]是那些 Figure 4-3 列出的已定义的名字[defined name]. 这样的字典条目是 "类(Class)", "系统类(System Class)", "类型(Type)", 或 "类型指定符(Type Specifier)" 类型的. 如何去把一个 car 为这样一个符号[symbol]的列表[list]解释为复合类型指定符[compound type specifiers]的描述可以在这样一个字典条目的 "复合类型指定符种类(Compound Type Specifier Kind)", "复合类型指定符语法(Compound Type Specifier Syntax)", "复合类型指定符参数(Compound Type Specifier Arguments)", 还有 "复合类型指定符描述(Compound Type Specifier Description)" 这些部分中找到.

> * 1.4.4.6.1 [ "复合类型指定符种类(Compound Type Specifier Kind)" 字典条目部分](#CTSKindSDE)
> * 1.4.4.6.2 [ "复合类型指定符语法(Compound Type Specifier Syntax)" 字典条目部分](#CTSSyntaxSDE)
> * 1.4.4.6.3 [ "复合类型指定符参数(Compound Type Specifier Arguments)" 字典条目部分](#CTSArgumentsSDE)
> * 1.4.4.6.4 [ "复合类型指定符描述(Compound Type Specifier Description)" 字典条目部分](#CTSDescriptionSDE)

##### 1.4.4.6.1 <span id = "CTSKindSDE"> "复合类型指定符种类(Compound Type Specifier Kind)" 字典条目部分</span>

一个 "abbreviating" 类型指定符[type specifier]描述了一个原则上可以枚举出这些元素[elements]但是实践上没有可操作性的一个子类型[subtype].

一个 "specializing" 类型指定符[type specifier]通过约束这个类型[type]的一个或多个成员的类型[type], 例如元素类型[element type]或复数部分类型[complex part type], 来描述一个子类型[subtype].

一个 "predicating" 类型指定符[type specifier]描述了一个只包含满足给定断言[predicate]的那些对象[object]的子类型[subtype].

一个 "combining" 类型指定符[type specifier]描述一种以组合的方式的子类型[subtype], 通过在其他类型[type]上使用组合操作符 (例如 "and", "or", 和 "not") .

##### 1.4.4.6.2 <span id = "CTSSyntaxSDE"> "复合类型指定符语法(Compound Type Specifier Syntax)" 字典条目部分</span>

这个关于一个类型[type]的信息描述了这个类型[type]的复合类型指定符[compound type specifier]的语法.

这个类型是否可以作为原子类型指定符[atomic type specifier]在这里没有说明; 见章节 1.4.4.6 (类型标识符的字典条目).

##### 1.4.4.6.3 <span id = "CTSArgumentsSDE"> "复合类型指定符参数(Compound Type Specifier Arguments)" 字典条目部分</span>

这个描述了定义在 "复合类型指定符语法(Compound Type Specifier Syntax)" 部分的结构的类型[type]信息.

##### 1.4.4.6.4 <span id = "CTSDescriptionSDE"> "复合类型指定符描述(Compound Type Specifier Description)" 字典条目部分</span>

这个描述了定义在 "复合类型指定符语法(Compound Type Specifier Syntax)" 部分的结构的意义.

#### 1.4.4.7 <span id = "ConstantValue">"常量值(Constant Value)" 字典条目部分</span>

这个描述了一个常量[constant variable]的不变的类型[type]和值[value].

#### 1.4.4.8 <span id = "DescriptionSDE">"描述(Description)" 字典条目部分</span>

这个操作符[opterator]还有它的所有目的方面的一个总结, 但是没有必要包含下面引用的所有字段("副作用(Side Effects)", "异常情况(Exceptional Situations)", 等等.)

#### 1.4.4.9 <span id = "ExamplesSDE">"示例(Examples)" 字典条目部分</span>

这个操作符[opterator]的使用示例. 这些示例不被认为是这个标准的一部分; 见章节 1.4.3 (本标准未正式规定的部分).

#### 1.4.4.10 <span id = "ExceptionalSituationsSED">"异常情况(Exceptional Situations)" 字典条目部分</span>

三种类型的信息可以出现在这里:

    被这个函数[function]检测到且正常发出来的情况.
    被这个函数[function]处理的情况.
    可能被这个函数[function]检测到的情况.

该字段既不包含传递给这个操作符[operator]作为参数或者作为动态变量由这个操作符[operator]调用的函数[function]所发出的状况, 也不包括当这个操作符[operator]是宏[macro]或特殊操作符[special operator]时, 执行这个操作符的子表达式所发出的状况.

#### 1.4.4.11 <span id = "InitialValueSDE">"初始值(Initial Value)" 字典条目部分</span>

这个信息描述了一个动态变量[dynamic variable]的初始值[value]. 因为这个变量可能会改变, 见 "值类型(Value Type)" 部分的类型限制.

#### 1.4.4.12 <span id = "ArgumentPrecedenceOrderSDE">"参数优先级顺序(Argument Precedence Order)" 字典条目部分</span>

这个信息描述了参数优先级顺序[argument precedence order]. 如果它被省略, 参数的优先级顺序[argument precedence order]就是默认的 (从左到右).

#### 1.4.4.13 <span id = "MethodSignatureSDE">"方法签名(Method Signature)" 字典条目部分</span>

这个广义函数[generic function]的描述包含了在这个标准定义在这个广义函数[generic function]上的的方法[method]描述. 一个方法签名被用于描述每一个方法[method]的形参[parameter]和形参指定符[parameter specializer]. 对于这个广义函数[generic function]定义的方法[method]必须是这个方法[method]的签名[signature]所描述形式.

F (x class) (y t) &optional z &key k

这个签名[signature]表示这个广义函数[generic function] F 的方法有两个必要参数[required parameter]: 必须是类[class] class 的广义实例[generalized instance]的 x; 还有可以是任何对象[object]的 y (换句话说, 可以类 t 的广义实例[generalized instance]). 另外, 这里有一个可选参数[optional parameter] z 和一个关键字参数[keyword parameter] k. 这个签名[signature]也表示这个 F 上的方法是一个主方法[primary method]并且没有限定符[qualifiers].

对于每个形参[parameter], 提供的实参[argument]必须是对应的广义函数描述的类型和某个方法的签名里的类型的交集 (不仅仅是这个规范里定义的方法[method], 也包括在允许定义方法[method]的地方具体实现定义的[ implementation-defined]或者用户定义的方法[method]).

#### 1.4.4.14 <span id = "NameSDE">"名称(Name)" 字典条目部分</span>

这个章节介绍字典条目. 它没有被显式标记. 它出现在一个横线的前面或后面.

在左边打印的大字体是已定义的名字[defined name]; 如果不只一个已定义的名字[defined name]要被这个条目描述, 所有这样的名字[name]都会显示出来, 通过逗号分割.

在右边打印的小的斜体的是这个字典条目是什么种类的表示. 可能的值是:

访问器(Accessor)

    这个是访问器[accessor]函数[function].

类(Class)

    这是一个类[class].

状况类型(Condition Type)

    这个是类型[type] condition 的子类型[subtype].

常量(Constant Variable)

    这是一个常量[constant variable].

声明(Declaration)

    这是一个声明标识符[declaration identifier].

函数(Function)

    这是一个函数[function].

局部函数(Local Function)

    这是一个词法上定义在宏表达式形式[macro form]的作用域里的函数[function].

局部宏(Local Macro)

    这是一个词法上定义在宏表达式形式[macro form]的作用域里的宏[macro].

宏(Macro)

    这是一个宏[macro].

重启动(Restart)

    这是一个重启动[restart].

特殊操作符(Special Operator)

    这是一个特殊操作符[special operator].

标准广义函数(Standard Generic Function)

    这是一个标准广义函数[standard generic function].

符号(Symbol)

    这是在某些特定情况下特别识别的符号[symbol], 例如宏[macro]的语法.

系统类(System Class)

    这就像一个类[class], 但是它可能表示的是内置类[built-in class]. (事实上没有类[class]必须成为内置类[built-in class].)

类型[Type]

    这是一个原子类型指定符[atomic type specifier], 并且依赖于每一个特定条目的信息, 可能会被其他类型指定符[type specifier]所接受.

类型指定符[Type Specifier]

    这是一个不是原子类型指定符[atomic type specifier]的已定义的名字[defined name], 但是可以被用于构建合法类型指定符[type specifier].

变量[Variable]

    这是一个动态变量[dynamic variable].


#### 1.4.4.15 <span id = "NotesSDE">The "注意(Notes)" 字典条目部分</span>

在其他地方没有的关于这个操作符[operator]的信息. 在其他情况中, 这个可能包含了交叉引用信息, 代码等价性, 格式上的暗示, 实现的暗示, 典型使用. 这个信息不被认为是这标准的一部分; 任何符合规范的实现[conforming implementation]和符合规范的程序[conforming program]允许忽略这个信息.

#### 1.4.4.16 <span id = "PronunciationSDE">"发音(Pronunciation)" 字典条目部分</span>

这个为已定义的名字[defined name]提供了一个推荐的发音方式, 这样人们没有在和最初的设计者们一起交流的情况下也能弄明白这个没有出现在正常英语中的单词如何发音. 这个信息是劝告性的, 不被认为是这个标准的一部分. 为了简洁性, 它只提供给带有特定于 Common Lisp 而不会出现在未删减的《Webster's Third New International Dictionary the English Language》中的名字的条目.

#### 1.4.4.17 <span id = "SeeAlsoSDE">"参见(See Also)" 字典条目部分</span>

对这个标准其他部分的引用的列表, 这些引用提供有关这个操作符[operator]信息. 这个列表不是这个标准的一部分.

#### 1.4.4.18 <span id = "SideEffectsSDE">The "副作用(Side Effects)" 字典条目部分</span>

任何因对包含该操作符[operator]的表达式形式[form]的求值而改变的东西.

#### 1.4.4.19 <span id = "SupertypesSDE">The "超类型(Supertypes)" 字典条目部分</span>

这个出现在一个类型[type]的字典条目中, 包含了一个标准化[standardized]类型[type]的列表, 它们必须是这个类型[type]的超类型[supertype].

在具体实现[implementation]中有一个对应类[class]的地方, 在类优先级列表[class precedence list]中这些类[class]的顺序和在这个章节中出现的顺序是一致的.

#### 1.4.4.20 <span id = "SyntaxSDE">"语法(Syntax)" 字典条目部分</span>

这个章节描述了如何在代码中使用已定义的名字[defined name]. 关于广义函数[generic function]的 "语法(Syntax)" 部分描述了广义函数[generic function]自身的 lambda 列表[lambda list], 而 "方法签名(Method Signatures)" 描述了已定义的方法[method]的 lambda 列表[lambda list]. 一个普通函数, 宏, 或者一个特殊操作符的 "语法(Syntax)" 描述叙述了它们的形参[parameter].

比如, 一个操作符[operator]描述可能是:

    F x y &optional z &key k

这个描述表示函数 F 有两个必要参数, x 和 y. 另外, 这里还有个可选参数 z 和一个关键字参数 k.

关于宏[macro]和特殊操作符[special operator], 语法由修改的 BNF 表示给定; 见章节 1.4.1.2 (修改后的 BNF 语法). 对于函数[function]给定一个 lambda 列表[lambda list]. 然而在两种情况下, 最外层的括号和默认值信息省略了.

> * 1.4.4.20.1 [重载操作符特殊的 "语法(Syntax)" 表示](#SpecialSyntaxNotations)
> * 1.4.4.20.2 [剩余参数的命名转化](#NamingConventions)
> * 1.4.4.20.3 [在 "语法(Syntax)" 部分要求非空剩余参数](#RequiringNonNullRestParameters)
> * 1.4.4.20.4 [在 "语法(Syntax)" 部分的返回值](#ReturnValuesInSyntaxSection)

##### 1.4.4.20.1 <span id = "SpecialSyntaxNotations">重载操作符特殊的 "语法(Syntax)" 表示</span>

如果同一个操作符带有不同数量的参数有两个描述, 那么额外的参数就被认为是可选的. 比如, 这两行:

    file-position stream => position

    file-position stream position-spec => success-p

操作上等价于:

    file-position stream &optional position-spec => result

并且不同点仅在于为每一个情况提供一个机会去为形参[parameter]和值[value]引入不同的的名字. 当操作符[operator] 使用形参[parameter]根据提供的实参[argument]数量以不同的方式被使用 (比如, 函数[function] /) 或者两种情况下返回值不同 (比如, 函数 file-position) 的方式重载时, 使用这个分开的 (多行) 表示法.

##### 1.4.4.20.2 <span id = "NamingConventions">剩余参数的命名转化</span>

在这个规范中, 如果选择的一个剩余参数[rest parameter]的名字为复数名词, 在 *parameter* 字体下的这个名字的使用引用了这个剩余参数[rest parameter]被绑定到的列表[list]. 在 *parameter* 字体下的这个名字的单数形式的使用引用了那个列表[list]的一个元素[element].

比如, 给定一个像这样的语法描述:

    F &rest arguments

可以通过名字来引用这些名为 arguments 的剩余参数[rest parameter], 或者通过 "一个 argument", "某个 argument", "每个 argument" 等等来引用其中一个.

##### 1.4.4.20.3 <span id = "RequiringNonNullRestParameters">在 "语法(Syntax)" 部分要求非空剩余参数</span>

在某些情况下, 当需要至少一个参数时, 使用剩余参数[rest parameter]将所有参数都视为单个聚合体是很有用的. 在代码[code]中可以使用各种命令式和声明性方法来表达这样的限制, 但是它们通常不会在 lambda 列表[lambda list]中显示出来. 在这个文档中为了表述目的,

    F &rest arguments+

意味着和下面的相等

    F &rest arguments

但是引入了额外的需要: 这里至少需要一个参数 argument.

##### 1.4.4.20.4 <span id = "ReturnValuesInSyntaxSection">在 "语法(Syntax)" 部分的返回值</span>

一个求值箭头 "=> " 先于要被返回的值[value]的列表. 比如:

    F a b c => x

表示这个 F 是一个操作符, 它需要三个必要参数[required parameter] (换句话说, a, b, 还有 c) 以及返回一个值[value] (就是 x). 如果不止一个值[value]被这个操作符返回, 这些值[value]的名字[name]会用逗号区分, 就像:

    F a b c => x, y, z

###### 1.4.4.20.4.1 在 "Syntax" 部分没有参数和返回值

如果允许没有实参[argument]或者没有返回的值[value], 一个特别的标记被用于使这个更显而易见. 比如,

    F <no arguments> => <no values>

表示这个 F 操作符不接受实参[argument]并且不返回值[value].

###### 1.4.4.20.4.2 在 "Syntax" 部分控制的非条件转移

一些操作符[operator]会有控制无条件转移的动作, 并且没有任何返回值. 这样的操作符[operator]通过以下方式表示:

    F a b c =>|

#### 1.4.4.21 <span id = "ValidContextSDE">"合法上下文(Valid Context)" 字典条目部分</span>

这个信息被用于例如 "声明(Declarations)" 字典条目, 用于约束这个声明可以出现的上下文.

一个给定的 "声明(Declaration)" 可能出现在一个声明[declaration] (换句话说, 一个 declare 表达式[expression]) , 一个全局声明[proclamation] (换句话说, 一个 declaim 或 proclaim 表达式形式[form]), 或者都有.

#### 1.4.4.22 <span id = "ValueTypeSDE">"值类型(Value Type)" 字典条目部分</span>

这个信息描述了一个动态变量[dynamic variable]的任何类型[type]约束条件.

除非明确指定外, 违反这个类型约束时结果是不可预测的.

## 1.5 <span id = "Conformance">规范性</span>

这个标准提出了一个符合规范的具体实现[conforming implementation]需要去实现的语法和语义 (以及它的附加文档). 另外, 它对符合规范的程序[conforming program]加了一些要求.

> * 1.5.1 [符合规范的实现](#ComformingImpl)
> * 1.5.2 [符合规范的程序](#ComformingProg)
 
### 1.5.1 <span id = "ComformingImpl">符合规范的实现</span>

一个符合规范的实现[conforming implementation]应该遵守这个章节中所述的要求.

> * 1.5.1.1 [必要的语言特性](#RequiredLanguageFeatures)
> * 1.5.1.2 [依赖具体实现的特性文档](#DocImplDepFeatures)
> * 1.5.1.3 [扩展文档](#DocExtensions)
> * 1.5.1.4 [异常情况的处理](#TreatmentExceptionalSituations)
> * 1.5.1.5 [规范性声明](#ConformanceStatement)

#### 1.5.1.1 <span id = "RequiredLanguageFeatures">必要的语言特性</span>

一个符合规范的实现[conforming implementation]需要去接受这个标准中指定的所有语言特性 (包括弃用的特性), 并带有这个标准中指定的意义.

一个符合规范的实现[conforming implementation]不需要在代码中包含替代或额外的语言元素, 以完成该标准中指定的一个语言特性. 

#### 1.5.1.2 <span id = "DocImplDepFeatures">依赖具体实现的特性文档</span>

一个符合规范的实现[conforming implementation]应该附加一个文档, 里面提供这个规范定义的语言的所有具体实现定义的[implementation-defined]方面的定义.

另外, 一个符合规范的实现[conforming implementation]鼓励 (但是不是必须) 去记录这个标准中被标注为依赖于具体实现[implementation-dependent]的条目, 尽管一些情况下这个文档可能简单地把这些条目标注为 "undefined". 

#### 1.5.1.3 <span id = "DocExtensions">扩展文档</span>

一个符合规范的实现[conforming implementation]应该附带一个文档分开叙述那些这个标准中没有但是具体实现[implementation]中有的特性, 但是加到这个语言标准中时不应导致任何的歧义和矛盾. 这样的扩展应该被描述为 "由 ANSI <\<standard number>> 指定的 Common Lisp 的扩展". 

#### 1.5.1.4 <span id = "TreatmentExceptionalSituations">异常情况的处理</span>

一个符合规范的实现[conforming implementation]应该和这个规范一致的方式去处理异常情况.

##### 1.5.1.4.1 异常情况下明显冲突的解决

如果在这个规范中不止一个片段应用于相同的情况但是以冲突的方式, 那么以最具体的方式描述这个情况的段落优先 (这个片段没有必要提供最约束的错误检测) .

###### 1.5.1.4.1.1 异常情况中的明显冲突解决示例

假设函数 foo 是操作数字的函数[function]集合 S 的一个成员. 假设一个段落阐述如果任何一个 S 中的函数[function]被给予一个 17 作为参数就会发出一个错误. 假设一个明显的冲突段落阐述如果参数为 17 则结果是未定义的. 那么第二个段落(更加针对 foo 的那个)会占主要地位, 因为这个情况的上下文描述最详细的, 即便对于参数 17 集合 S 中的其他函数需要去发出一个错误, 这个函数 foo 也不需要. 

#### 1.5.1.5 <span id = "ConformanceStatement">规范性声明</span>

一个符合规范的实现[conforming implementation]应该提供一个规范性声明作为使用这个实现的结果, 或者在附带的文档中加入这个声明. 如果这个具体实现符合这个标准的所有方面, 这个规范性声明应该为

    "<<Implementation>> 符合 ANSI <<standard number>> 的要求"

如果这个实现[implementation]符合这个标准中的一部分并非全部, 这个说明应该为

    "<<Implementation>> 符合 ANSI <<standard number>> 的要求, 除了以下例外: <<这个实现不符合的这个标准的要求的引用或完整列表>>".

### 1.5.2 <span id = "ComformingProg">符合规范的程序</span>

符合这个规范的代码应该坚持下面几条:

    符合规范的代码[conforming code]应该只使用这个标准指定的或者通过这个标准指定的扩展机制定义的语言语法和语义特性.

    符合规范的代码[conforming code]可能使用依赖实现的[implementation-dependent]特性和值, 但是不应该依赖于任何这些特征和值的特别解释, 除了那些在代码[code]的执行中发现的.

    符合规范的代码[conforming code]不应依赖未定义或者未指定情况的结果.

    符合规范的代码[conforming code]不使用这个标准禁止的任何构造.

    符合规范的代码[conforming code]不依赖于一个包含在具体实现中的扩展.

> * 1.5.2.1 [具体实现定义的语言特征的使用](#UseImplDefLangFeature)
> * 1.5.2.2 [可移植代码的字符集](#CharsetForPortCode)

#### 1.5.2.1 <span id = "UseImplDefLangFeature">具体实现定义的语言特征的使用</span>

注意, 符合规范的代码[conforming code]可能依赖一个特定的具体实现定义的[implementation-defined]特性或值. 也注意, 符合规范的代码[conforming code]和符合规范的实现[conforming implementation]的需求中不要求一个符合规范的代码[conforming code]被符合规范的实现[conforming implementation]处理时产生的结果总是相同的. 结果可能一样, 或者它们可能不同.

符合规范的代码[conforming code]可能可以运行于所有符合规范的实现[conforming implementation]中, 但是可能有具体实现定义的[implementation-defined]可允许行为导致这个代码不可移植. 比如, 下面就是一个符合规范的表达式形式[form]在不同实现会返回不同的值的示例:

```BNF
(evenp most-positive-fixnum) =>  implementation-dependent
(random) =>  implementation-dependent
(> lambda-parameters-limit 93) =>  implementation-dependent
(char-name #\A) =>  implementation-dependent
```

##### 1.5.2.1.1 读取时条件的使用

使用 #+ 和 #- 不会自动取消程序的规范性资格. 如果没有使用会使程序不规范的特性, 那么一个使用了 #+ 和 #- 的程序就是规范的. 当然, 符合规范的程序[conforming program]不一定是实际工作的程序. 下面的程序是符合规范的:

```Lisp
(defun foo ()
  #+ACME (acme:initialize-something)
  (print 'hello-there))
```

然而, 这个程序可能不会工作, 取决于特性 ACME 是否存在, 意味着名为 acme:initialize-something 的函数是否存在于这个环境中. 事实上, 在符合规范的程序[conforming program]里使用 #+ 或 #- 意味着增加一个变量 \*features* 作为这个程序的输入参数. 就像其他进入程序的数据一样, 程序员有责任去确保这个程序不做基于这个基本输入数据的无根据的假设. 

#### 1.5.2.2 <span id = "CharsetForPortCode">可移植代码的字符集</span>

可移植[portable]代码[code]只用标准字符[standard character]]编写. 

## 1.6 <span id = "LanguageExtensions">语言的扩展</span>

一个语言的扩展是指标准中已定义的名字[defined name]对应的具体实现定义的[implementation-defined]有别于标准中所描述的已记录的行为, 或者是这个标准指定为未定义, 未指定, 或通过实现可扩展的情况的已记录的后果. 比如, 如果这个标准说 "这个结果是未指定的(the results are unspecified)" , 那么一个扩展会去指定这个结果.

如果一个程序的正确行为依赖一个扩展所提供的结果, 那么只有带有同样扩展的实现会正确执行这段程序. 注意这样的程序可能是不符合规范的. 如果这个标准中说 "一个实现可以被扩展(an implementation may be extended)", 那么用了这个扩展的程序是一个符合规范的但是不可移植的程序.

假定一些扩展没有修改符合规范代码的行为也没有被这个标准显式禁止, 那么一个实现可以有这些扩展.

术语 "extension" 仅引用启动时可用的扩展. 一个具体实现可以自由地允许或禁止一个扩展的重定义.

下面的列表包含了关于特定类型扩展的具体实现指南.

额外的返回值

    一个实现必须返回这个标准指定的准确数量的返回值, 除非标准特别声明以外.

未经请求的信息

    除了这个标准里指定的外, 或者由于函数检测到状况[condition]的发出, 函数不会产生输出.

    未经请求的输出, 例如垃圾收集提醒和自动加载的预兆, 不应该直接到标准中定义的流[stream]变量的值对应的流[stream]里, 但是可以使用到 *terminal-io* 的同义流[synonym stream]间接到终端 I/O [terminal I/O] 中.

    来自函数比如 load 和 compile 的进度报告被认为是请求过的, 并且不会被这个禁令覆盖.

宏和特殊表达式的实现

    这个标准中定义的宏[macro]和特殊操作符[special operator]不能是函数[function]. 

## 1.7 <span id = "LanguageSubsets">语言的子集</span>

这个标准中描述的语言没有子集, 尽管并没有禁止子集.

对于一个被认为是子集的语言, 这个语言下合法的程序[program]一定有等价的语义并且可以被任何全语言的符合规范的实现[conforming implementation]直接运行 (没有语言外的预处理, 并且没有专门的兼容性包).

一个符合这个要求的语言应该被描述为一个 "由 ANSI <\<standard number>> 指定的 Common Lisp 的子集.''

## 1.8 <span id = "DeprecatedLanguageFeatures">弃用的语言特性</span>

废弃的语言特性是不希望出现在未来的 Common Lisp 标准中的, 但是为了符合这个标准需要被实现; 见章节 1.5.1.1 (必要的语言特性).

符合规范的程序[[conforming program]]可以使用废弃的语言特性; 然而, 避免使用它们是良好的编程风格. 在编译的时候允许编译器对这些特性的使用产生风格警告[style warning], 但是在程序执行的时候不应该有这样的警告. 

> * 1.8.1 [废弃的函数](#DeprecatedFunctions)
> * 1.8.2 [废弃的参数约定](#DeprecatedArgumentConventions)
> * 1.8.3 [废弃的变量](#DeprecatedVariables)
> * 1.8.4 [废弃的读取器语法](#DeprecatedReaderSyntax)

### 1.8.1 <span id = "DeprecatedFunctions">废弃的函数</span>

下面这块函数[function]是被废弃的.

    assoc-if-not   nsubst-if-not       require            
    count-if-not   nsubstitute-if-not  set                
    delete-if-not  position-if-not     subst-if-not       
    find-if-not    provide             substitute-if-not  
    gentemp        rassoc-if-not                          
    member-if-not  remove-if-not     

    Figure 1-2. 废弃的函数                    

### 1.8.2 <span id = "DeprecatedArgumentConventions">废弃的参数约定</span>

传递一个数字实参[argument]给 gensym 的能力已经废弃了.

这个给下面这段中的函数的 :test-not 实参[argument]已经被废弃.

    adjoin             nset-difference    search            
    assoc              nset-exclusive-or  set-difference    
    count              nsublis            set-exclusive-or  
    delete             nsubst             sublis            
    delete-duplicates  nsubstitute        subsetp           
    find               nunion             subst             
    intersection       position           substitute        
    member             rassoc             tree-equal        
    mismatch           remove             union             
    nintersection      remove-duplicates     

    Figure 1-3. 带有废弃的 :TEST-NOT 参数的函数               

在 eval-when 中名为 compile, load, 和 eval 的情况的使用被废弃了. 

### 1.8.3 <span id = "DeprecatedVariables">废弃的变量</span>

变量[variable] \*modules* 被废弃了. 

### 1.8.4 <span id = "DeprecatedReaderSyntax">废弃的读取器语法</span>

这个 #S 读取宏[reader macro]强制关键字名字到 KEYWORD 包里; 见章节 2.4.8.13 (井号S(#S)). 这个特性被废弃了; 在未来, 关键字的名字将会在它们被读入的包中被取出, 因此, 如果这是需要的, 那么实际上在关键字包中的符号[symbol]应该被使用. 

## 1.9 <span id = "SymbolsInTheCOMMON-LISPPackage">COMMON-LISP 包中的符号</span>

下面这段包括了 COMMON-LISP 包中 978 个外部符号[symbol]的完整枚举.

    &allow-other-keys            *print-miser-width*     
    &aux                         *print-pprint-dispatch*      
    &body                        *print-pretty*               
    &environment                 *print-radix*                
    &key                         *print-readably*             
    &optional                    *print-right-margin*         
    &rest                        *query-io*                   
    &whole                       *random-state*               
    *                            *read-base*                  
    **                           *read-default-float-format*  
    ***                          *read-eval*                  
    *break-on-signals*           *read-suppress*              
    *compile-file-pathname*      *readtable*                  
    *compile-file-truename*      *standard-input*             
    *compile-print*              *standard-output*            
    *compile-verbose*            *terminal-io*                
    *debug-io*                   *trace-output*               
    *debugger-hook*              +                            
    *default-pathname-defaults*  ++                           
    *error-output*               +++                          
    *features*                   -                            
    *gensym-counter*             /                            
    *load-pathname*              //                           
    *load-print*                 ///                          
    *load-truename*              /=                           
    *load-verbose*               1+                           
    *macroexpand-hook*           1-                           
    *modules*                    <                            
    *package*                    <=                           
    *print-array*                =                            
    *print-base*                 >                            
    *print-case*                 >=                           
    *print-circle*               abort                        
    *print-escape*               abs                          
    *print-gensym*               acons                        
    *print-length*               acos                         
    *print-level*                acosh                        
    *print-lines*                add-method                   

Figure 1-4. COMMON-LISP包中的符号 (1/12).

    adjoin                      atom          boundp                    
    adjust-array                base-char     break                     
    adjustable-array-p          base-string   broadcast-stream          
    allocate-instance           bignum        broadcast-stream-streams  
    alpha-char-p                bit           built-in-class            
    alphanumericp               bit-and       butlast                   
    and                         bit-andc1     byte                      
    append                      bit-andc2     byte-position             
    apply                       bit-eqv       byte-size                 
    apropos                     bit-ior       caaaar                    
    apropos-list                bit-nand      caaadr                    
    aref                        bit-nor       caaar                     
    arithmetic-error            bit-not       caadar                    
    arithmetic-error-operands   bit-orc1      caaddr                    
    arithmetic-error-operation  bit-orc2      caadr                     
    array                       bit-vector    caar                      
    array-dimension             bit-vector-p  cadaar                    
    array-dimension-limit       bit-xor       cadadr                    
    array-dimensions            block         cadar                     
    array-displacement          boole         caddar                    
    array-element-type          boole-1       cadddr                    
    array-has-fill-pointer-p    boole-2       caddr                     
    array-in-bounds-p           boole-and     cadr                      
    array-rank                  boole-andc1   call-arguments-limit      
    array-rank-limit            boole-andc2   call-method               
    array-row-major-index       boole-c1      call-next-method          
    array-total-size            boole-c2      car                       
    array-total-size-limit      boole-clr     case                      
    arrayp                      boole-eqv     catch                     
    ash                         boole-ior     ccase                     
    asin                        boole-nand    cdaaar                    
    asinh                       boole-nor     cdaadr                    
    assert                      boole-orc1    cdaar                     
    assoc                       boole-orc2    cdadar                    
    assoc-if                    boole-set     cdaddr                    
    assoc-if-not                boole-xor     cdadr                     
    atan                        boolean       cdar                      
    atanh                       both-case-p   cddaar                    

Figure 1-5. COMMON-LISP包中的符号 (2/12).

    cddadr             clear-input                  copy-tree                
    cddar              clear-output                 cos                      
    cdddar             close                        cosh                     
    cddddr             clrhash                      count                    
    cdddr              code-char                    count-if                 
    cddr               coerce                       count-if-not             
    cdr                compilation-speed            ctypecase                
    ceiling            compile                      debug                    
    cell-error         compile-file                 decf                     
    cell-error-name    compile-file-pathname        declaim                  
    cerror             compiled-function            declaration              
    change-class       compiled-function-p          declare                  
    char               compiler-macro               decode-float             
    char-code          compiler-macro-function      decode-universal-time    
    char-code-limit    complement                   defclass                 
    char-downcase      complex                      defconstant              
    char-equal         complexp                     defgeneric               
    char-greaterp      compute-applicable-methods   define-compiler-macro    
    char-int           compute-restarts             define-condition         
    char-lessp         concatenate                  define-method-combination  
    char-name          concatenated-stream          define-modify-macro      
    char-not-equal     concatenated-stream-streams  define-setf-expander     
    char-not-greaterp  cond                         define-symbol-macro      
    char-not-lessp     condition                    defmacro                 
    char-upcase        conjugate                    defmethod                
    char/=             cons                         defpackage               
    char<              consp                        defparameter             
    char<=             constantly                   defsetf                  
    char=              constantp                    defstruct                
    char>              continue                     deftype                  
    char>=             control-error                defun                    
    character          copy-alist                   defvar                   
    characterp         copy-list                    delete                   
    check-type         copy-pprint-dispatch         delete-duplicates        
    cis                copy-readtable               delete-file              
    class              copy-seq                     delete-if                
    class-name         copy-structure               delete-if-not            
    class-of           copy-symbol                  delete-package           

Figure 1-6. COMMON-LISP包中的符号 (3/12).

    denominator                    eq                   
    deposit-field                  eql                  
    describe                       equal                
    describe-object                equalp               
    destructuring-bind             error                
    digit-char                     etypecase            
    digit-char-p                   eval                 
    directory                      eval-when            
    directory-namestring           evenp                
    disassemble                    every                
    division-by-zero               exp                  
    do                             export               
    do*                            expt                 
    do-all-symbols                 extended-char        
    do-external-symbols            fboundp              
    do-symbols                     fceiling             
    documentation                  fdefinition          
    dolist                         ffloor               
    dotimes                        fifth                
    double-float                   file-author          
    double-float-epsilon           file-error           
    double-float-negative-epsilon  file-error-pathname  
    dpb                            file-length          
    dribble                        file-namestring      
    dynamic-extent                 file-position        
    ecase                          file-stream          
    echo-stream                    file-string-length   
    echo-stream-input-stream       file-write-date      
    echo-stream-output-stream      fill                 
    ed                             fill-pointer         
    eighth                         find                 
    elt                            find-all-symbols     
    encode-universal-time          find-class           
    end-of-file                    find-if              
    endp                           find-if-not          
    enough-namestring              find-method          
    ensure-directories-exist       find-package         
    ensure-generic-function        find-restart         

Figure 1-7. COMMON-LISP包中的符号 (4/12).

    find-symbol                       get-internal-run-time        
    finish-output                     get-macro-character          
    first                             get-output-stream-string     
    fixnum                            get-properties               
    flet                              get-setf-expansion           
    float                             get-universal-time           
    float-digits                      getf                         
    float-precision                   gethash                      
    float-radix                       go                           
    float-sign                        graphic-char-p               
    floating-point-inexact            handler-bind                 
    floating-point-invalid-operation  handler-case                 
    floating-point-overflow           hash-table                   
    floating-point-underflow          hash-table-count             
    floatp                            hash-table-p                 
    floor                             hash-table-rehash-size       
    fmakunbound                       hash-table-rehash-threshold  
    force-output                      hash-table-size              
    format                            hash-table-test              
    formatter                         host-namestring              
    fourth                            identity                     
    fresh-line                        if                           
    fround                            ignorable                    
    ftruncate                         ignore                       
    ftype                             ignore-errors                
    funcall                           imagpart                     
    function                          import                       
    function-keywords                 in-package                   
    function-lambda-expression        incf                         
    functionp                         initialize-instance          
    gcd                               inline                       
    generic-function                  input-stream-p               
    gensym                            inspect                      
    gentemp                           integer                      
    get                               integer-decode-float         
    get-decoded-time                  integer-length               
    get-dispatch-macro-character      integerp                     
    get-internal-real-time            interactive-stream-p         

Figure 1-8. COMMON-LISP包中的符号 (5/12).

    intern                                  lisp-implementation-type         
    internal-time-units-per-second          lisp-implementation-version      
    intersection                            list                             
    invalid-method-error                    list*                            
    invoke-debugger                         list-all-packages                
    invoke-restart                          list-length                      
    invoke-restart-interactively            listen                           
    isqrt                                   listp                            
    keyword                                 load                             
    keywordp                                load-logical-pathname-translations  
    labels                                  load-time-value                  
    lambda                                  locally                          
    lambda-list-keywords                    log                              
    lambda-parameters-limit                 logand                           
    last                                    logandc1                         
    lcm                                     logandc2                         
    ldb                                     logbitp                          
    ldb-test                                logcount                         
    ldiff                                   logeqv                           
    least-negative-double-float             logical-pathname                 
    least-negative-long-float               logical-pathname-translations    
    least-negative-normalized-double-float  logior                           
    least-negative-normalized-long-float    lognand                          
    least-negative-normalized-short-float   lognor                           
    least-negative-normalized-single-float  lognot                           
    least-negative-short-float              logorc1                          
    least-negative-single-float             logorc2                          
    least-positive-double-float             logtest                          
    least-positive-long-float               logxor                           
    least-positive-normalized-double-float  long-float                       
    least-positive-normalized-long-float    long-float-epsilon               
    least-positive-normalized-short-float   long-float-negative-epsilon      
    least-positive-normalized-single-float  long-site-name                   
    least-positive-short-float              loop                             
    least-positive-single-float             loop-finish                      
    length                                  lower-case-p                     
    let                                     machine-instance                 
    let*                                    machine-type                     

Figure 1-9. COMMON-LISP包中的符号 (6/12).

    machine-version                mask-field                  
    macro-function                 max                         
    macroexpand                    member                      
    macroexpand-1                  member-if                   
    macrolet                       member-if-not               
    make-array                     merge                       
    make-broadcast-stream          merge-pathnames             
    make-concatenated-stream       method                      
    make-condition                 method-combination          
    make-dispatch-macro-character  method-combination-error    
    make-echo-stream               method-qualifiers           
    make-hash-table                min                         
    make-instance                  minusp                      
    make-instances-obsolete        mismatch                    
    make-list                      mod                         
    make-load-form                 most-negative-double-float  
    make-load-form-saving-slots    most-negative-fixnum        
    make-method                    most-negative-long-float    
    make-package                   most-negative-short-float   
    make-pathname                  most-negative-single-float  
    make-random-state              most-positive-double-float  
    make-sequence                  most-positive-fixnum        
    make-string                    most-positive-long-float    
    make-string-input-stream       most-positive-short-float   
    make-string-output-stream      most-positive-single-float  
    make-symbol                    muffle-warning              
    make-synonym-stream            multiple-value-bind         
    make-two-way-stream            multiple-value-call         
    makunbound                     multiple-value-list         
    map                            multiple-value-prog1        
    map-into                       multiple-value-setq         
    mapc                           multiple-values-limit       
    mapcan                         name-char                   
    mapcar                         namestring                  
    mapcon                         nbutlast                    
    maphash                        nconc                       
    mapl                           next-method-p               
    maplist                        nil                         

Figure 1-10. COMMON-LISP包中的符号 (7/12).

    nintersection         package-error                  
    ninth                 package-error-package          
    no-applicable-method  package-name                   
    no-next-method        package-nicknames              
    not                   package-shadowing-symbols      
    notany                package-use-list               
    notevery              package-used-by-list           
    notinline             packagep                       
    nreconc               pairlis                        
    nreverse              parse-error                    
    nset-difference       parse-integer                  
    nset-exclusive-or     parse-namestring               
    nstring-capitalize    pathname                       
    nstring-downcase      pathname-device                
    nstring-upcase        pathname-directory             
    nsublis               pathname-host                  
    nsubst                pathname-match-p               
    nsubst-if             pathname-name                  
    nsubst-if-not         pathname-type                  
    nsubstitute           pathname-version               
    nsubstitute-if        pathnamep                      
    nsubstitute-if-not    peek-char                      
    nth                   phase                          
    nth-value             pi                             
    nthcdr                plusp                          
    null                  pop                            
    number                position                       
    numberp               position-if                    
    numerator             position-if-not                
    nunion                pprint                         
    oddp                  pprint-dispatch                
    open                  pprint-exit-if-list-exhausted  
    open-stream-p         pprint-fill                    
    optimize              pprint-indent                  
    or                    pprint-linear                  
    otherwise             pprint-logical-block           
    output-stream-p       pprint-newline                 
    package               pprint-pop                     

Figure 1-11. COMMON-LISP包中的符号 (8/12).

    pprint-tab                 read-char                   
    pprint-tabular             read-char-no-hang           
    prin1                      read-delimited-list         
    prin1-to-string            read-from-string            
    princ                      read-line                   
    princ-to-string            read-preserving-whitespace  
    print                      read-sequence               
    print-not-readable         reader-error                
    print-not-readable-object  readtable                   
    print-object               readtable-case              
    print-unreadable-object    readtablep                  
    probe-file                 real                        
    proclaim                   realp                       
    prog                       realpart                    
    prog*                      reduce                      
    prog1                      reinitialize-instance       
    prog2                      rem                         
    progn                      remf                        
    program-error              remhash                     
    progv                      remove                      
    provide                    remove-duplicates           
    psetf                      remove-if                   
    psetq                      remove-if-not               
    push                       remove-method               
    pushnew                    remprop                     
    quote                      rename-file                 
    random                     rename-package              
    random-state               replace                     
    random-state-p             require                     
    rassoc                     rest                        
    rassoc-if                  restart                     
    rassoc-if-not              restart-bind                
    ratio                      restart-case                
    rational                   restart-name                
    rationalize                return                      
    rationalp                  return-from                 
    read                       revappend                   
    read-byte                  reverse                     

Figure 1-12. COMMON-LISP包中的符号 (9/12).

    room                          simple-bit-vector                  
    rotatef                       simple-bit-vector-p                
    round                         simple-condition                   
    row-major-aref                simple-condition-format-arguments  
    rplaca                        simple-condition-format-control    
    rplacd                        simple-error                       
    safety                        simple-string                      
    satisfies                     simple-string-p                    
    sbit                          simple-type-error                  
    scale-float                   simple-vector                      
    schar                         simple-vector-p                    
    search                        simple-warning                     
    second                        sin                                
    sequence                      single-float                       
    serious-condition             single-float-epsilon               
    set                           single-float-negative-epsilon      
    set-difference                sinh                               
    set-dispatch-macro-character  sixth                              
    set-exclusive-or              sleep                              
    set-macro-character           slot-boundp                        
    set-pprint-dispatch           slot-exists-p                      
    set-syntax-from-char          slot-makunbound                    
    setf                          slot-missing                       
    setq                          slot-unbound                       
    seventh                       slot-value                         
    shadow                        software-type                      
    shadowing-import              software-version                   
    shared-initialize             some                               
    shiftf                        sort                               
    short-float                   space                              
    short-float-epsilon           special                            
    short-float-negative-epsilon  special-operator-p                 
    short-site-name               speed                              
    signal                        sqrt                               
    signed-byte                   stable-sort                        
    signum                        standard                           
    simple-array                  standard-char                      
    simple-base-string            standard-char-p                    

Figure 1-13. COMMON-LISP包中的符号 (10/12).

    standard-class             sublis                      
    standard-generic-function  subseq                      
    standard-method            subsetp                     
    standard-object            subst                       
    step                       subst-if                    
    storage-condition          subst-if-not                
    store-value                substitute                  
    stream                     substitute-if               
    stream-element-type        substitute-if-not           
    stream-error               subtypep                    
    stream-error-stream        svref                       
    stream-external-format     sxhash                      
    streamp                    symbol                      
    string                     symbol-function             
    string-capitalize          symbol-macrolet             
    string-downcase            symbol-name                 
    string-equal               symbol-package              
    string-greaterp            symbol-plist                
    string-left-trim           symbol-value                
    string-lessp               symbolp                     
    string-not-equal           synonym-stream              
    string-not-greaterp        synonym-stream-symbol       
    string-not-lessp           t                           
    string-right-trim          tagbody                     
    string-stream              tailp                       
    string-trim                tan                         
    string-upcase              tanh                        
    string/=                   tenth                       
    string<                    terpri                      
    string<=                   the                         
    string=                    third                       
    string>                    throw                       
    string>=                   time                        
    stringp                    trace                       
    structure                  translate-logical-pathname  
    structure-class            translate-pathname          
    structure-object           tree-equal                  
    style-warning              truename                    

Figure 1-14. COMMON-LISP包中的符号 (11/12).

    truncate                             values-list               
    two-way-stream                       variable                  
    two-way-stream-input-stream          vector                    
    two-way-stream-output-stream         vector-pop                
    type                                 vector-push               
    type-error                           vector-push-extend        
    type-error-datum                     vectorp                   
    type-error-expected-type             warn                      
    type-of                              warning                   
    typecase                             when                      
    typep                                wild-pathname-p           
    unbound-slot                         with-accessors            
    unbound-slot-instance                with-compilation-unit     
    unbound-variable                     with-condition-restarts   
    undefined-function                   with-hash-table-iterator  
    unexport                             with-input-from-string    
    unintern                             with-open-file            
    union                                with-open-stream          
    unless                               with-output-to-string     
    unread-char                          with-package-iterator     
    unsigned-byte                        with-simple-restart       
    untrace                              with-slots                
    unuse-package                        with-standard-io-syntax   
    unwind-protect                       write                     
    update-instance-for-different-class  write-byte                
    update-instance-for-redefined-class  write-char                
    upgraded-array-element-type          write-line                
    upgraded-complex-part-type           write-sequence            
    upper-case-p                         write-string              
    use-package                          write-to-string           
    use-value                            y-or-n-p                  
    user-homedir-pathname                yes-or-no-p               
    values                               zerop                     

Figure 1-15. COMMON-LISP包中的符号 (12/12).


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

1. 这个标记[token]完全由数字[digit], 正负号[sign], 比率标记[ratio marker], 小数点(.), 扩展字符(^ 或 _), 还有数字标记构成. 一个数字标记是一个字母. 一个字母是否会被当作数字标记取决于上下文, 但是与其他字母相邻的字母不能被视为数字标记. 指数标记[exponent marker]也是数字标记.

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

* 对于任何不是以 @ 符号[at-sign]或者点[dot]开始的表达式形式 form, `,form 等同于 form. (类似的警告适用于所有在逗号[comma]之后出现的表达式形式.)

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
  \#              | reference to = label   | D, d         | undefined              
  $              | undefined              | E, e         | undefined              
  %              | undefined              | F, f         | undefined              
  &              | undefined              | G, g         | undefined              
  '              | function abbreviation  | H, h         | undefined              
  (              | simple vector          | I, i         | undefined              
  )              | signals error          | J, j         | undefined              
  \*              | bit vector             | K, k         | undefined              
  ,              | undefined              | L, l         | undefined              
  :              | uninterned symbol      | M, m         | undefined              
  ;              | undefined              | N, n         | undefined              
  <              | signals error          | O, o         | octal rational         
  =              | labels following object| P, p         | pathname               
  \>              | undefined              | Q, q         | undefined              
  ?              | undefined*             | R, r         | radix-n rational       
  @              | undefined              | S, s         | structure              
  [              | undefined*             | T, t         | undefined              
  \              | character object       | U, u         | undefined              
  ]              | undefined*             | V, v         | undefined              
  ^              | undefined              | W, w         | undefined              
  _              | undefined              | X, x         | hexadecimal rational   
  `              | undefined              | Y, y         | undefined              
  \|              | balanced comment       | Z, z         | undefined              
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

在单字符[character]情况下, x 后面必须跟着一个非成分字符[character]. 在 #\ 被读取之后, 读取器回溯到斜线[slash]然后开始读取一个标记[token], 把最初的斜线[slash]作为单转义[single escape]字符[character] (不管它在当前读取表[current readtable]里是否为单转义字符).

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

一个 #n# 引用可能只出现在一个 #n= 标记后; 超前的引用是不允许的. 这个引用可能不会作为被标记的对象自身出现 (就是说, #n=#n#) 可能不会被写入, 因为这个 #n= 标记的对象[object]在这个情况下还没有被定义好.

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
