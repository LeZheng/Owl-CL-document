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

这个说明书使用了一个扩展 BNF 范式来描述 Common Lisp 宏表达式形式[macro form]和特殊表达式形式[special form]的语法. 这个章节讨论 BNF 范式的语法.

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

这里的术语 "clause" 似乎是 "废弃的(dead)" 因为它没有被使用于上面的BNF表达式里. 然而, 这个BNF的目的并不只是引导解析, 但是也定义有用的术语为了给在后面的描述性文本作参考. 像这个样子, 术语 "clause" 可能出现在后面跟着的文本中, 作为 "normal-clause 或 otherwise-clause" 的速记法.


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

    Common Lisp 指定了输入和输出为非交互的流模型. 这个指定了 交互式的输入输出流如何映射到非交互的模型的具体详情是具体实现定义的[implementation-defined].

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
<!-- TODO point of view 还有 sense of intent-->
> * 1.4.1.4.1 [符号大小写](#CaseInSymbols)
> * 1.4.1.4.2 [数字](#Numbers)
> * 1.4.1.4.3 [点字符的使用](#DotCharacterUse)
> * 1.4.1.4.4 [NIL](#NIL)

##### 1.4.1.4.1 <span id = "CaseInSymbols">符号大小写</span>

虽然捕捉一个符号[symbol]的过程中大小写[case]是很重要的, 但是 Lisp 读取器[Lisp reader]默认在捕捉前会尝试去规范符号[symbol]; 见章节 23.1.2 (Lisp 读取器上的读取表大小写的影响). 既然如此, 符号的大小写默认是不重要的. 这个文档自始自终, 除了明确指定外, 符号[symbol]的大小写是不重要的; 也就是说, HELLO, Hello, hElLo, 还有 hello 都是表示 "HELLO" 符号的等价方法.

反斜线符号[backslash]和竖线符号[vertical-bar]被用于显式引用大小写[case]还有其他解析相关的字符方面. 即便如此, 标记法 |hello| 和 \h\e\l\l\o 是表示 "hello" 符号的等价方式, 并且明显不同[distinct]于符号 "HELLO".

符号[symbol]对应的 Common Lisp 定义的名字[defined name]已经大写化[uppercase]了, 即便它们的名字通常以小写[lowercase]的方式出现在文档里.

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

除了另外的提示, 在这个指定的对象[object]被多次使用的情况下, 这个对象[object]是只求值一次还是被使用时每次都求值, 依赖于具体实现[implementation-dependent].

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
> * 1.4.4.17 ["也见(See Also)" 字典条目部分](#SeeAlsoSDE)
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

    这是一个原子类型指定符[atomic type specifier], 并且依赖于每一个特定条目的信息, 可能会被其他类型指定符[type specifier]所接受.<!--TODO 需要和后面的字典条目校对下-->

类型指定符[Type Specifier]

    这是一个不是原子类型指定符[atomic type specifier]的已定义的名字[defined name], 但是可以被用于构建合法类型指定符[type specifier].

变量[Variable]

    这是一个动态变量[dynamic variable].


#### 1.4.4.15 <span id = "NotesSDE">The "注意(Notes)" 字典条目部分</span>

在其他地方没有的关于这个操作符[operator]的信息. 在其他情况中, 这个可能包含了交叉引用信息, 代码等价性, 格式上的暗示, 实现的暗示, 典型使用. 这个信息不被认为是这标准的一部分; 任何符合规范的实现[conforming implementation]和符合规范的程序[conforming program]允许忽略这个信息.

#### 1.4.4.16 <span id = "PronunciationSDE">"发音(Pronunciation)" 字典条目部分</span>

这个为已定义的名字[defined name]提供了一个推荐的发音方式, 这样人们没有在和最初的设计者们一起交流的情况下也能弄明白这个没有出现在正常英语中的单词如何发音. 这个信息是劝告性的, 不被认为是这个标准的一部分. 为了简洁性, 它只提供给带有特定于 Common Lisp 而不会出现在未删减的《Webster's Third New International Dictionary the English Language》中的名字的条目.

#### 1.4.4.17 <span id = "SeeAlsoSDE">"也见(See Also)" 字典条目部分</span>

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
<!--TODO 校验到此-->
## 1.5 <span id = "Conformance">规范性</span>

这个标准提出了一个合格实现需要去实现的语法和语义 (以及它的附加文档). 另外, 它对符合规范的程序加了一些要求.

> * 1.5.1 [合格的实现](#ComformingImpl)
> * 1.5.2 [合格的程序](#ComformingProg)

 
### 1.5.1 <span id = "ComformingImpl">合格的实现</span>

一个合格的实现应该遵守这个章节中所述的要求.

> * 1.5.1.1 [需要的语言特性](#RequiredLanguageFeatures)
> * 1.5.1.2 [依赖具体实现的特性文档](#DocImplDepFeatures)
> * 1.5.1.3 [扩展文档](#DocExtensions)
> * 1.5.1.4 [异常情况的处理](#TreatmentExceptionalSituations)
> * 1.5.1.5 [规范性声明](#ConformanceStatement)

#### 1.5.1.1 <span id = "RequiredLanguageFeatures">需要的语言特性</span>

一个合格的实现需要去接受这个标准中指定的所有语言特性 (包括弃用的特性) 以及它们的意义.

符合规范的实现不需要在代码中包含替代或额外的语言元素, 以完成该标准中指定的一个语言特性. 

#### 1.5.1.2 <span id = "DocImplDepFeatures">依赖具体实现的特性文档</span>

一个符合规范的实现应该附加一个文档, 里面提供这个说明书中所说的所有依赖具体实现的定义.

另外, 一个符合标准的实现鼓励 (但是不是必须) 去文档化这个标准中标注的依赖实现的条目, 尽管一些情况下这个文档可能简单地把这些条目标注为 ``undefined.'' 

#### 1.5.1.3 <span id = "DocExtensions">扩展文档</span>

一个符合规范的实现应该附带一个文档分开叙述那些这个标准中没有但是实现中有的特性, 但是加到这个语言标准中不应导致任何的歧义和矛盾. 这样的扩展应该被描述为 ``extensions to Common Lisp as specified by ANSI <\<standard number>>.'' 

#### 1.5.1.4 <span id = "TreatmentExceptionalSituations">异常情况的处理</span>

一个符合规范的实现应该和这个说明书一样处理异常情况.

##### 1.5.1.4.1 异常情况下明显冲突的解决

如果在这个说明书中针对同样的情况出现不止一种互相冲突的段落, 那么以最确切的方式描述这个情况的段落优先 (提供最约束的错误检测是没必要的) .

###### 1.5.1.4.1.1 异常情况中的明显冲突解决示例

假设函数foo是操作数字的函数集合S的一个成员. 假设一个段落阐述如果任何一个S中的函数被给予一个17的参数就会发出一个错误. 假设一个明显的冲突段落阐述如果参数为17则结果是未定义的. 然后第二个段落(更加针对foo的那个)会占主要地位, 因为这个情况的上下文描述最详细的, 即便对于参数17集合S中的其他函数需要去发出一个错误这个函数foo也不需要. 

#### 1.5.1.5 <span id = "ConformanceStatement">规范性声明</span>

一个符合规范的实现应该提供一个规范性声明作为使用这个实现的结果, 或者在附带的文档中加入这个声明. 如果这个具体实现符合这个标准的所有方面, 这个规范性说明应该为

``<\<Implementation>> conforms with the requirements of ANSI <\<standard number>>''

如果这个实现符合这个标准中的一部分并非全部, 这个说明应该为

``<\<Implementation>> conforms with the requirements of ANSI <\<standard number>> with the following exceptions: <\<reference to or complete list of the requirements of the standard with which the implementation does not conform>>.'' 

### 1.5.2 <span id = "ComformingProg">合格的程序</span>

符合这个规范的代码应该坚持下面几条:

    合格的代码应该只使用定义在这个标准中的语言语法和语义特性或者这个标准中扩展机制指定的.

    合格的代码可能使用依赖实现的特征和值, 但是不应该依赖于任何这些特征和值的特别解释而不是那些在代码的执行中发现的.

    合格的代码不应依赖未定义或者未指定情况的结果.

    合格的代码不使用这个标准禁止的任何结构.

    合格的代码不依赖于一个具体实现的扩展.

> * 1.5.2.1 [具体实现定义的语言特征的使用](#UseImplDefLangFeature)
> * 1.5.2.2 [可移植代码的字符集](#CharsetForPortCode)


#### 1.5.2.1 <span id = "UseImplDefLangFeature">具体实现定义的语言特征的使用</span>

注意合格的代码可能依赖一个特别的实现定义的特征或值. 也注意同一份合格的代码被符合规范的不同实现处理时, 结果并不总是一样的. 结果可能一样, 或者它们可能不同.

合格的代码可能可以运行于所有符合规范的实现中, 但是可能有实现定义的行为导致这个代码不可移植. 比如, 下面就是一个符合规范的表达式形式的例子, 但是在不同实现会返回不同的值:

```BNF
(evenp most-positive-fixnum) =>  implementation-dependent
(random) =>  implementation-dependent
(> lambda-parameters-limit 93) =>  implementation-dependent
(char-name #\A) =>  implementation-dependent
```

##### 1.5.2.1.1 使用读取时条件

使用 #+ 和 #- 不会使程序变得不规范. 如果没有特征使得程序不规范, 那么一个使用了 #+ 和 #- 的程序就是规范的. 当然, 合格的程序不一定是实际工作的程序. 下面的程序是符合规范的:

```Lisp
(defun foo ()
  #+ACME (acme:initialize-something)
  (print 'hello-there))
```

然而, 这个程序可能不会工作, 取决于特征 ACME 是否存在, 意味着名为 acme:initialize-something 的函数是否存在于这个环境中. 事实上, 在符合规范的程序里使用 #+ 或 #- 意味着增加一个变量 \*features* 作为这个程序的输入参数. 就像其他进入程序的数据一样, 程序员有责任去确保这个程序不做基于这个基本输入数据的无根据的假设. 

#### 1.5.2.2 <span id = "CharsetForPortCode">可移植代码的字符集</span>

可移植代码只用标准字符集编写. 

## 1.6 <span id = "LanguageExtensions">语言的扩展</span>

一个语言的扩展是指标准中定义的名字对应的实现定义的有别于标准中所描述的行为, 或者标准指定一个情况的文档记录结果为 undefined, unspecified, 或者 extendable by the implementation. 比如, 如果这个标准说 ``the results are unspecified,'' , 那么一个扩展会指定这个结果.

如果一个程序的正确行为依赖一个扩展的结果, 只有带有同样实现的扩展会正确执行这段程序. 注意这样的程序可能是不符合规范的. 如果这个标准中说 ``an implementation may be extended'', 那么用了这个扩展的程序是一个符合规范的但是不可移植的程序.

假如一个扩展没有修改符合规范代码的行为并且没有被这个标准明确禁止, 那么一个实现可以有这个扩展.

术语 ``extension'' 仅适用于启动时可用的扩展. 一个具体实现可以自由地允许或禁止重定义扩展.

下面的列表包括了一个实现的关于某些类型的扩展的具体指引.

额外的返回值

    一个实现必须返回这个标准指定的准确数量的返回值, 除非标准明确声明外.

未经允许的信息

    除了这个标准里指定的外, 或者由于函数检测到信号的条件, 函数不会产生输出.

    未经允许的输出, 就像垃圾收集提醒和自动加载的预兆, 不应该直接到标准中定义的流变量里, 但是可以使用同义的流 *terminal-io* 间接到终端 I/O 中.

    来自函数比如 load 和 compile 的进度报告被认为是请求过得, 并且不会被这个禁止覆盖.

宏和特殊表达式的实现

    这个标准中定义的宏和特殊操作符不能是函数. 

## 1.7 <span id = "LanguageSubsets">语言的子集</span>

这个标准中描述的语言没有子集, 尽管并没有禁止子集.

对于一个被认为是子集的语言, 这个语言下合法的程序一定有等价的语义并且可以被任何符合规范的全语言实现直接运行 (没有语言外的预处理, 并且没有专门的兼容性包).

一个遵守这个要求的语言应该被描述为一个a ``subset of Common Lisp as specified by ANSI <\<standard number>>.''


## 1.8 <span id = "DeprecatedLanguageFeatures">弃用的语言特性</span>

废弃的语言特性是不希望出现在未来的 Common Lisp 标准中的, 但是为了符合这个标准需要被实现; 见章节 1.5.1.1 (Required Language Features).

符合规范的程序可以使用废弃的语言特性; 然而, 避免使用它们是良好的编程风格. 在编译的时候允许编译器对这些特性的使用产生警告, 但是在程序执行的时候不应该有警告. 

> * 1.8.1 [废弃的函数](#DeprecatedFunctions)
> * 1.8.2 [废弃的参数约定](#DeprecatedArgumentConventions)
> * 1.8.3 [废弃的变量](#DeprecatedVariables)
> * 1.8.4 [废弃的读取语法](#DeprecatedReaderSyntax)

### 1.8.1 <span id = "DeprecatedFunctions">废弃的函数</span>

下面这块函数是被废弃的.

    assoc-if-not   nsubst-if-not       require            
    count-if-not   nsubstitute-if-not  set                
    delete-if-not  position-if-not     subst-if-not       
    find-if-not    provide             substitute-if-not  
    gentemp        rassoc-if-not                          
    member-if-not  remove-if-not                          

### 1.8.2 <span id = "DeprecatedArgumentConventions">废弃的参数约定</span>

传递一个数字参数给 gensym 已经废弃了.

这个给函数的 :test-not 参数在下面的函数中是被废弃的.

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

在eval-when中使用的compile, load, 和 eval 是被废弃的. 

### 1.8.3 <span id = "DeprecatedVariables">废弃的变量</span>

变量 \*modules* 是废弃的. 

### 1.8.4 <span id = "DeprecatedReaderSyntax">废弃的读取语法</span>

这个 #S 读取宏强迫关键字名字到 KEYWORD 包里; 见章节 2.4.8.13 (Sharpsign S). 这个特征被废弃了; 在未来, 关键字的名字将会在他们被读入的包中被取出, 因此, 如果这是需要的, 那么实际上在关键字包中的符号应该被使用. 

## 1.9 <span id = "SymbolsInTheCOMMON-LISPPackage">COMMON-LISP 包中的符号</span>

下面这段包括了COMMON-LISP包中978个符号的完整枚举.

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