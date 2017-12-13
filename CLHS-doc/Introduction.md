# 1. 引言

> * 1.1 [面向人群, 目的, 和历史](#ScopePurposeAndHistory)
> * 1.2 [文档的组织结构](#OrganizationOfTheDocument)
> * 1.3 [参考的出版物](#ReferencedPublications)
> * 1.4 [定义](#Definitions)
> * 1.5 [Conformance](#Conformance)
> * 1.6 [Language Extensions](#LanguageExtensions)
> * 1.7 [Language Subsets](#LanguageSubsets)
> * 1.8 [Deprecated Language Features](#DeprecatedLanguageFeatures)
> * 1.9 [Symbols in the COMMON-LISP Package](#SymbolsInTheCOMMON-LISPPackage)

## 1.1 <span id = "ScopePurposeAndHistory">面向人群, 目的, 和历史</span>

> 这个文档之前的说明被设计于促进Common Lisp程序在各种数据处理系统之间的移植性. 这是一份面向CL实现者和博学的程序设计者的语言说明书. 它既不是教程也不是一个CL具体实现的导引.

### 1.1.2 历史

>> Lisp是一个有悠久历史的语言家族. 早期的Lisp中的关键思想是 John McCarthy 在 1956 Dartmouth 人工智能夏季研究项目中开发出来的. McCarthy的动机是去开发一个用于人工智能工作的代数列表处理语言. 早期Lisp方言的实现着手于 IBM 704,  IBM 7090, Digital Equipment Corporation (DEC) PDP-1,  DEC PDP-6, 以及 PDP-10. 在1960 到 1965主要的Lisp方言是 Lisp 1.5. 直到20世纪70年代早期出现了两个得益于早先努力占主导地位的Lisp方言: MacLisp 和 Interlisp. 关于早期Lisp方言的更多信息, 见 The Anatomy of Lisp 或者 Lisp 1.5 Programmer's Manual.
>>
>>MacLisp 对 Lisp 1.5 特殊变量和错误处理进行改进. MacLisp 也引入了可以带有可变数量参数的函数, 宏, 数组, 非本地动态退出, 快速计算, 第一个好的 Lisp 编译器, 还有突出的运行速度. 直到20世纪70年代末, MacLisp 被用于超过 50 个网站. 关于更多的 Maclisp 信息, 见 Maclisp Reference Manual, Revision 0 或 The Revised Maclisp Manual.
>>
>>Interlisp 提出了很多关于Lisp编程环境和方法论的看法。其中一个影响了Common Lisp的想法就是Warren Teitelman实现的循环结构，这个启发了后面Lisp Machines和MacLisp，以及现在的Common Lisp的loop宏. 关于更多 Interlisp 的信息, 见 Interlisp Reference Manual.
>>
>>虽然第一个Lisp实现是在 IBM 704 和 IBM 7090 上, 后来的工作聚焦于 DEC PDP-6 以及后来的 PDP-10 计算机, 再后来从20世纪60年代中期到70年代的大部分时间成为Lisp和人工智能工作的骨干是像 Massachusetts Institute of Technology (MIT), Stanford University, 还有 Carnegie Mellon University (CMU) 这样的地方. 这个 PDP-10 计算机和它的前辈 PDP-6 计算机被设计为特别适合于Lisp因为它们有36位的字以及18位的地址. 这个结构允许一个 cons 序对存储在一个字里; 单个指令可以解出 car 和 cdr 部分. 这个 PDP-6 和 PDP-10 拥有快速强大的栈指令可以快速的进行函数调用. 但是到1973年 PDP-10 的局限也是明显的: 它支持很少数量的研究者使用Lisp, 并且这个小的 18 位 地址空间 (2^18 = 262,144 字) 限制了单个程序大小. 对于地址空间问题的一个答复是 Lisp Machine, 一种特殊目的计算机被设计用于运行Lisp程序. 另外一种答复就是使用带有更大地址空间的通用目的计算机, 就像 DEC VAX 还有S-1 Mark IIA. 关于 S-1 Common Lisp 的更多信息, 见 ``S-1 Common Lisp Implementation.''
>>
>>这个 Lisp machine 概念发展于20世纪60年代末期. 在20世纪70年代早期, 和Daniel Bobrow一起工作的Peter Deutsch在个人迷你计算机Alto上实现了一个Lisp, 使用微代码来解释字节码实现语言. 在那以后不久,在 MIT 的Richard Greenblatt 开始着手一个不同的硬件和指令集的设计. 虽然作为Lisp machine 来说 Alto 并不是完全的成功, 但是在 Xerox 开发的D-series机器上一个 Interlisp 的方言Interlisp-D变得可用. 在早期的MIT Lisp Machines上一个称为Lisp Machine Lisp 的向上兼容的 MacLisp 延伸变得可用. 到了1981年，来自于 Xerox, Lisp Machines (LMI), 和 Symbolics 的商用Lisp Machine 出现在市场上. 关于 Lisp Machine Lisp 的更多信息, 见 Lisp Machine Manual.
>>
>>在20世纪70年代末期, Lisp Machine Lisp 开始朝向一个更完整的语言发展. 久经考验的lambda列表, setf, 多值, 以及像Common Lisp中的结构是 Lisp Machine 组的编程风格试验的结果. Jonl White 还有其他人将这些特点移植到 MacLisp. 大约 1980, Scott Fahlman 和在 CMU 的其他人开始致力于一个运行在Scientific Personal Integrated Computing Environment (SPICE) 工作站的Lisp. 这个工程的其中一个目标就是去设计一个比 Lisp Machine Lisp 更简单的方言.
>>
>>这个在 MIT 的 Macsyma 小组在20世纪70年代末期开始了一个项目，称为 New Implementation of Lisp (NIL) for the VAX, 由 White 领导. 这个 NIL 工程的其中一个定期目标是在保留和MacLisp的兼容性的同时去修复一些Lisp的历史遗留问题. 同时, 一个 Stanford University 的研究小组以及 Richard P. Gabriel 领导的劳伦斯利物莫国家实验室开始了运行于 S-1 Mark IIA 超级计算机之上的Lisp. S-1 Lisp的设计, 它具有不完全的功能, 是为Lisp实现适配先进的编译器技术的测试点. 最终这个 S-1 和 NIL 小组合作. 关于 NIL 工程的更多信息, 见 ``NIL---A Perspective.''
>>
>>第一个朝向Lisp标准化的尝试是在1969年, 那时 Anthony Hearn 和 University of Utah 的 Martin Griss定义了标准Lisp---是一个 Lisp 1.5 和其他方言的子集---to transport , 一个符号化的代数系统REDUCE. 在20世纪70年代, the Utah 小组首先为标准Lisp实现了一个可重定向编译的优化编译器, 并且还有一个被认为是Portable Standard Lisp (PSL)的扩展的实现. 直到20世纪80年代中期, PSL 运行在十多种计算机上. 关于标准Lisp的更多信息, 见 ``Standard LISP Report.''
>>
>>PSL 和 Franz Lisp---是Unix机器上的类MacLisp方言---是第一个在多种硬件平台广泛可用的Lisp方言的例子.
>>
>>其中一个最重要的Lisp发展是在20世纪70年代后半期: Scheme. Scheme, 是Gerald J. Sussman 和 Guy L. Steele Jr. 设计, 是一个简洁的Lisp方言，从20世纪60年代以来这里的设计带给Lisp很多编程语言语义上的理念. Sussman是20世纪60年代末到70年代很多Lisp技术其他进展背后最初的变革者之一.Scheme主要的贡献是词法作用域, 词法闭包, first-class continuations, 还有简单的语法 (没有区分值 cell 和 函数 cell). 这些贡献中很多对Common Lisp的设计产生了很大影响. 关于Scheme的更多信息, 见 IEEE Standard for the Scheme Programming Language 或者 ``Revised^3 Report on the Algorithmic Language Scheme.'' <!-- TODO 待核对 -->
>>
>>在20世纪70年代末期,面向对象编程概念对Lisp产生了很大影响. 在 MIT, 来自于 Smalltalk 的思想径直进入很多广泛使用的编程系统中. Flavors, 一个带有多继承的面向对象系统, 为了Lisp machine社区在 MIT 被 Howard Cannon 和 其他人开发出来. At Xerox,  Smalltalk 的经验和 Knowledge Representation Language (KRL) 引导了 Lisp Object Oriented Programming System (LOOPS) 还有后来的 Common LOOPS的发展. 关于跟多 Smalltalk 的信息, 见 Smalltalk-80: The Language and its Implementation. 关于Flavors更多信息, 见 Flavors: A Non-Hierarchical Approach to Object-Oriented Programming.
>>
>>这些系统影响了Common Lisp Object System (CLOS)的设计. CLOS 是为了标准化特地开发的, 并且写在了 ``Common Lisp Object System Specification.'' 然而, 在公布之后它的设计中小部分细节有了轻微的改变, 并且就像这个文档中所说,这个文章不应该被当作一份这个对象系统语义上的官方说明.
>>
>>在1980 Symbolics 和 LMI 发展了 Lisp Machine Lisp; stock-hardware 实现小组发展了 NIL, Franz Lisp, 还有 PSL; Xerox 发展了 Interlisp; 还有 CMU 的 SPICE 项目发展了称之为 SpiceLisp的类MacLisp方言.
>>
>>在1981年四月, 在一个 DARPA赞助的会议后分裂的Lisp社区, Symbolics, SPICE 项目, NIL 项目, 还有 S-1 Lisp 项目一起加入来定义 Common Lisp. 开始由 White 和 Gabriel 带领, 这个基层民众的尝试背后的驱动力由 Fahlman, Daniel Weinreb, David Moon, Steele, 和 Gabriel 提供. Common Lisp 被设计为一个家族语言的描述. 对于Common Lisp主要的影响是 Lisp Machine Lisp, MacLisp, NIL, S-1 Lisp, Spice Lisp, 还有 Scheme. Common Lisp: 这个语言是那个设计的描述. 部分地方它的语义是有意指定的因为那里感觉严格规范会限制Common Lisp的研究和使用.<!-- TODO 待核对 -->
>>
>>在1986年 X3J13 组织了一个技术小组去起草 ANSI Common Lisp 标准. 由于 Common Lisp的接纳, 这个小组的目标与最初的设计者不同. 这些目标包括更严格的移植性标准化,一个面向对象编程系统,一个状态系统,循环机制,以及一种处理大型字符集的方式.为了促成这个目标，一个新的语言说明(这个文档)被开发出来.

## 1.2 <span id = "OrganizationOfTheDocument">文档的组织结构</span>

> 这是一份参考文档, 不是一份教程文档. 为了尽可能方便, 这个报告的顺序是原始话题在前, 构建于它们之上的在后; 然而, 线性的阅读性并不是值得优先考虑的.
>
> 这个文档根据话题来拆分章节. 任何给定章节可能包含概念性内容或字典条目, 或者都有.
>
> 一个章节的字典部分把主题相关的已定义的名字分组后放得接近一些. 很多这样的分组是可能的，并且应该没有从分组中推断出的深层意义. 为了看到以字母顺序排列分组的定义的名字, 可以查阅索引. 关于定义名字的完整列表, 见章节 1.9 (Symbols in the COMMON-LISP Package).<!-- TODO 待核对 -->
>
> 为了补偿这份文档的无序部分, 提供一份词汇表; 见章节 26 (Glossary). 这个词汇表通过快速访问术语的定义来提供连通性,并且一些情况下提供示例或者和另外概念的交叉引用.
>
>关于这个文档中符号约定的内容, 见章节 1.4 (Definitions).
>
>关于一致性的内容, 见章节 1.5 (Conformance).
>
>关于扩展和子集的内容, 见章节 1.6 (Language Extensions) 和 Section 1.7 (Language Subsets).
>
>关于这个语言的程序如何被Lisp读取器解析, 见章节 2 (Syntax).
>
>关于这个语言的程序如何被编译和执行, 见章节 3 (Evaluation and Compilation).
>
>关于数据类型的内容, 见章节 4 (Types and Classes). 不是所有的类型和类都被定义在这个章节; 很多定义在它们主题对应的章节--比如, 数值类型定义在了章节 12 (Numbers). 关于标准化类型的完整列表, 见 Figure 4-2.
>
>关于常用目的控制和数据流, 见章节 5 (Data and Control Flow) 或章节 6 (Iteration).

## 1.3 <span id = "ReferencedPublications">参考的出版物</span>

* The Anatomy of Lisp, John Allen, McGraw-Hill, Inc., 1978.

* The Art of Computer Programming, Volume 3, Donald E. Knuth, Addison-Wesley Company (Reading, MA), 1973.

* The Art of the Metaobject Protocol, Kiczales et al., MIT Press (Cambridge, MA), 1991.

* \``Common Lisp Object System Specification,'' D. Bobrow, L. DiMichiel, R.P. Gabriel, S. Keene, G. Kiczales, D. Moon, SIGPLAN Notices V23, September, 1988.

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

* ``NIL---A Perspective,'' JonL White, Macsyma User's Conference, 1979.

* Performance and Evaluation of Lisp Programs, Richard P. Gabriel, MIT Press (Cambridge, MA), 1985.

* ``Principal Values and Branch Cuts in Complex APL,'' Paul Penfield Jr., APL 81 Conference Proceedings, ACM SIGAPL (San Francisco, September 1981), 248-256. Proceedings published as APL Quote Quad 12, 1 (September 1981).

* The Revised Maclisp Manual, Kent M. Pitman, Technical Report 295, Laboratory for Computer Science, MIT (Cambridge, MA), May 1983.

* ``Revised^3 Report on the Algorithmic Language Scheme,'' Jonathan Rees and William Clinger (editors), SIGPLAN Notices V21, #12, December, 1986.

* ``S-1 Common Lisp Implementation,'' R.A. Brooks, R.P. Gabriel, and G.L. Steele, Conference Record of the 1982 ACM Symposium on Lisp and Functional Programming, 108-113, 1982.

* Smalltalk-80: The Language and its Implementation, A. Goldberg and D. Robson, Addison-Wesley Company, 1983.

* ``Standard LISP Report,'' J.B. Marti, A.C. Hearn, M.L. Griss, and C. Griss, SIGPLAN Notices V14, #10, October, 1979.

* Webster's Third New International Dictionary the English Language, Unabridged, Merriam Webster (Springfield, MA), 1986.

* XP: A Common Lisp Pretty Printing System, R.C. Waters, Memo 1102a, Artificial Intelligence Laboratory, MIT (Cambridge, MA), September 1989.


## 1.4 <span id = "Definitions">定义</span>

这个章节中包含了这个手册里的符号约定和术语定义.

> * 1.4.1 [符号约定](#NotationalConventions)
> * 1.4.2 [错误术语](#ErrorTerminology)
> * 1.4.3 [本标准未正式规定的部分](#SectionsNotFormallyPartOfThisStandard)
> * 1.4.4 [解释的字典条目](#InterpretingDictionaryEntries)

### 1.4.1 <span id = "NotationalConventions">符号约定</span>

下面的符号约定适用于整个文档.

> * 1.4.1.1 [字体的线索](#FontKey)
> * 1.4.1.2 [修改BNF语法](#ModifiedBNFSyntax)
> * 1.4.1.3 [特殊符号](#SpecialSymbols)
> * 1.4.1.4 [多个符号表示的对象](#ObjectsWithMultipleNotations)
> * 1.4.1.5 [指示符](#Designators)
> * 1.4.1.6 [无意义的单词](#NonsenseWords)

#### 1.4.1.1 <span id = "FontKey">字体的线索</span>

这个文档中使用的字体表达的含义.

<u>*name*</u>

    表示定义在词汇表中的正常的术语. 如果这个字体被使用,那么这个词汇表定义优先于正常的英语使用.

    有时候这个词汇表术语会有下标, 就像 ``whitespace[2].'' 这样的定义选择多个词汇表定义的其中一个,这里是第二个. 这个下标标记法在术语词汇表里通常被使用于那种上下文不足以消除歧义的定义.

<u>*name*</u>

    表示当前文本的正式用语介绍. 这个一直对应一个词汇表条目,并且通常等价于使用 ``name,'' 但是这使得这个使用更加明显,可以在一些情况避免读者查阅词汇表.

**name**

    表示一个在COMMON-LISP包里的符号. 关于这种符号更多内容, 见章节 1.4.1.4.1 (符号案例).

name

    表示一个程序员可能用Common Lisp写的简单的名字或代码片段.

    这个字体也被用于某些标准化的不属于COMMON-LISP包里的名字, 比如关键字[1], 包名, 还有循环关键字.

*name*

    表示一个参数或者值的名字.

    有些情况下也使用标记 ``<<name>>'' (换句话说, 同样的字体, 但是带有 ``尖括号'' 包装) 来为包在里面的字提供更好的目视间距. 这些 ``尖括号'' 是伪变量用的, 并且事实上从来不会出现在程序的输入输出中.

#### 1.4.1.2 <span id = "ModifiedBNFSyntax">修改BNF语法</span>
这个说明书使用了一个扩展BNF范式来描述 Common Lisp 宏形式和特殊形式的语法. 这个章节讨论BNF范式的语法.

> * 1.4.1.2.1 [修改后BNF语法的拼接](#SplicingInModifiedBNFSyntax)
> * 1.4.1.2.2 [修改后BNF语法的间接](#IndirectionInModifiedBNFSyntax)
> * 1.4.1.2.3 [修改后BNF语法中间接定义的额外使用](#AdditionalUsesForIndirectDef)

##### 1.4.1.2.1 <span id = "SplicingInModifiedBNFSyntax">修改后BNF语法的拼接</span>

使用的主要扩展是以下这样的:

```
[[O]]
```

无论何时当一个多元素列表被拼接到一个更大的结构并且元素可以以任何顺序出现的情况下, 就会出现这个形式的表达式. 这个符号 O 表示要被拼接的多个句法元素的语法描述; 这个描述必须以这种形式

```
O1 | ... | Ol
```

其中每一个 Oi 可以是S表达式或者S\*表达式或者{S}1的形式 . 这个表达式 [[O]] 意味着这样形式的列表

```
(Oi1...Oij) 1<=j
```

被拼接到一个闭合的表达式中, 这样的情况下如果 n /=m 并且 1<=n,m<=j, 就存在 Oin/=Oim 和 Oin = Oim = Qk, 其中 1<=k <=n, Ok 是形式 Qk*. 此外, 对于每一个在结构 {Qk}1 中的 Oin,  这个元素需要出现在拼接后列表的某处.

比如说, 这个表达式

```
(x [[A | B* | C]] y)
```

意味着最多一个A, 任意数量的B, 以及最多一个C可以以任何形式出现. 它是以下任意形式的描述:

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

在第一种情况下, 不管A还是C都出现的太频繁, 而第二个例子中C出现得太频繁.

标记法 [[O1 | O2 | ...]]+ 添加额外的限制, 表示至少有一个选项必须被使用. 比如说:

```
(x [[A | B* | C]]+ y)
```

意味着至少一个A, 任意数量的B, 并且至少一个C以任意顺序出现, 但是在任何情况下至少它们之中的一个选项要满足. 它是以下这些的描述:

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

在第一个例子中, 没有任何条件满足; 在第二个例子中, A 和 C 都出现得太频繁; 还有在第三个示例中C出现得太频繁.

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

##### 1.4.1.2.2 <span id = "IndirectionInModifiedBNFSyntax">修改后BNF语法的间接</span>

引入一个间接的扩展是为了使这种语法可读性更高:

```BNF
O
```

如果 O 不是一个终止符, 它的定义的右边部分会完整地替换表达式 O. 比如, 下面的这个 BNF 表达式等价于上面那个例子:

```BNF
(x [[O]] y)

O::= A | B* | C
```

##### 1.4.1.2.3 <span id = "AdditionalUsesForIndirectDef">修改后BNF语法中间接定义的额外使用</span>

在一些情况下In some cases, 一个BNF辅助定义可能不会出现在这个BNF表达式里, 但是在其他地方是有用的. 比如说, 细想下面的定义:[>_<]待校验

```BNF
case keyform {normal-clause}* [otherwise-clause] => result*

ccase keyplace {normal-clause}* => result*

ecase keyform {normal-clause}* => result*

normal-clause::= (keys form*)

otherwise-clause::= ({otherwise | t} form*)

clause::= normal-clause | otherwise-clause
```

这里的术语 \``clause'' 似乎是 \``dead'' 因为它没有被使用于上面的BNF表达式里. 然而, 这个BNF的目的并不只是引导解析, 但是也定义有用的术语为了跟在后面的描述性文本的参考. 像这个样子, 术语 \``clause'' 可能出现在后面跟着的文本中, 作为 \``normal-clause or otherwise-clause.'' 的速记.<!-- TODO 待校验 -->


#### 1.4.1.3 <span id = "SpecialSymbols">特殊符号</span>

这里描述的特殊符号在这个文档中为了概念上的便利, 既不是Common Lisp语言也不是它的运行环境的一部分.

=>

    这个表示求值. 比如说:

     (+ 4 5) =>  9

    这个意味着对表达式 (+ 4 5) 求值的结果是 9.

    如果一个表达式形式返回多个值, 这些值可能通过空格,换行或者逗号区分. 比如说:

     (truncate 7 5)
    =>  1 2
     (truncate 7 5)
    =>  1
       2
     (truncate 7 5)
    =>  1, 2

    上面三个表达式的每一个都是等价的, 表示那个 (truncate 7 5) 返回两个值: 1 和 2.

    一些一致性实现事实上在显示返回值之前打印箭头 (或者一些其他的指示方式) , 有一些则没有.

OR=>

    记号 ``OR=> '' 被用于表示几个可能值的其中一个. 比如

     (char-name #\a)
    =>  NIL
    OR=>  "LOWERCASE-a"
    OR=>  "Small-A"
    OR=>  "LA01"

    表示 nil, "LOWERCASE-a", "Small-A", "LA01" 都是 (char-name #\a) 可能的结果---每一个都有相等的可能性. 除非明确指定, 它不应被认为可能值的集合已经是完整的了. 正常情况下, 上面的例子等价于

     (char-name #\a) =>  implementation-dependent

    但是它的意图是提供额外的信息去说明这些结果中可能会根据实现而不同.<!-- TODO 待验证 -->

NOT=>

    标记 ``NOT=> '' 用于表示不可能的结果. 这个可能被用于, 比如, 为了强调一种情况, 一些可预见的误解可能引导读者错误地相信这个结果是可能出现的. 比如,

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

    Common Lisp 指定了输入和输出为非交互的流模型. 这个指定了交互式的输入输出流如何映射到实现中定义的非交互的模型.

    比如, 一致性的实现中承诺去区分交互式输入如何中止的问题. 比如, 当最后的定界符被输入到非交互的流中，, read函数中止. 在一些CL的实现中, 一个交互式的对read的调用在最后的定界符一输入就返回, 甚至那个定界符不是换行. 在其他实现中, 总是需要换行. 在其他实现中, 可能会有一个命令去 ``activates'' 一个满的输入缓冲区, 而这个命令本身在程序输入流中不可见.

    这个文档的例子中, 标记 ``>> '' 先于交互式输入输出前. 这种方案下, ``this notation'' 表示用户输入.

    比如, 下面的标记

    (+ 1 (print (+ (sqrt (read)) (sqrt (read)))))
    >>  9 16
    >>  7
    =>  8

    显示一个要被求值的表达式 ``(+ 1 (print (+ (sqrt (read)) (sqrt (read)))))'' 的交互, ``9 16 '' 交互的输入, ``7'' 是交互的输出, 然后 ``8'' 是求值后的返回值.

    这个标记的使用是为了掩盖各个实现之间, 交互式输入输出的微小区别.

    有时, 非交互的流模型调用是通过换行. 换行字符如何交互式输入是用户接口的实现定义的细节, 但是在那个例子中, 可能会被使用的不是 ``<Newline>'' 就是 ``<NEWLINE>''.

     (progn (format t "~&Who? ") (read-line))
    >>  Who? Fred, Mary, and Sally<NEWLINE>
    =>  "Fred, Mary, and Sally", false

#### 1.4.1.4 <span id = "ObjectsWithMultipleNotations">多个符号表示的对象</span>

Common Lisp中的一些对象不止一种表示的方法. 这种情况下, 选择使用哪一种是随意的, 但是表示一个 \``point of view'' 或 \``sense of intent.'' 的惯例是存在的

> * 1.4.1.4.1 [符号大小写](#CaseInSymbols)
> * 1.4.1.4.2 [数字](#Numbers)
> * 1.4.1.4.3 [点](#DotCharacterUse)
> * 1.4.1.4.4 [NIL](#NIL)

##### 1.4.1.4.1 <span id = "CaseInSymbols">符号大小写</span>

虽然绑定一个符号的过程中大小写是很重要的, Lisp的读取器默认在绑定前会尝试去规范符号; 见章节 23.1.2 (Effect of Readtable Case on the Lisp Reader). 既然如此, 符号的大小写默认是不重要的. 这个文档自始自终, 除了明确指定外, 符号的大小写是不重要的; 也就是说, HELLO, Hello, hElLo, 还有 hello 都是表示 "HELLO" 符号的等价方法.

反斜线符号和竖线符号被用于明确引用大小写还有其他解析相关的字符方面. 即便如此, 标记法 |hello| 和 \h\e\l\l\o 是表示 "hello" 符号的等价方式, 并且明显不同于符号 "HELLO".

符号对应的Common Lisp定义的名字已经大写化了, 即便它们的名字通常以小写的方式出现在文档里.

##### 1.4.1.4.2 <span id = "Numbers">数字</span>

虽然 Common Lisp 为程序提供了很多方式去操纵有理数的输入和输出基数, 但是这个文档中的所有数字是十进制表示的除非明确指定外.

##### 1.4.1.4.3 <span id = "DotCharacterUse">点字符的使用</span>

圆点字符在表达式中自己出现就像

(item1 item2 . tail)

表示那个 tail 对应一个列表结尾的list对象. 比如,

(A B C . (D E F))

符号上等价于:

(A B C D E F)

虽然圆点是符号的一个合法字符, 但是没有标准化的符号包含了圆点字符, 所以这个文档中句子末尾的圆点总是解释为以一个句号而不是符号名的一部分. 比如, 这个文档中, 就像这样 ``This sample sentence refers to the symbol car.'' 的一个句子引用了一个名字为 "CAR" 的符号(带有三个字母), 不表示4个字母的 "CAR."

##### 1.4.1.4.4 <span id = "NIL">NIL</span>

nil 有很多意思. 它是COMMON-LISP 包中名字为 "NIL" 的符号, 也是布尔值 (而且是普遍的布尔) false, 它是空列表, 并且它是空类型的名字 (所有类型的子类).

在 Common Lisp 中, nil可以表示为 NIL 或者 (). 按照惯例, 这个表示法的选择提供了线索去判断它扮演的角色.

| 是否求值? | 表示法 | 通常表示的作用 |
|----------|-------|-------------|
|Yes       |nil    |作为 boolean. |
|Yes       |'nil   |作为符号.      |
|Yes       |'()    |表示空列表     |
|No        |nil    |作为符号或布尔值.|
|No        |()     |作为符号.       |

Figure 1-1. NIL表示法

仅在这个文档中, nil 有时也用false表示来强调它作为布尔值的角色.

比如:

```Lisp
(print ())                          ;avoided
(defun three nil 3)                 ;avoided
'(nil nil)                          ;list of two symbols
'(() ())                            ;list of empty lists
(defun three () 3)                  ;Emphasize empty parameter list.
(append '() '()) =>  ()              ;Emphasize use of empty lists
(not nil) =>  true                   ;Emphasize use as Boolean false
(get 'nil 'color)                   ;Emphasize use as a symbol
```

一个函数在一些情况下被说成 \``be false'' 或者 \``be true''. 因为看作布尔值时没有函数对象等同于 nil 并且所有函数对象都表示true, 所以去说一个函数是false是无意义的, 去说它是true是无聊的. 这些只是表示函数 \``returns false'' 或着 \``returns true,'' 的传统方式.

#### 1.4.1.5 Designators
<!-- TODO 待校验 -->
designator 是一个表示另一个对象的对象.

一个操作符的参数被描述为 designator 的地方, 这个操作符的描述以假定这个参数的值是指定的对象的方式编写; 这就表示, 这个参数是已指定的类型. (这个对象的具体类型由一个 \``<\<type>> designator'' 或者 \``designator for a <\<type>>'' 指定, 可以在结尾的词汇表中找到 \``<\<type>> designator.'')

比如, \``nil'' 和 ``the value of *standard-output*'' 作为流 designators 是操作上难以区分的 . 类似的, 作为string designators 符号 foo 和字符串 "FOO" 也是难以区分的.

除了另外的提示, 在这个指定的对象被多次使用的情况下, 这个对象是只求值一次还是被使用时每次都求值, 取决于具体的实现.

比如, mapcar 接受一个函数指定作为参数, 并且它的描述中写的就像它只是个函数. 事实上这个函数designator 是被马上求值还是在表达式形式的内部在每次被需要的时候求值是跟具体实现相关的. 大部分情况下, 程序不能发现其中的区别, 但是也有一些不正常的情况 (尤其是那些包含自身重定义或者相互重定义的函数) 确实符合并且发现这个区别. 下面的程序就是一个conforming program, 但是可能有或者没有确定的值, 取决于它的正确性是否依赖一个或其他的结果:

```Lisp
 (defun add-some (x)
   (defun add-some (x) (+ x 2))
   (+ x 1)) =>  ADD-SOME
 (mapcar 'add-some '(1 2 3 4))
=>  (2 3 4 5)
OR=>  (2 4 5 6)
```

在一些罕见的情况下, 这里可能有个需要在字典中去提及这个最初的designator对象为一个参数. 因为对一个参数取名会提及表示的对象, 短语 ``the <\<parameter-name>> designator'' 可以被用于提及来自于<\<parameter-name>> 被计算的值的 designator.

#### 1.4.1.6 <span id = "NonsenseWords">无意义的单词</span>

当需要一个没有前置关联语义的单词 (比如, 在一个示例中), 在Lisp社区中通常使用 \``foo,'' \``bar,'' \``baz,'' and \``quux.'' 的其中一种. 比如

```Lisp
 (defun foo (x) (+ x 1))
```

名字 foo 的使用表示 ``请用你喜欢的名字替换这里.'' 的方式.

这些无意义的单词有如此的使用率, 社区的新人开始去思考这里是否有他们忽略的已绑定的语义---当然这里是没有的.


### 1.4.2 <span id = "ErrorTerminology">错误术语</span>

错误可能或应该或必然出现的情况在这个标准中已经描述了. 用于描述这种情况的词规定为需要有确切的含义. 以下列表是这些词的词汇表.

Safe code

    这个代码在设置为最高(3)时会被安全性最优化处理. 安全性是代码的一个词法属性. 短语 ``the function F should signal an error'' 意味着如果F如果在代码被最高级安全性优化的情况下被调用, 会发出一个错误. 是F还是调用的代码来发出这个错误是有具体实现决定的.

Unsafe code

    这个代码是被最低级安全级别处理的.

    不安全代码可能会做错误检测. 具体实现允许一直把所有代码当作安全代码.

An error is signaled

    这个意味着在安全的和不安全的代码中都会发出一个错误. 合格的代码可能依赖安全和非安全代码中都会发出错误这一事实. 不管是安全的还是非安全的代码, 每一个具体实现需要去检测这个错误. 比如, ``an error is signaled if unexport is given a symbol not accessible in the current package.''

    如果没有指定明确的错误类型, 默认是 error.

An error should be signaled

    这个意味着一个错误在安全的代码中发出, 并且一个错误可能在不安全的代码中发出. 合格的代码可能依赖错误会在安全代码中发出这一事实. 每一个实现至少需要在安全的代码中去检测这个错误. 当这个错误没有发出, the ``consequences are undefined'' (见下方). 比如, ``+ should signal an error of type type-error if any argument is not of type number.''

<!-- TODO 待验证-->
Should be prepared to signal an error

    这个类似于 ``should be signaled'', 除了这个不需要在这个操作上采取额外工作去发现错误的情况 except that it does not imply that `extra effort' has to be taken on the part of an operator to discover an erroneous situation 如果这个操作符正常的动作可以在懒检测下被成功运行if the normal action of that operator can be performed successfully with only `lazy' checking. 一个实现总是允许去发出一个错误, 但是即使在安全代码里, 只需要发出这个错误, 如果没有发出可能导致不正确的结果. 在不安全代码中, the consequences are undefined.

    比如, 定义 ``find should be prepared to signal an error of type type-error if its second argument is not a proper list'' 不表示一定会发出一个错误 . 这个表达式形式

    (find 'a '(a b . c))

    必须在安全的代码中发出一个 type-error 类型的错误, 或者返回 A. 在不安全的代码中, the consequences are undefined. 相比之下,

    (find 'd '(a b . c))

    必须在安全代码中发出一个 type-error 类型的错误. 在不安全的代码中, the consequences are undefined. 同样的

    (find 'd '#1=(a b . #1#))

    在安全代码中可能返回 nil (作为一个具体实现定义的扩展), 可能从来不返回, 或者发出一个 type-error 类型的错误. 安全的代码中, the consequences are undefined.

    通常, 这个 ``should be prepared to signal'' 术语被用于类型检测的情况, 其中存在效率考虑, 使检测错误变得不切实际 , 改正操作符的行为不是实质性的.
<!--TODO 待校验-->

The consequences are unspecified

    这个意味着结果是不可预测但是无害的. 具体实现允许指定这种情况的结果. 没有conforming code 可能依赖这种情况的结果或影响, 并且所有的 conforming code 需要把这种情况的结果和影响认为是不可预测的但是无害的. 比如, ``if the second argument to shared-initialize specifies a name that does not correspond to any slots accessible in the object, the results are unspecified.''

The consequences are undefined

    这个意味着结果是不可预测的. 结果可能是无害的或者致命的. 没有 conforming code 可能依赖这个结果或影响. Conforming code 必须把这个结果当作不可预测的. 在 ``must,'' ``must not,'' 或者 ``may not'' 词语被使用的地方, 如果没有看到规定的需求或者没有明确指定特定的结果, 就会有 ``the consequences are undefined''.

    比如: ``Once a name has been declared by defconstant to be constant, any further assignment or binding of that variable has undefined consequences.''

An error might be signaled

    这个意味着存在未定的结果; 然而, 如果一个错误被发出, 它是后面指定的类型. 比如, ``open might signal an error of type file-error.''

The return values are unspecified

    这个意味着一个表达式形式的返回值数量和性质没有指定. 然而, 副作用和控制转移的发生与否是指定好的.

    一个程序可以是指定好的即便它使用了没有指定返回值的函数. 比如, 即便函数 F 的返回值没有指定, 就像 (length (list (F))) 表达式也是指定好的因为它不依赖任何F返回值的特定方面.

Implementations may be extended to cover this situation

    这个意味着这种情况存在未定义的结果; 然而, 一个合格的具体实现可以用一种更加具体的方式去对待这种情况. 比如, 一个具体实现可能发出一个错误, 或者应该发出一个错误, 或者甚至出现一个定义好的无错行为.

    没有 conforming code 可能依赖这个情况的结果; 所有 conforming code 必须把这种情况的结果当作是未定义的. 具体实现需要用文档记录如何对待这种情况.

    比如, ``implementations may be extended to define other type specifiers to have a corresponding class.''

Implementations are free to extend the syntax

    这意味着在这种情况下, 实现允许去定义被描述的表达式语法的清楚的扩展. 没有 conforming code 可能依赖这个扩展. 具体实现需要去用文档记录每一个这样的扩展. 所有 conforming code 需要去把这个语法当作是无意义的. 标准可能禁止一些扩展而允许其他的. 比如, ``no implementation is free to extend the syntax of defclass.''

A warning might be issued

    这意味着在适当条件下 (比如, 编译的时候), 具体实现是鼓励发出一个警告的. 然而, 一个合格的实现不需要发出一个警告.


### 1.4.3 <span id = "SectionsNotFormallyPartOfThisStandard">本标准未正式规定的部分</span>

这个标准的前页和书后的附属资料, 就像 \``Table of Contents,'' \``Index,'' \``Figures,'' \``Credits,'' 和 \``Appendix'' 没有正规地考虑过, 所以我们保留更新这些部分所需要的灵活性, 以便于即使在最后一刻也不需要担心修改文档的这些部分需要正式的投票. 这些条目很短并且很有用, 然而, 不推荐在这个文档的删减版本里把它们删除.

在概念性的章节里, 提供的名字以单词 \``Note'' 或 \``Notes'' 或 \``Example'' 或 \``Examples'' 开头的部分只用于说明目的, 并且不被认为是标准的一部分.

已经试着把这些章节放在父章节的末尾, 这样它们就可以被移除而不需要修改相邻章节的序号来减少文档的大小.

同样的, 字典条目中的这个 \``Examples'' 和 \``Notes'' 章节也不被认为是标准的一部分, 如有必要可以移除.

不过, 这些示例对剩下的部分提供了重要的说明和一致性检查, 并且这样的删减是不推荐的除非完全无法避免.

### 1.4.4 <span id = "InterpretingDictionaryEntries">解释的字典条目</span>

这个字典条目对于每个定义的名字都是按章节来分割的. 除非明确表示, 每个章节由确定这个章节的标签引入. 章节的删减表示这个章节是不可应用的或者没有提供什么有意思的信息.

这个章节定义了每个可能出现在字典章节的条目的意义.

> * 1.4.4.1 ``Affected By'' 字典条目部分(#AffectedBySDE)
> * 1.4.4.2 ``Arguments'' 字典条目部分(#ArgumentsSDE)
> * 1.4.4.3 ``Arguments and Values'' 字典条目部分(#ArgumentsAndValuesSDE)
> * 1.4.4.4 ``Binding Types Affected'' 字典条目部分(#BindingTypesAffectedSDE)
> * 1.4.4.5 ``Class Precedence List'' 字典条目部分(#ClassPrecedenceListSDE)
> * 1.4.4.6 类型标识符的字典条目(#DictionaryEntriesForTS)
> * 1.4.4.7 ``Constant Value'' 字典条目部分(#ConstantValue)
> * 1.4.4.8 ``Description'' 字典条目部分(#DescriptionSDE)
> * 1.4.4.9 ``Examples'' 字典条目部分(#ExamplesSDE)
> * 1.4.4.10 ``Exceptional Situations'' 字典条目部分(#ExceptionalSituationsSED)
> * 1.4.4.11 ``Initial Value'' 字典条目部分(#InitialValueSDE)
> * 1.4.4.12 ``Argument Precedence Order'' 字典条目部分(#ArgumentPrecedenceOrderSDE)
> * 1.4.4.13 ``Method Signature'' 字典条目部分(#MethodSignatureSDE)
> * 1.4.4.14 ``Name'' 字典条目部分(#NameSDE)
> * 1.4.4.15 ``Notes'' 字典条目部分(#NotesSDE)
> * 1.4.4.16 ``Pronunciation'' 字典条目部分(#PronunciationSDE)
> * 1.4.4.17 ``See Also'' 字典条目部分(#SeeAlsoSDE)
> * 1.4.4.18 ``Side Effects'' 字典条目部分(#SideEffectsSDE)
> * 1.4.4.19 ``Supertypes'' 字典条目部分(#SupertypesSDE)
> * 1.4.4.20 ``Syntax'' 字典条目部分(#SyntaxSDE)
> * 1.4.4.21 ``Valid Context'' 字典条目部分(#ValidContextSDE)
> * 1.4.4.22 ``Value Type'' 字典条目部分(#ValueTypeSDE)

#### 1.4.4.1 <span id = "AffectedBySDE">``Affected By'' 字典条目部分</span>

对于一个操作符, 它可以影响副作用或者返回值.

对于一个变量, 对它绑定或赋值都可能影响这个变量的值包括函数.

#### 1.4.4.2 <span id = "ArgumentsSDE">``Arguments'' 字典条目部分</span>

这个信息描述那些不作为表达式形式求值也没有返回值的声明还有特殊表达式条目的语法信息的条目.

#### 1.4.4.3 <span id = "ArgumentsAndValuesSDE">``Arguments and Values'' 字典条目部分</span>

这是对操作符接收参数还有返回值的英语描述, 包括遗漏的默认参数的信息 (就像optional参数和关键字参数). 对于特殊操作符和宏, 它们的参数不会被求值除非在它们的描述中就明确指定它们被求值.

除了明确指定以外, 如果这些类型约束被违反了, 那么结果将是未定义的.

#### 1.4.4.4 <span id = "BindingTypesAffectedSDE">``Binding Types Affected'' 字典条目部分</span>

这个信息警告读者这个类型的绑定可能被一个声明所影响. 事实上任何这样的绑定是否被影响取决于额外因素. 详情见 ``Description'' 章节中问题的声明.


#### 1.4.4.5 <span id = "ClassPrecedenceListSDE">``Class Precedence List'' 字典条目部分</span>

这个出现在类的字典条目, 并且包含一个Common Lisp定义的类的列表<!-- TODO 这个列表是什么 -->, 必须存在这个类的优先级列表中.

其他依赖于具体实现的类出现在实现的类的优先级列表中是允许的.

标准对象或者结构对象出现在类的优先级列表中也是允许的; 详情见章节 4.2.2 (Type Relationships).

除非在这个说明书里明确说明, 没有标准的类出现在具体实现的类的优先级列表中.

根据类和类型之间的关系定义, 这节列出来的类也是对应类的子类.

#### 1.4.4.6 <span id = "DictionaryEntriesForTS">Dictionary Entries for Type Specifiers</span>

这个原子类型说明符是那些 Figure 4-2 列出的定义的名字. 这个字典条目是 \``Class,'' \``Condition Type,'' \``System Class,'' 或者 \``Type'' 类型的. 一个如何去把这些类型或类解释为原子类型说明符的描述在每个字典条目的 \``Description'' 章节.

复合类型说明符是那些 Figure 4-3 列出的名字. 这样的字典条目是 \``Class,'' \``System Class,'' \``Type,'' 或 \``Type Specifier'' 类型的. 如何去解释复合类型列表的描述是看它的car部分是 \``Compound Type Specifier Kind,'' \``Compound Type Specifier Syntax,'' \``Compound Type Specifier Arguments,'' 还有 \``Compound Type Specifier Description'' 中哪个字典条目章节中的符号.

> * 1.4.4.6.1 ``Compound Type Specifier Kind'' 字典条目部分(#CTSKindSDE)
> * 1.4.4.6.2 ``Compound Type Specifier Syntax'' 字典条目部分(#CTSSyntaxSDE)
> * 1.4.4.6.3 ``Compound Type Specifier Arguments'' 字典条目部分(#CTSArgumentsSDE)
> * 1.4.4.6.4 ``Compound Type Specifier Description'' 字典条目部分(#CTSDescriptionSDE)

##### 1.4.4.6.1 <span id = "CTSKindSDE">``Compound Type Specifier Kind'' 字典条目部分</span>

一个 \``abbreviating'' 类型说明符描述了原则上可以枚举出来但是实践上没有可操作性的一个图表.

一个 \``specializing'' 类型说明符描述了约束了类型中一个或多个子类型的图表, 就像元素类型或者复数部分类型.

一个 ``predicating'' 类型说明符描述了一个只包含满足给定断言的那些对象的图表.

一个 \``combining'' 类型说明符描述一种以组合的方式的图表, 通过在其他类型上使用组合操作符 (就像 \``and,'' \``or,'' 和 \``not'') .

##### 1.4.4.6.2 <span id = "CTSSyntaxSDE">``Compound Type Specifier Syntax'' 字典条目部分</span>

这类信息描述了复合类型说明符的语法.

这个类型是否可以作为原子类型说明符在这里没有说明; 见章节 1.4.4.6 (Dictionary Entries for Type Specifiers).

##### 1.4.4.6.3 <span id = "CTSArgumentsSDE">``Compound Type Specifier Arguments'' 字典条目部分</span>

这个描述了定义在 ``Compound Type Specifier Syntax'' 章节的结构的类型信息.

##### 1.4.4.6.4 <span id = "CTSDescriptionSDE">``Compound Type Specifier Description'' 字典条目部分</span>

这个描述了定义在 ``Compound Type Specifier Syntax'' 章节的结构的意义.

#### 1.4.4.7 <span id = "ConstantValue">``Constant Value'' 字典条目部分</span>

这个描述了一个常变量的不变的类型和值.

#### 1.4.4.8 <span id = "DescriptionSDE">``Description'' 字典条目部分</span>

这个操作符还有它的所有预期方面的一个总结, 但是没有必要包含在它后面引用的所有域(\``Side Effects,'' \``Exceptional Situations,'' 等等.)

#### 1.4.4.9 <span id = "ExamplesSDE">``Examples'' 字典条目部分</span>

这个操作符的使用示例. 这些示例不被认为是这个标准的一部分; 见章节 1.4.3 (Sections Not Formally Part Of This Standard).

#### 1.4.4.10 <span id = "ExceptionalSituationsSED">``Exceptional Situations'' 字典条目部分</span>

三种类型的信息可以出现在这里:

    被这个函数检测到且发出来的情况.
    这个函数处理的情况.
    可能被这个函数检测到的情况.

<!-- TODO 解析不了，待请教 -->
This field does not include conditions that could be signaled by functions passed to and called by this operator as arguments or through dynamic variables, 如果是宏或特殊操作符, 也不包括这个操作符的执行子形式nor by executing subforms of this operator if it is a macro or special operator.

#### 1.4.4.11 <span id = "InitialValueSDE">``Initial Value'' 字典条目部分</span>

这个信息描述了一个动态变量的初始值. 因为这个变量可能会改变, 见 ``Value Type'' 章节的类型限制.

#### 1.4.4.12 <span id = "ArgumentPrecedenceOrderSDE">``Argument Precedence Order'' 字典条目部分</span>

这个信息描述了参数优先级顺序. 如果它被省略, 参数的优先级顺序就是默认的 (从左到右).

#### 1.4.4.13 <span id = "MethodSignatureSDE">``Method Signature'' 字典条目部分</span>

这个广义函数的描述包括了这个标准广义函数定义的方法描述. 一个方法签名被用于描述每一个方法的参数和 parameter specializers. 广义函数定义的方法必须是方法签名所描述表达式形式.

F (x class) (y t) &optional z &key k

这个签名表示这个广义函数F的方法有两个需要的参数: 必须是类class的实例的x; 还有可以是任何对象的 y (换句话说, 可以类 t 的泛化实例). 另外, 这里有一个可选的参数z 和一个关键字参数k. 这个签名也表示这个方法F是一个主方法并且没有修饰. <!-- TODO 修饰？？ -->

对于每个参数, 提供的实参必须是对应的广义函数描述的类型和方法签名里的类型的交集 (不仅仅是这个说明书里定义的方法, 也包括在允许定义方法的地方具体实现定义的或者用户定义的).

#### 1.4.4.14 <span id = "NameSDE">``Name'' 字典条目部分</span>

这个章节介绍字典条目. 它不是明确标记的. 它出现在前面并且后面跟一个水平栏.

在左边打印的是定义的名字; 如果这个条目不只一个定义的名字, 所有这样的名字都会显示出来, 通过逗号分割.

在右边打印的小的斜体的是这个字典条目是什么种类的介绍. 可能的值是:

Accessor

    这个是访问器函数.

Class

    这是一个类.

Condition Type

    这个是类型condition的表.

Constant Variable

    这是一个常变量.

Declaration

    这是一个定义标识符.

Function

    这是一个函数.

Local Function

    这是一个定义在宏形式里的词法作用域里的函数.

Local Macro

    这是定义在一些宏形式的词法作用域里的宏.

Macro

    这是一个宏.

Restart

    这是一个重启.

Special Operator

    这是一个特殊操作符.

Standard Generic Function

    这是一个标准的广义函数.

Symbol

    这是一个在特定场合被识别的符号, 就像宏的语法中.

System Class

    这是一个类, 但是它表示内置的类. (事实上没有类需要去成为内置类.)

Type

    这是一个原子类型分类符, 并且依赖于每一个特定条目的信息, 可能去组成其他类型分类符.

Type Specifier

    这是一个定义的不是原子类型分类符的名字, 但是可以被用于构建合法类型分类符.

Variable

    这是一个动态变量.


#### 1.4.4.15 <span id = "NotesSDE">The ``Notes'' 字典条目部分</span>

在其他地方没有的关于这个操作符的信息. 在其他东西中, 这个可能包含了跨引用信息, 代码等价, 文体暗示, 实现暗示, 典型使用. 这个信息不被认为是这标准的一部分; 任何确定的实现和程序允许忽略这个信息.

#### 1.4.4.16 <span id = "PronunciationSDE">``Pronunciation'' 字典条目部分</span>

这个为定义的名字提供了一个建议的发音方式, 这样人们没有在和最初的设计者们一起交流的情况下也能弄明白这个没有出现在正常英语中的单词如何发音. 这个信息是劝告性的, 不被认为是这个标准的一部分. 为了简洁性, 它只提供给特定Common Lisp名字的条目并且不会出现在未删减的 Webster's Third New International Dictionary the English Language.

#### 1.4.4.17 <span id = "SeeAlsoSDE">``See Also'' 字典条目部分</span>

这个标准中提供有关这个操作符信息的引用列表. 这个列表不是这个标准的一部分.

#### 1.4.4.18 <span id = "SideEffectsSDE">The ``Side Effects'' 字典条目部分</span>

包含这个操作符的表达式形式求值时作为结果的任何东西的改变.

#### 1.4.4.19 <span id = "SupertypesSDE">The ``Supertypes'' 字典条目部分</span>

这个出现在一个类型的字典条目中, 包含了这个类型的标准化基类的列表.

在具体实现中对应一个类的地方, 在类的优先级列表中的类顺序和在这个章节中出现的顺序是一致的.

#### 1.4.4.20 <span id = "SyntaxSDE">``Syntax'' 字典条目部分</span>

这个章节描述了如何在代码中使用定义的名字. 关于广义函数的 \``Syntax'' 部分描述了广义函数自身的lambda列表, 而 \``Method Signatures'' 描述了定义的方法的lambda列表. 关于一般函数, 宏, 或者一个特殊操作符 \``Syntax'' 描述了他们的参数.

比如, 一个操作符描述可能是:

F x y &optional z &key k

这个描述表示函数F有两个必须的参数, x 和 y. 另外, 这里还有个可选的参数 z 和一个关键字参数 k.

关于宏和特殊操作符, 语法已经通过修改的 BNF 标注给了; 见章节 1.4.1.2 (Modified BNF Syntax). 关于函数给定一个lambda列表. 然而在两种情况下, 最外层的括号和默认值信息省略了.

> * 1.4.4.20.1 重载操作符特殊的 ``Syntax'' 表示法(#SpecialSyntaxNotations)
> * 1.4.4.20.2 剩余参数的命名转化(#NamingConventions)
> * 1.4.4.20.3 在 ``Syntax'' 部分需要非空剩余参数(#RequiringNonNullRestParameters)
> * 1.4.4.20.4 在 ``Syntax'' 部分的返回值(#ReturnValuesInSyntaxSection)

##### 1.4.4.20.1 <span id = "SpecialSyntaxNotations">重载操作符特殊的 ``Syntax'' 表示法</span>

如果同一个操作符带有不同数量的参数有两个描述, 那么额外的参数就被认为是可选的. 比如, 这两行:

file-position stream => position

file-position stream position-spec => success-p

操作上等价于:

file-position stream &optional position-spec => result

并且不同点仅在于为每一个情况提供不同的参数和值的名字来介绍. 这个分开的 (多行) 表示法被用于当操作符以这种方式被重载, 并且参数根据提供的参数数量被用于不同的方式 (比如, 函数 /) 或者两种情况下返回值不同 (比如, 函数 file-position).

##### 1.4.4.20.2 <span id = "NamingConventions">剩余参数的命名转化</span>

在这个规格书中, 如果剩余参数的名字选择为复数名词, 这个名字表示剩余参数绑定的列表. 使用前面指定的名字的单数形式表示这个列表中的一个元素.

比如, 给定一个像这样的语法描述:

F &rest arguments

可以通过名字 arguments 引用这些剩余参数, 或者通过 \``an argument,'' \``some argument,'' \``each argument'' 等等来引用其中一个.

##### 1.4.4.20.3 <span id = "RequiringNonNullRestParameters">在 ``Syntax'' 部分需要非空剩余参数</span>

在一些情况下同时需要一个以上的参数以剩余参数作为单个聚集体引用所有参数是很有用的. 多种必要的宣称意味着在代码中表示这种约束是可以的, 然而它们通常不在lambda列表中显示自己. 在这个文档中为了表述目的,

F &rest arguments+

意味着和下面的相等

F &rest arguments

但是表示另外的意思: 这里至少需要一个参数.

##### 1.4.4.20.4 <span id = "ReturnValuesInSyntaxSection">在 ``Syntax'' 部分的返回值</span>

一个求值箭头 ``=> '' 表示前面的表达式返回的值. 比如:

F a b c => x

表示这个 F 是一个操作符, 它需要三个参数 (换句话说, a, b, 还有 c) 以及返回一个值 (就是 x). 如果不止一个值被这个操作符返回, 这些值的名字会用逗号区分, 就像:

F a b c => x, y, z

> * 1.4.4.20.4.1 在 ``Syntax'' 部分没有参数和返回值
> * 1.4.4.20.4.2 在 ``Syntax'' 部分控制无条件转移

###### 1.4.4.20.4.1 在 ``Syntax'' 部分没有参数和返回值

如果没有参数也没有返回值, 一个特别的标记被用于使这个更显而易见. 比如,

F <no arguments> => <no values>

表示这个 F 操作符不接受参数和返回值.

###### 1.4.4.20.4.2 在 ``Syntax'' 部分控制无条件转移

一些操作符会有控制无条件转移的动作, 并且没有任何返回值. 这样的操作符通过以下方式表示:

F a b c =>|


#### 1.4.4.21 <span id = "ValidContextSDE">``Valid Context'' 字典条目部分</span>

这个信息被用于例如 ``Declarations'' 字典条目, 用于约束这个声明可能出现的上下文.

一个给定的 ``Declaration'' 可能出现在一个声明 (换句话说, 一个声明表达式), 一个宣告 (换句话说, a declaim or proclaim form), 或者都有.

#### 1.4.4.22 <span id = "ValueTypeSDE">``Value Type'' 字典条目部分</span>

这个信息描述了一个动态变量的任何类型约束条件.
除非明确指定外, 违反这个类型约束时结果是不可预测的.


## 1.5 Conformance

这个标准提出了一个合格实现需要去实现的语法和语义 (以及它的附加文档). 另外, it imposes requirements on conforming programs. <!-- TODO -->

> * 1.5.1 合格的实现(#ComformingImpl)
> * 1.5.2 合格的程序(#ComformingProg)

 
### 1.5.1 <span id = "ComformingImpl">合格的实现</span>

一个合格的实现应该遵守这个章节中所述的要求.

> * 1.5.1.1 需要的语言特性(#RequiredLanguageFeatures)
> * 1.5.1.2 依赖具体实现的特性文档(#DocImplDepFeatures)
> * 1.5.1.3 扩展文档(#DocExtensions)
> * 1.5.1.4 异常情况的处理(#TreatmentExceptionalSituations)
> * 1.5.1.5 一致性声明(#ConformanceStatement)

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

#### 1.5.1.5 <span id = "ConformanceStatement">一致性声明</span>

一个符合规范的实现应该提供一个一致性声明作为使用这个实现的结果, 或者在附带的文档中加入这个声明. 如果这个具体实现符合这个标准的所有方面, 这个一致性说明应该为

``<\<Implementation>> conforms with the requirements of ANSI <\<standard number>>''

如果这个实现符合这个标准中的一部分并非全部, 这个说明应该为

``<\<Implementation>> conforms with the requirements of ANSI <\<standard number>> with the following exceptions: <\<reference to or complete list of the requirements of the standard with which the implementation does not conform>>.'' 

### 1.5.2 <span id = "ComformingProg">合格的程序ssss</span>

符合这个规范的代码应该坚持下面几条:

    合格的代码应该只使用定义在这个标准中的语言语法和语义特性或者这个标准中扩展机制指定的.

    合格的代码可能使用依赖实现的特征和值, 但是不应该依赖于任何这些特征和值的特别解释而不是那些在代码的执行中发现的.

    合格的代码不应依赖未定义或者未指定情况的结果.

    合格的代码不使用这个标准禁止的任何结构.

    合格的代码不依赖于一个具体实现的扩展.

> * 1.5.2.1 具体实现定义的语言特征的使用(#UseImplDefLangFeature)
> * 1.5.2.2 可移植代码的字符集(#CharsetForPortCode)


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
