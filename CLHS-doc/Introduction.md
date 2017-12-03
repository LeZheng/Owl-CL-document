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
>>其中一个最重要的Lisp发展是在20世纪70年代后半期: Scheme. Scheme, 是Gerald J. Sussman 和 Guy L. Steele Jr. 设计, 是一个简洁的Lisp方言，从20世纪60年代以来这里的设计带给Lisp很多编程语言语义上的理念. Sussman是20世纪60年代末到70年代很多Lisp技术其他进展背后最初的变革者之一.Scheme主要的贡献是词法作用域, 词法闭包, first-class continuations, 还有简单的语法 (没有区分值 cell 和 函数 cell). 这些贡献中很多对Common Lisp的设计产生了很大影响. 关于Scheme的更多信息, 见 IEEE Standard for the Scheme Programming Language 或者 ``Revised^3 Report on the Algorithmic Language Scheme.'' [>_<]:待核对
>>
>>在20世纪70年代末期,面向对象编程概念对Lisp产生了很大影响. 在 MIT, 来自于 Smalltalk 的思想径直进入很多广泛使用的编程系统中. Flavors, 一个带有多继承的面向对象系统, 为了Lisp machine社区在 MIT 被 Howard Cannon 和 其他人开发出来. At Xerox,  Smalltalk 的经验和 Knowledge Representation Language (KRL) 引导了 Lisp Object Oriented Programming System (LOOPS) 还有后来的 Common LOOPS的发展. 关于跟多 Smalltalk 的信息, 见 Smalltalk-80: The Language and its Implementation. 关于Flavors更多信息, 见 Flavors: A Non-Hierarchical Approach to Object-Oriented Programming.
>>
>>这些系统影响了Common Lisp Object System (CLOS)的设计. CLOS 是为了标准化特地开发的, 并且写在了 ``Common Lisp Object System Specification.'' 然而, 在公布之后它的设计中小部分细节有了轻微的改变, 并且就像这个文档中所说,这个文章不应该被当作一份这个对象系统语义上的官方说明.
>>
>>在1980 Symbolics 和 LMI 发展了 Lisp Machine Lisp; stock-hardware 实现小组发展了 NIL, Franz Lisp, 还有 PSL; Xerox 发展了 Interlisp; 还有 CMU 的 SPICE 项目发展了称之为 SpiceLisp的类MacLisp方言.
>>
>>在1981年四月, 在一个 DARPA赞助的会议后分裂的Lisp社区, Symbolics, SPICE 项目, NIL 项目, 还有 S-1 Lisp 项目一起加入来定义 Common Lisp. 开始由 White 和 Gabriel 带领, 这个基层民众的尝试背后的驱动力由 Fahlman, Daniel Weinreb, David Moon, Steele, 和 Gabriel 提供. Common Lisp 被设计为一个家族语言的描述. 对于Common Lisp主要的影响是 Lisp Machine Lisp, MacLisp, NIL, S-1 Lisp, Spice Lisp, 还有 Scheme. Common Lisp: 这个语言是那个设计的描述. 部分地方它的语义是有意指定的因为那里感觉严格规范会限制Common Lisp的研究和使用.[>_<]:待核对
>>
>>在1986年 X3J13 组织了一个技术小组去起草 ANSI Common Lisp 标准. 由于 Common Lisp的接纳, 这个小组的目标与最初的设计者不同. 这些目标包括更严格的移植性标准化,一个面向对象编程系统,一个状态系统,循环机制,以及一种处理大型字符集的方式.为了促成这个目标，一个新的语言说明(这个文档)被开发出来.

## 1.2 <span id = "OrganizationOfTheDocument">文档的组织结构</span>

> 这是一份参考文档, 不是一份教程文档. 为了尽可能方便, 这个报告的顺序是原始话题在前, 构建于它们之上的在后; 然而, 线性的阅读性并不是值得优先考虑的.
>
> 这个文档根据话题来拆分章节. 任何给定章节可能包含概念性内容或字典条目, 或者都有.
>
> 一个章节的字典部分把主题相关的已定义的名字分组后放得接近一些. 很多这样的分组是可能的，并且应该没有从分组中推断出的深层意义. 为了看到以字母顺序排列分组的定义的名字, 可以查阅索引. 关于定义名字的完整列表, 见章节 1.9 (Symbols in the COMMON-LISP Package).[>_<]:待核对
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

* ``Common Lisp Object System Specification,'' D. Bobrow, L. DiMichiel, R.P. Gabriel, S. Keene, G. Kiczales, D. Moon, SIGPLAN Notices V23, September, 1988.

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

> * 1.1 [符号约定](#NotationalConventions)
> * 1.2 [错误术语](#ErrorTerminology)
> * 1.3 [本标准未正式规定的部分](#SectionsNotFormallyPartOfThisStandard)
> * 1.4 [解释的字典条目](#InterpretingDictionaryEntries)

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

这里的术语 \``clause'' 似乎是 \``dead'' 因为它没有被使用于上面的BNF表达式里. 然而, 这个BNF的目的并不只是引导解析, 但是也定义有用的术语为了跟在后面的描述性文本的参考. 像这个样子, 术语 \``clause'' 可能出现在后面跟着的文本中, 作为 \``normal-clause or otherwise-clause.'' 的速记.[>_<]待校验


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

    但是它的意图是提供额外的信息去说明这些结果中可能会根据实现而不同.[>_<]待验证

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

比如, \``nil'' 和 \``the value of *standard-output*'' 作为流 designators 是操作上难以区分的 . 类似的, 作为string designators 符号 foo 和字符串 "FOO" 也是难以区分的.

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
