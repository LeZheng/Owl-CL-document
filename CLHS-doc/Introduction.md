 # 1. 引言

> * 1.1 [面向人群, 目的, 和历史](#ScopePurposeAndHistory)
> * 1.2 [文档的组织结构](#OrganizationOfTheDocument)
> * 1.3 [参考的出版物](#ReferencedPublications)
> * 1.4 [Definitions](#Definitions)
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