# 19 文件名

> * 19.1 [文件名概述](#OverviewFilenames)
> * 19.2 [Pathnames](#Pathnames)
> * 19.3 [逻辑路径名](#LogicalPathnames)
> * 19.4 [The Filenames Dictionary](#TheFilenamesDictionary)


## 19.1 <span id="OverviewFilenames">文件名概述</span>

这里有很多种文件系统, 在它们的表面语法细节, 以及它们的潜在的能力和结构上都有很大的不同. 由 Common Lisp 提供的用于引用和操作文件的工具被选择与许多类型的文件系统兼容, 同时最小化了不同文件系统之间的程序可见差异.

因为文件系统在命名文件的约定上有区别, 这里有两种不同的方式来表示文件名: 作为名称字符串(namestring)和作为路径名(pathname).

> * 19.1.1 [名称字符串作为文件名](#NamestringsFilenames)
> * 19.1.2 [路径名作为文件名](#PathnamesFilenames)
> * 19.1.3 [解析名称字符串为路径名](#ParseNamestrIntoPathnames)


### 19.1.1 <span id="NamestringsFilenames">名称字符串作为文件名</span>

一个名称字符串(namestring)是一个表示一个文件名的字符串.

通常, 名称字符串的语法涉及到使用具体实现定义的约定, 通常是指所命名文件所在的文件系统的惯例. 仅有的例外是一个逻辑路径名的名称字符串的语法, 它被定义在这个规范中; 见章节 19.3.1 (逻辑路径名名称字符串的语法).

一个符合规范的程序永远不能无条件地使用一个字面化的名称字符串, 而不是一个逻辑路径名的名称字符串因为 Common Lisp 没有定义任何保证可移植的名称字符串的语法, 除了逻辑路径名的名称字符串. 然而, 一个符合规范的程序, 如果足够小心, 可以成功地操纵用户提供的包含或引用不可移植的名称字符串的数据.

一个名称字符串可以通过函数 pathname 或 parse-namestring 强制转为路径名. 


### 19.1.2 <span id="PathnamesFilenames">路径名作为文件名</span>

路径名是结构化的对象, 可以用一种独立于具体实现的方式表示底层文件系统本地使用的文件名.

另外, 路径名还可以表示某些部分组成的文件名, 其中底层文件系统可能没有特定的名称字符串表示.

一个路径名不需要对应任何实际上已存在的文件, 并且多个路径名可以引用相同文件. 比如, 这个带有一个 :newest 的版本可能和一个带有相同成分除了某个数字作为版本的路径名引用相同的文件. 确实, 随着时间推移一个带有版本 :newest 的路径名可能引用不同的文件, 因为这样一个路径名的意义取决于文件系统的状态.

某些文件系统自然地为它们的文件名使用一个结构模型, 而其他的则没有. 在 Common Lisp 路径名模型中, 所有文件名都被视作有着一个特定结构, 即便那个结构没有反映在底层文件系统中. 由路径名暗示的结构和底层文件系统所使用的结构(如果有的话)之间的映射性质是具体实现定义的.

每一个路径名都有六个成分: 一个主机(host), 一个设备(device), 一个目录(directory), 一个名字(name), 一个类型(type), 和一个版本(version). 通过用路径名来命名文件, 即使在文件系统中看起来很不一样, Common Lisp 程序可以以相同的方式工作. 关于这些成分的详细描述, 见章节 19.2.1 (Pathname Components).

这些路径名成分到每个文件系统的特有概念的映射是具体实现定义的. 存在可想到的路径名, 在特定的实现中, 没有映射到语法有效的文件名. 一个具体实现可能使用多种策略去尝试找到一个映射; 例如, 一个实现可能会悄悄地截断超过底层文件系统施加的长度限制的文件名, 或者忽略文件系统不提供支持的某些路径名成员. 如果没有找到这样一个映射, 就会发出一个 file-error 类型的错误.

这个映射和关联的错误发出的时间是依赖于具体实现的. 具体来说, 它可能出现在这个路径名被构造时, 吧一个路径名强制转为名称字符串时, 或者尝试去打开或访问这个路径名表示的文件时.

下面这段列出了一些已定义的可应用于路径名的名字.

    *default-pathname-defaults*  namestring          pathname-name          
    directory-namestring         open                pathname-type          
    enough-namestring            parse-namestring    pathname-version       
    file-namestring              pathname            pathnamep              
    file-string-length           pathname-device     translate-pathname     
    host-namestring              pathname-directory  truename               
    make-pathname                pathname-host       user-homedir-pathname  
    merge-pathnames              pathname-match-p    wild-pathname-p        

    Figure 19-1. 路径名操作

### 19.1.3 <span id="ParseNamestrIntoPathnames">解析名称字符串为路径名</span>

解析是一个被用于转换一个名称字符串为一个路径名的操作. 除了在解析逻辑路径名名称字符串时, 这个操作是依赖于具体实现的, 因为这个名称字符串的格式是依赖于具体实现的.

一个符合规范的实现可以在它的路径名表示中自由地包含其他文件系统特性并且提供一个可以访问名称字符串中这样的参数的解析器. 符合规范的程序一定不能依赖这样的特性, 因为这些特性不是可移植的. 

## 19.2 <span id="Pathnames">Pathnames</span>

> * 19.2.1 [路径名成员](#PathnameComponents)
> * 19.2.2 [解释路径名成员值](#InterpretPathnameCompValues)
> * 19.2.3 [合并路径名](#MergingPathnames)

### 19.2.1 <span id="PathnameComponents">路径名成员</span>

一个路径名有六个成员: 一个主机(host), 一个设备(device), 一个目录(directory), 一个名字(name), 一个类型(type), 和一个版本(version).

#### 19.2.1.1 路径名主机成员

文件所在的文件系统的名称，或者逻辑主机的名称.

#### 19.2.1.2 路径名设备成员

在许多主机文件系统中对应于"设备"或"文件结构"概念: 包含这些文件的逻辑或物理设备的名字. 

#### 19.2.1.3 路径名目录成员

在许多主机文件系统中对应于"目录"概念: 一组相关文件的名字. 

#### 19.2.1.4 路径名名字成员

一组可以被认为是概念上相关的文件的"名称"部分. 

#### 19.2.1.5 路径名类型成员

在许多主机文件系统中对应于"文件类型"或"扩展"概念. 这说明了这是什么类型的文件. 这个成员总是为一个字符串, nil, :wild, 或 :unspecific. 


#### 19.2.1.6 路径名版本成员

对应于许多主机文件系统中的"版本号"概念.

这个版本是一个整数或者是下面列表中的一个符号: nil, :wild, :unspecific, 或 :newest (指在读取文件时, 文件系统中已经存在的最大版本号, 或者在编写新文件时比文件系统中已有的版本号更大的版本号). 具体实现可以定义其他特别的版本符号. 


### 19.2.2 <span id="InterpretPathnameCompValues">解释路径名成员值</span>

> * 19.2.2.1 [成员值中的字符串](#StringsComponentValues)
> * 19.2.2.2 [特殊路径名成员值](#SpecPathnameComponentValues)
> * 19.2.2.3 [通配符路径名上的限制](#RestrictionWildcardPathnames)
> * 19.2.2.4 [检查路径名成员的限制](#RestrictionExamPathnameComponents)
> * 19.2.2.5 [构造路径名的限制](#RestrictConstructPathnames)

#### 19.2.2.1 <span id="StringsComponentValues">成员值中的字符串</span>

##### 19.2.2.1.1 路径名成员中的特殊字符

路径名成员值中的字符串永远不会包含特殊字符, 这些字符表示路径名字段之间的间隔, 比如 Unix 文件名中的斜杠. 分离字符是否允许作为一个路径名成员中字符串的一部分是具体实现定义的; 然而, 如果实现确实允许这个, 它必须在构造一个名称字符串时, 安排正确地"引用"文件系统的字符. 例如,

```LISP
 ;; In a TOPS-20 implementation, which uses ^V to quote 
 (NAMESTRING (MAKE-PATHNAME :HOST "OZ" :NAME "<TEST>"))
=>  #P"OZ:PS:^V<TEST^V>"
NOT=>  #P"OZ:PS:<TEST>"
```

##### 19.2.2.1.2 路径名成员中的大小写

名称字符串总是使用本地文件系统的大小写约定, 但是操纵路径名成员的 Common Lisp 函数允许调用者通过给 :case 参数提供一个值去选择两种表示成员值大小写的约定中的任意一种. 下一段列出了和允许 :case 参数的路径名相关函数:

    make-pathname    pathname-directory  pathname-name  
    pathname-device  pathname-host       pathname-type  

    Figure 19-2. 使用 :CASE 参数的路径名函数

###### 19.2.2.1.2.1 路径名成员中的本地大小写

对于 Figure 19-2 中的函数, :case 参数的一个 :local 值 (对于这些函数的默认值) 表示这些函数应该接受并产生成员值中的字符串, 就好像它们已经根据主机文件系统的大小写约定来表示了一样.

如果这个文件系统两种大小写都支持, 在这个协议下给定或接收的字符串作为路径名成员值将被完全使用. 如果这个文件系统只支持一种大小写, 这个字符串会被转成那个大小写. 


###### 19.2.2.1.2.2 路径名成员中的通用大小写

对于 Figure 19-2 中的函数, :case 参数的一个 :common 值表示这些函数应该根据以下约定接受和产生成员值中的字符串:

* 所有都是大写意味着使用一个文件系统习惯的大小写.
* 所有都是小写字母表示使用与习惯情况相反的方法.
* 混合大小写就表示自身.

注意, 这些约定可以以这样一种方式被选择, 从 :local 转换到 :common 并转换回 :local 信息保持不变. 


#### 19.2.2.2 <span id="SpecPathnameComponentValues">特殊路径名成员值</span>

##### 19.2.2.2.1 NIL 作为一个成员值

作为一个成员值, nil 表示这个成员是"没有被填充"; 见章节 19.2.3 (合并路径名).

任何路径名成员值可以是 nil.

在构造一个路径名时, 在主机成员的 nil 可能意味着一个默认主机而不是在某些实现中的一个实际的 nil. 

##### 19.2.2.2.2 :WILD 作为一个成员值

如果 :wild 是一个路径名成员的值, 那个成员被认为是一个通配符, 它可以匹配任何东西.

一个符合标准的程序必须准备好遇到一个值: :wild 作为任何路径名成员的值, 或者作为目录成员的值的列表的元素.

在构造一个路径名时, 一个符合规范的程序可能使用 :wild 作为目录, 名字, 类型, 或版本成员的其中一个或所有的值, 但是一定不能使用 :wild 作为主机, 或设备成员的值.

在构造一个路径名时如果 :wild 被用作这个目录成员的值, 效果等价于指定 (:absolute :wild-inferiors), 或者等价于一个不支持 :wild-inferiors 的文件系统的 (:absolute :wild). 

##### 19.2.2.2.3 :UNSPECIFIC 作为成员值

如果 :unspecific 是一个路径名的成员值, 这个成员就被认为是"缺失的"或者在这个要被这个路径名表示的文件名中"没有意义的".

在这个实现可以访问的任何给定文件系统的任何成员上一个 :unspecific 值是否被允许是具体实现定义的. 一个符合规范的程序一定不能无条件使用一个 :unspecific 作为一个路径名成员的值, 因为这样一个值不保证在所有实现都是允许的. 然而, 一个符合规范的程序, 如果足够小心, 可以成功地操纵用户提供的包含或引用不可移植的路径名成员的数据. 当然, 一个符合规范的程序应该为一个路径名的任何成员都可能是 :unspecific 的可能性做好准备.

当读取任何路径名成员的值时, 符合规范的程序应该对值为 :unspecific 有所准备.

当写入任何路径名成员的值时, 如果 :unspecific 在文件系统中被赋予给一个路径名, 那么其后果是没有定义的, 因为它没有意义.

###### 19.2.2.2.3.1 成员值 NIL 和 :UNSPECIFIC 之间的联系

如果一个路径名被转换为一个名称字符串, 返回 nil 和 :unspecific 导致要被处理的那个域就像是空的一样. 这也就是说, nil 和 :unspecific 都导致这个成员不会出现在名称字符串中.

然而, 在合并一个路径名和一个默认值集合时, 只有一个成员的 nil 值会被那个成员的默认值替换, 而一个 :unspecific 值会被留下来就好像这个域已经被"填充"了; 见函数 merge-pathnames 和章节 19.2.3 (合并路径名). 


#### 19.2.2.3 <span id="RestrictionWildcardPathnames">通配符路径名上的限制</span>

通配符路径名可以和 directory 一起使用, 但是不能和 open 一起使用, 并且从 wild-pathname-p 返回 true. 在检查通配符路径名的通配符成员时, 符合规范的程序必须准备好在任何成员或目录成员的任何元素中遇到下列附加值:<!--TODO 待校验-->

* 符号 :wild, 它匹配任何东西.

* 一个包含依赖于具体实现的特殊通配符的字符串.

* 任何对象, 表示一个依赖于具体实现的通配符模式. 


#### 19.2.2.4 <span id="RestrictionExamPathnameComponents">检查路径名成员的限制</span>

一个符合条件的程序必须准备作为路径名成员的值来读取的可能的对象的空间, 要比一个符合条件的程序被允许写入这样一个成员的可能对象的空间大得多.

尽管在这个章节的子章节, 在章节 19.2.2.2 (特殊路径名成员值), 还有章节 19.2.2.3 (通配符路径名上的限制) 中讨论的值可以被应用于读取成员值时可见的值, 而对于构造路径名, 则应用了更多的限制性规则; 见章节 19.2.2.5 (构造路径名的限制).

在检查路径名成员时, 符合程序的程序应该知道以下限制.

##### 19.2.2.4.1 检查路径名主机成员的限制

什么对象被用于表示这个主机是依赖于具体实现的. 

##### 19.2.2.4.2 检查路径名设备成员的限制

这个设备可能是一个字符串, :wild, :unspecific, 或 nil.

注意, 这个 :wild 可能产生于读取这个路径名成员的尝试中, 尽管可移植程序被限制在写入这样的成员值时; 见章节 19.2.2.3 (通配符路径名上的限制) 以及章节 19.2.2.5 (构造路径名的限制). 


##### 19.2.2.4.3 检查路径名目录成员的限制

这个目录可能是一个字符串, :wild, :unspecific, 或 nil.

这个目录可以是一个字符串和符号的列表. 这个列表的 car 是符号 :absolute 或 :relative 其中之一, 表示:

:absolute

    一个 car 为符号 :absolute 的列表表示一个以根目录开始的目录路径. 列表 (:absolute) 表示根目录. 列表 (:absolute "foo" "bar" "baz") 表示 Unix 中的 "/foo/bar/baz" 目录 (除了可能的大小写下).

:relative

    一个 car 为符号 :relative 的列表表示一个以默认目录开始的目录路径. 列表 (:relative) 有着和 nil 相同的意义, 因此没有使用. 列表 (:relative "foo" "bar") 表示默认目录中名为 "foo" 的目录中的 "bar" 目录.

这个列表的每一个剩余元素都是一个字符串或符号.

每一个字符串命名目录结构中的单个层级. 这些字符串应该只包含目录名字自身---没有标点符号.

在这个列表的任何位置, 都可以使用符号来表示特殊的文件符号来替换一个字符串. 下面一段列出的符号有着标准意义. 如果有必要去表示不能用标准字符串和符号表示的文件系统的特性, 允许具体实现去添加和 string 互斥任意类型的额外对象.

给一个文件系统提供任何非字符串, 包含下面列出的任意符号, 如果它们对于这个文件系统是没有意义的, 那么就会发出一个 file-error 类型的错误. 比如, Unix 在大部分实现不支持 :wild-inferiors.

    符号           意义                                             
    :wild            目录结构中一级的通配符匹配
    :wild-inferiors  目录结构中任意数量的通配符匹配    
    :up              在目录结构中向上一级 (语义)         
    :back            在目录结构中向上一级 (语法)        

    Figure 19-3. 目录成员中的特殊标记

下面的注释适用于前面这段:

无效组合

    使用 :absolute 或 :wild-inferiors 后立即跟着 :up 或 :back 会发出一个 file-error 类型的错误.

语法 vs 语义

    "语法" 意味着那个 :back 动作只依赖于这个路径名, 不依赖这个文件系统的内容.

    "Semantic" 意味着那个 :up 的动作依赖于文件系统的内容; 为了解决一个包含了 :up 到一个目录成员只包含 :absolute 和字符串的路径名的路径名需要探索这个文件系统的问题.<!--TODO 待校对-->

    :up 和 :back 的区别仅在于文件系统支持多个多个目录名, 或许是通过符号链接. 例如, 假设这里有一个目录 (:absolute "X" "Y" "Z") 链接到 (:absolute "A" "B" "C") 并且这里也存在目录 (:absolute "A" "B" "Q") 和 (:absolute "X" "Y" "Q"). 那么 (:absolute "X" "Y" "Z" :up "Q") 表示 (:absolute "A" "B" "Q") 而 (:absolute "X" "Y" "Z" :back "Q") 表示 (:absolute "X" "Y" "Q")

###### 19.2.2.4.3.1 非分层文件系统中的目录成员

在非分层文件系统中, 一个路径名的目录成员的仅有的有效列表值是 (:absolute string) 和 (:absolute :wild). :relative 目录和关键字 :wild-inferiors, :up, 和 :back 在非分层文件系统中是不使用的. 


##### 19.2.2.4.4 检查路径名名称成员的限制

这个名称可以是一个字符串, :wild, :unspecific, 或 nil. 


##### 19.2.2.4.5 检查路径名类型成员的限制

这个类型可以是一个字符串, :wild, :unspecific, 或 nil. 


##### 19.2.2.4.6 检查路径名版本成员的限制

这个版本可以是任何符号或整数.

在读取, 覆盖, 追加, 代替, 或列出现有文件的目录时, 符号 :newest 引用已经存在于文件系统中最大的版本数字. 在创建一个新的文件时, 符号 :newest 引用大于任何已存在版本数字的最小的版本数字.

符号 nil, :unspecific, 和 :wild 有着特殊意义和限制; 见章节 19.2.2.2 (特殊路径名成员值) 和章节 19.2.2.5 (构造路径名的限制).

其他符号和整数有着具体实现定义的意义.


##### 19.2.2.4.7 关于路径名版本成员的注意事项

建议, 但不是必须, 具体实现执行以下操作:

* 使用从 1 开始的正整数作为版本数字.

* 识别符号 :oldest 来表示最小的已存在版本数字.

* 为其他特殊版本使用关键字. 


#### 19.2.2.5 <span id="RestrictConstructPathnames">构造路径名的限制</span>

从成员来构造一个路径名时, 符合规范的程序必须遵守这些规则:

* 任何成员可以是 nil. 在一些实现中, 主机中的 nil 可能意味着一个默认主机而不是一个实际的 nil.

* 主机, 设备, 目录, 名字, 以及类型可以是字符串. 在这些字符串中的字符的类型和数量上有着依赖于具体实现的限制.

* 目录可以是一个字符串和符号的列表. 在这个列表的长度和内容上有着依赖于具体实现的限制.

* 版本可以是 :newest.

* 任何成员都可以从另一个路径名的相应成员中获取. 当两个路径名是对于不同的文件系统时 (在支持多文件系统的实现中), 会出现一个适当的转换. 如果没有任何有意义的转换, 就会出现错误. 这个 "适当" 和 "有意义" 的定义是依赖于具体实现的.

* 对于某些成员, 一个实现可能支持其他的值, 但是一个可移植程序不能使用这些值. 一个符合规范的程序可以使用依赖于具体实现的值但是这会使它变得不可移植; 例如, 它可能只有在 Unix 文件系统中正常工作. 

### 19.2.3 <span id="MergingPathnames">合并路径名</span>

合并使用一个带有未填充成员的路径名, 并从默认值中为这些成员提供值.

如果一个成员的值是 nil, 那么那个成员会被认为是未填充的. 如果一个成员的值是任何非 nil 对象, 包括 :unspecific, 那么那个成员会被认为是已填充的.

除了显式指定的以外, 对于操作或查询文件系统中的文件的函数, 在访问这个文件系统前, 给这样一个函数的路径名参数会和 \*default-pathname-defaults* 合并(就像是通过 merge-pathnames).

#### 19.2.3.1 合并路径名的示例

虽然下面这些示例可能只有在允许 :unspecific 出现在指定位置并且允许四字母类型成员的实现中执行, 但是它们可以用来说明路径名合并的基本概念.

    ```LISP
    (pathname-type 
      (merge-pathnames (make-pathname :type "LISP")
                        (make-pathname :type "TEXT")))
    =>  "LISP"

    (pathname-type 
      (merge-pathnames (make-pathname :type nil)
                        (make-pathname :type "LISP")))
    =>  "LISP"

    (pathname-type 
      (merge-pathnames (make-pathname :type :unspecific)
                        (make-pathname :type "LISP")))
    =>  :UNSPECIFIC
    ```


## 19.3 <span id="LogicalPathnames">逻辑路径名</span>

> * 19.3.1 [逻辑路径名名称字符串的语法](#SyntaxLogicalPathnameNamestr)
> * 19.3.2 [逻辑路径名成员](#LogicalPathnameComponents)


### 19.3.1 <span id="SyntaxLogicalPathnameNamestr">逻辑路径名名称字符串的语法</span>

逻辑路径名名称字符串的语法如下. (注意, 与本文档中的许多符号描述不同, 这是对字符序列的语法描述, 而不是对象的结构描述.)

    logical-pathname::= [host host-marker]  
                        [relative-directory-marker] {directory directory-marker}*  
                        [name] [type-marker type [version-marker version]] 

    host::= word 

    directory::= word | wildcard-word | wild-inferiors-word 

    name::= word | wildcard-word 

    type::= word | wildcard-word 

    version::= pos-int | newest-word | wildcard-version 

    host-marker---一个冒号.

    relative-directory-marker---一个分号.

    directory-marker---一个分号.

    type-marker---一个点.

    version-marker---一个点.

    wild-inferiors-word---两个字符的序列 "**" (两个星号).

    newest-word---六个字符的序列 "newest" 或者六个字符的序列 "NEWEST".

    wildcard-version---一个星号.

    wildcard-word---一个或多个星号, 大写字母, 数字, 以及连字符, 包含至少一个星号, 其中没有相邻的两个星号.

    word---一个或多个大写字母, 数字, 以及连字符.

    pos-int---一个正整数.

#### 19.3.1.1 关于解析逻辑路径名名称字符串的额外信息

##### 19.3.1.1.1 逻辑路径名名称字符串的主机部分

这个主机必须已经被定义为一个逻辑路径名主机; 这个可以通过 logical-pathname-translations 的 setf 来完成.

逻辑路径名主机名 "SYS" 为具体实现保留了. 这个 SYS: 逻辑路径名的存在性和意义是具体实现定义的.

##### 19.3.1.1.2 逻辑路径名名称字符串的设备部分

这里没有逻辑路径名设备的语法, 因为一个逻辑路径名的设备成员总是为 :unspecific; 见章节 19.3.2.1 (一个逻辑路径名的未指定成员). 

##### 19.3.1.1.3 逻辑路径名名称字符串的目录部分

如果在这些 directory 之前有一个 relative-directory-marker, 这个目录成员会被解析为是相对的; 否则, 这个目录成员会被解析为绝对的.

如果指定了一个 wild-inferiors-marker, 它被解析为 :wild-inferiors. 

##### 19.3.1.1.4 逻辑路径名名称字符串的类型部分

一个源码文件的逻辑路径名的类型是 "LISP". 这应该被转换成任何在物理路径名中合适的类型. 

##### 19.3.1.1.5 逻辑路径名名称字符串的版本部分

一些文件系统没有版本. 对于这样一个文件系统, 逻辑路径名的转换会被忽略. 这意味着程序不能依赖于能够存储一个由逻辑路径名命名的文件的多个版本.

如果指定了一个 wildcard-version, 它被解析为 :wild. 

##### 19.3.1.1.6 逻辑路径名名称字符串的通配符

在一个 wildcard-word 中的每一个星号匹配一个零或多个字符的序列. 这个 wildcard-word "*" 解析为 :wild; 其他 wildcard-word 解析为字符串. 


##### 19.3.1.1.7 逻辑路径名名称字符串的小写字母

在解析 words 和 wildcard-words 时, 小写字母会被转换为大写的. 

##### 19.3.1.1.8 逻辑路径名名称字符串的其他语法

在一个逻辑路径名名称字符串中使用字符而不是在这里指定的那些的后果是不确定的.

使用任何没有在这里指定的值作为一个逻辑路径名的成员的后果是不确定的. 

### 19.3.2 <span id="LogicalPathnameComponents">逻辑路径名成员</span>

#### 19.3.2.1 一个逻辑路径名的未指定成员

一个逻辑路径名的设备成员总是为 :unspecific; 没有逻辑路径名的其他成员可以是 :unspecific. 

#### 19.3.2.2 空字符串作为逻辑路径名的成员

空字符串, "", 不是一个逻辑路径名任意成员的有效值. 

## 19.4 <span id="TheFilenamesDictionary">The Filenames Dictionary</span>

> * [系统类 PATHNAME](#SC-PATHNAME)
> * [系统类 LOGICAL-PATHNAME](#SC-LOGICAL-PATHNAME)
> * [函数 PATHNAME](#F-PATHNAME)
> * [函数 MAKE-PATHNAME](#F-MAKE-PATHNAME)
> * [函数 PATHNAMEP](#F-PATHNAMEP)
> * [函数 PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY, PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION](#F-PATHNAME-ALL)
> * [函数 LOAD-LOGICAL-PATHNAME-TRANSLATIONS](#F-LOAD-LOGICAL-PATHNAME-TRANSLATIONS)
> * [访问器 LOGICAL-PATHNAME-TRANSLATIONS](#A-LOGICAL-PATHNAME-TRANSLATIONS)
> * [函数 LOGICAL-PATHNAME](#F-LOGICAL-PATHNAME)
> * [变量 *DEFAULT-PATHNAME-DEFAULTS*](#V-DEFAULT-PATHNAME-DEFAULTS)
> * [函数 NAMESTRING, FILE-NAMESTRING, DIRECTORY-NAMESTRING, HOST-NAMESTRING, ENOUGH-NAMESTRING](#F-NAMESTRING-ALL)
> * [函数 PARSE-NAMESTRING](#F-PARSE-NAMESTRING)
> * [函数 WILD-PATHNAME-P](#F-WILD-PATHNAME-P)
> * [函数 PATHNAME-MATCH-P](#F-PATHNAME-MATCH-P)
> * [函数 TRANSLATE-LOGICAL-PATHNAME](#F-TRANSLATE-LOGICAL-PATHNAME)
> * [函数 TRANSLATE-PATHNAME](#F-TRANSLATE-PATHNAME)
> * [函数 MERGE-PATHNAMES](#F-MERGE-PATHNAMES)

### <span id="SC-PATHNAME">系统类 PATHNAME</span>

* 类优先级列表(Class Precedence List):

        pathname, t

* 描述(Description):

        一个路径名是一个表示一个文件名的结构化对象.

        这里有两种路径名---物理路径名和逻辑路径名. 


### <span id="SC-LOGICAL-PATHNAME">系统类 LOGICAL-PATHNAME</span>

* 类优先级列表(Class Precedence List):

        logical-pathname, pathname, t

* 描述(Description):

        一个使用独立于具体实现的名称字符串语法的并且有着独立于具体实现的成员值的路径名. 逻辑路径名不会直接引用文件名.

* 也见(See Also):

        章节 20.1 (File System Concepts), 章节 2.4.8.14 (Sharpsign P), 章节 22.1.3.11 (Printing Pathnames) 


### <span id="F-PATHNAME">函数 PATHNAME</span>

* 语法(Syntax):

        pathname pathspec => pathname

* 参数和值(Arguments and Values):

        pathspec---一个路径名标识符.
        pathname---一个路径名.

* 描述(Description):

        返回 pathspec 表示的路径名.

        如果这个 pathspec 标识符是一个流, 那么这个流可以是打开的或关闭的; 不管在哪种请看, 这个 pathname 返回被用于打开这个文件的对应文件名. 在一个文件流被关闭后, pathname 还是返回和它被打开时相同的路径名.

        如果这个 pathspec 标识符是一个通过打开一个逻辑路径名所创建的文件流, 就返回一个逻辑路径名.

* 示例(Examples):

    ```LISP
    ;; There is a great degree of variability permitted here.  The next
    ;; several examples are intended to illustrate just a few of the many
    ;; possibilities.  Whether the name is canonicalized to a particular
    ;; case (either upper or lower) depends on both the file system and the
    ;; implementation since two different implementations using the same
    ;; file system might differ on many issues.  How information is stored
    ;; internally (and possibly presented in #S notation) might vary,
    ;; possibly requiring `accessors' such as PATHNAME-NAME to perform case
    ;; conversion upon access.  The format of a namestring is dependent both
    ;; on the file system and the implementation since, for example, one
    ;; implementation might include the host name in a namestring, and
    ;; another might not.  #S notation would generally only be used in a
    ;; situation where no appropriate namestring could be constructed for use
    ;; with #P.
    (setq p1 (pathname "test"))
    =>  #P"CHOCOLATE:TEST" ; with case canonicalization (e.g., VMS)
    OR=>  #P"VANILLA:test"   ; without case canonicalization (e.g., Unix)
    OR=>  #P"test"
    OR=>  #S(PATHNAME :HOST "STRAWBERRY" :NAME "TEST")
    OR=>  #S(PATHNAME :HOST "BELGIAN-CHOCOLATE" :NAME "test")
    (setq p2 (pathname "test"))
    =>  #P"CHOCOLATE:TEST"
    OR=>  #P"VANILLA:test"
    OR=>  #P"test"
    OR=>  #S(PATHNAME :HOST "STRAWBERRY" :NAME "TEST")
    OR=>  #S(PATHNAME :HOST "BELGIAN-CHOCOLATE" :NAME "test")
    (pathnamep p1) =>  true
    (eq p1 (pathname p1)) =>  true
    (eq p1 p2)
    =>  true
    OR=>  false
    (with-open-file (stream "test" :direction :output)
      (pathname stream))
    =>  #P"ORANGE-CHOCOLATE:>Gus>test.lisp.newest"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 


### <span id="F-MAKE-PATHNAME">函数 MAKE-PATHNAME</span>

* 语法(Syntax):

        make-pathname &key host device directory name type version defaults case
        => pathname

* 参数和值(Arguments and Values):

        host---一个有效的物理路径名主机. 复杂的缺省行为; 见下方.
        device---一个有效的路径名设备. 复杂的缺省行为; 见下方.
        directory---一个有效的路径名目录. 复杂的缺省行为; 见下方.
        name---一个有效的路径名名称. 复杂的缺省行为; 见下方.
        type---一个有效的路径名类型. 复杂的缺省行为; 见下方.
        version---一个有效的路径名版本. 复杂的缺省行为; 见下方.
        defaults---一个路径名标识符. 默认是一个主机成员和 *default-pathname-defaults* 的主机成员相同, 而其他成员都是 nil 的路径名.
        case---:common 或 :local 其中之一. 默认是 :local.
        pathname---一个路径名.

* 描述(Description):

        从提供的关键字参数中构造并返回一个路径名.

        在这些通过显式提供 host, device, directory, name, type, 和 version 的成员被填充之后, merge-pathnames 使用的合并规则被用于填充 defaults 提供的默认值中的任何未提供的成员.

        无论何时构造路径名, 如果合适, 成员都可以被规范化. 对于可以为每个成员提供的参数的解释, 见章节 19.2.1 (Pathname Components).

        如果提供了 case, 它会像章节 19.2.2.1.2 (路径名成员中的大小写) 描述的那样被对待.

        这个产生的路径名 pathname 当且仅当它的主机成员是一个逻辑主机或者一个命名一个已定义的逻辑主机的字符串时是一个逻辑路径名.

        如果这个 directory 是一个字符串, 它应该是一个顶层目录的名字, 并且不应该包含任何标点符号字符; 这就是说, 指定一个字符串, str, 等价于指定列表 (:absolute str). 指定符号 :wild 等价于指定列表 (:absolute :wild-inferiors), 或者在一个不支持 :wild-inferiors 的文件系统中是 (:absolute :wild).

* 示例(Examples):

    ```LISP
    ;; Implementation A -- an implementation with access to a single
    ;;  Unix file system.  This implementation happens to never display
    ;;  the `host' information in a namestring, since there is only one host. 
    (make-pathname :directory '(:absolute "public" "games")
                    :name "chess" :type "db")
    =>  #P"/public/games/chess.db" 

    ;; Implementation B -- an implementation with access to one or more
    ;;  VMS file systems.  This implementation displays `host' information
    ;;  in the namestring only when the host is not the local host.
    ;;  It uses a double colon to separate a host name from the host's local
    ;;  file name.
    (make-pathname :directory '(:absolute "PUBLIC" "GAMES")
                    :name "CHESS" :type "DB")
    =>  #P"SYS$DISK:[PUBLIC.GAMES]CHESS.DB" 
    (make-pathname :host "BOBBY"
                    :directory '(:absolute "PUBLIC" "GAMES")
                    :name "CHESS" :type "DB")
    =>  #P"BOBBY::SYS$DISK:[PUBLIC.GAMES]CHESS.DB" 

    ;; Implementation C -- an implementation with simultaneous access to
    ;;  multiple file systems from the same Lisp image.  In this 
    ;;  implementation, there is a convention that any text preceding the
    ;;  first colon in a pathname namestring is a host name.
    (dolist (case '(:common :local))
      (dolist (host '("MY-LISPM" "MY-VAX" "MY-UNIX"))
        (print (make-pathname :host host :case case
                              :directory '(:absolute "PUBLIC" "GAMES")
                              :name "CHESS" :type "DB"))))
    >>  #P"MY-LISPM:>public>games>chess.db"
    >>  #P"MY-VAX:SYS$DISK:[PUBLIC.GAMES]CHESS.DB"
    >>  #P"MY-UNIX:/public/games/chess.db"
    >>  #P"MY-LISPM:>public>games>chess.db" 
    >>  #P"MY-VAX:SYS$DISK:[PUBLIC.GAMES]CHESS.DB" 
    >>  #P"MY-UNIX:/PUBLIC/GAMES/CHESS.DB" 
    =>  NIL
    ```

* 受此影响(Affected By):

        文件系统.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        merge-pathnames, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes):

        可移植程序不应该为任何成员提供 :unspecific. 见章节 19.2.2.2.3 (:UNSPECIFIC as a Component Value). 


### <span id="F-PATHNAMEP">函数 PATHNAMEP</span>

* 语法(Syntax):

        pathnamep object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 pathname 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (setq q "test")  =>  "test"
    (pathnamep q) =>  false
    (setq q (pathname "test"))
    =>  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY NIL :NAME "test" :TYPE NIL
          :VERSION NIL)
    (pathnamep q) =>  true 
    (setq q (logical-pathname "SYS:SITE;FOO.SYSTEM"))
    =>  #P"SYS:SITE;FOO.SYSTEM"
    (pathnamep q) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (pathnamep object) ==  (typep object 'pathname)


### <span id="F-PATHNAME-ALL">函数 PATHNAME-HOST, PATHNAME-DEVICE, PATHNAME-DIRECTORY, PATHNAME-NAME, PATHNAME-TYPE, PATHNAME-VERSION</span>

* 语法(Syntax):

        pathname-host pathname &key case => host

        pathname-device pathname &key case => device

        pathname-directory pathname &key case => directory

        pathname-name pathname &key case => name

        pathname-type pathname &key case => type

        pathname-version pathname => version

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符.
        case---:local 或 :common 其中之一. 默认是 :local.
        host---一个有效的路径名主机.
        device---一个有效的路径名设备.
        directory---一个有效的路径名目录.
        name---一个有效的路径名名称.
        type---一个有效的路径名类型.
        version---一个有效的路径名版本.

* 描述(Description):

        这些函数返回路径名 pathname 的成员.

        如果这个 pathname 标识符是一个路径名, 它表示被用于打开这个文件的名字. 这可能是, 但不一定是文件的实际名称.

        如果提供了 case, 它会像章节 19.2.2.1.2 (路径名成员中的大小写) 中描述的那样被对待.

* 示例(Examples):

    ```LISP
    (setq q (make-pathname :host "KATHY"
                            :directory "CHAPMAN" 
                            :name "LOGIN" :type "COM"))
    =>  #P"KATHY::[CHAPMAN]LOGIN.COM"
    (pathname-host q) =>  "KATHY"
    (pathname-name q) =>  "LOGIN"
    (pathname-type q) =>  "COM"

    ;; Because namestrings are used, the results shown in the remaining
    ;; examples are not necessarily the only possible results.  Mappings
    ;; from namestring representation to pathname representation are 
    ;; dependent both on the file system involved and on the implementation
    ;; (since there may be several implementations which can manipulate the
    ;; the same file system, and those implementations are not constrained
    ;; to agree on all details). Consult the documentation for each
    ;; implementation for specific information on how namestrings are treated
    ;; that implementation.

    ;; VMS
    (pathname-directory (parse-namestring "[FOO.*.BAR]BAZ.LSP"))
    =>  (:ABSOLUTE "FOO" "BAR")
    (pathname-directory (parse-namestring "[FOO.*.BAR]BAZ.LSP") :case :common)
    =>  (:ABSOLUTE "FOO" "BAR")

    ;; Unix
    (pathname-directory "foo.l") =>  NIL
    (pathname-device "foo.l") =>  :UNSPECIFIC
    (pathname-name "foo.l") =>  "foo"
    (pathname-name "foo.l" :case :local) =>  "foo"
    (pathname-name "foo.l" :case :common) =>  "FOO"
    (pathname-type "foo.l") =>  "l"
    (pathname-type "foo.l" :case :local) =>  "l"
    (pathname-type "foo.l" :case :common) =>  "L"
    (pathname-type "foo") =>  :UNSPECIFIC
    (pathname-type "foo" :case :common) =>  :UNSPECIFIC
    (pathname-type "foo.") =>  ""
    (pathname-type "foo." :case :common) =>  ""
    (pathname-directory (parse-namestring "/foo/bar/baz.lisp") :case :local)
    =>  (:ABSOLUTE "foo" "bar")
    (pathname-directory (parse-namestring "/foo/bar/baz.lisp") :case :local)
    =>  (:ABSOLUTE "FOO" "BAR")
    (pathname-directory (parse-namestring "../baz.lisp"))
    =>  (:RELATIVE :UP)
    (PATHNAME-DIRECTORY (PARSE-NAMESTRING "/foo/BAR/../Mum/baz"))
    =>  (:ABSOLUTE "foo" "BAR" :UP "Mum")
    (PATHNAME-DIRECTORY (PARSE-NAMESTRING "/foo/BAR/../Mum/baz") :case :common)
    =>  (:ABSOLUTE "FOO" "bar" :UP "Mum")
    (PATHNAME-DIRECTORY (PARSE-NAMESTRING "/foo/*/bar/baz.l"))
    =>  (:ABSOLUTE "foo" :WILD "bar")
    (PATHNAME-DIRECTORY (PARSE-NAMESTRING "/foo/*/bar/baz.l") :case :common)
    =>  (:ABSOLUTE "FOO" :WILD "BAR")

    ;; Symbolics LMFS
    (pathname-directory (parse-namestring ">foo>**>bar>baz.lisp"))
    =>  (:ABSOLUTE "foo" :WILD-INFERIORS "bar")
    (pathname-directory (parse-namestring ">foo>*>bar>baz.lisp"))
    =>  (:ABSOLUTE "foo" :WILD "bar")
    (pathname-directory (parse-namestring ">foo>*>bar>baz.lisp") :case :common)
    =>  (:ABSOLUTE "FOO" :WILD "BAR")
    (pathname-device (parse-namestring ">foo>baz.lisp")) =>  :UNSPECIFIC
    ```

* 受此影响(Affected By):

        这个具体实现和主机文件系统.

* 异常情况(Exceptional Situations):

        如果它的第一个参数不是一个路径名, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 


### <span id="F-LOAD-LOGICAL-PATHNAME-TRANSLATIONS">函数 LOAD-LOGICAL-PATHNAME-TRANSLATIONS</span>

* 语法(Syntax):

        load-logical-pathname-translations host => just-loaded

* 参数和值(Arguments and Values):

        host---一个字符串.
        just-loaded---一个广义 boolean.

* 描述(Description):

        搜索并加载一个名为 host 的逻辑主机的定义, 如果它没有被定义的话. 这个搜索的具体性质是具体实现定义的.

        如果这个主机 host 已经被定义, 不会去尝试查找或加载定义的尝试, 并且返回 false. 如果这个主机 host 还没有被定义, 但是成功找到并加载了一个定义, 就返回 true. 否则, 发出一个错误.

* 示例(Examples):

    ```LISP
    (translate-logical-pathname "hacks:weather;barometer.lisp.newest")
    >>  Error: The logical host HACKS is not defined.
    (load-logical-pathname-translations "HACKS")
    >>  ;; Loading SYS:SITE;HACKS.TRANSLATIONS
    >>  ;; Loading done.
    =>  true
    (translate-logical-pathname "hacks:weather;barometer.lisp.newest")
    =>  #P"HELIUM:[SHARED.HACKS.WEATHER]BAROMETER.LSP;0"
    (load-logical-pathname-translations "HACKS")
    =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果没有找到定义, 就会发出一个 error 类型的错误.

* 也见(See Also):

        logical-pathname

* 注意(Notes):

        逻辑路径名定义不止会被实现者创建, 也会被程序员创建. 因此, 重要的是要记录搜索策略. 比如, 一个具体实现可能定义了一个主机的定义要在某个特定已命名目录中称为"host.translations"的文件中找到.

### <span id="A-LOGICAL-PATHNAME-TRANSLATIONS">访问器 LOGICAL-PATHNAME-TRANSLATIONS</span>

* 语法(Syntax):

        logical-pathname-translations host => translations

        (setf (logical-pathname-translations host) new-translations)

* 参数和值(Arguments and Values):

        host--一个逻辑主机标识符.
        translations, new-translations---一个列表.

* 描述(Description):

        返回这个主机的转化列表. 每一个转化都是一个两元素的列表: from-wildcard 和 to-wildcard. 任何额外的元素都是具体实现定义的. from-wildcard 是一个主机为 host 的逻辑路径名. to-wildcard 是一个路径名.

        (setf (logical-pathname-translations host) translations) 设置一个逻辑路径名主机的转换列表. 如果 host 是一个之前没有被用作一个逻辑路径名主机的字符串, 那么就会定义一个新的逻辑路径名主机; 否则一个已存在的主机转换会被替换. 逻辑路径名主机名字可以使用 string-equal 比较.

        在设置这个转换列表时, 每个 from-wildcard 都可以是一个逻辑路径名, 它的主机为 host 或者一个可通过 (parse-namestring string host) 解析的逻辑路径名字符串, 其中 host 表示由 parse-namestring 定义的合适对象. 每个 to-wildcard 可以是可通过 (pathname to-wildcard) 强制转为一个路径名的任何东西. 如果 to-wildcard 强制转为一个逻辑路径名, translate-logical-pathname 会在它使用它时执行重复的转化步骤.<!--TODO 待校对-->

        host 是一个逻辑路径名的主机成员或是一个已经通过 logical-pathname-translations 的 setf 被定义为一个逻辑路径名主机名字的字符串.

* 示例(Examples):

    ```LISP
    ;;;A very simple example of setting up a logical pathname host.  No
    ;;;translations are necessary to get around file system restrictions, so
    ;;;all that is necessary is to specify the root of the physical directory
    ;;;tree that contains the logical file system.
    ;;;The namestring syntax on the right-hand side is implementation-dependent.
    (setf (logical-pathname-translations "foo")
          '(("**;*.*.*"              "MY-LISPM:>library>foo>**>")))
    
    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "foo:bar;baz;mum.quux.3")
    =>  #P"MY-LISPM:>library>foo>bar>baz>mum.quux.3"
    
    ;;;A more complex example, dividing the files among two file servers
    ;;;and several different directories.  This Unix doesn't support
    ;;;:WILD-INFERIORS in the directory, so each directory level must
    ;;;be translated individually.  No file name or type translations
    ;;;are required except for .MAIL to .MBX.
    ;;;The namestring syntax on the right-hand side is implementation-dependent.
    (setf (logical-pathname-translations "prog")
          '(("RELEASED;*.*.*"        "MY-UNIX:/sys/bin/my-prog/")
            ("RELEASED;*;*.*.*"      "MY-UNIX:/sys/bin/my-prog/*/")
            ("EXPERIMENTAL;*.*.*"    "MY-UNIX:/usr/Joe/development/prog/")
            ("EXPERIMENTAL;DOCUMENTATION;*.*.*"
                                      "MY-VAX:SYS$DISK:[JOE.DOC]")
            ("EXPERIMENTAL;*;*.*.*"  "MY-UNIX:/usr/Joe/development/prog/*/")
            ("MAIL;**;*.MAIL"        "MY-VAX:SYS$DISK:[JOE.MAIL.PROG...]*.MBX")))

    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "prog:mail;save;ideas.mail.3")
    =>  #P"MY-VAX:SYS$DISK:[JOE.MAIL.PROG.SAVE]IDEAS.MBX.3"

    ;;;Example translations for a program that uses three files main.lisp,
    ;;;auxiliary.lisp, and documentation.lisp.  These translations might be
    ;;;supplied by a software supplier as examples.

    ;;;For Unix with long file names
    (setf (logical-pathname-translations "prog")
          '(("CODE;*.*.*"             "/lib/prog/")))

    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "prog:code;documentation.lisp")
    =>  #P"/lib/prog/documentation.lisp"

    ;;;For Unix with 14-character file names, using .lisp as the type
    (setf (logical-pathname-translations "prog")
          '(("CODE;DOCUMENTATION.*.*" "/lib/prog/docum.*")
            ("CODE;*.*.*"             "/lib/prog/")))

    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "prog:code;documentation.lisp")
    =>  #P"/lib/prog/docum.lisp"

    ;;;For Unix with 14-character file names, using .l as the type
    ;;;The second translation shortens the compiled file type to .b
    (setf (logical-pathname-translations "prog")
          `(("**;*.LISP.*"            ,(logical-pathname "PROG:**;*.L.*"))
            (,(compile-file-pathname (logical-pathname "PROG:**;*.LISP.*"))
                                      ,(logical-pathname "PROG:**;*.B.*"))
            ("CODE;DOCUMENTATION.*.*" "/lib/prog/documentatio.*")
            ("CODE;*.*.*"             "/lib/prog/")))

    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "prog:code;documentation.lisp")
    =>  #P"/lib/prog/documentatio.l"

    ;;;For a Cray with 6 character names and no directories, types, or versions.
    (setf (logical-pathname-translations "prog")
          (let ((l '(("MAIN" "PGMN")
                      ("AUXILIARY" "PGAUX")
                      ("DOCUMENTATION" "PGDOC")))
                (logpath (logical-pathname "prog:code;"))
                (phypath (pathname "XXX")))
            (append
              ;; Translations for source files
              (mapcar #'(lambda (x)
                          (let ((log (first x))
                                (phy (second x)))
                            (list (make-pathname :name log
                                                  :type "LISP"
                                                  :version :wild
                                                  :defaults logpath)
                                  (make-pathname :name phy
                                                  :defaults phypath))))
                      l)
              ;; Translations for compiled files
              (mapcar #'(lambda (x)
                          (let* ((log (first x))
                                  (phy (second x))
                                  (com (compile-file-pathname
                                        (make-pathname :name log
                                                        :type "LISP"
                                                        :version :wild
                                                        :defaults logpath))))
                            (setq phy (concatenate 'string phy "B"))
                            (list com
                                  (make-pathname :name phy
                                                  :defaults phypath))))
                      l))))

    ;;;Sample use of that logical pathname.  The return value
    ;;;is implementation-dependent.          
    (translate-logical-pathname "prog:code;documentation.lisp")
    =>  #P"PGDOC"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 host 没有被正确提供, 就会发出一个 type-error 类型的错误.

* 也见(See Also):

        logical-pathname, 章节 19.1.2 (路径名作为文件名)

* 注意(Notes):

        具体实现可以定义在逻辑路径名主机上操作的额外函数, 比如去指定额外的转化规则或选项. 


### <span id="F-LOGICAL-PATHNAME">函数 LOGICAL-PATHNAME</span>

* 语法(Syntax):

        logical-pathname pathspec => logical-pathname

* 参数和值(Arguments and Values):

        pathspec---一个逻辑路径名, 一个逻辑路径名名称字符串, 或者一个流.
        logical-pathname---一个逻辑路径名.

* 描述(Description):

        logical-pathname 把 pathspec 转换为一个逻辑路径名并返回这个新的逻辑路径名. 如果 pathspec 是一个逻辑路径名名称字符串, 它应该包含一个主机成员并且它跟随的冒号. 如果 pathspec 是一个流, 它应该是一个可以让 pathname 返回一个逻辑路径名的流.

        如果 pathspec 是一个流, 这个流可以是打开的或者关闭的. 在一个文件被关闭后 logical-pathname 返回一个和这个文件打开时它返回的相同的逻辑路径名. 如果 pathspec 是一个使用 make-two-way-stream, make-echo-stream, make-broadcast-stream, make-concatenated-stream, make-string-input-stream, 或 make-string-output-stream 所创建的流, 那么就是一个错误.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 pathspec 没有被正确提供, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        logical-pathname, translate-logical-pathname, 章节 19.3 (逻辑路径名)

* 注意(Notes): None. 


### <span id="V-DEFAULT-PATHNAME-DEFAULTS">变量 *DEFAULT-PATHNAME-DEFAULTS*</span>

* 值类型(Value Type):

        一个路径名对象.

* 初始值(Initial Value):

        一个依赖于具体实现的路径名, 通常是在 Common Lisp 启动时当前的工作目录中.

* 描述(Description):

        一个被用作默认值的路径名, 当一个函数需要一个默认的路径名并且没有没有提供时.

* 示例(Examples):

    ```LISP
    ;; This example illustrates a possible usage for a hypothetical Lisp running on a
    ;; DEC TOPS-20 file system.  Since pathname conventions vary between Lisp 
    ;; implementations and host file system types, it is not possible to provide a
    ;; general-purpose, conforming example.
    *default-pathname-defaults* =>  #P"PS:<FRED>"
    (merge-pathnames (make-pathname :name "CALENDAR"))
    =>  #P"PS:<FRED>CALENDAR"
    (let ((*default-pathname-defaults* (pathname "<MARY>")))
      (merge-pathnames (make-pathname :name "CALENDAR")))
    =>  #P"<MARY>CALENDAR"
    ```

* 受此影响(Affected By):

        具体实现.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-NAMESTRING-ALL">函数 NAMESTRING, FILE-NAMESTRING, DIRECTORY-NAMESTRING, HOST-NAMESTRING, ENOUGH-NAMESTRING</span>

* 语法(Syntax):

        namestring pathname => namestring

        file-namestring pathname => namestring

        directory-namestring pathname => namestring

        host-namestring pathname => namestring

        enough-namestring pathname &optional defaults => namestring

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符.
        defaults---一个路径名标识符. 默认是 *default-pathname-defaults* 的值.
        namestring---一个字符串或 nil.

* 描述(Description):

        这些函数转换路径名 pathname 为一个名称字符串. 由路径名 pathname 表示的名称以一种依赖于具体实现的规范形式作为名称字符串返回.

        namestring 路径名 pathname 的完整形式.

        file-namestring 只返回路径名 pathname 的名称, 类型, 和版本成员.

        directory-namestring 返回目录名部分.

        host-namestring 返回主机名部分.

        enough-namestring 返回一个缩写的名称字符串, 它仅足以识别与 defaults 相关的 pathname 命名的文件. 在所有情况下, 它要求这样

            (merge-pathnames (enough-namestring pathname defaults) defaults)
            ==  (merge-pathnames (parse-namestring pathname nil defaults) defaults)

        并且这个 enough-namestring 的结果是满足这个标准的最短的合理字符串.

        不可能通过将三个较短的名称字符串以某种顺序连接在一起来构造一个有效的名称字符串.

* 示例(Examples):

    ```
    (namestring "getty")            
    =>  "getty"
    (setq q (make-pathname :host "kathy" 
                            :directory 
                              (pathname-directory *default-pathname-defaults*)
                            :name "getty")) 
    =>  #S(PATHNAME :HOST "kathy" :DEVICE NIL :DIRECTORY directory-name 
          :NAME "getty" :TYPE NIL :VERSION NIL)
    (file-namestring q) =>  "getty"
    (directory-namestring q) =>  directory-name
    (host-namestring q) =>  "kathy" 

    ;;;Using Unix syntax and the wildcard conventions used by the
    ;;;particular version of Unix on which this example was created:
    (namestring
      (translate-pathname "/usr/dmr/hacks/frob.l"
                          "/usr/d*/hacks/*.l"
                          "/usr/d*/backup/hacks/backup-*.*"))
    =>  "/usr/dmr/backup/hacks/backup-frob.l"
    (namestring
      (translate-pathname "/usr/dmr/hacks/frob.l"
                          "/usr/d*/hacks/fr*.l"
                          "/usr/d*/backup/hacks/backup-*.*"))
    =>  "/usr/dmr/backup/hacks/backup-ob.l"
    
    ;;;This is similar to the above example but uses two different hosts,
    ;;;U: which is a Unix and V: which is a VMS.  Note the translation
    ;;;of file type and alphabetic case conventions.
    (namestring
      (translate-pathname "U:/usr/dmr/hacks/frob.l"
                          "U:/usr/d*/hacks/*.l"
                          "V:SYS$DISK:[D*.BACKUP.HACKS]BACKUP-*.*"))
    =>  "V:SYS$DISK:[DMR.BACKUP.HACKS]BACKUP-FROB.LSP"
    (namestring
      (translate-pathname "U:/usr/dmr/hacks/frob.l"
                          "U:/usr/d*/hacks/fr*.l"
                          "V:SYS$DISK:[D*.BACKUP.HACKS]BACKUP-*.*"))
    =>  "V:SYS$DISK:[DMR.BACKUP.HACKS]BACKUP-OB.LSP"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        truename, merge-pathnames, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 


### <span id="F-PARSE-NAMESTRING">函数 PARSE-NAMESTRING</span>

* 语法(Syntax):

        parse-namestring thing &optional host default-pathname &key start end junk-allowed
        => pathname, position

* 参数和值(Arguments and Values):

        thing---一个字符串, 一个路径名, 或者一个和文件相关的流.
        host---一个有效路径名主机, 一个逻辑主机, 或者 nil.
        default-pathname---一个路径名标识符. 默认是 *default-pathname-defaults* 的值.
        start, end---thing 的边界索引标识符. 对于 start 和 end 默认是 0 和 nil.
        junk-allowed---一个广义 boolean. 默认是 false.
        pathname---路径名, 或 nil.
        position---对于 thing 的一个边界索引标识符.

* 描述(Description):

        转换 thing 为一个路径名.

        主机 host 提供一个相对于解析发生的主机名.

        如果 thing 是一个和一个文件关联的流, 处理的过程就好像是用来打开那个文件的路径名已经被提供了.

        如果 thing 是一个路径名, 主机 host 和 thing 的主机成员会被比较. 如果它们匹配, 立即返回两个值: thing 和 start; 否则 (如果它们不匹配), 发出一个错误.

        否则 (如果 thing 是一个字符串), parse-namestring 在这个 thing 里 start 和 end 限定的子字符串中解析一个文件的名字.

        如果 thing 是一个字符串那么这个 thing 里 start 和 end 限定的子字符串按照如下被解析为一个路径名:

        * 如果 host 是一个逻辑主机那么 thing 解析为主机 host 上的一个逻辑路径名名称字符串.

        * 如果 host 是 nil 并且 thing 是一个包含一个显式主机的语法有效的逻辑路径名名称字符串, 那么它被解析为一个逻辑路径名名称字符串.

        * 如果 host 是 nil, default-pathname 是一个逻辑路径名, 并且 thing 是一个没有一个显式主机的语法有效的逻辑路径名名称字符串, 那么它被解析为一个在 default-pathname 主机成员的主机上的逻辑路径名名称字符串.

        * 否则, 这个 thing 的解析是具体实现定义的.

        在第一个情况中, 这个逻辑路径名名称字符串的主机部分和它跟随的冒号是可选的.

        如果这个名称字符串的主机部分和 host 都出现了但是不匹配, 就会发出一个错误.

        如果 junk-allowed 是 true, 那么主要的值就是解析的路径名, 如果没有找到语法正确的路径名, 那么就是 nil. 如果 junk-allowed 是 false, 那么整个字符串子字符串会被扫描, 而主要的值就是解析的路径名.

        在每个情况中, 第二个值是 thing 中终止这个解析的索引, 或者如果这个解析在这个子字符串末尾终止, 就是超出这个子字符串的索引 (如果 junk-allowed 是 false, 情况总是如此).

        解析一个空字符串总是成功的, 产生一个所有成员等于 nil (除了主机)的路径名.

        如果 thing 包含一个显式主机名而没有显式的设备名, 那么 parse-namestring 是否会为那个 host 提供标准默认设备作为产生路径名的设备成员是具体实现定义的.

* 示例(Examples):

    ```LISP
    (setq q (parse-namestring "test"))  
    =>  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY NIL :NAME "test" 
          :TYPE NIL :VERSION NIL)
    (pathnamep q) =>  true
    (parse-namestring "test") 
    =>  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY NIL :NAME "test"
          :TYPE NIL :VERSION NIL), 4
    (setq s (open xxx)) =>  #<Input File Stream...>
    (parse-namestring s) 
    =>  #S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY NIL :NAME xxx 
          :TYPE NIL :VERSION NIL), 0
    (parse-namestring "test" nil nil :start 2 :end 4 )
    =>  #S(PATHNAME ...), 15
    (parse-namestring "foo.lisp")
    =>  #P"foo.lisp"
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 junk-allowed 是 false, 如果 thing 不完全由一个路径名的表示组成, 如果与具体实现的文化协定是合适的, 可能任何一边包围着空格, 那么就会发出一个 error 类型的错误.

        如果提供了 host 并且不是 nil, 那么 thing 会包含一个一个清单主机名, 如果这些主机不匹配就会发出一个 error 类型的错误.

        如果 thing 是一个逻辑路径名名称字符串并且如果这个名称字符串的主机部分和 host 都出现但是不匹配, 就会发出一个 error 类型的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.2.2.2.3 (:UNSPECIFIC as a Component Value), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 


### <span id="F-WILD-PATHNAME-P">函数 WILD-PATHNAME-P</span>

* 语法(Syntax):

        wild-pathname-p pathname &optional field-key => generalized-boolean

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符.
        field-key---:host, :device :directory, :name, :type, :version 其中之一, 或者 nil.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        wild-pathname-p 检验 pathname 中通配符成员的存在性.

        如果 pathname 是一个路径名 (就像是由 pathname 返回的), 那么它就表示被用来打开这个文件的名字. 这可能, 但不是必须, 是这个文件的实际名字.

        如果没有提供 field-key 或者是 nil, 如果 pathname 有着任何的通配符成员, 那么 wild-pathname-p 返回 true, 如果没有就是 nil. 如果 field-key 是 non-nil, 如果表示的 pathname 的成员是一个通配符, 那么 wild-pathname-p 返回 true, 如果那个成员不是一个通配符就返回 nil.

* 示例(Examples):

    ```LISP
    ;;;The following examples are not portable.  They are written to run
    ;;;with particular file systems and particular wildcard conventions.
    ;;;Other implementations will behave differently.  These examples are
    ;;;intended to be illustrative, not to be prescriptive.
    
    (wild-pathname-p (make-pathname :name :wild)) =>  true
    (wild-pathname-p (make-pathname :name :wild) :name) =>  true
    (wild-pathname-p (make-pathname :name :wild) :type) =>  false
    (wild-pathname-p (pathname "s:>foo>**>")) =>  true ;Lispm
    (wild-pathname-p (pathname :name "F*O")) =>  true ;Most places
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 pathname 不是一个路径名, 一个字符串, 或者一个和文件关联的流, 那么就会发出一个 type-error 类型的错误.

* 也见(See Also):

        pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes):

        不是所有的实现在所有域中都支持通配符. 见章节 19.2.2.2.2 (:WILD 作为一个成员值) 和章节 19.2.2.3 (通配符路径名上的限制). 


### <span id="F-PATHNAME-MATCH-P">函数 PATHNAME-MATCH-P</span>

* 语法(Syntax):

        pathname-match-p pathname wildcard => generalized-boolean

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符.
        wildcard---一个通配路径名的标识符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果路径名 pathname 匹配通配符 wildcard 那么 pathname-match-p 就返回 true, 否则返回 nil. 这个匹配规则是具体实现定义的, 但是应该和 directory 一致. wildcard 的缺失成员默认为 :wild.

        对于路径名 pathname 是一个通配路径名也是有效的; 在路径名 pathname 中的一个通配符域只能匹配 wildcard 的一个通配符域(换句话说, pathname-match-p 是可交换的). 对于 wildcard 是一个非通配路径名也是有效的.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 pathname 或 wildcard 不是一个路径名, 字符串, 或者一个和文件关联的流, 就会发出一个 type-error 类型的错误.

* 也见(See Also):

        directory, pathname, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 


### <span id="F-TRANSLATE-LOGICAL-PATHNAME">函数 TRANSLATE-LOGICAL-PATHNAME</span>

* 语法(Syntax):

        translate-logical-pathname pathname &key => physical-pathname

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符, 或者一个逻辑路径名名称字符串.
        physical-pathname---一个物理路径名.

* 描述(Description):

        把路径名 pathname 转换为一个物理路径名, 并返回它.

        如果 pathname 是一个流, 这个流可以是打开的或关闭的. 在文件关闭之后 translate-logical-pathname 还是返回和这个文件打开时返回的相同的物理路径名. 如果路径名 pathname 是一个用 make-two-way-stream, make-echo-stream, make-broadcast-stream, make-concatenated-stream, make-string-input-stream, make-string-output-stream 创建的流, 那就是一个错误.

        如果 pathname 是一个逻辑路径名名称字符串, 这个逻辑路径名名称字符串的主机部分和它跟随的冒号就是必须的.

        pathname 受限被强制转为一个路径名. 如果这个强制转换后的路径名是一个物理路径名, 它会被返回. 如果这个强制转换后的路径名是一个逻辑路径名, 这个逻辑路径名主机第一个匹配的转换 (根据 pathname-match-p) 会被应用, 就像是通过调用 translate-pathname 的一样. 如果这个结果是一个逻辑路径名, 会重复这个过程. 当这个结果最终为一个物理路径名时, 它会被返回. 如果没有匹配的转换, 就会发出一个错误.

        translate-logical-pathname 可能执行额外的转换, 通常是将文件类型转换为本地命名约定, 以适应长度有限的物理文件系统, 或者处理特殊的字符需求, 例如将连字符转换为下划线或大写字母到小写. 任何这样的额外转换都是具体实现定义的. 某些实现没有额外的转换.

        这里没有为 translate-logical-pathname 指定关键字参数, 但是具体实现允许通过添加关键字参数来扩展它.

* 示例(Examples):

        见 logical-pathname-translations.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 pathname 没有被正确提供, 就会发出一个 type-error 类型的错误.

        如果没有匹配的转换, 就会发出一个 file-error 类型的错误.

* 也见(See Also):

        logical-pathname, logical-pathname-translations, logical-pathname, 章节 20.1 (File System Concepts), 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None. 

### <span id="F-TRANSLATE-PATHNAME">函数 TRANSLATE-PATHNAME</span>

* 语法(Syntax):

translate-pathname source from-wildcard to-wildcard &key

=> translated-pathname

* 参数和值(Arguments and Values):

source---a pathname designator.

from-wildcard---a pathname designator.

to-wildcard---a pathname designator.

translated-pathname---a pathname.

* 描述(Description):

translate-pathname translates source (that matches from-wildcard) into a corresponding pathname that matches to-wildcard, and returns the corresponding pathname.

The resulting pathname is to-wildcard with each wildcard or missing field replaced by a portion of source. A ``wildcard field'' is a pathname component with a value of :wild, a :wild element of a list-valued directory component, or an implementation-defined portion of a component, such as the "*" in the complex wildcard string "foo*bar" that some implementations support. An implementation that adds other wildcard features, such as regular expressions, must define how translate-pathname extends to those features. A ``missing field'' is a pathname component with a value of nil.

The portion of source that is copied into the resulting pathname is implementation-defined. Typically it is determined by the user interface conventions of the file systems involved. Usually it is the portion of source that matches a wildcard field of from-wildcard that is in the same position as the wildcard or missing field of to-wildcard. If there is no wildcard field in from-wildcard at that position, then usually it is the entire corresponding pathname component of source, or in the case of a list-valued directory component, the entire corresponding list element.

During the copying of a portion of source into the resulting pathname, additional implementation-defined translations of case or file naming conventions might occur, especially when from-wildcard and to-wildcard are for different hosts.

It is valid for source to be a wild pathname; in general this will produce a wild result. It is valid for from-wildcard and/or to-wildcard to be non-wild pathnames.

There are no specified keyword arguments for translate-pathname, but implementations are permitted to extend it by adding keyword arguments.

translate-pathname maps customary case in source into customary case in the output pathname.

* 示例(Examples):

 ;; The results of the following five forms are all implementation-dependent.
 ;; The second item in particular is shown with multiple results just to 
 ;; emphasize one of many particular variations which commonly occurs.
 (pathname-name (translate-pathname "foobar" "foo*" "*baz")) =>  "barbaz"
 (pathname-name (translate-pathname "foobar" "foo*" "*"))
=>  "foobar"
OR=>  "bar"
 (pathname-name (translate-pathname "foobar" "*"    "foo*")) =>  "foofoobar"
 (pathname-name (translate-pathname "bar"    "*"    "foo*")) =>  "foobar"
 (pathname-name (translate-pathname "foobar" "foo*" "baz*")) =>  "bazbar"

 (defun translate-logical-pathname-1 (pathname rules)
   (let ((rule (assoc pathname rules :test #'pathname-match-p)))
     (unless rule (error "No translation rule for ~A" pathname))
     (translate-pathname pathname (first rule) (second rule))))
 (translate-logical-pathname-1 "FOO:CODE;BASIC.LISP"
                       '(("FOO:DOCUMENTATION;" "MY-UNIX:/doc/foo/")
                         ("FOO:CODE;"          "MY-UNIX:/lib/foo/")
                         ("FOO:PATCHES;*;"     "MY-UNIX:/lib/foo/patch/*/")))
=>  #P"MY-UNIX:/lib/foo/basic.l"

;;;This example assumes one particular set of wildcard conventions
;;;Not all file systems will run this example exactly as written
 (defun rename-files (from to)
   (dolist (file (directory from))
     (rename-file file (translate-pathname file from to))))
 (rename-files "/usr/me/*.lisp" "/dev/her/*.l")
   ;Renames /usr/me/init.lisp to /dev/her/init.l
 (rename-files "/usr/me/pcl*/*" "/sys/pcl/*/")
   ;Renames /usr/me/pcl-5-may/low.lisp to /sys/pcl/pcl-5-may/low.lisp
   ;In some file systems the result might be /sys/pcl/5-may/low.lisp
 (rename-files "/usr/me/pcl*/*" "/sys/library/*/")
   ;Renames /usr/me/pcl-5-may/low.lisp to /sys/library/pcl-5-may/low.lisp
   ;In some file systems the result might be /sys/library/5-may/low.lisp
 (rename-files "/usr/me/foo.bar" "/usr/me2/")
   ;Renames /usr/me/foo.bar to /usr/me2/foo.bar
 (rename-files "/usr/joe/*-recipes.text" "/usr/jim/cookbook/joe's-*-rec.text")
   ;Renames /usr/joe/lamb-recipes.text to /usr/jim/cookbook/joe's-lamb-rec.text
   ;Renames /usr/joe/pork-recipes.text to /usr/jim/cookbook/joe's-pork-rec.text
   ;Renames /usr/joe/veg-recipes.text to /usr/jim/cookbook/joe's-veg-rec.text

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If any of source, from-wildcard, or to-wildcard is not a pathname, a string, or a stream associated with a file an error of type type-error is signaled.

(pathname-match-p source from-wildcard) must be true or an error of type error is signaled.

* 也见(See Also):

namestring, pathname-host, pathname, logical-pathname, Section 20.1 (File System Concepts), Section 19.1.2 (路径名作为文件名)

* 注意(Notes):

The exact behavior of translate-pathname cannot be dictated by the Common Lisp language and must be allowed to vary, depending on the user interface conventions of the file systems involved.

The following is an implementation guideline. One file system performs this operation by examining each piece of the three pathnames in turn, where a piece is a pathname component or a list element of a structured component such as a hierarchical directory. Hierarchical directory elements in from-wildcard and to-wildcard are matched by whether they are wildcards, not by depth in the directory hierarchy. If the piece in to-wildcard is present and not wild, it is copied into the result. If the piece in to-wildcard is :wild or nil, the piece in source is copied into the result. Otherwise, the piece in to-wildcard might be a complex wildcard such as "foo*bar" and the piece in from-wildcard should be wild; the portion of the piece in source that matches the wildcard portion of the piece in from-wildcard replaces the wildcard portion of the piece in to-wildcard and the value produced is used in the result. 


### <span id="F-MERGE-PATHNAMES">函数 MERGE-PATHNAMES</span>

* 语法(Syntax):

merge-pathnames pathname &optional default-pathname default-version

=> merged-pathname

* 参数和值(Arguments and Values):

pathname---a pathname designator.

default-pathname---a pathname designator. The default is the value of *default-pathname-defaults*.

default-version---a valid pathname version. The default is :newest.

merged-pathname---a pathname.

* 描述(Description):

Constructs a pathname from pathname by filling in any unsupplied components with the corresponding values from default-pathname and default-version.

Defaulting of pathname components is done by filling in components taken from another pathname. This is especially useful for cases such as a program that has an input file and an output file. Unspecified components of the output pathname will come from the input pathname, except that the type should not default to the type of the input pathname but rather to the appropriate default type for output from the program; for example, see the function compile-file-pathname.

If no version is supplied, default-version is used. If default-version is nil, the version component will remain unchanged.

If pathname explicitly specifies a host and not a device, and if the host component of default-pathname matches the host component of pathname, then the device is taken from the default-pathname; otherwise the device will be the default file device for that host. If pathname does not specify a host, device, directory, name, or type, each such component is copied from default-pathname. If pathname does not specify a name, then the version, if not provided, will come from default-pathname, just like the other components. If pathname does specify a name, then the version is not affected by default-pathname. If this process leaves the version missing, the default-version is used. If the host's file name syntax provides a way to input a version without a name or type, the user can let the name and type default but supply a version different from the one in default-pathname.

If pathname is a stream, pathname effectively becomes (pathname pathname). merge-pathnames can be used on either an open or a closed stream.

If pathname is a pathname it represents the name used to open the file. This may be, but is not required to be, the actual name of the file.

merge-pathnames recognizes a logical pathname namestring when default-pathname is a logical pathname, or when the namestring begins with the name of a defined logical host followed by a colon. In the first of these two cases, the host portion of the logical pathname namestring and its following colon are optional.

merge-pathnames returns a logical pathname if and only if its first argument is a logical pathname, or its first argument is a logical pathname namestring with an explicit host, or its first argument does not specify a host and the default-pathname is a logical pathname.

Pathname merging treats a relative directory specially. If (pathname-directory pathname) is a list whose car is :relative, and (pathname-directory default-pathname) is a list, then the merged directory is the value of

 (append (pathname-directory default-pathname)
         (cdr  ;remove :relative from the front
           (pathname-directory pathname)))

except that if the resulting list contains a string or :wild immediately followed by :back, both of them are removed. This removal of redundant :back keywords is repeated as many times as possible. If (pathname-directory default-pathname) is not a list or (pathname-directory pathname) is not a list whose car is :relative, the merged directory is (or (pathname-directory pathname) (pathname-directory default-pathname))

merge-pathnames maps customary case in pathname into customary case in the output pathname.

* 示例(Examples):

 (merge-pathnames "CMUC::FORMAT"
                  "CMUC::PS:<LISPIO>.FASL")
=>  #P"CMUC::PS:<LISPIO>FORMAT.FASL.0"

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

*default-pathname-defaults*, pathname, logical-pathname, Section 20.1 (File System Concepts), Section 19.1.2 (路径名作为文件名)

* 注意(Notes):

The net effect is that if just a name is supplied, the host, device, directory, and type will come from default-pathname, but the version will come from default-version. If nothing or just a directory is supplied, the name, type, and version will come from default-pathname together. 

