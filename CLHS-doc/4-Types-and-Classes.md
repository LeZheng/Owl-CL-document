# 4. 类型和类

> * 4.1 [引言](#Introduction)
> * 4.2 [类型](#Types)
> * 4.3 [类](#Classes)
> * 4.4 [类型和类的字典](#TheTypesClassesDictionary)

## 4.1 <span id="Introduction">引言</span>

一个类型[type]是一个对象[object] (可能是无数)集合. 一个对象[object]可以属于超过一个类型[type]. 类型[type]从不显式地被 Common Lisp 表示为对象[object]. 相反, 它们是通过使用类型指定符[type specifier]间接引用的, 类型指定符是表示类型[type]的对象[object].

新的类型[type]可以使用 deftype, defstruct, defclass, 以及 define-condition 来定义.

函数[function] typep 作为一个集合成员资格测试, 被用于确定一个给定的对象[object]是否为给定的类型[type]. 函数 subtypep 作为一个子集测试, 被用于确定一个给定的类型[type]是否为另一个给定类型[type]的子类型[subtype]. 函数 type-of 返回一个给定对象[object]所属的具体类型[type], 即便这个对象[object]也属于一个或多个其他类型[type]. (比如, 每一个对象[object]都是类型[type] t, 但是 type-of 总是返回一个比 t 更具体的类型[type]的类型指定符[type specifier].)

对象[object]拥有类型[type], 而不是变量[variable]. 通常, 任何变量[variable]可以持有任何对象[object]作为它的值[value]. 可以通过显式类型声明[type declaration]来声明一个变量[variable]只能获取指定的类型[type]的值. 类型[type]被安排在一个有向的非循环图中, 除了等价的存在<!--TODO 不理解-->.

可以通过 declare, proclaim, declaim, 或 the 来做出和类型[type]相关的声明[declaration]. 关于声明[declaration]的更多信息, 见章节 3.3 (声明).

对象系统的基本对象[object]之一是类[class]. 一个类[class]确定其他被称之为它的实例[instance]的对象[object]集的结构和行为. 每一个对象[object]都是一个类[class]的直接实例[direct instance]. 一个对象[object]的类[class]确定可以在这个对象[object]上进行的操作集合. 关于更多信息, 见章节 4.3 (类).

可以写出具有特化[specialized]于函数实参[argument]对象[object]的类的行为的函数[function]. 关于更多信息, 见章节 7.6 (广义函数和方法).

一个对象[object]的类[class]的类[class]称为它的元类[metaclass]. 关于元类[metaclass]的更多信息, 见章节 7.4 (元对象). 

## 4.2 <span id="Types">类型</span>

> * 4.2.1 [数据类型定义](#DataTypeDefinition)
> * 4.2.2 [类型关系](#TypeRelationships)
> * 4.2.3 [类型指定符](#TypeSpecifiers)

### 4.2.1 <span id="DataTypeDefinition">数据类型定义</span>

关于类型使用的信息所在章节在 Figure 4-1 中指定. Figure 4-7 列出了和对象系统特别相关的类[class]. Figure 9-1 列出了已定义的状况[condition]类型[type].

  章节                     | 数据类型                          
  -------------------------|------------------------              
  章节 4.3 (类)             | 对象系统类型
  章节 7.5 (槽)             | 对象系统类型
  章节 7 (对象)             | 对象系统类型
  章节 7.6 (广义函数和方法)   | 对象系统类型
  章节 9.1 (状况系统概念)     | 状况系统类型
  章节 4 (类型和类)          | 各种类型
  章节 2 (语法)              | 所有类型---读取和打印语法
  章节 22.1 (Lisp 打印器)    | 所有类型---打印语法
  章节 3.2 (编译)            | 所有类型---编译问题

Figure 4-1. 数据类型信息的交叉引用

### 4.2.2 <span id="TypeRelationships">类型关系</span>

* 类型[type] cons, symbol, array, number, character, hash-table, function, readtable, package, pathname, stream, random-state, condition, restart, 还有任何由 defstruct, define-condition, 或 defclass 创建的单个类型是成对[pairwise]互斥[disjoint]的, 除了在 defclass 或 define-condition 中或者 destruct 的 :include 选项指定超类[superclasse]来显式建立的类型关系以外.

* 任何由 defstruct 创建的两个类型[type]是互斥[disjoint]的, 除非由于 defstruct 的 :include 选项, 一个是另一个的超类型[supertype].

* 任何两个由 defclass 或 define-condition 创建的不同[distinct]的类[class]是互斥[disjoint]的, 除非它们有一个共同的子类[subclass]或者一个类是另一个的子类[subclass].

* 可以扩展一个实现来添加指定类型[type]之间的其他子类型[subtype]关系, 只要它们不违反这里指定的类型关系和类互斥性的需求. 一个实现可能为任何指定的类型[type]定义额外的子类型[subtype]或超类型[supertype], 只要不违反互斥性的要求并且每一个额外的类型[type]是类型[type] t 的子类型[subtype]也是类型[type] nil 的超类型[supertype].

    任凭具体实现自行处理, 无论是 standard-object 还是 structure-object, 都可能出现在一个没有指定 standard-object 或 structure-object 的系统类[system class]的优先级列表中. 如果确实如此, 它必须在类[class] t 之前并且跟在所有标准化[standardized]类[class]后面.

### 4.2.3 <span id="TypeSpecifiers">类型指定符</span>

类型指定符[type specifier]可以是符号[symbol], 类[class]或列表[list]. Figure 4-2 列出了标准[standardized]原子类型指定符[atomic type specifier]的那些符号[symbol], 并且 Figure 4-3 列出了标准[standardized]复合类型指定符[compound type specifier]的那些名字[name]. 关于语法信息, 见对应类型指定符[type specifier]的字典条目. 可以通过 defclass, define-condition, defstruct, 或 deftype 来定义新的类型指定符[type specifier].

    arithmetic-error                  function            simple-condition        
    array                             generic-function    simple-error            
    atom                              hash-table          simple-string           
    base-char                         integer             simple-type-error       
    base-string                       keyword             simple-vector           
    bignum                            list                simple-warning          
    bit                               logical-pathname    single-float            
    bit-vector                        long-float          standard-char           
    broadcast-stream                  method              standard-class          
    built-in-class                    method-combination  standard-generic-function
    cell-error                        nil                 standard-method         
    character                         null                standard-object         
    class                             number              storage-condition       
    compiled-function                 package             stream                  
    complex                           package-error       stream-error            
    concatenated-stream               parse-error         string                  
    condition                         pathname            string-stream           
    cons                              print-not-readable  structure-class         
    control-error                     program-error       structure-object        
    division-by-zero                  random-state        style-warning           
    double-float                      ratio               symbol                  
    echo-stream                       rational            synonym-stream          
    end-of-file                       reader-error        t                       
    error                             readtable           two-way-stream          
    extended-char                     real                type-error              
    file-error                        restart             unbound-slot            
    file-stream                       sequence            unbound-variable        
    fixnum                            serious-condition   undefined-function      
    float                             short-float         unsigned-byte           
    floating-point-inexact            signed-byte         vector                  
    floating-point-invalid-operation  simple-array        warning                 
    floating-point-overflow           simple-base-string                          
    floating-point-underflow          simple-bit-vector                              

Figure 4-2. 标准化原子类型指定符

如果一个类型指定符[type specifier]是一个列表[list], 这个列表[list]的 car 部分是一个符号[symbol], 并且这个列表[list]的剩余部分附属类型[type]信息. 这样一个类型指定符称为复合类型指定符[compound type specifier]. 除显式声明外, 附属项可以是未指明的. 这个未指明的辅助项通过编写 * 来表示. 比如, 为了完全地指定一个向量[vector], 元素的类型[type]和这个向量[vector]的长度都必须存在.

```LISP
(vector double-float 100)
```

下面这个留着长度未指定:

```LISP
(vector double-float *)
```

下面这个留着元素类型未指定:

```LISP
(vector * 100)
```

假设这两种类型指定符[type specifier]是相同的除了第一个有一个 * 而第二个有一个更明确的指定. 那么第二个表示的是第一个表示的类型[type]的子类型[subtype].

如果一个列表[list]的末尾有一个或超过一个未指定项, 这些项可以被丢弃. 如果丢弃所有出现的 * 导致一个单元素[singleton]列表[list], 那么圆括号也可以被丢弃 (这个列表可以被它的 car 的符号[symbol]所替代). 比如, (vector double-float *) 可以被简写为 (vector double-float), 而 (vector * *) 可以被简写为 (vector) 然后是 vector.

    and           long-float    simple-base-string  
    array         member        simple-bit-vector   
    base-string   mod           simple-string       
    bit-vector    not           simple-vector       
    complex       or            single-float        
    cons          rational      string              
    double-float  real          unsigned-byte       
    eql           satisfies     values              
    float         short-float   vector              
    function      signed-byte                       
    integer       simple-array                      

Figure 4-3. 标准复合类型指定符的名字

下面这段展示了可以被用于复合类型指定符[compound type specifier]名字但是不能被用于原子类型指定符[atomic type specifier]的那些已定义的名字[defined name].

    and     mod  satisfies  
    eql     not  values     
    member  or              

Figure 4-4. 标准仅限复合类型指定符的名字

新的类型指定符可以通过两种方式出现.

* 通过使用 defstruct 不带 :type 指定符来定义一个结构体或者使用 defclass 或 define-condition 来定义一个类[class]自动导致结构体或类的名字成为一个新的类型指定符[type specifier]符号[symbol].
* 可以使用 deftype 来定义派生类型指定符[derived type specifier], 它表现地像其他类型指定符[type specifier]的'缩写(abbreviation)'一样.

一个类[class]对象[object]可以被用作一个类型指定符[type specifier]. 当使用这种方式的时候, 它表示这个类[class]的所有成员的集合.

下面这段展示了一些类型[type]和声明[declarations]相关的已定义的名字[defined name].

    coerce            defstruct  subtypep  
    declaim           deftype    the       
    declare           ftype      type      
    defclass          locally    type-of   
    define-condition  proclaim   typep     

Figure 4-5. 类型和声明相关的已定义的名字.

下面这段展示了所有是类型指定符[type specifier]名字[name]的已定义名字[defined name], 不管是原子类型指定符[atomic type specifier]或者复合类型指定符[compound type specifier]; 这个列表是 Figure 4-2 和 Figure 4-3 列表的结合.

    and                               function            simple-array            
    arithmetic-error                  generic-function    simple-base-string      
    array                             hash-table          simple-bit-vector       
    atom                              integer             simple-condition        
    base-char                         keyword             simple-error            
    base-string                       list                simple-string           
    bignum                            logical-pathname    simple-type-error       
    bit                               long-float          simple-vector           
    bit-vector                        member              simple-warning          
    broadcast-stream                  method              single-float            
    built-in-class                    method-combination  standard-char           
    cell-error                        mod                 standard-class          
    character                         nil                 standard-generic-function  
    class                             not                 standard-method         
    compiled-function                 null                standard-object         
    complex                           number              storage-condition       
    concatenated-stream               or                  stream                  
    condition                         package             stream-error            
    cons                              package-error       string                  
    control-error                     parse-error         string-stream           
    division-by-zero                  pathname            structure-class         
    double-float                      print-not-readable  structure-object        
    echo-stream                       program-error       style-warning           
    end-of-file                       random-state        symbol                  
    eql                               ratio               synonym-stream          
    error                             rational            t                       
    extended-char                     reader-error        two-way-stream          
    file-error                        readtable           type-error              
    file-stream                       real                unbound-slot            
    fixnum                            restart             unbound-variable        
    float                             satisfies           undefined-function      
    floating-point-inexact            sequence            unsigned-byte           
    floating-point-invalid-operation  serious-condition   values                  
    floating-point-overflow           short-float         vector                  
    floating-point-underflow          signed-byte         warning                    

Figure 4-6. 标准类型指定符的名字

## 4.3 <span id="Classes">类</span>

尽管对象系统来描述所有的标准化[standardized]类[class] (包括, 例如, number, hash-table, 和 symbol) 足够通用, 但下面这段包含了与理解对象系统相关的类[class]的列表.

    built-in-class    method-combination         standard-object   
    class             standard-class             structure-class   
    generic-function  standard-generic-function  structure-object  
    method            standard-method                              

Figure 4-7. 对象系统类

> * 4.3.1 [类的介绍](#IntroductionToClasses)
> * 4.3.2 [定义类](#DefiningClasses)
> * 4.3.3 [创建类的实例](#CreatingInstancesClasses)
> * 4.3.4 [继承](#Inheritance)
> * 4.3.5 [确定类优先级列表](#DeterminingClassPrecedenceList)
> * 4.3.6 [重定义类](#RedefiningClasses)
> * 4.3.7 [整合类和类型](#IntegratingTypesClasses)

### 4.3.1 <span id="IntroductionToClasses">类的介绍</span>

一个类[class]是一个决定其他被称之为它的实例[instance]的那些对象[object]集合的结构和行为的对象[object].

一个类[class]可以从其他类[class]中继承结构和行为. 一个出于继承其他类[class]的目的而在定义中引用它们的类[class]就称这个类是那些类[class]的子类[subclass]. 为了继承而指定的类[class]称为继承类[class]的超类[superclass].

一个类[class]可以有一个名字[name]. 函数[function] class-name 接受一个类[class]对象[object]并且返回它的名字[name]. 一个匿名类[class]的名字[name]是 nil. 一个符号[symbol]可以命名[name]一个类[class]. 函数[function] find-class 接受一个符号[symbol]并且返回这个符号[symbol]命名的类[class]. 如果一个名字[name]是一个符号[symbol]并且如果一个类[class]的这个名字[name]命名这个类[class], 那么这个类有一个专有名字[proper name]. 这就是说, 如果 S= (class-name C) 并且 C= (find-class S) 那么一个类[class] C 有一个专有名字[proper name]. 注意, (find-class S1) = (find-class S2) 并且 S1/=S2 是可能的. 如果 C= (find-class S), 我们就说 C 是名为 S 的类[class].

如果一个类[class] C2 在它的定义中显式指定 C1 作为超类[superclass]那么类[class] C1 就是类[class] C2 的一个直接超类[direct superclass]. 在这个情况下 C2 是 C1 的一个直接子类[direct subclass]. 如果 1 <= i < n 并且存在一系列的类[class] C2,...,Cn-1 而 Ci+1 是 Ci 的直接超类[direct superclass]那么类[class] Cn 是类[class] C1 的一个超类[superclass]. 一个类[class]不能被当作是它自身的超类[superclass]或子类[subclass]. 这也就是说, 如果 C1 是 C2 的一个超类[superclass], 那么 C1 /=C2. 由某个给定的类[class] C 及其所有超类[superclass]组成的类集合被称为 "C 及其超类".

每一个类[class]有一个类优先级列表[class precedence list], 它是给定类[class]及其超类[superclass]的集合的总序列. 这个总序列被表示为一个从最具体到最不具体的列表. 这个类优先级列表[class precedence list]被用于多种用途. 一般来说, 更具体的类[class]可以遮蔽[shadow[1]]从不具体的类中继承而来的特性. 方法[method]选择和组合过程使用类优先级列表[class precedence list]以从最具体到最不具体的顺序类对方法排序.

当一个类[class]被定义时, 定义表达式形式中提及到的直接超类[direct superclass]的顺序是很重要的. 每一个类[class]有一个局部优先级序列[local precedence order], 这是一个由该类[class]和跟在后面的直接超类[direct superclass]组成的列表[list], 按照定义表达式形式[form]中所提到的顺序.

类优先级列表[class precedence list]总是与列表中的每个类[class]的局部优先级序列[local precedence order]一致. 每个局部优先级序列[local precedence order]中的类都以相同的顺序出现在类优先级列表中. 如果局部优先级序列[local precedence order]和其他每个是不一致的, 就不能构建类优先级列表[class precedence list], 并且发出一个错误. 类优先级列表和它的运算在章节 4.3.5 (确定类优先级列表) 中讨论.

类[class]被组织成一个有向的无环图. 这里有两个显著的类[class], 名为 t 和 standard-object. 名为 t 的类[class]没有超类[superclass]. 它是除了它自身以外所有类[class]的超类[superclass]. 类[class] standard-object 是类[class] standard-class 的一个实例[instance]并且是每一个除了它自身以外类 standard-class 实例[instance]的超类[superclass].

这里由一个从对象系统类[class]空间到类型[type]空间的映射. 很多在这个文档中指定的标准类型[type]都有一个对应的有着和这个类型[type]相同名字[name]的类[class]. 一些类型[type]没有对应的类[class]. 类型[type]和类[class]系统的集成在章节 4.3.7 (整合类和类型) 中讨论.

类[class]由自身也是类[class]的实例[instance]的对象[object]来表示. 一个对象[object]的类[class]的类[class]被称为该对象[object]的元类[metaclass]. 当不可能出现错误解释时, 元类[metaclass]这个术语被用来引用一个有着自身也是类[class]的实例[instance]的类[class]. 元类[metaclass]确定作为它的实例[instance]的类[class]的继承形式, 以及那些类[class]的实例[instance]的表示形式. 这个对象系统提供一个默认的元类[metaclass], standard-class, 适用于大部分程序.

除另有指定外, 在这个标准中提及的所有类[class]都是类[class] standard-class 的实例[instance], 所有广义函数[generic function]都是类[class] standard-generic-function 的实例[instance], 并且所有方法[method]都是类[class] standard-method 的实例[instance]. 

#### 4.3.1.1 标准的元类

对象系统提供了许多预定义的元类[metaclass]. 这些包括类[class] standard-class, built-in-class, 还有 structure-class:

* 类[class] standard-class 是 defclass 定义的类[class]的默认类[class].

* 类[class] built-in-class 是这样的类[class]: 它的实例[instance]是具有限制能力的特殊实现的类[class]. 任何对应于标准类型[type]的类[class]可能是 built-in-class 的一个实例[instance]. 预定义的类型[type]指定符需要有着 Figure 4-8 中列出的对应类[class]. 这些类[class]中的每一个是否被实现为一个内置类[built-in class]是依赖于具体实现的[implementation-dependent].

* 所有通过 defstruct 定义的类[class]都是类[class] structure-class 的实例[instance]. 

### 4.3.2 <span id="DefiningClasses">定义类</span>

宏 defclass 被用于定义一个新命名的类[class].

一个类[class]的定义包括:

* 这个新类[class]的名字[name]. 对于新定义的类[class], 这个名字[name]是一个专有名字[proper name].

* 这个新定义的类[class]的直接超类列表[superclasses].

* 一个槽指定符[slot specifier]的集合. 每一个槽指定符[slot specifier]包括槽[slot]的名字[name]和 0 个或多个的槽[slot]选项. 一个槽[slot]选项只适用于单个槽[slot]. 如果一个类[class]定义包含两个相同名字[name]的槽指定符[slot specifier], 会发出一个错误.

* 一个类[class]选项的集合. 每个类[class]选项都属于整个类[class].

    defclass 表达式形式的槽[slot]选项和类[class]选项机制被用于:

    * 为给定的槽[slot]提供默认的初始值表达式形式[form].

    * 请求自动生成广义函数[generic function]的方法[method]，用于读取或写入槽[slot].

    * 控制一个给定的槽[slot]是否共享于这个类[class]的所有实例[instance]或者这个类[class]的每个实例[instance]是否有它自己的槽[slot].

    * 提供初始化参数和初始化参数默认值, 用于实例[instance]的创建.

    * 指定元类[metaclass]而不是默认的. 这个 :metaclass 选项保留给未来使用; 一个实现可以扩展去使用 :metaclass 选项.

    * 指定存储在槽[slot]中的期望的类型[type].

    * 指定槽[slot]的文档字符串[documentation string]. 

### 4.3.3 <span id="CreatingInstancesClasses">创建类的实例</span>

广义函数 make-instance 创建并返回一个类[class]的一个新的实例[instance]. 这个对象系统提供多种机制用于指明一个新的实例[instance]如何被初始化. 比如, 可以在新创建的实例[instance]中通过给 make-instance 提供参数或提供默认的初始化值来指定槽[slot]的初始值. 进一步的初始化活动可以通过为初始化协议中的广义函数[generic function]编写方法[method]来执行. 完全的初始化协议在章节 7.1 (对象创建和初始化) 中描述. 

### 4.3.4 <span id="Inheritance">继承</span>

一个类[class]可以从它的超类[superclass]中继承方法[method], 槽[slot], 还有一些 defclass 选项. 其他部分描述了方法[method]的继承, 槽[slot]和槽[slot]选项的继承以及类[class]选项的继承.

#### 4.3.4.1 继承的示例

```LISP
(defclass C1 () 
    ((S1 :initform 5.4 :type number) 
    (S2 :allocation :class)))

(defclass C2 (C1) 
    ((S1 :initform 5 :type integer)
    (S2 :allocation :instance)
    (S3 :accessor C2-S3)))
```

类 C1 的实例[instance]有一个名为 S1 的局部槽[local slot], 它的默认初始值是 5.4 并且它的值[number]应该始终为一个数字[number]. 类 C1 也有一个名为 S2 的共享槽[shared slot].

在 C2 的实例[instance]中有一个名为 S1 的局部槽[local slot]. S1 的默认初始值为 5. S1 的值应该总是为类型 (and integer number). 在 C2 的实例[instance]中也有名为 S2 和 S3 的局部槽[local slot]. 类 C2 有一个 C2-S3 方法[method]来读取槽 S3 的值; 也有一个 (setf C2-S3) 方法[method]来写入 S3 的值. 

#### 4.3.4.2 类选项的继承

这个 :default-initargs 类选项被继承. 一个类[class]的默认初始化参数集合是这个类[class]和它的超类[superclass]的 :default-initargs 类选项中提供的初始化参数的并集. 当给一个给定的初始化参数提供了不止一个默认初始值表达式形式[form]时, 使用的默认初始值表达式形式[form]是由类优先级列表[class precedence list]中最具体的类[class]提供的.

如果一个给定的 :default-initargs 类选项不止一次指定相同名字[name]的初始化参数, 会发出一个 program-error 类型[type]的错误. 

### 4.3.5 <span id="DeterminingClassPrecedenceList">确定类优先级列表</span>

一个类的 defclass 表达式形式提供这个类[class]和它的直接超类[superclass]的总序列. 这个序列称之为局部优先级序列[local precedence order]. 它是一个这个类[class]和它的直接超类[superclass]的有序列表. 类 C 的类优先级列表[class precedence list]是一个 C 和它的超类的总序列, 由 C 和它的超类[superclass]的每一个局部优先级序列[local precedence order]构成.

一个类[class]在它的直接超类[superclass]前面, 并且一个直接超类[superclass]先于那些在 defclass 表达式形式中的超类[superclass]列表中在它右边的其他直接超类[superclass]. 对于每一个类 C, 定义

    RC={(C,C1),(C1,C2),...,(Cn-1,Cn)}

其中 C1,...,Cn 是 C 的直接超类[superclass], 以 defclass 表达式形式提及的顺序. 这些有序对生成类 C 和它的直接超类[superclass]的总序列.

让 SC 是 C 和它的超类[superclass]的集合. 让 R 为

    R=Uc<ELEMENT-OF>SCRc

这个集合 R 可能会也可能不会生成一个部分列表, 取决于 Rc, c<ELEMENT-OF>SC, 是否是一致的; 假定它们是一致的, R 产生一个部分列表. 当这个 Rc 不是一致的时, 就说 R 是非一致的.

为了计算 C 的类优先级列表[class precedence list], 根据 R 产生的部分序列对 SC 的元素进行拓扑排序. 当这个拓扑排序必须从两个或更多类[class]的集合中选择一个类[class]时, 其中没有一个类的前面有关于 R 的其他类[class], 选择的类是确定的, 如下面所述.

如果 R 不是一致的, 会发出一个错误.

#### 4.3.5.1 拓扑排序 (未校对)
<!-- TODO 未校对 -->
拓扑排序是根据 R 中的元素通过在 SC 中找到一个 C 类来进行的, 这样就不会有其他元素先于这个元素. 这个类 C 被放在结果的最前面. 从 SC 中移除 C, 并且从 R 中移除所有表达式形式 (C,D) 对, D<ELEMENT-OF>SC. 重复这个过程, 在结果的末尾添加前面没有任何类的类. 当找不到前面没有类的元素时停止.

如果 SC 不是空并且这个过程已经停止, 那么集合 R 是不一致的. 如果在有限类[class]集合中的每个类[class]都前置另一个类, 那么 R 就包含一个循环. 这就是说, 这里有一个 Ci 先于 Ci+1 的链 C1,...,Cn , 1 <= i < n, 而 Cn 先于 C1.

有时候这里有多个来自 SC 并且前面没有类的类[class]. 在这个情况下选择有着直接子类[subclass]位于计算到目前为止的类优先级列表[class precedence list]的最右边的那个. (如果这里没有这样一个候选的类[class], R 不产生一个部分序列---这个 Rc, c<ELEMENT-OF>SC, 是不一致的.)

用更精确的术语, 让 {N1,...,Nm}, m>=2, 为来自于 SC 的前面没有类的类[class]. 让 (C1...Cn), n>=1, 为目前为止已构建的类优先级列表[class precedence list]. C1 是最具体的类[class], 而 Cn 最不具体的. 让 1 <= j <= n 成为最大一个数这样一来这里存在 i 其中 1 <= i <= m 并且 Ni 是 Cj 的一个直接超类[superclass]; Ni 被放到下一个.

这个从一组没有前导类的类[class]集合中选择的规则的效果是, 一个简单的超类[superclass]链中的类[class]在类优先级列表[class precedence list]中是相邻的, 并且每个相对独立的子图中的类[class]在类优先级列表[class precedence list]中是相邻的. 例如, 让 T1 和 T2 成为子图, 其唯一公共的元素是类 J. 假设没有 J 的超类出现在 T1 或 T2 中, 并且这个 J 存在于 T1 和 T2 的每个类的超类链中. 让 C1 是 T1 底部; 并且让 C2 是 T2 的底部. 假设这个顺序下 C 是一个直接超类是按照那个顺序 C1 和 C2 的类[class], 那么 C 的类优先级列表[class precedence list]以 C 开始后面跟着 T1 的所有类[class]除了 J. 后面是 T2 的所有类[class]. 类[class] J 和它的超类[superclass]出现在最后. 

#### 4.3.5.2 确定类优先级列表的示例

这个实例确定一个类 pie 的类优先级列表[class precedence list]. 定义下面这些类[class]:

```LISP
(defclass pie (apple cinnamon) ())

(defclass apple (fruit) ())

(defclass cinnamon (spice) ())

(defclass fruit (food) ())

(defclass spice (food) ())

(defclass food () ())
```

集合 Spie = {pie, apple, cinnamon, fruit, spice, food, standard-object, t}. 集合 R = {(pie, apple), (apple, cinnamon), (apple, fruit), (cinnamon, spice),
(fruit, food), (spice, food), (food, standard-object), (standard-object, t)}.

类 pie 的前面什么都没有, 所以它是第一个; 目前为止结果是 (pie). 从 S 中移除 pie 并且从 R 中移除提及到 pie 的对(pair)来得到 S = {apple, cinnamon, fruit, spice, food, standard-object, t} 还有 R = {(apple, cinnamon), (apple, fruit), (cinnamon, spice),
(fruit, food), (spice, food), (food, standard-object), (standard-object, t)}.

类 apple 前面没有任何东西, 所以它是下一个; 结果是 (pie apple). 移除 apple 和相关的对得到 S = {cinnamon, fruit, spice, food, standard-object, t} 以及 R = {(cinnamon, spice), (fruit, food), (spice, food), (food, standard-object),
(standard-object, t)}.

类 cinnamon 和 fruit 前面什么都没有, 所以目前为止有着类优先级列表[class precedence list]中最右边的直接子类[subclass]的那一个是下一个. 类 apple 是 fruit 的直接子类[subclass], 而类 pie 是一个 cinnamon 的直接子类[subclass]. 因为类优先级列表[class precedence list]中 apple 出现 pie 的右边, fruit 是下一个, 目前的结果是 (pie apple fruit). S = {cinnamon, spice, food, standard-object, t}; R = {(cinnamon, spice), (spice, food),(food, standard-object), (standard-object, t)}.

类 cinnamon 是下一个, 目前结果为 (pie apple fruit cinnamon). 这时 S = {spice, food, standard-object, t}; R = {(spice, food), (food, standard-object), (standard-object, t)}.

类 spice, food, standard-object, 还有 t 按这个顺序添加, 然后类优先级列表[class precedence list]为 (pie apple fruit cinnamon spice food standard-object t).

编写一组不能被排序的类[class]定义是可能的. 比如:

```LISP
(defclass new-class (fruit apple) ())

(defclass apple (fruit) ())
```

类 fruit 必须先于 apple 因为必须保留超类[superclass]的局部顺序. 类 apple 必须先于 fruit 因为一个类必须先于它自己的超类[superclass]. 当这个情况发生时, 会发出一个错误, 就像在这里系统尝试去计算 new-class 的类优先级列表[class precedence list]时发生的那样.

以下可能是一组相互冲突的定义:

```LISP
(defclass pie (apple cinnamon) ())

(defclass pastry (cinnamon apple) ())

(defclass apple () ())

(defclass cinnamon () ())
```

这个 pie 的类优先级列表[class precedence list]是 (pie apple cinnamon standard-object t).

这个 pastry 的类优先级列表[class precedence list]是 (pastry cinnamon apple standard-object t).

在 pie 的超类[superclass]的顺序中 apple 先于 cinnamon 不是问题, 但是在 pastry 中则有问题. 然而, 去构建一个同时有 pie 和 pastry 作为超类[superclass]的新的类[class]是不可能的. 

### 4.3.6 <span id="RedefiningClasses">重定义类</span>

作为一个 standard-class 的直接实例[direct instance]的类[class]可以被重定义, 但只有当重定义的新类[class]也是 standard-class 类的直接实例[direct instance]的情况下. 重定义一个类[class]会修改已存在的类[class]对象[object]来反映这个新的类[class]的定义; 它不会为这个类[class]创建一个新的类[class]对象[object]. 任何由旧的 defclass 表达式形式指定的 :reader, :writer, 或 :accessor 选项所创建的方法[method]对象[object]会从对应广义函数[generic function]中被移除. 由这个新的 defclass 表达式形式指定的那些方法[method]会被添加进去.

当这个类 C 被重定义时, 修改会传递到它的实例[instance]以及它的任何子类[subclass]的实例[instance]. 更新这样一个实例[instance]发生的时间依赖于具体实现[implementation-dependent], 但是不会晚于下一次这个实例[instance]的槽[slot]被读取或写入. 更新一个实例[instance]不会改变由函数[function] eq 定义的它的等价性. 更新的过程可能改变这个特别实例[instance]的槽[slot], 但是不会创建一个新实例[instance]. 更新一个实例是否消耗存储是依赖于具体实现的[implementation-dependent].

注意, 重定义一个类[class]可能导致槽[slot]被添加或删除. 如果一个类[class]被重定义, 以一种改变实例[instance]中可访问[accessible]的局部槽[local slot]的集合的方式, 那么实例[instance]就会被更新. 如果一个类[class]被重定义, 以一种没有改变实例[instance]中可访问[accessible]的局部槽[local slot]的集合的方式, 实例[instance]是否被更新是依赖于具体实现的[implementation-dependent].

在旧的类[class]和新的类[class]中都指定为共享的槽[slot]的值会被保留. 如果这样一个共享槽[shared slot]在旧的类[class]中没绑定, 它在新的类[class]里也是没有绑定的. 在旧的类[class]中是局部的而在新的类[class]中是共享的槽[slot]会被初始化. 新添加的共享槽[shared slot]会被初始化.

每一个新添加的共享槽[shared slot]会被设置为新类[class]的 defclass 表达式形式[form]中指定的这个槽的被捕获的初始化表达式形式[captured initialization form]的求值结果. 如果这里没有初始化表达式形式[initialization form], 这个槽[slot]就是未绑定的.

如果一个类[class]被重定义, 以一种改变实例[instance]中可访问[accessible]的局部槽[local slot]的集合的方式, 那么就会发生一个更新这个类[class]的实例[instance]的两步式步骤就会发生. 这个过程可能通过调用广义函数 make-instances-obsolete 来明确开始. 这个两步式的过程可以发生在一些实现的其他情况中. 例如, 在一些实现中如果槽[slot]在存储中的顺序被改变, 这个两步式过程也会被触发.

第一步通过添加新的局部槽[local slot]和丢弃这个类[class]的新版本中没定义的局部槽[local slot]来修改这个实例[instance]的结构. 第二部来初始化新添加的局部槽[local slot]并且执行任何其他的用户定义的动作. 这两个步骤在下面两个章节中会进一步说明.

> * 4.3.6.1 [修改实例的结构](#ModifyingStructureInstances)
> * 4.3.6.2 [初始化新添加的局部槽](#InitializingSlots)
> * 4.3.6.3 [定制类的重定义](#CustomizingClassRedefinition)

#### 4.3.6.1 <span id="ModifyingStructureInstances">修改实例的结构</span>

第一步修改重定义类[class]的实例[instance]的结构来使之符合新的类[class]定义. 在新的类[class]定义中增加的局部槽[local slot]而在旧的类[class]定义中既没有指定为局部的也不是共享的槽会被添加, 并且在新的类[class]定义中既不是局部也不是共享的而在旧的类[instance]定义中指定为局部的槽会被丢弃. 这些新添加和丢弃的槽[slot]的名字作为参数传递给下一章节所描述的 update-instance-for-redefined-class.

旧的和新的类[class]中都指定的局部槽[local slot]的值会被保留. 如果这样一个局部槽[local slot]是未绑定的, 它就保留为未绑定的.

在旧的类[class]中是共享的而在新的类[class]中指定为局部的槽[slot]的值会保留. 如果这样一个共享槽[shared slot]是未绑定的, 那么这个局部槽[local slot]也是为绑定的. 

#### 4.3.6.2 <span id="InitializingSlots">初始化新添加的局部槽</span>

第二步初始化新添加的那些局部槽[local slot]并且执行任何其他用户定义的动作. 这个步骤被广义函数 update-instance-for-redefined-class 实现, 这个函数在第一个修改实例[instance]结构的步骤完成后被调用.

广义函数 update-instance-for-redefined-class 需要 4 个必要参数: 在经历过第一个步骤之后要被更新的实例[instance], 被添加的局部槽[local slot]的名称列表, 被丢弃的局部槽[local slot]的名称列表, 还有一个包含被丢弃的并且有值的槽[slot]的名字和槽[slot]的值的属性列表. 被丢弃的槽[slot]中包括旧类[class]中是局部的而新类[class]中是共享的槽[slot].

广义函数 update-instance-for-redefined-class 也接受任意数量的初始化参数. 当它被系统调用来更新一个类[class]已经被重定义的实例[instance]时, 不会提供初始化参数.

这里有一个系统提供的关于 update-instance-for-redefined-class 主方法[method], 它的实例参数的参数指定符[parameter specializer]是一个 standard-object 类[class]. 首先这个方法[method]检测初始化参数的有效性, 如果一个提供的参数没有被合法声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (声明初始化参数的有效性).) 然后它调用广义函数 shared-initialize 并传入以下参数: 这个实例[instance], 新添加槽[slot]的名称[name]列表, 还有它收到的初始化参数. 

#### 4.3.6.3 <span id="CustomizingClassRedefinition">定制类的重定义</span>

这个 update-instance-for-redefined-class 的方法[method]可能被定义用来指定当一个实例[instance]被更新时采取的动作. 如果只定义了 update-instance-for-redefined-class 的 after 方法[after method], 那么它们将在系统提供的的初始化主方法[method]之后运行, 因此不会影响 update-instance-for-redefined-class 的默认行为. 被系统调用时由于没有传递初始化参数给 update-instance-for-redefined-class, 被 update-instance-for-redefined-class 的 before 方法[before method]填充的槽的初始化表达式形式[initialization form]不会被 shared-initialize 求值.

这个 shared-initialize 的方法[method]可能被定义用来定制类[class]的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 

### 4.3.7 <span id="IntegratingTypesClasses">整合类和类型</span>

对象系统映射类[class]的空间到类型[type]的空间. 每个有着特有的的名字的类[class]都有一个对应相同名字[name]的类型[type].

每个类的专有名字[proper name]是一个有效的类型指定符[type specifier]. 另外, 每个类[class]对象[object]是一个有效的类型指定符[type specifier]. 所以表达式 (typep object class) 在 object 的类[class]是 class 本身或者 class 的子类[subclass]情况下返回 true. 如果 class1 是 class2 的一个子类[subclass]或者它们是相同的类[class], 那么表达式 (subtypep class1 class2) 的求值返回多值 true 和 true; 否则它返回多值 false 和 true. 如果 I 是某个名为 S 的类[class] C 的实例[instance]并且 C 是 standard-class 的实例[instance], 那么如果 S 是 C 的专有名字[proper name], 那么表达式 (type-of I) 的求值返回 S; 否则, 它返回 C.

由于类[class]的名字和类[class]对象[object]都是类型指定符[type specifier], 它们可能被用于特殊表达式形式 the 以及类型声明中.

很多但不是全部预定义的类型指定符[type specifier]都有和类型[type]有着相同专有名字[proper name]的类[class]. 这些类型指定符[type specifier]列在 Figure 4-8. 比如, 类型[type] array 有一个对应的名为 array 的类. 例如 (vector double-float 100) 这样的列表作为类型指定符[type specifier]没有一个对应的类[class]. 操作符[operator] deftype 不会创建任何类[class].

对应于预定义类型指定符[type specifier]的每个类[class]可以通过以下三种方式实现, 由每个具体实现决定. 它可以是一个标准类[standard class], 一个结构体类[structure class], 或者一个系统类[system class].

一个内置类[built-in class]是一个广义实例[generalized instance]具有限制功能和特殊表示的类. 尝试使用 defclass 去定义 built-in-class 的子类[subclasse]会发出一个错误. 调用 make-instance 去创建一个内置类[built-in class]的广义实例[generalized instance]会发出一个错误. 对一个内置类[built-in class]的广义实例[generalized instance]调用 slot-value 会发出一个错误. 重定义一个内置类[built-in class]或使用 change-class 去改变一个内置类内置类[built-in class]对象[object]的类[class]或把一个对象[object]的类[class]改为内置类[built-in class]都会发出一个错误. 然而, 内置类[built-in class]可以被用作方法[method]的参数指定符[parameter specializer].

可以通过检查元类[metaclass]来确定一个类[class]是否为内置类[built-in class]. 一个标准类[standard class]是类[class] standard-class 的一个实例[instance], 一个内置类[built-in class]是类[class] built-in-class 的实例[instance], 并且一个结构体类[structure class]是类[class] structure-class 的实例[instance].

每一个用 defstruct 创建的没有使用 :type 选项的结构体[structure]类型[type]都有一个对应的类[class]. 这个类[class]是一个类[class] structure-class 的广义实例[generalized instance]. 这个 defstruct 的 :include 选项会创建一个对应被包含的结构体[structure]类型[type]的类[class]的直接子类[subclass].

槽[slot]是否被牵涉到在这个规范定义的类[class]的实例[instance]上的这个规范定义的函数[function]的操作中是依赖于具体实现的[implementation-dependent], 除非这个槽[slot]被这个规范显式定义.

如果在一个特定的具体实现[inplementation]中的这个规范定义的一个类[class]有着没有在这个规范中定义的槽[slot], 这些槽[slot]的名称不能是该规范中定义的包[package]的外部符号[external symbol], 也不能是 CL-USER 包中可访问的[accessible].

指定许多标准类型指定符[type specifier]有相应的类[class]的目的是使用户能够编写对这些类型进行区别对待的方法[method]. 方法[method]选择要求可以为每个类确定一个类优先级列表[class precedence list].

类型指定符[type specifier]之间的层次关系通过与这些类型[type]对应的类[class]之间的关系来反映.

Figure 4-8 列出了预定义类型指定符[type specifier]对应的类[class]的集合.

    arithmetic-error                  generic-function    simple-error          
    array                             hash-table          simple-type-error       
    bit-vector                        integer             simple-warning          
    broadcast-stream                  list                standard-class          
    built-in-class                    logical-pathname    standard-generic-function  
    cell-error                        method              standard-method         
    character                         method-combination  standard-object         
    class                             null                storage-condition       
    complex                           number              stream                  
    concatenated-stream               package             stream-error            
    condition                         package-error       string                  
    cons                              parse-error         string-stream           
    control-error                     pathname            structure-class         
    division-by-zero                  print-not-readable  structure-object        
    echo-stream                       program-error       style-warning           
    end-of-file                       random-state        symbol                  
    error                             ratio               synonym-stream          
    file-error                        rational            t                       
    file-stream                       reader-error        two-way-stream          
    float                             readtable           type-error              
    floating-point-inexact            real                unbound-slot            
    floating-point-invalid-operation  restart             unbound-variable        
    floating-point-overflow           sequence            undefined-function      
    floating-point-underflow          serious-condition   vector                  
    function                          simple-condition    warning                 

Figure 4-8. 对应预定义类型指定符的类

在这些类的条目中指定的类优先级列表[class precedence list]信息就是对象系统所需要的那些.

可以扩展单独的具体实现来定义其他类型指定符来拥有相应的类[class]. 单个具体实现可以扩展去添加其他子类[subclass]关系, 并将其他元素[element]添加到类优先级列表[class precedence list]中, 只要它们不违反该标准所指定的类型关系和互斥性需求. 一个已定义的没有指定直接超类[superclass]的标准类[class]保证和这个表中的所有类都是互斥的, 除了名为 t 的类. 

## 4.4 <span id="TheTypesClassesDictionary">类型和类的字典</span>

> * [类型 NIL](#TypeNIL)
> * [类型 BOOLEAN](#TypeBOOLEAN)
> * [系统类 FUNCTION](#SystemClassFUNCTION)
> * [类型 COMPILED-FUNCTION](#TypeCOMPILEDFUNCTION)
> * [系统类 GENERIC-FUNCTION](#SystemClassGENERICFUNCTION)
> * [系统类 STANDARD-GENERIC-FUNCTION](#SystemClassSTANDARDGENERICFUNCTION)
> * [系统类 CLASS](#SystemClassCLASS)
> * [系统类 BUILT-IN-CLASS](#SystemClassBUILTINCLASS)
> * [系统类 STRUCTURE-CLASS](#SystemClassSTRUCTURECLASS)
> * [系统类 STANDARD-CLASS](#SystemClassSTANDARDCLASS)
> * [系统类 METHOD](#SystemClassMETHOD)
> * [系统类 STANDARD-METHOD](#SystemClassSTANDARDMETHOD)
> * [类 STRUCTURE-OBJECT](#ClassSTRUCTUREOBJECT)
> * [类 STANDARD-OBJECT](#ClassSTANDARDOBJECT)
> * [系统类 METHOD-COMBINATION](#SystemClassMETHODCOMBINATION)
> * [系统类 T](#SystemClassT)
> * [类型指定符 SATISFIES](#TypeSpecifierSATISFIES)
> * [类型指定符 MEMBER](#TypeSpecifierMEMBER)
> * [类型指定符 NOT](#TypeSpecifierNOT)
> * [类型指定符 AND](#TypeSpecifierAND)
> * [类型指定符 OR](#TypeSpecifierOR)
> * [类型指定符 VALUES](#TypeSpecifierVALUES)
> * [类型指定符 EQL](#TypeSpecifierEQL)
> * [函数 COERCE](#FunctionCOERCE)
> * [宏 DEFTYPE](#MacroDEFTYPE)
> * [函数 SUBTYPEP](#FunctionSUBTYPEP)
> * [函数 TYPE-OF](#FunctionTYPEOF)
> * [函数 TYPEP](#FunctionTYPEP)
> * [状况类型 TYPE-ERROR](#ConditionTypeTYPEERROR)
> * [函数 TYPE-ERROR-DATUM, TYPE-ERROR-EXPECTED-TYPE](#FunctionTEDTEET)
> * [状况类型 SIMPLE-TYPE-ERROR](#ConditionTypeSIMPLETYPEERROR)

### <span id="TypeNIL">类型 NIL</span>

* 超类型(Supertypes):

        所有类型[type]

* 描述(Description):

        类型[type] nil 不含任何对象[object]所以也被称为空类型[empty type]. 类型[type] nil 是所有类型[type]的子类型[subtype]. 没有 nil 类型[type]的对象[object].

* 注意(Notes):

        包含对象[object] nil 的类型[type]是 null, 不是类型[type] nil. 

### <span id="TypeBOOLEAN">类型 BOOLEAN</span>

* 超类型(Supertypes):

        boolean, symbol, t

* 描述(Description):

        类型[type] boolean 包含符号[symbol] t 和 nil, 它们分别表示 true 和 false.

* 参见(See Also):

        t (常变量), nil (常变量), if, not, complement

* 注意(Notes):

        条件操作, 比如 if, 允许广义 boolean [generalized boolean]的使用, 不只是 boolean; 任何非 nil [non-nil]的值, 不只是 t, 对于广义的 boolean [generalized boolean]都视作 true. 然而, 作为惯例, 即便对于广义的 boolean [generalized boolean]当没有更好的选择来表示它自身时, 符号[symbol] t 被当作规范化的值来使用. 

### <span id="SystemClassFUNCTION">系统类 FUNCTION</span>

* 类优先级列表(Class Precedence List):

        function, t

* 描述(Description):

        当适当数量的参数被提供时, 一个函数[function]是一个表示要被执行的代码的对象. 一个函数[function]由 function 特殊表达式形式[special form], 函数[form] coerce, 或函数[form] compile 产生. 一个函数[function]可以通过把它作为第一个参数给 funcall, apply, 或 multiple-value-call 来直接调用.

* 复合类型指定符种类(Compound Type Specifier Kind):

        特化.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        function [arg-typespec [value-typespec]]

        arg-typespec::= (typespec*  
                        [&optional typespec*]  
                        [&rest typespec]  
                        [&key (keyword typespec)*]) 

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符[type specifier].
        value-typespec---一个类型指定符[type specifier].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这个 function 类型指定符的列表形式只能被用于声明不能用于辨别(discrimination). 这个类型[type]的每一个元素都是一个函数[function], 它接受由 argj-types 指定类型的参数并返回 value-type 指定类型的成员的返回值. 这里的 &optional, &rest, &key, 和 &allow-other-keys 标记可以出现在参数类型的列表中. 使用 &rest 提供的类型指定符[type specifier]是每一个实际参数的类型[type], 不是对应变量的类型[type].

        这个 &key 参数应该以 (keyword type) 这样形式的列表来提供. 这个关键字 keyword 必须是一个合法的关键字名字符号, 必须在调用的实际参数中提供. 这个通常是 KEYWORD 包中的符号[symbol]但是可以是任何符号[symbol]. 当 &key 在一个 function 类型指定符[type specifier]的 lambda 列表[lambda list]中被提供时, 这个给定的关键字参数[keyword parameter]必须是详尽的, 除非 &allow-other-keys 也出现了. &allow-other-keys 是一个指示符, 表示其他的关键字参数实际中也可以被提供, 如果提供了, 就可以被使用. 比如, 函数 make-list 类型可以按以下的方式来声明:

    ```LISP
    (function ((integer 0) &key (:initial-element t)) list)
    ```

        这个 value-type 可以是一个 values 类型指定符[type specifier]用来表示多值[multiple values]的类型.

        细想以下形式的一个声明:

    ```LISP
    (ftype (function (arg0-type arg1-type ...) val-type) f))
    ```

        任何在这个声明的作用域中的 (f arg0 arg1 ...) 表达式形式[form]等价于以下:

    ```LISP
    (the val-type (f (the arg0-type arg0) (the arg1-type arg1) ...))
    ```

        这也就是说, 如果任意参数不是指定的类型[type]或者结果不是指定的类型[type], 那么结果是未定义的. 具体来说, 如果任意参数不是正确的类型[type], 结果就不保证是指定的类型[type].

        因此, 一个函数[function]的 ftype 声明描述了这个函数[function]的调用[call], 不是这个函数[function]实际的定义.

        细想一个以下形式的声明:

    ```LISP
    (type (function (arg0-type arg1-type ...) val-type) fn-valued-variable)
    ```

        这个声明有着这样的解释, 在这个声明的作用域里, 如果 fn-valued-variable 的值被调用, 参数不是指定的类型[type], 那么结果是未定义的; 一个合法调用产生的值会是 val-type 类型.

        与变量类型声明一样, 嵌套声明意味着类型[type]的交集, 如下:

        * 细想以下两种 ftype 的声明:

            ```LISP
            (ftype (function (arg0-type1 arg1-type1 ...) val-type1) f)
            ```

            还有

            ```LISP
            (ftype (function (arg0-type2 arg1-type2 ...) val-type2) f)
            ```

            如果这些声明都生效, 那么在这些声明的共享作用域内, 对 f 的调用可以被认为它是按下面这种方式声明的:

            ```LISP
            (ftype (function ((and arg0-type1 arg0-type2) (and arg1-type1 arg1-type2 ...) ...)
                            (and val-type1 val-type2)) 
                f))
            ```

            允许忽略一个或全部有效的 ftype 声明.

        * 如果一个变量的两个(或更多)类型声明生效, 并且它们都是 function 声明, 这些声明也会类似地组合. 

### <span id="TypeCOMPILEDFUNCTION">类型 COMPILED-FUNCTION</span>

* 超类型(Supertypes):

        compiled-function, function, t

* 描述(Description):

        如果一个函数不包含必须在运行时展开的宏[macro]的引用, 并且它也不包含加载期值[load time values]的未解决的引用, 那么任何这样的函数都可以被具体实现[implementation]认为是一个编译后的函数[compiled function]. 见章节 3.2.2 (编译语义).

        一个词法上出现在一个被 compile-file 编译并且被 load 加载的文件[file]中的函数[function]定义是 compiled-function 类型[type]. compile 函数产生的函数[function]也是 compiled-function 类型[type]. 其他函数[function]也可能是 compiled-function 类型[type]. 

### <span id="SystemClassGENERICFUNCTION">系统类 GENERIC-FUNCTION</span>

* 类优先级列表(Class Precedence List):

        generic-function, function, t

* 描述(Description):

        一个广义函数[generic function]是一个行为取决于提供给它的实参[argument]标识或类[class]的函数[function]. 一个广义函数对象包含一个方法[method]的集合, 一个 lambda 列表[lambda list], 一个方法组合[method combination]类型[type], 还有其他信息. 方法[method]定义了广义函数[generic function]的特定于类的行为和操作; 一个方法[method]也说是特化[specialize]一个广义函数[generic function]. 当被调用时, 一个广义函数[generic function]基于那些类[class]和实参[argument]标识去执行它的那些方法[method]的一个子集.

        一个广义函数[generic function]可以和普通函数[function]相同的方式被使用; 特别地, 一个广义函数[generic function]可以被用作 funcall 和 apply 的参数, 并且可以被赋予一个全局或局部的名字. 

### <span id="SystemClassSTANDARDGENERICFUNCTION">系统类 STANDARD-GENERIC-FUNCTION</span>

* 类优先级列表(Class Precedence List):

        standard-generic-function, generic-function, function, t

* 描述(Description):

        类[class] standard-generic-function 是 defmethod, ensure-generic-function, defgeneric, 和 defclass 表达式形式[form]建立的广义函数[generic function]的默认类[class]. 

### <span id="SystemClassCLASS">系统类 CLASS</span>

* 类优先级列表(Class Precedence List):

        class, standard-object, t

* 描述(Description):

        类型[type] class 表示确定它们的实例[instance]的结构的行为的对象[object]. 与 class 类型[type]的对象[object]相关联的信息是描述其在类[class]的非循环有向图中的位置, 它的槽[slot]还有它的选项的信息. 

### <span id="SystemClassBUILTINCLASS">系统类 BUILT-IN-CLASS</span>

* 类优先级列表(Class Precedence List):

        built-in-class, class, standard-object, t

* 描述(Description):

        一个内置类[built-in class]是一个实例[instance]具有限制功能和特殊表示的类[class]. 尝试用 defclass 去定义一个内置类[built-in class]的子类[subclass]会发出一个 error 类型[type]的错误. 调用 make-instance 去创建一个内置类[built-in class]的实例[instance]会发出一个 error 类型[type]的错误. 在一个内置类[built-in class]的实例[instance]上调用 slot-value 会发出一个 error 类型[type]的错误. 重定义一个内置类[built-in class]或使用 change-class 去改变一个实例[instance]的类[class]为内置类[built-in class]或改变一个内置类[built-in class]为其他类会发出一个 error 类型[type]的错误. 然而, 内置类[built-in class]可以被用作方法[method]的参数指定符[parameter specializer]. 

### <span id="SystemClassSTRUCTURECLASS">系统类 STRUCTURE-CLASS</span>

* 类优先级列表(Class Precedence List):

        structure-class, class, standard-object, t

* 描述(Description):

        所有通过 defstruct 定义的类[class]的都是类[class] structure-class 的实例[instance]. 

### <span id="SystemClassSTANDARDCLASS">系统类 STANDARD-CLASS</span>

* 类优先级列表(Class Precedence List):

        standard-class, class, standard-object, t

* 描述(Description):

        类[class] standard-class 是 defclass 定义出来的类[class]的默认类[class]. 

### <span id="SystemClassMETHOD">系统类 METHOD</span>

* 类优先级列表(Class Precedence List):

        method, t

* 描述(Description):

        一个方法[method]是表示一个广义函数[generic function]的行为的模块化部分的对象[object].

        一个方法[method]包含了实现这个方法[method]行为的代码[code], 一个指定什么时候这个给定方法[method]可以被应用的参数指定符[parameter specializer]序列, 还有一个被用于方法组合机制来辨别方法的限定符[qualifier]序列. 每一个方法[method]的每一个必要参数都有一个关联的参数指定符[parameter specializer], 并且只有在参数满足方法的这些参数指定符[parameter specializer]时, 该方法才会被调用.

        方法组合机制控制方法[method]的选择, 它们执行的顺序, 还有这个广义函数返回的值. 对象系统提供一个默认方法组合类型并且提供一个机制来声明新的方法组合类型.

* 参见(See Also):

        章节 7.6 (广义函数和方法) 

### <span id="SystemClassSTANDARDMETHOD">系统类 STANDARD-METHOD</span>

* 类优先级列表(Class Precedence List):

        standard-method, method, standard-object, t

* 描述(Description):

        这个类[class] standard-method 是 defmethod 和 defgeneric 表达式形式[form]定义的方法[method]的默认类[class]. 

### <span id="ClassSTRUCTUREOBJECT">类 STRUCTURE-OBJECT</span>

* 类优先级列表(Class Precedence List):

        structure-object, t

* 描述(Description):

        类[class] structure-object 是 structure-class 的一个实例[instance], 并且是作为 structure-class 的实例[instance]的除了它自身以外的每一个类[class]的超类[superclass], 并且是 defstruct 定义的每一个类[class]的超类[superclass].

* 参见(See Also):

        defstruct, 章节 2.4.8.13 (井号S(#S)), 章节 22.1.3.12 (打印结构体) 

### <span id="ClassSTANDARDOBJECT">类 STANDARD-OBJECT</span>

* 类优先级列表(Class Precedence List):

        standard-object, t

* 描述(Description):

        类[class] standard-object 是 standard-class 的一个实例[instance]并且是作为 standard-class 的实例[instance]的除了它自身以外的每个类[class]的超类[superclass]. 

### <span id="SystemClassMETHODCOMBINATION">系统类 METHOD-COMBINATION</span>

* 类优先级列表(Class Precedence List):

        method-combination, t

* 描述(Description):

        每一个方法组合[method combination]对象[object]是类[class] method-combination 的间接实例[indirect instance]. 一个方法组合[method combination]对象[object]表示这个方法组合[method combination]被一个广义函数[generic function]所使用的信息. 一个[method combination]对象[object]包含了方法组合[method combination]的类型还有这个类型[type]要使用的参数相关的信息. 

### <span id="SystemClassT">系统类 T</span>

* 类优先级列表(Class Precedence List):

        t

* 描述(Description):

        所有对象[object]的集合. 类型[type] t 是每个类型[type]的超类型[supertype], 包括它自身. 每个对象[object]都是类型[type] t. 

### <span id="TypeSpecifierSATISFIES">类型指定符 SATISFIES</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        谓语(Predicating).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        satisfies predicate-name

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        predicate-name---一个符号.

* 复合类型指定符的描述(Compound Type Specifier Description):

        这个表示满足断言[predicate] predicate-name 的所有对象[object]的集合, 这个断言必须是全局函数[function]定义为单参数断言的符号[symbol]. predicate-name 需要一个名字; 不允许 lambda 表达式[lambda expression]. 比如, 类型指定符[type specifier] (and integer (satisfies evenp)) 表示偶数整型的集合. 表达式 (typep x '(satisfies p)) 等价于 (if (p x) t nil).

        这个参数是必要的. 符号[symbol] * 可以是参数, 但是它表示它自身 (这个符号[symbol] *), 不表示一个未指定的类型.

        符号 satisfies 作为类型指定符[type specifier]是不合法的. 

### <span id="TypeSpecifierMEMBER">类型指定符 MEMBER</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        结合(Combining).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        member object*

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        object---一个对象[symbol].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这表示这个集合包含 objects 命名的对象. 一个对象[object]只有在它 eql 指定的那些对象 objects 中的其中一个时才是这个类型[type].

        类型指定符[type specifier] (member) 和 nil 是等价的. * 可以在那些对象 objects 之中, 但是如果这样的话它表示它自身 (这个符号[symbol] *) 而不是表示一个未指定的值. 符号 member 作为类型指定符[type specifier]是非法的; 并且, 特别指出, 它既不是 (member) 的缩写也不是 (member *) 的缩写.

* 参见(See Also):

        类型[type] eql 

### <span id="TypeSpecifierNOT">类型指定符 NOT</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        结合(Combining).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        not typespec

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符[type specifier].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这表示所有不是类型[type] typespec 的对象的集合.

        这个参数是必要的, 并且不能是 *.

        符号 not 作为类型指定符[type specifier]是非法的. 

### <span id="TypeSpecifierAND">类型指定符 AND</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        结合(Combining).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        and typespec*

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符[type specifier].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这表示所有 typespecs 的交集所确定的类型[type]的对象[object]集合.

        * 不允许作为参数.

        类型指定符[type specifier] (and) 和 t 是等价的. 符号 and 作为一个类型指定符[type specifier]是不合法的, 并且, 特别指出, 它不是 (and) 的一个缩写. 

### <span id="TypeSpecifierOR">类型指定符 OR</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        结合(Combining).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        or typespec*

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符[type specifier].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这表示所有 typespecs 的并集所确定的类型[type]的对象[object]集合. 比如, 类型[type] list 定义等价于 (or null cons). 同样, 通过 position 返回的值是类型[type] (or null (integer 0 *)) 的一个对象[object]; 换句化说, 可以是 nil 或者一个非负整数[integer].

        * 不允许作为一个参数.

        类型指定符[type specifier] (or) 和 nil 是等价的. 符号 or 作为类型指定符[type specifier]是非法的; 并且, 特别指出, 这个不是 (or) 的缩写. 

### <span id="TypeSpecifierVALUES">类型指定符 VALUES</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        特化(Specializing).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        values value-typespec

        value-typespec::= typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys] 

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符[type specifier].

* 复合类型指定符的描述(Compound Type Specifier Description):

        这个类型指定符[type specifier]只能被用作一个 function 类型指定符[type specifier]或一个 the 特殊表达式形式[special form]的 value-type 部分. 当涉及到多值[multiple values]时它被用于指定单独的类型[type]. 这个 &optional 和 &rest 标记可以出现在 value-type 列表中; 它们指定和值一起传递给 multiple-value-call 的一个函数[function]的参数列表会正确地接收到那些值.

        符号 * 不能在 value-types 之中.

        符号 values 作为类型指定符[type specifier]是不合法的; 并且, 特别指出, 它不是 (values) 的缩写. 

### <span id="TypeSpecifierEQL">类型指定符 EQL</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        结合(Combining).

* 复合类型指定符语法(Compound Type Specifier Syntax):

        eql object

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        object---一个对象.

* 复合类型指定符的描述(Compound Type Specifier Description):

        表示对于 (eql object x) 为 true 的所有 x 的类型[type].

        参数 object 是必要的. 这个 object 可以是 *, 但是如果是这样它表示它自身(这个符号[symbol] *) 并且不表示一个未指定的值. 符号[symbol] eql 作为一个原子类型指定符[atomic type specifier]是不合法的. 

### <span id="FunctionCOERCE">函数 COERCE</span>

* 语法(Syntax):

        coerce object result-type => result

* 参数和值(Arguments and Values):

        object---一个对象[object].
        result-type---一个类型指定符[type specifier].
        result---一个类型[type] result-type 的对象[object], 除了在章节 12.1.5.3 (复有理数的正规表示规则) 所描述的情况外.

* 描述(Description):

        强制转换[coerce]对象 object 为类型[type] result-type.

        如果对象 object 已经是类型[type] result-type, 返回对象 object 自身, 一般不管是否会有可能强制一些其他类型的对象为 result-type.

        否则, 这个 object 根据以下规则强制转换为类型[type] result-type:

    * sequence

            如果这个 result-type 是 list 的一个可识别子类型[recognizable subtype], 并且这个对象[object]是一个序列[sequence], 那么这个 result 是一个和对象 object 有者相同[same]元素[element]的列表[list].

            如果 result-type 是 vector 的一个可识别子类型[recognizable subtype], 并且这个对象[object]是一个序列[sequence], 那么这个 result 是一个和对象 object 有者相同[same]元素[element]的向量[vector]. 如果 result-type 是一个特化的类型[type], 那么 result 会有一个实际数组元素类型[actual array element type], 它是对特化[specialized]类型[type]的元素类型部分进行提升的结果. 如果没有指定元素类型, 那么这个元素类型默认是 t. 如果具体实现[implementation]不能确定元素类型, 会发出一个错误.

    * character

            如果这个 result-type 是 character 并且这个对象[object]是一个字符标识符[character designator], 那么这个 result 是它表示的字符[character].

    * complex

            如果这个 result-type 是 complex 而这个对象[object]是一个实数[real], 那么这个 result 是通过构造一个实部是这个对象[object]并且虚部是将一个整数[integer] 0 强制转为这个对象[object]的类型[type]的结果(使用 coerce)的复数[complex]来获取到的. (然而, 如果这个实部是一个有理数[rational], 那么结果必须被表示为一个有理数[rational]而不是一个复数[complex]; 见章节 12.1.5.3 (复有理数的正规表示规则). 所以, 比如, (coerce 3 'complex) 允许的, 但是会返回 3, 它不是一个复数[complex].)

    * float

            如果 result-type 是 float, short-float, single-float, double-float, long-float 中的任何一个, 并且这个对象[object]是一个实数[real], 那么 result 是一个 result-type 类型[type]的浮点数[float], 无论那个浮点数[float]表示法允许的是多大的表征精度, 它都和这个对象[object]的符号和大小是相等的. (如果这个 result-type 是 float 并且对象 object 还不是一个浮点数[float], 那么这个 result 是一个单浮点数[single-float].)

    * function

            如果 result-type 是 function, 并且对象 object 是任何被 fbound 的函数名字[function name]但是既不是全局定义的宏名字[macro name]也不是特殊操作符[special operator], 那么这个 result 是对象 object 的函数值[functional value].

            如果 result-type 是 function, 而对象 object 是一个 lambda 表达式[lambda expression], 那么这个 result 是 object 在空词法环境[null lexical environment]的一个闭包[closure].

    * t

            任何对象 object 可以被强制转为 t 类型[type]的对象[object]. 这个情况下, 这个 object 被简单地返回.

* 示例(Examples):

    ```LISP
    (coerce '(a b c) 'vector) =>  #(A B C)
    (coerce 'a 'character) =>  #\A
    (coerce 4.56 'complex) =>  #C(4.56 0.0)
    (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
    (coerce 7/2 'complex) =>  7/2
    (coerce 0 'short-float) =>  0.0s0
    (coerce 3.5L0 'float) =>  3.5L0
    (coerce 7/2 'float) =>  3.5
    (coerce (cons 1 2) t) =>  (1 . 2)
    ```

        所有以下表达式形式[form]都会发出一个错误:

    ```LISP
    (coerce '(a b c) '(vector * 4))
    (coerce #(a b c) '(vector * 4))
    (coerce '(a b c) '(vector * 2))
    (coerce #(a b c) '(vector * 2))
    (coerce "foo" '(string 2))
    (coerce #(#\a #\b #\c) '(string 2))
    (coerce '(0 1) '(simple-bit-vector 3))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果一个强制转换是不可以的, 会发出一个 type-error 类型[type]的错误.

        (coerce x 'nil) 总是发出一个 type-error 类型[type]的错误.

        如果 result-type 是一个 function 但是对象 object 是一个没有被 fbound 的符号[symbol]或者这个符号[symbol]命名一个宏[macro]或特殊操作符[special operator], 那么就会发出一个 error 类型[type]的错误.

        如果 result-type 指定了元素数量而对象 object 是不同长度的, 那么应该发出 type-error 类型[type]的错误.

* 参见(See Also):

        rational, floor, char-code, char-int

* 注意(Notes):

        由于舍入问题, 没有提供从浮点数[float]到有理数[rational]和从比率[ratio]到整数[integer]的强制转换.

    ```LISP
    (coerce x 't) ==  (identity x) ==  x
    ```

### <span id="MacroDEFTYPE">宏 DEFTYPE</span>

* 语法(Syntax):

        deftype name lambda-list [[declaration* | documentation]] form* => name

* 参数和值(Arguments and Values):

        name---一个符号[symbol].
        lambda-list---一个 deftype lambda 列表[deftype lambda list].
        declaration---一个 declare 表达式[expression]; 不求值.
        documentation---一个字符串[string]; 不求值.
        form---一个表达式形式[form].

* 描述(Description):

        deftype 定义一个名为 name 的派生类型指定符[derived type specifier].

        新的类型指定符[type specifier]的意义在于一个函数, 它将这个类型指定符[type specifier]展开为另一个类型指定符[type specifier], 如果另一个类型指定符自身包含对另一个派生类型指定符[derived type specifier]的引用, 它本身就会被展开.

        新定义的类型指定符[type specifier]可以用 (name arg1 arg2 ...) 形式的一个列表来引用. 参数的数量必须和 lambda-list 一样. 如果新的类型指定符[type specifier]不接受参数, 或者它的所有参数是可选的, 这个类型指定符[type specifier]可以被用作原子类型指定符[atomic type specifier].

        给这个类型指定符[type specifier]的实参[argument]表达式[expression], arg1 ... argn, 是不求值的. 相反, 这些字面化[literal]对象[object]变成了相应的形参[parameter]被绑定[bound]的对象[object].

        这个 deftype 表达式形式[form]主体部分(不是 lambda-list)隐含在一个名为 name 的块[block]中, 并且作为一个隐式 progn [implicit progn]被求值, 返回一个新的类型指定符[type specifier].

        这个主体部分的词法环境[lexical environment]是 deftype 表达式形式被求值时的当前那个, 由 lambda-list 中的那些变量[variable]来扩展.

        作为展开式返回的类型说明符[type specifier]的递归展开必须终止, 包括在展开式中嵌套的那些类型指定符[type specifier]的展开式.

        如果完全展开一个类型指定符[type specifier]的结果包含任何环状结构, 那么其结果是未定义的, 除非是在被 member 和 eql 类型指定符[type specifier]引用的对象[object]中.

        这个 documentation 作为 type 种类的文档字符串[documentation string]关联到 name.

        如果一个 deftype 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须确保 name 在后续类型[type]声明中被识别. 如果这个 name 在后续类型[type]声明中被引用, 那么程序员[programmer]必须确保这个 deftype 表达式形式的主体部分可以在编译时被求值. 如果一个类型指定符[type specifier]的展开式没有在编译时被完全定义 (或许是因为它展开为一个未知类型指定符[type specifier]或者一个已命名函数[function]的满足因素[satisfies]没有在这个编译时环境中定义), 那么一个具体实现[implementation]可能忽略任何声明中这个类型[type]的引用 并且/或者 发出一个警告.

* 示例(Examples):

    ```LISP
    (defun equidimensional (a)
      (or (< (array-rank a) 2)
          (apply #'= (array-dimensions a)))) =>  EQUIDIMENSIONAL
    (deftype square-matrix (&optional type size)
      `(and (array ,type (,size ,size))
            (satisfies equidimensional))) =>  SQUARE-MATRIX
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        declare, defmacro, documentation, 章节 4.2.3 (类型指定符), 章节 3.4.11 (文档字符串和声明的语法交互)

* 注意(Notes): None. 

### <span id="FunctionSUBTYPEP">函数 SUBTYPEP</span>

* 语法(Syntax):

        subtypep type-1 type-2 &optional environment => subtype-p, valid-p

* 参数和值(Arguments and Values):

        type-1---一个类型指定符[type specifier].
        type-2---一个类型指定符[type specifier].
        environment---一个环境[environment]对象[object]. 默认是 nil, 表示空词法环境[null lexical environment]和当前的全局环境[global environment].
        subtype-p---一个广义的 boolean [generalized boolean].
        valid-p---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果 type-1 是 type-2 的一个可识别子类型[recognizable subtype], 第一个值[value]就是 true. 否则, 第一个值[value]就是 false, 表示 type-1 不是 type-2 的子类型[subtype], 或者 type-1 是 type-2 的子类型[subtype]但是不是一个可识别子类型[recognizable subtype].

        返回的第二个值[value]表示第一个值[value]的'确定性(certainty)'. 如果这个值是 true, 那么第一个值就是子类型[subtype]关系的精确表示. (当第一个值[value]是 true 时第二个值[value]总是为 true.)

        下面这段总结了返回值[value]的可能的组合.

            值 1     值 2     意义                                               
            true     true     type-1 确定是 type-2 的子类型[subtype].             
            false    true     type-1 确定不是 type-2 的子类型[subtype].         
            false    false    subtypep 不能确定关系, 所以 type-1 可能或可能不是 type-2 的子类型[subtype].  

            Figure 4-9. subtypep 结果的可能性

        subtypep 只有当至少一个参数涉及到后面其中一个类型指定符[type specifier]时才允许返回 false 和 false 的多值[values]: and, eql, function 的列表表达式, member, not, or, satisfies, or values. (在被类型[type]展开后, 如果一个类型指定符[type specifier]将一个符号[symbol]包含在某个位置, 该位置将调用它作为一个要被使用的类型指定符[type specifier]的意义, 那么这个类型指定符[type specifier]就'涉及'这样一个符号[symbol].) 一个可能的推论是, 如果 type-1 和 type-2 都不涉及这些类型指定符[type specifier], 那么 subtypep 不得不去准确确定关系. 具体来说, 如果那些参数是 equal 的并且不涉及任何这些类型指定符[type specifier]时 subtypep 返回 true 和 true 的多值[values].

        当 type-1 和 type-2 只涉及 Figure 4-2 中的名字, 或者由 defstruct, define-condition, 或 defclass 定义的类型[type]的名字, 或者只展开到那些名字的衍生类型[derived type]时, subtypep 第二个返回值一定不是 nil. 当列在 Figure 4-2 的类型指定符[type specifier]还有 defclass 和 defstruct 名称在一些情况下被实现为衍生类型[derived type]时, subtypep 把它们当作原语(primitive).

        subtypep 所反映的类型[type]之间的关系是特定于具体实现的那些. 比如, 如果一个实现只支持浮点数的单个类型, 在那个实现中 (subtypep 'float 'long-float) 返回 true 和 true 的多值[values] (因为两个类型[type]是一样的).

        对于所有除了 * 以外的 T1 和 T2, 当且仅当 (array T1) 和 (array T2) 指向相同特化表示的数组[array]时, 它们是总指向相同集合的两种不同的类型指定符[type specifier], 换句话说, 就是当且仅当 (upgraded-array-element-type 'T1) 和 (upgraded-array-element-type 'T2) 返回两个指向相同对象集合的不同类型指定符. 这是 `(array type-specifier) 和 `(array ,(upgraded-array-element-type 'type-specifier)) 指向相同特化数组[array]表示的另一种说法. 对于所有除了 * 以外的 T1 和 T2, 当且仅当 (array T1) 和 (array T2) 指向不同的有区别特化表示的数组时, 它们的交集是个空集合.

        因此,

    ```LISP
    (subtypep '(array T1) '(array T2)) =>  true
    ```

        当且仅当 (upgraded-array-element-type 'T1) 和 (upgraded-array-element-type 'T2) 返回两个指向相同对象[object]集合的不同类型指定符[type specifier]的时候.

        对于所有除了 * 以外的类型指定符 T1 和 T2,

    ```LISP
    (subtypep '(complex T1) '(complex T2)) =>  true, true
    ```

        如果:

        1. T1 是 T2 的子类型[subtype], 或者
        2. (upgraded-complex-part-type 'T1) 和 (upgraded-complex-part-type 'T2) 返回指向相同对象集合的不同类型指定符[type specifier]; 这个情况下, (complex T1) 和 (complex T2) 都指向相同的特化表示.

        否则多值[values]就是 false 和 true.

        表达式形式

    ```LISP
    (subtypep '(complex single-float) '(complex float))
    ```

        在所有实现一定返回 true, 但是

    ```LISP
    (subtypep '(array single-float) '(array float))
    ```

        只有在没有为单浮点[single float]和其他浮点数[float]作区分的特化数组[array]表示的具体实现中返回 true.

* 示例(Examples):

    ```LISP
    (subtypep 'compiled-function 'function) =>  true, true
    (subtypep 'null 'list) =>  true, true
    (subtypep 'null 'symbol) =>  true, true
    (subtypep 'integer 'string) =>  false, true
    (subtypep '(satisfies dummy) nil) =>  false, implementation-dependent
    (subtypep '(integer 1 3) '(integer 1 4)) =>  true, true
    (subtypep '(integer (0) (0)) 'nil) =>  true, true
    (subtypep 'nil '(integer (0) (0))) =>  true, true
    (subtypep '(integer (0) (0)) '(member)) =>  true, true ;or false, false
    (subtypep '(member) 'nil) =>  true, true ;or false, false
    (subtypep 'nil '(member)) =>  true, true ;or false, false
    ```

        让 <aet-x> 和 <aet-y> 是两个有区别的类型指定符[type specifier], 在一个给定实现中不总是指向相同对象[object]集合, 但是 make-array 对于它们, 会返回一个相同数组[array]类型[type]的对象[object].

        因此, 在每种情况下,

    ```LISP
    (subtypep (array-element-type (make-array 0 :element-type '<aet-x>))
        (array-element-type (make-array 0 :element-type '<aet-y>)))
    =>  true, true

    (subtypep (array-element-type (make-array 0 :element-type '<aet-y>))
        (array-element-type (make-array 0 :element-type '<aet-x>)))
    =>  true, true
    ```

        如果 (array <aet-x>) 和 (array <aet-y>) 是相同对象[object]集合的不同名称, 这些名字应该指向相同的对象[object]集合. 这意味着下面的测试也是正确的:

    ```LISP
    (subtypep '(array <aet-x>) '(array <aet-y>)) =>  true, true
    (subtypep '(array <aet-y>) '(array <aet-x>)) =>  true, true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        章节 4.2 (类型)

* 注意(Notes):

        对于 array 和 complex 类型, subtypep 规范中细微的差异是有必要的, 因为这里没有复数[complex]的创建函数允许产生的各个部分类型独立于这些部分的实际类型. 因此，在 complex 类型[type]的情况下, 虽然一个数字[number]可以是不止一个类型[type]的成员, 但引用的是它的实际类型. 比如, 17 是类型[type] (mod 18) 也是类型[type] (mod 256) 并且也是 integer 类型[type]; 还有 2.3f5 是类型[type] single-float 也是类型[type] float. 

### <span id="FunctionTYPEOF">函数 TYPE-OF</span>

* 语法(Syntax):

        type-of object => typespec

* 参数和值(Arguments and Values):

        object---一个对象[object].
        typespec---一个类型指定符[type specifier].

* 描述(Description):

    返回一个类型[type]的类型指定符[type specifier], typespec, 这个类型有着对象 object 作为它的一个元素[element]. 这个 typespec 满足以下条件:

    1. 对于任何是内置类型[built-in type]的元素[element]的对象 object:

        a. 返回的类型[type]是一个内置类型[built-in type]的可识别子类型[recognizable subtype].

        b. 返回的类型[type]没有涉及 and, eql, member, not, or, satisfies, 或 values.

    2. 对于所有对象 object, (typep object (type-of object)) 返回 true. 其中隐含的是类型指定符[type specifier]和 typep 一起使用是无效的, 比如 function 类型指定符[type specifier]的列表[list]表达式形式是从来不会被 type-of 返回的.

    3. type-of 返回的类型[type]总是为 class-of 返回的类[class]的可识别子类型[recognizable subtype]. 这也就是说,

        ```LISP
        (subtypep (type-of object) (class-of object)) =>  true, true
        ```

    4. 对于元类 structure-class 或 standard-class 的对象 object, 还有状况[condition], type-of 返回那个由 class-of 返回的类[class]的专有名字[proper name], 如果有的话, 否则返回这个类本身. 具体来说, 对于一个由不带 :type 选项的 defstruct 定义的结构体的构造器函数创建的对象 object, type-of 返回结构体的名字; 对于由 make-condition 创建的对象 object, 这个 typespec 就是状况[condition]类型[type]的名字[name].

    5. 对于类型[type] short-float, single-float, double-float, 或 long-float 中的每一个, 如果 object 是其中一个的元素[element], 这个 typespec 是那个类型[type]的可识别子类型[recognizable subtype].

* 示例(Examples):

    ```LISP
    (type-of 'a) =>  SYMBOL          
    (type-of '(1 . 2))
    =>  CONS
    OR=>  (CONS FIXNUM FIXNUM)
    (type-of #c(0 1))
    =>  COMPLEX
    OR=>  (COMPLEX INTEGER)
    (defstruct temp-struct x y z) =>  TEMP-STRUCT
    (type-of (make-temp-struct)) =>  TEMP-STRUCT
    (type-of "abc")
    =>  STRING
    OR=>  (STRING 3)
    (subtypep (type-of "abc") 'string) =>  true, true
    (type-of (expt 2 40))
    =>  BIGNUM
    OR=>  INTEGER
    OR=>  (INTEGER 1099511627776 1099511627776)
    OR=>  SYSTEM::TWO-WORD-BIGNUM
    OR=>  FIXNUM
    (subtypep (type-of 112312) 'integer) =>  true, true
    (defvar *foo* (make-array 5 :element-type t)) =>  *FOO*
    (class-name (class-of *foo*)) =>  VECTOR
    (type-of *foo*)
    =>  VECTOR
    OR=>  (VECTOR T 5)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        array-element-type, class-of, defstruct, typecase, typep, 章节 4.2 (类型)

* 注意(Notes):

        鼓励实现者去安排 type-of 返回一个可移植的值. 

### <span id="FunctionTYPEP">函数 TYPEP</span>

* 语法(Syntax):

        typep object type-specifier &optional environment => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        type-specifier---任何除了 values 以外的类型指定符[type specifier], 或者是一个第一个元素为 function 或 values 的类型指定符[type specifier]列表.
        environment---一个环境[environment]对象[object]. 默认是 nil, 表示空词法环境[null lexical environment]和当前的全局环境[global environment].
        generalized-boolean---一个广义的 boolean [generalized boolean].

* 描述(Description):

        如果 object 是类型指定符 type-specifier 所指定的类型[type]就返回 true;否则, 返回 false.

        表达式形式 (satisfies fn) 作为类型指定符 type-specifier 的话, 就通过应用函数 fn 给 object 来处理.

        (typep object '(array type-specifier)), 其中 type-specifier 不是 *, 当且仅当 object 是一个可以通过提供 type-specifier 作为 make-array 的 :element-type 参数所构建出来的数组[array]时返回 true. (array *) 指向所有数组[array]不管其元素类型, 而 (array type-specifier) 只指向那些可以通过把 type-specifier 作为 make-array 的 :element-type 参数所构建出来的数组[array]. 一个类似的解释可以应用于 (simple-array type-specifier) 和 (vector type-specifier). 见章节 15.1.2.1 (数组提升).

        (typep object '(complex type-specifier)) 对于所有可以通过给定 type-specifier 类型的数字到函数[function] complex 来产生的复数返回 true, 附加其他相同特化表示的复数[complex]. 任何这样的复数[complex]的实部和虚部必须满足:

    ```LISP
    (typep realpart 'type-specifier)
    (typep imagpart 'type-specifier)
    ```

        见函数[function] upgraded-complex-part-type.

* 示例(Examples):

    ```LISP
    (typep 12 'integer) =>  true
    (typep (1+ most-positive-fixnum) 'fixnum) =>  false
    (typep nil t) =>  true
    (typep nil nil) =>  false
    (typep 1 '(mod 2)) =>  true
    (typep #c(1 1) '(complex (eql 1))) =>  true
    ;; To understand this next example, you might need to refer to
    ;; Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).
    (typep #c(0 0) '(complex (eql 0))) =>  false
    ```

        让 Ax 和 Ay 为表示不同类型[type]的类型指定符[type specifier], 但是对于它们

    ```LISP
    (upgraded-array-element-type 'Ax)
    ```

        和

    ```LISP
    (upgraded-array-element-type 'Ay)
    ```

        表示相同的类型[type]. 注意

    ```LISP
    (typep (make-array 0 :element-type 'Ax) '(array Ax)) =>  true
    (typep (make-array 0 :element-type 'Ay) '(array Ay)) =>  true
    (typep (make-array 0 :element-type 'Ax) '(array Ay)) =>  true
    (typep (make-array 0 :element-type 'Ay) '(array Ax)) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 type-specifier 是 values, 或者第一个元素是 function 或 values 的类型指定符[type specifier]列表, 那么就会发出一个 error 类型[type]的错误.

        如果 type-specifier 不是一个类型指定符[type specifier], 那么结果是未定义的.

* 参见(See Also):

        type-of, upgraded-array-element-type, upgraded-complex-part-type, 章节 4.2.3 (类型指定符)

* 注意(Notes):

        鼓励具体实现[implementation]去识别和优化 (typep x (the class y)) 的情况, 因为它不需要在运行时展开 deftype 信息. 

### <span id="ConditionTypeTYPEERROR">状况类型 TYPE-ERROR</span>

* 类优先级列表(Class Precedence List):

        type-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] type-error 表示一个对象[object]不是期望类型的情况. 这个违反基准(offending datum)和期望类型(expected type)被 make-condition 的名为 :datum 和 :expected-type 的参数所初始化, 并且通过函数 type-error-datum 和 type-error-expected-type 来访问.

* 参见(See Also):

        type-error-datum, type-error-expected-type 

### <span id="FunctionTEDTEET">函数 TYPE-ERROR-DATUM, TYPE-ERROR-EXPECTED-TYPE</span>

* 语法(Syntax):

        type-error-datum condition => datum

        type-error-expected-type condition => expected-type

* 参数和值(Arguments and Values):

        condition---一个 type-error 类型[type]的状况[condition].
        datum---一个对象[object].
        expected-type---一个类型指定符[type specifier].

* 描述(Description):

        type-error-datum 返回由 condition 表示的情况[situation]的违反基准.

        type-error-expected-type 返回由 condition 表示的违反基准的期望类型.

* 示例(Examples):

    ```LISP
    (defun fix-digits (condition)
      (check-type condition type-error)
      (let* ((digits '(zero one two three four
              five six seven eight nine))
             (val (position (type-error-datum condition) digits)))
        (if (and val (subtypep 'fixnum (type-error-expected-type condition)))
          (store-value 7))))

    (defun foo (x)
      (handler-bind ((type-error #'fix-digits))
        (check-type x number)
        (+ x 3)))

    (foo 'seven)
    =>  10
    ```LISP

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        type-error, 章节 9 (状况)

* 注意(Notes): None. 

### <span id="ConditionTypeSIMPLETYPEERROR">状况类型 SIMPLE-TYPE-ERROR</span>

* 类优先级列表(Class Precedence List):

        simple-type-error, simple-condition, type-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] simple-type-error 的状况[condition]类似类型[type] type-error 的状况[condition], 除了它们提供了另一种机制来指定如何报告状况[condition]; 见类型 simple-condition.

* 参见(See Also):

        simple-condition, simple-condition-format-control, simple-condition-format-arguments, type-error-datum, type-error-expected-type 

