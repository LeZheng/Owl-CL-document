# 4. Types and Classes

> * 4.1 [引言](#Introduction)
> * 4.2 [类型](#Types)
> * 4.3 [类](#Classes)
> * 4.4 [类型和类的字典](#TheTypesClassesDictionary)

## 4.1 <span id="Introduction">引言</span>

一个类型(type)是一个(可能是无数)对象的集合. 一个对象可以属于超过一个类型. 类型从不显式地被 Common Lisp 表示为对象. 相反, 它们是通过使用类型指定符(type specifier)间接引用的, 类型指定符是表示类型的对象.

新的类型可以使用 deftype, defstruct, defclass, 和 define-condition 来定义.

函数 typep 作为一个集合从属关系测试, 被用于确定一个给定的对象(object) 是否为给定的类型(type). 函数 subtypep 作为一个子集测试, 被用于确定一个给定的类型是否为另一个给定类型的子类型(subtype). 函数 type-of 返回一个给定对象所属的具体类型, 即便这个对象也属于一个或多个其他类型. (比如, 每一个对象都是类型 t, 但是 type-of 总是返回一个比 t 更具体的类型的类型指定符.)

对象, 而不是变量, 拥有类型. 通常, 任何变量可以有任何对象作为它的值. 可以声明一个变量只能获取通过显式类型声明指定的类型的值. 类型被安排在一个有方向的非循环图中, 除了存在相等的情况.

可以通过 declare, proclaim, declaim, 或 the 来对类型做出声明. 关于声明的更多信息, 见章节 3.3 (Declarations).

对象系统的基本对象是类(class). 一个类确定其他称之为它的实例的对象集的结构和行为. 每一个对象都是一个类的直接实例. 一个对象的类确定可以在这个对象上进行的操作的集合. 关于更多信息, 见章节 4.3 (Classes).

可以写出具有特定于函数参数对象的类的行为的函数. 关于更多信息, 见章节 7.6 (Generic Functions and Methods).

一个对象的类的类称为它的元类(metaclass). 关于元类的更多信息, 见章节 7.4 (Meta-Objects). 

## 4.2 <span id="Types">类型</span>

> * 4.2.1 [数据类型定义](#DataTypeDefinition)
> * 4.2.2 [类型关系](#TypeRelationships)
> * 4.2.3 [类型指定符](#TypeSpecifiers)

### 4.2.1 <span id="DataTypeDefinition">数据类型定义</span>

类型使用的信息所在章节在  Figure 4-1 中指定. Figure 4-7 列出了和对象系统相关的类. Figure 9-1 列出了已定义的状况类型.

章节                                        | 数据类型                          
-------------------------------------------|------------------------              
Section 4.3 (Classes)                      | Object System types                
Section 7.5 (Slots)                        | Object System types                
Section 7 (Objects)                        | Object System types                
Section 7.6 (Generic Functions and Methods)| Object System types                
Section 9.1 (Condition System Concepts)    | Condition System types             
Section 4 (Types and Classes)              | Miscellaneous types                
Section 2 (Syntax)                         | All types---read and print syntax  
Section 22.1 (The Lisp Printer)            | All types---print syntax           
Section 3.2 (Compilation)                  | All types---compilation issues     

Figure 4-1. 数据类型信息的 Cross-References 

### 4.2.2 <span id="TypeRelationships">类型关系</span>

* 类型 cons, symbol, array, number, character, hash-table, function, readtable, package, pathname, stream, random-state, condition, restart, 还有任何由 defstruct, define-condition, 或 defclass 创建的单个类型是互斥的, 除了在 defclass 或者  define-condition 或者 destruct 的 :include 选项指定超类来明确建立类型关系的以外.

* 任何由 defstruct 创建的两个类型是互斥的, 除非由于 defstruct 的 :include 选项, 一个是另一个的超类.

* 任何两个由 defclass 或 define-condition 创建的有区别的类是互斥的, 除非它们有一个共同的子类或者一个类是另一个的超类.

* 可以扩展一个实现来添加指定类型之间的其他子类型关系, 只要它们不违反这里指定的类型关系和类互斥性的需求. 一个实现可能为任何指定的类型定义额外的子类或超类, 只要不违反互斥性的要求并且每一个额外的类是类型 t 的子类也是类型 nil 的超类.

    任凭具体实现自行处理, 无论是 standard-object 还是 structure-object, 都可能出现在一个系统类的优先级列表中, 而该系统类既没有指定 standard-object, 也没有指定 structure-object. 如果确实如此, 它必须在类 t 之前并且后面跟着所有标准化的类.<!-- TODO class precedence list ??--> 

### 4.2.3 <span id="TypeSpecifiers">类型指定符</span>

类型指定符(Type specifier)可以是符号, 类或列表. Figure 4-2 列出了标准原子类型指定符的列表, 并且 Figure 4-3 列出了标准化复合类型指定符的名字. 关于语法信息, 见对应类型指定符的字典条目. 可以通过 defclass, define-condition, defstruct, 或 deftype 来定义新的类型指定符.

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

如果一个类型指定符是一个列表, 这个列表的 car 部分是一个符号, 并且这个列表的剩余部分辅助类型信息. 这样一个类型指定符称为复合类型指定符(compound type specifier). 除明确声明外, 辅助项可以是未指明的. 这个未指明的辅助项写作 * 来表示. 比如, 为了完全地指定一个向量序列(vector), 元素的类型和这个序列的长度都必须被指定.

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

假设这两种类型指定符是相同的除了第一个有一个 * 而第二个有一个更明确的指定. 那么第二个表示的是第一个表示的子类型.

如果一个列表的末尾有一个或超过一个未指定项, 这些项可以被丢弃. 如果丢弃所有出现的 * 导致一个单元素的列表, 那么圆括号也可以被丢弃 (这个列表可以被它的 car 的符号所替代). 比如, (vector double-float *) 可以被简写为 (vector double-float), 并且 (vector * *) 可以被简写为 (vector) 然后是 vector.

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

Figure 4-3. 标准化的复合类型指定符的名字

下面这段展示了可以被用于复合类型指定符名字但是不能被用于原子类型指定符名字的已定义名字.

    and     mod  satisfies  
    eql     not  values     
    member  or              

Figure 4-4. 标准化的仅限复合类型指定符的名字

新的类型指定符可以以两种方式存在.

* 通过使用 defstruct 不带 :type 指定来定义一个结构或者使用 defclass 来定义一个类或者define-condition 自动导致结构或类的名字成为一个新的类型指定符的符号.
* 可以使用 deftype 来定义派生类型指定符, 它表示的像其他类型指定符的缩写一样.

一个类对象可以被用于一个类型指定符. 当使用这种方式的时候, 它表示这个类的所有成员的集合.

下面这段展示了一些类型和声明相关的定义的名字.

    coerce            defstruct  subtypep  
    declaim           deftype    the       
    declare           ftype      type      
    defclass          locally    type-of   
    define-condition  proclaim   typep     

Figure 4-5. 类型和声明相关的定义的名字.

下面这段展示了所有定义的类型指定符的名字, 不管是原子类型指定符或者复合类型指定符; 这个列表是 Figure 4-2 和 Figure 4-3 列表的结合.

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

Figure 4-6. 标准化类型指定符的名字

## 4.3 <span id="Classes">类</span>

尽管对象系统足够通用来描述所有的标准化类 (包括, 例如, number, hash-table, 和 symbol), 下面这段包含了与理解对象系统相关的类的列表.

    built-in-class    method-combination         standard-object   
    class             standard-class             structure-class   
    generic-function  standard-generic-function  structure-object  
    method            standard-method                              

Figure 4-7. 对象系统类

> * 4.3.1 [类的介绍](#IntroductionToClasses)
> * 4.3.2 [定义类](#DefiningClasses)
> * 4.3.3 [创建类的实例](#CreatingInstancesClasses)
> * 4.3.4 [继承](#Inheritance)
> * 4.3.5 [确定类的优先级列表](#DeterminingClassPrecedenceList)
> * 4.3.6 [重定义类](#RedefiningClasses)
> * 4.3.7 [整合类和类型](#IntegratingTypesClasses)

### 4.3.1 <span id="IntroductionToClasses">类的介绍</span>

一个类(class) 是一个对象, 它确定其他称之为实例的对象集合的结构和行为.

一个类可以从其他类中继承结构和行为. 一个类出于继承其他类的目的而在定义中引用其他类就称这个类是其他类的子类. 为了继承而指定的类称为继承类的超类.

一个类可以有一个名字(name). 函数 class-name 接受一个类对象并且返回它的名字. 一个匿名类的名字是 nil. 一个符号可以命名一个类. 函数 find-class 接受一个符号并且返回这个符号命名的类. 如果名字是一个符号并且如果这个类的名字命名这个类那么这个类有一个 proper 名字. 这就是说, 如果 S= (class-name C) 并且 C= (find-class S) 那么一个类 C 有一个 proper 名字. 注意, (find-class S1) = (find-class S2) 并且 S1/=S2 是可能的. 如果 C= (find-class S), 我们就说 C 是名为 S 的类. <!-- TODO proper ？？ -->

如果一个类 C2 在它的定义中明确指定 C1 作为超类那么类 C1 就是类 C2 的一个直接超类. 在这个情况下 C2 是 C1 的一个直接子类. 如果 1 <= i < n 并且存在一系列的类 C2,...,Cn-1 而 Ci+1 是 Ci 的直接超类那么类 Cn 是类 C1 的一个超类. 一个类不能被当作是它自身的超类或子类. 这也就是说, 如果 C1 是 C2 的一个超类, 那么 C1 /=C2. 由某个给定的类 C 及其所有超类组成的类集合被称为"C及其超类".

每一个类由一个类优先级列表(class precedence list), 它是给定类及其超类的集合的总排序. 这个总排序被表示为一个从最具体到最不具体的列表. 这个类优先级列表被用于多种用途. 一般来说, 更具体的类可以遮蔽从不具体的类中继承而来的特性. 方法(method)选择和组合过程使用类优先列表来从最具体到最不具体的顺序排列方法.

当一个类被定义时, 定义表达式形式中提及到的直接超类的顺序是很重要的. 每一个类有一个局部优先级顺序, 这是一个由该类和跟在后面的直接超类组成的列表, 按照定义表达式形式中所提到的顺序.

类优先级列表总是与列表中的每个类的局部优先级顺序一致. 每个局部优先级顺序中的类都以相同的顺序出现在类优先级列表中. 如果局部优先级顺序和其他每一个是不一致的, 就不能构建类优先级列表, 并且发出一个错误. 类优先级列表和它的运算在章节 4.3.5 (Determining the Class Precedence List).

类被组织成一个有向的无环图. 这里有两个显著的类, 名字为 t 和 standard-object. 类 t 没有超类. 它是除了它自身以外所有类的超类. 类 standard-object 是类 standard-class 的一个实例并且是每一个除了它自身以外类 standard-class 实例的超类.

这里由一个从对象系统类空间到类型空间的映射. 很多在这个文档中指定的标准类型都有一个对应的有这类型名字相同的类. 一些类型没有对应的类. 类型和类系统的集成在章节 4.3.7 (Integrating Types and Classes) 中讨论.

类由自身也是类的实例的对象来表示. 对象类的类被称为该对象的元类(metaclass). 当不可能出现错误解释时, 元类这个术语被用来引用一个类, 该类具有自身类的实例. 元类确定作为它实例的类的继承形式, 和那些类实例的表示形式. 这个对象系统提供一个默认的元类, standard-class, 适用于大部分程序.

除另有指定外, 在这个标准中提及的所有类都是类 standard-class 的实例, 所有广义函数都是类 standard-generic-function 的实例, 并且所有方法都是类 standard-method 的实例. 

#### 4.3.1.1 标准的元类

对象系统提供了许多预定义的元类. 这些包括类 standard-class, built-in-class, 还有 structure-class:

* 类 standard-class 是 defclass 定义的类的默认类.

* 类 built-in-class 是实例具有限制能力的特殊实现的类的类. 任何相当于标准类型的类可能是 built-in-class 的一个实例. 预定义的类型指定符需要有对应的类在 Figure 4-8 中被列出. 这些类中的每一个是否被实现为一个内置的类是依赖于具体实现的.

* 所有通过 defstruct 定义的类都是类 structure-class 的实例. 

### 4.3.2 <span id="DefiningClasses">定义类</span>

宏 defclass 被用于定义一个新命名的类is used to define a new named class.

一个类的定义包括:

* 这个新类的名字. 对于新定义的类这个名字是一个 proper 名字.

* 这个新定义的类的直接超类.

* 一个槽指定符的集合. 每一个槽指定符包括槽的名字和 0 或更多的槽选项. 一个槽选项只适用于单个槽. 如果一个类定义包含两个相同名字的槽指定符, 会发出一个错误.

* 一个类选项的集合. 每个类选项都属于整个类.

defclass 表达式形式的槽选项和类选项机制被用于:

* 为给定的槽提供默认的初始值表达式形式.

* 请求自动生成广义函数的方法，用于读取或写入槽.

* 控制一个给定的槽是否共享于类的所有实例或者这个类的每个实例是否有它自己的槽.

* 提供初始化参数和初始化参数默认值, 用于实例的创建.

* 指定元类而不是默认的. 这个 :metaclass 选项保留给未来使用; 一个实现可以扩展去使用 :metaclass 选项.

* 指定存储在槽中的期望的类型.

* 指定槽的文档字符串. 

### 4.3.3 <span id="CreatingInstancesClasses">创建类的实例</span>

广义函数 make-instance 创建并返回一个类的一个新的实例. 这个对象系统提供多种机制用于指明一个对象如何被初始化. 比如, 在新创建的对象中通过给 make-instance 提供参数或提供默认的初始化值来指定槽的初始化值是可能的. 进一步的初始化活动可以通过为广义函数编写的方法执行, 这些函数是初始化协议的一部分. 完全的初始化协议在章节 7.1 (Object Creation and Initialization) 中描述. 

### 4.3.4 <span id="Inheritance">继承</span>

一个类可以从它的超类中继承方法, 槽, 还有一些 defclass 选项. 其他部分描述了方法的继承, 槽和槽选项的继承以及类选项的继承.

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

类 C1 的实例有一个局部槽命名为 S1, 它的默认初始值是 5.4 并且它的值应该始终为一个数字. 类 C1 也有一个共享的槽名为 S2.

在 S2 的实例中有一个局部槽名为 S1. S1 的默认初始值为 5. S1 的值应该总是为类型 (and integer number). 在 S2 的实例中也有名为 S2 和 S3 的局部槽. 类 C2 有一个 C2-S3 方法来读取槽 S3 的值; 也有一个 (setf C2-S3) 方法来写入 S3 的值. 

#### 4.3.4.2 类选项的继承

这个 :default-initargs 类选项被继承. 一个类的默认初始化参数集合是这个类和它的超类的 :default-initargs 类选项中提供的初始化参数的并集. 当给一个给定的初始化参数提供了不止一个默认初始化值表达式时, 使用的默认初始值表达式形式是由类优先级列表中最具体的类提供的.

如果一个给定的 :default-initargs 类选项不止一次指定相同名字的初始化参数, 会发出一个类型 program-error 的错误. 

### 4.3.5 <span id="DeterminingClassPrecedenceList">确定类的优先级列表</span>
<!--TODO 需要重新校对 predecessor ordering ？？-->
一个类的 defclass 表达式形式提供这个类和它的直接超类的完整的列表. 这个列表称之为局部优先级列表(local precedence order). 它是一个这个类和它的直接超类的有序列表. 这个类 C 的类优先级列表是一个由每一个 C 和它的超类的局部优先级列表构成的完整的列表.

一个类在它的直接超类前面, 并且一个直接超类先于那些在 defclass 表达式中的超类列表中在它右边的其他直接超类. 对于每一个类 C, 定义

RC={(C,C1),(C1,C2),...,(Cn-1,Cn)}

其中 C1,...,Cn 是 C 的直接超类,在 defclass 表达式形式提及的列表中. 这些有序对生成类 C 和它的直接超类的总列表.

让 SC 是 C 和它的超类的集合. 让 R 为

R=Uc<ELEMENT-OF>SCRc

这个集合 R 可能或可能不会生成一个部分列表, 取决于 Rc, c<ELEMENT-OF>SC, 是否是一致的; 假定它们是一致的, R 产生一个部分列表. 当这个 Rc 不是一致的时, 就说 R 是非一致的.

为了计算 C 的优先级列表, 从拓扑上对 SC 的元素排序与 R 产生的部分列表有关. 当这个拓扑排序必须从两个或更多类的集合中选择一个类时, 其中没有一个类的前面有关于R的其他类, 选择的类是确定的, 如下面所述.

如果 R 不是一致的, 会发出一个错误.

#### 4.3.5.1 拓扑排序
<!--TODO 有问题-->
拓扑排序是根据 R 中的元素通过在 SC 中找到一个 C 类来进行的, 这样就不会有其他元素先于这个元素. 这个类 C 被放在结果的最前面. 从 SC 中移除 C, 并且从 R 中移除所有表达式 (C,D) 对, D<ELEMENT-OF>SC. 重复这个过程, 在结果的末尾添加没有任何 predecessors 的类. 当找不到没有 predecessor 的元素时停止.

如果 SC 不是空并且这个过程已经停止, 那么集合 R 是不一致的. 如果在有限集合中的每个类都前置另一个类，那么 R 就包含一个循环. 这就是说, 这里有一个 Ci 先于 Ci+1 的链 C1,...,Cn , 1 <= i < n, 并且 Cn 先于 C1.

有时候这里有多个来自 SC 没有 predecessors 的类. 在这个情况下选择迄今为止在类优先级列表中最右边的那个直接子类. (如果这里没有这样一个候选的类, R 不产生一个部分列表---这个 Rc, c<ELEMENT-OF>SC, 是不一致的.)

用更精确的术语, 让 {N1,...,Nm}, m>=2, 成为来自于 SC 的没有 predecessors 的类. 让 (C1...Cn), n>=1, 成为目前为止已构建的类优先级列表. C1 是最具体的类, 并且 Cn 最不具体的. 让 1 <= j <= n 成为最大一个数这样这里存在 i 其中 1 <= i <= m 并且 Ni 是 Cj 的一个直接子类; Ni 被放到下一个.

这条规则的作用是, 在没有 predecessor 的一组类中选择一个简单的超类链中的类在类优先级列表中是相邻的, 并且每个相对独立的子图中的类在类优先列表中都是相邻的 The effect of this rule for selecting from a set of classes with no predecessors is that the classes in a simple superclass chain are adjacent in the class precedence list and that classes in each relatively separated subgraph are adjacent in the class precedence list <!-- TODO 待校验 -->. 例如, 让 T1 和 T2 成为子图, 其唯一公共的元素是类 J. 假设没有 J 的超类出现在 T1 或 T2 中, 并且这个 J 存在于 T1 和 T2 的每个类的超类链中. 让 C1 是 T1 底部; 并且让 C2 是 T2 的底部. 假设这个顺序下 C 的直接超类是 C1 和 C2, 那么 C 的类优先级列表以 C 开始后面更这 T1 的所有类除了 J. 后面是 T2 的所有类. 类 J 和它的超类出现在最后. 

#### 4.3.5.2 确定类优先级列表的示例

这个实例确定一个类 pie 的类优先级列表. 定义下面这些类:

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

类 pie 的前面什么都没有, 所以它是第一个; 目前为止结果是 (pie). 从 S 中移除 pie 并且从 R 中移除提及到 pie 的对得到 S = {apple, cinnamon, fruit, spice, food, standard-object, t} 还有 R = {(apple, cinnamon), (apple, fruit), (cinnamon, spice),
(fruit, food), (spice, food), (food, standard-object), (standard-object, t)}.

类 apple 前面没有任何东西, 所以它是下一个; 结果是 (pie apple). 移除 apple 和相关的对得到 S = {cinnamon, fruit, spice, food, standard-object, t} 并且 R = {(cinnamon, spice), (fruit, food), (spice, food), (food, standard-object),
(standard-object, t)}.

类 cinnamon 和 fruit 前面什么都没有, 所以目前为止拥有在类优先级列表中最右边的直接子类的那一个是下一个. 类 apple 是 fruit 的直接子类, 而类 pie 是一个 cinnamon 的直接子类. 因为类优先级列表中 apple 出现 pie 的右边, fruit 是下一个, 目前的结果是 (pie apple fruit). S = {cinnamon, spice, food, standard-object, t}; R = {(cinnamon, spice), (spice, food),(food, standard-object), (standard-object, t)}.

类 cinnamon 是下一个, 目前结果为 (pie apple fruit cinnamon). 这时 S = {spice, food, standard-object, t}; R = {(spice, food), (food, standard-object), (standard-object, t)}.

类 spice, food, standard-object, 还有 t 按这个顺序添加, 然后类优先级列表为 (pie apple fruit cinnamon spice food standard-object t).

编写一组不能被排序的类定义是可能的. 比如:

```LISP
(defclass new-class (fruit apple) ())

(defclass apple (fruit) ())
```

类 fruit 必须先于 apple 因为必须保留超类的局部顺序. 类 apple 必须先于 fruit 因为一个类必须先于它的超类. 当这个情况发生时, 会发出一个错误, 当系统尝试去计算 new-class 的类优先级列表时会发生在这里.

以下可能是一组相互冲突的定义:

```LISP
(defclass pie (apple cinnamon) ())

(defclass pastry (cinnamon apple) ())

(defclass apple () ())

(defclass cinnamon () ())
```

这个 pie 的类优先级列表是 (pie apple cinnamon standard-object t).

这个 pastry 的类优先级列表是 (pastry cinnamon apple standard-object t).

在 pie 的超类的顺序中 apple 先于 cinnamon 不是问题, 但是在 pastry 中则有问题. 然而, 去构建一个同时有 pie 和 pastry 作为超类的新的类是不可能的. 

### 4.3.6 <span id="RedefiningClasses">重定义类</span>

作为一个 standard-class 的直接实例的类可以被重定义, 但是重定义的类也需要是 standard-class 类的实例. 重定义一个类会修改已存在的类对象来反映这个新的类的定义; 它不会为这个类创建一个新的类对象. 任何由旧的 defclass 表达式的 :reader, :writer, 或 :accessor 选项创建的方法对象会从对应广义函数中被移除. 新的 defclass 表达式指定的方法会被添加进去.

当这个类 C 被重定义了, 修改会传递到它的实例以及它的子类的实例. 更新这样一个实例发生的时间依赖于具体实现, 但是不会晚于下一次这个实例的槽被读取或写入. 更新一个实例不会改变函数 eq 定义的它的恒等条件. 更新的过程可能改变这个特别实例的槽, 但是不会创建一个新实例. 更新一个实例是否消耗存储是依赖于具体实现的.

注意, 重定义一个类可能倒是槽被添加或删除. 如果一个类被重新定义, 它改变了实例中可访问的局部槽的集合, 那么实例就会被更新. 如果一个类被重新定义, 它没有改变了实例中可访问的局部槽的集合, 实例是否被更新是依赖于具体实现的.

在旧的类和新的类中都指定为共享的槽的值会被保留. 如果这样一个共享槽在旧的类中没绑定, 它在新的类里也是没有绑定的. 在旧的类中是局部的而在新的类中是共享的槽会被初始化. 新添加的共享槽会被初始化.

每一个新添加的共享槽会被设置为新类的 defclass 表达式中指定的这个槽的初始化表达式的求值结果. 如果这里没有初始化表达式, 这个槽就是未绑定的.

如果一个类被重新定义, 其中类实例中可访问的本地槽的集合被改变, 那么更新这个类的实例的两步式步骤就会发生. 这个过程可能通过调用广义函数 make-instances-obsolete 来明确开始. 这个两步式的过程可以发生在一些实现的其他情况中. 例如, 在一些实现中如果槽在存储中的顺序被改变, 这个两步式过程也会被触发.

第一步通过添加新的局部槽和丢弃这个新的类中没定义的局部槽来修改这个实例的结构. 第二部来初始化新添加的槽并且执行任何其他的用户定义的动作. 这两个步骤在下面两个章节中会进一步说明.

> * 4.3.6.1 [修改实例的结构](#ModifyingStructureInstances)
> * 4.3.6.2 [初始化新添加的局部槽](#InitializingSlots)
> * 4.3.6.3 [定制化类重定义](#CustomizingClassRedefinition)

#### 4.3.6.1 <span id="ModifyingStructureInstances">修改实例的结构</span>

第一步修改重定义类的实例的结构来使之符合新的类定义. 在新的类定义中增加的局部槽而在旧的类定义中既没有指定为局部的也不是共享的槽会被添加, 并且在新的类定义中既不是局部也不是共享的而在旧的类定义中指定为局部的槽会被丢弃. 这些新添加和丢弃的槽的名字作为参数传递给下一章节所描述的 update-instance-for-redefined-class.

旧的和新的类中都指定的局部槽的值会被保留. 如果这样一个局部槽是未绑定的, 它就保留为未绑定的.

在旧的类中是共享的而在新的类中指定为局部的槽的值会保留. 如果这样一个共享槽是为绑定的, 这个后来的局部槽也是为绑定的. 

#### 4.3.6.2 <span id="InitializingSlots">初始化新添加的局部槽</span>

第二步初始化新添加的局部槽并且执行任何其他用户定义的动作. 这个步骤被广义函数 update-instance-for-redefined-class 实现, 这个函数在第一个修改实例结构的步骤完成后被调用.

广义函数 update-instance-for-redefined-class 需要 4 个必要参数: 在经历过第一个步骤之后要被更新的实例, 添加的局部槽的名称列表, 丢弃的局部槽的名称列表, 还有一个包含丢弃的槽的名字和槽的值的属性列表. 被丢弃的槽中包括旧类中是局部的而新类中是共享的槽.

广义函数 update-instance-for-redefined-class 也接受任意数量的初始化参数. 当它被系统调用来更新类被重定义的实例时, 不会提供初始化参数.

这里有一个系统提供的关于 update-instance-for-redefined-class 主方法, 它的实例参数的指定符是一个  standard-object 类. 首先这个方法检测初始化参数的正确性, 如果一个提供的参数没有被合法声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (Declaring the Validity of Initialization Arguments).) 然后它调用广义函数 shared-initialize 并传入以下参数: 这个实例, 新添加槽的名称列表, 还有它收到的初始化参数. 

#### 4.3.6.3 <span id="CustomizingClassRedefinition">定制化类重定义</span>

关于 update-instance-for-redefined-class 的方法可能被定义用来指定当一个实例被更新时采取的动作. 如果定义了 update-instance-for-redefined-class 的方法, 那么它们将在 system-supplied 的初始化主方法之后运行, 因此不会影响 update-instance-for-redefined-class 的默认行为. 被系统调用时由于没有传递初始化参数给 update-instance-for-redefined-class, 在 update-instance-for-redefined-class 方法之前添加的槽的初始化表达式形式不会被 shared-initialize 求值.

关于 shared-initialize 的方法可能被定义用来定制类的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 

### 4.3.7 <span id="IntegratingTypesClasses">整合类和类型</span>

对象系统映射类的空间到类型的空间. 每个有着 proper 的名字的类都有一个对应相同名字的类型.

每个类的 proper 名字是一个合法的类型指定符. 另外, 每个类对象是一个合法的类型指定符. 所以表达式 (typep object class) 在 object 的类是 class 本身或者 class 的子类情况下返回 true. 如果 class1 是 class2 的一个子类或者它们是相同的类, 表达式 (subtypep class1 class2) 的求值返回多值 true 和 true; 否则它返回多值 false 和 true. 如果 I 是某个名为 S 的类 C 的实例并且 C 是 standard-class 的实例, 那么如果 S 是 C 的 proper 的名字表达式 (type-of I) 的求值返回 S; 否则, 它返回 C.

由于类的名字和类对象是类型指定符, 它们可能被用于特殊表达式形式 the 还有类型声明.

很多但不是全部预定义的类型指定符都有和类型有着相同 proper 名字的类. 这些类型指定符列在 Figure 4-8. 比如, 类型 array 有一个对应的类名为 array. 没有类型指定符是一个列表, 比如 (vector double-float 100), 有着一个对应的类. 操作符 deftype 不会创建任何类.

对应于预定义类型说明符的每个类可以通过以下三种方式实现, 由每个具体实现决定. 它可以是一个 standard class, 一个 structure class, 或者一个 system class.

一个内置的类是一个泛化实例具有限制功能和特殊表示的类. 尝试使用 defclass 去定义 built-in-class 的子类会发出一个错误. 对一个内置类的泛化实例调用 slot-value 会发出一个错误. 重定义一个内置的类或使用 change-class 去改变一个内置类对象的类或把一个对象的类改为内置类都会发出一个错误. 然而, 内置的类可以被用作方法的参数指定符.

可以通过检测元类来确定一个类是否为内置类. 一个标准类是类 standard-class 的实例, 一个内置类是 built-in-class 的实例, 并且一个结构类是 structure-class 的实例.

每一个用 defstruct 创建的没有使用 :type 选项的结构类型都由一个对应的类. 这个类是一个类 structure-class 的泛化实例. 这个 defstruct 的 :include 选项会创建一个对应被包含的结构类型的类的直接子类.

槽是否被牵涉到在这个规范定义的类的实例上的这个规范定义的函数操作中是依赖于具体实现的, 除非这个槽被这个规范明确定义.

如果在一个特定的具体实现中的这个规范定义的类拥有的槽没有在这个规范中定义, 这些槽的名称不能是该规范中定义的包的外部符号, 也不能在 CL-USER 包中访问.

指定许多标准类型指定符有相应的类, 目的是使用户能够编写对这些类型进行区别对待的方法. 方法选择要求为每个类确定一个类优先级列表.

类型指定符之间的层次关系通过与这些类型对应的类之间的关系来反映.

Figure 4-8 列出了预定义类型指定符对应的类的集合.

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

在这些类的条目中指定的类优先级列表信息是对象系统所需要的.

可以扩展单独的具体实现来定义其他类型指定符来拥有相应的类. 单个具体实现可以扩展去添加其他子类关系, 并将其他元素添加到类优先级列表中, 只要它们不违反该标准所指定的类型关系和互斥性需求. 一个已定义的没有指定直接超类的标准类保证和这个表中的所有类都是互斥的, 除了名为 t 的类. 

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

        所有类型

* 描述(Description):

        类型 nil 不含任何对象所以也被称为空类型. 类型 nil 是所有类型的子类. 没有 nil 类型的对象.

* 注意(Notes):

        包含对象 nil 的类型是 null, 不是类型 nil. 

### <span id="TypeBOOLEAN">类型 BOOLEAN</span>

* 超类型(Supertypes):

        boolean, symbol, t

* 描述(Description):

        类型 boolean 包含符号 t 和 nil, 它们分别表示 true 和 false.

* 也见(See Also):

        t (constant variable), nil (constant variable), if, not, complement

* 注意(Notes):

        条件操作, 比如 if, 允许使用广义的 boolean, 不只是 boolean; 任何非 nil 的值, 不只是 t, 对于广义的 boolean 则视作 true. 然而, 作为惯例, 即便对于广义的 boolean 当没有更好的选择来表示它自身时符号 t 被当作正规的值来使用. 

### <span id="SystemClassFUNCTION">系统类 FUNCTION</span>

* 类优先级列表(Class Precedence List):

        function, t

* 描述(Description):

        当适当数量的参数被提供时一个 function 是一个表示要被执行的代码的对象. 一个 function 由 function 特殊表达式, 函数 coerce, 或函数 compile 产生. 一个 function 可以通过把它作为第一个参数给 funcall, apply, 或 multiple-value-call 来直接调用 by using it as the first argument to funcall, apply, or multiple-value-call.

* 复合类型指定符种类(Compound Type Specifier Kind):

        Specializing.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        function [arg-typespec [value-typespec]]

        arg-typespec::= (typespec*  
                        [&optional typespec*]  
                        [&rest typespec]  
                        [&key (keyword typespec)*]) 

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        typespec---一个类型指定符.
        value-typespec---一个类型指定符.

* 复合类型指定符的描述(Compound Type Specifier Description):

    * 这个 function 类型指定符的列表形式只能被用于声明不能用于辨别(discrimination). 这个类型的每一个元素都是一个函数, 它接受 argj-types 指定类型的参数并返回 value-type 指定类型的返回值. 这里的 &optional, &rest, &key, 和 &allow-other-keys 标记可以出现在参数类型的列表中. 这里 &rest 提供的类型指定符是每一个实际参数的类型, 不是对应变量的类型.

    * 这个 &key 参数应该像 (keyword type) 这样的表达式的列表来提供. 这个 keyword 必须是一个合法的关键字名字符号, 必须在调用的实际参数中提供. 这个通常是 KEYWORD 包中的符号但是可以是任何符号. 当 &key 在一个 function 类型指定符的 lambda 列表中被提供, 这个给定的关键字参数必须是详尽的除非 &allow-other-keys 也被提供. &allow-other-keys 是一个指示符表示其他的关键字参数实际中可以被提供, 如果提供了, 就可以被使用. 比如, 函数 make-list 类型可以按以下的方式来声明:

        ```LISP
        (function ((integer 0) &key (:initial-element t)) list)
        ```

    * 这个 value-type 可以是一个 values 类型指定符用来表示多值的类型.

    * 细想以下表达式的一个声明:

        ```LISP
        (ftype (function (arg0-type arg1-type ...) val-type) f))
        ```

    * 任何在这个声明的作用域中的 (f arg0 arg1 ...) 表达式等价于以下:

        ```LISP
        (the val-type (f (the arg0-type arg0) (the arg1-type arg1) ...))
        ```

    * 这也就是说, 如果任意参数不是指定的类型或者结果不是指定的类型, 那么结果是不可预料的. 具体来说, 如果任意参数不是正确的类型, 结果就不保证是指定的类型.

    * 因此, 一个函数的 ftype 声明描述了这个函数的调用, 不是这个函数实际的定义.

    * 细想一个以下表达式的声明:

        ```LISP
        (type (function (arg0-type arg1-type ...) val-type) fn-valued-variable)
        ```

    * 这个声明有着这样的解释, 在这个声明的作用域里, 如果 fn-valued-variable 的值被调用, 参数不是指定的类型, 那么结果是不可预料的; 一个合法调用的结果值会是 val-type 类型.

    * 与变量类型声明一样, 嵌套声明意味着类型的交集, 如下:

        * 细想以下两种 ftype 的声明:

            ```LISP
            (ftype (function (arg0-type1 arg1-type1 ...) val-type1) f))
            ```

            还有

            ```LISP
            (ftype (function (arg0-type2 arg1-type2 ...) val-type2) f))
            ```

            如果这些声明都生效, 那么在这些声明的共享作用域内, 对 f 的调用可以被认为好像 f 是按下面这种方式声明的:

            ```LISP
            (ftype (function ((and arg0-type1 arg0-type2) (and arg1-type1 arg1-type2 ...) ...)
                            (and val-type1 val-type2)) 
                f))
            ```

            它被允许忽略一个或全部的 ftype 声明.

        * 如果一个变量的两个 (or more) 类型声明生效, 并且它们都是函数声明, 这些声明也会类似地组合. 

### <span id="TypeCOMPILEDFUNCTION">类型 COMPILED-FUNCTION</span>

* 超类型(Supertypes):

        compiled-function, function, t

* 描述(Description):

        如果一个函数不包含在运行时必须展开的宏的引用, 并且它也不包含加载时值的未解析的引用, 那么任何函数都可以被认为是一个编译后的函数(compiled function). 见章节 3.2.2 (Compilation Semantics).

        一个函数的定义词法上出现在一个文件中, 这个文件已经被 compile-file 编译并且被 load 加载后这个函数的类型就是 compiled-function. compile 函数产生的函数也是 compiled-function 类型的. 其他函数也可能是 compiled-function 类型. 

### <span id="SystemClassGENERICFUNCTION">系统类 GENERIC-FUNCTION</span>

* 类优先级列表(Class Precedence List):

        generic-function, function, t

* 描述(Description):

        一个广义函数是一个行为取决于提供给它的参数的标识或类. 一个广义函数对象包含一个方法的集合, 一个 lambda 列表, 一个方法组合类型, 还有其他信息. 方法定义了广义函数的类特定的行为和操作; 一个方法也被称为特化一个广义函数. 当被调用时, 一个广义函数基于它的参数的类和标识去执行它的方法的一个子集.

        一个广义函数可以和普通函数相同的方式被使用; specifically, 一个广义函数可以被用作 funcall 和 apply 的参数, 并且可以被赋予一个全局或局部的名字. 

### <span id="SystemClassSTANDARDGENERICFUNCTION">系统类 STANDARD-GENERIC-FUNCTION</span>

* 类优先级列表(Class Precedence List):

        standard-generic-function, generic-function, function, t

* 描述(Description):

        类 standard-generic-function 是 defmethod, ensure-generic-function, defgeneric, 和 defclass 表达式建立的默认广义函数的类. 

### <span id="SystemClassCLASS">系统类 CLASS</span>

* 类优先级列表(Class Precedence List):

        class, standard-object, t

* 描述(Description):

        类型 class 表示确定它们实例的结构的行为的对象. 与 class 类型的对象相关联的信息是描述其在类的非循环图形中的位置、它的槽还有它的选项的信息. 

### <span id="SystemClassBUILTINCLASS">系统类 BUILT-IN-CLASS</span>

* 类优先级列表(Class Precedence List):

        built-in-class, class, standard-object, t

* 描述(Description):

        一个内置类是一个其实例具有限制功能和特殊表示的类. 尝试用 defclass 去定义一个内置类的子类会发出一个 error 类型的错误. 调用 make-instance 去创建一个内置类的实例会发出一个 error 类型的错误. 在一个内置类的实例上调用会发出一个 error 类型的错误. 重定义一个内置类或使用 change-class 去改变一个实例的类为内置类或改变一个内置类为其他类会发出一个 error 的错误. 然而, 内置类可以被用作方法的参数指定符. 

### <span id="SystemClassSTRUCTURECLASS">系统类 STRUCTURE-CLASS</span>

* 类优先级列表(Class Precedence List):

        structure-class, class, standard-object, t

* 描述(Description):

        所有通过 defstruct 定义的类的都是类 structure-class 的实例. 

### <span id="SystemClassSTANDARDCLASS">系统类 STANDARD-CLASS</span>

* 类优先级列表(Class Precedence List):

    standard-class, class, standard-object, t

* 描述(Description):

    类 standard-class 是 defclass 定义出来的默认类. 

### <span id="SystemClassMETHOD">系统类 METHOD</span>

* 类优先级列表(Class Precedence List):

        method, t

* 描述(Description):

        一个方法是表示一个广义函数的行为的模组化部分的对象.

        一个方法包含了实现这个方法行为的代码, 一个指定什么时候这个给定方法可以被应用的参数指定符序列, 还有一个被用于方法组合机制来辨别方法的限定符序列. 每一个方法的每一个必要参数都有一个关联的参数指定符, 并且只有在参数满足方法的参数指定符时, 该方法才会被调用.

        方法组合机制控制方法的选择, 它们执行的顺序, 还有广义函数返回的值. 对象系统提供一个默认方法组合类型并且提供一个机制来声明新的方法组合类型.

* 也见(See Also):

        章节 7.6 (Generic Functions and Methods) 

### <span id="SystemClassSTANDARDMETHOD">系统类 STANDARD-METHOD</span>

* 类优先级列表(Class Precedence List):

        standard-method, method, standard-object, t

* 描述(Description):

        这个类 standard-method 是 defmethod 和 defgeneric 表达式定义的方法的默认类. 

### <span id="ClassSTRUCTUREOBJECT">类 STRUCTURE-OBJECT</span>

* 类优先级列表(Class Precedence List):

        structure-object, t

* 描述(Description):

        类 structure-object 是 structure-class 的一个实例并且是 structure-class 实例的每一个类的超类除了它自身, 并且是 defstruct 定义的每一个类的超类.

* 也见(See Also):

        defstruct, Section 2.4.8.13 (Sharpsign S), Section 22.1.3.12 (Printing Structures) 

### <span id="ClassSTANDARDOBJECT">类 STANDARD-OBJECT</span>

* 类优先级列表(Class Precedence List):

        standard-object, t

* 描述(Description):

        类 standard-object 是 standard-class 的一个实例并且是 standard-class 的实例的每个类的超类除了它自身. 

### <span id="SystemClassMETHODCOMBINATION">系统类 METHOD-COMBINATION</span>

* 类优先级列表(Class Precedence List):

        method-combination, t

* 描述(Description):

        每一个方法组合对象是类 method-combination 的间接实例. 一个方法组合对象表示这个方法组合被一个广义函数所使用的信息. 一个方法组合对象包含了方法组合的类型还有这个类型要使用的参数信息. 

### <span id="SystemClassT">系统类 T</span>

* 类优先级列表(Class Precedence List):

        t

* 描述(Description):

        所有对象的集合. 类型 t 每个类型的超类型, 包括它自身. 每个对象都是类型 t. 

### <span id="TypeSpecifierSATISFIES">类型指定符 SATISFIES</span>

* 复合类型指定符种类(Compound Type Specifier Kind):

        Predicating.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        satisfies predicate-name

* 复合类型指定符的参数(Compound Type Specifier Arguments):

        predicate-name---一个符号.

* 复合类型指定符的描述(Compound Type Specifier Description):

        这个表示满足断言 predicate-name 的所有对象的集合, 这个断言必须是单个参数断言的全局函数定义的符号. predicate-name 需要一个名字;不允许 lambda 表达式. 比如, 类型指定符 (and integer (satisfies evenp)) 表示偶数整型的集合. 表达式 (typep x '(satisfies p)) 等价于 (if (p x) t nil).

        这个参数是必要的. 符号 * 可以是参数, 但是它表示它自身 (the symbol *), 不表示一个未指定的类型.

        符号 satisfies 作为类型指定符是不合法的. 

### <span id="TypeSpecifierMEMBER">类型指定符 MEMBER</span>

复合类型指定符种类(Compound Type Specifier Kind):

Combining.

复合类型指定符语法(Compound Type Specifier Syntax):

member object*

复合类型指定符的参数(Compound Type Specifier Arguments):

object---an object.

复合类型指定符的描述(Compound Type Specifier Description):

This denotes the set containing the named objects. An object is of this type if and only if it is eql to one of the specified objects.

The type specifiers (member) and nil are equivalent. * can be among the objects, but if so it denotes itself (the symbol *) and does not represent an unspecified value. The symbol member is not valid as a type specifier; and, specifically, it is not an abbreviation for either (member) or (member *).

* 也见(See Also):

the type eql 

### <span id="TypeSpecifierNOT">类型指定符 NOT</span>

复合类型指定符种类(Compound Type Specifier Kind):

Combining.

复合类型指定符语法(Compound Type Specifier Syntax):

not typespec

复合类型指定符的参数(Compound Type Specifier Arguments):

typespec---a type specifier.

复合类型指定符的描述(Compound Type Specifier Description):

This denotes the set of all objects that are not of the type typespec.

The argument is required, and cannot be *.

The symbol not is not valid as a type specifier. 

### <span id="TypeSpecifierAND">类型指定符 AND</span>

复合类型指定符种类(Compound Type Specifier Kind):

Combining.

复合类型指定符语法(Compound Type Specifier Syntax):

and typespec*

复合类型指定符的参数(Compound Type Specifier Arguments):

typespec---a type specifier.

复合类型指定符的描述(Compound Type Specifier Description):

This denotes the set of all objects of the type determined by the intersection of the typespecs.

* is not permitted as an argument.

The type specifiers (and) and t are equivalent. The symbol and is not valid as a type specifier, and, specifically, it is not an abbreviation for (and). 

### <span id="TypeSpecifierOR">类型指定符 OR</span>

复合类型指定符种类(Compound Type Specifier Kind):

Combining.

复合类型指定符语法(Compound Type Specifier Syntax):

or typespec*

复合类型指定符的参数(Compound Type Specifier Arguments):

typespec---a type specifier.

复合类型指定符的描述(Compound Type Specifier Description):

This denotes the set of all objects of the type determined by the union of the typespecs. For example, the type list by definition is the same as (or null cons). Also, the value returned by position is an object of type (or null (integer 0 *)); i.e., either nil or a non-negative integer.

* is not permitted as an argument.

The type specifiers (or) and nil are equivalent. The symbol or is not valid as a type specifier; and, specifically, it is not an abbreviation for (or). 

### <span id="TypeSpecifierVALUES">类型指定符 VALUES</span>

复合类型指定符种类(Compound Type Specifier Kind):

Specializing.

复合类型指定符语法(Compound Type Specifier Syntax):

values value-typespec

value-typespec::= typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys] 

复合类型指定符的参数(Compound Type Specifier Arguments):

typespec---a type specifier.

复合类型指定符的描述(Compound Type Specifier Description):

This type specifier can be used only as the value-type in a function type specifier or a the special form. It is used to specify individual types when multiple values are involved. The &optional and &rest markers can appear in the value-type list; they indicate the parameter list of a function that, when given to multiple-value-call along with the values, would correctly receive those values.

The symbol * may not be among the value-types.

The symbol values is not valid as a type specifier; and, specifically, it is not an abbreviation for (values). 

### <span id="TypeSpecifierEQL">类型指定符 EQL</span>

复合类型指定符种类(Compound Type Specifier Kind):

Combining.

复合类型指定符语法(Compound Type Specifier Syntax):

eql object

复合类型指定符的参数(Compound Type Specifier Arguments):

object---an object.

复合类型指定符的描述(Compound Type Specifier Description):

Represents the type of all x for which (eql object x) is true.

The argument object is required. The object can be *, but if so it denotes itself (the symbol *) and does not represent an unspecified value. The symbol eql is not valid as an atomic type specifier. 

### <span id="FunctionCOERCE">Function COERCE</span>

Syntax:

coerce object result-type => result

Arguments and Values:

object---an object.

result-type---a type specifier.

result---an object, of type result-type except in situations described in Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).

描述(Description):

Coerces the object to type result-type.

If object is already of type result-type, the object itself is returned, regardless of whether it would have been possible in general to coerce an object of some other type to result-type.

Otherwise, the object is coerced to type result-type according to the following rules:

sequence

    If the result-type is a recognizable subtype of list, and the object is a sequence, then the result is a list that has the same elements as object.

    If the result-type is a recognizable subtype of vector, and the object is a sequence, then the result is a vector that has the same elements as object. If result-type is a specialized type, the result has an actual array element type that is the result of upgrading the element type part of that specialized type. If no element type is specified, the element type defaults to t. If the implementation cannot determine the element type, an error is signaled.

character

    If the result-type is character and the object is a character designator, the result is the character it denotes.

complex

    If the result-type is complex and the object is a real, then the result is obtained by constructing a complex whose real part is the object and whose imaginary part is the result of coercing an integer zero to the type of the object (using coerce). (If the real part is a rational, however, then the result must be represented as a rational rather than a complex; see Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals). So, for example, (coerce 3 'complex) is permissible, but will return 3, which is not a complex.)

float

    If the result-type is any of float, short-float, single-float, double-float, long-float, and the object is a real, then the result is a float of type result-type which is equal in sign and magnitude to the object to whatever degree of representational precision is permitted by that float representation. (If the result-type is float and object is not already a float, then the result is a single float.)

function

    If the result-type is function, and object is any function name that is fbound but that is globally defined neither as a macro name nor as a special operator, then the result is the functional value of object.

    If the result-type is function, and object is a lambda expression, then the result is a closure of object in the null lexical environment.

t

    Any object can be coerced to an object of type t. In this case, the object is simply returned.

Examples:

 (coerce '(a b c) 'vector) =>  #(A B C)
 (coerce 'a 'character) =>  #\A
 (coerce 4.56 'complex) =>  #C(4.56 0.0)
 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
 (coerce 7/2 'complex) =>  7/2
 (coerce 0 'short-float) =>  0.0s0
 (coerce 3.5L0 'float) =>  3.5L0
 (coerce 7/2 'float) =>  3.5
 (coerce (cons 1 2) t) =>  (1 . 2)

All the following forms should signal an error:

 (coerce '(a b c) '(vector * 4))
 (coerce #(a b c) '(vector * 4))
 (coerce '(a b c) '(vector * 2))
 (coerce #(a b c) '(vector * 2))
 (coerce "foo" '(string 2))
 (coerce #(#\a #\b #\c) '(string 2))
 (coerce '(0 1) '(simple-bit-vector 3))

Affected By: None.

Exceptional Situations:

If a coercion is not possible, an error of type type-error is signaled.

(coerce x 'nil) always signals an error of type type-error.

An error of type error is signaled if the result-type is function but object is a symbol that is not fbound or if the symbol names a macro or a special operator.

An error of type type-error should be signaled if result-type specifies the number of elements and object is of a different length.

* 也见(See Also):

rational, floor, char-code, char-int

Notes:

Coercions from floats to rationals and from ratios to integers are not provided because of rounding problems.

 (coerce x 't) ==  (identity x) ==  x

### <span id="MacroDEFTYPE">Macro DEFTYPE</span>

Syntax:

deftype name lambda-list [[declaration* | documentation]] form* => name

Arguments and Values:

name---a symbol.

lambda-list---a deftype lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

描述(Description):

deftype defines a derived type specifier named name.

The meaning of the new type specifier is given in terms of a function which expands the type specifier into another type specifier, which itself will be expanded if it contains references to another derived type specifier.

The newly defined type specifier may be referenced as a list of the form (name arg1 arg2 ...). The number of arguments must be appropriate to the lambda-list. If the new type specifier takes no arguments, or if all of its arguments are optional, the type specifier may be used as an atomic type specifier.

The argument expressions to the type specifier, arg1 ... argn, are not evaluated. Instead, these literal objects become the objects to which corresponding parameters become bound.

The body of the deftype form (but not the lambda-list) is implicitly enclosed in a block named name, and is evaluated as an implicit progn, returning a new type specifier.

The lexical environment of the body is the one which was current at the time the deftype form was evaluated, augmented by the variables in the lambda-list.

Recursive expansion of the type specifier returned as the expansion must terminate, including the expansion of type specifiers which are nested within the expansion.

The consequences are undefined if the result of fully expanding a type specifier contains any circular structure, except within the objects referred to by member and eql type specifiers.

Documentation is attached to name as a documentation string of kind type.

If a deftype form appears as a top level form, the compiler must ensure that the name is recognized in subsequent type declarations. The programmer must ensure that the body of a deftype form can be evaluated at compile time if the name is referenced in subsequent type declarations. If the expansion of a type specifier is not defined fully at compile time (perhaps because it expands into an unknown type specifier or a satisfies of a named function that isn't defined in the compile-time environment), an implementation may ignore any references to this type in declarations and/or signal a warning.

Examples:

 (defun equidimensional (a)
   (or (< (array-rank a) 2)
       (apply #'= (array-dimensions a)))) =>  EQUIDIMENSIONAL
 (deftype square-matrix (&optional type size)
   `(and (array ,type (,size ,size))
         (satisfies equidimensional))) =>  SQUARE-MATRIX

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

* 也见(See Also):

declare, defmacro, documentation, Section 4.2.3 (Type Specifiers), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

Notes: None. 

### <span id="FunctionSUBTYPEP">Function SUBTYPEP</span>

Syntax:

subtypep type-1 type-2 &optional environment => subtype-p, valid-p

Arguments and Values:

type-1---a type specifier.

type-2---a type specifier.

environment---an environment object. The default is nil, denoting the null lexical environment and the current global environment.

subtype-p---a generalized boolean.

valid-p---a generalized boolean.

描述(Description):

If type-1 is a recognizable subtype of type-2, the first value is true. Otherwise, the first value is false, indicating that either type-1 is not a subtype of type-2, or else type-1 is a subtype of type-2 but is not a recognizable subtype.

A second value is also returned indicating the `certainty' of the first value. If this value is true, then the first value is an accurate indication of the subtype relationship. (The second value is always true when the first value is true.)

The next figure summarizes the possible combinations of values that might result.

Value 1  Value 2  Meaning                                               
true     true     type-1 is definitely a subtype of type-2.             
false    true     type-1 is definitely not a subtype of type-2.         
false    false    subtypep could not determine the relationship,        
                  so type-1 might or might not be a subtype of type-2.  

Figure 4-9. Result possibilities for subtypep

subtypep is permitted to return the values false and false only when at least one argument involves one of these type specifiers: and, eql, the list form of function, member, not, or, satisfies, or values. (A type specifier `involves' such a symbol if, after being type expanded, it contains that symbol in a position that would call for its meaning as a type specifier to be used.) One consequence of this is that if neither type-1 nor type-2 involves any of these type specifiers, then subtypep is obliged to determine the relationship accurately. In particular, subtypep returns the values true and true if the arguments are equal and do not involve any of these type specifiers.

subtypep never returns a second value of nil when both type-1 and type-2 involve only the names in Figure 4-2, or names of types defined by defstruct, define-condition, or defclass, or derived types that expand into only those names. While type specifiers listed in Figure 4-2 and names of defclass and defstruct can in some cases be implemented as derived types, subtypep regards them as primitive.

The relationships between types reflected by subtypep are those specific to the particular implementation. For example, if an implementation supports only a single type of floating-point numbers, in that implementation (subtypep 'float 'long-float) returns the values true and true (since the two types are identical).

For all T1 and T2 other than *, (array T1) and (array T2) are two different type specifiers that always refer to the same sets of things if and only if they refer to arrays of exactly the same specialized representation, i.e., if (upgraded-array-element-type 'T1) and (upgraded-array-element-type 'T2) return two different type specifiers that always refer to the same sets of objects. This is another way of saying that `(array type-specifier) and `(array ,(upgraded-array-element-type 'type-specifier)) refer to the same set of specialized array representations. For all T1 and T2 other than *, the intersection of (array T1) and (array T2) is the empty set if and only if they refer to arrays of different, distinct specialized representations.

Therefore,

 (subtypep '(array T1) '(array T2)) =>  true

if and only if

 (upgraded-array-element-type 'T1)  and
 (upgraded-array-element-type 'T2)  

return two different type specifiers that always refer to the same sets of objects.

For all type-specifiers T1 and T2 other than *,

 (subtypep '(complex T1) '(complex T2)) =>  true, true

if:

1. T1 is a subtype of T2, or
2. (upgraded-complex-part-type 'T1) and (upgraded-complex-part-type 'T2) return two different type specifiers that always refer to the same sets of objects; in this case, (complex T1) and (complex T2) both refer to the same specialized representation.

The values are false and true otherwise.

The form

 (subtypep '(complex single-float) '(complex float))

must return true in all implementations, but

 (subtypep '(array single-float) '(array float))

returns true only in implementations that do not have a specialized array representation for single floats distinct from that for other floats.

Examples:

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

Let <aet-x> and <aet-y> be two distinct type specifiers that do not always refer to the same sets of objects in a given implementation, but for which make-array, will return an object of the same array type.

Thus, in each case,

  (subtypep (array-element-type (make-array 0 :element-type '<aet-x>))
            (array-element-type (make-array 0 :element-type '<aet-y>)))
=>  true, true
 
  (subtypep (array-element-type (make-array 0 :element-type '<aet-y>))
            (array-element-type (make-array 0 :element-type '<aet-x>)))
=>  true, true

If (array <aet-x>) and (array <aet-y>) are different names for exactly the same set of objects, these names should always refer to the same sets of objects. That implies that the following set of tests are also true:

 (subtypep '(array <aet-x>) '(array <aet-y>)) =>  true, true
 (subtypep '(array <aet-y>) '(array <aet-x>)) =>  true, true

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

* 也见(See Also):

Section 4.2 (Types)

Notes:

The small differences between the subtypep specification for the array and complex types are necessary because there is no creation function for complexes which allows the specification of the resultant part type independently of the actual types of the parts. Thus in the case of the type complex, the actual type of the parts is referred to, although a number can be a member of more than one type. For example, 17 is of type (mod 18) as well as type (mod 256) and type integer; and 2.3f5 is of type single-float as well as type float. 

### <span id="FunctionTYPEOF">Function TYPE-OF</span>

Syntax:

type-of object => typespec

Arguments and Values:

object---an object.

typespec---a type specifier.

描述(Description):

Returns a type specifier, typespec, for a type that has the object as an element. The typespec satisfies the following:

1. For any object that is an element of some built-in type:

    a. the type returned is a recognizable subtype of that built-in type.

    b. the type returned does not involve and, eql, member, not, or, satisfies, or values.

2. For all objects, (typep object (type-of object)) returns true. Implicit in this is that type specifiers which are not valid for use with typep, such as the list form of the function type specifier, are never returned by type-of.

3. The type returned by type-of is always a recognizable subtype of the class returned by class-of. That is,

     (subtypep (type-of object) (class-of object)) =>  true, true

4. For objects of metaclass structure-class or standard-class, and for conditions, type-of returns the proper name of the class returned by class-of if it has a proper name, and otherwise returns the class itself. In particular, for objects created by the constructor function of a structure defined with defstruct without a :type option, type-of returns the structure name; and for objects created by make-condition, the typespec is the name of the condition type.

5. For each of the types short-float, single-float, double-float, or long-float of which the object is an element, the typespec is a recognizable subtype of that type.

Examples:

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

Affected By: None.

Exceptional Situations: None.

* 也见(See Also):

array-element-type, class-of, defstruct, typecase, typep, Section 4.2 (Types)

Notes:

Implementors are encouraged to arrange for type-of to return a portable value. 

### <span id="FunctionTYPEP">Function TYPEP</span>

Syntax:

typep object type-specifier &optional environment => generalized-boolean

Arguments and Values:

object---an object.

type-specifier---any type specifier except values, or a type specifier list whose first element is either function or values.

environment---an environment object. The default is nil, denoting the null lexical environment and the and current global environment.

generalized-boolean---a generalized boolean.

描述(Description):

Returns true if object is of the type specified by type-specifier; otherwise, returns false.

A type-specifier of the form (satisfies fn) is handled by applying the function fn to object.

(typep object '(array type-specifier)), where type-specifier is not *, returns true if and only if object is an array that could be the result of supplying type-specifier as the :element-type argument to make-array. (array *) refers to all arrays regardless of element type, while (array type-specifier) refers only to those arrays that can result from giving type-specifier as the :element-type argument to make-array. A similar interpretation applies to (simple-array type-specifier) and (vector type-specifier). See Section 15.1.2.1 (Array Upgrading).

(typep object '(complex type-specifier)) returns true for all complex numbers that can result from giving numbers of type type-specifier to the function complex, plus all other complex numbers of the same specialized representation. Both the real and the imaginary parts of any such complex number must satisfy:

 (typep realpart 'type-specifier)
 (typep imagpart 'type-specifier)

See the function upgraded-complex-part-type.

Examples:

 (typep 12 'integer) =>  true
 (typep (1+ most-positive-fixnum) 'fixnum) =>  false
 (typep nil t) =>  true
 (typep nil nil) =>  false
 (typep 1 '(mod 2)) =>  true
 (typep #c(1 1) '(complex (eql 1))) =>  true
;; To understand this next example, you might need to refer to
;; Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).
 (typep #c(0 0) '(complex (eql 0))) =>  false

Let Ax and Ay be two type specifiers that denote different types, but for which

 (upgraded-array-element-type 'Ax)

and

 (upgraded-array-element-type 'Ay)

denote the same type. Notice that

 (typep (make-array 0 :element-type 'Ax) '(array Ax)) =>  true
 (typep (make-array 0 :element-type 'Ay) '(array Ay)) =>  true
 (typep (make-array 0 :element-type 'Ax) '(array Ay)) =>  true
 (typep (make-array 0 :element-type 'Ay) '(array Ax)) =>  true

Affected By: None.

Exceptional Situations:

An error of type error is signaled if type-specifier is values, or a type specifier list whose first element is either function or values.

The consequences are undefined if the type-specifier is not a type specifier.

* 也见(See Also):

type-of, upgraded-array-element-type, upgraded-complex-part-type, Section 4.2.3 (Type Specifiers)

Notes:

Implementations are encouraged to recognize and optimize the case of (typep x (the class y)), since it does not involve any need for expansion of deftype information at runtime. 

### <span id="ConditionTypeTYPEERROR">Condition Type TYPE-ERROR</span>

类优先级列表(Class Precedence List):

type-error, error, serious-condition, condition, t

描述(Description):

The type type-error represents a situation in which an object is not of the expected type. The ``offending datum'' and ``expected type'' are initialized by the initialization arguments named :datum and :expected-type to make-condition, and are accessed by the functions type-error-datum and type-error-expected-type.

* 也见(See Also):

type-error-datum, type-error-expected-type 

### <span id="FunctionTEDTEET">Function TYPE-ERROR-DATUM, TYPE-ERROR-EXPECTED-TYPE</span>

Syntax:

type-error-datum condition => datum

type-error-expected-type condition => expected-type

Arguments and Values:

condition---a condition of type type-error.

datum---an object.

expected-type---a type specifier.

描述(Description):

type-error-datum returns the offending datum in the situation represented by the condition.

type-error-expected-type returns the expected type of the offending datum in the situation represented by the condition.

Examples:

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

Side Effects: None.

Affected By: None.

Exceptional Situations: None.

* 也见(See Also):

type-error, Section 9 (Conditions)

Notes: None. 

### <span id="ConditionTypeSIMPLETYPEERROR">Condition Type SIMPLE-TYPE-ERROR</span>

类优先级列表(Class Precedence List):

simple-type-error, simple-condition, type-error, error, serious-condition, condition, t

描述(Description):

Conditions of type simple-type-error are like conditions of type type-error, except that they provide an alternate mechanism for specifying how the condition is to be reported; see the type simple-condition.

* 也见(See Also):

simple-condition, simple-condition-format-control, simple-condition-format-arguments, type-error-datum, type-error-expected-type 

