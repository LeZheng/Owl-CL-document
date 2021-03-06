# 15. 数组

> * 15.1 [数组的概念](#ArrayConcepts)
> * 15.2 [数组的字典](#TheArraysDictionary)

## 15.1 <span id="ArrayConcepts">数组的概念</span>

> * 15.1.1 [数组元素](#ArrayElements)
> * 15.1.2 [特化的数组](#SpecializedArrays)

### 15.1.1 <span id="ArrayElements">数组元素</span>

一个数组[array]包含了一个称为为元素[element]的对象[object]的集合, 它们可以通过一个直角坐标系统而被单独的引用.

> * 15.1.1.1 [数组索引](#ArrayIndices)
> * 15.1.1.2 [数组维度](#ArrayDimensions)
> * 15.1.1.3 [数组秩](#ArrayRank)

#### 15.1.1.1 <span id="ArrayIndices">数组索引</span>

一个数组[array]元素[element]可以通过一个(可能是空的)索引序列来引用. 这个序列的长度必须等于这个数组[array]的秩[rank]. 每一个索引必须是一个小于对应数组[array]维数[dimension]的非负的 fixnum. 数组[array]索引是从 0 开始的. 

#### 15.1.1.2 <span id="ArrayDimensions">数组维度</span>

数组[array]的一个坐标轴称为一个维度[dimension].

每一个维度大小[dimension]都是一个非负 fixnum; 如果一个数组[array]的任意维度大小都是 0, 那么这个数组[array]就没有元素. 允许一个维度大小[dimension]是 0, 在这个情况下这个数组[array]没有元素, 并且任何尝试去访问[access]一个元素[element]都是一个错误. 然而, 这个数组[array]的其他属性, 比如这些维度大小[dimension]自身, 可能被使用.

##### 15.1.1.2.1 单独数组维度大小的实现限制

一个实现[implementation]可能在数组[array]的维度大小[dimension]上强加一个限制, 但是在这个限制上这里有一个最小要求. 见变量[variable] array-dimension-limit. 


#### 15.1.1.3 <span id="ArrayRank">数组秩</span>

一个数组[array]可以有任意数量的维度[dimension] (包括零). 这个维度的数量称为秩[rank].

如果一个数组[array]的秩[rank]是 0 那么就说这个数组是没有维度[dimension]的, 并且这些维度大小的乘积是 1 (见 array-total-size); 一个零维数组[array]因此只有一个单个元素.

> * 15.1.1.3.1 [向量](#Vectors)
> * 15.1.1.3.2 [多维数组](#MultiArrays)

##### 15.1.1.3.1 <span id="Vectors">向量</span>

一个秩[rank]为 1 的数组[array] (换句话说, 一个一维数组[array]) 被称为一个向量[vector].

###### 15.1.1.3.1.1 填充指针

一个填充指针[fill pointer]是一个不大于向量[vector]中元素[element]总数的非负整数[integer]. 不是所有的向量[vector]都有填充指针[fill pointer]. 见函数[function] make-array 和 adjust-array.

如果一个向量[vector]的一个元素[element]有着大于等于 0 但是小于这个填充指针[fill pointer] (如果有的话) 的索引, 那么就说它是有效的[active]. 对于一个没有填充指针[fill pointer]的数组[array], 所有元素[element]都被认为是有效的[active].

只有向量[vector]可以有填充指针[fill pointer]; 多维数组[array]没有. 可以创建一个存储被转移到有着填充指针[fill pointer]的向量[vector]的多维数组[array].

##### 15.1.1.3.2 <span id="MultiArrays">多维数组</span>

###### 15.1.1.3.2.1 多维数组的存储布局

多维数组[array]以行优先的顺序存储它们的成分; 这也就是说, 一个多维数组[array]内部被存储为一个一维数组[array], 其中多维度索引集有序地排列, 最后一个索引变化最快. 

###### 15.1.1.3.2.2 数组秩的实现限制

一个实现[implementation]可能在一个数组[array]秩[rank]上强加一个限制, 但是在这个限制上这里有一个最小的要求. 见变量[variable] array-rank-limit. 

### 15.1.2 <span id="SpecializedArrays">特化的数组</span>

一个数组[array]可以是一个普通[general]数组[array], 意味着每个元素[element]可以是任意对象[object], 或者它可能是一个特化[specialized]数组[array], 意味着每个元素[element]都有一个约束的类型[type].

"一个数组[array]被特化[specialized]为类型[type] <<\type>>" 的说法有时被用于强调一个数组[array]的元素类型[element type]. 即便当这个类型[type] <<\type>> 是 t 这个说法也是认可的, 尽管一个被特化[specialized]为类型[type] t 的数组[array]是一个普通[general]数组[array], 而不是一个特化[specialized]数组[array].

下面这段列出了一些可应用于数组[array]创建, 访问[access], 和信息操作的已定义的名字[defined name].

    adjust-array           array-has-fill-pointer-p  make-array                   
    adjustable-array-p     array-in-bounds-p         svref                        
    aref                   array-rank                upgraded-array-element-type  
    array-dimension        array-rank-limit          upgraded-complex-part-type   
    array-dimension-limit  array-row-major-index     vector                       
    array-dimensions       array-total-size          vector-pop                   
    array-displacement     array-total-size-limit    vector-push                  
    array-element-type     fill-pointer              vector-push-extend           

    Figure 15-1. 普通目的的数组相关的已定义名字

> * 15.1.2.1 [数组提升](#ArrayUpgrading)
> * 15.1.2.2 [特化数组的必要种类](#RKOSA)


#### 15.1.2.1 <span id="ArrayUpgrading">数组提升</span>

类型[type] T1 的提升数组元素类型[upgraded array element type] T2 是 T1 的超类型[supertype], 并且在 T1 可以被用作对象创建或类型区分的数组元素类型[array element type]时可以用来替换 T1.

在一个数组[array]创建期间, 需要的元素类型[element]被称为表达数组元素类型[expressed array element type]. 这个表达数组元素类型[expressed array element type]的提升数组元素类型[upgraded array element type]成为这个要被创建的数组[array]的实际数组元素类型[actual array element type].

类型[type]提升意味着在类型层次结构中向上移动. 一个类型[type]总是为它的提升数组元素类型[upgraded array element type]的子类型[subtype]. 同样, 如果一个类型[type] Tx 是另一个类型[type] Ty 的子类型[subtype], 那么 Tx 的提升数组元素类型[upgraded array element type]必须是 Ty 的提升数组元素类型[upgraded array element type]的子类型[subtype]. 两个互斥[disjoint]的类型[type]可以被提升为相同类型[type].

一个类型[type] T1 的提升数组元素类型[upgraded array element type] T2 是一个只有 T1 自身的函数; 这也就是说, 它独立于将要使用 T2 的数组[array]的任何其他属性, 例如秩[rank], 可调整性[adjustability], 填充指针[fill pointer], 或位移. 函数[function] upgraded-array-element-type 可以被符合规范的程序[conforming program]用来预测这个实现[implementation]会怎样提升[upgrade]一个给定类型[type]. 

#### 15.1.2.2 <span id="RKOSA">特化数组的必要种类</span>

元素[element]被约束为 character 类型[type]或 character 的子类型[subtype]的向量[vector]被称为字符串[string]. 字符串[string]是 string 类型[type]. 下一段中列出了和字符串[string]相关的已定义的名字[defined name].

字符串[string]是特化[specialized]数组[array], 逻辑上可以被包含在这一章中. 然而, 出于可读性的目的, 关于字符串[string]的大部分信息不会出现在这个章节中; 见章节 16 (字符串).

    char                string-equal         string-upcase  
    make-string         string-greaterp      string/=       
    nstring-capitalize  string-left-trim     string<        
    nstring-downcase    string-lessp         string<=       
    nstring-upcase      string-not-equal     string=        
    schar               string-not-greaterp  string>        
    string              string-not-lessp     string>=       
    string-capitalize   string-right-trim                   
    string-downcase     string-trim                         

    Figure 15-2. 操作字符串的操作符

元素[element]被约束为 bit 类型[type]的向量[vector]称为位向量[bit vector]. 位向量[bit vector]是 bit-vector 类型[type]的. 下一段中列出了在位数组[bit array]上操作的一些已定义的名字[defined name].

    bit        bit-ior   bit-orc2  
    bit-and    bit-nand  bit-xor   
    bit-andc1  bit-nor   sbit      
    bit-andc2  bit-not             
    bit-eqv    bit-orc1            

    Figure 15-3. 操作位数组的操作符

## 15.2 <span id="TheArraysDictionary">数组的字典</span>

> * [系统类 ARRAY](#SC-ARRAY)
> * [类型 SIMPLE-ARRAY](#T-SIMPLE-ARRAY)
> * [系统类 VECTOR](#SC-VECTOR)
> * [类型 SIMPLE-VECTOR](#T-SIMPLE-VECTOR)
> * [系统类 BIT-VECTOR](#SC-BIT-VECTOR)
> * [类型 SIMPLE-BIT-VECTOR](#T-SIMPLE-BIT-VECTOR)
> * [函数 MAKE-ARRAY](#F-MAKE-ARRAY)
> * [函数 ADJUST-ARRAY](#F-ADJUST-ARRAY)
> * [函数 ADJUSTABLE-ARRAY-P](#F-ADJUSTABLE-ARRAY-P)
> * [访问器 AREF](#A-AREF)
> * [函数 ARRAY-DIMENSION](#F-ARRAY-DIMENSION)
> * [函数 ARRAY-DIMENSIONS](#F-ARRAY-DIMENSIONS)
> * [函数 ARRAY-ELEMENT-TYPE](#F-ARRAY-ELEMENT-TYPE)
> * [函数 ARRAY-HAS-FILL-POINTER-P](#F-ARRAY-HAS-FILL-POINTER-P)
> * [函数 ARRAY-DISPLACEMENT](#F-ARRAY-DISPLACEMENT)
> * [函数 ARRAY-IN-BOUNDS-P](#F-ARRAY-IN-BOUNDS-P)
> * [函数 ARRAY-RANK](#F-ARRAY-RANK)
> * [函数 ARRAY-ROW-MAJOR-INDEX](#F-ARRAY-ROW-MAJOR-INDEX)
> * [函数 ARRAY-TOTAL-SIZE](#F-ARRAY-TOTAL-SIZE)
> * [函数 ARRAYP](#F-ARRAYP)
> * [访问器 FILL-POINTER](#A-FILL-POINTER)
> * [访问器 ROW-MAJOR-AREF](#A-ROW-MAJOR-AREF)
> * [函数 UPGRADED-ARRAY-ELEMENT-TYPE](#F-UPGRADED-ARRAY-ELEMENT-TYPE)
> * [常量 ARRAY-DIMENSION-LIMIT](#CV-ARRAY-DIMENSION-LIMIT)
> * [常量 ARRAY-RANK-LIMIT](#CV-ARRAY-RANK-LIMIT)
> * [常量 ARRAY-TOTAL-SIZE-LIMIT](#CV-ARRAY-TOTAL-SIZE-LIMIT)
> * [函数 SIMPLE-VECTOR-P](#F-SIMPLE-VECTOR-P)
> * [访问器 SVREF](#A-SVREF)
> * [函数 VECTOR](#F-VECTOR)
> * [函数 VECTOR-POP](#F-VECTOR-POP)
> * [函数 VECTOR-PUSH, VECTOR-PUSH-EXTEND](#F-V-PUSH-V-PUSH-EXTEND)
> * [函数 VECTORP](#F-VECTORP)
> * [访问器 BIT, SBIT](#A-BIT-SBIT)
> * [函数 BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR](#F-BIT-ALL)
> * [函数 BIT-VECTOR-P](#F-BIT-VECTOR-P)
> * [函数 SIMPLE-BIT-VECTOR-P](#F-SIMPLE-BIT-VECTOR-P)


### <span id="SC-ARRAY">系统类 ARRAY</span>

* 类优先级列表(Class Precedence List):

        array, t

* 描述(Description):

        一个数组[array]包含了根据一个笛卡儿坐标系统排列的对象[object]. 一个数组[array]提供了一个从 fixnum 的集合 {i0,i1,...,ir-1} 到对应数组[array]元素[element]的映射, 其中 0 <=ij < dj, r 是这个数组的秩, 并且 dj 是这个数组的维度[dimension] j 的大小.

        当一个数组[array]被创建时, 请求它的创建的程序可能声明所有元素[element]为一个特定类型[type], 称为表达数组元素类型[expressed array element type]. 具体实现允许去提升[upgrade]这个类型来产生实际数组元素类型[actual array element type], 它是这个数组[array]被实际特化[specialized]的元素类型[element type]. 见函数[function] upgraded-array-element-type.

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        array [{element-type | *} [dimension-spec]]

        dimension-spec::= rank | * | ({dimension | *}*) 

* 复合类型指定符参数(Compound Type Specifier Arguments):

        dimension---一个有效数组维数[valid array dimension].
        element-type---一个类型指定符[type specifier].
        rank---一个非负 fixnum.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示元素类型[element type], 秩[rank], 和维数[dimension]匹配给定 element-type, rank, 和 dimensions 的数组[array]的集合. 具体的说:

        如果 element-type 是符号[symbol] *, 数组[array]不会根据其元素类型[element type]被排除. 否则, 只包括那些实际数组元素类型[actual array element type]是 element-type 的提升结果的数组; 见章节 15.1.2.1 (数组提升).

        如果这个 dimension-spec 是一个 rank, 这个集合只包括那些有着那个 rank 的数组. 如果这个 dimension-spec 是 dimensions 的一个列表[list], 这个集合只包括那些有着由 dimensions 的长度[length]给定的秩[rank]并且有着指明的 dimensions 的数组; 在这个情况中, * 匹配对应维数[dimension]的任意值. 如果这个 dimension-spec 是符号[symbol] *, 这个集合不会被约束在秩[rank]或维数[dimension]的基础上.

* 参见(See Also):

        *print-array*, aref, make-array, vector, 章节 2.4.8.12 (井号A(#A)), 章节 22.1.3.8 (打印其他数组)

* 注意(Notes):

        注意类型 (array t) 是类型 (array *) 的一个适当的子类型[subtype]. 这个原因是类型 (array t) 是持有任何对象[object]的数组[array]的集合 (这些元素[element]是类型[type] t, 它包括所有对象[object]). 另一方面, 类型 (array *) 是所有数组[array]的集合, 包括例如只持有字符[character]的数组[array]. 类型 (array character) 不是类型 (array t) 的一个子类型[subtype]; 这两个集合是互斥[disjoint]的因为类型 (array character) 不是所有可以持有字符[character]的数组[array]的集合, 而是一组专门用来保存精确字符[character]并且没有其他对象[object]的数组的集合. 


### <span id="T-SIMPLE-ARRAY">类型 SIMPLE-ARRAY</span>

* 超类型(Supertypes):

        simple-array, array, t

* 描述(Description):

        一个存储没有被转移到另一个数组[array], 没有填充指针[fill pointer], 并且不是明显可调整[expressly adjustable]的数组[array]的类型[type]是类型[type] simple-array 的一个子类型[subtype]. 简单数组[simple array]概念允许实现使用特定的表示, 并允许用户声明某些值总是为简单数组[simple array].

        类型[type] simple-vector, simple-string, 和 simple-bit-vector 是类型[type] simple-array 的互斥[disjoint]的子类型[subtype], 对于它们分别意味着 (simple-array t (*)), 所有 c 为 character 类型[type]的子类型[subtype]的 (simple-array c (*)) 的并集, 以及 (simple-array bit (*)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-array [{element-type | *} [dimension-spec]]

        dimension-spec::= rank | * | ({dimension | *}*) 

* 复合类型指定符参数(Compound Type Specifier Arguments):

        dimension---一个有效数组维度大小[valid array dimension].
        element-type---一个类型指定符[type specifier].
        rank---一个非负 fixnum.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个复合类型指定符[compound type specifier]的处理方法和 array 类型[type]的复合类型指定符[compound type specifier]的处理方法完全相同, 除了这个集合被进一步约束为只包含简单数组[simple array].

* 注意(Notes):

        存储被转移的数组[displaced array], 带有填充指针[fill pointer]的向量[vector], 或实际可调整[actually adjustable]的数组[array]是否为简单数组[simple array]是依赖于具体实现的[implementation-dependent].

        (simple-array *) 不管元素类型引用了所有简单数组[simple array], (simple-array type-specifier) 只引用那些可以通过给定 type-specifier 作为 make-array 的 :element-type 参数得到的简单数组[simple array]. 


### <span id="SC-VECTOR">系统类 VECTOR</span>

* 类优先级列表(Class Precedence List):

        vector, array, sequence, t

* 描述(Description):

        任何一维数组[array]都是一个向量[vector].

        类型[type] vector 是类型[type] array 的一个子类型[subtype]; 对于所有类型[type] x, (vector x) 和 (array x (*)) 相同.

        类型[type] (vector t), 类型[type] string, 还有类型[type] bit-vector 是类型[type] vector 的互斥[disjoint]子类型[subtype].

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        vector [{element-type | *} [{size | *}]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum.
        element-type---一个类型指定符[type specifier].

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示那个元素类型[element type]和维度大小[dimension]都匹配指定值的特化向量[vector]的集合. 具体来说:

        如果 element-type 是符号[symbol] *, 向量[vector]不会根据它们的元素类型[element type]被排除. 否则, 只有那些实际数组元素类型[actual array element type]是 element-type 提升[upgrade]的结果的那些向量会被包括进去; 见章节 15.1.2.1 (数组提升).

        如果指定了一个 size, 那么这个集合只包括那些仅有的维度大小[dimension]是 size 的向量. 如果符号[symbol] * 被指定而不是一个 size, 那么这个集合不会被约束在这个维度大小[dimension]的基础上.

* 参见(See Also):

        章节 15.1.2.2 (特化数组的必要种类), 章节 2.4.8.3 (井号左括号(#()), 章节 22.1.3.7 (打印其他向量), 章节 2.4.8.12 (井号A(#A))

* 注意(Notes):

        类型[type] (vector e s) 等价于类型[type] (array e (s)).

        类型[type] (vector bit) 有着名称 bit-vector.

        所有类型[type] (vector C) 的并集(其中 C 是 character 的任意子类型[subtype])有着名字 string.

        (vector *) 引用所有的向量[vector], 不管元素类型是什么, (vector type-specifier) 只引用那些可以通过给定 type-specifier 作为 make-array 的 :element-type 参数得到的向量[vector]. 


### <span id="T-SIMPLE-VECTOR">类型 SIMPLE-VECTOR</span>

* 超类型(Supertypes):

        simple-vector, vector, simple-array, array, sequence, t

* 描述(Description):

        一个存储没有被转移到另一个数组[array]中, 没有填充指针[fill pointer], 不是明显可调整[expressly adjustable]的并且可能持有任何类型[type]元素的向量[vector]是类型[type] simple-vector 的子类型[subtype].

        类型[type] simple-vector 类型[type] vector 的一个子类型[subtype], 并且是类型[type] (vector t) 的子类型[subtype].

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *. 默认是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个和 (simple-array t (size)) 是相同的. 


### <span id="SC-BIT-VECTOR">系统类 BIT-VECTOR</span>

* 类优先级列表(Class Precedence List):

        bit-vector, vector, array, sequence, t

* 描述(Description):

        一个位向量[bit vector]是一个元素类型[element type]为 bit 的向量[vector].

        这个 bit-vector 类型[type]是 vector 类型[type]的一个子类型[subtype], 对于 bit-vector 意味着 (vector bit).

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        bit-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示和类型[type] (array bit (size)) 相同的类型[type]; 这也就是说, 大小为 size 的位向量[bit vector]的集合.

* 参见(See Also):

        章节 2.4.8.4 (井号星号(#*)), 章节 22.1.3.6 (打印位向量), 章节 15.1.2.2 (特化数组的必要种类) 


### <span id="T-SIMPLE-BIT-VECTOR">类型 SIMPLE-BIT-VECTOR</span>

* 超类型(Supertypes):

        simple-bit-vector, bit-vector, vector, simple-array, array, sequence, t

* 描述(Description):

        一个不会被转移到另一个数组[array]中, 没有填充指针[fill pointer], 并且不是明显可调整[expressly adjustable]的位向量[bit vector]的类型[type]是类型[type] simple-bit-vector 的一个子类型[subtype].

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-bit-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        size---一个非负 fixnum, 或者符号[symbol] *. 默认是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示和类型[type] (simple-array bit (size)) 相同的类型; 这也就是说, 大小为 size 的简单位向量[simple bit vector]的集合. 


### <span id="F-MAKE-ARRAY">函数 MAKE-ARRAY</span>

* 语法(Syntax):

        make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset
        => new-array

* 参数和值(Arguments and Values):

        dimensions---一个有效数组维度大小[valid array dimension]的列表[list]的标识符[designator].
        element-type---一个类型指定符[type specifier]. 默认是 t.
        initial-element---一个对象[object].
        initial-contents---一个对象[object].
        adjustable---一个广义 boolean [generalized boolean]. 默认是 nil.
        fill-pointer---这个要被创建的数组[array]的有效填充指针[valid fill pointer], 或者是 t 或 nil. 默认是 nil.
        displaced-to---一个数组[array]或 nil. 默认是 nil. 如果 initial-element 或 initial-contents 被提供了那么这个选项一定不能被提供.
        displaced-index-offset--- displaced-to 的一个有效数组行优先索引[valid array row-major index]. 默认是 0. 这个选项一定不能被提供, 除非提供了一个非 nil [non-nil]的 displaced-to.
        new-array---一个数组[array].

* 描述(Description):

        创建并返回一个由可以容纳 element-type 所给定的类型[type]的元素的最特化[specialized]类型[type]构成的数组[array]. 如果 dimensions 是 nil 那么一个零维数组[array]会被创建.

        dimensions 表示这个新数组的维度大小.

        element-type 表示要被存储到新数组 new-array 中的元素的类型[type]. 这个 new-array 实际上可以存储从 element-type 提升[upgrade]得到的类型[type]的任何对象[object]; 见章节 15.1.2.1 (数组提升).

        如果提供了 initial-element, 它被用于初始化 new-array 的每一个元素[element]. 如果提供了 initial-element, 它必须是 element-type 给定的类型[type]. 如果 :initial-contents 选项被提供了或者 displaced-to 是非 nil [non-nil], 那么 initial-element 不能被提供. 如果没有提供 initial-element, 那么后面去读取 new-array 的未初始化元素[element]的后果是未定义的, 除非提供了 initial-contents 或者 displaced-to 不是 nil.

        initial-contents 被用于初始化数组[array]的内容. 比如:

        (make-array '(4 2 3) :initial-contents
                    '(((a b c) (1 2 3))
                      ((d e f) (3 1 2))
                      ((g h i) (2 3 1))
                      ((j k l) (0 0 0))))

        initial-contents 由一个嵌套的序列[sequence]结构组成. 在这个结构中的层级的数量等价于数组[array]的秩. 这个嵌套的结构的每一个叶节点必须是 element-type 给定的类型[type]. 如果数组[array]是零维的, 那么 initial-contents 指定单个元素[element]. 否则, initial-contents 必须是一个长度和第一个维度相同的序列[sequence]; 每一个元素必须是一个维度大小为剩余维度大小的数组[array]的嵌套结构, 以此类推. 如果 initial-element 被提供了或者 displaced-to 非 nil [non-nil], 那么 initial-contents 不能被提供. 如果没有提供 initial-contents, 那么后面去读取 new-array 的未初始化元素的后果是未定义的, 除非提供了 initial-contents 或者 displaced-to 非 nil [non-nil].

        如果 adjustable 非 nil [non-nil], 那么这个数组就是明确可调整的[expressly adjustable] (所以实际上可调整的[actually adjustable]); 否则, 这个数组就不是明确可调整的[expressly adjustable] (这个数组是否为实际上可调整的[actually adjustable]是依赖于具体实现的[implementation-dependent]).

        如果 fill-pointer 非 nil [non-nil], 数组[array]必须是一维的; 这也就是说, 这个数组[array]必须是一个向量[vector]. 如果 fill-pointer 是 t, 这个向量[vector]的长度被用于初始化这个填充指针[fill pointer]. 如果 fill-pointer 是一个整数[integer], 它成为这个向量[vector]的初始填充指针[fill pointer].

        如果 displaced-to 非 nil [non-nil], make-array 会创建一个存储被转移的数组[displaced array]并且 displaced-to 就是那个存储被转移的数组[displaced array]的目标[target]. 在这个情况中, 如果 displaced-to 的实际数组元素类型[actual array element type]和要被创建的数组[array]的实际数组元素类型[actual array element type]不是类型等价的[type equivalent], 那么后果时未定义的. 如果 displaced-to 是 nil, 这个数组[array]就不是一个存储被转移的数组[displaced array].

        displaced-index-offset 被设置为这个数组[array]的索引偏移量. 当一个数组 A 在创建数组 B 时被给定用作给 make-array 的 :displaced-to 实参[argument], 那么数组 B 就被说是转移到数组 A. 在一个数组[array]中的元素的总数, 称为这个数组[array]的总大小, 被计算为所有维度的乘积. 这就需要 A 的总大小不小于 B 的总大小加上 displaced-index-offset 提供的 n 以后的大小. 这个转移的效果是数组 B 没有它自己的任何元素, 但是把到它自己的访问[access]被映射到对数组 A 的访问[access]. 这个映射处理这两个数组就好像它们是一维的, 以行优先的顺序获取元素, 并且映射一个对数组 B 的元素 k 的访问[access]到对数组 A 的第 k+n 个元素的访问[access].

        如果 make-array 被调用时 adjustable, fill-pointer, 和 displaced-to 都是 nil, 那么结果就是一个简单数组[simple array]. 如果 make-array 被调用时 adjustable, fill-pointer, 或 displaced-to 不止一个为 true, 产生的数组[array]是否为一个简单数组[simple array]是依赖于具体实现的[implementation-dependent].

        在创建数组 B 时, 当一个数组 A 被给定作为 make-array 的 :displaced-to 实参[argument]时, 那么就说数组 B 转移到了数组 A. 在一个数组[array]中的元素总数, 称为这个数组[array]的总大小, 通过所有维度的乘积计算出来. 如果 A 的总大小小于 B 加上 displaced-index-offset 提供的偏移 n 以后的总大小, 那么后果是未指定的. 这个转移的效果是数组 B 没有它自己的任何元素, 但是把到它自己的访问[access]被映射到对数组 A 的访问[access]. 这个映射处理这两个数组就好像它们是一维的, 以行优先的顺序获取元素, 并且映射一个对数组 B 的元素 k 的访问[access]到对数组 A 的第 k+n 个元素的访问[access].

* 示例(Examples):

    ```LISP
    (make-array 5) ;; Creates a one-dimensional array of five elements.
    (make-array '(3 4) :element-type '(mod 16)) ;; Creates a 
                    ;;two-dimensional array, 3 by 4, with four-bit elements.
    (make-array 5 :element-type 'single-float) ;; Creates an array of single-floats.

    (make-array nil :initial-element nil) =>  #0ANIL
    (make-array 4 :initial-element nil) =>  #(NIL NIL NIL NIL)
    (make-array '(2 4) 
                  :element-type '(unsigned-byte 2) 
                  :initial-contents '((0 1 2 3) (3 2 1 0)))
    =>  #2A((0 1 2 3) (3 2 1 0))
    (make-array 6
                  :element-type 'character 
                  :initial-element #\a 
                  :fill-pointer 3) =>  "aaa"
    ```

        以下是创建存储被转义的数组[displaced array]的示例.

    ```LISP
    (setq a (make-array '(4 3))) 
    =>  #<ARRAY 4x3 simple 32546632>
    (dotimes (i 4)
      (dotimes (j 3)
        (setf (aref a i j) (list i 'x j '= (* i j)))))
    =>  NIL
    (setq b (make-array 8 :displaced-to a
                          :displaced-index-offset 2))
    =>  #<ARRAY 8 indirect 32550757>
    (dotimes (i 8)
      (print (list i (aref b i))))
    >>  (0 (0 X 2 = 0)) 
    >>  (1 (1 X 0 = 0)) 
    >>  (2 (1 X 1 = 1)) 
    >>  (3 (1 X 2 = 2)) 
    >>  (4 (2 X 0 = 0)) 
    >>  (5 (2 X 1 = 2)) 
    >>  (6 (2 X 2 = 4)) 
    >>  (7 (3 X 0 = 0)) 
    =>  NIL
    ```

        这个最后一个例子依赖于那个数组[array]事实上以行优先顺序存储的事实.

    ```LISP
    (setq a1 (make-array 50))
    =>  #<ARRAY 50 simple 32562043>
    (setq b1 (make-array 20 :displaced-to a1 :displaced-index-offset 10))
    =>  #<ARRAY 20 indirect 32563346>
    (length b1) =>  20

    (setq a2 (make-array 50 :fill-pointer 10))
    =>  #<ARRAY 50 fill-pointer 10 46100216>
    (setq b2 (make-array 20 :displaced-to a2 :displaced-index-offset 10))
    =>  #<ARRAY 20 indirect 46104010>
    (length a2) =>  10
    (length b2) =>  20

    (setq a3 (make-array 50 :fill-pointer 10))
    =>  #<ARRAY 50 fill-pointer 10 46105663>
    (setq b3 (make-array 20 :displaced-to a3 :displaced-index-offset 10
                            :fill-pointer 5))
    =>  #<ARRAY 20 indirect, fill-pointer 5 46107432>
    (length a3) =>  10
    (length b3) =>  5
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        adjustable-array-p, aref, arrayp, array-element-type, array-rank-limit, array-dimension-limit, fill-pointer, upgraded-array-element-type

* 注意(Notes):

        这里没有指定去创建一个 adjustable-array-p 肯定返回 false 的数组[array]的方法. 这里没有指定去创建一个不是简单数组[simple array]的数组[array]的方法. 


### <span id="F-ADJUST-ARRAY">函数 ADJUST-ARRAY</span>

* 语法(Syntax):

        adjust-array array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset
        => adjusted-array

* 参数和值(Arguments and Values):

        array---一个数组[array].
        new-dimensions---一个有效数组维度大小[valid array dimension]或者一个有效数组维度大小[valid array dimension]的列表[list].
        element-type---一个类型指定符[type specifier].
        initial-element---一个对象[object]. 如果提供了 initial-contents 或 displaced-to, 那么 initial-element 一定不能被提供.
        initial-contents---一个对象[object]. 如果数组[array]有着大于零的秩, 那么 initial-contents 由嵌套的序列[sequence]组成, 它的深度必须等于数组 array 的秩. 否则, 数组[array]是零维的并且 initial-contents 提供单个元素. 如果给定了 initial-element 或 displaced-to 那么 initial-contents 一定不能被提供.
        fill-pointer---一个要被创建的数组 array 的有效填充指针[valid fill pointer], 或者 t, 或者 nil. 默认是 nil.
        displaced-to---一个数组[array]或 nil. 如果提供了 displaced-to 那么 initial-elements 和 initial-contents 一定不能提供.
        displaced-index-offset---一个 (fixnum 0 n) 类型[type]的对象[object], 其中 n 是 (array-total-size displaced-to). 当且仅当提供了 displaced-to 那么displaced-index-offset 可以被提供.
        adjusted-array---一个数组[array].

* 描述(Description):

        adjust-array 改变数组 array 的维度大小或元素. 结果是一个和数组 array 相同类型[type]和秩的数组[array], 它是修改后的数组 array, 或者一个数组 array 所转移到的新创建的数组[array], 并且有着给定的新维数 new-dimensions.

        new-dimensions 指定了数组 array 的每一个维度大小[dimension].

        element-type 指定了产生的数组[array]的元素[element]类型[type]. 在提供了 element-type 的情况下, 如果 element-type 的提升数组元素类型[upgraded array element type]和数组 array 的实际数组元素类型[actual array element type]不相同那么后果是未指定的.

        如果提供了 initial-contents, 它的处理会像 make-array 一样. 在这个情况中数组 array 的原始内容不会出现在产生的数组[array]中.

        如果 fill-pointer 是一个整数[integer], 它就成为那个产生的数组[array]的填充指针[fill pointer]. 如果 fill-pointer 是符号 t, 它表示那个产生的数组[array]的大小被用作填充指针[fill pointer]. 如果 fill-pointer 是 nil, 它表示填充指针[fill pointer]应该保持原样.

        如果 displaced-to 是非 nil [non-nil], 那么就会创建一个存储被转移的数组[displaced array]. 产生的数组[array]和通过 displaced-to 给定的数组[array]共享内容. 那个产生的数组[array]不能包含比那个存储被转移的数组[displaced array]更多的元素. 如果没有提供 displaced-to 或者是 nil, 那么产生的数组[array]不是一个存储被转移的数组[displaced array]. 如果一个被创建的数组 A 转移到数组 B 并且后续数组 B 被给到 adjust-array, 数组 A 仍然会转移到数组 B. 虽然数组 array 可能是一个存储被转移的数组[displaced array], 但是产生的数组[array]不是一个存储被转移的数组[displaced array]除非提供了 displaced-to 并且不是 nil. 这个 adjust-array 和存储被转移的数组[displaced array]的交互就像下面给定的三个数组 A, B, 和 C 一样:

        A 在调用前后都没有被转移

            (adjust-array A ...)

            这个 A 的维数被修改, 并且内容会被适当重排. A 的额外元素来自于 initial-element. 这个 initial-contents 的使用会导致所有旧的内容被丢弃.

        A 在调用之前没有被转移, 但是在调用之后被转移到 C

            (adjust-array A ... :displaced-to C)

            A 中的原始内容不会出现在之后的 A 中; A 现在包含了 C 的内容, 而 C 不带有任何的重排.

        A 在这个调用之前被转移到 B, 在调用之后被转移到 C

            (adjust-array A ... :displaced-to B)
            (adjust-array A ... :displaced-to C)

            B 和 C 可能相同. 如果在这个 adjust-array 调用中没有提供 displaced-index-offset, 它默认为 0, 那么之后 B 中的内容可能不会出现在 A 中除非这样的内容也出现在 C 中; 在 B 中的旧的偏移位不会被保留.

        A 在调用前被转移到 B, 但是在调用后没有被转移.

            (adjust-array A ... :displaced-to B)
            (adjust-array A ... :displaced-to nil)

            A 得到一个新的 "数据区域(data region)", 而 B 的内容被拷贝到这个里面并保留已存在的旧元素; A 的另外的元素从 initial-element 中提取, 如果提供的话. 但是, 这个 initial-contents 的使用会导致所有旧的内容被丢弃.

        如果提供了 displaced-index-offset, 它指定了产生的数组[array]从它被转移到的数组[array]开始的偏移. 如果没有提供 displaced-index-offset, 偏移就是 0. 这个产生的数组[array]的大小加上偏移值不能超过那个它被转移到的数组[array]的大小.

        如果只提供了 new-dimensions 和一个 initial-element 参数, 数组 array 的那些元素仍然在产生的数组[array]的范围中出现. 产生的数组[array]中没有出现在数组[array]范围中的元素被初始化为 initial-element; 如果没有提供 initial-element, 后面在元素被初始化之前去读取 new-array 的这样的新元素的后果是未定义的.

        如果提供了 initial-contents 或 displaced-to, 那么数组 array 中的原始内容不会出现在新的数组[array]中.

        如果数组 array 被调整为一个小于它的填充指针[fill pointer]的大小并且没有提供 fill-pointer 参数, 这样一来它的填充指针[fill pointer]在这个过程中会被适当调整, 那么后果是未指定的.

        如果 A 被转移到 B, 如果 B 以一种不再有足够元素来满足 A 的方式被调整, 那么后果是未指定的.

        如果 adjust-array 被应用到一个实际上可调整[actually adjustable]的数组[array], 那么返回的数组[array]和数组 array 相等[identical]. 如果由 adjust-array 返回的数组[array]和数组 array 不同[distinct], 那么参数 array 不会被改变.

        注意, 如果一个数组[array] A 被转移到另一个数组[array] B, 而 B 被转移到另一个数组[array] C, 并且 B 被 adjust-array 修改, 那么 A 现在引用 B 中的调整的内容. 这个意味着一个实现不能折叠这个链来使 A 直接引用 C 而忘记了这个通过 B 的引用链. 但是, 缓存技术是允许的, 只要它们保留这里指定的语义.

* 示例(Examples):

    ```LISP
    (adjustable-array-p
      (setq ada (adjust-array
                  (make-array '(2 3)
                              :adjustable t
                              :initial-contents '((a b c) (1 2 3)))
                  '(4 6)))) =>  T 
    (array-dimensions ada) =>  (4 6) 
    (aref ada 1 1) =>  2 
    (setq beta (make-array '(2 3) :adjustable t))
    =>  #2A((NIL NIL NIL) (NIL NIL NIL)) 
    (adjust-array beta '(4 6) :displaced-to ada)
    =>  #2A((A B C NIL NIL NIL)
          (1 2 3 NIL NIL NIL)
          (NIL NIL NIL NIL NIL NIL) 
          (NIL NIL NIL NIL NIL NIL))
    (array-dimensions beta) =>  (4 6)
    (aref beta 1 1) =>  2 
    ```

        假设在 m 中的 4×4 数组看上去这样:

        #2A(( alpha     beta      gamma     delta )
            ( epsilon   zeta      eta       theta )
            ( iota      kappa     lambda    mu    )
            ( nu        xi        omicron   pi    ))

        那么下面这个的结果

        (adjust-array m '(3 5) :initial-element 'baz)

        是一个 3×5 的数组, 内容如下

        #2A(( alpha     beta      gamma     delta     baz )
            ( epsilon   zeta      eta       theta     baz )
            ( iota      kappa     lambda    mu        baz ))

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果提供了填充指针[fill pointer]并且是非 nil [non-nil]但是 array 没有填充指针[fill pointer], 那么就会发出一个 error 类型[type]的错误.

* 参见(See Also):

        adjustable-array-p, make-array, array-dimension-limit, array-total-size-limit, array

* 注意(Notes): None. 


### <span id="F-ADJUSTABLE-ARRAY-P">函数 ADJUSTABLE-ARRAY-P</span>

* 语法(Syntax):

        adjustable-array-p array => generalized-boolean

* 参数和值(Arguments and Values):

        array---一个数组[array].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        当且仅当给定那个数组 array 作为 adjust-array 的第一个实参[argument]可以返回一个相同[identical]的值[value]时这个函数返回 true.

* 示例(Examples):

    ```LISP
    (adjustable-array-p 
      (make-array 5
                  :element-type 'character 
                  :adjustable t 
                  :fill-pointer 3)) =>  true
    (adjustable-array-p (make-array 4)) =>  implementation-dependent
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array]应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        adjust-array, make-array

* 注意(Notes): None. 


### <span id="A-AREF">访问器 AREF</span>

* 语法(Syntax):

        aref array &rest subscripts => element

        (setf (aref array &rest subscripts) new-element)

* 参数和值(Arguments and Values):

        array---一个数组[array].
        subscripts---数组 array 的有效数组索引的一个列表[list].
        element, new-element---一个对象.

* 描述(Description):

        访问[access]由 subscripts 所指定的数组 array 元素[element]. 如果没有提供 subscripts 并且数组 array 是零维数的, aref 访问[access]数组 array 的单个元素.

        aref 忽略填充指针[fill pointer]. 允许使用 aref 去访问[access]任何数组 array 元素[element], 不管是否有效[active].

* 示例(Examples):

        如果变量 foo 命名一个 3×5 的数组, 第一个索引可以是 0, 1, 或 2, 并且第二个元素为 0, 1, 2, 3, 或 4. 数组元素可以通过使用函数[function] aref 来引用; 比如, (aref foo 2 1) 引用这个数组 array 的 (2, 1) 元素.

    ```LISP
    (aref (setq alpha (make-array 4)) 3) =>  implementation-dependent
    (setf (aref alpha 3) 'sirens) =>  SIRENS
    (aref alpha 3) =>  SIRENS
    (aref (setq beta (make-array '(2 4) 
                        :element-type '(unsigned-byte 2)
                        :initial-contents '((0 1 2 3) (3 2 1 0))))
            1 2) =>  1
    (setq gamma '(0 2))
    (apply #'aref beta gamma) =>  2
    (setf (apply #'aref beta gamma) 3) =>  3
    (apply #'aref beta gamma) =>  3
    (aref beta 0 2) =>  3
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        bit, char, elt, row-major-aref, svref, 章节 3.2.1 (编译器术语)

* 注意(Notes): None. 


### <span id="F-ARRAY-DIMENSION">函数 ARRAY-DIMENSION</span>

* 语法(Syntax):

        array-dimension array axis-number => dimension

* 参数和值(Arguments and Values):

        array---一个数组[array].
        axis-number---大于等于 0 并且小于这个数组 array 的秩[rank]的一个整数[integer].
        dimension---一个非负整数[integer].

* 描述(Description):

        array-dimension 返回数组 array 的 axis-number 维度[dimension[1]]的大小. (忽略任何填充指针[fill pointer].)

* 示例(Examples):

    ```LISP
    (array-dimension (make-array 4) 0) =>  4
    (array-dimension (make-array '(2 3)) 1) =>  3
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        array-dimensions, length

* 注意(Notes):

        (array-dimension array n) ==  (nth n (array-dimensions array))


### <span id="F-ARRAY-DIMENSIONS">函数 ARRAY-DIMENSIONS</span>

* 语法(Syntax):

        array-dimensions array => dimensions

* 参数和值(Arguments and Values):

        array---一个数组[array].
        dimensions---一个整数[integer]列表[list].

* 描述(Description):

        返回数组 array 维数[dimension]的一个列表[list]. (如果数组 array 是一个带有填充指针[fill pointer]的向量[vector], 那么就忽略那个填充指针[fill pointer].)

* 示例(Examples):

    ```LISP
    (array-dimensions (make-array 4)) =>  (4)
    (array-dimensions (make-array '(2 3))) =>  (2 3)
    (array-dimensions (make-array 4 :fill-pointer 2)) =>  (4)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        array-dimension

* 注意(Notes): None. 


### <span id="F-ARRAY-ELEMENT-TYPE">函数 ARRAY-ELEMENT-TYPE</span>

* 语法(Syntax):

        array-element-type array => typespec

* 参数和值(Arguments and Values):

        array---一个数组[array].
        typespec---一个类型指定符[type specifier].

* 描述(Description):

        返回表示这个数组 array 的实际数组元素类型[actual array element type]的类型指定符[type specifier]. (由于数组[array]提升[upgrade], 这个类型指定符[type specifier]在一些情况下表示这个数组 array 的表达数组元素类型[expressed array element type]的超类型[supertype].)

* 示例(Examples):

    ```LISP
    (array-element-type (make-array 4)) =>  T
    (array-element-type (make-array 12 :element-type '(unsigned-byte 8))) 
    =>  implementation-dependent
    (array-element-type (make-array 12 :element-type '(unsigned-byte 5)))
    =>  implementation-dependent
    ```

        (array-element-type (make-array 5 :element-type '(mod 5)))

        可以是 (mod 5), (mod 8), fixnum, t, 或者任何其他是 (mod 5) 的超类型[supertype]的类型.


* 受此影响(Affected By):

        具体实现[implementation].

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        array, make-array, subtypep, upgraded-array-element-type

* 注意(Notes): None. 


### <span id="F-ARRAY-HAS-FILL-POINTER-P">函数 ARRAY-HAS-FILL-POINTER-P</span>

* 语法(Syntax):

        array-has-fill-pointer-p array => generalized-boolean

* 参数和值(Arguments and Values):

        array---一个数组[array].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果数组 array 有一个填充指针[fill pointer]就返回 true; 否则返回 false.

* 示例(Examples):

    ```LISP
    (array-has-fill-pointer-p (make-array 4)) =>  implementation-dependent
    (array-has-fill-pointer-p (make-array '(2 3))) =>  false
    (array-has-fill-pointer-p
      (make-array 8 
                  :fill-pointer 2 
                  :initial-element 'filler)) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array], 应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-array, fill-pointer

* 注意(Notes):

        因为除了秩[rank]为 1 之外的数组[array]不能有一个填充指针[fill pointer], 当 array-has-fill-pointer-p 的参数为这样一个数组时总是返回 nil. 


### <span id="F-ARRAY-DISPLACEMENT">函数 ARRAY-DISPLACEMENT</span>

* 语法(Syntax):

        array-displacement array => displaced-to, displaced-index-offset

* 参数和值(Arguments and Values):

        array---一个数组[array].
        displaced-to---一个数组[array]或 nil.
        displaced-index-offset---一个非负 fixnum.

* 描述(Description):

        如果这个数组 array 是一个存储被转移的数组[displaced array], 返回这个数组[array]的 :displaced-to 和 :displaced-index-offset 选项的值[value] (见函数[function] make-array 和 adjust-array). 如果这个数组 array 不是一个存储被转移的数组[displaced array], 返回 nil 和 0.

        如果 array-displacement 在一个数组 array 上被调用, 对于这个数组一个非 nil [non-nil]对象[object]被提供作为给 make-array 或 adjust-array 的 :displaced-to 实参[argument], 它一定返回这个对象[object]作为它的第一个值. array-displacement 是否为任何其他数组返回一个非 nil [non-nil]主值[primary value]是依赖于具体实现的[implementation-dependent].

* 示例(Examples):

    ```LISP
    (setq a1 (make-array 5)) =>  #<ARRAY 5 simple 46115576>
    (setq a2 (make-array 4 :displaced-to a1
                            :displaced-index-offset 1))
    =>  #<ARRAY 4 indirect 46117134>
    (array-displacement a2)
    =>  #<ARRAY 5 simple 46115576>, 1
    (setq a3 (make-array 2 :displaced-to a2
                            :displaced-index-offset 2))
    =>  #<ARRAY 2 indirect 46122527>
    (array-displacement a3)
    =>  #<ARRAY 4 indirect 46117134>, 2
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 array 不是一个数组[array], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-array

* 注意(Notes): None. 


### <span id="F-ARRAY-IN-BOUNDS-P">函数 ARRAY-IN-BOUNDS-P</span>

* 语法(Syntax):

        array-in-bounds-p array &rest subscripts => generalized-boolean

* 参数和值(Arguments and Values):

        array---一个数组[array].
        subscripts---一个长度等价于数组 array 的秩[rank]的整数[integer]列表[list].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果 subscripts 都在数组 array 的边界内就返回 true; 否则返回 false. (如果 array 是一个带有填充指针[fill pointer]的向量[vector], 那么忽略那个填充指针[fill pointer].)

* 示例(Examples):

    ```LISP
    (setq a (make-array '(7 11) :element-type 'string-char))
    (array-in-bounds-p a 0  0) =>  true
    (array-in-bounds-p a 6 10) =>  true
    (array-in-bounds-p a 0 -1) =>  false
    (array-in-bounds-p a 0 11) =>  false
    (array-in-bounds-p a 7  0) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        array-dimensions

* 注意(Notes):

        (array-in-bounds-p array subscripts)   
        ==  (and (not (some #'minusp (list subscripts)))


### <span id="F-ARRAY-RANK">函数 ARRAY-RANK</span>

* 语法(Syntax):

        array-rank array => rank

* 参数和值(Arguments and Values):

        array---一个数组[array].
        rank---一个非负整数[integer].

* 描述(Description):

        返回数组 array 的维度[dimension]数量.

* 示例(Examples):

    ```LISP
    (array-rank (make-array '())) =>  0
    (array-rank (make-array 4)) =>  1
    (array-rank (make-array '(4))) =>  1
    (array-rank (make-array '(2 3))) =>  2
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        array-rank-limit, make-array

* 注意(Notes): None. 


### <span id="F-ARRAY-ROW-MAJOR-INDEX">函数 ARRAY-ROW-MAJOR-INDEX</span>

* 语法(Syntax):

        array-row-major-index array &rest subscripts => index

* 参数和值(Arguments and Values):

        array---一个数组[array].
        subscripts---数组 array 的有效数组索引的一个列表[list].
        index---数组 array 的一个有效数组行优先索引[valid array row-major index].

* 描述(Description):

        根据数组 array 的行优先顺序计算由 subscripts 指定的元素的位置, 返回从数组 array 的开始计算的位置的元素的偏移量.

        对于一个一维数组, array-row-major-index 的结果等于 subscript.

        array-row-major-index 忽略填充指针[fill pointer].

* 示例(Examples):

    ```LISP
    (setq a (make-array '(4 7) :element-type '(unsigned-byte 8)))
    (array-row-major-index a 1 2) =>  9
    (array-row-major-index 
        (make-array '(2 3 4) 
                    :element-type '(unsigned-byte 8)
                    :displaced-to a
                    :displaced-index-offset 4)
        0 2 1) =>  9
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        一个没有错误检测的可能的 array-row-major-index 定义是

    ```LISP
    (defun array-row-major-index (a &rest subscripts)
      (apply #'+ (maplist #'(lambda (x y)
                                (* (car x) (apply #'* (cdr y))))
                          subscripts
                          (array-dimensions a))))
    ```


### <span id="F-ARRAY-TOTAL-SIZE">函数 ARRAY-TOTAL-SIZE</span>

* 语法(Syntax):

        array-total-size array => size

* 参数和值(Arguments and Values):

        array---一个数组[array].
        size---一个非负整数[integer].

* 描述(Description):

        返回这个数组 array 的数组总大小[array total size].

* 示例(Examples):

    ```LISP
    (array-total-size (make-array 4)) =>  4
    (array-total-size (make-array 4 :fill-pointer 2)) =>  4
    (array-total-size (make-array 0)) =>  0
    (array-total-size (make-array '(4 2))) =>  8
    (array-total-size (make-array '(4 0))) =>  0
    (array-total-size (make-array '())) =>  1
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数组[array], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-array, array-dimensions

* 注意(Notes):

        如果这个数组 array 是一个带有填充指针[fill pointer]的向量[vector], 那么在计算这个数组总大小[array total size]时这个填充指针[fill pointer]会被忽略.

        由于没有参数的乘积是 1, 所以一个零维数组的数组总大小[array total size]是 1.

        (array-total-size x)
            ==  (apply #'* (array-dimensions x))
            ==  (reduce #'* (array-dimensions x))


### <span id="F-ARRAYP">函数 ARRAYP</span>

* 语法(Syntax):

        arrayp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 array 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (arrayp (make-array '(2 3 4) :adjustable t)) =>  true
    (arrayp (make-array 6)) =>  true
    (arrayp #*1011) =>  true
    (arrayp "hi") =>  true
    (arrayp 'hi) =>  false
    (arrayp 12) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        typep

* 注意(Notes):

        (arrayp object) ==  (typep object 'array)


### <span id="A-FILL-POINTER">访问器 FILL-POINTER</span>

* 语法(Syntax):

        fill-pointer vector => fill-pointer

        (setf (fill-pointer vector) new-fill-pointer)

* 参数和值(Arguments and Values):

        vector---一个带有填充指针[fill pointer]的向量[vector].
        fill-pointer, new-fill-pointer---向量 vector 的一个有效填充指针[valid fill pointer].

* 描述(Description):

        访问[access]向量 vector 的填充指针[fill pointer].

* 示例(Examples):

    ```LISP
    (setq a (make-array 8 :fill-pointer 4)) =>  #(NIL NIL NIL NIL)
    (fill-pointer a) =>  4
    (dotimes (i (length a)) (setf (aref a i) (* i i))) =>  NIL
    a =>  #(0 1 4 9)
    (setf (fill-pointer a) 3) =>  3
    (fill-pointer a) =>  3
    a =>  #(0 1 4)
    (setf (fill-pointer a) 8) =>  8
    a =>  #(0 1 4 9 NIL NIL NIL NIL)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 vector 不是一个带有填充指针[fill pointer]的向量[vector]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-array, length

* 注意(Notes):

        这里没有会移除向量[vector]的填充指针[fill pointer]的操作符[operator]. 


### <span id="A-ROW-MAJOR-AREF">访问器 ROW-MAJOR-AREF</span>

* 语法(Syntax):

        row-major-aref array index => element

        (setf (row-major-aref array index) new-element)

* 参数和值(Arguments and Values):

        array---一个数组[array].
        index---对于数组 array 的一个有效数组行优先索引[valid array row-major index].
        element, new-element---一个对象[object].

* 描述(Description):

        将数组[array]看作是一个向量[vector], 它以行优先的顺序查看其元素[element], 并且返回给定索引 index 引用的向量[vector]的元素[element].

        row-major-aref 和 setf 一起使用是有效的.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        aref, array-row-major-index

* 注意(Notes):

        (row-major-aref array index) == 
          (aref (make-array (array-total-size array)
                            :displaced-to array
                            :element-type (array-element-type array))
                index)

        (aref array i1 i2 ...) == 
            (row-major-aref array (array-row-major-index array i1 i2))


### <span id="F-UPGRADED-ARRAY-ELEMENT-TYPE">函数 UPGRADED-ARRAY-ELEMENT-TYPE</span>

* 语法(Syntax):

        upgraded-array-element-type typespec &optional environment => upgraded-typespec

* 参数和值(Arguments and Values):

        typespec---一个类型指定符[type specifier].
        environment---一个环境[environment]对象[object]. 默认是 nil, 表示空词法环境[null lexical environment]和当前全局环境[global environment].
        upgraded-typespec---一个类型指定符[type specifier].

* 描述(Description):

        返回可以持有 typespec 所表示类型[type]的条目的最具体[specialized]数组[array]表示的元素类型[element type].

        这个 typespec 是 upgraded-typespec 的一个子类型[subtype] (并且可能类型等价[type equivalent]).

        如果 typespec 是 bit, 那么结果和 bit 是类型等价的[type equivalent]. 如果 typespec 是 base-char, 那么结果和 base-char 是类型等价的[type equivalent]. 如果 typespec 是 character, 那么结果和 character 是类型等价的[type equivalent].

        upgraded-array-element-type 的目的是去揭露一个实现如何执行这个提升[upgrade].

        这个环境 environment 被用于展开任何 typespec 中提及的衍生类型指定符[derived type specifier].

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        array-element-type, make-array

* 注意(Notes):

        除了存储分配的影响和正确处理这个可选的 environment 实参[argument]之外, upgraded-array-element-type 可以按如下定义:

        (defun upgraded-array-element-type (type &optional environment)
          (array-element-type (make-array 0 :element-type type)))


### <span id="CV-ARRAY-DIMENSION-LIMIT">常量 ARRAY-DIMENSION-LIMIT</span>

* 常量值(Constant Value):

        一个正的 fixnum, 它的准确大小是依赖于具体实现的[implementation-dependent], 但不小于 1024.

* 描述(Description):

        一个数组[array]的每个独立维度[dimension]的上边界.

* 示例(Examples): None.

* 参见(See Also):

        make-array

* 注意(Notes): None. 


### <span id="CV-ARRAY-RANK-LIMIT">常量 ARRAY-RANK-LIMIT</span>

* 常量值(Constant Value):

        一个正的 fixnum, 它的准确大小是依赖于具体实现的[implementation-dependent], 但是不小于 8.

* 描述(Description):

        一个数组[array]秩[rank]的上边界.

* 示例(Examples): None.

* 参见(See Also):

        make-array

* 注意(Notes): None. 

### <span id="CV-ARRAY-TOTAL-SIZE-LIMIT">常量 ARRAY-TOTAL-SIZE-LIMIT</span>

* 常量值(Constant Value):

        一个正的 fixnum, 准确大小是依赖于具体实现的[implementation-dependent], 但是不小于 1024.

* 描述(Description):

        一个数组[array]的数组总大小[array total size]的上限.

        具体实现[implementation]强加在数组总大小[array total size]的实际限制可能根据数组[array]的元素类型[element type]变化; 在这种情况中, 这个 array-total-size-limit 的值会是这些可能的限制中最小的.

* 示例(Examples): None.

* 参见(See Also):

        make-array, array-element-type

* 注意(Notes): None. 


### <span id="F-SIMPLE-VECTOR-P">函数 SIMPLE-VECTOR-P</span>

* 语法(Syntax):

        simple-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果 object 是 simple-vector 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (simple-vector-p (make-array 6)) =>  true
    (simple-vector-p "aaaaaa") =>  false
    (simple-vector-p (make-array 6 :fill-pointer t)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        simple-vector

* 注意(Notes):

        (simple-vector-p object) ==  (typep object 'simple-vector)


### <span id="A-SVREF">访问器 SVREF</span>

* 语法(Syntax):

        svref simple-vector index => element

        (setf (svref simple-vector index) new-element)

* 参数和值(Arguments and Values):

        simple-vector---一个简单向量[simple vector].
        index---simple-vector 的有效数组索引[valid array index].
        element, new-element---一个对象[object] (它的类型[type]是 simple-vector 的数组元素类型[array element type]的子类型[subtype]).

* 描述(Description):

        访问[access]由 index 指定的 simple-vector 中的元素[element].

* 示例(Examples):

    ```LISP
    (simple-vector-p (setq v (vector 1 2 'sirens))) =>  true
    (svref v 0) =>  1
    (svref v 2) =>  SIRENS
    (setf (svref v 1) 'newcomer) =>  NEWCOMER               
    v =>  #(1 NEWCOMER SIRENS)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        aref, sbit, schar, vector, 章节 3.2.1 (编译器术语)

* 注意(Notes):

        svref 和 aref 相同除了它要求第一个参数为一个简单向量[simple vector].

        (svref v i) ==  (aref (the simple-vector v) i)


### <span id="F-VECTOR">函数 VECTOR</span>

* 语法(Syntax):

        vector &rest objects => vector

* 参数和值(Arguments and Values):

        object---一个对象[object].
        vector---一个 (vector t *) 类型[type]的向量[vector].

* 描述(Description):

        创建一个新的[fresh]简单普通向量[simple general vector], 它的大小对应于对象 objects 的数量.

        这个向量[vector]被初始化去包含这些对象 objects.

* 示例(Examples):

    ```LISP
    (arrayp (setq v (vector 1 2 'sirens))) =>  true
    (vectorp v) =>  true
    (simple-vector-p v) =>  true         
    (length v) =>  3
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        make-array

* 注意(Notes):

        vector 与 list 类似.

        (vector a1 a2 ... an)
          ==  (make-array (list n) :element-type t
                                  :initial-contents 
                                    (list a1 a2 ... an))


### <span id="F-VECTOR-POP">函数 VECTOR-POP</span>

* 语法(Syntax):

        vector-pop vector => element

* 参数和值(Arguments and Values):

        vector---一个带有填充指针[fill pointer]的向量[vector].
        element---一个对象[object].

* 描述(Description):

        向量 vector 的填充指针[fill pointer]递减一, 并且检索向量 vector 中由这个新的填充指针[fill pointer]所指示的元素[element].

* 示例(Examples):

    ```LISP
    (vector-push (setq fable (list 'fable))
                  (setq fa (make-array 8
                                      :fill-pointer 2
                                      :initial-element 'sisyphus))) =>  2 
    (fill-pointer fa) =>  3 
    (eq (vector-pop fa) fable) =>  true
    (vector-pop fa) =>  SISYPHUS 
    (fill-pointer fa) =>  1 
    ```

* 副作用(Side Effects):

        填充指针[fill pointer]递减一.

* 受此影响(Affected By):

        这个填充指针[fill pointer]的值.

* 异常情况(Exceptional Situations):

        如果向量 vector 没有一个填充指针[fill pointer]那么应该发出一个 type-error 类型[type]的错误.

        如果填充指针[fill pointer]是零, vector-pop 会发出一个 error 类型[type]的错误.

* 参见(See Also):

        vector-push, vector-push-extend, fill-pointer

* 注意(Notes): None. 

### <span id="F-V-PUSH-V-PUSH-EXTEND">函数 VECTOR-PUSH, VECTOR-PUSH-EXTEND</span>

* 语法(Syntax):

        vector-push new-element vector => new-index-p

        vector-push-extend new-element vector &optional extension => new-index

* 参数和值(Arguments and Values):

        new-element---一个对象[object].
        vector---一个带有填充指针[fill pointer]的向量[vector].
        extension---一个正整数[integer]. 默认是依赖于具体实现的[implementation-dependent].
        new-index-p---对于 vector 的一个有效数组索引[valid array index], 或者 nil.
        new-index---对于 vector 的一个有效数组索引[valid array index].

* 描述(Description):

        vector-push 和 vector-push-extend 存储新元素 new-element 到向量 vector 中. vector-push 尝试去存储 new-element 到 vector 的由填充指针[fill pointer]所表示的元素中, 并且对填充指针[fill pointer]加一. 如果 (>= (fill-pointer vector) (array-dimension vector 0)), 不管是 vector 还是它的填充指针[fill pointer]都不会被影响. 否则, 就会发生存储和递增并且 vector-push 返回这个填充指针[fill pointer]之前的值, 它比离开这个向量时的值小 1.

        vector-push-extend 就像 vector-push 除了当填充指针[fill pointer]太大时, vector 使用 adjust-array 来扩展这样它就可以包含更多元素. 如果这个 vector 必须要被扩展, 那么 extension 就是要被添加给它的最小数量的元素.

        vector-push 和 vector-push-extend 返回这个 new-element 在 vector 中的索引. 如果 (>= (fill-pointer vector) (array-dimension vector 0)), vector-push 返回 nil.

* 示例(Examples):

    ```LISP
    (vector-push (setq fable (list 'fable))
                  (setq fa (make-array 8 
                                      :fill-pointer 2
                                      :initial-element 'first-one))) =>  2 
    (fill-pointer fa) =>  3 
    (eq (aref fa 2) fable) =>  true
    (vector-push-extend #\X
                        (setq aa 
                              (make-array 5
                                          :element-type 'character
                                          :adjustable t
                                          :fill-pointer 3))) =>  3 
    (fill-pointer aa) =>  4 
    (vector-push-extend #\Y aa 4) =>  4 
    (array-total-size aa) =>  at least 5 
    (vector-push-extend #\Z aa 4) =>  5 
    (array-total-size aa) =>  9 ;(or more)
    ```

* 受此影响(Affected By):

        这个填充指针[fill pointer]的值.

        向量 vector 如何被创建.

* 异常情况(Exceptional Situations):

        如果 vector-push-extend 尝试去扩展向量 vector 而 vector 事实上是不可调整的[actually adjustable], 那么应该发出一个 error 类型[type]的错误.

        如果向量 vector 没有一个填充指针[fill pointer]那么应该发出一个 error 类型[type]的错误.

* 参见(See Also):

        adjustable-array-p, fill-pointer, vector-pop

* 注意(Notes): None. 


### <span id="F-VECTORP">函数 VECTORP</span>

* 语法(Syntax):

        vectorp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 为 vector 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (vectorp "aaaaaa") =>  true
    (vectorp (make-array 6 :fill-pointer t)) =>  true
    (vectorp (make-array '(2 3 4))) =>  false
    (vectorp #*11) =>  true
    (vectorp #b11) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (vectorp object) ==  (typep object 'vector)


### <span id="A-BIT-SBIT">访问器 BIT, SBIT</span>

* 语法(Syntax):

        bit bit-array &rest subscripts => bit

        sbit bit-array &rest subscripts => bit

        (setf (bit bit-array &rest subscripts) new-bit)

        (setf (sbit bit-array &rest subscripts) new-bit)

* 参数和值(Arguments and Values):

        bit-array---对于 bit, 是一个位数组[bit array]; 对于 sbit, 一个简单位数组[simple bit array].
        subscripts---位数组 bit-array 的一个有效数组索引的列表[list].
        bit---一个比特[bit].

* 描述(Description):

        bit 和 sbit 访问[access]由 subscripts 指定的 bit-array 元素[element].
        
        当访问[access]元素[element]时, 这些函数[function]忽略填充指针[fill pointer].

* 示例(Examples):

    ```LISP
    (bit (setq ba (make-array 8 
                                :element-type 'bit 
                                :initial-element 1))
          3) =>  1
    (setf (bit ba 3) 0) =>  0
    (bit ba 3) =>  0
    (sbit ba 5) =>  1
    (setf (sbit ba 5) 1) =>  1
    (sbit ba 5) =>  1
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        aref, 章节 3.2.1 (编译器术语)

* 注意(Notes):

        bit 和 sbit 类似于 aref 除了它们要求数组[array]分别为一个位数组[bit array]和简单位数组[simple bit array].

        bit 和 sbit, 不像 char 和 schar, 允许第一个参数为一个任意秩[rank]的数组[array]. 


### <span id="F-BIT-ALL">函数 BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR</span>

* 语法(Syntax):

        bit-and bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-andc1 bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-andc2 bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-eqv bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-ior bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-nand bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-nor bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-orc1 bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-orc2 bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-xor bit-array1 bit-array2 &optional opt-arg => resulting-bit-array

        bit-not bit-array &optional opt-arg => resulting-bit-array

* 参数和值(Arguments and Values):

        bit-array, bit-array1, bit-array2---一个位数组[bit array].
        Opt-arg---一个位数组[bit array], 或 t, 或 nil. 默认是 nil.
        Bit-array, bit-array1, bit-array2, 和 opt-arg (如果是一个数组[array]) 必须全都是相同的秩[rank]和维数[dimension].
        resulting-bit-array---一个位数组[bit array].

* 描述(Description):

        这些函数在 bit-array1 和 bit-array2 上执行位逻辑操作并且返回一个秩[rank]和维数[dimension]匹配的数组[array], 如此以至于这个结果中的任何位都是有在这些参数的每一个的对应位上操作所产生的.

        在 bit-not 的情况下, 返回一个和 bit-array 秩[rank]和维数[dimension]匹配的数组[array], 这个数组包含了 bit-array 的一个所有位反转的拷贝.

        如果 opt-arg 是类型 (array bit) 那么这个结果的内容被破坏性地放置到 opt-arg 中. 如果 opt-arg 是符号 t, bit-array 或 bit-array1 会被这个结果替换; 如果 opt-arg 是 nil 或省略了, 那么就创建一个新的数组[array]来包含这个结果.

        下面这段指出有这些函数[function]中的每一个执行的逻辑操作.
                                                                                                       
    |函数                             |                        操作                                     |
    |---|---|
    |bit-and                          |                        and                                           |
    |bit-eqv                          |                        equivalence (exclusive nor)                   |
    |bit-not                          |                        complement                                    |
    |bit-ior                          |                        inclusive or                                  |
    |bit-xor                          |                        exclusive or                                  |
    |bit-nand                         |                        complement of bit-array1 and bit-array2       |
    |bit-nor                          |                        complement of bit-array1 or bit-array2        |
    |bit-andc1                        |                        and complement of bit-array1 with bit-array2  |
    |bit-andc2                        |                        and bit-array1 with complement of bit-array2  |
    |bit-orc1                         |                        or complement of bit-array1 with bit-array2   |
    |bit-orc2                         |                        or bit-array1 with complement of bit-array2   |
                                                                                                          
    Figure 15-4.  位数组上的位逻辑操作符 

* 示例(Examples):

    ```LISP
    (bit-and (setq ba #*11101010) #*01101011) =>  #*01101010
    (bit-and #*1100 #*1010) =>  #*1000      
    (bit-andc1 #*1100 #*1010) =>  #*0010
    (setq rba (bit-andc2 ba #*00110011 t)) =>  #*11001000
    (eq rba ba) =>  true
    (bit-not (setq ba #*11101010)) =>  #*00010101
    (setq rba (bit-not ba 
                        (setq tba (make-array 8 
                                              :element-type 'bit))))
    =>  #*00010101
    (equal rba tba) =>  true
    (bit-xor #*1100 #*1010) =>  #*0110
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        lognot, logand

* 注意(Notes): None. 


### <span id="F-BIT-VECTOR-P">函数 BIT-VECTOR-P</span>

* 语法(Syntax):

        bit-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 bit-vector 类型[type]的就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (bit-vector-p (make-array 6 
                              :element-type 'bit 
                              :fill-pointer t)) =>  true
    (bit-vector-p #*) =>  true
    (bit-vector-p (make-array 6)) =>  false
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        typep

* 注意(Notes):

        (bit-vector-p object) ==  (typep object 'bit-vector)


### <span id="F-SIMPLE-BIT-VECTOR-P">函数 SIMPLE-BIT-VECTOR-P</span>

* 语法(Syntax):

        simple-bit-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 simple-bit-vector 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (simple-bit-vector-p (make-array 6)) =>  false
    (simple-bit-vector-p #*) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        simple-vector-p

* 注意(Notes):

        (simple-bit-vector-p object) ==  (typep object 'simple-bit-vector)
