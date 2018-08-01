# 15 数组

> * 15.1 [数组的概念](#ArrayConcepts)
> * 15.2 [数组的字典](#TheArraysDictionary)

## 15.1 <span id="ArrayConcepts">数组的概念</span>

> * 15.1.1 [数组元素](#ArrayElements)
> * 15.1.2 [Specialized Arrays](#SpecializedArrays)

### 15.1.1 <span id="ArrayElements">数组元素</span>

一个数组包含了一个称为为元素的对象的集合, 它们可以通过一个直角坐标系统而被单独的引用.

> * 15.1.1.1 [数组索引](#ArrayIndices)
> * 15.1.1.2 [数组维度](#ArrayDimensions)
> * 15.1.1.3 [数组维数](#ArrayRank)

#### 15.1.1.1 <span id="ArrayIndices">数组索引</span>

一个数组元素可以通过一个(可能是空的)索引的序列被引用. 这个序列的长度必须等于这个数组的范围. 每一个索引必须是一个小于对应数组尺寸的非负的 fixnum. 数组索引是从 0 开始的. 


#### 15.1.1.2 <span id="ArrayDimensions">数组维度</span>

一个数组的坐标轴叫做维度.

每一个维度都是一个非负 fixnum; 如果一个数组的任意维度都是 0, 那么这个数组就没有元素. 允许一个维度是 0, 在这个情况下这个数组没有元素, 并且去访问一个元素的任何尝试都是一个错误. 然而, 这个数组的其他属性, 比如这些维度自身, 可能被使用.

##### 15.1.1.2.1 独立数组维度的实现限制

一个实现科恩嗯在数组的维度上强加一个限制, 但是在这个限制上这里有一个最小的需求. 见变量 array-dimension-limit. 


#### 15.1.1.3 <span id="ArrayRank">数组维数</span>

一个数组可以有任意数量的维度 (包括 zero). 这个维度的数量称为维数(rank).

如果一个数组的维数是 0 那么这个数组就被说成是没有维度的, 并且这个维度的结果是 1 (见 array-total-size); 一个零维数组因此只有一个单个元素.

> * 15.1.1.3.1 [向量](#Vectors)
> * 15.1.1.3.2 [多维数组](#MultiArrays)

##### 15.1.1.3.1 <span id="Vectors">向量</span>

一个维数为 1 的数组 (换句话说, 一个一维数组) 被称为一个向量(vector).

###### 15.1.1.3.1.1 填充指针

一个填充指针是一个不大于向量中元素总数的非负整数. 不是所有的向量都有填充指针. 见函数 make-array 和 adjust-array.

如果一个向量的一个元素有着大于等于 0 但是小于这个填充指针(如果有的话)的索引, 就说它是有效的. 对于一个没有填充指针的数组, 所有元素都被认为是有效的.

只有向量可以有填充指针; 多维数组没有. 一个多维数组 A multidimensional array that is displaced to a vector that has a fill pointer can be created. <!--TODO 待翻译-->

##### 15.1.1.3.2 <span id="">多维数组</span>

###### 15.1.1.3.2.1 多维数组的存储布局

多维数组以行优先的顺序存储它们的成分; 这也就是说, 一个多维数组内部被存储为一个一维数组, 其中多维度索引集有序地排列, 最后一个所有变化最快. 

###### 15.1.1.3.2.2 数组维数的实现限制

一个实现可能在一个数组的维数上强加一个限制, 但是在这个限制上这里有一个最小的需求. 见变量 array-rank-limit. 

### 15.1.2 <span id="SpecializedArrays">Specialized Arrays</span>

一个数组可以是一个普通数组, 意味着每个元素可以是任意对象, 或者它可能是一个特化的数组, 意味着每个元素都有一个约束的类型.

"一个数组被特化为 <<\type>>" 的说法有时被用于强调一个数组的元素类型. 即便当这个 <<\type>> 是 t 这个说法也是认可的, 尽管一个被特化为类型 t 的数组是一个普通数组, 而不是一个特化数组.

下面这段列出了一些可应用于数组创建, 访问, 和信息操作的已定义的名字.

    adjust-array           array-has-fill-pointer-p  make-array                   
    adjustable-array-p     array-in-bounds-p         svref                        
    aref                   array-rank                upgraded-array-element-type  
    array-dimension        array-rank-limit          upgraded-complex-part-type   
    array-dimension-limit  array-row-major-index     vector                       
    array-dimensions       array-total-size          vector-pop                   
    array-displacement     array-total-size-limit    vector-push                  
    array-element-type     fill-pointer              vector-push-extend           

    Figure 15-1. 普通目的的数组相关的已定义名字

> * 15.1.2.1 [Array Upgrading](#ArrayUpgrading)
> * 15.1.2.2 [Required Kinds of Specialized Arrays](#RKOSA)


#### 15.1.2.1 <span id="ArrayUpgrading">Array Upgrading</span>

类型 T1 的提升数组元素类型 T2 是 T1 的超类型, 并且在 T1 可以被用作对象创建或类型区分的时候可以用来替换 T1.

在一个数组创建期间, 需要的元素类型被称为表达数组元素类型. 这个表达数组元素类型的提升数组元素类型成为这个要被创建的数组的实际数组元素类型.

类型提升意味着在类型层次结构中向上移动. 一个类型总是为它的提升数组元素类型的子类型. 同样, 如果一个类型 Tx 是另一个类型 Ty 的子类型, 那么 Tx 的提升数组元素类型必须是 Ty 的提升数组元素类型的子类型. 两个互斥的类型可以被提升为相同类型.

The upgraded array element type T2 of a type T1 is a function only of T1 itself;<!--TODO 待翻译--> 这也就是说, 它独立于将要使用 T2 的数组的任何其他属性, 例如维数, 可调性, 填充指针, 或位移. 函数 upgraded-array-element-type 可以被符合规范的程序用来预测这个实现会怎样提升一个给定类型. 

#### 15.1.2.2 <span id="RKOSA">Required Kinds of Specialized Arrays</span>

元素被约束为 character 类型或 character 的子类型的向量被称为字符串. 字符串是 string 类型. 下一段中列出了和字符串相关的已存在的名字.

字符串是特化数组, 逻辑上可能包含在这一章中. 然而, 出于可读性的目的, 关于字符串的大部分信息不会出现在这个章节中; 见章节 16 (Strings).

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

元素被约束为 bit 类型的向量称为位向量. 位向量是 bit-vector 类型的. 下一段中列出了在位数组上操作的一些已定义的名字.

    bit        bit-ior   bit-orc2  
    bit-and    bit-nand  bit-xor   
    bit-andc1  bit-nor   sbit      
    bit-andc2  bit-not             
    bit-eqv    bit-orc1            

    Figure 15-3. 操作位数组的操作符

## 15.2 <span id="TheArraysDictionary">数组的字典</span>

> * [System Class ARRAY](#SC-ARRAY)
> * [Type SIMPLE-ARRAY](#T-SIMPLE-ARRAY)
> * [System Class VECTOR](#SC-VECTOR)
> * [Type SIMPLE-VECTOR](#T-SIMPLE-VECTOR)
> * [System Class BIT-VECTOR](#SC-BIT-VECTOR)
> * [Type SIMPLE-BIT-VECTOR](#T-SIMPLE-BIT-VECTOR)
> * [Function MAKE-ARRAY](#F-MAKE-ARRAY)
> * [Function ADJUST-ARRAY](#F-ADJUST-ARRAY)
> * [Function ADJUSTABLE-ARRAY-P](#F-ADJUSTABLE-ARRAY-P)
> * [Accessor AREF](#A-AREF)
> * [Function ARRAY-DIMENSION](#F-ARRAY-DIMENSION)
> * [Function ARRAY-DIMENSIONS](#F-ARRAY-DIMENSIONS)
> * [Function ARRAY-ELEMENT-TYPE](#F-ARRAY-ELEMENT-TYPE)
> * [Function ARRAY-HAS-FILL-POINTER-P](#F-ARRAY-HAS-FILL-POINTER-P)
> * [Function ARRAY-DISPLACEMENT](#F-ARRAY-DISPLACEMENT)
> * [Function ARRAY-IN-BOUNDS-P](#F-ARRAY-IN-BOUNDS-P)
> * [Function ARRAY-RANK](#F-ARRAY-RANK)
> * [Function ARRAY-ROW-MAJOR-INDEX](#F-ARRAY-ROW-MAJOR-INDEX)
> * [Function ARRAY-TOTAL-SIZE](#F-ARRAY-TOTAL-SIZE)
> * [Function ARRAYP](#F-ARRAYP)
> * [Accessor FILL-POINTER](#A-FILL-POINTER)
> * [Accessor ROW-MAJOR-AREF](#A-ROW-MAJOR-AREF)
> * [Function UPGRADED-ARRAY-ELEMENT-TYPE](#F-UPGRADED-ARRAY-ELEMENT-TYPE)
> * [Constant Variable ARRAY-DIMENSION-LIMIT](#CV-ARRAY-DIMENSION-LIMIT)
> * [Constant Variable ARRAY-RANK-LIMIT](#CV-ARRAY-RANK-LIMIT)
> * [Constant Variable ARRAY-TOTAL-SIZE-LIMIT](#CV-ARRAY-TOTAL-SIZE-LIMIT)
> * [Function SIMPLE-VECTOR-P](#F-SIMPLE-VECTOR-P)
> * [Accessor SVREF](#A-SVREF)
> * [Function VECTOR](#F-VECTOR)
> * [Function VECTOR-POP](#F-VECTOR-POP)
> * [Function VECTOR-PUSH, VECTOR-PUSH-EXTEND](#F-V-PUSH-V-PUSH-EXTEND)
> * [Function VECTORP](#F-VECTORP)
> * [Accessor BIT, SBIT](#A-BIT-SBIT)
> * [Function BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR](#F-BIT-ALL)
> * [Function BIT-VECTOR-P](#F-BIT-VECTOR-P)
> * [Function SIMPLE-BIT-VECTOR-P](#F-SIMPLE-BIT-VECTOR-P)


### <span id="">System Class ARRAY</span>

* 类优先级列表(Class Precedence List):

        array, t

* 描述(Description):

        一个数组包含了根据一个笛卡儿坐标系统排列的对象. 一个数组提供了一个从 fixnum 的集合 {i0,i1,...,ir-1} 到对应数组元素的映射, 其中 0 <=ij < dj, r 是这个数组的维数, 并且 dj 是这个数组的维度 j 的大小.

        当一个数组被创建时, 请求这个它的创建的程序可能声明所有元素为一个特定类型, 称为表达数组元素类型. 具体实现允许去提升这个类型来产生实际数组元素类型, 它是这个数组被实际指定的元素类型. 见函数 upgraded-array-element-type.

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        array [{element-type | *} [dimension-spec]]

        dimension-spec::= rank | * | ({dimension | *}*) 

* 复合类型指定符参数(Compound Type Specifier Arguments):

        dimension---一个有效数组大小.
        element-type---一个类型指定符.
        rank---一个非负 fixnum.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示元素类型, 维数, 和大小匹配给定 element-type, rank, 和 dimensions 的数组的集合. 具体的说:

        如果 element-type 是符号 *, 数组不被排除在它们的元素类型的基础上 arrays are not excluded on the basis of their element type.<!--TODO 待校对--> 否则, 只包括那些实际数组元素类型是 element-type 的提升结果的数组; 见章节 15.1.2.1 (Array Upgrading).

        如果这个 dimension-spec 是一个 rank, 这个集合只包括那些有着那个 rank 的数组. 如果这个 dimension-spec 一个 dimensions 的列表, 这个集合只包括那些有着由 dimensions 的长度给定的 rank 并且有着那个指明的 dimensions 的数组; 在这个情况中, * 匹配对应大小的任何值. 如果这个 dimension-spec 是符号 *, 这个集合不会被约束在维数或大小的基础上.

* 也见(See Also):

        *print-array*, aref, make-array, vector, 章节 2.4.8.12 (Sharpsign A), 章节 22.1.3.8 (Printing Other Arrays)

* 注意(Notes):

        注意类型 (array t) 是类型 (array *) 的一个适当的子类型. 这个原因是类型 (array t) 是持有任何对象的数组的集合 (这些元素是类型 t, 它包括所有对象). 另一方面, 类型 (array *) 是所有数组的集合, 包括例如只持有字符的数组. 类型 (array character) 不是类型 (array t) 的一个子类型; 这两个集合是互斥的因为类型 (array character) 不是所有持有字符的数组的集合, 而是一组专门用来保存精确字符并且没有其他对象的数组的集合. 

### <span id="">Type SIMPLE-ARRAY</span>

* 超类型(Supertypes):

        simple-array, array, t

* 描述(Description):

        一个没有被转移到另一个数组, 没有填充指针, 并且不是明显可调整的数组是类型 simple-array 的一个子类型. 简单数组的概念是允许实现使用特定的表示, 并允许用户声明某些值总是简单的数组.

        类型 simple-vector, simple-string, 和 simple-bit-vector 类型 simple-array 的互斥的子类型, 对于它们分别意味着 (simple-array t (*)), 所有 c 为 character 类型的子类型的 (simple-array c (*)) 的并集, 以及 (simple-array bit (*)).

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        simple-array [{element-type | *} [dimension-spec]]

        dimension-spec::= rank | * | ({dimension | *}*) 

* 复合类型指定符参数(Compound Type Specifier Arguments):

        dimension---一个有效数组大小.
        element-type---一个类型指定符.
        rank---一个非负 fixnum.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个复合类型指定符被准确地当作 array 类型会被当作的对应复合类型指定符, 除了那个被进一步约束为只包含简单数组的集合.

* 注意(Notes):

        没有实际存储的数组, 带有填充指针的向量, 或实际可调整的数组是否为简单数组是依赖于具体实现的.

        (simple-array *) 不管元素类型引用了所有简单数组, (simple-array type-specifier) 只引用那些可以通过给定 type-specifier 作为 make-array 的 :element-type 参数得到的简单数组. 


### <span id="">System Class VECTOR</span>

* 类优先级列表(Class Precedence List):

vector, array, sequence, t

* 描述(Description):

Any one-dimensional array is a vector.

The type vector is a subtype of type array; for all types x, (vector x) is the same as (array x (*)).

The type (vector t), the type string, and the type bit-vector are disjoint subtypes of type vector.

* 复合类型指定符类别(Compound Type Specifier Kind):

Specializing.

* 复合类型指定符语法(Compound Type Specifier Syntax):

vector [{element-type | *} [{size | *}]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

size---一个非负 fixnum.

element-type---一个类型指定符.

* 复合类型指定符描述(Compound Type Specifier Description):

This denotes the set of specialized vectors whose element type and dimension match the specified values. Specifically:

If element-type is the symbol *, vectors are not excluded on the basis of their element type. Otherwise, only those vectors are included whose actual array element type is the result of upgrading element-type; see Section 15.1.2.1 (Array Upgrading).

If a size is specified, the set includes only those vectors whose only dimension is size. If the symbol * is specified instead of a size, the set is not restricted on the basis of dimension.

* 也见(See Also):

Section 15.1.2.2 (Required Kinds of Specialized Arrays), Section 2.4.8.3 (Sharpsign Left-Parenthesis), Section 22.1.3.7 (Printing Other Vectors), Section 2.4.8.12 (Sharpsign A)

* 注意(Notes):

The type (vector e s) is equivalent to the type (array e (s)).

The type (vector bit) has the name bit-vector.

The union of all types (vector C), where C is any subtype of character, has the name string.

(vector *) refers to all vectors regardless of element type, (vector type-specifier) refers only to those vectors that can result from giving type-specifier as the :element-type argument to make-array. 


### <span id="">Type SIMPLE-VECTOR</span>

* 超类型(Supertypes):

simple-vector, vector, simple-array, array, sequence, t

* 描述(Description):

The type of a vector that is not displaced to another array, has no fill pointer, is not expressly adjustable and is able to hold elements of any type is a subtype of type simple-vector.

The type simple-vector is a subtype of type vector, and is a subtype of type (vector t).

* 复合类型指定符类别(Compound Type Specifier Kind):

Specializing.

* 复合类型指定符语法(Compound Type Specifier Syntax):

simple-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

size---a non-negative fixnum, or the symbol *. The default is the symbol *.

* 复合类型指定符描述(Compound Type Specifier Description):

This is the same as (simple-array t (size)). 


### <span id="">System Class BIT-VECTOR</span>

* 类优先级列表(Class Precedence List):

bit-vector, vector, array, sequence, t

* 描述(Description):

A bit vector is a vector the element type of which is bit.

The type bit-vector is a subtype of type vector, for bit-vector means (vector bit).

* 复合类型指定符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型指定符语法(Compound Type Specifier Syntax):

bit-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

size---a non-negative fixnum, or the symbol *.

* 复合类型指定符描述(Compound Type Specifier Description):

This denotes the same type as the type (array bit (size)); that is, the set of bit vectors of size size.

* 也见(See Also):

Section 2.4.8.4 (Sharpsign Asterisk), Section 22.1.3.6 (Printing Bit Vectors), Section 15.1.2.2 (Required Kinds of Specialized Arrays) 

### <span id="">Type SIMPLE-BIT-VECTOR</span>

* 超类型(Supertypes):

simple-bit-vector, bit-vector, vector, simple-array, array, sequence, t

* 描述(Description):

The type of a bit vector that is not displaced to another array, has no fill pointer, and is not expressly adjustable is a subtype of type simple-bit-vector.

* 复合类型指定符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型指定符语法(Compound Type Specifier Syntax):

simple-bit-vector [size]

* 复合类型指定符参数(Compound Type Specifier Arguments):

size---a non-negative fixnum, or the symbol *. The default is the symbol *.

* 复合类型指定符描述(Compound Type Specifier Description):

This denotes the same type as the type (simple-array bit (size)); that is, the set of simple bit vectors of size size. 

### <span id="">Function MAKE-ARRAY</span>

* 语法(Syntax):

make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset

=> new-array

* 参数和值(Arguments and Values):

dimensions---a designator for a list of valid array dimensions.

element-type---一个类型指定符. The default is t.

initial-element---一个对象.

initial-contents---一个对象.

adjustable---一个广义 boolean. The default is nil.

fill-pointer---a valid fill pointer for the array to be created, or t or nil. The default is nil.

displaced-to---an array or nil. The default is nil. This option must not be supplied if either initial-element or initial-contents is supplied.

displaced-index-offset---a valid array row-major index for displaced-to. The default is 0. This option must not be supplied unless a non-nil displaced-to is supplied.

new-array---an array.

* 描述(Description):

Creates and returns an array constructed of the most specialized type that can accommodate elements of type given by element-type. If dimensions is nil then a zero-dimensional array is created.

Dimensions represents the dimensionality of the new array.

element-type indicates the type of the elements intended to be stored in the new-array. The new-array can actually store any objects of the type which results from upgrading element-type; see Section 15.1.2.1 (Array Upgrading).

If initial-element is supplied, it is used to initialize each element of new-array. If initial-element is supplied, it must be of the type given by element-type. initial-element cannot be supplied if either the :initial-contents option is supplied or displaced-to is non-nil. If initial-element is not supplied, the consequences of later reading an uninitialized element of new-array are undefined unless either initial-contents is supplied or displaced-to is non-nil.

initial-contents is used to initialize the contents of array. For example:

 (make-array '(4 2 3) :initial-contents
             '(((a b c) (1 2 3))
              ((d e f) (3 1 2))
              ((g h i) (2 3 1))
              ((j k l) (0 0 0))))

initial-contents is composed of a nested structure of sequences. The numbers of levels in the structure must equal the rank of array. Each leaf of the nested structure must be of the type given by element-type. If array is zero-dimensional, then initial-contents specifies the single element. Otherwise, initial-contents must be a sequence whose length is equal to the first dimension; each element must be a nested structure for an array whose dimensions are the remaining dimensions, and so on. Initial-contents cannot be supplied if either initial-element is supplied or displaced-to is non-nil. If initial-contents is not supplied, the consequences of later reading an uninitialized element of new-array are undefined unless either initial-element is supplied or displaced-to is non-nil.

If adjustable is non-nil, the array is expressly adjustable (and so actually adjustable); otherwise, the array is not expressly adjustable (and it is implementation-dependent whether the array is actually adjustable).

If fill-pointer is non-nil, the array must be one-dimensional; that is, the array must be a vector. If fill-pointer is t, the length of the vector is used to initialize the fill pointer. If fill-pointer is an integer, it becomes the initial fill pointer for the vector.

If displaced-to is non-nil, make-array will create a displaced array and displaced-to is the target of that displaced array. In that case, the consequences are undefined if the actual array element type of displaced-to is not type equivalent to the actual array element type of the array being created. If displaced-to is nil, the array is not a displaced array.

The displaced-index-offset is made to be the index offset of the array. When an array A is given as the :displaced-to argument to make-array when creating array B, then array B is said to be displaced to array A. The total number of elements in an array, called the total size of the array, is calculated as the product of all the dimensions. It is required that the total size of A be no smaller than the sum of the total size of B plus the offset n supplied by the displaced-index-offset. The effect of displacing is that array B does not have any elements of its own, but instead maps accesses to itself into accesses to array A. The mapping treats both arrays as if they were one-dimensional by taking the elements in row-major order, and then maps an access to element k of array B to an access to element k+n of array A.

If make-array is called with adjustable, fill-pointer, and displaced-to each nil, then the result is a simple array. If make-array is called with one or more of adjustable, fill-pointer, or displaced-to being true, whether the resulting array is a simple array is implementation-dependent.

When an array A is given as the :displaced-to argument to make-array when creating array B, then array B is said to be displaced to array A. The total number of elements in an array, called the total size of the array, is calculated as the product of all the dimensions. The consequences are unspecified if the total size of A is smaller than the sum of the total size of B plus the offset n supplied by the displaced-index-offset. The effect of displacing is that array B does not have any elements of its own, but instead maps accesses to itself into accesses to array A. The mapping treats both arrays as if they were one-dimensional by taking the elements in row-major order, and then maps an access to element k of array B to an access to element k+n of array A.

* 示例(Examples):


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

The following is an example of making a displaced array.

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

The last example depends on the fact that arrays are, in effect, stored in row-major order.

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

adjustable-array-p, aref, arrayp, array-element-type, array-rank-limit, array-dimension-limit, fill-pointer, upgraded-array-element-type

* 注意(Notes):

There is no specified way to create an array for which adjustable-array-p definitely returns false. There is no specified way to create an array that is not a simple array. 


### <span id="">Function ADJUST-ARRAY</span>

* 语法(Syntax):

adjust-array array new-dimensions &key element-type initial-element initial-contents fill-pointer displaced-to displaced-index-offset

=> adjusted-array

* 参数和值(Arguments and Values):

array---an array.

new-dimensions---a valid array dimension or a list of valid array dimensions.

element-type---一个类型指定符.

initial-element---一个对象. Initial-element must not be supplied if either initial-contents or displaced-to is supplied.

initial-contents---一个对象. If array has rank greater than zero, then initial-contents is composed of nested sequences, the depth of which must equal the rank of array. Otherwise, array is zero-dimensional and initial-contents supplies the single element. initial-contents must not be supplied if either initial-element or displaced-to is given.

fill-pointer---a valid fill pointer for the array to be created, or t, or nil. The default is nil.

displaced-to---an array or nil. initial-elements and initial-contents must not be supplied if displaced-to is supplied.

displaced-index-offset---an object of type (fixnum 0 n) where n is (array-total-size displaced-to). displaced-index-offset may be supplied only if displaced-to is supplied.

adjusted-array---an array.

* 描述(Description):

adjust-array changes the dimensions or elements of array. The result is an array of the same type and rank as array, that is either the modified array, or a newly created array to which array can be displaced, and that has the given new-dimensions.

New-dimensions specify the size of each dimension of array.

Element-type specifies the type of the elements of the resulting array. If element-type is supplied, the consequences are unspecified if the upgraded array element type of element-type is not the same as the actual array element type of array.

If initial-contents is supplied, it is treated as for make-array. In this case none of the original contents of array appears in the resulting array.

If fill-pointer is an integer, it becomes the fill pointer for the resulting array. If fill-pointer is the symbol t, it indicates that the size of the resulting array should be used as the fill pointer. If fill-pointer is nil, it indicates that the fill pointer should be left as it is.

If displaced-to non-nil, a displaced array is created. The resulting array shares its contents with the array given by displaced-to. The resulting array cannot contain more elements than the array it is displaced to. If displaced-to is not supplied or nil, the resulting array is not a displaced array. If array A is created displaced to array B and subsequently array B is given to adjust-array, array A will still be displaced to array B. Although array might be a displaced array, the resulting array is not a displaced array unless displaced-to is supplied and not nil. The interaction between adjust-array and displaced arrays is as follows given three arrays, A, B, and C:

A is not displaced before or after the call

     (adjust-array A ...)

    The dimensions of A are altered, and the contents rearranged as appropriate. Additional elements of A are taken from initial-element. The use of initial-contents causes all old contents to be discarded.

A is not displaced before, but is displaced to C after the call

     (adjust-array A ... :displaced-to C)

    None of the original contents of A appears in A afterwards; A now contains the contents of C, without any rearrangement of C.

A is displaced to B before the call, and is displaced to C after the call

     (adjust-array A ... :displaced-to B)
     (adjust-array A ... :displaced-to C)

    B and C might be the same. The contents of B do not appear in A afterward unless such contents also happen to be in C If displaced-index-offset is not supplied in the adjust-array call, it defaults to zero; the old offset into B is not retained.

A is displaced to B before the call, but not displaced afterward.

     (adjust-array A ... :displaced-to B)
     (adjust-array A ... :displaced-to nil)

    A gets a new ``data region,'' and contents of B are copied into it as appropriate to maintain the existing old contents; additional elements of A are taken from initial-element if supplied. However, the use of initial-contents causes all old contents to be discarded.

If displaced-index-offset is supplied, it specifies the offset of the resulting array from the beginning of the array that it is displaced to. If displaced-index-offset is not supplied, the offset is 0. The size of the resulting array plus the offset value cannot exceed the size of the array that it is displaced to.

If only new-dimensions and an initial-element argument are supplied, those elements of array that are still in bounds appear in the resulting array. The elements of the resulting array that are not in the bounds of array are initialized to initial-element; if initial-element is not provided, the consequences of later reading any such new element of new-array before it has been initialized are undefined.

If initial-contents or displaced-to is supplied, then none of the original contents of array appears in the new array.

The consequences are unspecified if array is adjusted to a size smaller than its fill pointer without supplying the fill-pointer argument so that its fill-pointer is properly adjusted in the process.

If A is displaced to B, the consequences are unspecified if B is adjusted in such a way that it no longer has enough elements to satisfy A.

If adjust-array is applied to an array that is actually adjustable, the array returned is identical to array. If the array returned by adjust-array is distinct from array, then the argument array is unchanged.

Note that if an array A is displaced to another array B, and B is displaced to another array C, and B is altered by adjust-array, A must now refer to the adjust contents of B. This means that an implementation cannot collapse the chain to make A refer to C directly and forget that the chain of reference passes through B. However, caching techniques are permitted as long as they preserve the semantics specified here.

* 示例(Examples):

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

Suppose that the 4-by-4 array in m looks like this:

#2A(( alpha     beta      gamma     delta )
    ( epsilon   zeta      eta       theta )
    ( iota      kappa     lambda    mu    )
    ( nu        xi        omicron   pi    ))

Then the result of

 (adjust-array m '(3 5) :initial-element 'baz)

is a 3-by-5 array with contents

#2A(( alpha     beta      gamma     delta     baz )
    ( epsilon   zeta      eta       theta     baz )
    ( iota      kappa     lambda    mu        baz ))

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

An error of type error is signaled if fill-pointer is supplied and non-nil but array has no fill pointer.

* 也见(See Also):

adjustable-array-p, make-array, array-dimension-limit, array-total-size-limit, array

* 注意(Notes): None. 


### <span id="">Function ADJUSTABLE-ARRAY-P</span>

* 语法(Syntax):

adjustable-array-p array => generalized-boolean

* 参数和值(Arguments and Values):

array---an array.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if and only if adjust-array could return a value which is identical to array when given that array as its first argument.

* 示例(Examples):

 (adjustable-array-p 
   (make-array 5
               :element-type 'character 
               :adjustable t 
               :fill-pointer 3)) =>  true
 (adjustable-array-p (make-array 4)) =>  implementation-dependent

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

adjust-array, make-array

* 注意(Notes): None. 


### <span id="">Accessor AREF</span>

* 语法(Syntax):

aref array &rest subscripts => element

(setf (aref array &rest subscripts) new-element)

* 参数和值(Arguments and Values):

array---an array.

subscripts---a list of valid array indices for the array.

element, new-element---一个对象.

* 描述(Description):

Accesses the array element specified by the subscripts. If no subscripts are supplied and array is zero rank, aref accesses the sole element of array.

aref ignores fill pointers. It is permissible to use aref to access any array element, whether active or not.

* 示例(Examples):

If the variable foo names a 3-by-5 array, then the first index could be 0, 1, or 2, and then second index could be 0, 1, 2, 3, or 4. The array elements can be referred to by using the function aref; for example, (aref foo 2 1) refers to element (2, 1) of the array.

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

bit, char, elt, row-major-aref, svref, Section 3.2.1 (Compiler Terminology)

* 注意(Notes): None. 


### <span id="">Function ARRAY-DIMENSION</span>

* 语法(Syntax):

array-dimension array axis-number => dimension

* 参数和值(Arguments and Values):

array---an array.

axis-number---an integer greater than or equal to zero and less than the rank of the array.

dimension---a non-negative integer.

* 描述(Description):

array-dimension returns the axis-number dimension[1] of array. (Any fill pointer is ignored.)

* 示例(Examples):

 (array-dimension (make-array 4) 0) =>  4
 (array-dimension (make-array '(2 3)) 1) =>  3

* 受此影响(Affected By):

None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

array-dimensions, length

* 注意(Notes):

 (array-dimension array n) ==  (nth n (array-dimensions array))

### <span id="">Function ARRAY-DIMENSIONS</span>

* 语法(Syntax):

array-dimensions array => dimensions

* 参数和值(Arguments and Values):

array---an array.

dimensions---a list of integers.

* 描述(Description):

Returns a list of the dimensions of array. (If array is a vector with a fill pointer, that fill pointer is ignored.)

* 示例(Examples):

 (array-dimensions (make-array 4)) =>  (4)
 (array-dimensions (make-array '(2 3))) =>  (2 3)
 (array-dimensions (make-array 4 :fill-pointer 2)) =>  (4)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

array-dimension

* 注意(Notes): None. 

### <span id="">Function ARRAY-ELEMENT-TYPE</span>

* 语法(Syntax):

array-element-type array => typespec

* 参数和值(Arguments and Values):

array---an array.

typespec---一个类型指定符.

* 描述(Description):

Returns a type specifier which represents the actual array element type of the array, which is the set of objects that such an array can hold. (Because of array upgrading, this type specifier can in some cases denote a supertype of the expressed array element type of the array.)

* 示例(Examples):

 (array-element-type (make-array 4)) =>  T
 (array-element-type (make-array 12 :element-type '(unsigned-byte 8))) 
=>  implementation-dependent
 (array-element-type (make-array 12 :element-type '(unsigned-byte 5)))
=>  implementation-dependent

 (array-element-type (make-array 5 :element-type '(mod 5)))

could be (mod 5), (mod 8), fixnum, t, or any other type of which (mod 5) is a subtype.

* 受此影响(Affected By):

The implementation.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

array, make-array, subtypep, upgraded-array-element-type

* 注意(Notes): None. 


### <span id="">Function ARRAY-HAS-FILL-POINTER-P</span>

* 语法(Syntax):

array-has-fill-pointer-p array => generalized-boolean

* 参数和值(Arguments and Values):

array---an array.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if array has a fill pointer; otherwise returns false.

* 示例(Examples):

 (array-has-fill-pointer-p (make-array 4)) =>  implementation-dependent
 (array-has-fill-pointer-p (make-array '(2 3))) =>  false
 (array-has-fill-pointer-p
   (make-array 8 
               :fill-pointer 2 
               :initial-element 'filler)) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

make-array, fill-pointer

* 注意(Notes):

Since arrays of rank other than one cannot have a fill pointer, array-has-fill-pointer-p always returns nil when its argument is such an array. 


### <span id="">Function ARRAY-DISPLACEMENT</span>

* 语法(Syntax):

array-displacement array => displaced-to, displaced-index-offset

* 参数和值(Arguments and Values):

array---an array.

displaced-to---an array or nil.

displaced-index-offset---一个非负 fixnum.

* 描述(Description):

If the array is a displaced array, returns the values of the :displaced-to and :displaced-index-offset options for the array (see the functions make-array and adjust-array). If the array is not a displaced array, nil and 0 are returned.

If array-displacement is called on an array for which a non-nil object was provided as the :displaced-to argument to make-array or adjust-array, it must return that object as its first value. It is implementation-dependent whether array-displacement returns a non-nil primary value for any other array.

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if array is not an array.

* 也见(See Also):

make-array

* 注意(Notes): None. 

### <span id="">Function ARRAY-IN-BOUNDS-P</span>

* 语法(Syntax):

array-in-bounds-p array &rest subscripts => generalized-boolean

* 参数和值(Arguments and Values):

array---an array.

subscripts---a list of integers of length equal to the rank of the array.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if the subscripts are all in bounds for array; otherwise returns false. (If array is a vector with a fill pointer, that fill pointer is ignored.)

* 示例(Examples):

 (setq a (make-array '(7 11) :element-type 'string-char))
 (array-in-bounds-p a 0  0) =>  true
 (array-in-bounds-p a 6 10) =>  true
 (array-in-bounds-p a 0 -1) =>  false
 (array-in-bounds-p a 0 11) =>  false
 (array-in-bounds-p a 7  0) =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

array-dimensions

* 注意(Notes):

 (array-in-bounds-p array subscripts)   
 ==  (and (not (some #'minusp (list subscripts)))


### <span id="">Function ARRAY-RANK</span>

* 语法(Syntax):

array-rank array => rank

* 参数和值(Arguments and Values):

array---an array.

rank---a non-negative integer.

* 描述(Description):

Returns the number of dimensions of array.

* 示例(Examples):

 (array-rank (make-array '())) =>  0
 (array-rank (make-array 4)) =>  1
 (array-rank (make-array '(4))) =>  1
 (array-rank (make-array '(2 3))) =>  2

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

array-rank-limit, make-array

* 注意(Notes): None. 


### <span id="">Function ARRAY-ROW-MAJOR-INDEX</span>

* 语法(Syntax):

array-row-major-index array &rest subscripts => index

* 参数和值(Arguments and Values):

array---an array.

subscripts---a list of valid array indices for the array.

index---a valid array row-major index for the array.

* 描述(Description):

Computes the position according to the row-major ordering of array for the element that is specified by subscripts, and returns the offset of the element in the computed position from the beginning of array.

For a one-dimensional array, the result of array-row-major-index equals subscript.

array-row-major-index ignores fill pointers.

* 示例(Examples):

 (setq a (make-array '(4 7) :element-type '(unsigned-byte 8)))
 (array-row-major-index a 1 2) =>  9
 (array-row-major-index 
    (make-array '(2 3 4) 
                :element-type '(unsigned-byte 8)
                :displaced-to a
                :displaced-index-offset 4)
    0 2 1) =>  9

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

A possible definition of array-row-major-index, with no error-checking, is

 (defun array-row-major-index (a &rest subscripts)
   (apply #'+ (maplist #'(lambda (x y)
                            (* (car x) (apply #'* (cdr y))))
                       subscripts
                       (array-dimensions a))))



### <span id="">Function ARRAY-TOTAL-SIZE</span>

* 语法(Syntax):

array-total-size array => size

* 参数和值(Arguments and Values):

array---an array.

size---a non-negative integer.

* 描述(Description):

Returns the array total size of the array.

* 示例(Examples):

 (array-total-size (make-array 4)) =>  4
 (array-total-size (make-array 4 :fill-pointer 2)) =>  4
 (array-total-size (make-array 0)) =>  0
 (array-total-size (make-array '(4 2))) =>  8
 (array-total-size (make-array '(4 0))) =>  0
 (array-total-size (make-array '())) =>  1

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if its argument is not an array.

* 也见(See Also):

make-array, array-dimensions

* 注意(Notes):

If the array is a vector with a fill pointer, the fill pointer is ignored when calculating the array total size.

Since the product of no arguments is one, the array total size of a zero-dimensional array is one.

 (array-total-size x)
    ==  (apply #'* (array-dimensions x))
    ==  (reduce #'* (array-dimensions x))


### <span id="">Function ARRAYP</span>

* 语法(Syntax):

arrayp object => generalized-boolean

* 参数和值(Arguments and Values):

object---一个对象.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if object is of type array; otherwise, returns false.

* 示例(Examples):

 (arrayp (make-array '(2 3 4) :adjustable t)) =>  true
 (arrayp (make-array 6)) =>  true
 (arrayp #*1011) =>  true
 (arrayp "hi") =>  true
 (arrayp 'hi) =>  false
 (arrayp 12) =>  false

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

typep

* 注意(Notes):

 (arrayp object) ==  (typep object 'array)


### <span id="">Accessor FILL-POINTER</span>

* 语法(Syntax):

fill-pointer vector => fill-pointer

(setf (fill-pointer vector) new-fill-pointer)

* 参数和值(Arguments and Values):

vector---a vector with a fill pointer.

fill-pointer, new-fill-pointer---a valid fill pointer for the vector.

* 描述(Description):

Accesses the fill pointer of vector.

* 示例(Examples):

 (setq a (make-array 8 :fill-pointer 4)) =>  #(NIL NIL NIL NIL)
 (fill-pointer a) =>  4
 (dotimes (i (length a)) (setf (aref a i) (* i i))) =>  NIL
 a =>  #(0 1 4 9)
 (setf (fill-pointer a) 3) =>  3
 (fill-pointer a) =>  3
 a =>  #(0 1 4)
 (setf (fill-pointer a) 8) =>  8
 a =>  #(0 1 4 9 NIL NIL NIL NIL)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if vector is not a vector with a fill pointer.

* 也见(See Also):

make-array, length

* 注意(Notes):

There is no operator that will remove a vector's fill pointer. 

### <span id="">Accessor ROW-MAJOR-AREF</span>

* 语法(Syntax):

row-major-aref array index => element

(setf (row-major-aref array index) new-element)

* 参数和值(Arguments and Values):

array---an array.

index---a valid array row-major index for the array.

element, new-element---一个对象.

* 描述(Description):

Considers array as a vector by viewing its elements in row-major order, and returns the element of that vector which is referred to by the given index.

row-major-aref is valid for use with setf.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

aref, array-row-major-index

* 注意(Notes):

 (row-major-aref array index) == 
   (aref (make-array (array-total-size array)
                     :displaced-to array
                     :element-type (array-element-type array))
         index)

 (aref array i1 i2 ...) == 
     (row-major-aref array (array-row-major-index array i1 i2))


### <span id="">Function UPGRADED-ARRAY-ELEMENT-TYPE</span>

* 语法(Syntax):

upgraded-array-element-type typespec &optional environment => upgraded-typespec

* 参数和值(Arguments and Values):

typespec---一个类型指定符.

environment---an environment object. The default is nil, denoting the null lexical environment and the current global environment.

upgraded-typespec---一个类型指定符.

* 描述(Description):

Returns the element type of the most specialized array representation capable of holding items of the type denoted by typespec.

The typespec is a subtype of (and possibly type equivalent to) the upgraded-typespec.

If typespec is bit, the result is type equivalent to bit. If typespec is base-char, the result is type equivalent to base-char. If typespec is character, the result is type equivalent to character.

The purpose of upgraded-array-element-type is to reveal how an implementation does its upgrading.

The environment is used to expand any derived type specifiers that are mentioned in the typespec.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

array-element-type, make-array

* 注意(Notes):

Except for storage allocation consequences and dealing correctly with the optional environment argument, upgraded-array-element-type could be defined as:

 (defun upgraded-array-element-type (type &optional environment)
   (array-element-type (make-array 0 :element-type type)))


### <span id="">Constant Variable ARRAY-DIMENSION-LIMIT</span>

Constant Value:

A positive fixnum, the exact magnitude of which is implementation-dependent, but which is not less than 1024.

* 描述(Description):

The upper exclusive bound on each individual dimension of an array.

* 示例(Examples): None.

* 也见(See Also):

make-array

* 注意(Notes): None. 

### <span id="">Constant Variable ARRAY-RANK-LIMIT</span>

Constant Value:

A positive fixnum, the exact magnitude of which is implementation-dependent, but which is not less than 8.

* 描述(Description):

The upper exclusive bound on the rank of an array.

* 示例(Examples): None.

* 也见(See Also):

make-array

* 注意(Notes): None. 

### <span id="">Constant Variable ARRAY-TOTAL-SIZE-LIMIT</span>

Constant Value:

A positive fixnum, the exact magnitude of which is implementation-dependent, but which is not less than 1024.

* 描述(Description):

The upper exclusive bound on the array total size of an array.

The actual limit on the array total size imposed by the implementation might vary according the element type of the array; in this case, the value of array-total-size-limit will be the smallest of these possible limits.

* 示例(Examples): None.

* 也见(See Also):

make-array, array-element-type

* 注意(Notes): None. 


### <span id="">Function SIMPLE-VECTOR-P</span>

* 语法(Syntax):

simple-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

object---一个对象.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if object is of type simple-vector; otherwise, returns false..

* 示例(Examples):

 (simple-vector-p (make-array 6)) =>  true
 (simple-vector-p "aaaaaa") =>  false
 (simple-vector-p (make-array 6 :fill-pointer t)) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

simple-vector

* 注意(Notes):

 (simple-vector-p object) ==  (typep object 'simple-vector)


### <span id="">Accessor SVREF</span>

* 语法(Syntax):

svref simple-vector index => element

(setf (svref simple-vector index) new-element)

* 参数和值(Arguments and Values):

simple-vector---a simple vector.

index---a valid array index for the simple-vector.

element, new-element---an object (whose type is a subtype of the array element type of the simple-vector).

* 描述(Description):

Accesses the element of simple-vector specified by index.

* 示例(Examples):

 (simple-vector-p (setq v (vector 1 2 'sirens))) =>  true
 (svref v 0) =>  1
 (svref v 2) =>  SIRENS
 (setf (svref v 1) 'newcomer) =>  NEWCOMER               
 v =>  #(1 NEWCOMER SIRENS)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

aref, sbit, schar, vector, Section 3.2.1 (Compiler Terminology)

* 注意(Notes):

svref is identical to aref except that it requires its first argument to be a simple vector.

 (svref v i) ==  (aref (the simple-vector v) i)


### <span id="">Function VECTOR</span>

* 语法(Syntax):

vector &rest objects => vector

* 参数和值(Arguments and Values):

object---一个对象.

vector---a vector of type (vector t *).

* 描述(Description):

Creates a fresh simple general vector whose size corresponds to the number of objects.

The vector is initialized to contain the objects.

* 示例(Examples):

 (arrayp (setq v (vector 1 2 'sirens))) =>  true
 (vectorp v) =>  true
 (simple-vector-p v) =>  true         
 (length v) =>  3

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-array

* 注意(Notes):

vector is analogous to list.

 (vector a1 a2 ... an)
  ==  (make-array (list n) :element-type t
                          :initial-contents 
                            (list a1 a2 ... an))



### <span id="">Function VECTOR-POP</span>

* 语法(Syntax):

vector-pop vector => element

* 参数和值(Arguments and Values):

vector---a vector with a fill pointer.

element---一个对象.

* 描述(Description):

Decreases the fill pointer of vector by one, and retrieves the element of vector that is designated by the new fill pointer.

* 示例(Examples):

 (vector-push (setq fable (list 'fable))
              (setq fa (make-array 8
                                   :fill-pointer 2
                                   :initial-element 'sisyphus))) =>  2 
 (fill-pointer fa) =>  3 
 (eq (vector-pop fa) fable) =>  true
 (vector-pop fa) =>  SISYPHUS 
 (fill-pointer fa) =>  1 

* 副作用(Side Effects):

The fill pointer is decreased by one.

* 受此影响(Affected By):

The value of the fill pointer.

* 异常情况(Exceptional Situations):

An error of type type-error is signaled if vector does not have a fill pointer.

If the fill pointer is zero, vector-pop signals an error of type error.

* 也见(See Also):

vector-push, vector-push-extend, fill-pointer

* 注意(Notes): None. 

### <span id="">Function VECTOR-PUSH, VECTOR-PUSH-EXTEND</span>

* 语法(Syntax):

vector-push new-element vector => new-index-p

vector-push-extend new-element vector &optional extension => new-index

* 参数和值(Arguments and Values):

new-element---一个对象.

vector---a vector with a fill pointer.

extension---a positive integer. The default is implementation-dependent.

new-index-p---a valid array index for vector, or nil.

new-index---a valid array index for vector.

* 描述(Description):

vector-push and vector-push-extend store new-element in vector. vector-push attempts to store new-element in the element of vector designated by the fill pointer, and to increase the fill pointer by one. If the (>= (fill-pointer vector) (array-dimension vector 0)), neither vector nor its fill pointer are affected. Otherwise, the store and increment take place and vector-push returns the former value of the fill pointer which is one less than the one it leaves in vector.

vector-push-extend is just like vector-push except that if the fill pointer gets too large, vector is extended using adjust-array so that it can contain more elements. Extension is the minimum number of elements to be added to vector if it must be extended.

vector-push and vector-push-extend return the index of new-element in vector. If (>= (fill-pointer vector) (array-dimension vector 0)), vector-push returns nil.

* 示例(Examples):

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

* 受此影响(Affected By):

The value of the fill pointer.

How vector was created.

* 异常情况(Exceptional Situations):

An error of type error is signaled by vector-push-extend if it tries to extend vector and vector is not actually adjustable.

An error of type error is signaled if vector does not have a fill pointer.

* 也见(See Also):

adjustable-array-p, fill-pointer, vector-pop

* 注意(Notes): None. 

### <span id="">Function VECTORP</span>

* 语法(Syntax):

        vectorp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 为 vector 类型就返回 true; 否则, 返回 false.

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

* 也见(See Also): None.

* 注意(Notes):

        (vectorp object) ==  (typep object 'vector)


### <span id="">Accessor BIT, SBIT</span>

* 语法(Syntax):

        bit bit-array &rest subscripts => bit

        sbit bit-array &rest subscripts => bit

        (setf (bit bit-array &rest subscripts) new-bit)

        (setf (sbit bit-array &rest subscripts) new-bit)

* 参数和值(Arguments and Values):

        bit-array---对于 bit, 是一个位数组; 对于 sbit, 一个 simple-array.
        subscripts---bit-array 的一个有效数组索引的列表.
        bit---一个 bit.

* 描述(Description):

        bit 和 sbit 访问通过 subscripts 指定的 bit-array 元素.
        
        当访问元素时, 这些函数忽略填充指针.

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

* 也见(See Also):

        aref, 章节 3.2.1 (Compiler Terminology)

* 注意(Notes):

        bit 和 sbit 类似于 aref 除了它们要求数组分别为一个位数组和 simple-array.

        bit 和 sbit, 不像 char 和 schar, 允许第一个参数为一个任意维数的数组. 


### <span id="">Function BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR</span>

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

        bit-array, bit-array1, bit-array2---一个位数组.
        Opt-arg---一个位数组, 或 t, 或 nil. 默认是 nil.
        Bit-array, bit-array1, bit-array2, 和 opt-arg (if an array) 必须全都是相同的维数和大小.
        resulting-bit-array---一个位数组.

* 描述(Description):

        这些函数在 bit-array1 和 bit-array2 上执行按位的逻辑操作并且返回一个维数和大小匹配的数组, 如此以至于这个结果中的任何位都是有在这些参数的每一个的对应位上操作所产生的.

        在 bit-not 的情况下, 返回一个和 bit-array 维数和大小匹配的数组, 这个数组包含了 bit-array 的一个所有位反转的拷贝.

        如果 opt-arg 是类型 (array bit) 那么这个结果的内容被破坏性地放置到 opt-arg 中. 如果 opt-arg 是符号 t, bit-array 或 bit-array1 会被这个结果替换; 如果 opt-arg 是 nil 或省略了, 那么就创建一个新的数组来包含这个结果.

        下面这段指出有这些函数中的每一个执行的逻辑操作.
                                                                                                       
|Function                         |                        Operation                                     |
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
                                                                                                          
    Figure 15-4.  Bit-wise Logical Operations on Bit Arrays  

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

* 也见(See Also):

        lognot, logand

* 注意(Notes): None. 

### <span id="">Function BIT-VECTOR-P</span>

* 语法(Syntax):

        bit-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 bit-vector 类型的就返回 true; 否则, 返回 false.

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

* 也见(See Also):

        typep

* 注意(Notes):

        (bit-vector-p object) ==  (typep object 'bit-vector)


### <span id="">Function SIMPLE-BIT-VECTOR-P</span>

* 语法(Syntax):

        simple-bit-vector-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 simple-bit-vector 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (simple-bit-vector-p (make-array 6)) =>  false
    (simple-bit-vector-p #*) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        simple-vector-p

* 注意(Notes):

        (simple-bit-vector-p object) ==  (typep object 'simple-bit-vector)




