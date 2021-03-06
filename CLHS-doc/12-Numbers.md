# 12. 数字

> * 12.1 [数字的概念](#NumberConcepts)
> * 12.2 [数字的字典](#NumbersDictionary)

## 12.1 <span id="NumberConcepts">Number Concepts</span>

> * 12.1.1 [数值运算](#NumericOperations)
> * 12.1.2 [依赖于具体实现的数字常量](#IDNumericConstants)
> * 12.1.3 [有理数计算](#RationalComputations)
> * 12.1.4 [浮点计算](#FloatingPointComputations)
> * 12.1.5 [复数计算](#ComplexComputations)
> * 12.1.6 [区间标识符](#IntervalDesignators)
> * 12.1.7 [随机状态运算](#RandomStateOperations)

### 12.1.1 <span id="NumericOperations">数值运算</span>

Common Lisp 提供了大量和数字[number]相关的操作. 这个章节通过把这些操作分组成类别来强调它们之间的关系进而提供这些操作的一个综述.

下面这段展示了和算术操作相关的操作符[operator].

    *  1+         gcd   
    +  1-         incf  
    -  conjugate  lcm   
    /  decf             

    Figure 12-1. 算术相关的运算.

下面这段展示了和指数, 对数, 以及三角运算相关的已定义的名字[defined name].

    abs    cos    signum  
    acos   cosh   sin     
    acosh  exp    sinh    
    asin   expt   sqrt    
    asinh  isqrt  tan     
    atan   log    tanh    
    atanh  phase          
    cis    pi             

    Figure 12-2. 指数, 对数, 以及三角运算相关的已定义的名字.

下面这段展示了和数值比较和断言相关的操作符[operator].

    /=  >=      oddp   
    <   evenp   plusp  
    <=  max     zerop  
    =   min            
    >   minusp         

    Figure 12-3. 数值比较和断言相关的操作符.

下面这段展示了和数值类型处理和强制转换相关的已定义的名字[defined name].

    ceiling          float-radix           rational     
    complex          float-sign            rationalize  
    decode-float     floor                 realpart     
    denominator      fround                rem          
    fceiling         ftruncate             round        
    ffloor           imagpart              scale-float  
    float            integer-decode-float  truncate     
    float-digits     mod                                
    float-precision  numerator                          

    Figure 12-4. 数值类型处理和强制转换相关的已定义的名字.

> * 12.1.1.1 [数值运算中的结合律和交换律](#AssociativityCommutativityNO)
> * 12.1.1.2 [数值运算的传递性](#ContagionNumericOperations)
> * 12.1.1.3 [视整数为位和字节](#ViewingIntegersBitsBytes)


#### 12.1.1.1 <span id="AssociativityCommutativityNO">数值运算中的结合律和交换律</span>

对于数学上可结合的 (可能是可交换的) 的函数, 一个符合规范的具体实现[conforming implementation]可以以任何与结合重排 (可能是交换重排) 一致的方式处理实参[argument]. 这个不会影响实参[argument]表达式形式[form]被求值的顺序 ; 关于求值顺序的讨论, 见章节 3.1.2.1.2.3 (函数表达式形式). 唯一不确定的是形参[parameter]的值[value]被处理的顺序. 这个意味着具体实现[implementation]在自动强制转换应用的地方可能不同; 见章节 12.1.1.2 (数值运算的传递性).

一个符合规范的程序[conforming program]可以通过拆分这些操作到分开的(可能是嵌套的)函数表达式形式[function form]中, 或通过显式编写对执行强制转换函数[function]的调用来显式地控制处理的顺序.

##### 12.1.1.1.1 数值运算中的结合律和交换律的示例

思考下面表达式, 其中我们假设 1.0 和 1.0e-15 都表示 single-float:

 (+ 1/3 2/3 1.0d0 1.0 1.0e-15)

一个符合规范的具体实现[conforming implementation]可能从左到右处理这些实参[argument], 首先添加 1/3 和 2/3 来得到 1, 然后把那个转换为一个 double-float 用于和 1.0d0 结合, 接下来依次转换并添加 1.0 和 1.0e-15.

另一个符合规范的具体实现[conforming implementation]可能从右到左处理这些实参[argument], 首先执行 1.0 和 1.0e-15 的一个 single-float 加法 (可能在这个过程中丢失精度), 然后转换这个和为一个 double-float 并且加 1.0d0, 接下来转换 2/3 为一个 double-float 并且加上它, 接着转换 1/3 并且加上它.

第三种符合规范的具体实现[conforming implementation]可能首先扫描所有实参[argument], 处理所有有理数[rational]来保持这个计算准确, 然后在所有实参[argument]中找到一个最大浮点格式的参数并添加, 然后添加所有其他实参[argument], 依次转换它们 (所有这些可能是误导的尝试使计算尽可能准确).

在任何情况下, 这三种策略都是合法的.

一个符合规范的程序[conforming program]可以通过在编写时控制这个顺序, 比如,

 (+ (+ 1/3 2/3) (+ 1.0d0 1.0e-15) 1.0)

#### 12.1.1.2 <span id="ContagionNumericOperations">数值运算的传递性</span>

关于在数值运算中实参[argument]的隐式强制转换的传递性规则的信息, 见章节 12.1.4.4 (浮点精度传递规则), 章节 12.1.4.1 (浮点和有理数传递性的规则), 还有章节 12.1.5.2 (复数传递规则). 

#### 12.1.1.3 <span id="ViewingIntegersBitsBytes">视整数为位和字节</span>

> * 12.1.1.3.1 [整数上的逻辑操作](#LogicalOperationsIntegers)
> * 12.1.1.3.2 [整数上的字节操作](#ByteOperationsIntegers)

##### 12.1.1.3.1 <span id="LogicalOperationsIntegers">整数上的逻辑操作</span>

逻辑运算需要整数[integer]来作为参数; 如果提供一个不是整数[integer]的参数, 那么应该发出一个 type-error 类型[type]的错误. 给逻辑运算的整数[integer]参数被当作是用二补符号表示的.

下面这段展示了和数字上逻辑运算相关的已定义的名字[defined name].

    ash          boole-ior       logbitp   
    boole        boole-nand      logcount  
    boole-1      boole-nor       logeqv    
    boole-2      boole-orc1      logior    
    boole-and    boole-orc2      lognand   
    boole-andc1  boole-set       lognor    
    boole-andc2  boole-xor       lognot    
    boole-c1     integer-length  logorc1   
    boole-c2     logand          logorc2   
    boole-clr    logandc1        logtest   
    boole-eqv    logandc2        logxor    

    Figure 12-5. 数字上逻辑运算相关的已定义的名字. 

##### 12.1.1.3.2 <span id="ByteOperationsIntegers">整数上的字节操作</span>

字节操作函数[function]使用称之为字节指定符[byte specifier]的对象[object]来指定一个整数[integer]内特定字节[byte]的大小和位置. 一个字节指定符[byte specifier]的表示依赖于具体实现[implementation-dependent]; 它可能是也可能不是一个数字[number]. 函数[function] byte 会构造一个字节指定符[byte specifier], 它是各种其他字节操作函数[function]都会接受的.

下面这段展示和操作一个数字[number]的字节[byte]相关的已定义的名字[defined name].

    byte           deposit-field  ldb-test    
    byte-position  dpb            mask-field  
    byte-size      ldb                        

    Figure 12-6. 字节操作相关的已定义的名字. 

### 12.1.2 <span id="IDNumericConstants">依赖于具体实现的数字常量</span>

下面这段展示了和数字[number]的依赖于具体实现[implementation-dependent]的详情相关的已定义名字[defined name].

    double-float-epsilon           most-negative-fixnum           
    double-float-negative-epsilon  most-negative-long-float       
    least-negative-double-float    most-negative-short-float      
    least-negative-long-float      most-negative-single-float     
    least-negative-short-float     most-positive-double-float     
    least-negative-single-float    most-positive-fixnum           
    least-positive-double-float    most-positive-long-float       
    least-positive-long-float      most-positive-short-float      
    least-positive-short-float     most-positive-single-float     
    least-positive-single-float    short-float-epsilon            
    long-float-epsilon             short-float-negative-epsilon   
    long-float-negative-epsilon    single-float-epsilon           
    most-negative-double-float     single-float-negative-epsilon  

    Figure 12-7. 数字的依赖于具体实现的详情相关的已定义名字. 


### 12.1.3 <span id="RationalComputations">有理数计算</span>

这个章节中的规则适用于有理数[rational]计算.

> * 12.1.3.1 [无界的有理数精度规则](#RuleUnboundedRationalPrecision)
> * 12.1.3.2 [有理数的规范表示规则](#RuleCanonicalReprRationals)
> * 12.1.3.3 [浮点的置换性规则](#RuleFloatSubstitutability)

#### 12.1.3.1 <span id="RuleUnboundedRationalPrecision">无界的有理数精度规则</span>

从一般意义上来说有理数计算不可能上溢 (尽管这里可能没有足够的存储来表示一个结果), 因为整数[integer]和比数[ratio]原则上是任意大小的. 

#### 12.1.3.2 <span id="RuleCanonicalReprRationals">有理数的规范表示规则</span>

如果任何计算产生一个结果是两个整数的数学比, 并且分母可以整除分子, 那么这个结果会被转换为等价的整数[integer].

如果分母不能整除分子, 那么一个有理数[rational]的标准表示是分子和分母的比率[ratio], 其中分子和分母最大的公约数是 1 , 并且分母是大于 1 的正数.

当作为一个输入时 (在默认语法中), 符号 -0 总是表示整数[integer] 0. 一个符合规范的具体实现[conforming implementation]一定不能有着与整数 0 的表示不同的"负零"的表示. 但是, 这样的区别对于浮点数[float]也是有可能的; 见类型[type] float. 

#### 12.1.3.3 <span id="RuleFloatSubstitutability">浮点的置换性规则</span>

当一个无理数学函数[function]的参数都是有理数[rational]而真正的数学结果也是(数学上)有理数时, 除非另有说明, 否则一个实现可以自由地返回一个准确的有理数[rational]结果或者一个 single-float 近似值. 如果参数都是有理数[rational]但是结果不能被表示为一个有理数[rational]数字, 那么总是会返回一个 single-float 近似值.

当一个无理数学函数[function]的参数都是类型 (or rational (complex rational)) 并且真正的数学结果是(数学上)一个带有有理数实部和虚部的复数, 除非另有说明, 否则一个具体实现可以自由地返回一个类型 (or rational (complex rational)) 的精准结果或者一个 single-float (只有当真实的数学结果的虚部为零时才允许) 或者 (complex single-float). 如果参数都是类型 (or rational (complex rational)) 但是结果不能被表示为一个有理数[rational]或者复有理数[complex rational], 那么返回值会是 single-float 类型(只有当真实的数学结果的虚部为零时才允许) 或者 (complex single-float).

浮点数置换性不能应用于有理数学函数[function] +, -, *, 和 / 也不能应用于相关操作符[operator] 1+, 1-, incf, decf, 和 conjugate. 对于有理数学函数[function], 如果所有参数都是有理数[rational], 那么结果就是有理数[rational]; 如果所有参数都是类型 (or rational (complex rational)), 那么结果也就是类型 (or rational (complex rational)).

    函数      简单结果                                   
    abs       (abs #c(3 4)) =>  5 or 5.0                       
    acos      (acos 1) =>  0 or 0.0                            
    acosh     (acosh 1) =>  0 or 0.0                           
    asin      (asin 0) =>  0 or 0.0                            
    asinh     (asinh 0) =>  0 or 0.0                           
    atan      (atan 0) =>  0 or 0.0                            
    atanh     (atanh 0) =>  0 or 0.0                           
    cis       (cis 0) =>  1 or #c(1.0 0.0)                     
    cos       (cos 0) =>  1 or 1.0                             
    cosh      (cosh 0) =>  1 or 1.0                            
    exp       (exp 0) =>  1 or 1.0                             
    expt      (expt 8 1/3) =>  2 or 2.0                        
    log       (log 1) =>  0 or 0.0                             
              (log 8 2) =>  3 or 3.0                           
    phase     (phase 7) =>  0 or 0.0                           
    signum    (signum #c(3 4)) =>  #c(3/5 4/5) or #c(0.6 0.8)  
    sin       (sin 0) =>  0 or 0.0                             
    sinh      (sinh 0) =>  0 or 0.0                            
    sqrt      (sqrt 4) =>  2 or 2.0                            
              (sqrt 9/16) =>  3/4 or 0.75                      
    tan       (tan 0) =>  0 or 0.0                             
    tanh      (tanh 0) =>  0 or 0.0                            

    Figure 12-8. 受浮点数置换性规则影响的函数 


### 12.1.4 <span id="FloatingPointComputations">浮点计算</span>

下面规则应用于浮点数计算.

> * 12.1.4.1 [浮点和有理数传递性的规则](#RuleFloatRationalContagion)
> * 12.1.4.2 [浮点近似的规则](#RuleFloatApproximation)
> * 12.1.4.3 [浮点的上溢和下溢规则](#RuleFloatUnderflowOverflow)
> * 12.1.4.4 [浮点精度传递规则](#RuleFloatPrecisionContagion)


#### 12.1.4.1 <span id="RuleFloatRationalContagion">浮点和有理数传递性的规则</span>

当有理数[rational]和浮点数[float]通过一个数值函数进行组合时, 这个有理数[rational]首先被转换为一个相同格式的浮点数[float]. 对于像 + 这样的接受超过两个参数的函数[function], 允许部分操作使用有理数[rational]进行操作, 其余部分则使用浮点运算来完成.

当有理数[rational]和浮点数[float]被一个数值函数进行比较时, 函数[function] rational 会被调用来将这个浮点数[float]转换为一个有理数[rational]然后执行一个准确的比较. 在复数[complex]的情况下, 实部和虚部会被单独处理.

##### 12.1.4.1.1 浮点和有理数传递性的规则的示例

```LISP
;;;; Combining rationals with floats.
;;; This example assumes an implementation in which 
;;; (float-radix 0.5) is 2 (as in IEEE) or 16 (as in IBM/360),
;;; or else some other implementation in which 1/2 has an exact 
;;;  representation in floating point.
(+ 1/2 0.5) =>  1.0
(- 1/2 0.5d0) =>  0.0d0
(+ 0.5 -0.5 1/2) =>  0.5

;;;; Comparing rationals with floats.
;;; This example assumes an implementation in which the default float 
;;; format is IEEE single-float, IEEE double-float, or some other format
;;; in which 5/7 is rounded upwards by FLOAT.
(< 5/7 (float 5/7)) =>  true
(< 5/7 (rational (float 5/7))) =>  true
(< (float 5/7) (float 5/7)) =>  false
```

#### 12.1.4.2 <span id="RuleFloatApproximation">浮点近似的规则</span>

浮点数[float]的计算只是近似的, 尽管它们被描述为结果在数学上是准确的. 两个数学上相同的表达式可能在计算上是不同的因为浮点近似过程中存在固有的错误. 一个浮点数[float]的精度不一定与这个数字的精度相关. 比如, 3.142857142857142857 相比 3.14159 是 <PI> 的一个更精确的近似值, 但是后者更准确. 精度指的是在表示中保留的位元数. 当一个操作符组合一个短浮点数[short float]和一个长浮点数[long float]时, 结果会是一个长浮点数[long float]. Common Lisp 函数假定给它们的参数的准确度不会超过它们的精度. 因此当两个 small float 被组合时, 结果是一个 small float. Common Lisp 函数不会从一个较大的值转换到较小的一个. 


#### 12.1.4.3 <span id="RuleFloatUnderflowOverflow">浮点的上溢和下溢规则</span>

如果一个浮点数运算导致指数上溢或下溢, 就分别会发出一个 floating-point-overflow 或 floating-point-underflow 类型[type]的错误. 


#### 12.1.4.4 <span id="RuleFloatPrecisionContagion">浮点精度传递规则</span>

一个数值函数的结果是给这个函数[function]的所有浮点数参数中最大格式的一个浮点数[float]. 

### 12.1.5 <span id="ComplexComputations">复数计算</span>

以下规则应用于复数[complex]计算:

> * 12.1.5.1 [复数的置换性规则](#RuleComplexSubstitutability)
> * 12.1.5.2 [复数传递规则](#RuleComplexContagion)
> * 12.1.5.3 [复数的正规表示规则](#RuleCanonicalReprComplexRationals)
> * 12.1.5.4 [主值和分支切割](#PrincipalValuesBranchCuts)

#### 12.1.5.1 <span id="RuleComplexSubstitutability">复数的置换性规则</span>

除了在无理和超越函数[function]的执行期间, 除非它的实参[argument]中的一个或多个是复数[complex], 否则数值函数[function]不会产生[yield]一个复数[complex]. 

#### 12.1.5.2 <span id="RuleComplexContagion">复数传递规则</span>

当一个实数[real]和一个复数[complex]都是计算的一部分时, 这个实数[real]首先通过提供一个 0 的虚部来转换为复数[complex]. 

#### 12.1.5.3 <span id="RuleCanonicalReprComplexRationals">复数的正规表示规则</span>

如果任何计算的结果都是一个实部是 rational 类型[type], 虚部是零的复数[complex], 那么结果被转换成这个实部的有理数[rational]. 这个规则不能应用于两个部分都是浮点数[float]的复数[complex]. 比如, #C(5 0) 和 5 在 Common Lisp (它们在 eql 下总是相同的[same]) 中不是不同[different]的对象[object]; #C(5.0 0.0) 和 5.0 在 Common Lisp 中总是为不同[different]的对象[object] (它们在 eql 下总是不相同的, 尽管它们在 equalp 和 = 下是相同的[same]).


##### 12.1.5.3.1 复数的正规表示规则的示例

```LISP
#c(1.0 1.0) =>  #C(1.0 1.0)
#c(0.0 0.0) =>  #C(0.0 0.0)
#c(1.0 1) =>  #C(1.0 1.0)
#c(0.0 0) =>  #C(0.0 0.0)
#c(1 1) =>  #C(1 1)
#c(0 0) =>  0
(typep #c(1 1) '(complex (eql 1))) =>  true
(typep #c(0 0) '(complex (eql 0))) =>  false
```

##### 12.1.5.4 <span id="PrincipalValuesBranchCuts">主值和分支切割</span>

很多无理函数和超越函数在复数领域会被多次定义; 比如, 对于对数函数, 一般来说有无穷多的复数值. 在每个这样的例子中, 必须为这个函数选择一个首要[principal]的值[value]来返回. 通常情况下, 这样的值不能被选择, 从而使范围连续; 在被称为分支切割的区域中, 必须定义分支, 这又定义了范围内的不连续点. Common Lisp 为这些复数函数定义了分支切割, 首要[principal]的值[value], 还有边界状况, 这些函数遵循 "复数 APL 中的主值和分支切割". 适用于每个函数的分支切割规则和那个函数的描述相对应.

下面这段列出了在复数域的适用部分中所遵守的恒等式, 即使是在分支切割上:

    sin i z = i sinh z  sinh i z = i sin z        arctan i z = i arctanh z  
    cos i z = cosh z    cosh i z = cos z          arcsinh i z = i arcsin z  
    tan i z = i tanh z  arcsin i z = i arcsinh z  arctanh i z = i arctan z  

    Figure 12-9. 复数域的三角恒等式

在分支切割的讨论中提到的象限编号如下图所示.

    见 http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_12-1-5-4.html

    Figure 12-10. 分支切割的象限编号

### 12.1.6 <span id="IntervalDesignators">区间标识符</span>

一个数值类型指定符[type specifier]的复合类型指定符[compound type specifier]形式允许用户在实数线上去指定一个描述这个类型[type]的子类型[subtype]的区间, 该类型将由相应的原子类型指定符[atomic type specifier]描述. 某个类型[type] T 的子类型[subtype]是使用称为类型[type] T 的区间标识符[interval designator]的有序对象[object]对指定的.

T 类型[type]的两个区间标识符[interval designator]的第一个可以是以下任意一种:

一个类型[type] T 的数字 N

    这个表示包含 N 的下边界. 这也就是说, T 的子类型[subtype]的元素[element]会大于等于 N.

一个元素[element]为类型[type] T 的数字 M 的单元素[singleton]列表[list]

    这个表示不包含 M 的下边界. 这也就是说, T 的子类型[subtype]的元素[element]会大于 M.

符号 *

    这表示在区间上没有下限.

T 类型[type]的两个区间标识符[interval designator]的第二个可以是以下任意一种:

一个类型[type] T 的数字 N

    这个表示不包含 N 的上边界. 这也就是说, T 的子类型[subtype]的元素[element]会小于等于 N.

一个元素[element]为类型[type] T 的数字 M 的单元素[singleton]列表[list]

    这个表示不包含 M 的上边界. 这也就是说, T 的子类型[subtype]的元素[element]会小于 M.

符号 *

    这表示在区间上没有上限. 


### 12.1.7 <span id="RandomStateOperations">随机状态运算</span>

下面这段列出了可应用于随机状态[random state]的已定义的名字[defined name].

    *random-state*     random            
    make-random-state  random-state-p    

    Figure 12-11. 随机状态已定义的名字 


## 12.2 <span id="NumbersDictionary">数字的字典</span>

> * [系统类 NUMBER](#SC-NUMBER)
> * [系统类 COMPLEX](#SC-COMPLEX)
> * [系统类 REAL](#SC-REAL)
> * [系统类 FLOAT](#SC-FLOAT)
> * [类型 SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT](#T-SF-SF-DF-LF)
> * [系统类 RATIONAL](#SC-RATIONAL)
> * [系统类 RATIO](#SC-RATIO)
> * [系统类 INTEGER](#SC-INTEGER)
> * [类型 SIGNED-BYTE](#T-SIGNED-BYTE)
> * [类型 UNSIGNED-BYTE](#T-UNSIGNED-BYTE)
> * [类型特化符 MOD](#TS-MOD)
> * [类型 BIT](#T-BIT)
> * [类型 FIXNUM](#T-FIXNUM)
> * [类型 BIGNUM](#T-BIGNUM)
> * [函数 =, /=, <, >, <=, >=](#F-Compare)
> * [函数 MAX, MIN](#F-MAX-MIN)
> * [函数 MINUSP, PLUSP](#F-MINUSP-PLUSP)
> * [函数 ZEROP](#Function-ZEROP)
> * [函数 FLOOR, FFLOOR, CEILING, FCEILING, TRUNCATE, FTRUNCATE, ROUND, FROUND](#F-F-F-C-F-T-F-R-F)
> * [函数 SIN, COS, TAN](#F-SIN-COS-TAN)
> * [函数 ASIN, ACOS, ATAN](#F-ASIN-ACOS-ATAN)
> * [常量 PI](#CV-PI)
> * [函数 SINH, COSH, TANH, ASINH, ACOSH, ATANH](#F-S-C-T-A-A-A)
> * [函数 *](#F-Multiply)
> * [函数 +](#F-Add)
> * [函数 -](#F-Sub)
> * [函数 /](#F-Div)
> * [函数 1+, 1-](#F-OP-OI)
> * [函数 ABS](#F-ABS)
> * [函数 EVENP, ODDP](#F-EVENP-ODDP)
> * [函数 EXP, EXPT](#F-EXP-EXPT)
> * [函数 GCD](#F-GCD)
> * [宏 INCF, DECF](#M-INCF-DECF)
> * [函数 LCM](#F-LCM)
> * [函数 LOG](#F-LOG)
> * [函数 MOD, REM](#F-MOD-REM)
> * [函数 SIGNUM](#F-SIGNUM)
> * [函数 SQRT, ISQRT](#F-SQRT-ISQRT)
> * [系统类 RANDOM-STATE](#SC-RANDOM-STATE)
> * [函数 MAKE-RANDOM-STATE](#F-MAKE-RANDOM-STATE)
> * [函数 RANDOM](#F-RANDOM)
> * [函数 RANDOM-STATE-P](#F-RANDOM-STATE-P)
> * [变量 *RANDOM-STATE*](#V-RANDOM-STATE)
> * [函数 NUMBERP](#F-NUMBERP)
> * [函数 CIS](#F-CIS)
> * [函数 COMPLEX](#F-COMPLEX)
> * [函数 COMPLEXP](#F-COMPLEXP)
> * [函数 CONJUGATE](#F-CONJUGATE)
> * [函数 PHASE](#F-PHASE)
> * [函数 REALPART, IMAGPART](#F-REALPART-IMAGPART)
> * [函数 UPGRADED-COMPLEX-PART-TYPE](#F-UPGRADED-COMPLEX-PART-TYPE)
> * [函数 REALP](#F-REALP)
> * [函数 NUMERATOR, DENOMINATOR](#F-NUMERATOR-DENOMINATOR)
> * [函数 RATIONAL, RATIONALIZE](#F-RATIONAL-RATIONALIZE)
> * [函数 RATIONALP](#F-RATIONALP)
> * [函数 ASH](#F-ASH)
> * [函数 INTEGER-LENGTH](#F-INTEGER-LENGTH)
> * [函数 INTEGERP](#F-INTEGERP)
> * [函数 PARSE-INTEGER](#F-PARSE-INTEGER)
> * [函数 BOOLE](#F-BOOLE)
> * [常量 BOOLE-1, BOOLE-2, BOOLE-AND, BOOLE-ANDC1, BOOLE-ANDC2, BOOLE-C1, BOOLE-C2, BOOLE-CLR, BOOLE-EQV, BOOLE-IOR, BOOLE-NAND, BOOLE-NOR, BOOLE-ORC1, BOOLE-ORC2, BOOLE-SET, BOOLE-XOR](#CV-B)
> * [函数 LOGAND, LOGANDC1, LOGANDC2, LOGEQV, LOGIOR, LOGNAND, LOGNOR, LOGNOT, LOGORC1, LOGORC2, LOGXOR](#F-L)
> * [函数 LOGBITP](#F-LOGBITP)
> * [函数 LOGCOUNT](#F-LOGCOUNT)
> * [函数 LOGTEST](#F-LOGTEST)
> * [函数 BYTE, BYTE-SIZE, BYTE-POSITION](#F-BYTE-BYTE-SIZE-BYTE-POSITION)
> * [函数 DEPOSIT-FIELD](#F-DEPOSIT-FIELD)
> * [函数 DPB](#F-DPB)
> * [访问器 LDB](#A-LDB)
> * [函数 LDB-TEST](#F-LDB-TEST)
> * [访问器 MASK-FIELD](#A-MASK-FIELD)
> * [常量 MOST-POSITIVE-FIXNUM, MOST-NEGATIVE-FIXNUM](#CV-MM)
> * [函数 DECODE-FLOAT, SCALE-FLOAT, FLOAT-RADIX, FLOAT-SIGN, FLOAT-DIGITS, FLOAT-PRECISION, INTEGER-DECODE-FLOAT](#F-DSFFFFI)
> * [函数 FLOAT](#F-FLOAT)
> * [函数 FLOATP](#F-FLOATP)
> * [常量 MOST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT, MOST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT, MOST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-NORMALIZED-LONG-FLOAT, MOST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT, MOST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT, MOST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT](#CV-MOST-LEAST)
> * [常量 SHORT-FLOAT-EPSILON, SHORT-FLOAT-NEGATIVE-EPSILON, SINGLE-FLOAT-EPSILON, SINGLE-FLOAT-NEGATIVE-EPSILON, DOUBLE-FLOAT-EPSILON, DOUBLE-FLOAT-NEGATIVE-EPSILON, LONG-FLOAT-EPSILON, LONG-FLOAT-NEGATIVE-EPSILON](#CV-EPSILON)
> * [状况类型 ARITHMETIC-ERROR](#CT-ARITHMETIC-ERROR)
> * [函数 ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION](#F-AEO-AEO)
> * [状况类型 DIVISION-BY-ZERO](#CT-DIVISION-BY-ZERO)
> * [状况类型 FLOATING-POINT-INVALID-OPERATION](#CT-FLOATING-POINT-INVALID-OPERATION)
> * [状况类型 FLOATING-POINT-INEXACT](#CT-FLOATING-POINT-INEXACT)
> * [状况类型 FLOATING-POINT-OVERFLOW](#CT-FLOATING-POINT-OVERFLOW)
> * [状况类型 FLOATING-POINT-UNDERFLOW](#CT-FLOATING-POINT-UNDERFLOW)

### <span id="SC-NUMBER">系统类 NUMBER</span>

* 类优先级列表(Class Precedence List):

        number, t

* 描述(Description):

        类型[type] number 包含了表示数学数字的对象[object]. 类型[type] real 和 complex 是 number 互斥[disjoint]的子类型[subtype].

        函数[function] = 测试数值等价. 函数[function] eql, 当它的参数都是数字[number]时, 测试它们是否有着相同的类型[type]和数值. 两个数字[number]在 eql 或 = 下是相同的[same]但在 eq 下不一定相同[same].

* 注意(Notes):

        Common Lisp 在一些命名问题上和数学不同. 在数学中, 实数的集合习惯上被描述为是复数的一个子集, 但是在 Common Lisp 中, 类型[type] real 和类型[type] complex 是互斥的. 包含了所有数学复数的 Common Lisp 类型称为 number. 造成这些差异的原因包括历史先例, 和其他流行计算机语言兼容, 以及多种时间和空间效率问题.


### <span id="SC-COMPLEX">系统类 COMPLEX</span>

* 类优先级列表(Class Precedence List):

        complex, number, t

* 描述(Description):

        类型[type] complex 包括所有数学上的复数除了被包含在类型[type] rational 中的那些. 复数[complex]被表示为一个实部和一个虚部的笛卡尔形式, 其中的每个部分都是一个实数[real]. 实部和虚部都是有理数[rational]或都是相同的浮点数[float]类型[type]. 虚部可以是一个浮点数[float] 0, 但不能是一个有理数[rational] zero, 因为这样的一个数字被 Common Lisp 表示为一个有理数[rational]而不是一个复数[complex].

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        complex [typespec | *]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        typespec---一个表示类型[type] real 的一个子类型[subtype]的类型指定符[type specifier].

* 复合类型指定符描述(Compound Type Specifier Description):

        这个类型[type]的每个元素都是实部和虚部为类型 (upgraded-complex-part-type typespec) 的一个复数[complex]. 这个类型[type]包括那些可以通过给 complex 传递类型[type] typespec 的数字来产生的复数[complex].

        (complex type-specifier) 指的是所有可以通过给函数[function] complex 传递类型 type-specifier 的数字[number]来产生的复数[complex], 加上所有其他的相同的专门的表示.

* 参见(See Also):

        章节 12.1.5.3 (复数的正规表示规则), 章节 2.3.2 (从标记构建数字), 章节 22.1.3.1.4 (打印复数)

* 注意(Notes):

        一个实部为 r 和虚部为 i 的复数[complex]的输入语法是 #C(r i). 关于更多详情, 见章节 2.4 (标准宏字符).

        对于每个浮点数[float] n, 这里会有一个表示相同数学数值的复数[complex], 可以通过 (COERCE n 'COMPLEX) 来获取. 


### <span id="SC-REAL">系统类 REAL</span>

* 类优先级列表(Class Precedence List):

        real, number, t

* 描述(Description):

        类型[type] real 包括表示数学上实数的所有数字[number], 尽管这里有着在 Common Lisp 中没有准确表示的数学上的实数. 只有实数可以使用 <, >, <=, 和 >= 函数来排序.

        类型[type] rational 和 float 是类型[type] real 互斥[disjoint]的子类型[subtype].

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        real [lower-limit [upper-limit]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        lower-limit, upper-limit---类型 real 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示下限 lower-limit 和上限 upper-limit 所描述区间上的实数. 


### <span id="SC-FLOAT">系统类 FLOAT</span>

* 类优先级列表(Class Precedence List):

        float, real, number, t

* 描述(Description):

        一个浮点数[float]是一个 s*f*b^e-p 形式的数学上有理数 (但不是一个 Common Lisp 有理数[rational]), 其中 s 是 +1 或 -1, 就是那个正负号; b 是一个大于 1 的整数[integer], 这个表示的基数或底数; p 是一个正整数[integer], 这个浮点数[float]的精度 (以 b 为基数的数字); f 是一个在 b^p-1 和 b^p-1 (包含的) 之间的正整数[integer], 表示有效位数; 还有 e 是一个整数[integer], 表示指数. p 的值和 e 的范围取决于这个具体实现以及在这个具体实现中浮点数[float]的类型. 另外, 这里有一个浮点数 0; 依赖于具体实现, 这里也可以有一个 "负零". 如果这里没有负零, 那么 0.0 和 -0.0 都被简单解释为浮点数零. (= 0.0 -0.0) 总是为 true. 如果这里存在负零, (eql -0.0 0.0) 是 false, 否则就是 true.

        类型[type] short-float, single-float, double-float, 和 long-float 是类型[type] float 的子类型[subtype]. 它们中的任意两个都必须是互斥[disjoint]的类型[type]或者相同[same]类型[type]; 如果是相同[same]类型[type], 那么在上面顺序中在它们之间的其他类型[type]也必须是相同[same]类型[type]. 比如, 如果类型[type] single-float 和类型[type] long-float 是相同[same]类型, 那么类型[type] double-float 也必须是相同[same]类型[type].

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        float [lower-limit [upper-limit]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        lower-limit, upper-limit---类型[type] float 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示下限 lower-limit 和上限 upper-limit 所描述区间上的浮点数[float].

* 参见(See Also):

        Figure 2-9, 章节 2.3.2 (从标记构建数字), 章节 22.1.3.1.3 (打印浮点数)

* 注意(Notes):

        注意所有数学上的整数不仅能被表示为 Common Lisp 实数, 也可以是浮点复数[complex float]. 比如, 数学上的数字 1 可能的表示包括整数[integer] 1, 浮点数[float] 1.0, 或者复数[complex] #C(1.0 0.0). 


### <span id="T-SF-SF-DF-LF">类型 SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT</span>

* 超类型(Supertypes):

        short-float: short-float, float, real, number, t

        single-float: single-float, float, real, number, t

        double-float: double-float, float, real, number, t

        long-float: long-float, float, real, number, t

* 描述(Description):

        对于这四个已定义的 float 类型[type]的子类型[subtype], 类型[type] short-float 和类型[type] long-float 的中间是类型[type] single-float 和类型[type] double-float. 这些类别的精度定义是具体实现定义的[implementation-defined]. 精度 (以 "位" 来测量, 用 p log 2b 来计算) 和指数大小 (也是以 "位" 来测量, 用 log 2(n+1) 计算, 其中 n 是最大指数值) 建议至少和下一段中的值一样的. 每个已定义的 float 类型[type]的子类型[subtype]可能有也可能没有一个负零.

        格式    最小精度           最小指数大小  
                                      
        Short   13 bits            5 bits                 
        Single  24 bits            8 bits                 
        Double  50 bits            8 bits                 
        Long    50 bits            8 bits                 

        Figure 12-12. 建议的最小浮点精度和指数大小

        浮点数[float]的内部表示可能少于 4 个. 如果存在更少的不同表示, 则适用以下规则:

            如果这里只有一个, 那就是类型[type] single-float. 在这个表示中, 一个对象[object]同时为类型[type] single-float, double-float, short-float, 和 long-float.

            两种内部表示可以被安排为以下两种方式:

                提供两种类型[type]: single-float 和 short-float. 一个对象[object]同时为 single-float, double-float, 和 long-float.
                提供两种类型[type]: single-float 和 double-float. 一个对象[object]同时为 single-float 和 short-float, 或 double-float 和 long-float.

            三种内部表示可以被安排以下两种方式:

                提供三种类型[type]: short-float, single-float, 和 double-float. 一个对象[object]同时为 double-float 和 long-float.
                提供三种类型[type]: single-float, double-float, 和 long-float. 一个对象[object]同时为 single-float 和 short-float. 

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        short-float [short-lower-limit [short-upper-limit]]

        single-float [single-lower-limit [single-upper-limit]]

        double-float [double-lower-limit [double-upper-limit]]

        long-float [long-lower-limit [long-upper-limit]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        short-lower-limit, short-upper-limit---类型[type] short-float 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

        single-lower-limit, single-upper-limit---类型[type] single-float 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

        double-lower-limit, double-upper-limit---类型[type] double-float 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

        long-lower-limit, long-upper-limit---类型[type] long-float 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        每一个都表示在区间指示符[interval designator]指定的区间内的指定类型[type]的浮点数[float]集合. 


### <span id="SC-RATIONAL">系统类 RATIONAL</span>

* 类优先级列表(Class Precedence List):

        rational, real, number, t

* 描述(Description):

        如果一个有理数[rational]的值是整形的, 那么它的正规表示是一个整数[integer], 否则就是一个比数[ratio].

        类型[type] integer 和 ratio 是类型[type] rational 的互斥[disjoint]的子类型[subtype].

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        rational [lower-limit [upper-limit]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        lower-limit, upper-limit---类型[type] rational 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示由下限 lower-limit 和上限 upper-limit 描述的有理数[rational]. 

### <span id="SC-RATIO">系统类 RATIO</span>

* 类优先级列表(Class Precedence List):

        ratio, rational, real, number, t

* 描述(Description):

        一个比数[ratio]是一个表示数学上两个非零整数的比数的数字[number], 这两个整数分别为分子和分母, 它的最大公约数为 1, 其中分母为正的并且大于 1.

* 参见(See Also):

        Figure 2-9, 章节 2.3.2 (从标记构建数字), 章节 22.1.3.1.2 (打印比数) 


### <span id="SC-INTEGER">系统类 INTEGER</span>

* 类优先级列表(Class Precedence List):

        integer, rational, real, number, t

* 描述(Description):

        一个整数[integer]是一个数学上的整数. 一个整数[integer]的大小是没有限制的.

        类型[type] fixnum 和 bignum 组成类型[type] integer 的一个详尽的分区[exhaustive partition].

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        integer [lower-limit [upper-limit]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        lower-limit, upper-limit---类型[type] integer 的区间指示符[interval designator]. 对于 lower-limit 和 upper-limit 中的每个默认值都是符号[symbol] *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示由下限 lower-limit 和上限 upper-limit 描述的整数[integer].

* 参见(See Also):

        Figure 2-9, 章节 2.3.2 (从标记构建数字), 章节 22.1.3.1.1 (打印整数)

* 注意(Notes):

        类型[type] (integer lower upper), 其中 lower 和 upper 分别是 most-negative-fixnum 和 most-positive-fixnum, 也被称为 fixnum.

        类型[type] (integer 0 1) 也被称为 bit. 类型[type] (integer 0 *) 也被称为 unsigned-byte. 


### <span id="T-SIGNED-BYTE">类型 SIGNED-BYTE</span>

* 超类型(Supertypes):

        signed-byte, integer, rational, real, number, t

* 描述(Description):

        这个原子类型指定符[type specifier] signed-byte 表示和类型指定符[type specifier] integer 表示的相同的类型; 但是, 这两个类型指定符[type specifier]的列表形式有着不同的语义.

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        signed-byte [s | *]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        s---一个正整数[integer].

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示可以以二进制补码的形式用 s 比特的字节[byte]所表示的整数[integer]的集合. 这个等价于 (integer -2^s-1 2^s-1-1). 类型 signed-byte 或类型 (signed-byte *) 和类型[type] integer 是相同的. 


### <span id="T-UNSIGNED-BYTE">类型 UNSIGNED-BYTE</span>

* 超类型(Supertypes):

        unsigned-byte, signed-byte, integer, rational, real, number, t

* 描述(Description):

        这个原子类型指定符[type specifier] unsigned-byte 表示类型指定符[type specifier] (integer 0 *) 所表示的相同类型.

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        unsigned-byte [s | *]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        s---一个正整数[integer].

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示可以以一个大小为 s 比特的字节[byte]表示的非负整数[integer]的集合. 这个等价于 m=2^s 时的 (mod m), 或者等价于 n=2^s-1 时的 (integer 0 n). 类型[type] unsigned-byte 或类型[type] (unsigned-byte *) 和类型 (integer 0 *) 相同, 都表示非负的整数[integer]集合.

* 注意(Notes):

        类型[type] (unsigned-byte 1) 也被称为 bit. 


### <span id="TS-MOD">类型指定符 MOD</span>

* 复合类型指定符类别(Compound Type Specifier Kind):

        缩写的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        mod n

* 复合类型指定符参数(Compound Type Specifier Arguments):

        n---一个正整数[integer].

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示小于 n 的非负整数[integer]集合. 这等价于 (integer 0 (n)) 或 (integer 0 m), 其中 m=n-1.

        这个参数是必须的, 并且不能为 *.

        符号 mod 作为一个类型指定符[type specifier]是无效的. 


### <span id="T-BIT">类型 BIT</span>

* 超类型(Supertypes):

        bit, unsigned-byte, signed-byte, integer, rational, real, number, t

* 描述(Description):

        类型[type] bit 等价于类型[type] (integer 0 1) 和 (unsigned-byte 1). 


### <span id="T-FIXNUM">类型 FIXNUM</span>

* 超类型(Supertypes):

        fixnum, integer, rational, real, number, t

* 描述(Description):

        一个 fixnum 是一个值在 most-negative-fixnum 和 most-positive-fixnum (包含的) 之间的整数[integer]. 具体哪些整数[integer]是 fixnum 是由具体实现定义的[implementation-defined]. 类型[type] fixnum 需要是 (signed-byte 16) 的一个超类型. 


### <span id="T-BIGNUM">类型 BIGNUM</span>

* 超类型(Supertypes):

        bignum, integer, rational, real, number, t

* 描述(Description):

        类型[type] bignum 准确地被定义为 (and integer (not fixnum)). 


### <span id="F-Compare">函数 =, /=, <, >, <=, >=</span>

* 语法(Syntax):

        = &rest numbers+ => generalized-boolean

        /= &rest numbers+ => generalized-boolean

        < &rest numbers+ => generalized-boolean

        > &rest numbers+ => generalized-boolean

        <= &rest numbers+ => generalized-boolean

        >= &rest numbers+ => generalized-boolean

* 参数和值(Arguments and Values):

        number---对于 <, >, <=, >=: 一个实数[real]; 对于 =, /=: 一个数字[number].

        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        =, /=, <, >, <=, 和 >= 在它们的参数上按照如下执行数学比较:

        =

            如果所有数字 numbers 的值都是相同的那么 = 的结果为 true; 否则就是 false. 如果两个复数[complex]的实部和虚部根据 = 都是相等的, 那么它们就被 = 认为是相等的.

        /=

            如果没有两个数字 numbers 在值上是相等的那么 /= 的值就是 true; 否则就是 false.

        <

            如果这些数字 numbers 是单调递增的那么 < 的值就是 true; 否则就是 false.

        >

            如果这些数字 numbers 是单调递减的那么 > 的值就是 true; 否则就是 false.

        <=

            如果这些数字 numbers 是单调非递减的那么 <= 的值就是 true; 否则就是 false.

        >=

            如果这些数字 numbers 是单调非递增的那么 >= 的值就是 true; 否则就是 false.

        =, /=, <, >, <=, 和 >= 执行必要的类型转换.

* 示例(Examples):

        这些函数的用法如下面这段所示.

        (= 3 3) 是 true.              (/= 3 3) 是 false.             
        (= 3 5) 是 false.             (/= 3 5) 是 true.              
        (= 3 3 3 3) 是 true.          (/= 3 3 3 3) 是 false.         
        (= 3 3 5 3) 是 false.         (/= 3 3 5 3) 是 false.         
        (= 3 6 5 2) 是 false.         (/= 3 6 5 2) 是 true.          
        (= 3 2 3) 是 false.           (/= 3 2 3) 是 false.           
        (< 3 5) 是 true.              (<= 3 5) 是 true.              
        (< 3 -5) 是 false.            (<= 3 -5) 是 false.            
        (< 3 3) 是 false.             (<= 3 3) 是 true.              
        (< 0 3 4 6 7) 是 true.        (<= 0 3 4 6 7) 是 true.        
        (< 0 3 4 4 6) 是 false.       (<= 0 3 4 4 6) 是 true.        
        (> 4 3) 是 true.              (>= 4 3) 是 true.              
        (> 4 3 2 1 0) 是 true.        (>= 4 3 2 1 0) 是 true.        
        (> 4 3 3 2 0) 是 false.       (>= 4 3 3 2 0) 是 true.        
        (> 4 3 1 2 0) 是 false.       (>= 4 3 1 2 0) 是 false.       
        (= 3) 是 true.                (/= 3) 是 true.                
        (< 3) 是 true.                (<= 3) 是 true.                
        (= 3.0 #c(3.0 0.0)) 是 true.  (/= 3.0 #c(3.0 1.0)) 是 true.  
        (= 3 3.0) 是 true.            (= 3.0s0 3.0d0) 是 true.       
        (= 0.0 -0.0) 是 true.         (= 5/2 2.5) 是 true.           
        (> 0.0 -0.0) 是 false.        (= 0 -0.0) 是 true.            
        (<= 0 x 9) 如果 x 在 0 和 9 之间包括 9 就是 true                               
        (< 0.0 x 1.0) 如果 x 在 0.0 和 1.0 之间不包括 1.0 就是 true                              
        (< -1 j (length v)) 如果 j 是一个序列 v 的有效索引就是 true

        Figure 12-13. /=, =, <, >, <=, 和 >= 的使用

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果某个实参[argument]不是一个实数[real], 那么就会发出一个 type-error 类型[type]的错误. 如果不能满足它的合约(contract)可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        = 有别于 eql, = 在 (= 0.0 -0.0) 总是为 true, 因为 = 比较它的操作数的数学值, 然而可以这么说, eql 比较表征值. 


### <span id="F-MAX-MIN">函数 MAX, MIN</span>

* 语法(Syntax):

        max &rest reals+ => max-real

        min &rest reals+ => min-real

* 参数和值(Arguments and Values):

        real---一个实数[real].
        max-real, min-real---一个实数[real].

* 描述(Description):

        max 返回最大的实数 real (接近于正无穷). min 返回最小的实数 real (接近于负无穷).

        对于 max, 具体实现可以选择去原样返回最大的参数, 也可以应用浮点数传递性[contagion]规则, 将所有参数都列入传递性[contagion]目的的考虑中. 另外, 如果不止一个参数是 = 的, 那么它们中的任何一个都可能被选择作为返回值. 比如, 如果这些实数 reals 是有理数[rational]和浮点数[float]的混合, 并且最大值是个有理数[rational], 那么具体实现可以自由地产生有理数[rational]或它的近似浮点数[float]; 如果这个最大的参数是一个比任何其他浮点数[float]参数的最大格式都小的格式的浮点数[float], 那么具体实现可以自由地以给定的格式返回那个参数或者将其扩展到更大的格式. 相似的备注适用于 min (用 "最小参数" 替换 "最大参数").

* 示例(Examples):

    ```LISP
    (max 3) =>  3 
    (min 3) =>  3
    (max 6 12) =>  12 
    (min 6 12) =>  6
    (max -6 -12) =>  -6 
    (min -6 -12) =>  -12
    (max 1 3 2 -7) =>  3 
    (min 1 3 2 -7) =>  -7
    (max -2 3 0 7) =>  7 
    (min -2 3 0 7) =>  -2
    (max 5.0 2) =>  5.0 
    (min 5.0 2)
    =>  2
    OR=>  2.0
    (max 3.0 7 1)
    =>  7
    OR=>  7.0 
    (min 3.0 7 1)
    =>  1
    OR=>  1.0
    (max 1.0s0 7.0d0) =>  7.0d0
    (min 1.0s0 7.0d0)
    =>  1.0s0
    OR=>  1.0d0
    (max 3 1 1.0s0 1.0d0)
    =>  3
    OR=>  3.0d0
    (min 3 1 1.0s0 1.0d0)
    =>  1
    OR=>  1.0s0 
    OR=>  1.0d0
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何数字不是一个实数[real], 那么应该会发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MINUSP-PLUSP">函数 MINUSP, PLUSP</span>

* 语法(Syntax):

        minusp real => generalized-boolean

        plusp real => generalized-boolean

* 参数和值(Arguments and Values):

        real---一个实数[real].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果实数 real 小于 0 那么 minusp 返回 true; 否则, 返回 false.

        如果实数 real 大于 0 那么 plusp 返回 true; 否则, 返回 false.

        不管一个具体实现[implementation]是否提供了正负浮点数[float]零的不同表示, (minusp -0.0) 总是返回 false.

* 示例(Examples):

    ```LISP
    (minusp -1) =>  true
    (plusp 0) =>  false
    (plusp least-positive-single-float) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 real 不是一个实数[real], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="Function-ZEROP">函数 ZEROP</span>

* 语法(Syntax):

        zerop number => generalized-boolean

* 发音(Pronunciation):

        ['zee(,)roh(,)pee]

* 参数和值(Arguments and Values):

        number---一个数字[number].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果数字 number 是零 (整数[integer], 浮点数,[float] 或复数[complex]) 就返回 true; 否则, 返回 false.

        不管一个具体实现[implementation]是否提供了正负浮点数[float]零的不同表示, (zerop -0.0) 总是返回 true.

* 示例(Examples):

    ```LISP
    (zerop 0) =>  true
    (zerop 1) =>  false
    (zerop -0.0) =>  true
    (zerop 0/100) =>  true
    (zerop #c(0 0.0)) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 number 不是一个数字[number], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        (zerop number) ==  (= number 0)


### <span id="F-F-F-C-F-T-F-R-F">函数 FLOOR, FFLOOR, CEILING, FCEILING, TRUNCATE, FTRUNCATE, ROUND, FROUND</span>

* 语法(Syntax):

        floor number &optional divisor => quotient, remainder

        ffloor number &optional divisor => quotient, remainder

        ceiling number &optional divisor => quotient, remainder

        fceiling number &optional divisor => quotient, remainder

        truncate number &optional divisor => quotient, remainder

        ftruncate number &optional divisor => quotient, remainder

        round number &optional divisor => quotient, remainder

        fround number &optional divisor => quotient, remainder

* 参数和值(Arguments and Values):

        number---一个实数[real].
        divisor---一个非零实数[real]. 默认为整数[integer] 1.
        quotient---对于 floor, ceiling, truncate, 和 round: 一个整数[integer]; 对于 ffloor, fceiling, ftruncate, 和 fround: 一个浮点数[float].
        remainder---一个实数[real].

* 描述(Description):

        这些函数用 divisor 来除数字 number, 返回商 quotient 和余数 remainder, 比如

        quotient*divisor+remainder=number

        商 quotient 总是表示一个数学上的整数. 当一个以上的数学整数是可能的时候 (换句话说, 当余数不是零), 舍入或截断的类型取决于操作符[operator]:

        floor, ffloor

            floor 和 ffloor 产生一个朝向负无穷截断的商 quotient; 这也就是说, 这个商 quotient 表示不大于那个数学上商的最大数学整数.

        ceiling, fceiling

            ceiling 和 fceiling 产生一个朝向正无穷截断的商 quotient; 这也就是说, 商 quotient 表示不小于那个数学上结果的最小数学整数.

        truncate, ftruncate

            truncate 和 ftruncate 产生一个朝向 0 截断的商 quotient; 这也就是说, 这个商 quotient 表示和那个数学上的商相同符号的数学上的整数, 并且有着不大于那个数学上的商的最大整形大小.

        round, fround

            round 和 fround 产生一个四舍五入到最近的数学整数的商 quotient; 如果这个数学上的商刚好在两个整数中间, (也积极是说, 它有着 integer+1/2 的形式), 那么这个商 quotient 会被四舍五入到偶数 (可以被2整除).

        所有这些函数在这些数字 numbers 上执行类型转换操作.

        如果 x 和 y 都是整数[integer]那么余数 remainder 也是一个整数[integer], 如果 x 和 y 都是有理数[rational]那么余数 remainder 也是一个有理数[rational], 如果 x 或 y 是一个浮点数[float]那么余数 remainder 也是一个浮点数[float].

        ffloor, fceiling, ftruncate, 和 fround 以以下方式处理不同类型[type]的参数: 如果 number 是一个浮点数[float], 并且 divisor 不是一个长格式的浮点数[float], 那么第一个结果是一个和 number 相同类型[type]的浮点数[float]. 否则, 第一个结果是由传递性[contagion]规则决定的类型[type]; 见章节 12.1.1.2 (数值运算的传递性).

* 示例(Examples):

    ```LISP
    (floor 3/2) =>  1, 1/2
    (ceiling 3 2) =>  2, -1
    (ffloor 3 2) =>  1.0, 1
    (ffloor -4.7) =>  -5.0, 0.3
    (ffloor 3.5d0) =>  3.0d0, 0.5d0
    (fceiling 3/2) =>  2.0, -1/2
    (truncate 1) =>  1, 0
    (truncate .5) =>  0, 0.5
    (round .5) =>  0, 0.5
    (ftruncate -7 2) =>  -3.0, -1
    (fround -7 2) =>  -4.0, 1
    (dolist (n '(2.6 2.5 2.4 0.7 0.3 -0.3 -0.7 -2.4 -2.5 -2.6))
      (format t "~&~4,1@F ~2,' D ~2,' D ~2,' D ~2,' D"
              n (floor n) (ceiling n) (truncate n) (round n)))
    >>  +2.6  2  3  2  3
    >>  +2.5  2  3  2  2
    >>  +2.4  2  3  2  2
    >>  +0.7  0  1  0  1
    >>  +0.3  0  1  0  0
    >>  -0.3 -1  0  0  0
    >>  -0.7 -1  0  0 -1
    >>  -2.4 -3 -2 -2 -2
    >>  -2.5 -3 -2 -2 -2
    >>  -2.6 -3 -2 -2 -3
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        当只给定 number 时, 这两个结果是精确的; 这两个结果的数学和总是等于 number 的数学值.

        (function number divisor) 和 (function (/ number divisor)) (其中 function 是 floor, ceiling, ffloor, fceiling, truncate, round, ftruncate, 和 fround 中的任何一个) 返回相同的第一个值, 但是它们返回不同的余数作为第二个值. 比如:

        (floor 5 2) =>  2, 1
        (floor (/ 5 2)) =>  2, 1/2

        如果需要一个类似于 round 的效果, 但是在商刚好在两个整数中间时总是向上或向下舍入 (而不是朝向最接近的偶数), 程序员应该考虑像 (floor (+ x 1/2)) 或 (ceiling (- x 1/2)) 这样的构造. 


### <span id="F-SIN-COS-TAN">函数 SIN, COS, TAN</span>

* 语法(Syntax):

        sin radians => number

        cos radians => number

        tan radians => number

* 参数和值(Arguments and Values):

        radians---一个用弧度表示的数字[number].
        number---一个数字[number].

* 描述(Description):

        sin, cos, 和 tan 分别返回弧度 radians 正弦, 余弦和正切.

* 示例(Examples):

    ```LISP
    (sin 0) =>  0.0
    (cos 0.7853982) =>  0.707107
    (tan #c(0 1)) =>  #C(0.0 0.761594)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 radians 不是一个数字[number], 那么应该发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also):

        asin, acos, atan, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes): None. 

### <span id="F-ASIN-ACOS-ATAN">函数 ASIN, ACOS, ATAN</span>

* 语法(Syntax):

        asin number => radians

        acos number => radians

        atan number1 &optional number2 => radians

* 参数和值(Arguments and Values):

        number---一个数字[number].
        number1---如果没有提供 number2 就是一个数字[number], 如果提供了 number2 就是一个实数[real].
        number2---一个实数[real].
        radians---一个数字[number] (弧度).

* 描述(Description):

        asin, acos, 和 atan 分别计算反正弦, 反余弦和反正切.

        反正弦, 反余弦和反正切 (只提供了 number1 的情况下) 函数可以被数学上定义为 number 或 number1 被指定为下一段中的 x 那样.

            函数          定义                         
            Arc sine     -i log  (ix+ sqrt(1-x^2) )         
            Arc cosine   (<PI>/2) - arcsin  x               
            Arc tangent  -i log  ((1+ix) sqrt(1/(1+x^2)) )  

            Figure 12-14. 反正弦, 反余弦和反正切的数学上的定义

        这些公式在数学上是正确的, 假设计算完全准确. 它们不一定是实值计算中最简单的.

        如果 number1 和 number2 都提供给了 atan, 那么结果是 number1/number2 的反正切. 当没有提供负零时, atan 的值总是在 -<PI> (不包含) 到 <PI> (包含) 之间. 当支持负零时, 两个参数的反正切的范围包括 -<PI>.

        对于一个实数[real] number1, 结果也是一个实数[real]并且位于 -<PI>/2 和 <PI>/2 之间(都不包含). 如果没有提供 number2, 那么 number1 可以是一个复数[complex]. 如果都提供了, 假设 number1 不是零那么 number2 可以是零.

        下面反正弦的定义决定了范围和分支切割:

        arcsin z = -i log (iz+sqrt(1-z^2))

        反正弦函数的分支切割为两块: 一个沿着负实轴向 -1 的左边 (包含), 与第二象限连续, 而另一个沿着正实轴向 1 的右边 (包含), 与第四象限连续. 范围是包含实部在 -<PI>/2 和 <PI>/2 之间的数字的复平面的条带. 当切仅当一个数字的虚部时非负的的时候, 实部等价于 -<PI>/2 的这个数在这个范围内; 当切仅当一个数的虚部时非正的时候, 实部等价于 <PI>/2 的这个数在这个范围内.

        下面反余弦的定义决定了范围和分支切割:

        arccos z = <PI>/2- arcsin z

        或者, 这个也是等价的,

        arccos z = -i log (z+i sqrt(1-z^2))

        arccos z = 2 log (sqrt((1+z)/2) + i sqrt((1-z)/2))/i

        反余弦函数的分支切割为两块: 一个沿着负实轴向 -1 的左边 (包含), 与第二象限连续, 而另一个沿着正实轴向 1 的右边 (包含), 与第四象限连续. 这个和反正弦是相同的分支切割. 范围是包含了实部在 0 和 <PI> 之间的数字的复数带. 当切仅当一个数字的虚部时非负的的时候, 实部等价于 0 的这个数在这个范围内; 当切仅当一个数的虚部时非正的时候, 实部等价于 <PI> 的这个数在这个范围内.

        下面(单参数)反正切的定义决定了范围和分支切割:

        arctan z = log (1+iz) - log (1-iz)/(2i)

        注意不要简化这个公式; "明显" 的简化可能会改变分支切割或分支切割上的值不正确地修改. 反正切函数的分支切割为两块: 一个沿着正虚轴到 i 上 (不包含), 与第二象限连续, 一个沿着负虚轴到 -i 下 (exclusive), 与第四象限连续. 点 i 和 -i 被排除在这个域外. 范围是包含了实部在 -<PI>/2 和 <PI>/2 之间的数字的复数带. 当切仅当一个数字的虚部严格为正时, 实部等价于 -<PI>/2 的这个数在这个范围内; 当切仅当一个数字的虚部严格为负时, 实部等价于 <PI>/2 的这个数在这个范围内. 因此这个反正切的范围和反正弦的范围除了点 -<PI>/2 and <PI>/2 被排除之外是相同的.

        对于 atan, 这个 number1 (表示为 x) 和 number2 (表示为 y) 的符号被用于获得象限信息. 下一段详细描述了各种特殊情况. 星号 (*) 表示在这段的这个条目适用于支持负零的具体实现.

            y Condition  x Condition  Cartesian locus  Range of result         
            y = 0        x > 0        Positive x-axis  0                       
            * y = +0     x > 0        Positive x-axis  +0                      
            * y = -0     x > 0        Positive x-axis  -0                      
            y > 0        x > 0        Quadrant I       0 < result< <PI>/2      
            y > 0        x = 0        Positive y-axis  <PI>/2                  
            y > 0        x < 0        Quadrant II      <PI>/2 < result< <PI>   
            y = 0        x < 0        Negative x-axis  <PI>                    
            * y = +0     x < 0        Negative x-axis  +<PI>                   
            * y = -0     x < 0        Negative x-axis  -<PI>                   
            y < 0        x < 0        Quadrant III     -<PI>< result< -<PI>/2  
            y < 0        x = 0        Negative y-axis  -<PI>/2                 
            y < 0        x > 0        Quadrant IV      -<PI>/2 < result< 0     
            y = 0        x = 0        Origin           undefined consequences  
            * y = +0     x = +0       Origin           +0                      
            * y = -0     x = +0       Origin           -0                      
            * y = +0     x = -0       Origin           +<PI>                   
            * y = -0     x = -0       Origin           -<PI>                   

            Figure 12-15. 反正切的象限信息

* 示例(Examples):

    ```LISP
    (asin 0) =>  0.0 
    (acos #c(0 1))  =>  #C(1.5707963267948966 -0.8813735870195432)
    (/ (atan 1 (sqrt 3)) 6)  =>  0.087266 
    (atan #c(0 2)) =>  #C(-1.5707964 0.54930615)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果数字 number 不是一个数字[number], 那么 acos 和 asin 应该发出一个 type-error 类型[type]的错误. 如果提供了一个参数并且这个参数不是一个数字[number], 或者如果提供了两个参数而这两个参数都不是实数[real], 那么 atan 应该发出一个 type-error 类型[type]的错误.

        acos, asin, 和 atan 可能发出 arithmetic-error.

* 参见(See Also):

        log, sqrt, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        即便数字 number 不是一个复数[complex], asin 或 acos 的结果也可以是一个复数[complex]; 这个发生在数字 number 的绝对值大于 1 时. 


### <span id="CV-PI">常量 PI</span>

* 值(Value):

        一个依赖于具体实现[implementation-dependent]的长浮点数[long float].

* 描述(Description):

        对数学常数 <PI> 的最好的长浮点数[long float]近似.

* 示例(Examples):

    ```LISP
    ;; In each of the following computations, the precision depends 
    ;; on the implementation.  Also, if `long float' is treated by 
    ;; the implementation as equivalent to some other float format 
    ;; (e.g., `double float') the exponent marker might be the marker
    ;; for that equivalent (e.g., `D' instead of `L').
    pi =>  3.141592653589793L0
    (cos pi) =>  -1.0L0

    (defun sin-of-degrees (degrees)
      (let ((x (if (floatp degrees) degrees (float degrees pi))))
        (sin (* x (/ (float pi x) 180)))))
    ```

* 参见(See Also): None.

* 注意(Notes):

        一个对 <PI> 的某个其他精度的近似值可以通过编写 (float pi x) 来获取, 其中 x 是一个期望精度的浮点数[float], 或者编写 (coerce pi type), 其中 type 是期望的类型, 比如 short-float. 


### <span id="F-S-C-T-A-A-A">函数 SINH, COSH, TANH, ASINH, ACOSH, ATANH</span>

* 语法(Syntax):

        sinh number => result

        cosh number => result

        tanh number => result

        asinh number => result

        acosh number => result

        atanh number => result

* 参数和值(Arguments and Values):

        number---一个数字[number].
        result---一个数字[number].

* 描述(Description):

        这些函数计算双曲正弦, 余弦, 正切, 反正弦, 反余弦, 和反正切函数, 这些函数在数学上定义为在下一个图中给出一个参数 x 的形式.

            函数                    定义                              
            Hyperbolic sine         (e^x-e^-x)/2                            
            Hyperbolic cosine       (e^x+e^-x)/2                            
            Hyperbolic tangent      (e^x-e^-x)/(e^x+e^-x)                   
            Hyperbolic arc sine     log  (x+sqrt(1+x^2))                    
            Hyperbolic arc cosine   2 log  (sqrt((x+1)/2) + sqrt((x-1)/2))  
            Hyperbolic arc tangent  (log  (1+x) - log (1-x))/2              

            Figure 12-16. 双曲函数的数学定义

        下面这个反双曲余弦的定义决定了范围和分支切割:

        arccosh z = 2 log (sqrt((z+1)/2) + sqrt((z-1)/2)).

        反双曲余弦函数的分支切割沿着实轴到 1 的左边 (包含), 沿着负实轴无限延伸, 与第二象限 (在 0 和 1 之间) 和第一象限相连. 这个范围是包含了实部为非负并且虚部在 -<PI> (不包含) 和 <PI> (包含) 之间的数字的复平面的半条. 如果一个数字的虚部在 0 (包含) 和 <PI> (包含) 之间, 那么实部为 0 的这个数在这个范围内.

        下面这个反双曲正弦的定义决定了范围和分支切割:

        arcsinh z = log (z+sqrt(1+z^2)).

        这个反双曲正弦函数的分支切割分为两块: 一个沿着正虚轴到 i (包含) 上, 与第一象限相连, 而另一个沿着负虚轴到 -i (包含) 下, 与第三象限相连. 这个范围是包含虚部在 -<PI>/2 和 <PI>/2 之间的数字的复数带. 当且仅当一个实部为非负时, 这个虚部与 -<PI>/2 相等的复数在这个范围内; 当且仅当一个数字的虚部为非负时, 这个虚部等价于 <PI>/2 的数字在这个范围内.

        下面这个反双曲正切的定义决定了范围和分支切割:

        arctanh z = log (1+z) - log (1-z)/2.

        注意:

        i arctan z = arctanh iz.

        这个反双曲正切函数的分支切割分为两块: 一个沿着负实轴到one along the negative real -1 (包含) 的左边, 与第三象限相连, 而另一个沿着正实轴到 1 (包含) 的右边, 与第一象限相连. 点 -1 和 1 被排除在这个域外. 这个范围是包含虚部在 -<PI>/2 和 <PI>/2 之间的数字的复数带. 当且仅当一个数字的实部是严格负的时, 这个虚部等于 -<PI>/2 的数字在这个范围内; 当且仅当一个数字的虚部是严格正的时, 这个虚部等于 <PI>/2 的数字在这个范围内. 因此反双曲正切函数的范围除了点 points -<PI>i/2 和 <PI>i/2 被排除之外和反向双曲正切函数是相同的.

* 示例(Examples):

    ```LISP
    (sinh 0) =>  0.0 
    (cosh (complex 0 -1)) =>  #C(0.540302 -0.0)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 number 不是一个数字[number], 那么应该发出一个 type-error 类型[type]的错误. 也可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also):

        log, sqrt, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        即便 number 不是一个复数[complex], acosh 的结果也可以是一个复数[complex]; 这个发生在 number 小于 1 时. 同样, 即便 number 不是一个复数[complex], atanh 的结果也可以是一个复数[complex]; 这个发生在 number 的绝对值大于 1 时.

        假设计算完全准确, 分支切割公式在数学上是正确的. 实现者应该在数值分析中参考一个好的文本. 上面给出的公式并不一定是实数值计算最简单的公式; 它们被选择以合适的方式为复杂的案例定义分支切割. 


### <span id="F-Multiply">函数 *</span>

* 语法(Syntax):

        * &rest numbers => product

* 参数和值(Arguments and Values):

        number---一个数字[number].
        product---一个数字[number].

* 描述(Description):

        返回那些数字 numbers 的乘积, 在这个过程中执行任何必要的类型转换. 如果没有提供数字 numbers, 返回 1.

* 示例(Examples):

    ```LISP
    (*) =>  1
    (* 3 5) =>  15
    (* 1.0 #c(22 33) 55/98) =>  #C(12.346938775510203 18.520408163265305)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果某个实参[argument]不是一个数字[number], 可能发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also):

        章节 12.1.1 (数值运算), 章节 12.1.3 (有理数计算), 章节 12.1.4 (浮点计算), 章节 12.1.5 (复数计算)

* 注意(Notes): None. 


### <span id="F-Add">函数 +</span>

* 语法(Syntax):

        + &rest numbers => sum

* 参数和值(Arguments and Values):

        number---一个数字[number].
        sum---一个数字[number].

* 描述(Description):

        返回数字 numbers 的和, 在这个过程中执行任何必要的类型转换. 如果没有提供 numbers, 返回 0.

* 示例(Examples):

    ```LISP
    (+) =>  0
    (+ 1) =>  1
    (+ 31/100 69/100) =>  1
    (+ 1/5 0.8) =>  1.0
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果某个实参[argument]不是一个数字[number], 可能发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also):

        章节 12.1.1 (数值运算), 章节 12.1.3 (有理数计算), 章节 12.1.4 (浮点计算), 章节 12.1.5 (复数计算)

* 注意(Notes): None. 


### <span id="F-Sub">函数 -</span>

* 语法(Syntax):

        - number => negation

        - minuend &rest subtrahends+ => difference

* 参数和值(Arguments and Values):

        number, minuend, subtrahend---一个数字[number].
        negation, difference---一个数字[number].

* 描述(Description):

        函数[function] - 执行数学减法和取反.

        如果只提供了一个 number, 对这个数字 number 的相反数被返回.

        如果给定了不止一个实参[argument], 它从被减数 minuend 减去所有的减数 subtrahends 并返回这个结果.

        函数[function] - 执行必要的类型转换.

* 示例(Examples):

    ```LISP
    (- 55.55) =>  -55.55
    (- #c(3 -5)) =>  #C(-3 5)
    (- 0) =>  0
    (eql (- 0.0) -0.0) =>  true
    (- #c(100 45) #c(0 45)) =>  100
    (- 10 1 2 3 4) =>  0
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果某个实参[argument]不是一个数字[number], 可能发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error 类型[type]的错误.

* 参见(See Also):

        章节 12.1.1 (数值运算), 章节 12.1.3 (有理数计算), 章节 12.1.4 (浮点计算), 章节 12.1.5 (复数计算)

* 注意(Notes): None. 


### <span id="F-Div">函数 /</span>

* 语法(Syntax):

        / number => reciprocal

        / numerator &rest denominators+ => quotient

* 参数和值(Arguments and Values):

        number, denominator---一个非零数字[number].
        numerator, quotient, reciprocal---一个数字[number].

* 描述(Description):

        函数[function] / 执行除法或取倒数.

        如果没有提供分母 denominator, 那么函数[function] / 返回数字 number 的倒数.

        如果至少提供了一个分母 denominator, 函数[function] / 用所有分母 denominators 去除这个分子 numerator 并返回产生的商 quotient.

        如果每个实参[argument]都是一个整数[integer]或一个比数[ratio], 如果结果不是一个整数[integer], 那么就是一个比数[ratio].

        函数[function] / 执行必要的类型转换.

        如果任何实参 argument 是一个浮点数[float]那么就应用浮点数传递性规则; 见章节 12.1.4 (浮点计算).

* 示例(Examples):

    ```LISP
    (/ 12 4) =>  3
    (/ 13 4) =>  13/4
    (/ -8) =>  -1/8
    (/ 3 4 5) =>  3/20
    (/ 0.5) =>  2.0
    (/ 20 5) =>  4
    (/ 5 20) =>  1/4
    (/ 60 -2 3 5.0) =>  -2.0
    (/ 2 #c(2 2)) =>  #C(1/2 -1/2)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果除了第一个以外的任何实参[argument]是 0 那么后果是不确定的. 如果这里只有一个实参[argument], 如果它是 0 那么后果是不确定的.

        如果某个实参[argument]不是一个数字[number], 可能发出一个 type-error 类型[type]的错误. 如果尝试去用 0 除可能发出一个 division-by-zero. 可能发出 arithmetic-error.

* 参见(See Also):

        floor, ceiling, truncate, round

* 注意(Notes): None. 

### <span id="F-OP-OI">函数 1+, 1-</span>

* 语法(Syntax):

        1+ number => successor

        1- number => predecessor

* 参数和值(Arguments and Values):

        number---一个数字[number].
        successor, predecessor---一个数字[number].

* 描述(Description):

        1+ 返回比它的参数 number 多一的数字[number]. 1- 返回比它的参数 number 少一的数字[number].

* 示例(Examples):

    ```LISP
    (1+ 99) =>  100 
    (1- 100) =>  99 
    (1+ (complex 0.0)) =>  #C(1.0 0.0) 
    (1- 5/3) =>  2/3 
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的实参[argument]不是一个数字[number], 那么可能发出 type-error 类型[type]的错误. 可能发出 arithmetic-error.

* 参见(See Also):

        incf, decf

* 注意(Notes):

        (1+ number) ==  (+ number 1)
        (1- number) ==  (- number 1)

        我们鼓励实现者使前面两个表达式的性能相同. 


### <span id="F-ABS">函数 ABS</span>

* 语法(Syntax):

        abs number => absolute-value

* 参数和值(Arguments and Values):

        number---一个数字[number].
        absolute-value---一个非负实数[real].

* 描述(Description):

        abs 返回数字 number 的绝对值.
 
        如果 number 是一个实数[real], 结果是和 number 相同类型[type]的.

        如果 number 是一个复数[complex], 结果是和 number 相同大小的一个正实数[real]. 即便这个数字 number 的部件是有理数[rational], 结果可以是一个浮点数[float], 而一个准确的有理数结果也是可能的. 因此 (abs #c(3 4)) 的结果可能是 5 或 5.0, 取决于具体实现.

* 示例(Examples):

    ```LISP
    (abs 0) =>  0
    (abs 12/13) =>  12/13
    (abs -1.09) =>  1.09
    (abs #c(5.0 -5.0)) =>  7.071068
    (abs #c(5 5)) =>  7.071068
    (abs #c(3/5 4/5)) =>  1 or approximately 1.0
    (eql (abs -0.0) -0.0) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        如果数字 number 是一个复数[complex], 结果等价于以下:

        (sqrt (+ (expt (realpart number) 2) (expt (imagpart number) 2)))

        一个具体实现不应该直接将这个公式用于所有的复数[complex], 但是应该处理非常大或非常小的组件以避免中间的上溢或下溢. 

### <span id="F-EVENP-ODDP">函数 EVENP, ODDP</span>

* 语法(Syntax):

        evenp integer => generalized-boolean

        oddp integer => generalized-boolean

* 参数和值(Arguments and Values):

        integer---一个整数[integer].

        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果整数 integer 是偶数 (可以被 2 整除) evenp 返回 true; 否则, 返回 false.

        如果整数 integer 是奇数 (不能被 2 整除) oddp 返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (evenp 0) =>  true
    (oddp 10000000000000000000000) =>  false
    (oddp -1) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 integer 不是一个整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        (evenp integer) ==  (not (oddp integer))
        (oddp integer)  ==  (not (evenp integer))


### <span id="F-EXP-EXPT">函数 EXP, EXPT</span>

* 语法(Syntax):

        exp number => result

        expt base-number power-number => result

* 参数和值(Arguments and Values):

        number---一个数字[number].
        base-number---一个数字[number].
        power-number---一个数字[number].
        result---一个数字[number].

* 描述(Description):

        exp 和 expt 执行求幂.

        exp 将 e 提升到幂 number 并返回, e 是自然对数的底数. exp 没有分支切割.

        expt 将 base-number 提升到幂 power-number 并返回. 如果这个 base-number 是一个有理数[rational]而 power-number 是一个整数[integer], 计算是准确的, 结果将是 rational 类型[type]的; 否则可能产生一个浮点近似值. 对于一个复有理数[complex rational]到整数[integer]幂的 expt, 计算一定是准确的并且结果是 (or rational (complex rational)) 类型的.

        即便每个参数都不是复数[complex], 如果 base-number 是负的并且 power-number 不是一个整数[integer], expt 的结果仍可以是一个复数[complex]. 结果总是为首要[principal]复数[complex]值[value]. 比如, (expt -8 1/3) 不允许返回 -2, 尽管 -2 是 -8 的立方根之一. 首要[principal]立方根是一个近似等于 #C(1.0 1.73205), 而不是 -2 的复数[complex].

        expt 被定义为 b^x = e^x log b. 这就精确地定义了首要[principal]值[value]. expt 的范围是整个复数域. 视为 x 的函数, 其中 b 固定, 这里没有分支切割. 视为 b 的函数, 其中 x 固定, 这里通常没有沿着负实轴和第二象限相连的分支切割. 这个区域排除了原点. 按照定义, 0^0=1. 如果 b=0 和 x 的实部是严格正的, 那么 b^x=0. 对于所有其他 x 的值, 0^x 是一个错误.

        当 power-number 是一个整数[integer] 0, 那么结果总是为 base-number 的类型[type]的值 1, 即便 base-number 是 0 (任何类型[type]的). 就是说:

        (expt x 0) ==  (coerce 1 (type-of x))

        如果 power-number 是一个其他任何类型[type]的 0, 那么结果也是值 1, 以章节 12.1.1.2 (数值运算的传递性) 的传递性规则应用后的参数类型[type], 有一个例外: 如果 base-number 是 0 当 power-number 是 zero 并且不是 integer 类型[type]时, 后果是未定义的.

* 示例(Examples):

    ```LISP
    (exp 0) =>  1.0
    (exp 1) =>  2.718282
    (exp (log 5)) =>  5.0 
    (expt 2 8) =>  256
    (expt 4 .5) =>  2.0
    (expt #c(0 1) 2) =>  -1
    (expt #c(2 2) 3) =>  #C(-16 16)
    (expt #c(2 2) 4) =>  -64 
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        log, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        expt 的具体实现允许去为一个 rational 类型[type]的 power-number 和一个 float 类型[type]的 power-number 的情况使用不同的算法.

        请注意以下逻辑, (sqrt (expt x 3)) 不等价于 (expt x 3/2).

        (setq x (exp (/ (* 2 pi #c(0 1)) 3)))         ;exp(2.pi.i/3)
        (expt x 3) =>  1 ;except for round-off error
        (sqrt (expt x 3)) =>  1 ;except for round-off error
        (expt x 3/2) =>  -1 ;except for round-off error



### <span id="F-GCD">函数 GCD</span>

* 语法(Syntax):

        gcd &rest integers => greatest-common-denominator

* 参数和值(Arguments and Values):

        integer---一个整数[integer].
        greatest-common-denominator---一个非负整数[integer].

* 描述(Description):

        返回这些整数 integers 的最大公约数. 如果只提供了一个整数 integer, 返回它的绝对值. 如果没有提供整数 integers, gcd 返回 0, 这是这个操作符的一个特性.

* 示例(Examples):

    ```LISP
    (gcd) =>  0
    (gcd 60 42) =>  6
    (gcd 3333 -33 101) =>  1
    (gcd 3333 -33 1002001) =>  11
    (gcd 91 -49) =>  7
    (gcd 63 -42 35) =>  7
    (gcd 5) =>  5
    (gcd -4) =>  4
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何一个 integer 不是一个整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        lcm

* 注意(Notes):

        对于三个或更多参数,

        (gcd b c ... z) ==  (gcd (gcd a b) c ... z)


### <span id="M-INCF-DECF">宏 INCF, DECF</span>

* 语法(Syntax):

        incf place [delta-form] => new-value

        decf place [delta-form] => new-value

* 参数和值(Arguments and Values):

        place---一个位置[place].
        delta-form---一个表达式形式[form]; 用来求值产生一个差值 delta. 默认是 1.
        delta---一个数字[number].
        new-value---一个数字[number].

* 描述(Description):

        incf 和 decf 分别被用于递增或递减位置 place 的值[value].

        这个差值 delta 会被添加到 place 中的数 (在 incf 的情况中) 或者从 place 中的数中减去 (在 decf 的情况中) 然后结果存储到 place 中.

        自动执行任何必要的类型转换.

        关于 places 的子表达式形式[subform]的求值[evaluation]信息, 见章节 5.1.1.1 (位置的子表达式形式求值).

* 示例(Examples):

    ```LISP
    (setq n 0)
    (incf n) =>  1      
    n =>  1
    (decf n 3) =>  -2   
    n =>  -2
    (decf n -5) =>  3      
    (decf n) =>  2      
    (incf n 0.5) =>  2.5
    (decf n) =>  1.5
    n =>  1.5
    ```

* 副作用(Side Effects):

        place 会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        +, -, 1+, 1-, setf

* 注意(Notes): None. 

### <span id="F-LCM">函数 LCM</span>

* 语法(Syntax):

        lcm &rest integers => least-common-multiple

* 参数和值(Arguments and Values):

        integer---一个整数[integer].
        least-common-multiple---一个非负整数[integer].

* 描述(Description):

        lcm 返回这些整数 integers 的最小公倍数.

        如果没有提供整数 integer, 返回整数[integer] 1.

        如果只提供一个整数 integer, 返回那个整数 integer 的绝对值.

        对于两个不是 0 的参数,

        (lcm a b) ==  (/ (abs (* a b)) (gcd a b))

        如果一个参数为 0 或两个参数都为 0,

        (lcm a 0) ==  (lcm 0 a) ==  0

        对于三个或更多参数,

        (lcm a b c ... z) ==  (lcm (lcm a b) c ... z)

* 示例(Examples):

    ```LISP
    (lcm 10) =>  10
    (lcm 25 30) =>  150
    (lcm -24 18 10) =>  360
    (lcm 14 35) =>  70
    (lcm 0 5) =>  0
    (lcm 1 2 3 4 5 6) =>  60
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何一个参数不是整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        gcd

* 注意(Notes): None. 


### <span id="F-LOG">函数 LOG</span>

* 语法(Syntax):

        log number &optional base => logarithm

* 参数和值(Arguments and Values):

        number---一个非零数字[number].
        base---一个数字[number].
        logarithm---一个数字[number].

* 描述(Description):

        log 返回数字 number 在基数 base 下的对数. 如果没有提供基数 base, 那么它的值就是 e, 也就是那个自然对数的基数.

        当给定一个负实数[real] number 时, log 可能返回一个复数[complex].

        (log -1.0) ==  (complex 0.0 (float pi 0.0))

        如果基数 base 是 zero, log 返回 zero.

        (log 8 2) 的结果可能是 3 或 3.0, 取决于具体实现. 即便一个整数结果是可能的, 一个具体实现仍可以使用浮点计算.

        这个单参数的对数函数 (自然对数) 的分支切割位于沿着负实轴, 与第二象限相连的区域. 这个区域不包括原点.

        复对数的数学定义如下, 不管具体实现是否支持负零:

        (log x) ==  (complex (log (abs x)) (phase x))

        因此这个单参数对数函数的范围是那个在不支持负零时包含虚部在 -<PI> (不包含) 和 <PI> (包含) 之间的复数带以及支持负零时包含虚部在 -<PI> (包含) 和 <PI> (包含) 之间的复数带.

        两个参数的对数函数被定义为

        (log base number)
        ==  (/ (log number) (log base))

        这就精确地定义了首要[principal]值[value]. 这个两个参数的对数函数的范围是整个复数域.

* 示例(Examples):

    ```LISP
    (log 100 10)
    =>  2.0
    =>  2
    (log 100.0 10) =>  2.0
    (log #c(0 1) #c(0 -1))
    =>  #C(-1.0 0.0)
    OR=>  #C(-1 0)
    (log 8.0 2) =>  3.0

    (log #c(-16 16) #c(2 2)) =>  3 or approximately #c(3.0 0.0)
                                  or approximately 3.0 (unlikely)
    ```

* 受此影响(Affected By):

        具体实现.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        exp, expt, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes): None. 

### <span id="F-MOD-REM">函数 MOD, REM</span>

* 语法(Syntax):

        mod number divisor => modulus

        rem number divisor => remainder

* 参数和值(Arguments and Values):

        number---一个实数[real].
        divisor---一个实数[real].
        modulus, remainder---一个实数[real].

* 描述(Description):

        mod 和 rem 分别是模量和剩余函数的泛化.

        mod 在数字 number 和除数 divisor 上执行操作 floor 并且返回这个 floor 操作的余数.

        rem 在数字 number 和除数 divisor 上执行操作 truncate 并且返回这个 truncate 操作的余数.

        当数字 number 和除数 divisor 都是整数[integer]时, mod 和 rem 是模量和剩余函数.

* 示例(Examples):

    ```LISP
    (rem -1 5) =>  -1
    (mod -1 5) =>  4
    (mod 13 4) =>  1
    (rem 13 4) =>  1
    (mod -13 4) =>  3
    (rem -13 4) =>  -1
    (mod 13 -4) =>  -3
    (rem 13 -4) =>  1
    (mod -13 -4) =>  -1
    (rem -13 -4) =>  -1
    (mod 13.4 1) =>  0.4
    (rem 13.4 1) =>  0.4
    (mod -13.4 1) =>  0.6
    (rem -13.4 1) =>  -0.4
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        floor, truncate

* 注意(Notes):

        mod 的结果是 0 或一个带有和 divisor 相同符号的实数. 

### <span id="F-SIGNUM">函数 SIGNUM</span>

* 语法(Syntax):

        signum number => signed-prototype

* 参数和值(Arguments and Values):

        number---一个数字[number].
        signed-prototype---一个数字[number].

* 描述(Description):

        signum 确定一个表示数字 number 是否为负数, 0, 或正数的一个数字值.

        对于一个有理数[rational], signum 根据数字 number 是否为负数, 0, 或正数来返回 -1, 0, 或 1 的其中一个. 对于一个浮点数[float], 结果是一个相同格式的浮点数[float], 它的值为负一, 零, 或一. 对于一个复数[complex] z, (signum z) 是同阶的单位大小的复数, 但单位大小, 除非 z 是复数 0, 在这种情况下, 结果是 z.

        对于有理数[rational]实参[argument], signum 是一个有理数学函数, 但是它对于一个复数参数可能是无理数的.

        如果数字 number 是一个浮点数[float], 结果是一个浮点数[float]. 如果数字 number 是一个有理数[rational], 结果就是一个有理数[rational]. 如果数字 number 是一个复浮点数[complex float], 结果就是一个复浮点数[complex float]. 如果数字 number 是一个复有理数[complex rational], 结果就是一个复数[complex], 但是它的结果是一个复有理数[complex rational]还是一个负浮点数[complex float]是依赖于具体实现的[implementation-dependent].

* 示例(Examples):

    ```LISP
    (signum 0) =>  0
    (signum 99) =>  1
    (signum 4/5) =>  1
    (signum -99/100) =>  -1
    (signum 0.0) =>  0.0
    (signum #c(0 33)) =>  #C(0.0 1.0)
    (signum #c(7.5 10.0)) =>  #C(0.6 0.8)
    (signum #c(0.0 -14.7)) =>  #C(0.0 -1.0)
    (eql (signum -0.0) -0.0) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        (signum x) ==  (if (zerop x) x (/ x (abs x)))



### <span id="F-SQRT-ISQRT">函数 SQRT, ISQRT</span>

* 语法(Syntax):

        sqrt number => root

        isqrt natural => natural-root

* 参数和值(Arguments and Values):

        number, root---一个数字[number].
        natural, natural-root---一个非负整数[integer].

* 描述(Description):

        sqrt 和 isqrt 计算平方根.

        sqrt 返回数字 number 的首要[principal]平方根. 如果数字 number 不是一个复数[complex]但是是负的, 那么结果就是一个复数[complex].

        isqrt 返回小于等于自然数 natural 准确平方根的最大整数[integer].

        如果数字 number 是一个正有理数[rational], 那么平方根 root 是一个有理数[rational]还是一个浮点数[rational]是依赖于具体实现的[implementation-dependent]. 如果数字 number 是一个负有理数[rational], 那么平方根 root 是一个复有理数[complex rational]还是一个复浮点数[complex float].

        复数平方根的数学定义 (不管是否支持 -0)) 如下:

        (sqrt x) = (exp (/ (log x) 2))

        平方根的分支切割位于沿着负实轴, 与第二象限相连. 这个返回由右半平面组成, 包括非负的虚轴, 不包括负的虚轴.

* 示例(Examples):

    ```LISP
    (sqrt 9.0) =>  3.0
    (sqrt -9.0) =>  #C(0.0 3.0)
    (isqrt 9) =>  3
    (sqrt 12) =>  3.4641016
    (isqrt 12) =>  3
    (isqrt 300) =>  17
    (isqrt 325) =>  18
    (sqrt 25)
    =>  5
    OR=>  5.0
    (isqrt 25) =>  5
    (sqrt -1) =>  #C(0.0 1.0)
    (sqrt #c(0 2)) =>  #C(1.0 1.0)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果参数不是一个数字[number], 函数[function] sqrt 应该发出一个 type-error 类型[type]的错误.

        如果参数不是一个非负整数[integer], 函数[function] isqrt 应该发出一个 type-error 类型[type]的错误.

        函数 sqrt 和 isqrt 可能发出 arithmetic-error.

* 参见(See Also):

        exp, log, 章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes):

        (isqrt x) ==  (values (floor (sqrt x))) 

        但它可能更有效率. 


### <span id="SC-RANDOM-STATE">系统类 RANDOM-STATE</span>

* 类优先级列表(Class Precedence List):

        random-state, t

* 描述(Description):

        一个随机状态[random state]对象[object]包含了被伪随机数字生成器使用的状态信息. 一个随机状态[random state]对象[object]的性质是依赖于具体实现的[mplementation-dependent]. 它可以被打印出来并且被相同的具体实现[implementation]读回去, 但是在另一个实现[implementation]中可能不能正确地作为一个随机状态随机状态[random state]运行.

        具体实现[implementation]需要去为 random-state 类型[type]的对象[object]提供一个读取语法, 但是那个语法的具体的特性是依赖于具体实现的[implementation-dependent].

* 参见(See Also):

        *random-state*, random, 章节 22.1.3.10 (打印随机状态) 


### <span id="F-MAKE-RANDOM-STATE">函数 MAKE-RANDOM-STATE</span>

* 语法(Syntax):

        make-random-state &optional state => new-state

* 参数和值(Arguments and Values):

        state---一个随机状态[random state], 或 nil, 或 t. 默认是 nil.
        new-state---一个随机状态[random state]对象[object].

* 描述(Description):

        创建一个适合用作 *random-state* 的值[value]的 random-state 类型[type]的新[fresh]对象[object].

        如果状态 state 是一个随机状态[random state]对象[object], 那么这个 new-state 是那个对象[object]的一个拷贝[copy[5]]. 如果状态 state 是 nil, 新的状态 new-state 是当前随机状态[current random state]的一个拷贝[copy[5]]. 如果状态 state 是 t, 那么新状态 new-state 是一个通过某种方式被随机初始化的新的[fresh]随机状态[random state]对象[object].

* 示例(Examples):

    ```LISP
    (let* ((rs1 (make-random-state nil))
            (rs2 (make-random-state t))
            (rs3 (make-random-state rs2))
            (rs4 nil))
      (list (loop for i from 1 to 10 
                  collect (random 100)
                  when (= i 5)
                    do (setq rs4 (make-random-state)))
            (loop for i from 1 to 10 collect (random 100 rs1))
            (loop for i from 1 to 10 collect (random 100 rs2))
            (loop for i from 1 to 10 collect (random 100 rs3))
            (loop for i from 1 to 10 collect (random 100 rs4))))
    =>  ((29 25 72 57 55 68 24 35 54 65)
        (29 25 72 57 55 68 24 35 54 65)
        (93 85 53 99 58 62 2 23 23 59)
        (93 85 53 99 58 62 2 23 23 59)
        (68 24 35 54 65 54 55 50 59 49))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 state 不是一个随机状态[random state], 或 nil, 或 t, 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        random, *random-state*

* 注意(Notes):

        make-random-state 一个重要的使用是允许在单个程序中同样的一系列伪随机数字[number]被生成多次. 

### <span id="F-RANDOM">函数 RANDOM</span>

* 语法(Syntax):

        random limit &optional random-state => random-number

* 参数和值(Arguments and Values):

        limit---一个正整数[integer], 或者一个正浮点数[float].
        random-state---一个随机状态[random state]. 默认是当前随机状态[current random state].
        random-number---一个小于 limit 并且和 limit 相同类型[type]的非负数字[number].

* 描述(Description):

        返回一个小于 limit 并且和 limit 相同类型[type]的非负伪随机数字[number].

        被这个函数修改的随机状态 random-state 编码由这个随机数生成器维护的内部状态.

        使用了一个近似均匀的选择分布. 如果 limit 是一个整数[integer], 每一个可能的结果发生的可能性 (近似) 是 1/limit.

* 示例(Examples):

    ```LISP
    (<= 0 (random 1000) 1000) =>  true
    (let ((state1 (make-random-state))
          (state2 (make-random-state)))
      (= (random 1000 state1) (random 1000 state2))) =>  true
    ```

* 副作用(Side Effects):

        这个随机状态 random-state 被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 limit 不是一个正整数[integer]或一个正实数[real], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        make-random-state, *random-state*

* 注意(Notes):

        关于生成随机数的信息见 Common Lisp: The Language. 


### <span id="F-RANDOM-STATE-P">函数 RANDOM-STATE-P</span>

* 语法(Syntax):

        random-state-p object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 random-state 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (random-state-p *random-state*) =>  true
    (random-state-p (make-random-state)) =>  true
    (random-state-p 'test-function) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        make-random-state, *random-state*

* 注意(Notes):

        (random-state-p object) ==  (typep object 'random-state)


### <span id="V-RANDOM-STATE">变量 *RANDOM-STATE*</span>

* 值类型(Value Type):

        一个随机状态[random state].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        当前随机状态[current random state], 比如, 当一个随机状态[random state]没有提供给 random 函数[function]时, 它会被使用.

* 示例(Examples):

    ```LISP
    (random-state-p *random-state*) =>  true
    (setq snap-shot (make-random-state))
    ;; The series from any given point is random,
    ;; but if you backtrack to that point, you get the same series.
    (list (loop for i from 1 to 10 collect (random))
          (let ((*random-state* snap-shot))
            (loop for i from 1 to 10 collect (random)))
          (loop for i from 1 to 10 collect (random))
          (let ((*random-state* snap-shot))
            (loop for i from 1 to 10 collect (random))))
    =>  ((19 16 44 19 96 15 76 96 13 61)
        (19 16 44 19 96 15 76 96 13 61)
        (16 67 0 43 70 79 58 5 63 50)
        (16 67 0 43 70 79 58 5 63 50))
    ```

* 受此影响(Affected By):

        具体实现[implemnetation].

        random.

* 参见(See Also):

        make-random-state, random, random-state

* 注意(Notes):

        将 *random-state* 绑定[binding]到一个不同的随机状态[random state]对象[object]时, 正确地保存和还原旧的随机状态[random state]对象[object]. 


### <span id="F-NUMBERP">函数 NUMBERP</span>

* 语法(Syntax):

        numberp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 number 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (numberp 12) =>  true
    (numberp (expt 2 130)) =>  true
    (numberp #c(5/3 7.2)) =>  true
    (numberp nil) =>  false
    (numberp (cons 1 2)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (numberp object) ==  (typep object 'number)


### <span id="F-CIS">函数 CIS</span>

* 语法(Syntax):

        cis radians => number

* 参数和值(Arguments and Values):

        radians---一个实数[real].
        number---一个复数[complex].

* 描述(Description):

        cis 返回 e^i* radians 的值, 它是一个实部等价于弧度 radians 的余弦并且虚部等价于弧度 radians 的正弦的复数[complex].

* 示例(Examples):

    ```LISP
    (cis 0) =>  #C(1.0 0.0)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes): None. 

### <span id="F-COMPLEX">函数 COMPLEX</span>

* 语法(Syntax):

        complex realpart &optional imagpart => complex

* 参数和值(Arguments and Values):

        realpart---一个实数[real].
        imagpart---一个实数[real].
        complex---一个有理数[rational]或一个复数[complex].

* 描述(Description):

        complex 返回一个实部为 realpart 并且虚部为 imagpart 的数字[number].

        如果 realpart 是一个有理数[rational]并且虚部是一个有理数[rational] 0, 那么 complex 结果是一个有理数[rational] realpart. 否则, 结果就是一个复数[complex].

        如果 realpart 或 imagpart 任意一个是浮点数[float], 那么非浮点数在这个复数[complex]被创建之前被转化为一个浮点数[float]. 如果 imagpart 没有被提供, 这个虚部就是和实部相同类型[type]的零; 换句话说, 实际使用 (coerce 0 (type-of realpart)).

        类型提升意味着在类型层次结构中向上移动. 在复数[complex]的情况下, 这个类型指定符 type-specifier 必须是 (upgraded-complex-part-type type-specifier) 的子类型. 如果类型指定符 type-specifier1 是类型指定符 type-specifier2 的子类型, 那么 (upgraded-complex-element-type 'type-specifier1) 必须也是 (upgraded-complex-element-type 'type-specifier2) 的一个子类型. 两个互斥的类型可以被提升到同一个类型.

* 示例(Examples):

    ```LISP
    (complex 0) =>  0
    (complex 0.0) =>  #C(0.0 0.0)
    (complex 1 1/2) =>  #C(1 1/2)
    (complex 1 .99) =>  #C(1.0 0.99)
    (complex 3/2 0.0) =>  #C(1.5 0.0)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        realpart, imagpart, 章节 2.4.8.11 (井号C(#C))

* 注意(Notes): None. 


### <span id="F-COMPLEXP">函数 COMPLEXP</span>

* 语法(Syntax):

        complexp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是类型[type] complex 就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (complexp 1.2d2) =>  false
    (complexp #c(5/3 7.2)) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        complex (函数[function]和类型[type]), typep

* 注意(Notes):

        (complexp object) ==  (typep object 'complex)


### <span id="F-CONJUGATE">函数 CONJUGATE</span>

* 语法(Syntax):

        conjugate number => conjugate

* 参数和值(Arguments and Values):

        number---一个数字[number].
        conjugate---一个数字[number].

* 描述(Description):

        返回数字 number 的复变位数(conjugate). 一个实数[real]的复变位数(conjugate) 是它自身.

* 示例(Examples):

    ```LISP
    (conjugate #c(0 -1)) =>  #C(0 1)
    (conjugate #c(1 1)) =>  #C(1 -1)
    (conjugate 1.5) =>  1.5
    (conjugate #C(3/5 4/5)) =>  #C(3/5 -4/5)
    (conjugate #C(0.0D0 -1.0D0)) =>  #C(0.0D0 1.0D0)
    (conjugate 3.7) =>  3.7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        对于一个复数[complex] z,

        (conjugate z) ==  (complex (realpart z) (- (imagpart z)))


### <span id="F-PHASE">函数 PHASE</span>

* 语法(Syntax):

        phase number => phase

* 参数和值(Arguments and Values):

        number---一个数字[number].
        phase---一个数字[number].

* 描述(Description):

        phase 返回表示弧度的数字 number 的相位 (它的极坐标表示的角度部分), 如果不支持负零, 那么范围就在 -<PI> (不包含) 到 <PI> (包含) 之间, 如果支持负零, 那么返回就在 -<PI> (包含) 到 <PI> (包含) 之间. 正实数的相位是零; 对于复实数就是 <PI>. 零的相位被定义为零.

        如果数字 number 是一个复浮点数[complex float], 结果就是和 number 各部分相同类型[type]的浮点数[float]. 如果数字 number 是一个浮点数[float], 结果就是一个相同类型[type]的浮点数[float]. 如果数字 number 是一个有理数[rational]或一个复有理数[complex rational], 结果就是一个单浮点数[single float].

        phase 的分支切割位于沿着复实轴, 与第二象限相连的部分. 范围由 -<PI> (不包含) 和 <PI> (不包含) 之间的实轴组成.

        这个 phase 的数学定义如下:

        (phase x) = (atan (imagpart x) (realpart x))

* 示例(Examples):

    ```LISP
    (phase 1) =>  0.0s0
    (phase 0) =>  0.0s0
    (phase (cis 30)) =>  -1.4159266
    (phase #c(0 1)) =>  1.5707964
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个数字[number]就应该发出一个 type-error 类型的错误. 可能发出 arithmetic-error.

* 参见(See Also):

        章节 12.1.3.3 (浮点的置换性规则)

* 注意(Notes): None. 


### <span id="F-REALPART-IMAGPART">函数 REALPART, IMAGPART</span>

* 语法(Syntax):

        realpart number => real

        imagpart number => real

* 参数和值(Arguments and Values):

        number---一个数字[number].
        real---一个实数[real].

* 描述(Description):

        realpart 和 imagpart 分别返回数字 number 的实部和虚部. 如果数字 number 是一个实数[real], 那么 realpart 返回数字 number 而 imagpart 返回 (* 0 number), 它的作用是, 一个有理数[rational]的虚部是 0 而浮点数[float]是相同格式的浮点数零.

* 示例(Examples):

    ```LISP
    (realpart #c(23 41)) =>  23
    (imagpart #c(23 41.0)) =>  41.0
    (realpart #c(23 41.0)) =>  23.0
    (imagpart 23.0) =>  0.0
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 number 不是一个数字[number], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        complex

* 注意(Notes): None. 


### <span id="F-UPGRADED-COMPLEX-PART-TYPE">函数 UPGRADED-COMPLEX-PART-TYPE</span>

* 语法(Syntax):

        upgraded-complex-part-type typespec &optional environment => upgraded-typespec

* 参数和值(Arguments and Values):

        typespec---一个类型指定符[type specifier].
        environment---一个环境[environment]对象[object]. 默认是 nil, 表示空词法环境[null lexical environment]和当前全局环境[global environment].
        upgraded-typespec---一个类型指定符[type specifier].

* 描述(Description):

        upgraded-complex-part-type 返回可以持有类型[type] typespec 的部分的最具体的复数[complex]表示的部分类型.

        这个 typespec 是 upgraded-typespec 的一个子类型[subtype] (也可能是类型等价的[type equivalent]).

        upgraded-complex-part-type 的目的是去揭露一个具体实现如何执行它的提升[upgrade].

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        complex (函数[function]和类型[type])

* 注意(Notes):

### <span id="F-REALP">函数 REALP</span>

* 语法(Syntax):

        realp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[real].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 不是 real 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (realp 12) =>  true
    (realp #c(5/3 7.2)) =>  false
    (realp nil) =>  false
    (realp (cons 1 2)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (realp object) ==  (typep object 'real)


### <span id="F-NUMERATOR-DENOMINATOR">函数 NUMERATOR, DENOMINATOR</span>

* 语法(Syntax):

        numerator rational => numerator

        denominator rational => denominator

* 参数和值(Arguments and Values):

        rational---一个有理数[rational].
        numerator---一个整数[integer].
        denominator---一个正整数[integer].

* 描述(Description):

        numerator 和 denominator 分解有理数 rational 为规范形式并计算那个数字的分子和分母.

        numerator 和 denominator 返回有理数 rational 的规范形式的分子和分母.

        如果有理数 rational 是一个整数[integer], numerator 返回有理数并且 denominator 返回 1.

* 示例(Examples):

    ```LISP
    (numerator 1/2) =>  1
    (denominator 12/36) =>  3
    (numerator -1) =>  -1
    (denominator (/ -33)) =>  33
    (numerator (/ 8 -6)) =>  -4
    (denominator (/ 8 -6)) =>  3
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        /

* 注意(Notes):

        (gcd (numerator x) (denominator x)) =>  1


### <span id="F-RATIONAL-RATIONALIZE">函数 RATIONAL, RATIONALIZE</span>

* 语法(Syntax):

        rational number => rational

        rationalize number => rational

* 参数和值(Arguments and Values):

        number---一个实数[real].
        rational---一个有理数[rational].

* 描述(Description):

        rational 和 rationalize 转换这些实数 reals 为有理数[rational].

        如果数字 number 已经是一个有理数[rational], 它就会被返回.

        如果数字 number 是一个浮点数[float], rational 返回一个在数学上等价于这个浮点数[float]的有理数[rational]. rationalize 返回一个有理数[rational], 它将该浮点数[float]近似为底层浮点表示的精度.

        rational 假定这个浮点数[float]是完全准确的.

        rationalize 假定那个浮点数[float]仅对于那个浮点表示的精度是精确的.

* 示例(Examples):

    ```LISP
    (rational 0) =>  0
    (rationalize -11/100) =>  -11/100
    (rational .1) =>  13421773/134217728 ;implementation-dependent
    (rationalize .1) =>  1/10
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        具体实现[implementation].

* 异常情况(Exceptional Situations):

        如果数字 number 不是一个实数[real], 那么应该发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error.

* 参见(See Also): None.

* 注意(Notes):

        情况总是如下

        (float (rational x) x) ==  x

        并且

        (float (rationalize x) x) ==  x

        这也就是说, 通过任意一个方法来有理化一个浮点数[float], 然后把它转换会一个相同格式的浮点数[float]会产生原始的数字 number. 

### <span id="F-RATIONALP">函数 RATIONALP</span>

* 语法(Syntax):

        rationalp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 rational 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (rationalp 12) =>  true
    (rationalp 6/5) =>  true
    (rationalp 1.212) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        rational

* 注意(Notes):

        (rationalp object) ==  (typep object 'rational)


### <span id="F-ASH">函数 ASH</span>

* 语法(Syntax):

        ash integer count => shifted-integer

* 参数和值(Arguments and Values):

        integer---一个整数[integer].
        count---一个整数[integer].
        shifted-integer---一个整数[integer].

* 描述(Description):

        ash 在整数 integer 的二进制表示上执行算术位移操作, 这个整数被当作二进制数对待.

        如果 count 是正的 ash 就将整数 integer 左位移 count 位, 如果 count 是负的就右位移 count 位. 返回和整数 integer 相同符号的位移后的值.

        数学上说, ash 执行计算 floor(integer*2^count). 逻辑上, ash 移动整数 integer 中的所有位到左边, 在右边用零位填充, 或者移动它们到右边, 丢弃那些位.

        ash 被定义为行为上表现得就好像整数 integer 是以两个补码的形式表示的, 不管这些整数[integer]内部怎么表示.

* 示例(Examples):

    ```LISP
    (ash 16 1) =>  32
    (ash 16 0) =>  16
    (ash 16 -1) =>  8
    (ash -100000000000000000000000000000000 -100) =>  -79
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 integer 不是一个整数[integer]那么应该发出一个 type-error 类型[type]的错误. 如果 count 不是一个整数[integer]那么应该发出一个 type-error 类型[type]的错误. 可能发出 arithmetic-error.

* 参见(See Also): None.

* 注意(Notes):

        (logbitp j (ash n k))
        ==  (and (>= j k) (logbitp (- j k) n))


### <span id="F-INTEGER-LENGTH">函数 INTEGER-LENGTH</span>

* 语法(Syntax):

        integer-length integer => number-of-bits

* 参数和值(Arguments and Values):

        integer---一个整数[integer].
        number-of-bits---一个非负整数[integer].

* 描述(Description):

        返回以二进制补码形式去表示整数 integer 所需的位数.

* 示例(Examples):

    ```LISP
    (integer-length 0) =>  0
    (integer-length 1) =>  1
    (integer-length 3) =>  2
    (integer-length 4) =>  3
    (integer-length 7) =>  3
    (integer-length -1) =>  0
    (integer-length -4) =>  2
    (integer-length -7) =>  3
    (integer-length -8) =>  3
    (integer-length (expt 2 9)) =>  10
    (integer-length (1- (expt 2 9))) =>  9
    (integer-length (- (expt 2 9))) =>  9
    (integer-length (- (1+ (expt 2 9)))) =>  10
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 integer 不是一个整数[integer]那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        这个函数可以这样被定义:

    ```LISP
    (defun integer-length (integer)
      (ceiling (log (if (minusp integer)
                        (- integer)
                        (1+ integer))
                    2)))
    ```

        如果整数 integer 非负, 那么它的值就可以用无符号的二进制形式表示在一个宽度不小于 (integer-length integer) 的字段中. 不管整数 integer 的符号, 它的值可以用带符号的二进制补码形式表示在一个位宽度不小于 (+ (integer-length integer) 1) 的字段中. 


### <span id="F-INTEGERP">函数 INTEGERP</span>

* 语法(Syntax):

        integerp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象是 integer 类型[type]就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (integerp 1) =>  true
    (integerp (expt 2 130)) =>  true
    (integerp 6/5) =>  false
    (integerp nil) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (integerp object) ==  (typep object 'integer)


### <span id="F-PARSE-INTEGER">函数 PARSE-INTEGER</span>

* 语法(Syntax):

        parse-integer string &key start end radix junk-allowed => integer, pos

* 参数和值(Arguments and Values):

        string---一个字符串[string].
        start, end---字符串 string 的边界下标标识符[bounding index designator]. 默认的 start 和 end 分别为 0 和 nil.
        radix---一个基数[radix]. 默认是 10.
        junk-allowed---一个广义 boolean [generalized boolean]. 默认是 false.
        integer---一个整数[integer]或者 false.
        pos---字符串 string 的边界下标[bounding index].

* 描述(Description):

        parse-integer 从由 start 和 end 限定的字符串 string 的子字符串中以指定基数 radix 来解析一个整数[integer].

        parse-integer 预期一个可选的符号 (+ 或 -) 后面跟着一个被解释为指定基数 radix 的非空数字序列. 可选的前后空格[whitespace[1]]会被忽略.

        parse-integer 不识别语法的进制指定符前缀 #O, #B, #X, 和 #nR, 也不识别一个尾随的小数点.

        如果 junk-allowed 是 false, 如果子字符串不是完全由一个带符号整数[integer]表示, 可能周围存在空白[whitespace[1]]字符[character], 那么会发出一个 parse-error 类型[type]的错误.

        返回的第一个值[value]是被解析得到的整数[integer], 如果没有发现语法上正确的整数[integer]但是 junk-allowed 是 true 那么就是 nil.

        第二个值[value]是这个字符串[string]中终止这个解析的定界符的下标, 或者如果这个解析在子字符串的最后终止, 那么就是字符串的上边界[bounding index] (如果 junk-allowed 为 false 则情况总是如此).

* 示例(Examples):

    ```LISP
    (parse-integer "123") =>  123, 3
    (parse-integer "123" :start 1 :radix 5) =>  13, 3
    (parse-integer "no-integer" :junk-allowed t) =>  NIL, 0
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 junk-allowed 是 false, 如果子字符串不是完全由一个整数[integer]表示, 可能周围存在空白[whitespace[1]]字符, 那么就会发出一个错误.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="F-BOOLE">函数 BOOLE</span>

* 语法(Syntax):

        boole op integer-1 integer-2 => result-integer

* 参数和值(Arguments and Values):

        op---一个位逻辑操作指定符[bit-wise logical operation specifier].
        integer-1---一个整数[integer].
        integer-2---一个整数[integer].
        result-integer---一个整数[integer].

* 描述(Description):

        boole 在整数 integer-1 和 integer-2 上执行一个位逻辑操作符, 它们被当作是以两个补码表示的二进制数.

        要执行的操作符和返回值由操作符 op 决定.

        boole 返回下面这段中的操作符 op 指定的值.

            Op           Result                                      
            boole-1      integer-1                                   
            boole-2      integer-2                                   
            boole-andc1  and complement of integer-1 with integer-2  
            boole-andc2  and integer-1 with complement of integer-2  
            boole-and    and                                         
            boole-c1     complement of integer-1                     
            boole-c2     complement of integer-2                     
            boole-clr    always 0 (all zero bits)                    
            boole-eqv    equivalence (exclusive nor)                 
            boole-ior    inclusive or                                
            boole-nand   not-and                                     
            boole-nor    not-or                                      
            boole-orc1   or complement of integer-1 with integer-2   
            boole-orc2   or integer-1 with complement of integer-2   
            boole-set    always -1 (all one bits)                    
            boole-xor    exclusive or                                

            Figure 12-17. 位逻辑操作符

* 示例(Examples):

    ```LISP
    (boole boole-ior 1 16) =>  17
    (boole boole-and -2 5) =>  4
    (boole boole-eqv 17 15) =>  -31

    ;;; These examples illustrate the result of applying BOOLE and each
    ;;; of the possible values of OP to each possible combination of bits.
    (progn
      (format t "~&Results of (BOOLE <op> #b0011 #b0101) ...~
              ~%---Op-------Decimal-----Binary----Bits---~%")
      (dolist (symbol '(boole-1     boole-2    boole-and  boole-andc1
                        boole-andc2 boole-c1   boole-c2   boole-clr
                        boole-eqv   boole-ior  boole-nand boole-nor
                        boole-orc1  boole-orc2 boole-set  boole-xor))
        (let ((result (boole (symbol-value symbol) #b0011 #b0101)))
          (format t "~& ~A~13T~3,' D~23T~:*~5,' B~31T ...~4,'0B~%" 
                  symbol result (logand result #b1111)))))
    >>  Results of (BOOLE <op> #b0011 #b0101) ...
    >>  ---Op-------Decimal-----Binary----Bits---
    >>   BOOLE-1       3          11    ...0011
    >>   BOOLE-2       5         101    ...0101
    >>   BOOLE-AND     1           1    ...0001
    >>   BOOLE-ANDC1   4         100    ...0100
    >>   BOOLE-ANDC2   2          10    ...0010
    >>   BOOLE-C1     -4        -100    ...1100
    >>   BOOLE-C2     -6        -110    ...1010
    >>   BOOLE-CLR     0           0    ...0000
    >>   BOOLE-EQV    -7        -111    ...1001
    >>   BOOLE-IOR     7         111    ...0111
    >>   BOOLE-NAND   -2         -10    ...1110
    >>   BOOLE-NOR    -8       -1000    ...1000
    >>   BOOLE-ORC1   -3         -11    ...1101
    >>   BOOLE-ORC2   -5        -101    ...1011
    >>   BOOLE-SET    -1          -1    ...1111
    >>   BOOLE-XOR     6         110    ...0110
    =>  NIL
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的第一个参数不是一个位逻辑操作指定符[bit-wise logical operation specifier]或者任何后面的参数不是一个整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also):

        logand

* 注意(Notes):

        通常情况下,

        (boole boole-and x y) ==  (logand x y)

        更偏向于使用数字指标而不是位逻辑操作指定符[bit-wise logical operation specifier]的程序员[programmer]可以通过下面这样的手法来得到同样效果:

    ```LISP
    ;; The order of the values in this `table' are such that
    ;; (logand (boole (elt boole-n-vector n) #b0101 #b0011) #b1111) => n
    (defconstant boole-n-vector
        (vector boole-clr   boole-and  boole-andc1 boole-2
                boole-andc2 boole-1    boole-xor   boole-ior
                boole-nor   boole-eqv  boole-c1    boole-orc1
                boole-c2    boole-orc2 boole-nand  boole-set))
    =>  BOOLE-N-VECTOR
    (proclaim '(inline boole-n))
    =>  implementation-dependent
    (defun boole-n (n integer &rest more-integers)
      (apply #'boole (elt boole-n-vector n) integer more-integers))
    =>  BOOLE-N
    (boole-n #b0111 5 3) =>  7
    (boole-n #b0001 5 3) =>  1
    (boole-n #b1101 5 3) =>  -3
    (loop for n from #b0000 to #b1111 collect (boole-n n 5 3))
    =>  (0 1 2 3 4 5 6 7 -8 -7 -6 -5 -4 -3 -2 -1)
    ```LISP


### <span id="CV-B">常量 BOOLE-1, BOOLE-2, BOOLE-AND, BOOLE-ANDC1, BOOLE-ANDC2, BOOLE-C1, BOOLE-C2, BOOLE-CLR, BOOLE-EQV, BOOLE-IOR, BOOLE-NAND, BOOLE-NOR, BOOLE-ORC1, BOOLE-ORC2, BOOLE-SET, BOOLE-XOR</span>

* 常量值(Constant Value):

        这些变量[variable]中的每一个的值[value]的标识和性质都是依赖于具体实现的[implementation-dependent], 除了它必须和其他任何一个的值[value]都不同[distinct], 并且它必须是一个给函数[function] boole 的有效的第一个实参[argument].

* 描述(Description):

        这些常量[constant]中的每一个的值[value]都是十六个可能的位逻辑操作指定符[bit-wise logical operation specifier]中的一个.

* 示例(Examples):

    ```LISP
    (boole boole-ior 1 16) =>  17
    (boole boole-and -2 5) =>  4
    (boole boole-eqv 17 15) =>  -31
    ```

* 参见(See Also):

        boole

* 注意(Notes): None. 


### <span id="F-L">函数 LOGAND, LOGANDC1, LOGANDC2, LOGEQV, LOGIOR, LOGNAND, LOGNOR, LOGNOT, LOGORC1, LOGORC2, LOGXOR</span>

* 语法(Syntax):

        logand &rest integers => result-integer

        logandc1 integer-1 integer-2 => result-integer

        logandc2 integer-1 integer-2 => result-integer

        logeqv &rest integers => result-integer

        logior &rest integers => result-integer

        lognand integer-1 integer-2 => result-integer

        lognor integer-1 integer-2 => result-integer

        lognot integer => result-integer

        logorc1 integer-1 integer-2 => result-integer

        logorc2 integer-1 integer-2 => result-integer

        logxor &rest integers => result-integer

* 参数和值(Arguments and Values):

        integers---多个整数[integer].
        integer---一个整数[integer].
        integer-1---一个整数[integer].
        integer-2---一个整数[integer].
        result-integer---一个整数[integer].

* 描述(Description):

        函数[function] logandc1, logandc2, logand, logeqv, logior, lognand, lognor, lognot, logorc1, logorc2, 和 logxor 在它们的实参[argument]上执行位逻辑操作, 这些参数会被看作二进制数.

        下面这段列出了这些函数[function]中的每一个的意义. 在 'identity' 出现的位置, 表示当没有提供实参[argument]时这个函数[function]产生的值[value].

        Function  Identity  Operation performed                         
        logandc1  ---       and complement of integer-1 with integer-2  
        logandc2  ---       and integer-1 with complement of integer-2  
        logand    -1        and                                         
        logeqv    -1        equivalence (exclusive nor)                 
        logior    0         inclusive or                                
        lognand   ---       complement of integer-1 and integer-2       
        lognor    ---       complement of integer-1 or integer-2        
        lognot    ---       complement                                  
        logorc1   ---       or complement of integer-1 with integer-2   
        logorc2   ---       or integer-1 with complement of integer-2   
        logxor    0         exclusive or                                

        Figure 12-18. 整数上的位逻辑操作

        负的整数会被看作它们是以两个补数的二进制表示的.

* 示例(Examples):

    ```LISP
    (logior 1 2 4 8) =>  15
    (logxor 1 3 7 15) =>  10
    (logeqv) =>  -1
    (logand 16 31) =>  16
    (lognot 0) =>  -1
    (lognot 1) =>  -2
    (lognot -1) =>  0
    (lognot (1+ (lognot 1000))) =>  999

    ;;; In the following example, m is a mask.  For each bit in
    ;;; the mask that is a 1, the corresponding bits in x and y are
    ;;; exchanged.  For each bit in the mask that is a 0, the 
    ;;; corresponding bits of x and y are left unchanged.
    (flet ((show (m x y)
              (format t "~%m = #o~6,'0O~%x = #o~6,'0O~%y = #o~6,'0O~%"
                      m x y)))
      (let ((m #o007750)
            (x #o452576)
            (y #o317407))
        (show m x y)
        (let ((z (logand (logxor x y) m)))
          (setq x (logxor z x))
          (setq y (logxor z y))
          (show m x y))))
    >>  m = #o007750
    >>  x = #o452576
    >>  y = #o317407
    >>  
    >>  m = #o007750
    >>  x = #o457426
    >>  y = #o312557
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何参数不是一个整数[argument], 那么应该发出一个 type-error 错误.

* 参见(See Also):

        boole

* 注意(Notes):

        (logbitp k -1) 对于 k 的所有值都返回 true.

        因为下面的函数不是结合的, 它们只取两个参数而不是任意数量的参数.

        (lognand n1 n2) ==  (lognot (logand n1 n2))
        (lognor n1 n2) ==  (lognot (logior n1 n2))
        (logandc1 n1 n2) ==  (logand (lognot n1) n2)
        (logandc2 n1 n2) ==  (logand n1 (lognot n2))
        (logiorc1 n1 n2) ==  (logior (lognot n1) n2)
        (logiorc2 n1 n2) ==  (logior n1 (lognot n2))
        (logbitp j (lognot x)) ==  (not (logbitp j x))


### <span id="F-LOGBITP">函数 LOGBITP</span>

* 语法(Syntax):

        logbitp index integer => generalized-boolean

* 参数和值(Arguments and Values):

        index---一个非负整数[integer].
        integer---一个整数[integer].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        logbitp 被用于测试整数 integer 中的特定位的值, 它会被当作一个二进制数处理. 如果整数 integer 的索引为 index (这也就是说, 它的权重是 2^index)的位是 1 比特, logbitp 的值就是 true; 否则就是 false.

        那些负的整数 integers 会被看作是以两个补数的二进制表示的.

* 示例(Examples):

    ```LISP
    (logbitp 1 1) =>  false
    (logbitp 0 1) =>  true
    (logbitp 3 10) =>  true
    (logbitp 1000000 -1) =>  true
    (logbitp 2 6) =>  true
    (logbitp 0 6) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 index 不是一个非负整数[integer], 那么应该发出一个 type-error 类型[type]的错误. 如果整数 integer 不是一个整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        (logbitp k n) ==  (ldb-test (byte 1 k) n)


### <span id="F-LOGCOUNT">函数 LOGCOUNT</span>

* 语法(Syntax):

        logcount integer => number-of-on-bits

* 参数和值(Arguments and Values):

        integer---一个整数[integer].
        number-of-on-bits---一个非负整数[integer].

* 描述(Description):

        计算并返回整数 integer 的二进制补码表示中处于 'on' 或 'set' 的位的数量. 如果整数 integer 是负的, 0 位会被计数; 否则, 1 位会被计数.

* 示例(Examples):

    ```LISP
    (logcount 0) =>  0
    (logcount -1) =>  0
    (logcount 7) =>  3
    (logcount  13) =>  3 ;Two's-complement binary: ...0001101
    (logcount -13) =>  2 ;Two's-complement binary: ...1110011
    (logcount  30) =>  4 ;Two's-complement binary: ...0011110
    (logcount -30) =>  4 ;Two's-complement binary: ...1100010
    (logcount (expt 2 100)) =>  1
    (logcount (- (expt 2 100))) =>  100
    (logcount (- (1+ (expt 2 100)))) =>  1
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果它的参数不是一个整数[integer], 那么应该发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        即使这个具体实现[implementation]内部不是把整数[integer]表示为二进制补码形式, logcount 还是表现地就像它所做的那样.

        以下恒等式总是符合的:

            (logcount x)
        ==  (logcount (- (+ x 1)))
        ==  (logcount (lognot x))


### <span id="F-LOGTEST">函数 LOGTEST</span>

* 语法(Syntax):

        logtest integer-1 integer-2 => generalized-boolean

* 参数和值(Arguments and Values):

        integer-1---一个整数[integer].
        integer-2---一个整数[integer].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果 interger-1 中的 1 表示的位在 integer-2 中是 1 就返回 true; 否则就是 false. integer-1 和 integer-2 被当作二进制的.

        负的 integer-1 和 integer-2 被当作就好像它们是以二进制补码表示的.

* 示例(Examples):

    ```LISP
    (logtest 1 7) =>  true
    (logtest 1 2) =>  false
    (logtest -2 -1) =>  true
    (logtest 0 -1) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 integer-1 不是一个整数[integer]就会发出一个 type-error 类型[type]的错误. 如果 integer-2 不是一个整数[integer]就会发出一个 type-error 类型[type]的错误.

* 参见(See Also): None.

* 注意(Notes):

        (logtest x y) ==  (not (zerop (logand x y)))


### <span id="F-BYTE-BYTE-SIZE-BYTE-POSITION">函数 BYTE, BYTE-SIZE, BYTE-POSITION</span>

* 语法(Syntax):

        byte size position => bytespec

        byte-size bytespec => size

        byte-position bytespec => position

* 参数和值(Arguments and Values):

        size, position---一个非负整数[integer].
        bytespec---一个字节指定符[byte specifier].

* 描述(Description):

        byte 返回一个字节指定符[byte specifier], 它表示宽度为 size 的字节[byte], 而它的位有着权重 2^position + size - 1 到 2^position, 并且它的表示是依赖于具体实现的[implementation-dependent].

        byte-size 返回 bytespec 指定的位数.

        byte-position 返回 bytespec 指定的位置.

* 示例(Examples):

    ```LISP
    (setq b (byte 100 200)) =>  #<BYTE-SPECIFIER size 100 position 200>
    (byte-size b) =>  100
    (byte-position b) =>  200
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        ldb, dpb

* 注意(Notes):

        (byte-size (byte j k)) ==  j
        (byte-position (byte j k)) ==  k

        一个 0 大小的字节[byte]是允许的; 它指的是一个宽度为 0 的字节[byte]. 比如,

        (ldb (byte 0 3) #o7777) =>  0
        (dpb #o7777 (byte 0 3) 0) =>  0


### <span id="F-DEPOSIT-FIELD">函数 DEPOSIT-FIELD</span>

* 语法(Syntax):

        deposit-field newbyte bytespec integer => result-integer

* 参数和值(Arguments and Values):

        newbyte---一个整数[integer].
        bytespec---一个字节指定符[byte specifier].
        integer---一个整数[integer].
        result-integer---一个整数[integer].

* 描述(Description):

        替换整数 integer 中的一个位的域; 具体的说, 返回一个整数[integer], 其中 bytespec 所指定的字节[byte]中包含了 newbyte 的位, 而其他地方则包含整数 integer 的位.

* 示例(Examples):

    ```LISP
    (deposit-field 7 (byte 2 1) 0) =>  6
    (deposit-field -1 (byte 4 0) 0) =>  15
    (deposit-field 0 (byte 2 1) -3) =>  -7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        byte, dpb

* 注意(Notes):

        (logbitp j (deposit-field m (byte s p) n))
        ==  (if (and (>= j p) (< j (+ p s)))
                (logbitp j m)
                (logbitp j n))

        deposit-field 对于 mask-field 就像是 dpb 对于 ldb. 

### <span id="F-DPB">函数 DPB</span>

* 语法(Syntax):

        dpb newbyte bytespec integer => result-integer

* 发音(Pronunciation):

        [,duh'pib] 或 [,duh'puhb] 或 ['dee'pee'bee]

* 参数和值(Arguments and Values):

        newbyte---一个整数[integer].
        bytespec---一个字节指定符[byte specifier].
        integer---一个整数[integer].
        result-integer---一个整数[integer].

* 描述(Description):

        dpb (deposit byte) 被用于替换整数 integer 中一个位的域. dpb 返回一个和整数 integer 除了 bytespec 指定的位以外都相同的整数[integer].

        让 s 为 bytespec 指定的大小; 然后这个 newbyte 的低 s 位出现在 bytespec 指定的字节的结果中. 这个 newbyte 被解释为是右对齐的, 就好像是 ldb 的结果.

* 示例(Examples):

    ```LISP
    (dpb 1 (byte 1 10) 0) =>  1024
    (dpb -2 (byte 2 10) 0) =>  2048
    (dpb 1 (byte 2 10) 2048) =>  1024
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        byte, deposit-field, ldb

* 注意(Notes):

        (logbitp j (dpb m (byte s p) n))
        ==  (if (and (>= j p) (< j (+ p s)))
                (logbitp (- j p) m)
                (logbitp j n))

        通常情况下, 对于所有有效的 x, y, 和 z 的值

        (dpb x (byte 0 y) z) =>  z

        历史上, "dpb" 的名称来自于 DEC 的 PDP-10 汇编语言指令, 意思是 "存储字节(deposit byte)". 


### <span id="A-LDB">访问器 LDB</span>

* 语法(Syntax):

        ldb bytespec integer => byte

        (setf (ldb bytespec place) new-byte)

* 发音(Pronunciation):

        ['lidib] 或 ['liduhb] 或 ['el'dee'bee]

* 参数和值(Arguments and Values):

        bytespec---一个字节指定符[byte specifier].
        integer---一个整数[integer].
        byte, new-byte---一个非负整数[integer].

* 描述(Description):

        ldb 提取并返回整数 integer 中 bytespec 所指定的字节[byte].

        ldb 返回一个整数[integer], 在这个整数中 2^(s-1) 到 2^0 的位和整数中 2^(p+s-1) 到 2^p 的一样, 其他位都是 0; s 是 (byte-size bytespec) 并且 p 是 (byte-position bytespec).

        setf 可以和 ldb 一起使用来修改存储在给定 place 中的这个整数 integer 中的一个字节. 当一个 ldb 表达式形式被提供给 setf 时, 求值顺序是从左到右的. 效果就是去执行一个 dpb 操作然后把结果存储回那个 place 中.

* 示例(Examples):

    ```LISP
    (ldb (byte 2 1) 10) =>  1
    (setq a (list 8)) =>  (8)
    (setf (ldb (byte 2 1) (car a)) 1) =>  1
    a =>  (10)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        byte, byte-position, byte-size, dpb

* 注意(Notes):

        (logbitp j (ldb (byte s p) n))
            ==  (and (< j s) (logbitp (+ j p) n))

        通常, 对于所有 x 和 y 的有效值

        (ldb (byte 0 x) y) =>  0

        在历史上, "ldb" 的名称来自于 DEC 的 PDP-10 汇编语言指令, 意思是"载入字节(load byte)". 


### <span id="F-LDB-TEST">函数 LDB-TEST</span>

* 语法(Syntax):

        ldb-test bytespec integer => generalized-boolean

* 参数和值(Arguments and Values):

        bytespec---一个字节指定符[byte specifier].
        integer---一个整数[integer].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果整数 integer 中 bytespec 所指定的字节的任意位不是 0 就返回 true; 否则返回 false.

* 示例(Examples):

    ```LISP
    (ldb-test (byte 4 1) 16) =>  true
    (ldb-test (byte 3 1) 16) =>  false
    (ldb-test (byte 3 2) 16) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        byte, ldb, zerop

* 注意(Notes):

        (ldb-test bytespec n) == 
        (not (zerop (ldb bytespec n))) == 
        (logtest (ldb bytespec -1) n)


### <span id="A-MASK-FIELD">访问器 MASK-FIELD</span>

* 语法(Syntax):

        mask-field bytespec integer => masked-integer

        (setf (mask-field bytespec place) new-masked-integer)

* 参数和值(Arguments and Values):

        bytespec---一个字节指定符[byte specifier].
        integer---一个整数[integer].
        masked-integer, new-masked-integer---一个非负整数[integer].

* 描述(Description):

        mask-field 在整数 integer 上执行 "掩码(mask)" 操作. 它返回一个和整数 integer 在 bytespec 所指定的字节[byte]上有着相同比特而其他都是 0 比特的整数[integer].

        setf 可以和 mask-field 一起使用来修改存储在一个给定位置 place 的整数[integer]中的一个字节. 这个的效果是去执行一个 deposit-field 操作然后存储结果到那个 place 中.

* 示例(Examples):

    ```LISP
    (mask-field (byte 1 5) -1) =>  32
    (setq a 15) =>  15
    (mask-field (byte 2 0) a) =>  3
    a =>  15
    (setf (mask-field (byte 2 0) a) 1) =>  1
    a =>  13
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        byte, ldb

* 注意(Notes):

        (ldb bs (mask-field bs n)) ==  (ldb bs n)
        (logbitp j (mask-field (byte s p) n))
          ==  (and (>= j p) (< j s) (logbitp j n))
        (mask-field bs n) ==  (logand n (dpb -1 bs 0))



### <span id="CV-MM">常量 MOST-POSITIVE-FIXNUM, MOST-NEGATIVE-FIXNUM</span>

* 常量值(Constant Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        most-positive-fixnum 是最接近于这个具体实现提供的正无穷的 fixnum, 并且大于等于 2^15 - 1 和 array-dimension-limit.

        most-negative-fixnum 是最接近于这个具体实现提供的负无穷的 fixnum, 小于等于 -2^15.

* 示例(Examples): None.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="F-DSFFFFI">函数 DECODE-FLOAT, SCALE-FLOAT, FLOAT-RADIX, FLOAT-SIGN, FLOAT-DIGITS, FLOAT-PRECISION, INTEGER-DECODE-FLOAT</span>

* 语法(Syntax):

        decode-float float => significand, exponent, sign

        scale-float float integer => scaled-float

        float-radix float => float-radix

        float-sign float-1 &optional float-2 => signed-float

        float-digits float => digits1

        float-precision float => digits2

        integer-decode-float float => significand, exponent, integer-sign

* 参数和值(Arguments and Values):

        digits1---一个非负整数[integer].
        digits2---一个非负整数[integer].
        exponent---一个整数[integer].
        float---一个浮点数[float].
        float-1---一个浮点数[float].
        float-2---一个浮点数[float].
        float-radix---一个整数[integer].
        integer---一个非负整数[integer].
        integer-sign---整数[integer] -1, 或者整数[integer] 1.
        scaled-float---一个浮点数[float].
        sign---一个和浮点数 float 相同类型[type]但是数学上等价于 1.0 或 -1.0 一个浮点数[float].
        signed-float---一个浮点数[float].
        significand---一个浮点数[float].

* 描述(Description):

        decode-float 计算出描绘浮点数 float 的三个值. 第一个值和浮点数 float 相同类型[type]并且表示有效位数. 第二个值表示一个指数, 那个基数 (在 b 的描述中被提到) 必须被提升到那个指数来获取那个值, 当这个值和第一个值相乘时, 产生浮点数 float 的绝对值. 如果浮点数 float 是 0, 任何整数[integer]值可能被返回, 假设保持 scale-float 展示不变. 第三个值是和浮点数 float 相同类型[type]并且如果浮点数 float 大于等于 0 就是 1.0, 否则就是 -1.0.

        decode-float 用 b 的一个整数幂来除浮点数 float 进而使它的值在 1/b (包含) 和 1 (不包含) 之间, 并且返回这个商作为一个值. 然而, 如果浮点数 float 是 zero, 结果和这个浮点数 float 的绝对值相等 (这也就是说, 如果这里有个负零, 它的有效位数被认为是正零).

        scale-float 返回 (* float (expt (float b float) integer)), 其中 b 是这个浮点表示的基数. 浮点数 float 没有必要在 1/b 和 1 之间.

        float-radix 返回 float 的基数.

        float-sign 返回一个数字 z, 这个 z 和 float-1 有着相同的符号并且 z 和 float-2 有着相同的绝对值. 如果没有提供 float-2, 它的值就是 (float 1 float-1). 如果一个具体实现对于负零和正零有着不同的表示, 那么 (float-sign -0.0) => -1.0.

        float-digits 返回在浮点数 float 中使用的基数 b 的数量 (包括任何隐式的数量, 例如一个 "隐藏位(hidden bit)").

        float-precision 返回在浮点数 float 中出现的有效基数 b 的数量; 如果浮点数 float 是一个浮点数[float]零, 那么结果就是一个整数[integer]零.

        对于标准化[normalized]浮点数[float], 这个 float-digits 和 float-precision 的结果是一样的, 但是精度小于非标准化[denormalized]或零值的表示位数.

        integer-decode-float 计算出描绘浮点数 float 的三个值 - 这个有效位数被缩放为一个整数[integer], 最后两个值和 decode-float 返回的一样. 如果浮点数 float 是零, integer-decode-float 返回 0 作为第一个值. 第二个值与第一个值的关系和 decode-float 相同:

        (multiple-value-bind (signif expon sign)
                              (integer-decode-float f)
          (scale-float (float signif f) expon)) ==  (abs f)

* 示例(Examples):

    ```LISP
    ;; Note that since the purpose of this functionality is to expose
    ;; details of the implementation, all of these examples are necessarily
    ;; very implementation-dependent.  Results may vary widely.
    ;; Values shown here are chosen consistently from one particular implementation.
    (decode-float .5) =>  0.5, 0, 1.0
    (decode-float 1.0) =>  0.5, 1, 1.0
    (scale-float 1.0 1) =>  2.0
    (scale-float 10.01 -2) =>  2.5025
    (scale-float 23.0 0) =>  23.0
    (float-radix 1.0) =>  2
    (float-sign 5.0) =>  1.0
    (float-sign -5.0) =>  -1.0
    (float-sign 0.0) =>  1.0
    (float-sign 1.0 0.0) =>  0.0
    (float-sign 1.0 -10.0) =>  10.0
    (float-sign -1.0 10.0) =>  -10.0
    (float-digits 1.0) =>  24
    (float-precision 1.0) =>  24
    (float-precision least-positive-single-float) =>  1
    (integer-decode-float 1.0) =>  8388608, -23, 1
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        具体实现关于浮点数[float]的表示法.

* 异常情况(Exceptional Situations):

        如果函数[function] decode-float, float-radix, float-digits, float-precision, and integer-decode-float 仅有的参数不是一个浮点数[float], 那么它们应该发出一个错误.

        如果函数[function] scale-float 的第一个参数不是一个浮点数[float]或者它的第二个参数不是一个整数[integer], 那么它应该发出一个错误.

        如果函数[function] float-sign 的第一个参数不是一个浮点数[float]或者提供的第二个参数不是一个浮点数[float], 那么它应该发出一个错误.

* 参见(See Also): None.

* 注意(Notes):

        decode-float 或 integer-decode-float的第一个结果与第二个结果的幂的基数的乘积, 与第三个结果的乘积恰好等于浮点数 float 的值.

        (multiple-value-bind (signif expon sign)
                              (decode-float f)
          (scale-float signif expon))
        ==  (abs f)

        并且

        (multiple-value-bind (signif expon sign)
                              (decode-float f)
          (* (scale-float signif expon) sign))
        ==  f


### <span id="F-FLOAT">函数 FLOAT</span>

* 语法(Syntax):

        float number &optional prototype => float

* 参数和值(Arguments and Values):

        number---一个实数[real].
        prototype---一个浮点数[float].
        float---一个浮点数[float].

* 描述(Description):

        float 把一个实数[real]转换为浮点数[float].

        如果提供了一个原型 prototype, 会返回一个和数字 number 数学上相等的但是有着和原型 prototype 相同格式的浮点数[float].

        如果没有提供原型 prototype, 那么如果这个数字 number 已经是一个浮点数[float], 就把它返回; 否则, 就返回一个和数字 number 数学上相等的但是是一个单精度浮点[single float]的浮点数[float].

* 示例(Examples):

    ```LISP
    (float 0) =>  0.0
    (float 1 .5) =>  1.0
    (float 1.0) =>  1.0
    (float 1/2) =>  0.5
    =>  1.0d0
    OR=>  1.0
    (eql (float 1.0 1.0d0) 1.0d0) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        coerce

* 注意(Notes): None. 


### <span id="F-FLOATP">函数 FLOATP</span>

* 语法(Syntax):

        floatp object

        generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象[object].
        generalized-boolean---一个广义 boolean [generalized boolean].

* 描述(Description):

        如果对象 object 是 float 类型[type]的就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (floatp 1.2d2) =>  true
    (floatp 1.212) =>  true
    (floatp 1.2s2) =>  true
    (floatp (expt 2 130)) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also): None.

* 注意(Notes):

        (floatp object) ==  (typep object 'float)


### <span id="CV-MOST-LEAST">常量 MOST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT, MOST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT, MOST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-NORMALIZED-LONG-FLOAT, MOST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT, MOST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT, MOST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT</span>

* 常量值(Constant Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这些常变量[constant variable]为程序去检测多种浮点格式的具体实现定义的[implementation-defined]限制提供了一种方式.

        这些变量[variable]中, 每个名字[name]中有着 "-normalized" 的必须有着一个标准化[normalized]浮点数[float]的值[value], 而每个名字中没有 "-normalized" 的可能有着一个标准化[normalized]浮点数[float]或者非标准化[denormalized]浮点数[float]的值[value], 视情况而定.

        这些变量[variable]中, 每个名字[name]中有着 "short-float" 的必须有着一个短浮点数[short float]的值[value], 每个名字[name]中有着 "single-float" 必须有着一个单精度浮点数[single float]的值[value], 每个名字[name]中有着 "double-float" 的必须有着一个双精度浮点数[double float]的值[value], 而每个名字[name]中有着 "long-float" 的必须有着一个长浮点数[long float]的值[value].

            * most-positive-short-float, most-positive-single-float, most-positive-double-float, most-positive-long-float

            这些常变量[constant variable]中的每一个都是名字所暗示的浮点格式中最大数值的正浮点数[float] (最接近, 但不等于正无穷).

            * least-positive-short-float, least-positive-normalized-short-float, least-positive-single-float, least-positive-normalized-single-float, least-positive-double-float, least-positive-normalized-double-float, least-positive-long-float, least-positive-normalized-long-float

            这些常变量[constant variable]中的每一个都是名字所暗示的浮点格式中最小数值的正浮点数[float] (非零).

            * least-negative-short-float, least-negative-normalized-short-float, least-negative-single-float, least-negative-normalized-single-float, least-negative-double-float, least-negative-normalized-double-float, least-negative-long-float, least-negative-normalized-long-float

            这些常变量[constant variable]中的每一个都是名字所暗示的浮点格式中最小数值的负(非零)浮点数[float]. (如果一个实现支持 -0 作为一个和 +0 不同[different]的对象[object], 这个值一定不能是 -0.)

            * most-negative-short-float, most-negative-single-float, most-negative-double-float, most-negative-long-float

            这些常变量[constant variable]中的每一个都是名字所暗示的浮点格式中最大数值的负(非零)浮点数[float] (值接近于, 但不等于负无穷).

* 示例(Examples): None.

* 参见(See Also): None.

* 注意(Notes):


### <span id="CV-EPSILON">常量 SHORT-FLOAT-EPSILON, SHORT-FLOAT-NEGATIVE-EPSILON, SINGLE-FLOAT-EPSILON, SINGLE-FLOAT-NEGATIVE-EPSILON, DOUBLE-FLOAT-EPSILON, DOUBLE-FLOAT-NEGATIVE-EPSILON, LONG-FLOAT-EPSILON, LONG-FLOAT-NEGATIVE-EPSILON</span>

* 常量值(Constant Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这些常量 short-float-epsilon, single-float-epsilon, double-float-epsilon, 和 long-float-epsilon 中的每一个的值都是给定格式下的最小正浮点数[float] <EPSILON>, 这样一来下面表达式在求值时是 true 的:

        (not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>)))

        这些常量 short-float-negative-epsilon, single-float-negative-epsilon, double-float-negative-epsilon, 和 long-float-negative-epsilon 中的每一个的值都是给定格式下的最小正浮点数[float] <EPSILON>, 这样一来下面表达式在求值时是 true 的:

        (not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>)))

* 示例(Examples): None.

* 参见(See Also): None.

* 注意(Notes): None. 


### <span id="CT-ARITHMETIC-ERROR">状况类型 ARITHMETIC-ERROR</span>

* 类优先级列表(Class Precedence List):

        arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] arithmetic-error 由运算操作期间发生的错误状况组成. 这个操作符和操作数通过使用给 make-condition 的名为 :operation 和 :operands 的初始化参数来初始化, 并且可以通过函数 arithmetic-error-operation 和 arithmetic-error-operands 访问.

* 参见(See Also):

        arithmetic-error-operation, arithmetic-error-operands 


### <span id="F-AEO-AEO">函数 ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION</span>

* 语法(Syntax):

        arithmetic-error-operands condition => operands

        arithmetic-error-operation condition => operation

* 参数和值(Arguments and Values):

        condition---一个 arithmetic-error 类型[type]的状况[condition].
        operands---一个列表[list].
        operation---一个函数标识符[function designator].

* 描述(Description):

        arithmetic-error-operands 返回一个操作数的列表[list], 这些操作数被用于发出这个状况 condition 的操作符的违规调用中.

        arithmetic-error-operation 返回一个在发出这个状况 condition 的违规调用中违规的操作符列表[list].

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 参见(See Also):

        arithmetic-error, 章节 9 (状况)

* 注意(Notes):


### <span id="CT-DIVISION-BY-ZERO">状况类型 DIVISION-BY-ZERO</span>

* 类优先级列表(Class Precedence List):

        division-by-zero, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] division-by-zero 由因为被 0 除所导致的错误状况所组成. 


### <span id="CT-FLOATING-POINT-INVALID-OPERATION">状况类型 FLOATING-POINT-INVALID-OPERATION</span>

* 类优先级列表(Class Precedence List):

        floating-point-invalid-operation, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] floating-point-invalid-operation 由因为某些浮点数陷阱造成的错误状况组成.

        浮点数陷阱是否发生, 并且它们是否或如何被启用或禁用是依赖于具体实现的[implementation-dependent]. 因此, 符合规范的代码可能为这个状况建立处理者, 但是一定不能依赖于它的发送.  

### <span id="CT-FLOATING-POINT-INEXACT">状况类型 FLOATING-POINT-INEXACT</span>

* 类优先级列表(Class Precedence List):

        floating-point-inexact, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] floating-point-inexact 由因为某些浮点数陷阱造成的错误状况组成.

        浮点数陷阱是否发生, 并且它们是否或如何被启用或禁用是依赖于具体实现的[implementation-dependent]. 因此, 符合规范的代码可能为这个状况建立处理者, 但是一定不能依赖于它的发送. 

### <span id="CT-FLOATING-POINT-OVERFLOW">状况类型 FLOATING-POINT-OVERFLOW</span>

* 类优先级列表(Class Precedence List):

        floating-point-overflow, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] floating-point-overflow 由因为浮点上溢造成的错误状况组成. 


### <span id="CT-FLOATING-POINT-UNDERFLOW">状况类型 FLOATING-POINT-UNDERFLOW</span>

* 类优先级列表(Class Precedence List):

        floating-point-underflow, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型[type] floating-point-underflow 由因为浮点下溢造成的错误状况组成. 
        