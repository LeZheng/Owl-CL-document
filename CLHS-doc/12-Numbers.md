# 12. 数字

> * 12.1 [数字的概念](#NumberConcepts)
> * 12.2 [数字的字典](#NumbersDictionary)

## 12.1 <span id="NumberConcepts">Number Concepts</span>

> * 12.1.1 [数值运算](#NumericOperations)
> * 12.1.2 [依赖于具体实现的数字常量](#IDNumericConstants)
> * 12.1.3 [有理数计算](#RationalComputations)
> * 12.1.4 [浮点计算](#FloatingPointComputations)
> * 12.1.5 [复数计算](#ComplexComputations)
> * 12.1.6 [Interval Designators](#IntervalDesignators)
> * 12.1.7 [随机状态运算](#RandomStateOperations)

### 12.1.1 <span id="NumericOperations">数值运算</span>

Common Lisp 提供了大量和数字相关的操作. 这个章节通过把这些操作分组成类别来强调它们之间的关系进而提供这些操作的一个综述.

下面这段展示了和算术操作相关的运算.

    *  1+         gcd   
    +  1-         incf  
    -  conjugate  lcm   
    /  decf             

    Figure 12-1. 算术相关的运算.

下面这段展示了和指数, 对数, 以及三角运算相关的已定义的名字.

    abs    cos    signum  
    acos   cosh   sin     
    acosh  exp    sinh    
    asin   expt   sqrt    
    asinh  isqrt  tan     
    atan   log    tanh    
    atanh  phase          
    cis    pi             

    Figure 12-2. 指数, 对数, 以及三角运算相关的已定义的名字.

下面这段展示了和数值比较和断言相关的操作符.

    /=  >=      oddp   
    <   evenp   plusp  
    <=  max     zerop  
    =   min            
    >   minusp         

    Figure 12-3. 数值比较和断言相关的操作符.

下面这段展示了和数值类型处理和强制转换相关的已定义的名字.

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

对于数学上相关联的 (可能是可交换的), 一个符合规范的具体实现可以以任何方式处理参数, 这些参数与关联的 (可能是可交换的) 重排一致 a conforming implementation may process the arguments in any manner consistent with associative (and possibly commutative) rearrangement.<!-- TODO 待校验 --> 这个不会影响参数表达式形式被求值的顺序 ; 关于求值顺序的讨论, 见章节 3.1.2.1.2.3 (Function Forms). 唯一不确定的是参数的值被处理的顺序. 这个意味着具体实现在自动强制转换应用的地方可能不同; 见章节 12.1.1.2 (Contagion in Numeric Operations).

一个符合规范的程序可以通过拆分这些操作到分开的函数表达式形式中, 或通过显式写明对执行强制转换函数的调用来控制显式处理的顺序.

##### 12.1.1.1.1 数值运算中的结合律和交换律的示例

思考下面表达式, 其中我们假设 1.0 和 1.0e-15 都表示单个的浮点数:

 (+ 1/3 2/3 1.0d0 1.0 1.0e-15)

一个符合规范的具体实现可能从左到右处理这些参数, 首先添加 1/3 和 2/3 来得到 1, 然后把那个转换为一个双精度浮点用于和 1.0d0 结合, 接下来依次转换并添加 1.0 和 1.0e-15.

另一个符合规范的具体实现可能从右到左处理这些参数, 首先执行一个单精度浮点数加法 1.0 和 1.0e-15 (可能在这个过程中丢失精度), 然后转换这个和为一个双精度浮点并且加 1.0d0, 接下来转换 2/3 为一个双精度浮点并且加上它, 接着转换 1/3 并且加上那个.

第三种符合规范的具体实现可能首先扫描所有参数, 处理所有有理数来保持这个计算准确, 然后在所有参数中找到一个最大浮点格式的参数并添加, 然后添加所有其他参数, 依次转换它们 (所有这些可能是误导的尝试使计算尽可能准确).

在任何情况下, 这三种策略都是合法的.

一个符合规范的程序可以通过在编写时控制这个顺序, 比如,

 (+ (+ 1/3 2/3) (+ 1.0d0 1.0e-15) 1.0)


#### 12.1.1.2 <span id="ContagionNumericOperations">数值运算的传递性</span>

关于在数值运算中参数的隐式强制转换的传递性规则的信息, 见章节 12.1.4.4 (Rule of Float Precision Contagion), 章节 12.1.4.1 (Rule of Float and Rational Contagion), 还有章节 12.1.5.2 (Rule of Complex Contagion). 


#### 12.1.1.3 <span id="ViewingIntegersBitsBytes">视整数为位和字节</span>

> * 12.1.1.3.1 [整数上的逻辑操作](#LogicalOperationsIntegers)
> * 12.1.1.3.2 [整数上的字节操作](#ByteOperationsIntegers)


##### 12.1.1.3.1 <span id="LogicalOperationsIntegers">整数上的逻辑操作</span>

逻辑运算需要整数来作为参数; 如果提供一个不是整数的参数, 那么应该发出一个 type-error 类型的错误. 给逻辑运算的整数参数被当作是用两种补码表示法表示的.

下面这段展示了和数字上逻辑运算相关的已定义的名字.

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

字节操作函数使用称之为字节指定符的对象来指定一个整数内特定字节的大小和位置. 一个字节指定符的表示是依赖于具体实现的; 它可能是也可能不是一个数字. 函数 byte 会构造一个字节指定符, 它是各种其他字节操作函数都会接受的.

下面这段展示和操作一个数字的字节相关的已定义的名字.

    byte           deposit-field  ldb-test    
    byte-position  dpb            mask-field  
    byte-size      ldb                        

    Figure 12-6. 字节操作相关的已定义的名字. 

### 12.1.2 <span id="IDNumericConstants">依赖于具体实现的数字常量</span>

下面这段展示了和数字的依赖于具体实现的详情相关的已定义名字.

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

这个章节中的规则适用于有理数计算.

> * 12.1.3.1 [无界的有理数精度规则](#RuleUnboundedRationalPrecision)
> * 12.1.3.2 [有理数的规范表示规则](#RuleCanonicalReprRationals)
> * 12.1.3.3 [浮点的置换性规则](#RuleFloatSubstitutability)


#### 12.1.3.1 <span id="RuleUnboundedRationalPrecision">无界的有理数精度规则</span>

从一般意义上来说有理数计算不可能上溢 (尽管这里可能没有足够的存储来表示一个结果), 因为整数和比数原则上是任意大小的. 

#### 12.1.3.2 <span id="RuleCanonicalReprRationals">有理数的规范表示规则</span>

如果任何计算产生一个结果是两个整数的数学比, 并且分母可以整除分子, 那么这个结果会被转换为等价的整数.

如果分母不能整除分子, 那么一个有理数的标准表示是分子和分母的比例, 其中分子和分母最大的公约数是 1 , 并且分母是大于 1 的正数.

当作为一个输入时 (在默认语法中), 符号 -0 总是表示 0. 一个符合规范的具体实现不应该有与它对整数 0 的表示不同的"负零"的表示. 但是, 这样的区别对于浮点数也是有可能的; 见类型 float. 

#### 12.1.3.3 <span id="RuleFloatSubstitutability">浮点的置换性规则</span>

当一个不合理的数学函数的参数都是有理数的而真正的数学结果也是(数学上)有理数, 除非另有说明, 否则一个实现可以自由地返回一个准确的有理数结果或者一个浮点近似值. 如果参数都是有理数但是结果不能被表示为一个有理数数字, 那么总是会返回一个单精度浮点数表示.

当一个不合理的数学函数的参数都是类型 (or rational (complex rational)) 并且真正的数学结果是(数学上)一个带有有理数实部和虚部的复数, 除非另有说明, 否则一个具体实现可以自由地返回一个类型 (or rational (complex rational)) 的精准结果或者一个单精度浮点数 (只有当真实的数学结果的虚部为零时才允许) 或者 (complex single-float). 如果参数都是类型 (or rational (complex rational)) 但是结果不能被表示为一个有理数或者复数, 那么返回值会是 single-float 类型(只有当真实的数学结果的虚部为零时才允许) 或者 (complex single-float).

浮点数置换性不能应用于有理数函数 +, -, *, 和 / 也不能应用于相关操作符 1+, 1-, incf, decf, 和 conjugate. 对于有理数函数, 如果所有参数都是有理数, 那么结果就是有理数; 如果所有参数都是类型 (or rational (complex rational)), 那么结果也就是类型 (or rational (complex rational)).

    Function  Sample Results                                   
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

当有理数和浮点数通过一个数值函数进行组合时, 这个有理数首先被转换为一个相同格式的浮点数. 对于像 + 这样的接受超过两个参数的函数, 允许部分操作使用有理数进行操作, 其余部分则使用浮点运算来完成.

当有理数和浮点数被一个数值函数进行比较时, 函数 rational 会被调用来将这个浮点数转换为一个有理数然后执行一个准确的比较. 在复数的情况下, 实部和虚部会被单独处理.

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

浮点数的计算只是近似的, 尽管它们被描述为结果在数学上是准确的. 两个数学上相同的表达式可能在计算上是不同的因为浮点近似过程中固有的错误. 一个浮点数的精度不一定与这个数字的精度相关. 比如, 3.142857142857142857 相比 3.14159 是一个 <PI> 的更精确的近似, 但是后者更准确. 精度指的是在表示中保留的位元数. 当一个操作符组合一个短浮点数和一个长浮点数时, 结果会是一个长浮点数. Common Lisp 函数假定给它们的参数的精度不会超过它们的精度. 因此当两个 small float 被组合时, 结果是一个 small float. Common Lisp 函数不会从一个较大的值转换到较小的一个. 


#### 12.1.4.3 <span id="RuleFloatUnderflowOverflow">浮点的上溢和下溢规则</span>

如果一个浮点数运算分别导致指数上溢或下溢, 就会发出一个 floating-point-overflow 或 floating-point-underflow 类型的错误. 


#### 12.1.4.4 <span id="RuleFloatPrecisionContagion">浮点精度传递规则</span>

一个数值函数的结果是给这个函数的所有浮点数参数中最大格式的一个浮点数 . 

### 12.1.5 <span id="ComplexComputations">复数计算</span>

以下规则应用于复数计算:

> * 12.1.5.1 [复数的置换性规则](#RuleComplexSubstitutability)
> * 12.1.5.2 [复数传递规则](#RuleComplexContagion)
> * 12.1.5.3 [复数的正规表示规则](#RuleCanonicalReprComplexRationals)
> * 12.1.5.4 [主值和分支切割](#PrincipalValuesBranchCuts)


#### 12.1.5.1 <span id="RuleComplexSubstitutability">复数的置换性规则</span>
<!--TODO irrational and transcendental functions ??-->
除了在无理数和超越函数的执行期间 Except during the execution of irrational and transcendental functions, 除非它的参数中的一个或多个是一个复数, 否则数值函数不会产生一个复数. 


#### 12.1.5.2 <span id="RuleComplexContagion">复数传递规则</span>

当一个实数和一个复数都是计算的一部分时, 这个实数首先通过提供一个虚部的0来转换为复数. 


#### 12.1.5.3 <span id="RuleCanonicalReprComplexRationals">复数的正规表示规则</span>

如果任何计算的结果都是一个实部是有理数, 虚部是零的复数, 结果被转换成这个实部的有理数. 这个规则不能应用于两个部分都是浮点数的复数. 比如, #C(5 0) 和 5 在 Common Lisp(它们在 eql 下总是相同的) 中不是不同的对象; #C(5.0 0.0) 和 5.0 在 Common Lisp 中总是为不同的对象 (它们在 eql 下总是不相同的, 尽管它们在 equalp 和 = 下是相同的).


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
<!--TODO 待校验-->
很多无理数和超越函数在复数领域会被多次定义; 比如, 对于对数函数, 一般来说有无穷多的复数值. 在每个这样的例子中, 必须为这个函数选择一个主要的值来返回. 通常情况下, 这样的值不能被选择, 从而使范围连续; 在被称为分支切割的领域中, 必须定义分支, 这又定义了范围内的不连续点. Common Lisp 为这些复数函数定义了分支切割, 主值, 还有边界状况, 这些函数遵循 "复数 APL 中的主值和分支切割". 适用于每个函数的分支切割规则和那个函数的描述相对应.

下面这段列出了在复数域的适用部分中所遵守的恒等式, 即使是在分支切割上:

    sin i z = i sinh z  sinh i z = i sin z        arctan i z = i arctanh z  
    cos i z = cosh z    cosh i z = cos z          arcsinh i z = i arcsin z  
    tan i z = i tanh z  arcsin i z = i arcsinh z  arctanh i z = i arctan z  

    Figure 12-9. 复数域的三角恒等式

在分支切割的讨论中提到的象限编号如下图所示.

    见 http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_12-1-5-4.html

    Figure 12-10. 分支切割的象限编号

### 12.1.6 <span id="IntervalDesignators">Interval Designators</span>
<!-- TODO 待翻译 -->
The compound type specifier form of the numeric type specifiers permit the user to specify an interval on the real number line which describe a subtype of the type which would be described by the corresponding atomic type specifier. A subtype of some type T is specified using an ordered pair of objects called interval designators for type T.

T 类型的两个间隔指示器的第一个可以是以下任意一种:

a number N of type T

    This denotes a lower inclusive bound of N. That is, elements of the subtype of T will be greater than or equal to N.

a singleton list whose element is a number M of type T

    This denotes a lower exclusive bound of M. That is, elements of the subtype of T will be greater than M.

符号 *

    这表示在区间上没有下限.

T 类型的两个间隔指示器的第二个可以是以下任意一种:

a number N of type T

    This denotes an upper inclusive bound of N. That is, elements of the subtype of T will be less than or equal to N.

a singleton list whose element is a number M of type T

    This denotes an upper exclusive bound of M. That is, elements of the subtype of T will be less than M.

符号 *

    这表示在区间上没有上限. 


### 12.1.7 <span id="RandomStateOperations">随机状态运算</span>

下面这段列出了可应用于随机状态的已定义的名字.

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

The type number contains objects which represent mathematical numbers. The types real and complex are disjoint subtypes of number.

The function = tests for numerical equality. The function eql, when its arguments are both numbers, tests that they have both the same type and numerical value. Two numbers that are the same under eql or = are not necessarily the same under eq.

* 注意(Notes):

Common Lisp differs from mathematics on some naming issues. In mathematics, the set of real numbers is traditionally described as a subset of the complex numbers, but in Common Lisp, the type real and the type complex are disjoint. The Common Lisp type which includes all mathematical complex numbers is called number. The reasons for these differences include historical precedent, compatibility with most other popular computer languages, and various issues of time and space efficiency. 


### <span id="SC-COMPLEX">系统类 COMPLEX</span>

* 类优先级列表(Class Precedence List):

complex, number, t

* 描述(Description):

The type complex includes all mathematical complex numbers other than those included in the type rational. Complexes are expressed in Cartesian form with a real part and an imaginary part, each of which is a real. The real part and imaginary part are either both rational or both of the same float type. The imaginary part can be a float zero, but can never be a rational zero, for such a number is always represented by Common Lisp as a rational rather than a complex.

* 复合类型特化符类别(Compound Type Specifier Kind):

Specializing.

* 复合类型特化符语法(Compound Type Specifier Syntax):

complex [typespec | *]

* 复合类型特化符参数(Compound Type Specifier Arguments):

typespec---a type specifier that denotes a subtype of type real.

* 复合类型特化符描述(Compound Type Specifier Description):

Every element of this type is a complex whose real part and imaginary part are each of type (upgraded-complex-part-type typespec). This type encompasses those complexes that can result by giving numbers of type typespec to complex.

(complex type-specifier) refers to all complexes that can result from giving numbers of type type-specifier to the function complex, plus all other complexes of the same specialized representation.

* 也见(See Also):

Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals), Section 2.3.2 (Constructing Numbers from Tokens), Section 22.1.3.1.4 (Printing Complexes)

* 注意(Notes):

The input syntax for a complex with real part r and imaginary part i is #C(r i). For further details, see Section 2.4 (Standard Macro Characters).

For every float, n, there is a complex which represents the same mathematical number and which can be obtained by (COERCE n 'COMPLEX). 


### <span id="SC-REAL">系统类 REAL</span>

* 类优先级列表(Class Precedence List):

real, number, t

* 描述(Description):

The type real includes all numbers that represent mathematical real numbers, though there are mathematical real numbers (e.g., irrational numbers) that do not have an exact representation in Common Lisp. Only reals can be ordered using the <, >, <=, and >= functions.

The types rational and float are disjoint subtypes of type real.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

real [lower-limit [upper-limit]]

* 复合类型特化符参数(Compound Type Specifier Arguments):

lower-limit, upper-limit---interval designators for type real. The defaults for each of lower-limit and upper-limit is the symbol *.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the reals on the interval described by lower-limit and upper-limit. 


### <span id="SC-FLOAT">系统类 FLOAT</span>

* 类优先级列表(Class Precedence List):

float, real, number, t

* 描述(Description):

A float is a mathematical rational (but not a Common Lisp rational) of the form s*f*b^e-p, where s is +1 or -1, the sign; b is an integer greater than 1, the base or radix of the representation; p is a positive integer, the precision (in base-b digits) of the float; f is a positive integer between b^p-1 and b^p-1 (inclusive), the significand; and e is an integer, the exponent. The value of p and the range of e depends on the implementation and on the type of float within that implementation. In addition, there is a floating-point zero; depending on the implementation, there can also be a ``minus zero''. If there is no minus zero, then 0.0 and -0.0 are both interpreted as simply a floating-point zero. (= 0.0 -0.0) is always true. If there is a minus zero, (eql -0.0 0.0) is false, otherwise it is true.

The types short-float, single-float, double-float, and long-float are subtypes of type float. Any two of them must be either disjoint types or the same type; if the same type, then any other types between them in the above ordering must also be the same type. For example, if the type single-float and the type long-float are the same type, then the type double-float must be the same type also.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

float [lower-limit [upper-limit]]

* 复合类型特化符参数(Compound Type Specifier Arguments):

lower-limit, upper-limit---interval designators for type float. The defaults for each of lower-limit and upper-limit is the symbol *.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the floats on the interval described by lower-limit and upper-limit.

* 也见(See Also):

Figure 2-9, Section 2.3.2 (Constructing Numbers from Tokens), Section 22.1.3.1.3 (Printing Floats)

* 注意(Notes):

Note that all mathematical integers are representable not only as Common Lisp reals, but also as complex floats. For example, possible representations of the mathematical number 1 include the integer 1, the float 1.0, or the complex #C(1.0 0.0). 


### <span id="T-SF-SF-DF-LF">类型 SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT</span>

* 超类型(Supertypes):

short-float: short-float, float, real, number, t

single-float: single-float, float, real, number, t

double-float: double-float, float, real, number, t

long-float: long-float, float, real, number, t

* 描述(Description):

For the four defined subtypes of type float, it is true that intermediate between the type short-float and the type long-float are the type single-float and the type double-float. The precise definition of these categories is implementation-defined. The precision (measured in ``bits'', computed as p log 2b) and the exponent size (also measured in ``bits,'' computed as log 2(n+1), where n is the maximum exponent value) is recommended to be at least as great as the values in the next figure. Each of the defined subtypes of type float might or might not have a minus zero.

Format  Minimum Precision  Minimum Exponent Size  
----------


                                          
Short   13 bits            5 bits                 
Single  24 bits            8 bits                 
Double  50 bits            8 bits                 
Long    50 bits            8 bits                 

Figure 12-12. Recommended Minimum Floating-Point Precision and Exponent Size

There can be fewer than four internal representations for floats. If there are fewer distinct representations, the following rules apply:

    If there is only one, it is the type single-float. In this representation, an object is simultaneously of types single-float, double-float, short-float, and long-float.
    Two internal representations can be arranged in either of the following ways:

        Two types are provided: single-float and short-float. An object is simultaneously of types single-float, double-float, and long-float.
        Two types are provided: single-float and double-float. An object is simultaneously of types single-float and short-float, or double-float and long-float.

    Three internal representations can be arranged in either of the following ways:

        Three types are provided: short-float, single-float, and double-float. An object can simultaneously be of type double-float and long-float.
        Three types are provided: single-float, double-float, and long-float. An object can simultaneously be of types single-float and short-float.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

short-float [short-lower-limit [short-upper-limit]]

single-float [single-lower-limit [single-upper-limit]]

double-float [double-lower-limit [double-upper-limit]]

long-float [long-lower-limit [long-upper-limit]]

* 复合类型特化符参数(Compound Type Specifier Arguments):

short-lower-limit, short-upper-limit---interval designators for type short-float. The defaults for each of lower-limit and upper-limit is the symbol *.

single-lower-limit, single-upper-limit---interval designators for type single-float. The defaults for each of lower-limit and upper-limit is the symbol *.

double-lower-limit, double-upper-limit---interval designators for type double-float. The defaults for each of lower-limit and upper-limit is the symbol *.

long-lower-limit, long-upper-limit---interval designators for type long-float. The defaults for each of lower-limit and upper-limit is the symbol *.

* 复合类型特化符描述(Compound Type Specifier Description):

Each of these denotes the set of floats of the indicated type that are on the interval specified by the interval designators. 


### <span id="SC-RATIONAL">系统类 RATIONAL</span>

* 类优先级列表(Class Precedence List):

rational, real, number, t

* 描述(Description):

The canonical representation of a rational is as an integer if its value is integral, and otherwise as a ratio.

The types integer and ratio are disjoint subtypes of type rational.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

rational [lower-limit [upper-limit]]

* 复合类型特化符参数(Compound Type Specifier Arguments):

lower-limit, upper-limit---interval designators for type rational. The defaults for each of lower-limit and upper-limit is the symbol *.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the rationals on the interval described by lower-limit and upper-limit. 

### <span id="SC-RATIO">系统类 RATIO</span>

* 类优先级列表(Class Precedence List):

ratio, rational, real, number, t

* 描述(Description):

A ratio is a number representing the mathematical ratio of two non-zero integers, the numerator and denominator, whose greatest common divisor is one, and of which the denominator is positive and greater than one.

* 也见(See Also):

Figure 2-9, Section 2.3.2 (Constructing Numbers from Tokens), Section 22.1.3.1.2 (Printing Ratios) 


### <span id="SC-INTEGER">系统类 INTEGER</span>

* 类优先级列表(Class Precedence List):

integer, rational, real, number, t

* 描述(Description):

An integer is a mathematical integer. There is no limit on the magnitude of an integer.

The types fixnum and bignum form an exhaustive partition of type integer.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

integer [lower-limit [upper-limit]]

* 复合类型特化符参数(Compound Type Specifier Arguments):

lower-limit, upper-limit---interval designators for type integer. The defaults for each of lower-limit and upper-limit is the symbol *.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the integers on the interval described by lower-limit and upper-limit.

* 也见(See Also):

Figure 2-9, Section 2.3.2 (Constructing Numbers from Tokens), Section 22.1.3.1.1 (Printing Integers)

* 注意(Notes):

The type (integer lower upper), where lower and upper are most-negative-fixnum and most-positive-fixnum, respectively, is also called fixnum.

The type (integer 0 1) is also called bit. The type (integer 0 *) is also called unsigned-byte. 


### <span id="T-SIGNED-BYTE">类型 SIGNED-BYTE</span>

* 超类型(Supertypes):

signed-byte, integer, rational, real, number, t

* 描述(Description):

The atomic type specifier signed-byte denotes the same type as is denoted by the type specifier integer; however, the list forms of these two type specifiers have different semantics.

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

signed-byte [s | *]

* 复合类型特化符参数(Compound Type Specifier Arguments):

s---a positive integer.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the set of integers that can be represented in two's-complement form in a byte of s bits. This is equivalent to (integer -2^s-1 2^s-1-1). The type signed-byte or the type (signed-byte *) is the same as the type integer. 


### <span id="T-UNSIGNED-BYTE">类型 UNSIGNED-BYTE</span>

* 超类型(Supertypes):

unsigned-byte, signed-byte, integer, rational, real, number, t

* 描述(Description):

The atomic type specifier unsigned-byte denotes the same type as is denoted by the type specifier (integer 0 *).

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

unsigned-byte [s | *]

* 复合类型特化符参数(Compound Type Specifier Arguments):

s---a positive integer.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the set of non-negative integers that can be represented in a byte of size s (bits). This is equivalent to (mod m) for m=2^s, or to (integer 0 n) for n=2^s-1. The type unsigned-byte or the type (unsigned-byte *) is the same as the type (integer 0 *), the set of non-negative integers.

* 注意(Notes):

The type (unsigned-byte 1) is also called bit. 


### <span id="TS-MOD">类型特化符 MOD</span>

* 复合类型特化符类别(Compound Type Specifier Kind):

Abbreviating.

* 复合类型特化符语法(Compound Type Specifier Syntax):

mod n

* 复合类型特化符参数(Compound Type Specifier Arguments):

n---a positive integer.

* 复合类型特化符描述(Compound Type Specifier Description):

This denotes the set of non-negative integers less than n. This is equivalent to (integer 0 (n)) or to (integer 0 m), where m=n-1.

The argument is required, and cannot be *.

The symbol mod is not valid as a type specifier. 


### <span id="T-BIT">类型 BIT</span>

* 超类型(Supertypes):

bit, unsigned-byte, signed-byte, integer, rational, real, number, t

* 描述(Description):

The type bit is equivalent to the type (integer 0 1) and (unsigned-byte 1). 


### <span id="T-FIXNUM">类型 FIXNUM</span>

* 超类型(Supertypes):

fixnum, integer, rational, real, number, t

* 描述(Description):

A fixnum is an integer whose value is between most-negative-fixnum and most-positive-fixnum inclusive. Exactly which integers are fixnums is implementation-defined. The type fixnum is required to be a supertype of (signed-byte 16). 


### <span id="T-BIGNUM">类型 BIGNUM</span>

* 超类型(Supertypes):

bignum, integer, rational, real, number, t

* 描述(Description):

The type bignum is defined to be exactly (and integer (not fixnum)). 


### <span id="F-Compare">函数 =, /=, <, >, <=, >=</span>

* 语法(Syntax):

= &rest numbers+ => generalized-boolean

/= &rest numbers+ => generalized-boolean

< &rest numbers+ => generalized-boolean

> &rest numbers+ => generalized-boolean

<= &rest numbers+ => generalized-boolean

>= &rest numbers+ => generalized-boolean

* 参数和值(Arguments and Values):

number---for <, >, <=, >=: a real; for =, /=: a number.

generalized-boolean---a generalized boolean.

* 描述(Description):

=, /=, <, >, <=, and >= perform arithmetic comparisons on their arguments as follows:

=

    The value of = is true if all numbers are the same in value; otherwise it is false. Two complexes are considered equal by = if their real and imaginary parts are equal according to =.

/=

    The value of /= is true if no two numbers are the same in value; otherwise it is false.

<

    The value of < is true if the numbers are in monotonically increasing order; otherwise it is false.

>

    The value of > is true if the numbers are in monotonically decreasing order; otherwise it is false.

<=

    The value of <= is true if the numbers are in monotonically nondecreasing order; otherwise it is false.

>=

    The value of >= is true if the numbers are in monotonically nonincreasing order; otherwise it is false.

=, /=, <, >, <=, and >= perform necessary type conversions.

* 示例(Examples):

The uses of these functions are illustrated in the next figure.

(= 3 3) is true.              (/= 3 3) is false.             
(= 3 5) is false.             (/= 3 5) is true.              
(= 3 3 3 3) is true.          (/= 3 3 3 3) is false.         
(= 3 3 5 3) is false.         (/= 3 3 5 3) is false.         
(= 3 6 5 2) is false.         (/= 3 6 5 2) is true.          
(= 3 2 3) is false.           (/= 3 2 3) is false.           
(< 3 5) is true.              (<= 3 5) is true.              
(< 3 -5) is false.            (<= 3 -5) is false.            
(< 3 3) is false.             (<= 3 3) is true.              
(< 0 3 4 6 7) is true.        (<= 0 3 4 6 7) is true.        
(< 0 3 4 4 6) is false.       (<= 0 3 4 4 6) is true.        
(> 4 3) is true.              (>= 4 3) is true.              
(> 4 3 2 1 0) is true.        (>= 4 3 2 1 0) is true.        
(> 4 3 3 2 0) is false.       (>= 4 3 3 2 0) is true.        
(> 4 3 1 2 0) is false.       (>= 4 3 1 2 0) is false.       
(= 3) is true.                (/= 3) is true.                
(< 3) is true.                (<= 3) is true.                
(= 3.0 #c(3.0 0.0)) is true.  (/= 3.0 #c(3.0 1.0)) is true.  
(= 3 3.0) is true.            (= 3.0s0 3.0d0) is true.       
(= 0.0 -0.0) is true.         (= 5/2 2.5) is true.           
(> 0.0 -0.0) is false.        (= 0 -0.0) is true.            
(<= 0 x 9) is true if x is between 0 and 9, inclusive                               
(< 0.0 x 1.0) is true if x is between 0.0 and 1.0, exclusive                               
(< -1 j (length v)) is true if j is a valid array index for a vector v                               

Figure 12-13. Uses of /=, =, <, >, <=, and >=

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Might signal type-error if some argument is not a real. Might signal arithmetic-error if otherwise unable to fulfill its contract.

* 也见(See Also): None.

* 注意(Notes):

= differs from eql in that (= 0.0 -0.0) is always true, because = compares the mathematical values of its operands, whereas eql compares the representational values, so to speak. 


### <span id="F-MAX-MIN">函数 MAX, MIN</span>

* 语法(Syntax):

max &rest reals+ => max-real

min &rest reals+ => min-real

* 参数和值(Arguments and Values):

real---a real.

max-real, min-real---a real.

* 描述(Description):

max returns the real that is greatest (closest to positive infinity). min returns the real that is least (closest to negative infinity).

For max, the implementation has the choice of returning the largest argument as is or applying the rules of floating-point contagion, taking all the arguments into consideration for contagion purposes. Also, if one or more of the arguments are =, then any one of them may be chosen as the value to return. For example, if the reals are a mixture of rationals and floats, and the largest argument is a rational, then the implementation is free to produce either that rational or its float approximation; if the largest argument is a float of a smaller format than the largest format of any float argument, then the implementation is free to return the argument in its given format or expanded to the larger format. Similar remarks apply to min (replacing ``largest argument'' by ``smallest argument'').

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if any number is not a real.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MINUSP-PLUSP">函数 MINUSP, PLUSP</span>

* 语法(Syntax):

minusp real => generalized-boolean

plusp real => generalized-boolean

* 参数和值(Arguments and Values):

real---a real.

generalized-boolean---a generalized boolean.

* 描述(Description):

minusp returns true if real is less than zero; otherwise, returns false.

plusp returns true if real is greater than zero; otherwise, returns false.

Regardless of whether an implementation provides distinct representations for positive and negative float zeros, (minusp -0.0) always returns false.

* 示例(Examples):

 (minusp -1) =>  true
 (plusp 0) =>  false
 (plusp least-positive-single-float) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if real is not a real.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="Function-ZEROP">函数 ZEROP</span>

* 语法(Syntax):

zerop number => generalized-boolean

* 发音(Pronunciation)::

['zee(,)roh(,)pee]

* 参数和值(Arguments and Values):

number---a number.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if number is zero (integer, float, or complex); otherwise, returns false.

Regardless of whether an implementation provides distinct representations for positive and negative floating-point zeros, (zerop -0.0) always returns true.

* 示例(Examples):

 (zerop 0) =>  true
 (zerop 1) =>  false
 (zerop -0.0) =>  true
 (zerop 0/100) =>  true
 (zerop #c(0 0.0)) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if number is not a number.

* 也见(See Also): None.

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

number---a real.

divisor---a non-zero real. The default is the integer 1.

quotient---for floor, ceiling, truncate, and round: an integer; for ffloor, fceiling, ftruncate, and fround: a float.

remainder---a real.

* 描述(Description):

These functions divide number by divisor, returning a quotient and remainder, such that

quotient*divisor+remainder=number

The quotient always represents a mathematical integer. When more than one mathematical integer might be possible (i.e., when the remainder is not zero), the kind of rounding or truncation depends on the operator:

floor, ffloor

    floor and ffloor produce a quotient that has been truncated toward negative infinity; that is, the quotient represents the largest mathematical integer that is not larger than the mathematical quotient.

ceiling, fceiling

    ceiling and fceiling produce a quotient that has been truncated toward positive infinity; that is, the quotient represents the smallest mathematical integer that is not smaller than the mathematical result.

truncate, ftruncate

    truncate and ftruncate produce a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer of the same sign as the mathematical quotient, and that has the greatest integral magnitude not greater than that of the mathematical quotient.

round, fround

    round and fround produce a quotient that has been rounded to the nearest mathematical integer; if the mathematical quotient is exactly halfway between two integers, (that is, it has the form integer+1/2), then the quotient has been rounded to the even (divisible by two) integer.

All of these functions perform type conversion operations on numbers.

The remainder is an integer if both x and y are integers, is a rational if both x and y are rationals, and is a float if either x or y is a float.

ffloor, fceiling, ftruncate, and fround handle arguments of different types in the following way: If number is a float, and divisor is not a float of longer format, then the first result is a float of the same type as number. Otherwise, the first result is of the type determined by contagion rules; see Section 12.1.1.2 (Contagion in Numeric Operations).

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

When only number is given, the two results are exact; the mathematical sum of the two results is always equal to the mathematical value of number.

(function number divisor) and (function (/ number divisor)) (where function is any of one of floor, ceiling, ffloor, fceiling, truncate, round, ftruncate, and fround) return the same first value, but they return different remainders as the second value. For example:

 (floor 5 2) =>  2, 1
 (floor (/ 5 2)) =>  2, 1/2

If an effect is desired that is similar to round, but that always rounds up or down (rather than toward the nearest even integer) if the mathematical quotient is exactly halfway between two integers, the programmer should consider a construction such as (floor (+ x 1/2)) or (ceiling (- x 1/2)). 


### <span id="F-SIN-COS-TAN">函数 SIN, COS, TAN</span>

* 语法(Syntax):

sin radians => number

cos radians => number

tan radians => number

* 参数和值(Arguments and Values):

radians---a number given in radians.

number---a number.

* 描述(Description):

sin, cos, and tan return the sine, cosine, and tangent, respectively, of radians.

* 示例(Examples):

 (sin 0) =>  0.0
 (cos 0.7853982) =>  0.707107
 (tan #c(0 1)) =>  #C(0.0 0.761594)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if radians is not a number. Might signal arithmetic-error.

* 也见(See Also):

asin, acos, atan, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes): None. 

### <span id="F-ASIN-ACOS-ATAN">函数 ASIN, ACOS, ATAN</span>

* 语法(Syntax):

asin number => radians

acos number => radians

atan number1 &optional number2 => radians

* 参数和值(Arguments and Values):

number---a number.

number1---a number if number2 is not supplied, or a real if number2 is supplied.

number2---a real.

radians---a number (of radians).

* 描述(Description):

asin, acos, and atan compute the arc sine, arc cosine, and arc tangent respectively.

The arc sine, arc cosine, and arc tangent (with only number1 supplied) functions can be defined mathematically for number or number1 specified as x as in the next figure.

Function     Definition                         
Arc sine     -i log  (ix+ sqrt(1-x^2) )         
Arc cosine   (<PI>/2) - arcsin  x               
Arc tangent  -i log  ((1+ix) sqrt(1/(1+x^2)) )  

Figure 12-14. Mathematical definition of arc sine, arc cosine, and arc tangent

These formulae are mathematically correct, assuming completely accurate computation. They are not necessarily the simplest ones for real-valued computations.

If both number1 and number2 are supplied for atan, the result is the arc tangent of number1/number2. The value of atan is always between -<PI> (exclusive) and <PI> (inclusive) when minus zero is not supported. The range of the two-argument arc tangent when minus zero is supported includes -<PI>.

For a real number1, the result is a real and lies between -<PI>/2 and <PI>/2 (both exclusive). number1 can be a complex if number2 is not supplied. If both are supplied, number2 can be zero provided number1 is not zero.

The following definition for arc sine determines the range and branch cuts:

arcsin z = -i log (iz+sqrt(1-z^2))

The branch cut for the arc sine function is in two pieces: one along the negative real axis to the left of -1 (inclusive), continuous with quadrant II, and one along the positive real axis to the right of 1 (inclusive), continuous with quadrant IV. The range is that strip of the complex plane containing numbers whose real part is between -<PI>/2 and <PI>/2. A number with real part equal to -<PI>/2 is in the range if and only if its imaginary part is non-negative; a number with real part equal to <PI>/2 is in the range if and only if its imaginary part is non-positive.

The following definition for arc cosine determines the range and branch cuts:

arccos z = <PI>/2- arcsin z

or, which are equivalent,

arccos z = -i log (z+i sqrt(1-z^2))

arccos z = 2 log (sqrt((1+z)/2) + i sqrt((1-z)/2))/i

The branch cut for the arc cosine function is in two pieces: one along the negative real axis to the left of -1 (inclusive), continuous with quadrant II, and one along the positive real axis to the right of 1 (inclusive), continuous with quadrant IV. This is the same branch cut as for arc sine. The range is that strip of the complex plane containing numbers whose real part is between 0 and <PI>. A number with real part equal to 0 is in the range if and only if its imaginary part is non-negative; a number with real part equal to <PI> is in the range if and only if its imaginary part is non-positive.

The following definition for (one-argument) arc tangent determines the range and branch cuts:

arctan z = log (1+iz) - log (1-iz)/(2i)

Beware of simplifying this formula; ``obvious'' simplifications are likely to alter the branch cuts or the values on the branch cuts incorrectly. The branch cut for the arc tangent function is in two pieces: one along the positive imaginary axis above i (exclusive), continuous with quadrant II, and one along the negative imaginary axis below -i (exclusive), continuous with quadrant IV. The points i and -i are excluded from the domain. The range is that strip of the complex plane containing numbers whose real part is between -<PI>/2 and <PI>/2. A number with real part equal to -<PI>/2 is in the range if and only if its imaginary part is strictly positive; a number with real part equal to <PI>/2 is in the range if and only if its imaginary part is strictly negative. Thus the range of arc tangent is identical to that of arc sine with the points -<PI>/2 and <PI>/2 excluded.

For atan, the signs of number1 (indicated as x) and number2 (indicated as y) are used to derive quadrant information. The next figure details various special cases. The asterisk (*) indicates that the entry in the figure applies to implementations that support minus zero.

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

Figure 12-15. Quadrant information for arc tangent

* 示例(Examples):

 (asin 0) =>  0.0 
 (acos #c(0 1))  =>  #C(1.5707963267948966 -0.8813735870195432)
 (/ (atan 1 (sqrt 3)) 6)  =>  0.087266 
 (atan #c(0 2)) =>  #C(-1.5707964 0.54930615)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

acos and asin should signal an error of type type-error if number is not a number. atan should signal type-error if one argument is supplied and that argument is not a number, or if two arguments are supplied and both of those arguments are not reals.

acos, asin, and atan might signal arithmetic-error.

* 也见(See Also):

log, sqrt, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

The result of either asin or acos can be a complex even if number is not a complex; this occurs when the absolute value of number is greater than one. 


### <span id="CV-PI">常量 PI</span>

Value:

an implementation-dependent long float.

* 描述(Description):

The best long float approximation to the mathematical constant <PI>.

* 示例(Examples):

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

* 也见(See Also): None.

* 注意(Notes):

An approximation to <PI> in some other precision can be obtained by writing (float pi x), where x is a float of the desired precision, or by writing (coerce pi type), where type is the desired type, such as short-float. 


### <span id="F-S-C-T-A-A-A">函数 SINH, COSH, TANH, ASINH, ACOSH, ATANH</span>

* 语法(Syntax):

sinh number => result

cosh number => result

tanh number => result

asinh number => result

acosh number => result

atanh number => result

* 参数和值(Arguments and Values):

number---a number.

result---a number.

* 描述(Description):

These functions compute the hyperbolic sine, cosine, tangent, arc sine, arc cosine, and arc tangent functions, which are mathematically defined for an argument x as given in the next figure.

Function                Definition                              
Hyperbolic sine         (e^x-e^-x)/2                            
Hyperbolic cosine       (e^x+e^-x)/2                            
Hyperbolic tangent      (e^x-e^-x)/(e^x+e^-x)                   
Hyperbolic arc sine     log  (x+sqrt(1+x^2))                    
Hyperbolic arc cosine   2 log  (sqrt((x+1)/2) + sqrt((x-1)/2))  
Hyperbolic arc tangent  (log  (1+x) - log (1-x))/2              

Figure 12-16. Mathematical definitions for hyperbolic functions

The following definition for the inverse hyperbolic cosine determines the range and branch cuts:

arccosh z = 2 log (sqrt((z+1)/2) + sqrt((z-1)/2)).

The branch cut for the inverse hyperbolic cosine function lies along the real axis to the left of 1 (inclusive), extending indefinitely along the negative real axis, continuous with quadrant II and (between 0 and 1) with quadrant I. The range is that half-strip of the complex plane containing numbers whose real part is non-negative and whose imaginary part is between -<PI> (exclusive) and <PI> (inclusive). A number with real part zero is in the range if its imaginary part is between zero (inclusive) and <PI> (inclusive).

The following definition for the inverse hyperbolic sine determines the range and branch cuts:

arcsinh z = log (z+sqrt(1+z^2)).

The branch cut for the inverse hyperbolic sine function is in two pieces: one along the positive imaginary axis above i (inclusive), continuous with quadrant I, and one along the negative imaginary axis below -i (inclusive), continuous with quadrant III. The range is that strip of the complex plane containing numbers whose imaginary part is between -<PI>/2 and <PI>/2. A number with imaginary part equal to -<PI>/2 is in the range if and only if its real part is non-positive; a number with imaginary part equal to <PI>/2 is in the range if and only if its imaginary part is non-negative.

The following definition for the inverse hyperbolic tangent determines the range and branch cuts:

arctanh z = log (1+z) - log (1-z)/2.

Note that:

i arctan z = arctanh iz.

The branch cut for the inverse hyperbolic tangent function is in two pieces: one along the negative real axis to the left of -1 (inclusive), continuous with quadrant III, and one along the positive real axis to the right of 1 (inclusive), continuous with quadrant I. The points -1 and 1 are excluded from the domain. The range is that strip of the complex plane containing numbers whose imaginary part is between -<PI>/2 and <PI>/2. A number with imaginary part equal to -<PI>/2 is in the range if and only if its real part is strictly negative; a number with imaginary part equal to <PI>/2 is in the range if and only if its imaginary part is strictly positive. Thus the range of the inverse hyperbolic tangent function is identical to that of the inverse hyperbolic sine function with the points -<PI>i/2 and <PI>i/2 excluded.

* 示例(Examples):

 (sinh 0) =>  0.0 
 (cosh (complex 0 -1)) =>  #C(0.540302 -0.0)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if number is not a number. Might signal arithmetic-error.

* 也见(See Also):

log, sqrt, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

The result of acosh may be a complex even if number is not a complex; this occurs when number is less than one. Also, the result of atanh may be a complex even if number is not a complex; this occurs when the absolute value of number is greater than one.

The branch cut formulae are mathematically correct, assuming completely accurate computation. Implementors should consult a good text on numerical analysis. The formulae given above are not necessarily the simplest ones for real-valued computations; they are chosen to define the branch cuts in desirable ways for the complex case. 


### <span id="F-Multiply">函数 *</span>

* 语法(Syntax):

* &rest numbers => product

* 参数和值(Arguments and Values):

number---a number.

product---a number.

* 描述(Description):

Returns the product of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 1 is returned.

* 示例(Examples):

 (*) =>  1
 (* 3 5) =>  15
 (* 1.0 #c(22 33) 55/98) =>  #C(12.346938775510203 18.520408163265305)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Might signal type-error if some argument is not a number. Might signal arithmetic-error.

* 也见(See Also):

Section 12.1.1 (Numeric Operations), Section 12.1.3 (Rational Computations), Section 12.1.4 (Floating-point Computations), Section 12.1.5 (Complex Computations)

* 注意(Notes): None. 


### <span id="F-Add">函数 +</span>

* 语法(Syntax):

+ &rest numbers => sum

* 参数和值(Arguments and Values):

number---a number.

sum---a number.

* 描述(Description):

Returns the sum of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 0 is returned.

* 示例(Examples):

 (+) =>  0
 (+ 1) =>  1
 (+ 31/100 69/100) =>  1
 (+ 1/5 0.8) =>  1.0

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Might signal type-error if some argument is not a number. Might signal arithmetic-error.

* 也见(See Also):

Section 12.1.1 (Numeric Operations), Section 12.1.3 (Rational Computations), Section 12.1.4 (Floating-point Computations), Section 12.1.5 (Complex Computations)

* 注意(Notes): None. 


### <span id="F-Sub">函数 -</span>

* 语法(Syntax):

- number => negation

- minuend &rest subtrahends+ => difference

* 参数和值(Arguments and Values):

number, minuend, subtrahend---a number.

negation, difference---a number.

* 描述(Description):

The function - performs arithmetic subtraction and negation.

If only one number is supplied, the negation of that number is returned.

If more than one argument is given, it subtracts all of the subtrahends from the minuend and returns the result.

The function - performs necessary type conversions.

* 示例(Examples):

 (- 55.55) =>  -55.55
 (- #c(3 -5)) =>  #C(-3 5)
 (- 0) =>  0
 (eql (- 0.0) -0.0) =>  true
 (- #c(100 45) #c(0 45)) =>  100
 (- 10 1 2 3 4) =>  0

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Might signal type-error if some argument is not a number. Might signal arithmetic-error.

* 也见(See Also):

Section 12.1.1 (Numeric Operations), Section 12.1.3 (Rational Computations), Section 12.1.4 (Floating-point Computations), Section 12.1.5 (Complex Computations)

* 注意(Notes): None. 


### <span id="F-Div">函数 /</span>

* 语法(Syntax):

/ number => reciprocal

/ numerator &rest denominators+ => quotient

* 参数和值(Arguments and Values):

number, denominator---a non-zero number.

numerator, quotient, reciprocal---a number.

* 描述(Description):

The function / performs division or reciprocation.

If no denominators are supplied, the function / returns the reciprocal of number.

If at least one denominator is supplied, the function / divides the numerator by all of the denominators and returns the resulting quotient.

If each argument is either an integer or a ratio, and the result is not an integer, then it is a ratio.

The function / performs necessary type conversions.

If any argument is a float then the rules of floating-point contagion apply; see Section 12.1.4 (Floating-point Computations).

* 示例(Examples):

 (/ 12 4) =>  3
 (/ 13 4) =>  13/4
 (/ -8) =>  -1/8
 (/ 3 4 5) =>  3/20
 (/ 0.5) =>  2.0
 (/ 20 5) =>  4
 (/ 5 20) =>  1/4
 (/ 60 -2 3 5.0) =>  -2.0
 (/ 2 #c(2 2)) =>  #C(1/2 -1/2)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The consequences are unspecified if any argument other than the first is zero. If there is only one argument, the consequences are unspecified if it is zero.

Might signal type-error if some argument is not a number. Might signal division-by-zero if division by zero is attempted. Might signal arithmetic-error.

* 也见(See Also):

floor, ceiling, truncate, round

* 注意(Notes): None. 

### <span id="F-OP-OI">函数 1+, 1-</span>

* 语法(Syntax):

1+ number => successor

1- number => predecessor

* 参数和值(Arguments and Values):

number---a number.

successor, predecessor---a number.

* 描述(Description):

1+ returns a number that is one more than its argument number. 1- returns a number that is one less than its argument number.

* 示例(Examples):

 (1+ 99) =>  100 
 (1- 100) =>  99 
 (1+ (complex 0.0)) =>  #C(1.0 0.0) 
 (1- 5/3) =>  2/3 

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Might signal type-error if its argument is not a number. Might signal arithmetic-error.

* 也见(See Also):

incf, decf

* 注意(Notes):

 (1+ number) ==  (+ number 1)
 (1- number) ==  (- number 1)

Implementors are encouraged to make the performance of both the previous expressions be the same. 


### <span id="F-ABS">函数 ABS</span>

* 语法(Syntax):

abs number => absolute-value

* 参数和值(Arguments and Values):

number---a number.

absolute-value---a non-negative real.

* 描述(Description):

abs returns the absolute value of number.

If number is a real, the result is of the same type as number.

If number is a complex, the result is a positive real with the same magnitude as number. The result can be a float even if number's components are rationals and an exact rational result would have been possible. Thus the result of (abs #c(3 4)) can be either 5 or 5.0, depending on the implementation.

* 示例(Examples):

 (abs 0) =>  0
 (abs 12/13) =>  12/13
 (abs -1.09) =>  1.09
 (abs #c(5.0 -5.0)) =>  7.071068
 (abs #c(5 5)) =>  7.071068
 (abs #c(3/5 4/5)) =>  1 or approximately 1.0
 (eql (abs -0.0) -0.0) =>  true

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

If number is a complex, the result is equivalent to the following:

(sqrt (+ (expt (realpart number) 2) (expt (imagpart number) 2)))

An implementation should not use this formula directly for all complexes but should handle very large or very small components specially to avoid intermediate overflow or underflow. 

### <span id="F-EVENP-ODDP">函数 EVENP, ODDP</span>

* 语法(Syntax):

evenp integer => generalized-boolean

oddp integer => generalized-boolean

* 参数和值(Arguments and Values):

integer---an integer.

generalized-boolean---a generalized boolean.

* 描述(Description):

evenp returns true if integer is even (divisible by two); otherwise, returns false.

oddp returns true if integer is odd (not divisible by two); otherwise, returns false.

* 示例(Examples):

 (evenp 0) =>  true
 (oddp 10000000000000000000000) =>  false
 (oddp -1) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if integer is not an integer.

* 也见(See Also): None.

* 注意(Notes):

 (evenp integer) ==  (not (oddp integer))
 (oddp integer)  ==  (not (evenp integer))


### <span id="F-EXP-EXPT">函数 EXP, EXPT</span>

* 语法(Syntax):

exp number => result

expt base-number power-number => result

* 参数和值(Arguments and Values):

number---a number.

base-number---a number.

power-number---a number.

result---a number.

* 描述(Description):

exp and expt perform exponentiation.

exp returns e raised to the power number, where e is the base of the natural logarithms. exp has no branch cut.

expt returns base-number raised to the power power-number. If the base-number is a rational and power-number is an integer, the calculation is exact and the result will be of type rational; otherwise a floating-point approximation might result. For expt of a complex rational to an integer power, the calculation must be exact and the result is of type (or rational (complex rational)).

The result of expt can be a complex, even when neither argument is a complex, if base-number is negative and power-number is not an integer. The result is always the principal complex value. For example, (expt -8 1/3) is not permitted to return -2, even though -2 is one of the cube roots of -8. The principal cube root is a complex approximately equal to #C(1.0 1.73205), not -2.

expt is defined as b^x = e^x log b. This defines the principal values precisely. The range of expt is the entire complex plane. Regarded as a function of x, with b fixed, there is no branch cut. Regarded as a function of b, with x fixed, there is in general a branch cut along the negative real axis, continuous with quadrant II. The domain excludes the origin. By definition, 0^0=1. If b=0 and the real part of x is strictly positive, then b^x=0. For all other values of x, 0^x is an error.

When power-number is an integer 0, then the result is always the value one in the type of base-number, even if the base-number is zero (of any type). That is:

 (expt x 0) ==  (coerce 1 (type-of x))

If power-number is a zero of any other type, then the result is also the value one, in the type of the arguments after the application of the contagion rules in Section 12.1.1.2 (Contagion in Numeric Operations), with one exception: the consequences are undefined if base-number is zero when power-number is zero and not of type integer.

* 示例(Examples):

 (exp 0) =>  1.0
 (exp 1) =>  2.718282
 (exp (log 5)) =>  5.0 
 (expt 2 8) =>  256
 (expt 4 .5) =>  2.0
 (expt #c(0 1) 2) =>  -1
 (expt #c(2 2) 3) =>  #C(-16 16)
 (expt #c(2 2) 4) =>  -64 

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

log, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

Implementations of expt are permitted to use different algorithms for the cases of a power-number of type rational and a power-number of type float.

Note that by the following logic, (sqrt (expt x 3)) is not equivalent to (expt x 3/2).

 (setq x (exp (/ (* 2 pi #c(0 1)) 3)))         ;exp(2.pi.i/3)
 (expt x 3) =>  1 ;except for round-off error
 (sqrt (expt x 3)) =>  1 ;except for round-off error
 (expt x 3/2) =>  -1 ;except for round-off error



### <span id="F-GCD">函数 GCD</span>

* 语法(Syntax):

gcd &rest integers => greatest-common-denominator

* 参数和值(Arguments and Values):

integer---an integer.

greatest-common-denominator---a non-negative integer.

* 描述(Description):

Returns the greatest common divisor of integers. If only one integer is supplied, its absolute value is returned. If no integers are given, gcd returns 0, which is an identity for this operation.

* 示例(Examples):

 (gcd) =>  0
 (gcd 60 42) =>  6
 (gcd 3333 -33 101) =>  1
 (gcd 3333 -33 1002001) =>  11
 (gcd 91 -49) =>  7
 (gcd 63 -42 35) =>  7
 (gcd 5) =>  5
 (gcd -4) =>  4

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if any integer is not an integer.

* 也见(See Also):

lcm

* 注意(Notes):

For three or more arguments,

 (gcd b c ... z) ==  (gcd (gcd a b) c ... z)


### <span id="M-INCF-DECF">宏 INCF, DECF</span>

* 语法(Syntax):

incf place [delta-form] => new-value

decf place [delta-form] => new-value

* 参数和值(Arguments and Values):

place---a place.

delta-form---a form; evaluated to produce a delta. The default is 1.

delta---a number.

new-value---a number.

* 描述(Description):

incf and decf are used for incrementing and decrementing the value of place, respectively.

The delta is added to (in the case of incf) or subtracted from (in the case of decf) the number in place and the result is stored in place.

Any necessary type conversions are performed automatically.

For information about the evaluation of subforms of places, see Section 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

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

* 副作用(Side Effects):

Place is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

+, -, 1+, 1-, setf

* 注意(Notes): None. 

### <span id="F-LCM">函数 LCM</span>

* 语法(Syntax):

lcm &rest integers => least-common-multiple

* 参数和值(Arguments and Values):

integer---an integer.

least-common-multiple---a non-negative integer.

* 描述(Description):

lcm returns the least common multiple of the integers.

If no integer is supplied, the integer 1 is returned.

If only one integer is supplied, the absolute value of that integer is returned.

For two arguments that are not both zero,

 (lcm a b) ==  (/ (abs (* a b)) (gcd a b))

If one or both arguments are zero,

 (lcm a 0) ==  (lcm 0 a) ==  0

For three or more arguments,

 (lcm a b c ... z) ==  (lcm (lcm a b) c ... z)

* 示例(Examples):

 (lcm 10) =>  10
 (lcm 25 30) =>  150
 (lcm -24 18 10) =>  360
 (lcm 14 35) =>  70
 (lcm 0 5) =>  0
 (lcm 1 2 3 4 5 6) =>  60

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if any argument is not an integer.

* 也见(See Also):

gcd

* 注意(Notes): None. 


### <span id="F-LOG">函数 LOG</span>

* 语法(Syntax):

log number &optional base => logarithm

* 参数和值(Arguments and Values):

number---a non-zero number.

base---a number.

logarithm---a number.

* 描述(Description):

log returns the logarithm of number in base base. If base is not supplied its value is e, the base of the natural logarithms.

log may return a complex when given a real negative number.

 (log -1.0) ==  (complex 0.0 (float pi 0.0))

If base is zero, log returns zero.

The result of (log 8 2) may be either 3 or 3.0, depending on the implementation. An implementation can use floating-point calculations even if an exact integer result is possible.

The branch cut for the logarithm function of one argument (natural logarithm) lies along the negative real axis, continuous with quadrant II. The domain excludes the origin.

The mathematical definition of a complex logarithm is as follows, whether or not minus zero is supported by the implementation:

(log x) ==  (complex (log (abs x)) (phase x))

Therefore the range of the one-argument logarithm function is that strip of the complex plane containing numbers with imaginary parts between -<PI> (exclusive) and <PI> (inclusive) if minus zero is not supported, or -<PI> (inclusive) and <PI> (inclusive) if minus zero is supported.

The two-argument logarithm function is defined as

 (log base number)
 ==  (/ (log number) (log base))

This defines the principal values precisely. The range of the two-argument logarithm function is the entire complex plane.

* 示例(Examples):

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

* 受此影响(Affected By):

The implementation.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

exp, expt, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes): None. 

### <span id="F-MOD-REM">函数 MOD, REM</span>

* 语法(Syntax):

mod number divisor => modulus

rem number divisor => remainder

* 参数和值(Arguments and Values):

number---a real.

divisor---a real.

modulus, remainder---a real.

* 描述(Description):

mod and rem are generalizations of the modulus and remainder functions respectively.

mod performs the operation floor on number and divisor and returns the remainder of the floor operation.

rem performs the operation truncate on number and divisor and returns the remainder of the truncate operation.

mod and rem are the modulus and remainder functions when number and divisor are integers.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

floor, truncate

* 注意(Notes):

The result of mod is either zero or a real with the same sign as divisor. 

### <span id="F-SIGNUM">函数 SIGNUM</span>

* 语法(Syntax):

signum number => signed-prototype

* 参数和值(Arguments and Values):

number---a number.

signed-prototype---a number.

* 描述(Description):

signum determines a numerical value that indicates whether number is negative, zero, or positive.

For a rational, signum returns one of -1, 0, or 1 according to whether number is negative, zero, or positive. For a float, the result is a float of the same format whose value is minus one, zero, or one. For a complex number z, (signum z) is a complex number of the same phase but with unit magnitude, unless z is a complex zero, in which case the result is z.

For rational arguments, signum is a rational function, but it may be irrational for complex arguments.

If number is a float, the result is a float. If number is a rational, the result is a rational. If number is a complex float, the result is a complex float. If number is a complex rational, the result is a complex, but it is implementation-dependent whether that result is a complex rational or a complex float.

* 示例(Examples):

 (signum 0) =>  0
 (signum 99) =>  1
 (signum 4/5) =>  1
 (signum -99/100) =>  -1
 (signum 0.0) =>  0.0
 (signum #c(0 33)) =>  #C(0.0 1.0)
 (signum #c(7.5 10.0)) =>  #C(0.6 0.8)
 (signum #c(0.0 -14.7)) =>  #C(0.0 -1.0)
 (eql (signum -0.0) -0.0) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

 (signum x) ==  (if (zerop x) x (/ x (abs x)))



### <span id="F-SQRT-ISQRT">函数 SQRT, ISQRT</span>

* 语法(Syntax):

sqrt number => root

isqrt natural => natural-root

* 参数和值(Arguments and Values):

number, root---a number.

natural, natural-root---a non-negative integer.

* 描述(Description):

sqrt and isqrt compute square roots.

sqrt returns the principal square root of number. If the number is not a complex but is negative, then the result is a complex.

isqrt returns the greatest integer less than or equal to the exact positive square root of natural.

If number is a positive rational, it is implementation-dependent whether root is a rational or a float. If number is a negative rational, it is implementation-dependent whether root is a complex rational or a complex float.

The mathematical definition of complex square root (whether or not minus zero is supported) follows:

(sqrt x) = (exp (/ (log x) 2))

The branch cut for square root lies along the negative real axis, continuous with quadrant II. The range consists of the right half-plane, including the non-negative imaginary axis and excluding the negative imaginary axis.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The function sqrt should signal type-error if its argument is not a number.

The function isqrt should signal type-error if its argument is not a non-negative integer.

The functions sqrt and isqrt might signal arithmetic-error.

* 也见(See Also):

exp, log, Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes):

 (isqrt x) ==  (values (floor (sqrt x))) 

but it is potentially more efficient. 


### <span id="SC-RANDOM-STATE">系统类 RANDOM-STATE</span>

* 类优先级列表(Class Precedence List):

random-state, t

* 描述(Description):

A random state object contains state information used by the pseudo-random number generator. The nature of a random state object is implementation-dependent. It can be printed out and successfully read back in by the same implementation, but might not function correctly as a random state in another implementation.

Implementations are required to provide a read syntax for objects of type random-state, but the specific nature of that syntax is implementation-dependent.

* 也见(See Also):

*random-state*, random, Section 22.1.3.10 (Printing Random States) 


### <span id="F-MAKE-RANDOM-STATE">函数 MAKE-RANDOM-STATE</span>

* 语法(Syntax):

make-random-state &optional state => new-state

* 参数和值(Arguments and Values):

state---a random state, or nil, or t. The default is nil.

new-state---a random state object.

* 描述(Description):

Creates a fresh object of type random-state suitable for use as the value of *random-state*.

If state is a random state object, the new-state is a copy[5] of that object. If state is nil, the new-state is a copy[5] of the current random state. If state is t, the new-state is a fresh random state object that has been randomly initialized by some means.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if state is not a random state, or nil, or t.

* 也见(See Also):

random, *random-state*

* 注意(Notes):

One important use of make-random-state is to allow the same series of pseudo-random numbers to be generated many times within a single program. 

### <span id="F-RANDOM">函数 RANDOM</span>

* 语法(Syntax):

random limit &optional random-state => random-number

* 参数和值(Arguments and Values):

limit---a positive integer, or a positive float.

random-state---a random state. The default is the current random state.

random-number---a non-negative number less than limit and of the same type as limit.

* 描述(Description):

Returns a pseudo-random number that is a non-negative number less than limit and of the same type as limit.

The random-state, which is modified by this function, encodes the internal state maintained by the random number generator.

An approximately uniform choice distribution is used. If limit is an integer, each of the possible results occurs with (approximate) probability 1/limit.

* 示例(Examples):

 (<= 0 (random 1000) 1000) =>  true
 (let ((state1 (make-random-state))
       (state2 (make-random-state)))
   (= (random 1000 state1) (random 1000 state2))) =>  true

* 副作用(Side Effects):

The random-state is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if limit is not a positive integer or a positive real.

* 也见(See Also):

make-random-state, *random-state*

* 注意(Notes):

See Common Lisp: The Language for information about generating random numbers. 


### <span id="F-RANDOM-STATE-P">函数 RANDOM-STATE-P</span>

* 语法(Syntax):

random-state-p object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type random-state; otherwise, returns false.

* 示例(Examples):

 (random-state-p *random-state*) =>  true
 (random-state-p (make-random-state)) =>  true
 (random-state-p 'test-function) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-random-state, *random-state*

* 注意(Notes):

 (random-state-p object) ==  (typep object 'random-state)


### <span id="V-RANDOM-STATE">变量 *RANDOM-STATE*</span>

Value Type:

a random state.

Initial Value:

implementation-dependent.

* 描述(Description):

The current random state, which is used, for example, by the function random when a random state is not explicitly supplied.

* 示例(Examples):

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

* 受此影响(Affected By):

The implementation.

random.

* 也见(See Also):

make-random-state, random, random-state

* 注意(Notes):

Binding *random-state* to a different random state object correctly saves and restores the old random state object. 


### <span id="F-NUMBERP">函数 NUMBERP</span>

* 语法(Syntax):

numberp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type number; otherwise, returns false.

* 示例(Examples):

 (numberp 12) =>  true
 (numberp (expt 2 130)) =>  true
 (numberp #c(5/3 7.2)) =>  true
 (numberp nil) =>  false
 (numberp (cons 1 2)) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

 (numberp object) ==  (typep object 'number)



### <span id="F-CIS">函数 CIS</span>

* 语法(Syntax):

cis radians => number

* 参数和值(Arguments and Values):

radians---a real.

number---a complex.

* 描述(Description):

cis returns the value of e^i* radians, which is a complex in which the real part is equal to the cosine of radians, and the imaginary part is equal to the sine of radians.

* 示例(Examples):

 (cis 0) =>  #C(1.0 0.0)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes): None. 

### <span id="F-COMPLEX">函数 COMPLEX</span>

* 语法(Syntax):

complex realpart &optional imagpart => complex

* 参数和值(Arguments and Values):

realpart---a real.

imagpart---a real.

complex---a rational or a complex.

* 描述(Description):

complex returns a number whose real part is realpart and whose imaginary part is imagpart.

If realpart is a rational and imagpart is the rational number zero, the result of complex is realpart, a rational. Otherwise, the result is a complex.

If either realpart or imagpart is a float, the non-float is converted to a float before the complex is created. If imagpart is not supplied, the imaginary part is a zero of the same type as realpart; i.e., (coerce 0 (type-of realpart)) is effectively used.

Type upgrading implies a movement upwards in the type hierarchy lattice. In the case of complexes, the type-specifier must be a subtype of (upgraded-complex-part-type type-specifier). If type-specifier1 is a subtype of type-specifier2, then (upgraded-complex-element-type 'type-specifier1) must also be a subtype of (upgraded-complex-element-type 'type-specifier2). Two disjoint types can be upgraded into the same thing.

* 示例(Examples):

 (complex 0) =>  0
 (complex 0.0) =>  #C(0.0 0.0)
 (complex 1 1/2) =>  #C(1 1/2)
 (complex 1 .99) =>  #C(1.0 0.99)
 (complex 3/2 0.0) =>  #C(1.5 0.0)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

realpart, imagpart, Section 2.4.8.11 (Sharpsign C)

* 注意(Notes): None. 


### <span id="F-COMPLEXP">函数 COMPLEXP</span>

* 语法(Syntax):

complexp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type complex; otherwise, returns false.

* 示例(Examples):

 (complexp 1.2d2) =>  false
 (complexp #c(5/3 7.2)) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

complex (function and type), typep

* 注意(Notes):

 (complexp object) ==  (typep object 'complex)


### <span id="F-CONJUGATE">函数 CONJUGATE</span>

* 语法(Syntax):

conjugate number => conjugate

* 参数和值(Arguments and Values):

number---a number.

conjugate---a number.

* 描述(Description):

Returns the complex conjugate of number. The conjugate of a real number is itself.

* 示例(Examples):

 (conjugate #c(0 -1)) =>  #C(0 1)
 (conjugate #c(1 1)) =>  #C(1 -1)
 (conjugate 1.5) =>  1.5
 (conjugate #C(3/5 4/5)) =>  #C(3/5 -4/5)
 (conjugate #C(0.0D0 -1.0D0)) =>  #C(0.0D0 1.0D0)
 (conjugate 3.7) =>  3.7

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

For a complex number z,

 (conjugate z) ==  (complex (realpart z) (- (imagpart z)))



### <span id="F-PHASE">函数 PHASE</span>

* 语法(Syntax):

phase number => phase

* 参数和值(Arguments and Values):

number---a number.

phase---a number.

* 描述(Description):

phase returns the phase of number (the angle part of its polar representation) in radians, in the range -<PI> (exclusive) if minus zero is not supported, or -<PI> (inclusive) if minus zero is supported, to <PI> (inclusive). The phase of a positive real number is zero; that of a negative real number is <PI>. The phase of zero is defined to be zero.

If number is a complex float, the result is a float of the same type as the components of number. If number is a float, the result is a float of the same type. If number is a rational or a complex rational, the result is a single float.

The branch cut for phase lies along the negative real axis, continuous with quadrant II. The range consists of that portion of the real axis between -<PI> (exclusive) and <PI> (inclusive).

The mathematical definition of phase is as follows:

(phase x) = (atan (imagpart x) (realpart x))

* 示例(Examples):

 (phase 1) =>  0.0s0
 (phase 0) =>  0.0s0
 (phase (cis 30)) =>  -1.4159266
 (phase #c(0 1)) =>  1.5707964

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if its argument is not a number. Might signal arithmetic-error.

* 也见(See Also):

Section 12.1.3.3 (Rule of Float Substitutability)

* 注意(Notes): None. 


### <span id="F-REALPART-IMAGPART">函数 REALPART, IMAGPART</span>

* 语法(Syntax):

realpart number => real

imagpart number => real

* 参数和值(Arguments and Values):

number---a number.

real---a real.

* 描述(Description):

realpart and imagpart return the real and imaginary parts of number respectively. If number is real, then realpart returns number and imagpart returns (* 0 number), which has the effect that the imaginary part of a rational is 0 and that of a float is a floating-point zero of the same format.

* 示例(Examples):

 (realpart #c(23 41)) =>  23
 (imagpart #c(23 41.0)) =>  41.0
 (realpart #c(23 41.0)) =>  23.0
 (imagpart 23.0) =>  0.0

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if number is not a number.

* 也见(See Also):

complex

* 注意(Notes): None. 


### <span id="F-UPGRADED-COMPLEX-PART-TYPE">函数 UPGRADED-COMPLEX-PART-TYPE</span>

* 语法(Syntax):

upgraded-complex-part-type typespec &optional environment => upgraded-typespec

* 参数和值(Arguments and Values):

typespec---a type specifier.

environment---an environment object. The default is nil, denoting the null lexical environment and the and current global environment.

upgraded-typespec---a type specifier.

* 描述(Description):

upgraded-complex-part-type returns the part type of the most specialized complex number representation that can hold parts of type typespec.

The typespec is a subtype of (and possibly type equivalent to) the upgraded-typespec.

The purpose of upgraded-complex-part-type is to reveal how an implementation does its upgrading.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

complex (function and type)

* 注意(Notes):

### <span id="F-REALP">函数 REALP</span>

* 语法(Syntax):

realp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type real; otherwise, returns false.

* 示例(Examples):

 (realp 12) =>  true
 (realp #c(5/3 7.2)) =>  false
 (realp nil) =>  false
 (realp (cons 1 2)) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

 (realp object) ==  (typep object 'real)


### <span id="F-NUMERATOR-DENOMINATOR">函数 NUMERATOR, DENOMINATOR</span>

* 语法(Syntax):

numerator rational => numerator

denominator rational => denominator

* 参数和值(Arguments and Values):

rational---a rational.

numerator---an integer.

denominator---a positive integer.

* 描述(Description):

numerator and denominator reduce rational to canonical form and compute the numerator or denominator of that number.

numerator and denominator return the numerator or denominator of the canonical form of rational.

If rational is an integer, numerator returns rational and denominator returns 1.

* 示例(Examples):

 (numerator 1/2) =>  1
 (denominator 12/36) =>  3
 (numerator -1) =>  -1
 (denominator (/ -33)) =>  33
 (numerator (/ 8 -6)) =>  -4
 (denominator (/ 8 -6)) =>  3

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

/

* 注意(Notes):

 (gcd (numerator x) (denominator x)) =>  1



### <span id="F-RATIONAL-RATIONALIZE">函数 RATIONAL, RATIONALIZE</span>

* 语法(Syntax):

rational number => rational

rationalize number => rational

* 参数和值(Arguments and Values):

number---a real.

rational---a rational.

* 描述(Description):

rational and rationalize convert reals to rationals.

If number is already rational, it is returned.

If number is a float, rational returns a rational that is mathematically equal in value to the float. rationalize returns a rational that approximates the float to the accuracy of the underlying floating-point representation.

rational assumes that the float is completely accurate.

rationalize assumes that the float is accurate only to the precision of the floating-point representation.

* 示例(Examples):

 (rational 0) =>  0
 (rationalize -11/100) =>  -11/100
 (rational .1) =>  13421773/134217728 ;implementation-dependent
 (rationalize .1) =>  1/10

* 副作用(Side Effects): None.

* 受此影响(Affected By):

The implementation.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if number is not a real. Might signal arithmetic-error.

* 也见(See Also): None.

* 注意(Notes):

It is always the case that

 (float (rational x) x) ==  x

and

 (float (rationalize x) x) ==  x

That is, rationalizing a float by either method and then converting it back to a float of the same format produces the original number. 

### <span id="F-RATIONALP">函数 RATIONALP</span>

* 语法(Syntax):

rationalp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type rational; otherwise, returns false.

* 示例(Examples):

 (rationalp 12) =>  true
 (rationalp 6/5) =>  true
 (rationalp 1.212) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

rational

* 注意(Notes):

 (rationalp object) ==  (typep object 'rational)


### <span id="F-ASH">函数 ASH</span>

* 语法(Syntax):

ash integer count => shifted-integer

* 参数和值(Arguments and Values):

integer---an integer.

count---an integer.

shifted-integer---an integer.

* 描述(Description):

ash performs the arithmetic shift operation on the binary representation of integer, which is treated as if it were binary.

ash shifts integer arithmetically left by count bit positions if count is positive, or right count bit positions if count is negative. The shifted value of the same sign as integer is returned.

Mathematically speaking, ash performs the computation floor(integer*2^count). Logically, ash moves all of the bits in integer to the left, adding zero-bits at the right, or moves them to the right, discarding bits.

ash is defined to behave as if integer were represented in two's complement form, regardless of how integers are represented internally.

* 示例(Examples):

 (ash 16 1) =>  32
 (ash 16 0) =>  16
 (ash 16 -1) =>  8
 (ash -100000000000000000000000000000000 -100) =>  -79

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if integer is not an integer. Should signal an error of type type-error if count is not an integer. Might signal arithmetic-error.

* 也见(See Also): None.

* 注意(Notes):

 (logbitp j (ash n k))
 ==  (and (>= j k) (logbitp (- j k) n))


### <span id="F-INTEGER-LENGTH">函数 INTEGER-LENGTH</span>

* 语法(Syntax):

integer-length integer => number-of-bits

* 参数和值(Arguments and Values):

integer---an integer.

number-of-bits---a non-negative integer.

* 描述(Description):

Returns the number of bits needed to represent integer in binary two's-complement format.

* 示例(Examples):

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

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if integer is not an integer.

* 也见(See Also): None.

* 注意(Notes):

This function could have been defined by:

(defun integer-length (integer)
  (ceiling (log (if (minusp integer)
                    (- integer)
                    (1+ integer))
                2)))

If integer is non-negative, then its value can be represented in unsigned binary form in a field whose width in bits is no smaller than (integer-length integer). Regardless of the sign of integer, its value can be represented in signed binary two's-complement form in a field whose width in bits is no smaller than (+ (integer-length integer) 1). 


### <span id="F-INTEGERP">函数 INTEGERP</span>

* 语法(Syntax):

integerp object => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if object is of type integer; otherwise, returns false.

* 示例(Examples):

 (integerp 1) =>  true
 (integerp (expt 2 130)) =>  true
 (integerp 6/5) =>  false
 (integerp nil) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

 (integerp object) ==  (typep object 'integer)

### <span id="F-PARSE-INTEGER">函数 PARSE-INTEGER</span>

* 语法(Syntax):

parse-integer string &key start end radix junk-allowed => integer, pos

* 参数和值(Arguments and Values):

string---a string.

start, end---bounding index designators of string. The defaults for start and end are 0 and nil, respectively.

radix---a radix. The default is 10.

junk-allowed---a generalized boolean. The default is false.

integer---an integer or false.

pos---a bounding index of string.

* 描述(Description):

parse-integer parses an integer in the specified radix from the substring of string delimited by start and end.

parse-integer expects an optional sign (+ or -) followed by a a non-empty sequence of digits to be interpreted in the specified radix. Optional leading and trailing whitespace[1] is ignored.

parse-integer does not recognize the syntactic radix-specifier prefixes #O, #B, #X, and #nR, nor does it recognize a trailing decimal point.

If junk-allowed is false, an error of type parse-error is signaled if substring does not consist entirely of the representation of a signed integer, possibly surrounded on either side by whitespace[1] characters.

The first value returned is either the integer that was parsed, or else nil if no syntactically correct integer was seen but junk-allowed was true.

The second value is either the index into the string of the delimiter that terminated the parse, or the upper bounding index of the substring if the parse terminated at the end of the substring (as is always the case if junk-allowed is false).

* 示例(Examples):

 (parse-integer "123") =>  123, 3
 (parse-integer "123" :start 1 :radix 5) =>  13, 3
 (parse-integer "no-integer" :junk-allowed t) =>  NIL, 0

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If junk-allowed is false, an error is signaled if substring does not consist entirely of the representation of an integer, possibly surrounded on either side by whitespace[1] characters.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-BOOLE">函数 BOOLE</span>

* 语法(Syntax):

boole op integer-1 integer-2 => result-integer

* 参数和值(Arguments and Values):

Op---a bit-wise logical operation specifier.

integer-1---an integer.

integer-2---an integer.

result-integer---an integer.

* 描述(Description):

boole performs bit-wise logical operations on integer-1 and integer-2, which are treated as if they were binary and in two's complement representation.

The operation to be performed and the return value are determined by op.

boole returns the values specified for any op in the next figure.

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

Figure 12-17. Bit-Wise Logical Operations

* 示例(Examples):

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

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if its first argument is not a bit-wise logical operation specifier or if any subsequent argument is not an integer.

* 也见(See Also):

logand

* 注意(Notes):

In general,

 (boole boole-and x y) ==  (logand x y)

Programmers who would prefer to use numeric indices rather than bit-wise logical operation specifiers can get an equivalent effect by a technique such as the following:

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



### <span id="CV-B">常量 BOOLE-1, BOOLE-2, BOOLE-AND, BOOLE-ANDC1, BOOLE-ANDC2, BOOLE-C1, BOOLE-C2, BOOLE-CLR, BOOLE-EQV, BOOLE-IOR, BOOLE-NAND, BOOLE-NOR, BOOLE-ORC1, BOOLE-ORC2, BOOLE-SET, BOOLE-XOR</span>

* 常量值(Constant Value):

The identity and nature of the values of each of these variables is implementation-dependent, except that it must be distinct from each of the values of the others, and it must be a valid first argument to the function boole.

* 描述(Description):

Each of these constants has a value which is one of the sixteen possible bit-wise logical operation specifiers.

* 示例(Examples):

 (boole boole-ior 1 16) =>  17
 (boole boole-and -2 5) =>  4
 (boole boole-eqv 17 15) =>  -31

* 也见(See Also):

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

        integers---多个整数.
        integer---一个整数.
        integer-1---一个整数.
        integer-2---一个整数.
        result-integer---一个整数.

* 描述(Description):

        函数 logandc1, logandc2, logand, logeqv, logior, lognand, lognor, lognot, logorc1, logorc2, 和 logxor 在它们的参数上执行位逻辑操作, 这些参数会被看作二进制数.

        下面这段列出了这些函数中的每一个的意义. 在 'identity' 出现的位置, 表示当没有提供参数时函数产生的值.

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

        如果任何参数不是一个整数, 那么应该发出一个 type-error 错误.

* 也见(See Also):

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

        index---一个非负整数.
        integer---一个整数.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        logbitp 被用于测试整数 integer 中的特定位的值, 它会被当作一个二进制数处理. 如果整数 integer 的索引为 index (这也就是说, 它的权重是 2^index)的位是 1 比特, logbitp 的值就是 true; 否则就是 false.

        负的整数会被看作它们是以两个补数的二进制表示的.

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

        如果 index 不是一个非负整数, 那么应该发出一个 type-error 类型的错误. 如果整数 integer 不是一个整数, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        (logbitp k n) ==  (ldb-test (byte 1 k) n)


### <span id="F-LOGCOUNT">函数 LOGCOUNT</span>

* 语法(Syntax):

        logcount integer => number-of-on-bits

* 参数和值(Arguments and Values):

        integer---一个整数.
        number-of-on-bits---一个非负整数.

* 描述(Description):

        计算并返回整数 integer 的两个补数的二进制表示中出于 'on' 或 'set' 的位的数量. 如果整数 integer 是负的, 计算的是 0 的位; 否则, 计算的就是 1 的位.

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

        如果它的参数不是一个整数, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        即使这个具体实现内部不是把整数表示为两个补数的二进制数, logcount 还是表现地就像它所做的那样.

        以下恒等式总是符合的:

            (logcount x)
        ==  (logcount (- (+ x 1)))
        ==  (logcount (lognot x))


### <span id="F-LOGTEST">函数 LOGTEST</span>

* 语法(Syntax):

        logtest integer-1 integer-2 => generalized-boolean

* 参数和值(Arguments and Values):

        integer-1---一个整数.
        integer-2---一个整数.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果 interger-1 中的 1 表示的位在 integer-2 中是 1 就返回 true; 否则就是 false. integer-1 和 integer-2 被当作二进制的.

        负的 integer-1 和 integer-2 被当作就好像它们是以两个补数的二进制表示的.

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

        如果 integer-1 不是一个整数就会发出一个 type-error 类型的错误. 如果 integer-2 不是一个整数就会发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        (logtest x y) ==  (not (zerop (logand x y)))


### <span id="F-BYTE-BYTE-SIZE-BYTE-POSITION">函数 BYTE, BYTE-SIZE, BYTE-POSITION</span>

* 语法(Syntax):

        byte size position => bytespec

        byte-size bytespec => size

        byte-position bytespec => position

* 参数和值(Arguments and Values):

        size, position---一个非负整数.
        bytespec---一个字节指定符.

* 描述(Description):

        byte 返回一个字节指定符, 它表示宽度为 size 的, 而它的位有着权重 2^position + size - 1 到 2^position, 并且它的表示是依赖于具体实现的.

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

* 也见(See Also):

        ldb, dpb

* 注意(Notes):

        (byte-size (byte j k)) ==  j
        (byte-position (byte j k)) ==  k

        一个 0 大小的字节是允许的; 它指的是一个宽度为 0 的字节. 比如,

        (ldb (byte 0 3) #o7777) =>  0
        (dpb #o7777 (byte 0 3) 0) =>  0


### <span id="F-DEPOSIT-FIELD">函数 DEPOSIT-FIELD</span>

* 语法(Syntax):

        deposit-field newbyte bytespec integer => result-integer

* 参数和值(Arguments and Values):

        newbyte---一个整数.
        bytespec---一个字节指定符.
        integer---一个整数.
        result-integer---一个整数.

* 描述(Description):

        替换整数 integer 中的一个位域; 具体的说, 返回一个整数, 其中 bytespec 所指定的字节中包含了 newbyte 的位, 而其他地方则包含整数 integer 的位.

* 示例(Examples):

    ```LISP
    (deposit-field 7 (byte 2 1) 0) =>  6
    (deposit-field -1 (byte 4 0) 0) =>  15
    (deposit-field 0 (byte 2 1) -3) =>  -7
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

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

* 发音(Pronunciation)::

        [,duh'pib] 或 [,duh'puhb] 或 ['dee'pee'bee]

* 参数和值(Arguments and Values):

        newbyte---一个整数.
        bytespec---一个字节指定符.
        integer---一个整数.
        result-integer---一个整数.

* 描述(Description):

        dpb (deposit byte) 被用于替换整数 integer 中一个位域. dpb 返回一个和整数 integer 除了 bytespec 指定的位以外都相同的整数.

        让 s 为 bytespec 指定的大小; 然后这个 newbyte 的低 s 位出现在 bytespec 指定的字节的结果中. Newbyte 被解释为是右对齐的, 就好像是 ldb 的结果.

* 示例(Examples):

    ```LISP
    (dpb 1 (byte 1 10) 0) =>  1024
    (dpb -2 (byte 2 10) 0) =>  2048
    (dpb 1 (byte 2 10) 2048) =>  1024
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        byte, deposit-field, ldb

* 注意(Notes):

        (logbitp j (dpb m (byte s p) n))
        ==  (if (and (>= j p) (< j (+ p s)))
                (logbitp (- j p) m)
                (logbitp j n))

        通常情况下, 对于所有有效的 x, y, 和 z 的值

        (dpb x (byte 0 y) z) =>  z

        历史上, "dpb" 的名称来自于DEC的PDP-10汇编语言指令, 意思是 "存储字节(deposit byte)". 


### <span id="A-LDB">访问器 LDB</span>

* 语法(Syntax):

        ldb bytespec integer => byte

        (setf (ldb bytespec place) new-byte)

* 发音(Pronunciation)::

        ['lidib] 或 ['liduhb] 或 ['el'dee'bee]

* 参数和值(Arguments and Values):

        bytespec---一个字节指定符.
        integer---一个整数.
        byte, new-byte---一个非负整数.

* 描述(Description):

        ldb 提取并返回整数 integer 中 bytespec 所指定的字节.

        ldb 返回一个整数, 在这个整数中 2^(s-1) 到 2^0 的位和整数中 2^(p+s-1) 到 2^p 的一样, 其他位都是 0; s 是 (byte-size bytespec) 并且 p 是 (byte-position bytespec).

        setf 可以和 ldb 一起使用来修改存储在一个给定 place 中的这个整数中的字节. 当一个 ldb 表达式形式被提供给 setf 时, 求值顺序是从左到右的. 效果就是去执行一个 dpb 操作然后把结果存储回那个 place 中.

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

* 也见(See Also):

        byte, byte-position, byte-size, dpb

* 注意(Notes):

        (logbitp j (ldb (byte s p) n))
            ==  (and (< j s) (logbitp (+ j p) n))

        通常,

        (ldb (byte 0 x) y) =>  0

        对于所有 x 和 y 的有效值.

        在历史上, "ldb"的名称来自于DEC的PDP-10汇编语言指令, 意思是"载入字节(load byte)". 


### <span id="F-LDB-TEST">函数 LDB-TEST</span>

* 语法(Syntax):

        ldb-test bytespec integer => generalized-boolean

* 参数和值(Arguments and Values):

        bytespec---一个字节指定符.
        integer---一个整数.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果整数 integer 中 bytespec 指定的字节的任意位不是 0 就返回 true; 否则返回 false.

* 示例(Examples):

    ```LISP
    (ldb-test (byte 4 1) 16) =>  true
    (ldb-test (byte 3 1) 16) =>  false
    (ldb-test (byte 3 2) 16) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

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

        bytespec---一个字节指定符.
        integer---一个整数.
        masked-integer, new-masked-integer---一个非负整数.

* 描述(Description):

        mask-field 在整数 integer 上执行 "掩码(mask)" 操作. 它返回一个和整数 integer 在 bytespec 指定的字节上有着相同比特而其他都是 0 比特的整数.

        setf 可以和 mask-field 一起使用来修改存储在一个给定 place 的整数中的一个字节. 这个的效果是去执行一个 deposit-field 操作然后存储结果到那个 place 中.

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

* 也见(See Also):

        byte, ldb

* 注意(Notes):

        (ldb bs (mask-field bs n)) ==  (ldb bs n)
        (logbitp j (mask-field (byte s p) n))
          ==  (and (>= j p) (< j s) (logbitp j n))
        (mask-field bs n) ==  (logand n (dpb -1 bs 0))



### <span id="CV-MM">常量 MOST-POSITIVE-FIXNUM, MOST-NEGATIVE-FIXNUM</span>

* 常量值(Constant Value):

        依赖于具体实现.

* 描述(Description):

        most-positive-fixnum 是最接近于这个具体实现提供的正无穷的 fixnum, 并且大于等于 2^15 - 1 和 array-dimension-limit.

        most-negative-fixnum 是最接近于这个具体实现提供的负无穷的 fixnum, 小于等于 -2^15.

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-DSFFFFI">函数 DECODE-FLOAT, SCALE-FLOAT, FLOAT-RADIX, FLOAT-SIGN, FLOAT-DIGITS, FLOAT-PRECISION, INTEGER-DECODE-FLOAT</span>
<!--TODO 待校验 待翻译-->
* 语法(Syntax):

        decode-float float => significand, exponent, sign

        scale-float float integer => scaled-float

        float-radix float => float-radix

        float-sign float-1 &optional float-2 => signed-float

        float-digits float => digits1

        float-precision float => digits2

        integer-decode-float float => significand, exponent, integer-sign

* 参数和值(Arguments and Values):

        digits1---一个非负整数.
        digits2---一个非负整数.
        exponent---一个整数.
        float---一个浮点数.
        float-1---一个浮点数.
        float-2---一个浮点数.
        float-radix---一个整数.
        integer---一个非负整数.
        integer-sign---整数 -1, 或者整数 1.
        scaled-float---一个浮点数.
        sign---一个和浮点数 float 相同类型但是数学上等价于 1.0 或 -1.0 一个浮点数.
        signed-float---一个浮点数.
        significand---一个浮点数.

* 描述(Description):

        decode-float 计算描绘浮点数 float 的三个值. 第一个值和浮点数 float 相同类型并且表示有效位数 significand. 第二个值表示一个指数, 对于这个指数那个基数必须被提升到 The second value represents the exponent to which the radix (notated in this description by b) must be raised to obtain the value that, 当和第一个值相乘时, 产生浮点数 float 的完整值. 如果浮点数 float 是 0, 任何整数值可能被返回, provided that the identity shown for scale-float holds. 第三个值是和浮点数 float 相同类型并且如果浮点数 float 大于等于 0 就是 1.0 否则就是 -1.0.

        decode-float divides float by an integral power of b so as to bring its value between 1/b (inclusive) and 1 (exclusive), 并且返回这个商作为对一个值. 然而, 如果浮点数 float 是 zero, 结果和这个浮点数 float 的绝对值相等 (这也就是说, 如果这里有个负零, 它的有效位数被认为是正零).

        scale-float 返回 (* float (expt (float b float) integer)), 其中 b 是这个浮点表示的基数. 浮点数 float 没有必要在 1/b 和 1 之间.

        float-radix 返回 float 的基数.

        float-sign 返回一个数字 z, 这个 z 和 float-1 有着相同的符号并且 z 和 float-2 有着相同的绝对值. 如果没有提供 float-2, 它的值就是 (float 1 float-1). 如果一个具体实现对于负零和正零有着不同的表示, 那么 (float-sign -0.0) => -1.0.

        float-digits 返回在浮点数 float 中使用的基数 b 的数量 (包括任何隐式的数量, 例如一个 "隐藏位(hidden bit)").

        float-precision 返回在浮点数 float 中出现的重要的基数 b 的数量; 如果浮点数 float 是一个浮点数零, 那么结果就是一个整数零.

        对于标准化浮点数, 这个 float-digits 和 float-precision 的结果是一样的, 但是精度比非标准化或零的表示数字要小.

        integer-decode-float 计算描绘浮点数 float 的三个值 - 这个有效数字被缩放为一个整数, 和 decode-float 返回的最后两个值一样. 如果浮点数 float 是零, integer-decode-float 返回 0 作为第一个值. 第二个值与第一个值具有和 decode-float 中的相同的关系:

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

        具体实现对于浮点数的表示法.

* 异常情况(Exceptional Situations):

        如果函数 decode-float, float-radix, float-digits, float-precision, and integer-decode-float 仅有的参数不是一个浮点数, 那么它们应该发出一个错误.

        如果函数 scale-float 的第一个参数不是一个浮点数或者它的第二个参数不是一个整数, 那么它应该发出一个错误.

        如果函数 float-sign 的第一个参数不是一个浮点数或者提供的第二个参数不是一个浮点数, 那么它应该发出一个错误.

* 也见(See Also): None.

* 注意(Notes):

        The product of the first result of decode-float or integer-decode-float, of the radix raised to the power of the second result, and of the third result is exactly equal to the value of float.

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

        number---一个实数.
        prototype---一个浮点数.
        float---一个浮点数.

* 描述(Description):

        float 把一个实数转换为浮点数.

        如果提供了一个 prototype, 会返回一个和 number 数学上相等的但是有着和 prototype 相同格式的浮点数.

        如果没有提供 prototype, 那么如果这个数字 number 已经是一个浮点数, 就把它返回; 否则, 就返回一个和 number 数学上相等的但是是一个单精度浮点的浮点数.

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

* 也见(See Also):

        coerce

* 注意(Notes): None. 


### <span id="F-FLOATP">函数 FLOATP</span>

* 语法(Syntax):

        floatp object

        generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 float 类型的就返回 true; 否则, 返回 false.

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

* 也见(See Also): None.

* 注意(Notes):

        (floatp object) ==  (typep object 'float)


### <span id="CV-MOST-LEAST">常量 MOST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-SHORT-FLOAT, LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT, MOST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-DOUBLE-FLOAT, LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT, MOST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-LONG-FLOAT, LEAST-POSITIVE-NORMALIZED-LONG-FLOAT, MOST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-SINGLE-FLOAT, LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-SHORT-FLOAT, LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT, MOST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-SINGLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT, MOST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-DOUBLE-FLOAT, LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT, MOST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-LONG-FLOAT, LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT</span>

* 常量值(Constant Value):

        依赖于具体实现.

* 描述(Description):

        这些常量为程序去检测具体实现为多种浮点格式定义的限制提供了一种方式.

        这些变量中, 每个名字中有着 "-normalized" 的必须有着一个标准化浮点数的值, 而每个名字中没有 "-normalized" 的可能有着一个标准化的或者非标准化的浮点数的值, 视情况而定.

        这些变量中, 每个名字中有着 "short-float" 的必须有着一个短浮点数的值, 每个名字中有着 "single-float" 必须有着一个单精度浮点数的值, 每个名字中有着 "double-float" 的必须有着一个双精度浮点数的值, 而每个名字中有着 "long-float" 的必须有着一个长浮点数的值.

            most-positive-short-float, most-positive-single-float, most-positive-double-float, most-positive-long-float

            对于这些常量的名字所暗示的浮点格式, 这些常量中的每一个都有着作为它的值最大 (最接近, 但不等于正无穷) 正浮点数.

            least-positive-short-float, least-positive-normalized-short-float, least-positive-single-float, least-positive-normalized-single-float, least-positive-double-float, least-positive-normalized-double-float, least-positive-long-float, least-positive-normalized-long-float

            对于这些常量的名字所暗示的浮点格式, 这些常量中的每一个都有着作为它的值的最小 (非零) 正浮点数.

            least-negative-short-float, least-negative-normalized-short-float, least-negative-single-float, least-negative-normalized-single-float, least-negative-double-float, least-negative-normalized-double-float, least-negative-long-float, least-negative-normalized-long-float

            对于这些常量的名字所暗示的浮点格式, 这些常量中的每一个都有着作为它的值的最小 (非零) 负浮点数. (如果一个实现支持 -0 作为一个和 +0 不同的对象, 这个值一定不能是 -0.)

            most-negative-short-float, most-negative-single-float, most-negative-double-float, most-negative-long-float

            对于这些常量的名字所暗示的浮点格式, 这些常量中的每一个都有着作为它的值的最大 (值接近于, 但不等于负无穷) 负浮点数.

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes):


### <span id="CV-EPSILON">常量 SHORT-FLOAT-EPSILON, SHORT-FLOAT-NEGATIVE-EPSILON, SINGLE-FLOAT-EPSILON, SINGLE-FLOAT-NEGATIVE-EPSILON, DOUBLE-FLOAT-EPSILON, DOUBLE-FLOAT-NEGATIVE-EPSILON, LONG-FLOAT-EPSILON, LONG-FLOAT-NEGATIVE-EPSILON</span>

* 常量值(Constant Value):

        依赖于具体实现.

* 描述(Description):

        这些常量 short-float-epsilon, single-float-epsilon, double-float-epsilon, 和 long-float-epsilon 中的每一个的值都是给定格式下的最小正的浮点极小值 <EPSILON>, 这样一来下面表达式在求值时是 true 的:

        (not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>)))

        这些常量 short-float-negative-epsilon, single-float-negative-epsilon, double-float-negative-epsilon, 和 long-float-negative-epsilon 中的每一个的值都是给定格式下的最小正的浮点极小值, 这样一来下面表达式在求值时是 true 的:

        (not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>)))

* 示例(Examples): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="CT-ARITHMETIC-ERROR">状况类型 ARITHMETIC-ERROR</span>

* 类优先级列表(Class Precedence List):

        arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 arithmetic-error 由运算操作期间发生的错误状况组成. 这个操作符和操作数通过给 make-condition 的名为 :operation 和 :operands 的初始化参数来初始化, 并且可以通过函数 arithmetic-error-operation 和 arithmetic-error-operands 访问.

* 也见(See Also):

        arithmetic-error-operation, arithmetic-error-operands 


### <span id="F-AEO-AEO">函数 ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION</span>

* 语法(Syntax):

        arithmetic-error-operands condition => operands

        arithmetic-error-operation condition => operation

* 参数和值(Arguments and Values):

        condition---一个 arithmetic-error 类型的状况.
        operands---一个列表.
        operation---一个函数标识符.

* 描述(Description):

        arithmetic-error-operands 返回一个操作数的列表, 这些操作数被用于对发出这个状况的操作符的违规调用中.

        arithmetic-error-operation 返回一个在发出这个状况的违规调用中违规的操作符列表.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        arithmetic-error, 章节 9 (Conditions)

* 注意(Notes):


### <span id="CT-DIVISION-BY-ZERO">状况类型 DIVISION-BY-ZERO</span>

* 类优先级列表(Class Precedence List):

        division-by-zero, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 division-by-zero 由因为被 0 除所导致的错误状况所组成. 

<!--TODO 浮点陷阱 ??-->
### <span id="CT-FLOATING-POINT-INVALID-OPERATION">状况类型 FLOATING-POINT-INVALID-OPERATION</span>

* 类优先级列表(Class Precedence List):

        floating-point-invalid-operation, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 floating-point-invalid-operation 由因为某些浮点陷阱造成的错误状况组成.

        浮点陷阱是否发生, 并且它们是否或如何被启用或禁用是依赖于具体实现的. 因此, 符合规范的代码可能为这个状况建立处理者, 但是一定不能依赖于它的发送.  

### <span id="CT-FLOATING-POINT-INEXACT">状况类型 FLOATING-POINT-INEXACT</span>

* 类优先级列表(Class Precedence List):

        floating-point-inexact, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 floating-point-inexact 由因为某些浮点陷阱造成的错误状况组成.

        浮点陷阱是否发生, 并且它们是否或如何被启用或禁用是依赖于具体实现的. 因此, 符合规范的代码可能为这个状况建立处理者, 但是一定不能依赖于它的发送. 

### <span id="CT-FLOATING-POINT-OVERFLOW">状况类型 FLOATING-POINT-OVERFLOW</span>

* 类优先级列表(Class Precedence List):

        floating-point-overflow, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 floating-point-overflow 由因为浮点上溢造成的错误状况组成. 


### <span id="CT-FLOATING-POINT-UNDERFLOW">状况类型 FLOATING-POINT-UNDERFLOW</span>

* 类优先级列表(Class Precedence List):

        floating-point-underflow, arithmetic-error, error, serious-condition, condition, t

* 描述(Description):

        类型 floating-point-underflow 由因为浮点下溢造成的错误状况组成. 


