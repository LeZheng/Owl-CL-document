# 8. 结构体

## 8.1 结构体的字典

> * [宏 DEFSTRUCT](#MacroDEFSTRUCT)
> * [函数 COPY-STRUCTURE](#FunctionCOPYSTRUCTURE)

### <span id="MacroDEFSTRUCT">宏 DEFSTRUCT</span>

* 语法(Syntax):

        defstruct name-and-options [documentation] {slot-description}*
        => structure-name

        name-and-options::= structure-name | (structure-name [[options]]) 

        options::= conc-name-option | 
                  {constructor-option}* | 
                  copier-option | 
                  include-option | 
                  initial-offset-option | 
                  named-option | 
                  predicate-option | 
                  printer-option | 
                  type-option 

        conc-name-option::= :conc-name | (:conc-name) | (:conc-name conc-name) 

        constructor-option::= :constructor | 
                              (:constructor) | 
                              (:constructor constructor-name) | 
                              (:constructor constructor-name constructor-arglist) 

        copier-option::= :copier | (:copier) | (:copier copier-name) 

        predicate-option::= :predicate | (:predicate) | (:predicate predicate-name) 

        include-option::= (:include included-structure-name {slot-description}*) 

        printer-option::= print-object-option | print-function-option 

        print-object-option::= (:print-object printer-name) | (:print-object) 

        print-function-option::= (:print-function printer-name) | (:print-function) 

        type-option::= (:type type) 

        named-option::= :named 

        initial-offset-option::= (:initial-offset initial-offset) 

        slot-description::= slot-name |  
                            (slot-name [slot-initform [[slot-option]]]) 

        slot-option::= :type slot-type |  
                      :read-only slot-read-only-p 

* 参数和值(Arguments and Values):

        conc-name---一个字符串标识符[string designator].
        constructor-arglist---一个 boa lambda 列表[boa lambda list].
        constructor-name---一个符号[symbol].
        copier-name---一个符号[symbol].
        included-structure-name---一个已经定义的结构体名字[structure name]. 注意, 派生类型[derived type]是不允许的, 即使它会展开成一个结构体名称[structure name].
        initial-offset---一个非负整数[integer].
        predicate-name---一个符号[symbol].
        printer-name---一个函数名字[function name]或一个 lambda 表达式[lambda expression].
        slot-name---一个符号[symbol].
        slot-initform---一个表达式形式[form].
        slot-read-only-p---一个广义的 boolean [generalized boolean].
        structure-name---一个符号[symbol].
        type---类型指定符[type specifier] list, vector, 或者 (vector size) 之一, 或者其他某个具体实现[implementation]定义为合适的类型指定符[type specifier].
        documentation---一个字符串[string]; 不求值.

* 描述(Description):

        defstruct 定义一个结构化的类型[type], 名为 structure-type, 并带有由槽选项 slot-options 指定的已命名的槽.

        defstruct 为槽定义读取器[reader]并且为 setf 能在这样的读取器[reader]函数上正常工作做准备. 而且, 除非被重写, 它都会定义一个名为 name-p 的断言, 定义一个名为 make-constructor-name 的构造函数, 并且定义一个名为 copy-constructor-name 的复制函数. 所有这些自动创建的函数的名字可能被自动声明为 inline (由具体实现[implementation]决定).

        如果提供了文档 documentation, 它会作为 structure 种类的文档字符串[documentation string]绑定给 structure-name, 并且除非使用了 :type, 这个 documentation 也作为 type 种类的文档字符串[documentation string]绑定给 structure-name 并且作为文档字符串[documentation string]绑定给名为 structure-name 的类[class]的类[class]对象[object].

        defstruct 定义一个构造函数被用于创建由 defstruct 创建的结构体的实例. 默认名字是 make-structure-name. 可以通过给 constructor 选项传递名字作为参数来提供一个不同的名字. nil 表示没有构造函数会被创建.

        在一个新的结构体类型被定义后, 那个类型的实例通常可以使用这个类型的构造函数来创建. 一个对构造函数的调用如下:

        (constructor-function-name
        slot-keyword-1 form-1
        slot-keyword-2 form-2
        ...)

        给这个构造函数的参数都是关键字参数. 每个槽关键字参数必须是一个名字对应一个结构体槽名字的关键字. 所有这些关键字 keywords 和表达式形式 forms 被求值. 如果一个槽没有以这种方式被初始化, 它会在构造函数被调用时通过求值槽描述中的 slot-initform 来初始化. 如果没有提供 slot-initform 而在显式赋值前去试读取这个槽的值, 结果是未定义的.

        为一个 defstruct 组件提供的每个 slot-initform, 当被构造函数用于其他未提供的组件时, 在每个对这个构造函数的调用上被再次求值. 这个 slot-initform 不被求值除非在一个特定结构体实例的创建中需要它. 如果从不需要它, 这里可以没有类型不匹配的错误, 即便指定了这个槽的类型[type]; 在这个情况下没有警告应该被发出. 比如, 在下面顺序中, 只有最后一个调用是一个错误.

    ```LISP
    (defstruct person (name 007 :type string)) 
    (make-person :name "James")
    (make-person)
    ```

        它就好像 slot-initforms 被用作这个构造函数的关键字参数[keyword parameter]的初始化表达式形式[initialization form].

        命名槽的这些符号[symbol]一定不能被具体实现[implementation]用作这个构造函数中 lambda 变量[lambda variable]的名字[name], 因为这些符号[symbol]中的一个或多个可能已经被公告为 special 或可能被定义为一个常变量[constant variable]的名字. 这个槽默认初始化表达式形式在这个 defstruct 表达式形式出现的词法环境[lexical environment]中以及对这个构造函数调用所出现的动态环境[dynamic environment]中被求值.

        比如, 如果表达式形式 (gensym) 被用作一个初始化表达式形式, 不管是在构造函数调用中或是作为 defstruct 的默认初始化表达式形式, 每个对这个构造函数的调用都会调用 gensym 来产生一个新的符号[symbol].

        在 defstruct 中的每个槽描述 slot-description 都可以指定 0 个或多个槽选项 slot-options. 一个 slot-option 由一个关键字和值(它不是一个要被求值的表达式形式, 而是值本身)的对组成. 比如:

    ```LISP
    (defstruct ship
      (x-position 0.0 :type short-float)
      (y-position 0.0 :type short-float)
      (x-velocity 0.0 :type short-float)
      (y-velocity 0.0 :type short-float)
      (mass *default-ship-mass* :type short-float :read-only t))
    ```

        这个指定了每个槽总是包含一个短浮点数[short float], 并且一旦一个 ship 被构造最后一个槽不能被修改.

        可用的槽选项是:

            :type type

                这个指定了这个槽的内容总是为 type 类型. 这完全类似于一个变量或函数的声明; 它有效地声明这个读取器[reader]函数的结果类型. 在初始化一个槽或者对它赋值时类型[type]是否被检测是依赖于具体实现[implementation-dependent]. 这个 type 不求值; 它必须是一个有效的类型指定符[type specifier].

            :read-only x

                当 x 是 true 时, 这个指定了这个槽不能被修改; 它总是会包含构造时提供的值. setf 不会接受这个槽的读取器[reader]函数. 如果 x 是 false, 这个 slot-option 就没有效果. x 不会被求值.

                当这个选项是 false 或没提供, 写入[write]这个槽的能力是通过一个 setf 函数[setf function]还是一个 setf 展开器[setf expander]实现的依赖于具体实现[implementation-dependent].

        下面的关键字选项可用于 defstruct. 一个 defstruct 选项可以是一个关键字或一个关键字和它的参数的列表[list]; 单独指定关键字相当于指定一个包含关键字和无参数的列表. 这个 defstruct 选项的语法有别于槽选项使用的语法. 这些选项的任何部分都不会被求值.

            :conc-name

                这个为读取器[reader] (或访问[access])函数的名字提供自动前缀. 默认行为是一个结构体的所有读取器[reader]函数的名字都以这个结构体的名字后面跟着连字符开始.

                :conc-name 提供一个要被使用的替代前缀, 如果一个连字符被用作分隔符, 它必须作为这个前缀的部分被提供. 如果 :conc-name 是 nil 或没有提供参数, 那么没有前缀被使用; 而这个读取器[reader]函数的名字就和槽的名字相同. 如果提供一个非 nil 前缀, 每个槽的这个读取器[reader]函数[function]的名字由这个前缀和槽的名字拼接构造而成, 并且在这个 defstruct 表达式形式被展开的当前包[package]中捕捉产生的符号[symbol].

                注意, 无论为 :conc-name 提供了什么, 匹配槽名字的没有关联前缀的槽关键字和构造函数一起使用. 这个读取器函数名可以和 setf 一起使用. 这里有一个例子:

                (defstruct (door (:conc-name dr-)) knob-color width material) =>  DOOR
                (setq my-door (make-door :knob-color 'red :width 5.0)) 
                =>  #S(DOOR :KNOB-COLOR RED :WIDTH 5.0 :MATERIAL NIL)
                (dr-width my-door) =>  5.0
                (setf (dr-width my-door) 43.7) =>  43.7
                (dr-width my-door) =>  43.7

                不管这个 :conc-name 选项是非被显式提供, 下面规则决定了生成的读取器[reader] (或访问器[accessor])名字的名称冲突: 对于任何有着名为 X1 的一个名为 R 的读取器[reader]函数的结构体[structure]类型[type] S1, 它被另一个结构体[structure]类型[type] S2 继承，而 S2 的名为 X2 的槽有着相同名字 R 的读取器[reader]函数, S2 的定义不会为 R 生成定义; 反而, 这个 R 的定义从 S1 的定义继承而来. (在这种情况下, 如果 X1 和 X2 是不同的槽, 具体实现[implementation]可能会发出一个风格警告.)

            :constructor

                这个选项接受 0 个, 1 个或 2 个参数. 如果至少提供了一个参数并且第一个参数不是 nil, 那么那个参数是一个指定这个构造函数名字的符号[symbol]. 如果这个参数没有被提供 (或者如果这个选项自身就没有被提供), 那么这个构造器的名字通过字符串 "MAKE-" 和这个结构体的名字拼接而成, 在 defstruct 被展开时当前包[package]中捕捉这个名字. 如果提供了这个参数并且是 nil, 那么没有构造函数被定义.

                如果 :constructor 以 (:constructor name arglist) 这种形式给定, 那么代替创建一个关键字驱动的构造函数, defstruct 会定义一个 "位置参数(positional)" 构造函数, 接受的参数的意义由这个参数的位置来决定, 也可能由关键字决定. 参数列表 arglist 被用于描述给这个构造器的参数. 在像 (:constructor make-foo (a b c)) 这样最简单的情况中, 它定义了 make-foo 为一个三参数的构造函数, 这个构造函数的参数被用于初始化名为 a, b, 和 c 的槽.

                由于一个这个类型构造器 "根据参数的顺序(By Order of Arguments)" 来操作, 它有时也被认为是一个 "boa 构造器".

                关于一个 "boa 构造器" 的这个参数列表 arglist 如何被处理, 见章节 3.4.6 (Boa Lambda 列表).

                允许不止一次使用 :constructor 选项, 这样你可以定义多个不同的构造函数, 每一个都接收不同的参数.

                当且仅当没有指定显式的 :constructor 选项或者这个 :constructor 选项被指定但没有 name 参数时, defstruct 创建这个默认命名的关键字构造函数.

                只有在这里没有指定其他 :constructor 选项时, 这个 (:constructor nil) 是有意义的. 它阻止 defstruct 产生任何构造器.

                否则, defstruct 对应每个提供的 :constructor 选项创建一个构造函数. 允许指定多个关键字构造函数以及多个"boa 构造函数".

            :copier

                这个选项接受一个参数, 一个符号[symbol], 它指定了这个复制函数的名字. 如果没有提供这个参数或者没有提供这个选项, 这个复制器的名字由字符串 "COPY-" 和这个结构体的名字拼接而成, 在 defstruct 被展开时当前包[package]中捕捉这个名字. 如果提供的这个参数是 nil, 那么没有复制函数被定义.

                自动定义的这个赋值函数是一个单个实参[argument]的函数, 这个参数一定是要被定义的结构体类型. 这个赋值函数创建一个有着和它的实参[argument]相同类型[type]的新[fresh]结构体, 并且有着和原始结构体相同的成员值; 也就是说, 成员值不会被递归拷贝. 如果这个 defstruct :type 选项没有被使用, 下面的等价性适用:

                (copier-name x) = (copy-structure (the structure-name x))

            :include

                这个选项被用于构建一个新的结构体定义, 作为另一个结构体定义的扩展. 比如:

                (defstruct person name age sex)

                为了创建一个有着 name, age, 和 sex 属性以及在 person 结构体上操作的函数[function]的新的结构体 astronaut, astronaut 使用 :include 按如下定义:

                (defstruct (astronaut (:include person)
                                      (:conc-name astro-))
                    helmet-size
                    (favorite-beverage 'tang))

                :include 导致这个要被定义的结构体有着和被包含结构体相同的槽. 这样做使得被包括结构体的读取器[reader]函数也可以工作在要被定义的结构体上. 因此在此例中, 一个 astronaut 有五个槽: 三个在 person 中定义而两个在 astronaut 自身中定义. 通过 person 结构体定义的这些读取器[reader]函数可以被应用于这个 astronaut 结构体的实例, 并且它们会正常工作. 此外, astronaut 有着它自己的对于由 person 结构体定义的组件的读取器函数. 下面例子说明了 astronaut 结构体的使用:

                (setq x (make-astronaut :name 'buzz
                                        :age 45.
                                        :sex t
                                        :helmet-size 17.5))
                (person-name x) =>  BUZZ
                (astro-name x) =>  BUZZ
                (astro-favorite-beverage x) =>  TANG

                (reduce #'+ astros :key #'person-age) ; obtains the total of the ages 
                                                      ; of the possibly empty
                                                      ; sequence of astros

                读取器[reader]函数 person-name 和 astro-name 的区别是 person-name 可以准确应用于任何 person, 包括 astronaut, 而 astro-name 只能被应用于一个 astronaut. 一个具体实现可以检查读取器[reader]函数的不正确使用.

                在一个 defstruct 中最多只能提供一个 :include. 给 :include 的参数是必须的并且一定是某个之前定义的结构体的名字. 这个这个要被定义的结构体没有 :type 选项, 那么这个被包含的结构体也必须没有给它提供的 :type 选项. 如果这个要被定义的结构体有一个 :type 选项, 那么这个被包含的结构体必须用一个指定相同表示类型[type]的 :type 选项来声明.

                如果没有涉及 :type 选项, 那么这个被包含的结构体定义的结构体名字成为一个数据类型[data type]的名字, 因此一个有效的类型指定符[type specifier]被 typep 所识别; 它成为被包含结构体的一个子类型[subtype]. 在上述例子中, astronaut 是 person 的一个子类型[subtype]; 因此

                (typep (make-astronaut) 'person) =>  true

                表示 person 上的所有操作也可以工作在 astronaut 上.

                使用 :include 的结构体可以为包括的槽指定有别于被包含的结构体指定的默认值或槽选项, 通过像这样给定 :include 选项:

                (:include included-structure-name slot-description*)

                每个 slot-description 必须有一个和被包含的结构体这栋的某个槽相同的槽名字. 如果一个 slot-description 没有槽初始化表达式形式 slot-initform, 那么在这个新的结构体中那个槽就没有初始值. 否则它的初始值表达式形式就会被 slot-description 中的槽初始化表达式形式 slot-initform 替代. 一个普通的可写槽可以变为只读的. 如果一个槽在被包含的结构体中是只读的, 那么它在包含的结构体中也必须如此. 如果为一个槽提供了类型[type], 它必须是这个被包含的结构体中指定的类型[type]的子类型[subtype].

                比如, 如果一个 astronaut 中默认 age 是 45, 那么就是

                (defstruct (astronaut (:include person (age 45)))
                    helmet-size
                    (favorite-beverage 'tang))

                如果 :include 和 :type 选项一起使用, 那么效果是, 首先跳过表示被包含结构所需要的一样多的表示元素, 接着跳过任何 :initial-offset 选项提供的额外元素, 然后从这一点开始分配元素. 比如:

                (defstruct (binop (:type list) :named (:initial-offset 2))
                  (operator '? :type symbol)   
                  operand-1
                  operand-2) =>  BINOP
                (defstruct (annotated-binop (:type list)
                                            (:initial-offset 3)
                                            (:include binop))
                  commutative associative identity) =>  ANNOTATED-BINOP
                (make-annotated-binop :operator '*
                                      :operand-1 'x
                                      :operand-2 5
                                      :commutative t
                                      :associative t
                                      :identity 1)
                  =>  (NIL NIL BINOP * X 5 NIL NIL NIL T T 1)

                前两个 nil 元素源于 binop 定义中为 2 的 :initial-offset. 接下来的四个元素包含了结构体名字和 binop 的三个槽. 接下来的三个 nil 元素源自 annotated-binop 定义中 3 的 :initial-offset. 最后三个列表元素包含了  annotated-binop 的额外的槽.

            :initial-offset

                :initial-offset 指导 defstruct 在它开始分配这个主体中描述的槽之前去跳过一个确定的槽的数量. 这个选项的参数是 defstruct 应该跳过的槽的数量. :initial-offset 只能在 :type 也被提供时使用.

                :initial-offset 允许从一个具象元素开始分配槽而不是在第一个. 比如, 这个表达式形式

                (defstruct (binop (:type list) (:initial-offset 2))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                会导致以下 make-binop 的行为:

                (make-binop :operator '+ :operand-1 'x :operand-2 5)
                =>  (NIL NIL + X 5)
                (make-binop :operand-2 4 :operator '*)
                =>  (NIL NIL * NIL 4)

                选择器函数 binop-operator, binop-operand-1, 和 binop-operand-2 本质上分别等价于 third, fourth, 和 fifth. 类似的, 表达式形式

                (defstruct (binop (:type list) :named (:initial-offset 2))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                会导致以下 make-binop 的行为:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (NIL NIL BINOP + X 5)
                (make-binop :operand-2 4 :operator '*) =>  (NIL NIL BINOP * NIL 4)

                前两个 nil 元素源自 binop 的定义中 2 的 :initial-offset. 接下来四个元素包含这个结构体的名字和三个 binop 的槽.

            :named

                :named 指定这个结构体是已命名的. 如果没有提供 :type, 那么结构体总是已命名的.

                例如:

                (defstruct (binop (:type list))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                这个定义了一个构造函数 make-binop 和三个选择器函数, 也就是 binop-operator, binop-operand-1, and binop-operand-2. (然而它不会定义一个断言 binop-p, 出于下面解释的原因.)

                这个 make-binop 的效果仅仅是构造一个长度 3 的列表:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (+ X 5)  
                (make-binop :operand-2 4 :operator '*) =>  (* NIL 4)

                它就像是函数 list, 除了它接受关键字参数并且执行适合于 binop 概念数据类型的槽默认值初始化.<!--TODO 待校对--> 同样地, 选择器函数 binop-operator, binop-operand-1, and binop-operand-2 本质上分别等价于 car, cadr, 和 caddr. 它们可能不是完全等价, 比如由于一个具体实现完全可以添加错误检测代码来确保给每个选择器函数的实参是长度为 3 的列表.

                binop 是一个概念数据类型, 由于它不是 Common Lisp 类型系统的一部分. typep 不识别 binop 为一个类型指定符[type specifier], 并且当给定一个 binop 结构体时 type-of 返回 list. 这里没有方法去区分一个 make-binop 构造的数据类型和其他具有正确结构的列表[list].

                这里没有任何方式去从 make-binop 创建的一个结构体中重新获得这个结构体名字 binop. 只有在这个结构被命名时才可以这样做. 已命名结构具有这样的属性: 给定这个结构体的一个实例, 可以可靠地重新获得结构体名称(它命名这个类型). 对于不带有 :type 选项来定义的的结构体, 这个结构体名字实际上成为 Common Lisp 数据类型系统的一部分. type-of, 当应用于这样一个结构体时, 返回这个结构体的名字作为这个对象[object]的类型[type]; typep 识别这个结构体名字为一个有效的类型指定符[type specifier].

                对于使用一个 :type 选项定义的结构体, type-of 返回一个类型指定符[type specifier], 比如 list 或 (vector t), 取决于提供给这个 :type 选项的类型. 这个结构体名字不会称为一个有效类型指定符[type specifier]. 然而, 如果这个 :named 选项也被提供, 那么这个结构体(通过 defstruct 构造函数创建的)的第一个组件总是包含这个结构体的名字. 这允许这个结构体的名字从这个结构体的一个实例中被重新获取并且允许去为这个概念类型定义一个合理的断言: 自动定义的这个结构体的断言 name-p 通过首先检测它的参数为适当的类型 (list, (vector t), 或者诸如此类) 然后检测第一个组件是否包含了这个适当的类型名字.

                细想上面展示的 binop 例子, 仅修改为包含 :named 选项:

                (defstruct (binop (:type list) :named)
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                和之前一样, 这个定义一个构造函数 make-binop 和三个选择器函数 binop-operator, binop-operand-1, 和 binop-operand-2. 它也定义了断言 binop-p. 现在 make-binop 的效果是构造一个长度 4 的列表:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (BINOP + X 5)
                (make-binop :operand-2 4 :operator '*) =>  (BINOP * NIL 4)

                这个结构体和之前有着相同的布局除了这个结构体名字被包含作为第一个列表元素. 选择器函数 binop-operator, binop-operand-1, 和 binop-operand-2 本质上分别等价于 cadr, caddr, 和 cadddr. 断言 binop-p 或多或少等价于这个定义:

                (defun binop-p (x)
                  (and (consp x) (eq (car x) 'binop))) =>  BINOP-P

                名字 binop 对于 typep 仍然不是一个有效的可识别的类型指定符[type specifier], 但是至少这里有一种区分 binop 和其他类似定义的结构体的方法.

            :predicate

                这个选项接受一个参数, 它表示这个类型断言的名字. 如果这个参数没有被提供或者这个选项自身没有被提供, 这个断言的名字通过拼接这个结构体的名字到字符串 "-P" 来完成, 在 defstruct 被展开的当前包[package]中捕获这个名字. 如果提供了这个参数并且是 nil, 那么没有断言会被定义. 当且仅当这个结构体是已命名的时候一个断言可以被定义; 如果提供了 :type 而 :named 没有被提供, 那么 :predicate 必须是未提供的或者值为 nil.

            :print-function, :print-object

                这个 :print-function 和 :print-object 选项指定了应该为类型 structure-name 的结构体[structure]生成一个 print-object 方法[method]. 这些选项不是同义词, 而是执行类似的服务; 选择使用哪一个选项 (:print-function 或 :print-object) 影响这个名为 printer-name 的函数如何被调用. 这些选项只有一个可以被使用, 并且这些选项只能在 :type 没有被提供时使用.

                如果这个 :print-function 选项被使用, 那么当一个类型 structure-name 的结构体要被打印时, 指定的这个打印函数在三个实参[argument]上被调用:

                    要被打印的结构体 (一个普通的 structure-name 的实例[generalized instance]).

                    一个要打印到的流[stream].

                    一个表示当前深度的整数[integer]. 这个整数的大小可能在不同的具体实现[implementation]之间有所不同; 但是, 它可以可靠地与 *print-level* 进行比较, 以确定深度缩写(depth abbreviation)是否合适.

                指定 (:print-function printer-name) 近似等价于指定:

                (defmethod print-object ((object structure-name) stream)
                  (funcall (function printer-name) object stream <<current-print-depth>>))

                其中这个 <<current-print-depth>> 表示这个打印器对当前打印深度的判断. <<current-print-depth>> 是否总是为 0 以及 *print-level* 如果非 nil [non-nil]的话是否随着打印递归递减陆续会被重新绑定到更小的值依赖于具体实现[implementation-dependent], 或者说 current-print-depth 的值随着打印递归递减是否会改变而 *print-level* 在相同的遍历过程中是否保持不变依赖于具体实现[implementation-dependent].

                如果使用了这个 :print-object 选项, 那么当一个类型 structure-name 的结构体要被打印时, 指定的打印函数会在两个参数上被调用:

                    要被打印的结构体.

                    要打印到的流.

                指定 (:print-object printer-name) 等价于指定:

                (defmethod print-object ((object structure-name) stream)
                  (funcall (function printer-name) object stream))

                如果没有提供 :type 选项, 并且提供一个 :print-function 或一个 :print-object 选项, 也没有提供 printer-name, 那么会生成一个为 structure-name 特化[specialized]的 print-object 方法[method], 它使用 #S 标记实现了结构体的默认打印行为; 见章节 22.1.3.12 (打印结构体).

                如果既没有提供一个 :print-function 选项也没有提供一个 :print-object 选项, 那么 defstruct 不会生成一个为 structure-name 特化[specialized]的 print-object 方法[method], 而是从一个 :include 选项中的已命名结构体或从打印结构体的默认行为中继承某个默认行为; 见函数[function] print-object 和章节 22.1.3.12 (打印结构体).

                当 *print-circle* 是 true 时, 一个用户定义的打印函数可以使用 write, prin1, princ, or format 来打印对象[object]到提供的流[stream]中并且认为循环会被检测并且通过 #n# 语法打印. 这个应用于 print-object 上的方法[method], 除了 :print-function 选项. 如果用户提供的打印函数打印到一个流[stream]而不是提供的那个, 那么为这个流[stream]重新开始循环检测. 见变量[variable] *print-circle*.

            :type

                :type 显式指定了这个结构体要使用的表示法. 它的参数必须是以下类型[type]之一:

                vector

                    这个和指定 (vector t) 产生相同的结果. 这个结构体被表示为普通的向量[vector], 把组件存储为向量元素. 如果这个结构体是 :named 那么第一个组件是向量元素 1, 否则就是元素 0.

                (vector element-type)

                    这个结构体被表示为一个 (可能是特化的) 向量[vector], 把组件存储为向量元素. 每个组件必须是可以存储到指定类型[type]的向量[vector]中的类型[type]. 如果这个结构体是 :named 那么第一个组件是 vector 的元素 1, 否则就是元素 0. 当且仅当这个类型[symbol]符号是提供的 element-type 的一个子类型[subtype]时, 这个结构体可以是 :named.<!--TODO 最后一句有歧义，看原文-->

                list

                    这个结构体被表示为一个列表[list]. 如果这个结构体是 :named 那么第一个元素就是 cadr 那个, 如果不是 :named 那么就是 car 那个.

                指定这个选项的效果是, 强制一个特定的表示, 并强制将组件按照在 defstruct 中指定的顺序存储到指定的表示法的相应连续元素中. 它还可以防止结构名称成为 typep 所识别的有效类型指定符[type specifier].

                比如:

                (defstruct (quux (:type list) :named) x y)

                应该产生一个构造器, 它构造一个列表[list], 就像是 list 创建的一样, 其中 quux 作为它的 car.

                如果这个类型如下定义:

                (deftype quux () '(satisfies quux-p))

                那么这个表达式形式

                (typep (make-quux) 'quux)

                应该准确返回下面这个所做的结果

                (typep (list 'quux nil nil) 'quux)

                如果没有提供 :type, 这个结构体被表示为一个 structure-object 类型[type]的对象[object].

                没有 :type 选项的 defstruct 定义一个类[class], 其中这个结构体的名字作为它的名字. 结构体实例[instance]的元类[metaclass]是 structure-class.

        重定义一个 defstruct 结构体的后果是未定义的.

        在没有提供 defstruct 选项的情况下, 以下函数会被自动定义, 用来操作这个新的结构体的实例:

            断言(Predicate)

                定义一个名为 structure-name-p 的断言, 用来测试这个结构体类型中的从属关系. 如果一个对象 object 是这个类型[type]的, 那么断言 (structure-name-p object) 就是 true; 否则它就是 false. typep 也可以和这个新类型[type]的名字一起使用来检测一个对象[object]是否为这个类型[type]. 这样一个函数调用的形式为 (typep object 'structure-name).

            组件读取器函数(Component reader functions)

                定义读取器[reader]函数, 用来读取这个结构体的组件. 对于每个槽的名字, 这里有一个对应的名为 structure-name-slot-name 的读取器[reader]函数. 这个函数读取[read]那个槽的内容. 每个读取器[reader]函数接收一个参数, 它是这个结构体类型的一个实例. setf 可以和这些读取器[reader]函数中的任何一个一起使用来修改槽的内容.

            构造函数(Constructor function)

                定义一个名为 make-structure-name 的构造函数. 这个函数创建并返回这个结构体类型的新的实例.

            复制函数(Copier function)

                定义一个名为 copy-structure-name 的复制函数. 这个赋值函数接收一个该结构体类型的对象并创建一个相同类型的新的对象, 它是第一个对象的复制. 复制函数使用和原始相同的组件条目来创建一个新的结构体. 两个结构体实例的对应组件是 eql 的.

          如果一个 defstruct 表达式形式[form]作为顶层表达式形式[top level form]出现, 编译器[compiler]必须使这个结构体[structure]类型[type]名字在后续的声明中(比如 deftype)可以被识别为一个有效类型[type]名字并且使得结构体槽的读取器[reader]函数被 setf 识别. 另外, 编译器[compiler]必须保存足够的关于结构体[structure]类型[type]的信息以便在同一个文件[file]的后续的 deftype 中的进一步结构体定义可以使用 :include 来引用这个结构体[structure]类型[type]的名字. defstruct 产生的函数不会定义在编译时环境, 虽然编译器[compiler]可能保存足够的关于这个函数的信息来把后续的调用编码为 inline 的. 这个 #S 读取器宏[reader macro]在编译时可能或可能不会识别这个新定义的结构体[structure]类型[type].

* 示例(Examples):

        一个结构体定义的示例如下:

    ```LISP
    (defstruct ship
      x-position
      y-position
      x-velocity
      y-velocity
      mass)
    ```

        这个声明每一个 ship 是一个带有五个已命名组件的对象[object]. 这个表达式形式的求值做了以下这些事:

            它定义 ship-x-position 为一个函数, 接受一个参数, 一个 ship, 返回这个 ship 的 x-position; ship-y-position 和其他组件都给定了类似的函数定义. 这些函数被称为访问[access]函数, 由于它们被用来访问[access]这个结构体的元素.

            ship 成为一个类型[type]的名字, ship 的实例就是这个类型的元素. ship 变得可应用于 typep, 比如; 如果 x 是一个 ship 那么 (typep x 'ship) 是 true, 如果 x 是除了 ship 以外的任何对象[object]那么就是 false.

            一个名为 ship-p 的单参数函数被定义; 这是一个断言, 如果它的参数是一个 ship 那么就是 true 否则就是 false.

            一个名为 make-ship 函数会被定义, 当被调用时, 创建一个带有五个组件的数据结构, 和访问[access]函数一起使用是合适的. 因此执行

            (setq ship2 (make-ship))

            设置 ship2 为一个新创建的 ship 对象[object]. 在一个 make-ship 的调用中可以通过下面这种方式使用关键字参数来提供任何想要的组件的初始值:

            (setq ship2 (make-ship :mass *default-ship-mass*
                                    :x-position 0
                                    :y-position 0))

            这个构造一个新的 ship 并且初始化它的组件中的三个. 这个函数也被称作 "构造函数(constructor function)" 因为它构造一个新的结构体.

            一个名为 copy-ship 的单参数函数被定义, 当给定一个 ship 对象[object]时, 创建一个新的 ship 对象[object], 这个对象是给定的那个的一个复制. 这个函数被称作 "复制函数(copier function)".

        setf 可以被用于修改一个 ship 的组件:

        (setf (ship-x-position ship2) 100)

        这个修改 ship2 的 x-position 为 100. 这个可以工作是因为 defstruct 表现得就好像它为每个访问[access]函数生成了一个合适的 defsetf.

    ```LISP
    ;;;
    ;;; Example 1
    ;;; define town structure type
    ;;; area, watertowers, firetrucks, population, elevation are its components
    ;;;
    (defstruct town
                area
                watertowers
                (firetrucks 1 :type fixnum)    ;an initialized slot
                population 
                (elevation 5128 :read-only t)) ;a slot that can't be changed
    =>  TOWN
    ;create a town instance
    (setq town1 (make-town :area 0 :watertowers 0)) =>  #S(TOWN...)
    ;town's predicate recognizes the new instance
    (town-p town1) =>  true
    ;new town's area is as specified by make-town
    (town-area town1) =>  0
    ;new town's elevation has initial value
    (town-elevation town1) =>  5128
    ;setf recognizes reader function
    (setf (town-population town1) 99) =>  99
    (town-population town1) =>  99
    ;copier function makes a copy of town1
    (setq town2 (copy-town town1)) =>  #S(TOWN...)
    (= (town-population town1) (town-population town2))  =>  true
    ;since elevation is a read-only slot, its value can be set only
    ;when the structure is created
    (setq town3 (make-town :area 0 :watertowers 3 :elevation 1200))
    =>  #S(TOWN...)
    ;;;
    ;;; Example 2
    ;;; define clown structure type
    ;;; this structure uses a nonstandard prefix
    ;;;
    (defstruct (clown (:conc-name bozo-))
                (nose-color 'red)         
                frizzy-hair-p polkadots) =>  CLOWN
    (setq funny-clown (make-clown)) =>  #S(CLOWN)
    ;use non-default reader name
    (bozo-nose-color funny-clown) =>  RED        
    (defstruct (klown (:constructor make-up-klown) ;similar def using other
                (:copier clone-klown)              ;customizing keywords
                (:predicate is-a-bozo-p))
                nose-color frizzy-hair-p polkadots) =>  klown
    ;custom constructor now exists
    (fboundp 'make-up-klown) =>  true
    ;;;
    ;;; Example 3
    ;;; define a vehicle structure type
    ;;; then define a truck structure type that includes 
    ;;; the vehicle structure
    ;;;
    (defstruct vehicle name year (diesel t :read-only t)) =>  VEHICLE
    (defstruct (truck (:include vehicle (year 79)))
                load-limit                          
                (axles 6)) =>  TRUCK
    (setq x (make-truck :name 'mac :diesel t :load-limit 17))
    =>  #S(TRUCK...)
    ;vehicle readers work on trucks
    (vehicle-name x)
    =>  MAC
    ;default taken from :include clause 
    (vehicle-year x)
    =>  79 
    (defstruct (pickup (:include truck))     ;pickup type includes truck
                camper long-bed four-wheel-drive) =>  PICKUP
    (setq x (make-pickup :name 'king :long-bed t)) =>  #S(PICKUP...)
    ;:include default inherited
    (pickup-year x) =>  79
    ;;;
    ;;; Example 4
    ;;; use of BOA constructors
    ;;;
    (defstruct (dfs-boa                      ;BOA constructors
                  (:constructor make-dfs-boa (a b c)) 
                  (:constructor create-dfs-boa
                    (a &optional b (c 'cc) &rest d &aux e (f 'ff))))
                a b c d e f) =>  DFS-BOA
    ;a, b, and c set by position, and the rest are uninitialized
    (setq x (make-dfs-boa 1 2 3)) =>  #(DFS-BOA...)
    (dfs-boa-a x) =>  1
    ;a and b set, c and f defaulted
    (setq x (create-dfs-boa 1 2)) =>  #(DFS-BOA...)
    (dfs-boa-b x) =>  2
    (eq (dfs-boa-c x) 'cc) =>  true
    ;a, b, and c set, and the rest are collected into d
    (setq x (create-dfs-boa 1 2 3 4 5 6)) =>  #(DFS-BOA...)
    (dfs-boa-d x) =>  (4 5 6)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何两个槽的名字(不管是直接出现或是通过 :include 选项继承)在 string= 下是相同的[same], defstruct 应该发出一个 program-error 类型[type]的错误.

        如果这个 included-structure-name 没有命名一个结构体[structure]类型[type], 那么结果是未定义的.

* 也见(See Also):

        documentation, print-object, setf, subtypep, type-of, typep, 章节 3.2 (编译)

* 注意(Notes):

        这个 printer-name 应该观察像 *print-escape* 这样的打印控制变量的值.

        对于 slot-initform 和对应槽的 :type 选项的类型不匹配引起的一个警告的约束是有必要的, 因为为了指定槽选项一个 slot-initform 必须被指定; 在某些情况下, 可能不存在合适的默认项.

        defstruct 安排槽访问器和 setf 是可用的机制依赖于具体实现[implementation-dependent]; 比如, 它可能使用 setf 函数[setf function], setf 展开器[setf expander], 或者某个依赖于具体实现的[implementation-dependent]对该具体实现[implementation]的 setf 代码[code]是已知的其他机制. 


### <span id="FunctionCOPYSTRUCTURE">函数 COPY-STRUCTURE</span>

* 语法(Syntax):

        copy-structure structure => copy

* 参数和值(Arguments and Values):

        structure---一个结构体[structure].
        copy---一个结构体 structure 的赋值.

* 描述(Description):

        返回这个结构体 structure 的一个复制体[copy[6]].

        只有这个结构体 structure 本身被赋值; 这些槽的值不会.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defstruct 的这个 :copier 选项

* 注意(Notes):

        这个复制体和给定的结构体 structure 在 equalp 下是相同的, 但是在 equal 下是不同的. 

