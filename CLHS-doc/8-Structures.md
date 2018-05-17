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

        conc-name---一个字符串表示符.
        constructor-arglist---一个 boa lambda 列表.
        constructor-name---一个符号.
        copier-name---一个符号.
        included-structure-name---一个已经定义的结构体名字. 注意, 派生类型是不允许的, 即使它会扩展成一个结构体名称.
        initial-offset---一个非负整数.
        predicate-name---一个符号.
        printer-name---一个函数名字或一个 lambda 表达式.
        slot-name---一个符号.
        slot-initform---一个表达式形式.
        slot-read-only-p---一个广义的 boolean.
        structure-name---一个符号.
        type---类型指定符 list, vector, 或者 (vector size) 之一, 或者其他某个具体实现定义的类型指定符也是合适的.
        documentation---一个字符串; 不求值.

* 描述(Description):

        defstruct 定义一个结构化的类型, 名为 structure-type, 并带有 slot-options 指定的已命名的槽.

        defstruct 为槽定义读取器并且为 setf 能在这样的读取器函数上正常工作做准备. 而且, 除非被重写, 它都会定义一个名为 name-p 的断言, 定义一个名为 make-constructor-name 的构造函数, 并且定义一个名为 copy-constructor-name 的构造函数. 所有这些自动创建的函数的名字可能被自动声明为 inline (由具体实现决定).

        如果提供了 documentation, 它会被绑定给 structure-name 作为 structure 种类的文档字符串, 并且除非使用了 :type, 这个 documentation 也绑定给 structure-name 作为 type 种类的文档字符串并且作为文档字符串绑定给名为 structure-name 的类的类对象.

        defstruct 定义一个构造器函数被用于创建由 defstruct 创建的结构体的实例. 默认名字是 make-structure-name. 可以通过给 constructor 选项传递名字来提供一个不同的名字. nil 表示没有构造器函数会被创建.

        在一个新的结构体类型被定义后, 那个类型的实例通常可以使用这个类型的构造器函数来创建. 一个对构造器函数的调用如下:

        (constructor-function-name
        slot-keyword-1 form-1
        slot-keyword-2 form-2
        ...)

        给这个构造器函数的参数都是关键字参数. 每个槽关键字参数必须是一个名字对应一个结构体槽的名字的关键字. 所有这些关键字和表达式形式被求值. 如果一个槽没有以这种方式被初始化, 它会在构造器函数被调用时通过求值槽描述中的 slot-initform 来初始化, 如果尝试在一个值被显式赋予前读取这个槽的值, 那么结果是未定义的.

        为一个 defstruct 组件提供的每个 slot-initform, 当被构造函数用于其他未提供的组件时, 在每个对这个构造函数的调用上被再次求值. 这个 slot-initform 不被求值除非在一个特殊结构体实例的创建中需要它. 如果从不需要它, 这里可以没有类型不匹配的错误, 即便这个槽的类型没有指定; 在这个情况下没有警告应该被发出. 比如, 在下面序列中, 只有最后一个调用是一个错误.<!--TODO 待校验-->

    ```LISP
    (defstruct person (name 007 :type string)) 
    (make-person :name "James")
    (make-person)
    ```

        它就好像 slot-initforms 被用作这个构造函数的关键字的初始化表达式形式.

        命名槽的这些符号一定不能被具体实现用作这个构造函数中 lambda 变量的名字, 因为这些符号中的一个或多个可能已经被声明未 special 或可能被定义未一个常量的名字. 这个槽默认初始化表达式形式在这个 defstruct 表达式形式出现的词法环境中, 在对这个构造函数调用出现的动态环境中被求值.

        比如, 如果表达式形式 (gensym) 被用作一个初始化表达式形式, 不管是在构造函数调用中或是作为 defstruct 的默认初始化表达式形式, 每个对这个构造函数的调用都会调用 gensym 来产生一个新的符号.

        在 defstruct 中的每个 slot-description 都可以指定 0 个或多个 slot-options. 一个 slot-option 由一个关键字和值(它不是一个要被求值的表达式形式, 而是值本身)的对组成. 比如:

    ```LISP
    (defstruct ship
      (x-position 0.0 :type short-float)
      (y-position 0.0 :type short-float)
      (x-velocity 0.0 :type short-float)
      (y-velocity 0.0 :type short-float)
      (mass *default-ship-mass* :type short-float :read-only t))
    ```

        这个指定了每个槽总是包含一个短浮点数(short float), 以及一旦一个 ship 被构造最后一个槽不能被修改.

        可用的 slot-options 是:

            :type type

                这个指定了这个槽的内容总是为 type 类型. 这完全类似于一个变量或函数的声明; 它有效地声明这个 reader 函数的结果类型. 在初始化一个槽或者对它赋值时类型是否被检测是依赖于具体实现的. Type 不求值; 它必须是一个有效的类型指定符.

            :read-only x

                当 x 是 true 时, 这个指定了这个槽不能被修改; 它总是会包含构造时提供的值. setf 不会接受这个槽的 reader 函数. 如果 x 是 false, 这个 slot-option 就没有效果. X 不会被求值.

                当这个选项是 false 或没提供, 写入这个槽的能力是否通过一个 setf 函数或一个 setf 展开器实现是依赖于具体实现的.

        下面的关键字选项可用于 defstruct. 一个 defstruct 选项可以是一个关键字或一个关键字和它的参数的列表; 单独指定关键字相当于指定一个包含关键字和无参数的列表. 这个 defstruct 选项的语法有别于 slot-options 使用的语法. 这些选项的任何部分都没有被求值.

            :conc-name

                这个为读取器(或访问)函数的名字提供自动前缀. 默认行为是一个结构体的所有读取器函数的名字都以这个结构体的名字后面跟着连字符开始.

                :conc-name 提供一个要被使用的替代前缀, 如果一个连字符被用作分隔符, 它必须作为这个前缀的部分被提供. 如果 :conc-name 是 nil 或没有提供参数, 那么没有前缀被使用; 而这个读取器函数的名字就和槽的名字相同. 如果提供一个非 nil 前缀, 每个槽的这个读取器函数的名字由这个前缀和槽的名字拼接构造而成, 并且在这个 defstruct 表达式形式被展开的当前包中捕捉产生的符号.

                注意, 无论为 :conc-name 提供了生么, 匹配槽名字的没有关联前缀的槽关键字和构造函数一起使用. 这个读取器函数名可以和 setf 一起使用. 这里有一个例子:

                (defstruct (door (:conc-name dr-)) knob-color width material) =>  DOOR
                (setq my-door (make-door :knob-color 'red :width 5.0)) 
                =>  #S(DOOR :KNOB-COLOR RED :WIDTH 5.0 :MATERIAL NIL)
                (dr-width my-door) =>  5.0
                (setf (dr-width my-door) 43.7) =>  43.7
                (dr-width my-door) =>  43.7

                不管这个 :conc-name 选项是非被显示提供, 下面规则决定了生成的读取器(或访问器)名字的名称冲突: 对于任何有着名为 X1 的一个名为 R 的读取器函数的结构体类型 S1, 它被另一个结构体类型 S2 继承，而 S2 的名为 X2 的槽有着相同名字 R 的读取器函数, S2 的定义不会为 R 生成定义; 反而, 这个 R 的定义从 S1 的定义继承而来. (在这种情况下, 如果 X1 和 X2 是不同的槽, 具体实现可能会发出一个风格警告.)

            :constructor

                This option takes zero, one, or two arguments. If at least one argument is supplied and the first argument is not nil, then that argument is a symbol which specifies the name of the constructor function. If the argument is not supplied (or if the option itself is not supplied), the name of the constructor is produced by concatenating the string "MAKE-" and the name of the structure, interning the name in whatever package is current at the time defstruct is expanded. If the argument is provided and is nil, no constructor function is defined.

                If :constructor is given as (:constructor name arglist), then instead of making a keyword driven constructor function, defstruct defines a ``positional'' constructor function, taking arguments whose meaning is determined by the argument's position and possibly by keywords. Arglist is used to describe what the arguments to the constructor will be. In the simplest case something like (:constructor make-foo (a b c)) defines make-foo to be a three-argument constructor function whose arguments are used to initialize the slots named a, b, and c.

                Because a constructor of this type operates ``By Order of Arguments,'' it is sometimes known as a ``boa constructor.''

                For information on how the arglist for a ``boa constructor'' is processed, see Section 3.4.6 (Boa Lambda Lists).

                It is permissible to use the :constructor option more than once, so that you can define several different constructor functions, each taking different parameters.

                defstruct creates the default-named keyword constructor function only if no explicit :constructor options are specified, or if the :constructor option is specified without a name argument.

                (:constructor nil) is meaningful only when there are no other :constructor options specified. It prevents defstruct from generating any constructors at all.

                Otherwise, defstruct creates a constructor function corresponding to each supplied :constructor option. It is permissible to specify multiple keyword constructor functions as well as multiple ``boa constructors''.

            :copier

                This option takes one argument, a symbol, which specifies the name of the copier function. If the argument is not provided or if the option itself is not provided, the name of the copier is produced by concatenating the string "COPY-" and the name of the structure, interning the name in whatever package is current at the time defstruct is expanded. If the argument is provided and is nil, no copier function is defined.

                The automatically defined copier function is a function of one argument, which must be of the structure type being defined. The copier function creates a fresh structure that has the same type as its argument, and that has the same component values as the original structure; that is, the component values are not copied recursively. If the defstruct :type option was not used, the following equivalence applies:

                (copier-name x) = (copy-structure (the structure-name x))

            :include

                This option is used for building a new structure definition as an extension of another structure definition. For example:

                (defstruct person name age sex)

                To make a new structure to represent an astronaut that has the attributes of name, age, and sex, and functions that operate on person structures, astronaut is defined with :include as follows:

                (defstruct (astronaut (:include person)
                                      (:conc-name astro-))
                    helmet-size
                    (favorite-beverage 'tang))

                :include causes the structure being defined to have the same slots as the included structure. This is done in such a way that the reader functions for the included structure also work on the structure being defined. In this example, an astronaut therefore has five slots: the three defined in person and the two defined in astronaut itself. The reader functions defined by the person structure can be applied to instances of the astronaut structure, and they work correctly. Moreover, astronaut has its own reader functions for components defined by the person structure. The following examples illustrate the use of astronaut structures:

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

                The difference between the reader functions person-name and astro-name is that person-name can be correctly applied to any person, including an astronaut, while astro-name can be correctly applied only to an astronaut. An implementation might check for incorrect use of reader functions.

                At most one :include can be supplied in a single defstruct. The argument to :include is required and must be the name of some previously defined structure. If the structure being defined has no :type option, then the included structure must also have had no :type option supplied for it. If the structure being defined has a :type option, then the included structure must have been declared with a :type option specifying the same representation type.

                If no :type option is involved, then the structure name of the including structure definition becomes the name of a data type, and therefore a valid type specifier recognizable by typep; it becomes a subtype of the included structure. In the above example, astronaut is a subtype of person; hence

                (typep (make-astronaut) 'person) =>  true

                indicating that all operations on persons also work on astronauts.

                The structure using :include can specify default values or slot-options for the included slots different from those the included structure specifies, by giving the :include option as:

                (:include included-structure-name slot-description*)

                Each slot-description must have a slot-name that is the same as that of some slot in the included structure. If a slot-description has no slot-initform, then in the new structure the slot has no initial value. Otherwise its initial value form is replaced by the slot-initform in the slot-description. A normally writable slot can be made read-only. If a slot is read-only in the included structure, then it must also be so in the including structure. If a type is supplied for a slot, it must be a subtype of the type specified in the included structure.

                For example, if the default age for an astronaut is 45, then

                (defstruct (astronaut (:include person (age 45)))
                    helmet-size
                    (favorite-beverage 'tang))

                If :include is used with the :type option, then the effect is first to skip over as many representation elements as needed to represent the included structure, then to skip over any additional elements supplied by the :initial-offset option, and then to begin allocation of elements from that point. For example:

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

                The first two nil elements stem from the :initial-offset of 2 in the definition of binop. The next four elements contain the structure name and three slots for binop. The next three nil elements stem from the :initial-offset of 3 in the definition of annotated-binop. The last three list elements contain the additional slots for an annotated-binop.

            :initial-offset

                :initial-offset instructs defstruct to skip over a certain number of slots before it starts allocating the slots described in the body. This option's argument is the number of slots defstruct should skip. :initial-offset can be used only if :type is also supplied.

                :initial-offset allows slots to be allocated beginning at a representational element other than the first. For example, the form

                (defstruct (binop (:type list) (:initial-offset 2))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                would result in the following behavior for make-binop:

                (make-binop :operator '+ :operand-1 'x :operand-2 5)
                =>  (NIL NIL + X 5)
                (make-binop :operand-2 4 :operator '*)
                =>  (NIL NIL * NIL 4)

                The selector functions binop-operator, binop-operand-1, and binop-operand-2 would be essentially equivalent to third, fourth, and fifth, respectively. Similarly, the form

                (defstruct (binop (:type list) :named (:initial-offset 2))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                would result in the following behavior for make-binop:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (NIL NIL BINOP + X 5)
                (make-binop :operand-2 4 :operator '*) =>  (NIL NIL BINOP * NIL 4)

                The first two nil elements stem from the :initial-offset of 2 in the definition of binop. The next four elements contain the structure name and three slots for binop.

            :named

                :named specifies that the structure is named. If no :type is supplied, then the structure is always named.

                For example:

                (defstruct (binop (:type list))
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                This defines a constructor function make-binop and three selector functions, namely binop-operator, binop-operand-1, and binop-operand-2. (It does not, however, define a predicate binop-p, for reasons explained below.)

                The effect of make-binop is simply to construct a list of length three:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (+ X 5)  
                (make-binop :operand-2 4 :operator '*) =>  (* NIL 4)

                It is just like the function list except that it takes keyword arguments and performs slot defaulting appropriate to the binop conceptual data type. Similarly, the selector functions binop-operator, binop-operand-1, and binop-operand-2 are essentially equivalent to car, cadr, and caddr, respectively. They might not be completely equivalent because, for example, an implementation would be justified in adding error-checking code to ensure that the argument to each selector function is a length-3 list.

                binop is a conceptual data type in that it is not made a part of the Common Lisp type system. typep does not recognize binop as a type specifier, and type-of returns list when given a binop structure. There is no way to distinguish a data structure constructed by make-binop from any other list that happens to have the correct structure.

                There is not any way to recover the structure name binop from a structure created by make-binop. This can only be done if the structure is named. A named structure has the property that, given an instance of the structure, the structure name (that names the type) can be reliably recovered. For structures defined with no :type option, the structure name actually becomes part of the Common Lisp data-type system. type-of, when applied to such a structure, returns the structure name as the type of the object; typep recognizes the structure name as a valid type specifier.

                For structures defined with a :type option, type-of returns a type specifier such as list or (vector t), depending on the type supplied to the :type option. The structure name does not become a valid type specifier. However, if the :named option is also supplied, then the first component of the structure (as created by a defstruct constructor function) always contains the structure name. This allows the structure name to be recovered from an instance of the structure and allows a reasonable predicate for the conceptual type to be defined: the automatically defined name-p predicate for the structure operates by first checking that its argument is of the proper type (list, (vector t), or whatever) and then checking whether the first component contains the appropriate type name.

                Consider the binop example shown above, modified only to include the :named option:

                (defstruct (binop (:type list) :named)
                  (operator '? :type symbol)
                  operand-1
                  operand-2) =>  BINOP

                As before, this defines a constructor function make-binop and three selector functions binop-operator, binop-operand-1, and binop-operand-2. It also defines a predicate binop-p. The effect of make-binop is now to construct a list of length four:

                (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (BINOP + X 5)
                (make-binop :operand-2 4 :operator '*) =>  (BINOP * NIL 4)

                The structure has the same layout as before except that the structure name binop is included as the first list element. The selector functions binop-operator, binop-operand-1, and binop-operand-2 are essentially equivalent to cadr, caddr, and cadddr, respectively. The predicate binop-p is more or less equivalent to this definition:

                (defun binop-p (x)
                  (and (consp x) (eq (car x) 'binop))) =>  BINOP-P

                The name binop is still not a valid type specifier recognizable to typep, but at least there is a way of distinguishing binop structures from other similarly defined structures.

            :predicate

                This option takes one argument, which specifies the name of the type predicate. If the argument is not supplied or if the option itself is not supplied, the name of the predicate is made by concatenating the name of the structure to the string "-P", interning the name in whatever package is current at the time defstruct is expanded. If the argument is provided and is nil, no predicate is defined. A predicate can be defined only if the structure is named; if :type is supplied and :named is not supplied, then :predicate must either be unsupplied or have the value nil.

            :print-function, :print-object

                The :print-function and :print-object options specify that a print-object method for structures of type structure-name should be generated. These options are not synonyms, but do perform a similar service; the choice of which option (:print-function or :print-object) is used affects how the function named printer-name is called. Only one of these options may be used, and these options may be used only if :type is not supplied.

                If the :print-function option is used, then when a structure of type structure-name is to be printed, the designated printer function is called on three arguments:

                    the structure to be printed (a generalized instance of structure-name).

                    a stream to print to.

                    an integer indicating the current depth. The magnitude of this integer may vary between implementations; however, it can reliably be compared against *print-level* to determine whether depth abbreviation is appropriate.

                Specifying (:print-function printer-name) is approximately equivalent to specifying:

                (defmethod print-object ((object structure-name) stream)
                  (funcall (function printer-name) object stream <<current-print-depth>>))

                where the <<current-print-depth>> represents the printer's belief of how deep it is currently printing. It is implementation-dependent whether <<current-print-depth>> is always 0 and *print-level*, if non-nil, is re-bound to successively smaller values as printing descends recursively, or whether current-print-depth varies in value as printing descends recursively and *print-level* remains constant during the same traversal.

                If the :print-object option is used, then when a structure of type structure-name is to be printed, the designated printer function is called on two arguments:

                    the structure to be printed.

                    the stream to print to.

                Specifying (:print-object printer-name) is equivalent to specifying:

                (defmethod print-object ((object structure-name) stream)
                  (funcall (function printer-name) object stream))

                If no :type option is supplied, and if either a :print-function or a :print-object option is supplied, and if no printer-name is supplied, then a print-object method specialized for structure-name is generated that calls a function that implements the default printing behavior for structures using #S notation; see Section 22.1.3.12 (Printing Structures).

                If neither a :print-function nor a :print-object option is supplied, then defstruct does not generate a print-object method specialized for structure-name and some default behavior is inherited either from a structure named in an :include option or from the default behavior for printing structures; see the function print-object and Section 22.1.3.12 (Printing Structures).

                When *print-circle* is true, a user-defined print function can print objects to the supplied stream using write, prin1, princ, or format and expect circularities to be detected and printed using the #n# syntax. This applies to methods on print-object in addition to :print-function options. If a user-defined print function prints to a stream other than the one that was supplied, then circularity detection starts over for that stream. See the variable *print-circle*.

            :type

                :type explicitly specifies the representation to be used for the structure. Its argument must be one of these types:

                vector

                    This produces the same result as specifying (vector t). The structure is represented as a general vector, storing components as vector elements. The first component is vector element 1 if the structure is :named, and element 0 otherwise.

                (vector element-type)

                    The structure is represented as a (possibly specialized) vector, storing components as vector elements. Every component must be of a type that can be stored in a vector of the type specified. The first component is vector element 1 if the structure is :named, and element 0 otherwise. The structure can be :named only if the type symbol is a subtype of the supplied element-type.

                list

                    The structure is represented as a list. The first component is the cadr if the structure is :named, and the car if it is not :named.

                Specifying this option has the effect of forcing a specific representation and of forcing the components to be stored in the order specified in defstruct in corresponding successive elements of the specified representation. It also prevents the structure name from becoming a valid type specifier recognizable by typep.

                For example:

                (defstruct (quux (:type list) :named) x y)

                should make a constructor that builds a list exactly like the one that list produces, with quux as its car.

                If this type is defined:

                (deftype quux () '(satisfies quux-p))

                then this form

                (typep (make-quux) 'quux)

                should return precisely what this one does

                (typep (list 'quux nil nil) 'quux)

                If :type is not supplied, the structure is represented as an object of type structure-object.

                defstruct without a :type option defines a class with the structure name as its name. The metaclass of structure instances is structure-class.

        重定义一个 defstruct 结构体的后果是未定义的.

        在没有提供 defstruct 选项的情况下, 以下函数会被自动定义用来操作这个新的结构体的实例:

            Predicate

                一个名为 structure-name-p 的断言被定义用来测试这个结构体类型中的从属关系. 如果一个对象 object 是这个类型的, 那么断言 (structure-name-p object) 就是 true; 否则它就是 false. typep 也可以和这个新类型的名字一起使用来检测一个对象是否未这个类型. 这样一个函数调用的表达式形式为 (typep object 'structure-name).

            Component reader functions

                读取器函数(reader function)被定义用来读取这个结构体的组件. 对于每个槽的名字, 这里有一个对应的名为 structure-name-slot-name 的读取器函数. 这个函数读取那个槽的内容. 每个读取器函数接收一个参数, 它是这个结构体类型的一个实例. setf 可以和这些读取器函数中的任何一个一起使用来修改槽的内容.

            Constructor function

                一个名为 make-structure-name 的构造器函数(constructor function)被定义. 这个函数创建并返回这个结构体类型的新的实例.

            Copier function

                一个名为 copy-structure-name 的拷贝器函数(copier function)被定义. 这个拷贝器函数接收一个这个结构体类型的对象并创建一个相同类型的新的对象, 它是第一个对象的拷贝. 拷贝器函数使用和原始相同的组件条目来创建一个新的结构体. 两个结构体实例的对应组件是 eql 的.

        如果一个 defstruct 表达式形式作为顶层表达式形式出现, 编译器必须使这个结构体类型名字在后续的声明中(比如 deftype)可以被识别为一个有效类型名字并且使得结构体槽的 reader 函数被 setf 识别. 另外, 编译器必须保存足够关于结构体类型的信息以便进一步的结构体定义可以在同一个文件的后续的 deftype 中使用 :include 来引用这个结构体类型的名字. defstruct 产生的函数不会定义在编译时环境, 虽然编译器可能保存足够的关于这个函数的信息来把后续的调用编码为 inline 的. 这个 #S 读取器宏在编译时可能或可能不会识别这个新定义的结构体类型.

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

        这个声明每一个 ship 是一个带有五个已命名组件的对象. The evaluation of this form does the following:

            它定义 ship-x-position 为一个函数, 接受一个参数, 一个 ship, 返回这个 ship 的 x-position; ship-y-position 和其他组件都给定了类似的函数定义. 这些函数被称为访问函数, 由于它们被用来访问这个结构体的元素.

            ship 成为一个类型的名字, ship 的实例就是这个类型的元素. ship 变得可应用于 typep, 比如; 如果 x 是一个 ship 那么 (typep x 'ship) 是 true, 如果 x 是除了 ship 以外的任何对象那么就是 false.

            一个名为 ship-p 的单参数函数被定义; 这是一个断言, 如果它的参数是一个 ship 那么就是 true 否则就是 false.

            一个名为 make-ship 函数会被定义, 当被调用时, 创建一个带有五个组件的数据结构, 和访问函数一起使用是合适的. 因此执行

            (setq ship2 (make-ship))

            设置 ship2 为一个新创建的 ship 对象. 可以通过在一个对 make-ship 的调用中已下面这种方式使用关键字参数来提供任何想要的组件的初始值:

            (setq ship2 (make-ship :mass *default-ship-mass*
                                    :x-position 0
                                    :y-position 0))

            这个构造一个新的 ship 并且初始化它的组件中的三个. 这个函数也被称作 "构造函数(constructor function)" 因为它构造一个新的结构体.

            一个名为 copy-ship 的单参数函数被定义, 当给定一个 ship 对象, 就创建一个新的 ship 对象, 这个对象是给定的那个的一个复制. 这个函数被称作 "拷贝函数(copier function)".

        setf 可以被用于修改一个 ship 的组件:

        (setf (ship-x-position ship2) 100)

        这个修改 ship2 的 x-position 为 100. 这个可以工作是因为 defstruct 表现得就好像它为每个访问函数生成了一个合适的 defsetf.

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

        如果任何两个槽的名字(不管是直接出现或是通过 :include 选项继承)在 string= 下是相等的, defstruct 应该发出一个 program-error 类型的错误.

        如果这个 included-structure-name 没有命名一个结构体类型, 那么结果是未定义的.

* 也见(See Also):

        documentation, print-object, setf, subtypep, type-of, typep, Section 3.2 (Compilation)

* 注意(Notes):

        这个 printer-name 应该观察像 *print-escape* 这样的打印控制变量的值.

        对由于一个 slot-initform 和对应槽的 :type 选项的类型不匹配引起的一个警告的约束是有必要的, 因为一个 slot-initform 必须被指定用来指定槽选项; 在某些情况下, 可能不存在合适的默认项.<!--TODO 待校验-->

        defstruct 安排槽访问器和 setf 是可用的原理是依赖于具体实现的; 比如, 它可能使用 setf 函数, setf 展开式, 或者某个其他依赖于具体实现的对具体实现的 setf 代码是已知的机制. 


### <span id="FunctionCOPYSTRUCTURE">函数 COPY-STRUCTURE</span>

* 语法(Syntax):

        copy-structure structure => copy

* 参数和值(Arguments and Values):

        structure---一个结构体.
        copy---一个结构体的拷贝.

* 描述(Description):

        返回这个结构体的一个拷贝.

        只有这个结构体本身被拷贝; 这些槽的值不会.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defstruct 的这个 :copier 选项

* 注意(Notes):

        这个拷贝和给定的结构体在 equalp 下是相同的, 但是在 equal 下是不同的. 

