# 7. Objects

> * 7.1 [对象创建和初始化](#ObjectCreationInit)
> * 7.2 [修改一个实例的类](#ChangeClassInstance)
> * 7.3 [重新初始化一个实例](#ReinitInstance)
> * 7.4 [元对象](#MetaObjects)
> * 7.5 [槽](#Slots)
> * 7.6 [广义函数和方法](#GenericFunctionsMethods)
> * 7.7 [The Objects Dictionary](#TheObjectsDictionary)

## 7.1 <span id="ObjectCreationInit">对象创建和初始化</span>

广义函数 make-instance 创建并且返回一个类的实例. 第一个参数是一个类或者一个类的实例, 而剩余参数组成初始化参数列表.

一个新的实例的初始化由多个独立不同的步骤组成, 包括以下这些: 将显式提供的初始化参数与未提供的初始化参数的缺省值相结合, 检查初始化参数的有效性, 为实例分配存储, 用值来填充槽(slot), 并且执行用户提供的方法来执行额外的初始化. make-instance 的每个步骤都是通过一个广义函数实现的, 以提供一种定制该步骤的机制. 另外, make-instance 自身也是一个广义函数并且因此也可以被定制.

这个对象系统为每个步骤指定了系统提供的主要方法并且因此为整个初始化过程指定了一个定义明确的标准行为. 这个标准行为提供了四个简单的机制用于控制初始化:

* 声明一个符号作为一个槽的初始化参数. 一个初始化参数是通过给 defclass 使用 :initarg 槽选项来声明的. 这个为在一个 make-instance 调用中给一个槽提供一个值提供了一个机制This provides a mechanism for supplying a value for a slot in a call to make-instance.

* 为一个初始化参数提供一个默认值表达式形式. 初始化参数的默认值表达式形式通过给 defclass 使用 :default-initargs 类选项来定义的. 如果没有给 make-instance 明确提供一个初始化参数作为参数, 那么默认值表达式形式就会在定义它的 defclass 表达式形式的词法环境中被求值, 并且结果值被用作这个初始化参数的值.

* 为一个槽提供一个默认的初始值表达式形式. 一个槽的默认初始值表达式形式通过给 defclass 提供 :initform 槽选项来定义的. 如果没有给 make-instance 提供和那个槽关联的初始化参数或者通过 :default-initargs<!-- TODO 待校验 -->, 那么这个默认初始值表达式形式就会在定义它的 defclass 表达式形式的词法环境中被求值, 并且这个结果被存储到这个槽中. 当创建一个实例时, 更新一个实例来符合重定义的类时, 或者更新一个实例来符合不同类的定义时, 一个局部槽的这个 :initform 表达式形式可能被使用. 当定义或者重定义这个类时, 一个共享槽的这个 :initform 表达式形式会被使用.

* 为 initialize-instance 和 shared-initialize 定义方法. 上面描述的槽填充行为是由一个系统提供的 initialize-instance 主方法来实现的, 它调用 shared-initialize. 广义函数 shared-initialize 分这四种情况实现了初始化部分<!-- TODO 待校验 -->: 创建一个实例的时候, 重新初始化一个实例的时候, 更新一个实例来符合重定义的类时, 还有更新一个实例来符合一个不同的类的定义时. shared-initialize 的系统提供的主方法直接实现上述槽填充行为, 而 initialize-instance 简单地调用 shared-initialize.

> * 7.1.1 [初始化参数](#InitArguments)
> * 7.1.2 [声明初始化参数的有效性](#DeclaringValidityInitArg)
> * 7.1.3 [初始化参数的默认值](#DefaultInitArg)
> * 7.1.4 [初始化参数的规则](#RulesInitArg)
> * 7.1.5 [Shared-Initialize](#SharedInitialize)
> * 7.1.6 [Initialize-Instance](#InitializeInstance)
> * 7.1.7 [Make-Instance 和 Initialize-Instance 的定义](#DefMIII)

### 7.1.1 <span id="InitArguments">初始化参数</span>

一个初始化参数控制对象的创建和初始化. 使用关键字符号来命名初始化参数往往是很方便的, 但是初始化参数的名字可以是任何符号, 包括 nil. 一个初始化参数可以以两种方式被使用: 用一个值去填充一个槽或者为一个初始化方法提供一个参数. 一个单个的初始化参数可以被同时用作这两个目的.

一个初始化参数列表是一个初始化参数名字和值的属性列表. 它的结构与属性列表相同, 也与处理过的 &key 参数的参数列表部分相同. 在这些列表中, 如果一个初始化参数名称在初始化参数列表中出现不止一次, 那么最左边出现的就会提供值, 而其余的会被忽略. 给 make-instance 的参数 (在第一个参数后面的) 组成初始化参数列表.

一个初始化参数可以和一个槽关联. 如果这个初始化参数在初始化列表中有一个值, 这个值会被存储到新创建对象的那个槽中, 覆盖任何和这个槽关联的 :initform 表达式形式. 一个单个的初始化参数可以初始化不止一个槽. 初始化一个共享槽的初始化参数存储它的值到那个共享槽中, 替换掉任何先前的值.

一个初始化参数可以和一个方法关联. 当一个对象被创建并且提供一个特定的初始化参数时, 广义函数 initialize-instance, shared-initialize, 和 allocate-instance 会被调用, 这个初始化参数的名字和值作为一个关键字参数对. 如果在关键字参数列表中没有给这个关键字参数提供一个值, 这个方法的 lambda 列表会提供一个默认值.

初始化参数被用于四种情况: 创建一个实例时, 重新初始化一个实例时, 更新一个实例去符合一个重定义的类, 还有更新一个实例去符合一个不同的类的定义.

由于初始化参数被用于控制某个特定类的实例的创建和初始化, 我们就说一个初始化参数是那个类的初始化参数. 


### 7.1.2 <span id="DeclaringValidityInitArg">声明初始化参数的有效性</span>

在使用初始化参数的四种情况的任何一种时, 都会检测初始化参数的有效性. 一个初始化参数在一种情况是合法的但是在另一种缺是不合法的. 比如, 系统提供的针对类 standard-class 的 make-instance 的主方法检测它的初始化参数的合法性, 如果提供的一个初始化参数在那个情况下没有被声明为有效的, 那么就会发出一个错误.

关于声明初始化参数的有效性这里由两个意义.

* 填充槽的初始化参数通过给 defclass 的 :initarg 槽选项被声明为有效的. 这个 :initarg 槽选项从超类中继承下来. 因此一个类的填充槽的有效初始化参数是这个类和它的超类声明为有效的填充槽的初始化参数的并集. 填充槽的初始化参数在所有四种环境中都是有效的.

* 给方法提供参数的初始化参数通过定义这些方法被声明为有效的. 在这个方法的 lambda 列表中指定的每个关键字参数的关键字名字成为这个方法可应用的所有类的初始化参数. 一个可应用的方法的 lambda 列表中 &allow-other-keys 的出现会禁用初始化参数的有效性检测. 因此方法继承控制了提供参数给方法的有效初始化参数的集合. 用于声明初始化参数有效的方法定义的广义函数如下所示:

    -- 创建一个类的实例: allocate-instance, initialize-instance, 和 shared-initialize. 通过这些方法声明为有效的初始化参数在创建一个类的一个实例时是有效的.

    -- 重新初始化一个实例: reinitialize-instance 和 shared-initialize. 通过这些方法声明为有效的初始化参数在重新初始化一个实例时是有效的.

    -- 更新一个实例来符合重定义的类: update-instance-for-redefined-class 和 shared-initialize. 通过这些方法声明为有效的初始化参数在更新一个实例来符合重定义的类时是有效的.

    -- 更新一个实例来复合一个不同类的定义: update-instance-for-different-class 和 shared-initialize. 通过这些方法声明为有效的初始化参数在更新一个实例来复合一个不同类的定义时是有效的.

一个类的有效初始化参数集是填充槽和给方法提供参数的初始化参数以及预定义的初始化参数 :allow-other-keys 的集合. :allow-other-keys 的默认值是 nil. 如果初始化参数 :allow-other-keys 的值是 true 那么初始化参数的有效性检测就会被禁用. 

### 7.1.3 <span id="DefaultInitArg">初始化参数的默认值</span>

可以使用类选项 :default-initargs 来给一个初始化参数提供一个默认值表达式形式. 如果一个初始化参数被某个特定的类声明为有效的, 它的默认值表达式形式可能被一个不同的类指定. 在这个情况下 :default-initargs 被用于给一个继承的初始化参数提供一个默认值.

这个 :default-initargs 选项仅被用于给初始化参数提供默认值; 它不会声明一个符号作为有效初始化参数的名字. 此外, 这个 :default-initargs 选项仅在创建一个实例时被用于给初始化提供默认值.

给这个 :default-initargs 类选项的参数是一个交替的初始化参数名字和表达式形式的列表. 每个表达式形式是对应初始化参数的默认值表达式形式. 一个初始化参数的默认值表达式形式当且仅当这个初始化参数没有出现在 make-instance 的参数中并且不是由一个更具体的类缺省的时被使用和求值. <!-- TODO defaulted 缺省的 ??--> 默认值表达式形式在提供它的 defclass 表达式形式的词法环境中被求值; 结果值被用作这个初始化参数的值.

提供给 make-instance 的初始化参数和默认的初始化参数组合来产生一个默认的初始化参数列表. 默认的初始化参数列表是一个交替初始化参数名称和值的列表, 其中未提供的初始化参数是默认值, 其中显式提供的初始化参数出现在列表中默认的初始化参数的前面. 默认的初始化参数根据提供默认值的这些类的类优先级列表中的顺序来排序.

就槽的初始化而言, :default-initargs 和 :initform 的目的存在一个区别. 这个 :default-initargs 类选项在不知道这个初始化参数是初始化一个槽还是传递给一个方法的情况下为用户提供一个机制去给这个初始化参数提供一个默认值表达式形式. 如果那个初始化参数没有在一个 make-instance 的调用中显式提供, 就使用这个默认值表达式形式, 就像在这个调用中提供了一样. 与此相反, 这个 :initform 槽选项为用户提供一个机制去给一个槽提供一个默认初始化表达式形式. 一个 :initform 表达式形式当且仅当没有给 make-instance 传递一个和这个槽关联的初始化参数或者在 :default-initargs 没有提供默认值的时候被用于初始化一个槽.

初始化参数的默认值表达式形式的求值顺序和 :initform 表达式形式的求值顺序是没有定义的. 如果求值的顺序很重要, 应该使用 initialize-instance 或 shared-initialize 方法. 


### 7.1.4 <span id="RulesInitArg">初始化参数的规则<\span>

这个 :initarg 槽选项对于一个给定的槽可能被指定不止一次.

下面的规则指定了初始化参数的多次定义:

* 如果相同的初始化参数名出现在不止一个 :initarg 槽选项中时, 一个给定的初始化参数可以被用于初始化不止一个槽.

* 一个给定的初始化参数名可以出现在不止一个初始化方法的 lambda 列表中.

* 一个给定的初始化参数名可以同时出现在一个 :initarg 槽选项和一个初始化方法的 lambda 列表中.

如果在 make-instance 的参数中给定初始化同一个槽的两个或更多初始化参数, 在这个初始化参数列表中的这些初始化参数中的最左边的那个来提供值, 即便这些初始化参数有着不同的名字.

如果初始化同一个槽的两个或更多不同的初始化参数有默认值并且都没有提供给 make-instance 作为参数, 那么最具体的那些类的 :default-initargs 类选项来提供值. 如果一个单个的 :default-initargs 类选项指定了两个或更多初始化相同槽的初始化参数并且都没有显式出现在 make-instance 的参数中, 在 :default-initargs 类选项中最左边那个来提供值, 并且忽略剩余默认值表达式形式的值.

在 make-instance 的参数中给定的初始化参数出现在默认初始化参数的左边. 假设类 C1 和 C2 为不同的槽提供默认初始化参数值, 并且 C1 比 C2 更具体; 那么在默认初始化参数列表中 C1 提供的默认初始化参数值在 C2 提供的值的左边. 如果一个单个的 :default-initargs 类选项为两个不同的槽提供了初始化参数的值, the initialization argument whose value is specified farther to the left in the :default-initargs class option appears farther to the left in the defaulted initialization argument list.<!--TODO 待翻译-->

如果一个槽同时有一个 :initform 表达式形式和一个 :initarg 槽选项, 并且这个初始化参数使用 :default-initargs 提供默认值或提供给 make-instance, 那么这个 :initform 表达式形式不会被使用也不会被求值.

下面都是上述规则的一个示例:

```LISP
(defclass q () ((x :initarg a)))
(defclass r (q) ((x :initarg b))
  (:default-initargs a 1 b 2))
```

                                                                       
| 表达式形式                   |  默认初始化参数列表 | X 槽的内容 |
|-                           |-                 |-          |         
|(make-instance 'r)          |  (a 1 b 2)       |  1        |     
|(make-instance 'r 'a 3)     |  (a 3 b 2)       |  3        |             
|(make-instance 'r 'b 4)     |  (b 4 a 1)       |  4        |         
|(make-instance 'r 'a 1 'a 2)|  (a 1 a 2 b 2)   |  1        |                 


### 7.1.5 <span id="SharedInitialize">Shared-Initialize</span>

在一个实例被创建时, 一个实例被重新初始化时, 一个实例被更新去符合一个重定义的类时, 还有一个实例被更新去符合一个不同的类时, 广义函数 shared-initialize 被用于使用初始化参数和 :initform 表达式形式来填充一个实例的槽. 它使用标准方法组合. 它接受以下参数: 要被初始化的实例, 这个实例中可以访问的槽的名字集合的一份详述, 还有任意数量的初始化参数. 在前两个后面的参数一定组成一个初始化参数列表.

给 shared-initialize 的第二个参数可能是以下其中之一:

* 它可以是指定那些槽名字的集合的槽名字的列表 (可能是空的).

* 它可以是符号 t, 它指定了所有槽的集合.

这里有个系统提供的 shared-initialize 的主方法, 其中第一个参数特化是类 standard-object. 这个方法在每个槽上表现如下, 不管是共享的或是局部的:

* 如果这个初始化参数列表中的一个初始化参数为那个槽指定了一个值, 那个值就会被存储到那个槽中, 即便在这个方法执行前一个值已经被存储到那个槽里. 受影响的槽独立于 shared-initialize 第二个参数表示的槽.

* 任何在这个点是未绑定的第二个参数表示的槽都会根据它们的 :initform 表达式形式来初始化. 对于任何有着一个 :initform 表达式形式的槽, 那个表达式形式会在它的 defclass 定义的词法环境中被求值, 并且结果被存储到那个槽中. 比如, 如果一个 before 方法存储一个值到槽中, 这个 :initform 表达式形式不会被用来给这个槽提供一个值. 如果第二个参数指定了一个不对应这个实例中任何可访问的槽的名字, 结果是未定义的.

* 在章节 7.1.4 (Rules for Initialization Arguments) 中提及的规则也是遵守的.

广义函数 shared-initialize 会被系统提供的 reinitialize-instance, update-instance-for-different-class, update-instance-for-redefined-class, 和 initialize-instance 的主方法调用. 因此, 可以为 shared-initialize 写一个方法来指定发生在所有这些上下问中的动作. 


### 7.1.6 <span id="InitializeInstance">Initialize-Instance</span>

广义函数 initialize-instance 被 make-instance 调用来初始化一个新创建的实例. 它使用标准方法组合. 可以定义 initialize-instance 的方法, 以便执行任何无法通过为槽提供初始值来实现的初始化.

在初始化期间, initialize-instance 在以下动作执行后被调用:

* 默认初始化参数列表已经通过结合提供的初始化参数列表和这个类的默认初始化参数被计算.

* 默认初始化参数列表的有效性已经被检测. 如果初始化参数中的任何一个没有被有效声明, 就会发出一个错误.

* 一个槽还没有被绑定的实例被创建出来.

广义函数 initialize-instance 被调用并带有一个新的实例和默认初始化参数. 这里有一个系统提供的 initialize-instance 的主方法, 其中参数特化是类 standard-object. 这个方法调用广义函数 shared-initialize 根据初始化参数和槽的 :initform 表达式形式来填充槽; 广义函数 shared-initialize 被调用时带有以下参数: 这个实例, t, 还有默认初始化参数.

注意, initialize-instance 在它的 shared-initialize 调用中提供默认初始化参数列表, 因此，由系统提供的 shared-initialize 主要方法执行的第一步考虑了在调用 make-instance 和默认初始化参数列表中提供的初始化参数.

initialize-instance 方法可以被定义用来指定一个实例被初始化时采取的动作. 只有在 initialize-instance 的方法被定义之后, 它们会在系统提供的用于初始化的主方法之后被运行, 并且因此不会和 initialize-instance 的默认行为冲突.

对象系统提供了两个在 initialize-instance 方法的主体中有用的函数. 函数 slot-boundp 返回一个表示一个指定的槽是否有一个值的广义 boolean 值; 这提供了一种机制, 用于在 initialize-instance 的方法之后编写初始化槽的方法, 只有在尚未初始化的情况下才会初始化槽. 函数 slot-makunbound 使这个槽没有值. 


### 7.1.7 <span id="DefMIII">Make-Instance 和 Initialize-Instance 的定义</span>

广义函数 make-instance 的行为表现就像它是如下定义的那样, 除了某些优化是允许的:

```LISP
(defmethod make-instance ((class standard-class) &rest initargs)
  ...
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))
```

在 make-instance 的定义中, 省略的代码增加了 initargs 的默认初始化参数, 并检查产生的初始化参数，以确定是否提供了一个初始化参数, 既不填充槽, 也不向可应用的方法提供参数.

广义函数 initialize-instance 的行为表现就像它是如下定义的那样, 除了某些优化是允许的:

```LISP
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs)))
```

这些程序可以被定制.

程序员接口级别的定制包括使用给 defclass 的 :initform, :initarg, 和 :default-initargs 选项, 还有为 make-instance, allocate-instance, 和 initialize-instance 定义方法. 也可以为 shared-initialize 定义方法, 它会被广义函数 reinitialize-instance, update-instance-for-redefined-class, update-instance-for-different-class, 和 initialize-instance 调用. 元对象级别支持额外的定制.

具体实现允许去对 initialize-instance 和 shared-initialize 做某些优化. 在章节 7 中 shared-initialize 的描述提及了可能的定制. 

## 7.2 <span id="ChangeClassInstance">修改一个实例的类</span>

函数 change-class 可以被用来修改一个类的实例从它的当前类, Cfrom, 到一个不同的类, Cto; 它修改这个实例的结构来符合这个类 Cto 的定义.

注意, 修改一个实例的类可能导致添加或删除槽. 修改一个实例的类不会修改由 eq 函数定义的它的同一性.

当 change-class 在一个实例上被调用, 会发生一个两步的更新过程. 第一步通过添加新的局部槽还有废弃这个新版本的实例中没有指定的局部槽来修改这个实例的结构. 第二部初始化新添加的局部槽并执行其他任何用户定义的动作. 这两步在以下两个章节中有进一步的描述.

### 7.2.1 修改实例的结构

为了使这个实例去符合类 Cto, 类 Cto 指定而类 Cfrom 没有指定的局部槽会被添加, 并且类 Cfrom 指定而类 Cto 没有指定的局部槽会被丢弃.

类 Cto 和类 Cfrom 都指定的局部槽会被保留. 如果这样一个局部槽没有被绑定, 它就保持未绑定状态.

在类 Cfrom 中指定为共享的但是在类 Cto 中指定为局部的槽的值会被保留.

更新中的第一步不会影响任何共享槽的值. 

### 7.2.2 初始化新添加的局部槽

更新操作的第二步初始化新添加的槽并且执行任何其他用户定义的动作. 这个步骤由广义函数 update-instance-for-different-class 来实现. 广义函数 update-instance-for-different-class 在更新的第一步完成后被 change-class 调用.

广义函数 update-instance-for-different-class 在 change-class 计算的参数上被调用. 传递的第一个参数是这个要被更新的实例的一个拷贝并且是类 Cfrom 的一个实例; 这个拷贝在广义函数 change-class 中有着动态范围. 第二个参数是 change-class 到目前为止更新的实例并且那是类 Cto 的一个实例. 剩余的参数是一个初始化参数列表.

这里有一个系统提供的 update-instance-for-different-class 的主方法, 它有两个特化参数, 其中的每一个都是类 standard-object. 首先这个方法检测初始化参数的有效性, 如果提供的一个初始化参数没有被有效声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (Declaring the Validity of Initialization Arguments).) 然后它调用广义函数 shared-initialize 并传递以下参数: 这个新的实例, 新添加槽的名字的一个列表, 还有它收到的那些初始化参数. 

### 7.2.3 定制一个实例的类的改变

update-instance-for-different-class 方法可以被定义用来指定一个实例被更新时发生的动作. 只有在 update-instance-for-different-class 方法被定义之后, 它们会在系统提供的初始化主方法之后被运行并且不会干涉 update-instance-for-different-class 的默认行为.

shared-initialize 的方法可能被定义用来定制类的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 

## 7.3 <span id="ReinitInstance">重新初始化一个实例</span>

广义函数 reinitialize-instance 可以被用于根据初始化参数来修改槽的值.

重新初始化的过程修改一些槽的值并执行任何用户定义的动作. 它不会修改一个实例的结构来添加或删除槽, 并且它也不会使用任何 :initform 表达式形式来初始化槽.

广义函数 reinitialize-instance 可以被直接调用. 它接受一个必要参数, 这个实例. 它也接受任意数量的初始化参数来被 reinitialize-instance 或 shared-initialize 的方法使用. 在必要的实例后面的参数必须组成一个初始化参数列表.

这里有一个系统提供的 reinitialize-instance 主方法, 其中参数特化是类 standard-object. 首先这个方法检查初始化参数的有效性, 而过一个提供的参数没有被有效声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (Declaring the Validity of Initialization Arguments).) 然后它调用广义函数 shared-initialize 并传递如下参数: 这个实例, nil, 还有它收到的初始化参数.

### 7.3.1 定制重新初始化行为

reinitialize-instance 方法可以被定义, 用来指定一个实例被更新时采取的动作. 只有在 reinitialize-instance 方法被定义之后, 它们才会在系统提供的初始化主方法之后被运行并且因此不会影响 reinitialize-instance 的默认行为.

shared-initialize 方法可以被定义, 用来定制类的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 


## 7.4 <span id="MetaObjects">元对象</span>

对象系统的实现操纵类, 方法和广义函数. 对象系统包含了由类方法定义的广义函数的集合; 这些广义函数的行为定义了这个对象系统的行为. 这些方法被定义的对应的类的实例称之为元对象.

### 7.4.1 标准元对象

对象系统提供了一个元对象的集合, 称之为标准元对象. 这些包括类 standard-object 和类 standard-method, standard-generic-function, method-combination 的实例.

* 类 standard-method 是 defmethod 和 defgeneric 表达式形式定义出来的方法的默认类.

* 类 standard-generic-function 是 defmethod, defgeneric, 和 defclass 定义的广义函数的默认类.

* 名为 standard-object 的类是类 standard-class 的一个实例并且它是每个 standard-class 的一个实例类的一个超类, 除了它自身和 structure-class.

* 每个方法组合对象都是类 method-combination 的子类的一个实例. 


## 7.5 <span id="Slots">槽</span>

> * 7.5.1 [槽的介绍](#IntroductionSlots)
> * 7.5.2 [访问槽](#AccessingSlots)
> * 7.5.3 [槽和槽选项的继承](#InheritanceSlotsSlotOptions)


### 7.5.1 <span id="IntroductionSlots">槽的介绍</span>

一个元类 standard-class 的对象由 0 个或更多已命名的槽. 一个对象的槽由这个对象的类决定. 每个槽都可以持有一个值. 一个槽的名字是一个语法上可用作一个变量名的符号.

当一个槽没有一个值时, 这个槽就被称为未绑定的(unbound). 当一个未绑定的槽被读取时, 广义函数 slot-unbound 会被调用. 系统提供的类 t 上的 slot-unbound 主方法会发出一个错误. 如果 slot-unbound 返回了, 它的主要的值那时会被用作那个槽的值.

一个槽的默认初始值表达式形式被 :initform 槽选项所定义. 当这个 :initform 表达式形式被用于提供一个值的时候, 它会在求值 defclass 表达式形式的词法环境中被求值. 这个 :initform 求值 defclass 表达式形式的词法环境被称为一个被捕获的初始化表达式形式(captured initialization form). 关于更多详情, 见章节 7.1 (Object Creation and Initialization).

一个局部槽被定义为一个槽, 它仅对于一个实例是可访问的, 那个实例就是分配槽的那个. 一个共享槽被定义为一个槽, 它对于给定类和它的子类而言超过一个实例都是可见的.

当一个类的 defclass 表达式形式包含了一个给定名字的槽指定符, 就说这个类以这个名字定义一个槽. 定义一个局部槽不会马上创建一个槽; 它导致这个类的实例被创建时一个槽会被创建. 定义一个共享槽会马上创建一个槽.

给 defclass 的这个 :allocation 槽选项控制被定义的槽的类型. 如果这个 :allocation 槽选项的值是 :instance, 一个局部槽会被创建. 如果 :allocation 的值是 :class, 就会创建一个共享槽.

如果一个槽是由一个实例的类定义的, 或者是从该类的超类中继承的, 就说这个槽在这个类的实例中是可以访问的. 在一个实例中一个给定的名字的可访问的槽最多一个. 一个类定义的共享槽在这个类的所有实例中都是可以访问的. 关于槽的继承的详细解释在章节 7.5.3 (Inheritance of Slots and Slot Options) 中给出. 


### 7.5.2 <span id="AccessingSlots">访问槽</span>

槽可以通过两种方式被访问: 通过使用基本函数 slot-value 和通过使用 defclass 表达式形式产生的广义函数.

函数 slot-value 可以通过这个 defclass 表达式形式中指定的槽名字来使用, 用于访问给定类的一个实例中一个可访问的特定的槽.

宏 defclass 为读取或写入槽提供语法. 如果需要一个 reader 方法, 用于读取这个槽的值的一个方法会被自动生成, 但是用于存储一个值到这个槽的方法不会被生成. 如果需要一个 writer 方法, 用于存储一个值到这个槽的一个方法会被自动生成, 但是读取它的值的方法不会被生成. 如果需要一个 accessor 方法, 一个用于读取这个槽的值的方法和一个用于存储一个值到这个槽的方法会被自动生成. Reader 和 writer 方法通过使用 slot-value 来实现.

当一个槽指定了 reader 或者 writer 方法, 所生成方法所属的广义函数的名字是直接指定的. 如果为这个 writer 方法指定的名字是这个符号 name, 那么用于写入这个槽的广义函数的名字就是 name, 这个广义函数接受两个参数: 新的值和这个实例, 以这样的顺序. 如果为 accessor 方法指定的名字是符号 name, 用于读取这个槽的广义函数的名字就是符号 name, 而用于写入这个槽的广义函数的名字就是列表 (setf name).

通过提供 :reader, :writer, 或 :accessor 槽选项创建或修改的一个广义函数可以被当作一个普通的广义函数.

注意, slot-value 可以被用于读取或写入一个槽的值, 不管那个槽的 reader 或 writer 方法是否存在. 当使用 slot-value 时, 没有 reader 或 writer 方法会被调用.

宏 with-slots 可以被用于建立一个词法环境, 在这个环境中指定的槽就像它们是变量一样是词法上可用的. 宏 with-slots 调用函数 slot-value 来访问特定的槽.

宏 with-accessors 可以被用于建立一个词法环境, 在这个环境中指定的槽通过它们的访问器是词法上可用的, 就像它们是变量一样. 宏 with-accessors 调用合适的访问器来访问指定的槽. 


### 7.5.3 <span id="InheritanceSlotsSlotOptions">槽和槽选项的继承</span>

一个类 C 的一个实例中所有可访问槽的名字集合是 C 和它的所有超类定义的槽的名字的并集. 一个实例的结构是这个实例中局部槽的名字集合.

在最简单的情况下, 在C及其超类中只有一个类定义了一个带有给定槽名的槽. 如果 C 的一个超类定义了一个槽, 就说这个槽是继承的. 槽的特征是由定义类的槽指定符决定的. 细想一个槽 S 的定义类. 如果这个 :allocation 槽选项的值是 :instance, 那么 S 是一个局部槽并且 C 的每一个实例都有它自己的名为 S 的槽存储它自己的值. 如果这个 :allocation 槽选项的值是 :class, 那么 S 是一个共享槽, 定义 S 的类存储这个值, 并且所有 C 的实例可以访问那个单个的槽. 如果这个 :allocation 槽选项被省略, 就使用 :instance.

通常情况下, 在 C 及其超类中有多个类可以定义带有给定名称的槽. 这样的情况下, 在 C 的一个实例中给定名字的槽只有一个可以访问, 而这个槽的特性是这几个槽指定符的一个结合, 按如下计算:

* 对于一个给定的槽名字的所有槽指定符按照从最具体到最不具体的顺序排列, 根据 C 的类优先级列表中定义它们的类的顺序. 下面的所有关于槽指定符的特性的引用都是指这种排序.

* 一个槽的分配由最具体的槽指定符来控制. 如果最具体的槽指定符不包括一个 :allocation 槽选项, 就是用 :instance. 较不具体的槽指定符不会影响这个分配.

* 一个槽的默认初始值表达式形式是包含一个 :initform 槽选项的最具体的槽指定符中该选项的值. 如果没有包含 :initform 槽选项的槽指定符, 这个槽就没有默认的初始值表达式形式.

* 一个槽的内容总是为 (and T1 ... Tn) 类型, 其中 T1 ...Tn 是包含在所有槽指定符中 :type 槽选项的值. 如果没有包含 :type 槽选项的槽指定符, 这个槽的内容总是为类型 t. 尝试去存储一个不满足一个槽的类的值到这个槽中的结果是未定义的.

* 初始化一个给定槽的初始化参数的集合是声明在所有这些槽指定符的 :initarg 槽选项中的初始化参数的并集.

* 一个槽的文档字符串是包含 :documentation 槽选项的最具体的槽指定符的该选项的值. 如果没有包含一个 :documentation 槽选项的槽指定符, 那么该槽没有文档字符串.

这个分配规则的一个后果是一个共享槽可以被遮蔽. 比如, 如果一个类 C1 定义了一个名为 S 的槽, 其中 :allocation 槽选项的值是 :class, 这个槽在 C1 的实例中和所有它的子类中都是可以访问的. 然而, 如果 C2 是 C1 的一个子类并且也定义了名为 S 的槽, C1 的槽不会被 C2 的实例和子类所共享. 当一个类 C1 定义了一个共享槽时, 任何 C1 的子类 C2 会共享这单个的槽除非这个 C2 的 defclass 表达式形式指定了一个相同名字的槽, 或者这里有一个定义了相同名字的槽的 C2 的超类并且这个超类在 C2 的类优先级列表中先于 C1.

这个类型规则的一个后果是一个槽的值满足贡献给这个槽的所有槽指定符的类型约束. 由于尝试存储一个不满足这个槽的类型约束的值到该槽中的结果是未定义的, 一个槽中的值可能不满足它的类型约束.

这个 :reader, :writer, 和 :accessor 槽选项创建方法而不是定义一个槽的属性. 在章节 7.6.7 (Inheritance of Methods) 所描述的观念中 reader 和 writer 方法是继承的.

访问槽的方法只使用槽的名字和槽的值的类型. 假设一个超类提供了一个方法, 该方法希望访问给定名称的共享槽, 并且一个子类定义了一个相同名字的局部槽. 如果超类提供的这个方法在子类的实例上被调用, 这个方法会访问这个局部槽. 


## 7.6 <span id="GenericFunctionsMethods">广义函数和方法</span>

> * 7.6.1 [广义函数的介绍](#IntroductionGF)
> * 7.6.2 [方法的介绍](#IntroductionMethods)
> * 7.6.3 [关于参数指定符和限定符的一致性](#AgreeParamSpecQualifiers)
> * 7.6.4 [广义函数的所有方法的一致 Lambda-list](#LambdaMethodsGF)
> * 7.6.5 [广义函数和方法中的关键字参数](#KeywordArgGFAndMethods)
> * 7.6.6 [方法选择和组合](#MethodSelComb)
> * 7.6.7 [方法的继承](#InheritanceMethods)


### 7.6.1 <span id="IntroductionGF">广义函数的介绍</span>

一个广义函数是一个行为取决于提供给它的参数的类或同一性的函数. 一个广义函数对象和一个方法的集合, 一个 lambda 列表, 一个方法组合, 还有其他信息相关联.<!--TODO identity 同一性？身份?-->

像一个普通函数一样, 一个广义函数接受参数, 执行一系列动作, 并且可能返回有用的值. 一个普通函数由有一个在函数被调用时总是被执行的代码中的单个主体. 一个广义函数有着一个代码的主体的集合, 其中一个子集会被选择来执行. 选择的代码主体和它们的组合方式由给这个广义函数的一个或多个参数的类或同一性决定, 以及它们的方法组合.

普通函数和广义函数用相同的语法来调用.

广义函数是可以作为参数传递和用作给 funcall 和 apply 的第一个参数的真实函数.

函数名与广义函数的绑定可以通过以下几种方式建立. 它可以通过 ensure-generic-function, defmethod (隐式的, 应归于 ensure-generic-function) 或 defgeneric (也是隐式的, 应归于 ensure-generic-function) 在全局环境中建立. 没有为在词法环境中建立一个函数名与广义函数的绑定提供标准化机制.

当一个 defgeneric 表达式形式被求值, 采取这三个动作中的其中一个 (应归于 ensure-generic-function):

* 如果一个给定名字的广义函数已经存在, 这个存在的广义函数对象会被修改. 当前 defgeneric 表达式形式指定的方法会被添加, 并且这个通过前面的 defgeneric 表达式形式定义的已存在的广义函数中的方法会被移除. 通过当前 defgeneric 表达式形式添加的方法可能替换由 defmethod, defclass, define-condition, 或 defstruct 定义的方法. 广义函数中没有其他方法受到影响或替换.

* 如果这个给定的名字命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

* 否则就会使用这个 defgeneric 表达式形式中的方法定义指定的方法来创建一个广义函数.

某些操作符允许规范一个广义函数的选项, 就像它使用的方法组合的类型或它的参数优先级顺序. 这些操作符会会被称为 "指定广义函数选项的操作符". 这个类别中唯一的标准化操作符是 defgeneric.

某些操作符为一个广义函数定义方法. 这些操作符会被称作方法定义操作符; 它们关联的表达式形式被称作方法定义表达式形式. 标准化的方法定义操作符列在下面这段中.

    defgeneric        defmethod  defclass  
    define-condition  defstruct            

    Figure 7-1. 标准化的方法定义操作符, 注意这些方法定义操作符中只有 defgeneric 可以指定广义函数选项. defgeneric 还有任何具体实现定义的可以指定广义函数选项的操作符都被称为 "指定广义函数选项的操作符".


### 7.6.2 <span id="IntroductionMethods">方法的介绍</span>
<!--TODO parameter specializer 参数特化符 特化参数??-->
方法定义了特定类或者特定同一性的行为以及一个广义函数的操作.

一个方法对象和实现这个方法的代码相关联, 在这个给定方法可应用时指定的一个参数特化序列, 一个 lambda 列表, 还有一个被方法组合机制用来区分方法的限定符序列.

一个方法对象不是一个函数并且不能像函数一样被调用. 在这个对象系统中的各种机制接收一个方法对象并调用它的方法函数, 就像这个广义函数被调用时那样. 这个发生时就说这个方法被调用.

方法定义表达式形式包含了当广义函数的参数导致它定义的方法被调用时要运行的代码 A method-defining form contains the code that is to be run when the arguments to the generic function cause the method that it defines to be invoked.<!--TODO 待校验--> 当一个方法定义表达式形式被求值, 一个方法对象会被创建并且采取这四种动作中的一个:

* 如果一个给定名字的广义函数已经存在并且如果一个在特化参数和限定符上都符合新的那个的方法对象已经存在, 那个新的对象会替换旧的那个. 对于一个方法的定义, 在参数指定器和限定符上与另一个方法达成一致的, 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

* 如果一个给定名字的广义函数已经存在并且这里没有在特化参数和限定符上都符合新的那个的方法对象, 那个存在的广义函数对象会被修改来包含那个新的方法对象.

* 如果给定的名字命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

* 否则就会用那个方法定义表达式形式指定的方法创建一个广义函数.

如果一个新的方法的 lambda 列表和广义函数是不相等的, 就会发出一个错误. 如果一个不能指定广义函数选项的方法定义操作符创建了一个新的广义函数, 这个广义函数的 lambda 列表是来自于这个方法定义表达式中的方法的 lambda 列表, 在这种方式下它们是一致的. 关于一致性的讨论, 见章节 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

每个方法都有一个专门的 lambda 列表, 它决定了何时这个方法可以被应用. 一个专门的 lambda 列表就像一个普通 lambda 列表除了一个特化参数可以出现来代替一个必要参数. 一个特化参数是一个列表 (variable-name parameter-specializer-name), 其中 parameter-specializer-name 以下的一种:

一个符号

    表示一个以该符号命名的类的参数特化符.

一个类

    表示一个参数特化符就是类本身.

(eql form)

    表示一个参数特化符需满足类型指定 (eql object), 其中 object 是求值 form 的结果. 这个 form 表达式形式在方法定义表达式形式被求值的词法环境中被求值. 注意, 这个 form 只被求值一次, 在方法被定义的时候, 而不是每次广义函数被调用的时候.

参数特化符名字在用户级别接口的宏 (defmethod) 中使用, 而在函数接口中使用参数特化符.

只有必要参数可以被特化, 并且这里对于每一个必要参数都必须是参数特化符. 为了表达的简单性, 如果一个方法定义表达式形式的特化 lambda 列表中的某个必要参数仅仅是一个变量名, 它的参数特化符默认是类 t.

给定一个广义函数和一个参数集合, 一个可应用的方法是一个参数特化符被它们对应的参数所满足的广义函数的方法. 下面的定义指定了什么是可应用的方法, 以及满足参数特化符的参数的含义.

让 <A1, ..., An> 依次是给一个广义函数的必要参数. 让 <P1, ..., Pn> 依次是对应方法 M 的必要参数的参数特化符. 当每一个 Ai 都是类型指定符 Pi 指定的类型时, 方法 M 是可应用的. 由于每个有效参数特化符也是一个有效的类型指定符, 在方法选择去决定一个参数是否满足一个参数特化符时, 可以使用函数 typep.

一个所有参数特化符都是类 t 的方法被称为默认方法; 它总是可应用的但是可能被一个更具体的方法所遮蔽.

方法可以有限定符, 它给方法组合过程提供一种区分方法的方式. 一个带有一个或多个限定符的方法被称为限定方法(qualified method).<!--TODO qualified method ？？--> 一个没有限定符的方法被称为一个非限定方法. 一个限定符是任何非列表元素. 标准方法组合类型定义的限定符是符号.

在这个规范中, 术语 "主方法(primary method)" 和 "辅助方法(auxiliary method)" 在方法组合类型中根据它们的用途被用于区分方法. 在标准方法组合中, 主方法是非限定方法而辅助方法是有以下之一的单个限定符的方法: :around, :before, 或 :after. 有这些限定符的方法分别被称为 around 方法, before 方法, 还有 after 方法. 当使用 define-method-combination 简单的表达式形式定义一个方法组合类型时, 主要方法是用方法组合的类型命名的方法, 而辅助方法有着限定符 :around. 因此术语 "主方法(primary method)" 和 "辅助方法(auxiliary method)" 只有在给定方法组合类型中有相关定义. 

### 7.6.3 <span id="AgreeParamSpecQualifiers">关于参数特化符和限定符的一致性</span>

如果以下条件保持不变, 则两个方法在参数特化符和限定符之间达成一致:

1. 两个方法有相同数量的必要参数. 假设这两个方法的参数特化符为 P1,1...P1,n 和 P2,1...P2,n.

2. 对于每一个 1<=i<=n, P1,i 和 P2,i 一致. 如果 P1,i 和 P2,i 是相同的类或者如果 P1,i=(eql object1), P2,i=(eql object2), 并且 (eql object1 object2), 那么参数特化符 P1,i 和 P2,i 是一致的. 否则 P1,i 和 P2,i 是不一致的.

3. 两个限定符列表在 equal 下是相等的. 


### 7.6.4 <span id="LambdaMethodsGF">广义函数的所有方法的一致 Lambda-list</span>

这些规则定义了一个 lambda 列表的集合的一致性, 包括对于一个给定广义函数的每个方法的那个 lambda 列表还有这个广义函数自身指定的那个 lambda 列表, 如果存在的话.

1. 每个 lambda 列表必须有相同数量的必要参数.

2. 每个 lambda 列表必须有着相同数量的可选参数. 每个方法可以为可选参数提供它自己的默认值.

3. 如果任何 lambda 列表提及 &rest 或 &key, 那么每个 lambda 列表必须提及它们或它们的其中之一.

4. 如果广义函数的 lambda 列表提及 &key, 每个方法必须接受 &key 后面提及的所有关键字的名字, 不管是通过显式地指定它们, 还是通过指定 &allow-other-keys, 或者通过指定 &rest 而不是 &key. 每个方法可以接收它自己的额外的关键字参数. 关键字名字的有效性检测在广义函数中进行, 不是在每个方法中. 一个方法被调用就像提供了名字为 :allow-other-keys 而值为 true 的关键字参数对一样, 尽管没有这样的参数对会被传递.

5. &allow-other-keys 的使用在 lambda 列表中不需要是一致的. 如果 &allow-other-keys 在这个广义函数的任何一个可应用方法的 lambda 列表中被提及, 那么在对这个广义函数的调用中任何关键字参数都可能被提及.

6. &aux 的使用在方法中不需要是一致的.

    如果一个不能指定广义函数选项的方法定义操作符创建了一个广义函数, 并且这个方法的 lambda 列表提及关键字参数, 这个广义函数的 lambda 列表会提及 &key (但是没有关键字参数). 


### 7.6.5 <span id="KeywordArgGFAndMethods">广义函数和方法中的关键字参数</span>

当一个广义函数或者它的任何一个方法在一个 lambda 列表中提及 &key, 广义函数接受的特定关键字参数根据所应用的方法而变化. 对于一个特定的调用, 被这个广义函数所接收的关键字参数的集合是所有可应用方法的关键字参数的并集, 如果这个广义函数定义中的 &key 后提及关键字参数, 那么还包括这些关键字参数. 一个有着 &rest 没有 &key 的方法不会影响可接受参数的集合. 如果任何可应用的方法或广义函数定义的 lambda 列表中包含 &allow-other-keys, 所有关键字都可以被这个广义函数所接受.

这个 lambda 列表一致性规则要求每一个方法接受所有在广义函数的 &key 参数后提及的所有关键字参数, 通过显式地接收它们, 通过指定 &allow-other-keys, 通过指定 &rest 但不是 &key. 除了广义函数定义中提到的关键字参数之外, 每一个方法可以接受它自身的额外的关键字参数.

如果传递给一个广义函数一个没有可应用方法接收的关键字参数, 应该会发出一个错误; 见章节 3.5 (Error Checking in Function Calls).

 7.6.5.1 广义函数和方法的关键字参数示例

比如, 假设这里为 width 按照如下定义两个方法:

```LISP
(defmethod width ((c character-class) &key font) ...)

(defmethod width ((p picture-class) &key pixel-size) ...)
```

假设这里没有 width 的其他方法并且没有 width 广义函数. 以下表达式形式的求值应该会发出一个错误 由于这个关键字参数 :pixel-size 不能被这个可应用的方法所接受.

```LISP
(width (make-instance `character-class :char #\Q) 
      :font 'baskerville :pixel-size 10)
```

以下表达式形式的求值应该发出一个错误.

```LISP
 (width (make-instance `picture-class :glyph (glyph #\Q)) 
        :font 'baskerville :pixel-size 10)
```

如果名为 character-picture-class 的类同时是 picture-class 和 character-class 的子类, 那么以下表达式形式的求值不会发出一个错误.

```LISP
(width (make-instance `character-picture-class :char #\Q)
      :font 'baskerville :pixel-size 10)
```

### 7.6.6 <span id="MethodSelComb">方法选择和组合</span>

当用特定的参数调用一个广义函数时, 它必须决定要执行的代码. 这个代码被称为这些参数的有效方法. 有效的方法是一个广义函数中可应用方法的组合, 它调用这些方法的一部分或全部.

如果一个广义函数被调用而没有可应用的方法, 广义函数 no-applicable-method 会被调用, 这个调用的结果被用作对原始广义函数调用的结果. 调用 no-applicable-method 优先于检测可接受的关键字参数; 见章节 7.6.5 (Keyword Arguments in Generic Functions and Methods).

当这个有效方法已经被决定时, 会用传递给这个广义函数相同的参数去调用它. 不管它返回什么值都会作为这个广义函数的值被返回.

> * 7.6.6.1 [确定有效方法](#DetermEffectMethod)
> * 7.6.6.2 [标准方法组合](#StandMethodComb)
> * 7.6.6.3 [声明方法组合](#DeclaraMethodComb)
> * 7.6.6.4 [内建的方法组合类型](#BuiltInMethodCombTypes)


#### 7.6.6.1 <span id="DetermEffectMethod">确定有效方法</span>

有效方法通过以下三个步骤来决定:

1. 选择可应用的方法.

2. 通过优先级顺序对可应用方法排序, 把最具体的方法放在第一位.

3. 对排序后的可应用方法列表应用方法组合, 产生有效方法.

##### 7.6.6.1.1 选择可应用的方法

这个步骤在章节 7.6.2 (Introduction to Methods) 中描述. 

##### 7.6.6.1.2 通过优先级顺序对可应用方法排序

为了比较两个方法的优先级, 它们的参数指定符会被按顺序检查. 默认的检查顺序是从左到右, 但是可以通过对 defgeneric 或任何指定广义函数选项的操作符指定 :argument-precedence-order 选项来指定一个替代的顺序.

每一个方法的对应参数指定符都会被比较. 当一对参数指定符一致时, 比较下一对的一致性. 如果所有对应的参数指定符都一致, 那么两个方法必须有不同的限定符; 在这个情况下, 任何一个方法都可以优先于另一个. 关于一致性的更多信息, 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

如果某些对应参数不一致, 第一对不一致的参数决定了这个优先级. 如果两个参数指定符都是类, 两个方法中更具体的是方法参数指定符在这个对应参数的类优先级列表中出现的更早的那个方法. 由于可应用方法被选择的这个方法, 参数指定符保证存在于参数的类的类优先列表中.

如果一对对应参数指定符中只有一个是 (eql object), 带有这个参数指定符的方法优先于另一个方法. 如果两个参数指定符都是 eql 表达式, 这个参数指定符一定是一致的 (否则对于这个参数这两个方法不会都是可应用的).

产生的可应用方法列表中最具体的方法在第一个, 最不具体的在最后一个. 

##### 7.6.6.1.3 对排序后的可应用方法使用方法组合

在这个简单的例子中---如果使用了标准方法组合并且所有可应用的方法都是主方法---这个有效方法就是最具体的方法. 这个方法可以通过函数 call-next-method 调用下一个最具体的方法. 这个 call-next-method 会调用的方法被称为下一个方法. 断言 next-method-p 检测是否存在下一个方法. 如果 call-next-method 被调用并且没有下一个最具体的方法, 广义函数 no-next-method 会被调用.

通常, 有效方法是可应用方法的某个组合. 它由一个表达式形式来描述，这个表达式形式包含对某些或全部可应用方法的调用, 返回值或多值来作为广义函数返回的值或多值, 并可选地使一些方法可以通过 call-next-method 访问.

在这个有效方法中的每一个方法的角色由它的限定符和方法的特性所决定. 一个限定符用于标记一个方法, 而限定符的含义由这个过程的这一步使用这些标记的方式决定. 如果一个可应用方法由一个不识别的限定符, 这个步骤会发出一个错误并且不会在有效方法中包含那个方法.

当标准方法组合和限定符方法一起使用时, 产生的有效方法就像章节 7.6.6.2 (Standard Method Combination) 中所描述的那样.

另一个方法组合的类型可以通过使用 defgenric 或者任何其他指定广义函数选项的操作符的 :method-combination 选项来指定. 在这个情况下, 这个过程的这个步骤可以被定制.

新的方法组合类型可以通过使用 define-method-combination 宏来定义. 


#### 7.6.6.2 <span id="StandMethodComb">标准方法组合</span>

标准方法组合由类 standard-generic-function 支持. 如果没有指定其他类型的方法组合或者指定了内置的方法组合类型 standard, 那么这个标准方法组合就会被使用.

主方法(primary method)定义了这个有效方法的主要动作, 而辅助方法(auxiliary method)以三种方式之一修改那个动作. 一个主方法没有方法限定符.

一个辅助方法是一个限定符为 :before, :after, 或 :around 的方法. 标准方法组合不允许每个方法有超过一个限定符; 如果一个方法定义中指定了每个方法有超过一个限定符, 就会发出一个错误.

* 一个 before 方法有着作为它仅有限定符的关键字 :before. 一个 before 方法指定在任何主方法之前执行的代码.

* 一个 after 方法有着作为它仅有限定符的关键字 :after. 一个 after 方法指定在主方法后面运行的代码.

* 一个 around 方法有着作为它仅有限定符的关键字 :around. 一个 around 方法指定了要被运行的替代其他可应用方法的代码, 但是它可能包含显式的调用某些被遮蔽的方法的代码 (通过 call-next-method).

标准方法组合的语义如下:

* 如果这里有任何 around 方法, 最具体的 around 方法会被调用. 它提供这个广义函数的值或多值.

* 在一个 around 方法的主体内, call-next-method 可以被用于调用下一个方法. 当下一个方法返回时, 这个 around 方法可以执行更多的代码, 可能基于返回的值和多值. 如果调用了 call-next-method 并且这里没有可应用的方法被调用, 那么广义函数 no-next-method 会被调用. 函数 next-method-p 可能被用于确定是否存在下一个方法.

* 如果一个 around 方法调用了 call-next-method, 下一个最具体的 around 方法会被调用, 如果存在的话. 如果这里没有 around 方法或者如果 call-next-method 被最不具体的 around 方法所调用, 其他方法会按如下所述被调用:

    -- 所有的 before 方法会被调用, 以最具体优先的顺序. 它们的返回值会被忽略. 如果在一个 before 方法中使用 call-next-method 那么就会发出一个错误.

    -- 最具体的主方法会被调用. 在一个主方法的主体内, call-next-method 可以被用于调用下一个最具体的主方法. 当那个方法返回时, 前一个主要方法可以执行更多代码, 可能基于返回的值或多值. 如果使用了 call-next-method 并且这里没有更多可应用的主方法, 广义函数 no-next-method 会被调用. 函数 next-method-p 可能被用于确定是否存在下一个方法. 如果 call-next-method 没有被使用, 只有最具体的主方法会被调用.

    -- 所有的 after 方法按最不具体的优先顺序被调用. 它们的值会被忽略. 如果在一个 after 方法中使用 call-next-method, 那么会发出一个错误.

* 如果没有 around 方法被调用, 最具体的主方法提供这个广义函数返回的值或多值. 在最不具体的 around 方法中调用 call-next-method 返回的值或多值是最具体的主方法返回的那些.

在标准方法组合中, 如果这里有一个可应用的方法但是没有可应用的主方法, 会发出一个错误.

这个 before 方法按最具体优先的顺序被运行而 after 方法则按最不具体优先的顺序被运行. 这个区别的设计原理可以用一个例子来说明. 假设类 C1 通过添加 before 和 after 方法修改了它的超类 C2 的行为. 不管这个类 C2 的行为是通过 C2 的方法直接定义或是从它的超类中继承而来, 都不会影响在类 C1 的实例上调用方法的相关顺序. 类 C1 的 before 方法在类 C2 的所有方法之前运行. 类 C1 的 after 方法在类 C2 的所有方法之后运行.

相比之下, 所有 around 方法在任何其他方法运行前运行. 因此一个较不具体的 around 方法在一个较具体的主方法之前运行.

如果只使用了主方法而没有使用 call-next-method, 那么只有最具体的方法会被调用; 这也就是说, 较为具体的方法遮蔽更一般的方法. 


#### 7.6.6.3 <span id="DeclaraMethodComb">声明方法组合</span>

宏 define-method-combination 定义方法组合的新的表达式形式. 它为定制有效方法的产生提供了一个机制. 对于产生一个有效方法的默认过程在章节 7.6.6.1 (Determining the Effective Method) 中已描述. 这里有两个 define-method-combination 表达式形式. 短表达式形式是一个简单的工具而长表达式形式则更加强大和详细. 长表达式形式类似于 defmacro, 在它的主体中是一个计算一个 Lisp 表达式形式的表达式; 它为在方法组合中实现任意控制结构和方法限定符的任意处理提供一个机制. 


#### 7.6.6.4 <span id="BuiltInMethodCombTypes">内建的方法组合类型</span>

这个对象系统提供了一个内建的方法组合类型集合. 为了指定一个广义函数去使用这些方法组合类型之一, 那个方法组合类型的名字会传递给 defgeneric 的 :method-combination 选项或传递给任何指定广义函数选项的其他操作符的 :method-combination 选项.

内建的方法组合类型的名字列在下面这一段.

    +    append  max  nconc  progn     
    and  list    min  or     standard  

    Figure 7-2. 内建的方法组合类型

standard 内建的方法组合类型的语义描述在章节 7.6.6.2 (Standard Method Combination). 其他内置的方法组合类型称为简单内建的方法组合类型.

简单内建方法组合类型表现得就像它们是通过 define-method-combination 的短表达式形式定义出来的. 它们识别方法的两种角色:

* 一个 around 方法有着作为它唯一限定符的关键字符号 :around. 这个 :around 方法的意义和标准方法组合中一样. around 方法中支持使用函数 call-next-method 和 next-method-p.

* 一个主方法有着作为它唯一限定符的方法组合类型的名字. 比如, 内建的方法组合类型 and 识别单一限定符为 and 的方法; 这些是主方法. 在主方法中不支持使用函数 call-next-method 和 next-method-p.

简单内建方法组合类型的语义如下:

* 如果这里有任何一个 around 方法, 最具体的 around 方法会被调用. 它提供了这个广义函数的值或多值.

* 在一个 around 方法的主体内, 函数 call-next-method 可以被用于调用下一个方法. 如果 call-next-method 被调用而这里没有可应用的方法被调用, 那么就会调用广义函数 no-next-method. 函数 next-method-p 可能被用于确定是否存在下一个方法. 当When the next method returns, the around method can execute more code, perhaps based on the returned value or values.

* 如果一个 around 方法调用了 call-next-method, 下一个最具体的 around 方法被调用, 如果存在一个可应用的话. 如果这里没有 around 方法或者 call-next-method 被最不具体的 around 方法调用,从内建方法组合类型的名称和可应用的主方法列表中衍生出的 Lisp 表达式形式被求值来产生广义函数的值. 假设这个方法组合类型的名字是 operator 并且对广义函数的调用是

    (generic-function a1...an)

让 M1,...,Mk 是按次序可应用的主方法; 那么衍生的 Lisp 表达式形式是

    (operator <M1 a1...an>...<Mk a1...an>)

如果表达式形式 <Mi a1...an> 被求值, 方法 Mi 会被应用给参数 a1...an. 比如, 如果 operator 是 or, 那么表达式形式 <Mi a1...an> 当且仅当 <Mj a1...an>, 1<=j<\i 返回 nil 时被求值.

主方法的默认顺序是 :most-specific-first. 然而, 这个顺序可以通过提供 :most-specific-last 作为 :method-combination 选项的第二个参数来倒转.

简单内建方法组合类型要求一个方法一个限定符. 如果这里存在没有限定符或者这个方法组合类型不支持的限定符的可应用的方法就会发出一个错误. 如果这里有可应用的 around 方法并且没有可应用的主方法, 那么也会发出一个错误. 


### 7.6.7 <span id="">Inheritance of Methods</span>

A subclass inherits methods in the sense that any method applicable to all instances of a class is also applicable to all instances of any subclass of that class.

The inheritance of methods acts the same way regardless of which of the method-defining operators created the methods.

The inheritance of methods is described in detail in Section 7.6.6 (Method Selection and Combination). 


## 7.7 <span id="">The Objects Dictionary</span>

> *  [标准广义函数 FUNCTION-KEYWORDS](#SGF-FUNCTION-KEYWORDS)
> *  [函数 ENSURE-GENERIC-FUNCTION](#F-ENSURE-GENERIC-FUNCTION)
> *  [标准广义函数 ALLOCATE-INSTANCE](#SGF-ALLOCATE-INSTANCE)
> *  [标准广义函数 REINITIALIZE-INSTANCE](#SGF-REINITIALIZE-INSTANCE)
> *  [标准广义函数 SHARED-INITIALIZE](#SGF-SHARED-INITIALIZE)
> *  [标准广义函数 UPDATE-INSTANCE-FOR-DIFFERENT-CLASS](#SGF-U-I-F-D-C)
> *  [标准广义函数 UPDATE-INSTANCE-FOR-REDEFINED-CLASS](#SGF-U-I-F-R-C)
> *  [标准广义函数 CHANGE-CLASS](#SGF-CHANGE-CLASS)
> *  [函数 SLOT-BOUNDP](#F-SLOT-BOUNDP)
> *  [函数 SLOT-EXISTS-P](#F-SLOT-EXISTS-P)
> *  [函数 SLOT-MAKUNBOUND](#F-SLOT-MAKUNBOUND)
> *  [标准广义函数 SLOT-MISSING](#SGF-SLOT-MISSING)
> *  [标准广义函数 SLOT-UNBOUND](#SGF-SLOT-UNBOUND)
> *  [函数 SLOT-VALUE](#F-SLOT-VALUE)
> *  [标准广义函数 METHOD-QUALIFIERS](#SGF-METHOD-QUALIFIERS)
> *  [标准广义函数 NO-APPLICABLE-METHOD](#SGF-NO-APPLICABLE-METHOD)
> *  [标准广义函数 NO-NEXT-METHOD](#SGF-NO-NEXT-METHOD)
> *  [标准广义函数 REMOVE-METHOD](#SGF-REMOVE-METHOD)
> *  [标准广义函数 MAKE-INSTANCE](#SGF-MAKE-INSTANCE)
> *  [标准广义函数 MAKE-INSTANCES-OBSOLETE](#SGF-MAKE-INSTANCES-OBSOLETE)
> *  [标准广义函数 MAKE-LOAD-FORM](#SGF-MAKE-LOAD-FORM)
> *  [函数 MAKE-LOAD-FORM-SAVING-SLOTS](#F-MAKE-LOAD-FORM-SAVING-SLOTS)
> *  [宏 WITH-ACCESSORS](#M-WITH-ACCESSORS)
> *  [宏 WITH-SLOTS](#M-WITH-SLOTS)
> *  [宏 DEFCLASS](#M-DEFCLASS)
> *  [宏 DEFGENERIC](#M-DEFGENERIC)
> *  [宏 DEFMETHOD](#M-DEFMETHOD)
> *  [访问器 FIND-CLASS](#A-FIND-CLASS)
> *  [局部函数 NEXT-METHOD-P](#LF-NEXT-METHOD-P)
> *  [局部宏 CALL-METHOD, MAKE-METHOD](#LM-CALL-METHOD-MAKE-METHOD)
> *  [局部函数 CALL-NEXT-METHOD](#LF-CALL-NEXT-METHODs)
> *  [标准广义函数 COMPUTE-APPLICABLE-METHODS](#SGF-COMPUTE-APPLICABLE-METHODS)
> *  [宏 DEFINE-METHOD-COMBINATION](#M-DEFINE-METHOD-COMBINATION)
> *  [标准广义函数 FIND-METHOD](#SGF-FIND-METHOD)
> *  [标准广义函数 ADD-METHOD](#SGF-ADD-METHOD)
> *  [标准广义函数 INITIALIZE-INSTANCE](#SGF-INITIALIZE-INSTANCE)
> *  [标准广义函数 CLASS-NAME](#SGF-CLASS-NAME)
> *  [标准广义函数 (SETF CLASS-NAME)](#SGF-SETF-CLASS-NAME)
> *  [函数 CLASS-OF](#F-CLASS-OF)
> *  [状况类型 UNBOUND-SLOT](#CT-UNBOUND-SLOT)
> *  [函数 UNBOUND-SLOT-INSTANCE](#F-UNBOUND-SLOT-INSTANCE)


### <span id="SGF-FUNCTION-KEYWORDS">标准广义函数 FUNCTION-KEYWORDS</span>

* 语法(Syntax):

        function-keywords method => keys, allow-other-keys-p

* 方法签名(Method Signatures):

        function-keywords (method standard-method)

* 参数和值(Arguments and Values):

        method---一个方法.
        keys---一个列表.
        allow-other-keys-p---一个广义 boolean.

* 描述(Description):

        返回一个方法的关键字参数说明符.

        返回两个值: 一个显式命名的关键字的列表和一个表示 &allow-other-keys 在这个方法定义中是否被指定的广义的 boolean.

* 示例(Examples):

    ```LISP
    (defmethod gf1 ((a integer) &optional (b 2)
                    &key (c 3) ((:dee d) 4) e ((eff f)))
      (list a b c d e f))
    =>  #<STANDARD-METHOD GF1 (INTEGER) 36324653>
    (find-method #'gf1 '() (list (find-class 'integer))) 
    =>  #<STANDARD-METHOD GF1 (INTEGER) 36324653>
    (function-keywords *)
    =>  (:C :DEE :E EFF), false
    (defmethod gf2 ((a integer))
      (list a b c d e f))
    =>  #<STANDARD-METHOD GF2 (INTEGER) 42701775>
    (function-keywords (find-method #'gf1 '() (list (find-class 'integer))))
    =>  (), false
    (defmethod gf3 ((a integer) &key b c d &allow-other-keys)
      (list a b c d e f))
    (function-keywords *)
    =>  (:B :C :D), true
    ```

* 受此影响(Affected By):

        defmethod

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defmethod

* 注意(Notes): None. 


### <span id="">函数 ENSURE-GENERIC-FUNCTION</span>

* 语法(Syntax):

        ensure-generic-function function-name &key argument-precedence-order declare documentation environment generic-function-class lambda-list method-class method-combination

        => generic-function

* 参数和值(Arguments and Values):

        function-name---一个函数的名字.
        关键字参数对应 defgeneric 的选项参数, 除了 :method-class 和 :generic-function-class 参数可以是类对象和名字.
        Method-combination -- 方法组合对象.
        Environment -- 与宏扩展函数的 &environment 参数相同, 用于区分编译时和运行时环境.
        generic-function---一个广义函数对象.

* 描述(Description):

        函数 ensure-generic-function 被用于定义一个全局命名的没有方法的广义函数或者去指定或修改属于一个全局命名广义函数的选项和声明 as a whole <!--TODO as a whole ??-->.

        如果 function-name 没有在全局环境中被 fbound, 一个新的广义函数会被创建. 如果 (fdefinition function-name) 是一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

        如果 function-name 是一个列表, 它必须是表达式形式 (setf symbol). 如果 function-name 指定一个对于任何后面的参数有一个不同的值的广义函数, 广义函数被修改为有这个新值: :argument-precedence-order, :declare, :documentation, :method-combination.

        如果 function-name 指定一个对于 :lambda-list 参数有一个不同的值的广义函数, 并且这个新的值和所有已存在的方法的 lambda 列表是相等的或者这里没有对应方法, 那么这个值就会被修改; 否则就会发出一个错误.

        如果 function-name 指定一个对于 :generic-function-class 参数有一个不同的值的广义函数并且如果这个新的广义函数类和旧的兼容, 那么 change-class 会被调用来修改这个广义函数的类; 否则就会发出一个错误.

        如果 function-name 制定一个对于 :method-class 参数有一个不同的值的广义函数, 那么这个值会被修改, 但是任何存在的方法不会改变.

* 示例(Examples): None.

* 受此影响(Affected By):

        function-name 的函数绑定的存在.

* 异常情况(Exceptional Situations):

        如果 (fdefinition function-name) 是一个普通函数, 一个宏, 或者一个特殊操作符, 会发出一个 error 类型的错误.

        如果 function-name 指定了一个对于 :lambda-list 参数有一个不同的值的广义函数, 并且这个新的值和任何一个已存在方法的 lambda 列表不相同, 会发出一个 error 类型的错误.

        如果 function-name 指定了一个对于 :generic-function-class 参数有一个不同的值的广义函数并且如果这个新的广义函数和旧的不兼容, 会发出一个 error 类型的错误.

* 也见(See Also):

        defgeneric

* 注意(Notes): None. 


### <span id="SGF-ALLOCATE-INSTANCE">标准广义函数 ALLOCATE-INSTANCE</span>

* 语法(Syntax):

        allocate-instance class &rest initargs &key &allow-other-keys => new-instance

* 方法签名(Method Signatures):

        allocate-instance (class standard-class) &rest initargs
        allocate-instance (class structure-class) &rest initargs

* 参数和值(Arguments and Values):

        class---一个类.
        initargs---一个 keyword/value 对(初始化参数的名字和值) 的列表 .
        new-instance---一个类是 class 的对象.

* 描述(Description):

        广义函数 allocate-instance 创建并返回一个 class 的新的实例, 但是没有把它初始化. 当这个 class 是一个标准类时, 这就意味着这些槽是未绑定的; 当这个 class 是一个结构类时, 这就意味着这些槽的值是没有被指定的.

        allocate-instance 的调用者被期望已经检查了初始化参数.

        广义函数 allocate-instance 被 make-instance 所调用, 像章节 7.1 (Object Creation and Initialization) 中描述的那样.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defclass, make-instance, class-of, Section 7.1 (Object Creation and Initialization)

* 注意(Notes):

        给 allocate-instance 添加方法的后果是没有指定的. 这个功能可能被元对象协议添加进来. 


Standard Generic Function REINITIALIZE-INSTANCE

* 语法(Syntax):

reinitialize-instance instance &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

reinitialize-instance (instance standard-object) &rest initargs

* 参数和值(Arguments and Values):

instance---an object.

initargs---an initialization argument list.

* 描述(Description):

The generic function reinitialize-instance can be used to change the values of local slots of an instance according to initargs. This generic function can be called by users.

The system-supplied primary method for reinitialize-instance checks the validity of initargs and signals an error if an initarg is supplied that is not declared as valid. The method then calls the generic function shared-initialize with the following arguments: the instance, nil (which means no slots should be initialized according to their initforms), and the initargs it received.

* 示例(Examples): None.

Side Effects:

The generic function reinitialize-instance changes the values of local slots.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The system-supplied primary method for reinitialize-instance signals an error if an initarg is supplied that is not declared as valid.

* 也见(See Also):

initialize-instance, shared-initialize, update-instance-for-redefined-class, update-instance-for-different-class, slot-boundp, slot-makunbound, Section 7.3 (Reinitializing an Instance), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

Initargs are declared as valid by using the :initarg option to defclass, or by defining methods for reinitialize-instance or shared-initialize. The keyword name of each keyword parameter specifier in the lambda list of any method defined on reinitialize-instance or shared-initialize is declared as a valid initialization argument name for all classes for which that method is applicable. 


Standard Generic Function SHARED-INITIALIZE

* 语法(Syntax):

shared-initialize instance slot-names &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

shared-initialize (instance standard-object) slot-names &rest initargs

* 参数和值(Arguments and Values):

instance---an object.

slot-names---a list or t.

initargs---a list of keyword/value pairs (of initialization argument names and values).

* 描述(Description):

The generic function shared-initialize is used to fill the slots of an instance using initargs and :initform forms. It is called when an instance is created, when an instance is re-initialized, when an instance is updated to conform to a redefined class, and when an instance is updated to conform to a different class. The generic function shared-initialize is called by the system-supplied primary method for initialize-instance, reinitialize-instance, update-instance-for-redefined-class, and update-instance-for-different-class.

The generic function shared-initialize takes the following arguments: the instance to be initialized, a specification of a set of slot-names accessible in that instance, and any number of initargs. The arguments after the first two must form an initialization argument list. The system-supplied primary method on shared-initialize initializes the slots with values according to the initargs and supplied :initform forms. Slot-names indicates which slots should be initialized according to their :initform forms if no initargs are provided for those slots.

The system-supplied primary method behaves as follows, regardless of whether the slots are local or shared:

    If an initarg in the initialization argument list specifies a value for that slot, that value is stored into the slot, even if a value has already been stored in the slot before the method is run.

    Any slots indicated by slot-names that are still unbound at this point are initialized according to their :initform forms. For any such slot that has an :initform form, that form is evaluated in the lexical environment of its defining defclass form and the result is stored into the slot. For example, if a before method stores a value in the slot, the :initform form will not be used to supply a value for the slot.

    The rules mentioned in Section 7.1.4 (Rules for Initialization Arguments) are obeyed.

The slots-names argument specifies the slots that are to be initialized according to their :initform forms if no initialization arguments apply. It can be a list of slot names, which specifies the set of those slot names; or it can be the symbol t, which specifies the set of all of the slots.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

initialize-instance, reinitialize-instance, update-instance-for-redefined-class, update-instance-for-different-class, slot-boundp, slot-makunbound, Section 7.1 (Object Creation and Initialization), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

Initargs are declared as valid by using the :initarg option to defclass, or by defining methods for shared-initialize. The keyword name of each keyword parameter specifier in the lambda list of any method defined on shared-initialize is declared as a valid initarg name for all classes for which that method is applicable.

Implementations are permitted to optimize :initform forms that neither produce nor depend on side effects, by evaluating these forms and storing them into slots before running any initialize-instance methods, rather than by handling them in the primary initialize-instance method. (This optimization might be implemented by having the allocate-instance method copy a prototype instance.)

Implementations are permitted to optimize default initial value forms for initargs associated with slots by not actually creating the complete initialization argument list when the only method that would receive the complete list is the method on standard-object. In this case default initial value forms can be treated like :initform forms. This optimization has no visible effects other than a performance improvement. 


Standard Generic Function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

* 语法(Syntax):

update-instance-for-different-class previous current &rest initargs &key &allow-other-keys => implementation-dependent

* 方法签名(Method Signatures):

update-instance-for-different-class (previous standard-object) (current standard-object) &rest initargs

* 参数和值(Arguments and Values):

previous---a copy of the original instance.

current---the original instance (altered).

initargs---an initialization argument list.

* 描述(Description):

The generic function update-instance-for-different-class is not intended to be called by programmers. Programmers may write methods for it. The function update-instance-for-different-class is called only by the function change-class.

The system-supplied primary method on update-instance-for-different-class checks the validity of initargs and signals an error if an initarg is supplied that is not declared as valid. This method then initializes slots with values according to the initargs, and initializes the newly added slots with values according to their :initform forms. It does this by calling the generic function shared-initialize with the following arguments: the instance (current), a list of names of the newly added slots, and the initargs it received. Newly added slots are those local slots for which no slot of the same name exists in the previous class.

Methods for update-instance-for-different-class can be defined to specify actions to be taken when an instance is updated. If only after methods for update-instance-for-different-class are defined, they will be run after the system-supplied primary method for initialization and therefore will not interfere with the default behavior of update-instance-for-different-class.

Methods on update-instance-for-different-class can be defined to initialize slots differently from change-class. The default behavior of change-class is described in Section 7.2 (Changing the Class of an Instance).

The arguments to update-instance-for-different-class are computed by change-class. When change-class is invoked on an instance, a copy of that instance is made; change-class then destructively alters the original instance. The first argument to update-instance-for-different-class, previous, is that copy; it holds the old slot values temporarily. This argument has dynamic extent within change-class; if it is referenced in any way once update-instance-for-different-class returns, the results are undefined. The second argument to update-instance-for-different-class, current, is the altered original instance. The intended use of previous is to extract old slot values by using slot-value or with-slots or by invoking a reader generic function, or to run other methods that were applicable to instances of the original class.

* 示例(Examples):

See the example for the function change-class.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The system-supplied primary method on update-instance-for-different-class signals an error if an initialization argument is supplied that is not declared as valid.

* 也见(See Also):

change-class, shared-initialize, Section 7.2 (Changing the Class of an Instance), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

Initargs are declared as valid by using the :initarg option to defclass, or by defining methods for update-instance-for-different-class or shared-initialize. The keyword name of each keyword parameter specifier in the lambda list of any method defined on update-instance-for-different-class or shared-initialize is declared as a valid initarg name for all classes for which that method is applicable.

The value returned by update-instance-for-different-class is ignored by change-class. 


Standard Generic Function UPDATE-INSTANCE-FOR-REDEFINED-CLASS

* 语法(Syntax):

update-instance-for-redefined-class instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys

=> result*

* 方法签名(Method Signatures):

update-instance-for-redefined-class (instance standard-object) added-slots discarded-slots property-list &rest initargs

* 参数和值(Arguments and Values):

instance---an object.

added-slots---a list.

discarded-slots---a list.

property-list---a list.

initargs---an initialization argument list.

result---an object.

* 描述(Description):

The generic function update-instance-for-redefined-class is not intended to be called by programmers. Programmers may write methods for it. The generic function update-instance-for-redefined-class is called by the mechanism activated by make-instances-obsolete.

The system-supplied primary method on update-instance-for-redefined-class checks the validity of initargs and signals an error if an initarg is supplied that is not declared as valid. This method then initializes slots with values according to the initargs, and initializes the newly added-slots with values according to their :initform forms. It does this by calling the generic function shared-initialize with the following arguments: the instance, a list of names of the newly added-slots to instance, and the initargs it received. Newly added-slots are those local slots for which no slot of the same name exists in the old version of the class.

When make-instances-obsolete is invoked or when a class has been redefined and an instance is being updated, a property-list is created that captures the slot names and values of all the discarded-slots with values in the original instance. The structure of the instance is transformed so that it conforms to the current class definition. The arguments to update-instance-for-redefined-class are this transformed instance, a list of added-slots to the instance, a list discarded-slots from the instance, and the property-list containing the slot names and values for slots that were discarded and had values. Included in this list of discarded slots are slots that were local in the old class and are shared in the new class.

The value returned by update-instance-for-redefined-class is ignored.

* 示例(Examples):

  
 (defclass position () ())
 
 (defclass x-y-position (position)
     ((x :initform 0 :accessor position-x)
      (y :initform 0 :accessor position-y)))
 
;;; It turns out polar coordinates are used more than Cartesian 
;;; coordinates, so the representation is altered and some new
;;; accessor methods are added.
 
 (defmethod update-instance-for-redefined-class :before
    ((pos x-y-position) added deleted plist &key)
   ;; Transform the x-y coordinates to polar coordinates
   ;; and store into the new slots.
   (let ((x (getf plist 'x))
         (y (getf plist 'y)))
     (setf (position-rho pos) (sqrt (+ (* x x) (* y y)))
           (position-theta pos) (atan y x))))
  
 (defclass x-y-position (position)
     ((rho :initform 0 :accessor position-rho)
      (theta :initform 0 :accessor position-theta)))
  
;;; All instances of the old x-y-position class will be updated
;;; automatically.
 
;;; The new representation is given the look and feel of the old one.
 
 (defmethod position-x ((pos x-y-position))  
    (with-slots (rho theta) pos (* rho (cos theta))))
 
 (defmethod (setf position-x) (new-x (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((y (position-y pos)))
        (setq rho (sqrt (+ (* new-x new-x) (* y y)))
              theta (atan y new-x))
        new-x)))
 
 (defmethod position-y ((pos x-y-position))
    (with-slots (rho theta) pos (* rho (sin theta))))
 
 (defmethod (setf position-y) (new-y (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((x (position-x pos)))
        (setq rho (sqrt (+ (* x x) (* new-y new-y)))
              theta (atan new-y x))
        new-y)))
 

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The system-supplied primary method on update-instance-for-redefined-class signals an error if an initarg is supplied that is not declared as valid.

* 也见(See Also):

make-instances-obsolete, shared-initialize, Section 4.3.6 (Redefining Classes), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

Initargs are declared as valid by using the :initarg option to defclass, or by defining methods for update-instance-for-redefined-class or shared-initialize. The keyword name of each keyword parameter specifier in the lambda list of any method defined on update-instance-for-redefined-class or shared-initialize is declared as a valid initarg name for all classes for which that method is applicable. 


Standard Generic Function CHANGE-CLASS

* 语法(Syntax):

change-class instance new-class &key &allow-other-keys => instance

* 方法签名(Method Signatures):

change-class (instance standard-object) (new-class standard-class) &rest initargs

change-class (instance t) (new-class symbol) &rest initargs

* 参数和值(Arguments and Values):

instance---an object.

new-class---a class designator.

initargs---an initialization argument list.

* 描述(Description):

The generic function change-class changes the class of an instance to new-class. It destructively modifies and returns the instance.

If in the old class there is any slot of the same name as a local slot in the new-class, the value of that slot is retained. This means that if the slot has a value, the value returned by slot-value after change-class is invoked is eql to the value returned by slot-value before change-class is invoked. Similarly, if the slot was unbound, it remains unbound. The other slots are initialized as described in Section 7.2 (Changing the Class of an Instance).

After completing all other actions, change-class invokes update-instance-for-different-class. The generic function update-instance-for-different-class can be used to assign values to slots in the transformed instance. See Section 7.2.2 (Initializing Newly Added Local Slots).

If the second of the above methods is selected, that method invokes change-class on instance, (find-class new-class), and the initargs.

* 示例(Examples):

 
 (defclass position () ())
  
 (defclass x-y-position (position)
     ((x :initform 0 :initarg :x)
      (y :initform 0 :initarg :y)))
  
 (defclass rho-theta-position (position)
     ((rho :initform 0)
      (theta :initform 0)))
  
 (defmethod update-instance-for-different-class :before ((old x-y-position) 
                                                         (new rho-theta-position)
                                                         &key)
   ;; Copy the position information from old to new to make new
   ;; be a rho-theta-position at the same position as old.
   (let ((x (slot-value old 'x))
         (y (slot-value old 'y)))
     (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
           (slot-value new 'theta) (atan y x))))
  
;;; At this point an instance of the class x-y-position can be
;;; changed to be an instance of the class rho-theta-position using
;;; change-class:
 
 (setq p1 (make-instance 'x-y-position :x 2 :y 0))
  
 (change-class p1 'rho-theta-position)
  
;;; The result is that the instance bound to p1 is now an instance of
;;; the class rho-theta-position.   The update-instance-for-different-class
;;; method performed the initialization of the rho and theta slots based
;;; on the value of the x and y slots, which were maintained by
;;; the old instance.
 

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

update-instance-for-different-class, Section 7.2 (Changing the Class of an Instance)

* 注意(Notes):

The generic function change-class has several semantic difficulties. First, it performs a destructive operation that can be invoked within a method on an instance that was used to select that method. When multiple methods are involved because methods are being combined, the methods currently executing or about to be executed may no longer be applicable. Second, some implementations might use compiler optimizations of slot access, and when the class of an instance is changed the assumptions the compiler made might be violated. This implies that a programmer must not use change-class inside a method if any methods for that generic function access any slots, or the results are undefined. 


Function SLOT-BOUNDP

* 语法(Syntax):

slot-boundp instance slot-name => generalized-boolean

* 参数和值(Arguments and Values):

instance---an object.

slot-name---a symbol naming a slot of instance.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if the slot named slot-name in instance is bound; otherwise, returns false.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If no slot of the name slot-name exists in the instance, slot-missing is called as follows:

 (slot-missing (class-of instance)
               instance
               slot-name
               'slot-boundp)

(If slot-missing is invoked and returns a value, a boolean equivalent to its primary value is returned by slot-boundp.)

The specific behavior depends on instance's metaclass. An error is never signaled if instance has metaclass standard-class. An error is always signaled if instance has metaclass built-in-class. The consequences are undefined if instance has any other metaclass--an error might or might not be signaled in this situation. Note in particular that the behavior for conditions and structures is not specified.

* 也见(See Also):

slot-makunbound, slot-missing

* 注意(Notes):

The function slot-boundp allows for writing after methods on initialize-instance in order to initialize only those slots that have not already been bound.

Although no implementation is required to do so, implementors are strongly encouraged to implement the function slot-boundp using the function slot-boundp-using-class described in the Metaobject Protocol. 


Function SLOT-EXISTS-P

* 语法(Syntax):

slot-exists-p object slot-name => generalized-boolean

* 参数和值(Arguments and Values):

object---an object.

slot-name---a symbol.

generalized-boolean---a generalized boolean.

* 描述(Description):

Returns true if the object has a slot named slot-name.

* 示例(Examples): None.

* 受此影响(Affected By):

defclass, defstruct

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

defclass, slot-missing

* 注意(Notes):

Although no implementation is required to do so, implementors are strongly encouraged to implement the function slot-exists-p using the function slot-exists-p-using-class described in the Metaobject Protocol. 


Function SLOT-MAKUNBOUND

* 语法(Syntax):

slot-makunbound instance slot-name => instance

* 参数和值(Arguments and Values):

instance -- instance.

Slot-name---a symbol.

* 描述(Description):

The function slot-makunbound restores a slot of the name slot-name in an instance to the unbound state.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If no slot of the name slot-name exists in the instance, slot-missing is called as follows:

(slot-missing (class-of instance)
              instance
              slot-name
              'slot-makunbound)

(Any values returned by slot-missing in this case are ignored by slot-makunbound.)

The specific behavior depends on instance's metaclass. An error is never signaled if instance has metaclass standard-class. An error is always signaled if instance has metaclass built-in-class. The consequences are undefined if instance has any other metaclass--an error might or might not be signaled in this situation. Note in particular that the behavior for conditions and structures is not specified.

* 也见(See Also):

slot-boundp, slot-missing

* 注意(Notes):

Although no implementation is required to do so, implementors are strongly encouraged to implement the function slot-makunbound using the function slot-makunbound-using-class described in the Metaobject Protocol. 


Standard Generic Function SLOT-MISSING

* 语法(Syntax):

slot-missing class object slot-name operation &optional new-value => result*

* 方法签名(Method Signatures):

slot-missing (class t) object slot-name operation &optional new-value

* 参数和值(Arguments and Values):

class---the class of object.

object---an object.

slot-name---a symbol (the name of a would-be slot).

operation---one of the symbols setf, slot-boundp, slot-makunbound, or slot-value.

new-value---an object.

result---an object.

* 描述(Description):

The generic function slot-missing is invoked when an attempt is made to access a slot in an object whose metaclass is standard-class and the slot of the name slot-name is not a name of a slot in that class. The default method signals an error.

The generic function slot-missing is not intended to be called by programmers. Programmers may write methods for it.

The generic function slot-missing may be called during evaluation of slot-value, (setf slot-value), slot-boundp, and slot-makunbound. For each of these operations the corresponding symbol for the operation argument is slot-value, setf, slot-boundp, and slot-makunbound respectively.

The optional new-value argument to slot-missing is used when the operation is attempting to set the value of the slot.

If slot-missing returns, its values will be treated as follows:

    If the operation is setf or slot-makunbound, any values will be ignored by the caller.

    If the operation is slot-value, only the primary value will be used by the caller, and all other values will be ignored.

    If the operation is slot-boundp, any boolean equivalent of the primary value of the method might be is used, and all other values will be ignored.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The default method on slot-missing signals an error of type error.

* 也见(See Also):

defclass, slot-exists-p, slot-value

* 注意(Notes):

The set of arguments (including the class of the instance) facilitates defining methods on the metaclass for slot-missing. 


Standard Generic Function SLOT-UNBOUND

* 语法(Syntax):

slot-unbound class instance slot-name => result*

* 方法签名(Method Signatures):

slot-unbound (class t) instance slot-name

* 参数和值(Arguments and Values):

class---the class of the instance.

instance---the instance in which an attempt was made to read the unbound slot.

slot-name---the name of the unbound slot.

result---an object.

* 描述(Description):

The generic function slot-unbound is called when an unbound slot is read in an instance whose metaclass is standard-class. The default method signals an error of type unbound-slot. The name slot of the unbound-slot condition is initialized to the name of the offending variable, and the instance slot of the unbound-slot condition is initialized to the offending instance.

The generic function slot-unbound is not intended to be called by programmers. Programmers may write methods for it. The function slot-unbound is called only indirectly by slot-value.

If slot-unbound returns, only the primary value will be used by the caller, and all other values will be ignored.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The default method on slot-unbound signals an error of type unbound-slot.

* 也见(See Also):

slot-makunbound

* 注意(Notes):

An unbound slot may occur if no :initform form was specified for the slot and the slot value has not been set, or if slot-makunbound has been called on the slot. 


Function SLOT-VALUE

* 语法(Syntax):

slot-value object slot-name => value

* 参数和值(Arguments and Values):

object---an object.

name---a symbol.

value---an object.

* 描述(Description):

The function slot-value returns the value of the slot named slot-name in the object. If there is no slot named slot-name, slot-missing is called. If the slot is unbound, slot-unbound is called.

The macro setf can be used with slot-value to change the value of a slot.

* 示例(Examples):

 (defclass foo () 
   ((a :accessor foo-a :initarg :a :initform 1)
    (b :accessor foo-b :initarg :b)
    (c :accessor foo-c :initform 3)))
=>  #<STANDARD-CLASS FOO 244020371>
 (setq foo1 (make-instance 'foo :a 'one :b 'two))
=>  #<FOO 36325624>
 (slot-value foo1 'a) =>  ONE
 (slot-value foo1 'b) =>  TWO
 (slot-value foo1 'c) =>  3
 (setf (slot-value foo1 'a) 'uno) =>  UNO
 (slot-value foo1 'a) =>  UNO
 (defmethod foo-method ((x foo))
   (slot-value x 'a))
=>  #<STANDARD-METHOD FOO-METHOD (FOO) 42720573>
 (foo-method foo1) =>  UNO

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If an attempt is made to read a slot and no slot of the name slot-name exists in the object, slot-missing is called as follows:

 (slot-missing (class-of instance)
               instance
               slot-name
               'slot-value)

(If slot-missing is invoked, its primary value is returned by slot-value.)

If an attempt is made to write a slot and no slot of the name slot-name exists in the object, slot-missing is called as follows:

 (slot-missing (class-of instance)
               instance
               slot-name
               'setf
               new-value)

(If slot-missing returns in this case, any values are ignored.)

The specific behavior depends on object's metaclass. An error is never signaled if object has metaclass standard-class. An error is always signaled if object has metaclass built-in-class. The consequences are unspecified if object has any other metaclass--an error might or might not be signaled in this situation. Note in particular that the behavior for conditions and structures is not specified.

* 也见(See Also):

slot-missing, slot-unbound, with-slots

* 注意(Notes):

Although no implementation is required to do so, implementors are strongly encouraged to implement the function slot-value using the function slot-value-using-class described in the Metaobject Protocol.

Implementations may optimize slot-value by compiling it inline. 


Standard Generic Function METHOD-QUALIFIERS

* 语法(Syntax):

method-qualifiers method => qualifiers

* 方法签名(Method Signatures):

method-qualifiers (method standard-method)

* 参数和值(Arguments and Values):

method---a method.

qualifiers---a proper list.

* 描述(Description):

Returns a list of the qualifiers of the method.

* 示例(Examples):

 (defmethod some-gf :before ((a integer)) a)
=>  #<STANDARD-METHOD SOME-GF (:BEFORE) (INTEGER) 42736540>
 (method-qualifiers *) =>  (:BEFORE)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

define-method-combination

* 注意(Notes): None. 


Standard Generic Function NO-APPLICABLE-METHOD

* 语法(Syntax):

no-applicable-method generic-function &rest function-arguments => result*

* 方法签名(Method Signatures):

no-applicable-method (generic-function t) &rest function-arguments

* 参数和值(Arguments and Values):

generic-function---a generic function on which no applicable method was found.

function-arguments---arguments to the generic-function.

result---an object.

* 描述(Description):

The generic function no-applicable-method is called when a generic function is invoked and no method on that generic function is applicable. The default method signals an error.

The generic function no-applicable-method is not intended to be called by programmers. Programmers may write methods for it.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The default method signals an error of type error.

* 也见(See Also):

* 注意(Notes): None. 


Standard Generic Function NO-NEXT-METHOD

* 语法(Syntax):

no-next-method generic-function method &rest args => result*

* 方法签名(Method Signatures):

no-next-method (generic-function standard-generic-function) (method standard-method) &rest args

* 参数和值(Arguments and Values):

generic-function -- generic function to which method belongs.

method -- method that contained the call to call-next-method for which there is no next method.

args -- arguments to call-next-method.

result---an object.

* 描述(Description):

The generic function no-next-method is called by call-next-method when there is no next method.

The generic function no-next-method is not intended to be called by programmers. Programmers may write methods for it.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The system-supplied method on no-next-method signals an error of type error.

* 也见(See Also):

call-next-method

* 注意(Notes): None. 


Standard Generic Function REMOVE-METHOD

* 语法(Syntax):

remove-method generic-function method => generic-function

* 方法签名(Method Signatures):

remove-method (generic-function standard-generic-function) method

* 参数和值(Arguments and Values):

generic-function---a generic function.

method---a method.

* 描述(Description):

The generic function remove-method removes a method from generic-function by modifying the generic-function (if necessary).

remove-method must not signal an error if the method is not one of the methods on the generic-function.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

find-method

* 注意(Notes): None. 


Standard Generic Function MAKE-INSTANCE

* 语法(Syntax):

make-instance class &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

make-instance (class standard-class) &rest initargs

make-instance (class symbol) &rest initargs

* 参数和值(Arguments and Values):

class---a class, or a symbol that names a class.

initargs---an initialization argument list.

instance---a fresh instance of class class.

* 描述(Description):

The generic function make-instance creates and returns a new instance of the given class.

If the second of the above methods is selected, that method invokes make-instance on the arguments (find-class class) and initargs.

The initialization arguments are checked within make-instance.

The generic function make-instance may be used as described in Section 7.1 (Object Creation and Initialization).

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If any of the initialization arguments has not been declared as valid, an error of type error is signaled.

* 也见(See Also):

defclass, class-of, allocate-instance, initialize-instance, Section 7.1 (Object Creation and Initialization)

* 注意(Notes): None. 


Standard Generic Function MAKE-INSTANCES-OBSOLETE

* 语法(Syntax):

make-instances-obsolete class => class

* 方法签名(Method Signatures):

make-instances-obsolete (class standard-class)

make-instances-obsolete (class symbol)

* 参数和值(Arguments and Values):

class---a class designator.

* 描述(Description):

The function make-instances-obsolete has the effect of initiating the process of updating the instances of the class. During updating, the generic function update-instance-for-redefined-class will be invoked.

The generic function make-instances-obsolete is invoked automatically by the system when defclass has been used to redefine an existing standard class and the set of local slots accessible in an instance is changed or the order of slots in storage is changed. It can also be explicitly invoked by the user.

If the second of the above methods is selected, that method invokes make-instances-obsolete on (find-class class).

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

update-instance-for-redefined-class, Section 4.3.6 (Redefining Classes)

* 注意(Notes): None. 


Standard Generic Function MAKE-LOAD-FORM

* 语法(Syntax):

make-load-form object &optional environment => creation-form[, initialization-form]

* 方法签名(Method Signatures):

make-load-form (object standard-object) &optional environment

make-load-form (object structure-object) &optional environment

make-load-form (object condition) &optional environment

make-load-form (object class) &optional environment

* 参数和值(Arguments and Values):

object---an object.

environment---an environment object.

creation-form---a form.

initialization-form---a form.

* 描述(Description):

The generic function make-load-form creates and returns one or two forms, a creation-form and an initialization-form, that enable load to construct an object equivalent to object. Environment is an environment object corresponding to the lexical environment in which the forms will be processed.

The file compiler calls make-load-form to process certain classes of literal objects; see Section 3.2.4.4 (Additional Constraints on Externalizable Objects).

Conforming programs may call make-load-form directly, providing object is a generalized instance of standard-object, structure-object, or condition.

The creation form is a form that, when evaluated at load time, should return an object that is equivalent to object. The exact meaning of equivalent depends on the type of object and is up to the programmer who defines a method for make-load-form; see Section 3.2.4 (Literal Objects in Compiled Files).

The initialization form is a form that, when evaluated at load time, should perform further initialization of the object. The value returned by the initialization form is ignored. If make-load-form returns only one value, the initialization form is nil, which has no effect. If object appears as a constant in the initialization form, at load time it will be replaced by the equivalent object constructed by the creation form; this is how the further initialization gains access to the object.

Both the creation-form and the initialization-form may contain references to any externalizable object. However, there must not be any circular dependencies in creation forms. An example of a circular dependency is when the creation form for the object X contains a reference to the object Y, and the creation form for the object Y contains a reference to the object X. Initialization forms are not subject to any restriction against circular dependencies, which is the reason that initialization forms exist; see the example of circular data structures below.

The creation form for an object is always evaluated before the initialization form for that object. When either the creation form or the initialization form references other objects that have not been referenced earlier in the file being compiled, the compiler ensures that all of the referenced objects have been created before evaluating the referencing form. When the referenced object is of a type which the file compiler processes using make-load-form, this involves evaluating the creation form returned for it. (This is the reason for the prohibition against circular references among creation forms).

Each initialization form is evaluated as soon as possible after its associated creation form, as determined by data flow. If the initialization form for an object does not reference any other objects not referenced earlier in the file and processed by the file compiler using make-load-form, the initialization form is evaluated immediately after the creation form. If a creation or initialization form F does contain references to such objects, the creation forms for those other objects are evaluated before F, and the initialization forms for those other objects are also evaluated before F whenever they do not depend on the object created or initialized by F. Where these rules do not uniquely determine an order of evaluation between two creation/initialization forms, the order of evaluation is unspecified.

While these creation and initialization forms are being evaluated, the objects are possibly in an uninitialized state, analogous to the state of an object between the time it has been created by allocate-instance and it has been processed fully by initialize-instance. Programmers writing methods for make-load-form must take care in manipulating objects not to depend on slots that have not yet been initialized.

It is implementation-dependent whether load calls eval on the forms or does some other operation that has an equivalent effect. For example, the forms might be translated into different but equivalent forms and then evaluated, they might be compiled and the resulting functions called by load, or they might be interpreted by a special-purpose function different from eval. All that is required is that the effect be equivalent to evaluating the forms.

The method specialized on class returns a creation form using the name of the class if the class has a proper name in environment, signaling an error of type error if it does not have a proper name. Evaluation of the creation form uses the name to find the class with that name, as if by calling find-class. If a class with that name has not been defined, then a class may be computed in an implementation-defined manner. If a class cannot be returned as the result of evaluating the creation form, then an error of type error is signaled.

Both conforming implementations and conforming programs may further specialize make-load-form.

* 示例(Examples):

 (defclass obj ()
    ((x :initarg :x :reader obj-x)
     (y :initarg :y :reader obj-y)
     (dist :accessor obj-dist)))
=>  #<STANDARD-CLASS OBJ 250020030>
 (defmethod shared-initialize :after ((self obj) slot-names &rest keys)
   (declare (ignore slot-names keys))
   (unless (slot-boundp self 'dist)
     (setf (obj-dist self)
           (sqrt (+ (expt (obj-x self) 2) (expt (obj-y self) 2))))))
=>  #<STANDARD-METHOD SHARED-INITIALIZE (:AFTER) (OBJ T) 26266714>
 (defmethod make-load-form ((self obj) &optional environment)
   (declare (ignore environment))
   ;; Note that this definition only works because X and Y do not
   ;; contain information which refers back to the object itself.
   ;; For a more general solution to this problem, see revised example below.
   `(make-instance ',(class-of self)
                   :x ',(obj-x self) :y ',(obj-y self)))
=>  #<STANDARD-METHOD MAKE-LOAD-FORM (OBJ) 26267532>
 (setq obj1 (make-instance 'obj :x 3.0 :y 4.0)) =>  #<OBJ 26274136>
 (obj-dist obj1) =>  5.0
 (make-load-form obj1) =>  (MAKE-INSTANCE 'OBJ :X '3.0 :Y '4.0)

In the above example, an equivalent instance of obj is reconstructed by using the values of two of its slots. The value of the third slot is derived from those two values.

Another way to write the make-load-form method in that example is to use make-load-form-saving-slots. The code it generates might yield a slightly different result from the make-load-form method shown above, but the operational effect will be the same. For example:

 ;; Redefine method defined above.
 (defmethod make-load-form ((self obj) &optional environment)
    (make-load-form-saving-slots self
                                 :slot-names '(x y)
                                 :environment environment))
=>  #<STANDARD-METHOD MAKE-LOAD-FORM (OBJ) 42755655>
 ;; Try MAKE-LOAD-FORM on object created above.
 (make-load-form obj1)
=>  (ALLOCATE-INSTANCE '#<STANDARD-CLASS OBJ 250020030>),
    (PROGN
      (SETF (SLOT-VALUE '#<OBJ 26274136> 'X) '3.0)
      (SETF (SLOT-VALUE '#<OBJ 26274136> 'Y) '4.0)
      (INITIALIZE-INSTANCE '#<OBJ 26274136>))

In the following example, instances of my-frob are ``interned'' in some way. An equivalent instance is reconstructed by using the value of the name slot as a key for searching existing objects. In this case the programmer has chosen to create a new object if no existing object is found; alternatively an error could have been signaled in that case.

 (defclass my-frob ()
    ((name :initarg :name :reader my-name)))
 (defmethod make-load-form ((self my-frob) &optional environment)
   (declare (ignore environment))
   `(find-my-frob ',(my-name self) :if-does-not-exist :create))

In the following example, the data structure to be dumped is circular, because each parent has a list of its children and each child has a reference back to its parent. If make-load-form is called on one object in such a structure, the creation form creates an equivalent object and fills in the children slot, which forces creation of equivalent objects for all of its children, grandchildren, etc. At this point none of the parent slots have been filled in. The initialization form fills in the parent slot, which forces creation of an equivalent object for the parent if it was not already created. Thus the entire tree is recreated at load time. At compile time, make-load-form is called once for each object in the tree. All of the creation forms are evaluated, in implementation-dependent order, and then all of the initialization forms are evaluated, also in implementation-dependent order.

 (defclass tree-with-parent () ((parent :accessor tree-parent)
                                (children :initarg :children)))
 (defmethod make-load-form ((x tree-with-parent) &optional environment)
   (declare (ignore environment))
   (values
     ;; creation form
     `(make-instance ',(class-of x) :children ',(slot-value x 'children))
     ;; initialization form
     `(setf (tree-parent ',x) ',(slot-value x 'parent))))

In the following example, the data structure to be dumped has no special properties and an equivalent structure can be reconstructed simply by reconstructing the slots' contents.

 (defstruct my-struct a b c)
 (defmethod make-load-form ((s my-struct) &optional environment)
    (make-load-form-saving-slots s :environment environment))

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The methods specialized on standard-object, structure-object, and condition all signal an error of type error.

It is implementation-dependent whether calling make-load-form on a generalized instance of a system class signals an error or returns creation and initialization forms.

* 也见(See Also):

compile-file, make-load-form-saving-slots, Section 3.2.4.4 (Additional Constraints on Externalizable Objects) Section 3.1 (Evaluation), Section 3.2 (Compilation)

* 注意(Notes):

The file compiler calls make-load-form in specific circumstances detailed in Section 3.2.4.4 (Additional Constraints on Externalizable Objects).

Some implementations may provide facilities for defining new subclasses of classes which are specified as system classes. (Some likely candidates include generic-function, method, and stream). Such implementations should document how the file compiler processes instances of such classes when encountered as literal objects, and should document any relevant methods for make-load-form. 


Function MAKE-LOAD-FORM-SAVING-SLOTS

* 语法(Syntax):

make-load-form-saving-slots object &key slot-names environment

=> creation-form, initialization-form

* 参数和值(Arguments and Values):

object---an object.

slot-names---a list.

environment---an environment object.

creation-form---a form.

initialization-form---a form.

* 描述(Description):

Returns forms that, when evaluated, will construct an object equivalent to object, without executing initialization forms. The slots in the new object that correspond to initialized slots in object are initialized using the values from object. Uninitialized slots in object are not initialized in the new object. make-load-form-saving-slots works for any instance of standard-object or structure-object.

Slot-names is a list of the names of the slots to preserve. If slot-names is not supplied, its value is all of the local slots.

make-load-form-saving-slots returns two values, thus it can deal with circular structures. Whether the result is useful in an application depends on whether the object's type and slot contents fully capture the application's idea of the object's state.

Environment is the environment in which the forms will be processed.

* 示例(Examples): None.

Side Effects: None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-load-form, make-instance, setf, slot-value, slot-makunbound

* 注意(Notes):

make-load-form-saving-slots can be useful in user-written make-load-form methods.

When the object is an instance of standard-object, make-load-form-saving-slots could return a creation form that calls allocate-instance and an initialization form that contains calls to setf of slot-value and slot-makunbound, though other functions of similar effect might actually be used. 


Macro WITH-ACCESSORS

* 语法(Syntax):

with-accessors (slot-entry*) instance-form declaration* form*

=> result*

slot-entry::= (variable-name accessor-name) 

* 参数和值(Arguments and Values):

variable-name---a variable name; not evaluated.

accessor-name---a function name; not evaluated.

instance-form---a form; evaluated.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

Creates a lexical environment in which the slots specified by slot-entry are lexically available through their accessors as if they were variables. The macro with-accessors invokes the appropriate accessors to access the slots specified by slot-entry. Both setf and setq can be used to set the value of the slot.

* 示例(Examples):

 (defclass thing ()
           ((x :initarg :x :accessor thing-x)
            (y :initarg :y :accessor thing-y)))
=>  #<STANDARD-CLASS THING 250020173>
 (defmethod (setf thing-x) :before (new-x (thing thing))
   (format t "~&Changing X from ~D to ~D in ~S.~%"
           (thing-x thing) new-x thing))
 (setq thing1 (make-instance 'thing :x 1 :y 2)) =>  #<THING 43135676>
 (setq thing2 (make-instance 'thing :x 7 :y 8)) =>  #<THING 43147374>
 (with-accessors ((x1 thing-x) (y1 thing-y))
                 thing1
   (with-accessors ((x2 thing-x) (y2 thing-y))
                   thing2
     (list (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2))
           (setq x1 (+ y1 x2))
           (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2))
           (setf (thing-x thing2) (list x1))
           (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2)))))
>>  Changing X from 1 to 9 in #<THING 43135676>.
>>  Changing X from 7 to (9) in #<THING 43147374>.
=>  ((1 1 2 2 7 7 8 8)
     9
     (9 9 2 2 7 7 8 8) 
     (9)
     (9 9 2 2 (9) (9) 8 8))

* 受此影响(Affected By):

defclass

* 异常情况(Exceptional Situations):

The consequences are undefined if any accessor-name is not the name of an accessor for the instance.

* 也见(See Also):

with-slots, symbol-macrolet

* 注意(Notes):

A with-accessors expression of the form:

(with-accessors (slot-entry1 ... slot-entryn) instance-form form1 ... formk)

expands into the equivalent of

(let ((in instance-form))
  (symbol-macrolet (Q1 ... Qn) form1 ... formk))

where Qi is

 (variable-namei () (accessor-namei in))


Macro WITH-SLOTS

* 语法(Syntax):

with-slots (slot-entry*) instance-form declaration* form*

=> result*

slot-entry::= slot-name | (variable-name slot-name) 

* 参数和值(Arguments and Values):

slot-name---a slot name; not evaluated.

variable-name---a variable name; not evaluated.

instance-form---a form; evaluted to produce instance.

instance---an object.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

The macro with-slots establishes a lexical environment for referring to the slots in the instance named by the given slot-names as though they were variables. Within such a context the value of the slot can be specified by using its slot name, as if it were a lexically bound variable. Both setf and setq can be used to set the value of the slot.

The macro with-slots translates an appearance of the slot name as a variable into a call to slot-value.

* 示例(Examples):

 (defclass thing ()
           ((x :initarg :x :accessor thing-x)
            (y :initarg :y :accessor thing-y)))
=>  #<STANDARD-CLASS THING 250020173>
 (defmethod (setf thing-x) :before (new-x (thing thing))
   (format t "~&Changing X from ~D to ~D in ~S.~%"
           (thing-x thing) new-x thing))
 (setq thing (make-instance 'thing :x 0 :y 1)) =>  #<THING 62310540>
 (with-slots (x y) thing (incf x) (incf y)) =>  2
 (values (thing-x thing) (thing-y thing)) =>  1, 2
 (setq thing1 (make-instance 'thing :x 1 :y 2)) =>  #<THING 43135676>
 (setq thing2 (make-instance 'thing :x 7 :y 8)) =>  #<THING 43147374>
 (with-slots ((x1 x) (y1 y))
             thing1
   (with-slots ((x2 x) (y2 y))
               thing2
     (list (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2))
           (setq x1 (+ y1 x2))
           (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2))
           (setf (thing-x thing2) (list x1))
           (list x1 (thing-x thing1) y1 (thing-y thing1)
                 x2 (thing-x thing2) y2 (thing-y thing2)))))
>>  Changing X from 7 to (9) in #<THING 43147374>.
=>  ((1 1 2 2 7 7 8 8)
     9
     (9 9 2 2 7 7 8 8) 
     (9)
     (9 9 2 2 (9) (9) 8 8))

* 受此影响(Affected By):

defclass

* 异常情况(Exceptional Situations):

The consequences are undefined if any slot-name is not the name of a slot in the instance.

* 也见(See Also):

with-accessors, slot-value, symbol-macrolet

* 注意(Notes):

A with-slots expression of the form:

(with-slots (slot-entry1 ... slot-entryn) instance-form form1 ... formk)

expands into the equivalent of

(let ((in instance-form))
  (symbol-macrolet (Q1 ... Qn) form1 ... formk))

where Qi is

(slot-entryi () (slot-value in 'slot-entryi))

if slot-entryi is a symbol and is

(variable-namei () (slot-value in 'slot-namei))

if slot-entryi is of the form

(variable-namei 'slot-namei))


Macro DEFCLASS

* 语法(Syntax):

defclass class-name ({superclass-name}*) ({slot-specifier}*) [[class-option]]

=> new-class

slot-specifier::= slot-name | (slot-name [[slot-option]])
slot-name::= symbol
slot-option::= {:reader reader-function-name}* | 
               {:writer writer-function-name}* | 
               {:accessor reader-function-name}* | 
               {:allocation allocation-type} | 
               {:initarg initarg-name}* | 
               {:initform form} | 
               {:type type-specifier} | 
               {:documentation string} 
function-name::= {symbol | (setf symbol)}
class-option::= (:default-initargs . initarg-list) | 
                (:documentation string) | 
                (:metaclass class-name) 

* 参数和值(Arguments and Values):

Class-name---a non-nil symbol.

Superclass-name--a non-nil symbol.

Slot-name--a symbol. The slot-name argument is a symbol that is syntactically valid for use as a variable name.

Reader-function-name---a non-nil symbol. :reader can be supplied more than once for a given slot.

Writer-function-name---a generic function name. :writer can be supplied more than once for a given slot.

Reader-function-name---a non-nil symbol. :accessor can be supplied more than once for a given slot.

Allocation-type---(member :instance :class). :allocation can be supplied once at most for a given slot.

Initarg-name---a symbol. :initarg can be supplied more than once for a given slot.

Form---a form. :init-form can be supplied once at most for a given slot.

Type-specifier---a type specifier. :type can be supplied once at most for a given slot.

Class-option--- refers to the class as a whole or to all class slots.

Initarg-list---a list of alternating initialization argument names and default initial value forms. :default-initargs can be supplied at most once.

Class-name---a non-nil symbol. :metaclass can be supplied once at most.

new-class---the new class object.

* 描述(Description):

The macro defclass defines a new named class. It returns the new class object as its result.

The syntax of defclass provides options for specifying initialization arguments for slots, for specifying default initialization values for slots, and for requesting that methods on specified generic functions be automatically generated for reading and writing the values of slots. No reader or writer functions are defined by default; their generation must be explicitly requested. However, slots can always be accessed using slot-value.

Defining a new class also causes a type of the same name to be defined. The predicate (typep object class-name) returns true if the class of the given object is the class named by class-name itself or a subclass of the class class-name. A class object can be used as a type specifier. Thus (typep object class) returns true if the class of the object is class itself or a subclass of class.

The class-name argument specifies the proper name of the new class. If a class with the same proper name already exists and that class is an instance of standard-class, and if the defclass form for the definition of the new class specifies a class of class standard-class, the existing class is redefined, and instances of it (and its subclasses) are updated to the new definition at the time that they are next accessed. For details, see Section 4.3.6 (Redefining Classes).

Each superclass-name argument specifies a direct superclass of the new class. If the superclass list is empty, then the superclass defaults depending on the metaclass, with standard-object being the default for standard-class.

The new class will inherit slots and methods from each of its direct superclasses, from their direct superclasses, and so on. For a discussion of how slots and methods are inherited, see Section 4.3.4 (Inheritance).

The following slot options are available:

    The :reader slot option specifies that an unqualified method is to be defined on the generic function named reader-function-name to read the value of the given slot.

    The :writer slot option specifies that an unqualified method is to be defined on the generic function named writer-function-name to write the value of the slot.

    The :accessor slot option specifies that an unqualified method is to be defined on the generic function named reader-function-name to read the value of the given slot and that an unqualified method is to be defined on the generic function named (setf reader-function-name) to be used with setf to modify the value of the slot.

    The :allocation slot option is used to specify where storage is to be allocated for the given slot. Storage for a slot can be located in each instance or in the class object itself. The value of the allocation-type argument can be either the keyword :instance or the keyword :class. If the :allocation slot option is not specified, the effect is the same as specifying :allocation :instance.

        If allocation-type is :instance, a local slot of the name slot-name is allocated in each instance of the class.

        If allocation-type is :class, a shared slot of the given name is allocated in the class object created by this defclass form. The value of the slot is shared by all instances of the class. If a class C1 defines such a shared slot, any subclass C2 of C1 will share this single slot unless the defclass form for C2 specifies a slot of the same name or there is a superclass of C2 that precedes C1 in the class precedence list of C2 and that defines a slot of the same name.

    The :initform slot option is used to provide a default initial value form to be used in the initialization of the slot. This form is evaluated every time it is used to initialize the slot. The lexical environment in which this form is evaluated is the lexical environment in which the defclass form was evaluated. Note that the lexical environment refers both to variables and to functions. For local slots, the dynamic environment is the dynamic environment in which make-instance is called; for shared slots, the dynamic environment is the dynamic environment in which the defclass form was evaluated. See Section 7.1 (Object Creation and Initialization).

    No implementation is permitted to extend the syntax of defclass to allow (slot-name form) as an abbreviation for (slot-name :initform form).

    The :initarg slot option declares an initialization argument named initarg-name and specifies that this initialization argument initializes the given slot. If the initialization argument has a value in the call to initialize-instance, the value will be stored into the given slot, and the slot's :initform slot option, if any, is not evaluated. If none of the initialization arguments specified for a given slot has a value, the slot is initialized according to the :initform slot option, if specified.

    The :type slot option specifies that the contents of the slot will always be of the specified data type. It effectively declares the result type of the reader generic function when applied to an object of this class. The consequences of attempting to store in a slot a value that does not satisfy the type of the slot are undefined. The :type slot option is further discussed in Section 7.5.3 (Inheritance of Slots and Slot Options).

    The :documentation slot option provides a documentation string for the slot. :documentation can be supplied once at most for a given slot.

Each class option is an option that refers to the class as a whole. The following class options are available:

    The :default-initargs class option is followed by a list of alternating initialization argument names and default initial value forms. If any of these initialization arguments does not appear in the initialization argument list supplied to make-instance, the corresponding default initial value form is evaluated, and the initialization argument name and the form's value are added to the end of the initialization argument list before the instance is created; see Section 7.1 (Object Creation and Initialization). The default initial value form is evaluated each time it is used. The lexical environment in which this form is evaluated is the lexical environment in which the defclass form was evaluated. The dynamic environment is the dynamic environment in which make-instance was called. If an initialization argument name appears more than once in a :default-initargs class option, an error is signaled.

    The :documentation class option causes a documentation string to be attached with the class object, and attached with kind type to the class-name. :documentation can be supplied once at most.

    The :metaclass class option is used to specify that instances of the class being defined are to have a different metaclass than the default provided by the system (the class standard-class).

Note the following rules of defclass for standard classes:

    It is not required that the superclasses of a class be defined before the defclass form for that class is evaluated.

    All the superclasses of a class must be defined before an instance of the class can be made.

    A class must be defined before it can be used as a parameter specializer in a defmethod form.

The object system can be extended to cover situations where these rules are not obeyed.

Some slot options are inherited by a class from its superclasses, and some can be shadowed or altered by providing a local slot description. No class options except :default-initargs are inherited. For a detailed description of how slots and slot options are inherited, see Section 7.5.3 (Inheritance of Slots and Slot Options).

The options to defclass can be extended. It is required that all implementations signal an error if they observe a class option or a slot option that is not implemented locally.

It is valid to specify more than one reader, writer, accessor, or initialization argument for a slot. No other slot option can appear more than once in a single slot description, or an error is signaled.

If no reader, writer, or accessor is specified for a slot, the slot can only be accessed by the function slot-value.

If a defclass form appears as a top level form, the compiler must make the class name be recognized as a valid type name in subsequent declarations (as for deftype) and be recognized as a valid class name for defmethod parameter specializers and for use as the :metaclass option of a subsequent defclass. The compiler must make the class definition available to be returned by find-class when its environment argument is a value received as the environment parameter of a macro.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If there are any duplicate slot names, an error of type program-error is signaled.

If an initialization argument name appears more than once in :default-initargs class option, an error of type program-error is signaled.

If any of the following slot options appears more than once in a single slot description, an error of type program-error is signaled: :allocation, :initform, :type, :documentation.

It is required that all implementations signal an error of type program-error if they observe a class option or a slot option that is not implemented locally.

* 也见(See Also):

documentation, initialize-instance, make-instance, slot-value, Section 4.3 (Classes), Section 4.3.4 (Inheritance), Section 4.3.6 (Redefining Classes), Section 4.3.5 (Determining the Class Precedence List), Section 7.1 (Object Creation and Initialization)

* 注意(Notes): None. 


Macro DEFGENERIC

* 语法(Syntax):

defgeneric function-name gf-lambda-list [[option | {method-description}*]]

=> new-generic

option::= (:argument-precedence-order parameter-name+) | 
          (declare gf-declaration+) | 
          (:documentation gf-documentation) | 
          (:method-combination method-combination method-combination-argument*) | 
          (:generic-function-class generic-function-class) | 
          (:method-class method-class) 

method-description::= (:method method-qualifier* specialized-lambda-list [[declaration* | documentation]] form*) 

* 参数和值(Arguments and Values):

function-name---a function name.

generic-function-class---a non-nil symbol naming a class.

gf-declaration---an optimize declaration specifier; other declaration specifiers are not permitted.

gf-documentation---a string; not evaluated.

gf-lambda-list---a generic function lambda list.

method-class---a non-nil symbol naming a class.

method-combination-argument---an object.

method-combination-name---a symbol naming a method combination type.

method-qualifiers, specialized-lambda-list, declarations, documentation, forms---as per defmethod.

new-generic---the generic function object.

parameter-name---a symbol that names a required parameter in the lambda-list. (If the :argument-precedence-order option is specified, each required parameter in the lambda-list must be used exactly once as a parameter-name.)

* 描述(Description):

The macro defgeneric is used to define a generic function or to specify options and declarations that pertain to a generic function as a whole.

If function-name is a list it must be of the form (setf symbol). If (fboundp function-name) is false, a new generic function is created. If (fdefinition function-name) is a generic function, that generic function is modified. If function-name names an ordinary function, a macro, or a special operator, an error is signaled.

The effect of the defgeneric macro is as if the following three steps were performed: first, methods defined by previous defgeneric forms are removed; second, ensure-generic-function is called; and finally, methods specified by the current defgeneric form are added to the generic function.

Each method-description defines a method on the generic function. The lambda list of each method must be congruent with the lambda list specified by the gf-lambda-list option. If no method descriptions are specified and a generic function of the same name does not already exist, a generic function with no methods is created.

The gf-lambda-list argument of defgeneric specifies the shape of lambda lists for the methods on this generic function. All methods on the resulting generic function must have lambda lists that are congruent with this shape. If a defgeneric form is evaluated and some methods for that generic function have lambda lists that are not congruent with that given in the defgeneric form, an error is signaled. For further details on method congruence, see Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

The generic function passes to the method all the argument values passed to it, and only those; default values are not supported. Note that optional and keyword arguments in method definitions, however, can have default initial value forms and can use supplied-p parameters.

The following options are provided. Except as otherwise noted, a given option may occur only once.

    The :argument-precedence-order option is used to specify the order in which the required arguments in a call to the generic function are tested for specificity when selecting a particular method. Each required argument, as specified in the gf-lambda-list argument, must be included exactly once as a parameter-name so that the full and unambiguous precedence order is supplied. If this condition is not met, an error is signaled.

    The declare option is used to specify declarations that pertain to the generic function.

    An optimize declaration specifier is allowed. It specifies whether method selection should be optimized for speed or space, but it has no effect on methods. To control how a method is optimized, an optimize declaration must be placed directly in the defmethod form or method description. The optimization qualities speed and space are the only qualities this standard requires, but an implementation can extend the object system to recognize other qualities. A simple implementation that has only one method selection technique and ignores optimize declaration specifiers is valid.

    The special, ftype, function, inline, notinline, and declaration declarations are not permitted. Individual implementations can extend the declare option to support additional declarations. If an implementation notices a declaration specifier that it does not support and that has not been proclaimed as a non-standard declaration identifier name in a declaration proclamation, it should issue a warning.

    The declare option may be specified more than once. The effect is the same as if the lists of declaration specifiers had been appended together into a single list and specified as a single declare option.

    The :documentation argument is a documentation string to be attached to the generic function object, and to be attached with kind function to the function-name.

    The :generic-function-class option may be used to specify that the generic function is to have a different class than the default provided by the system (the class standard-generic-function). The class-name argument is the name of a class that can be the class of a generic function. If function-name specifies an existing generic function that has a different value for the :generic-function-class argument and the new generic function class is compatible with the old, change-class is called to change the class of the generic function; otherwise an error is signaled.

    The :method-class option is used to specify that all methods on this generic function are to have a different class from the default provided by the system (the class standard-method). The class-name argument is the name of a class that is capable of being the class of a method.

    The :method-combination option is followed by a symbol that names a type of method combination. The arguments (if any) that follow that symbol depend on the type of method combination. Note that the standard method combination type does not support any arguments. However, all types of method combination defined by the short form of define-method-combination accept an optional argument named order, defaulting to :most-specific-first, where a value of :most-specific-last reverses the order of the primary methods without affecting the order of the auxiliary methods.

The method-description arguments define methods that will be associated with the generic function. The method-qualifier and specialized-lambda-list arguments in a method description are the same as for defmethod.

The form arguments specify the method body. The body of the method is enclosed in an implicit block. If function-name is a symbol, this block bears the same name as the generic function. If function-name is a list of the form (setf symbol), the name of the block is symbol.

Implementations can extend defgeneric to include other options. It is required that an implementation signal an error if it observes an option that is not implemented locally.

defgeneric is not required to perform any compile-time side effects. In particular, the methods are not installed for invocation during compilation. An implementation may choose to store information about the generic function for the purposes of compile-time error-checking (such as checking the number of arguments on calls, or noting that a definition for the function name has been seen).

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If function-name names an ordinary function, a macro, or a special operator, an error of type program-error is signaled.

Each required argument, as specified in the gf-lambda-list argument, must be included exactly once as a parameter-name, or an error of type program-error is signaled.

The lambda list of each method specified by a method-description must be congruent with the lambda list specified by the gf-lambda-list option, or an error of type error is signaled.

If a defgeneric form is evaluated and some methods for that generic function have lambda lists that are not congruent with that given in the defgeneric form, an error of type error is signaled.

A given option may occur only once, or an error of type program-error is signaled.

If function-name specifies an existing generic function that has a different value for the :generic-function-class argument and the new generic function class is compatible with the old, change-class is called to change the class of the generic function; otherwise an error of type error is signaled.

Implementations can extend defgeneric to include other options. It is required that an implementation signal an error of type program-error if it observes an option that is not implemented locally.

* 也见(See Also):

defmethod, documentation, ensure-generic-function, generic-function, Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function)

* 注意(Notes): None. 


Macro DEFMETHOD

* 语法(Syntax):

defmethod function-name {method-qualifier}* specialized-lambda-list [[declaration* | documentation]] form*

=> new-method

function-name::= {symbol | (setf symbol)}

method-qualifier::= non-list

specialized-lambda-list::= ({var | (var parameter-specializer-name)}* 
                            [&optional {var | (var [initform [supplied-p-parameter] ])}*] 
                            [&rest var] 
                            [&key{var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
                                 [&allow-other-keys] ] 
                            [&aux {var | (var [initform] )}*] ) 
parameter-specializer-name::= symbol | (eql eql-specializer-form)

* 参数和值(Arguments and Values):

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

var---a variable name.

eql-specializer-form---a form.

Form---a form.

Initform---a form.

Supplied-p-parameter---variable name.

new-method---the new method object.

* 描述(Description):

The macro defmethod defines a method on a generic function.

If (fboundp function-name) is nil, a generic function is created with default values for the argument precedence order (each argument is more specific than the arguments to its right in the argument list), for the generic function class (the class standard-generic-function), for the method class (the class standard-method), and for the method combination type (the standard method combination type). The lambda list of the generic function is congruent with the lambda list of the method being defined; if the defmethod form mentions keyword arguments, the lambda list of the generic function will mention ..... key (but no keyword arguments). If function-name names an ordinary function, a macro, or a special operator, an error is signaled.

If a generic function is currently named by function-name, the lambda list of the method must be congruent with the lambda list of the generic function. If this condition does not hold, an error is signaled. For a definition of congruence in this context, see Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

Each method-qualifier argument is an object that is used by method combination to identify the given method. The method combination type might further restrict what a method qualifier can be. The standard method combination type allows for unqualified methods and methods whose sole qualifier is one of the keywords :before, :after, or :around.

The specialized-lambda-list argument is like an ordinary lambda list except that the names of required parameters can be replaced by specialized parameters. A specialized parameter is a list of the form (var parameter-specializer-name). Only required parameters can be specialized. If parameter-specializer-name is a symbol it names a class; if it is a list, it is of the form (eql eql-specializer-form). The parameter specializer name (eql eql-specializer-form) indicates that the corresponding argument must be eql to the object that is the value of eql-specializer-form for the method to be applicable. The eql-specializer-form is evaluated at the time that the expansion of the defmethod macro is evaluated. If no parameter specializer name is specified for a given required parameter, the parameter specializer defaults to the class t. For further discussion, see Section 7.6.2 (Introduction to Methods).

The form arguments specify the method body. The body of the method is enclosed in an implicit block. If function-name is a symbol, this block bears the same name as the generic function. If function-name is a list of the form (setf symbol), the name of the block is symbol.

The class of the method object that is created is that given by the method class option of the generic function on which the method is defined.

If the generic function already has a method that agrees with the method being defined on parameter specializers and qualifiers, defmethod replaces the existing method with the one now being defined. For a definition of agreement in this context. see Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

The parameter specializers are derived from the parameter specializer names as described in Section 7.6.2 (Introduction to Methods).

The expansion of the defmethod macro ``refers to'' each specialized parameter (see the description of ignore within the description of declare). This includes parameters that have an explicit parameter specializer name of t. This means that a compiler warning does not occur if the body of the method does not refer to a specialized parameter, while a warning might occur if the body of the method does not refer to an unspecialized parameter. For this reason, a parameter that specializes on t is not quite synonymous with an unspecialized parameter in this context.

Declarations at the head of the method body that apply to the method's lambda variables are treated as bound declarations whose scope is the same as the corresponding bindings.

Declarations at the head of the method body that apply to the functional bindings of call-next-method or next-method-p apply to references to those functions within the method body forms. Any outer bindings of the function names call-next-method and next-method-p, and declarations associated with such bindings are shadowed[2] within the method body forms.

The scope of free declarations at the head of the method body is the entire method body, which includes any implicit local function definitions but excludes initialization forms for the lambda variables.

defmethod is not required to perform any compile-time side effects. In particular, the methods are not installed for invocation during compilation. An implementation may choose to store information about the generic function for the purposes of compile-time error-checking (such as checking the number of arguments on calls, or noting that a definition for the function name has been seen).

Documentation is attached as a documentation string to the method object.

* 示例(Examples): None.

* 受此影响(Affected By):

The definition of the referenced generic function.

* 异常情况(Exceptional Situations):

If function-name names an ordinary function, a macro, or a special operator, an error of type error is signaled.

If a generic function is currently named by function-name, the lambda list of the method must be congruent with the lambda list of the generic function, or an error of type error is signaled.

* 也见(See Also):

defgeneric, documentation, Section 7.6.2 (Introduction to Methods), Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function), Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes): None. 


Accessor FIND-CLASS

* 语法(Syntax):

find-class symbol &optional errorp environment => class

(setf (find-class symbol &optional errorp environment) new-class)

* 参数和值(Arguments and Values):

symbol---a symbol.

errorp---a generalized boolean. The default is true.

environment -- same as the &environment argument to macro expansion functions and is used to distinguish between compile-time and run-time environments. The &environment argument has dynamic extent; the consequences are undefined if the &environment argument is referred to outside the dynamic extent of the macro expansion function.

class---a class object, or nil.

* 描述(Description):

Returns the class object named by the symbol in the environment. If there is no such class, nil is returned if errorp is false; otherwise, if errorp is true, an error is signaled.

The class associated with a particular symbol can be changed by using setf with find-class; or, if the new class given to setf is nil, the class association is removed (but the class object itself is not affected). The results are undefined if the user attempts to change or remove the class associated with a symbol that is defined as a type specifier in this standard. See Section 4.3.7 (Integrating Types and Classes).

When using setf of find-class, any errorp argument is evaluated for effect, but any values it returns are ignored; the errorp parameter is permitted primarily so that the environment parameter can be used.

The environment might be used to distinguish between a compile-time and a run-time environment.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

If there is no such class and errorp is true, find-class signals an error of type error.

* 也见(See Also):

defmacro, Section 4.3.7 (Integrating Types and Classes)

* 注意(Notes): None. 


Local Function NEXT-METHOD-P

* 语法(Syntax):

next-method-p <no arguments> => generalized-boolean

* 参数和值(Arguments and Values):

generalized-boolean---a generalized boolean.

* 描述(Description):

The locally defined function next-method-p can be used within the body forms (but not the lambda list) defined by a method-defining form to determine whether a next method exists.

The function next-method-p has lexical scope and indefinite extent.

Whether or not next-method-p is fbound in the global environment is implementation-dependent; however, the restrictions on redefinition and shadowing of next-method-p are the same as for symbols in the COMMON-LISP package which are fbound in the global environment. The consequences of attempting to use next-method-p outside of a method-defining form are undefined.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

call-next-method, defmethod, call-method

* 注意(Notes): None. 


Local Macro CALL-METHOD, MAKE-METHOD

* 语法(Syntax):

call-method method &optional next-method-list => result*

make-method form => method-object

* 参数和值(Arguments and Values):

method---a method object, or a list (see below); not evaluated.

method-object---a method object.

next-method-list---a list of method objects; not evaluated.

results---the values returned by the method invocation.

* 描述(Description):

The macro call-method is used in method combination. It hides the implementation-dependent details of how methods are called. The macro call-method has lexical scope and can only be used within an effective method form.

Whether or not call-method is fbound in the global environment is implementation-dependent; however, the restrictions on redefinition and shadowing of call-method are the same as for symbols in the COMMON-LISP package which are fbound in the global environment. The consequences of attempting to use call-method outside of an effective method form are undefined.

The macro call-method invokes the specified method, supplying it with arguments and with definitions for call-next-method and for next-method-p. If the invocation of call-method is lexically inside of a make-method, the arguments are those that were supplied to that method. Otherwise the arguments are those that were supplied to the generic function. The definitions of call-next-method and next-method-p rely on the specified next-method-list.

If method is a list, the first element of the list must be the symbol make-method and the second element must be a form. Such a list specifies a method object whose method function has a body that is the given form.

Next-method-list can contain method objects or lists, the first element of which must be the symbol make-method and the second element of which must be a form.

Those are the only two places where make-method can be used. The form used with make-method is evaluated in the null lexical environment augmented with a local macro definition for call-method and with bindings named by symbols not accessible from the COMMON-LISP-USER package.

The call-next-method function available to method will call the first method in next-method-list. The call-next-method function available in that method, in turn, will call the second method in next-method-list, and so on, until the list of next methods is exhausted.

If next-method-list is not supplied, the call-next-method function available to method signals an error of type control-error and the next-method-p function available to method returns nil.

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

call-next-method, define-method-combination, next-method-p

* 注意(Notes): None. 


Local Function CALL-NEXT-METHOD

* 语法(Syntax):

call-next-method &rest args => result*

* 参数和值(Arguments and Values):

arg---an object.

results---the values returned by the method it calls.

* 描述(Description):

The function call-next-method can be used within the body forms (but not the lambda list) of a method defined by a method-defining form to call the next method.

If there is no next method, the generic function no-next-method is called.

The type of method combination used determines which methods can invoke call-next-method. The standard method combination type allows call-next-method to be used within primary methods and around methods. For generic functions using a type of method combination defined by the short form of define-method-combination, call-next-method can be used in around methods only.

When call-next-method is called with no arguments, it passes the current method's original arguments to the next method. Neither argument defaulting, nor using setq, nor rebinding variables with the same names as parameters of the method affects the values call-next-method passes to the method it calls.

When call-next-method is called with arguments, the next method is called with those arguments.

If call-next-method is called with arguments but omits optional arguments, the next method called defaults those arguments.

The function call-next-method returns any values that are returned by the next method.

The function call-next-method has lexical scope and indefinite extent and can only be used within the body of a method defined by a method-defining form.

Whether or not call-next-method is fbound in the global environment is implementation-dependent; however, the restrictions on redefinition and shadowing of call-next-method are the same as for symbols in the COMMON-LISP package which are fbound in the global environment. The consequences of attempting to use call-next-method outside of a method-defining form are undefined.

* 示例(Examples): None.

* 受此影响(Affected By):

defmethod, call-method, define-method-combination.

* 异常情况(Exceptional Situations):

When providing arguments to call-next-method, the following rule must be satisfied or an error of type error should be signaled: the ordered set of applicable methods for a changed set of arguments for call-next-method must be the same as the ordered set of applicable methods for the original arguments to the generic function. Optimizations of the error checking are possible, but they must not change the semantics of call-next-method.

* 也见(See Also):

define-method-combination, defmethod, next-method-p, no-next-method, call-method, Section 7.6.6 (Method Selection and Combination), Section 7.6.6.2 (Standard Method Combination), Section 7.6.6.4 (Built-in Method Combination Types)

* 注意(Notes): None. 


Standard Generic Function COMPUTE-APPLICABLE-METHODS

* 语法(Syntax):

compute-applicable-methods generic-function function-arguments => methods

* 方法签名(Method Signatures):

compute-applicable-methods (generic-function standard-generic-function)

* 参数和值(Arguments and Values):

generic-function---a generic function.

function-arguments---a list of arguments for the generic-function.

methods---a list of method objects.

* 描述(Description):

Given a generic-function and a set of function-arguments, the function compute-applicable-methods returns the set of methods that are applicable for those arguments sorted according to precedence order. See Section 7.6.6 (Method Selection and Combination).

* 受此影响(Affected By):

defmethod

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

Section 7.6.6 (Method Selection and Combination)

* 注意(Notes): None. 


Macro DEFINE-METHOD-COMBINATION

* 语法(Syntax):

define-method-combination name [[short-form-option]]

=> name

define-method-combination name lambda-list (method-group-specifier*) [(:arguments . args-lambda-list)] [(:generic-function generic-function-symbol)] [[declaration* | documentation]] form*

=> name

short-form-option::= :documentation documentation |  
                     :identity-with-one-argument identity-with-one-argument | 
                     :operator operator 

method-group-specifier::= (name {qualifier-pattern+ | predicate} [[long-form-option]]) 

long-form-option::= :description description | 
                    :order order | 
                    :required required-p 

* 参数和值(Arguments and Values):

args-lambda-list---a define-method-combination arguments lambda list.

declaration---a declare expression; not evaluated.

description---a format control.

documentation---a string; not evaluated.

forms---an implicit progn that must compute and return the form that specifies how the methods are combined, that is, the effective method.

generic-function-symbol---a symbol.

identity-with-one-argument---a generalized boolean.

lambda-list---ordinary lambda list.

name---a symbol. Non-keyword, non-nil symbols are usually used.

operator---an operator. Name and operator are often the same symbol. This is the default, but it is not required.

order---:most-specific-first or :most-specific-last; evaluated.

predicate---a symbol that names a function of one argument that returns a generalized boolean.

qualifier-pattern---a list, or the symbol *.

required-p---a generalized boolean.

* 描述(Description):

The macro define-method-combination is used to define new types of method combination.

There are two forms of define-method-combination. The short form is a simple facility for the cases that are expected to be most commonly needed. The long form is more powerful but more verbose. It resembles defmacro in that the body is an expression, usually using backquote, that computes a form. Thus arbitrary control structures can be implemented. The long form also allows arbitrary processing of method qualifiers.

Short Form

    The short form syntax of define-method-combination is recognized when the second subform is a non-nil symbol or is not present. When the short form is used, name is defined as a type of method combination that produces a Lisp form (operator method-call method-call ...). The operator is a symbol that can be the name of a function, macro, or special operator. The operator can be supplied by a keyword option; it defaults to name.

    Keyword options for the short form are the following:

        The :documentation option is used to document the method-combination type; see description of long form below.

        The :identity-with-one-argument option enables an optimization when its value is true (the default is false). If there is exactly one applicable method and it is a primary method, that method serves as the effective method and operator is not called. This optimization avoids the need to create a new effective method and avoids the overhead of a function call. This option is designed to be used with operators such as progn, and, +, and max.

        The :operator option specifies the name of the operator. The operator argument is a symbol that can be the name of a function, macro, or special form.

    These types of method combination require exactly one qualifier per method. An error is signaled if there are applicable methods with no qualifiers or with qualifiers that are not supported by the method combination type.

    A method combination procedure defined in this way recognizes two roles for methods. A method whose one qualifier is the symbol naming this type of method combination is defined to be a primary method. At least one primary method must be applicable or an error is signaled. A method with :around as its one qualifier is an auxiliary method that behaves the same as an around method in standard method combination. The function call-next-method can only be used in around methods; it cannot be used in primary methods defined by the short form of the define-method-combination macro.

    A method combination procedure defined in this way accepts an optional argument named order, which defaults to :most-specific-first. A value of :most-specific-last reverses the order of the primary methods without affecting the order of the auxiliary methods.

    The short form automatically includes error checking and support for around methods.

    For a discussion of built-in method combination types, see Section 7.6.6.4 (Built-in Method Combination Types).

Long Form

    The long form syntax of define-method-combination is recognized when the second subform is a list.

    The lambda-list receives any arguments provided after the name of the method combination type in the :method-combination option to defgeneric.

    A list of method group specifiers follows. Each specifier selects a subset of the applicable methods to play a particular role, either by matching their qualifiers against some patterns or by testing their qualifiers with a predicate. These method group specifiers define all method qualifiers that can be used with this type of method combination.

    The car of each method-group-specifier is a symbol which names a variable. During the execution of the forms in the body of define-method-combination, this variable is bound to a list of the methods in the method group. The methods in this list occur in the order specified by the :order option.

    If qualifier-pattern is a symbol it must be *. A method matches a qualifier-pattern if the method's list of qualifiers is equal to the qualifier-pattern (except that the symbol * in a qualifier-pattern matches anything). Thus a qualifier-pattern can be one of the following: the empty list, which matches unqualified methods; the symbol *, which matches all methods; a true list, which matches methods with the same number of qualifiers as the length of the list when each qualifier matches the corresponding list element; or a dotted list that ends in the symbol * (the * matches any number of additional qualifiers).

    Each applicable method is tested against the qualifier-patterns and predicates in left-to-right order. As soon as a qualifier-pattern matches or a predicate returns true, the method becomes a member of the corresponding method group and no further tests are made. Thus if a method could be a member of more than one method group, it joins only the first such group. If a method group has more than one qualifier-pattern, a method need only satisfy one of the qualifier-patterns to be a member of the group.

    The name of a predicate function can appear instead of qualifier-patterns in a method group specifier. The predicate is called for each method that has not been assigned to an earlier method group; it is called with one argument, the method's qualifier list. The predicate should return true if the method is to be a member of the method group. A predicate can be distinguished from a qualifier-pattern because it is a symbol other than nil or *.

    If there is an applicable method that does not fall into any method group, the function invalid-method-error is called.

    Method group specifiers can have keyword options following the qualifier patterns or predicate. Keyword options can be distinguished from additional qualifier patterns because they are neither lists nor the symbol *. The keyword options are as follows:

        The :description option is used to provide a description of the role of methods in the method group. Programming environment tools use (apply #'format stream format-control (method-qualifiers method)) to print this description, which is expected to be concise. This keyword option allows the description of a method qualifier to be defined in the same module that defines the meaning of the method qualifier. In most cases, format-control will not contain any format directives, but they are available for generality. If :description is not supplied, a default description is generated based on the variable name and the qualifier patterns and on whether this method group includes the unqualified methods.

        The :order option specifies the order of methods. The order argument is a form that evaluates to :most-specific-first or :most-specific-last. If it evaluates to any other value, an error is signaled. If :order is not supplied, it defaults to :most-specific-first.

        The :required option specifies whether at least one method in this method group is required. If its value is true and the method group is empty (that is, no applicable methods match the qualifier patterns or satisfy the predicate), an error is signaled. If :required is not supplied, it defaults to nil.

    The use of method group specifiers provides a convenient syntax to select methods, to divide them among the possible roles, and to perform the necessary error checking. It is possible to perform further filtering of methods in the body forms by using normal list-processing operations and the functions method-qualifiers and invalid-method-error. It is permissible to use setq on the variables named in the method group specifiers and to bind additional variables. It is also possible to bypass the method group specifier mechanism and do everything in the body forms. This is accomplished by writing a single method group with * as its only qualifier-pattern; the variable is then bound to a list of all of the applicable methods, in most-specific-first order.

    The body forms compute and return the form that specifies how the methods are combined, that is, the effective method. The effective method is evaluated in the null lexical environment augmented with a local macro definition for call-method and with bindings named by symbols not accessible from the COMMON-LISP-USER package. Given a method object in one of the lists produced by the method group specifiers and a list of next methods, call-method will invoke the method such that call-next-method has available the next methods.

    When an effective method has no effect other than to call a single method, some implementations employ an optimization that uses the single method directly as the effective method, thus avoiding the need to create a new effective method. This optimization is active when the effective method form consists entirely of an invocation of the call-method macro whose first subform is a method object and whose second subform is nil or unsupplied. Each define-method-combination body is responsible for stripping off redundant invocations of progn, and, multiple-value-prog1, and the like, if this optimization is desired.

    The list (:arguments . lambda-list) can appear before any declarations or documentation string. This form is useful when the method combination type performs some specific behavior as part of the combined method and that behavior needs access to the arguments to the generic function. Each parameter variable defined by lambda-list is bound to a form that can be inserted into the effective method. When this form is evaluated during execution of the effective method, its value is the corresponding argument to the generic function; the consequences of using such a form as the place in a setf form are undefined. Argument correspondence is computed by dividing the :arguments lambda-list and the generic function lambda-list into three sections: the required parameters, the optional parameters, and the keyword and rest parameters. The arguments supplied to the generic function for a particular call are also divided into three sections; the required arguments section contains as many arguments as the generic function has required parameters, the optional arguments section contains as many arguments as the generic function has optional parameters, and the keyword/rest arguments section contains the remaining arguments. Each parameter in the required and optional sections of the :arguments lambda-list accesses the argument at the same position in the corresponding section of the arguments. If the section of the :arguments lambda-list is shorter, extra arguments are ignored. If the section of the :arguments lambda-list is longer, excess required parameters are bound to forms that evaluate to nil and excess optional parameters are bound to their initforms. The keyword parameters and rest parameters in the :arguments lambda-list access the keyword/rest section of the arguments. If the :arguments lambda-list contains &key, it behaves as if it also contained &allow-other-keys.

    In addition, &whole var can be placed first in the :arguments lambda-list. It causes var to be bound to a form that evaluates to a list of all of the arguments supplied to the generic function. This is different from &rest because it accesses all of the arguments, not just the keyword/rest arguments.

    Erroneous conditions detected by the body should be reported with method-combination-error or invalid-method-error; these functions add any necessary contextual information to the error message and will signal the appropriate error.

    The body forms are evaluated inside of the bindings created by the lambda list and method group specifiers. Declarations at the head of the body are positioned directly inside of bindings created by the lambda list and outside of the bindings of the method group variables. Thus method group variables cannot be declared in this way. locally may be used around the body, however.

    Within the body forms, generic-function-symbol is bound to the generic function object.

    Documentation is attached as a documentation string to name (as kind method-combination) and to the method combination object.

    Note that two methods with identical specializers, but with different qualifiers, are not ordered by the algorithm described in Step 2 of the method selection and combination process described in Section 7.6.6 (Method Selection and Combination). Normally the two methods play different roles in the effective method because they have different qualifiers, and no matter how they are ordered in the result of Step 2, the effective method is the same. If the two methods play the same role and their order matters, an error is signaled. This happens as part of the qualifier pattern matching in define-method-combination.

If a define-method-combination form appears as a top level form, the compiler must make the method combination name be recognized as a valid method combination name in subsequent defgeneric forms. However, the method combination is executed no earlier than when the define-method-combination form is executed, and possibly as late as the time that generic functions that use the method combination are executed.

* 示例(Examples):

Most examples of the long form of define-method-combination also illustrate the use of the related functions that are provided as part of the declarative method combination facility.

;;; Examples of the short form of define-method-combination
 
 (define-method-combination and :identity-with-one-argument t) 
  
 (defmethod func and ((x class1) y) ...)
 
;;; The equivalent of this example in the long form is:
 
 (define-method-combination and 
         (&optional (order :most-specific-first))
         ((around (:around))
          (primary (and) :order order :required t))
   (let ((form (if (rest primary)
                   `(and ,@(mapcar #'(lambda (method)
                                       `(call-method ,method))
                                   primary))
                   `(call-method ,(first primary)))))
     (if around
         `(call-method ,(first around)
                       (,@(rest around)
                        (make-method ,form)))
         form)))
  
;;; Examples of the long form of define-method-combination
 
;The default method-combination technique
 (define-method-combination standard ()
         ((around (:around))
          (before (:before))
          (primary () :required t)
          (after (:after)))
   (flet ((call-methods (methods)
            (mapcar #'(lambda (method)
                        `(call-method ,method))
                    methods)))
     (let ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                        (progn ,@(call-methods before)
                               (call-method ,(first primary)
                                            ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary)))))
       (if around
           `(call-method ,(first around)
                         (,@(rest around)
                          (make-method ,form)))
           form))))
  
;A simple way to try several methods until one returns non-nil
 (define-method-combination or ()
         ((methods (or)))
   `(or ,@(mapcar #'(lambda (method)
                      `(call-method ,method))
                  methods)))
  
;A more complete version of the preceding
 (define-method-combination or 
         (&optional (order ':most-specific-first))
         ((around (:around))
          (primary (or)))
   ;; Process the order argument
   (case order
     (:most-specific-first)
     (:most-specific-last (setq primary (reverse primary)))
     (otherwise (method-combination-error "~S is an invalid order.~@
     :most-specific-first and :most-specific-last are the possible values."
                                          order)))
   ;; Must have a primary method
   (unless primary
     (method-combination-error "A primary method is required."))
   ;; Construct the form that calls the primary methods
   (let ((form (if (rest primary)
                   `(or ,@(mapcar #'(lambda (method)
                                      `(call-method ,method))
                                  primary))
                   `(call-method ,(first primary)))))
     ;; Wrap the around methods around that form
     (if around
         `(call-method ,(first around)
                       (,@(rest around)
                        (make-method ,form)))
         form)))
  
;The same thing, using the :order and :required keyword options
 (define-method-combination or 
         (&optional (order ':most-specific-first))
         ((around (:around))
          (primary (or) :order order :required t))
   (let ((form (if (rest primary)
                   `(or ,@(mapcar #'(lambda (method)
                                      `(call-method ,method))
                                  primary))
                   `(call-method ,(first primary)))))
     (if around
         `(call-method ,(first around)
                       (,@(rest around)
                        (make-method ,form)))
         form)))
  
;This short-form call is behaviorally identical to the preceding
 (define-method-combination or :identity-with-one-argument t)
 
;Order methods by positive integer qualifiers
;:around methods are disallowed to keep the example small
 (define-method-combination example-method-combination ()
         ((methods positive-integer-qualifier-p))
   `(progn ,@(mapcar #'(lambda (method)
                         `(call-method ,method))
                     (stable-sort methods #'<
                       :key #'(lambda (method)
                                (first (method-qualifiers method)))))))
 
 (defun positive-integer-qualifier-p (method-qualifiers)
   (and (= (length method-qualifiers) 1)
        (typep (first method-qualifiers) '(integer 0 *))))
  
;;; Example of the use of :arguments
 (define-method-combination progn-with-lock ()
         ((methods ()))
   (:arguments object)
   `(unwind-protect
        (progn (lock (object-lock ,object))
               ,@(mapcar #'(lambda (method)
                             `(call-method ,method))
                         methods))
      (unlock (object-lock ,object))))
  

* 受此影响(Affected By): None.

Side Effects:

The compiler is not required to perform any compile-time side-effects.

* 异常情况(Exceptional Situations):

Method combination types defined with the short form require exactly one qualifier per method. An error of type error is signaled if there are applicable methods with no qualifiers or with qualifiers that are not supported by the method combination type. At least one primary method must be applicable or an error of type error is signaled.

If an applicable method does not fall into any method group, the system signals an error of type error indicating that the method is invalid for the kind of method combination in use.

If the value of the :required option is true and the method group is empty (that is, no applicable methods match the qualifier patterns or satisfy the predicate), an error of type error is signaled.

If the :order option evaluates to a value other than :most-specific-first or :most-specific-last, an error of type error is signaled.

* 也见(See Also):

call-method, call-next-method, documentation, method-qualifiers, method-combination-error, invalid-method-error, defgeneric, Section 7.6.6 (Method Selection and Combination), Section 7.6.6.4 (Built-in Method Combination Types), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

The :method-combination option of defgeneric is used to specify that a generic function should use a particular method combination type. The first argument to the :method-combination option is the name of a method combination type and the remaining arguments are options for that type. 


Standard Generic Function FIND-METHOD

* 语法(Syntax):

find-method generic-function method-qualifiers specializers &optional errorp

=> method

* 方法签名(Method Signatures):

find-method (generic-function standard-generic-function) method-qualifiers specializers &optional errorp

* 参数和值(Arguments and Values):

generic-function---a generic function.

method-qualifiers---a list.

specializers---a list.

errorp---a generalized boolean. The default is true.

method---a method object, or nil.

* 描述(Description):

The generic function find-method takes a generic function and returns the method object that agrees on qualifiers and parameter specializers with the method-qualifiers and specializers arguments of find-method. Method-qualifiers contains the method qualifiers for the method. The order of the method qualifiers is significant. For a definition of agreement in this context, see Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

The specializers argument contains the parameter specializers for the method. It must correspond in length to the number of required arguments of the generic function, or an error is signaled. This means that to obtain the default method on a given generic-function, a list whose elements are the class t must be given.

If there is no such method and errorp is true, find-method signals an error. If there is no such method and errorp is false, find-method returns nil.

* 示例(Examples):

 (defmethod some-operation ((a integer) (b float)) (list a b))
=>  #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
 (find-method #'some-operation '() (mapcar #'find-class '(integer float)))
=>  #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
 (find-method #'some-operation '() (mapcar #'find-class '(integer integer)))
>>  Error: No matching method
 (find-method #'some-operation '() (mapcar #'find-class '(integer integer)) nil)
=>  NIL

* 受此影响(Affected By):

add-method, defclass, defgeneric, defmethod

* 异常情况(Exceptional Situations):

If the specializers argument does not correspond in length to the number of required arguments of the generic-function, an an error of type error is signaled.

If there is no such method and errorp is true, find-method signals an error of type error.

* 也见(See Also):

Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers)

* 注意(Notes): None. 


Standard Generic Function ADD-METHOD

* 语法(Syntax):

add-method generic-function method => generic-function

* 方法签名(Method Signatures):

add-method (generic-function standard-generic-function) (method method)

* 参数和值(Arguments and Values):

generic-function---a generic function object.

method---a method object.

* 描述(Description):

The generic function add-method adds a method to a generic function.

If method agrees with an existing method of generic-function on parameter specializers and qualifiers, the existing method is replaced.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The lambda list of the method function of method must be congruent with the lambda list of generic-function, or an error of type error is signaled.

If method is a method object of another generic function, an error of type error is signaled.

* 也见(See Also):

defmethod, defgeneric, find-method, remove-method, Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers)

* 注意(Notes): None. 


Standard Generic Function INITIALIZE-INSTANCE

* 语法(Syntax):

initialize-instance instance &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

initialize-instance (instance standard-object) &rest initargs

* 参数和值(Arguments and Values):

instance---an object.

initargs---a defaulted initialization argument list.

* 描述(Description):

Called by make-instance to initialize a newly created instance. The generic function is called with the new instance and the defaulted initialization argument list.

The system-supplied primary method on initialize-instance initializes the slots of the instance with values according to the initargs and the :initform forms of the slots. It does this by calling the generic function shared-initialize with the following arguments: the instance, t (this indicates that all slots for which no initialization arguments are provided should be initialized according to their :initform forms), and the initargs.

Programmers can define methods for initialize-instance to specify actions to be taken when an instance is initialized. If only after methods are defined, they will be run after the system-supplied primary method for initialization and therefore will not interfere with the default behavior of initialize-instance.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

shared-initialize, make-instance, slot-boundp, slot-makunbound, Section 7.1 (Object Creation and Initialization), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes): None. 


Standard Generic Function CLASS-NAME

* 语法(Syntax):

class-name class => name

* 方法签名(Method Signatures):

class-name (class class)

* 参数和值(Arguments and Values):

class---a class object.

name---a symbol.

* 描述(Description):

Returns the name of the given class.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

find-class, Section 4.3 (Classes)

* 注意(Notes):

If S is a symbol such that S =(class-name C) and C =(find-class S), then S is the proper name of C. For further discussion, see Section 4.3 (Classes).

The name of an anonymous class is nil. 


Standard Generic Function (SETF CLASS-NAME)

* 语法(Syntax):

(setf class-name) new-value class => new-value

* 方法签名(Method Signatures):

(setf class-name) new-value (class class)

* 参数和值(Arguments and Values):

new-value---a symbol.

class---a class.

* 描述(Description):

The generic function (setf class-name) sets the name of a class object.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

find-class, proper name, Section 4.3 (Classes)

* 注意(Notes): None. 


Function CLASS-OF

* 语法(Syntax):

class-of object => class

* 参数和值(Arguments and Values):

object---an object.

class---a class object.

* 描述(Description):

Returns the class of which the object is a direct instance.

* 示例(Examples):

 (class-of 'fred) =>  #<BUILT-IN-CLASS SYMBOL 610327300>
 (class-of 2/3) =>  #<BUILT-IN-CLASS RATIO 610326642>
 
 (defclass book () ()) =>  #<STANDARD-CLASS BOOK 33424745>
 (class-of (make-instance 'book)) =>  #<STANDARD-CLASS BOOK 33424745>
 
 (defclass novel (book) ()) =>  #<STANDARD-CLASS NOVEL 33424764>
 (class-of (make-instance 'novel)) =>  #<STANDARD-CLASS NOVEL 33424764>

 (defstruct kons kar kdr) =>  KONS
 (class-of (make-kons :kar 3 :kdr 4)) =>  #<STRUCTURE-CLASS KONS 250020317>

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-instance, type-of

* 注意(Notes): None. 


Condition Type UNBOUND-SLOT

Class Precedence List:

unbound-slot, cell-error, error, serious-condition, condition, t

* 描述(Description):

The object having the unbound slot is initialized by the :instanceinitialization argument to make-condition, and is accessed by the function unbound-slot-instance.

The name of the cell (see cell-error) is the name of the slot.

* 也见(See Also):

cell-error-name, unbound-slot-object, Section 9.1 (Condition System Concepts) 


Function UNBOUND-SLOT-INSTANCE

* 语法(Syntax):

unbound-slot-instance condition => instance

* 参数和值(Arguments and Values):

condition---a condition of type unbound-slot.

instance---an object.

* 描述(Description):

Returns the instance which had the unbound slot in the situation represented by the condition.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

cell-error-name, unbound-slot, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


