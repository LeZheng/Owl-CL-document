# 7. Objects

> * 7.1 [对象创建和初始化](#ObjectCreationInit)
> * 7.2 [修改一个实例的类](#ChangeClassInstance)
> * 7.3 [重新初始化一个实例](#ReinitInstance)
> * 7.4 [元对象](#MetaObjects)
> * 7.5 [槽](#Slots)
> * 7.6 [广义函数和方法](#GenericFunctionsMethods)
> * 7.7 [对象字典](#TheObjectsDictionary)

## 7.1 <span id="ObjectCreationInit">对象创建和初始化</span>

广义函数[generic function] make-instance 创建并且返回一个类[class]的实例[instance]. 第一个参数是一个类[class]或者一个类[class]的名字[name], 而剩余参数组成初始化参数列表[initialization argument list].

一个新的实例[instance]的初始化由多个独立不同的步骤组成, 包括以下这些: 将显式提供的初始化参数与未提供的初始化参数的默认值相结合, 检查初始化参数的有效性, 为实例[instance]分配存储, 用值来填充这些槽[slot], 并且执行用户提供的执行额外初始化的方法[method]. make-instance 的每个步骤都是通过一个广义函数[generic function]实现的, 以提供一种定制该步骤的机制. 另外, make-instance 自身也是一个广义函数[generic function]并且因此也可以被定制.

这个对象系统为每个步骤指定了系统提供的主方法[method]并且因此为整个初始化过程指定了一个定义明确的标准行为. 这个标准行为提供了四个简单的机制用于控制初始化:

* 声明一个符号[symbol]作为一个槽[slot]的初始化参数. 一个初始化参数是通过对 defclass 使用 :initarg 槽选项来声明的. 这个提供了一个机制, 可以在一个 make-instance 调用中来给一个槽[slot]提供值.

* 为一个初始化参数提供一个默认值表达式形式. 初始化参数的默认值表达式形式通过对 defclass 使用 :default-initargs 类选项来定义的. 如果没有显式给 make-instance 提供一个初始化参数作为参数, 那么默认值表达式形式就会在定义它的 defclass 表达式形式所在的词法环境中被求值, 并且产生的值被用作这个初始化参数的值.

* 为一个槽[slot]提供一个默认的初始值表达式形式. 一个槽[slot]的默认初始值表达式形式通过对 defclass 使用 :initform 槽选项来定义的. 如果没有给 make-instance 提供和那个槽[slot]关联的初始化参数或者通过 :default-initargs 提供默认值, 那么这个默认初始值表达式形式就会在定义它的 defclass 表达式形式所在的词法环境中被求值, 并且产生的这个值被存储到这个槽[slot]中. 当创建一个实例[instance]时, 更新一个实例[instance]来符合重定义的类[class]时, 或者更新一个实例[instance]来符合不同类[class]的定义时, 一个局部槽[local slot]的这个 :initform 表达式形式可能被使用. 当定义或者重定义这个类[class]时, 一个共享槽[shared slot]的这个 :initform 表达式形式会被使用.

* 为 initialize-instance 和 shared-initialize 定义方法[method]. 上面描述的槽填充行为是由一个系统提供的 initialize-instance 主方法[method]来实现的, 它调用 shared-initialize. 广义函数[generic function] shared-initialize 实现了由这四种情况所共享的初始化部分: 创建一个实例[instance]的时候, 重新初始化一个实例[instance]的时候, 更新一个实例[instance]来符合重定义的类[class]时, 还有更新一个实例[instance]来符合一个不同的类[class]的定义时. 系统提供的 shared-initialize 主方法[method]直接实现上述槽填充行为, 而 initialize-instance 简单地调用 shared-initialize.

> * 7.1.1 [初始化参数](#InitArguments)
> * 7.1.2 [声明初始化参数的有效性](#DeclaringValidityInitArg)
> * 7.1.3 [初始化参数的默认值](#DefaultInitArg)
> * 7.1.4 [初始化参数的规则](#RulesInitArg)
> * 7.1.5 [Shared-Initialize](#SharedInitialize)
> * 7.1.6 [Initialize-Instance](#InitializeInstance)
> * 7.1.7 [Make-Instance 和 Initialize-Instance 的定义](#DefMIII)

### 7.1.1 <span id="InitArguments">初始化参数</span>

一个初始化参数控制对象[object]的创建和初始化. 使用关键字符号[symbol]来命名初始化参数往往是很方便的, 但是初始化参数的名字[name]可以是任何符号[symbol], 包括 nil. 一个初始化参数可以以两种方式被使用: 用一个值去填充一个槽[slot]或者为一个初始化方法[method]提供一个参数. 一个单独的初始化参数可以被同时用作这两个目的.

一个初始化参数列表[initialization argument list]是一个初始化参数名字和值的属性列表[property list]. 它的结构与属性列表[property list]相同, 也与 &key 参数处理的参数列表部分相同. 在这些列表中, 如果一个初始化参数名称在初始化参数列表中出现不止一次, 那么最左边出现的就会提供值, 而其余的会被忽略. 给 make-instance 的那些参数 (在第一个参数后面的) 组成初始化参数列表[initialization argument list].

一个初始化参数可以和一个槽[slot]关联. 如果这个初始化参数在初始化参数列表[initialization argument list]中有一个值, 这个值会被存储到新创建对象[object]的那个槽[slot]中, 覆盖任何和这个槽[slot]关联的 :initform 表达式形式. 一个单独的初始化参数可以初始化不止一个槽[slot]. 初始化一个共享槽[shared slot]的初始化参数存储它的值到那个共享槽[shared slot]中, 替换掉任何先前的值.

一个初始化参数可以和一个方法[method]关联. 当一个对象[object]被创建并且提供一个特定的初始化参数时, 这个初始化参数的名字和值作为一个关键字参数对来调用广义函数[generic function] initialize-instance, shared-initialize, 和 allocate-instance. 如果在初始化参数列表[initialization argument list]中没有给这个初始化参数提供一个值, 这个方法[method]的 lambda 列表[lambda list]会提供一个默认值.

初始化参数被用于四种情况: 创建一个实例[instance]时, 重新初始化一个实例[instance]时, 更新一个实例[instance]去符合一个重定义的类[class], 还有更新一个实例[instance]去符合一个不同的类[class]的定义.

由于初始化参数被用于控制某个特定类[class]的实例[instance]的创建和初始化, 我们就说一个初始化参数是那个类的"一个初始化参数". 

### 7.1.2 <span id="DeclaringValidityInitArg">声明初始化参数的有效性</span>

在使用初始化参数的四种情况的任何一种时, 都会检测初始化参数的有效性. 一个初始化参数可能在一种情况是有效的但是在另一种却是无效的. 比如, 系统提供的针对类[class] standard-class 的 make-instance 的主方法[method]检测它的初始化参数的有效性, 如果提供的一个初始化参数在那个情况下没有被有效声明, 那么就会发出一个错误.

关于声明初始化参数有效性这里有两个方法.

* 填充槽[slot]的初始化参数通过给 defclass 的 :initarg 槽选项来声明为有效的. 这个 :initarg 槽选项从超类[superclass]中继承下来. 因此填充一个类[class]的槽[slot]的有效初始化参数集合是这个类[class]和它的超类[superclass]声明为有效的填充槽[slot]的初始化参数的并集. 填充槽[slot]的初始化参数在所有四种情况中都是有效的.

* 给方法[method]提供参数的初始化参数通过定义这些方法[method]来声明为有效的. 在这个方法[method]的 lambda 列表[lambda list]中指定的每个关键字参数的关键字名字成为这个方法[method]可应用的所有类[class]的初始化参数. 一个可应用方法的 lambda 列表[lambda list]中 &allow-other-keys 的出现会禁用初始化参数的有效性检测. 因此方法[method]继承控制了给方法[method]提供参数的有效初始化参数的集合. 用于声明初始化参数有效的方法[method]定义的广义函数[generic function]如下所示:

    -- 创建一个类[class]的实例[instance]: allocate-instance, initialize-instance, 和 shared-initialize. 通过这些方法[method]声明为有效的初始化参数在创建一个类[class]的一个实例[instance]时是有效的.

    -- 重新初始化一个实例[instance]: reinitialize-instance 和 shared-initialize. 通过这些方法[method]声明为有效的初始化参数在重新初始化一个实例[instance]时是有效的.

    -- 更新一个实例[instance]来符合重定义的类[class]: update-instance-for-redefined-class 和 shared-initialize. 通过这些方法[method]声明为有效的初始化参数在更新一个实例[instance]来符合重定义的类[class]时是有效的.

    -- 更新一个实例[instance]来复合一个不同类[class]的定义: update-instance-for-different-class 和 shared-initialize. 通过这些方法[method]声明为有效的初始化参数在更新一个实例[instance]来复合一个不同类[class]的定义时是有效的.

一个类[class]的有效初始化参数集是那些填充槽[slot]或给方法[method]提供参数的初始化参数以及预定义的初始化参数 :allow-other-keys 的集合. :allow-other-keys 的默认值是 nil. 如果初始化参数 :allow-other-keys 的值是 true 那么初始化参数的有效性检测就会被禁用. 

### 7.1.3 <span id="DefaultInitArg">初始化参数的默认值</span>

可以使用类[class]选项 :default-initargs 来给一个初始化参数提供一个默认值表达式形式[form]. 如果一个初始化参数被某个特定的类[class]声明为有效的, 它的默认值表达式形式可能被一个不同的类[class]指定. 在这个情况下 :default-initargs 被用于给一个继承的初始化参数提供一个默认值.

这个 :default-initargs 选项仅被用于给初始化参数提供默认值; 它不会声明一个符号[symbol]作为有效初始化参数的名字. 此外, 这个 :default-initargs 选项仅在创建一个实例[instance]时被用于给初始化提供默认值.

给这个 :default-initargs 类选项的参数是一个初始化参数名字和表达式形式[form]交替的列表. 每个表达式形式[form]是对应初始化参数的默认值表达式形式. 一个初始化参数的默认值表达式形式[form]当且仅当这个初始化参数没有出现在 make-instance 的参数中并且没有被一个更具体的类[class]省略的时候被使用和求值.默认值表达式形式[form]在提供它的 defclass 表达式形式的词法环境中被求值; 产生的值被用作这个初始化参数的值.

提供给 make-instance 的初始化参数和默认初始化参数组合来产生一个默认初始化参数列表[defaulted initialization argument list]. 一个默认初始化参数列表[defaulted initialization argument list]是一个交替初始化参数名称和值的列表, 其中未提供的初始化参数是默认值, 在这个列表中显式提供的初始化参数出现在默认的初始化参数的前面. 默认初始化参数根据提供默认值的这些类[class]的类优先级列表[class precedence list]中的顺序来排序.

就槽[slot]的初始化而言, :default-initargs 和 :initform 选项的目的存在一个区别. 在不知道这个初始化参数是初始化一个槽[slot]还是传递给一个方法[method]的情况下, 这个 :default-initargs 类选项为用户提供一个机制去给这个初始化参数提供一个默认值表达式形式[form]. 如果那个初始化参数没有在一个 make-instance 的调用中显式提供, 那么就使用这个默认值表达式形式, 就像在这个调用中提供了一样. 与此相反, 这个 :initform 槽选项为用户提供一个机制去给一个槽[slot]提供一个默认初始值表达式形式. 一个 :initform 表达式形式当且仅当没有给 make-instance 传递一个和这个槽[slot]关联的初始化参数或者被 :default-initargs 省略的时候被用于初始化一个槽[slot].

初始化参数的默认值表达式形式的求值顺序和 :initform 表达式形式的求值顺序是没有定义的. 如果求值的顺序很重要, 应该使用 initialize-instance 或 shared-initialize 方法[method]. 

### 7.1.4 <span id="RulesInitArg">初始化参数的规则<\span>

这个 :initarg 槽选项对于一个给定的槽[slot]可能不止一次被指定.

下面的规则指出了初始化参数被多次定义的时机:

* 如果相同的初始化参数名出现在超过一个 :initarg 槽选项中时, 一个给定的初始化参数可以被用于初始化不止一个槽[slot].

* 一个给定的初始化参数名可以出现在不止一个初始化方法[method]的 lambda 列表[lambda list]中.

* 一个给定的初始化参数名可以同时出现在一个 :initarg 槽选项和一个初始化方法[method]的 lambda 列表[lambda list]中.

如果在 make-instance 的参数中给定初始化同一个槽[slot]的两个或更多初始化参数, 在这个初始化参数列表[initialization argument list]中的这些初始化参数中的最左边的那个来提供值, 即便这些初始化参数有着不同的名字.

如果初始化同一个槽的两个或更多不同的初始化参数有默认值并且在给 make-instance 的那些参数中都没有显式提供, 那么出现在最具体的那些类[class]的 :default-initargs 类选项中的初始化参数来提供值. 如果一个单独的 :default-initargs 类选项指定了两个或更多初始化相同槽[slot]的初始化参数并且在给 make-instance 的那些参数中都没有显式提供, 在 :default-initargs 类选项中最左边那个来提供值, 并且忽略剩余默认值表达式形式[form]的值.

在 make-instance 的参数中显式给定的初始化参数出现在默认初始化参数的左边. 假设类 C1 和 C2 为不同的槽[slot]提供默认初始化参数值, 并且 C1 比 C2 更具体; 那么在默认初始化参数列表[defaulted initialization argument list]中 C1 提供的默认初始化参数值在 C2 提供的值的左边. 如果一个单独的 :default-initargs 类选项为两个不同的槽[slot]提供了初始化参数的值, 被指定的值距离 :default-initargs 的左边更远的那个初始化参数在默认初始化参数列表[defaulted initialization argument list]中也距离左边更远.

如果一个槽[slot]同时有一个 :initform 表达式形式和一个 :initarg 槽选项, 并且这个初始化参数使用 :default-initargs 提供默认值或者给 make-instance 提供了这个参数, 那么这个捕获的 :initform 表达式形式既不会被使用也不会被求值.

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

在一个实例[instance]被创建时, 一个实例[instance]被重新初始化时, 一个实例[instance]被更新去符合一个重定义的类[class]时, 以及一个实例[instance]被更新去符合一个不同的类[class]时, 广义函数[generic function] shared-initialize 被用于使用初始化参数和 :initform 表达式形式来填充一个实例[instance]的槽[slot]. 它使用标准方法[method]组合. 它接受以下参数: 要被初始化的实例[instance], 这个实例[instance]中可以访问的[accessible]槽[slot]的名字[name]集合的一份说明, 还有任意数量的初始化参数. 在前两个后面的参数一定组成一个初始化参数列表[initialization argument list].

给 shared-initialize 的第二个参数可能是以下其中之一:

* 它可以是一个槽[slot]名字的列表[list] (可能是空的), 指定了那些槽名字的集合.

* 它可以是符号 t, 指定了所有槽[slot]的集合.

这里有个系统提供的 shared-initialize 的主方法[method], 其中第一个参数特化符[parameter specializer]是类[class] standard-object. 不管槽是共享的还是局部的, 这个方法[method]在每个槽[slot]上表现如下:

* 如果这个初始化参数列表[initialization argument list]中的一个初始化参数为那个槽[slot]指定了一个值, 那么这个值就会被存储到那个槽[slot]中, 即便在这个方法[method]执行前一个值已经被存储到那个槽[slot]里. 受影响的槽[slot]独立于由 shared-initialize 的第二个参数表示的槽[slot].

* 任何由第二个参数表示的在这个点仍是未绑定的槽[slot]都会根据它们的 :initform 表达式形式来初始化. 对于任何有着一个 :initform 表达式形式的槽[slot], 那个表达式形式[form]会在它的定义 defclass 表达式形式所在的词法环境中被求值, 并且结果被存储到那个槽[slot]中. 比如, 如果一个 before 方法[before method]存储一个值到槽[slot]中, 这个 :initform 表达式形式不会被用来给这个槽[slot]提供一个值. 如果第二个参数指定了一个不对应这个实例[instance]中任何可访问的[accessiable]槽[slot]的名字, 结果是未指定的.

* 在章节 7.1.4 (初始化参数的规则) 中提及的规则也是遵守的.

广义函数 shared-initialize 会被系统提供的 reinitialize-instance, update-instance-for-different-class, update-instance-for-redefined-class, 和 initialize-instance 的主方法[method]调用. 因此, 可以为 shared-initialize 写一个方法[method]来指定发生在所有这些上下文中的动作. 

### 7.1.6 <span id="InitializeInstance">Initialize-Instance</span>

广义函数[generic function] initialize-instance 被 make-instance 调用来初始化一个新创建的实例[instance]. 它使用标准方法组合[standard method combination]. 可以定义 initialize-instance 的方法[method], 以便执行任何无法通过为槽[slot]提供初始值来实现的初始化.

在初始化期间, initialize-instance 在以下动作执行后被调用:

* 默认初始化参数列表[defaulted initialization argument list]已经通过结合提供的初始化参数列表[initialization argument list]和这个类[class]的默认初始化参数被计算出来.

* 默认初始化参数列表[defaulted initialization argument list]的有效性已经被检测. 如果初始化参数中的任何一个还没有被有效声明, 那么就会发出一个错误.

* 一个槽[slot]还没有被绑定的实例[instance]被创建出来.

使用一个新的实例和默认初始化参数来调用广义函数 initialize-instance. 这里有一个系统提供的 initialize-instance 的主方法[method], 其中参数特化符[parameter specializer]是类[class] standard-object. 这个方法[method]调用广义函数 shared-initialize 根据槽[slot]的初始化参数和 :initform 表达式形式来填充槽[slot]; 使用以下参数来调用广义函数 shared-initialize : 这个实例[instance], t, 还有默认初始化参数.

注意, initialize-instance 在它对 shared-initialize 调用中提供默认初始化参数列表[defaulted initialization argument list], 因此，由系统提供的 shared-initialize 主方法[method]执行的第一步考虑了在调用 make-instance 中提供的初始化参数和默认初始化参数列表[defaulted initialization argument list].

initialize-instance 的方法[method]可以被定义来指定一个实例[instance]被初始化时采取的动作. 如果只有 initialize-instance 的 after 方法[after method]被定义, 它们会在系统提供的用于初始化的主方法[method]之后被运行, 并且因此不会和 initialize-instance 的默认行为冲突.

对象系统提供了两个在 initialize-instance 方法的主体中有用的函数[function]. 函数[function] slot-boundp 返回一个广义 boolean 值, 表示一个指定的槽[slot]是否有一个值的; 这提供了一种机制, 用于编写 initialize-instance 的 after 方法[after method], 用于当且仅当槽尚未初始化的情况下去初始化槽. 函数[function] slot-makunbound 使这个槽[slot]没有值. 

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

在 make-instance 的定义中省略的代码用默认初始化参数扩充了 initargs , 并检查产生的初始化参数，以确定是否提供了一个初始化参数, 既不填充槽[slot], 也不向可应用的方法[method]提供参数.

广义函数 initialize-instance 的行为表现就像它是如下定义的那样, 除了某些优化是允许的:

```LISP
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs)))
```

这些程序可以被定制.

程序员接口级别的定制包括使用给 defclass 的 :initform, :initarg, 和 :default-initargs 选项, 还有为 make-instance, allocate-instance, 和 initialize-instance 定义方法[method]. 也可以为 shared-initialize 定义方法[method], 它会被广义函数 reinitialize-instance, update-instance-for-redefined-class, update-instance-for-different-class, 和 initialize-instance 调用. 元对象级别支持额外的定制.

具体实现允许去对 initialize-instance 和 shared-initialize 做某些优化. 在章节 7 中 shared-initialize 的描述提及了可能的优化. 

## 7.2 <span id="ChangeClassInstance">修改一个实例的类</span>

函数[function] change-class 可以被用来修改一个实例[instance]的类[class], 从它的当前类 Cfrom 到一个不同的类 Cto; 它修改这个实例[instance]的结构来符合这个类 Cto 的定义.

注意, 修改一个实例[instance]的类[class]可能导致槽[slot]被添加或删除. 修改一个实例[instance]的类[class]不会修改由 eq 函数定义的它的同一性(identity).

当 change-class 在一个实例[instance]上被调用, 会发生一个分为两步的更新过程. 第一步通过添加新的局部槽[local slot]还有废弃这个新版本的实例[instance]中没有指定的局部槽[local slot]来修改这个实例的结构. 第二步初始化新添加的局部槽[local slot]并执行其他任何用户定义的动作. 这两步在以下两个章节中有进一步的描述.

### 7.2.1 修改实例的结构

为了使这个实例[instance]去符合类 Cto, 类 Cto 指定而类 Cfrom 没有指定的局部槽[local slot]会被添加, 并且类 Cfrom 指定而类 Cto 没有指定的局部槽[local slot]会被丢弃.

类 Cto 和类 Cfrom 都指定的局部槽[local slot]会被保留. 如果这样一个局部槽[local slot]没有被绑定, 它就保持未绑定状态.

在类 Cfrom 中指定为共享的但是在类 Cto 中指定为局部的槽[slot]的值会被保留.

这个更新中的第一步不会影响任何共享槽[shared slot]的值. 

### 7.2.2 初始化新添加的局部槽

更新操作的第二步初始化新添加的槽[slot]并且执行任何其他用户定义的动作. 这个步骤由广义函数 update-instance-for-different-class 来实现. 广义函数 update-instance-for-different-class 在更新的第一步完成后被 change-class 所调用.

广义函数 update-instance-for-different-class 在 change-class 计算的参数上被调用. 传递的第一个参数是这个要被更新的实例[instance]的一个拷贝并且是类 Cfrom 的一个实例[instance]; 这个拷贝在广义函数 change-class 中有着动态范围[dynamic extent]. 第二个参数是 change-class 到目前为止更新的实例[instance]并且那是类 Cto 的一个实例[instance]. 剩余的参数是一个初始化参数列表[initialization argument list].

这里有一个系统提供的 update-instance-for-different-class 的主方法[method], 它有两个参数特化符, 其中的每一个都是类[class] standard-object. 首先这个方法[method]检测初始化参数的有效性, 如果提供的一个初始化参数没有被有效声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (声明初始化参数的有效性).) 然后它调用广义函数 shared-initialize 并传递以下参数: 这个新的实例[instance], 新添加槽的名字[name]的一个列表, 还有它收到的那些初始化参数. 

### 7.2.3 定制一个实例的类的变更

update-instance-for-different-class 方法[method]可以被定义用来指定一个实例[instance]被更新时发生的动作. 如果只有 update-instance-for-different-class 的 after 方法[after method]被定义, 它们会在系统提供的初始化主方法[method]之后被运行并且不会干涉 update-instance-for-different-class 的默认行为.

shared-initialize 的方法[method]可能被定义用来定制类[class]的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 

## 7.3 <span id="ReinitInstance">重新初始化一个实例</span>

广义函数 reinitialize-instance 可以被用于根据初始化参数来修改槽的值.

重新初始化的过程修改一些槽的值并执行任何用户定义的动作. 它不会修改一个实例的结构来添加或删除槽, 并且它也不会使用任何 :initform 表达式形式来初始化槽.

广义函数 reinitialize-instance 可以被直接调用. 它接受一个必要参数, 这个实例. 它也接受任意数量的初始化参数来被 reinitialize-instance 或 shared-initialize 的方法使用. 在必要的实例后面的参数必须组成一个初始化参数列表.

这里有一个系统提供的 reinitialize-instance 主方法, 其中参数特化是类 standard-object. 首先这个方法检查初始化参数的有效性, 而过一个提供的参数没有被有效声明就会发出一个错误. (关于更多信息, 见章节 7.1.2 (Declaring the Validity of Initialization Arguments).) 然后它调用广义函数 shared-initialize 并传递如下参数: 这个实例, nil, 还有它收到的初始化参数.

### 7.3.1 定制重新初始化行为

reinitialize-instance 方法可以被定义, 用来指定一个实例被更新时采取的动作. 如果只有 reinitialize-instance 的 after 方法被定义, 它们会在系统提供的初始化主方法之后被运行并且因此不会影响 reinitialize-instance 的默认行为.

shared-initialize 方法可以被定义用来定制类的重定义行为. 关于更多信息, 见章节 7.1.5 (Shared-Initialize). 


## 7.4 <span id="MetaObjects">元对象</span>

对象系统的具体实现操纵类, 方法和广义函数. 对象系统包含了由类方法定义的广义函数的集合; 这些广义函数的行为定义了这个对象系统的行为. 这些方法被定义的对应的类的实例称之为元对象.

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

一个槽的默认初始值表达式形式被 :initform 槽选项所定义. 当这个 :initform 表达式形式被用于提供一个值的时候, 它会在求值 defclass 表达式形式的词法环境中被求值. 这个 :initform 连同求值 defclass 表达式形式的词法环境一起被称为一个被捕获的初始化表达式形式(captured initialization form). 关于更多详情, 见章节 7.1 (Object Creation and Initialization).

一个局部槽被定义为一个槽, 它仅对于一个实例是可访问的, 那个实例就是分配槽的那个. 一个共享槽被定义为一个槽, 它对于给定类和它的子类而言超过一个实例都是可见的.

当一个类的 defclass 表达式形式包含了一个给定名字的槽指定符, 就说这个类以这个名字定义一个槽. 定义一个局部槽不会马上创建一个槽; 它导致这个类的实例被创建时一个槽会被创建. 定义一个共享槽会马上创建一个槽.

给 defclass 的这个 :allocation 槽选项控制被定义的槽的类型. 如果这个 :allocation 槽选项的值是 :instance, 一个局部槽会被创建. 如果 :allocation 的值是 :class, 就会创建一个共享槽.

如果一个槽是由一个实例的类定义的, 或者是从该类的超类中继承的, 就说这个槽在这个类的实例中是可以访问的. 在一个实例中一个给定的名字的可访问的槽最多一个. 一个类定义的共享槽在这个类的所有实例中都是可以访问的. 关于槽的继承的详细解释在章节 7.5.3 (Inheritance of Slots and Slot Options) 中给出. 

### 7.5.2 <span id="AccessingSlots">访问槽</span>

槽可以通过两种方式被访问: 通过使用基本函数 slot-value 和通过使用 defclass 表达式形式产生的广义函数.

函数 slot-value 可以通过这个 defclass 表达式形式中指定的槽名字来使用, 用于访问给定类的一个实例中一个可访问的特定的槽.

宏 defclass 为读取或写入槽提供语法. 如果请求了一个 reader 方法, 用于读取这个槽的值的一个方法会被自动生成, 但是用于存储一个值到这个槽的方法不会被生成. 如果请求了一个 writer 方法, 用于存储一个值到这个槽的一个方法会被自动生成, 但是读取它的值的方法不会被生成. 如果请求了一个 accessor 方法, 一个用于读取这个槽的值的方法和一个用于存储一个值到这个槽的方法会被自动生成. Reader 和 writer 方法通过使用 slot-value 来实现.

当一个槽指定了 reader 或者 writer 方法, 所生成方法所属的广义函数的名字是直接指定的. 如果为这个 writer 方法指定的名字是这个符号 name, 那么用于写入这个槽的广义函数的名字就是 name, 这个广义函数接受两个参数: 新的值和这个实例, 以这样的顺序. 如果为 accessor 方法指定的名字是符号 name, 用于读取这个槽的广义函数的名字就是符号 name, 而用于写入这个槽的广义函数的名字就是列表 (setf name).

通过提供 :reader, :writer, 或 :accessor 槽选项创建或修改的一个广义函数可以被当作一个普通的广义函数.

注意, slot-value 可以被用于读取或写入一个槽的值, 不管那个槽的 reader 或 writer 方法是否存在. 当使用 slot-value 时, 没有 reader 或 writer 方法会被调用.

宏 with-slots 可以被用于建立一个词法环境, 在这个环境中指定的槽就像它们是变量一样是词法上可用的. 宏 with-slots 调用函数 slot-value 来访问特定的槽.

宏 with-accessors 可以被用于建立一个词法环境, 在这个环境中指定的槽通过它们的访问器是词法上可用的, 就像它们是变量一样. 宏 with-accessors 调用合适的访问器来访问指定的槽. 


### 7.5.3 <span id="InheritanceSlotsSlotOptions">槽和槽选项的继承</span>

一个类 C 的一个实例中所有可访问槽的名字集合是 C 和它的所有超类定义的槽的名字的并集. 一个实例的结构是这个实例中局部槽的名字集合.

在最简单的情况下, 在 C 及其超类中只有一个类定义了一个带有给定槽名的槽. 如果 C 的一个超类定义了一个槽, 就说这个槽是继承的. 槽的特征是由定义类的槽指定符决定的. 细想一个槽 S 的定义类. 如果这个 :allocation 槽选项的值是 :instance, 那么 S 是一个局部槽并且 C 的每一个实例都有它自己的名为 S 的槽存储它自己的值. 如果这个 :allocation 槽选项的值是 :class, 那么 S 是一个共享槽, 定义 S 的类存储这个值, 并且所有 C 的实例可以访问那个单个的槽. 如果这个 :allocation 槽选项被省略, 就使用 :instance.

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

一个广义函数是一个行为取决于提供给它的参数的类或同一性(identity)的函数. 一个广义函数对象和一个方法集合, 一个 lambda 列表, 一个方法组合, 还有其他信息相关联.

像一个普通函数一样, 一个广义函数接受参数, 执行一系列动作, 并且可能返回有用的值. 一个普通函数由有一个在函数被调用时总是被执行的代码中的单个主体. 一个广义函数有着一个代码主体的集合, 其中一个子集会被选择来执行. 选择的代码主体和它们的组合方式由给这个广义函数的一个或多个参数的类或同一性决定, 以及它们的方法组合.

普通函数和广义函数用相同的语法来调用.

广义函数是可以作为参数传递给 funcall 和 apply 并且用作第一个参数的真实函数.

函数名与广义函数的绑定可以通过以下几种方式建立. 它可以通过 ensure-generic-function, defmethod (隐式的, 应归于 ensure-generic-function) 或 defgeneric (也是隐式的, 应归于 ensure-generic-function) 在全局环境中建立. 没有为在词法环境中建立一个函数名与广义函数的绑定提供标准化机制.

当一个 defgeneric 表达式形式被求值, 采取这三个动作中的其中一个 (应归于 ensure-generic-function):

* 如果一个给定名字的广义函数已经存在, 这个存在的广义函数对象会被修改. 当前 defgeneric 表达式形式指定的方法会被添加, 并且这个通过前面的 defgeneric 表达式形式定义的已存在的广义函数中的方法会被移除. 通过当前 defgeneric 表达式形式添加的方法可能替换由 defmethod, defclass, define-condition, 或 defstruct 定义的方法. 广义函数中没有其他方法受到影响或替换.

* 如果这个给定的名字命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

* 否则就会使用这个 defgeneric 表达式形式中的方法定义指定的方法来创建一个广义函数.

某些操作符允许规范一个广义函数的选项, 就像它使用的方法组合的类型或它的参数优先级顺序. 这些操作符会被称为 "指定广义函数选项的操作符". 这个类别中唯一的标准化操作符是 defgeneric.

某些操作符为一个广义函数定义方法. 这些操作符会被称作方法定义操作符; 它们关联的表达式形式被称作方法定义表达式形式. 标准化的方法定义操作符列在下面这段中.

    defgeneric        defmethod  defclass  
    define-condition  defstruct            

    Figure 7-1. 标准化的方法定义操作符, 注意这些方法定义操作符中只有 defgeneric 可以指定广义函数选项. defgeneric 还有任何具体实现定义的可以指定广义函数选项的操作符都被称为 "指定广义函数选项的操作符".


### 7.6.2 <span id="IntroductionMethods">方法的介绍</span>

方法定义了一个广义函数的特定于类或者特定于同一性的行为以及操作.

一个方法对象和实现这个方法的代码, 一个指定这个给定方法何时是可应用的参数特化序列, 一个 lambda 列表, 还有一个被方法组合机制用来区分方法的限定符序列相关联.

一个方法对象不是一个函数并且不能像函数一样被调用. 在这个对象系统中的各种机制接收一个方法对象并调用它的方法函数, 就像这个广义函数被调用时那样. 这个发生时就说这个方法被调用.

方法定义表达式形式包含了当广义函数的参数导致它定义的方法被调用时要运行的代码. 当一个方法定义表达式形式被求值, 一个方法对象会被创建并且采取这四种动作中的一个:

* 如果一个给定名字的广义函数已经存在并且如果一个在参数特化符和限定符上都符合新的那个的方法对象已经存在, 那个新的对象会替换旧的那个. 关于一个方法在参数特化符和限定符上与另一个方法达成一致的定义, 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

* 如果一个给定名字的广义函数已经存在并且这里没有在参数特化符和限定符上都符合新的那个的方法对象, 那个存在的广义函数对象会被修改来包含那个新的方法对象.

* 如果给定的名字命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

* 否则就会用那个方法定义表达式形式指定的方法创建一个广义函数.

如果一个新的方法的 lambda 列表和广义函数是不相等的, 就会发出一个错误. 如果一个不能指定广义函数选项的方法定义操作符创建了一个新的广义函数, 这个广义函数的 lambda 列表是来自于这个方法定义表达式中的方法的 lambda 列表, 在这种方式下它们是一致的. 关于一致性的讨论, 见章节 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

每个方法都有一个专门的 lambda 列表, 它决定了何时这个方法可以被应用. 一个专门的 lambda 列表就像一个普通 lambda 列表除了一个参数特化符可以出现来代替一个必要参数. 一个参数特化符是一个列表 (variable-name parameter-specializer-name), 其中 parameter-specializer-name 以下的一种:

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

方法可以有限定符, 它给方法组合过程提供一种区分方法的方式. 一个带有一个或多个限定符的方法被称为受限方法(qualified method). 一个没有限定符的方法被称为一个非受限方法. 一个限定符是任何非列表元素. 标准方法组合类型定义的限定符是符号.

在这个规范中, 术语 "主方法(primary method)" 和 "辅助方法(auxiliary method)" 在方法组合类型中根据它们的用途被用于区分方法. 在标准方法组合中, 主方法是非受限方法而辅助方法是有以下之一的单个限定符的方法: :around, :before, 或 :after. 有这些限定符的方法分别被称为 around 方法, before 方法, 还有 after 方法. 当使用 define-method-combination 简单的表达式形式定义一个方法组合类型时, 主要方法是用方法组合的类型命名的方法, 而辅助方法有着限定符 :around. 因此术语 "主方法(primary method)" 和 "辅助方法(auxiliary method)" 只有在给定方法组合类型中有相关定义. 

### 7.6.3 <span id="AgreeParamSpecQualifiers">关于参数特化符和限定符的一致性</span>

如果遵循以下条件, 则两个方法在参数特化符和限定符之间达成一致:

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

#### 7.6.5.1 广义函数和方法的关键字参数示例

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

当用特定的参数调用一个广义函数时, 它必须决定要执行的代码. 这个代码被称为这些参数的生效方法. 生效方法是一个广义函数中可应用方法的组合, 它调用这些方法的一部分或全部.

如果一个广义函数被调用而没有可应用的方法, 广义函数 no-applicable-method 会被调用, 这个调用的结果被用作对原始广义函数调用的结果. 调用 no-applicable-method 优先于检测可接受的关键字参数; 见章节 7.6.5 (Keyword Arguments in Generic Functions and Methods).

当这个生效方法已经被决定时, 会用传递给这个广义函数相同的参数去调用它. 不管它返回什么值都会作为这个广义函数的值被返回.

> * 7.6.6.1 [确定生效方法](#DetermEffectMethod)
> * 7.6.6.2 [标准方法组合](#StandMethodComb)
> * 7.6.6.3 [声明方法组合](#DeclaraMethodComb)
> * 7.6.6.4 [内建的方法组合类型](#BuiltInMethodCombTypes)


#### 7.6.6.1 <span id="DetermEffectMethod">确定生效方法</span>

生效方法通过以下三个步骤来决定:

1. 选择可应用的方法.

2. 通过优先级顺序对可应用方法排序, 把最具体的方法放在第一位.

3. 对排序后的可应用方法列表应用方法组合, 产生生效方法.

##### 7.6.6.1.1 选择可应用的方法

这个步骤在章节 7.6.2 (Introduction to Methods) 中描述. 

##### 7.6.6.1.2 通过优先级顺序对可应用方法排序

为了比较两个方法的优先级, 它们的参数指定符会被按顺序检查. 默认的检查顺序是从左到右, 但是可以通过对 defgeneric 或任何指定广义函数选项的操作符指定 :argument-precedence-order 选项来指定一个替代的顺序.

每一个方法的对应参数指定符都会被比较. 当一对参数指定符一致时, 比较下一对的一致性. 如果所有对应的参数指定符都一致, 那么两个方法必须有不同的限定符; 在这个情况下, 任何一个方法都可以优先于另一个. 关于一致性的更多信息, 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

如果某些对应参数不一致, 第一对不一致的参数决定了这个优先级. 如果两个参数指定符都是类, 两个方法中更具体的是方法参数指定符在这个对应参数的类优先级列表中出现的更早的那个方法. 由于可应用方法被选择的这个方法, 参数指定符保证存在于参数的类的类优先列表中.

如果一对对应参数指定符中只有一个是 (eql object), 带有这个参数指定符的方法优先于另一个方法. 如果两个参数指定符都是 eql 表达式, 这个参数指定符一定是一致的 (否则对于这个参数这两个方法不会都是可应用的).

产生的可应用方法列表中最具体的方法在第一个, 最不具体的在最后一个. 

##### 7.6.6.1.3 对排序后的可应用方法使用方法组合

在这个简单的例子中---如果使用了标准方法组合并且所有可应用的方法都是主方法---这个生效方法就是最具体的方法. 这个方法可以通过函数 call-next-method 调用下一个最具体的方法. 这个 call-next-method 会调用的方法被称为下一个方法. 断言 next-method-p 检测是否存在下一个方法. 如果 call-next-method 被调用并且没有下一个最具体的方法, 广义函数 no-next-method 会被调用.

通常, 生效方法是可应用方法的某个组合. 它由一个表达式形式来描述，这个表达式形式包含对某些或全部可应用方法的调用, 返回值或多值来作为广义函数返回的值或多值, 并可选地使一些方法可以通过 call-next-method 访问.

在这个生效方法中的每一个方法的角色由它的限定符和方法的特性所决定. 一个限定符用于标记一个方法, 而限定符的含义由这个过程的这一步使用这些标记的方式决定. 如果一个可应用方法由一个不识别的限定符, 这个步骤会发出一个错误并且不会在生效方法中包含那个方法.

当标准方法组合和限定符方法一起使用时, 产生的生效方法就像章节 7.6.6.2 (Standard Method Combination) 中所描述的那样.

另一个方法组合的类型可以通过使用 defgenric 或者任何其他指定广义函数选项的操作符的 :method-combination 选项来指定. 在这个情况下, 这个过程的这个步骤可以被定制.

新的方法组合类型可以通过使用 define-method-combination 宏来定义. 


#### 7.6.6.2 <span id="StandMethodComb">标准方法组合</span>

标准方法组合由类 standard-generic-function 支持. 如果没有指定其他类型的方法组合或者指定了内置的方法组合类型 standard, 那么这个标准方法组合就会被使用.

主方法(primary method)定义了这个生效方法的主要动作, 而辅助方法(auxiliary method)以三种方式之一修改那个动作. 一个主方法没有方法限定符.

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

宏 define-method-combination 定义方法组合的新的表达式形式. 它为定制生效方法的产生提供了一个机制. 对于产生一个生效方法的默认过程在章节 7.6.6.1 (Determining the Effective Method) 中已描述. 这里有两个 define-method-combination 表达式形式. 短表达式形式是一个简单的工具而长表达式形式则更加强大和详细. 长表达式形式类似于 defmacro, 在它的主体中是一个计算一个 Lisp 表达式形式的表达式; 它为在方法组合中实现任意控制结构和方法限定符的任意处理提供一个机制. 


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


### 7.6.7 <span id="InheritanceMethods">方法的继承</span>

一个子类继承方法的意义在于任何适用于类的所有实例的方法也适用于该类的任何子类的所有实例.

不管那个方法定义操作符创建了这个方法, 方法的继承还是表现一样.

方法的继承在章节 7.6.6 (Method Selection and Combination) 中详细描述. 

## 7.7 <span id="TheObjectsDictionary">对象字典</span>

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
> *  [局部函数 CALL-NEXT-METHOD](#LF-CALL-NEXT-METHOD)
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

        函数 ensure-generic-function 被用于定义一个全局命名的没有方法的广义函数或者作为整体去指定或修改属于一个全局命名广义函数的选项和声明.

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


### <span id="SGF-REINITIALIZE-INSTANCE">标准广义函数 REINITIALIZE-INSTANCE</span>

* 语法(Syntax):

        reinitialize-instance instance &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

        reinitialize-instance (instance standard-object) &rest initargs

* 参数和值(Arguments and Values):

        instance---一个对象.
        initargs---一个初始化参数列表.

* 描述(Description):

        广义函数 reinitialize-instance 可以被用于根据 initargs 来修改一个实例的局部槽的值. 这个广义函数可以被用户调用.

        系统提供的 reinitialize-instance 主方法检查 initargs 的有效性, 如果提供的 initargs 没有被有效声明, 就会发出一个错误. 这个方法接下来用以下参数来调用广义函数 shared-initialize: 这个实例, nil (这个意味着根据槽的初始化表达式形式没有槽应该被初始化), 还有它收到的 initargs.

* 示例(Examples): None.

* 副作用(Side Effects):

        广义函数 reinitialize-instance 修改局部槽的值.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果提供的一个 initarg 没有被有效声明, 系统提供的 reinitialize-instance 主方法会发出一个错误.

* 也见(See Also):

        initialize-instance, shared-initialize, update-instance-for-redefined-class, update-instance-for-different-class, slot-boundp, slot-makunbound, Section 7.3 (Reinitializing an Instance), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

        Initargs 通过使用给 defclass 的 :initarg 选项, 或者通过定义 reinitialize-instance 或 shared-initialize 方法来声明为有效的. 任何定义在 reinitialize-instance 或 shared-initialize 方法的 lambda 列表中的每个关键字参数指定符的关键字名字对于那个方法可应用的所有类都被声明为有效的初始化参数.


### <span id="SGF-SHARED-INITIALIZE">标准广义函数 SHARED-INITIALIZE</span>

* 语法(Syntax):

        shared-initialize instance slot-names &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

        shared-initialize (instance standard-object) slot-names &rest initargs

* 参数和值(Arguments and Values):

        instance---一个对象.
        slot-names---一个列表或者 t.
        initargs---一个 keyword/value 对的列表 (其中是初始化参数的名字和值).

* 描述(Description):

        广义函数 shared-initialize 被用于使用 initargs 和 :initform 来填充一个实例 instance 的槽. 当一个实例被创建时, 当一个实例被重新初始化时, 当一个实例被更新去符合一个重定义的类时,  还有当一个实例被更新来符合一个新的类时, 它会被调用. 广义函数 shared-initialize 被系统提供的 initialize-instance, reinitialize-instance, update-instance-for-redefined-class, 和 update-instance-for-different-class 的主函数调用.

        广义函数 shared-initialize 接受以下参数: 要被初始化的实例 instance, 这个实例中可访问的槽名字集合 slot-names 的一个说明, 还有任意数量的初始化参数 initargs. 在前两个后面的参数必须组成一个初始化参数列表. 系统提供的 shared-initialize 主方法根据 initargs 和提供的 :initform 表达式形式用值初始化这些槽. Slot-names 表示要被初始化的槽, 如果没有 为这些槽提供 initargs 根据它们的 :initform 表达式形式.

        系统提供的主方法表现如下, 不管这个槽是局部的还是共享的:

            如果在初始化参数列表中的一个 initarg 为这个槽指定了一个值, 这个值会存储到这个槽中, 即便在这个方法被运行之前已经存储一个值到这个槽中.

            任何 slot-names 指定的槽在根据它们的 :initform 表达式形式来初始化时都是未绑定的. 对于任何这样由一个 :initform 表达式形式的槽, 这个表达式形式都会在它的定义 defclass 表达式形式的词法环境中被求值并且结果被存储到这个槽中. 比如, 如果一个 before 方法存储一个值到这个槽中, 那么这个 :initform 表达式形式不会为这个槽提供一个值.

            在章节 7.1.4 (Rules for Initialization Arguments) 中提及的规则也是遵守的.

        这个 slots-names 参数指定了要被初始化的槽, 如果没有提供初始化参数时就根据它们的 :initform 表达式形式. 它可以是一个槽名字的列表, 其中指定了这些槽名字的集合; 或者它可以是符号 t, 它表示所有槽的集合.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        initialize-instance, reinitialize-instance, update-instance-for-redefined-class, update-instance-for-different-class, slot-boundp, slot-makunbound, Section 7.1 (Object Creation and Initialization), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

        可以通过对 defclass 使用 :initarg 选项或者定义 shared-initialize 的方法来有效声明 initargs. 任何定义在 shared-initialize 上的方法的 lambda 列表中的关键字参数指定符的关键字名字对于所有这个方法可应用的类而言都是有效的 initarg.

        具体实现允许去优化 :initform 表达式形式: 通过对这些表达式形式进行求值, 并在运行任何 initialize-instance 方法之前将它们存储到槽中, 而不是在 initialize-instance 主方法中处理它们, 从而使它们无法产生或依赖于副作用. (这种优化可以通过使用 allocate-instance 方法复制原型实例来实现.)

        当仅有的接受完整的参数列表的方法是 standard-object 上的方法时, 具体实现允许去通过没有实际创建完整初始化参数列表来优化和槽关联的默认初始化值表达式. 在这个情况下默认初始值表达式形式可以被当作 :initform 表达式形式对待. 除了性能改进之外, 这种优化没有明显的效果. 

### <span id="SGF-U-I-F-D-C">标准广义函数 UPDATE-INSTANCE-FOR-DIFFERENT-CLASS</span>

* 语法(Syntax):

        update-instance-for-different-class previous current &rest initargs &key &allow-other-keys => implementation-dependent

* 方法签名(Method Signatures):

        update-instance-for-different-class (previous standard-object) (current standard-object) &rest initargs

* 参数和值(Arguments and Values):

        previous---一个原始实例的拷贝.
        current---原始实例 (修改后的).
        initargs---一个初始化参数列表.

* 描述(Description):

        广义函数 update-instance-for-different-class 不旨在被程序员调用. 程序员可能为它写方法. 函数 update-instance-for-different-class 只有通过函数 change-class 被调用.

        系统提供的 update-instance-for-different-class 主方法会检查 initargs 的有效性, 如果提供的一个 initarg 没有被有效声明就会发出一个错误. 接下来这个方法会根据 initargs 的值来初始化槽, 并且根据新添加的槽的 :initform 表达式形式来初始化这些新添加的槽. 它通过使用以下参数调用广义函数 shared-initialize 来完成这个: 这个实例 (当前的), 一个新添加槽的名字的列表, 还有它接受到的 initargs. 新添加的槽是那些之前类中不存在相同名字的槽的局部槽.

        update-instance-for-different-class 方法可以被定义用来指定当一个类被更新时要采取的动作. 如果只有 update-instance-for-different-class 的 after 方法被定义, 它们会在系统提供的初始化主方法之后被运行, 因此不会影响 update-instance-for-different-class 的默认行为.

        update-instance-for-different-class 的方法可以被定义用不同于 change-class 的方式来初始化槽. 这个 change-class 的默认行为在章节 7.2 (Changing the Class of an Instance) 中描述.

        给 update-instance-for-different-class 的参数通过 change-class 计算. 当 change-class 在一个实例上被调用, 会创建那个实例的一个拷贝; change-class 接下来会破坏性地修改原始的实例. 给 update-instance-for-different-class 的第一个参数, previous, 就是那个拷贝; 它临时持有旧的槽的值. 这个参数有着 change-class 内的动态范围; 如果一旦 update-instance-for-different-class 返回后它被引用, 结果是未定义的. 给 update-instance-for-different-class 的第二个参数, current, 是修改后的原始实例. 这个 previous 的使用意图是用 slot-value 或 with-slots 或通过调用一个 reader 广义函数来提取旧槽的, 或者用来运行可应用于原始类的实例的其他方法.

* 示例(Examples):

        见函数 change-class 的示例.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果一个提供的初始化参数没有被有效声明, 那么系统提供的 update-instance-for-different-class 主方法就会发出一个错误.

* 也见(See Also):

        change-class, shared-initialize, Section 7.2 (Changing the Class of an Instance), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

        Initargs 通过使用给 defclass 的 :initarg 选项或者通过定义 update-instance-for-different-class 或 shared-initialize 的方法来合法声明. 定义在 update-instance-for-different-class 或 shared-initialize 上的方法的 lambda 列表中的每个关键字参数指定符的关键字名字被声明为一个对于所有那个方法可应用的类的有效 initarg 名字.

        这个 update-instance-for-different-class 返回的值会被 change-class 忽略. 


### <span id="SGF-U-I-F-R-C">标准广义函数 UPDATE-INSTANCE-FOR-REDEFINED-CLASS</span>

* 语法(Syntax):

        update-instance-for-redefined-class instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys
        => result*

* 方法签名(Method Signatures):

        update-instance-for-redefined-class (instance standard-object) added-slots discarded-slots property-list &rest initargs

* 参数和值(Arguments and Values):

        instance---一个对象.
        added-slots---一个列表.
        discarded-slots---一个列表.
        property-list---一个列表.
        initargs---一个初始化参数列表.
        result---一个对象.

* 描述(Description):

        广义函数 update-instance-for-redefined-class 不打算给程序员调用. 程序员可以为它写方法. 广义函数 update-instance-for-redefined-class 通过 make-instances-obsolete 的机制被调用.

        系统提供的 update-instance-for-redefined-class 主方法检查 initargs 的有效性, 如果一个 initarg 没有被有效声明就会发出一个错误. 接下来这个方法根据 initargs 的值来初始化槽, 并且根据新添加的槽的 :initform 表达式形式来初始化新的 added-slots. 它通过使用以下参数来调用广义函数 shared-initialize 来实现这个: 这个实例(instance), 给这个实例的新的 added-slots 的名称列表, 还有它收到的 initargs. 新添加的槽是那些在旧版本的类中不存在相同名字的局部槽.

        当 make-instances-obsolete 被调用或者当一个类被重定义并且一个实例被更新时, 会创建一个捕获在这个原始实例中所有 discarded-slots 的名字和值的属性列表. 这个实例的结构会被转化以便符合当前类的定义. 给 update-instance-for-redefined-class 的参数是这个转化后的实例, 一个给这个实例的 added-slots 列表, 一个来自这个实例的 discarded-slots 列表, 以及这个包含有值但是被丢弃的槽的名字和值的属性列表. 这个被丢弃的槽的列表中包括那些在旧的类中是局部的在新的类中是共享的那些槽.

        这个 update-instance-for-redefined-class 返回的值会被忽略.

* 示例(Examples):

    ```LISP  
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果提供的一个 initarg 没有被有效声明系统提供的 update-instance-for-redefined-class 主方法会发出一个错误.

* 也见(See Also):

        make-instances-obsolete, shared-initialize, Section 4.3.6 (Redefining Classes), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes):

        Initargs 可以通过给 defclass 使用 :initarg, 或者通过为 update-instance-for-redefined-class 或 shared-initialize 定义方法来有效声明. 定义在 update-instance-for-redefined-class or shared-initialize 上的任何方法的 lambda 列表中的每个关键字参数指定符的关键字名字, 对于那个方法可应用的所有类都是有效的关键字. 


### <span id="SGF-CHANGE-CLASS">标准广义函数 CHANGE-CLASS</span>

* 语法(Syntax):

        change-class instance new-class &key &allow-other-keys => instance

* 方法签名(Method Signatures):

        change-class (instance standard-object) (new-class standard-class) &rest initargs

        change-class (instance t) (new-class symbol) &rest initargs

* 参数和值(Arguments and Values):

        instance---一个对象.
        new-class---一个类指定符.
        initargs---一个初始化参数列表.

* 描述(Description):

        广义函数 change-class 修改一个实例的类为 new-class. 它破坏性地修改并返回这个实例.

        如果在旧的类中存在任何和 new-class 中的局部槽名字相同的槽, 那个槽的值会被保留. 这个意味着如果这个槽有一个值, 在 change-class 被调用之后 slot-value 返回的值和在 change-class 之前是 eql 的. 类似地, 如果这个槽没有被绑定, 它就保持未绑定状态. 其他槽按照章节 7.2 (Changing the Class of an Instance) 中所描述的被初始化.

        在完成所有其他动作之后, change-class 调用 update-instance-for-different-class. 广义函数 update-instance-for-different-class 可以被用于给转化后的实例中的槽赋值. 见章节 7.2.2 (Initializing Newly Added Local Slots).

        如果上述方法中的第二个被选择, 那个方法在 instance, (find-class new-class), 以及 initargs 上调用 change-class.

* 示例(Examples):

    ```LISP
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
    ```

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        update-instance-for-different-class, Section 7.2 (Changing the Class of an Instance)

* 注意(Notes):

        函数 change-class 有几个语义难点. 首先, 它执行一个破坏性的操作, 并且可以在一个选择一个方法的实例上调用该方法. 当由于方法被组合而导致多个方法被调用时, 当前执行的方法或要被执行的方法可能不再是可应用的. 其次, 一些具体实现可能使用编译器对槽访问的优化, 当一个实例的类被修改时可能违背编译器所做的假设. 这个意味着如果一个广义函数的任何方法访问了任何槽, 那么程序员一定不能在那个方法中使用 change-class, 否则结果是未定义的. 


### <span id="F-SLOT-BOUNDP">函数 SLOT-BOUNDP</span>

* 语法(Syntax):

        slot-boundp instance slot-name => generalized-boolean

* 参数和值(Arguments and Values):

        instance---一个对象.
        slot-name---命名 instance 的一个槽的名字.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果在 instance 中名为 slot-name 的槽已经是绑定的, 那么就返回 true; 否则, 返回 false.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在这个 instance 中没有名为 slot-name 的槽, 那么 slot-missing 会按如下被调用:

        (slot-missing (class-of instance)
                      instance
                      slot-name
                      'slot-boundp)

        (如果 slot-missing 被调用并且返回了一个值, 它的主要的值的一个 boolean 等价物( boolean equivalent)会被 slot-boundp 返回.)

        这个特殊的行为依赖于实例(instance)的元类. 如果 instance 元类为 standard-class, 那么从来不会发出一个错误. 如果 instance 元类为 built-in-class, 那么总是会发出一个错误. 如果 instance 有着其他元类那么结果是未定义的--在这个情况可能会也可能不会发出一个错误. 特别注意, 对于状况(condition)和结构体(structure)的行为是没有指定的.

* 也见(See Also):

        slot-makunbound, slot-missing

* 注意(Notes):

        函数 slot-boundp 允许为 initialize-instance 编写 after 方法仅用来来初始化那些还没有被绑定的槽.

        虽然没有具体实现被要求, 但是强烈鼓励实现者去使用元对象协议中描述的函数 slot-boundp-using-class 来实现函数 slot-boundp. 


### <span id="F-SLOT-EXISTS-P">函数 SLOT-EXISTS-P</span>

* 语法(Syntax):

        slot-exists-p object slot-name => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        slot-name---一个符号.
        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        如果这个对象由一个名为 slot-name 的槽就返回 true.

* 示例(Examples): None.

* 受此影响(Affected By):

        defclass, defstruct

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        defclass, slot-missing

* 注意(Notes):

        虽然没有具体实现被要求, 但是还是强烈鼓励实现者去使用元对象协议中描述的函数 slot-exists-p-using-class 来实现函数 slot-exists-p. 

### <span id="F-SLOT-MAKUNBOUND">函数 SLOT-MAKUNBOUND</span>

* 语法(Syntax):

        slot-makunbound instance slot-name => instance

* 参数和值(Arguments and Values):

        instance -- 实例.
        Slot-name---一个符号.

* 描述(Description):

        函数 slot-makunbound 将一个实例中名为 slot-name 的槽复原到未绑定状态.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果在这个实例中不存在名为 slot-name 的槽, slot-missing 会按如下被调用:

        (slot-missing (class-of instance)
                      instance
                      slot-name
                      'slot-makunbound)

        (在这个情况下任何 slot-missing 返回的值都被会 slot-makunbound 忽略.)

        这个具体行为依赖于实例的元类. 如果实例的元类为 standard-class 那么从不发出一个错误. 如果实例的元类为 built-in-class 那么总是发出一个错误. 如果实例有着任何其他元类那么结果是未定义的--在这个情况下一个错误可能或可能不会发出. 特别要注意的是, 状况(condition)和结构体(structure)的行为没有指定.

* 也见(See Also):

        slot-boundp, slot-missing

* 注意(Notes):

        虽然没有实现被要求, 还是强烈鼓励实现者用元对象协议中描述的函数 slot-makunbound-using-class 来实现函数 slot-makunbound. 


### <span id="SGF-SLOT-MISSING">标准广义函数 SLOT-MISSING</span>

* 语法(Syntax):

        slot-missing class object slot-name operation &optional new-value => result*

* 方法签名(Method Signatures):

        slot-missing (class t) object slot-name operation &optional new-value

* 参数和值(Arguments and Values):

        class---object 的类.
        object---一个对象.
        slot-name---一个符号 (the name of a would-be slot).
        operation---符号 setf, slot-boundp, slot-makunbound, 或 slot-value 的其中之一.
        new-value---一个对象.
        result---一个对象.

* 描述(Description):

        当尝试去访问一个元对象是 standard-class 的对象的槽并且在那个类中没有名为 slot-name 的槽时, 函数 slot-missing 会被调用. 这个默认方法会发出一个错误.

        广义函数 slot-missing 不打算给程序员调用. 程序员可以为它写方法.

        广义函数 slot-missing 可能在 slot-value, (setf slot-value), slot-boundp, 还有 slot-makunbound 求值期间被调用. 对于这些操作符中的每一个, 对应 operation 参数的符号分别是 slot-value, setf, slot-boundp, 还有 slot-makunbound.

        当这个操作符尝试去设置这个槽的值时, 使用给 slot-missing 可选的 new-value 参数.

        如果 slot-missing 返回, 它的值会按照如下方式对待:

            如果这个 operation 是 setf 或 slot-makunbound, 任何值都会被调用者忽略.

            如果这个 operation 是 slot-value, 只有主要的值会被调用者使用, 而其他所有值都会被忽略.

            如果这个 operation 是 slot-boundp, 任何这个方法主要值的 boolean 等价物可能被使用, 而其他所有值都会被忽略.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        这个 slot-missing 默认方法发出一个 error 类型的错误.

* 也见(See Also):

        defclass, slot-exists-p, slot-value

* 注意(Notes):

        这个参数的集合 (包括这个实例的类) 有助于为 slot-missing 定义元类方法. 


### <span id="SGF-SLOT-UNBOUND">标准广义函数 SLOT-UNBOUND</span>

* 语法(Syntax):

      slot-unbound class instance slot-name => result*

* 方法签名(Method Signatures):

      slot-unbound (class t) instance slot-name

* 参数和值(Arguments and Values):

        class---这个 instance 的类.
        instance---尝试去读取未绑定的槽所在的实例.
        slot-name---未绑定槽的名字.
        result---一个对象.

* 描述(Description):

        当元类为 standard-class 的一个实例的一个未绑定的槽被读取时, 广义函数 slot-unbound 会被调用. 这个默认方法会发出一个 unbound-slot 类型的错误. 这个 unbound-slot 状况的名称槽被初始化为这个违规变量的名字, 而这个 unbound-slot 状况的实例槽被初始化为这个违规实例.

        广义函数 slot-unbound 不旨在被程序员调用. 程序员可以为它写方法. 函数 slot-unbound 只会被 slot-value 间接调用.

        如果 slot-unbound 返回, 只有主要值会被调用者使用, 其他所有值都会被忽略.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        这个 slot-unbound 上的默认方法发出一个 unbound-slot 类型的错误.

* 也见(See Also):

        slot-makunbound

* 注意(Notes):

        如果为一个槽指定 :initform 表达式形式或这没有设置槽的值, 或者在那个槽上调用了 slot-makunbound, 那么可能出现一个未绑定的槽. 


### <span id="F-SLOT-VALUE">函数 SLOT-VALUE</span>

* 语法(Syntax):

        slot-value object slot-name => value

* 参数和值(Arguments and Values):

        object---一个对象.
        name---一个符号.
        value---一个对象.

* 描述(Description):

        函数 slot-value 返回这个 object 中名为 slot-name 的槽的值. 如果这里没有名为 slot-name 的槽, 就会调用 slot-missing. 如果这个槽是未绑定的, slot-unbound 就会被调用.

        宏 setf 可以和 slot-value 一起使用来改变一个槽的值.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果尝试去读取一个槽但是没有名为 slot-name 存在于这个对象中, slot-missing 会像下面这样被调用:

        (slot-missing (class-of instance)
                      instance
                      slot-name
                      'slot-value)

        (如果 slot-missing 被调用, 它的主要返回值被 slot-value 返回.)

        如果尝试去写入一个槽但是没有名为 slot-name 存在于这个对象中, slot-missing 会像下面这样被嗲用:

        (slot-missing (class-of instance)
                      instance
                      slot-name
                      'setf
                      new-value)

        (如果这个情况下 slot-missing 返回了, 任何值都会被忽略.)

        这个特殊行为依赖于 object 的元类. 如果实例的元类为 standard-class 那么从不发出一个错误. 如果实例的元类为 built-in-class 那么总是发出一个错误. 如果实例有着任何其他元类那么结果是未定义的--在这个情况下一个错误可能或可能不会发出. 特别要注意的是, 状况(condition)和结构体(structure)的行为没有指定.

* 也见(See Also):

        slot-missing, slot-unbound, with-slots

* 注意(Notes):

        虽然没有具体实现被要求做这个, 但是强烈鼓励实现者通过使用元对象协议中描述的函数 slot-value-using-class 来实现函数 slot-value.

        具体实现可能通过编译 slot-value 为内联的(inline)来优化它. 


### <span id="SGF-METHOD-QUALIFIERS">标准广义函数 METHOD-QUALIFIERS</span>

* 语法(Syntax):

        method-qualifiers method => qualifiers

* 方法签名(Method Signatures):

        method-qualifiers (method standard-method)

* 参数和值(Arguments and Values):

        method---一个方法.
        qualifiers---一个合适的列表.

* 描述(Description):

        返回这个方法的限定符列表.

* 示例(Examples):

    ```LISP
    (defmethod some-gf :before ((a integer)) a)
    =>  #<STANDARD-METHOD SOME-GF (:BEFORE) (INTEGER) 42736540>
    (method-qualifiers *) =>  (:BEFORE)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        define-method-combination

* 注意(Notes): None. 


### <span id="SGF-NO-APPLICABLE-METHOD">标准广义函数 NO-APPLICABLE-METHOD</span>

* 语法(Syntax):

        no-applicable-method generic-function &rest function-arguments => result*

* 方法签名(Method Signatures):

        no-applicable-method (generic-function t) &rest function-arguments

* 参数和值(Arguments and Values):

        generic-function---一个没有找到可应用方法的广义函数.
        function-arguments---给这个 generic-function 的参数.
        result---一个对象.

* 描述(Description):

        当一个广义函数被调用而这个广义函数上没有方法可应用, 那么这个广义函数 no-applicable-method 就会被调用. 默认方法会发出一个错误.

        广义函数 no-applicable-method 不旨在被程序员调用. 程序员可能为它写方法.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        这个默认方法会发出一个 error 类型的错误.

* 也见(See Also):

* 注意(Notes): None. 


### <span id="SGF-NO-NEXT-METHOD">标准广义函数 NO-NEXT-METHOD</span>

* 语法(Syntax):

        no-next-method generic-function method &rest args => result*

* 方法签名(Method Signatures):

        no-next-method (generic-function standard-generic-function) (method standard-method) &rest args

* 参数和值(Arguments and Values):

        generic-function -- method 所属的广义函数.

        method -- 包含了对 call-next-method 的调用但是没有下一个方法的方法.

        args -- 给 call-next-method 的参数.

        result---一个对象.

* 描述(Description):

        当这里没有下一个方法时, 广义函数 no-next-method 被 call-next-method 调用.

        广义函数 no-next-method 不打算被程序员所调用. 程序员可以为它写方法.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        系统提供的 no-next-method 上的方法会发出一个 error 类型的错误.

* 也见(See Also):

        call-next-method

* 注意(Notes): None. 


### <span id="SGF-REMOVE-METHOD">标准广义函数 REMOVE-METHOD</span>

* 语法(Syntax):

        remove-method generic-function method => generic-function

* 方法签名(Method Signatures):

        remove-method (generic-function standard-generic-function) method

* 参数和值(Arguments and Values):

        generic-function---一个广义函数.
        method---一个方法.

* 描述(Description):

        广义函数 remove-method 通过修改这个 generic-function (如果必要的话)来从 generic-function 中移除一个方法.

        如果这个方法不是这个 generic-function 上的方法, 那么一定不会发出一个错误.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        find-method

* 注意(Notes): None. 


### <span id="SGF-MAKE-INSTANCE">标准广义函数 MAKE-INSTANCE</span>

* 语法(Syntax):

        make-instance class &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

        make-instance (class standard-class) &rest initargs
        make-instance (class symbol) &rest initargs

* 参数和值(Arguments and Values):

        class---一个类, 或者命名一个类的符号.
        initargs---一个初始化参数列表.
        instance---一个类 class 的新的实例.

* 描述(Description):

        广义函数 make-instance 创建并返回这个给定 class 的新的实例.

        如果选择了上述方法中的第二个, 那个方法在 (find-class class) 和 initargs 上调用 make-instance.

        初始化参数在 make-instance 中被检测.

        广义函数 make-instance 按章节 7.1 (Object Creation and Initialization) 中所描述的那样被使用.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果任何一个初始化参数没有被有效声明, 会发出一个 error 类型的错误.

* 也见(See Also):

        defclass, class-of, allocate-instance, initialize-instance, Section 7.1 (Object Creation and Initialization)

* 注意(Notes): None. 


### <span id="SGF-MAKE-INSTANCES-OBSOLETE">标准广义函数 MAKE-INSTANCES-OBSOLETE</span>

* 语法(Syntax):

        make-instances-obsolete class => class

* 方法签名(Method Signatures):

        make-instances-obsolete (class standard-class)
        make-instances-obsolete (class symbol)

* 参数和值(Arguments and Values):

        class---一个类指定符.

* 描述(Description):

        函数 make-instances-obsolete 具有启动更新 class 的实例的过程的效果. 在更新期间, 广义函数 update-instance-for-redefined-class 会被调用.

        当 defclass 被用来重定义一个已存在的标准类并且在一个实例中可访问的局部槽的集合被改变或者存储中的槽的顺序被改变, 广义函数 make-instances-obsolete 会被系统自动调用. 它也可以被用户显式调用.

        如果选择了上述的第二个方法, 那个方法在 (find-class class) 上调用 make-instances-obsolete.

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        update-instance-for-redefined-class, Section 4.3.6 (Redefining Classes)

* 注意(Notes): None. 


### <span id="SGF-MAKE-LOAD-FORM">标准广义函数 MAKE-LOAD-FORM</span>

* 语法(Syntax):

        make-load-form object &optional environment => creation-form[, initialization-form]

* 方法签名(Method Signatures):

        make-load-form (object standard-object) &optional environment
        make-load-form (object structure-object) &optional environment
        make-load-form (object condition) &optional environment
        make-load-form (object class) &optional environment

* 参数和值(Arguments and Values):

        object---一个对象.
        environment---一个环境对象.
        creation-form---一个表达式形式.
        initialization-form---一个表达式形式.

* 描述(Description):

        广义函数 make-load-form 创建并返回一个或两个表达式形式, 一个 creation-form 和一个 initialization-form, 这个启用 load 去构造一个 object 等价的对象. Environment 是一个和这个表达式形式被处理时所在词法环境对应的环境对象.

        文件编译器调用 make-load-form 来处理字面化对象确定的类; 见章节 3.2.4.4 (Additional Constraints on Externalizable Objects).

        符合规范的程序可能直接调用 make-load-form, 提供的 object 是 standard-object, structure-object, 或 condition 的一个普通实例.

        这个创建表达式形式(creation-form)是一个在 load 时求值并且应该返回一个和 object 等价的对象的表达式形式. 这个等价的确切意义取决于对象的类型以及为 make-load-form 定义方法的程序员; 见章节 3.2.4 (Literal Objects in Compiled Files).

        这个初始化表达式形式(initialization-form)是一个在 load 时求值并且应该执行这个对象的进一步初始化的表达式形式. 这个初始化表达式形式返回的值会被忽略. 如果 make-load-form 只返回一个值, 初始化表达式形式就是 nil, 它是没有效果的. 如果 object 作为一个常量出现在初始化表达式形式中, 在 load 时它会被创建表达式形式构造的等价对象所替代; 这就是进一步的初始化如何获得对对象的访问.

        不管是 creation-form 还是 initialization-form 都可能包含对任何可外部化对象的引用. 然而, 在创建表达式形式中, 这里一定不能有任何循环依赖. 一个循环依赖的示例就是, 当这个对象 X 的创建表达式形式包含了对对象 Y 的一个引用, 并且对象 Y 的创建表达式形式包含了对对象 X 的引用. 初始化表达式形式不受任何对循环依赖的限制, 这就是初始化表达式形式存在的原因; 见下面环状数据结构.

        一个对象的创建表达式形式总是在这个对象的初始化表达式形式之前被求值. 当创建表达式形式或是初始化表达式形式引用了其他在这个编译的文件中之前没有被引用的对象时, 编译器保证在求值这些引用表达式形式之前所有这些引用的对象都已经被创建. 当引用的对象是文件编译器使用 make-load-form 处理的类型时, 这个就涉及到求值为它返回的创建表达式形式. (这就是禁止在创建表达式形式中循环引用的原因).

        每个初始化表达式形式在它关联的创建表达式形式之后尽快被求值, 由数据流决定. 如果一个对象的初始化表达式形式没有引用出现在该文件中且之前没有被引用并且被文件编译器使用 make-load-form 处理的任何其他对象, 初始化表达式形式会在创建表达式形式之后被立即求值. 如果一个创建或初始化表达式形式 F 确实包含了对这样一个对象的引用, 这些其他对象的创建表达式形式在 F 之前被求值, 并且这些其他对象的初始化表达式形式也会在 F 之前被求值, 不管它们是否依赖 F 创建和初始化的对象. 在这些规则不能唯一确定一个在两个创建/初始化表达式形式之间的顺序的地方, 求值的顺序是未指定的.

        在这些创建和初始化表达式形式要被求值时, 对象可能处于一个未初始化状态, 类似一个对象在被 allocate-instance 创建和被 initialize-instance 完全处理之间的状态. 程序员为 make-load-form 写方法必须关注操纵的对象不依赖没有被初始化的槽.

        load 是否在表达式形式上调用 eval 或者执行某个其他有等价效果的操作是取决于具体实现的. 比如, 这些表达式形式可能被转成不同但是等价的表达式形式然后被求值, 它们可能被编译并且产生的函数被 load 调用, 或者它们可能被一个特殊目的的有别于 eval 的函数所解释. 所需要的只是效果和求值这些表达式形式等价.

        如果一个类在 environment 中有特定的名字, 那么这个在 class 上特化的方法返回一个使用该类的名字的创建表达式形式, 如果没有一个特定的名字就会发出一个 error 类型的错误. 这个创建表达式形式的求值使用这个名字去找到这个名字对应的类, 就像是通过调用 find-class 一样. 如果这个名字的一个类还没有被定义, 那么一个类可能以一种依赖于具体实现的方法被推断. 如果一个类不能被求值这个创建表达式形式作为结果返回, 那么就会发出一个 error 类型的错误.

        不管是符合规范的实现还是符合规范的程序都可能进一步特化 make-load-form.

* 示例(Examples):

    ```LISP
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
    ```

        在上述示例中, 一个 obj 等价的实例通过使用它的两个槽的值被重新构建. 第三个槽的值由那两个值得到.

        在那个示例中写 make-load-form 方法的另一种方式是使用 make-load-form-saving-slots. 它产生的代码可能和上面展示的 make-load-form 方法产生稍微不同的结果, 但是运行效果是一样的. 比如:

    ```LISP
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
    ```

        在下面这个示例中, my-frob 的实例以某种方式被 "捕捉(interned)". 通过使用名称槽作为键来搜索已存在的对象来重新构建一个等价实例. 在这个情况下如果没有找到已存在的对象, 程序员选择创建一个新对象; 或者在这种情况下可能会发出一个错误.

    ```LISP
    (defclass my-frob ()
        ((name :initarg :name :reader my-name)))
    (defmethod make-load-form ((self my-frob) &optional environment)
      (declare (ignore environment))
      `(find-my-frob ',(my-name self) :if-does-not-exist :create))
    ```

        在下面这个示例中, 被转储的数据结构是环状的, 因为每个 parent 有着它的 children 的一个列表并且每个 child 有一个指回它的 parent 的引用. 如果在一个这样结构的对象上调用 make-load-form, 创建表达式形式创建一个等价对象并且填充在 children 槽中, 它强制进行它的 children, grandchildren, 等等的等价对象的创建. 在这个时候没有 parent 的槽被填充. 这个初始化表达式形式填充这个 parent 的槽, 如果等价对象没有被创建, 它强制创建这个 parent 的等价对象. 因此整个树在加载时被重新创建. 在编译时, make-load-form 对于这个树中的每个对象被调用一次. 所有创建表达式形式都被求值, 以依赖于具体实现的顺序, 然后所有初始化表达式形式被求值, 也按照依赖于实现的顺序.

    ```LISP
    (defclass tree-with-parent () ((parent :accessor tree-parent)
                                    (children :initarg :children)))
    (defmethod make-load-form ((x tree-with-parent) &optional environment)
      (declare (ignore environment))
      (values
        ;; creation form
        `(make-instance ',(class-of x) :children ',(slot-value x 'children))
        ;; initialization form
        `(setf (tree-parent ',x) ',(slot-value x 'parent))))
    ```

        在下面示例中, 被转储的数据结构没有特殊属性并且一个等价结构可以简单地通过重新构建槽的内容来重新构建.

    ```LISP
    (defstruct my-struct a b c)
    (defmethod make-load-form ((s my-struct) &optional environment)
        (make-load-form-saving-slots s :environment environment))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        在 standard-object, structure-object, and condition 上指定的所有这方法都会发出一个 error 类型的错误.

        在一个系统类的普通实例上调用 make-load-form 是发出一个错误还是返回创建和初始化表达式形式, 取决于具体实现.

* 也见(See Also):

        compile-file, make-load-form-saving-slots, Section 3.2.4.4 (Additional Constraints on Externalizable Objects) Section 3.1 (Evaluation), Section 3.2 (Compilation)

* 注意(Notes):

        在特殊情况下文件编译器调用 make-load-form 在章节 3.2.4.4 (Additional Constraints on Externalizable Objects) 中详述.

        某些具体实现可能为定义被指定为系统类的类的子类提供工具. (一些候选项包括 generic-function, method, 还有 stream). 这样的具体实现应该记录文件编译器在作为字面化对象遇到时如何处理这样的类的实例, 并且应该记录任何和 make-load-form 相关的方法. 


### <span id="F-MAKE-LOAD-FORM-SAVING-SLOTS">函数 MAKE-LOAD-FORM-SAVING-SLOTS</span>

* 语法(Syntax):

        make-load-form-saving-slots object &key slot-names environment
        => creation-form, initialization-form

* 参数和值(Arguments and Values):

        object---一个对象.
        slot-names---一个列表.
        environment---一个环境对象.
        creation-form---一个表达式形式.
        initialization-form---一个表达式形式.

* 描述(Description):

        返回在求值时不执行初始化表达式形式来构造和 object 等价的对象的表达式形式. 这个新的对象中和 object 对应初始化槽的那些槽使用来自 object 的值来初始化. 在 object 中没有初始化的槽在新的对象中不会被初始化. make-load-form-saving-slots 对任何 standard-object 或 structure-object 的实例都起作用.

        Slot-names 是要保留的槽的名称列表. 如果没有提供 slot-names, 它的值就是所有局部槽.

        make-load-form-saving-slots 返回两个值, 因此它可以处理环状结构. 在一个应用中结果是否有用取决于这个对象 object 的类型和槽的内容是否完全捕捉了这个对象状态的应用意义.

        Environment 是这些表达式形式被处理时所处的环境.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        make-load-form, make-instance, setf, slot-value, slot-makunbound

* 注意(Notes):

        make-load-form-saving-slots 在用户编写的 make-load-form 方法中是很有用的.

        当这个 object 是 standard-object 的一个实例时, make-load-form-saving-slots 返回一个调用 allocate-instance 的创建表达式形式和一个包含对 slot-value 的 setf 和 slot-makunbound 的调用的初始化表达式形式, 尽管事实上可能使用其他类似效果的函数. 


### <span id="M-WITH-ACCESSORS">宏 WITH-ACCESSORS</span>

* 语法(Syntax):

        with-accessors (slot-entry*) instance-form declaration* form*
        => result*

        slot-entry::= (variable-name accessor-name) 

* 参数和值(Arguments and Values):

        variable-name---一个变量名字; 不求值.
        accessor-name---一个函数名; 不求值.
        instance-form---一个表达式形式; 求值.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式的 progn.
        results---这个 forms 返回的值.

* 描述(Description):

        创建一个词法环境, 在这里 slot-entry 指定的那些槽通过它们的访问器是词法上可用的, 就像它们是变量一样. 宏 with-accessors 调用合适的访问器来访问 slot-entry 指定的那些槽. 不管 setf 还是 setq 都可以被用来设置槽的值.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By):

        defclass

* 异常情况(Exceptional Situations):

        如果任何 accessor-name 不是这个实例的一个访问器的名字, 那么结果是未定义的.

* 也见(See Also):

        with-slots, symbol-macrolet

* 注意(Notes):

        这个表达式形式的一个 with-accessors 表达式:

        (with-accessors (slot-entry1 ... slot-entryn) instance-form form1 ... formk)

        展开为下面这个的等价体

        (let ((in instance-form))
          (symbol-macrolet (Q1 ... Qn) form1 ... formk))

        其中 Qi 是

        (variable-namei () (accessor-namei in))


### <span id="M-WITH-SLOTS">宏 WITH-SLOTS</span>

* 语法(Syntax):

        with-slots (slot-entry*) instance-form declaration* form*
        => result*

        slot-entry::= slot-name | (variable-name slot-name) 

* 参数和值(Arguments and Values):

        slot-name---一个槽的名字; 不求值.
        variable-name---一个变量名; 不求值.
        instance-form---一个表达式形式; 求值来产生这个实例.
        instance---一个对象.
        declaration---一个 declare 表达式; 不求值.
        forms---一个隐式的 progn.
        results---这个 forms 返回的值.

* 描述(Description):

        宏 with-slots 为引用这个 instance 实例中名为给定 slot-names 的槽建立一个词法环境就好像它们是变量一样. 在这样一个上下文中这个槽的值可以通过使用它们的槽名来指定, 就好像它们是一个词法上绑定的变量. 不管是 setf 还是 setq 都可以被用于设置这个槽的值.

        宏 with-slots 把这个槽名作为变量的出现转化为一个对 slot-value 的调用.

* 示例(Examples):

    ```LISP
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
    ```

* 受此影响(Affected By):

        defclass

* 异常情况(Exceptional Situations):

        如果任何的 slot-name 都不是这个 instance 实例中槽的名字, 那么结果是未定义的.

* 也见(See Also):

        with-accessors, slot-value, symbol-macrolet

* 注意(Notes):

        一个 with-slots 表达式 of the form:

        (with-slots (slot-entry1 ... slot-entryn) instance-form form1 ... formk)

        展开为下面这个的等价体

        (let ((in instance-form))
          (symbol-macrolet (Q1 ... Qn) form1 ... formk))

        其中 Qi 是

        (slot-entryi () (slot-value in 'slot-entryi))

        如果 slot-entryi 是一个符号的话就是

        (variable-namei () (slot-value in 'slot-namei))

        如果 slot-entryi 是一个表达式形式, 那么就是

        (variable-namei 'slot-namei))


### <span id="M-DEFCLASS">宏 DEFCLASS</span>

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

        Class-name---一个非 nil 符号.
        Superclass-name--一个非 nil 符号.
        Slot-name--一个符号. 这个 slot-name 参数是一个你符号, 它用作一个变量名是句法上有效的.
        Reader-function-name---一个非 nil 符号. 可以为一个给定的槽提供超过一个 :reader.
        Writer-function-name---一个广义函数名字. 可以为一个给定的槽提供超过一个 :writer.
        Reader-function-name---一个非 nil 符号. 可以为一个给定的槽提供超过一个 :accessor.
        Allocation-type---(member :instance :class). 可以为一个给定的槽提供最多一个 :allocation.
        Initarg-name---一个符号. 可以为一个给定的槽提供超过一个 :initarg.
        Form---一个表达式形式. 可以为一个给定的槽提供最多一个 :init-form.
        Type-specifier---一个类型指定符. 可以为一个给定的槽提供最多一个 :type.
        Class-option--- 指的是整个类或类的所有槽.
        Initarg-list---一个依次为初始化参数名字和默认初始化值表达式形式的列表. 可以提供最多一个 :default-initargs.
        Class-name---一个非 nil 符号. 可以提供最多一个 :metaclass can be supplied once at most.
        new-class---新的类对象.

* 描述(Description):

        宏 defclass 定义一个新的已命名的类. 它把新的类对象作为它的结果返回.

        defclass 的语法为给槽指定初始化参数, 为槽指定默认初始化值, 还有为给读取和写入槽的值自动生成指定广义函数的方法提供选项. 默认没有 reader 或 writer 函数被定义; 它们的生成必须是显式地请求. 然而, 槽可以总是通过使用 slot-value 来访问.

        定义一个新的类同时也导致一个相同名字的类型被定义. 如果给定 object 对象的类是 class-name 命名的类或是 class-name 命名的类的子类, 那么断言 (typep object class-name) 返回 true. 一个类对象可以被用作一个类型指定符. 因此如果这个 object 对象的类是 class 自身或者 class 的子类, 那么 (typep object class) 返回 true.

        这个 class-name 参数指定这个新的类的特定的名字. 如果相同特定名字的一个类已经存在并且那个类是 standard-class 的一个实例, 并且如果这个新的类定义的 defclass 表达式形式指定一个类 standard-class 的类, 那么这个存在的类会被重定义, 并且它的实例 (还有它的子类) 在它们被下一次访问时会被更新到新的定义. 关于详细信息, 见章节  4.3.6 (Redefining Classes).

        每个 superclass-name 参数指定这个新的类的一个直接超类. 如果这个超类列表是空的, 那么这个超类默认值取决于这个元类, 而对于 standard-object 默认值就是 standard-class.

        这个新的类会从它的每个直接超类, 直接超类的直接超类, 等等继承槽和方法. 关于槽和方法如何被继承的讨论, 见章节 4.3.4 (Inheritance).

        以下槽选项是可用的:

            这个 :reader 槽选项指定在名为 reader-function-name 广义函数上定义一个未限定的方法来读取这个给定槽的值.

            这个 :writer 槽选项指定在名为 writer-function-name 广义函数上定义一个未限定的方法来写入这个槽的值.

            这个 :accessor 槽选项指定在名为 reader-function-name 的广义函数上定义一个未限定的方法来读取这个给定槽的值并且在名为 (setf reader-function-name) 的广义函数上定义一个未限定符方法来和 setf 一起使用来修改这个槽的值.

            这个 :allocation 槽选项用于指定这个给定的槽要被存储的位置. 一个槽的存储可以位于每个实例或者这个类对象自身. 这个 allocation-type 参数的值可以是关键字 :instance 或者关键字 :class. 如果 :allocation 槽选项没有被指定, 结果和指定 :allocation :instance 一样.

                如果 allocation-type 是 :instance, 一个名为 slot-name 的局部槽会被分配在这个类的每个实例.

                如果 allocation-type 是 :class, 这个给定名字的共享槽会被分配在这个 defclass 表达式形式创建的类对象中. 这个槽的值被这个类的所有实例所共享. 如果一个类 C1 定义了这样一个共享槽, 任何 C1 的子类 C2 会共享同一个槽除非这个 C2 的 defclass 表达式形式指定了一个相同名字的槽或者这里有一个在 C2 的优先级列表中优先于 C1 并且定义了相同名字的槽的 C2 的超类.

            这个 :initform 槽选项被用于提供在这个槽的初始化中使用的默认初始值表达式形式. 这个表达式形式在每次被用来初始化这个槽时都会被求值. 这个表达式形式被求值所在词法环境是defclass 表达式形式被求值所在的词法环境. 注意这个词法环境既引用了变量也引用了函数. 对于局部槽, 动态环境是 make-instance 被调用所在的动态环境; 对于共享槽, 动态环境是 defclass 表达式形式被求值所在的动态环境. 见章节 7.1 (Object Creation and Initialization).

            没有具体实现被允许去扩展 defclass 的语法来允许 (slot-name form) 作为一个 (slot-name :initform form) 的简写.

            这个 :initarg 槽选项声明一个名为 initarg-name 的初始化参数并且指定这个初始化参数初始化给定的槽. 在对 initialize-instance 的调用中如果这个初始化参数有一个值, 那么就会被存储到给定的槽中, 并且, 如果存在这个槽的 :initform 槽选项, 就不会求值. 如果没有为一个给定的槽指定有值的初始化参数, 而指定了 :initform 槽选项的话, 就根据这个槽选项来初始化这个槽.

            这个 :type 槽选项指定这个槽的内容总是为指定的数据类型. 它有效地声明应用到这个类的一个对象的 reader 广义函数的结果类型. 尝试去存储一个不符合一个槽的类型的值到这个槽中的后果是未定义的. 这个 :type 槽选项在章节 7.5.3 (Inheritance of Slots and Slot Options) 中被进一步讨论.

            这个 :documentation 槽选项为这个槽提供一个文档字符串. :documentation 提供给一个槽最多一次.

        每个类选项是一个把这个类当作整体的选项. 以下类选项是可用的:

            这个 :default-initargs 类选项后面跟着一个依次为初始化参数名和默认初始化值表达式形式的列表. 如果这些初始化参数中的任何一个没有出现在提供给 make-instance 的初始化列表中, 对应的默认初始值表达式形式就会被求值, 并且这个初始化参数名字和这个表达式形式的值会在这个实例被创建前被添加到这个初始化参数列表的末尾; 见章节 7.1 (Object Creation and Initialization). 默认初始值表达式形式在每次被使用时都会求值. 这个表达式形式被求值时所在词法环境是 defclass 表达式形式被求值时所在词法环境. 而动态环境是 make-instance 被调用时所处的动态环境. 如果一个初始化参数名字在一个 :default-initargs 槽选项中出现不止一次, 就会发出一个错误.

            这个 :documentation 类选项导致一个文档字符串被绑定到这个类对象, 以及 type 种类的 class-name 上. :documentation 最多只能被提供一次.

            这个 :metaclass 类选项用于指定这个要被定义的类的实例有着和系统提供的默认值(类 standard-class)不同的元类.

        注意以下这些标准类的 defclass 的规则:

            一个类的超类不需要在这个类的 defclass 表达式形式被求值前被定义.

            一个类的所有超类必须在这个类的一个实例被创建前被定义.

            一个类必须在它被用作 defmethod 表达式形式中的参数指定符之前被定义.

        对象系统可以扩展, 以覆盖不遵守这些规则的情况.

        某些槽选项被一个类从它的超类中继承下来, 而某些可以通过提供一个局部槽描述来遮蔽或修改. 除了 :default-initargs 以外没有其他类选项被继承. 关于槽和槽选项如何被继承的详情, 见章节 7.5.3 (Inheritance of Slots and Slot Options).

        给 defclass 的选项可以被扩展. 如果一个具体实现发现一个类选项或一个槽选项没有被本地实现, 那么这个具体实现就需要去发出一个错误.

        为一个槽指定超过一个的 reader, writer, accessor, 或初始化参数是有效的. 没有其他槽选项可以在单个槽描述中出现超过一次, 否则就发出一个错误.

        如果没有为一个槽指定 reader, writer, 或 accessor, 这个槽只能通过函数 slot-value 来访问.

        如果一个 defclass 表达式形式作为一个顶层表达式形式出现, 编译器必须使这个类名在后面的声明中(就像 deftype)被识别为一个有效的类型名字, 在 defmethod 的参数特化符中被识别为一个有效的类名并且可以用作后面的 defclass 的 :metaclass 选项. 当 find-class 的 environment 参数是作为一个宏的环境参数接收到的值时, 编译器必须使可用的类定义能够被 它返回.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这里有任何槽名字重复, 就会发出一个 program-error 类型的错误.

        如果一个初始化参数名字不止一次出现在 :default-initargs 类选项中, 就会发出一个 program-error 类型的错误.

        如果在单个槽描述中下面的任何一个槽选项出现超过一次, 就会发出一个 program-error 类型的错误: :allocation, :initform, :type, :documentation.

        如果具体实现发现一个类选项或一个槽选项没有被本地实现, 所有这样的实现都需要去发出一个 program-error 类型的错误.

* 也见(See Also):

        documentation, initialize-instance, make-instance, slot-value, Section 4.3 (Classes), Section 4.3.4 (Inheritance), Section 4.3.6 (Redefining Classes), Section 4.3.5 (Determining the Class Precedence List), Section 7.1 (Object Creation and Initialization)

* 注意(Notes): None. 


### <span id="M-DEFGENERIC">宏 DEFGENERIC</span>

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

        function-name---一个函数对象.
        generic-function-class---命名一个类的非 nil 符号.
        gf-declaration---一个 optimize 声明指定符; 不允许其他声明指定符.
        gf-documentation---一个字符串; 不求值.
        gf-lambda-list---一个广义函数 lambda 列表.
        method-class---命名一个类的非 nil 符号.
        method-combination-argument---一个对象.
        method-combination-name---命名一个方法组合类型的符号.
        method-qualifiers, specialized-lambda-list, declarations, documentation, forms---as per defmethod.
        new-generic---这个广义函数对象.
        parameter-name---在 lambda-list 中命名一个必要参数的符号. (如果 :argument-precedence-order 选项被指定, 在 lambda-list 中的每个必要参数必须必须被作为 parameter-name 准确使用一次.)

* 描述(Description):

        宏 defgeneric 被用于定义一个广义函数或指定或用来指定属于整个广义函数的选项和声明.

        如果 function-name 是一个列表那么它必须是表达式形式 (setf symbol). 如果 (fboundp function-name) 是 false, 一个新的广义函数就会被创建. 如果 (fdefinition function-name) 是一个广义函数, 那个广义函数就会被修改. 如果 function-name 命名了一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

        这个 defgeneric 宏的效果就好像执行了下面三步: 首先, 通过前面的 defgeneric 表达式形式定义的方法会被移除; 其次, ensure-generic-function 会被调用; 最终, 通过当前 defgeneric 表达式形式指定的方法会被添加到这个广义函数中.

        每个 method-description 在这个广义函数上定义一个方法. 每个方法的 lambda 列表必须和 gf-lambda-list 选项指定的 lambda 列表一致. 如果没有指定方法描述并且相同名字的广义函数不存在, 一个没有方法的广义函数会被创建.

        defgeneric 的 gf-lambda-list 参数指定了这个广义函数上的方法的 lambda 列表的外形. 在这个产生的广义函数上的方法必须有着和这个外形一致的 lambda 列表. 如果一个 defgeneric 表达式形式被求值并且这个广义函数的一些方法有着和这个广义函数中被给定的不一致的 lambda 列表, 就会发出一个错误. 关于方法一致性的详情, 见章节 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

        广义函数把所有给它的参数值传递给方法, 并且只有那些; 不支持默认值. 注意, 方法定义中的可选参数和关键字参数, however, 可以有默认初始值表达式形式并且可以使用 supplied-p 参数.

        以下选项是支持的. 除非另有说明, 一个给定选项只能出现一次.

            这个 :argument-precedence-order 选项在选定一个特定的方法时被用于指定这些必要参数在一个对这个广义函数的调用中被检查特异度的顺序. 每个必要参数, 依照在 gf-lambda-list 参数中指定的, 必须被作为一个 parameter-name 准确包含一次这样才能提供完整且无歧义的优先级顺序. 如果这个条件没有满足, 就会出现一个错误.

            这个 declare 选项被用于指定属于这个广义函数的声明.

            一个 optimize 声明指定符是允许的. 它指定了方法选择是否要对速度或空间进行优化, 但是它在方法上无效. 为了控制一个方法如何被优化, 一个 optimize 声明必须直接放置在 defmethod 表达式形式或方法描述中. 优化特性 speed 和 space 是这个标准要求的仅有的特性, 但是一个具体实现可以扩展这个对象系统来识别其他特性. 一个只有一种方法选择技术并且忽略 optimize 声明指定符的简单实现是有效的.

            special, ftype, function, inline, notinline, 以及 declaration 声明是不允许的. 个别具体实现可以去扩展这个 declare 选项来支持额外的声明. 如果一个具体实现注意到一个不支持的声明指定符并且在一个 declaration 全局声明中没有被声明为一个非标准声明标识符的名字, 它应该发出一个警告.

            这个 declare 选项可能被指定超过一次. 效果和这些声明指定符列表被一起追加到同一个列表中并指定单个的 declare 选项是一样的.

            这个 :documentation 参数是一个绑定给这个广义函数对象的文档字符串, 并且也绑定给 function 种类的 function-name.

            这个 :generic-function-class 选项可以被用来指定这个广义函数去拥有一个和系统提供的默认值(类 standard-generic-function)不一样的类. 这个 class-name 参数是一个可以是广义函数的类的类名. 如果 function-name 指定了一个有着不同的 :generic-function-class 参数值的已存在的广义函数并且这个新的广义函数类和旧的兼容, 那么 change-class 会被调用来修改这个广义函数的类; 否则就发出一个错误.

            这个 :method-class 选项被用于指定这个广义函数上的所有方法都有着和提供提供的默认值(类 standard-method)不同的类. 这个 class-name 参数是能够称为一个方法的类的类名.

            这个 :method-combination 选项后面跟着一个命名一个方法组合类型的符号. 跟随该符号的参数(如果有的话)依赖于方法组合的类型. 注意, 标准方法组合类型不支持任何参数. 然而, 通过 define-method-combination 的短表达式形式定义的所有方法组合的类型都接受一个名为 order 的可选参数, 默认为 :most-specific-first, 在这里一个 :most-specific-last 的值在不影响辅助方法的顺序下倒转主方法的顺序.

        这个 method-description 参数定义了和这个广义函数关联的方法. 在一个方法描述中的 method-qualifier 和 specialized-lambda-list 参数和 defmethod 中的是一样的.

        这个 form 参数指定这个方法的主体(body). 这个方法的主体闭合在一个隐式的 block 中. 如果 function-name 是一个符号, 这个 block 就具有和这个广义函数相同的名字. 如果 function-name 是这样的一个表达式形式 (setf symbol), 这个 block 的名字就是 symbol.

        具体实现可以去扩展 defgeneric 来包含其他选项. 如果一个具体实现发现一个选项没有被本地实现, 它就需要去发出一个错误.

        defgeneric 不需要去执行任何编译时的副作用. 特别是, 这些方法不会为了在编译时调用而被设置. 一个具体实现可以选择为了编译时错误检查的目的去存储关于这个广义函数的信息 (比如在调用上检查参数数量, 或者记录这个函数名的定义已经出现过).

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 function-name 命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个 program-error 类型的错误.

        每个必要参数, 如 gf-lambda-list 参数中所指定的, 必须被准确包含一次作为一个 parameter-name, 否则就会发出一个 program-error 类型的错误.

        通过一个 method-description 指定的每个方法的 lambda 列表必须和 gf-lambda-list 选项指定的 lambda 列表一致, 否则就会发出一个 error 类型的错误.

        如果一个 defgeneric 表达式形式被求值并且这个广义函数的某些方法拥有和这个 defgeneric 表达式形式中给定的不一致的 lambda 列表, 就会发出一个 error 类型的错误.

        一个给定的选项可能只能出现一次, 否则就会发出一个 program-error 类型的错误.

        如果 function-name 指定一个已存在的且有着不相同值的 :generic-function-class 参数的广义函数而这个新的广义函数类和旧的兼容, change-class 会被调用来改变这个广义函数的类; 否则就会发出一个 error 类型的错误.

        具体实现可以去扩展 defgeneric 来包含其他选项. 如果一个具体实现发现一个选项没有被本地实现, 它就需要去发出一个 program-error 类型的错误.

* 也见(See Also):

        defmethod, documentation, ensure-generic-function, generic-function, Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function)

* 注意(Notes): None. 


### <span id="M-DEFMETHOD">宏 DEFMETHOD</span>

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

        declaration---一个 declare 表达式; 不求值.
        documentation---一个字符串; 不求值.
        var---一个变量名.
        eql-specializer-form---一个表达式形式.
        Form---一个表达式形式.
        Initform---一个表达式形式.
        Supplied-p-parameter---variable name.
        new-method---the new method object.

* 描述(Description):

        宏 defmethod 在一个广义函数上定义方法.

        如果 (fboundp function-name) 是 nil, 就会创建一个广义函数, 其中采用默认的参数优先级顺序 (每个参数都比在它的参数列表中它右边的参数更具体), 广义函数的类 (类 standard-generic-function), 方法的类 (类 standard-method), 方法组合类型 (标准方法组合类型). 这个广义函数的 lambda 列表和这个被定义的方法一致; 如果这个 defmethod 表达式形式提及关键字参数, 广义函数的 lambda 列表也会提及 ..... key (但是没有关键字参数). 如果 function-name 命名一个普通函数, 一个宏, 或者一个特殊操作符, 就会发出一个错误.

        如果 function-name 当前命名一个广义函数, 这个方法的 lambda 列表必须和这个广义函数的一致. 如果这个状况没有被处理, 就会发出一个错误. 对于在这个上下文中的一致性定义, 见章节 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

        每个 method-qualifier 参数是一个被方法组合用于识别这个给定方法的对象. 方法组合类型可能会进一步限制方法限定符的作用. 标准方法组合类型允许非受限方法和单个限定符是关键字 :before, :after, 或 :around 之一的方法.

        这个 specialized-lambda-list 参数和一个普通 lambda 列表类似, 除了必要参数的名字被特化参数替代. 一个特化参数是表达式形式 (var parameter-specializer-name) 的列表. 只有必要参数可以被特化. 如果 parameter-specializer-name 是一个符号, 它就命名一个类; 如果它是一个列表, 它就是表达式形式 (eql eql-specializer-form). 这个参数特化符名字 (eql eql-specializer-form) 表示对应的参数必须和一个对象 eql, 这个对象是对于这个方法可应用的 eql-specializer-form 的值. 这个 eql-specializer-form 在这个 defmethod 宏展开被求值的时候被求值. 如果一个给定的必要参数没有指定参数特化符的名字, 那么这个参数特化符默认是类 t. 关于进一步讨论, 见章节 7.6.2 (Introduction to Methods).

        这个 form 参数指定这个方法的主体. 这个方法的主体被闭合在一个隐式的 block 中. 如果 function-name 是一个符号, 这个 block 具有和这个广义函数相同的名字. 如果 function-name 是表达式形式 (setf symbol), 那么这个 block 的名字是 symbol.

        这个被创建的方法的类是这个方法定义所在的广义函数的方法类选项给定的.

        如果这个广义函数已经有一个和要被定义的方法在参数特化符和限定符上一样的方法, defmethod 用现在要被定义的方法来替换已存在的那个. 关于这个上下文的一致性定义. 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

        这些参数特化符源自于章节 7.6.2 (Introduction to Methods) 中描述的参数特化符的名字.

        The expansion of the defmethod macro ``refers to'' each specialized parameter (见 declare 描述中的 ignore 的描述). 这个包括拥有显式参数特化符名字 t 的参数. 这个就意味着, 如果这个方法的主体没有引用一个特化的参数, 编译器不会发出警告, 而在这个方法的主体没有引用一个未特化参数时可能发出警告. 出于这个原因, 在这个上下文中一个在 t 上特化的参数并不完全等同于一个未特化的参数.

        在这个方法体头部的应用于这个方法的 lambda 变量的声明被当作是绑定声明, 它的作用域和对应绑定相同.

        在这个方法主体的头部应用于 call-next-method or next-method-p 的函数绑定的声明可应用于这个方法的主体表达式形式中对这个方法的引用. 任何更外部的名为 call-next-method 和 next-method-p 的函数的绑定, 以及和这样的绑定相关联的声明在这个方法主体表达式形式中都会被遮蔽.

        在这个方法主体的头部的自由声明的作用域是整个方法主体, 它包括任何隐式的局部方法定义但是不包括这些 lambda 变量的初始化表达式形式.

        defmethod 不需要执行任何编译时副作用. 特别是, 这个方法不会为了在编译期间调用而被设置. 一个具体实现可以选择为了编译时的错误检查去存储关于这个广义函数的信息 (比如在调用上检查参数数量, 或者记录这个函数名的定义已经出现过).

        Documentation 作为文档字符串关联给这个方法对象.

* 示例(Examples): None.

* 受此影响(Affected By):

        引用的广义函数的定义.

* 异常情况(Exceptional Situations):

        如果 function-name 命名一个普通函数, 一个宏, 或者一个特殊操作符, 那么就会发出一个 error 类型的错误.

        如果 function-name 当前命名一个广义函数, 这个方法的 lambda 列表必须和这个广义函数的一致, 否则就会发出一个 error 类型的错误.

* 也见(See Also):

        defgeneric, documentation, Section 7.6.2 (Introduction to Methods), Section 7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function), Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes): None. 


### <span id="A-FIND-CLASS">访问器 FIND-CLASS</span>

* 语法(Syntax):

        find-class symbol &optional errorp environment => class

        (setf (find-class symbol &optional errorp environment) new-class)

* 参数和值(Arguments and Values):

        symbol---一个符号.

        errorp---一个广义 boolean. 默认是 true.

        environment -- 和给宏展开函数的 &environment 参数一样并且被用于区分编译时和运行时环境. 这个 &environment 有着动态范围; 如果这个 &environment 参数在这个宏展开函数的动态范围之外被引用, 那么结果是未定义的.

        class---一个类对象, 或 nil.

* 描述(Description):

        返回在这个 environment 中 symbol 命名的类对象. 如果这里没有这样一个类, 如果 errorp 是 false 就返回 nil; 否则, 如果 errorp 是 true, 就会发出一个错误.

        这个和特定符号关联的类可以通过用 find-class 来使用 setf 去改变; 或者, 如果给 setf 的新的类是 nil, 这个类关联就会被移除 (但是这个类对象自身不会被影响). 如果用户尝试去改变或移除一个在这个标准中被定义为类型指定符的符号所关联的类, 结果是未定义的. 见章节 4.3.7 (Integrating Types and Classes).

        当使用 find-class 的 setf 时, 任何 errorp 参数会为了效果被求值, 但是它返回的任何值都会被忽略; 这个 errorp 参数首先被允许, 这样就可以使用这个 environment 参数.

        这个 environment 可能被用于区分编译时环境和运行时环境.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这里没有这样一个类并且 errorp 是 true, find-class 会发出一个 error 类型的错误.

* 也见(See Also):

        defmacro, Section 4.3.7 (Integrating Types and Classes)

* 注意(Notes): None. 


### <span id="LF-NEXT-METHOD-P">局部函数 NEXT-METHOD-P</span>

* 语法(Syntax):

        next-method-p <no arguments> => generalized-boolean

* 参数和值(Arguments and Values):

        generalized-boolean---一个广义的 boolean.

* 描述(Description):

        这样局部定义的函数 next-method-p 可以在一个方法定义表达式形式所定义的主体表达式形式中(而不是 lambda 列表)被用来确定是否存在下一个方法.

        函数 next-method-p 有着词法作用域和不确定的范围.

        这个 next-method-p 在全局环境中是否是 fbound 的是依赖于具体实现的; 然而, 这个 next-method-p 的重定义和遮蔽的限制和 COMMON-LISP 包中在全局环境中被 fbound 的符号一样. 尝试在一个方法定义表达式形式外部使用 next-method-p 的后果是未定义的.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        call-next-method, defmethod, call-method

* 注意(Notes): None. 


### <span id="LM-CALL-METHOD-MAKE-METHOD">局部宏 CALL-METHOD, MAKE-METHOD</span>

* 语法(Syntax):

        call-method method &optional next-method-list => result*

        make-method form => method-object

* 参数和值(Arguments and Values):

        method---一个方法对象, 或者一个列表 (见下方); 不求值.
        method-object---一个方法对象.
        next-method-list---一个方法对象的列表; 不求值.
        results---这个方法调用返回的值.

* 描述(Description):

        宏 call-method 被用于方法组合. 它隐藏了依赖于具体实现的方法如何被调用的细节. 宏 call-method 有词法作用域并且只能在生效方法表达式形式中被使用.

        call-method 在全局环境中是否为 fbound 是依赖于具体实现的; 然而, 在 call-method 的重定义和遮蔽上的限制和那些 COMMON-LISP 包中在全局环境里是 fbound 的符号一样. 尝试在一个生效方法表达式形式外部使用 call-method 的后果是.

        宏 call-method 调用指定的方法, 把参数还有 call-next-method 和 next-method-p 的定义提供给它. 如果这个 call-method 的调用在词法上 make-method 的内部, 参数是提供给那个方法的那些. 否则参数是提供给那个广义函数的那些. 这个 call-next-method 和 next-method-p 的定义依赖指定的 next-method-list.

        如果 method 是一个列表, 这个列表的第一个元素必须是 make-method 并且第二个元素必须是一个表达式形式. 这样一个列表指定了一个方法函数的主体为给定表达式形式的方法对象.

        Next-method-list 可以包含多个方法对象或列表, 这个列表的第一个元素必须是 make-method 并且第二个元素必须是一个表达式形式.

        那些是可以使用 make-method 的仅有的两处. 和 make-method 一起使用的那个 form 在空词法环境中被求值, 可以通过一个 call-method 的局部宏定义和不能从 COMMON-LISP-USER 包中访问的符号命名的绑定来扩展.

        对 method 可用的 call-next-method 会调用 next-method-list 中的第一个方法. 在那个方法中可用的 call-next-method 函数依次会调用 next-method-list 中的第二个方法, 以此类推, 直到后续方法的列表耗尽.

        如果没有提供 next-method-list, 对 method 可用的 call-next-method 函数发出一个 control-error 类型的错误并且对 method 可用的这个 next-method-p 函数返回 nil.

* 示例(Examples):

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        call-next-method, define-method-combination, next-method-p

* 注意(Notes): None. 


### <span id="LF-CALL-NEXT-METHOD">局部函数 CALL-NEXT-METHOD</span>

* 语法(Syntax):

        call-next-method &rest args => result*

* 参数和值(Arguments and Values):

        arg---一个对象.
        results---它调用的方法的返回值.

* 描述(Description):

        函数 call-next-method 可以在一个通过方法定义表达式形式定义的方法的主体表达式形式中(但不包括 lambda 列表)被用来调用下一个方法.

        如果没有下一个方法, 那么广义函数 no-next-method 会被调用.

        使用的方法组合的类型决定哪些方法可以调用 call-next-method. 标准方法组合类型允许 call-next-method 在主方法和 around 方法中被使用. 对于使用通过 define-method-combination 的短表达式形式定义的方法组合类型的方法, call-next-method 只能被用于 around 方法.

        当 call-next-method 以无参数的情况被调用, 它传递当前方法的原始参数给下一个方法. 不管是参数违约, 还是使用 setq, 或者使用与方法参数相同的名称重新绑定变量, 这些变量都不会影响 call-next-method 传递给它所调用的方法的值.

        当用参数调用 call-next-method 时, 下一个方法会用这些参数来调用.

        如果用参数调用了 call-next-method 但是省略了可选参数, 那么下一个被调用的方法省略那些参数.

        函数 call-next-method 返回任何下一个方法返回的值.

        函数 call-next-method 有着词法作用域和不确定的范围并且只能在一个通过方法定义表达式形式定义的方法的主体中被使用.

        call-next-method 在全局环境中是否为 fbound 是依赖于具体实现的; 然而, 在 call-next-method 的重定义和遮蔽上的约束和 COMMON-LISP 包里在全局环境中被 fbound 的方法一样. 尝试在一个方法定义表达式形式的外部去使用 call-next-method 的后果是不确定的.

* 示例(Examples): None.

* 受此影响(Affected By):

        defmethod, call-method, define-method-combination.

* 异常情况(Exceptional Situations):

        当给 call-next-method 提供参数时, 下面的规则必须被满足, 否则就会发出一个 error 类型的错误: 对于 call-next-method 的一个被改变的参数集的可应用方法有序集必须和这个广义函数的原始参数的可应用方法有序集相同. 错误检查的优化是可以的，但是它们不能改变 call-next-method 的语义.

* 也见(See Also):

        define-method-combination, defmethod, next-method-p, no-next-method, call-method, Section 7.6.6 (Method Selection and Combination), Section 7.6.6.2 (Standard Method Combination), Section 7.6.6.4 (Built-in Method Combination Types)

* 注意(Notes): None. 


### <span id="SGF-COMPUTE-APPLICABLE-METHODS">标准广义函数 COMPUTE-APPLICABLE-METHODS</span>

* 语法(Syntax):

        compute-applicable-methods generic-function function-arguments => methods

* 方法签名(Method Signatures):

        compute-applicable-methods (generic-function standard-generic-function)

* 参数和值(Arguments and Values):

        generic-function---一个广义函数.
        function-arguments---这个 generic-function 的参数列表.
        methods---一个方法对象列表.

* 描述(Description):

        给定一个 generic-function 和一个 function-arguments 集合, 函数 compute-applicable-methods 返回可应用于这些根据优先级顺序排序后的参数的方法集合. 见章节 7.6.6 (Method Selection and Combination).

* 受此影响(Affected By):

        defmethod

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        Section 7.6.6 (Method Selection and Combination)

* 注意(Notes): None. 


### <span id="M-DEFINE-METHOD-COMBINATION">宏 DEFINE-METHOD-COMBINATION</span>
<!--TODO role ??  qualifier patterns?? -->
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

        args-lambda-list---一个 define-method-combination 参数 lambda 列表.
        declaration---一个 declare 表达式形式; 不求值.
        description---一个格式化控制(format control).
        documentation---一个字符串; 不求值.
        forms---一个计算并返回指定这些方法如何组合的表达式形式的隐式的 progn, 这也就是说, 这个生效方法(effective method).
        generic-function-symbol---一个符号.
        identity-with-one-argument---一个广义 boolean.
        lambda-list---普通 lambda 列表.
        name---一个符号. 通常使用非关键字, 非 nil 的符号.
        operator---一个操作符. Name 和 operator 经常是同一个符号. 这是默认的, 但不是必须的.
        order---:most-specific-first 或 :most-specific-last; 求值的.
        predicate---一个命名一个单参数并返回一个广义 boolean 的函数的符号.
        qualifier-pattern---一个列表, 或者符号 *.
        required-p---一个广义 boolean.

* 描述(Description):

        宏 define-method-combination 被用于定义新的方法组合类型.

        这里有两个 define-method-combination 的表达式形式. 短表达式形式是一个简单的工具, 用于最常见的情况. 长表达式形式更强大但也更繁琐. 它类似于 defmacro , 其中的主体是一个表达式, 通常使用反引号, 那个计算一个表达式形式. 因此可以实现任意控制结构. 长表达式形式也允许任意方法限定符的处理.

        短表达式形式

            这个 define-method-combination 的短表达式形式的语法当它的第二个子表达式形式是非 nil 的符号或不存在时被识别. 当使用这个短表达式形式时, name 被定义为一个产生一个 Lisp 表达式形式(operator method-call method-call ...)的方法组合的类型 . 这个 operator 是一个符号, 它可以是一个函数, 宏, 或者特殊操作符的名字. 这个 operator 可以通过一个关键字选项来提供; 它默认为 name.

            以下是短表达式形式的关键字选项:

                这个 :documentation 选项被用于记录这个 method-combination 类型; 参见下面的长表达式形式的描述.

                这个 :identity-with-one-argument 选项在它的值为 true 时(默认是 false)启动一个优化. 如果这里只有一个可应用方法并且它是一个主方法, 那个方法当作生效方法并且 operator 不会被调用. 这个优化避免去创建一个新的生效方法并且避免了一个方法调用的开销. 这个选项被设计来和例如 progn, and, +, 和 max 这样的操作符一起使用.

                这个 :operator 选项指定这个操作符的名字. 这个操作符 operator 参数是一个可以为一个函数, 宏, 或特殊表达式形式的名字的符号.

            这些方法组合的类型必须一个方法一个限定符. 如果这里这里存在没有限定符或有着这个方法组合类型不支持的限定符, 就会发出一个错误.

            以这种方法定义的一个方法组合过程识别方法的两个角色. 一个方法, 当它的一个限定符是命名这个方法组合类型的符号时, 这个方法被定义为一个主方法. 至少一个主方法必须是可应用的, 否则就会发出一个错误. 一个限定符为 :around 的方法是一个辅助方法, 它的行为和标准方法组合类型中的 around 方法一样. 函数 call-next-method 只能被用于 around 方法中; 它不能被用于 define-method-combination 宏的短表达式形式定义的主方法中.

            以这种方法定义的一个方法组合过程接受一个名为 order 的可选参数, 它默认为 :most-specific-first. 一个 :most-specific-last 值在不影响辅助方法顺序的情况下倒转这个主方法的顺序.

            这个短表达式形式自动包括错误检查和 around 方法的支持.

            对于一个内建方法组合类型的讨论, 见章节 7.6.6.4 (Built-in Method Combination Types).

        长表达式形式

            define-method-combination 的长表达式形式语法在它的第二个子表达式形式为一个列表时被识别.

            这个 lambda-list 接受给 defgeneric 的 :method-combination 选项中的方法组合类型的名字后面的任何参数.

            一个方法组说明符的列表如下. 每个说明符选择可应用方法的子集来扮演特定的角色, 或者通过匹配某些模式的限定符, 或者通过断言来测试他们的限定符. 这些方法组说明符定义了所有可以和这个方法组合类型一起使用的方法限定符.

            每个 method-group-specifier 的 car 是一个命名一个变量的符号. 在 define-method-combination 的主体中的表达式形式的执行期间, 这个变量被绑定给这个方法组中的一个方法列表. 这个列表中的方法以 :order 选项指定的顺序出现.

            如果 qualifier-pattern 是一个符号那么它必须是 *. 如果一个方法的限定符列表和 一个 qualifier-pattern 是 equal 的(除了这个符号 * 在一个 qualifier-pattern 中匹配任何东西), 那么这个方法匹配这个 qualifier-pattern. 因此一个 qualifier-pattern 可以是以下之一: 空列表, 它匹配未受限方法; 符号 *, 它匹配所有方法; 一个真实的列表, 它匹配和这个列表长度相同数量限定符的方法, 而其中每个限定符匹配这个列表的元素; 或者一个以 * 结尾的点对列表(这个 * 匹配任何数量的额外限定符).

            每个可应用方法都是根据 qualifier-patterns 来检测并且以从左到右的顺序判断. 在一个 qualifier-pattern 匹配后或一个断言返回 true, 这个方法就成为对应方法组的一个成员并且不会做进一步检测. 因此如果一个方法可以是超过一个方法组的一个成员, 它只加入到第一个这样的组中. 如果一个方法组有超过一个 qualifier-pattern, 一个方法只需要返回这些 qualifier-patterns 中的一个就可以称为这个组的成员.

            一个 predicate 函数的名字可以出现在一个方法组的说明符中, 而不是  qualifier-patterns. 这个断言 predicate 被每一个没有被赋给一个更早的方法组的方法所调用; 它用一个参数来调用, 那个方法的限定符列表. 如果那个方法是这个方法组的一个成员, 那么这个 predicate 应该返回 true. 一个断言 predicate 可以和一个 qualifier-pattern 区分开来因为它是一个符号而不是 nil 或 *.

            如果这里有一个不属于任何方法组的可应用方法, 那么函数 invalid-method-error 会被调用.

            方法组说明符可以有关键字选项更在这个限定符模式或断言后. 关键字选项有别于额外限定符模式因为它们既不是列表也不是符号 *. 这些关键字选项如下:

                这个 :description 选项被用于提供这个方法组中方法作用的描述. 编程环境工具使用 (apply #'format stream format-control (method-qualifiers method)) 来打印这个描述, 预计会很简介. 这个关键字选项允许一个方法限定符的描述被定义在定义这个方法限定符的意义的相同模块中. 大部分情况下, format-control 不会包含任何 format 指令, 但是普遍上它们是可用的. 如果没有提供 :description, 会生成一个基于这个变量名和限定符模式还有这个方法组是否包含非受限方法的描述.

                这个 :order 选项指定了这些方法的顺序. 这个 order 参数是一个求值为 :most-specific-first 或 :most-specific-last 的表达式形式. 如果它求值为任何其他值, 就会发出一个错误. 如果没有提供 :order, 它默认为 :most-specific-first.

                这个 :required 选项指定这个方法组中是否至少需要一个方法. 如果它的值为 true 而这个方法组是空的 (也就是说, 没有可应用的方法匹配这个限定符模式或满足这个断言), 就会发出一个错误. 如果没有提供 :required, 它默认为 nil.

            这个方法组说明符的使用提供了一个方便的语法来选择方法, 来把它们划分给可能的角色, 并且去执行必要的错误检查. 在主体表达式形式中通过使用正常的 列表处理操作和函数 method-qualifiers 和 invalid-method-error 来执行方法的进一步过滤是可能的. 允许在方法组说明符中命名的变量上使用 setq, 也允许去绑定额外的变量. 绕开这个方法组说明符机制并在主体表达式形式中执行任何操作都是可能的. 这个通过写一个单个方法组来完成, 其中 * 作为它仅有的 qualifier-pattern; 这个变量接下来以最具体优先的顺序绑定给所有这些可应用方法的列表.

            主体表达式形式计算并返回指定这些方法如何组合的表达式形式, 换言之, 生效方法. 这个生效方法在空词法环境中被求值, 以 call-method 的局部宏定义和 COMMON-LISP-USER package 包中不可访问的符号命名的绑定为参数. 给定一个在由这些方法组说明符产生的其中一个列表中的方法对象和一个后续方法的列表, call-method 会调用那个方法, 这样 call-next-method 有可用的后续方法.

            当一个生效方法除了调用一个单一的方法之外没有其他效果, 一些具体实现采用一个优化, 使用这个单个的方法直接作为这个生效方法, 因此避免了创建一个新的生效方法的需要. 当生效方法表达式形式完全由一个 call-method 宏的调用组成, 它的第一个子表达式形式是一个方法对象而第二个子表达式形式是 nil 或未提供的, 那么这个优化就会被启用. 如果需要这种优化, 那么每个 define-method-combination 主体有责任去去除多余的 progn, and, multiple-value-prog1, 诸如此类的调用.

            列表 (:arguments . lambda-list) 可以在任何声明或文档字符串之前出现. 当这个方法组合类型执行一些特定的行为来作为组合的方法的一部分并且这个行为需要访问给这个广义函数的参数时, 这个表达式形式是很有用的. 每个通过 lambda-list 定义的参数变量被绑定到一个表达式形式, 这个表达式形式可以被插入到这个生效方法中. 当这个表达式形式在这个生效方法的执行期间被求值时, 它的值是给这个广义函数的对应参数; 在一个 setf 表达式形式中使用这样一个表达式形式作为一个 place 的后果是未定义的. 参数的匹配通过划分这个 :arguments lambda-list 还有广义函数的 lambda-list 为三个部分来计算: 必要参数, 可选参数, 还有关键字和剩余参数. 为特定调用提供的广义函数的参数也被划分为三个部分; 必要参数部分包括这个广义函数所拥有的必要参数, 可选参数部分包含了这个广义函数拥有的可选参数, 以及关键字/剩余参数部分包含了剩余的参数. :arguments lambda-list 的必要和可选部分的每个参数都在这些参数的对应部分中访问同一个位置的参数. 如果 :arguments lambda-list 的部分更短, 额外的参数就会被忽略. 如果这个 :arguments lambda-list 的部分更长, 超出的必要参数绑定给求值为 nil 的表达式形式而超出的可选参数绑定给它们的初始化表达式形式. :arguments lambda-list 中的关键字参数和剩余参数访问这些章节的关键字/剩余部分. 如果这个 :arguments lambda-list 包含了 &key, 它表现为就好像它也包含了 &allow-other-keys.

            另外, &whole var 可以被放置在 :arguments lambda-list 的第一个. 这个导致 var 被绑定给一个表达式形式, 这个表达式形式求值为一个提供给这个广义函数的所有参数的列表. 这个和 &rest 不同因为它访问所有这些参数, 不只是关键字/剩余参数.

            被这个主体检测到的错误的状况应该使用 method-combination-error 或 invalid-method-error 来报告; 这些函数添加任何必要的上下文信息给这个错误信息并且会发出一个合适的错误.

            这个主体表达式形式在这个 lambda 列表和方法组说明符创建的绑定中求值. 位于主体头部的声明直接位于 lambda 列表创建的绑定内部以及方法组变量绑定的外部. 因此方法组变量不能以这种方式声明. 但是 locally 可以在这个主体周围使用.

            在主体表达式形式中, generic-function-symbol 被绑定到这个广义函数对象.

            Documentation 作为一个文档字符串关联到 name (作为 method-combination 种类) 以及这个方法组合对象.

            注意, 两个有着相同特化符的方法, 但是限定符不同, 不会被章节 7.6.6 (Method Selection and Combination) 中描述方法选择和组合处理的步骤 2 中描述的算法所排序. 通常这两个方法在这个生效方法中扮演着不同的角色, 不管在那个步骤 2 的结果中如何被排序, 这个生效方法是相同的. 如果这两个方法扮演着相同的角色并且它们的顺序很重要, 就会发出一个错误. 这是在 define-method-combination 中匹配的限定符模式的一部分.

        如果一个 define-method-combination 表达式形式作为顶层表达式形式出现, 编译器必须是这个方法组合的名字在后续的 defgeneric 表达式形式中被识别为一个生效方法组合名字. 然而, 方法组合执行的时间不早于 define-method-combination 表达式形式被执行时, 并且可能在使用这个方法组合的广义函数执行的时候执行.

* 示例(Examples):

        define-method-combination 的长表达式形式的大多数例子也说明了作为声明方法组合工具的一部分提供的相关函数的使用.

    ```LISP
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
    ```

* 受此影响(Affected By): None.

* 副作用(Side Effects):

        编译器不需要去执行任何编译时的副作用.

* 异常情况(Exceptional Situations):

        用短表达式形式定义的方法组合类型需要每个方法一个限定符. 如果这里有一些可应用的方法没有限定符或者有着这个方法组合类型不支持的限定符, 就会发出一个 error 类型的错误. 至少一个主方法必须是可应用的否则就会发出一个 error 类型的错误.

        如果一个可应用的方法不属于任何方法组, 系统会发出一个 error 类型的错误, 表示这个方法对于使用的这个方法组合的种类是非法的.

        如果这个 :required 选项的值是 true 并且这个方法组是空的 (这也就是说, 没有可应用的方法匹配这个限定符模式或满足这个断言), 那么就会发出一个 error 类型的错误.

        如果 :order 选项求值为一个值而不是 :most-specific-first 或 :most-specific-last, 就会发出一个 error 类型的错误.

* 也见(See Also):

        call-method, call-next-method, documentation, method-qualifiers, method-combination-error, invalid-method-error, defgeneric, Section 7.6.6 (Method Selection and Combination), Section 7.6.6.4 (Built-in Method Combination Types), Section 3.4.11 (Syntactic Interaction of Documentation Strings and Declarations)

* 注意(Notes):

        这个 defgeneric 的 :method-combination 选项被用于指定一个广义函数应该使用一个特殊的方法组合类型. 给 :method-combination 的第一个参数是一个方法组合的名字而剩下的参数是这个类型的选项. 


### <span id="">标准广义函数 FIND-METHOD</span>

* 语法(Syntax):

        find-method generic-function method-qualifiers specializers &optional errorp
        => method

* 方法签名(Method Signatures):

        find-method (generic-function standard-generic-function) method-qualifiers specializers &optional errorp

* 参数和值(Arguments and Values):

        generic-function---一个广义函数.
        method-qualifiers---一个列表.
        specializers---一个里诶包.
        errorp---一个广义 boolean. 默认值是 true.
        method---一个方法对象, 或 nil.

* 描述(Description):

        广义函数 find-method 接受一个广义函数并且返回限定符和参数特化符与 find-method 参数中的 method-qualifiers 和 specializers 一致的方法对象. Method-qualifiers 包含了这个方法的方法限定符. 方法限定符的顺序是重要的. 对于在这个上下文中一致性的定义, 见章节 7.6.3 (Agreement on Parameter Specializers and Qualifiers).

        这个 specializers 参数包含了这个方法的参数特化符. 它必须和这个广义函数的必要参数的数量对应, 否则就会发出一个错误. 这个意味着为了获取一个给定 generic-function 上的默认方法, 需要给定一个元素为类 t 的列表.

        如果这里没有这样一个方法并且 errorp 是 true, find-method 会发出一个错误. 如果这里没有这样一个方法而 errorp 是 false, find-method 返回 nil.

* 示例(Examples):

    ```LISP
    (defmethod some-operation ((a integer) (b float)) (list a b))
    =>  #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
    (find-method #'some-operation '() (mapcar #'find-class '(integer float)))
    =>  #<STANDARD-METHOD SOME-OPERATION (INTEGER FLOAT) 26723357>
    (find-method #'some-operation '() (mapcar #'find-class '(integer integer)))
    >>  Error: No matching method
    (find-method #'some-operation '() (mapcar #'find-class '(integer integer)) nil)
    =>  NIL
    ```

* 受此影响(Affected By):

        add-method, defclass, defgeneric, defmethod

* 异常情况(Exceptional Situations):

        如果 specializers 参数不对应 generic-function 必要参数的数量, 就会发出一个 error 类型的错误.

        如果这里没有这样一个方法并且 errorp 是 true, find-method 发出一个 error 类型的错误.

* 也见(See Also):

        Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers)

* 注意(Notes): None. 


### <span id="SGF-ADD-METHOD">标准广义函数 ADD-METHOD</span>

* 语法(Syntax):

        add-method generic-function method => generic-function

* 方法签名(Method Signatures):

        add-method (generic-function standard-generic-function) (method method)

* 参数和值(Arguments and Values):

        generic-function---一个广义函数对象.
        method---一个方法对象.

* 描述(Description):

        广义函数 add-method 添加一个方法到广义函数.

        如果 method 和广义函数中一个已存在的方法在参数特化符和限定符上一致, 那个存在的方法就会被替换.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        method 的方法函数的 lambda 列表必须和广义函数的 lambda 列表一致, 否则就会发出一个 error 类型的错误.

        如果 method 是另一个广义函数的方法对象, 就会发出一个 error 类型的错误.

* 也见(See Also):

        defmethod, defgeneric, find-method, remove-method, Section 7.6.3 (Agreement on Parameter Specializers and Qualifiers)

* 注意(Notes): None. 


### <span id="SGF-INITIALIZE-INSTANCE">标准广义函数 INITIALIZE-INSTANCE</span>

* 语法(Syntax):

        initialize-instance instance &rest initargs &key &allow-other-keys => instance

* 方法签名(Method Signatures):

        initialize-instance (instance standard-object) &rest initargs

* 参数和值(Arguments and Values):

        instance---一个对象.
        initargs---一个默认初始化参数列表.

* 描述(Description):

        被 make-instance 调用来初始化新创建的实例. 这个广义函数用这个新的实例和默认初始化参数列表来调用.

        系统提供的 initialize-instance 主方法根据 initargs 和槽的 :initform 表达式形式的值来初始化实例 instance 的槽. 它通过使用以下参数调用广义函数 shared-initialize 来完成这个: 这个实例 instance, t (这个表示所有没有提供初始化参数的槽应该根据它们的 :initform 表达式形式来初始化), 还有 initargs.

        程序员可以为 initialize-instance 定义方法来指定在一个实例被初始化时采取的动作. 如果只定义了 alter 方法, 它们会在系统提供的用于初始化的主方法之后被运行, 因此不会影响到 initialize-instance 的默认行为.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        shared-initialize, make-instance, slot-boundp, slot-makunbound, Section 7.1 (Object Creation and Initialization), Section 7.1.4 (Rules for Initialization Arguments), Section 7.1.2 (Declaring the Validity of Initialization Arguments)

* 注意(Notes): None. 


### <span id="SGF-CLASS-NAME">标准广义函数 CLASS-NAME</span>

* 语法(Syntax):

        class-name class => name

* 方法签名(Method Signatures):

        class-name (class class)

* 参数和值(Arguments and Values):

        class---一个类对象.

        name---一个符号.

* 描述(Description):

        返回给定类的名字.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        find-class, Section 4.3 (Classes)

* 注意(Notes):

        如果 S 是一个符号,其中 S =(class-name C) 并且 C =(find-class S), 那么 S 是 C 的一个特有的名字. 关于进一步讨论, 见章节 4.3 (Classes).

        一个匿名类的名字是 nil. 


### <span id="SGF-SETF-CLASS-NAME">标准广义函数 (SETF CLASS-NAME)</span>

* 语法(Syntax):

        (setf class-name) new-value class => new-value

* 方法签名(Method Signatures):

        (setf class-name) new-value (class class)

* 参数和值(Arguments and Values):

        new-value---一个符号.
        class---一个类.

* 描述(Description):

        广义函数 (setf class-name) 设置一个类对象的名字.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        find-class, proper name, Section 4.3 (Classes)

* 注意(Notes): None. 


### <span id="F-CLASS-OF">函数 CLASS-OF</span>

* 语法(Syntax):

        class-of object => class

* 参数和值(Arguments and Values):

        object---一个对象.
        class---一个类对象.

* 描述(Description):

        返回这个直接实例为 object 的类.

* 示例(Examples):

    ```LISP
    (class-of 'fred) =>  #<BUILT-IN-CLASS SYMBOL 610327300>
    (class-of 2/3) =>  #<BUILT-IN-CLASS RATIO 610326642>
    
    (defclass book () ()) =>  #<STANDARD-CLASS BOOK 33424745>
    (class-of (make-instance 'book)) =>  #<STANDARD-CLASS BOOK 33424745>
    
    (defclass novel (book) ()) =>  #<STANDARD-CLASS NOVEL 33424764>
    (class-of (make-instance 'novel)) =>  #<STANDARD-CLASS NOVEL 33424764>

    (defstruct kons kar kdr) =>  KONS
    (class-of (make-kons :kar 3 :kdr 4)) =>  #<STRUCTURE-CLASS KONS 250020317>
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        make-instance, type-of

* 注意(Notes): None. 


### <span id="CT-UNBOUND-SLOT">状况类型 UNBOUND-SLOT</span>

* 类优先级列表(Class Precedence List):

        unbound-slot, cell-error, error, serious-condition, condition, t

* 描述(Description):

        这个有着未绑定槽的对象通过给 make-condition 的 :instanceinitialization 参数被初始化, 并且通过函数 unbound-slot-instance 被访问.

        这个格(cell)(见 cell-error)的名字是这个槽的名字t.

* 也见(See Also):

        cell-error-name, unbound-slot-object, Section 9.1 (Condition System Concepts) 


### <span id="F-UNBOUND-SLOT-INSTANCE">函数 UNBOUND-SLOT-INSTANCE</span>

* 语法(Syntax):

        unbound-slot-instance condition => instance

* 参数和值(Arguments and Values):

        condition---一个类型 unbound-slot 的状况.
        instance---一个对象.

* 描述(Description):

        返回这个 condition 所表示的情况下有着未绑定槽的实例.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cell-error-name, unbound-slot, Section 9.1 (Condition System Concepts)

* 注意(Notes): None. 


