# 25. 环境

> * 25.1 [外部环境](#TheExternalEnvironment)
> * 25.2 [环境字典](#TheEnvironmentDictionary)

## 25.1 <span id="TheExternalEnvironment">外部环境</span>

> * 25.1.1 [顶层循环](#TopLevelLoop)
> * 25.1.2 [调试工具](#DebuggingUtilities)
> * 25.1.3 [环境查询](#EnvironmentInquiry)
> * 25.1.4 [时间](#Time)

### 25.1.1 <span id="TopLevelLoop">顶层循环</span>

顶层循环是用户用来和 Common Lisp 系统正常交互的 Common Lisp 机制. 这个循环有时被称为 Lisp read-eval-print 循环[Lisp read-eval-print loop], 因为它通常由一个读取一个表达式、对它求值然后打印结果这样的无止尽的循环组成.

这个顶层循环没有被完全指定; 因此用户接口是具体实现定义的[implementation-defined]. 这个顶层循环打印所有来自于求值一个表达式形式[form]的值. 下一段中列出了由 Lisp read-eval-print 循环[Lisp read-eval-print loop]来维护的变量.

    *    +    /    -  
    **   ++   //
    ***  +++  ///

    Figure 25-1. 由 Read-Eval-Print 循环来保存的变量

### 25.1.2 <span id="DebuggingUtilities">调试工具</span>

下一段展示了和调试相关的已定义的名字[defined name].

    *debugger-hook*  documentation    step
    apropos          dribble          time
    apropos-list     ed               trace
    break            inspect          untrace  
    describe         invoke-debugger

    Figure 25-2. 和调试相关的已定义的名字

### 25.1.3 <span id="EnvironmentInquiry">环境查询</span>

环境查询的已定义名字[defined name]提供了关于硬件和软件配置的信息, 一个 Common Lisp 程序基于这些信息执行.

下一段展示了和环境查询相关的已定义名字[defined name].

    *features*                   machine-instance  short-site-name
    lisp-implementation-type     machine-type      software-type
    lisp-implementation-version  machine-version   software-version  
    long-site-name               room

    Figure 25-3. 和环境查询相关的已定义名字.

### 25.1.4 <span id="Time">时间</span>

时间在 Common Lisp 中用四种不同的方式被表示: 解码时间[decoded time], 通用时间[universal time], 内部时间[internal time]和秒. 解码时间[decoded time]和通用时间[universal time]主要被用于表示日历时间, 并且只精确到秒. 内部时间[internal time]主要被用于表示计算机时间的度量 (例如运行时间) 并且精确到某个依赖于具体实现[implementation-dependent]的秒的碎片, 称之为内部时间单元[internal time unit], 由 internal-time-units-per-second 指定. 一个内部时间[internal time]可以被用于绝对[absolute]和相对[relative]的时间[time]度量. 一个通用时间[universal time]和一个解码时间[decoded time]都只能被用于绝对[absolute]时间[time]度量. 在一个 sleep 函数的情况中, 时间间隔被表示为一个秒的非负实数[real].

下一段展示了和时间[time]相关的已定义名字[defined name].

    decode-universal-time   get-internal-run-time
    encode-universal-time   get-universal-time
    get-decoded-time        internal-time-units-per-second  
    get-internal-real-time  sleep

    Figure 25-4. 时间相关的已定义名字.

> * 25.1.4.1 [解码时间](#DecodedTime)
> * 25.1.4.2 [通用时间](#UniversalTime)
> * 25.1.4.3 [内部时间](#InternalTime)
> * 25.1.4.4 [秒](#Seconds)

#### 25.1.4.1 <span id="DecodedTime">解码时间</span>

一个解码时间[decoded time]是一个 9 个值的有序序列, 加在一起表示日历时间中的一个点 (忽略闰秒[leap seconds]):

    Second

        一个 0 和 59 之间的整数[integer], 是包含的.

    Minute

        一个 0 和 59 之间的整数[integer], 是包含的.

    Hour

        一个 0 和 23 之间的整数[integer], 是包含的.

    Date

        一个 1 和 31 之间的整数[integer], 是包含的 (当然这个上限实际上依赖于月份和年).

    Month

        一个 1 和 12 之间的整数[integer], 是包含的; 1 意味着 January, 2 意味着 February, 以此类推; 12 意味着 December.

    Year

        一个表示公元年的整数[integer]. 然而, 如果这个整数[integer]在 0 和 99 之间, 使用 "显而易见的(obvious)" 的年; 更确切地说, 假设那一年等于整数[integer]模100, 并且在本年度的 50 年内 (向后包含但是向前不包含). 因此, 在 1978 年, 年 28 是 1928 但是年 27 是 2027. (以这种格式返回时间的函数总是返回全年数字.)

    Day of week

        一个 0 和 6 之间的整数[integer], 是包含的; 0 意味着周一(Monday), 1 意味着周二(Tuesday), 以此类推; 6 意味着周日(Sunday).

    Daylight saving time flag

        一个广义 boolean [generalized boolean], 如果为 true, 表示夏令时生效.

    Time zone

        一个时区[time zone].

下一段展示了和解码时间[decoded time]相关的已定义名字[defined name].

    decode-universal-time  get-decoded-time  

    Figure 25-5. 解码时间中涉及时间的已定义名字.


#### 25.1.4.2 <span id="UniversalTime">通用时间</span>

通用时间[universal time]是一个表示为一个单独的非负整数[integer]的绝对[absolute]时间[time]---这个整数[integer]是从 January 1, 1900 GMT (忽略闰秒[leap seconds]) 半夜开始的秒数. 因此时间 1 就是 00:00:01 (也就是说, 12:00:01 a.m.) January 1, 1900 GMT. 类似地, 时间 2398291201 对应于时间 00:00:01 January 1, 1976 GMT. 回想一下, 1900 年不是闰年; 出于 Common Lisp 的用途, 当且仅当一个年的数字可以被 4 整除, 那么这年是一个闰年, 除了可以被 100 整除的年不是闰年, 可以被 400 整除的年是闰年. 因此 2000 年会是一个闰年. 因为通用时间[universal time]必须是一个非负整数[integer], 在 January 1, 1900 GMT 半夜之前的时间不能被 Common Lisp 处理.

    decode-universal-time  get-universal-time  
    encode-universal-time

    Figure 25-6. 在通用时间中涉及时间的已定义名称.


#### 25.1.4.3 <span id="InternalTime">内部时间</span>

内部时间[internal time]把时间表示为一个单独的整数[integer], 根据一个称之为内部时间单位[internal time unit]的依赖于具体实现[implementation-dependent]的单位. 相对时间用这些单位的数量来衡量. 绝对时间是相对于任意的时间基数的.

下一段展示了和内部时间[internal time]相关的已定义名字[defined name].

    get-internal-real-time  internal-time-units-per-second  
    get-internal-run-time

    Figure 25-7. 在内部时间中涉及时间的已定义名称.


#### 25.1.4.4 <span id="Seconds">秒</span>

一个函数, sleep, 它的参数是一个秒的非负实数[real]. 非正式地, 把这看作一个相对的[relative]通用时间[universal time]可能是有用的, 但它在一个重要的方面是不同的: 通用时间[universal time]总是非负整数[integer], 而 sleep 的参数可以是任何非负实数[real], 以便考虑到分数秒的可能性.

    sleep

    Figure 25-8. 在秒中涉及时间的已定义名称.


## 25.2 <span id="TheEnvironmentDictionary">环境字典</span>

> * [函数 DECODE-UNIVERSAL-TIME](#F-DECODE-UNIVERSAL-TIME)
> * [函数 ENCODE-UNIVERSAL-TIME](#F-ENCODE-UNIVERSAL-TIME)
> * [函数 GET-UNIVERSAL-TIME, GET-DECODED-TIME](#F-GET-TIME-ALL)
> * [函数 SLEEP](#F-SLEEP)
> * [函数 APROPOS, APROPOS-LIST](#F-APROPOS-ALL)
> * [函数 DESCRIBE](#F-DESCRIBE)
> * [标准广义函数 DESCRIBE-OBJECT](#SGF-DESCRIBE-OBJECT)
> * [宏 TRACE, UNTRACE](#M-TRACE-UNTRACE)
> * [宏 STEP](#M-STEP)
> * [宏 TIME](#M-TIME)
> * [常量 INTERNAL-TIME-UNITS-PER-SECOND](#CV-INTERNAL-TIME-UNITS-PER-SECOND)
> * [函数 GET-INTERNAL-REAL-TIME](#F-GET-INTERNAL-REAL-TIME)
> * [函数 GET-INTERNAL-RUN-TIME](#F-GET-INTERNAL-RUN-TIME)
> * [函数 DISASSEMBLE](#F-DISASSEMBLE)
> * [标准广义函数 DOCUMENTATION, (SETF DOCUMENTATION)](#SGF-DOCUMENTATION-ALL)
> * [函数 ROOM](#F-ROOM)
> * [函数 ED](#F-ED)
> * [函数 INSPECT](#F-INSPECT)
> * [函数 DRIBBLE](#F-DRIBBLE)
> * [常量 -](#V-MINUS)
> * [常量 +, ++, +++](#V-ADD-ALL)
> * [常量 *, **, ***](#V-ASTERISK)
> * [常量 /, //, ///](#V-SLASH)
> * [函数 LISP-IMPLEMENTATION-TYPE, LISP-IMPLEMENTATION-VERSION](#F-LISP-IMPLEMENTATION-ALL)
> * [函数 SHORT-SITE-NAME, LONG-SITE-NAME](#F-SITE-NAME-ALL)
> * [函数 MACHINE-INSTANCE](#F-MACHINE-INSTANCE)
> * [函数 MACHINE-TYPE](#F-MACHINE-TYPE)
> * [函数 MACHINE-VERSION](#F-MACHINE-VERSION)
> * [函数 SOFTWARE-TYPE, SOFTWARE-VERSION](#F-SOFTWARE-ALL)
> * [函数 USER-HOMEDIR-PATHNAME](#F-USER-HOMEDIR-PATHNAME)

### <span id="F-DECODE-UNIVERSAL-TIME">函数 DECODE-UNIVERSAL-TIME</span>

* 语法(Syntax):

        decode-universal-time universal-time &optional time-zone
        => second, minute, hour, date, month, year, day, daylight-p, zone

* 参数和值(Arguments and Values):

        universal-time---一个通用时间[universal time].
        time-zone---一个时区[time zone].
        second, minute, hour, date, month, year, day, daylight-p, zone---一个解码时间[decoded time].

* 描述(Description):

        返回给定的通用时间[universal time]表示的解码时间[decoded time].

        如果没有提供 time-zone, 它默认为当前时区, 调整为夏令时. 如果提供了 time-zone, 忽略夏令时信息. 如果提供了 time-zone 那么夏令时标志就是 nil.

* 示例(Examples):

    ```LISP
    (decode-universal-time 0 0) =>  0, 0, 0, 1, 1, 1900, 0, false, 0

    ;; The next two examples assume Eastern Daylight Time.
    (decode-universal-time 2414296800 5) =>  0, 0, 1, 4, 7, 1976, 6, false, 5
    (decode-universal-time 2414293200) =>  0, 0, 1, 4, 7, 1976, 6, true, 5

    ;; This example assumes that the time zone is Eastern Daylight Time
    ;; (and that the time zone is constant throughout the example).
    (let* ((here (nth 8 (multiple-value-list (get-decoded-time)))) ;Time zone
            (recently (get-universal-time))
            (a (nthcdr 7 (multiple-value-list (decode-universal-time recently))))
            (b (nthcdr 7 (multiple-value-list (decode-universal-time recently here)))))
      (list a b (equal a b))) =>  ((T 5) (NIL 5) NIL)
    ```

* 受此影响(Affected By):

        用于计算任何给定会话的夏令时何时生效或是否生效的依赖于具体实现的[implementation-dependent]的机制.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        encode-universal-time, get-universal-time, 章节 25.1.4 (时间)

* 注意(Notes): None.


### <span id="F-ENCODE-UNIVERSAL-TIME">函数 ENCODE-UNIVERSAL-TIME</span>

* 语法(Syntax):

        encode-universal-time second minute hour date month year &optional time-zone
        => universal-time

* 参数和值(Arguments and Values):

        second, minute, hour, date, month, year, time-zone---一个解码时间[decoded time]的对应部分. (注意, 在一个完整的解码时间[decoded time]中, 9 个值中的一些是冗余的, 因此不用作此函数的输入.)
        universal-time---一个通用时间[universal time].

* 描述(Description):

        encode-universal-time 把一个解码时间格式的时间转换为一个通用时间[universal time].

        如果提供了 time-zone, 不会执行夏令时的调整.

* 示例(Examples):

    ```LISP
    (encode-universal-time 0 0 0 1 1 1900 0) =>  0
    (encode-universal-time 0 0 1 4 7 1976 5) =>  2414296800
    ;; The next example assumes Eastern Daylight Time.
    (encode-universal-time 0 0 1 4 7 1976) =>  2414293200
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        decode-universal-time, get-decoded-time

* 注意(Notes): None.


### <span id="F-GET-TIME-ALL">函数 GET-UNIVERSAL-TIME, GET-DECODED-TIME</span>

* 语法(Syntax):

        get-universal-time <no arguments> => universal-time

        get-decoded-time <no arguments>
        => second, minute, hour, date, month, year, day, daylight-p, zone

* 参数和值(Arguments and Values):

        universal-time---一个通用时间[universal time].
        second, minute, hour, date, month, year, day, daylight-p, zone---一个解码时间[decoded time].

* 描述(Description):

        get-universal-time 返回当前时间, 表示为一个通用时间[universal time].

        get-decoded-time 返回当前时间, 表示为一个解码时间[decoded time].

* 示例(Examples):

    ```LISP
    ;; At noon on July 4, 1976 in Eastern Daylight Time.
    (get-decoded-time) =>  0, 0, 12, 4, 7, 1976, 6, true, 5
    ;; At exactly the same instant.
    (get-universal-time) =>  2414332800
    ;; Exactly five minutes later.
    (get-universal-time) =>  2414333100
    ;; The difference is 300 seconds (five minutes)
    (- * **) =>  300
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        一天的时间 (换句话说, 流逝的时间), 系统时钟保持精确时间的能力, 以及系统时钟初始设置的准确性.

* 异常情况(Exceptional Situations):

        如果当前时间不能确定, 那么就会发出一个 error 类型[type]的错误.

* 也见(See Also):

        decode-universal-time, encode-universal-time, 章节 25.1.4 (时间)

* 注意(Notes):

        (get-decoded-time) ==  (decode-universal-time (get-universal-time))

        没有要求具体实现[implementation]有着一种方式去核实返回的时间是正确的. 然而, 如果一个具体实现[implementation]提供正确性检测 (例如, 未能正确初始化系统时钟可以可靠地检测到) 并且那个正确性检测没通过, 强烈鼓励 (但不是必须) 具体实现[implementation]去发出一个 error 类型[type]的错误 (而不是, 例如, 返回一个已知是错误的值), 它是可纠正的[correctable], 允许用户去交互式地设置正确的时间.


### <span id="F-SLEEP">函数 SLEEP</span>

* 语法(Syntax):

        sleep seconds => nil

* 参数和值(Arguments and Values):

        seconds---一个非负实数[real].

* 描述(Description):

        停止执行并进入休眠状态, 大约 seconds 表示的实时秒数, 然后恢复执行.

* 示例(Examples):

    ```LISP
    (sleep 1) =>  NIL

    ;; Actually, since SLEEP is permitted to use approximate timing,
    ;; this might not always yield true, but it will often enough that
    ;; we felt it to be a productive example of the intent.
    (let ((then (get-universal-time))
          (now  (progn (sleep 10) (get-universal-time))))
      (>= (- now then) 10))
    =>  true
    ```

* 副作用(Side Effects):

        导致执行终止.

* 受此影响(Affected By):

        调度程序的粒度.

* 异常情况(Exceptional Situations):

        如果 seconds 不是一个非负实数[real], 就会发出一个 type-error 类型[type]的错误.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-APROPOS-ALL">函数 APROPOS, APROPOS-LIST</span>

* 语法(Syntax):

        apropos string &optional package => <no values>

        apropos-list string &optional package => symbols

* 参数和值(Arguments and Values):

        string---一个字符串标识符[string designator].
        package---一个包标识符[package designator]或 nil. 默认是 nil.
        symbols---一个符号[symbol]列表[list].

* 描述(Description):

        这些函数搜索名字[name]中包含子字符串 string 的已捕捉的[interned]符号[symbol].

        对于 apropos, 作为每一个被找到的符号[symbol], 它的名字被打印在标准输出[standard output]. 另外, 如果这样的一个符号[symbol]被定义为一个函数[function]或动态变量[dynamic variable], 关于这些定义的信息也会被打印.

        对于 apropos-list, 随着这个搜索的进行不会产生输出; 而是当搜索完成时返回匹配的符号[symbol]的列表.

        如果 package 非 nil [non-nil], 只有包 pacakge 中可访问的[accessible]符号[symbol]会被搜索; 否则任何包[package]中的所有可访问的[accessible]符号[symbol]都会被搜索.

        由于一个符号[symbol]可能通过多个继承路径可用, apropos 可能打印关于相同[same]符号[symbol]的信息不止一次, 或者 apropos-list 可能返回一个包含重复符号[symbol]的列表[list].

        这个搜索是否是大小写敏感的是具体实现定义的[implementation-defined].

* 示例(Examples): None.

* 受此影响(Affected By):

        在任何要被搜索的包[package]中当前被捕捉的[interned]符号[symbol]的集合.

        apropos 也受 *standard-output* 影响.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-DESCRIBE">函数 DESCRIBE</span>

* 语法(Syntax):

        describe object &optional stream => <no values>

* 参数和值(Arguments and Values):

        object---一个符号[symbol].
        stream---一个输出[output]流标识符[stream designator]. 默认是标准输出[standard output].

* 描述(Description):

        describe 显示关于对象 object 的信息到流 stream.

        比如, 一个符号[symbol]的描述 describe 可能展示这个符号[symbol]的值, 它的定义, 已经它的每一个属性. 一个浮点数[float]的描述可能以一种有助于跟踪舍入错误的方式显示数字的内部表示. 在所有情况中, 然而, 这个描述输出的性质和格式是依赖于具体实现的[implementation-dependent].

        describe 可以描述它在这个对象 object 中找到的东西; 在这样的情况中, 通常使用诸如增加缩进或在表中定位等符号图案, 以便从视觉上区分这种递归描述和参数 object 的描述.

        描述这个对象的实际行为是由 describe-object 实现的. describe 存在作为一个管理缺省参数的接口 (包括参数 t 和 nil 到流[stream]对象[object]的转换) 以及去抑制来自 describe-object 的任何返回值.

        describe 不打算作为一个交互式的函数. 在一个复合规范的实现[conforming implementation]中, 默认情况下, describe 一定不能为用户输入提示. 用户定义的 describe-objec 方法同样受限.

* 示例(Examples): None.

* 副作用(Side Effects):

        输出到标准输出[standard output]或终端 I/O [terminal I/O].

* 受此影响(Affected By):

        *standard-output* 和 *terminal-io*, 用户定义的类[class]的对象[object]上的 describe-object 和 print-object 方法.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        inspect, describe-object

* 注意(Notes): None.


### <span id="SGF-DESCRIBE-OBJECT">标准广义函数 DESCRIBE-OBJECT</span>

* 语法(Syntax):

        describe-object object stream => implementation-dependent

* 方法签名(Method Signatures):

        describe-object (object standard-object) stream

* 参数和值(Arguments and Values):

        object---一个符号[symbol].
        stream---一个流[stream].

* 描述(Description):

        广义函数 describe-object 打印对象 object 的描述到流 stream. describe-object 被 describe 调用; 它一定不能被用户调用.

        每一个具体实现需要去提供一个类[class] standard-object 上的方法[method]以及足够的其他类上的方法[method]来确保这里总是有一个可应用的方法[method]. 具体实现可以自由地去为其他类[class]添加方法[method]. 如果用户不想去继承一个具体实现提供的方法[method], 那么用户可以为他们自己的类[class]编写 describe-object 方法[method].

        在 describe-object 上的方法[method]可以递归调用 describe. 缩进, 深度限制, 还有环状检测都是自动处理的, 如果这里有更多结构层级, 那么假设每一个方法[method]处理结构的一层并且递归调用 describe. 如果没有遵守这个规则的后果是未定义的.

        在一些具体实现中, 传递给一个 describe-object 方法的这个 stream 流参数不是那个原始的流 stream, 而是一个实现了 describe 部分的中间流. 因此方法[method]不应该依赖于这个流[stream]的标识.

* 示例(Examples):

    ```LISP
    (defclass spaceship ()
      ((captain :initarg :captain :accessor spaceship-captain)
        (serial# :initarg :serial-number :accessor spaceship-serial-number)))

    (defclass federation-starship (spaceship) ())

    (defmethod describe-object ((s spaceship) stream)
      (with-slots (captain serial#) s
        (format stream "~&~S is a spaceship of type ~S,~
                        ~%with ~A at the helm ~
                          and with serial number ~D.~%"
                s (type-of s) captain serial#)))

    (make-instance 'federation-starship
                    :captain "Rachel Garrett"
                    :serial-number "NCC-1701-C")
    =>  #<FEDERATION-STARSHIP 26312465>

    (describe *)
    >>  #<FEDERATION-STARSHIP 26312465> is a spaceship of type FEDERATION-STARSHIP,
    >>  with Rachel Garrett at the helm and with serial number NCC-1701-C.
    =>  <no values>
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        describe

* 注意(Notes):

        和应用于 print-object 相同的实现机制被应用于 describe-object.

        使这个 describe-object 的返回值是未指定的原因是去避免强制用户在所有它们的方法[method]中去包含显示的 (values). describe 负责这个.


### <span id="M-TRACE-UNTRACE">宏 TRACE, UNTRACE</span>

* 语法(Syntax):

        trace function-name* => trace-result

        untrace function-name* => untrace-result

* 参数和值(Arguments and Values):

        function-name---一个函数名字[function name].
        trace-result---依赖于具体实现的[implementation-dependent], 除非没有提供 function-names, 在这个情况中 trace-result 是一个函数名字[function name]的列表[list].
        untrace-result---依赖于具体实现的[implementation-dependent].

* 描述(Description):

        trace 和 untrace 控制那个跟踪工具的调用.

        用一个或多个函数名 function-names 来调用 trace 导致表示的函数[function]被 "追踪". 无论何时一个被追踪的函数[function]被调用, 关于这个调用, 关于这些参数传递, 以及关于最后返回值的信息会被打印到跟踪输出[trace output]. 如果 trace 没有和函数名 function-names 一起使用, 不会执行跟踪的动作; 反而, 一个当前正在被跟踪的函数[function]列表会被返回.

        用一个或多个函数名调用 untrace 导致那些函数被 "解除追踪(untraced)" (换句话说, 不再被追踪). 如果 untrace 没有和函数名 function-names 一起使用, 所有当前正在被追踪的函数都会被解除追踪.

        如果一个要被追踪的函数[function]已经是 open-coded 的(例如, 由于它被申明为 inline), 一个对该函数的调用可能不会产生跟踪输出.

* 示例(Examples):

    ```LISP
    (defun fact (n) (if (zerop n) 1 (* n (fact (- n 1)))))
    =>  FACT
    (trace fact)
    =>  (FACT)
    ;; Of course, the format of traced output is implementation-dependent.
    (fact 3)
    >>  1 Enter FACT 3
    >>  | 2 Enter FACT 2
    >>  |   3 Enter FACT 1
    >>  |   | 4 Enter FACT 0
    >>  |   | 4 Exit FACT 1
    >>  |   3 Exit FACT 1
    >>  | 2 Exit FACT 2
    >>  1 Exit FACT 6
    =>  6
    ```

* 副作用(Side Effects):

        可能改变由 function-names 命名的那些函数[function]的定义.

* 受此影响(Affected By):

        受这些已命名的函数是否被定义或者是否已经被跟踪所影响.

* 异常情况(Exceptional Situations):

        跟踪一个已经被跟踪的函数, 或者解除一个当前没有被跟踪的函数的跟踪, 都不应该产生有害的效果, 但是可能发出一个警告.

* 也见(See Also):

        *trace-output*, step

* 注意(Notes):

        trace 和 untrace 可能也接受具体实现定义的[implementation-dependent]额外的参数格式. 这个跟踪输出的格式是依赖于具体实现的[implementation-dependent].

        虽然 trace 可以被扩展来允许非标准选项, 不过还是鼓励(但不是必须)具体实现[implementation]去警告关于既没有被这个标准指定也没有被这个实现[implementation]添加为一个扩展的语法或选项的使用, 因为它们可以是印刷错误或依赖一些其他实现[implementation]支持而当前实现[implementation]不支持的特性的症状.


### <span id="M-STEP">宏 STEP</span>

* 语法(Syntax):

        step form => result*

* 参数和值(Arguments and Values):

        form---一个表达式形式[form]; 按以下描述求值.
        results---由表达式形式 form 返回的那些值[value].

* 描述(Description):

        step 实现了一个调试模式, 在其中程序员允许去单步[step]调试一个表达式形式[form]的求值[evaluation]. 这个交互的具体性质, 包括哪个 I/O 流会被使用并且这个步进是否有着词法或动态的作用域, 是具体实现定义的[implementation-defined].

        step 在当前环境[envirnonment]中求值表达式形式 form. 一个对 step 的调用可以被编译, 但是对于一个实现来说, 只交互遍历被解释的计算部分是可以接受的.

        对于一个符合规范的实现[conforming implementation]除了正常求值表达式形式 form 以外不采取动作是技术上允许的. 在这样一个情况中, (step form) 等价于, 例如, (let () form). 在这种情况下的实现中, 相关文档应该提到这个事实.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        trace

* 注意(Notes):

        鼓励具体实现[implementation]去提供包含一个命令列表的帮助来回应 ? 的输入或一个 "help key" 的按下.


### <span id="M-TIME">宏 TIME</span>

* 语法(Syntax):

        time form => result*

* 参数和值(Arguments and Values):

        form---一个表达式形式[form]; 按如下所述求值.
        results---由这个表达式形式 form 返回的那些值[value].

* 描述(Description):

        time 在当前环境[environment]中 (词法和动态的) 求值表达式形式 form. 一个对 time 的调用可以被编译.

        time 把各种时间数据和其他信息打印到跟踪输出[trace output]. 这个打印信息的性质和格式是具体实现定义的[implementation-defined]. 鼓励具体实现去提供诸如经过的实际时间, 机器运行时间, 以及存储管理统计等信息.

* 示例(Examples): None.

* 受此影响(Affected By):

        结果的准确性除其他外, 还取决于底层操作系统提供的相应功能的准确性.

        结果的大小可能取决于硬件, 操作系统, lisp 具体实现和全局环境的状态. 一些经常影响结果具体问题是硬件速度, 调度器(如果有的话)的性质, 进程竞争过程(如果有的话), 系统分页, 这个调用是否被解释或编译, 这个被调用的函数是否被编译, 涉及的垃圾收集器的种类以及它是否运行, 内部数据结构(例如, 哈希表)是否隐式地重组, 等等.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        get-internal-real-time, get-internal-run-time

* 注意(Notes):

        通常情况下, 这些时间并不能保证足够可靠来进行市场比较. 它们的值主要是启发式的, 用于调优.

        有关解释计时结果所涉及的复杂问题的有用背景资料, 见 Performance and Evaluation of Lisp Programs.


### <span id="CV-INTERNAL-TIME-UNITS-PER-SECOND">常量 INTERNAL-TIME-UNITS-PER-SECOND</span>

* 常量值(Constant Value):

        一个正整数[integer], 它的大小是依赖于具体实现的[implementation-dependent].

* 描述(Description):

        一秒钟内的内部时间单位[internal time unit]的数量.

* 示例(Examples): None.

* 也见(See Also):

        get-internal-run-time, get-internal-real-time

* 注意(Notes):

        这些单元构成了内部时间格式表示的基础.


### <span id="F-GET-INTERNAL-REAL-TIME">函数 GET-INTERNAL-REAL-TIME</span>

* 语法(Syntax):

        get-internal-real-time <no arguments> => internal-time

* 参数和值(Arguments and Values):

        internal-time---一个非负整数[integer].

* 描述(Description):

        get-internal-real-time 以整数[integer]形式返回内部时间单元[internal time unit]中的当前时间, 相对于任意的时间基数. 两次调用这个函数的值的区别就是这两次调用之间逝去的实际时间总和 (换句话说, 时钟时间).

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        一天的时间 (换句话说, 时间的流逝). 时间基数影响结果的大小.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        internal-time-units-per-second

* 注意(Notes): None.


### <span id="F-GET-INTERNAL-RUN-TIME">函数 GET-INTERNAL-RUN-TIME</span>

* 语法(Syntax):

        get-internal-run-time <no arguments> => internal-time

* 参数和值(Arguments and Values):

        internal-time---一个非负整数[integer].

* 描述(Description):

        以整数[integer]形式返回内部时间单元[internal time unit]中的当前运行时间. 这个量的确切含义是由具体实现定义的[implementation-defined]; 它可能测量实际时间, 运行时间, CPU 周期, 或者某个其他量. 目的是这个函数的两个调用的值之间的差值是两个调用之间的时间总量, 在此期间, 代表执行程序花费了计算工作量. <!--TODO 最后一句？？-->

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        具体实现[implementation], 一天的时间 (换句话说, 时间的流逝).

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        internal-time-units-per-second

* 注意(Notes):

        依赖于具体实现[implementation], 换页时间和垃圾收集时间可能被包含在这个测量中. 同样的, 在多任务环境中, 或许不可能去展示这个运行过程的时间, 因此在一些实现[implementation]中, 其他进程在这期间所用掉的时间也可能被包含在这个测量中.


### <span id="F-DISASSEMBLE">函数 DISASSEMBLE</span>

* 语法(Syntax):

        disassemble fn => nil

* 参数和值(Arguments and Values):

        fn---一个扩展函数标识符[extended function designator]或一个 lambda 表达式[lambda expression].

* 描述(Description):

        函数[function] disassemble 是一个调试工具, 它用某种依赖于具体实现[implementation-dependent]的语言组成符号指令或表达式, 这些符号指令或表达式表示用于生成函数[function]的代码, 该函数由参数 fb 命名或命名. 结果以一种依赖于具体实现[implementation-dependent]的格式显示到标准输出[standard output].

        如果 fn 是一个 lambda 表达式[lambda expression]或一个被解释的函数[interpreted function], 它首先会被编译然后结果被解体.

        如果这个 fn 标识符[designator]是一个函数名字[function name], 那么它命名[name]的函数[function]会被解体. (如果那个函数[function]是一个被解释的函数[interpreted function], 它首先被编译但是这个隐式编译的结果不会被安装.)

* 示例(Examples):

    ```LISP
    (defun f (a) (1+ a)) =>  F
    (eq (symbol-function 'f)
        (progn (disassemble 'f)
                (symbol-function 'f))) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-output*.

* 异常情况(Exceptional Situations):

        如果 fn 不是一个扩展函数标识符[extended function designator]或一个 lambda 表达式[lambda expression], 那么就应该发出一个 type-error 类型[type]的错误.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="SGF-DOCUMENTATION-ALL">标准广义函数 DOCUMENTATION, (SETF DOCUMENTATION)</span>

* 语法(Syntax):

        documentation x doc-type => documentation

        (setf documentation) new-value x doc-type => new-value

* 参数优先级顺序(Argument Precedence Order):

        doc-type, object

* 方法签名(Method Signatures):

    函数, 宏, 和特殊表达式:

        documentation (x function) (doc-type (eql 't))
        documentation (x function) (doc-type (eql 'function))
        documentation (x list) (doc-type (eql 'function))
        documentation (x list) (doc-type (eql 'compiler-macro))
        documentation (x symbol) (doc-type (eql 'function))
        documentation (x symbol) (doc-type (eql 'compiler-macro))
        documentation (x symbol) (doc-type (eql 'setf))

        (setf documentation) new-value (x function) (doc-type (eql 't))
        (setf documentation) new-value (x function) (doc-type (eql 'function))
        (setf documentation) new-value (x list) (doc-type (eql 'function))
        (setf documentation) new-value (x list) (doc-type (eql 'compiler-macro))
        (setf documentation) new-value (x symbol) (doc-type (eql 'function))
        (setf documentation) new-value (x symbol) (doc-type (eql 'compiler-macro))
        (setf documentation) new-value (x symbol) (doc-type (eql 'setf))

    方法组合:

        documentation (x method-combination) (doc-type (eql 't))
        documentation (x method-combination) (doc-type (eql 'method-combination))
        documentation (x symbol) (doc-type (eql 'method-combination))

        (setf documentation) new-value (x method-combination) (doc-type (eql 't))
        (setf documentation) new-value (x method-combination) (doc-type (eql 'method-combination))
        (setf documentation) new-value (x symbol) (doc-type (eql 'method-combination))

    方法:

        documentation (x standard-method) (doc-type (eql 't))

        (setf documentation) new-value (x standard-method) (doc-type (eql 't))

    包:

        documentation (x package) (doc-type (eql 't))

        (setf documentation) new-value (x package) (doc-type (eql 't))

    类型, 类, 以及结构体名字:

        documentation (x standard-class) (doc-type (eql 't))
        documentation (x standard-class) (doc-type (eql 'type))
        documentation (x structure-class) (doc-type (eql 't))
        documentation (x structure-class) (doc-type (eql 'type))
        documentation (x symbol) (doc-type (eql 'type))
        documentation (x symbol) (doc-type (eql 'structure))

        (setf documentation) new-value (x standard-class) (doc-type (eql 't))
        (setf documentation) new-value (x standard-class) (doc-type (eql 'type))
        (setf documentation) new-value (x structure-class) (doc-type (eql 't))
        (setf documentation) new-value (x structure-class) (doc-type (eql 'type))
        (setf documentation) new-value (x symbol) (doc-type (eql 'type))
        (setf documentation) new-value (x symbol) (doc-type (eql 'structure))

    变量:

        documentation (x symbol) (doc-type (eql 'variable))

        (setf documentation) new-value (x symbol) (doc-type (eql 'variable))

* 参数和值(Arguments and Values):

        x---一个符号[symbol].
        doc-type---一个符号[symbol].
        documentation---一个字符串[string]或 nil.
        new-value---一个字符串[string].

* 描述(Description):

        如果可用的话, 广义函数[generic function] documentation 返回和给定的对象[object]相关联的文档字符串[documentation string]; 否则它返回 nil.

        广义函数[generic function] (setf documentation) 更新和 x 相关联的文档字符串[documentation string]为 new-value. 如果 x 是一个列表[list], 它必须是 (setf symbol) 形式.

        文档字符串[documentation string]可用于调试目的. 符合规范的程序[conforming program]允许在文档字符串[documentation string]存在时去使用它们, 但是不应该将它们的正确行为依赖于这些文档字符串[documentation string]的存在性. 允许一个具体实现[implementation]出于具体实现定义的[implementation-defined]原因在任何时间丢弃文档字符串[documentation string].

        返回的文档字符串[documentation string]的性质依赖于 doc-type, 如下:

        compiler-macro

            返回这个名字[name]为函数名[function] x 的编译器宏[compiler macro]的文档字符串[documentation string].

        function

            如果 x 是一个函数名[function name], 返回名字[name]为 x 的函数[function], 宏[macro], 或特殊操作符[special operator]的文档字符串[documentation string].

            如果 x 是一个函数[function], 返回和 x 关联的文档字符串[documentation string].

        method-combination

            如果 x 是一个符号[symbol], 返回名为 x 的方法组合[method combination]的文档字符串[documentation string].

            如果 x 是一个方法组合[method combination], 返回和 x 关联的文档字符串[documentation string].

        setf

            返回名字[name]为符号[symbol] x 的 setf 展开器[setf expander]的文档字符串[documentation string].

        structure

            返回和结构体名[structure name] x 相关联的文档字符串[documentation string].

        t

            返回参数 x 自身的类[class]上特化的文档字符串[documentation string]. 比如, 如果 x 是一个函数[function], 返回和那个函数[function] x 关联的文档字符串[documentation string].

        type

            如果 x 是一个符号[symbol], 那么如果这里有这样一个类[class], 就返回名字[name]为符号[symbol] x 的类[class]的文档字符串[documentation string]. 否则, 返回类型指定符[type specifier]符号[symbol] x 的类型[type]的文档字符串[documentation string].

            如果 x 是一个结构体类[structure class]或者标准类[standard class], 那么返回和类[class] x 关联的文档字符串[documentation string].

        variable

            返回名字[name]为 x 的动态变量[dynamic variable]或常变量[constant variable]的文档字符串[documentation string].

        一个符合规范的实现[conforming implementation]或一个符合规范的程序[conforming program]可能扩展这个可接受作为 doc-type 的符号[symbol]集合.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        这个标准没有规定检索 defclass 表达式形式中指定的各个槽的文档字符串[documentation string]的方法, 但是具体实现[implementation]仍然可以提供调试工具 和/或 编程语言扩展来操纵这个信息. 我们鼓励希望提供这种支持的实现者查阅元对象协议[Metaobject Protocol], 以获得关于如何做到这一点的建议.


### <span id="F-ROOM">函数 ROOM</span>

* 语法(Syntax):

        room &optional x => 依赖于具体实现[implementation-dependent]

* 参数和值(Arguments and Values):

        x---t, nil, 或 :default 其中之一.

* 描述(Description):

        room 打印关于内部存储的状态信息以及它的管理到标准输出[standard output]. 这可能包括内存使用量和内存压缩程度的描述, 如果合适的话, 可能会按内部数据类型分解. 打印信息的性质和格式是依赖于具体实现[implementation-dependent]的. 目的是提供程序员[programmer]可能用于为特定实现[implementation]优化程序[program]的信息.

        (room nil) 打印出最少量的信息. (room t) 打印出最多量的信息. (room) 或 (room :default) 打印出可能有用的中间量的信息.

* 示例(Examples): None.

* 副作用(Side Effects):

        输出到标准输出[standard output].

* 受此影响(Affected By):

        *standard-output*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-ED">函数 ED</span>

* 语法(Syntax):

        ed &optional x => 依赖于具体实现[implementation-dependent]

* 参数和值(Arguments and Values):

        x---nil, 一个路径名[pathname], 一个字符串[string], 或一个函数名[function name]. 默认是 nil.

* 描述(Description):

        如果这个实现[implementation]提供一个常驻的编辑器, 那么 ed 调用那个编辑器.

        如果 x 是 nil, 就进入那个编辑器. 如果之前已经进入那个编辑器, 那么如果可能的话, 恢复它的之前状态.

        如果 x 是一个路径名[pathname]或字符串[string], 它会被用作一个要被编辑的文件[file]的路径名标识符[pathname designator].

        如果 x 是一个函数名[function name], 编辑它的定义的文本. 获取那个函数文本的方式是具体实现定义的[implementation-defined].

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果这个实现[implementation]没有提供一个常驻的编辑器, 那么后果是未定义的.

        如果提供了它的参数但是不是一个符号[symbol], 一个路径名[pathname], 或 nil, 那么可能发出一个 type-error 类型的错误.

        如果尝试编辑一个文件[file]期间在执行某个文件系统[file system]上的操作时发生一个故障, 那么会发出一个 file-error 类型[type]的错误.

        如果 x 是一个通配符[wild]路径名[pathname]的标识符[designator], 那么就可能会发出 file-error 类型[type]的错误.

        也可能被发出依赖于具体实现[implementation-dependent]的额外状况.

* 也见(See Also):

        pathname, logical-pathname, compile-file, load, 章节 19.1.2 (路径名作为文件名)

* 注意(Notes): None.


### <span id="F-INSPECT">函数 INSPECT</span>

* 语法(Syntax):

        inspect object => 依赖于具体实现[implementation-dependent]

* 参数和值(Arguments and Values):

        object---一个符号[symbol].

* 描述(Description):

        inspect 是 describe 的一个交互式版本. 这个交互的性质是依赖于具体实现[implementation-dependent], 但是这个 inspect 的目的是使浏览数据结构, 检验并修改它的各个部分变得容易.

* 示例(Examples): None.

* 副作用(Side Effects):

        依赖于具体实现[implementation-dependent].

* 受此影响(Affected By):

        依赖于具体实现[implementation-dependent].

* 异常情况(Exceptional Situations):

        依赖于具体实现[implementation-dependent].

* 也见(See Also):

        describe

* 注意(Notes):

        鼓励具体实现通过提供包含命令列表的帮助去回应 ? 的输入或一个 "help key" 的按下.


### <span id="F-DRIBBLE">函数 DRIBBLE</span>

* 语法(Syntax):

        dribble &optional pathname => 依赖于具体实现[implementation-dependent]

* 参数和值(Arguments and Values):

        pathname---一个路径名标识符[pathname designator].

* 描述(Description):

        绑定[bind] *standard-input* 和 *standard-output* 或采取其他适当的操作, 以便将输入/输出交互的记录发送到 pathname 命名的文件. dribble 旨在创建交互式会话的可读记录.

        如果 pathname 是一个逻辑路径名[logical pathname], 它会像是调用 translate-logical-pathname 一样被转换为一个物理路径名.

        (dribble) 终止输入和输出的记录并且关闭这个 dribble 文件.

        如果 dribble 被调用, 但是来自上次对 dribble 调用的到 "dribble file" 的流[stream]仍然打开的情况下, 效果是具体实现定义的[implementation-defined]. 比如, 这个已经打开[open]的流[stream]可能被关闭[closed], 或者 dribbling 可能对于旧的流[stream]和新的流[stream]同时发生, 或者那个旧的流[stream]可能保持打开但是不接受进一步的输出, 或者这个新的请求会被忽略, 或者采取某个其他动作.

* 示例(Examples): None.

* 受此影响(Affected By):

        具体实现[implementation].

* 异常情况(Exceptional Situations):

        如果创建这个 dribble 文件期间在执行某个文件系统[file system]上的操作时出现一个失败, 那么一个 file-error 类型[type]的错误会被发出.

        如果 pathname 是一个通配符[wild]路径名[pathname]的标识符[designator], 那么就可能会发出 file-error 类型[type]的错误.

* 也见(See Also):

        章节 19.1.2 (路径名作为文件名)

* 注意(Notes):

        dribble 可以在后续的表达式形式[form]执行之前返回. 它也可以进入到一个递归的交互式循环中, 只有当 (dribble) 完成时返回.

        dribble 主要用于交互式调试; 在一个程序中使用它时, 不能依赖它的效果.


### <span id="V-MINUS">常量 -</span>

* 值类型(Value Type):

        一个表达式形式[form].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这个 - 的值[value]是当前被 Lisp read-eval-print 循环[Lisp read-eval-print loop]求值的表达式形式[form].

* 示例(Examples):

    ```LISP
    (format t "~&Evaluating ~S~%" -)
    >>  Evaluating (FORMAT T "~&Evaluating ~S~%" -)
    =>  NIL
    ```

* 受此影响(Affected By):

        Lisp read-eval-print 循环[Lisp read-eval-print loop].

* 也见(See Also):

        + (变量), * (变量), / (变量), 章节 25.1.1 (顶层循环)

* 注意(Notes): None.


### <span id="V-ADD-ALL">常量 +, ++, +++</span>

* 值类型(Value Type):

        一个对象[object].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这些 +, ++, 和 +++ 变量[variable]由 Lisp read-eval-print 循环[Lisp read-eval-print loop]维护, 来保存最近求值的表达式形式[form].

        这个 + 的值[value]是最后求值的表达式形式[form], 这个 ++ 的值[value]是 + 的前一个值, 而 +++ 的值[value]是 ++ 的前一个值.

* 示例(Examples):

    ```LISP
    (+ 0 1) =>  1
    (- 4 2) =>  2
    (/ 9 3) =>  3
    (list + ++ +++) =>  ((/ 9 3) (- 4 2) (+ 0 1))
    (setq a 1 b 2 c 3 d (list a b c)) =>  (1 2 3)
    (setq a 4 b 5 c 6 d (list a b c)) =>  (4 5 6)
    (list a b c) =>  (4 5 6)
    (eval +++) =>  (1 2 3)
    #.`(,@++ d) =>  (1 2 3 (1 2 3))
    ```

* 受此影响(Affected By):

        Lisp read-eval-print 循环[Lisp read-eval-print loop].

* 也见(See Also):

        - (variable), * (variable), / (variable), 章节 25.1.1 (顶层循环)

* 注意(Notes): None.


### <span id="V-ASTERISK">常量 *, **, ***</span>

* 值类型(Value Type):

        一个对象[object].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这些 *, **, 和 *** 变量[variable]由 Lisp read-eval-print 循环[Lisp read-eval-print loop]维护, 来保存每一次通过这个循环打印的结果值.

        这个 * 是最近被打印的主要值[primary value], 这个 ** 的值[value]是 * 的前一个值, 而 *** 的值[value]是 ** 的前一个值.

        如果产生多个值, * 只包含第一个值; 如果没有值产生, 那么 * 包含 nil.

        *, **, 和 *** 的这些值[value]被 Lisp read-eval-print 循环[Lisp read-eval-print loop]在打印顶层表达式形式[form]的返回值[return value]之前被更新. 如果这样一个表达式形式[form]的求值在它正常返回之前被中止, 那么 *, **, 和 *** 的值都不会被更新.

* 示例(Examples):

    ```LISP
    (values 'a1 'a2) =>  A1, A2
    'b =>  B
    (values 'c1 'c2 'c3) =>  C1, C2, C3
    (list * ** ***) =>  (C1 B A1)

    (defun cube-root (x) (expt x 1/3)) =>  CUBE-ROOT
    (compile *) =>  CUBE-ROOT
    (setq a (cube-root 27.0)) =>  3.0
    (* * 9.0) =>  27.0
    ```

* 受此影响(Affected By):

        Lisp read-eval-print 循环[Lisp read-eval-print loop].

* 也见(See Also):

        - (变量), + (变量), / (变量), 章节 25.1.1 (顶层循环)

* 注意(Notes):

        *   ==  (car /)
        **  ==  (car //)
        *** ==  (car ///)


### <span id="V-SLASH">常量 /, //, ///</span>

* 值类型(Value Type):

        一个属性列表[proper list].

* 初始值(Initial Value):

        依赖于具体实现[implementation-dependent].

* 描述(Description):

        这些 /, //, 和 /// 变量[value]由 Lisp read-eval-print 循环[Lisp read-eval-print loop]维护, 来保存这个循环最后被打印的结果的值.

        这个 / 的值[value]是最近被打印的值[value]的列表[list], 这个 // 的值[value]是 / 的前一个值, 而这个 /// 的值[value]是 // 的前一个值.

        /, //, 和 /// 的值被 Lisp read-eval-print 循环[Lisp read-eval-print loop]在打印顶层表达式形式[form]的返回值[return value]之前被更新. 如果这样一个表达式形式[form]的求值在它正常返回之前被中止, 那么 /, //, 和 /// 的这些值都不会被更新.

* 示例(Examples):

    ```LISP
    (floor 22 7) =>  3, 1
    (+ (* (car /) 7) (cadr /)) =>  22
    ```

* 受此影响(Affected By):

        Lisp read-eval-print 循环[Lisp read-eval-print loop].

* 也见(See Also):

        - (变量), + (变量), * (变量), 章节 25.1.1 (顶层循环)

* 注意(Notes): None.


### <span id="F-LISP-IMPLEMENTATION-ALL">函数 LISP-IMPLEMENTATION-TYPE, LISP-IMPLEMENTATION-VERSION</span>

* 语法(Syntax):

        lisp-implementation-type <no arguments> => description

        lisp-implementation-version <no arguments> => description

* 参数和值(Arguments and Values):

description---a string or nil.

* 描述(Description):

        lisp-implementation-type 和 lisp-implementation-version 标识当前的 Common Lisp 实现.

        lisp-implementation-type 返回一个标识这个特定 Common Lisp 实现的通用名称的字符串[string].

        lisp-implementation-version 返回一个标识这个特定 Common Lisp 实现的版本的字符串[string].

        如果不能产生适当的和相关的结果, 会返回 nil 而不是一个字符串[string].

* 示例(Examples):

    ```LISP
    (lisp-implementation-type)
    =>  "ACME Lisp"
    OR=>  "Joe's Common Lisp"
    (lisp-implementation-version)
    =>  "1.3a"
    =>  "V2"
    OR=>  "Release 17.3, ECO #6"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-SITE-NAME-ALL">函数 SHORT-SITE-NAME, LONG-SITE-NAME</span>

* 语法(Syntax):

        short-site-name <no arguments> => description

        long-site-name <no arguments> => description

* 参数和值(Arguments and Values):

        description---一个字符串[string]或 nil.

* 描述(Description):

        short-site-name 和 long-site-name 返回一个标识这个计算机硬件的物理部署信息的字符串[string], 如果不能产生适当的描述, 就返回 nil.

* 示例(Examples):

    ```LISP
    (short-site-name)
    =>  "MIT AI Lab"
    OR=>  "CMU-CSD"
    (long-site-name)
    =>  "MIT Artificial Intelligence Laboratory"
    OR=>  "CMU Computer Science Department"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        具体实现, 计算机硬件的物理部署信息, 以及安装/配置过程.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-MACHINE-INSTANCE">函数 MACHINE-INSTANCE</span>

* 语法(Syntax):

        machine-instance <no arguments> => description

* 参数和值(Arguments and Values):

        description---一个字符串[string]或 nil.

* 描述(Description):

        返回一个标识 Common Lisp 正在运行的这个计算机硬件的特定实例的字符串[string], 如果没有这样的字符串[string]可以被计算就返回 nil.

* 示例(Examples):

    ```LISP
    (machine-instance)
    =>  "ACME.COM"
    OR=>  "S/N 123231"
    OR=>  "18.26.0.179"
    OR=>  "AA-00-04-00-A7-A4"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        机器实例, 以及具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        machine-type, machine-version

* 注意(Notes): None.


### <span id="F-MACHINE-TYPE">函数 MACHINE-TYPE</span>

* 语法(Syntax):

        machine-type <no arguments> => description

* 参数和值(Arguments and Values):

        description---一个字符串[string]或 nil.

* 描述(Description):

        返回一个标识这个 Common Lisp 正在运行的计算机硬件的通用名称的字符串[string].

* 示例(Examples):

    ```LISP
    (machine-type)
    =>  "DEC PDP-10"
    OR=>  "Symbolics LM-2"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        这个机器类型. 具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        machine-version

* 注意(Notes): None.


### <span id="F-MACHINE-VERSION">函数 MACHINE-VERSION</span>

* 语法(Syntax):

        machine-version <no arguments> => description

* 参数和值(Arguments and Values):

        description---一个字符串[string]或 nil.

* 描述(Description):

        返回一个标识这个 Common Lisp 正在运行的计算机硬件的版本的字符串[string], 如果不能计算出这样的值就返回 nil.

* 示例(Examples):

    ```LISP
    (machine-version) =>  "KL-10, microcode 9"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        机器的版本, 和具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        machine-type, machine-instance

* 注意(Notes): None.


### <span id="F-SOFTWARE-ALL">函数 SOFTWARE-TYPE, SOFTWARE-VERSION</span>

* 语法(Syntax):

        software-type <no arguments> => description

        software-version <no arguments> => description

* 参数和值(Arguments and Values):

        description---一个字符串[string]或 nil.

* 描述(Description):

        software-type 返回一个标识任何相关支持软件的通用名字的字符串[string], 如果不能产生适当的或相关的值就返回 nil.

        software-version 返回一个标识任何相关支持软件的版本的字符串[string], 如果不能产生适当的或相关的值就返回 nil.

* 示例(Examples):

    ```LISP
    (software-type) =>  "Multics"
    (software-version) =>  "1.3x"
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        操作系统环境.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        这些信息应该对具体实现[implementation]的维护者有用.


### <span id="F-USER-HOMEDIR-PATHNAME">函数 USER-HOMEDIR-PATHNAME</span>

* 语法(Syntax):

        user-homedir-pathname &optional host => pathname

* 参数和值(Arguments and Values):

        host---一个字符串[string], 一个字符串[string]列表[list], 或 :unspecific.
        pathname---一个路径名[pathname]或 nil.

* 描述(Description):

        user-homedir-pathname 确定在主机 host 上对应用户家目录的路径名[pathname]. 如果没有提供 host, 它的值依赖于具体实现[implementation-dependent]. 对于一个 :unspecific 的描述, 见章节 19.2.1 (路径名成员).

        家目录的定义是依赖于具体实现的[implementation-dependent], 但是在 Common Lisp 中的定义意味着用户保存例如初始化文件和邮件之类的个人文件的目录.

        user-homedir-pathname 为主机 host 上的用户家目录返回一个不带任何名称, 类型, 或版本成员的路径名[pathname] (这些成员都是 nil).

        如果不能去确定主机 host 上的用户家目录, 就么就会返回 nil. 如果没有提供 host, 那么 user-homedir-pathname 从不返回 nil.

* 示例(Examples):

    ```LISP
    (pathnamep (user-homedir-pathname)) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        主机计算机文件系统, 以及具体实现[implementation].

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.