# 21 流

> * 21.1 [流的概念](#StreamConcepts)
> * 21.2 [流的字典](#TheStreamsDictionary)

## 21.1 <span id="StreamConcepts">流的概念</span>

> * 21.1.1 [流的介绍](#IntroductionStreams)
> * 21.1.2 [流变量](#StreamVariables)
> * 21.1.3 [给标准函数的流参数](#StreamArgStandFun)
> * 21.1.4 [复合流上的限制](#RestrictCompositeStreams)

### 21.1.1 <span id="IntroductionStreams">流的介绍</span>

流是一个对象, 它可以与输入或输出函数一起使用, 以确定该操作的字符或字节的适当源或接收器. 一个字符流是一个字符的源或接收器. 一个二进制流是一个字节的源或接收器.

一些操作符可以在任何种类的流上执行; 下一段中提供了一个标准操作符的列表, 这些操作可能对任何类型的流都有用

    close                 stream-element-type  
    input-stream-p        streamp              
    interactive-stream-p  with-open-stream     
    output-stream-p                            

    Figure 21-1. 一些一般用途的流操作

其他操作只对特定的流类型有意义. 比如, read-char 只定义给字符流而 read-byte 只定义给二进制流.

> * 21.1.1.1 [抽象的流分类](#AbstractClassStreams1)
> * 21.1.1.2 [抽象的流分类](#AbstractClassStreams2)
> * 21.1.1.3 [流的其他子类](#OtherSubclassesStream)

#### 21.1.1.1 <span id="AbstractClassStreams1">抽象的流分类</span>

##### 21.1.1.1.1 输入, 输出, 和双向流

一个流, 不管是字符流或二进制流, 都可以是一个输入流 (数据源), 一个输出流 (数据的接收器), 两者都是, 或者 (比如, 当 ":direction :probe" 给定给 open 时) 都不是.

下一段中展示了和输入流相关的操作符.

    clear-input  read-byte            read-from-string            
    listen       read-char            read-line                   
    peek-char    read-char-no-hang    read-preserving-whitespace  
    read         read-delimited-list  unread-char                 

    Figure 21-2. 和输入流相关的操作符.

下一段中展示了和输出流相关的操作符.

    clear-output   prin1            write            
    finish-output  prin1-to-string  write-byte       
    force-output   princ            write-char       
    format         princ-to-string  write-line       
    fresh-line     print            write-string     
    pprint         terpri           write-to-string  

    Figure 21-3. 和输出流相关的操作符.

一个同时为输入流和输出流的流被称为双向流. 见函数 input-stream-p 和 output-stream-p.

任何在 Figure 21-2 或 Figure 21-3 列出的操作符可以和双向流一起使用. 另外, 下一段列出了与双向流相关的操作符列表.

    y-or-n-p  yes-or-no-p    

    Figure 21-4. 和双向流相关的操作符. 


##### 21.1.1.1.2 打开和关闭的流

流可以使打开的或关闭的.

除非被显式指定, 否则创建和返回流的操作返回的是打开的流.

关闭流的操作标志着它作为数据来源或接收器的结束, 允许实现收回其内部数据结构, 并释放在打开时可能被流锁定的任何外部资源.

除非被显式指定, 否则当一个关闭的流被用于调用流时后果是未定义的.

对于一个关闭的流, 把流强制转换为路径名是允许的; 在一些情况下, 例如在一个真实名字计算中, 对于一个打开的流和同一流被关闭后的结果是不同的. 

##### 21.1.1.1.3 Interactive Streams

交互式流是可以在上面执行交互式查询的一种流.

一个交互式流的准确定义是具体实现定义的, 并且可能依赖于底层操作系统. 实现可能选择用作识别交互流的特征的东西的一些示例包括:

* 这个流与一个人 (或同等的) 连接在一起, 程序可以提示信息并期望根据提示接收不同的输入 The stream is connected to a person (or equivalent) in such a way that the program can prompt for information and expect to receive different input depending on the prompt.<!-- TODO 待校验 -->

* 该程序期望提示输入和支持 "正常的输入编辑".

* read-char 可能等待用户在返回之前去输入一些东西而不是立即返回一个字符或 end-of-file.

将一些流划分为交互式流的一般意图是允许它们与包含批处理 (或后台或命令文件) 输入的流区分开来. 批处理流的输出通常被丢弃或保存以供以后查看, 因此对此类流的交互式查询可能没有预期的效果.

终端 I/O 可能是也可能不是一个交互式流. 

#### 21.1.1.2 <span id="AbstractClassStreams2">抽象的流分类</span>

##### 21.1.1.2.1 文件流

一些流, 称为文件流, 提供了对文件的访问. 一个 file-stream 类的对象被用来表示一个文件流.

打开一个文件的基础操作是 open, 它通常返回一个文件流 (详情见它的目录条目). 关闭一个流的基础操作是 close. 宏 with-open-file 是用来表达在给定代码主体的持续时间内打开文件的常用习惯，并确保在退出该主体时关闭那个产生的流.

#### 21.1.1.3 <span id="OtherSubclassesStream">流的其他子类</span>

类 stream 有许多由这个规范定义的子类. 下面这段展示了关于这些子类的一些信息.

    类                  相关操作符             
    broadcast-stream     make-broadcast-stream         
                        broadcast-stream-streams      
    concatenated-stream  make-concatenated-stream      
                        concatenated-stream-streams   
    echo-stream          make-echo-stream              
                        echo-stream-input-stream      
                        echo-stream-output-stream     
    string-stream        make-string-input-stream      
                        with-input-from-string        
                        make-string-output-stream     
                        with-output-to-string         
                        get-output-stream-string      
    synonym-stream       make-synonym-stream           
                        synonym-stream-symbol         
    two-way-stream       make-two-way-stream           
                        two-way-stream-input-stream   
                        two-way-stream-output-stream  

    Figure 21-5. 和特定的流相关的已定义的名字

### 21.1.2 <span id="StreamVariables">流变量</span>

值必须是流的变量有时称为流变量.

特定的流变量被这个规范定义为在没有指定特定流的情况下, 作为输入或输出的适当来源. 下一个段中会出现一个完整的标准流变量列表. 如果任何时候这些变量的任意一个的值不是一个打开的流, 后果是未定义的.

    词汇术语          变量名      
    debug I/O        *debug-io*         
    error output     *error-output*     
    query I/O        *query-io*         
    standard input   *standard-input*   
    standard output  *standard-output*  
    terminal I/O     *terminal-io*      
    trace output     *trace-output*     

    Figure 21-6. 标准流变量

注意, 按照惯例, 标准流变量如果一定式输入流, 那么就以 "-input*" 结尾, 如果一定是输出流那么就以 "-output*" 结尾, 如果一定是双向流就以 "-io*" 结尾.

用户程序可以赋值或绑定任何标准流变量除了 \*terminal-io\*. 


### 21.1.3 <span id="StreamArgStandFun">给标准函数的流参数</span>

下面这段中的操作符接受打开或关闭的流作为流参数.

    broadcast-stream-streams     file-author       pathnamep                     
    close                        file-namestring   probe-file                    
    compile-file                 file-write-date   rename-file                   
    compile-file-pathname        host-namestring   streamp                       
    concatenated-stream-streams  load              synonym-stream-symbol         
    delete-file                  logical-pathname  translate-logical-pathname    
    directory                    merge-pathnames   translate-pathname            
    directory-namestring         namestring        truename                      
    dribble                      open              two-way-stream-input-stream   
    echo-stream-input-stream     open-stream-p     two-way-stream-output-stream  
    echo-stream-ouput-stream     parse-namestring  wild-pathname-p               
    ed                           pathname          with-open-file                
    enough-namestring            pathname-match-p                                

    Figure 21-7. 接受打开或关闭的流的操作符

下面这段中的操作符只接受打开的流作为流参数.

    clear-input               output-stream-p          read-char-no-hang           
    clear-output              peek-char                read-delimited-list         
    file-length               pprint                   read-line                   
    file-position             pprint-fill              read-preserving-whitespace  
    file-string-length        pprint-indent            stream-element-type         
    finish-output             pprint-linear            stream-external-format      
    force-output              pprint-logical-block     terpri                      
    format                    pprint-newline           unread-char                 
    fresh-line                pprint-tab               with-open-stream            
    get-output-stream-string  pprint-tabular           write                       
    input-stream-p            prin1                    write-byte                  
    interactive-stream-p      princ                    write-char                  
    listen                    print                    write-line                  
    make-broadcast-stream     print-object             write-string                
    make-concatenated-stream  print-unreadable-object  y-or-n-p                    
    make-echo-stream          read                     yes-or-no-p                 
    make-synonym-stream       read-byte                                            
    make-two-way-stream       read-char                                            

    Figure 21-8. 只接受打开的流的操作符

### 21.1.4 <span id="RestrictCompositeStreams">复合流上的限制</span>

如果一个复合流的任意成员在这个流被关闭之前关闭, 那么后果是未定义的.

如果 synonym-stream 流符号从它被创建到它被关闭这段时间都没有被绑定到一个打开的流, 那么后果是未定义的. 

## 21.2 <span id="TheStreamsDictionary">流的字典</span>

> * [系统类 STREAM](#SC-STREAM)
> * [系统类 BROADCAST-STREAM](#SC-BROADCAST-STREAM)
> * [系统类 CONCATENATED-STREAM](#SC-CONCATENATED-STREAM)
> * [系统类 ECHO-STREAM](#SC-ECHO-STREAM)
> * [系统类 FILE-STREAM](#SC-FILE-STREAM)
> * [系统类 STRING-STREAM](#SC-STRING-STREAM)
> * [系统类 SYNONYM-STREAM](#SC-SYNONYM-STREAM)
> * [系统类 TWO-WAY-STREAM](#SC-TWO-WAY-STREAM)
> * [函数 INPUT-STREAM-P, OUTPUT-STREAM-P](#F-INPUT-AND-OUTPUT-STREAM-P)
> * [函数 INTERACTIVE-STREAM-P](#F-INTERACTIVE-STREAM-P)
> * [函数 OPEN-STREAM-P](#F-OPEN-STREAM-P)
> * [函数 STREAM-ELEMENT-TYPE](#F-STREAM-ELEMENT-TYPE)
> * [函数 STREAMP](#F-STREAMP)
> * [函数 READ-BYTE](#F-READ-BYTE)
> * [函数 WRITE-BYTE](#F-WRITE-BYTE)
> * [函数 PEEK-CHAR](#F-PEEK-CHAR)
> * [函数 READ-CHAR](#F-READ-CHAR)
> * [函数 READ-CHAR-NO-HANG](#F-READ-CHAR-NO-HANG)
> * [函数 TERPRI, FRESH-LINE](#F-TERPRI-FRESH-LINE)
> * [函数 UNREAD-CHAR](#F-UNREAD-CHAR)
> * [函数 WRITE-CHAR](#F-WRITE-CHAR)
> * [函数 READ-LINE](#F-READ-LINE)
> * [函数 WRITE-STRING, WRITE-LINE](#F-WRITE-STRING-WRITE-LINE)
> * [函数 READ-SEQUENCE](#F-READ-SEQUENCE)
> * [函数 WRITE-SEQUENCE](#F-WRITE-SEQUENCE)
> * [函数 FILE-LENGTH](#F-FILE-LENGTH)
> * [函数 FILE-POSITION](#F-FILE-POSITION)
> * [函数 FILE-STRING-LENGTH](#F-FILE-STRING-LENGTH)
> * [函数 OPEN](#F-OPEN)
> * [函数 STREAM-EXTERNAL-FORMAT](#F-STREAM-EXTERNAL-FORMAT)
> * [宏 WITH-OPEN-FILE](#M-WITH-OPEN-FILE)
> * [函数 CLOSE](#F-CLOSE)
> * [宏 WITH-OPEN-STREAM](#M-WITH-OPEN-STREAM)
> * [函数 LISTEN](#F-LISTEN)
> * [函数 CLEAR-INPUT](#F-CLEAR-INPUT)
> * [函数 FINISH-OUTPUT, FORCE-OUTPUT, CLEAR-OUTPUT](#F-FINISH-AND-FORCE-AND-CLEAR-OUTPUT)
> * [函数 Y-OR-N-P, YES-OR-NO-P](#F-Y-OR-N-P-YES-OR-NO-P)
> * [函数 MAKE-SYNONYM-STREAM](#F-MAKE-SYNONYM-STREAM)
> * [函数 SYNONYM-STREAM-SYMBOL](#F-SYNONYM-STREAM-SYMBOL)
> * [函数 BROADCAST-STREAM-STREAMS](#F-BROADCAST-STREAM-STREAMS)
> * [函数 MAKE-BROADCAST-STREAM](#F-MAKE-BROADCAST-STREAM)
> * [函数 MAKE-TWO-WAY-STREAM](#F-MAKE-TWO-WAY-STREAM)
> * [函数 TWO-WAY-STREAM-INPUT-STREAM, TWO-WAY-STREAM-OUTPUT-STREAM](#F-T-W-S-INPUT-AND-OUTPUT-STREAM)
> * [函数 ECHO-STREAM-INPUT-STREAM, ECHO-STREAM-OUTPUT-STREAM](#F-ECHO-STREAM-INPUT-AND-OUTPUT-STREAM)
> * [函数 MAKE-ECHO-STREAM](#F-MAKE-ECHO-STREAM)
> * [函数 CONCATENATED-STREAM-STREAMS](#F-CONCATENATED-STREAM-STREAMS)
> * [函数 MAKE-CONCATENATED-STREAM](#F-MAKE-CONCATENATED-STREAM)
> * [函数 GET-OUTPUT-STREAM-STRING](#F-GET-OUTPUT-STREAM-STRING)
> * [函数 MAKE-STRING-INPUT-STREAM](#F-MAKE-STRING-INPUT-STREAM)
> * [函数 MAKE-STRING-OUTPUT-STREAM](#F-MAKE-STRING-OUTPUT-STREAM)
> * [宏 WITH-INPUT-FROM-STRING](#M-WITH-INPUT-FROM-STRING)
> * [宏 WITH-OUTPUT-TO-STRING](#M-WITH-OUTPUT-TO-STRING)
> * [变量 *DEBUG-IO*, *ERROR-OUTPUT*, *QUERY-IO*, *STANDARD-INPUT*, *STANDARD-OUTPUT*, *TRACE-OUTPUT*](#V-IO-ALL)
> * [变量 *TERMINAL-IO*](#V-TERMINAL-IO)
> * [状况类型 STREAM-ERROR](#CT-STREAM-ERROR)
> * [函数 STREAM-ERROR-STREAM](#F-STREAM-ERROR-STREAM)
> * [状况类型 END-OF-FILE](#CT-END-OF-FILE)


### <span id="SC-STREAM">系统类 STREAM</span>

* 类优先级列表(Class Precedence List):

        stream, t

* 描述(Description):

        一个流式一个可以和一个输入或输出函数一起使用, 以确定该操作的字符或字节的适当源或接收器.

        关于更多完整信息, 见章节 21.1 (流的概念).

* 也见(See Also):

        章节 21.1 (流的概念), 章节 22.1.3.13 (Printing Other Objects), 章节 22 (Printer), 章节 23 (Reader) 

### <span id="SC-BROADCAST-STREAM">系统类 BROADCAST-STREAM</span>

* 类优先级列表(Class Precedence List):

        broadcast-stream, stream, t

* 描述(Description):

        一个广播流式一个输出流, 它和一个包含零个或多个输出流的集合关联, 这样一来发送到这个广播流的任何输出会作为输出传递到每一个关联的输出流上. (如果一个广播流没有成员流, 那么所有到广播流的输出会被废弃.)

        可以在一个广播流上执行的操作的集合是可应用到它关联的那些输出流的操作的交集.

        一些输出操作 (比如, fresh-line) 返回基于这个操作时那个流的状态的值. 由于这些值可能有别于这些成员流的每一个, 有必要具体描述它们的返回值:

        * stream-element-type 从最后一个成员流中返回这个值, 如果这里没有成员流就返回 t.

        * fresh-line returns 从最后一个成员流中返回这个值, 如果这里没有成员流就返回 nil.

        * 函数 file-length, file-position, file-string-length, 和 stream-external-format 从最后一个成员流来返回值; 如果这里没有成员流, file-length 和 file-position 返回 0, file-string-length 返回 1, 而 stream-external-format 返回 :default.

        * 函数 streamp 和 output-stream-p 对于广播流总是返回 true.

        * 函数 open-stream-p 检验这个广播流是否打开, 而不是它的成员流是否打开.

        * 函数 input-stream-p 和 interactive-stream-p 返回一个具体实现定义的, 广义 boolean 值.

        * 对于输入操作 clear-input, listen, peek-char, read-byte, read-char-no-hang, read-char, read-line, 和 unread-char, 如果所指示的操作被执行, 其后果是未定义的. 然而, 一个实现允许去定义这样一个行为作为一个依赖于具体实现的扩展.

        对于任何上面或这个文档的其他地方没有显式指定返回值的输出操作, 定义了这样一个操作返回的值式在它的最后一个成员流上执行这个操作产生的值; 在前面所有的流上执行这个操作产生的值会被丢弃. 如果这里没有成员流, 这个值是依赖于具体实现的. 

* 也见(See Also):

        broadcast-stream-streams, make-broadcast-stream 


### <span id="SC-CONCATENATED-STREAM">系统类 CONCATENATED-STREAM</span>

* 类优先级列表(Class Precedence List):

        concatenated-stream, stream, t

* 描述(Description):

        一个串联流是一个输入流, 它式一个包含零个或多个输入流的复合流, 这样, 可以从串联流中读取的数据序列与可以从每个组成流中读取的数据序列的连接相同.

        来自串联流的输入从第一个相关的输入流中获取, 直到它到达文件的末尾; 然后这个流会被废弃, 并且后续的输入取自于下一个输入流, 以此类推. 在相关输入流上的文件结束总是由串联流来管理---串联流的客户端只有在尝试从串联流中获取数据，但是它没有剩余的输入流来获取这些数据时才会看到文件的结束.

* 也见(See Also):

        concatenated-stream-streams, make-concatenated-stream 


### <span id="SC-ECHO-STREAM">系统类 ECHO-STREAM</span>

* 类优先级列表(Class Precedence List):

        echo-stream, stream, t

* 描述(Description):

        一个回音流是一个双向流, 它从一个关联的输入流中得到它的输入并且发送它的输出到一个关联的输出流.

        所有从输入流中获取的输入都与输出流相呼应. 输入是否在遇到后或者在从输入流中读取之后立即响应, 是依赖于具体实现的.

* 也见(See Also):

        echo-stream-input-stream, echo-stream-output-stream, make-echo-stream 


### <span id="SC-FILE-STREAM">系统类 FILE-STREAM</span>

* 类优先级列表(Class Precedence List):

        file-stream, stream, t

* 描述(Description):

        一个 file-stream 类型的对象是一个直接源和接收器都是一个文件的流. 这样一个流通过 open 和 with-open-file 显式创建, 以及通过像 load 这样的处理文件的函数隐式打开.

* 也见(See Also):

        load, open, with-open-file 


### <span id="SC-STRING-STREAM">系统类 STRING-STREAM</span>

* 类优先级列表(Class Precedence List):

        string-stream, stream, t

* 描述(Description):

        一个字符串流式一个读取输入自一个关联字符串或写入输出到一个关联字符串的流.

        一个字符串流的流元素类型总是为 character 类型的子类型.

* 也见(See Also):

        make-string-input-stream, make-string-output-stream, with-input-from-string, with-output-to-string 


### <span id="SC-SYNONYM-STREAM">系统类 SYNONYM-STREAM</span>

* 类优先级列表(Class Precedence List):

        synonym-stream, stream, t

* 描述(Description):

        一个是另一个流的别名的流, 它是一个动态变量的值, 这个变量的名字是这个同义流的同义流符号.

        任何在一个同义流上的操作会在一个流上被执行, 这个流是由那个同义流符号命名的动态变量的值. 如果那个变量的值将要改变, 或者如果那个变量的值将要被绑定, 那么这个流会操作在那个变量的新值上.

* 也见(See Also):

        make-synonym-stream, synonym-stream-symbol 


### <span id="SC-TWO-WAY-STREAM">系统类 TWO-WAY-STREAM</span>

* 类优先级列表(Class Precedence List):

        two-way-stream, stream, t

* 描述(Description):

        一个从一个关联输入流中接收它的输入并且发送它的输出到一个关联的输出流的双向复合流.

* 也见(See Also):

        make-two-way-stream, two-way-stream-input-stream, two-way-stream-output-stream 


### <span id="F-INPUT-AND-OUTPUT-STREAM-P">函数 INPUT-STREAM-P, OUTPUT-STREAM-P</span>

* 语法(Syntax):

        input-stream-p stream => generalized-boolean

        output-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个输入流, input-stream-p 返回 true; 否则, 返回 false.

        如果流 stream 是一个输出流, output-stream-p 返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (input-stream-p *standard-input*) =>  true
    (input-stream-p *terminal-io*) =>  true
    (input-stream-p (make-string-output-stream)) =>  false

    (output-stream-p *standard-output*) =>  true
    (output-stream-p *terminal-io*) =>  true
    (output-stream-p (make-string-input-stream "jr")) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-INTERACTIVE-STREAM-P">函数 INTERACTIVE-STREAM-P</span>

* 语法(Syntax):

        interactive-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个交互式流, 就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (when (> measured limit)
      (let ((error (round (* (- measured limit) 100)
                          limit)))
        (unless (if (interactive-stream-p *query-io*)
                    (yes-or-no-p "The frammis is out of tolerance by ~D%.~@
                                  Is it safe to proceed? " error)
                    (< error 15))  ;15% is acceptable
          (error "The frammis is out of tolerance by ~D%." error))))
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 21.1 (流的概念)

* 注意(Notes): None. 


### <span id="F-OPEN-STREAM-P">函数 OPEN-STREAM-P</span>

* 语法(Syntax):

        open-stream-p stream => generalized-boolean

* 参数和值(Arguments and Values):

        stream---一个流.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果流 stream 是一个打开的流就返回 true; 否则, 返回 false.

        直到流被 close 显式关闭, 或者直到退出 with-output-to-string, with-open-file, with-input-from-string, 或 with-open-stream 表达式形式而被隐式关闭之前, 流都是打开的.

* 示例(Examples):

    ```LISP
    (open-stream-p *standard-input*) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        close.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-STREAM-ELEMENT-TYPE">函数 STREAM-ELEMENT-TYPE</span>

* 语法(Syntax):

        stream-element-type stream => typespec

* 参数和值(Arguments and Values):

        stream---一个流.
        typespec---一个类型指定符.

* 描述(Description):

        stream-element-type 返回可以从流 stream 中读到或写入到流 stream 中的对象的一个类型指定符.

        由 open 创建的流有着被约束为 integer 或一个 character 类型的子类型的元素类型.

* 示例(Examples):

    ```LISP
    ;; Note that the stream must accomodate at least the specified type,
    ;; but might accomodate other types.  Further note that even if it does
    ;; accomodate exactly the specified type, the type might be specified in
    ;; any of several ways.
    (with-open-file (s "test" :element-type '(integer 0 1)
                              :if-exists :error
                              :direction :output)
      (stream-element-type s))
    =>  INTEGER
    OR=>  (UNSIGNED-BYTE 16)
    OR=>  (UNSIGNED-BYTE 8)
    OR=>  BIT
    OR=>  (UNSIGNED-BYTE 1)
    OR=>  (INTEGER 0 1)
    OR=>  (INTEGER 0 (2))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None.

### <span id="F-STREAMP">函数 STREAMP</span>

* 语法(Syntax):

        streamp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 stream 类型就返回 true; 否则, 返回 false.

        如果对象 object 是一个流, streamp 不会被这个流是打开的还是关闭的所影响.

* 示例(Examples):

    ```LISP
    (streamp *terminal-io*) =>  true
    (streamp 1) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (streamp object) ==  (typep object 'stream)

### <span id="F-READ-BYTE">函数 READ-BYTE</span>

* 语法(Syntax):

        read-byte stream &optional eof-error-p eof-value => byte

* 参数和值(Arguments and Values):

        stream---一个二进制输入流.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        byte---一个整数, 或者是 eof-value.

* 描述(Description):

        read-byte 从流 stream 中读取并返回一个字节.

        如果到达了文件的末尾并且 eof-error-p 是 false, 那么返回这个 eof-value.

* 示例(Examples):

    ```LISP
    (with-open-file (s "temp-bytes" 
                        :direction :output
                        :element-type 'unsigned-byte)
        (write-byte 101 s)) =>  101
    (with-open-file (s "temp-bytes" :element-type 'unsigned-byte)
        (format t "~S ~S" (read-byte s) (read-byte s nil 'eof)))
    >>  101 EOF
    =>  NIL
    ```

* 副作用(Side Effects):

        修改流 stream.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误.

        如果 stream 不是一个二进制输入流, 那么应该发出一个 error 类型的错误.

        如果在流 stream 中没有剩余字节并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        read-char, read-sequence, write-byte

* 注意(Notes): None. 


### <span id="F-WRITE-BYTE">函数 WRITE-BYTE</span>

* 语法(Syntax):

        write-byte byte stream => byte

* 参数和值(Arguments and Values):

        byte---一个流的流元素类型的整数.
        stream---一个二进制输出流.

* 描述(Description):

        write-byte 向流 stream 写入一个字节 byte.

* 示例(Examples):

    ```LISP
    (with-open-file (s "temp-bytes" 
                        :direction :output
                        :element-type 'unsigned-byte)
        (write-byte 101 s)) =>  101
    ```

* 副作用(Side Effects):

        流 stream 会被修改.

* 受此影响(Affected By):

        流 stream 的元素类型.

* 异常情况(Exceptional Situations):

        如果 stream 不是一个流, 那么应该发出一个 type-error 类型的错误. 如果 stream 不是一个二进制输出流, 那么应该发出一个 error 类型的错误.

        如果 byte 不是流 stream 的一个流元素类型的整数, 可能会发出一个 type-error 类型的错误.

* 也见(See Also):

        read-byte, write-char, write-sequence

* 注意(Notes): None. 


### <span id="F-PEEK-CHAR">函数 PEEK-CHAR</span>

* 语法(Syntax):

        peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        peek-type---一个字符或 t 或 nil.
        input-stream---输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 eof-value.

* 描述(Description):
<!--TODO 待校验-->
        peek-char 在没有实际读取的情况下获取流 input-stream 中的下一个字符, 这样就可以在以后的时间里读取这个字符. 它还可以用于跳过并丢弃输入流 input-stream 中的中间字符, 直到找到一个特定的字符为止.

        如果 peek-type 没有被提供或者是 nil, peek-char 返回要从 input-stream 中读取的下一个字符, 实际上没有把它从 input-stream 中移除. 下一次从 input-stream 完成输入时, 这个字符仍然在那里. 如果 peek-type 是 t, 那么 peek-char 跳过空白字符, 但是不跳过注释, 然后在下一个字符执行窥视操作. 最后一个被检验的字符, 开始一个对象的那个, 不会从 input-stream 中被移除. 如果 peek-type 是一个字符, 那么 peek-char 跳过输入字符直到找到一个和那个字符 char= 的字符; 那个字符留在 input-stream 中.

        如果到达了文件的末尾并且 eof-error-p 是 false, 就返回 eof-value.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        当 input-stream 是一个回音流时, 只是被窥视的字符不会被重复. 在这个 peek-type 不是 nil 的情况中, 通过 peek-char 传递的这些字符会被当作通过 read-char 一样处理, 因此, 除非它们被 unread-char 标记出来否则就会得到回响.

* 示例(Examples):

    ```LISP
    (with-input-from-string (input-stream "    1 2 3 4 5")
        (format t "~S ~S ~S" 
                (peek-char t input-stream)
                (peek-char #\4 input-stream)
                (peek-char nil input-stream)))
    >>  #\1 #\4 #\4
    =>  NIL
    ```

* 受此影响(Affected By):

        *readtable*, *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果 eof-error-p 是 true 并且到达文件的末尾, 就会发出一个 end-of-file 类型的错误.

        如果 peek-type 是一个字符, 到达文件的末尾, 并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

        如果 recursive-p 是 true 到达文件的末尾, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-READ-CHAR">函数 READ-CHAR</span>

* 语法(Syntax):

        read-char &optional input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 eof-value.

* 描述(Description):

        read-char 从 input-stream 返回下一个字符.

        当 input-stream 是一个回音流时, 在第一次看到这个字符时这个字符在 input-stream 上被响应. 不被read-char 所响应的字符是 unread-char 放在那里的, 因此被认为已经在之前的一次对 read-char 的调用中得到了响应.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        如果到达文件的末尾并且 eof-error-p 是 false, 就返回 eof-value.

* 示例(Examples):

    ```LISP
    (with-input-from-string (is "0123")
        (do ((c (read-char is) (read-char is nil 'the-end)))
            ((not (characterp c)))
        (format t "~S " c)))
    >>  #\0 #\1 #\2 #\3
    =>  NIL
    ```

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果在一个字符可以被读取之前到达文件的末尾, 并且 eof-error-p 是 true, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        read-byte, read-sequence, write-char, read

* 注意(Notes):

        对应的输入函数是 write-char. 


### <span id="F-READ-CHAR-NO-HANG">函数 READ-CHAR-NO-HANG</span>

* 语法(Syntax):

        read-char-no-hang &optional input-stream eof-error-p eof-value recursive-p => char

* 参数和值(Arguments and Values):

        input-stream -- 一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        char---一个字符或 nil 或 eof-value.

* 描述(Description):

        如果一个字符可用, read-char-no-hang 就从 input-stream 返回一个字符. 如果没有字符可用, read-char-no-hang 就返回 nil.

        如果 recursive-p 是 true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        如果到达文件末尾并且 eof-error-p 是 false, 就返回 eof-value.

* 示例(Examples):

    ```LISP
    ;; This code assumes an implementation in which a newline is not
    ;; required to terminate input from the console.
    (defun test-it ()
      (unread-char (read-char))
      (list (read-char-no-hang) 
            (read-char-no-hang) 
            (read-char-no-hang)))
    =>  TEST-IT
    ;; Implementation A, where a Newline is not required to terminate
    ;; interactive input on the console.
    (test-it)
    >>  a
    =>  (#\a NIL NIL)
    ;; Implementation B, where a Newline is required to terminate
    ;; interactive input on the console, and where that Newline remains
    ;; on the input stream.
    (test-it)
    >>  a<NEWLINE>
    =>  (#\a #\Newline NIL)
    ```

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        当 eof-error-p 是 true 时, 如果到达文件末尾, 就会发出一个 end-of-file 类型的错误.

* 也见(See Also):

        listen

* 注意(Notes):

        read-char-no-hang 准确地像 read-char, 除了如果有必要去等待来获取一个字符 (比如来自于键盘), 会在没有等待的情况下立即返回 nil. 


### <span id="F-TERPRI-FRESH-LINE">函数 TERPRI, FRESH-LINE</span>

* 语法(Syntax):

        terpri &optional output-stream => nil

        fresh-line &optional output-stream => generalized-boolean

* 参数和值(Arguments and Values):

        output-stream -- 一个输出流标识符. 默认是标准输出.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        terpri 输出一个新行到 output-stream 中.

        fresh-line 类似于 terpri 但是当且仅当这个 output-stream 没有在一行的开头时输出一个新行. 如果出于某个原因不能确定这个, 那么无论怎样都会输出一个新行. 如果 fresh-line 输出一个新行就返回 true; 否则它返回 false.

* 示例(Examples):

    ```LISP
    (with-output-to-string (s)
        (write-string "some text" s)
        (terpri s)
        (terpri s)
        (write-string "more text" s))
    =>  "some text

    more text"
    (with-output-to-string (s)
        (write-string "some text" s)
        (fresh-line s)
        (fresh-line s)
        (write-string "more text" s))
    =>  "some text
    more text"
    ```

* 副作用(Side Effects):

        这个 output-stream 会被修改.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations):

        None.

* 也见(See Also): None.

* 注意(Notes):

      terpri 和下面这个效果是一样的

      (write-char #\Newline output-stream)


### <span id="F-UNREAD-CHAR">函数 UNREAD-CHAR</span>

* 语法(Syntax):

        unread-char character &optional input-stream => nil

* 参数和值(Arguments and Values):

        character---一个字符; 必须是从 input-stream 读到的最后一个字符.
        input-stream---一个输入流标识符. 默认是标准输入.

* 描述(Description):

        unread-char 把 character 放置回 input-stream 的前面, 这样一来它会再一次成为 input-stream 中的下一个字符.

        当 input-stream 是一个回音流时, 不要尝试去撤销字符 character 已经在 input-stream 上完成的任何响应. 然而, 通过 unread-char 放置在 input-stream 的字符以这样方式被标记来抑制后来通过 read-char 的再响应.

        在中间没有在一个流上调用 read-char (或者某个其他隐式读取字符的输入操作) 的情况下, 在同一个流上连续两次调用 unread-char 是一个错误.

        调用 peek-char 或 read-char 提交所有前面的字符. 在 peek-char 返回的任何字符之前调用 unread-char (包括由 peek-char 传递的那些具有非 nil peek-type 类型的字符) 的后果是不明确的. 特别是, 在 peek-char 之后调用 unread-char 的后果是未指定的.

* 示例(Examples):

    ```LISP
    (with-input-from-string (is "0123")
        (dotimes (i 6)
          (let ((c (read-char is)))
            (if (evenp i) (format t "~&~S ~S~%" i c) (unread-char c is)))))
    >>  0 #\0
    >>  2 #\1
    >>  4 #\2
    =>  NIL
    ```LISP

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        peek-char, read-char, 章节 21.1 (流的概念)

* 注意(Notes):

        unread-char 旨在成为一种有效的机制, 允许 Lisp 读取器和其他解析器在输入流 input-stream 中执行一个字符的展望. 


### <span id="F-WRITE-CHAR">函数 WRITE-CHAR</span>

* 语法(Syntax):

        write-char character &optional output-stream => character

* 参数和值(Arguments and Values):

        character---一个字符.
        output-stream -- 一个输出流标识符. 默认是标准输出.

* 描述(Description):

        write-char 把字符 character 输出到 output-stream.

* 示例(Examples):

    ```LISP
    (write-char #\a)
    >>  a
    =>  #\a
    (with-output-to-string (s) 
      (write-char #\a s)
      (write-char #\Space s)
      (write-char #\b s))
    =>  "a b"
    ```

* 副作用(Side Effects):

        流 output-stream 会被修改.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        read-char, write-byte, write-sequence

* 注意(Notes): None. 


### <span id="F-READ-LINE">函数 READ-LINE</span>

* 语法(Syntax):

        read-line &optional input-stream eof-error-p eof-value recursive-p
        => line, missing-newline-p

* 参数和值(Arguments and Values):

        input-stream---一个输入流标识符. 默认是标准输入.
        eof-error-p---一个广义 boolean. 默认是 true.
        eof-value---一个对象. 默认是 nil.
        recursive-p---一个广义 boolean. 默认是 false.
        line---一个字符串或 eof-value.
        missing-newline-p---一个广义 boolean.

* 描述(Description):

        从 input-stream 读取一个文本行, 由一个新行标识或文件末尾来结尾.

        If recursive-p is true, 这个调用预期会被嵌入到一个更高层次的对 read 或一个 Lisp 读取器使用的类似函数的调用中.

        这个主要的值, line, 是读取到的那行, 表示为一个字符串 (如果有这个字符串的话, 不带末尾的新行标识). 如果 eof-error-p 是 false 并且在读到任何字符之前到达 input-stream 的文件末尾, 返回 eof-value 作为这个 line.

        第二个值, missing-newline-p, 是一个广义 boolean, 如果 line 是由新行标识来终止的就是 false, 如果 line 由 input-stream 文件结尾来终止(或者如果这个 line 就是 eof-value)就是 true .

* 示例(Examples):

    ```LISP
    (setq a "line 1
    line2")
    =>  "line 1
    line2"
    (read-line (setq input-stream (make-string-input-stream a)))
    =>  "line 1", false
    (read-line input-stream)
    =>  "line2", true
    (read-line input-stream nil nil)
    =>  NIL, true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-input*, *terminal-io*.

* 异常情况(Exceptional Situations):

        如果在这行中任何字符被读取到之前就到达文件的末尾, 如果 eof-error-p 是 true 就会发出一个错误.

* 也见(See Also):

        read

* 注意(Notes):

        对应输出函数是 write-line.


### <span id="F-WRITE-STRING-WRITE-LINE">函数 WRITE-STRING, WRITE-LINE</span>

* 语法(Syntax):

        write-string string &optional output-stream &key start end => string

        write-line string &optional output-stream &key start end => string

* 参数和值(Arguments and Values):

        string---一个字符串.
        output-stream -- 一个输出流标识符. 默认是标准输出.
        start, end---字符串 string 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.

* 描述(Description):

        write-string 把字符串 string 由 start 和 end 限定的子字符串的字符写到 output-stream 中. write-line 做相同的事, 但是会在之后输出一个新行标识.

* 示例(Examples):

    ```LISP
    (prog1 (write-string "books" nil :end 4) (write-string "worms"))
    >>  bookworms
    =>  "books"
    (progn (write-char #\*)
            (write-line "test12" *standard-output* :end 5) 
            (write-line "*test2")
            (write-char #\*)
            nil)
    >>  *test1
    >>  *test2
    >>  *
    =>  NIL
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By):

        *standard-output*, *terminal-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        read-line, write-char

* 注意(Notes):

        write-line 和 write-string 返回 string, 不是由 start 和 end 限定的子字符串.

        (write-string string)
        ==  (dotimes (i (length string)
              (write-char (char string i)))

        (write-line string)
        ==  (prog1 (write-string string) (terpri))

### <span id="F-READ-SEQUENCE">函数 READ-SEQUENCE</span>

* 语法(Syntax):

        read-sequence sequence stream &key start end => position

* 参数和值(Arguments and Values):

        sequence---一个序列.
        stream---一个输入流.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.
        position---一个大于等于零, 并且小于等于序列 sequence 长度的整数.

* 描述(Description):

        通过用读取自流 stream 的元素来替换序列 sequence 中由 start 和 end 限定的元素来破坏性地修改序列 sequence.

        通过从流 stream 中拷贝连续元素到序列 sequence 来破坏性地修改它. 如果在拷贝这个子序列的所有元素之前到达这个流 stream 的文件末尾, 那么在序列 sequence 的末尾旁边的元素不会被更新.

        position 是序列 sequence 中第一个没有被更新的元素的索引, 由于到达文件的末尾的话它可能小于 end.

* 示例(Examples):

    ```LISP
    (defvar *data* (make-array 15 :initial-element nil))
    (values (read-sequence *data* (make-string-input-stream "test string")) *data*)
    =>  11, #(#\t #\e #\s #\t #\Space #\s #\t #\r #\i #\n #\g NIL NIL NIL NIL)
    ```

* 副作用(Side Effects):

        修改流 stream 和 sequence.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 就应该准备发出一个 type-error 类型的错误. 如果 start 不是一个非负整数, 就会发出一个 type-error 类型的错误. 如果 end 不是一个非负整数或 nil, 就会发出一个 type-error 类型的错误.

        如果读取自流 stream 的一个元素不是 sequence 的一个元素类型的成员, 就会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (Compiler Terminology), write-sequence, read-line

* 注意(Notes):

        read-sequence 和遍历指定的子序列并每次从流 stream 中读取一个元素并将其存储到序列 sequence 中的效果是相同的, 但是可能比等价循环更有效率. 一个有效的实现更有可能存在于 sequence 是一个具有与 stream 相同的元素类型的向量的情况下. 


### <span id="F-WRITE-SEQUENCE">函数 WRITE-SEQUENCE</span>

* 语法(Syntax):

        write-sequence sequence stream &key start end => sequence

* 参数和值(Arguments and Values):

        sequence---一个序列.
        stream---一个输出流.
        start, end---序列 sequence 的边界索引标识符. 对于 start 和 end 默认分别为 0 和 nil.

* 描述(Description):

        write-sequence 把序列 sequence 中由 start 和 end 限定的子序列写入到流 stream 中.

* 示例(Examples):

    ```LISP
    (write-sequence "bookworms" *standard-output* :end 4)
    >>  book
    =>  "bookworms"
    ```

* 副作用(Side Effects):

        修改流 stream.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果序列 sequence 不是一个 proper 序列, 就应该准备发出一个 type-error 类型的错误. 如果 start 不是一个非负整数, 就会发出一个 type-error 类型的错误. 如果 end 不是一个非负整数或 nil, 就会发出一个 type-error 类型的错误.

        如果限定的序列 sequence 的一个元素不是流 stream 的流元素类型的一个成员, 可能会发出一个 type-error 类型的错误.

* 也见(See Also):

        章节 3.2.1 (Compiler Terminology), read-sequence, write-string, write-line

* 注意(Notes):

        write-sequence 和遍历指定的子序列并每次写入一个元素到流 stream 中的效果是相同的, 但是可能比等价循环更有效率. 一个有效的实现更有可能存在于 sequence 是一个具有与 stream 相同的元素类型的向量的情况下.


### <span id="F-FILE-LENGTH">函数 FILE-LENGTH</span>

* 语法(Syntax):

        file-length stream => length

* 参数和值(Arguments and Values):

        stream---和一个文件关联的流.
        length---一个非负整数或 nil.

* 描述(Description):

        file-length 返回流 stream 的长度, 如果长度不能确定就返回 nil.

        对于一个二进制文件, 长度是以流 stream 的元素类型的单位来测量的.

* 示例(Examples):

    ```LISP
    (with-open-file (s "decimal-digits.text" 
                        :direction :output :if-exists :error)
      (princ "0123456789" s)
      (truename s))
    =>  #P"A:>Joe>decimal-digits.text.1"
    (with-open-file (s "decimal-digits.text")
      (file-length s))
    =>  10
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果流 stream 不是一个和文件关联的流就会发出一个 type-error 类型的错误.

* 也见(See Also):

        open

* 注意(Notes): None. 

### <span id="F-FILE-POSITION">函数 FILE-POSITION</span>

* 语法(Syntax):

file-position stream => position

file-position stream position-spec => success-p

* 参数和值(Arguments and Values):

stream---一个流.

position-spec---a file position designator.

position---a file position or nil.

success-p---一个广义 boolean.

* 描述(Description):

Returns or changes the current position within a stream.

When position-spec is not supplied, file-position returns the current file position in the stream, or nil if this cannot be determined.

When position-spec is supplied, the file position in stream is set to that file position (if possible). file-position returns true if the repositioning is performed successfully, or false if it is not.

An integer returned by file-position of one argument should be acceptable as position-spec for use with the same file.

For a character file, performing a single read-char or write-char operation may cause the file position to be increased by more than 1 because of character-set translations (such as translating between the Common Lisp f#\Newline character and an external ASCII carriage-return/line-feed sequence) and other aspects of the implementation. For a binary file, every read-byte or write-byte operation increases the file position by 1.

* 示例(Examples):

 (defun tester ()
   (let ((noticed '()) file-written)
     (flet ((notice (x) (push x noticed) x))
       (with-open-file (s "test.bin" 
                          :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-exists :error)
          (notice (file-position s)) ;1
          (write-byte 5 s) 
          (write-byte 6 s)
          (let ((p (file-position s)))
            (notice p) ;2
            (notice (when p (file-position s (1- p))))) ;3
          (write-byte 7 s)
          (notice (file-position s)) ;4
          (setq file-written (truename s)))
        (with-open-file (s file-written
                           :element-type '(unsigned-byte 8)
                           :direction :input)
          (notice (file-position s)) ;5
          (let ((length (file-length s)))
            (notice length) ;6
            (when length
              (dotimes (i length)
                (notice (read-byte s)))))) ;7,...
        (nreverse noticed))))
=>  tester
 (tester)
=>  (0 2 T 2 0 2 5 7)
OR=>  (0 2 NIL 3 0 3 5 6 7)
OR=>  (NIL NIL NIL NIL NIL NIL)

* 副作用(Side Effects):

When the position-spec argument is supplied, the file position in the stream might be moved.

* 受此影响(Affected By):

The value returned by file-position increases monotonically as input or output operations are performed.

* 异常情况(Exceptional Situations):

If position-spec is supplied, but is too large or otherwise inappropriate, an error is signaled.

* 也见(See Also):

file-length, file-string-length, open

* 注意(Notes):

Implementations that have character files represented as a sequence of records of bounded size might choose to encode the file position as, for example, <<record-number>>*<<max-record-size>>+<<character-within-record>>. This is a valid encoding because it increases monotonically as each character is read or written, though not necessarily by 1 at each step. An integer might then be considered ``inappropriate'' as position-spec to file-position if, when decoded into record number and character number, it turned out that the supplied record was too short for the specified character number. 


### <span id="F-FILE-STRING-LENGTH">函数 FILE-STRING-LENGTH</span>

* 语法(Syntax):

file-string-length stream object => length

* 参数和值(Arguments and Values):

stream---an output character file stream.

object---a string or a character.

length---a non-negative integer, or nil.

* 描述(Description):

file-string-length returns the difference between what (file-position stream) would be after writing object and its current value, or nil if this cannot be determined.

The returned value corresponds to the current state of stream at the time of the call and might not be the same if it is called again when the state of the stream has changed.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-OPEN">函数 OPEN</span>

* 语法(Syntax):

open filespec &key direction element-type if-exists if-does-not-exist external-format

=> stream

* 参数和值(Arguments and Values):

filespec---a pathname designator.

direction---one of :input, :output, :io, or :probe. The default is :input.

element-type---a type specifier for recognizable subtype of character; or a type specifier for a finite recognizable subtype of integer; or one of the symbols signed-byte, unsigned-byte, or :default. The default is character.

if-exists---one of :error, :new-version, :rename, :rename-and-delete, :overwrite, :append, :supersede, or nil. The default is :new-version if the version component of filespec is :newest, or :error otherwise.

if-does-not-exist---one of :error, :create, or nil. The default is :error if direction is :input or if-exists is :overwrite or :append; :create if direction is :output or :io, and if-exists is neither :overwrite nor :append; or nil when direction is :probe.

external-format---an external file format designator. The default is :default.

stream---a file stream or nil.

* 描述(Description):

open creates, opens, and returns a file stream that is connected to the file specified by filespec. Filespec is the name of the file to be opened. If the filespec designator is a stream, that stream is not closed first or otherwise affected.

The keyword arguments to open specify the characteristics of the file stream that is returned, and how to handle errors.

If direction is :input or :probe, or if if-exists is not :new-version and the version component of the filespec is :newest, then the file opened is that file already existing in the file system that has a version greater than that of any other file in the file system whose other pathname components are the same as those of filespec.

An implementation is required to recognize all of the open keyword options and to do something reasonable in the context of the host operating system. For example, if a file system does not support distinct file versions and does not distinguish the notions of deletion and expunging, :new-version might be treated the same as :rename or :supersede, and :rename-and-delete might be treated the same as :supersede.

:direction

    These are the possible values for direction, and how they affect the nature of the stream that is created:

    :input

        Causes the creation of an input file stream.

    :output

        Causes the creation of an output file stream.

    :io

        Causes the creation of a bidirectional file stream.

    :probe

        Causes the creation of a ``no-directional'' file stream; in effect, the file stream is created and then closed prior to being returned by open.

:element-type

    The element-type specifies the unit of transaction for the file stream. If it is :default, the unit is determined by file system, possibly based on the file.

:if-exists

    if-exists specifies the action to be taken if direction is :output or :io and a file of the name filespec already exists. If direction is :input, not supplied, or :probe, if-exists is ignored. These are the results of open as modified by if-exists:

    :error

        An error of type file-error is signaled.

    :new-version

        A new file is created with a larger version number.

    :rename

        The existing file is renamed to some other name and then a new file is created.

    :rename-and-delete

        The existing file is renamed to some other name, then it is deleted but not expunged, and then a new file is created.

    :overwrite

        Output operations on the stream destructively modify the existing file. If direction is :io the file is opened in a bidirectional mode that allows both reading and writing. The file pointer is initially positioned at the beginning of the file; however, the file is not truncated back to length zero when it is opened.

    :append

        Output operations on the stream destructively modify the existing file. The file pointer is initially positioned at the end of the file.

        If direction is :io, the file is opened in a bidirectional mode that allows both reading and writing.

    :supersede

        The existing file is superseded; that is, a new file with the same name as the old one is created. If possible, the implementation should not destroy the old file until the new stream is closed.

    nil

        No file or stream is created; instead, nil is returned to indicate failure.

:if-does-not-exist

    if-does-not-exist specifies the action to be taken if a file of name filespec does not already exist. These are the results of open as modified by if-does-not-exist:

    :error

        An error of type file-error is signaled.

    :create

        An empty file is created. Processing continues as if the file had already existed but no processing as directed by if-exists is performed.

    nil

        No file or stream is created; instead, nil is returned to indicate failure.

:external-format

    This option selects an external file format for the file: The only standardized value for this option is :default, although implementations are permitted to define additional external file formats and implementation-dependent values returned by stream-external-format can also be used by conforming programs.

    The external-format is meaningful for any kind of file stream whose element type is a subtype of character. This option is ignored for streams for which it is not meaningful; however, implementations may define other element types for which it is meaningful. The consequences are unspecified if a character is written that cannot be represented by the given external file format.

When a file is opened, a file stream is constructed to serve as the file system's ambassador to the Lisp environment; operations on the file stream are reflected by operations on the file in the file system.

A file can be deleted, renamed, or destructively modified by open.

For information about opening relative pathnames, see Section 19.2.3 (Merging Pathnames).

* 示例(Examples):

 (open filespec :direction :probe)  =>  #<Closed Probe File Stream...>
 (setq q (merge-pathnames (user-homedir-pathname) "test"))
=>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY directory-name
    :NAME "test" :TYPE NIL :VERSION :NEWEST>
 (open filespec :if-does-not-exist :create) =>  #<Input File Stream...>
 (setq s (open filespec :direction :probe)) =>  #<Closed Probe File Stream...>
 (truename s) =>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY
    directory-name :NAME filespec :TYPE extension :VERSION 1>
 (open s :direction :output :if-exists nil) =>  NIL 

* 受此影响(Affected By):

The nature and state of the host computer's file system.

* 异常情况(Exceptional Situations):

If if-exists is :error, (subject to the constraints on the meaning of if-exists listed above), an error of type file-error is signaled.

If if-does-not-exist is :error (subject to the constraints on the meaning of if-does-not-exist listed above), an error of type file-error is signaled.

If it is impossible for an implementation to handle some option in a manner close to what is specified here, an error of type error might be signaled.

An error of type file-error is signaled if (wild-pathname-p filespec) returns true.

An error of type error is signaled if the external-format is not understood by the implementation.

The various file systems in existence today have widely differing capabilities, and some aspects of the file system are beyond the scope of this specification to define. A given implementation might not be able to support all of these options in exactly the manner stated. An implementation is required to recognize all of these option keywords and to try to do something ``reasonable'' in the context of the host file system. Where necessary to accomodate the file system, an implementation deviate slightly from the semantics specified here without being disqualified for consideration as a conforming implementation. If it is utterly impossible for an implementation to handle some option in a manner similar to what is specified here, it may simply signal an error.

With regard to the :element-type option, if a type is requested that is not supported by the file system, a substitution of types such as that which goes on in upgrading is permissible. As a minimum requirement, it should be the case that opening an output stream to a file in a given element type and later opening an input stream to the same file in the same element type should work compatibly.

* 也见(See Also):

with-open-file, close, pathname, logical-pathname, Section 19.2.3 (Merging Pathnames), Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes):

open does not automatically close the file when an abnormal exit occurs.

When element-type is a subtype of character, read-char and/or write-char can be used on the resulting file stream.

When element-type is a subtype of integer, read-byte and/or write-byte can be used on the resulting file stream.

When element-type is :default, the type can be determined by using stream-element-type. 


### <span id="F-STREAM-EXTERNAL-FORMAT">函数 STREAM-EXTERNAL-FORMAT</span>

* 语法(Syntax):

stream-external-format stream => format

* 参数和值(Arguments and Values):

stream---a file stream.

format---an external file format.

* 描述(Description):

Returns an external file format designator for the stream.

* 示例(Examples):

 (with-open-file (stream "test" :direction :output)
   (stream-external-format stream))
=>  :DEFAULT
OR=>  :ISO8859/1-1987
OR=>  (:ASCII :SAIL)
OR=>  ACME::PROPRIETARY-FILE-FORMAT-17
OR=>  #<FILE-FORMAT :ISO646-1983 2343673>

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

the :external-format argument to the function open and the with-open-file macro.

* 注意(Notes):

The format returned is not necessarily meaningful to other implementations. 


### <span id="M-WITH-OPEN-FILE">宏 WITH-OPEN-FILE</span>

* 语法(Syntax):

with-open-file (stream filespec options*) declaration* form*

=> results

* 参数和值(Arguments and Values):

stream -- a variable.

filespec---a pathname designator.

options -- forms; evaluated.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

with-open-file uses open to create a file stream to file named by filespec. Filespec is the name of the file to be opened. Options are used as keyword arguments to open.

The stream object to which the stream variable is bound has dynamic extent; its extent ends when the form is exited.

with-open-file evaluates the forms as an implicit progn with stream bound to the value returned by open.

When control leaves the body, either normally or abnormally (such as by use of throw), the file is automatically closed. If a new output file is being written, and control leaves abnormally, the file is aborted and the file system is left, so far as possible, as if the file had never been opened.

It is possible by the use of :if-exists nil or :if-does-not-exist nil for stream to be bound to nil. Users of :if-does-not-exist nil should check for a valid stream.

The consequences are undefined if an attempt is made to assign the stream variable. The compiler may choose to issue a warning if such an attempt is detected.

* 示例(Examples):

 (setq p (merge-pathnames "test"))
=>  #<PATHNAME :HOST NIL :DEVICE device-name :DIRECTORY directory-name
    :NAME "test" :TYPE NIL :VERSION :NEWEST>
 (with-open-file (s p :direction :output :if-exists :supersede)
    (format s "Here are a couple~%of test data lines~%")) =>  NIL
 (with-open-file (s p)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Reached end of file.")
     (format t "~&*** ~A~%" l)))
>>  *** Here are a couple
>>  *** of test data lines
=>  "Reached end of file."

;; Normally one would not do this intentionally because it is
;; not perspicuous, but beware when using :IF-DOES-NOT-EXIST NIL
;; that this doesn't happen to you accidentally...
 (with-open-file (foo "no-such-file" :if-does-not-exist nil)
   (read foo))
>>  hello?
=>  HELLO? ;This value was read from the terminal, not a file!

;; Here's another bug to avoid...
 (with-open-file (foo "no-such-file" :direction :output :if-does-not-exist nil)
   (format foo "Hello"))
=>  "Hello" ;FORMAT got an argument of NIL!

* 副作用(Side Effects):

Creates a stream to the file named by filename (upon entry), and closes the stream (upon exit). In some implementations, the file might be locked in some way while it is open. If the stream is an output stream, a file might be created.

* 受此影响(Affected By):

The host computer's file system.

* 异常情况(Exceptional Situations):

See the function open.

* 也见(See Also):

open, close, pathname, logical-pathname, Section 19.1.2 (Pathnames as Filenames)

* 注意(Notes): None. 


### <span id="F-CLOSE">函数 CLOSE</span>

* 语法(Syntax):

close stream &key abort => result

* 参数和值(Arguments and Values):

stream---a stream (either open or closed).

abort---一个广义 boolean. 默认是 false.

result---t if the stream was open at the time it was received as an argument, or implementation-dependent otherwise.

* 描述(Description):

close closes stream. Closing a stream means that it may no longer be used in input or output operations. The act of closing a file stream ends the association between the stream and its associated file; the transaction with the file system is terminated, and input/output may no longer be performed on the stream.

If abort is true, an attempt is made to clean up any side effects of having created stream. If stream performs output to a file that was created when the stream was created, the file is deleted and any previously existing file is not superseded.

It is permissible to close an already closed stream, but in that case the result is implementation-dependent.

After stream is closed, it is still possible to perform the following query operations upon it: streamp, pathname, truename, merge-pathnames, pathname-host, pathname-device, pathname-directory,pathname-name, pathname-type, pathname-version, namestring, file-namestring, directory-namestring, host-namestring, enough-namestring, open, probe-file, and directory.

The effect of close on a constructed stream is to close the argument stream only. There is no effect on the constituents of composite streams.

For a stream created with make-string-output-stream, the result of get-output-stream-string is unspecified after close.

* 示例(Examples):

 (setq s (make-broadcast-stream)) =>  #<BROADCAST-STREAM>
 (close s) =>  T
 (output-stream-p s) =>  true

* 副作用(Side Effects):

The stream is closed (if necessary). If abort is true and the stream is an output file stream, its associated file might be deleted.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

open

* 注意(Notes): None. 


### <span id="M-WITH-OPEN-STREAM">宏 WITH-OPEN-STREAM</span>

* 语法(Syntax):

with-open-stream (var stream) declaration* form*

=> result*

* 参数和值(Arguments and Values):

var---a variable name.

stream---a form; evaluated to produce a stream.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---the values returned by the forms.

* 描述(Description):

with-open-stream performs a series of operations on stream, returns a value, and then closes the stream.

Var is bound to the value of stream, and then forms are executed as an implicit progn. stream is automatically closed on exit from with-open-stream, no matter whether the exit is normal or abnormal. The stream has dynamic extent; its extent ends when the form is exited.

The consequences are undefined if an attempt is made to assign the the variable var with the forms.

* 示例(Examples):

 (with-open-stream (s (make-string-input-stream "1 2 3 4"))
    (+ (read s) (read s) (read s))) =>  6

* 副作用(Side Effects):

The stream is closed (upon exit).

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

close

* 注意(Notes): None. 

### <span id="F-LISTEN">函数 LISTEN</span>

* 语法(Syntax):

listen &optional input-stream => generalized-boolean

* 参数和值(Arguments and Values):

input-stream---an input stream designator. 默认是标准输入.

generalized-boolean---一个广义 boolean.

* 描述(Description):

Returns true if there is a character immediately available from input-stream; otherwise, returns false. On a non-interactive input-stream, listen returns true except when at end of file[1]. If an end of file is encountered, listen returns false. listen is intended to be used when input-stream obtains characters from an interactive device such as a keyboard.

* 示例(Examples):

 (progn (unread-char (read-char)) (list (listen) (read-char)))
>>  1
=>  (T #\1)
 (progn (clear-input) (listen))
=>  NIL ;Unless you're a very fast typist!

* 副作用(Side Effects): None.

* 受此影响(Affected By):

*standard-input*

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

interactive-stream-p, read-char-no-hang

* 注意(Notes): None. 


### <span id="F-CLEAR-INPUT">函数 CLEAR-INPUT</span>

* 语法(Syntax):

clear-input &optional input-stream => nil

* 参数和值(Arguments and Values):

input-stream---an input stream designator. 默认是标准输入.

* 描述(Description):

Clears any available input from input-stream.

If clear-input does not make sense for input-stream, then clear-input does nothing.

* 示例(Examples):

;; The exact I/O behavior of this example might vary from implementation
;; to implementation depending on the kind of interactive buffering that
;; occurs.  (The call to SLEEP here is intended to help even out the 
;; differences in implementations which do not do line-at-a-time buffering.)

(defun read-sleepily (&optional (clear-p nil) (zzz 0))
  (list (progn (print '>) (read))
        ;; Note that input typed within the first ZZZ seconds 
        ;; will be discarded.
        (progn (print '>) 
               (if zzz (sleep zzz))
               (print '>>)
               (if clear-p (clear-input))
               (read))))

(read-sleepily)
>>  > 10
>>  >
>>  >> 20
=>  (10 20)

(read-sleepily t)
>>  > 10
>>  >
>>  >> 20
=>  (10 20)

(read-sleepily t 10)
>>  > 10
>>  > 20  ; Some implementations won't echo typeahead here.
>>  >> 30
=>  (10 30)

* 副作用(Side Effects):

The input-stream is modified.

* 受此影响(Affected By):

*standard-input*

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if input-stream is not a stream designator.

* 也见(See Also):

clear-output

* 注意(Notes): None. 

### <span id="F-FINISH-AND-FORCE-AND-CLEAR-OUTPUT">函数 FINISH-OUTPUT, FORCE-OUTPUT, CLEAR-OUTPUT</span>

* 语法(Syntax):

finish-output &optional output-stream => nil

force-output &optional output-stream => nil

clear-output &optional output-stream => nil

* 参数和值(Arguments and Values):

output-stream---an output stream designator. The default is standard output.

* 描述(Description):

finish-output, force-output, and clear-output exercise control over the internal handling of buffered stream output.

finish-output attempts to ensure that any buffered output sent to output-stream has reached its destination, and then returns.

force-output initiates the emptying of any internal buffers but does not wait for completion or acknowledgment to return.

clear-output attempts to abort any outstanding output operation in progress in order to allow as little output as possible to continue to the destination.

If any of these operations does not make sense for output-stream, then it does nothing. The precise actions of these functions are implementation-dependent.

* 示例(Examples):

;; Implementation A
 (progn (princ "am i seen?") (clear-output))
=>  NIL

;; Implementation B
 (progn (princ "am i seen?") (clear-output))
>>  am i seen?
=>  NIL

* 副作用(Side Effects): None.

* 受此影响(Affected By):

*standard-output*

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if output-stream is not a stream designator.

* 也见(See Also):

clear-input

* 注意(Notes): None. 


### <span id="F-Y-OR-N-P-YES-OR-NO-P">函数 Y-OR-N-P, YES-OR-NO-P</span>

* 语法(Syntax):

y-or-n-p &optional control &rest arguments => generalized-boolean

yes-or-no-p &optional control &rest arguments => generalized-boolean

* 参数和值(Arguments and Values):

control---a format control.

arguments---format arguments for control.

generalized-boolean---一个广义 boolean.

* 描述(Description):

These functions ask a question and parse a response from the user. They return true if the answer is affirmative, or false if the answer is negative.

y-or-n-p is for asking the user a question whose answer is either ``yes'' or ``no.'' It is intended that the reply require the user to answer a yes-or-no question with a single character. yes-or-no-p is also for asking the user a question whose answer is either ``Yes'' or ``No.'' It is intended that the reply require the user to take more action than just a single keystroke, such as typing the full word yes or no followed by a newline.

y-or-n-p types out a message (if supplied), reads an answer in some implementation-dependent manner (intended to be short and simple, such as reading a single character such as Y or N). yes-or-no-p types out a message (if supplied), attracts the user's attention (for example, by ringing the terminal's bell), and reads an answer in some implementation-dependent manner (intended to be multiple characters, such as YES or NO).

If format-control is supplied and not nil, then a fresh-line operation is performed; then a message is printed as if format-control and arguments were given to format. In any case, yes-or-no-p and y-or-n-p will provide a prompt such as ``(Y or N)'' or ``(Yes or No)'' if appropriate.

All input and output are performed using query I/O.

* 示例(Examples):

 (y-or-n-p "(t or nil) given by")
>>  (t or nil) given by (Y or N) Y
=>  true
 (yes-or-no-p "a ~S message" 'frightening) 
>>  a FRIGHTENING message (Yes or No) no
=>  false
 (y-or-n-p "Produce listing file?") 
>>  Produce listing file?
>>  Please respond with Y or N. n
=>  false

* 副作用(Side Effects):

Output to and input from query I/O will occur.

* 受此影响(Affected By):

*query-io*.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

format

* 注意(Notes):

yes-or-no-p and yes-or-no-p do not add question marks to the end of the prompt string, so any desired question mark or other punctuation should be explicitly included in the text query. 


### <span id="F-MAKE-SYNONYM-STREAM">函数 MAKE-SYNONYM-STREAM</span>

* 语法(Syntax):

make-synonym-stream symbol => synonym-stream

* 参数和值(Arguments and Values):

symbol---a symbol that names a dynamic variable.

synonym-stream---a synonym stream.

* 描述(Description):

Returns a synonym stream whose synonym stream symbol is symbol.

* 示例(Examples):

 (setq a-stream (make-string-input-stream "a-stream")
        b-stream (make-string-input-stream "b-stream"))
=>  #<String Input Stream> 
 (setq s-stream (make-synonym-stream 'c-stream))
=>  #<SYNONYM-STREAM for C-STREAM> 
 (setq c-stream a-stream)
=>  #<String Input Stream> 
 (read s-stream) =>  A-STREAM
 (setq c-stream b-stream)
=>  #<String Input Stream> 
 (read s-stream) =>  B-STREAM

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if its argument is not a symbol.

* 也见(See Also):

Section 21.1 (流的概念)

* 注意(Notes): None. 

### <span id="F-SYNONYM-STREAM-SYMBOL">函数 SYNONYM-STREAM-SYMBOL</span>

* 语法(Syntax):

synonym-stream-symbol synonym-stream => symbol

* 参数和值(Arguments and Values):

synonym-stream---a synonym stream.

symbol---a symbol.

* 描述(Description):

Returns the symbol whose symbol-value the synonym-stream is using.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-synonym-stream

* 注意(Notes): None. 

### <span id="F-BROADCAST-STREAM-STREAMS">函数 BROADCAST-STREAM-STREAMS</span>

* 语法(Syntax):

broadcast-stream-streams broadcast-stream => streams

* 参数和值(Arguments and Values):

broadcast-stream---a broadcast stream.

streams---a list of streams.

* 描述(Description):

Returns a list of output streams that constitute all the streams to which the broadcast-stream is broadcasting.

* 示例(Examples): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAKE-BROADCAST-STREAM">函数 MAKE-BROADCAST-STREAM</span>

* 语法(Syntax):

make-broadcast-stream &rest streams => broadcast-stream

* 参数和值(Arguments and Values):

stream---一个输出流.

broadcast-stream---a broadcast stream.

* 描述(Description):

Returns a broadcast stream.

* 示例(Examples):

 (setq a-stream (make-string-output-stream)
        b-stream (make-string-output-stream)) =>  #<String Output Stream>
 (format (make-broadcast-stream a-stream b-stream)
          "this will go to both streams") =>  NIL
 (get-output-stream-string a-stream) =>  "this will go to both streams"
 (get-output-stream-string b-stream) =>  "this will go to both streams"

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if any stream is not an output stream.

* 也见(See Also):

broadcast-stream-streams

* 注意(Notes): None. 



### <span id="F-MAKE-TWO-WAY-STREAM">函数 MAKE-TWO-WAY-STREAM</span>

* 语法(Syntax):

make-two-way-stream input-stream output-stream => two-way-stream

* 参数和值(Arguments and Values):

input-stream---一个流.

output-stream---一个流.

two-way-stream---a two-way stream.

* 描述(Description):

Returns a two-way stream that gets its input from input-stream and sends its output to output-stream.

* 示例(Examples):

 (with-output-to-string (out)
    (with-input-from-string (in "input...")
      (let ((two (make-two-way-stream in out)))
        (format two "output...")
        (setq what-is-read (read two))))) =>  "output..."
 what-is-read =>  INPUT... 

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if input-stream is not an input stream. Should signal an error of type type-error if output-stream is not an output stream.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-T-W-S-INPUT-AND-OUTPUT-STREAM">函数 TWO-WAY-STREAM-INPUT-STREAM, TWO-WAY-STREAM-OUTPUT-STREAM</span>

* 语法(Syntax):

two-way-stream-input-stream two-way-stream => input-stream

two-way-stream-output-stream two-way-stream => output-stream

* 参数和值(Arguments and Values):

two-way-stream---a two-way stream.

input-stream---一个输入流.

output-stream---一个输出流.

* 描述(Description):

two-way-stream-input-stream returns the stream from which two-way-stream receives input.

two-way-stream-output-stream returns the stream to which two-way-stream sends output.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-ECHO-STREAM-INPUT-AND-OUTPUT-STREAM">函数 ECHO-STREAM-INPUT-STREAM, ECHO-STREAM-OUTPUT-STREAM</span>

* 语法(Syntax):

echo-stream-input-stream echo-stream => input-stream

echo-stream-output-stream echo-stream => output-stream

* 参数和值(Arguments and Values):

echo-stream---an echo stream.

input-stream---一个输入流.

output-stream---一个输出流.

* 描述(Description):

echo-stream-input-stream returns the input stream from which echo-stream receives input.

echo-stream-output-stream returns the output stream to which echo-stream sends output.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None. 


### <span id="F-MAKE-ECHO-STREAM">函数 MAKE-ECHO-STREAM</span>

* 语法(Syntax):

make-echo-stream input-stream output-stream => echo-stream

* 参数和值(Arguments and Values):

input-stream---一个输入流.

output-stream---一个输出流.

echo-stream---an echo stream.

* 描述(Description):

Creates and returns an echo stream that takes input from input-stream and sends output to output-stream.

* 示例(Examples):

 (let ((out (make-string-output-stream)))
    (with-open-stream 
        (s (make-echo-stream
            (make-string-input-stream "this-is-read-and-echoed")
            out))
      (read s)
      (format s " * this-is-direct-output")
      (get-output-stream-string out)))
=>  "this-is-read-and-echoed * this-is-direct-output"

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

echo-stream-input-stream, echo-stream-output-stream, make-two-way-stream

* 注意(Notes): None. 


### <span id="F-CONCATENATED-STREAM-STREAMS">函数 CONCATENATED-STREAM-STREAMS</span>

* 语法(Syntax):

concatenated-stream-streams concatenated-stream => streams

* 参数和值(Arguments and Values):

concatenated-stream -- a concatenated stream.

streams---a list of input streams.

* 描述(Description):

Returns a list of input streams that constitute the ordered set of streams the concatenated-stream still has to read from, starting with the current one it is reading from. The list may be empty if no more streams remain to be read.

The consequences are undefined if the list structure of the streams is ever modified.

* 示例(Examples): None.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes): None.


### <span id="F-MAKE-CONCATENATED-STREAM">函数 MAKE-CONCATENATED-STREAM</span>

* 语法(Syntax):

make-concatenated-stream &rest input-streams => concatenated-stream

* 参数和值(Arguments and Values):

input-stream---一个输入流.

concatenated-stream---a concatenated stream.

* 描述(Description):

Returns a concatenated stream that has the indicated input-streams initially associated with it.

* 示例(Examples):

 (read (make-concatenated-stream
         (make-string-input-stream "1")
         (make-string-input-stream "2"))) =>  12

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal type-error if any argument is not an input stream.

* 也见(See Also):

concatenated-stream-streams

* 注意(Notes): None. 


### <span id="F-GET-OUTPUT-STREAM-STRING">函数 GET-OUTPUT-STREAM-STRING</span>

* 语法(Syntax):

get-output-stream-string string-output-stream => string

* 参数和值(Arguments and Values):

string-output-stream---一个流.

string---一个字符串.

* 描述(Description):

Returns a string containing, in order, all the characters that have been output to string-output-stream. This operation clears any characters on string-output-stream, so the string contains only those characters which have been output since the last call to get-output-stream-string or since the creation of the string-output-stream, whichever occurred most recently.

* 示例(Examples):

 (setq a-stream (make-string-output-stream)
        a-string "abcdefghijklm") =>  "abcdefghijklm"
 (write-string a-string a-stream) =>  "abcdefghijklm"
 (get-output-stream-string a-stream) =>  "abcdefghijklm"
 (get-output-stream-string a-stream) =>  ""

* 副作用(Side Effects):

The string-output-stream is cleared.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The consequences are undefined if stream-output-string is closed.

The consequences are undefined if string-output-stream is a stream that was not produced by make-string-output-stream. The consequences are undefined if string-output-stream was created implicitly by with-output-to-string or format.

* 也见(See Also):

make-string-output-stream

* 注意(Notes): None. 


### <span id="F-MAKE-STRING-INPUT-STREAM">函数 MAKE-STRING-INPUT-STREAM</span>

* 语法(Syntax):

make-string-input-stream string &optional start end => string-stream

* 参数和值(Arguments and Values):

string---一个字符串.

start, end---bounding index designators of string. The defaults for start and end are 0 and nil, respectively.

string-stream---an input string stream.

* 描述(Description):

Returns an input string stream. This stream will supply, in order, the characters in the substring of string bounded by start and end. After the last character has been supplied, the string stream will then be at end of file.

* 示例(Examples):

 (let ((string-stream (make-string-input-stream "1 one ")))
   (list (read string-stream nil nil)
         (read string-stream nil nil)
         (read string-stream nil nil)))
=>  (1 ONE NIL)

 (read (make-string-input-stream "prefixtargetsuffix" 6 12)) =>  TARGET

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

with-input-from-string

* 注意(Notes): None. 


### <span id="F-MAKE-STRING-OUTPUT-STREAM">函数 MAKE-STRING-OUTPUT-STREAM</span>

* 语法(Syntax):

make-string-output-stream &key element-type => string-stream

* 参数和值(Arguments and Values):

element-type---a type specifier. The default is character.

string-stream---an output string stream.

* 描述(Description):

Returns an output string stream that accepts characters and makes available (via get-output-stream-string) a string that contains the characters that were actually output.

The element-type names the type of the elements of the string; a string is constructed of the most specialized type that can accommodate elements of that element-type.

* 示例(Examples):

 (let ((s (make-string-output-stream)))
   (write-string "testing... " s)
   (prin1 1234 s)
   (get-output-stream-string s))
=>  "testing... 1234"

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

get-output-stream-string, with-output-to-string

* 注意(Notes): None. 


### <span id="M-WITH-INPUT-FROM-STRING">宏 WITH-INPUT-FROM-STRING</span>

* 语法(Syntax):

with-input-from-string (var string &key index start end) declaration* form*

=> result*

* 参数和值(Arguments and Values):

var---a variable name.

string---a form; evaluated to produce a string.

index---a place.

start, end---bounding index designators of string. The defaults for start and end are 0 and nil, respectively.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

result---the values returned by the forms.

* 描述(Description):

Creates an input string stream, provides an opportunity to perform operations on the stream (returning zero or more values), and then closes the string stream.

String is evaluated first, and var is bound to a character input string stream that supplies characters from the subsequence of the resulting string bounded by start and end. The body is executed as an implicit progn.

The input string stream is automatically closed on exit from with-input-from-string, no matter whether the exit is normal or abnormal. The input string stream to which the variable var is bound has dynamic extent; its extent ends when the form is exited.

The index is a pointer within the string to be advanced. If with-input-from-string is exited normally, then index will have as its value the index into the string indicating the first character not read which is (length string) if all characters were used. The place specified by index is not updated as reading progresses, but only at the end of the operation.

start and index may both specify the same variable, which is a pointer within the string to be advanced, perhaps repeatedly by some containing loop.

The consequences are undefined if an attempt is made to assign the variable var.

* 示例(Examples):

 (with-input-from-string (s "XXX1 2 3 4xxx"
                             :index ind
                             :start 3 :end 10)
    (+ (read s) (read s) (read s))) =>  6
 ind =>  9
 (with-input-from-string (s "Animal Crackers" :index j :start 6)
   (read s)) =>  CRACKERS

The variable j is set to 15.

* 副作用(Side Effects):

The value of the place named by index, if any, is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

make-string-input-stream, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="M-WITH-OUTPUT-TO-STRING">宏 WITH-OUTPUT-TO-STRING</span>

* 语法(Syntax):

with-output-to-string (var &optional string-form &key element-type) declaration* form*

=> result*

* 参数和值(Arguments and Values):

var---a variable name.

string-form---a form or nil; if non-nil, evaluated to produce string.

string---a string that has a fill pointer.

element-type---a type specifier; evaluated. The default is character.

declaration---a declare expression; not evaluated.

forms---an implicit progn.

results---If a string-form is not supplied or nil, a string; otherwise, the values returned by the forms.

* 描述(Description):

with-output-to-string creates a character output stream, performs a series of operations that may send results to this stream, and then closes the stream.

The element-type names the type of the elements of the stream; a stream is constructed of the most specialized type that can accommodate elements of the given type.

The body is executed as an implicit progn with var bound to an output string stream. All output to that string stream is saved in a string.

If string is supplied, element-type is ignored, and the output is incrementally appended to string as if by use of vector-push-extend.

The output stream is automatically closed on exit from with-output-from-string, no matter whether the exit is normal or abnormal. The output string stream to which the variable var is bound has dynamic extent; its extent ends when the form is exited.

If no string is provided, then with-output-from-string produces a stream that accepts characters and returns a string of the indicated element-type. If string is provided, with-output-to-string returns the results of evaluating the last form.

The consequences are undefined if an attempt is made to assign the variable var.

* 示例(Examples):

 (setq fstr (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)) =>  ""
 (with-output-to-string (s fstr)
    (format s "here's some output")
    (input-stream-p s)) =>  false
 fstr =>  "here's some output"

* 副作用(Side Effects):

The string is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The consequences are undefined if destructive modifications are performed directly on the string during the dynamic extent of the call.

* 也见(See Also):

make-string-output-stream, vector-push-extend, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="V-IO-ALL">变量 *DEBUG-IO*, *ERROR-OUTPUT*, *QUERY-IO*, *STANDARD-INPUT*, *STANDARD-OUTPUT*, *TRACE-OUTPUT*</span>

Value Type:

For *standard-input*: an input stream

For *error-output*, *standard-output*, and *trace-output*: an output stream.

For *debug-io*, *query-io*: a bidirectional stream.

Initial Value:

implementation-dependent, but it must be an open stream that is not a generalized synonym stream to an I/O customization variables but that might be a generalized synonym stream to the value of some I/O customization variable. The initial value might also be a generalized synonym stream to either the symbol *terminal-io* or to the stream that is its value.

* 描述(Description):

These variables are collectively called the standardized I/O customization variables. They can be bound or assigned in order to change the default destinations for input and/or output used by various standardized operators and facilities.

The value of *debug-io*, called debug I/O, is a stream to be used for interactive debugging purposes.

The value of *error-output*, called error output, is a stream to which warnings and non-interactive error messages should be sent.

The value of *query-io*, called query I/O, is a bidirectional stream to be used when asking questions of the user. The question should be output to this stream, and the answer read from it.

The value of *standard-input*, called standard input, is a stream that is used by many operators as a default source of input when no specific input stream is explicitly supplied.

The value of *standard-output*, called standard output, is a stream that is used by many operators as a default destination for output when no specific output stream is explicitly supplied.

The value of *trace-output*, called trace output, is the stream on which traced functions (see trace) and the time macro print their output.

* 示例(Examples):

 (with-output-to-string (*error-output*)
   (warn "this string is sent to *error-output*"))
 =>  "Warning: this string is sent to *error-output*
" ;The exact format of this string is implementation-dependent.

 (with-input-from-string (*standard-input* "1001")
    (+ 990 (read))) =>  1991                       

 (progn (setq out (with-output-to-string (*standard-output*)
                     (print "print and format t send things to")
                     (format t "*standard-output* now going to a string")))
        :done)
=>  :DONE
 out
=>  "
\"print and format t send things to\" *standard-output* now going to a string"

 (defun fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))
=>  FACT
 (trace fact)
=>  (FACT)
;; Of course, the format of traced output is implementation-dependent.
 (with-output-to-string (*trace-output*)
   (fact 3)) 
=>  "
1 Enter FACT 3
| 2 Enter FACT 2
|   3 Enter FACT 1
|   3 Exit FACT 1
| 2 Exit FACT 2
1 Exit FACT 6"

* 也见(See Also):

*terminal-io*, synonym-stream, time, trace, Section 9 (Conditions), Section 23 (Reader), Section 22 (Printer)

* 注意(Notes):

The intent of the constraints on the initial value of the I/O customization variables is to ensure that it is always safe to bind or assign such a variable to the value of another I/O customization variable, without unduly restricting implementation flexibility.

It is common for an implementation to make the initial values of *debug-io* and *query-io* be the same stream, and to make the initial values of *error-output* and *standard-output* be the same stream.

The functions y-or-n-p and yes-or-no-p use query I/O for their input and output.

In the normal Lisp read-eval-print loop, input is read from standard input. Many input functions, including read and read-char, take a stream argument that defaults to standard input.

In the normal Lisp read-eval-print loop, output is sent to standard output. Many output functions, including print and write-char, take a stream argument that defaults to standard output.

A program that wants, for example, to divert output to a file should do so by binding *standard-output*; that way error messages sent to *error-output* can still get to the user by going through *terminal-io* (if *error-output* is bound to *terminal-io*), which is usually what is desired. 


### <span id="V-TERMINAL-IO">变量 *TERMINAL-IO*</span>

Value Type:

a bidirectional stream.

Initial Value:

implementation-dependent, but it must be an open stream that is not a generalized synonym stream to an I/O customization variables but that might be a generalized synonym stream to the value of some I/O customization variable.

* 描述(Description):

The value of *terminal-io*, called terminal I/O, is ordinarily a bidirectional stream that connects to the user's console. Typically, writing to this stream would cause the output to appear on a display screen, for example, and reading from the stream would accept input from a keyboard. It is intended that standard input functions such as read and read-char, when used with this stream, cause echoing of the input into the output side of the stream. The means by which this is accomplished are implementation-dependent.

The effect of changing the value of *terminal-io*, either by binding or assignment, is implementation-defined.

* 示例(Examples):

 (progn (prin1 'foo) (prin1 'bar *terminal-io*))
>>  FOOBAR
=>  BAR
 (with-output-to-string (*standard-output*)
   (prin1 'foo) 
   (prin1 'bar *terminal-io*))
>>  BAR
=>  "FOO"

* 受此影响(Affected By): None.

* 也见(See Also):

*debug-io*, *error-output*, *query-io*, *standard-input*, *standard-output*, *trace-output*

* 注意(Notes): None. 


### <span id="CT-STREAM-ERROR">状况类型 STREAM-ERROR</span>

* 类优先级列表(Class Precedence List):

stream-error, error, serious-condition, condition, t

* 描述(Description):

The type stream-error consists of error conditions that are related to receiving input from or sending output to a stream. The ``offending stream'' is initialized by the :streaminitialization argument to make-condition, and is accessed by the function stream-error-stream.

* 也见(See Also):

stream-error-stream 


### <span id="F-STREAM-ERROR-STREAM">函数 STREAM-ERROR-STREAM</span>

* 语法(Syntax):

        stream-error-stream condition => stream

* 参数和值(Arguments and Values):

        condition---一个 stream-error 类型的状况.
        stream---一个流.

* 描述(Description):

        返回一个 stream-error 类型的状况的违规的流.

* 示例(Examples):

    ```LISP
    (with-input-from-string (s "(FOO")
      (handler-case (read s)
        (end-of-file (c)
          (format nil "~&End of file on ~S." (stream-error-stream c)))))
    "End of file on #<String Stream>."
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        stream-error, 章节 9 (Conditions)

* 注意(Notes): None. 


### <span id="CT-END-OF-FILE">状况类型 END-OF-FILE</span>

* 类优先级列表(Class Precedence List):

        end-of-file, stream-error, error, serious-condition, condition, t

* 描述(Description):

        类型 end-of-file 包含与在没有更多数据的流中完成的读取操作相关的错误状况.

* 也见(See Also):

        stream-error-stream 


