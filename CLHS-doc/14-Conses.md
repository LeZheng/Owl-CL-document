# 14 构造(Conses)

> * 14.1 [构造的概念](#ConsConcepts)
> * 14.2 [构造的字典](#TheConsesDictionary)


## 14.1 <span id="ConsConcepts">构造的概念</span>

一个构造(cons)是一个有着称为 car 和 cdr 两部分的复合数据对象.

    car  cons    rplacd  
    cdr  rplaca          

    Figure 14-1. 和构造相关的已定义的名字.

根据上下文, 一组连接的构造可以以各种不同的方式被查看. 提供了各种各样的操作来支持这些不同的视图.<!--TODO 待校对-->

> * 14.1.1 [构造作为树](#ConsesTrees)
> * 14.1.2 [构造作为列表](#ConsesLists)


### 14.1.1 <span id="ConsesTrees">构造作为树</span>

一个树是一个由构造(cons)和原子(atom)组成的二元递归数据结构: 这些构造(cons)自身也是树 (有时称之为 "子树(subtrees)" 或 "分枝(branches)"), 而这些原子是终止节点 (有时也称为叶). 典型地, 这些叶表示数据而分枝确定这些数据的关系.

    caaaar  caddar  cdar       nsubst         
    caaadr  cadddr  cddaar     nsubst-if      
    caaar   caddr   cddadr     nsubst-if-not  
    caadar  cadr    cddar      nthcdr         
    caaddr  cdaaar  cdddar     sublis         
    caadr   cdaadr  cddddr     subst          
    caar    cdaar   cdddr      subst-if       
    cadaar  cdadar  cddr       subst-if-not   
    cadadr  cdaddr  copy-tree  tree-equal     
    cadar   cdadr   nsublis                   

    Figure 14-2. 和树相关的一些已定义的名字.

#### 14.1.1.1 对必须是树的参数的普通限制

除非有明确的声明, 对于任何接受一个需要为树的参数的标准化函数, 如果该树是环状的, 其后果是未定义的. 

### 14.1.2 <span id="ConsesLists">构造作为列表</span>

一个列表是一个构造(cons)的链, 其中每一个构造(cons)的 car 是这个列表的元素, 而每个构造(cons)的 cdr 是这个链中的下一个链接或一个终止原子.

一个 proper 列表是一个以空列表终止的列表. 这个空列表是一个 proper 列表, 但不是一个构造(cons).

一个非 proper 列表是一个不是 proper 列表的列表; 这也就是说, 它是一个环状列表或一个点列表.

一个点列表是一个有着一个不是空列表的终止原子的列表. 一个非 nil 的原子自身不会被当作任何种类的列表---甚至不是一个点列表.

一个环状列表是一个构造(cons)的链, 由于这个链中的某个构造(cons)是后面一个构造(cons)的 cdr 所以它没有终止.

    append      last           nbutlast  rest       
    butlast     ldiff          nconc     revappend  
    copy-alist  list           ninth     second     
    copy-list   list*          nreconc   seventh    
    eighth      list-length    nth       sixth      
    endp        make-list      nthcdr    tailp      
    fifth       member         pop       tenth      
    first       member-if      push      third      
    fourth      member-if-not  pushnew              

    Figure 14-3. 和列表相关的已定义的名字.

> * 14.1.2.1 [列表作为关联列表](#ListsAssociationLists)
> * 14.1.2.2 [列表作为集合](#ListsSets)
> * 14.1.2.3 [对必须是列表的参数的普通限制](#GeneralRestrictParametersLists)


#### 14.1.2.1 <span id="ListsAssociationLists">列表作为关联列表</span>

一个关联列表是一个表示一个键和值关联的构造(cons)的列表, 其中每一个构造(cons)的 car 是那个键而 cdr 是和那个键关联的值.

    acons  assoc-if      pairlis  rassoc-if      
    assoc  assoc-if-not  rassoc   rassoc-if-not  

    Figure 14-4. 和关联列表相关的已定义的名字.


#### 14.1.2.2 <span id="ListsSets">列表作为集合</span>

列表有时可以通过把它们的元素当作无序的并且假定这里没有重复元素来视作集合.

    adjoin         nset-difference    set-difference    union  
    intersection   nset-exclusive-or  set-exclusive-or         
    nintersection  nunion             subsetp                  

    Figure 14-5. 和集合相关的已定义的名字.


#### 14.1.2.3 <span id="GeneralRestrictParametersLists">对必须是列表的参数的普通限制</span>

除非有明确的声明, 对于任何接受一个需要为列表的参数的标准化函数, 如果接收到的值是一个点列表, 都应该准备发出一个 type-error 类型的错误.

除非有明确的声明, 对于任何接受一个需要为列表的参数的标准化函数, 如果列表是环状的那么后果是未定义的. 


## 14.2 <span id="TheConsesDictionary">构造的字典</span>

> * [系统类 LIST](#SC-LIST)
> * [系统类 NULL](#SC-NULL)
> * [系统类 CONS](#SC-CONS)
> * [类型 ATOM](#T-ATOM)
> * [函数 CONS](#F-CONS)
> * [函数 CONSP](#F-CONSP)
> * [函数 ATOM](#F-ATOM)
> * [函数 RPLACA, RPLACD](#F-RPLACA-RPLACD)
> * [访问器 CAR, CDR, CAAR, CADR, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, CDAAR, CDADR, CDDAR, CDDDR, CAAAAR, CAAADR, CAADAR, CAADDR, CADAAR, CADADR, CADDAR, CADDDR, CDAAAR, CDAADR, CDADAR, CDADDR, CDDAAR, CDDADR, CDDDAR, CDDDDR](#A-CR-ALL)
> * [函数 COPY-TREE](#F-COPY-TREE)
> * [函数 SUBLIS, NSUBLIS](#F-SUBLIS-NSUBLIS)
> * [函数 SUBST, SUBST-IF, SUBST-IF-NOT, NSUBST, NSUBST-IF, NSUBST-IF-NOT](#F-SUBST-ALL)
> * [函数 TREE-EQUAL](#F-TREE-EQUAL)
> * [函数 COPY-LIST](#F-COPY-LIST)
> * [函数 LIST, LIST*](#F-LIST-LIST)
> * [函数 LIST-LENGTH](#F-LIST-LENGTH)
> * [函数 LISTP](#F-LISTP)
> * [函数 MAKE-LIST](#F-MAKE-LIST)
> * [宏 PUSH](#M-PUSH)
> * [宏 POP](#M-POP)
> * [访问器 FIRST, SECOND, THIRD, FOURTH, FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH](#A-FSTFFSSENT)
> * [访问器 NTH](#A-NTH)
> * [函数 ENDP](#F-ENDP)
> * [函数 NULL](#F-NULL)
> * [函数 NCONC](#F-NCONC)
> * [函数 APPEND](#F-APPEND)
> * [函数 REVAPPEND, NRECONC](#F-REVAPPEND-NRECONC)
> * [函数 BUTLAST, NBUTLAST](#F-BUTLAST-NBUTLAST)
> * [函数 LAST](#F-LAST)
> * [函数 LDIFF, TAILP](#F-LDIFF-TAILP)
> * [函数 NTHCDR](#F-NTHCDR)
> * [访问器 REST](#A-REST)
> * [函数 MEMBER, MEMBER-IF, MEMBER-IF-NOT](#F-MEMBER-ALL)
> * [函数 MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, MAPCON](#F-MAP-ALL)
> * [函数 ACONS](#F-ACONS)
> * [函数 ASSOC, ASSOC-IF, ASSOC-IF-NOT](#F-ASSOC-ALL)
> * [函数 COPY-ALIST](#F-COPY-ALIST)
> * [函数 PAIRLIS](#F-PAIRLIS)
> * [函数 RASSOC, RASSOC-IF, RASSOC-IF-NOT](#F-RASSOC-ALL)
> * [函数 GET-PROPERTIES](#F-GET-PROPERTIES)
> * [访问器 GETF](#A-GETF)
> * [宏 REMF](#M-REMF)
> * [函数 INTERSECTION, NINTERSECTION](#F-INTERSECTION-NINTERSECTION)
> * [函数 ADJOIN](#F-ADJOIN)
> * [宏 PUSHNEW](#M-PUSHNEW)
> * [函数 SET-DIFFERENCE, NSET-DIFFERENCE](#F-SET-DIFFERENCE-ALL)
> * [函数 SET-EXCLUSIVE-OR, NSET-EXCLUSIVE-OR](#F-SET-EXCLUSIVE-OR-ALL)
> * [函数 SUBSETP](#F-SUBSETP)
> * [函数 UNION, NUNION](#F-UNION-NUNION)


### <span id="SC-LIST">系统类 LIST</span>

* 类优先级列表(Class Precedence List):

        list, sequence, t

* 描述(Description):

        一个列表是一个 cons 的链, 其中每一个 cons 的 car 是这个列表的一个元素, 而每个 cons 的 cdr 是这个链的下一个链接或一个终止原子 atom.

        一个 proper 列表是一个由空列表 () 来终止的 cons 链, 这个空列表自身也是一个 proper 列表. 一个点列表是一个有着不是空列表终止 atom 的列表. 一个环状列表是一个由于链中的某个 cons 是后面的 cons 的 cdr 而没有终止的 cons 链.

        点列表和环状列表也是列表, 但通常在这个规范中的那个非限制术语 "list" 意味着 proper 列表. 然而, 类型 list 明确地包含了点列表和环状列表.

        对于一个列表中的每个元素这里都有一个 cons. 空列表没有元素并且不是一个 cons.

        类型 cons 和 null 构成 list 类型的详尽的分区.

* 也见(See Also):

        章节 2.4.1 (Left-Parenthesis), 章节 22.1.3.5 (Printing Lists and Conses) 


### <span id="SC-NULL">系统类 NULL</span>

* 类优先级列表(Class Precedence List):

        null, symbol, list, sequence, t

* 描述(Description):

        null 仅有的对象是 nil, 它表示空列表并且也可以被标记为 ().

* 也见(See Also):

        章节 2.3.4 (Symbols as Tokens), 章节 2.4.1 (Left-Parenthesis), 章节 22.1.3.3 (Printing Symbols) 


### <span id="SC-CONS">系统类 CONS</span>

* 类优先级列表(Class Precedence List):

        cons, list, sequence, t

* 描述(Description):

        一个 cons 是一个有着两个部分的复合对象, 这两个部分称为 car 和 cdr. 这些组成了一个点对. 每个部分可以是任何对象.

* 复合类型指定符类别(Compound Type Specifier Kind):

        详细的.

* 复合类型指定符语法(Compound Type Specifier Syntax):

        cons [car-typespec [cdr-typespec]]

* 复合类型指定符参数(Compound Type Specifier Arguments):

        car-typespec---一个类型指定符, 或者符号 *. 默认是符号 *.

        cdr-typespec---一个类型指定符, 或者符号 *. 默认是符号 *.

* 复合类型指定符描述(Compound Type Specifier Description):

        这个表示这个 car 受 car-typespec 约束而 cdr 受 cdr-typespec 约束的 cons 集合. (如果 car-typespec 或 cdr-typespec 是 *, 它就好像被表示为类型 t.)

* 也见(See Also):

        章节 2.4.1 (Left-Parenthesis), 章节 22.1.3.5 (Printing Lists and Conses) 


### <span id="T-ATOM">类型 ATOM</span>

* 超类型(Supertypes):

        atom, t

* 描述(Description):

        它等价于 (not cons). 


### <span id="F-CONS">函数 CONS</span>

* 语法(Syntax):

        cons object-1 object-2 => cons

* 参数和值(Arguments and Values):

        object-1---一个对象.
        object-2---一个对象.
        cons---一个 cons.

* 描述(Description):

        创建一个新的 cons, 它的 car 是对象 object-1 而它的 cdr 是对象 object-2.

* 示例(Examples):

    ```LISP
    (cons 1 2) =>  (1 . 2)
    (cons 1 nil) =>  (1)
    (cons nil 2) =>  (NIL . 2)
    (cons nil nil) =>  (NIL)
    (cons 1 (cons 2 (cons 3 (cons 4 nil)))) =>  (1 2 3 4)
    (cons 'a 'b) =>  (A . B)
    (cons 'a (cons 'b (cons 'c '()))) =>  (A B C)
    (cons 'a '(b c d)) =>  (A B C D)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        list

* 注意(Notes):

        如果 object-2 是一个列表, cons 可以被认为产生一个和 object-2 相似但是前面加上对象 object-1 的新的列表. 


### <span id="F-CONSP">函数 CONSP</span>

* 语法(Syntax):

        consp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 cons 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (consp nil) =>  false
    (consp (cons 1 2)) =>  true
    ```

        空列表不是一个 cons, 因此

        (consp '()) ==  (consp 'nil) =>  false

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        listp

* 注意(Notes):

        (consp object) ==  (typep object 'cons) ==  (not (typep object 'atom)) ==  (typep object '(not atom))


### <span id="F-ATOM">函数 ATOM</span>

* 语法(Syntax):

        atom object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象是 atom 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (atom 'sss) =>  true
    (atom (cons 1 2)) =>  false
    (atom nil) =>  true
    (atom '()) =>  true
    (atom 3) =>  true
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also): None.

* 注意(Notes):

        (atom object) ==  (typep object 'atom) ==  (not (consp object))
        ==  (not (typep object 'cons)) ==  (typep object '(not cons))


### <span id="F-RPLACA-RPLACD">函数 RPLACA, RPLACD</span>

* 语法(Syntax):

        rplaca cons object => cons

        rplacd cons object => cons

* 发音(Pronunciation):

        rplaca: [,ree'plakuh] or [,ruh'plakuh]

        rplacd: [,ree'plakduh] or [,ruh'plakduh] or [,ree'plakdee] or [,ruh'plakdee]

* 参数和值(Arguments and Values):

        cons---一个 cons.
        object---一个对象.

* 描述(Description):

        rplaca 替换这个 cons 的 car 为对象 object.

        rplacd 替换这个 cons 的 cdr 为对象 object.

* 示例(Examples):

    ```LISP
    (defparameter *some-list* (list* 'one 'two 'three 'four)) =>  *some-list*
    *some-list* =>  (ONE TWO THREE . FOUR)
    (rplaca *some-list* 'uno) =>  (UNO TWO THREE . FOUR)
    *some-list* =>  (UNO TWO THREE . FOUR)
    (rplacd (last *some-list*) (list 'IV)) =>  (THREE IV)
    *some-list* =>  (UNO TWO THREE IV)
    ```

* 副作用(Side Effects):

        这个 cons 被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

        如果 cons 不是一个构造(cons) 那么应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes): None. 

### <span id="A-CR-ALL">访问器 CAR, CDR, CAAR, CADR, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, CDAAR, CDADR, CDDAR, CDDDR, CAAAAR, CAAADR, CAADAR, CAADDR, CADAAR, CADADR, CADDAR, CADDDR, CDAAAR, CDAADR, CDADAR, CDADDR, CDDAAR, CDDADR, CDDDAR, CDDDDR</span>

* 语法(Syntax):

        car x => object

        cdr x => object

        caar x => object

        cadr x => object

        cdar x => object

        cddr x => object

        caaar x => object

        caadr x => object

        cadar x => object

        caddr x => object

        cdaar x => object

        cdadr x => object

        cddar x => object

        cdddr x => object

        caaaar x => object

        caaadr x => object

        caadar x => object

        caaddr x => object

        cadaar x => object

        cadadr x => object

        caddar x => object

        cadddr x => object

        cdaaar x => object

        cdaadr x => object

        cdadar x => object

        cdaddr x => object

        cddaar x => object

        cddadr x => object

        cdddar x => object

        cddddr x => object

        (setf (car x) new-object)

        (setf (cdr x) new-object)

        (setf (caar x) new-object)

        (setf (cadr x) new-object)

        (setf (cdar x) new-object)

        (setf (cddr x) new-object)

        (setf (caaar x) new-object)

        (setf (caadr x) new-object)

        (setf (cadar x) new-object)

        (setf (caddr x) new-object)

        (setf (cdaar x) new-object)

        (setf (cdadr x) new-object)

        (setf (cddar x) new-object)

        (setf (cdddr x) new-object)

        (setf (caaaar x) new-object)

        (setf (caaadr x) new-object)

        (setf (caadar x) new-object)

        (setf (caaddr x) new-object)

        (setf (cadaar x) new-object)

        (setf (cadadr x) new-object)

        (setf (caddar x) new-object)

        (setf (cadddr x) new-object)

        (setf (cdaaar x) new-object)

        (setf (cdaadr x) new-object)

        (setf (cdadar x) new-object)

        (setf (cdaddr x) new-object)

        (setf (cddaar x) new-object)

        (setf (cddadr x) new-object)

        (setf (cdddar x) new-object)

        (setf (cddddr x) new-object)

* 发音(Pronunciation):

        cadr: ['ka,duhr]

        caddr: ['kaduh,duhr] or ['ka,dduhr]

        cdr: ['k,duhr]

        cddr: ['kduh,duhr] or ['kuh,dduhr]

* 参数和值(Arguments and Values):

        x---一个列表.
        object---一个对象.
        new-object---一个对象.

* 描述(Description):

        如果 x 是一个 cons, car 返回这个 cons 的 car. 如果 x 是 nil, car 返回 nil.

        如果 x 是一个 cons, cdr 返回这个 cons 的 cdr. 如果 x 是 nil, cdr 返回 nil.

        提供了执行多达四个 car 和 cdr 操作组合的函数. 它们的名字有一个 C, 后面跟着 2, 3, 或 4 个 A 或 D, 最后是一个 R. 在每个函数名字中的 A 和 D 的系列被选择用来确定这个函数执行的 car 和 cdr 操作的系列. 这个 A 和 D 出现的顺序是对应操作被执行的顺序的倒序. 下一段准确地定义了这些关系.<!--TODO series 系列 ？？-->

            This place ...  Is equivalent to this place ...  
            (caar x)        (car (car x))                    
            (cadr x)        (car (cdr x))                    
            (cdar x)        (cdr (car x))                    
            (cddr x)        (cdr (cdr x))                    
            (caaar x)       (car (car (car x)))              
            (caadr x)       (car (car (cdr x)))              
            (cadar x)       (car (cdr (car x)))              
            (caddr x)       (car (cdr (cdr x)))              
            (cdaar x)       (cdr (car (car x)))              
            (cdadr x)       (cdr (car (cdr x)))              
            (cddar x)       (cdr (cdr (car x)))              
            (cdddr x)       (cdr (cdr (cdr x)))              
            (caaaar x)      (car (car (car (car x))))        
            (caaadr x)      (car (car (car (cdr x))))        
            (caadar x)      (car (car (cdr (car x))))        
            (caaddr x)      (car (car (cdr (cdr x))))        
            (cadaar x)      (car (cdr (car (car x))))        
            (cadadr x)      (car (cdr (car (cdr x))))        
            (caddar x)      (car (cdr (cdr (car x))))        
            (cadddr x)      (car (cdr (cdr (cdr x))))        
            (cdaaar x)      (cdr (car (car (car x))))        
            (cdaadr x)      (cdr (car (car (cdr x))))        
            (cdadar x)      (cdr (car (cdr (car x))))        
            (cdaddr x)      (cdr (car (cdr (cdr x))))        
            (cddaar x)      (cdr (cdr (car (car x))))        
            (cddadr x)      (cdr (cdr (car (cdr x))))        
            (cdddar x)      (cdr (cdr (cdr (car x))))        
            (cddddr x)      (cdr (cdr (cdr (cdr x))))        

            Figure 14-6. CAR 和 CDR 变体

        setf 也可以和这些函数中的任意一个一起使用来改变一个已存在的 x 的成分, 但是 setf 不会创建新的成分. 所以, 比如, 一个 cons 的 car 可以用 car 的 setf 来赋值, 但是 nil 的car 不能使用 car 的 setf 来赋值. 相似地, 一个 car 为一个 cons 的 cons, 它的 car 的 car 可以使用 caar 的 setf 来赋值, 但是 nil 和一个 car 为 nil 的 cons 不能使用 caar 的 setf 来赋值.

        参数 x 允许为一个点列表或者一个环状列表.

* 示例(Examples):

    ```LISP
    (car nil) =>  NIL  
    (cdr '(1 . 2)) =>  2
    (cdr '(1 2)) =>  (2)
    (cadr '(1 2)) =>  2 
    (car '(a b c)) =>  A
    (cdr '(a b c)) =>  (B C)
    ```

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果函数 car 和 cdr 收到一个参数不是一个列表, 那么它应该发出一个 type-error 类型的错误. 其他函数 (caar, cadr, ... cddddr) 应该为错误检查的目的而表现地就好像是通过对 car 和 cdr 的适当调用来定义的.

* 也见(See Also):

        rplaca, first, rest

* 注意(Notes):

        一个 cons 的 car 也可以通过使用 rplaca 来修改, 而一个 cons 的 cdr 可以使用 rplacd 来修改.

        (car x)    ==  (first x)
        (cadr x)   ==  (second x) ==  (car (cdr x))
        (caddr x)  ==  (third x)  ==  (car (cdr (cdr x)))
        (cadddr x) ==  (fourth x) ==  (car (cdr (cdr (cdr x))))


### <span id="F-COPY-TREE">函数 COPY-TREE</span>

* 语法(Syntax):

        copy-tree tree => new-tree

* 参数和值(Arguments and Values):

        tree---一个树.
        new-tree---一个树.

* 描述(Description):

        创建一个 cons 树的一个拷贝.

        如果 tree 不是一个 cons, 它会被返回; 否则, 结果就是在树 tree 的 car 和 cdr 上调用 copy-tree 的结果的 cons. 换句话说, 在根据 tree 表示的树中的所有 cons 会被递归复制, 只有在没有 cons 遇到时停止.

        copy-tree 不保持环状和子结构的共享.

* 示例(Examples):

    ```LISP
    (setq object (list (cons 1 "one")
                        (cons 2 (list 'a 'b 'c))))
    =>  ((1 . "one") (2 A B C))
    (setq object-too object) =>  ((1 . "one") (2 A B C))
    (setq copy-as-list (copy-list object))
    (setq copy-as-alist (copy-alist object))
    (setq copy-as-tree (copy-tree object))
    (eq object object-too) =>  true
    (eq copy-as-tree object) =>  false
    (eql copy-as-tree object) =>  false
    (equal copy-as-tree object) =>  true
    (setf (first (cdr (second object))) "a"
          (car (second object)) "two"
          (car object) '(one . 1)) =>  (ONE . 1)
    object =>  ((ONE . 1) ("two" "a" B C))
    object-too =>  ((ONE . 1) ("two" "a" B C))
    copy-as-list =>  ((1 . "one") ("two" "a" B C))
    copy-as-alist =>  ((1 . "one") (2 "a" B C))
    copy-as-tree =>  ((1 . "one") (2 A B C)) 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        tree-equal

* 注意(Notes): None. 

### <span id="F-SUBLIS-NSUBLIS">函数 SUBLIS, NSUBLIS</span>

* 语法(Syntax):

        sublis alist tree &key key test test-not => new-tree

        nsublis alist tree &key key test test-not => new-tree

* 参数和值(Arguments and Values):

        alist---一个关联列表.
        tree---一个树.
        test---一个返回一个广义 boolean 的两参数函数的标识符.
        test-not---一个返回一个广义 boolean 的两参数函数的标识符.
        key---一个单参数函数的标识符, 或者 nil.
        new-tree---一个树.

* 描述(Description):

        sublis 对树 tree (一个 cons 结构) 中的对象进行替换. nsublis 和 sublis 相似但是它会破坏性修改树 tree 中相关的结构.

        sublis 查找树 tree 中的所有子树和叶; 如果一个子树或叶作为关联列表 alist 中的一个键出现 (这也就是说, 这个键和这个子树或叶满足这个测试条件 test), 那么它就会被和那个键关联的对象所替换. 这个操作是非破坏性的. 实际上, sublis 可以同时执行多个 subst 操作.

        如果 sublis 成功了, 一个树 tree 的新的拷贝会被返回, 其中每个出现的这样的子树或叶都会被它关联的对象所替代. 如果没有做出改变, 返回原始的树. 原始的树保持不修改, 但是产生的树可能和它共享 cell.

        nsublis 允许去修改树 tree, 否则就返回和 sublis 一样的值.

* 示例(Examples):

    ```LISP
    (sublis '((x . 100) (z . zprime))
            '(plus x (minus g z x p) 4 . x))
    =>  (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100)
    (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
            '(* (/ (+ x y) (+ x p)) (- x y))
            :test #'equal)
    =>  (* (/ (- X Y) (+ X P)) (+ X Y))
    (setq tree1 '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))
    =>  (1 (1 2) ((1 2 3)) (((1 2 3 4))))
    (sublis '((3 . "three")) tree1) 
    =>  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4))))
    (sublis '((t . "string"))
              (sublis '((1 . "") (4 . 44)) tree1)
              :key #'stringp)
    =>  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44))))
    tree1 =>  (1 (1 2) ((1 2 3)) (((1 2 3 4))))
    (setq tree2 '("one" ("one" "two") (("one" "Two" "three"))))
    =>  ("one" ("one" "two") (("one" "Two" "three"))) 
    (sublis '(("two" . 2)) tree2) 
    =>  ("one" ("one" "two") (("one" "Two" "three"))) 
    tree2 =>  ("one" ("one" "two") (("one" "Two" "three"))) 
    (sublis '(("two" . 2)) tree2 :test 'equal) 
    =>  ("one" ("one" 2) (("one" "Two" "three"))) 

    (nsublis '((t . 'temp))
              tree1
              :key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
    =>  ((QUOTE TEMP) (QUOTE TEMP) QUOTE TEMP) 
    ```

* 副作用(Side Effects):

        nsublis 修改树 tree.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        subst, 章节 3.2.1 (Compiler Terminology), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数被废弃了.

        由于那些副作用变体 (e.g., nsublis) 潜在地改变了它要经过的路径, 它们对共享或环状结构的影响可能会以令人惊讶的方式不同于它们的无副作用替代方案. 为了看到这个, 细想下面这个带副作用的行为, 它可能被某些具体实现展示:

        (defun test-it (fn)
          (let* ((shared-piece (list 'a 'b))
                  (data (list shared-piece shared-piece)))
            (funcall fn '((a . b) (b . a)) data)))
        (test-it #'sublis) =>  ((B A) (B A))
        (test-it #'nsublis) =>  ((A B) (A B))


### <span id="F-SUBST-ALL">函数 SUBST, SUBST-IF, SUBST-IF-NOT, NSUBST, NSUBST-IF, NSUBST-IF-NOT</span>

* 语法(Syntax):

        subst new old tree &key key test test-not => new-tree

        subst-if new predicate tree &key key => new-tree

        subst-if-not new predicate tree &key key => new-tree

        nsubst new old tree &key key test test-not => new-tree

        nsubst-if new predicate tree &key key => new-tree

        nsubst-if-not new predicate tree &key key => new-tree

* 参数和值(Arguments and Values):

        new---一个对象.
        old---一个对象.
        predicate---命名一个函数的符号或者一个返回广义 boolean 值的单参数函数.
        tree---一个树.
        test---一个返回一个广义 boolean 的两参数函数的标识符.
        test-not---一个返回一个广义 boolean 的两参数函数的标识符.
        key---一个单参数函数的标识符, 或者 nil.
        new-tree---一个树.

* 描述(Description):

        subst, subst-if, 和 subst-if-not 在树 tree 上执行替换操作. 每个函数都搜索树 tree, 以查找满足测试条件 test 的某个元素或子表达式的某个旧项的出现.

        nsubst, nsubst-if, 和 nsubst-if-not 分别类似于 subst, subst-if, 和 subst-if-not, 除了原始的树会被修改.

        subst 做一份树 tree 的拷贝, 用新的 new 替换树 tree 中每一个旧值 old 和满足测试条件 test 的子树和叶 (不管那个子树和叶是它的父节点的 car 还是 cdr).

        nsubst 是 subst 的一个破坏性版本. 树 tree 的列表结构会被破坏性地替换, 用新值 new 替换树 tree 中的旧值 old 和满足测试条件 test 的叶.

        对于 subst, subst-if, 和 subst-if-not, 如果这些函数成功了, 这个树 tree 的一个新的拷贝会被返回, 其中每一个这样的元素的出现都被替换成那个新的元素或子表达式. 如果没有发生变化, 返回原始的树. 原始的树保持不变, 但是产生的树可能和它共享存储.

        对于 nsubst, nsubst-if, 和 nsubst-if-not 原始的树会被修改并作为这个函数的结果返回, 但是结果可能和树 tree 是 eq 的.

* 示例(Examples):

    ```LISP
    (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
    (subst "two" 2 tree1) =>  (1 (1 "two") (1 "two" 3) (1 "two" 3 4))
    (subst "five" 5 tree1) =>  (1 (1 2) (1 2 3) (1 2 3 4))
    (eq tree1 (subst "five" 5 tree1)) =>  implementation-dependent
    (subst 'tempest 'hurricane
            '(shakespeare wrote (the hurricane)))
    =>  (SHAKESPEARE WROTE (THE TEMPEST))
    (subst 'foo 'nil '(shakespeare wrote (twelfth night)))
    =>  (SHAKESPEARE WROTE (TWELFTH NIGHT . FOO) . FOO)
    (subst '(a . cons) '(old . pair)
            '((old . spice) ((old . shoes) old . pair) (old . pair))
            :test #'equal)
    =>  ((OLD . SPICE) ((OLD . SHOES) A . CONS) (A . CONS))

    (subst-if 5 #'listp tree1) =>  5
    (subst-if-not '(x) #'consp tree1) 
    =>  (1 X)

    tree1 =>  (1 (1 2) (1 2 3) (1 2 3 4))
    (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y)))) 
    =>  (1 (1 2) X X)
    tree1 =>  (1 (1 2) X X)
    ```

* 副作用(Side Effects):

        nsubst, nsubst-if, 和 nsubst-if-not 可能修改树 tree 的树结构.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        substitute, nsubstitute, 章节 3.2.1 (Compiler Terminology), 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数被废弃了.

        函数 subst-if-not 和 nsubst-if-not 被废弃了.

        subst 的一个可能的定义:

        (defun subst (old new tree &rest x &key test test-not key)
          (cond ((satisfies-the-test old tree :test test
                                      :test-not test-not :key key)
                  new)
                ((atom tree) tree)
                (t (let ((a (apply #'subst old new (car tree) x))
                          (d (apply #'subst old new (cdr tree) x)))
                      (if (and (eql a (car tree))
                              (eql d (cdr tree)))
                          tree
                          (cons a d))))))


### <span id="F-TREE-EQUAL">函数 TREE-EQUAL</span>

* 语法(Syntax):

        tree-equal tree-1 tree-2 &key test test-not => generalized-boolean

* 参数和值(Arguments and Values):

        tree-1---一个树.
        tree-2---一个树.
        test---一个返回一个广义 boolean 的两参数函数的标识符.
        test-not---一个返回一个广义 boolean 的两参数函数的标识符.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        tree-equal 测试两个树是否是相同的形状并且有着相同的叶. 如果 tree-1 和 tree-2 都是原子并且满足测试条件 test, 或者它们都是 cons 并且 tree-1 的 car 和 tree-2 的 car 是 tree-equal 的而 tree-1 的 cdr 和 tree-2 的 cdr 也是 tree-equal 的, 那么 tree-equal 返回 true. 否则, tree-equal 返回 false.

        tree-equal 递归地比较 cons 而不是其他有成分的对象.

        给 :test 或 :test-not 函数的第一个参数是 tree-1 或者 tree-1 的一个 car 或 cdr; 第二个参数是 tree-2 或 tree-2 的一个 car 或 cdr.

* 示例(Examples):

    ```LISP
    (setq tree1 '(1 (1 2))
          tree2 '(1 (1 2))) =>  (1 (1 2))
    (tree-equal tree1 tree2) =>  true
    (eql tree1 tree2) =>  false
    (setq tree1 '('a ('b 'c))
          tree2 '('a ('b 'c))) =>  ('a ('b 'c)) 
    =>  ((QUOTE A) ((QUOTE B) (QUOTE C)))
    (tree-equal tree1 tree2 :test 'eq) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 tree-1 和 tree-2 都是环状的那么后果是不确定的.

* 也见(See Also):

        equal, 章节 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

        这个 :test-not 参数被废弃了. 

### <span id="F-COPY-LIST">函数 COPY-LIST</span>

* 语法(Syntax):

        copy-list list => copy

* 参数和值(Arguments and Values):

        list---一个 proper 列表或者一个点列表.
        copy---一个列表.

* 描述(Description):

        返回列表 list 的一个拷贝. 如果列表 list 是一个点列表, 那么产生的列表也是一个点列表.

        只有列表 list 的列表结构被拷贝; 产生的列表中的元素和给定列表 list 中的对应元素相同.

* 示例(Examples):

    ```LISP
    (setq lst (list 1 (list 2 3))) =>  (1 (2 3))
    (setq slst lst) =>  (1 (2 3))
    (setq clst (copy-list lst)) =>  (1 (2 3))
    (eq slst lst) =>  true
    (eq clst lst) =>  false
    (equal clst lst) =>  true
    (rplaca lst "one") =>  ("one" (2 3))
    slst =>  ("one" (2 3))
    clst =>  (1 (2 3))
    (setf (caadr lst) "two") =>  "two"
    lst =>  ("one" ("two" 3))
    slst =>  ("one" ("two" 3))
    clst =>  (1 ("two" 3))
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果列表 list 是一个环状列表那么后果是未定义的.

* 也见(See Also):

        copy-alist, copy-seq, copy-tree

* 注意(Notes):

        创建的拷贝和列表 list 是 equal 的, 但不是 eq 的. 


### <span id="F-LIST-LIST">函数 LIST, LIST*</span>

* 语法(Syntax):

        list &rest objects => list

        list* &rest objects+ => result

* 参数和值(Arguments and Values):

        object---一个对象.
        list---一个列表.
        result---一个对象.

* 描述(Description):

        list 返回一个包含提供的那些对象 objects 的列表.

        list* 类似于 list, 除了给 list 的最后一个参数为构造的最后一个 cons 的 car, 而给 list* 的最后一个参数是构造的最后一个 cons 的cdr. 因此, 任何给定的对 list* 的调用总是产生比相同数量参数的 list 调用少一个 cons.

        如果给 list* 的最后一个参数是一个列表, 那么效果就是构造一个新的相似的列表, 但是有着额外元素被添加到前面, 这些元素和 list* 的前面参数对应.

        如果 list* 只接收到一个对象 object, 那么返回那个对象 object, 不管它是否为一个列表.

* 示例(Examples):

    ```LISP
    (list 1) =>  (1)
    (list* 1) =>  1
    (setq a 1) =>  1
    (list a 2) =>  (1 2)
    '(a 2) =>  (A 2)
    (list 'a 2) =>  (A 2)
    (list* a 2) =>  (1 . 2)
    (list) =>  NIL ;i.e.,  ()
    (setq a '(1 2)) =>  (1 2)
    (eq a (list* a)) =>  true
    (list 3 4 'a (car '(b . c)) (+ 6 -2)) =>  (3 4 A B 4)
    (list* 'a 'b 'c 'd) ==  (cons 'a (cons 'b (cons 'c 'd))) =>  (A B C . D)
    (list* 'a 'b 'c '(d e f)) =>  (A B C D E F)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        cons

* 注意(Notes):

        (list* x) ==  x


### <span id="F-LIST-LENGTH">函数 LIST-LENGTH</span>

* 语法(Syntax):

        list-length list => length

* 参数和值(Arguments and Values):

        list---一个 proper 列表或一个环状列表.
        length---一个非负整数, 或者 nil.

* 描述(Description):

        如果列表 list 是一个 proper 列表就返回这个列表 list 的长度. 如果列表 list 是一个环状列表就返回 nil.

* 示例(Examples):

    ```LISP
    (list-length '(a b c d)) =>  4
    (list-length '(a (b c) d)) =>  3
    (list-length '()) =>  0
    (list-length nil) =>  0
    (defun circular-list (&rest elements)
      (let ((cycle (copy-list elements))) 
        (nconc cycle cycle)))
    (list-length (circular-list 'a 'b)) =>  NIL
    (list-length (circular-list 'a)) =>  NIL
    (list-length (circular-list)) =>  0
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 list 不是一个 proper 列表或者一个环状列表, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        length

* 注意(Notes):

        list-length 可以通过以下方式来实现:

        (defun list-length (x)  
          (do ((n 0 (+ n 2))           ;Counter.
                (fast x (cddr fast))    ;Fast pointer: leaps by 2.
                (slow x (cdr slow)))    ;Slow pointer: leaps by 1.
              (nil)
            ;; If fast pointer hits the end, return the count.
            (when (endp fast) (return n))
            (when (endp (cdr fast)) (return (+ n 1)))
            ;; If fast pointer eventually equals slow pointer,
            ;;  then we must be stuck in a circular list.
            ;; (A deeper property is the converse: if we are
            ;;  stuck in a circular list, then eventually the
            ;;  fast pointer will equal the slow pointer.
            ;;  That fact justifies this implementation.)
            (when (and (eq fast slow) (> n 0)) (return nil))))
 

### <span id="F-LISTP">函数 LISTP</span>

* 语法(Syntax):

        listp object => generalized-boolean

* 参数和值(Arguments and Values):

        object---一个对象.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果对象 object 是 list 类型就返回 true; 否则, 返回 false.

* 示例(Examples):

    ```LISP
    (listp nil) =>  true
    (listp (cons 1 2)) =>  true
    (listp (make-array 6)) =>  false
    (listp t) =>  false
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        consp

* 注意(Notes):

        如果对象 object 是一个 cons, listp 不会检查对象 object 是否为一个 proper 列表; 对于任何种类的列表它都返回 true.

        (listp object) ==  (typep object 'list) ==  (typep object '(or cons null))


### <span id="F-MAKE-LIST">函数 MAKE-LIST</span>

* 语法(Syntax):

        make-list size &key initial-element => list

* 参数和值(Arguments and Values):

        size---一个非负整数.
        initial-element---一个对象. 默认是 nil.
        list---一个列表.

* 描述(Description):

        返回长度为给定大小 size 的列表, 其中每一个元素都是初始化元素 initial-element.

* 示例(Examples):

    ```LISP
    (make-list 5) =>  (NIL NIL NIL NIL NIL)
    (make-list 3 :initial-element 'rah) =>  (RAH RAH RAH)
    (make-list 2 :initial-element '(1 2 3)) =>  ((1 2 3) (1 2 3))
    (make-list 0) =>  NIL ;i.e.,  ()
    (make-list 0 :initial-element 'new-element) =>  NIL 
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 size 不是一个非负整数, 那么应该发出一个 type-error 类型的错误.

* 也见(See Also):

        cons, list

* 注意(Notes): None. 

### <span id="M-PUSH">宏 PUSH</span>

* 语法(Syntax):

        push item place => new-place-value

* 参数和值(Arguments and Values):

        item---一个对象.
        place---一个 place, 它的值可能是任何对象.
        new-place-value---一个列表 (这个 place 的新值).

* 描述(Description):

        push 在存储在 place 的列表前面加上 item, 把产生的列表存储到, 然后返回那个列表.

        关于 place 的子表达式形式求值的信息, 见章节 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

    ```LISP
    (setq llst '(nil)) =>  (NIL)
    (push 1 (car llst)) =>  (1)
    llst =>  ((1))
    (push 1 (car llst)) =>  (1 1)
    llst =>  ((1 1))
    (setq x '(a (b c) d)) =>  (A (B C) D)
    (push 5 (cadr x)) =>  (5 B C)  
    x =>  (A (5 B C) D)
    ```

* 副作用(Side Effects):

        place 的内容会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        pop, pushnew, 章节 5.1 (Generalized Reference)

* 注意(Notes):

        (push item place) 的效果等价于

        (setf place (cons item place))

        除了这个 place 的子表达式形式只被求值一次, 并且这个 item 在 place 之前被求值. 

### <span id="M-POP">宏 POP</span>

* 语法(Syntax):

        pop place => element

* 参数和值(Arguments and Values):

        place---一个 place, 它的值是一个列表 (可能, 但不是必须, 是一个点列表或环状列表).<!--TODO 原文好像有问题-->
        element---一个对象 (这个 place 的内容的 car).

* 描述(Description):

        pop 读取 place 的值, 记住获取到的列表的 car, 把这个列表的 cdr 写回到那个 place 中, 并且最后产生这个原始获取的列表的 car 部分.

        关于 place 的子表达式形式求值的信息, 见章节 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

    ```LISP
    (setq stack '(a b c)) =>  (A B C)
    (pop stack) =>  A  
    stack =>  (B C)
    (setq llst '((1 2 3 4))) =>  ((1 2 3 4))
    (pop (car llst)) =>  1
    llst =>  ((2 3 4))
    ```

* 副作用(Side Effects):

        place 的内容会被修改.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        push, pushnew, 章节 5.1 (Generalized Reference)

* 注意(Notes):

        (pop place) 的效果大致等价于

        (prog1 (car place) (setf place (cdr place)))

        除了后面的可能会求值 place 中的任意子表达式形式三次, 而 pop 只求值它们一次. 


### <span id="A-FSTFFSSENT">访问器 FIRST, SECOND, THIRD, FOURTH, FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, TENTH</span>

* 语法(Syntax):

        first list => object

        second list => object

        third list => object

        fourth list => object

        fifth list => object

        sixth list => object

        seventh list => object

        eighth list => object

        ninth list => object

        tenth list => object

        (setf (first list) new-object)

        (setf (second list) new-object)

        (setf (third list) new-object)

        (setf (fourth list) new-object)

        (setf (fifth list) new-object)

        (setf (sixth list) new-object)

        (setf (seventh list) new-object)

        (setf (eighth list) new-object)

        (setf (ninth list) new-object)

        (setf (tenth list) new-object)

* 参数和值(Arguments and Values):

        list---一个列表, 它可能是一个点列表或者一个环状列表.
        object, new-object---一个对象.

* 描述(Description):

        函数 first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, 和 tenth 分别访问列表 list 的第一, 第二, 第三, 第四, 第五, 第六, 第七, 第八, 第九, 和第十的元素. 具体地说,

        (first list)    ==   (car list)
        (second list)   ==   (car (cdr list))
        (third list)    ==   (car (cddr list))
        (fourth list)   ==   (car (cdddr list))
        (fifth list)    ==   (car (cddddr list))
        (sixth list)    ==   (car (cdr (cddddr list)))
        (seventh list)  ==   (car (cddr (cddddr list)))
        (eighth list)   ==   (car (cdddr (cddddr list)))
        (ninth list)    ==   (car (cddddr (cddddr list)))
        (tenth list)    ==   (car (cdr (cddddr (cddddr list))))

        setf 也可以和这些函数中的任何一个一起使用来改变一个已存在的成分. 应用相同的等式. 比如:

        (setf (fifth list) new-object) ==  (setf (car (cddddr list)) new-object)

* 示例(Examples):

    ```LISP
    (setq lst '(1 2 3 (4 5 6) ((V)) vi 7 8 9 10)) 
    =>  (1 2 3 (4 5 6) ((V)) VI 7 8 9 10)
    (first lst) =>  1
    (tenth lst) =>  10
    (fifth lst) =>  ((V))
    (second (fourth lst)) =>  5
    (sixth '(1 2 3)) =>  NIL
    (setf (fourth lst) "four") =>  "four"
    lst =>  (1 2 3 "four" ((V)) VI 7 8 9 10)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        car, nth

* 注意(Notes):

        first 功能上等价于 car, second 功能上等价于 cadr, third 功能上等价于 caddr, 并且 fourth 功能上等价于 cadddr.

        这里使用的序号是从一开始的, 相比于 nth 使用的从零开始的序号:

        (fifth x) ==  (nth 4 x)


### <span id="A-NTH">访问器 NTH</span>

* 语法(Syntax):

        nth n list => object

        (setf (nth n list) new-object)

* 参数和值(Arguments and Values):

        n---一个非负整数.
        list---一个列表, 它可能是一个点列表或一个环状列表.
        object---一个对象.
        new-object---一个对象.

* 描述(Description):

        nth 查找列表 list 的第 n 个元素, 其中这个列表 list 的 car 是第 0 个元素. 具体来说,

        (nth n list) ==  (car (nthcdr n list))

        nth 可以被用于给 setf 指定一个 place. 具体来说,

        (setf (nth n list) new-object) ==  (setf (car (nthcdr n list)) new-object)

* 示例(Examples):

    ```LISP
    (nth 0 '(foo bar baz)) =>  FOO
    (nth 1 '(foo bar baz)) =>  BAR
    (nth 3 '(foo bar baz)) =>  NIL
    (setq 0-to-3 (list 0 1 2 3)) =>  (0 1 2 3)
    (setf (nth 2 0-to-3) "two") =>  "two"
    0-to-3 =>  (0 1 "two" 3)
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

        elt, first, nthcdr

* 注意(Notes): None. 

### <span id="F-ENDP">函数 ENDP</span>

* 语法(Syntax):

        endp list => generalized-boolean

* 参数和值(Arguments and Values):

        list---一个列表, 它可能是一个点列表或一个环状列表.
        generalized-boolean---一个广义 boolean.

* 描述(Description):

        如果列表 list 是一个空列表就返回 true. 如果列表 list 是一个 cons 就返回 false.

* 示例(Examples):

    ```LISP
    (endp nil) =>  true
    (endp '(1 2)) =>  false
    (endp (cddr '(1 2))) =>  true
    ```

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

        如果 list 不是一个列表就应该发出一个 type-error 类型的错误.

* 也见(See Also): None.

* 注意(Notes):

        endp 的目的是特使 proper 列表的结束. 由于 endp 不会进入到一个 cons, 它被定义为通过一个点列表 it is well-defined to pass it a dotted list. 然而, if shorter "lists" are iteratively produced by calling cdr on such a dotted list and those "lists" are tested with endp, a situation that has undefined consequences will eventually result when the non-nil atom (which is not in fact a list) finally becomes the argument to endp. 由于这是使用 endp 的常用方法, 所以它是保守的编程风格, 并且与 endp 的意图一致, 即把 endp 当作一个在 proper 列表上的函数, 而不是强制执行一个参数类型的适当列表, 除非参数是原子的. Since this is the usual way in which endp is used, it is conservative programming style and consistent with the intent of endp to treat endp as simply a function on proper lists which happens not to enforce an argument type of proper list except when the argument is atomic. 
        <!--TODO 待翻译-->


### <span id="F-NULL">函数 NULL</span>

* 语法(Syntax):

null object => boolean

* 参数和值(Arguments and Values):

object---一个对象.

boolean---a boolean.

* 描述(Description):

Returns t if object is the empty list; otherwise, returns nil.

* 示例(Examples):

 (null '()) =>  T
 (null nil) =>  T
 (null t) =>  NIL
 (null 1) =>  NIL

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

not

* 注意(Notes):

null is intended to be used to test for the empty list whereas not is intended to be used to invert a boolean (or generalized boolean). Operationally, null and not compute the same result; which to use is a matter of style.

 (null object) ==  (typep object 'null) ==  (eq object '())


### <span id="F-NCONC">函数 NCONC</span>

* 语法(Syntax):

nconc &rest lists => concatenated-list

* 参数和值(Arguments and Values):

list---each but the last must be a list (which might be a dotted list but must not be a circular list); the last list may be any object.

concatenated-list---一个列表.

* 描述(Description):

Returns a list that is the concatenation of lists. If no lists are supplied, (nconc) returns nil. nconc is defined using the following recursive relationship:

 (nconc) =>  ()
 (nconc nil . lists) ==  (nconc . lists)
 (nconc list) =>  list
 (nconc list-1 list-2) ==  (progn (rplacd (last list-1) list-2) list-1)
 (nconc list-1 list-2 . lists) ==  (nconc (nconc list-1 list-2) . lists)

* 示例(Examples):

 (nconc) =>  NIL
 (setq x '(a b c)) =>  (A B C)
 (setq y '(d e f)) =>  (D E F)
 (nconc x y) =>  (A B C D E F)
 x =>  (A B C D E F)

Note, in the example, that the value of x is now different, since its last cons has been rplacd'd to the value of y. If (nconc x y) were evaluated again, it would yield a piece of a circular list, whose printed representation would be (A B C D E F D E F D E F ...), repeating forever; if the *print-circle* switch were non-nil, it would be printed as (A B C . #1=(D E F . #1#)).

 (setq foo (list 'a 'b 'c 'd 'e)
       bar (list 'f 'g 'h 'i 'j)
       baz (list 'k 'l 'm)) =>  (K L M)
 (setq foo (nconc foo bar baz)) =>  (A B C D E F G H I J K L M)
 foo =>  (A B C D E F G H I J K L M)
 bar =>  (F G H I J K L M)
 baz =>  (K L M)

 (setq foo (list 'a 'b 'c 'd 'e)
       bar (list 'f 'g 'h 'i 'j)
       baz (list 'k 'l 'm)) =>  (K L M)
 (setq foo (nconc nil foo bar nil baz)) =>  (A B C D E F G H I J K L M) 
 foo =>  (A B C D E F G H I J K L M)
 bar =>  (F G H I J K L M)
 baz =>  (K L M)

* 副作用(Side Effects):

The lists are modified rather than copied.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

append, concatenate

* 注意(Notes): None. 


### <span id="F-APPEND">函数 APPEND</span>

* 语法(Syntax):

append &rest lists => result

* 参数和值(Arguments and Values):

list---each must be a proper list except the last, which may be any object.

result---一个对象. This will be a list unless the last list was not a list and all preceding lists were null.

* 描述(Description):

append returns a new list that is the concatenation of the copies. lists are left unchanged; the list structure of each of lists except the last is copied. The last argument is not copied; it becomes the cdr of the final dotted pair of the concatenation of the preceding lists, or is returned directly if there are no preceding non-empty lists.

* 示例(Examples):

 (append '(a b c) '(d e f) '() '(g)) =>  (A B C D E F G)
 (append '(a b c) 'd) =>  (A B C . D)
 (setq lst '(a b c)) =>  (A B C)
 (append lst '(d)) =>  (A B C D)
 lst =>  (A B C)
 (append) =>  NIL
 (append 'a) =>  A

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

nconc, concatenate

* 注意(Notes): None. 

### <span id="F-REVAPPEND-NRECONC">函数 REVAPPEND, NRECONC</span>

* 语法(Syntax):

revappend list tail => result-list

nreconc list tail => result-list

* 参数和值(Arguments and Values):

list---a proper list.

tail---一个对象.

result-list---一个对象.

* 描述(Description):

revappend constructs a copy[2] of list, but with the elements in reverse order. It then appends (as if by nconc) the tail to that reversed list and returns the result.

nreconc reverses the order of elements in list (as if by nreverse). It then appends (as if by nconc) the tail to that reversed list and returns the result.

The resulting list shares list structure with tail.

* 示例(Examples):

 (let ((list-1 (list 1 2 3))
       (list-2 (list 'a 'b 'c)))
   (print (revappend list-1 list-2))
   (print (equal list-1 '(1 2 3)))
   (print (equal list-2 '(a b c))))
>>  (3 2 1 A B C) 
>>  T
>>  T
=>  T

 (revappend '(1 2 3) '()) =>  (3 2 1)
 (revappend '(1 2 3) '(a . b)) =>  (3 2 1 A . B)
 (revappend '() '(a b c)) =>  (A B C)
 (revappend '(1 2 3) 'a) =>  (3 2 1 . A)
 (revappend '() 'a) =>  A   ;degenerate case

 (let ((list-1 '(1 2 3))
       (list-2 '(a b c)))
   (print (nreconc list-1 list-2))
   (print (equal list-1 '(1 2 3)))
   (print (equal list-2 '(a b c))))
>>  (3 2 1 A B C) 
>>  NIL
>>  T
=>  T

* 副作用(Side Effects):

revappend does not modify either of its arguments. nreconc is permitted to modify list but not tail.

Although it might be implemented differently, nreconc is constrained to have side-effect behavior equivalent to:

 (nconc (nreverse list) tail)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

reverse, nreverse, nconc

* 注意(Notes):

The following functional equivalences are true, although good implementations will typically use a faster algorithm for achieving the same effect:

 (revappend list tail) ==  (nconc (reverse list) tail)
 (nreconc list tail) ==  (nconc (nreverse list) tail)


### <span id="F-BUTLAST-NBUTLAST">函数 BUTLAST, NBUTLAST</span>

* 语法(Syntax):

butlast list &optional n => result-list

nbutlast list &optional n => result-list

* 参数和值(Arguments and Values):

list---a list, which might be a dotted list but must not be a circular list.

n---一个非负整数.

result-list---一个列表.

* 描述(Description):

butlast returns a copy of list from which the last n conses have been omitted. If n is not supplied, its value is 1. If there are fewer than n conses in list, nil is returned and, in the case of nbutlast, list is not modified.

nbutlast is like butlast, but nbutlast may modify list. It changes the cdr of the cons n+1 from the end of the list to nil.

* 示例(Examples):

 (setq lst '(1 2 3 4 5 6 7 8 9)) =>  (1 2 3 4 5 6 7 8 9)
 (butlast lst) =>  (1 2 3 4 5 6 7 8)
 (butlast lst 5) =>  (1 2 3 4)
 (butlast lst (+ 5 5)) =>  NIL
 lst =>  (1 2 3 4 5 6 7 8 9)
 (nbutlast lst 3) =>  (1 2 3 4 5 6)
 lst =>  (1 2 3 4 5 6)
 (nbutlast lst 99) =>  NIL
 lst =>  (1 2 3 4 5 6)
 (butlast '(a b c d)) =>  (A B C)
 (butlast '((a b) (c d))) =>  ((A B))
 (butlast '(a)) =>  NIL
 (butlast nil) =>  NIL
 (setq foo (list 'a 'b 'c 'd)) =>  (A B C D)
 (nbutlast foo) =>  (A B C)
 foo =>  (A B C)
 (nbutlast (list 'a)) =>  NIL
 (nbutlast '()) =>  NIL

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if list is not a proper list or a dotted list. Should signal an error of type type-error if n is not a non-negative integer.

* 也见(See Also): None.

* 注意(Notes):

 (butlast list n) ==  (ldiff list (last list n))


### <span id="F-LAST">函数 LAST</span>

* 语法(Syntax):

last list &optional n => tail

* 参数和值(Arguments and Values):

list---a list, which might be a dotted list but must not be a circular list.

n---一个非负整数. The default is 1.

tail---一个对象.

* 描述(Description):

last returns the last n conses (not the last n elements) of list). If list is (), last returns ().

If n is zero, the atom that terminates list is returned. If n is greater than or equal to the number of cons cells in list, the result is list.

* 示例(Examples):

 (last nil) =>  NIL
 (last '(1 2 3)) =>  (3)
 (last '(1 2 . 3)) =>  (2 . 3)
 (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
 (last x) =>  (D)
 (rplacd (last x) (list 'e 'f)) x =>  (A B C D E F)
 (last x) =>  (F)

 (last '(a b c))   =>  (C)

 (last '(a b c) 0) =>  ()
 (last '(a b c) 1) =>  (C)
 (last '(a b c) 2) =>  (B C)
 (last '(a b c) 3) =>  (A B C)
 (last '(a b c) 4) =>  (A B C)

 (last '(a . b) 0) =>  B
 (last '(a . b) 1) =>  (A . B)
 (last '(a . b) 2) =>  (A . B)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

The consequences are undefined if list is a circular list. Should signal an error of type type-error if n is not a non-negative integer.

* 也见(See Also):

butlast, nth

* 注意(Notes):

The following code could be used to define last.

 (defun last (list &optional (n 1))
   (check-type n (integer 0))
   (do ((l list (cdr l))
        (r list)
        (i 0 (+ i 1)))
       ((atom l) r)
     (if (>= i n) (pop r))))


### <span id="F-LDIFF-TAILP">函数 LDIFF, TAILP</span>

* 语法(Syntax):

ldiff list object => result-list

tailp object list => generalized-boolean

* 参数和值(Arguments and Values):

list---a list, which might be a dotted list.

object---一个对象.

result-list---一个列表.

generalized-boolean---一个广义 boolean.

* 描述(Description):

If object is the same as some tail of list, tailp returns true; otherwise, it returns false.

If object is the same as some tail of list, ldiff returns a fresh list of the elements of list that precede object in the list structure of list; otherwise, it returns a copy[2] of list.

* 示例(Examples):

 (let ((lists '#((a b c) (a b c . d))))
   (dotimes (i (length lists)) ()
     (let ((list (aref lists i)))
       (format t "~2&list=~S ~21T(tailp object list)~
                  ~44T(ldiff list object)~%" list)
         (let ((objects (vector list (cddr list) (copy-list (cddr list))
                                '(f g h) '() 'd 'x)))
           (dotimes (j (length objects)) ()
             (let ((object (aref objects j)))
               (format t "~& object=~S ~21T~S ~44T~S"
                       object (tailp object list) (ldiff list object))))))))
>>  
>>  list=(A B C)         (tailp object list)    (ldiff list object)
>>   object=(A B C)      T                      NIL
>>   object=(C)          T                      (A B)
>>   object=(C)          NIL                    (A B C)
>>   object=(F G H)      NIL                    (A B C)
>>   object=NIL          T                      (A B C)
>>   object=D            NIL                    (A B C)
>>   object=X            NIL                    (A B C)
>>  
>>  list=(A B C . D)     (tailp object list)    (ldiff list object)
>>   object=(A B C . D)  T                      NIL
>>   object=(C . D)      T                      (A B)
>>   object=(C . D)      NIL                    (A B C . D)
>>   object=(F G H)      NIL                    (A B C . D)
>>   object=NIL          NIL                    (A B C . D)
>>   object=D            T                      (A B C)
>>   object=X            NIL                    (A B C . D)
=>  NIL

* 副作用(Side Effects):

Neither ldiff nor tailp modifies either of its arguments.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list is not a proper list or a dotted list.

* 也见(See Also):

set-difference

* 注意(Notes):

If the list is a circular list, tailp will reliably yield a value only if the given object is in fact a tail of list. Otherwise, the consequences are unspecified: a given implementation which detects the circularity must return false, but since an implementation is not obliged to detect such a situation, tailp might just loop indefinitely without returning in that case.

tailp could be defined as follows:

 (defun tailp (object list)
   (do ((list list (cdr list)))
       ((atom list) (eql list object))
      (if (eql object list)
          (return t))))

and ldiff could be defined by:

(defun ldiff (list object)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)))
    (when (eql object list)
      (return (nreverse r)))))


### <span id="F-NTHCDR">函数 NTHCDR</span>

* 语法(Syntax):

nthcdr n list => tail

* 参数和值(Arguments and Values):

n---一个非负整数.

list---a list, which might be a dotted list or a circular list.

tail---一个对象.

* 描述(Description):

Returns the tail of list that would be obtained by calling cdr n times in succession.

* 示例(Examples):

 (nthcdr 0 '()) =>  NIL
 (nthcdr 3 '()) =>  NIL
 (nthcdr 0 '(a b c)) =>  (A B C)
 (nthcdr 2 '(a b c)) =>  (C)
 (nthcdr 4 '(a b c)) =>  ()
 (nthcdr 1 '(0 . 1)) =>  1

 (locally (declare (optimize (safety 3)))
   (nthcdr 3 '(0 . 1)))
 Error: Attempted to take CDR of 1.

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should signal an error of type type-error if n is not a non-negative integer.

For n being an integer greater than 1, the error checking done by (nthcdr n list) is the same as for (nthcdr (- n 1) (cdr list)); see the function cdr.

* 也见(See Also):

cdr, nth, rest

* 注意(Notes): None. 

### <span id="A-REST">访问器 REST</span>

* 语法(Syntax):

rest list => tail

(setf (rest list) new-tail)

* 参数和值(Arguments and Values):

list---a list, which might be a dotted list or a circular list.

tail---一个对象.

* 描述(Description):

rest performs the same operation as cdr, but mnemonically complements first. Specifically,

 (rest list) ==  (cdr list)
 (setf (rest list) new-tail) ==  (setf (cdr list) new-tail)

* 示例(Examples):

 (rest '(1 2)) =>  (2)
 (rest '(1 . 2)) =>  2
 (rest '(1)) =>  NIL
 (setq *cons* '(1 . 2)) =>  (1 . 2)
 (setf (rest *cons*) "two") =>  "two"
 *cons* =>  (1 . "two")

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

cdr, nthcdr

* 注意(Notes):

rest is often preferred stylistically over cdr when the argument is to being subjectively viewed as a list rather than as a cons. 

### <span id="F-MEMBER-ALL">函数 MEMBER, MEMBER-IF, MEMBER-IF-NOT</span>

* 语法(Syntax):

member item list &key key test test-not => tail

member-if predicate list &key key => tail

member-if-not predicate list &key key => tail

* 参数和值(Arguments and Values):

item---一个对象.

list---a proper list.

predicate---a designator for a function of one argument that returns a generalized boolean.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

tail---一个列表.

* 描述(Description):

member, member-if, and member-if-not each search list for item or for a top-level element that satisfies the test. The argument to the predicate function is an element of list.

If some element satisfies the test, the tail of list beginning with this element is returned; otherwise nil is returned.

list is searched on the top level only.

* 示例(Examples):

 (member 2 '(1 2 3)) =>  (2 3)                                 
 (member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr) =>  ((3 . 4))
 (member 'e '(a b c d)) =>  NIL

 (member-if #'listp '(a b nil c d)) =>  (NIL C D)
 (member-if #'numberp '(a #\Space 5/3 foo)) =>  (5/3 FOO)
 (member-if-not #'zerop 
                 '(3 6 9 11 . 12)
                 :key #'(lambda (x) (mod x 3))) =>  (11 . 12)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list is not a proper list.

* 也见(See Also):

find, position, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

The function member-if-not is deprecated.

In the following

 (member 'a '(g (a y) c a d e a f)) =>  (A D E A F)

the value returned by member is identical to the portion of the list beginning with a. Thus rplaca on the result of member can be used to alter the part of the list where a was found (assuming a check has been made that member did not return nil). 


### <span id="F-MAP-ALL">函数 MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, MAPCON</span>

* 语法(Syntax):

mapc function &rest lists+ => list-1

mapcar function &rest lists+ => result-list

mapcan function &rest lists+ => concatenated-results

mapl function &rest lists+ => list-1

maplist function &rest lists+ => result-list

mapcon function &rest lists+ => concatenated-results

* 参数和值(Arguments and Values):

function---a designator for a function that must take as many arguments as there are lists.

list---a proper list.

list-1---the first list (which must be a proper list).

result-list---一个列表.

concatenated-results---一个列表.

* 描述(Description):

The mapping operation involves applying function to successive sets of arguments in which one argument is obtained from each sequence. Except for mapc and mapl, the result contains the results returned by function. In the cases of mapc and mapl, the resulting sequence is list.

function is called first on all the elements with index 0, then on all those with index 1, and so on. result-type specifies the type of the resulting sequence. If function is a symbol, it is coerced to a function as if by symbol-function.

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.

mapc is like mapcar except that the results of applying function are not accumulated. The list argument is returned.

maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.

mapl is like maplist except that the results of applying function are not accumulated; list-1 is returned.

mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of nconc rather than list. That is,

 (mapcon f x1 ... xn)
   ==  (apply #'nconc (maplist f x1 ... xn))

and similarly for the relationship between mapcan and mapcar.

* 示例(Examples):

 (mapcar #'car '((1 a) (2 b) (3 c))) =>  (1 2 3) 
 (mapcar #'abs '(3 -4 2 -5 -6)) =>  (3 4 2 5 6)
 (mapcar #'cons '(a b c) '(1 2 3)) =>  ((A . 1) (B . 2) (C . 3))

 (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) 
=>  ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)) 
 (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
=>  ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
 (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
=>  (0 0 1 0 1 1 1)
;An entry is 1 if the corresponding element of the input
;  list was the last instance of that element in the input list.

 (setq dummy nil) =>  NIL 
 (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
        '(1 2 3 4)
        '(a b c d e)
        '(x y z)) =>  (1 2 3 4) 
 dummy =>  (1 A X 2 B Y 3 C Z)                   

 (setq dummy nil) =>  NIL 
 (mapl #'(lambda (x) (push x dummy)) '(1 2 3 4)) =>  (1 2 3 4) 
 dummy =>  ((4) (3 4) (2 3 4) (1 2 3 4)) 

 (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
          '(nil nil nil d e)
          '(1 2 3 4 5 6)) =>  (D 4 E 5) 
 (mapcan #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))
=>  (1 3 4 5)

In this case the function serves as a filter; this is a standard Lisp idiom using mapcan.

 (mapcon #'list '(1 2 3 4)) =>  ((1 2 3 4) (2 3 4) (3 4) (4)) 

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if any list is not a proper list.

* 也见(See Also):

dolist, map, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes): None. 


### <span id="F-ACONS">函数 ACONS</span>

* 语法(Syntax):

acons key datum alist => new-alist

* 参数和值(Arguments and Values):

key---一个对象.

datum---一个对象.

alist---一个关联列表.

new-alist---一个关联列表.

* 描述(Description):

Creates a fresh cons, the cdr of which is alist and the car of which is another fresh cons, the car of which is key and the cdr of which is datum.

* 示例(Examples):

 (setq alist '()) =>  NIL
 (acons 1 "one" alist) =>  ((1 . "one"))
 alist =>  NIL
 (setq alist (acons 1 "one" (acons 2 "two" alist))) =>  ((1 . "one") (2 . "two"))
 (assoc 1 alist) =>  (1 . "one")
 (setq alist (acons 1 "uno" alist)) =>  ((1 . "uno") (1 . "one") (2 . "two"))
 (assoc 1 alist) =>  (1 . "uno")

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

assoc, pairlis

* 注意(Notes):

(acons key datum alist) ==  (cons (cons key datum) alist)



### <span id="F-ASSOC-ALL">函数 ASSOC, ASSOC-IF, ASSOC-IF-NOT</span>

* 语法(Syntax):

assoc item alist &key key test test-not => entry

assoc-if predicate alist &key key => entry

assoc-if-not predicate alist &key key => entry

* 参数和值(Arguments and Values):

item---一个对象.

alist---一个关联列表.

predicate---a designator for a function of one argument that returns a generalized boolean.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

entry---a cons that is an element of alist, or nil.

* 描述(Description):

assoc, assoc-if, and assoc-if-not return the first cons in alist whose car satisfies the test, or nil if no such cons is found.

For assoc, assoc-if, and assoc-if-not, if nil appears in alist in place of a pair, it is ignored.

* 示例(Examples):

 (setq values '((x . 100) (y . 200) (z . 50))) =>  ((X . 100) (Y . 200) (Z . 50))
 (assoc 'y values) =>  (Y . 200)
 (rplacd (assoc 'y values) 201) =>  (Y . 201)
 (assoc 'y values) =>  (Y . 201)
 (setq alist '((1 . "one")(2 . "two")(3 . "three"))) 
=>  ((1 . "one") (2 . "two") (3 . "three"))
 (assoc 2 alist) =>  (2 . "two")
 (assoc-if #'evenp alist) =>  (2 . "two")
 (assoc-if-not #'(lambda(x) (< x 3)) alist) =>  (3 . "three")
 (setq alist '(("one" . 1)("two" . 2))) =>  (("one" . 1) ("two" . 2))
 (assoc "one" alist) =>  NIL
 (assoc "one" alist :test #'equalp) =>  ("one" . 1)
 (assoc "two" alist :key #'(lambda(x) (char x 2))) =>  NIL 
 (assoc #\o alist :key #'(lambda(x) (char x 2))) =>  ("two" . 2)
 (assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z))) =>   (R . X)
 (assoc 'goo '((foo . bar) (zoo . goo))) =>  NIL
 (assoc '2 '((1 a b c) (2 b c d) (-7 x y z))) =>  (2 B C D)
 (setq alist '(("one" . 1) ("2" . 2) ("three" . 3)))
=>  (("one" . 1) ("2" . 2) ("three" . 3))
 (assoc-if-not #'alpha-char-p alist
               :key #'(lambda (x) (char x 0))) =>  ("2" . 2)

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if alist is not an association list.

* 也见(See Also):

rassoc, find, member, position, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

The function assoc-if-not is deprecated.

It is possible to rplacd the result of assoc, provided that it is not nil, in order to ``update'' alist.

The two expressions

 (assoc item list :test fn)

and

 (find item list :test fn :key #'car)

are equivalent in meaning with one exception: if nil appears in alist in place of a pair, and item is nil, find will compute the car of the nil in alist, find that it is equal to item, and return nil, whereas assoc will ignore the nil in alist and continue to search for an actual cons whose car is nil. 


### <span id="F-COPY-ALIST">函数 COPY-ALIST</span>

* 语法(Syntax):

copy-alist alist => new-alist

* 参数和值(Arguments and Values):

alist---一个关联列表.

new-alist---一个关联列表.

* 描述(Description):

copy-alist returns a copy of alist.

The list structure of alist is copied, and the elements of alist which are conses are also copied (as conses only). Any other objects which are referred to, whether directly or indirectly, by the alist continue to be shared.

* 示例(Examples):

(defparameter *alist* (acons 1 "one" (acons 2 "two" '())))
*alist* =>  ((1 . "one") (2 . "two"))
(defparameter *list-copy* (copy-list *alist*))
*list-copy* =>  ((1 . "one") (2 . "two"))
(defparameter *alist-copy* (copy-alist *alist*))
*alist-copy* =>  ((1 . "one") (2 . "two"))
(setf (cdr (assoc 2 *alist-copy*)) "deux") =>  "deux"
*alist-copy* =>  ((1 . "one") (2 . "deux"))
*alist* =>  ((1 . "one") (2 . "two"))
(setf (cdr (assoc 1 *list-copy*)) "uno") =>  "uno"
*list-copy* =>  ((1 . "uno") (2 . "two"))
*alist* =>  ((1 . "uno") (2 . "two"))

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

copy-list

* 注意(Notes): None. 

### <span id="F-PAIRLIS">函数 PAIRLIS</span>

* 语法(Syntax):

pairlis keys data &optional alist => new-alist

* 参数和值(Arguments and Values):

keys---a proper list.

data---a proper list.

alist---一个关联列表. The default is the empty list.

new-alist---一个关联列表.

* 描述(Description):

Returns an association list that associates elements of keys to corresponding elements of data. The consequences are undefined if keys and data are not of the same length.

If alist is supplied, pairlis returns a modified alist with the new pairs prepended to it. The new pairs may appear in the resulting association list in either forward or backward order. The result of

 (pairlis '(one two) '(1 2) '((three . 3) (four . 19)))

might be

 ((one . 1) (two . 2) (three . 3) (four . 19))

or

 ((two . 2) (one . 1) (three . 3) (four . 19))

* 示例(Examples):

 (setq keys '(1 2 3)
        data '("one" "two" "three")
        alist '((4 . "four"))) =>  ((4 . "four"))
 (pairlis keys data) =>  ((3 . "three") (2 . "two") (1 . "one"))
 (pairlis keys data alist)
=>  ((3 . "three") (2 . "two") (1 . "one") (4 . "four"))
 alist =>  ((4 . "four"))

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if keys and data are not proper lists.

* 也见(See Also):

acons

* 注意(Notes): None. 


### <span id="F-RASSOC-ALL">函数 RASSOC, RASSOC-IF, RASSOC-IF-NOT</span>

* 语法(Syntax):

rassoc item alist &key key test test-not => entry

rassoc-if predicate alist &key key => entry

rassoc-if-not predicate alist &key key => entry

* 参数和值(Arguments and Values):

item---一个对象.

alist---一个关联列表.

predicate---a designator for a function of one argument that returns a generalized boolean.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

entry---a cons that is an element of the alist, or nil.

* 描述(Description):

rassoc, rassoc-if, and rassoc-if-not return the first cons whose cdr satisfies the test. If no such cons is found, nil s returned.

If nil appears in alist in place of a pair, it is ignored.

* 示例(Examples):

 (setq alist '((1 . "one") (2 . "two") (3 . 3))) 
=>  ((1 . "one") (2 . "two") (3 . 3))
 (rassoc 3 alist) =>  (3 . 3)
 (rassoc "two" alist) =>  NIL
 (rassoc "two" alist :test 'equal) =>  (2 . "two")
 (rassoc 1 alist :key #'(lambda (x) (if (numberp x) (/ x 3)))) =>  (3 . 3)
 (rassoc 'a '((a . b) (b . c) (c . a) (z . a))) =>  (C . A)
 (rassoc-if #'stringp alist) =>  (1 . "one")
 (rassoc-if-not #'vectorp alist) =>  (3 . 3)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

assoc, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

The function rassoc-if-not is deprecated.

It is possible to rplaca the result of rassoc, provided that it is not nil, in order to ``update'' alist.

The expressions

 (rassoc item list :test fn)

and

 (find item list :test fn :key #'cdr)

are equivalent in meaning, except when the item is nil nd nil appears in place of a pair in the alist. See the function assoc. 


### <span id="F-GET-PROPERTIES">函数 GET-PROPERTIES</span>

* 语法(Syntax):

get-properties plist indicator-list => indicator, value, tail

* 参数和值(Arguments and Values):

plist---a property list.

indicator-list---a proper list (of indicators).

indicator---an object that is an element of indicator-list.

value---一个对象.

tail---一个列表.

* 描述(Description):

get-properties is used to look up any of several property list entries all at once.

It searches the plist for the first entry whose indicator is identical to one of the objects in indicator-list. If such an entry is found, the indicator and value returned are the property indicator and its associated property value, and the tail returned is the tail of the plist that begins with the found entry (i.e., whose car is the indicator). If no such entry is found, the indicator, value, and tail are all nil.

* 示例(Examples):

 (setq x '()) =>  NIL
 (setq *indicator-list* '(prop1 prop2)) =>  (PROP1 PROP2)
 (getf x 'prop1) =>  NIL
 (setf (getf x 'prop1) 'val1) =>  VAL1
 (eq (getf x 'prop1) 'val1) =>  true
 (get-properties x *indicator-list*) =>  PROP1, VAL1, (PROP1 VAL1)
 x =>  (PROP1 VAL1)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

get, getf

* 注意(Notes): None. 


### <span id="A-GETF">访问器 GETF</span>

* 语法(Syntax):

getf plist indicator &optional default => value

(setf (getf place indicator &optional default) new-value)

* 参数和值(Arguments and Values):

plist---a property list.

place---a place, the value of which is a property list.

indicator---一个对象.

default---一个对象. The default is nil.

value---一个对象.

new-value---一个对象.

* 描述(Description):

getf finds a property on the plist whose property indicator is identical to indicator, and returns its corresponding property value. If there are multiple properties[1] with that property indicator, getf uses the first such property. If there is no property with that property indicator, default is returned.

setf of getf may be used to associate a new object with an existing indicator in the property list held by place, or to create a new assocation if none exists. If there are multiple properties[1] with that property indicator, setf of getf associates the new-value with the first such property. When a getf form is used as a setf place, any default which is supplied is evaluated according to normal left-to-right evaluation rules, but its value is ignored.

setf of getf is permitted to either write the value of place itself, or modify of any part, car or cdr, of the list structure held by place.

* 示例(Examples):

 (setq x '()) =>  NIL
 (getf x 'prop1) =>  NIL
 (getf x 'prop1 7) =>  7
 (getf x 'prop1) =>  NIL
 (setf (getf x 'prop1) 'val1) =>  VAL1
 (eq (getf x 'prop1) 'val1) =>  true
 (getf x 'prop1) =>  VAL1
 (getf x 'prop1 7) =>  VAL1
 x =>  (PROP1 VAL1)

;; Examples of implementation variation permitted.
 (setq foo (list 'a 'b 'c 'd 'e 'f)) =>  (A B C D E F)
 (setq bar (cddr foo)) =>  (C D E F)
 (remf foo 'c) =>  true
 foo =>  (A B E F)
 bar
=>  (C D E F)
OR=>  (C)
OR=>  (NIL)
OR=>  (C NIL)
OR=>  (C D)

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

get, get-properties, setf, Section 5.1.2.2 (Function Call Forms as Places)

* 注意(Notes):

There is no way (using getf) to distinguish an absent property from one whose value is default; but see get-properties.

Note that while supplying a default argument to getf in a setf situation is sometimes not very interesting, it is still important because some macros, such as push and incf, require a place argument which data is both read from and written to. In such a context, if a default argument is to be supplied for the read situation, it must be syntactically valid for the write situation as well. For example,

 (let ((plist '()))
   (incf (getf plist 'count 0))
   plist) =>  (COUNT 1)



### <span id="M-REMF">宏 REMF</span>

* 语法(Syntax):

remf place indicator => generalized-boolean

* 参数和值(Arguments and Values):

place---a place.

indicator---一个对象.

generalized-boolean---一个广义 boolean.

* 描述(Description):

remf removes from the property list stored in place a property[1] with a property indicator identical to indicator. If there are multiple properties[1] with the identical key, remf only removes the first such property. remf returns false if no such property was found, or true if a property was found.

The property indicator and the corresponding property value are removed in an undefined order by destructively splicing the property list. remf is permitted to either setf place or to setf any part, car or cdr, of the list structure held by that place.

For information about the evaluation of subforms of place, see Section 5.1.1.1 (Evaluation of Subforms to Places).

* 示例(Examples):

 (setq x (cons () ())) =>  (NIL)
 (setf (getf (car x) 'prop1) 'val1) =>  VAL1
 (remf (car x) 'prop1) =>  true
 (remf (car x) 'prop1) =>  false

* 副作用(Side Effects):

The property list stored in place is modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

remprop, getf

* 注意(Notes): None. 


### <span id="F-INTERSECTION-NINTERSECTION)">函数 INTERSECTION, NINTERSECTION</span>

* 语法(Syntax):

intersection list-1 list-2 &key key test test-not => result-list

nintersection list-1 list-2 &key key test test-not => result-list

* 参数和值(Arguments and Values):

list-1---a proper list.

list-2---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

result-list---一个列表.

* 描述(Description):

intersection and nintersection return a list that contains every element that occurs in both list-1 and list-2.

nintersection is the destructive version of intersection. It performs the same operation, but may destroy list-1 using its cells to construct the result. list-2 is not destroyed.

The intersection operation is described as follows. For all possible ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. It is an error if :test and :test-not are supplied in the same function call.

If :key is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element. If :key is not supplied or nil, the list-1 and list-2 elements are used.

For every pair that satifies the test, exactly one of the two elements of the pair will be put in the result. No element from either list appears in the result that does not satisfy the test for an element from the other list. If one of the lists contains duplicate elements, there may be duplication in the result.

There is no guarantee that the order of elements in the result will reflect the ordering of the arguments in any particular way. The result list may share cells with, or be eq to, either list-1 or list-2 if appropriate.

* 示例(Examples):

 (setq list1 (list 1 1 2 3 4 a b c "A" "B" "C" "d")
       list2 (list 1 4 5 b c d "a" "B" "c" "D")) 
  =>  (1 4 5 B C D "a" "B" "c" "D")
 (intersection list1 list2) =>  (C B 4 1 1)
 (intersection list1 list2 :test 'equal) =>  ("B" C B 4 1 1)
 (intersection list1 list2 :test #'equalp) =>  ("d" "C" "B" "A" C B 4 1 1) 
 (nintersection list1 list2) =>  (1 1 4 B C)
 list1 =>  implementation-dependent ;e.g.,  (1 1 4 B C)
 list2 =>  implementation-dependent ;e.g.,  (1 4 5 B C D "a" "B" "c" "D")
 (setq list1 (copy-list '((1 . 2) (2 . 3) (3 . 4) (4 . 5))))
=>  ((1 . 2) (2 . 3) (3 . 4) (4 . 5)) 
 (setq list2 (copy-list '((1 . 3) (2 . 4) (3 . 6) (4 . 8))))
=>  ((1 . 3) (2 . 4) (3 . 6) (4 . 8)) 
 (nintersection list1 list2 :key #'cdr) =>  ((2 . 3) (3 . 4)) 
 list1 =>  implementation-dependent ;e.g.,  ((1 . 2) (2 . 3) (3 . 4)) 
 list2 =>  implementation-dependent ;e.g.,  ((1 . 3) (2 . 4) (3 . 6) (4 . 8)) 

* 副作用(Side Effects):

nintersection can modify list-1, but not list-2.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists.

* 也见(See Also):

union, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

Since the nintersection side effect is not required, it should not be used in for-effect-only positions in portable code. 


### <span id="F-ADJOIN">函数 ADJOIN</span>

* 语法(Syntax):

adjoin item list &key key test test-not => new-list

* 参数和值(Arguments and Values):

item---一个对象.

list---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

new-list---一个列表.

* 描述(Description):

Tests whether item is the same as an existing element of list. If the item is not an existing element, adjoin adds it to list (as if by cons) and returns the resulting list; otherwise, nothing is added and the original list is returned.

The test, test-not, and key affect how it is determined whether item is the same as an element of list. For details, see Section 17.2.1 (Satisfying a Two-Argument Test).

* 示例(Examples):

 (setq slist '()) =>  NIL 
 (adjoin 'a slist) =>  (A) 
 slist =>  NIL 
 (setq slist (adjoin '(test-item 1) slist)) =>  ((TEST-ITEM 1)) 
 (adjoin '(test-item 1) slist) =>  ((TEST-ITEM 1) (TEST-ITEM 1)) 
 (adjoin '(test-item 1) slist :test 'equal) =>  ((TEST-ITEM 1)) 
 (adjoin '(new-test-item 1) slist :key #'cadr) =>  ((TEST-ITEM 1)) 
 (adjoin '(new-test-item 1) slist) =>  ((NEW-TEST-ITEM 1) (TEST-ITEM 1)) 

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list is not a proper list.

* 也见(See Also):

pushnew, Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

 (adjoin item list :key fn)
   ==  (if (member (fn item) list :key fn) list (cons item list))



### <span id="M-PUSHNEW">宏 PUSHNEW</span>

* 语法(Syntax):

pushnew item place &key key test test-not

=> new-place-value

* 参数和值(Arguments and Values):

item---一个对象.

place---a place, the value of which is a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

new-place-value---a list (the new value of place).

* 描述(Description):

pushnew tests whether item is the same as any existing element of the list stored in place. If item is not, it is prepended to the list, and the new list is stored in place.

pushnew returns the new list that is stored in place.

Whether or not item is already a member of the list that is in place is determined by comparisons using :test or :test-not. The first argument to the :test or :test-not function is item; the second argument is an element of the list in place as returned by the :key function (if supplied).

If :key is supplied, it is used to extract the part to be tested from both item and the list element, as for adjoin.

The argument to the :key function is an element of the list stored in place. The :key function typically returns part part of the element of the list. If :key is not supplied or nil, the list element is used.

For information about the evaluation of subforms of place, see Section 5.1.1.1 (Evaluation of Subforms to Places).

It is implementation-dependent whether or not pushnew actually executes the storing form for its place in the situation where the item is already a member of the list held by place.

* 示例(Examples):

 (setq x '(a (b c) d)) =>  (A (B C) D)
 (pushnew 5 (cadr x)) =>  (5 B C)   
 x =>  (A (5 B C) D)
 (pushnew 'b (cadr x)) =>  (5 B C)  
 x =>  (A (5 B C) D)
 (setq lst '((1) (1 2) (1 2 3))) =>  ((1) (1 2) (1 2 3))
 (pushnew '(2) lst) =>  ((2) (1) (1 2) (1 2 3))
 (pushnew '(1) lst) =>  ((1) (2) (1) (1 2) (1 2 3))
 (pushnew '(1) lst :test 'equal) =>  ((1) (2) (1) (1 2) (1 2 3))
 (pushnew '(1) lst :key #'car) =>  ((1) (2) (1) (1 2) (1 2 3)) 

* 副作用(Side Effects):

The contents of place may be modified.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations): None.

* 也见(See Also):

push, adjoin, Section 5.1 (Generalized Reference)

* 注意(Notes):

The effect of

 (pushnew item place :test p)

is roughly equivalent to

 (setf place (adjoin item place :test p))

except that the subforms of place are evaluated only once, and item is evaluated before place. 

### <span id="F-SET-DIFFERENCE-ALL">函数 SET-DIFFERENCE, NSET-DIFFERENCE</span>

* 语法(Syntax):

set-difference list-1 list-2 &key key test test-not => result-list

nset-difference list-1 list-2 &key key test test-not => result-list

* 参数和值(Arguments and Values):

list-1---a proper list.

list-2---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

result-list---一个列表.

* 描述(Description):

set-difference returns a list of elements of list-1 that do not appear in list-2.

nset-difference is the destructive version of set-difference. It may destroy list-1.

For all possible ordered pairs consisting of one element from list-1 and one element from list-2, the :test or :test-not function is used to determine whether they satisfy the test. The first argument to the :test or :test-not function is the part of an element of list-1 that is returned by the :key function (if supplied); the second argument is the part of an element of list-2 that is returned by the :key function (if supplied).

If :key is supplied, its argument is a list-1 or list-2 element. The :key function typically returns part of the supplied element. If :key is not supplied, the list-1 or list-2 element is used.

An element of list-1 appears in the result if and only if it does not match any element of list-2.

There is no guarantee that the order of elements in the result will reflect the ordering of the arguments in any particular way. The result list may share cells with, or be eq to, either of list-1 or list-2, if appropriate.

* 示例(Examples):

 (setq lst1 (list "A" "b" "C" "d")
       lst2 (list "a" "B" "C" "d")) =>  ("a" "B" "C" "d")
 (set-difference lst1 lst2) =>  ("d" "C" "b" "A")
 (set-difference lst1 lst2 :test 'equal) =>  ("b" "A")
 (set-difference lst1 lst2 :test #'equalp) =>  NIL 
 (nset-difference lst1 lst2 :test #'string=) =>  ("A" "b")
 (setq lst1 '(("a" . "b") ("c" . "d") ("e" . "f")))
=>  (("a" . "b") ("c" . "d") ("e" . "f")) 
 (setq lst2 '(("c" . "a") ("e" . "b") ("d" . "a")))
=>  (("c" . "a") ("e" . "b") ("d" . "a")) 
 (nset-difference lst1 lst2 :test #'string= :key #'cdr)
=>  (("c" . "d") ("e" . "f")) 
 lst1 =>  (("a" . "b") ("c" . "d") ("e" . "f")) 
 lst2 =>  (("c" . "a") ("e" . "b") ("d" . "a")) 

;; Remove all flavor names that contain "c" or "w".
 (set-difference '("strawberry" "chocolate" "banana"
                  "lemon" "pistachio" "rhubarb")
          '(#\c #\w)
          :test #'(lambda (s c) (find c s)))
=>  ("banana" "rhubarb" "lemon")    ;One possible ordering.

* 副作用(Side Effects):

nset-difference may destroy list-1.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists.

* 也见(See Also):

Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated. 

### <span id="F-SET-EXCLUSIVE-OR-ALL">函数 SET-EXCLUSIVE-OR, NSET-EXCLUSIVE-OR</span>

* 语法(Syntax):

set-exclusive-or list-1 list-2 &key key test test-not => result-list

nset-exclusive-or list-1 list-2 &key key test test-not => result-list

* 参数和值(Arguments and Values):

list-1---a proper list.

list-2---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

result-list---一个列表.

* 描述(Description):

set-exclusive-or returns a list of elements that appear in exactly one of list-1 and list-2.

nset-exclusive-or is the destructive version of set-exclusive-or.

For all possible ordered pairs consisting of one element from list-1 and one element from list-2, the :test or :test-not function is used to determine whether they satisfy the test.

If :key is supplied, it is used to extract the part to be tested from the list-1 or list-2 element. The first argument to the :test or :test-not function is the part of an element of list-1 extracted by the :key function (if supplied); the second argument is the part of an element of list-2 extracted by the :key function (if supplied). If :key is not supplied or nil, the list-1 or list-2 element is used.

The result contains precisely those elements of list-1 and list-2 that appear in no matching pair.

The result list of set-exclusive-or might share storage with one of list-1 or list-2.

* 示例(Examples):

 (setq lst1 (list 1 "a" "b")
       lst2 (list 1 "A" "b")) =>  (1 "A" "b")
 (set-exclusive-or lst1 lst2) =>  ("b" "A" "b" "a")
 (set-exclusive-or lst1 lst2 :test #'equal) =>  ("A" "a")
 (set-exclusive-or lst1 lst2 :test 'equalp) =>  NIL 
 (nset-exclusive-or lst1 lst2) =>  ("a" "b" "A" "b") 
 (setq lst1 (list (("a" . "b") ("c" . "d") ("e" . "f"))))
=>  (("a" . "b") ("c" . "d") ("e" . "f"))
 (setq lst2 (list (("c" . "a") ("e" . "b") ("d" . "a"))))
=>  (("c" . "a") ("e" . "b") ("d" . "a")) 
 (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr)
=>  (("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")) 
 lst1 =>  (("a" . "b") ("c" . "d") ("e" . "f"))
 lst2 =>  (("c" . "a") ("d" . "a")) 

* 副作用(Side Effects):

nset-exclusive-or is permitted to modify any part, car or cdr, of the list structure of list-1 or list-2.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists.

* 也见(See Also):

Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

Since the nset-exclusive-or side effect is not required, it should not be used in for-effect-only positions in portable code. 


### <span id="F-SUBSETP">函数 SUBSETP</span>

* 语法(Syntax):

subsetp list-1 list-2 &key key test test-not => generalized-boolean

* 参数和值(Arguments and Values):

list-1---a proper list.

list-2---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

generalized-boolean---一个广义 boolean.

* 描述(Description):

subsetp returns true if every element of list-1 matches some element of list-2, and false otherwise.

Whether a list element is the same as another list element is determined by the functions specified by the keyword arguments. The first argument to the :test or :test-not function is typically part of an element of list-1 extracted by the :key function; the second argument is typically part of an element of list-2 extracted by the :key function.

The argument to the :key function is an element of either list-1 or list-2; the return value is part of the element of the supplied list element. If :key is not supplied or nil, the list-1 or list-2 element itself is supplied to the :test or :test-not function.

* 示例(Examples):

 (setq cosmos '(1 "a" (1 2))) =>  (1 "a" (1 2))
 (subsetp '(1) cosmos) =>  true
 (subsetp '((1 2)) cosmos) =>  false
 (subsetp '((1 2)) cosmos :test 'equal) =>  true
 (subsetp '(1 "A") cosmos :test #'equalp) =>  true
 (subsetp '((1) (2)) '((1) (2))) =>  false
 (subsetp '((1) (2)) '((1) (2)) :key #'car) =>  true

* 副作用(Side Effects): None.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists.

* 也见(See Also):

Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated. 


### <span id="F-UNION-NUNION">函数 UNION, NUNION</span>

* 语法(Syntax):

union list-1 list-2 &key key test test-not => result-list

nunion list-1 list-2 &key key test test-not => result-list

* 参数和值(Arguments and Values):

list-1---a proper list.

list-2---a proper list.

test---一个返回一个广义 boolean 的两参数函数的标识符.

test-not---一个返回一个广义 boolean 的两参数函数的标识符.

key---一个单参数函数的标识符, 或者 nil.

result-list---一个列表.

* 描述(Description):

union and nunion return a list that contains every element that occurs in either list-1 or list-2.

For all possible ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not is used to determine whether they satisfy the test. The first argument to the :test or :test-not function is the part of the element of list-1 extracted by the :key function (if supplied); the second argument is the part of the element of list-2 extracted by the :key function (if supplied).

The argument to the :key function is an element of list-1 or list-2; the return value is part of the supplied element. If :key is not supplied or nil, the element of list-1 or list-2 itself is supplied to the :test or :test-not function.

For every matching pair, one of the two elements of the pair will be in the result. Any element from either list-1 or list-2 that matches no element of the other will appear in the result.

If there is a duplication between list-1 and list-2, only one of the duplicate instances will be in the result. If either list-1 or list-2 has duplicate entries within it, the redundant entries might or might not appear in the result.

The order of elements in the result do not have to reflect the ordering of list-1 or list-2 in any way. The result list may be eq to either list-1 or list-2 if appropriate.

* 示例(Examples):

 (union '(a b c) '(f a d))
=>  (A B C F D)
OR=>  (B C F A D)
OR=>  (D F A B C)
 (union '((x 5) (y 6)) '((z 2) (x 4)) :key #'car)
=>  ((X 5) (Y 6) (Z 2))
OR=>  ((X 4) (Y 6) (Z 2))

 (setq lst1 (list 1 2 '(1 2) "a" "b")
       lst2 (list 2 3 '(2 3) "B" "C"))
=>  (2 3 (2 3) "B" "C")
 (nunion lst1 lst2)
=>  (1 (1 2) "a" "b" 2 3 (2 3) "B" "C") 
OR=>  (1 2 (1 2) "a" "b" "C" "B" (2 3) 3)

* 副作用(Side Effects):

nunion is permitted to modify any part, car or cdr, of the list structure of list-1 or list-2.

* 受此影响(Affected By): None.

* 异常情况(Exceptional Situations):

Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists.

* 也见(See Also):

intersection, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects)

* 注意(Notes):

The :test-not parameter is deprecated.

Since the nunion side effect is not required, it should not be used in for-effect-only positions in portable code. 


