;;лобосов никита андреевич 324
;;фал -> днф

;;запуск: (start '"список-фал")

;;перевод фал в днф
;;импликация: >
;;кон-ция:    &
;;диз-ция:    +
;;отрицание:  !
;;константы:  TRUE
;;			  FALSE

;;программа переводит фал во внутреннее представление, затем строит ДНФ по алгоритму:
;;1) избавиться от всех логических операций, содержащихся в формуле, заменив их основными: конъюнкцией, дизъюнкцией, отрицанием.
;;2) заменить знак отрицания, относящийся ко всему выражению, знаками отрицания, относящимися к отдельным переменным.
;;3) избавиться от знаков двойного отрицания.
;;4) применить, если нужно, к операциям конъюнкции и дизъюнкции свойства дистрибутивности и формулы поглощения.
;;перевести днф, в сокрднф при помощи метода Блейка


;;начало работы и тесты внизу!!!


(defun atom-in-list (s a)
	(append (list a) (list s)))
	
;;внутреннее представление
;;аргументы операций заключаем в скобки и объединяем в список ((арг1) опер. (арг2))
;;переменные и конст в скобки вместе с отрицаниями

(defun represent (s)
	(makelist (findsgn (makelist (findsgn (makelist (findsgn s '>)) '+)) '&)))

(defun findsgn (s sgn)
	(findwlist s sgn NIL))

(defun findwlist (s sgn sa)
	(cond ((null s) ())
		((check-bad s) (lstelem sa (act-for-bad s)))
		((null (cdr s)) (lstelem sa (car s)))
		((eq (car s) sgn) (findsgn (intrep sa sgn (cdr s)) sgn))
		(T (findwlist (cdr s) sgn (lstelem sa (car s))))
	))
	
	;;проверка, что не набор '! и скобка
(defun check-bad (s)
	(cond ((not (eq (car s) '!)) NIL)
		  (T (cond ((eq (cadr s) '!) (check-bad (cdr s)))
				   ((and (null (cddr s)) (not (atom (cadr s)))) T)
				   (T NIL)
			 ))
	))
	
	;;действия для случая выше
(defun act-for-bad (s)
	(cond ((null (cdr s)) (list (represent (car s))))
		  (T (append (list '!) (act-for-bad (cdr s))))
	))
	
	;;внутреннее  представление двух аргументов и символа операции
(defun intrep (s1 sgn s2)
	(intrep2 s1 sgn (secarg s2 sgn)))

(defun intrep2 (s1 sgn s2)
		(append (list (append (list (represent (makelist s1))) (cons sgn (list (represent (makelist (car s2))))))) (makelist (car (cdr s2)))))

(defun lstelem (s a)
	(append s (list a)))

	;;поиск второго аргумента и одновременное запоминание, где мы остановились
(defun secarg (s a)
	(secarg2 s a nil))
	
(defun secarg2 (s a s1)
	(cond ((null s) (list s1))
		((eq (car s) a) (append (list s1) (list s)))
		(T (secarg2 (cdr s) a (lstelem s1 (car s))))
		))
	
	;;удаление лишних скобок
(defun makelist (s)
	(cond ((atom (car s)) s)
		((not (null (cdr s))) s)
		(T (makelist (car s)))
	))
	
;;перевод в днф
;;первый шаг
;;избавиться от импликант(здесь проблема, смотреть в последнем тесте)
(defun fststep (s)
	(wtchpart s))
	
(defun wtchpart (s)
	(viaep s))
	
(defun viaep (s)
	(cond ((eq (car s) '!) (cons '! (viaep (cdr s))))
		(T (pre-findimp s))
	))
 
 (defun pre-findimp (s)
	(findimp s))
 
(defun findimp (s)
	(cond ((and (null (cdr s)) (not (atom (car s)))) (list (wtchpart (makelist s))))
		  ((null (cdr s)) s)
		  ((eq '> (cadr s)) (repimp s))
		  (T (intcheck 'wtchpart s))
	))

(defun repimp (s)
	(cond ((atom (caar s)) (append (list (cons '! (wtchpart (car s)))) (list '+) (list (wtchpart (car (cddr s))))))
		(T (append (list (cons '! (list (wtchpart (car s))))) (list '+) (list (forwtchpart (cddr s)))))
	))

(defun forwtchpart (s)
	(wtchpart (car s)))

	;;проверка на всех уровнях
(defun intcheck (ff s)
	(append (list (funcall ff (car s))) (list (cadr s)) (list (funcall ff (car (cddr s))))))
	
;;проверка двойных отрицаний (промежуточный шаг)
(defun intermstep (s)
	(makelist (chckdep s)))
	
(defun chckdep (s)
	(cond ((eq '! (car s)) (deldep s))
		((null (cdr s)) s)	
		(T (intcheck 'intermstep s))
	))
	
(defun deldep (s)
	(cond ((eq (cadr s) '!) (intermstep (cddr s)))
		((atom (cadr s)) s)
		(T (cons '! (list (intermstep (cadr s)))))
	))

;;применение правила де моргана (второй шаг)
(defun demorgan (s)
	(cond ((null (cdr s)) s)
		((eq '! (car s)) (usedm s))
		(T (intcheck 'demorgan s))
	))

	;;поиск + и & и примениение де моргана к ним
(defun usedm (s)
	(cond ((atom (cadr s)) s)
		((eq (cadadr s) '+) (dmchange s '&))
		((eq (cadadr s) '&) (dmchange s '+))
		(T (cadadr s))
	))

	;;приведение к внутр. представлению
(defun dmchange (s sgn)
	(let ((lb (caadr s)) 	  ;;левая скобка
		(rb (caddr (cadr s)))) ;;правая скобка 
		(demorgan (append (list (otherep lb)) (list sgn) (list (otherep rb))))
	))
	
	;;проверка ! в скобке	
(defun otherep (s)
	(cond ((eq (car s) '!) 
			(cond ((atom (cadr s)) (cdr s))
				(T (cadr s))
			)
		  )
		(T (cond ((null (cdr s)) (atom-in-list (car s) '!))
				(T (atom-in-list s '!))
		   )
		)
	))


;;третий шаг (дистрибутивность)
(defun distrib (s)
		  (cond ((atom (car s)) s)
				(T (let ((lb (car s))
					    (rb (caddr s))
					    (sgn (cadr s)))
					  (cond ((eq '& sgn) (pre-ch-brack (distrib lb) sgn (distrib rb)))
							((eq '+ sgn) (append (list (distrib lb)) (list sgn) (list (distrib rb))))
							(T (print 'error))
					  )
				))
		  ))
	
	;;наличие кон-ции в скобках
(defun pre-ch-brack (lb sgn rb)
	(cond ((and (atom (car lb)) (atom (car rb))) (make-a-a lb sgn rb))
		((atom (car lb)) (ch-brack-1at lb rb))
		((atom (car rb)) (ch-brack-1at rb lb))
		(T (ch-brack-0at lb rb))
	))

(defun ch-brack-1at (lb rb)
	(cond ((eq (cadr rb) '+) (make-a-d lb rb))
		(T (make-a-a lb '& rb))
	))

(defun ch-brack-0at (lb rb)
	(let ((lb1 (car lb))
		  (sgn1 (cadr lb))
		  (rb1 (caddr lb))
		  (lb2 (car rb))
		  (sgn2 (cadr rb))
		  (rb2 (caddr rb))) 
		(cond ((and (eq sgn1 '&) (eq sgn2 '&)) (make-a-a lb '& rb))
			  ((eq sgn1 '&) (make-a-d lb rb))
			  ((eq sgn2 '&) (make-a-d rb lb))
			  ((and (eq sgn1 '+) (eq sgn2 '+)) (make-d-d lb rb))
			  (T (print 'error2))
		)
	))
	
	
(defun make-a-a (lb sgn rb)
	(append (list lb) (list sgn) (list rb)))
	
(defun make-a-d (lb rb)
	(let ((llb (list lb))
		(lk (list '&))
		(ld (list '+))
		(lb2 (list (car rb)))
		(rb2 (list (caddr rb))))
		(append (list (distrib (append llb lk lb2))) ld (list (distrib (append llb lk rb2))))
	))
	
(defun make-d-d (lb rb)
		(let ((lb1 (list (car lb)))
			 (rb1 (list (caddr lb)))
			 (lb2 (list (car rb)))
			 (rb2 (list (caddr rb)))
			 (lk (list '&))
			 (ld (list '+)))
			(append 
				(list (append (list (distrib (append lb1 lk lb2))) ld (list (distrib (append lb1 lk rb2)))))
				ld
				(list (append (list (distrib (append rb1 lk lb2))) ld (list (distrib (append rb1 lk rb2)))))
			)
		)
	)
		
;;обычное представление днф
;;каждая буква конъюнкции в отдельныъх скобках, каждая кон-ция в отдельных скобках, знаки операций отсутствуют
(defun srep (s)
	(cond ((atom (car s)) (list (list s)))
		(T (let ((lb (car s))
				 (sgn (cadr s))
				 (rb (caddr s)))
				 (cond ((eq sgn '&) (list (append (srep-k lb) (srep-k rb))))
					   ((and (atom (car lb)) (atom (car rb))) (append (list (list lb)) (list (list rb))))
					   ((atom (car lb)) (append (list (list lb)) (srep rb)))
					   ((atom (car rb)) (append (srep lb) (list (list rb))))
					   ((eq sgn '+) (append (srep lb) (srep rb)))
					   (T (print 'err-srep))
					)
			))
	))		
	
	;;кон-цию в скобку
(defun srep-k (s)
	(cond ((atom (car s)) (list s))
		(T (let ((lb (car s))
				 (sgn (cadr s))
				 (rb (caddr s)))
				 (cond ((eq sgn '&) (append (srep-k lb) (srep-k rb)))
					   (T (print 'err-srep-k))
					)
			))
	))

;;проверка наличия констант
(defun find-const (s)
	(cond ((s-refcond 'TRUE 'FALSE s) (list (list (list 'TRUE))))
		  (T (pre-find-false s))
	))	

(defun s-refcond (a b s)
	(or (find-in-list (list (list a)) s) (find-in-list (list (mref b)) s)))
	
	;;поиск списка в списке списков (подается список, сравнивается со списком)
(defun find-in-list (el s)
	(cond ((null (cdr s)) (equal el (car s)))
		  (T (or (equal el (car s)) (find-in-list el (cdr s))))
	))

	;;поиск (a)(! a) или (FALSE) и удаление TRUE
(defun pre-find-false (s)
	(cond ((null (cdr s)) (list (check-not-only-true (del-true (find-false (car s))))))
		  (T (append (list (check-not-only-true (del-true (find-false (car s))))) (pre-find-false (cdr s))))
	))
								
	;;проверка, чтобы не удалить всю кон-цию содержащую TRUE
(defun check-not-only-true (s)
	(cond ((null (car (makelist s))) (list (list 'TRUE)))
		  (T s)
	))
	
	;;возвр список, обернутый для append
(defun find-false (s)
	(cond ((refcond 'FALSE 'TRUE s '1) (list (list 'FALSE)))
		  ((find-oppos s) (list (list 'FALSE)))
		  (T s)
	))

	;;поиск (a) (! a)
(defun find-oppos (s)
	(find-oppos-d s s))
	
(defun find-oppos-d (s s0)
		(cond ((null (cdr s)) (find-in-list (cons '! (car s)) s0))
			  (T (or (find-in-list (cons '! (car s)) s0) (find-oppos-d (cdr s) s0)))
	))
	
	;;удаление true
(defun del-true (s)
	(cond ((null (cdr s)) (del-true2 (car s)))
		  (T (append (del-true2 (car s)) (del-true (cdr s))))
	))

(defun del-true2 (s)
	(cond ((or (equal (list 'TRUE) s) (equal (cons '! (list 'FALSE)) s)) NIL)
		  (T (list s))
	))
	
	;;условие с отрицанием
(defun refcond (a b s n)
	(cond ((eq n '1) (or (find-in-list (list a) s) (find-in-list (mref b) s)))
		  ((eq n '2) (or (find-in-list (list (list a)) s) (find-in-list (list (mref b)) s)))
	))
	
	;;добавляем ! перед
(defun mref (a)
	(cons '! (list a)))

;;проверка на NIL
(defun if-nil (s)
	(cond ((or (null (car (makelist s)))) (list (list (list 'FALSE))))
		  (T s)
	))
	
;;проверка, что нет кон-ций - FALSEов
(defun pre-ch-f-betwkon (s)
	(cond ((null (cdr s)) s)
		  (T (ch-f-betwkon s))
	))

(defun ch-f-betwkon (s)
	(cond ((null (cdr s)) (eq-false (car s)))
		  (T (append (eq-false (car s))(ch-f-betwkon (cdr s))))
	))	
	
(defun eq-false (s)
	(cond ((eq-and-eqref 'FALSE 'TRUE s '2) NIL)
		  (T (list s))
	))
		
(defun eq-and-eqref (a b s n)	
	(cond ((eq n '1) (or (equal (list a) s) (equal (mref b) s)))
		  ((eq n '2) (or (equal (list (list a)) s) (equal (list (mref b)) s)))
	))	  
		  
;;общая проверка повторов(используется bl1-ch-rep-kon из метода блейка)
(defun ch-repeat (s)
	(bl1-ch-rep-kon (ch-rep-let s)))
	
	;;проверка повторяющихся кон-ций
(defun ch-rep-kon0 (s)
	(ch-rep-kon s s))

(defun ch-rep-kon (s1 s2)
	(cond ((null (cdr s1)) (ch-this-kon (car s1) s2))
		  (T (ch-rep-kon (cdr s1) (ch-this-kon (car s1) s2)))
	))
	
(defun ch-this-kon (fe s)
	(append (cons fe (eq-kon fe s))))
	
(defun eq-kon (fe s)
	(cond ((null s) s)
		  ((equal fe (car s)) (append NIL (eq-kon fe (cdr s))))
		  (T (append (list (car s)) (eq-kon fe (cdr s))))
	))
	
	;;проверка повторяющихся букв
(defun ch-rep-let (s)
	(cond ((null (cdr s)) (list (ch-rep-kon0 (car s))))
		(T (append (list (ch-rep-kon0 (car s))) (ch-rep-let (cdr s))))
	))		
	

;;далее во всех переменных вида ...i i-указывает на уровень глубины (снизу) списка в днф (s0 - сама атом)
;;пара - (x) и (! x)
;;метод блейка из 2-х шагов
(defun blake (s3)
	(blake-2 (bl1-ch-rep-kon (blake-1 s3))))
	
	;;первый шаг метода
(defun blake-1 (s3)
	 (new-kon s3))	
		
		;;доп функции
;;_______________________________________________		
		
		;;проверка на пару и возврат найденого элемента
(defun is-pair (la2 lm2)
	(cond ((equal la2 (reflect-cond lm2)) lm2)
		  (T NIL)
	))
		
		;;отрицание буквы
(defun reflect-cond (s1)
	(cond ((eq (car s1) '!) (cdr s1))
		  (T (cons '! s1))
	))	
	
;;________________________________________________

		;;подготовка списка сравнения
(defun new-kon (s3)
	(beg-new-kon (list (car s3)) (cdr s3)))
		
		;;проход по кон-циям
(defun beg-new-kon (sa3 s3)
	(cond ((null s3) sa3)
		  (T (beg-new-kon (cons (car s3) sa3) (make-new-kon sa3 (car s3) (cdr s3))))
	))
	
		;;возвращает список с вставленой новой кон-цией
(defun make-new-kon (sa3 k2 sl3)
	(append (comp-w-sa sa3 sa3 k2) sl3))
		
	

		;;проход по доп. списку
(defun comp-w-sa (sa3 sw3 k2)
	(cond ((null (cdr sw3)) (pre-is-rep-in-ka sa3 (pre-merg (car sw3) k2)))
		  (T (append (pre-is-rep-in-ka sa3 (pre-merg (car sw3) k2)) (comp-w-sa sa3 (cdr sw3) k2)))
	))

	
			;;проверка, есть ли уже такая кон-ция в доп. списке
(defun pre-is-rep-in-ka (sa3 s3)
	(cond ((not (is-rep-in-ka sa3 (car s3))) NIL)
		  (T s3)
	))
			
(defun is-rep-in-ka (sa3 s2)
	(cond ((null (cdr sa3)) (eq-two-kon (car sa3) s2))
		  (T (and (eq-two-kon (car sa3) s2) (is-rep-in-ka (cdr sa3) s2)))
	))
		
			
				;;равенство длины !!!!важно:(T - неэквивалентны)
(defun eq-two-kon (sa2 sm2)
	(cond ((eq (length sa2)(length sm2)) (beg-eq-two-kon sa2 sm2))
		  (T)
	))
		
				;;удаление одинаковых частей для дальнейшей проверки
(defun beg-eq-two-kon (sa2 sm2)
	(cond ((null (cdr sm2)) (find-del (car sm2) sa2))
		  (T (beg-eq-two-kon (find-del (car sm2) sa2) (cdr sm2)))
	))	
	
		;;проверка, нашлось ли что-то
(defun pre-merg (ka2 km2)
	(let ((eil1 (is-in-two ka2 km2)))
		(cond (eil1 (ch-rep-pm (make-merg ka2 km2 eil1)))
			  (T NIL)
		)
	))
	
			;;проверка на (! x)(x) и удаление повторов
(defun ch-rep-pm (s2)
	(cond ((null s2) NIL)
		  ((find-oppos s2) NIL)
		  (T (list (ch-rep-kon0 s2)))
	))
	
			;;проверка наличия пары для двух списков
(defun is-in-two (ka2 km2) 
	(cond ((null (cdr km2)) (is-in-ka ka2 (car km2)))
		  (T (or (is-in-ka ka2 (car km2)) (is-in-two ka2 (cdr km2))))
	))
	
		;;наличие буквы в кон-ции
(defun is-in-ka (ka2 lm1)
	(cond ((null (cdr ka2)) (is-pair (car ka2) lm1))
		  (T (or (is-pair (car ka2) lm1) (is-in-ka (cdr ka2) lm1)))
	))
	
		;;получение новой кон-ции
(defun make-merg (ka2 km2 l1)
	(append (find-del (reflect-cond l1) ka2) (find-del l1 km2)))
		
		;;найти и уничтожить
(defun find-del (l1 k2)
	(cond ((null (cdr k2)) (eq-find-del l1 (car k2)))
		  (T (append (eq-find-del l1 (car k2)) (find-del l1 (cdr k2))))
	))
	
		;;проверка на равенство
(defun eq-find-del (la1 lm1)
	(cond ((equal la1 lm1) NIL)
		  (T (list lm1))
	))

		;;проверка повторов конъюнкций(добавлена true-fr-rx-x)
(defun bl1-ch-rep-kon (s3)
	(cond ((true-fr-rx-x s3) '(((TRUE))))
		(T (bl1-crk-beg (list (car s3)) (cdr s3)))
	))
		
			;;проверка есть ли (! а) и (а)
(defun true-fr-rx-x (s3)
	(cond ((null (cdr s3)) NIL)
		  ((eq (length (car s3)) 1) (or (tfrx-ch (car s3) (cdr s3)) (true-fr-rx-x (cdr s3))))
		  (T (true-fr-rx-x (cdr s3)))
	))
			
			;;проход по ост. списку
(defun tfrx-ch (s2 s3)
	(cond ((null (cdr s3)) (pre-beg-tfrx-ch s2 (car s3)))
		  (T (or (pre-beg-tfrx-ch s2 (car s3)) (tfrx-ch s2 (cdr s3))))
	))
	
			;;проверка равенства отрицанию
(defun pre-beg-tfrx-ch (sm2 sa2)
	(cond ((eq (length sa2) 1) (equal (reflect-cond (car sm2)) (car sa2)))
		  (T NIL)
	))

			;;проход по осн. списку
(defun bl1-crk-beg (sa3 sm3)
	(cond ((null sm3) sa3)
		  ((null (cdr sm3)) (append (bl1-add (car sm3) sa3) sa3))
		  (T (bl1-crk-beg (append (bl1-add (car sm3) sa3) sa3) (cdr sm3)))
	))
		
			;;добавление в доп. список, если нет
(defun bl1-add (s2 s3)
	(cond ((not (bl1-kon-in s2 s3)) NIL)
		  (T (list s2))
	))
			;;проход по доп. списку с возвратом наличия проверяемой кон-ции
(defun bl1-kon-in (s2 s3)
	(cond ((null (cdr s3)) (eq-two-kon (car s3) s2))
		  (T (and (eq-two-kon (car s3) s2) (bl1-kon-in s2 (cdr s3))))
	))
	

	;;второй шаг метода
(defun blake-2 (s3)
	(bl2-beg s3))

	
		;;проход по кон-циям
(defun bl2-beg (s3)
	(cond ((null (cdr s3)) s3)
		  (T (append (is-nil (bl2-thr-sa-2 (car s3) (cdr s3))) (bl2-beg (bl2-thr-sa-1 (car s3) (cdr s3)))))
	))
				
				;;проверка на NIL
(defun is-nil (s)
	(cond ((null s) NIL)
		  (T (list s))
	))
				
		;;проход по оставшемуся списку для преобр. списка
(defun bl2-thr-sa-1 (s2 s3)
	(let ((fp-21 (find-part (car s3) s2)))
		(cond ((null (cdr s3)) fp-21)
			  (T (append fp-21 (bl2-thr-sa-1 s2 (cdr s3))))
		)
	))
	
		;;проход по оставшемуся списку для проверочного элемента
(defun bl2-thr-sa-2 (s2 s3)
	(let ((fp-12 (find-part s2 (car s3))))
		(cond ((null (cdr s3)) (car fp-12))
			  (T (bl2-thr-sa-2 (and s2 (car fp-12)) (cdr s3)))
		)
	))
		
		;;проверить длины
(defun find-part (sa2 sm2)
	(cond ((null sa2) NIL)
		  ((null sm2) (list sa2))
		  ((> (length sa2) (length sm2)) (ch-entr sa2 sm2))
		  (T (list sa2))
	))
		
		;;проверить вхождение
(defun ch-entr (sa2 sm2)
	(let ((che-l (car (beg-eq-two-kon sm2 sa2))))
		(cond ((null che-l) NIL)
			  (T (list sa2))
		)
	))
	
	
;;окончательное представление сокрДНФ на выходе (каждая буква кон-ции в отдельных скобках, между кон-циями - "+")
(defun finall-rep (s3)
	(cond ((null (cdr s3)) (car s3))
		  (T (append (car s3) (list '+) (finall-rep (cdr s3))))
	))
	
	
(defun finall-and (s)
	(cond ((null (cdr s)) (list (ins-rep (car s))))
		  (T (append (list (ins-rep (car s))) (finall-and (cdr s))))
	))
	
(defun ins-rep (s)
	(cond ((null (cdr s)) (car s))
		  (T (append (car s) '(&) (ins-rep (cdr s))))
	))




;;начало работы
(defun start (s)
	(finall-rep
	(finall-and
	(blake
	(if-nil
	(ch-repeat
	(pre-ch-f-betwkon
	(if-nil
	(ch-repeat
	(if-nil
	(find-const
	(srep
	(distrib
	(demorgan
	(intermstep
	(fststep
	(represent 
	s    )))))))))))))))))

;;тесты		
(defun test (f)
	(let ((ts (append '((FALSE)) 
					'((a)) 
					'((! ! ! a)) 
					'((! a > ! ! b)) 
					'((a & b)) 
					'((a & b + a & b)) 
					'((!((a + ! a) & c))) 
					'((a & ! (b > ! ! c) + ! (! (r & p & p))))
				))
		  (cts (append '((FALSE)) 
					'((TRUE)) 
					'((TRUE & FALSE)) 
					'(((a + b) & c & FALSE)) 
					'((!(a + FALSE)))
				))
		  (bl-tests (append '((((a)(b)(c))))
							'((((a)) ((! a))))
							'((((! a))((! a))((! a))))
							'((((FALSE))))
							'(( ((! a)(b)) ((a)) ))
							'(( ((b)) ((c)(! a)(! b)) ((a)(b)) ))
							'(( ((! b)(! a)) ((a)(b)) ((c)(b)) ))
							'(( ((b)) ((c)(! a)(! b)) ((a)(b)) ((d)(! c)) ))							
				))
		  (isrepik (append '(( ((c)(b)) ((! b)(! a)) ((a)(b)) )) '( ((c)(! a)) ) 
				))
		  (blake-test (append '(( ((! x1)(! x2)) ((x1)(! x2)(x4)) ((x2)(! x3)(x4)) ))
							  '(( ((x1)(! x2)(x3)) ((! x1)(x2)(! x3)) ((! x2)(! x3)(! x4)) ))
							  '(( ((x1)) ((! x1)(x2)) ((! x1)(! x2)(x3)) ((! x1)(x2)(x3)(x4)) ))
							  '(( ((x1)(! x2)(x4)) ((! x1)(! x2)(x3)) ((! x3)(! x4)) ))
				))
		  (r-tests (append  '(((! A + D + FALSE) > ! (B & !  C + D)))
							'((a > (b + !  c & d) + !  e))
							'((!  (A & T & !  B) > C > !  B))
							'((TRUE & FALSE))
							'((A))
							'((A & !  A > B & !  B > C & !  C))
							'((!  (A + B & !  C)))
							'((! (A & !  B + B & !  C + !  D)))
							'((! (A & !  B + B & !  C)))
							'((! (A & B & ! C + A & B & D + B & C & D)))
							'((FALSE > FALSE > FALSE))
							'((FALSE > FALSE > TRUE))
							'((TRUE & TRUE))
							'((X + ! Y))
							'((X + ! X + Y + ! Y))
							'((A & ! A > B + ! B))
				))
		 )
		(mapcar f r-tests)
	))