#lang racket


; constructores

(define (paradigmadocs name date encryptFunction decryptFunction)
(if (and (esName? name) (esDate? date) (esFuns? encryptFunction decryptFunction))  
(list name date encryptFunction decryptFunction '()) null))

;funciones
(define (esName? name)
(string? name))

(define (date D M Y)
(if (and (number? D) (number? M) (number? Y))  (list D M Y) #f))

(define (esDate? date)
(if date #t #f))

(define (esFuns? A B) (if (string=? "test" (B (A "test"))) #t #f))

(define (check_unique_user List username)
(if (null? List) #t (if (string=? username (caar List)) #f (check_unique_user (cdr List) username))))

(define (add_to_list List Index Element)
(if (null? List) '() (if (= 0 Index)  (cons (cons Element (car List)) (cdr List)) (cons (car List) (add_to_list (cdr List) (- Index 1) Element)))))

(define (login_success? List username password)
(if (null? List) #f (if (and (string=? username (caar List)) (string=? password (cadar List))) #t (login_success? (cdr List) username password))))

(define (docUpdate List username content)
(if (null? List) '() (if (string=? username (caar List)) (cons (add_to_list (car List) 2 content) (cdr List)) (cons (car List) (docUpdate (cdr List) username content)))))

(define (addCreate List username id nombre contenido)
(docUpdate List username (list id nombre contenido '())))


;selectores
(define (getName paradigmadocs)
(car paradigmadocs))

(define (getDate paradigmadocs)
(cadr paradigmadocs))

(define (getEncrypt paradigmadocs)
(caddr paradigmadocs))

(define (getDecrypt paradigmadocs)
(cadddr paradigmadocs))

(define (getUsers paradigmadocs) (cadddr (cdr paradigmadocs)))

(define (getActiveUser paradigmadocs) 
(if (null? (cdddr (cddr paradigmadocs))) ""  (cadddr (cddr paradigmadocs))))

(define (getAccessDetails paradigmadocs owner docId user)
(let ((accessId (getEntityA (getEntityB (getDocfromDocId (getEntityB (getUserFromUsername paradigmadocs owner) 0 2) docId) 0 3) (lambda (acc) (equal? (car acc) user)))))
(cond
((equal? user owner) #\a)
(accessId  (cadr accessId))
(else #f))))


(define (setEntityA Container NewData Pred)
(cond
((null? Container) '())
((Pred (car Container)) (cons NewData (setEntityA (cdr Container) NewData Pred)))
(else (cons (car Container) (setEntityA (cdr Container) NewData Pred)))))


(define (setEntityB Container NewData Current Index)
(cond
((null? Container) '())
((equal? Current Index) (cons NewData (setEntityB (cdr Container) NewData  (+ Current 1) Index)))
(else (cons (car Container) (setEntityB (cdr Container) NewData (+ Current 1) Index)))))


(define (getEntityA Container Pred)
(cond
((null? Container) #f)
((Pred (car Container)) (car Container))
(else (getEntityA (cdr Container) Pred))))


(define (getEntityB Container Current Index)
(cond
((null? Container) #f)
((equal? Current Index) (car Container))
(else (getEntityB (cdr Container) (+ Current 1) Index))))



(define (getUserFromUsername paradigmadocs username)
(getEntityA (getUsers paradigmadocs) (lambda (user) (equal? (car user) username))))

(define (getDocfromDocId docs docId)
(getEntityA docs (lambda (doc) (equal? (car doc) docId))))

(define (getDocId User Users)
(if (equal? User (car (car Users))) (length (caddr (car Users))) (getDocId User (cdr Users))))

(define (addShare paradigmadocs username docId access)
(let
((doc (getDocfromDocId (caddr (getUserFromUsername paradigmadocs username)) docId)) (docs (caddr (getUserFromUsername paradigmadocs username))))
(setEntityA (getUsers paradigmadocs) (setEntityB (getUserFromUsername paradigmadocs username) (setEntityA docs (setEntityB doc (append access (cadddr doc)) 0 3) (lambda (x) (equal? (car x) docId))) 0 2) (lambda (u) (equal? (car u) username)))))


(define (addHelper paradigmadocs username docId newText)
(let
((doc (getDocfromDocId (caddr (getUserFromUsername paradigmadocs username)) docId)) (docs (caddr (getUserFromUsername paradigmadocs username))))
(setEntityA (getUsers paradigmadocs) (setEntityB (getUserFromUsername paradigmadocs username) (setEntityA docs (setEntityB doc (string-append (caddr doc) " " newText) 0 2) (lambda (x) (equal? (car x) docId))) 0 2) (lambda (u) (equal? (car u) username)))))

(define (access a b)
(list a b))

(define (accessRemover paradigmadocs username)
(setEntityA (getUsers paradigmadocs) (setEntityB (getUserFromUsername paradigmadocs username) (map (lambda (element) (list (car element) (cadr element) (caddr element) (list ))) (caddr (getUserFromUsername paradigmadocs username))) 0 2) (lambda (con) (equal? (car con) username))))

; modificadores

(define (register paradigmadocs date username password)
(if (check_unique_user (getUsers paradigmadocs) username) (list (getName paradigmadocs) (getDate paradigmadocs) (getEncrypt paradigmadocs) (getDecrypt paradigmadocs) (cons (list username password '() '()) (getUsers paradigmadocs))) paradigmadocs))


(define (login paradigmadocs username password operation)
(let ((isloginsuccess? (login_success? (getUsers paradigmadocs) username password)))
(cond
((equal? #f isloginsuccess?)  paradigmadocs)
((equal? operation revokeAllAccesses)  (revokeAllAccesses (append paradigmadocs (list username))))
((equal? operation paradigmadocs->string)  (paradigmadocs->string (append paradigmadocs (list username))))
(else (lambda (x . y) (operation (append paradigmadocs (list username)) (cons x y))))
)))


(define (create paradigmadocs args)
(list (getName paradigmadocs) (car args) (getEncrypt paradigmadocs) (getDecrypt paradigmadocs) (addCreate (getUsers paradigmadocs) (getActiveUser paradigmadocs) (getDocId (getActiveUser paradigmadocs) (getUsers paradigmadocs)) (cadr args) (caddr args))))


(define (share paradigmadocs args)
(list (getName paradigmadocs) (getDate paradigmadocs) (getEncrypt paradigmadocs) (getDecrypt paradigmadocs) (addShare paradigmadocs (getActiveUser paradigmadocs) (car args) (cdr args))))


(define (add paradigmadocs args)
(list (getName paradigmadocs) (cadr args) (getEncrypt paradigmadocs) (getDecrypt paradigmadocs) (addHelper paradigmadocs (getActiveUser paradigmadocs) (car args) (caddr args))))


(define (revokeAllAccesses paradigmadocs)
(list (getName paradigmadocs) (getDate paradigmadocs) (getEncrypt paradigmadocs) (getDecrypt paradigmadocs) (accessRemover paradigmadocs (getActiveUser paradigmadocs)) ))

(define (paradigmadocs->string paradigmadocs)
(string-append "Nombre del doc: " (getName paradigmadocs) "\n" "Fecha: " (number->string (car (getDate paradigmadocs)))  "/" (number->string (cadr (getDate paradigmadocs))) "/"  (number->string (caddr (getDate paradigmadocs)) ) "\n" "Numero de usuarios: " (number->string (length (getUsers paradigmadocs))) 
"\n" "Nombre de usuario del usuario activo: " (getActiveUser paradigmadocs) "\n" "Numero de documentos para usuario activos: " (number->string (length (caddr (getUserFromUsername paradigmadocs (getActiveUser paradigmadocs)) 
)))))


(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define emptyGDocs (paradigmadocs "gdocs" (date 25 10 2021) encryptFn encryptFn))

; Ver el estado

emptyGDocs
(newline)

(define GDocs (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))

; Estrucutra para la creación de archivos y posterior actualizaciones:

GDocs
(newline)

; Ejemplos para su ingreso y creación de un archivo.
(define GDocs1 ((login GDocs "user2" "pass2" create) (date 28 10 2021) "doc1" "hola1"))


; Estructura actual.
GDocs1
(newline)

(define GDocs2 ((login GDocs1 "user2" "pass2" create) (date 28 10 2021) "doc2" "hola2"))
GDocs2
(newline)

(define GDocs3 ((login GDocs2 "user1" "pass1" create) (date 28 10 2021) "doc2" "hola2"))
GDocs3
(newline)

(define GDocs4 ((login GDocs3 "user2" "pass2" share) 1 (access "user1" #\r) (access "user3" #\w)))
GDocs4
(newline)

(define GDocs5 ((login GDocs4 "user2" "pass2" add) 1 (date 30 10 2021) "Hola de nuevo"))
GDocs5
(newline)

(define GDocs6 (login GDocs5 "user2" "pass2" revokeAllAccesses)) 
GDocs6
(newline)

(define S (login GDocs6 "user2" "pass2" paradigmadocs->string))
(display S)

